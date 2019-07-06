#------------------------------------------------------------------------------
# Program Name: A3.2.Adjust_Shipping_Fuel_Cons.R
# Authors Names: Steve Smith, Linh Vu
# Date Last Updated: 5 June 2016
# Program Purpose: Reads in exogenous time series for global shipping fuel consumption
#                  Adds difference with reported shipping fuel to global international shipping sector
#
#                  The idea here is that emissions from shipping fuel reported by a region
#                  (either domestic or international) will be estimated within that region.
#                  Because, however, the reported data underestimates shipping fuel consumption,
#                  this script adds the difference to a global sector.
#
#                  Note that this additional fuel is not otherwise accounted for. Because
#                  we are using a sectoral approach, we are implicitly assuming the difference
#                  is within statistical reporting differences.
#
#                  The output is energy data with additional consumption for international
#                  shipping.
# Input: A.IEA_BP_energy_ext.csv, A.IEA_en_stat_ctry_hist.csv, Shipping_Fuel_Consumption.xlsx
#        IEA_product_fuel.csv
# Output: A.IEA_BP_energy_ext.csv, A.intl_shipping_en.csv
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "analysis_functions.R", "process_db_functions.R",
                  "timeframe_functions.R", "interpolation_extension_functions.R",
                  "common_data.R" ) # Additional function files required.
    log_msg <- "Add under-counted shipping emissions to global region" # First message to be printed to the log
    script_name <- "A3.2.Adjust_Shipping_Fuel_Cons.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Read in files
    iea_data <- readData( "MED_OUT", "A.IEA_en_stat_ctry_hist" )
    iea_data_extended <- readData( "MED_OUT", "A.IEA_BP_energy_ext" )
    iea_data_before_shipping_adj <- iea_data_extended
    shipping_fuel <- readData( "ENERGY_IN", "Shipping_Fuel_Consumption" , ".xlsx",
                                   skip = 4, sheet_selection = "Data" )
    IEA_product_fuel <- readData( "EN_MAPPINGS", "IEA_product_fuel" )

# Pre-1855 shipping coal extrapolation
    shipping_coal_extrap <- readData( "ENERGY_IN", "Shipping_Fuel_Consumption" , ".xlsx",
                                 sheet_selection = "Pre-1855_Extrap" )

# -----------------------------------------------------------------------------------------
# 2. Clean reported shipping data
# Result is time series for total shipping fuel from 1850 to 2012
# in three categories: hard_coal, heavy_oil, diesel_oil
    shipping_fuel <- shipping_fuel[ 1:163, 1:4 ]
    names( shipping_fuel ) <- c( "year", "hard_coal", "heavy_oil", "diesel_oil" )

# Melt and recast
    shipping_fuel <- melt( shipping_fuel, id = "year" )  ### TODO: use gather()
    names( shipping_fuel ) <- c( "year", "fuel", "ship_fuel" )
    last_shipping_data_year <- max( shipping_fuel$year )

# -----------------------------------------------------------------------------------------
# 3. Compute IEA shipping fuel. Aggregates fishing, bunker, shipping fuel
#    by fuel. Result is long-form dataframe of shipping fuel activity by fuel per year.

# Total IEA shipping = fishing + international shipping + domestic aviation
# For purposes here, treat all coal as hard coal
    iea_data$fuel <- IEA_product_fuel$fuel[ match( iea_data$PRODUCT, IEA_product_fuel$product ) ]
    iea_data$fuel [ which ( iea_data$fuel %in% c( 'hard_coal', 'brown_coal' ) ) ] <- 'hard_coal'

    Fishing_fuel <- aggregate( iea_data[ X_IEA_years ],
                    by=list( fuel = iea_data$fuel,
                             FLOW = iea_data$FLOW ), sum )

# Retrieve IEA fishing fuel data
    Fishing_fuel <- Fishing_fuel[ which(Fishing_fuel$FLOW == "FISHING" ), ]
    Fishing_fuel <- Fishing_fuel[ c( "fuel", X_IEA_years ) ]

# Aggregate CEDS data by fuel and sector
    ceds_iea_data <- aggregate( iea_data_extended[ X_IEA_years ],
                                by=list( fuel = iea_data_extended$fuel,
                                         sector = iea_data_extended$sector ), sum )
    ceds_iea_data$fuel [ which ( ceds_iea_data$fuel %in% c( 'hard_coal', 'brown_coal' ) ) ] <- 'hard_coal'

    Int_Bunker_fuel <- ceds_iea_data[ which(ceds_iea_data$sector == "1A3di_International-shipping" ), ]
    Domestic_Ship_fuel <- ceds_iea_data[ which(ceds_iea_data$sector == "1A3dii_Domestic-navigation" ), ]

    Int_Bunker_fuel <- Int_Bunker_fuel[ c( "fuel", X_IEA_years ) ]
    Domestic_Ship_fuel <- Domestic_Ship_fuel[ c( "fuel", X_IEA_years ) ]

    Total_IEA_Ship_Fuel <- rbind( Int_Bunker_fuel, Domestic_Ship_fuel, Fishing_fuel)
    Total_IEA_Ship_Fuel <- aggregate( Total_IEA_Ship_Fuel[ X_IEA_years ],
                                by=list( fuel = Total_IEA_Ship_Fuel$fuel ), sum )

# Keep only diesel_oil, heavy_oil, and coal
    Total_IEA_Ship_Fuel <- filter( Total_IEA_Ship_Fuel,
                                   fuel %in% c( "hard_coal", "heavy_oil", "diesel_oil" ) )

# Melt
    Total_IEA_Ship_Fuel <- melt( Total_IEA_Ship_Fuel, id = "fuel" )
    names( Total_IEA_Ship_Fuel ) <- c( "fuel", "year", "IEA_fuel" )
    Total_IEA_Ship_Fuel$year <- xYearToNum( Total_IEA_Ship_Fuel$year )

# -----------------------------------------------------------------------------------------
# 4. Determine amount of additional shipping fuel to be added
#
#    Add to global international shipping sector fuel consumption equal to the difference between
#    shipping_fuel variable and the total_IEA_shipping fuel.
#    Where total_IEA_shipping is > shipping_fuel:
#      diesel_oil : (ignore, as this shouldn't happen) ### Should there be a check for this?
#      heavy_oil : Account for this in diesel oil (e.g., add less diesel oil)
#      coal : ignore (no adjustment )
#
#    Ignore coking coal, biomass, and natural gas
#
#    Desired result is a time series by fuel from 1850 to the end of shipping_fuel data
#
#    The above will generally be < IEA years. Last step is, for the last year of IEA data,
#    calculate average IEA underreport for last three years,and add that amount.
#
#    Then make constant for any years after IEA years.
#
#    For years before IEA years, the global international shipping sector will contain
#    the entire shipping fuel estimate, since there is no IEA data at that point.

# Combine two dfs
    comp <- merge( Total_IEA_Ship_Fuel, shipping_fuel, all = T )

# If IEA < ship_fuel, add the difference to a new df
    comp <- dplyr::mutate( comp, diff = ship_fuel - IEA_fuel )
    to_add <- filter( comp, diff > 0 )

# For heavy_oil, if IEA > ship_fuel, subtract the difference from added diesel_oil
    to_subtract <- filter( comp, fuel == "heavy_oil", diff < 0 )
    if( nrow( to_subtract ) > 0) to_subtract$fuel <- "diesel_oil"

# Result is shipping fuel to be added to global intl shipping -- put this in a df
    global_intl_ship <- merge( select( to_add, year, fuel, global_fuel = diff ),
                               select( to_subtract, year, fuel, adj = diff ),
                               all = T )
    adj_years <- !is.na( global_intl_ship$adj )  # where is IEA > ship_fuel?
    global_intl_ship$global_fuel[ adj_years ] <-
      global_intl_ship$global_fuel[ adj_years ] + global_intl_ship$adj[ adj_years ]
    global_intl_ship$adj <- NULL  # already made adjustment so don't need this anymore

# Extend global_int_ship from 1850 to end of emissions years
    global_intl_ship_full <- merge( data.frame( year = 1850:max( emissions_years ) ),
                               data.frame( fuel = c( "hard_coal", "heavy_oil", "diesel_oil" ) ) ) %>%
      merge( global_intl_ship, all = T )
    global_intl_ship_full$global_fuel[ is.na( global_intl_ship_full$global_fuel ) ] <- 0

# Give all ship_fuel to global_fuel before IEA starts
    global_intl_ship_full <- merge( global_intl_ship_full, shipping_fuel, all = T )  ###TODO: consider using a join() function from dplyr
    before_IEA <- global_intl_ship_full$year < min( IEA_years )
    global_intl_ship_full$global_fuel[ before_IEA ] <- global_intl_ship_full$ship_fuel[ before_IEA ]
    global_intl_ship_full$ship_fuel <- NULL  # already copied over so don't need this anymore

# To extend beyond IEA, extend using average absolute IEA underreport for last three years with shipping data
    # Find year range, default is last 3 years of shipping data, check if IEA data doesn't go that far
    range_min = min( last_shipping_data_year - 2, IEA_end_year -2)
    range_max = min( last_shipping_data_year, IEA_end_year )

    avg <- filter( global_intl_ship_full, year %in% seq( range_min, range_max ) ) %>%
      group_by( fuel ) %>%
      dplyr::summarise( global_fuel = mean( global_fuel ) )
    extended <- global_intl_ship_full$year %in% c( seq( IEA_end_year, end_year )  )
    global_intl_ship_full$global_fuel[ extended ] <-
      avg$global_fuel[ match( global_intl_ship_full$fuel[ extended ], avg$fuel ) ]

# Cast to wide format
    global_intl_ship_wide <- global_intl_ship_full
    global_intl_ship_wide$sector <- "1A3di_International-shipping"
    global_intl_ship_wide$iso <- "global"
    global_intl_ship_wide$units <- "kt"
    global_intl_ship_wide$year <- paste0( "X", global_intl_ship_wide$year )
    global_intl_ship_wide <- cast( global_intl_ship_wide,
                                  fuel + sector + iso + units ~ year, value = "global_fuel" ) ### TODO: use spread()

# Add global_intl_ship_wide to CEDS
    iea_data_extended <- rbind( iea_data_extended, global_intl_ship_wide[
      names( global_intl_ship_wide ) %in% names( iea_data_extended ) ] )

# Diagnostics: should have global_fuel = IEA_fuel + ship_fuel
    ship_check <- merge( global_intl_ship_full, select( comp, -diff ), all = T )
    ship_check[ is.na( ship_check ) ] <- 0
    ship_check <- dplyr::mutate( ship_check, check = IEA_fuel + global_fuel - ship_fuel ) %>%
      filter( check != 0 ) %>%
      dplyr::arrange( dplyr::desc( year ) )

# -----------------------------------------------------------------------------
# 5. Extrapolate pre-1855 global shipping coal using extrapolation data from
#    Fouquet & Pearson (1998).

# Reformat and extend extrap values
    global_coal <- select( shipping_coal_extrap, year = Year, global = Total_Ship_Extrap ) %>%  ### TODO: use gather in this section instead of melt
      melt( id = "year" )
    names( global_coal ) <- c( "year", "iso", "consumption" )
    british_coal <- select( shipping_coal_extrap, year = Year, gbr = British_Shipping_Coal ) %>%
      melt( id = "year" )
    names( british_coal ) <- c( "year", "iso", "consumption" )
    coal_extrap <- rbind( global_coal, british_coal )
    coal_extrap$sector <- "1A3di_International-shipping"
    coal_extrap$fuel <- "hard_coal"
    coal_extrap$units <- "kt"
    coal_extrap$year <- paste0( "X", coal_extrap$year )
    coal_extrap <- cast( coal_extrap, iso + sector + fuel + units ~ year, value = "consumption" )
    coal_extrap$X1750 <- 0
    coal_extrap <- interpolateValues( coal_extrap )

# Add extended years to global_intl_ship_wide
    ship_out <- rbind.fill( coal_extrap, global_intl_ship_wide )
    ship_out[ is.na( ship_out ) ] <- 0
    ship_out <- ship_out[ c( "iso", "sector", "fuel", "units", X_extended_years ) ]
    ship_out <- group_by( ship_out, iso, sector, fuel, units ) %>%
      summarise_all( max )

# Separate for output
    global_shipping <- ship_out[which(ship_out$iso == 'global'),]
    else_shipping <- ship_out[which(ship_out$iso != 'global'),]

# -----------------------------------------------------------------------------
# 6. Output
    writeData( global_shipping, "MED_OUT", "A.intl_shipping_en" )
    writeData( else_shipping, "DIAG_OUT", "A.intl_shipping_en_country_break-out" )
    writeData( iea_data_extended, "MED_OUT", "A.IEA_BP_energy_ext" )
    writeData( iea_data_before_shipping_adj, "DIAG_OUT", "A.IEA_BP_energy_ext_before_shipadj" )
    writeData( ship_check, "DIAG_OUT", "A.intl_shipping_discrepancy" )

# Every script should finish with this line
    logStop()

# END

