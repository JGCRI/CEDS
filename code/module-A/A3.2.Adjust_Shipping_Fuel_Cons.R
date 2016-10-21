#------------------------------------------------------------------------------
# Program Name: A3.2.Adjust_Shipping_Fuel_Cons.R
# Authors Names: Steve Smith, Linh Vu
# Date Last Updated: 7 Mar 2016
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

# Before we can load headers we need some paths defined. They may be provided by
#   a system environment variable or may have already been set in the workspace.
# Set variable PARAM_DIR to be the data system directory
    dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
    for ( i in 1:length( dirs ) ) {
        setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
        wd <- grep( 'CEDS/input', list.dirs(), value = T )
        if ( length(wd) > 0 ) {
            setwd( wd[1] )
            break
        }
    }
    PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "analysis_functions.R", "process_db_functions.R", 
                  "timeframe_functions.R", "interpolation_extention_functions.R", 
                  "common_data.R" ) # Additional function files required.
    log_msg <- "Add under-counted shipping emissions to global region" # First message to be printed to the log
    script_name <- "A3.2.Adjust_Shipping_Fuel_Cons.R"
    
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Read in files
    iea_data <- readData( "MED_OUT", "A.IEA_en_stat_ctry_hist" )
    iea_data_extended <- readData( "MED_OUT", "A.IEA_BP_energy_ext" )
    shipping_fuel <- readData( "ENERGY_IN", "Shipping_Fuel_Consumption" , ".xlsx", 
                                   skip_rows = 4, sheet_selection = "Data" )
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
    shipping_fuel <- melt( shipping_fuel, id = "year" )
    names( shipping_fuel ) <- c( "year", "fuel", "ship_fuel" )
 
# -----------------------------------------------------------------------------------------
# 3. Compute IEA shipping fuel
# Total IEA shipping = fishing + international shipping + domestic aviation    
# For purposes here, treat all coal as hard coal
  
    iea_data$fuel <- IEA_product_fuel$fuel[ match( iea_data$PRODUCT, IEA_product_fuel$product ) ]
    iea_data$fuel [ which ( iea_data$fuel %in% c( 'hard_coal', 'brown_coal' ) ) ] <- 'hard_coal'
    
    Fishing_fuel <- aggregate( iea_data[ X_IEA_years ],
                    by=list( fuel = iea_data$fuel,
                             FLOW = iea_data$FLOW ), sum )
  
    Fishing_fuel <- Fishing_fuel[ which(Fishing_fuel$FLOW == "FISHING" ), ]
    Fishing_fuel <- Fishing_fuel[ c( "fuel", X_IEA_years ) ]
    
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
# Add to global international shipping sector fuel consumption equal to the difference between 
# shipping_fuel variable and the total_IEA_shipping fuel.
# Where total_IEA_shipping is > shipping_fuel:
#   diesel_oil : (ignore, as this shouldn't happen)
#   heavy_oil : Account for this in diesel oil (e.g., add less diesel oil)
#   coal : ignore (no adjustment )
# 
# Ignore coking coal, biomass, and natural gas
#
# Desired result is a time series by fuel from 1850 to the end of shipping_fuel data 
# 
# The above will generally be < IEA years. Last step is, for the last year of IEA data, 
# calculate average IEA underreport for last three years,and add that amount.
# 
# Then make constant for any years after IEA years.
#
# For years before IEA years, the global international shipping sector will contain 
# the entire shipping fuel estimate, since there is no IEA data at that point. 
    
# Combine two dfs
    comp <- merge( Total_IEA_Ship_Fuel, shipping_fuel, all = T )

# If IEA < ship_fuel, add the difference to a new df
    comp <- mutate( comp, diff = ship_fuel - IEA_fuel )
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
    global_intl_ship_full <- merge( global_intl_ship_full, shipping_fuel, all = T )
    before_IEA <- global_intl_ship_full$year < min( IEA_years )
    global_intl_ship_full$global_fuel[ before_IEA ] <- 
      global_intl_ship_full$ship_fuel[ before_IEA ]
    global_intl_ship_full$ship_fuel <- NULL  # already copied over so don't need this anymore
    
# For 2013-2014, extend using average IEA underreport for 2010-2012
    avg <- filter( global_intl_ship_full, year %in% seq( 2010, 2012 ) ) %>%
      group_by( fuel ) %>% summarise( global_fuel = mean( global_fuel ) )
    extended <- global_intl_ship_full$year %in% c( 2013, BP_years )
    global_intl_ship_full$global_fuel[ extended ] <- 
      avg$global_fuel[ match( global_intl_ship_full$fuel[ extended ], avg$fuel ) ]
    
# Cast to wide format
    global_intl_ship_wide <- global_intl_ship_full
    global_intl_ship_wide$sector <- "1A3di_International-shipping"
    global_intl_ship_wide$iso <- "global"
    global_intl_ship_wide$units <- "kt"
    global_intl_ship_wide$year <- paste0( "X", global_intl_ship_wide$year )
    global_intl_ship_wide <- cast( global_intl_ship_wide, 
                                  fuel + sector + iso + units ~ year, value = "global_fuel" )

# Add global_intl_ship_wide to CEDS
    iea_data_extended <- rbind( iea_data_extended, global_intl_ship_wide[ 
      names( global_intl_ship_wide ) %in% names( iea_data_extended ) ] )
    
# Diagnostics: should have global_fuel = IEA_fuel + ship_fuel
    ship_check <- merge( global_intl_ship_full, select( comp, -diff ), all = T )
    ship_check[ is.na( ship_check ) ] <- 0
    ship_check <- mutate( ship_check, check = IEA_fuel + global_fuel - ship_fuel ) %>%
      filter( check != 0 ) %>% arrange( desc( year ) )
        
# -----------------------------------------------------------------------------
# 5. Extrapolate pre-1855 shipping coal
# Reformat and extend extrap values
    global_coal <- select( shipping_coal_extrap, year = Year, global = Total_Ship_Extrap ) %>%
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
      summarise_each( fun = "max" )

#Seperate for output
    global_shipping <- ship_out[which(ship_out$iso == 'global'),]
    else_shipping <- ship_out[which(ship_out$iso != 'global'),]
    
# -----------------------------------------------------------------------------
# 6. Output
    writeData( global_shipping, "MED_OUT", "A.intl_shipping_en" )
    writeData( else_shipping, "DIAG_OUT", "A.intl_shipping_en_country_break-out" )
    writeData( iea_data_extended, "MED_OUT", "A.IEA_BP_energy_ext" )
    writeData( ship_check, "DIAG_OUT", "A.intl_shipping_discrepancy" )

# Every script should finish with this line
 logStop()
    
# END
    