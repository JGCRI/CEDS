#------------------------------------------------------------------------------
# Program Name: A8.2.Adjust_Shipping_Fuel_Cons.R
# Authors Names: Steve Smith, Patrick O'Rourke, Linh Vu
# Date Last Updated: December 6, 2019
# Program Purpose: Reads in exogenous time series for global shipping fuel consumption
#                  ( domestic and international ).
#
#                  Adds any positive differences in shipping ( exogenous > CEDS ) to CEDS,
#                  and adds negative differences in shipping ( CEDS > exogenous ) for heavy_oil
#                  to what would have been added to global diesel_oil.
#
#                  The idea here is that emissions from shipping fuel reported by a region
#                  (either domestic or international) will be estimated within that region.
#                  Because, however, the reported IEA data underestimates shipping fuel consumption,
#                  this script adds the difference to a global sector.
#
#                  Note that this additional fuel is not otherwise accounted for. Because
#                  we are using a sectoral approach, we are implicitly assuming the difference
#                  is within statistical reporting differences.
#
#                  The output is energy data with additional consumption for international
#                  shipping for the "global" iso.
# Input: A.comb_user_added.csv, Shipping_Fuel_Consumption.xlsx,
# Output: A.pre_adj_shipping_difference.csv, A.post_adj_shipping_discrepancy.csv,
#         A.intl_shipping_en_country_break-out.csv, A.comb_int_shipping_adjusted.csv
# TODO:
#-------------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "analysis_functions.R", "process_db_functions.R",
                  "timeframe_functions.R", "interpolation_extension_functions.R",
                  "common_data.R" ) # Additional function files required.
    log_msg <- "Add under-counted shipping emissions to global region..." # First message to be printed to the log
    script_name <- "A8.2.Adjust_Shipping_Fuel_Cons.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Read in files
#   CEDS comb. activity data after adding user-defined data
    CEDS_comb_activity <- readData('MED_OUT','A.comb_user_added')

#   IEA data and fuel mapping file
    iea_data <- readData( "MED_OUT", "A.IEA_en_stat_ctry_hist" )
    IEA_product_fuel <- readData( "EN_MAPPINGS", "IEA_product_fuel" )

#   Exogenous time series for global shipping fuel consumption
    shipping_fuel <- readData( "ENERGY_IN", "Shipping_Fuel_Consumption" , ".xlsx",
                                skip = 4, sheet_selection = "Data" )

#   Pre-1855 shipping coal extrapolation
    shipping_coal_extrap <- readData( "ENERGY_IN", "Shipping_Fuel_Consumption" , ".xlsx",
                                      sheet_selection = "Pre-1855_Extrap" )

# -----------------------------------------------------------------------------------------
# 2. Clean exogenous reported shipping data
#   Result is time series for total shipping fuel from 1850 to 2012
#   in three categories: hard_coal, heavy_oil, diesel_oil
    shipping_fuel <- shipping_fuel %>%
        dplyr::select( Year, Coal, Resid, Distillate_and_Other ) %>%
        dplyr::rename( year = Year,
                       hard_coal = Coal,
                       heavy_oil = Resid,
                       diesel_oil = Distillate_and_Other ) %>%
        tidyr::gather( fuel, ship_fuel, c( "hard_coal", "heavy_oil", "diesel_oil" ) )

#   Define the last year of exogenous shipping fuel data
    last_shipping_data_year <- max( shipping_fuel$year )

# -----------------------------------------------------------------------------------------
#   3. Compute CEDS shipping fuel. Aggregates international bunker and domestic shipping
#   shipping fuel by fuel type. Result is long-form dataframe of shipping fuel activity by
#   CEDS fuel per year.

#   Total CEDS shipping = international shipping + domestic navigation
#   For our purposes here, treat all coal (brown and hard coal) as hard_coal. Coal coke is ignored.
    CEDS_clean <- CEDS_comb_activity %>%
        dplyr::select( -agg_sector, -agg_fuel ) %>%
        dplyr::rename( sector = CEDS_sector, fuel = CEDS_fuel ) %>%
        dplyr::mutate( units = "kt" ) %>%
        dplyr::select( iso, sector, fuel, units, X_extended_years )

    CEDS_shipping_fuel <- CEDS_clean %>%
        dplyr::select( -iso ) %>%
        dplyr::mutate( fuel = if_else( fuel %in% c( "brown_coal" ), "hard_coal", fuel ) ) %>%
        dplyr::filter( fuel %in% c( "hard_coal", "heavy_oil", "diesel_oil" ),
                       sector %in% c( "1A3di_International-shipping", "1A3dii_Domestic-navigation" ) ) %>%
        dplyr::mutate( sector = "shipping_fuel" ) %>%
        dplyr::group_by( sector, fuel, units ) %>%
        dplyr::summarise_all( funs( sum( ., na.rm = TRUE ) ) ) %>%
        dplyr::ungroup( ) %>%
        tidyr::gather( year, CEDS_ship_fuel, X_extended_years ) %>%
        dplyr::mutate( year = gsub( "X", "", year ) ) %>%
        dplyr::mutate( year = as.numeric( year ) )

#   The exogenous shipping data includes fuel consumption for the "fishing" sector as well,
#   but fuel consumption for this sector is included within CEDS "1A4c_Agriculture-forestry-fishing".
#   Retrieve IEA fishing fuel data and add it to the CEDS shipping fuel estimate ( which at this point
#   in the script is only internatitonal shipping and domestic navigation). Here too brown_coal is
#   included within the hard_coal CEDS fuel.
    iea_fishing_data <- iea_data %>%
        dplyr::rename( product = PRODUCT ) %>%
        dplyr::left_join( IEA_product_fuel, by = "product" ) %>%
        dplyr::filter( !is.na( fuel ),
                        FLOW == "FISHING") %>%
        dplyr::mutate( fuel = if_else( fuel == "brown_coal", "hard_coal", fuel ) ) %>%
        dplyr::filter( fuel %in% c( "hard_coal", "heavy_oil", "diesel_oil" ) ) %>%
        dplyr::select( -product, -cdiac_fuel, -biofuel_flag, -iso ) %>%
        dplyr::group_by( FLOW, fuel ) %>%
        dplyr::summarise_all( funs( sum( ., na.rm = T ) ) ) %>%
        dplyr::ungroup( ) %>%
        dplyr::mutate( sector = "shipping_fuel",
                       units = "kt" ) %>%
        dplyr::select( sector, fuel, units, X_IEA_years ) %>%
        tidyr::gather( key = year, value = fishing_fuel_consumption, X_IEA_years ) %>%
        dplyr::mutate( year = gsub( "X", "", year ) ) %>%
        dplyr::mutate( year = as.numeric( year ) )

    CEDS_shipping_fuel_with_fishing <- CEDS_shipping_fuel %>%
        dplyr::left_join( iea_fishing_data, by = c( "sector", "fuel", "units", "year" ) ) %>%
        dplyr::mutate( fishing_fuel_consumption = if_else( is.na( fishing_fuel_consumption ),
                                                           0, fishing_fuel_consumption ) ) %>%
        dplyr::mutate( final_shipping_fuel = CEDS_ship_fuel + fishing_fuel_consumption ) %>%
        dplyr::select( -CEDS_ship_fuel, -fishing_fuel_consumption ) %>%
        dplyr::rename( CEDS_ship_fuel = final_shipping_fuel )


# -----------------------------------------------------------------------------------------
# 4. Determine amount of additional shipping fuel to be added
#
#    Add to global international shipping sector fuel consumption equal to the difference between
#    shipping_fuel variable and the total CEDS shipping fuel. Add only positive differences (exogenous > CEDS),
#    except for heavy_oil where the negative differnce ( exogenous < CEDS ) is taken from what would have
#    been added to diesel_oil
#
#    We are ignoring shipping data for coking coal, biomass, and natural gas.
#
#    Where total CEDS shipping is > shipping_fuel:
#
#      diesel_oil : (ignore, as this shouldn't happen)
#            TODO: During int. shipping ext PR - check to see if there are any negatives to add
#
#      heavy_oil : Account for this in diesel oil (e.g., add less diesel oil)
#
#      coal (CEDS hard_coal and brown_coal, both renamed to "hard_coal"): ignore (no adjustment since this doesn't occur)
#            TODO: During int. shipping ext PR - check to see if there are any negatives to add
#

#   Combine two dfs and take difference in shipping fuel ( difference = exogenous shipping estimate - CEDS shipping )
    combined_shipping <- shipping_fuel %>%
        dplyr::full_join( CEDS_shipping_fuel_with_fishing, by = c( "year", "fuel" ) ) %>%
        dplyr::arrange( fuel, year ) %>%
        dplyr::select( sector, year, fuel, units, ship_fuel, CEDS_ship_fuel ) %>%
        dplyr::mutate( difference = ship_fuel - CEDS_ship_fuel )

#   If CEDS is data has more recent years with non-NA values than the exogenous shipping speadsheet,
#   then compute the average difference for the 3 last years provided in the spreadsheet, by fuel.
#   Apply this difference to CEDS years which are more recent than the final spreadsheet year, by fuel.
#   TODO: During int. shipping ext PR -check if this was a good assumption

#       Define the last year of non-NA data exogenously provided, by fuel.
        last_exogenous_years <- shipping_fuel %>%
            dplyr::filter( !is.na( ship_fuel ) ) %>%
            dplyr::select( year, fuel ) %>%
            dplyr::group_by( fuel ) %>%
            dplyr::summarise_all( max ) %>%
            dplyr::rename( last_year = year ) %>%
            dplyr::ungroup( )

    if( any( last_exogenous_years$last_year < end_year ) ){

        exogenous_years_for_average <- last_exogenous_years %>%
            dplyr::filter( last_year < end_year ) %>%
            dplyr::mutate( first_year_for_average = last_year - 2 )

#       Compute average to be applied
        data_for_underreported_avg <- combined_shipping %>%
            dplyr::left_join( exogenous_years_for_average, by = "fuel" ) %>%
            dplyr::filter( year >= first_year_for_average & year <= last_year ) %>%
            dplyr::select( -first_year_for_average )

        if( any( is.na( data_for_underreported_avg$ship_fuel ) ) ){

            stop( "Some values are NA from exogenously specified shipping fuel consumption data. ",
                  "See Shipping_Fuel_Consumption.xlsx" )

        }

        underreported_avg <- data_for_underreported_avg %>%
            dplyr::select( -ship_fuel, -CEDS_ship_fuel, -year ) %>%
            dplyr::group_by( sector, fuel, units, last_year ) %>%
            dplyr::summarise_all( mean ) %>%
            dplyr::ungroup( ) %>%
            dplyr::rename( average_difference = difference )

#       Add average back to larger difference file
        combined_shipping_ext <- combined_shipping %>%
            dplyr::left_join( underreported_avg, by = c( "sector", "fuel", "units" ) ) %>%
            dplyr::mutate( difference = if_else( year > last_year & is.na( difference ),
                                                 average_difference, difference ) ) %>%
            dplyr::select( -last_year, -average_difference )

    } else{

        combined_shipping_ext <- combined_shipping

    }

#       Reformat diagnostic comparsion of CEDS default and exogenous shipping assumptions
        pre_shipping_adjustment_diagnostic <- combined_shipping_ext %>%
            dplyr::mutate( sector = "Total global shiping fuel consumption" ) %>%
            dplyr::rename( Shipping_Fuel_Consumption.xlsx = ship_fuel,
                           absolute_difference = difference ) %>%
            dplyr::filter( !(is.na( Shipping_Fuel_Consumption.xlsx ) ) ) %>%
            dplyr::mutate( relative_difference = absolute_difference / CEDS_ship_fuel )

#   Subset differences which are > 0, to be added to global international shipping
    int_shipping_fuel_to_add <- combined_shipping_ext %>%
        dplyr::filter( difference > 0 ) %>%
        dplyr::select( -ship_fuel, -CEDS_ship_fuel )

#   For heavy_oil, if CEDS shipping > exogenous shipping estimate, subtract the difference from what would have
#   been added for global international shipping diesel_oil
    int_shipping_diesel_to_subtract <- combined_shipping_ext %>%
        dplyr::filter( fuel == "heavy_oil",
                       difference < 0 ) %>%
        dplyr::mutate( fuel = "diesel_oil" ) %>%
        dplyr::rename( diff_to_subtract = difference ) %>%
        dplyr::select( -ship_fuel, -CEDS_ship_fuel )

#   Combine int_shipping_fuel_to_add with int_shipping_diesel_to_subtract -- final shipping data to be added to CEDS
    int_shipping_fuel_to_add_with_subtracted_fuel <- int_shipping_fuel_to_add %>%
        dplyr::full_join( int_shipping_diesel_to_subtract, by = c( "sector", "year", "fuel", "units" ) ) %>%
        dplyr::mutate_at( .vars = c( "difference", "diff_to_subtract" ), funs( if_else( is.na( . ), 0, . ) ) ) %>%
        dplyr::mutate( consumption_to_add = difference + diff_to_subtract ) %>%
        dplyr::select( -difference, - diff_to_subtract ) %>%
        dplyr::mutate( iso = "global",
                       sector = "1A3di_International-shipping",
                       year = paste0( "X", year ) )

#   Add adjustment in global shipping by fuel to the global iso in CEDS data
    CEDS_adjusted_int_shipping <- CEDS_clean %>%
        tidyr::gather( year, fuel_consumption, X_extended_years ) %>%
        dplyr::left_join( int_shipping_fuel_to_add_with_subtracted_fuel, by = c( "iso", "sector", "fuel", "units", "year" ) ) %>%
        dplyr::mutate( consumption_to_add = if_else( is.na( consumption_to_add ), 0, consumption_to_add ) ) %>%
        dplyr::mutate( adjusted_fuel_consumption = fuel_consumption + consumption_to_add ) %>%
        dplyr::select( -fuel_consumption, -consumption_to_add  ) %>%
        tidyr::spread( year, adjusted_fuel_consumption )

#   Check that nothing is negative
#   TODO: During int. shipping ext PR - - change this check to a stop
    if( any( CEDS_adjusted_int_shipping[ , X_extended_years ] < 0 ) ){

        warning( "Adjusted fuel consumption data now includes negative consumption ",
              "for at least one year and fuel combinations. Check A8.2.Adjust_Shipping_Fuel_Cons.R..." )

    }

# Produce diagnostic comparing new CEDS global shipping fuel sum to the exogenous spread sheet (by fuel)
# Note: CEDS brown_coal is renamed "hard_coal" for this check
    CEDS_shipping_data_for_check <- CEDS_adjusted_int_shipping %>%
        dplyr::filter( sector %in% c( "1A3di_International-shipping", "1A3dii_Domestic-navigation" ),
                       fuel %in% c( "brown_coal", "hard_coal", "heavy_oil", "diesel_oil" ) ) %>%
        dplyr::mutate( fuel = if_else( fuel == "brown_coal", "hard_coal", fuel ) ) %>%
        dplyr::mutate( iso = "global_sum" ) %>%
        dplyr::group_by( iso, sector, fuel, units ) %>%
        dplyr::summarise_all( funs( sum( ., na.rm = T ) ) ) %>%
        dplyr::ungroup( ) %>%
        tidyr::gather( key = year, value = CEDS_global_sum, X_extended_years ) %>%
        dplyr::mutate( year = gsub( "X", "", year ) ) %>%
        dplyr::mutate( year = as.numeric( year ) )

    exogenous_shipping_fuel_for_check <- shipping_fuel %>%
        dplyr::rename( exogenous_shipping_fuel = ship_fuel )

    pre_fouquet_and_pearson_ext_diagnostic_check <- CEDS_shipping_data_for_check %>%
        dplyr::left_join( exogenous_shipping_fuel_for_check, by = c( "year", "fuel" ) ) %>%
        dplyr::filter( !is.na( exogenous_shipping_fuel ) ) %>%
        dplyr::mutate( absolute_difference = CEDS_global_sum - exogenous_shipping_fuel ) %>%
        dplyr::mutate( relative_difference = absolute_difference / exogenous_shipping_fuel ) %>%
        dplyr::arrange( fuel, year ) %>%
        dplyr::filter( absolute_difference != 0 )

# TODO: During int. shipping ext PR - Make sure data is consistent for 1960 for all fuels (e.g., make sure oil fuel in ships falls
#       going back in time as indicated in Shipping_Fuel_Consumption.xlsx)
#       Check this after rerunning system

# -----------------------------------------------------------------------------
# 5. Extrapolate pre-1855 global shipping coal using extrapolation data from
#    Fouquet & Pearson (1998).

#   Reformat and extend extrap values
    global_coal <- shipping_coal_extrap %>%
        dplyr::select( year = Year, global = Total_Ship_Extrap ) %>%
        dplyr::mutate( iso = "global" ) %>%
        dplyr::rename( consumption = global ) %>%
        dplyr::select( year, iso, consumption )

    british_coal <- shipping_coal_extrap %>%
        dplyr::select( year = Year, gbr = British_Shipping_Coal ) %>%
        dplyr::mutate( iso = "gbr" ) %>%
        dplyr::rename( consumption = gbr ) %>%
        dplyr::select( year, iso, consumption )

    coal_extrap <- dplyr::bind_rows( global_coal, british_coal ) %>%
        dplyr::mutate( sector = "1A3di_International-shipping",
                       fuel = "hard_coal",
                       units = "kt",
                       year = paste0( "X", year ) ) %>%
        tidyr::spread( year, consumption ) %>%
        dplyr::mutate( X1750 = 0 ) %>%
        interpolateValues( ) %>%
        dplyr::arrange( desc( iso ) )

#   Subset CEDS updated global international shipping data
    CEDS_global_intl_ship_wide <- CEDS_adjusted_int_shipping %>%
        dplyr::filter( iso == "global",
                       sector == "1A3di_International-shipping",
                       fuel %in% c( "hard_coal", "heavy_oil", "diesel_oil" ) ) %>%
        dplyr::select( fuel, sector, iso, units, X_extended_years )

#   Add extended coal data (coal_extrap) to CEDS_global_intl_ship_wide where it's values a larger
#   than what already exists within CEDS at this point
    ship_out <- bind_rows( coal_extrap, CEDS_global_intl_ship_wide ) %>%
        dplyr::mutate_at( .vars = X_extended_years, funs( if_else( is.na( . ), 0, . ) ) ) %>%
        dplyr::select( iso, sector, fuel, units, X_extended_years ) %>%
        dplyr::group_by( iso, sector, fuel, units ) %>%
        dplyr::summarise_all( max ) %>%
        dplyr::ungroup( )

    final_global_shipping <- ship_out %>%
        dplyr::filter( iso == "global" )

    else_shipping <- ship_out %>%
        dplyr::filter( iso != "global" )

# Replace global shipping values in CEDS_adjusted_int_shipping with values from final_global_shipping
# TODO: During int. shipping ext PR - check that this worked correctly (since the test input to this script already had this occur based on early structure)
    CEDS_final_shipping_activity <- CEDS_adjusted_int_shipping %>%
        dplyr::filter( !( fuel %in% c( "hard_coal", "heavy_oil", "diesel_oil" ) &
                          iso == "global" & sector == "1A3di_International-shipping" ) ) %>%
        dplyr::bind_rows( final_global_shipping ) %>%
        dplyr::arrange( iso, sector, fuel )

# TODO: During int. shipping ext PR  - check if need to add global shipping data as 0 consumption for other
#       CEDS fuels that don't exist based on how set their values originally in A6.4 (I believe it is this script)

# -----------------------------------------------------------------------------
# 6. Output

#   Output diagnostic comparison of exogenous shipping data and CEDS global sum, pre-adjustment to CEDs data
    writeData( pre_shipping_adjustment_diagnostic, "DIAG_OUT", "A.pre_adj_shipping_difference" )

#   Output diagnostic comparison of exogenous shipping data and CEDS global sum, post-adjustment to CEDs data (but before fouquet and pearson extension)
    writeData( pre_fouquet_and_pearson_ext_diagnostic_check,
               "DIAG_OUT", "A.post_adj_shipping_discrepancy" )

#   Output shipping data for non-global isos which were not updated
    writeData( else_shipping, "DIAG_OUT", "A.intl_shipping_en_country_break-out" )

#   Output CEDS activity data with shipping adjustments
    writeData( CEDS_final_shipping_activity, "MED_OUT", "A.comb_int_shipping_adjusted" )

# Every script should finish with this line
    logStop( )

# END

