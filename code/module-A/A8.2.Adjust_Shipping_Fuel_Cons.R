#------------------------------------------------------------------------------
# Program Name: A8.2.Adjust_Shipping_Fuel_Cons.R
# Authors Names: Steve Smith, Patrick O'Rourke, Linh Vu
# Date Last Updated: December 13, 2019
# Program Purpose: Reads in exogenous time series for global shipping fuel consumption
#                  ( domestic and international ).
#
#                  Adds any positive differences in shipping ( exogenous > CEDS ) to CEDS,
#                  and adds negative differences in shipping ( CEDS > exogenous ) for heavy_oil
#                  and diesel_oil in the modern years where appropriate (see section 4)
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
# Input: A.comb_user_added.csv, A.IEA_en_stat_ctry_hist.csv, IEA_product_fuel.csv,
#        Shipping_Fuel_Consumption.xlsx
# Output: A.ship-diff_pre_adjustments.csv, A.ship-diff_pre_fouquet_ext.csv,
#         A.ship-diff_post_adjustments.csv, CEDS_Global_Shipping_Fuel_Consumption-comparison.pdf,
#         A.intl_shipping_en_country_break-out.csv, A.comb_int_shipping_adjusted.csv
# TODO:
#-------------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if( "input" %in% dir( ) ) "code/parameters/" else "../code/parameters/"

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

    printLog( "Calculating CEDS global shipping fuel consumption data. Fishing fuel consumption",
              "is only accounted for from", IEA_start_year, "to", IEA_end_year, ", as",
              "these are the years data is available from IEA. CEDS includes fishing fuel consumption",
              "within the sector '1A4c_Agriculture-forestry-fishing', and thus extended CEDS fishing fuel consumption",
              "cannot be separated from this aggregate sector or accounted for here..." )

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
#    Add the difference between the exogenous shipping data (shipping_fuel) and CEDS total shipping fuel to
#    the "global" iso's international shipping sector. Add only positive differences (exogenous > CEDS),
#    except for heavy_oil and light_oil, as the negative differnces ( exogenous < CEDS ) are accounted for
#    in total petroleum consumption by ships for these CEDS fuels in modern years (see below for more details).
#
#    We are ignoring shipping data for coking coal, biomass, and natural gas.
#
#    Where total CEDS shipping is > shipping_fuel (negative differences) exist:
#
#      diesel_oil and heavy_oil
#           Modern Years (start_year [1960] - end_year [2014] ):
#               Negative differences for diesel_oil or heavy_oil will be accounted
#               for in the other liquid fuel (i.e. a negative difference for
#               diesel_oil is added to what would have been added to heavy_oil in the same year, and
#               vice-a-versa).
#
#               Given that values for the "global" iso's int. shipping
#               are currently 0 in CEDS, unless specified in the user-energy routine as otherwise, if
#                   (1) a negative difference from one liquid fuel and
#                   (2) the difference for the other liquid fuel
#               sum to a negative amount, then negative activity data will be generated (i.e. if a difference of -5kt is found for
#               diesel_oil and added to a difference of 4kt for heavy_oil, the revised difference for heavy_oil will
#               be negative, which subtracted from 0 would lead to negative activity data).
#
#               Given this, in situations where negative activity is generated from this process, the activity value is reset to 0,
#               which allows CEDs to account for at least some of the negative difference with the exogenous data
#               without having negative activity.
#
#               The code is written so that if the "global" iso comes into the script
#               with non-zero data for int. shipping, then the routine should still work, and can still account for at least some negative
#               differences as well.
#
#           Extended Years (1750-1959) - CEDS values are retained if a negative difference exists.
#
#      coal (CEDS hard_coal and brown_coal, both renamed to "hard_coal"):
#           We do not make adjustments for negative differences in shipping coal consumption.

    printLog( "Adjusting CEDS 'global' iso's international shipping fuel consumption using Shipping_Fuel_Consumption.xlsx..." )

#   Combine two dfs and take difference in shipping fuel ( difference = exogenous shipping estimate - CEDS shipping )
    combined_shipping <- shipping_fuel %>%
        dplyr::full_join( CEDS_shipping_fuel_with_fishing, by = c( "year", "fuel" ) ) %>%
        dplyr::arrange( fuel, year ) %>%
        dplyr::select( sector, year, fuel, units, ship_fuel, CEDS_ship_fuel ) %>%
        dplyr::mutate( difference = ship_fuel - CEDS_ship_fuel )

#   If CEDS is data has more recent years with non-NA values than the exogenous shipping speadsheet,
#   then compute the average difference for the 3 last years provided in the spreadsheet, by fuel.
#   Apply this difference to CEDS years which are more recent than the final spreadsheet year, by fuel.

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

# Account for differences, if they exist
if( nrow( combined_shipping_ext %>% dplyr::filter( difference != 0, !is.na( difference ) ) ) > 0 ){

#   Subset differences which are > 0, to be added to the "global" iso's international shipping
    int_shipping_fuel_to_add <- combined_shipping_ext %>%
        dplyr::filter( difference > 0 ) %>%
        dplyr::select( -ship_fuel, -CEDS_ship_fuel )

#   Subset heavy_oil negative differences for modern years
#   (from start_year [1960] to end_year [2014], to add to diesel_oil positive values
    int_shipping_dieseloil_to_subtract <- combined_shipping_ext %>%
        dplyr::filter( fuel == "heavy_oil",
                       difference < 0,
                       year >= start_year ) %>%
        dplyr::mutate( fuel = "diesel_oil" ) %>%
        dplyr::rename( diff_to_subtract = difference ) %>%
        dplyr::select( -ship_fuel, -CEDS_ship_fuel )

#   Subset diesel_oil negative differences for modern years
#   (from start_year [1960] to end_year [2014], to add to heavy_oil positive values
    int_shipping_heavyoil_to_subtract <- combined_shipping_ext %>%
        dplyr::filter( fuel == "diesel_oil",
                       difference < 0,
                       year >= start_year ) %>%
        dplyr::mutate( fuel = "heavy_oil" ) %>%
        dplyr::rename( diff_to_subtract = difference ) %>%
        dplyr::select( -ship_fuel, -CEDS_ship_fuel )

#   Combine the negative differences
    int_shipping_to_subtract <- int_shipping_dieseloil_to_subtract %>%
        dplyr::bind_rows( int_shipping_heavyoil_to_subtract )

#   Final shipping data to be added to CEDS: Combine the postive and negative differences, if they both exist
    if( nrow( int_shipping_to_subtract ) > 0 & nrow( int_shipping_fuel_to_add ) > 0 ){

        int_shipping_fuel_to_add_with_subtracted_fuel <- int_shipping_fuel_to_add %>%
            dplyr::full_join( int_shipping_to_subtract, by = c( "sector", "year", "fuel", "units" ) ) %>%
            dplyr::mutate_at( .vars = c( "difference", "diff_to_subtract" ), funs( if_else( is.na( . ), 0, . ) ) ) %>%
            dplyr::mutate( consumption_to_add = difference + diff_to_subtract ) %>%
            dplyr::select( -difference, - diff_to_subtract ) %>%
            dplyr::mutate( iso = "global",
                           sector = "1A3di_International-shipping",
                           year = paste0( "X", year ) )

    } else if( nrow( int_shipping_to_subtract ) == 0 & nrow( int_shipping_fuel_to_add ) > 0 ){

        int_shipping_fuel_to_add_with_subtracted_fuel <- int_shipping_fuel_to_add %>%
            dplyr::rename( consumption_to_add = difference ) %>%
            dplyr::mutate( iso = "global",
                           sector = "1A3di_International-shipping",
                           year = paste0( "X", year ) )



    } else if( nrow( int_shipping_to_subtract ) > 0 & nrow( int_shipping_fuel_to_add ) == 0  ){

        int_shipping_fuel_to_add_with_subtracted_fuel <- int_shipping_to_subtract %>%
            dplyr::rename( consumption_to_add = diff_to_subtract ) %>%
            dplyr::mutate( iso = "global",
                           sector = "1A3di_International-shipping",
                           year = paste0( "X", year ) )

    }

#   Add adjustment in global shipping by fuel to the "global" iso in CEDS data. The differences from
#   above are added to the current values for the "global" iso's int. shipping sector. If adding the
#   differences leads to a value less than 0, then the value is set to 0.
    CEDS_clean_wo_glb_int_ship <- CEDS_clean %>%
        dplyr::filter( !( iso == "global" & sector == "1A3di_International-shipping" ) )

    CEDS_new_glb_int_shipping <- CEDS_clean %>%
        dplyr::filter( iso == "global" & sector == "1A3di_International-shipping" ) %>%
        tidyr::gather( year, fuel_consumption, X_extended_years ) %>%
        dplyr::left_join( int_shipping_fuel_to_add_with_subtracted_fuel, by = c( "iso", "sector", "fuel", "units", "year" ) ) %>%
        dplyr::mutate( consumption_to_add = if_else( is.na( consumption_to_add ), 0, consumption_to_add ) ) %>%
        dplyr::mutate( adjusted_fuel_consumption = fuel_consumption + consumption_to_add ) %>%
        dplyr::mutate( adjusted_fuel_consumption = if_else( adjusted_fuel_consumption < 0, 0,
                                                            adjusted_fuel_consumption ) ) %>%
#       Note: The above line is setting negative global int. shipping data to zero. This implies that
#             the negative difference lead to negative activity data. CEDS cannot have negative activity data.
#             Thus CEDS incorporates the negative difference to the point
#             where the activity becomes 0.
        dplyr::select( -fuel_consumption, -consumption_to_add  ) %>%
        tidyr::spread( year, adjusted_fuel_consumption )

    CEDS_adjusted_int_shipping <- CEDS_clean_wo_glb_int_ship %>%
        dplyr::bind_rows( CEDS_new_glb_int_shipping ) %>%
        dplyr::arrange( iso, sector, fuel )

} else{

    CEDS_adjusted_int_shipping <- CEDS_clean # Set adjusted data to the original cleaned CEDS data if there are no differences
                                             # between CEDS and exogenous spreadsheet

}

#   Check that new CEDS data has the same number of rows and columns as the original CEDS data
    if( ( nrow( CEDS_adjusted_int_shipping ) != nrow( CEDS_clean ) ) |
        ( length( CEDS_adjusted_int_shipping ) != length( CEDS_clean ) ) ){

        stop( "CEDS data with adjusted global int. shipping fuel consumption does not have the same number of rows or columns ",
              "as it did preadjustment. Please check A8.2.Adjust_Shipping_Fuel_Cons.R..." )

    }

#   Check that nothing is negative
    if( any( CEDS_adjusted_int_shipping[ , X_extended_years ] < 0 ) ){

        stop( "Adjusted fuel consumption data now includes negative consumption ",
              "for at least one year and fuel combinations. Please check A8.2.Adjust_Shipping_Fuel_Cons.R..." )

    }

# Produce diagnostic comparing new CEDS global shipping fuel sum to the exogenous spreadsheet (by fuel)
# Note: CEDS brown_coal is renamed "hard_coal" for this check
    CEDS_shipping_data_for_check <- CEDS_adjusted_int_shipping %>%
        dplyr::filter( sector %in% c( "1A3di_International-shipping", "1A3dii_Domestic-navigation" ),
                       fuel %in% c( "brown_coal", "hard_coal", "heavy_oil", "diesel_oil" ) ) %>%
        dplyr::mutate( fuel = if_else( fuel == "brown_coal", "hard_coal", fuel ) ) %>%
        dplyr::mutate( iso = "global_sum",
                       sector = "shipping_fuel" ) %>%
        dplyr::group_by( iso, sector, fuel, units ) %>%
        dplyr::summarise_all( funs( sum( ., na.rm = T ) ) ) %>%
        dplyr::ungroup( ) %>%
        tidyr::gather( key = year, value = CEDS_global_sum, X_extended_years ) %>%
        dplyr::mutate( year = gsub( "X", "", year ) ) %>%
        dplyr::mutate( year = as.numeric( year ) )

    iea_fishing_data_for_check <- iea_fishing_data %>%
        dplyr::mutate( sector = "shipping_fuel" )

    CEDS_shipping_with_fishing_for_check <- CEDS_shipping_data_for_check %>%
        dplyr::left_join( iea_fishing_data_for_check, by = c( "sector", "fuel", "units", "year" ) ) %>%
        dplyr::mutate( fishing_fuel_consumption = if_else( is.na( fishing_fuel_consumption ),
                                                           0, fishing_fuel_consumption ) ) %>%
        dplyr::mutate( final_shipping_fuel = CEDS_global_sum + fishing_fuel_consumption ) %>%
        dplyr::select( -CEDS_global_sum, -fishing_fuel_consumption ) %>%
        dplyr::rename( CEDS_ship_fuel = final_shipping_fuel )

    exogenous_shipping_fuel_for_check <- shipping_fuel %>%
        dplyr::rename( exogenous_shipping_fuel = ship_fuel )

    pre_fouquet_and_pearson_ext_diagnostic_check <- CEDS_shipping_with_fishing_for_check %>%
        dplyr::left_join( exogenous_shipping_fuel_for_check, by = c( "year", "fuel" ) ) %>%
        dplyr::filter( !is.na( exogenous_shipping_fuel ) ) %>%
        dplyr::mutate( absolute_difference = exogenous_shipping_fuel - CEDS_ship_fuel ) %>%
        dplyr::mutate( relative_difference = absolute_difference / CEDS_ship_fuel ) %>%
        dplyr::arrange( fuel, year ) %>%
        dplyr::filter( absolute_difference != 0 )

# -----------------------------------------------------------------------------
# 5. Extrapolate pre-1855 global shipping coal using extrapolation data from
#    Fouquet & Pearson (1998).
#
#   Sum CEDS global shipping data (int. shipping and domestic navigation).
#   For any fuel & year combination where Fouquet & Pearson estimate higher consumption
#   than CEDS' global sum at this point, then add the positive difference to CEDS
#   "global" iso's int. shipping estimate. If CEDS > Fouquet and Pearson, then retain CEDS data

    printLog( "Adjusting 'global' iso's international shipping fuel consumption from 1855 ",
              "backwards using data from Fouquet & Pearson (1998). Note that this adjustment does ",
              "not account for CEDS fishing fuel consumption data, as CEDS extends fishing fuel consumption ",
              "within the aggregate sector '1A4c_Agriculture-forestry-fishing', ",
              "and thus extended CEDS fishing fuel consumption cannot be separated from ",
              "this aggregate sector or accounted for here..." )

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

#   Subset and aggregate CEDS updated shipping data
#   Note that the exogenous data being used here for extrapolation of coal shipping data
#   only goes until 1855, thus we do not need to worry about adding in IEA fishing fuel consumption.
    CEDS_global_shipping <- CEDS_adjusted_int_shipping %>%
        dplyr::filter( sector %in% c( "1A3di_International-shipping", "1A3dii_Domestic-navigation" ),
                       fuel %in% c( "brown_coal", "hard_coal", "heavy_oil", "diesel_oil" ) ) %>%
        dplyr::mutate( fuel = if_else( fuel == "brown_coal", "hard_coal", fuel ) ) %>%
        dplyr::select( iso, sector, fuel, units, X_extended_years ) %>%
        dplyr::mutate( iso = "global_sum", sector = "shipping_fuel" ) %>%
        dplyr::group_by( iso, sector, fuel, units ) %>%
        dplyr::summarise_all( funs( sum( . , na.rm = T ) ) ) %>%
        dplyr::ungroup( ) %>%
        tidyr::gather( key = year, value = CEDS_ship_fuel, X_extended_years )

#   Comput difference between exogenous coal extrapolation data and CEDS global shipping fuel consumption.
#   Retain only the positive differences to be added to the "global" iso's int. shipping data.
    global_coal_extrap <- coal_extrap %>%
        dplyr::filter( iso == "global" ) %>%
        tidyr::gather( key = year, value = Fouquet_Pearson_ship_fuel , paste0( "X", historical_pre_extension_year : 1855 ) ) %>%
        dplyr::mutate( sector = "shipping_fuel",
                       iso = "global_sum" )

    extrap_shipping_to_add <- CEDS_global_shipping %>%
        dplyr::left_join( global_coal_extrap, by = c( "iso", "sector", "fuel", "units", "year" ) ) %>%
        dplyr::mutate( Fouquet_Pearson_ship_fuel = if_else( is.na( Fouquet_Pearson_ship_fuel ),
                                                            0, Fouquet_Pearson_ship_fuel ) ) %>%
        dplyr::mutate( diff_to_add = Fouquet_Pearson_ship_fuel - CEDS_ship_fuel ) %>%
        dplyr::filter( diff_to_add > 0 ) %>%
        dplyr::select( -Fouquet_Pearson_ship_fuel, -CEDS_ship_fuel ) %>%
        dplyr::mutate( iso = "global", sector = "1A3di_International-shipping" )

#   Add the positive difference to the "global" iso's int. shipping data
    CEDS_final_glb_int_shipping <- CEDS_adjusted_int_shipping %>%
        dplyr::filter( sector == "1A3di_International-shipping",
                       iso == "global" ) %>%
        tidyr::gather( key = year, value = CEDS_glb_int_ship_fuel, X_extended_years ) %>%
        dplyr::left_join( extrap_shipping_to_add, by = c( "iso", "sector", "fuel", "units", "year" ) ) %>%
        dplyr::mutate( diff_to_add = if_else( is.na( diff_to_add ), 0, diff_to_add ) ) %>%
        dplyr::mutate( final_glb_int_shipping_fuel = CEDS_glb_int_ship_fuel + diff_to_add ) %>%
        dplyr::select( -CEDS_glb_int_ship_fuel, -diff_to_add ) %>%
        tidyr::spread( year, final_glb_int_shipping_fuel )

#   Combine the df of new global international shipping data with the rest of CEDS data
    CEDS_final_shipping_activity <- CEDS_adjusted_int_shipping %>%
        dplyr::filter( !( iso == "global" & sector == "1A3di_International-shipping" ) ) %>%
        dplyr::bind_rows( CEDS_final_glb_int_shipping ) %>%
        dplyr::arrange( iso, sector, fuel )

#   Check that new CEDS data has the same number of rows and columns as the original CEDS data
    if( ( nrow( CEDS_final_shipping_activity ) != nrow( CEDS_adjusted_int_shipping ) ) |
        ( length( CEDS_final_shipping_activity ) != length( CEDS_adjusted_int_shipping ) ) ){

        stop( "CEDS data with adjusted global int. shipping fuel consumption does not have the same number of rows or columns ",
              "as it did pre-Fouquet & Pearson adjustment. Please check A8.2.Adjust_Shipping_Fuel_Cons.R..." )

    }

#   Check that nothing is negative
    if( any( CEDS_final_shipping_activity[ , X_extended_years ] < 0 ) ){

        stop( "Adjusted fuel consumption data now includes negative consumption ",
              "for at least one year and fuel combinations after the Fouquet & Pearson adjustment. ",
              "Please check A8.2.Adjust_Shipping_Fuel_Cons.R..." )

    }

#   Subset Fouquet and Pearson shipping fuel consumption for specific isos (to be a diagnostic output)
    else_shipping <- coal_extrap %>%
        dplyr::filter( iso != "global" )

# -----------------------------------------------------------------------------
# 6. Create final diagnostic comparisons of new CEDS vs. both exogenous sets of data

#   Combine both exogenous shipping estimates, and aggregate by aggregate fuel
    global_coal_extrap_for_diag <- global_coal_extrap %>%
        dplyr::rename( exogenous_shipping_data = Fouquet_Pearson_ship_fuel )

    max_fouquet_pearson_year <- max( global_coal_extrap_for_diag$year )
    fouquet_pearson_fuels <- unique( global_coal_extrap_for_diag$fuel )

    shipping_fuel_for_diag <- shipping_fuel %>%
        dplyr::mutate( iso = "global_sum",
                       sector = "shipping_fuel",
                       exogenous_shipping_data = ship_fuel,
                       units = "kt",
                       year = paste0( "X", year ) ) %>%
        dplyr::select( iso, sector, fuel, units, year, exogenous_shipping_data ) %>%
        dplyr::filter( !( year <= max_fouquet_pearson_year & fuel %in% fouquet_pearson_fuels ) )
        # This filter is done as years less than or equal to max_fouquet_pearson_year are taken
        # from fouquet & pearson for fuels in fouquet_pearson_fuels. Therefore we remove to avoid
        # duplicating info when binding these two dfs.

    exogenous_shipping_data <- dplyr::bind_rows( global_coal_extrap_for_diag, shipping_fuel_for_diag ) %>%
        dplyr::arrange( iso, sector, fuel, year ) %>%
        dplyr::mutate( fuel = if_else( fuel %in% c( "heavy_oil", "diesel_oil" ), "diesel_and_heavy_oil", fuel ) ) %>%
        dplyr::mutate( fuel = if_else( fuel == "hard_coal", "brown_and_hard_coal", fuel ) ) %>%
        dplyr::group_by( iso, sector, fuel, units, year ) %>%
        dplyr::summarise_all( funs( sum( ., na.rm = T ) ) ) %>%
        dplyr::ungroup( ) %>%
        dplyr::mutate( year = gsub( "X", "", year ),
                       source = "Exogenous_Shipping_Estimate" ) %>%
        dplyr::select( source, iso, sector, fuel, units, year, exogenous_shipping_data ) %>%
        dplyr::rename( ship_fuel = exogenous_shipping_data )

# Aggregate CEDS shipping data (int. shipping, domestic navigation, and fishing in modern years)
# to aggregate shipping and aggregate fuel
    iea_fishing_data_for_check <- iea_fishing_data %>%
        dplyr::mutate( iso = "global_sum",
                       sector = "shipping_fuel",
                       fuel = if_else( fuel == "hard_coal", "brown_and_hard_coal", fuel ),
                       year = paste0( "X", year ) ) %>%
        dplyr::mutate( fuel = if_else( fuel %in% c( "heavy_oil", "diesel_oil" ), "diesel_and_heavy_oil", fuel ) ) %>%
        dplyr::group_by( iso, sector, fuel, units, year ) %>%
        dplyr::summarise_all( funs( sum( ., na.rm = T ) ) ) %>%
        dplyr::ungroup( )

    CEDS_shipping_data_for_check <- CEDS_final_shipping_activity %>%
        dplyr::filter( sector %in% c( "1A3di_International-shipping", "1A3dii_Domestic-navigation" ),
                       fuel %in% c( "brown_coal", "hard_coal", "heavy_oil", "diesel_oil" ) ) %>%
        dplyr::mutate( fuel = if_else( fuel %in% c( "brown_coal", "hard_coal" ), "brown_and_hard_coal", fuel ) ) %>%
        dplyr::mutate( fuel = if_else( fuel %in% c( "heavy_oil", "diesel_oil" ), "diesel_and_heavy_oil", fuel ) ) %>%
        dplyr::mutate( iso = "global_sum",
                       sector = "shipping_fuel" ) %>%
        dplyr::group_by( iso, sector, fuel, units ) %>%
        dplyr::summarise_all( funs( sum( ., na.rm = T ) ) ) %>%
        dplyr::ungroup( ) %>%
        tidyr::gather( key = year, value = CEDS_global_sum, X_extended_years )

    CEDS_shipping_with_fishing_for_check <- CEDS_shipping_data_for_check %>%
        dplyr::left_join( iea_fishing_data_for_check, by = c( "iso", "sector", "fuel", "units", "year" ) ) %>%
        dplyr::mutate( fishing_fuel_consumption = if_else( is.na( fishing_fuel_consumption ),
                                                           0, fishing_fuel_consumption ) ) %>%
        dplyr::mutate( final_shipping_fuel = CEDS_global_sum + fishing_fuel_consumption,
                       source = "New_CEDS_Shipping_Estimate",
                       year = gsub( "X", "", year ) ) %>%
        dplyr::select( source, iso, sector, fuel, units, year, final_shipping_fuel ) %>%
        dplyr::rename( ship_fuel = final_shipping_fuel )

# Graph comparison - convert to Megatonnes (Mt)
     shipping_comparison_data <- dplyr::bind_rows( exogenous_shipping_data, CEDS_shipping_with_fishing_for_check ) %>%
         dplyr::mutate( year = as.numeric( year ) ) %>%
         dplyr::mutate( ship_fuel = ship_fuel / 1000,
                        units = "Mt" )

     max <- max( shipping_comparison_data$ship_fuel ) * 1.1
     graph_start_year <- 1825

     plot <- ggplot( shipping_comparison_data, aes( x = year, y = ship_fuel, color = fuel,
                                                    shape = source, linetype = source ) ) +
         geom_line( data = dplyr::filter( shipping_comparison_data, source == "New_CEDS_Shipping_Estimate" ),
                    size = 0.5, aes( x = year, y = ship_fuel, color = fuel ), alpha = 1 ) +
         geom_line( data = dplyr::filter( shipping_comparison_data, source == "Exogenous_Shipping_Estimate" ),
                    size = 2.5, aes( x = year, y = ship_fuel, color = fuel ), alpha = .25 ) +
         scale_x_continuous( breaks = seq( from = historical_pre_extension_year, to = end_year, by = 50 ) ) +
         ggtitle( "Global Shipping Fuel Consumption" ) +
         labs( x = "" , y = '[Mt/yr]' ) +
         theme( panel.background = element_blank( ),
                panel.grid.minor = element_line( colour="gray95" ),
                panel.grid.major = element_line( colour="gray88" ) ) +
         # scale_y_continuous( limits = c( 0, max ), labels = comma ) +  ----> the labels = comma argument requires the package "scales", which is not a standard CEDS package
         scale_y_continuous( limits = c( 0, max ) )+
         scale_color_discrete( name = 'Fuel')+
         scale_shape_manual( name = 'Source',
                             values = c( 46, 19 ) ) +
         scale_linetype_manual( name= 'Source',
                                values = c( 'solid','solid' ) ) +
         guides( linetype = guide_legend( override.aes = list( size = c( 1.5, 0.5 ) ) ) )

     ggsave( '../diagnostic-output/ceds-comparisons/sector-level/CEDS_Global_Shipping_Fuel_Consumption-comparison.pdf',
             width = 14, height = 7 )


#    Create a table with the comparison between exogenous and new CEDS
     shipping_comparison_data_final <- shipping_comparison_data %>%
         dplyr::mutate( ship_fuel = ship_fuel * 1000,
                        units = "kt" ) %>%
         tidyr::spread( source, ship_fuel ) %>%
         dplyr::mutate( absolute_difference = Exogenous_Shipping_Estimate - New_CEDS_Shipping_Estimate ) %>%
         dplyr::mutate( relative_difference = absolute_difference / New_CEDS_Shipping_Estimate )

# -----------------------------------------------------------------------------
# 7. Output data

#   Output diagnostic comparison of exogenous shipping data and CEDS global sum,
#   pre-adjustment to CEDs data
    writeData( pre_shipping_adjustment_diagnostic, "DIAG_OUT", "A.ship-diff_pre_adjustments",
               domain_extension = 'ceds-comparisons/sector-level/' )

#   Output diagnostic comparison of exogenous shipping data and CEDS global sum,
#   post-adjustment to CEDs data, but before fouquet and pearson extension
    writeData( pre_fouquet_and_pearson_ext_diagnostic_check,
               "DIAG_OUT", "A.ship-diff_pre_fouquet_ext" ,
               domain_extension = 'ceds-comparisons/sector-level/' )

#   Output diagnostic comparison of exogenous shipping data vs. new CEDS global shipping sum
    writeData( shipping_comparison_data_final,
               "DIAG_OUT", "A.ship-diff_post_adjustments" ,
               domain_extension = 'ceds-comparisons/sector-level/' )

#   Output shipping data for non-global isos which were not updated but data was available for use
    writeData( else_shipping, "DIAG_OUT", "A.intl_shipping_en_country_break-out" )

#   Output CEDS activity data with shipping adjustments
    writeData( CEDS_final_shipping_activity, "MED_OUT", "A.comb_int_shipping_adjusted" )

# Every script should finish with this line
    logStop( )

# END

