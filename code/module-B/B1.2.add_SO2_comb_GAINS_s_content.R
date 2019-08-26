# Program Name: B1.2.add_SO2_GAINS_s_content.R
# Author:  Rachel Hoesly, Patrick O'Rourke
# Date Last Updated: April 11, 2019
# Program Purpose: Add GAINS sulfur content data into defualt sulfur content database
#                  for the EU nations (2005 data) and India (2010 data applied to
#                  all CEDS years)
# Input Files: GAINS_country_mapping.csv, GAINS_fuel_mapping.csv, GAINS_sector_mapping.csv,
#              Master_Sector_Level_map.csv, GAINS_scont@SO2-EU28.csv, GAINS_scont@SO2-IND.csv,
#              GAINS_aen_act_sect-nohead-EU28.csv, GAINS_aen_act_sect-nohead-IND.csv
# Output Files: B.SO2_GAINS_s_content.csv
# Notes:
# TODO: (1) For India data: account for coal imports over time and different levels
#       of consumption of subfuels (GAINS fuels) which map to 1 CEDS fuel over time
#       (2) When India GAINS s_content, energy data, or this script is updated, one should check
#        U.SO2_RAINS_Asia_s_content.csv as this file should include only (and all) sectors
#        not found in this script's output (B.SO2_GAINS_s_content.csv). This is because
#        updating those data or this processing script could result in different GAINS sectors in
#        B.SO2_GAINS_s_content.csv, which  would change what is required in
#        U.SO2_RAINS_Asia_s_content.csv. One should also modify
#        U.SO2_India_LightAndDieselOils_s_content.csv as the value for light_oil
#        road in B.SO2_GAINS_s_content.csv is applied to 2004 in
#        U.SO2_India_LightAndDieselOils_s_content.csv for all light_oil sectors
#        (for backwards extension purposes), and this value could likewise change.

# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'timeframe_functions.R', "data_functions.R", "analysis_functions.R",
                  "process_db_functions.R", 'interpolation_extension_functions.R' ) # Additional function files may be required.
    log_msg <- "Processing GAINS S content data..." # First message to be printed to the log
    script_name <- "B1.2.add_SO2_GAINS_s_content.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------

# 1. Reading data and mapppings into script

# Read in GAINS sulfur content
    gains_s_content_EU  <- readData( "EM_INV",  "GAINS_scont@SO2-EU28" )
    gains_s_content_IND  <- readData( "EM_INV",  "GAINS_scont@SO2-IND" )

# Read in GAINS energy data
    gainsenergy_EU <- readData( "EM_INV", "GAINS_aen_act_sect-nohead-EU28" )
    gainsenergy_IND <- readData( "EM_INV", "GAINS_aen_act_sect-nohead-IND" )

# Read in GAINS mapping files
    gainstoiso <- readData( "GAINS_MAPPINGS",  "GAINS_country_mapping" )
    fuelmap <- readData(  "GAINS_MAPPINGS", "GAINS_fuel_mapping" )
    sectormap <- readData( "GAINS_MAPPINGS",  "GAINS_sector_mapping" )
    MSL <- readData( "MAPPINGS", "Master_Sector_Level_map")

# ---------------------------------------------------------------------------

# 2. Define functions for GAINS s content data processing

#   A.) Define function for initial GAINS scont data processing
    initial_processing_GAINS_scont <- function( df_in, fuel_mapping, country_mapping,
                                                sector_mapping ){

#       Initial cleaning of data
        names( df_in ) <- df_in[ 6, ]

#           Note: This next line assumes that variable columns start at col. number 4
        GAINS_variable_cols <- colnames( df_in[ , 4:length( colnames ( df_in ) ) ] )

        df_clean <- df_in %>%
            dplyr::slice( -( 1:7 ) ) %>%
            dplyr::mutate_at( GAINS_variable_cols, funs( as.numeric ) ) %>%
            tidyr::gather( key = GAINS.sectors, value = s_content, GAINS_variable_cols ) %>%
            dplyr::mutate( s_content_fraction = s_content / 100 ) %>%
            dplyr::select( -s_content )

        colnames( df_clean ) <- c( "country", "reg_abb", "GAINS.fuel", "GAINS.sectors",
                                   "s_content_fraction" )

#       Map to CEDS isos, fuels, and sectors (leave GAINS fuels and sectors in data for
#       creating weights with energy consumption data)
        df_mapped <- df_clean %>%
            dplyr::left_join( country_mapping, by = "country" ) %>%
            dplyr::left_join( fuel_mapping, by = "GAINS.fuel" ) %>%
            dplyr::left_join( sector_mapping, by = "GAINS.sectors" ) %>%
            dplyr::mutate( ISO.code = tolower( ISO.code ) ) %>%
            dplyr::select( ISO.code, reg_abb, GAINS.fuel, fuel, GAINS.sectors, working_sectors_v1,
                           s_content_fraction ) %>%
            dplyr::filter( ! ( is.na ( ISO.code) ),
                           ! ( is.na ( fuel ) ),
                           ! ( is.na ( working_sectors_v1 ) ),
                           ! ( is.na ( s_content_fraction ) ) )

        colnames( df_mapped ) <- c( "iso", "reg_abb", "GAINS.fuel",
                                    "fuel", "GAINS.sectors", "sector", "s_content_fraction")

        df_out <- df_mapped

        return( df_out )

    }

#   B. Define function for initial GAINS activity data processing and creation of activity based weights
#      (by CEDS isos, sectors, and fuels)
    initial_processing_GAINS_activity <- function( activity_df_in, scont_data,
                                                   fuel_mapping, country_mapping,
                                                   sector_mapping ){

#       Define fuels and sectors of interest (from associated ash retention data)
        GAINS_scontent_fuels <- unique( scont_data$GAINS.fuel )
        GAINS_scontent_sectors <- unique( scont_data$GAINS.sectors )

#       Gather to long format and Replace "n.a" values with 0
#       Note - this assumes that all GAINS fuels will begin at column 4
        gainsenergy_no_sum <- activity_df_in %>%
            dplyr::select( -Sum )

        GAINS_activity_fuel_cols <- colnames( gainsenergy_no_sum[ , 4:length( colnames ( gainsenergy_no_sum ) ) ] )

        gains_activity_clean <- gainsenergy_no_sum %>%
            tidyr::gather( key = GAINS.fuel, value = Consumption, GAINS_activity_fuel_cols ) %>%
            dplyr::mutate( Consumption = as.numeric( Consumption ) ) %>%
            dplyr::mutate( Consumption = if_else( is.na( Consumption ), 0, Consumption ) ) %>%

#       Retain only GAINS fuels which were in the associated ash retention data and filter out "sum sectors"
            dplyr::filter( GAINS.fuel %in% GAINS_scontent_fuels,
                           ! ( sector.activity %in% c( "ALL SUM",  "SUM" ) ) ) %>%

#       Map to CEDS isos and fuels -- filter out observations that are NA for isos and fuels
            dplyr::left_join( fuel_mapping, by = "GAINS.fuel" ) %>%
            dplyr::rename ( country = cou_abb,
                            GAINS.sectors = sector.activity ) %>%
            dplyr::left_join( country_mapping, by = "country" ) %>%
            dplyr::mutate( ISO.code = tolower( ISO.code ) ) %>%
            dplyr::filter( ! ( is.na ( ISO.code ) ),
                           ! ( is.na ( fuel ) ) ) %>%
            dplyr::select( ISO.code, reg_abb, GAINS.sectors, GAINS.fuel, fuel,
                           Consumption ) %>%

#       Fix GAINS.sectors to match GAINS.sectors in ash retention data
#           Remove GAINS.sectors that have all values 0, but disagg activity for this sector has non-zero data
            dplyr::filter( ! (GAINS.sectors %in% c( "IN_BO", "TRA_OT", "TRA_RD") ) ) %>%
#           Replace the above sectors with disaggregate data
            dplyr::mutate( GAINS.sectors = if_else( GAINS.sectors %in% c( "IN_BO_CHEM", "IN_BO_CON",
                                                                          "IN_BO_OTH", "IN_BO_OTH_L",
                                                                          "IN_BO_OTH_S", "IN_BO_PAP" ),
                                                    "IN_BO",
                                                    if_else( GAINS.sectors == "TRA_OT_RAI", "TRA_OT",
                                                             if_else( GAINS.sectors %in% c( "TRA_RD_HDB", "TRA_RD_HDT",
                                                                                            "TRA_RD_LD2", "TRA_RD_LD4C",
                                                                                            "TRA_RD_LD4T", "TRA_RD_M4" ),
                                                                      "TRA_RD", GAINS.sectors ) ) ) ) %>%
#           Convert disaggregate sectors to agg sector names (for disaggg sectors for which no agg sector
#           was present in activity, but is present in the ash retention data)
            dplyr::mutate( GAINS.sectors = if_else( GAINS.sectors == "IN_OCTOT", "IN_OC",
                                                    if_else( GAINS.sectors %in% c( "TRA_OTS_L", "TRA_OTS_M" ),
                                                             "TRA_OTS", GAINS.sectors ) ) ) %>%
#           Rename PP_NEW_L to PP_NEW for coal only
            dplyr::mutate( GAINS.sectors = if_else ( GAINS.sectors == "PP_NEW_L" &
                                                         fuel %in% c( "hard_coal" , "brown_coal" ),
                                                     "PP_NEW", GAINS.sectors ) ) %>%
#           Aggregate by iso, subregion, gains sector, gains fuel, and ceds fuel
            dplyr::group_by( ISO.code, reg_abb, GAINS.sectors, GAINS.fuel, fuel ) %>%
            dplyr::summarise_all( sum ) %>%
            dplyr::ungroup( ) %>%

#       Map to CEDS sectors and retain only obersvations with GAINS.sectors within associated ash retention data
            dplyr::left_join( sector_mapping, by = "GAINS.sectors" ) %>%
            dplyr::select( ISO.code, reg_abb, GAINS.sectors, working_sectors_v1, GAINS.fuel, fuel, Consumption) %>%
            dplyr::filter( GAINS.sectors %in% GAINS_scontent_sectors,
                           ! (is.na ( working_sectors_v1 ) ) )

#       Aggregate by iso, CEDS fuel and CEDS detailed sector - so that can calculate weights
        gains_activity_aggregated_iso_Cfuel_csec <- gains_activity_clean %>%
            dplyr::group_by( ISO.code, fuel, working_sectors_v1 ) %>%
            dplyr::select( ISO.code, fuel, working_sectors_v1, Consumption ) %>%
            dplyr::summarise_all( sum ) %>%
            dplyr::rename( Agg_consumption = Consumption ) %>%
            dplyr::ungroup( )

#       Calculate weights, data  with weights = NAN are where there was --> 0 consumption / 0 agg consumption
        gains_activity_weights <- gains_activity_clean %>%
            dplyr::left_join( gains_activity_aggregated_iso_Cfuel_csec, by = c( "ISO.code", "fuel",
                                                                                "working_sectors_v1" ) ) %>%
            dplyr::mutate( Activity_based_weights = Consumption/Agg_consumption ) %>%
            dplyr::rename( iso = ISO.code,
                           sector = working_sectors_v1 )


        df_out <- gains_activity_weights

        return( df_out )

    }

#   C.) Define function for calculating mean GAINS scont
    calculate_GAINS_scont_mean <- function( df_in, df_in_activity_weights, s_content_year){

        df_in_weighted_mean <- df_in %>%
            dplyr::left_join( df_in_activity_weights, by = c( "iso", "reg_abb", "GAINS.fuel",
                                                               "fuel", "GAINS.sectors", "sector" ) ) %>%

#       Remove data that is NA for Consumption, Agg_consumption, and Activity_based_weights
            dplyr::filter( ! ( is.na ( Consumption ) ),
                           ! ( is.na ( Agg_consumption ) ),
                           ! ( is.na ( Activity_based_weights ) ) ) %>%

#       Multiple weights by s content  and sum for weighted average s content
            dplyr::select( -Consumption, -Agg_consumption ) %>%
            dplyr::group_by( iso, reg_abb, GAINS.fuel, fuel, GAINS.sectors, sector ) %>%
            dplyr::mutate( Weighted_s_cont = s_content_fraction * Activity_based_weights ) %>%
            dplyr::group_by( iso,  fuel, sector ) %>%
            dplyr::select( iso, sector, fuel,  Weighted_s_cont ) %>%
            dplyr::summarise_all( sum ) %>%
            dplyr::ungroup( )

        colnames( df_in_weighted_mean ) <- c( "iso", "fuel", "sector", s_content_year )

        df_out <- df_in_weighted_mean

        return( df_out )

        }


#   D.) Define function for final processing of GAINS scont data
    final_GAINS_scont_procsesing <- function( df_in, s_content_year){

#       Add units
        df_in$units <- 'fraction'
        df_in <- df_in[ , c( "iso", "sector", "fuel", "units", s_content_year ) ]

#       Retain non zero values
        df_in <- df_in[ which( df_in[, s_content_year] > 0 ), ]

#       Note 0_Temp-Aggregated is a temporary/aggregated sector, here used to
#       aggregate DOM and IN_*. Copy the values of 0_Temp-Aggregated back to those
#       sectors
        disagg_sectors <- c( "1A2a_Ind-Comb-Iron-steel",
                            "1A2b_Ind-Comb-Non-ferrous-metals",
                             "1A2c_Ind-Comb-Chemicals",
                            "1A2d_Ind-Comb-Pulp-paper",
                            "1A2e_Ind-Comb-Food-tobacco",
                            "1A2f_Ind-Comb-Non-metalic-minerals",
                            "1A2g_Ind-Comb-Construction",
                            "1A2g_Ind-Comb-machinery",
                            "1A2g_Ind-Comb-mining-quarying",
                            "1A2g_Ind-Comb-other",
                            "1A2g_Ind-Comb-textile-leather",
                            "1A2g_Ind-Comb-transpequip",
                            "1A2g_Ind-Comb-wood-products",
                            "1A4a_Commercial-institutional",
                            "1A4b_Residential",
                            "1A4c_Agriculture-forestry-fishing",
                            "1A5_Other-unspecified" )

#       Disaggregate 0_Temp-Aggregated
        gains_s_content_disagg <- dplyr::filter( df_in, sector == "0_Temp-Aggregated" ) %>%
                       repeatAndAddVector( "sector", disagg_sectors )

#       Reintegrate disaggregated cells into main dataframe
        gains_s_content_final <- dplyr::filter( df_in, sector != "0_Temp-Aggregated" ) %>%
                                 dplyr::bind_rows( gains_s_content_disagg ) %>%
                                 dplyr::arrange( iso, sector, fuel )

        return(gains_s_content_final)

    }

# ---------------------------------------------------------------------------

# 2. Process GAINS s content data
#   A. Add working sectors to GAINS sector map and correct domestic navigation sector
    sector_map_with_working_sectors <- sectormap %>%
        dplyr::mutate( detailed_sectors = if_else ( detailed_sectors == "1A3dii_Domestic-navigation-shipping",
                                                    "1A3dii_Domestic-navigation", detailed_sectors ) ) %>%
        dplyr::left_join( MSL, by = "detailed_sectors" ) %>%
        dplyr::select( GAINS.sectors, GAINS.long.name, working_sectors_v1 )

#   B. Process EU s content data
    gains_s_content_EU_initial <- initial_processing_GAINS_scont( gains_s_content_EU, fuelmap,
                                                                  gainstoiso, sector_map_with_working_sectors )

    gains_EU_activity_weights <- initial_processing_GAINS_activity( gainsenergy_EU, gains_s_content_EU_initial,
                                                                     fuelmap, gainstoiso,
                                                                     sector_map_with_working_sectors )

    gains_s_content_EU_mean <- calculate_GAINS_scont_mean( gains_s_content_EU_initial, gains_EU_activity_weights,
                                                            "X2005" )

    gains_s_content_EU_final <- final_GAINS_scont_procsesing( gains_s_content_EU_mean,
                                                              "X2005" )

#   C. Process India s content data
    gains_s_content_IND_initial <- initial_processing_GAINS_scont( gains_s_content_IND, fuelmap,
                                                                   gainstoiso, sector_map_with_working_sectors )

    gains_IND_activity_weights <- initial_processing_GAINS_activity( gainsenergy_IND, gains_s_content_IND_initial,
                                                                     fuelmap, gainstoiso,
                                                                     sector_map_with_working_sectors )

    gains_s_content_IND_mean <- calculate_GAINS_scont_mean( gains_s_content_IND_initial, gains_IND_activity_weights,
                                                           "X2010" )

    gains_s_content_IND_final <- final_GAINS_scont_procsesing( gains_s_content_IND_mean,
                                                              "X2010" )

#       Expand India data to all CEDS unextended years (1960-2014) - same data for all CEDS years
#       Note: We have not taken into account for India how coal imports may change s content
#              values for different years, or how GAINS subfuels (multiple GAINS fuels
#             mapped to 1 CEDS fuel) could vary in consumption from year to year (for weights).
#       TODO: Account for the above note (retention values varying with coal imports and
#             consupmtion of GAINS subfuels over time)
    all_years_before2010 <- paste0( "X", 1960:2009 )
    all_years_after2010 <- paste0( "X", 2011:2014 )
    all_years <- paste0( "X", 1960:2014 )

    gains_s_content_IND_final_extended <- gains_s_content_IND_final %>%
        dplyr::mutate_at( all_years_before2010, funs( + X2010 ) ) %>%
        dplyr::mutate_at( all_years_after2010, funs( + X2010 ) ) %>%
        dplyr::select( iso, sector, fuel, units, all_years )

#   D. Combine EU and India data
    gains_s_content_final <- dplyr::bind_rows(gains_s_content_EU_final,
                                              gains_s_content_IND_final_extended) %>%
        dplyr::select( iso, sector, fuel, units, all_years )

# -------------------------------------------------------------------------------

# 3. Output

    writeData( gains_s_content_final, domain = "DEFAULT_EF_PARAM",
               fn = "B.SO2_GAINS_s_content" )

    logStop()

# END
