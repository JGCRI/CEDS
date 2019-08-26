# ---------------------------------------------------------------------------
# Program Name: B1.2.add_SO2_GAINS_ash_ret.R
# Author: Leyang Feng, Ryan Bolt, Rachel Hoesly, Linh Vu, Patrick O'Rourke
# Date Last Updated: April 4, 2019
# Program Purpose: Add GAINS ash retention data into defualt ash retention database
#                  for EU (2005 data) and India (data for 2010 used for all CEDS years)
# Input Files: GAINS_country_mapping.csv, GAINS_fuel_mapping.csv,
#              GAINS_sector_mapping.csv, GAINS_sinash@SO2-EU28_nohead.csv,
#              GAINS_sinash@SO2-IND_nohead.csv, GAINS_aen_act_sect-nohead-EU28.csv,
#              GAINS_aen_act_sect-nohead-IND.csv, Master_Sector_Level_map.csv
# Output Files: B.SO2_GAINS_s_ash_ret.csv
# Notes:
# TODO: For India data: account for coal imports over time and different levels
#       of consumption of subfuels (GAINS fuels) which map to 1 CEDS fuel over time

# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'timeframe_functions.R', "data_functions.R", "analysis_functions.R",
                  "process_db_functions.R", 'interpolation_extension_functions.R' ) # Additional function files may be required.
    log_msg <- "Processing GAINS ash_retention data..." # First message to be printed to the log
    script_name <- "B1.2.add_SO2_comb_GAINS_ash_ret.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 1. Reading data and mapppings into script

# Read in GAINS ash retention data and mapping files
    gainsash_ret_input_EU <- readData( "EM_INV", "GAINS_sinash@SO2-EU28_nohead" )
    gainsash_ret_input_IND <- readData( "EM_INV", "GAINS_sinash@SO2-IND_nohead")

    gainsenergy_EU <- readData( "EM_INV", "GAINS_aen_act_sect-nohead-EU28" )
    gainsenergy_IND <- readData( "EM_INV", "GAINS_aen_act_sect-nohead-IND" )

    gainstoiso <- readData( "GAINS_MAPPINGS", "GAINS_country_mapping" )
    fuelmap <- readData(  "GAINS_MAPPINGS", "GAINS_fuel_mapping" )
    sectormap <- readData( "GAINS_MAPPINGS", "GAINS_sector_mapping" )
    MSL <- readData( "MAPPINGS", "Master_Sector_Level_map")

# ---------------------------------------------------------------------------
# 2. Define functions for script

#   A. Define function for initial GAINS ash retention data processing
#      Note: replace_INOC_with_INBO can be set to TRUE or FALSE. TRUE implies
#            the IN_OC data for GAINS fuel DC should be replaced by data from IN_BO.
#            This is currently only applied to European data, as the values for DC
#            were much larger than they were for other sectors, for the same fuel.
    initial_processing_GAINS_ashret <- function( df_in, country_mapping,
                                                fuel_mapping, sector_mapping,
                                                replace_INOC_with_INBO ){

        gainsash_ret <- df_in

#       Replace "n.a" values with 0
        gainsash_ret[ gainsash_ret == "n.a" ] <- 0

#       Convert value columns to numeric - note, this assumes that all GAINS variable columns will begin at column 4
        gainsash_ret[ , 4:ncol( gainsash_ret ) ] <-
            lapply( gainsash_ret[ , 4:ncol( gainsash_ret ) ], as.numeric )

#       Eurpean data for
        if( replace_INOC_with_INBO ){

            printLog ( "Applying IN_BO values to IN_OC for GAINS fuel DC..." )

            gainsash_ret <- gainsash_ret %>%
                dplyr::mutate( IN_OC = if_else( fuel == "DC", IN_BO, IN_OC ) )

        } else{

        }

#       Gather to long form - note, this assumes that all GAINS variable columns will begin at column 4
        GAINS_variable_cols <- colnames( gainsash_ret[ , 4:length( colnames ( gainsash_ret ) ) ] )

        gainsash_ret_out <- gainsash_ret %>%
             tidyr::gather( key = variable, value = value, GAINS_variable_cols ) %>%

#       Get rid of "SUM" fuels and rename columns
             dplyr::filter( fuel != "SUM" ) %>%
             dplyr::rename( GAINS.fuel = fuel,
                            GAINS.sectors = variable,
                            ash_retention = value,
                            country = cou_abb ) %>%

#       Map to CEDS isos, fuels, and sectors, but retain GAINS fuel and sector information for
#           left_join of weights generated from GAINS activity data
            dplyr::left_join( country_mapping, by = "country" ) %>%
            dplyr::left_join( fuel_mapping, by = "GAINS.fuel" ) %>%
            dplyr::left_join( sector_mapping, by = "GAINS.sectors" ) %>%
            dplyr::select( ISO.code, reg_abb, GAINS.fuel, fuel, GAINS.sectors, working_sectors_v1,
                           ash_retention ) %>%
            dplyr::filter( ! ( is.na ( ISO.code) ),
                           ! ( is.na ( fuel ) ),
                           ! ( is.na ( working_sectors_v1 ) ) ) %>%
            dplyr::mutate( ISO.code = tolower( ISO.code ) )

        df_out <- gainsash_ret_out

        return( df_out )

}

#   B. Define function for initial GAINS activity data processing and creation of activity based weights
#      (by CEDS isos, sectors, and fuels)
        initial_processing_GAINS_activity <- function( activity_df_in, ash_data,
                                                       fuel_mapping, country_mapping,
                                                       sector_mapping ){

#       Define fuels and sectors of interest (from associated ash retention data)
        GAINS_ash_fuels <- unique( ash_data$GAINS.fuel )
        GAINS_ash_sectors <- unique( ash_data$GAINS.sectors )

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
            dplyr::filter( GAINS.fuel %in% GAINS_ash_fuels,
                           ! ( sector.activity %in% c( "ALL SUM",  "SUM" ) ) ) %>%

#       Map to CEDS isos and fuels-- filter out observations that are NA for isos and fuels
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
            dplyr::filter( GAINS.sectors %in% GAINS_ash_sectors,
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
            dplyr::mutate( Activity_based_weights = Consumption/Agg_consumption )


        df_out <- gains_activity_weights

        return( df_out )

}

#  C. Define function for calculating mean GAINS ash retention
    calculate_GAINS_ashret_mean <- function( df_in, df_in_activity_weights,
                                             ash_year ){

    df_in_weighted_mean <- df_in %>%
        dplyr::left_join( df_in_activity_weights, by = c( "ISO.code", "reg_abb", "GAINS.fuel",
                                                          "fuel", "GAINS.sectors", "working_sectors_v1" ) ) %>%

#   Remove data that is NA for Consumption, Agg_consumption, and Activity_based_weights
        dplyr::filter( ! ( is.na ( Consumption ) ),
                       ! ( is.na ( Agg_consumption ) ),
                       ! ( is.na ( Activity_based_weights ) ) ) %>%

#   Multiple weights by ash retention and sum for weighted average ash retention
        dplyr::select( -Consumption, -Agg_consumption ) %>%
        dplyr::group_by( ISO.code, reg_abb, GAINS.fuel, fuel, GAINS.sectors, working_sectors_v1 ) %>%
        dplyr::mutate( Weighted_Ash_Retention = ash_retention * Activity_based_weights ) %>%
        dplyr::group_by( ISO.code,  fuel, working_sectors_v1 ) %>%
        dplyr::select( ISO.code, working_sectors_v1, fuel,  Weighted_Ash_Retention ) %>%
        dplyr::summarise_all( sum ) %>%
        dplyr::ungroup( )

    colnames( df_in_weighted_mean ) <- c( "iso", "fuel", "sector", ash_year )

    df_out <- df_in_weighted_mean

    return( df_out )

    }

#   D. Define function for final data processing of GAINS ash retention
    final_GAINS_ashret_procsesing <- function( df_in, ash_year ){

        df_in_weighted_mean <- df_in

#       Add units
        df_in_weighted_mean$units <- 'fraction'

#       Keep only CEDS-form columns
        df_in_weighted_mean <- df_in_weighted_mean[ , c( "iso", "sector", "fuel",
                                                         "units", ash_year ) ]
#       Drop 0 values
        df_in_weighted_mean <- df_in_weighted_mean[ which( df_in_weighted_mean[, ash_year] > 0 ), ]

#       Note 0_Temp-Aggregated is a temporary/aggregated sector, here used to aggregate DOM
#       and IN_*. Copy the values of 0_Temp-Aggregated back to those sectors
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

#       Disaggregate 0_Temp-Aggregated sector into disagg sectors
        df_in_weighted_mean_disagg <- dplyr::filter( df_in_weighted_mean,
                                                     sector == "0_Temp-Aggregated" ) %>%
        repeatAndAddVector( "sector", disagg_sectors )

#       Bind disaggregted data with data that did not need to be disaggregated, then
#       order for final output
        df_out <- dplyr::filter( df_in_weighted_mean,
                          sector != "0_Temp-Aggregated" ) %>%
                  dplyr::bind_rows( df_in_weighted_mean_disagg ) %>%
                  dplyr::arrange( iso, sector, fuel )

        return(df_out)

}

# -------------------------------------------------------------------------------

# 3. Process GAINS ash retention data

#   A.) Map the GAINS sector map to working_sectors_v1
    sector_map_with_working_sectors <- sectormap %>%
        dplyr::mutate( detailed_sectors = if_else ( detailed_sectors == "1A3dii_Domestic-navigation-shipping",
                                                    "1A3dii_Domestic-navigation", detailed_sectors ) ) %>%
        dplyr::left_join( MSL, by = "detailed_sectors" ) %>%
        dplyr::select( GAINS.sectors, GAINS.long.name, working_sectors_v1 )

#   B.) EU processing
    gainsash_ret_mapped_EU <- initial_processing_GAINS_ashret( gainsash_ret_input_EU, gainstoiso,
                                                               fuelmap, sector_map_with_working_sectors,
                                                               replace_INOC_with_INBO = TRUE )

    gains_EU_activity_weights <- initial_processing_GAINS_activity( gainsenergy_EU, gainsash_ret_mapped_EU,
                                                                     fuelmap, gainstoiso, sector_map_with_working_sectors)


    gainsash_ret_mean_EU <- calculate_GAINS_ashret_mean( gainsash_ret_mapped_EU, gains_EU_activity_weights,
                                                         "X2005" )

    gainsash_ret_final_EU <- final_GAINS_ashret_procsesing( gainsash_ret_mean_EU, "X2005")

#   C.) India processing
    gainsash_ret_mapped_IND <- initial_processing_GAINS_ashret( gainsash_ret_input_IND, gainstoiso,
                                                                fuelmap, sector_map_with_working_sectors,
                                                                replace_INOC_with_INBO = FALSE )

    gains_IND_activity_weights <- initial_processing_GAINS_activity( gainsenergy_IND, gainsash_ret_mapped_IND,
                                                            fuelmap, gainstoiso, sector_map_with_working_sectors)

    gainsash_ret_mean_IND <- calculate_GAINS_ashret_mean( gainsash_ret_mapped_IND, gains_IND_activity_weights,
                                                          "X2010" )

    gainsash_ret_final_IND <- final_GAINS_ashret_procsesing( gainsash_ret_mean_IND, "X2010")

#       Expand India data to all CEDS unextended years (1960-2014) - same data for all CEDS years
#       Note: We have not taken into account for India how coal imports may change ash
#             retention values for different years, or how GAINS subfuels (multiple GAINS fuels
#             mapped to 1 CEDS fuel) could vary in consumption from year to year (for weights).
#       TODO: Account for the above note (retention values varying with coal imports and
#             consupmtion of GAINS subfuels over time)
        all_years_before2010 <- paste0( "X", 1960:2009 )
        all_years_after2010 <- paste0( "X", 2011:2014 )
        all_years <- paste0( "X", 1960:2014 )

        gainsash_ret_final_IND_extended <- gainsash_ret_final_IND %>%
            dplyr::mutate_at( all_years_before2010, funs( + X2010 ) ) %>%
            dplyr::mutate_at( all_years_after2010, funs( + X2010 ) ) %>%
            dplyr::select( iso, sector, fuel, units, all_years )


#   C.) Combine EU and India data
    gainsash_ret_mapped_final <- dplyr::bind_rows(gainsash_ret_final_EU, gainsash_ret_final_IND_extended) %>%
        dplyr::select( iso, sector, fuel, units, all_years )

# -------------------------------------------------------------------------------
# 4. Output
     writeData( gainsash_ret_mapped_final,
                domain = "DEFAULT_EF_PARAM",
                fn = "B.SO2_GAINS_s_ash_ret" )

    logStop()

# END
