# ---------------------------------------------------------------------------
# Program Name: B1.2.add_SO2_GAINS_ash_ret.R
# Author: Leyang Feng, Ryan Bolt, Rachel Hoesly, Linh Vu
# Date Last Updated: 19 April 2016
# Program Purpose: Add 2005 GAINS ash retention data into defualt ash retention database
#
# Input Files: GAINS_country_mapping.csv, GAINS_fuel_mapping.csv,
#             GAINS_sector_mapping.csv, sinash@SO2-EU28_nohead.csv
# Output Files: B.SO2_GAINS_s_ash_ret.csv
# Notes:
# TODO:
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'timeframe_functions.R', "data_functions.R", "analysis_functions.R",
                  "process_db_functions.R", 'interpolation_extension_functions.R' ) # Additional function files may be required.
    log_msg <- "Processing GAINS ash_retention data" # First message to be printed to the log
    script_name <- "B1.2.add_SO2_comb_GAINS_ash_ret.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 1. Reading data and mapppings into script

# Read in GAINS ash retention data and mapping files
    gainsash_ret_input <- readData( "EM_INV", "GAINS_sinash@SO2-EU28_nohead" )
    gainstoiso <- readData( "GAINS_MAPPINGS", "GAINS_country_mapping" )
    fuelmap <- readData(  "GAINS_MAPPINGS", "GAINS_fuel_mapping" )
    sectormap <- readData( "GAINS_MAPPINGS", "GAINS_sector_mapping" )

# ---------------------------------------------------------------------------
# 2. GAINS ash retention data processing
#    Standard processing of input data. Map to CEDS,

# Begin processing; eplace "n.a" values with 0
    gainsash_ret <- gainsash_ret_input
    gainsash_ret[ gainsash_ret == "n.a" ] <- 0
# Convert value columns to numeric
    gainsash_ret[ , 4:ncol( gainsash_ret ) ] <-
                   lapply( gainsash_ret[ , 4:ncol( gainsash_ret ) ], as.numeric )

# Melt to long form  ### use gather
    gainsash_ret <- melt( gainsash_ret,
                          id.vars = c( "cou_abb", "reg_abb", "fuel" ) )

# Get rid of "SUM" fuels
    gainsash_ret <- gainsash_ret[ -which( gainsash_ret$fuel == 'SUM' ), ]
# Rename columns
    colnames( gainsash_ret ) <- c( "iso", "reg_abb", "fuel", "sector", "X2005" )

# Match CEDS fuels to GAINS fuels and add as a column
    gainsash_ret$fuel <- fuelmap[ match( gainsash_ret$fuel,
                                         fuelmap$GAINS.fuel ),
                                  'fuel' ]
# Match isos to GAINS countries, convert to lowercase, and add as a column
    gainsash_ret$iso <- tolower( gainstoiso[ match( gainsash_ret$iso,
                                                    gainstoiso$country ),
                                             'ISO.code' ] )

# Map CEDS sectors to GAINS sectors
    gainsash_ret_mapped <- mapCEDS_sector_fuel( mapping_data = gainsash_ret,
                           mapping_file = sectormap,
                           data_match_col = 'sector',
                           map_match_col = 'GAINS.sectors',
                           map_merge_col = c( 'detailed_sectors' ),
                           new_col_names = c( 'sector' ),
                           level_map_in = 'detailed_sectors',
                           level_out = 'working_sectors_v1',
                           aggregate = TRUE,
                           aggregate_col = c( 'X2005' ),
                           oneToOne = FALSE,
                           agg.fun = mean )

# 2.1 Aggregate by taking the mean of the Sulfur Retention Values.
    gainsash_ret_mapped <- aggregate( gainsash_ret_mapped[ c( "X2005" ) ],
                                      by = gainsash_ret_mapped[ c( "iso",
                                                                   "sector",
                                                                   "fuel" ) ],
                                      FUN = mean )

# Add units
    gainsash_ret_mapped$units <- 'fraction'
# Keep only CEDS-form columns
    gainsash_ret_mapped <- gainsash_ret_mapped[ , c( "iso", "sector", "fuel",
                                                     "units", "X2005" ) ]
# Drop 0 values
    gainsash_ret_mapped <-
           gainsash_ret_mapped[ which( gainsash_ret_mapped$X2005 > 0 ), ]

# Note 0_Temp-Aggregated is a temporary/aggregated sector, here used to aggregate DOM
# and IN_*. Copy the values of 0_Temp-Aggregated back to those sectors
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

# Disaggregate 0_Temp-Aggregated sector into disagg sectors
    gainsash_ret_mapped_disagg <- filter( gainsash_ret_mapped,
                                          sector == "0_Temp-Aggregated" ) %>%
                           repeatAndAddVector( "sector", disagg_sectors )

# Bind disaggregted data with data that did not need to be disaggregated, then
# order for final output
    gainsash_ret_mapped_final <- filter( gainsash_ret_mapped,
                                         sector != "0_Temp-Aggregated" ) %>%
                                 bind_rows( gainsash_ret_mapped_disagg ) %>%
                                            dplyr::arrange( iso, sector, fuel )

# -------------------------------------------------------------------------------
# 3. Output
    writeData( gainsash_ret_mapped_final,
               domain = "DEFAULT_EF_PARAM",
               fn = "B.SO2_GAINS_s_ash_ret" )

    logStop()
# END
