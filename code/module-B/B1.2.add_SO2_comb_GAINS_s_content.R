# Program Name: B1.2.add_SO2_GAINS_s_content.R
# Author:  Rachel Hoesly
# Date Last Updated: 19 April 2016
# Program Purpose: Add 2005 GAINS sulfur content data into defualt sulfur content database
#
# Input Files: GAINS_country_mapping.csv, GAINS_fuel_mapping.csv,
#             GAINS_sector_mapping.csv, GAINS_scont@SO2-EU28.csv
# Output Files: B.SO2_GAINS_s_content.csv
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
    script_name <- "B1.2.add_SO2_GAINS_s_content.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 1. Reading data and mapppings into script

# Read in GAINS sulfur content
    gains_s_content  <- readData( "EM_INV",  "GAINS_scont@SO2-EU28" )
# Read in GAINS mapping files
    gainstoiso <- readData( "GAINS_MAPPINGS",  "GAINS_country_mapping" )
    fuelmap <- readData(  "GAINS_MAPPINGS", "GAINS_fuel_mapping" )
    sectormap <- readData( "GAINS_MAPPINGS",  "GAINS_sector_mapping" )

# ---------------------------------------------------------------------------
# 2. GAINS s content data processing

# Clean up column headers and empty rows
    names( gains_s_content ) <- gains_s_content[ 6, ]
    gains_s_content <- gains_s_content[ -1:-7, ]

# Convert values to numeric
    gains_s_content[ , 4:ncol( gains_s_content ) ] <-
        lapply( gains_s_content[ , 4:ncol( gains_s_content ) ], as.numeric )

# Melt to long form & rename
    gains_s_content <- melt( gains_s_content, id.vars = c( "cou_abb", "reg_abb", "fuel" ) )
    colnames( gains_s_content ) <- c( "iso", "reg_abb", "fuel", "sector", "X2005" )

# units given in %, divide by 100 to convert to fraction
    gains_s_content$X2005 <- gains_s_content$X2005 / 100

# Map CEDS fuels and iso
    gains_s_content$fuel <- fuelmap[ match( gains_s_content$fuel,
                                            fuelmap$GAINS.fuel ),
                                     'fuel' ]
    gains_s_content$iso <- tolower( gainstoiso[ match( gains_s_content$iso,
                                                       gainstoiso$country ),
                                                'ISO.code' ] )

# Remove unmatched data & incomplete/NA entries
    gains_s_content <-
      gains_s_content[ complete.cases( gains_s_content[ , c( 'iso', 'fuel',
                                                             'sector', 'X2005' ) ] ), ]

# Convert to CEDS sectors
    gains_s_content <- mapCEDS_sector_fuel( mapping_data = gains_s_content,
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

# Take the mean of the Sulfur Retention Values for any duplicated ID groups
    gains_s_content <- aggregate( gains_s_content[ c( "X2005" ) ],
                                  by = gains_s_content[ c( "iso",
                                                           "sector",
                                                           "fuel" ) ],
                                  FUN = mean )

# add units
    gains_s_content$units <- 'fraction'
    gains_s_content <- gains_s_content[ , c( "iso", "sector", "fuel",
                                             "units", "X2005" ) ]

# retain non zero values
    gains_s_content <- gains_s_content[ which( gains_s_content$X2005 > 0 ), ]

# Note 0_Temp-Aggregated is a temporary/aggregated sector, here used to
# aggregate DOM and IN_*. Copy the values of 0_Temp-Aggregated back to those
# sectors
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

# Disaggregate 0_Temp-Aggregated
    gains_s_content_disagg <- filter( gains_s_content,
                                      sector == "0_Temp-Aggregated" ) %>%
                       repeatAndAddVector( "sector", disagg_sectors )

# Reintegrate disaggregated cells into main dataframe
    gains_s_content_final <- filter( gains_s_content,
                                     sector != "0_Temp-Aggregated" ) %>%
                                 bind_rows( gains_s_content_disagg ) %>%
                                        dplyr::arrange( iso, sector, fuel )

# -------------------------------------------------------------------------------
# 3. Output
    writeData( gains_s_content_final, domain = "DEFAULT_EF_PARAM",
               fn = "B.SO2_GAINS_s_content" )

    logStop()
# END
