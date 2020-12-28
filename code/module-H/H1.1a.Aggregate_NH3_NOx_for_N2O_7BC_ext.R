# Program Name: H1.1a.Aggregate_NH3_NOx_for_N2O_7BC_ext.R
# Author: Patrick O'Rourke
# Last updated: February 13, 2020
# Program Purpose: To aggregate NH3 and NOx final emissions for sectors 1 and 2
#                  after converting to units of N. This is used for historical extension of
#                  the "7BC_Indirect-N2O-non-agricultural-N" sector.
# Input: CEDS_NH3_emissions_by_country_CEDS_sector_[version_stamp].csv,
#        CEDS_NOx_emissions_by_country_CEDS_sector_[version_stamp].csv
# Output Files: H.N2O_7BC_extension-NH3_and_NOx_sectors_1_2.csv
# Notes: If NOx and/or NH3 emissions change within CEDS, this script should be run before
#        running N2O, in order to more accurately extend N2O emissions for sector 7BC_Indirect-N2O-non-agricultural-N
# TODO: Check if this is supposed to be an activity driver or EF trend - then find out how to incorporate
#       fully (required columns, etc.)
# TODO: Other script TODOs

#--------------------------------------------------------------------------------------------------
# 0. Get and set working directories, read in global settings and headers

# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if( "input" %in% dir( ) ) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "IO_functions.R","data_functions.R", "analysis_functions.R", "timeframe_functions.R" )
log_msg <- paste0( "Processing NH3 and NOx sectors 1 and 2 final emissions for historical extension ",
                   "of '7BC_Indirect-N2O-non-agricultural-N' N2o emissions..." )

script_name <- "H1.1a.Aggregate_NH3_NOx_for_N2O_7BC_ext.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

#--------------------------------------------------------------------------------------------------
# Read in data NH3 and NOx final emissions

#   Read in NH3 final emissions
    CEDS_final_NH3 <- readData( "FIN_OUT", domain_extension = 'current-versions/',
                                paste0( "CEDS_NH3_emissions_by_country_CEDS_sector_", version_stamp ),
                                meta = F )

#   Read in NOx final emissions
    CEDS_final_NOx <- readData( "FIN_OUT", domain_extension = 'current-versions/',
                            paste0( "CEDS_NOx_emissions_by_country_CEDS_sector_", version_stamp ),
                            meta = F )

#--------------------------------------------------------------------------------------------------
# 2. Define functions internal to script and other script constants not set by the user.

# Atomic weights - H, N, O
# Source: https://ciaaw.org/atomic-weights.htm (median value of range selected)
  H_atomic_weight_range <- c( 1.00784, 1.00811 )
  median_H_atomic_weight <- median( H_atomic_weight_range )

  N_atomic_weight_range <- c( 14.00643, 14.00728 )
  median_N_atomic_weight <- median( N_atomic_weight_range )

  O_atomic_weight_range <- c( 15.99903, 15.99977 )
  median_O_atomic_weight <- median ( O_atomic_weight_range )

# NH3 to N
  NH3_N_per_NH3 <- median_N_atomic_weight / ( median_N_atomic_weight + ( 3 * median_H_atomic_weight ) )

# NOx to N
  NO_weight <- median_N_atomic_weight + median_O_atomic_weight
  NO2_weight <- median_N_atomic_weight + ( 2 * median_O_atomic_weight )
  NO_and_NO2_weights <- c( NO_weight, NO2_weight )
  NO_and_NO2_avg_weight <- mean( NO_and_NO2_weights )
  NOx_N_weight <- median_N_atomic_weight

  # TODO: (Question for PR) Does this seem appropriate (assumes 50% NO & 50% NO2)?
  NOx_N_per_NOx <-  NOx_N_weight / NO_and_NO2_avg_weight

# Function to convert from units of NOx or NH3 to units of N
  convert_NH3_or_NOx_to_N <- function( CEDS_final_emissions, em_conversion_to_N, X_years ){

#   Sum working_sectors which are in aggregate sectors 1 and 2
    sectors_1_and_2 <- CEDS_final_emissions %>%
      dplyr::filter( startsWith( sector, "1" ) | startsWith( sector, "2" ) ) %>%
      dplyr::mutate( sector = "Aggregate_sectors_1_and_2" ) %>%
      dplyr::group_by( iso, sector, em, units ) %>%
      dplyr::summarize_all( sum, na.rm = TRUE ) %>%
      dplyr::ungroup( ) %>%
      dplyr::select( iso, sector, em, units, X_years )

#   Convert to kt of Nitrogen, from kt of NH3 and NOx
    sectors_1_and_2_N <- sectors_1_and_2 %>%
      dplyr::mutate_at( .vars = X_years, .funs = funs( . * em_conversion_to_N ) ) %>%
      dplyr::mutate( em = "N" )

    return(sectors_1_and_2_N )

}

#-------------------------------------------------------------------------------------------------
# 3. Clean CEDS final NH3 emissions

# Convert kt NH3 to kt N
  N_NH3_sect_1_2 <- convert_NH3_or_NOx_to_N( CEDS_final_emissions = CEDS_final_NH3,
                                             em_conversion_to_N = NH3_N_per_NH3,
                                             X_years = X_extended_years )

# Convert kt NOx to kt N
  N_NOx_sect_1_2 <- convert_NH3_or_NOx_to_N( CEDS_final_emissions = CEDS_final_NOx,
                                             em_conversion_to_N = NOx_N_per_NOx,
                                             X_years = X_extended_years )

# Sum both NH3 and NOx
  N_NH3_and_NOx_sect_1_2 <- dplyr::bind_rows( N_NH3_sect_1_2, N_NOx_sect_1_2 ) %>%
    dplyr::group_by( iso, sector, em, units ) %>%
    dplyr::summarize_all( sum, na.rm = TRUE ) %>%
    dplyr::ungroup( )

#-------------------------------------------------------------------------------------------------
# 4. Output Data

# Create metadata note

  meta_names <- c( "Data.Type", "Emission", "Region", "Sector", "Start.Year",
                   "End.Year", "Source.Comment" )

  meta_note <- c( "Emissions", "N2O", "All", "7BC_Indirect-N2O-non-agricultural-N",
                  historical_pre_extension_year, end_year,
                  paste0( "kt of N from NH3 and NOx sectors 1 and 2, by CEDS iso. ",
                          "NH3 and NOx emissions are from CEDS_",  version_stamp, "." ) )

  source_info <- script_name

  addMetaData( meta_note, meta_names, source_info )

# Save extension data
  writeData( N_NH3_and_NOx_sect_1_2, 'EXT_IN', domain_extension = 'extension-data/',
             'H.N2O_7BC_extension-NH3_and_NOx_sectors_1_2' )

  logStop( )




