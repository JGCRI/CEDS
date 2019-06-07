# ------------------------------------------------------------------------------
# Program Name: A7.1.base_activity.R
# Author: Rachel Hoesly
# Date Last Modified: 20 April 2018
# Program Purpose: Extend CEDS activity backward
# Input Files:  A.NC_activity.csv
# Output Files:
# TODO:
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
  PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
  headers <- c( "data_functions.R")
  log_msg <- "Creating database for CEDS process_activity_data extension before 1960"
  script_name <- "A7.1.base_activity.R"

  source( paste0( PARAM_DIR, "header.R" ) )
  initialize( script_name, log_msg, headers )


# ---------------------------------------------------------------------------
# 1. Load Data

  ceds_activity <- readData( 'MED_OUT', paste0( 'A.NC_activity' ) )

# ---------------------------------------------------------------------------
# 2. Extend Data frame

  ceds_activity[ paste0('X', historical_pre_extension_year: 1959)] <- NA
  ceds_activity <- ceds_activity[ c( 'iso' , 'sector' , 'fuel' , 'units' , X_extended_years ) ]

# ---------------------------------------------------------------------------
# 3. Output

 writeData( ceds_activity, "MED_OUT" , paste0( 'A.NC_activity_extended_db' ) )

 logStop( )

