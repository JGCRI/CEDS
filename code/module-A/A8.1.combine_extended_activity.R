#------------------------------------------------------------------------------
# Program Name: A8.1.combine_extended_activity.R
# Author: Rachel Hoesly
# Date Last Updated: 01 May 2018
# Program Purpose:
# Input Files: A.total_default_activity_extended.csv
#              A.total_default_activity_extended.csv
# Output Files: A.total_default_activity_extended.csv
# Notes:

# ------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c('data_functions.R', 'common_data.R') # Additional function files required.
log_msg <- paste0( "Combining extended combustion data" ) # First message to be printed to the log
script_name <- "A8.1.combine_extended_activity.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 1. Load Data

A.comb_default_activity_extended <- readData('MED_OUT','A.comb_user_added')
A.NC_default_activity_extended <- readData('MED_OUT','A.NC_default_activity_extended')


# ---------------------------------------------------------------------------
# 2. Combine combustion and non combustion extended activity data


total <- A.comb_default_activity_extended %>%
    dplyr::rename( sector = CEDS_sector, fuel = CEDS_fuel ) %>%
    dplyr::select( -agg_sector, -agg_fuel) %>%
    dplyr::bind_rows( A.NC_default_activity_extended ) %>%
    dplyr::arrange(iso, sector, fuel)

# ----------------------------------------------------------------------------

# Write out the data
writeData( total , "MED_OUT", "A.total_default_activity_extended" )

logStop()
# END