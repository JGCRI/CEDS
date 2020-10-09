# ------------------------------------------------------------------------------
# Program Name: A5.2.add_NC_activity_population.R
# Author: Jon Seibert
# Date Last Modified: February 19, 2020
# Program Purpose: To add population data to the activity database.
# Input Files: A.final_population_activity.csv
# Output Files: A.NC_activty_db.csv
# Notes:
# TODO:

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "timeframe_functions.R", "process_db_functions.R",
                  "analysis_functions.R" ) # Additional function files required.
    log_msg <- "Adding population data to the activity database..." # First message to be printed to the log
    script_name <- "A5.2.add_NC_activity_population.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# -----------------------------------------------------------------------------
# 0.5. Settings

# ------------------------------------------------------------------------------
# 1. Read in files
    population_data <- readData( "MED_OUT", "A.final_population_activity" )

# ------------------------------------------------------------------------------
# 2. Output

# Add population_data to the activity database, extending or truncating it as necessary.
# By default, it will be extended forward to the common end year, but not backwards.
# Only do this if the activityCheck header function determines that the activities in
# the reformatted activity_data are all present in the Master List.
    if ( activityCheck( population_data, check_all = FALSE ) ) {
        addToActivityDb( population_data )
    }

    logStop()

# END
