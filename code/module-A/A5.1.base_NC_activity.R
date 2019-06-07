#------------------------------------------------------------------------------
# Program Name: A5.1.base_NC_activity.R
# Author: Jon Seibert
# Date Last Modified: June 16, 2015
# Program Purpose: To create the empty process activity database.
# Input Files: none
# Output Files: A.NC_activty_db.csv
# Notes:
# TODO:
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "timeframe_functions.R" ) # Additional function files required.
    log_msg <- "Creation of initial blank process activity database" # First message to be printed to the log
    script_name <- "A5.1.base_NC_activity.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# -----------------------------------------------------------------------------
# 1. Create new, blank activity database

# ----------------------------------------------------------------------------------
# createNewActivityDb
# Brief:         Creates a new, empty process activity database
# Details:       Uses standard format headers and common_data.R year limits to generate
#                       an empty activity database file to be filled in.
# Dependencies:  common_data.R, data_functions.R, IO_functions.R
# Author:        Jon Seibert
# Parameters:    none
# Return:        none
# Input files:   common_data.R
# Output files:  A.NC_activity_db.csv
    createNewActivityDb <- function() {

    # Read in necessary files and data: common_data.R required
    # to avoid variable overwrite carryover
        source( paste( PARAM_DIR, "common_data.R", sep = "" ) )

    # Use values from common_data.R
        years <- seq( start_year, end_year )
        X_years <- paste0( "X", years )

    # Initialize an empty df
        results <- data.frame( iso = "", activity = "", units = "" )

    # Tack on as many rows as there are years   ### This seems like an odd way to do this but as long as it's
                                                ### not being used for anything bigger it's ok
        for ( yr in X_years ) {
            df <- data.frame( yr = 0 )
            results <- cbind( results, df )
        }
        names( results ) <- c( "iso", "activity", "units", X_years )
    # Empty the dataframe
        results <- subset( results, results$iso != "" )

    # Output an empty data frame (only column headers)
        writeData( results, domain = "MED_OUT", fn = "A.NC_activity_db", meta = FALSE )

    }

# Execute the function defined above  ### Why is it in a function? Will this ever be used elsewhere? It's not dynamic at all.
    createNewActivityDb()

    logStop()
# END
