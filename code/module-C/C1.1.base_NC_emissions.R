#------------------------------------------------------------------------------
# Program Name: C1.1.base_NC_emissions.R
# Author: Jon Seibert
# Date Last Modified: June 30, 2015
# Program Purpose: To create the empty process emissions database
# Input Files: none
# Output Files: C.[em]_emissions_db.csv
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
    log_msg <- "Creation of initial blank process emissions database" # First message to be printed to the log
    script_name <- "C1.1.base_NC_emissions_db.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers, common_data = FALSE )  

    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "SO2"
    
# -------------------------------------------------------------------------------------------
# 0.5. Define functions

# createNewEmissionsDb
# Brief:         Creates a new, empty emissions database
# Details:       Uses standard format headers and common_data.R year limits to generate
#                       an empty emissions database file to be filled in.
# Dependencies:  common_data.R, IO_functions.R
# Author:        Jon Seibert
# Parameters:
#   em:             emissions species (ex. SO2, CO2, BC) [required]
# Return:        none
# Input files:   common_data.R
# Output files:  C.[em]_emissions_db.csv
    createNewEmissionsDb <- function( em ) {
        
        # Read in necessary files and data: common_data.R required 
        # to avoid variable overwrite carryover
        source( paste( PARAM_DIR, "common_data.R", sep = "" ) )
        
        # Use values from common_data.R
        years <- seq( start_year, end_year )
        X_years <- paste0( "X", years )
        
        results <- data.frame( iso = "", sector = "", fuel = "", units = "" )
        
        for ( yr in X_years ) {
            df <- data.frame( yr = 0 )
            results <- cbind( results, df )
        }
        names( results ) <- c( "iso", "sector", "fuel", "units", X_years )
        
        results <- subset( results, results$iso != "" )
        
        # Output
        writeData( results, domain = "MED_OUT", 
                   fn = paste0( "C.", em, "_", "NC", "_emissions_db" ), 
                   meta = FALSE )
    }
    
# ------------------------------------------------------------------------------
# 1. Create new, blank emission database
#    Call the function defined above to create a blank default df
    
    createNewEmissionsDb( em )
    
    logStop()
# END
