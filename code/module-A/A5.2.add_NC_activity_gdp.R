# ------------------------------------------------------------------------------
# Program Name: A5.2.add_NC_activity_gdp.R
# Author: Jon Seibert
# Date Last Modified: June 19, 2015
# Program Purpose: To process and reformat non-combustion (process) GDP activity_data,
#                  and add it to the activity database.
# Input Files: A.NC_activty_db.csv, 2011_NC_SO2_ctry.csv, GDP.xlsx, activity_input_mapping.csv
# Output Files: A.NC_activty_db.csv
# Notes:
# TODO:
# -------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "timeframe_functions.R",
                  "process_db_functions.R", "analysis_functions.R" ) # Additional function files required.
    log_msg <- "Initial reformatting of gdp process activity activity_data" # First message to be printed to the log
    script_name <- "A5.2.add_NC_activity_gdp.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# -----------------------------------------------------------------------------
# 0.5. Settings

# Input file
    input_name <- "GDP"
    input_ext <- ".xlsx"
    input <- paste0( input_name, input_ext )

# Location of input files relative to wd (input)
    input_domain <- "ACTIVITY_IN"

# ------------------------------------------------------------------------------
# 1. Read in files

# iso_mapping <- readData( "MAPPINGS","2011_NC_SO2_ctry" )
    act_input <- readData( "MAPPINGS", "activity_input_mapping" )
    MSL <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx",
                     sheet_selection = "Sectors" )
# Read in activity data
    activity_data <- readData( input_domain, input_name, input_ext )

    activity_data <- activity_data[[ 6 ]]

# Set variables with the activity of the input file and the associated units
    activity_name <- act_input$activity[ act_input$file == input ]

    unit <- 'B2005USD'

# ------------------------------------------------------------------------------
# 2. Reformatting

# Apply reformatting header function
    activity_data <- cleanData( activity_data )

# Applies activity and unit assignments, and reorders columns to standard form
    activity_data$activity <- activity_name
    activity_data$units <- unit

    activity_years <- names( activity_data )[ grep( "X", names( activity_data ) ) ]

    results <- cbind( activity_data[ c( "iso","activity","units" ) ] ,
                      activity_data[ activity_years ] ) ### Does this need to be cbind?

# Sort results by iso and activity
    results <- results[ with( results, order( iso, activity ) ), ]

# ------------------------------------------------------------------------------
# 3. Output

# Add reformatted activity_data to the activity database, extending or truncating it as necessary.
# By default, it will be extended forward to the common end year, but not backwards.
# Only do this if the activityCheck header function determines that the activities in
# the reformatted activity_data are all present in the Master List.
    if( activityCheck( results, check_all = FALSE ) ){
        addToActivityDb( results )
    }

    logStop()
# END
