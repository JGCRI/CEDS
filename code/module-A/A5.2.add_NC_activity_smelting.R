#------------------------------------------------------------------------------
# Program Name: A5.2.add_NC_activity_smelting.R
# Author: Jon Seibert
# Date Last Modified: June 19, 2015
# Program Purpose: To process and reformat non-combustion (process) smelting activity_data,
#                  and add it to the activity database.
# Input Files: A.NC_activty_db.csv, 2011_NC_SO2_ctry.csv, Smelter-Feedstock-Sulfur.xlsx,
#             activity_input_mapping.csv
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
    headers <- c( "data_functions.R", "timeframe_functions.R", "process_db_functions.R",
                  "analysis_functions.R" ) # Additional function files required.
    log_msg <- "Initial reformatting of smelting process activity activity_data" # First message to be printed to the log
    script_name <- "A5.2.add_NC_activity_smelting.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# -----------------------------------------------------------------------------
# 0.5. Settings

# Input file
    input_name <- "Smelter-Feedstock-Sulfur"
    input_ext <- ".xlsx"
    input <- paste0( input_name, input_ext )

# Location of input files relative to wd (input)
    input_domain <- "ACTIVITY_IN"

# ------------------------------------------------------------------------------
# 1. Read in files

# iso_mapping <- readData( "MAPPINGS","2011_NC_SO2_ctry" )
    act_input <- readData( "MAPPINGS", "activity_input_mapping")
    MSL <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx",
                     sheet_selection = "Sectors" )

    activity_data <- readData( input_domain, input_name, input_ext )

# Set variables with the activity of the input file and the associated units
    activity_name <- act_input$activity[ act_input$file == input ]

# Check that this activity is actually used, otherwise don't add
    if ( activity_name %in% MSL$activity ) {

    ### Do we need to check that this is only one?
        unit <- unique( MSL$units[ MSL$activity == activity_name ] )

# ------------------------------------------------------------------------------
# 2. Reformatting

    # Apply reformatting header function
        activity_data <- cleanData( activity_data )

    # Apply activity and unit assignments, and reorders columns to standard form
        activity_data$activity <- activity_name
        activity_data$units <- unit
        results <- cbind( activity_data[ c( "iso", "activity", "units" ) ] ,
                          activity_data[ 2:( length( activity_data ) - 3 ) ] )

    # Sort results by iso and activity
        results <- results[ with( results, order( iso, activity ) ), ]

# ------------------------------------------------------------------------------
# 3. Output

    # Add reformatted activity_data to the activity database, extending or truncating it as necessary.
    # By default, it will be extended forward to the common end year, but not backwards.
    # Only do this if the activityCheck header function determines that the activities in
    # the reformatted activity_data are all present in the Master List.
        if ( activityCheck( results, check_all = FALSE ) ) {
              addToActivityDb( results )
        }
    }

    logStop()
# END
