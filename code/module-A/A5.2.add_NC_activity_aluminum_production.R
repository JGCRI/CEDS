#------------------------------------------------------------------------------
# Program Name: A5.2.add_NC_activity_aluminum_production.R
# Author: Jon Seibert, Andrea Mott
# Date Last Modified: May 18, 2021
# Program Purpose: To process and reformat non-combustion (process) aluminum production activity_data,
#                  and add it to the activity database.
# Input Files: A.NC_activty_db.csv, 2011_NC_SO2_ctry.csv, Aluminum_Production.xlsx,
#             activity_input_mapping.csv
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
                  "analysis_functions.R", "interpolation_extension_functions.R" ) # Additional function files required.
    log_msg <- "Initial reformatting of smelting process activity activity_data" # First message to be printed to the log
    script_name <- "A5.2.add_NC_activity_aluminum_smelting.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# -----------------------------------------------------------------------------
# 0.5. Settings

# Input file
    input_name <- "Aluminum_Production"
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
        activity_data$units <- "kt"
        results <- cbind( activity_data[ c( "iso", "activity", "units" ) ] ,
                          activity_data[ 2:( length( activity_data ) - 3 ) ] )

        # remove redundant usa row
        results <- na.omit(results)

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

        # Detect last year in activity data
        activity_end_year <- as.numeric(sub('.', '', tail(names(results), 1)))

        # Historically extend, filling in 0s, then reorder years
        results[ paste0('X', historical_pre_extension_year: 1849)] <- 0
        results <- results[ c( 'iso' , 'activity' , 'units' , paste0('X', historical_pre_extension_year:activity_end_year) ) ]

        # If last year of activity data ≥ last CEDS year, truncate to last CEDS year,
        # otherwise extend to last CEDS year
        if (activity_end_year >= end_year) {
            results <- results[ c( 'iso' , 'activity' , 'units' , X_extended_years ) ]
        } else {
            # Extend aluminum forward
            results[ paste0('X', (activity_end_year + 1):end_year)] <- NA
            results[ paste0('X', (activity_end_year + 1):end_year)] <- as.numeric(as.character(results[ paste0('X', (activity_end_year + 1):end_year)]))
            results <- extend_and_interpolate(results, paste0('X', activity_end_year:end_year))
        }

        # write results
        writeData( results, "MED_OUT", "A.Aluminum_production", meta = F )
        writeData( results, "EXT_IN", "A.Aluminum_production", domain_extension = "extension-data/")
    }

    logStop()
# END
