# ------------------------------------------------------------------------------
# Program Name: A5.2.add_NC_activity_energy.R
# Author: Rachel Hoesly adapted from Jon Seibert
# Date Last Modified: Sept 21, 2015
# Program Purpose: To process and reformat non-combustion (process) GDP activity_data,
#                  and add it to the activity database.
# Input Files: A.other_IEA_energy_values.csv, activity_input_mapping.csv,
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
    script_name <- "A5.2.add_NC_activity_energy.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# -----------------------------------------------------------------------------
# 0.5. Settings

# Input file
    input_name <- "A.other_IEA_energy_values"
    input_ext <- ".csv"
    input <- paste0( input_name, input_ext )

# Location of input files relative to wd (input)
    input_domain <- "MED_OUT"

# ------------------------------------------------------------------------------
# 1. Read in files

# iso_mapping <- readData( "MAPPINGS","2011_NC_SO2_ctry" )
    act_input <- readData( "MAPPINGS", "activity_input_mapping" )
    MSL <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx",
                     sheet_selection = "Sectors" )

# Read in the non-combustion energy activity
    activity_data <- readData( input_domain, input_name, input_ext )

# ------------------------------------------------------------------------------
# 2. Reformatting

# Applies activity and unit assignments, and reorders columns to standard form
    activity_data$activity <- activity_data$sector
    results <- cbind( activity_data[ c( "iso", "activity", "units" ) ] ,
                      activity_data[ X_IEA_years ] )

# Sort results by iso and activity
    results <- results[ with( results, order( iso, activity ) ), ]

# ------------------------------------------------------------------------------
# 3. Output

# Add reformatted activity_data to the activity database, extending or truncating it as necessary.
# By default, it will be extended forward to the common end year, but not backwards.
# Only do this if the activityCheck header function determines that the activities in
# the reformatted activity_data are all present in the Master List.
    activityCheck( results, check_all = FALSE )
    addToActivityDb( results )

    logStop()
# END
