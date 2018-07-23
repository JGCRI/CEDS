# ---------------------------------------------------------------------------
# Program Name: B1.2.add_comb_control_percent.R
# Author: Rachel Hoesly
# Date Last Updated: 23 Nov 2015
# Program Purpose: Adds control_percent data in the EF_parameters folder to the
#                  ControlFrac_db. User to dynamically incorporate any data in the
#                  relevant folder.
# Input Files: files in the EF_parameters folder contailing control_percent and em
# Output Files: B.[em]_ControlFrac_db
# Notes:
# TODO:
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'process_db_functions.R', 'data_functions.R',
                  'interpolation_extension_functions.R', 'common_data.R' )
# Additional function files may be required.
    log_msg <- "Adding control percent data to data base" # First message to be printed to the log
    script_name <- "B1.2.add_comb_control_percent.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "SO2"

# ---------------------------------------------------------------------------
# 0.5 Load Packages

    loadPackage( 'tools' )

# ---------------------------------------------------------------------------
# 1. Reading data and mapppings into script
#    Select any files in the target folder

    printLog( "Reading in files" )

# Read in parameter files
    files_list <- list.files( path =  './default-emissions-data/EF_parameters',
                              pattern = '*.csv' )
    files_list <- tools::file_path_sans_ext( files_list )

# Select files that contain the phrase "control_percent"
    control_percent_file_list <- files_list[ grep( pattern = "_control_percent",
                                                   files_list ) ]

# Deselect metadata files
    control_percent_file_list <-
        control_percent_file_list[ -grep( pattern = "metadata",
                                          control_percent_file_list ) ]

# Select files with the given emission in the title
    control_percent_file_list <-
        control_percent_file_list[ grep( pattern = em,
                                         control_percent_file_list ) ]

# Read in data and add to a list of dataframes
    control_percent_list <- lapply ( X = control_percent_file_list,
                                     FUN = readData,
                                     domain = "DEFAULT_EF_PARAM")

# ---------------------------------------------------------------------------
# 2. Expand "all" variable and extend over time, convert list to one df

    printLog( 'Expanding and Extending control percent data' )

# Expand all, interpolate and Extend forward and back
    control_percent_extended <- lapply( X = control_percent_list,
                                        FUN = extendDefaultEF,
                                        pre_ext_method_default = 'linear_0' )

# Combine all data into a single data frame
    control_percent <- do.call( "rbind.fill", control_percent_extended )

# Indicate units
    control_percent$units <- 'percent'

# ---------------------------------------------------------------------------
# 2. Add to existing parameter Dbs

# If there is any data, incorporate and overwrite into the combustion control
# pct dataframe. Otherwise print a message that no data was added
    if ( length( control_percent_list ) > 0 ) {
        printLog( paste( 'Adding new data to existing control percent',
                         'database for', em ) )
        addToDb_overwrite( new_data = control_percent, em = em,
                           file_extension = 'ControlFrac_db' )
    } else {
        printLog( paste( 'No data to be added to existing control',
                         'percent data base for', em ) )
    }

    logStop()
# END
