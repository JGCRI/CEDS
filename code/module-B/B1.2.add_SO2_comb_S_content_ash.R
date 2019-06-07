# ---------------------------------------------------------------------------
# Program Name: B1.2.add_SO2_S_content_ash.R
# Author: Rachel Hoesly
# Date Last Updated: 23 Nov 2015
# Program Purpose: generates list of sulfur content and ash retention datafiles
#        and adds them to EF_parameter database
# Input Files: variable files in input/default-emissions-data/EF_parameters
# Output Files: B.SO2_S_Content_db.csv, B.SO2_AshRet_db.csv
# Notes:
# TODO:
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "analysis_functions.R",
                  'process_db_functions.R', 'common_data.R',
                  'interpolation_extension_functions.R' ) # Additional function files may be required.
    log_msg <- paste( "Adding data files of EF parameters (sulfur and",
                      "ash ret) to SO2 EF parameter databases" )

# First message to be printed to the log
    script_name <- "B1.2.add_SO2_S_content_ash.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 0.5 Load Packages

    loadPackage('tools')

# ---------------------------------------------------------------------------
# 1. Select list of files, and read in data

# Read in files in the EF_parameters folder
    printLog( 'Loading EF parameter data files' )
    files_list <- list.files( path = './default-emissions-data/EF_parameters',
                              pattern = '*.csv' )
    files_list <- tools::file_path_sans_ext( files_list )

# select s content and ash ret files
    s_content_file_list <- files_list[ grep( pattern = "s_content",
                                             files_list ) ]
    ash_ret_file_list <-  files_list[ grep( pattern = "_s_ash_ret",
                                            files_list ) ]

# select SO2 files
    s_content_file_list <- s_content_file_list[ grep( pattern = 'SO2',
                                                      s_content_file_list ) ]
    ash_ret_file_list <-  ash_ret_file_list[ grep( pattern = 'SO2',
                                                   ash_ret_file_list ) ]

# Deselect meta-data files
    s_content_file_list <- s_content_file_list [ -grep( pattern = "metadata",
                                                        s_content_file_list ) ]
    ash_ret_file_list <- ash_ret_file_list [ -grep( pattern = "metadata",
                                                    ash_ret_file_list ) ]

# Read in all identified files and add their data to a list of dfs
    ash_ret_list <- lapply ( X = ash_ret_file_list,
                             FUN = readData,
                             domain = "DEFAULT_EF_PARAM" )
    s_content_list <- lapply ( X = s_content_file_list,
                               FUN = readData,
                               domain = "DEFAULT_EF_PARAM" )

# ---------------------------------------------------------------------------
# 2. Expand all variable
    printLog('Expanding data and converting to wide form')

# Expand all, interpolate and extend forward and back
    ash_ret_extended <- lapply( X = ash_ret_list,
                                FUN = extendDefaultEF,
                                pre_ext_method_default = 'constant' )
    s_content_extended <- lapply( X = s_content_list,
                                  FUN = extendDefaultEF,
                                  pre_ext_method_default = 'constant' )

# Convert from lists of dfs to single large dfs
    s_content <- do.call( "rbind.fill", s_content_extended )
    ash_ret <- do.call( "rbind.fill", ash_ret_extended )

# Set units
    s_content$units <- 'kt/kt'
    ash_ret$units <- 'kt/kt'

# ---------------------------------------------------------------------------
# 2. Add to existing parameter Dbs

# Use addToDb_overwrite function to include the data
    addToDb_overwrite( new_data = s_content, em = 'SO2',
                       file_extension = 'S_Content_db' )
    addToDb_overwrite( new_data = ash_ret, em = 'SO2',
                       file_extension = 'AshRet_db' )

    logStop()
# END
