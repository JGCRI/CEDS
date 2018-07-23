# ---------------------------------------------------------------------------
# Program Name: B1.2.add_comb_default_EF.R
# Author: Rachel Hoesly
# Date Last Updated: 23 Nov 2015
# Program Purpose: Adds default_EF data in the EF_parameters folder to the
#                  ControlFrac_db
# Input Files: files in the EF_parameters folder contailing default_EF and em
# Output Files: B.[em]_comb_User_Added_EF.csv
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
    log_msg <- "Adding additional emission factors" # First message to be printed to the log
    script_name <- "B1.2.add_comb_default_EF.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "CO2"

# ---------------------------------------------------------------------------
# 0.5 Load Packages

    loadPackage( 'tools' )

# ---------------------------------------------------------------------------
# 1. Reading data and mapppings into script

# Read in all files in the EF_parameters folder
    files_list <- list.files( path = './default-emissions-data/EF_parameters',
                              pattern = '*.csv' )
    files_list <- tools::file_path_sans_ext( files_list )

# select files with "_EF"
    EF_file_list <- files_list[ grep( pattern = "_EF", files_list ) ]

# Deselect files with "metadata"
    EF_file_list <- EF_file_list[ -grep( pattern = "metadata", EF_file_list ) ]

# Files with the emission in the name
    EF_file_list <- EF_file_list[ grep( pattern = paste0( '\\.', em ), EF_file_list ) ]

# Read in all files
    EF_list <- lapply ( X = EF_file_list, FUN = readData,
                        domain = "DEFAULT_EF_PARAM" )
# ---------------------------------------------------------------------------
# 2. Expand "all" variable and extend over time, convert list to one df

# Expand all, interpolate and Extend forward and back
    EF_extended <- lapply( X = EF_list, FUN = extendDefaultEF,
                           pre_ext_method_default = 'none' )

# Add all EFs to a single dataframe
    EF <- do.call( "rbind.fill", EF_extended )

    EF$units <- 'kt/kt'
# ---------------------------------------------------------------------------
# 2. Add to existing parameter Dbs

# If there are any emissions, add to the dataframe and overwrite; also write out
# a diagnostic file recording the user-defined emissions. Otherwise, print a
# message that no emissions were added
    if ( length( EF_list ) > 0 ) {
        printLog( paste( 'Adding new data to existing emission factor',
                         'data base for', em ) )
        writeData( EF, 'DIAG_OUT',
                   paste0( 'B.', em, '_comb_User_Added_EF' ) )
        addToDb_overwrite( new_data = EF, em = em,
                           file_extension = 'comb_EF_db' )
    } else {
        printLog( paste( 'No data to be added to existing EF database for ', em ) )
    }

    logStop()
# END
