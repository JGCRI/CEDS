# ------------------------------------------------------------------------------
# Program Name: E.REAS32_emissions.R
# Author(s): Rachel Hoesly, Leyang Feng
# Date Last Updated: April 19, 2021
# Program Purpose: To read in & reformat REAS emissions data.
# Input Files: All REAS data (v3.2)
# Output Files: E.em_REAS_inventory.csv
# TODO:
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "analysis_functions.R",
                  'interpolation_extension_functions.R' ) # Any additional function files required
    log_msg <- "Initial reformatting of REAS Emissions" # First message to be printed to the log
    script_name <- "E.REAS32_emissions.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )
# Describes which emission species is being analyzed
    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[1]
    if ( is.na( em ) ) em <- "SO2"

# ------------------------------------------------------------------------------
# 0.5 Settings/Load Files & Convert all txt files to csv
#     logging does not support txt files, so convert to csv

    MCL <- readData( "MAPPINGS", "Master_Country_List" )
    loadPackage( 'tools' )


# Stop script if running for unsupported emissions species
    if ( em %!in% c( 'BC', 'CH4', 'CO', 'CO2', 'NH3', 'N2O', 'NMVOC', 'NOx',
                     'OC', 'SO2' ) ) {
        stop ( paste( 'REAS script is not supported for emission species', em ) )
    }


# ------------------------------------------------------------------------------
# 1. Read in Data

    # list files in the folder
    reas_dir <- paste0('./emissions-inventories/REASv3.2/',em, '/')
    files <- list.files( reas_dir, pattern = paste0('WC_',em,'.txt' ) )

# define function to read in and process single file

    read_process_reas <- function ( file_name ) {

    # read files and assign column names
      read_in_data <- read.table( paste0(reas_dir, file_name ),
                                  stringsAsFactors = FALSE,
                                  col.names = c('sector', paste0('X', 1950:2015)),
                                  strip.white = T,
                                  fill = T )

    # remove rows with na
      read_in_data <- tidyr::drop_na(read_in_data)

    # set certain year columns to numeric
      read_in_data[2:6] <- lapply(read_in_data[2:6], as.numeric)

    # add iso column
      read_in_data$iso <- tolower(strsplit(file_name, "[_]")[[1]][4])

    # add units column
      read_in_data$units <- "kt"

    # rearrange columns
      read_in_data <- dplyr::select(read_in_data, iso, sector, units, X1950:X2015)

      return ( read_in_data )
    }


    if ( length( files ) > 0 ) {
    # apply function to list of files
        reas_data_list <- lapply( X = files , FUN = read_process_reas )

    # bind all data together
        reas_data <- do.call( "rbind", reas_data_list )


   } else {
   # if no data to process for this emissions species, create dummy file.
       reas_data <- data.frame()
   }

# ------------------------------------------------------------------------------
# 2. Output
# Write Data:
    writeData( reas_data, domain = "MED_OUT",
               fn = paste0( "E.", em, "_REAS32_inventory" ), meta = TRUE )

# Diagnostic files:
    writeData( unique( reas_data$iso ), domain = "DIAG_OUT",
               fn = paste0( "E.", em, "_REAS32_countries" ), meta = FALSE )
    writeData( unique( reas_data$sector ), domain = "DIAG_OUT",
               fn = paste0( "E.", em, "_REAS32_sectors" ), meta = FALSE )

# Every script should finish with this line-
    logStop()

# END
