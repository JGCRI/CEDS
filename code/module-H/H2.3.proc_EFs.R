#------------------------------------------------------------------------------
# Program Name: H2.3.proc_EFs.R
# Author: Rachel Hoesly
# Date Last Updated: March 22, 2016
# Program Purpose: Process extendtion EFs database to finalize and sort CEDS EFs database.
# Input Files: None
# Output Files: None
# Notes:
# TODO:
# ------------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "process_db_functions.R" ) # Additional function files required.
log_msg <- paste0( "Processing CEDS extension EFs database" ) # First message to be printed to the log
script_name <- "H2.3.proc_EFs"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "OC"

# ------------------------------------------------------------------------------------

loadPackage('zoo')

# ---------------------------------------------------------------------------
# 1. Load files

EFs_all <- readData( 'MED_OUT',paste0('H.',em,'_total_EFs_extended_db') )

# ---------------------------------------------------------------------------
# 2. Sort

EFs_all[ X_extended_years ] <- t(na.locf(t(EFs_all[ X_extended_years ]), fromLast = TRUE))

# EFs_all <- replace( EFs_all, is.na(EFs_all), 0)

if(anyNA( EFs_all )) stop("There are NAs in final activity database.")

final <- EFs_all[ with( EFs_all, order( iso, sector, fuel ) ), ]

# ---------------------------------------------------------------------------
# 5. Write to file

writeData( final, "MED_OUT" , paste0('H.',em,'_total_EFs_extended'))

logStop()
