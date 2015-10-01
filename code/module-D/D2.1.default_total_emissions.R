#------------------------------------------------------------------------------
# Program Name: D2.1.default_total_emissions.R
# Author(s): Jon Seibert
# Date Last Modified: July 7, 2015
# Program Purpose: To fill out missing sections in the process emissions database
#                  and combine it with combustion emissions data to produce total
#                  emissions data.
# Input Files: D.[em]_default_comb_emissions.csv, C.[em]_NC_emissions.csv
# Output Files:  D.[em]_default_total_emissions.csv
# Notes: 
# TODO:
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Before we can load headers we need some paths defined. They may be provided by
#   a system environment variable or may have already been set in the workspace.
# Set variable PARAM_DIR to be the data system directory
dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
for ( i in 1:length( dirs ) ) {
  setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
  wd <- grep( 'CEDS/input', list.dirs(), value = T )
  if ( length( wd ) > 0 ) {
    setwd( wd[ 1 ] )
    break
    
  }
}
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R" ) # Additional function files required.
log_msg <- "Integration of process emissions data" # First message to be printed to the log
script_name <- "D2.1.default_total_emissions.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Read in files

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"
em_lc <- tolower( em )

default_emissions <- readData( "MED_OUT", paste0( "D.", em, "_default_comb_emissions" ) )

emissions_data <- readData( "MED_OUT", paste0( "C.", em, "_", "NC", "_emissions" ), meta = FALSE )

# ----------------------------------------------------------------------------
# 2.  Combine with default combustion emissions data

total_default_em <- rbind( emissions_data, default_emissions )
total_default_em <- total_default_em[ with( total_default_em, order( iso, sector, fuel ) ), ]


# ------------------------------------------------------------------------------
# 4. Output

writeData(total_default_em, domain = "MED_OUT", fn = paste0( "D.", em, "_default_total_emissions" ), meta = TRUE )

logStop()
# END
