# Program Name: B1.1.base_NH3_comb_EF.R
# Author: Rachel Hoesly
# Date Last Updated: 19 Jan 2016 
# Program Purpose: Generate base emission factors
#                  for NH3
#
# Input Files:    files in the EF_parameters folder contailing control_percent and em
#               
# Output Files:  
# Notes: 
#        
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers

# Before we can load headers we need some paths defined. They may be provided by
#   a system environment variable or may have already been set in the workspace.
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
headers <- c( ) 
#                 Additional function files may be required.
log_msg <- "Processing GAINS EMF-30 data. Using as base comb EF where appropriate" 
# First message to be printed to the log
script_name <- 'B1.1.base_NH3_comb_EF.R'

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "NH3"
em_lc <- tolower( em )   

# Stop script if running for unsupported species
if ( em %!in% c('NH3') ) {
  stop (paste( 'not supported for emission species', em, 'remove from script
               list in B1.2.add_comb_EF.R and/or makefile'))
}
# ---------------------------------------------------------------------------
# 1. Load Data

activity_data <- readData( "MED_OUT", "A.comb_activity" )


# ---------------------------------------------------------------------------
# 2. 

default_efs <- activity_data
default_efs[,X_emissions_years] <- 0

# Sort
printLog('Sorting')
base_efs <-  default_efs[ with( default_efs, order( iso, sector, fuel ) ), ]


# ---------------------------------------------------------------------------
# 6. Output

writeData(base_efs, domain = "MED_OUT", fn = paste0('B.',em,'_comb_EF_db'))


logStop()

# END

