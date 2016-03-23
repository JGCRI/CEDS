# ------------------------------------------------------------------------------
# Program Name: H1.1.base_activity.R
# Author: Rachel Hoesly
# Program Purpose: Extend CEDS activity backward
#               
# Output Files:
# TODO: 
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers

# Set working directory
dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
for ( i in 1:length( dirs ) ) {
  setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
  wd <- grep( 'CEDS/input', list.dirs(), value = T )
  if ( length(wd) > 0 ) {
    setwd( wd[1] )
    break
    
  }
}
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R") # Additional function files may be required.
log_msg <- "Creating database for CEDS activity_data extention before 1960" # First message to be printed to the log
script_name <- "H1.1.base_activity.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "OC"

# ---------------------------------------------------------------------------
# 1. Load Data

ceds_activity <- readData( 'MED_OUT', paste0( 'A.total_activity' ) )

# ---------------------------------------------------------------------------
# 2. Extend Data frame

ceds_activity[ paste0('X', historical_pre_extension_year: 1959)] <- NA
ceds_activity <- ceds_activity[ c( 'iso' , 'sector' , 'fuel' , 'units' , X_extended_years ) ]

# ---------------------------------------------------------------------------
# 3. Output

 writeData( ceds_activity, "MED_OUT" , paste0('H.',em,'_total_activity_extended_db'))

 logStop()

