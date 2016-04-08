# ------------------------------------------------------------------------------
# Program Name: H1.1.base_activity.R
# Author: Rachel Hoesly
# Date Last Modified: 7 April 2016
# Program Purpose: Extend CEDS activity backward
# Input Files:  A.total_activity.csv, A.intl_shipping_en.csv
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
if ( is.na( em ) ) em <- "NH3"

# ---------------------------------------------------------------------------
# 1. Load Data

ceds_activity <- readData( 'MED_OUT', paste0( 'A.total_activity' ) )
shipping_fuel <- readData( 'MED_OUT', 'A.intl_shipping_en' )


# ---------------------------------------------------------------------------
# 2. Extend Data frame

ceds_activity[ paste0('X', historical_pre_extension_year: 1959)] <- NA
ceds_activity <- ceds_activity[ c( 'iso' , 'sector' , 'fuel' , 'units' , X_extended_years ) ]
ceds_activity[ which( ceds_activity$sector == '1A3di_International-shipping'), paste0('X',1750:1959) ] <- 0

# ---------------------------------------------------------------------------
# 3. Add Shipping Fuel
ceds_activity <- replaceValueColMatch( ceds_activity, shipping_fuel,
                                       x.ColName = paste0('X',1750:1959),
                                       match.x = c('iso','sector','fuel','units'),
                                       addEntries = F)

# ---------------------------------------------------------------------------
# 3. Output

 writeData( ceds_activity, "MED_OUT" , paste0('H.',em,'_total_activity_extended_db'))

 logStop()

