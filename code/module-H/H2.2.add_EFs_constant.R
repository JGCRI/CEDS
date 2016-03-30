# ------------------------------------------------------------------------------
# Program Name: H2.2.add_EFs_constant.R
# Author: Rachel Hoesly
# Program Purpose: 
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
log_msg <- "Extend EFs back to 1750 with constant values" # First message to be printed to the log
script_name <- "H2.2.add_EFs_constant.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "BC"

# ---------------------------------------------------------------------------
# 1. Load Data

ceds_EFs <- readData( 'MED_OUT', paste0('H.',em,'_total_EFs_extended_db') )  
extension_drivers_EF<- readData("EXT_IN", 'CEDS_historical_extension_methods_EF')

# ---------------------------------------------------------------------------
# 1. Constant Extention
ceds_EF_extended <- ceds_EFs

constant <- extension_drivers_EF[which( extension_drivers_EF$method == 'constant'),c('sector','fuel')]
ceds_EF_extended[ which( paste(ceds_EF_extended$sector ,ceds_EF_extended$fuel,sep="-") %in% paste(constant$sector ,constant$fuel,sep="-")) , 
                  paste0('X', historical_pre_extension_year: (historical_end_extension_year))] <- 
          ceds_EF_extended[ which( paste(ceds_EF_extended$sector ,ceds_EF_extended$fuel,sep="-") %in% paste(constant$sector ,constant$fuel,sep="-")) , 
                            paste0('X', (historical_end_extension_year+1))]

ceds_EF_extended <- ceds_EF_extended[ c( 'iso' , 'sector' , 'fuel' , 'units' , X_extended_years ) ]

# ---------------------------------------------------------------------------
# 4. Output

writeData( ceds_EF_extended, "MED_OUT" , paste0('H.',em,'_total_EFs_extended_db'))

logStop()

