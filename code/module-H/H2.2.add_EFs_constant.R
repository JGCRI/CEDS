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
if ( is.na( em ) ) em <- "NOx"

# ---------------------------------------------------------------------------
# 1. Load Data

ceds_EFs <- readData( 'MED_OUT', paste0('H.',em,'_total_EFs_extended_db') )  
extension_drivers_EF<- readData("EXT_IN", 'CEDS_historical_extension_methods_EF')

# ---------------------------------------------------------------------------
# 1. Constant Extention

trend <- 'constant'

# Expand fuels - all-comb
expand <- extension_drivers_EF[which(extension_drivers_EF$fuel == 'all-comb' ) ,]
extension_drivers_EF <- extension_drivers_EF[which(extension_drivers_EF$fuel != 'all-comb' ) ,]
comb_fuels <- c('biomass', 'hard_coal','brown_coal','coal_coke','natural_gas','heavy_oil','diesel_oil','light_oil')
for (i in seq_along(comb_fuels)){
  expand$fuel <- rep(comb_fuels[i], times= nrow(expand) )
  extension_drivers_EF <- rbind( extension_drivers_EF, expand )
}
extension_drivers_EF <- extension_drivers_EF[ which( extension_drivers_EF$em %in% c(em , 'all' )), ]

# delete, all row for a sector-fuel if there is a sector-fuel entry for the specific emission species
driver_em <- extension_drivers_EF[which( extension_drivers_EF$em == em), ]
if( nrow(driver_em) > 0 ){
  em_instruction <- unique( paste( driver_em$sector,driver_em$fuel,driver_em$start_year,driver_em$end_year  ,sep = '-'))
  extension_drivers_EF <- extension_drivers_EF[ which( 
    paste( extension_drivers_EF$sector, extension_drivers_EF$fuel , extension_drivers_EF$start_year, extension_drivers_EF$end_year, extension_drivers_EF$em, sep = '-') %!in%  
      paste( em_instruction ,'all' ,sep = '-') ), ]
}

# select em
extension_drivers_EF$em <- em

# select method
extension_drivers_EF <- extension_drivers_EF[ which( extension_drivers_EF$method == trend ) ,]

drivers <- extension_drivers_EF


# ---------------------------------------------------------------------------
# 1. Constant Extention

ceds_EF_extended <- ceds_EFs

intervals <- unique(paste(drivers$start_year,drivers$end_year, sep = '-'))

for ( i in seq_along(intervals)){
  
  constant <- drivers[ which( paste(drivers$start_year, drivers$end_year ,sep = '-') %in% intervals[i] ), ]
  
  ceds_EF_extended[ which( paste(ceds_EF_extended$sector ,ceds_EF_extended$fuel,sep="-") %in% paste(constant$sector ,constant$fuel,sep="-")) , 
                  paste0('X', historical_pre_extension_year: (historical_end_extension_year))] <- 
          ceds_EF_extended[ which( paste(ceds_EF_extended$sector ,ceds_EF_extended$fuel,sep="-") %in% paste(constant$sector ,constant$fuel,sep="-")) , 
                            paste0('X', (historical_end_extension_year+1))]

  ceds_EF_extended <- ceds_EF_extended[ c( 'iso' , 'sector' , 'fuel' , 'units' , X_extended_years ) ]

}

# ---------------------------------------------------------------------------
# 4. Output

writeData( ceds_EF_extended, "MED_OUT" , paste0('H.',em,'_total_EFs_extended_db'))

logStop()

