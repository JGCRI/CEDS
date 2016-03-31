# ------------------------------------------------------------------------------
# Program Name: H2.2.add_EFs_EF-converge.R
# Author: Rachel Hoesly
# Program Purpose: extend EF back by converging to a given value
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
log_msg <- "Converging EFs to given values" # First message to be printed to the log
script_name <- "H2.2.add_EFs_EF-converge.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "NOx"

# ---------------------------------------------------------------------------
# 1. Load Package

loadPackage('zoo')

# ---------------------------------------------------------------------------
# 1. Load Data

ceds_EFs <- readData( 'MED_OUT', paste0('H.',em,'_total_EFs_extended_db') )  
extension_drivers_EF<- readData("EXT_IN", 'CEDS_historical_extension_methods_EF')

CO_converge <- readData( domain = 'EXT_IN', domain_extension = 'extention-data/', file_name = 'regional_biomass_CO_converge')

# ---------------------------------------------------------------------------
# 2. Select relavent driver-methods

trend <- 'Emissions-converge'

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

if ( nrow( extension_drivers_EF) > 0 ) {

drivers_EFconverge <- extension_drivers_EF


# ---------------------------------------------------------------------------
# 3. Select, extend, and merge

drivers_EFconverge <- unique( drivers_EFconverge[ , c('sector','fuel','start_year','end_year','converge_year','converge_value')] )

for ( i in seq_along(drivers_EFconverge$sector)){
drivers <- drivers_EFconverge[i,]

selected_EFs <- ceds_EFs[which( paste( ceds_EFs$sector, ceds_EFs$fuel , sep = '-') %in%  
                                  paste( drivers$sector, drivers$fuel , sep = '-')), 
                         c('iso','sector','fuel', paste0('X',drivers[1,'start_year']: (drivers[1,'end_year']+1)))]
selected_EFs [ paste0('X',drivers[1,'start_year']: drivers[1,'end_year']) ] <- NA

# insert converge value
selected_EFs [ paste0('X',drivers[1,'converge_year']) ]  <- drivers[1,'converge_value'] 
# interpolate
selected_EFs [ paste0('X', drivers[1,'converge_year']: (drivers[1,'end_year']+1) ) ] <- 
    t( na.approx( t(selected_EFs [ paste0('X', drivers[1,'converge_year']: (drivers[1,'end_year']+1) ) ] ) ) )
# extend
selected_EFs[paste0('X',drivers[1,'start_year']: (drivers[1,'end_year'])) ] <- 
  t(na.locf(t(selected_EFs[ paste0('X',drivers[1,'start_year']: (drivers[1,'end_year'])) ]),fromLast = T))

# add to final extention template
ceds_EFs <- replaceValueColMatch(ceds_EFs, selected_EFs,
                                 x.ColName = paste0('X',drivers[1,'start_year']: (drivers[1,'end_year'])) ,
                                 match.x = c('iso','sector','fuel'),
                                 addEntries = FALSE)


}
}
# ---------------------------------------------------------------------------
# 4. Converge to Values
if( em == 'CO'){
  
  res_biomass <- ceds_EFs[which( ceds_EFs$iso %in% CO_converge$iso &
                                 ceds_EFs$sector == '1A4b_Residential' &
                                 ceds_EFs$fuel =='biomass'), c( 'iso','sector','fuel', paste0('X',1750: 1965) )   ]
  res_biomass[ paste0('X',1750: 1964) ] <- NA
  res_biomass[ paste0('X', 1950) ] <- CO_converge[match( res_biomass$iso, CO_converge$iso),'CO_1920']
  res_biomass[ paste0('X', 1950: 1965) ] <- t( na.approx ( t(res_biomass[ paste0('X', 1950: 1965) ]) ))
  res_biomass[ paste0('X', 1750: 1950) ] <- t( na.locf ( t(res_biomass[ paste0('X', 1750: 1950) ]) , fromLast = T) )
  
  ceds_EFs <- replaceValueColMatch(ceds_EFs, res_biomass,
                                   x.ColName = paste0('X',1750: 1965) ,
                                   match.x = c('iso','sector','fuel'),
                                   addEntries = FALSE)
}

# ---------------------------------------------------------------------------
# 5. Output

writeData( ceds_EFs, "MED_OUT" , paste0('H.',em,'_total_EFs_extended_db'))

logStop()







