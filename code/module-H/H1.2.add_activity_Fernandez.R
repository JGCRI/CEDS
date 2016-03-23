# ------------------------------------------------------------------------------
# Program Name: H1.2.add_activity_Fernandez.R
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
headers <- c( "data_functions.R","process_db_functions.R") # Additional function files may be required.
log_msg <- "Extending CEDS activity_data before 1960 with Fernandez biomass data" # First message to be printed to the log
script_name <- "H1.2.add_activity_Fernandez.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "OC"

# ---------------------------------------------------------------------------
# 0.5 Load Packages


# ---------------------------------------------------------------------------
# 1. Load files

activity_all <- readData( 'MED_OUT',paste0('H.',em,'_total_activity_extended_db') )
extension_drivers_all <- readData("EXT_IN", 'CEDS_historical_extension_drivers_activity')
biomass_fernandes <- readData('MED_OUT','A.Fernandes_residential_biomass')

# ---------------------------------------------------------------------------
# 2. Select data to extend based on extension drivers

extension_drivers <- extension_drivers_all[which( extension_drivers_all$driver_data_source == 'Fernandez' ), ]
activity <- activity_all

# ---------------------------------------------------------------------------
# 3. Driver data processing

biomass <- biomass_fernandes[,c('iso','year','consumption')]
biomass$X_year <- paste0('X',biomass$year)
biomass <- cast(biomass, iso ~ X_year, value = 'consumption',
                fun.aggregate = sum)

# ---------------------------------------------------------------------------
# 4. Extend Data


drivers <- extension_drivers
year_intervals <- unique(paste(drivers$ext_start_year,drivers$ext_end_year,sep='-'))

for (i in seq_along(year_intervals)) {
  
  interval <- year_intervals[i]
  
  drivers <- extension_drivers[ which( extension_drivers$driver_data_source == 'Fernandez' &
                                       paste(extension_drivers$ext_start_year,extension_drivers$ext_end_year,sep='-') == interval ), ]
  
  ratio_year <- unique(drivers[,'ext_end_year'])
  ext_start_year <- unique(drivers[,'ext_start_year'])
  extention_years <- paste0('X',ext_start_year:ratio_year)
  
  # select extension data for current method
  sectors <- drivers[, c('sector','fuel') ]
  sectors <- paste(sectors$sector,sectors$fuel,sep='-')
  
  # select ceds data to extend
  ceds_extention_ratios <- activity[ which( paste(activity$sector, activity$fuel, sep="-") %in% sectors  ) , ]
  
  #extended data template
  ceds_extention_ratios <- ceds_extention_ratios[,c('iso','sector','fuel',paste0('X',ratio_year))]
  names(ceds_extention_ratios)[which(names(ceds_extention_ratios) == paste0('X',ratio_year))] <- 'CEDS_ratio_year'
  
  # add Driver identifyer ratio year
  ceds_extention_ratios <- merge(ceds_extention_ratios, biomass[,c("iso", paste0('X',ratio_year))],
                                 by.x = c('iso'),
                                 by.y = c("iso"),
                                 all.x = TRUE, all.y = FALSE)
  names(ceds_extention_ratios)[which(names(ceds_extention_ratios) == paste0('X',ratio_year))] <- 'CDIAC_ratio_year'
  
  
  # calculate ratio
  ceds_extention_ratios$ratio <- ceds_extention_ratios$CEDS_ratio_year/ceds_extention_ratios$CDIAC_ratio_year
  # make all infinite ratios zero
  ceds_extention_ratios[!is.finite(ceds_extention_ratios$ratio) , 'ratio'] <- 0
  
  # add driver data and use ratio to calculate extended value
  ceds_extended <- ceds_extention_ratios[,c('iso','fuel','sector','ratio')]
  ceds_extended[extention_years] <- biomass[ match( ceds_extended$iso , biomass$iso )
                                             ,extention_years]
  ceds_extended[is.na(ceds_extended)] <- 0
  
  # calculate extended data
  ceds_extended[ extention_years ] <- ceds_extended$ratio * ceds_extended[ extention_years ]
  
  # add to final extention template
  activity <- replaceValueColMatch(activity, ceds_extended,
                                              x.ColName = extention_years,
                                              match.x = c('iso','sector','fuel'),
                                              addEntries = FALSE)

  }

# ---------------------------------------------------------------------------
# 5. Write to database

if( !(nrow(activity_all) == nrow(activity) & ncol(activity_all) == ncol(activity) ) ){ 
  stop( "New and old activity do not match") } else{
writeData( activity, "MED_OUT" , paste0('H.',em,'_total_activity_extended_db')) }

logStop()


