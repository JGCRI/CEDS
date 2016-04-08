# ------------------------------------------------------------------------------
# Program Name: H1.2.add_activity_Fernandez.R
# Author: Rachel Hoesly
# Program Purpose: Extend CEDS activity backward with Fernandez Biomass data according
#                 to the extend activity driver file
# Input Files:  A.residential_biomass_full,  CEDS_historical_extension_drivers_activity
#               H.EM_total_activity_extended_db 
# Output Files: H.EM_total_activity_extended_db
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
if ( is.na( em ) ) em <- "NH3"

# ---------------------------------------------------------------------------
# 0.5 Load Packages


# ---------------------------------------------------------------------------
# 1. Load files

activity_all <- readData( 'MED_OUT',paste0('H.',em,'_total_activity_extended_db') )
extension_drivers_all <- readData("EXT_IN", 'CEDS_historical_extension_drivers_activity')
biomass_fernandes <- readData('MED_OUT','A.residential_biomass_full')

# ---------------------------------------------------------------------------
# 2. Select data to extend based on extension drivers

extension_drivers <- extension_drivers_all[which( extension_drivers_all$driver_data_source == 'Fernandez' ), ]
activity <- activity_all

# ---------------------------------------------------------------------------
# 3. Driver data processing

biomass <- biomass_fernandes[,c('iso','year','units','ceds_tot_final')]
biomass$X_year <- paste0('X',biomass$year)
biomass <- cast(biomass, iso + units ~ X_year, value = 'ceds_tot_final',
                fun.aggregate = sum)
biomass$fuel <- 'biomass'
biomass$sector <- '1A4b_Residential'


# ---------------------------------------------------------------------------
# 4. Extend Data

year_intervals <- unique(paste(extension_drivers$ext_start_year,extension_drivers$ext_end_year,sep='-'))

if ( length( year_intervals) > 1 ) stop( 'Fernandas residential biomass script cannot handle multiple time intervals, please check activity drivers.')


  # add to final extention template
  activity <- replaceValueColMatch(activity, biomass,
                                              x.ColName = paste0('X',extension_drivers$ext_start_year[1]:extension_drivers$ext_end_year[1]),
                                              match.x = c('iso','sector','fuel'),
                                              addEntries = FALSE)



# ---------------------------------------------------------------------------
# 5. Write to database

if( !(nrow(activity_all) == nrow(activity) & ncol(activity_all) == ncol(activity) ) ){ 
  stop( "New and old activity do not match") } else{
writeData( activity, "MED_OUT" , paste0('H.',em,'_total_activity_extended_db')) }

logStop()


