# ------------------------------------------------------------------------------
# Program Name: A7.2.add_activity_CDIAC.R
# Author: Rachel Hoesly
# Program Purpose: Extend CEDS activity backward with CDIAC data
# Date Last Updated: August 30, 2019
# Input Files: A.NC_activity_extended_db.csv, CEDS_historical_extension_drivers_activity.csv
#              CDIAC_fuel_map.csv, E.CO2_CDIAC_inventory.csv, E.CO2_CDIAC_solid_fuel_cumulative.csv
# Output Files: A.NC_activity_extended_db.csv
# TODO:
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R","process_db_functions.R") # Additional function files may be required.
log_msg <- "Extending CEDS activity_data before 1960 with CDIAC data" # First message to be printed to the log
script_name <- "A7.2.add_activity_CDIAC.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 1. Load files

activity_all <- readData( 'MED_OUT', 'A.NC_activity_extended_db' )
extension_drivers_all <- readData("EXT_IN", 'CEDS_historical_extension_drivers_activity')

cdiac_fuel_map <- readData('EM_INV', domain_extension = "CDIAC/",  'CDIAC_fuel_map'  )
cdiac <- readData( "MED_OUT" , 'E.CO2_CDIAC_inventory' )
cdiac_solid_fuel_cumulative <- readData( "MED_OUT", "E.CO2_CDIAC_solid_fuel_cumulative" )


# ---------------------------------------------------------------------------
# 2. Check that all methods are addressed in script

# Section in the script that correpsond to "extra_driver_info" in CEDS_historical_extension_drivers.csv
script_methods <- c('fuels','Total_CO2','cement_production','solid_fuels_cumulative','liquid_fuels', 'gas_fuels', 'liquid_and_gas_fuels')

driver_info <- unique(extension_drivers_all[which( extension_drivers_all$driver_data_source %in% c( 'CDIAC') ),
                                            'extra_driver_info'])
if( any( driver_info %!in% script_methods) ) stop('CDIAC Methods specified in CEDS_historical_extension_drivers (extra_driver_info)
                                                  that are not addressed in H1.2.add_activity_CDIAC.R. Please Check.' )

# ---------------------------------------------------------------------------
# 2. Select data to extend based on extension drivers

extension_drivers <- extension_drivers_all[which( extension_drivers_all$driver_data_source %in% c( 'CDIAC') ), ]
activity <- activity_all

# ---------------------------------------------------------------------------
# 3. Driver data processing

# bind cumulative solid fuel to cdiac
cdiac <- rbind( cdiac, cdiac_solid_fuel_cumulative )

# cdiac
cdiac_combustion <- cdiac[ which( cdiac$fuel %in% c("solid_fuels_cumulative" , "liquid_fuels" , "gas_fuels",'liquid_and_gas_fuels') ) , ]
cdiac_cement <- cdiac[ which( cdiac$fuel %in% c("cement_production") ) , ]
cdiac_total <- cdiac[ which( cdiac$fuel %in% c("Total_CO2") ) , ]



# ---------------------------------------------------------------------------
# 3. Extend Combustion Sectors
#    Driver: CDIAC-fuels, CDIAC-solid_fuels_cumulative, CDIAC-liquid_and_gas_fuels

drivers <- extension_drivers[ which( extension_drivers$extra_driver_info %in% c('fuels','solid_fuels_cumulative','liquid_fuels','gas_fuels','liquid_and_gas_fuels')), ]
year_intervals <- unique(paste(drivers$ext_start_year,drivers$ext_end_year,sep='-'))

for (i in seq_along(year_intervals)) {

  interval <- year_intervals[i]

  drivers <- extension_drivers[ which( extension_drivers$extra_driver_info %in% c('fuels','solid_fuels_cumulative','liquid_fuels','gas_fuels','liquid_and_gas_fuels') &
                                       paste(extension_drivers$ext_start_year,extension_drivers$ext_end_year,sep='-') == interval ), ]

  ratio_year <- unique(drivers[,'ext_end_year'])+1
  ext_start_year <- unique(drivers[,'ext_start_year'])
  extension_years <- paste0('X',ext_start_year:unique(drivers[,'ext_end_year']))

  # select extension data for current method
  sectors <- drivers[, c('sector','fuel') ]
  sectors <- paste(sectors$sector,sectors$fuel,sep='-')

  # select ceds data to extend
  ceds_extension_ratios <- activity[ which( paste(activity$sector, activity$fuel, sep="-")
                                                       %in% sectors  ) , ]
  # add "extra_driver_info" - CDIA driver
  ceds_extension_ratios$extra_driver_info <- drivers[ match( paste(ceds_extension_ratios$sector, ceds_extension_ratios$fuel, sep="-") ,
                                                             paste(drivers$sector, drivers$fuel, sep="-")) , 'extra_driver_info']

  #extended data template
  ceds_extension_ratios <- ceds_extension_ratios[,c('iso','sector','fuel','extra_driver_info',paste0('X',ratio_year))]
  names(ceds_extension_ratios)[which(names(ceds_extension_ratios) == paste0('X',ratio_year))] <- 'CEDS_ratio_year'


  # add Driver data - map to CDIAC fuel and ratio year
  ceds_extension_ratios$CDIAC_fuel <- cdiac_fuel_map[ match( ceds_extension_ratios$fuel ,
                                                             cdiac_fuel_map$ceds_fuel ) , 'cdiac_fuel']
  # add "CDIAC fuel" for process data driven by CDIAC fuels
  ceds_extension_ratios[which(ceds_extension_ratios$fuel == 'process' & ceds_extension_ratios$extra_driver_info == 'liquid_fuels'), 'CDIAC_fuel'] <- 'liquid_fuels'
  ceds_extension_ratios[which(ceds_extension_ratios$fuel == 'process' & ceds_extension_ratios$extra_driver_info == 'solid_fuels_cumulative'), 'CDIAC_fuel'] <- 'solid_fuels_cumulative'
  ceds_extension_ratios[which(ceds_extension_ratios$fuel == 'process' & ceds_extension_ratios$extra_driver_info == 'gas_fuels'), 'CDIAC_fuel'] <- 'gas_fuels'
  ceds_extension_ratios[which(ceds_extension_ratios$fuel == 'process' & ceds_extension_ratios$extra_driver_info == 'liquid_and_gas_fuels'), 'CDIAC_fuel'] <- 'liquid_and_gas_fuels'

  # add driver ratio year
  ceds_extension_ratios <- merge(ceds_extension_ratios, cdiac_combustion[,c("iso","fuel", paste0('X',ratio_year))],
                                 by.x = c('iso', 'CDIAC_fuel'),
                                 by.y = c("iso","fuel"),
                                 all.x = TRUE, all.y = FALSE)
  names(ceds_extension_ratios)[which(names(ceds_extension_ratios) == paste0('X',ratio_year))] <- 'CDIAC_ratio_year'

  # calculate ratio
  ceds_extension_ratios$ratio <- ceds_extension_ratios$CEDS_ratio_year/ceds_extension_ratios$CDIAC_ratio_year
  # make all infinite ratios zero
  ceds_extension_ratios[!is.finite(ceds_extension_ratios$ratio) , 'ratio'] <- 0

  # add driver data and use ratio to calculate extended value
  ceds_extended <- ceds_extension_ratios[,c('iso','fuel','sector',"CDIAC_fuel",'ratio')]
  ceds_extended[, extension_years] <- NA
  ceds_extended <- replaceValueColMatch(ceds_extended , cdiac_combustion,
                                        x.ColName = extension_years ,
                                        match.x = c('iso', 'CDIAC_fuel'),
                                        match.y = c("iso","fuel"),
                                        addEntries = FALSE)
  ceds_extended[ extension_years ] <- ceds_extended$ratio * ceds_extended[ extension_years ]
  ceds_extended[is.na(ceds_extended)] <- 0

  # add to final extension template
  activity <- replaceValueColMatch(activity, ceds_extended,
                                              x.ColName = extension_years,
                                              match.x = c('iso','sector','fuel'),
                                              addEntries = FALSE)

}

# ---------------------------------------------------------------------------
# 5. Write to database

if( !(nrow(activity_all) == nrow(activity) & ncol(activity_all) == ncol(activity) ) ){
  stop( "New and old activity do not match") } else{
    writeData( activity, "MED_OUT" , paste0('A.NC_activity_extended_db')) }

logStop()
