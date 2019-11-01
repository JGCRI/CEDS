# ------------------------------------------------------------------------------
# Program Name: A7.2.add_activity_population.R
# Author: Rachel Hoesly
# Date Last Updated: August 30, 2019
# Program Purpose: Extend CEDS activity backward with population data
# Input Files: A.NC_activity_extended_db.csv, CEDS_historical_extension_drivers_activity.csv,
#              A.UN_pop_master.csv
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
log_msg <- "Extending CEDS activity_data before 1960 with population data" # First message to be printed to the log
script_name <- "A7.2.add_activity_population.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 1. Load files

activity_all <- readData( 'MED_OUT', 'A.NC_activity_extended_db' )
extension_drivers_all <- readData("EXT_IN", 'CEDS_historical_extension_drivers_activity')
un_pop <- readData( "MED_OUT" , 'A.UN_pop_master' )

# ---------------------------------------------------------------------------
# 2. Select data to extend based on extension drivers

extension_drivers <- extension_drivers_all[which( extension_drivers_all$driver_data_source == 'population' ), ]
activity <- activity_all

# ---------------------------------------------------------------------------
# 3. Driver data processing

# un population
un_pop$X_year <- paste0( 'X', un_pop$year)
population <- cast( un_pop[which ( un_pop$year %in% historical_pre_extension_year:end_year ) , ] ,
                    iso ~ X_year, value = 'pop')

# ---------------------------------------------------------------------------
# 4. Extend Data

  drivers <- extension_drivers[ which( extension_drivers$driver_data_source == 'population'), ]

  ratio_year <- unique(drivers[,'ext_end_year'])
  ext_start_year <- unique(drivers[,'ext_start_year'])
  extension_years <- paste0('X',ext_start_year:ratio_year)


  # select extension data for current method
  sectors <- drivers[, c('sector','fuel') ]
  sectors <- paste(sectors$sector,sectors$fuel,sep='-')

  # select ceds data to extend
  ceds_extension <- activity[ which( paste(activity$sector, activity$fuel, sep="-") %in% sectors  ) , ]

  # add population
  ceds_extension[extension_years] <- population[match(ceds_extension$iso, population$iso)  , extension_years ]

  # add to final activity
  activity <- replaceValueColMatch(activity, ceds_extension,
                                   x.ColName = extension_years,
                                   match.x = c('iso','sector','fuel'),
                                   addEntries = FALSE)


# ---------------------------------------------------------------------------
# 5. Write to database

if( !(nrow(activity_all) == nrow(activity) & ncol(activity_all) == ncol(activity) ) ){
  stop( "New and old activity do not match") } else{
    writeData( activity, "MED_OUT" , 'A.NC_activity_extended_db' ) }

logStop()
