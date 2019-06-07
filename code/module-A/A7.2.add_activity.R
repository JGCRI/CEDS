#------------------------------------------------------------------------------
# Program Name: A7.2.add_activity.R
# Author: Rachel Hoesly
# Date Last Updated: 28 June 2016
# Program Purpose: To select and run the correct script(s) to extend CEDS activity data.
# Input Files: None
# Output Files: None
# Notes:
# TODO: section 2 - Check CEDS_historical_extension_drivers_activity.csv to ensure all sectors/fuels/time
# are covered by a method
# ------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c('data_functions.R') # Additional function files required.
log_msg <- paste0( "Calling species-specific child script to extend CEDS activity data" ) # First message to be printed to the log
script_name <- "A7.2.add_activity.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------------
# 0.5 Load Packages, Define Functions
  loadPackage('tools')

  # Create a function that can be applied to source all child scripts for the given
  # emissions type.
  MODULE_A <- "../code/module-A/"
  source_child <- function( file_name ){ source( paste( MODULE_A, file_name, sep = "" ) ) }

# ------------------------------------------------------------------------------------
# 1. Load Data

  extension_drivers <- readData("EXT_IN", 'CEDS_historical_extension_drivers_activity')

# ------------------------------------------------------------------------------------
# 2. Check driver document and scripts for completeness

  # Check CEDS_historical_extension_drivers_activity.csv to ensure all sectors/fuels/time
  # are covered by a method


  # Check CEDS_historical_extension_drivers_activity.csv to ensure methods are valid, with
  # corresponding scripts

  printLog('Checking historical extension methods.')
  all_drivers <- unique(extension_drivers$driver_data_source)

  # Get list of extension (add_activity)
  driver_scripts <- list.files(path =  '../code/module-A',pattern = 'A7.2.add_activity_*')
  driver_scripts <- tools::file_path_sans_ext( driver_scripts )
  driver_scripts <- gsub("A7.2.add_activity_","",driver_scripts)
  driver_scripts <- driver_scripts[ driver_scripts %!in% c("A7.2.add_activity") ]

  drivers_without_scripts <- all_drivers[ all_drivers %!in% c(driver_scripts,NA) ]
  scripts_without_drivers <- driver_scripts[ driver_scripts %!in% c(all_drivers) ]
  if( length(scripts_without_drivers) > 0 ) scripts_without_drivers <- paste0('A7.2.add_activity_',scripts_without_drivers,'.R')

  if ( length(drivers_without_scripts)>0 & length(scripts_without_drivers) == 0 ) {
          stop(paste0("The following extension drivers are specified in extension/CEDS_historical_extension_drivers.csv without corresponding scripts: ",
                      paste(drivers_without_scripts, collapse=" , " )))}

  if ( length(drivers_without_scripts)>0 & length(scripts_without_drivers) > 0 ) {
          stop(paste0("The following extension drivers are specified in extension/CEDS_historical_extension_drivers.csv without corresponding scripts: ",
               paste(drivers_without_scripts, collapse=" , " ) , ". The following scripts exists without specified extension drivers: " ,
               paste(scripts_without_drivers, collapse=" , " ) , ".")
               )}

# ------------------------------------------------------------------------------------

printLog('Running add_activity scripts to extend activity data')
# Set scripts to generate extended activity data (species independent)

# scripts have to be called in order of time period, most recent to most historical -
# Bond-CDIAC must be called before CDIAC fuels

  scripts <- c( "A7.2.add_activity_population.R",
                "A7.2.add_activity_pulp_paper_consumption.R",
                "A7.2.add_activity_CDIAC.R")


# Run all child scripts for the given emissions type. The call to
# invisible() prevents extraneous output from appearing in the console.
# lapply calls source_child on each script in the list.
invisible( lapply( scripts, source_child ) )

logStop()
# END
