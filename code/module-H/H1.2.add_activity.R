#------------------------------------------------------------------------------
# Program Name: H1.2.add_activity.R
# Author: Rachel Hoesly
# Date Last Updated: March 22, 2016
# Program Purpose: To select and run the correct script(s) to extend CEDS activity data.
# Input Files: None
# Output Files: None
# Notes: 
# TODO: section 2 - Check CEDS_historical_extension_drivers_activity.csv to ensure all sectors/fuels/time
# are covered by a method
# ------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------
# Before we can run other scripts we need some paths defined. They may be provided by
#   a system environment variable or may have already been set in the workspace.
dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
for ( i in 1:length( dirs ) ) {
  setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
  wd <- grep( 'CEDS/input', list.dirs(), value = T )
  if ( length( wd ) > 0 ) {
    setwd( wd[ 1 ] )
    break
    
  }
}

PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
headers <- c('data_functions.R') # Additional function files required.
log_msg <- paste0( "Calling species-specific child script to extend CEDS activity data" ) # First message to be printed to the log
script_name <- "H1.2.add_activity.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "OC"

# ------------------------------------------------------------------------------------
# 0.5 Load Packages, Define Functions
  loadPackage('tools')
  
  # Create a function that can be applied to source all child scripts for the given
  # emissions type.
  MODULE_H <- "../code/module-H/"
  source_child <- function( file_name ){ source( paste( MODULE_H, file_name, sep = "" ) ) }

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
  
  # Get list of extention (add_activity)
  driver_scripts <- list.files(path =  '../code/module-H',pattern = 'H1.2.add_activity_*')
  driver_scripts <- file_path_sans_ext( driver_scripts )
  driver_scripts <- gsub("H1.2.add_activity_","",driver_scripts)
  driver_scripts <- driver_scripts[ driver_scripts %!in% c("H1.2.add_activity") ]
  
  drivers_without_scripts <- all_drivers[ all_drivers %!in% c(driver_scripts,NA) ]
  scripts_without_drivers <- driver_scripts[ driver_scripts %!in% c(all_drivers) ]
  if( length(scripts_without_drivers) > 0 ) scripts_without_drivers <- paste0('H1.2.add_activity_',scripts_without_drivers,'.R')
  
  if ( length(drivers_without_scripts)>0 & length(scripts_without_drivers) == 0 ) {
          stop(paste0("The following extension drivers are specified in extention/CEDS_historical_extension_drivers.csv without corresponding scripts: ", 
                      paste(drivers_without_scripts, collapse=" , " )))}
  
  if ( length(drivers_without_scripts)>0 & length(scripts_without_drivers) > 0 ) {
          stop(paste0("The following extension drivers are specified in extention/CEDS_historical_extension_drivers.csv without corresponding scripts: ", 
               paste(drivers_without_scripts, collapse=" , " ) , ". The following scripts exists without specified extension drivers: " ,
               paste(scripts_without_drivers, collapse=" , " ) , ".")
               )}

# ------------------------------------------------------------------------------------

printLog('Running add_activity scripts to extend activity data')
# Set scripts to generate extended activity data (species independent)

# scripts have to be called in order of time period, most recent to most historical - 
# Bond-CDIAC must be called before CDIAC fuels

  scripts <- c( "H1.2.add_activity_CDIAC-Bond.R" , 
                "H1.2.add_activity_CDIAC.R" ,
                "H1.2.add_activity_Fernandez.R" , "H1.2.add_activity_population.R"   )


# Run all child scripts for the given emissions type. The call to
# invisible() prevents extraneous output from appearing in the console.
# lapply calls source_child on each script in the list.
invisible( lapply( scripts, source_child ) )

logStop()
# END
