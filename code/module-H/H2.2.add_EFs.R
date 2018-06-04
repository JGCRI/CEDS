#------------------------------------------------------------------------------
# Program Name: H2.1.add_EFs.R
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
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c('data_functions.R') # Additional function files required.
log_msg <- paste0( "Calling species-specific child script to extend CEDS EFs data" ) # First message to be printed to the log
script_name <- "H2.1.add_EFs.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "CO2"

# ------------------------------------------------------------------------------------
# 0.5 Load Packages, Define Functions
  loadPackage('tools')

  # Create a function that can be applied to source all child scripts for the given
  # emissions type.
  MODULE_H <- "../code/module-H/"
  source_child <- function( file_name ){ source( paste( MODULE_H, file_name, sep = "" ) ) }

# ------------------------------------------------------------------------------------
# 1. Load Data

  # extension_drivers <- readData("EXT_IN", 'CEDS_historical_extension_drivers_activity')

# ------------------------------------------------------------------------------------
# 2. Check driver document and scripts for completeness



# ------------------------------------------------------------------------------------

printLog('Running add_activity scripts to extend activity data')
# Set scripts to generate extended activity data (species independent)


  scripts <- c( 'H2.2.add_EFs_Emissions-trend.R' ,
                'H2.2.add_EFs_EF-trend.R',
                'H2.2.add_EFs_EF-converge.R',
                'H2.2.add_EFs_constant.R',
                'H2.2.add_EFs_default.R')


# Run all child scripts for the given emissions type. The call to
# invisible() prevents extraneous output from appearing in the console.
# lapply calls source_child on each script in the list.
invisible( lapply( scripts, source_child ) )

logStop()
# END
