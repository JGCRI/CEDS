#------------------------------------------------------------------------------
# Program Name: B1.1.base_comb_EF_control_percent.R
# Authors: Rachel Hoesly
# Date Last Updated: Nov 24, 2015
# Program Purpose: Create default control percentage Db for all emission species
# Input Files: A.comb_activity
# Output Files: B.[em]_ControlFrac_db
# Notes: 
# TODO: 
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Before we can load headers we need some paths defined. They may be provided by
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
headers <- c( "data_functions.R", "analysis_functions.R" ) # Additional function files required.
log_msg <- "Create default control_percentage" # First message to be printed to the log
script_name <- "B1.1.base_comb_EF_control_percent.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )
# ------------------------------------------------------------------------------
# 1. Read in files
# for running directly from R - defines emission species (normally from command line)
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"

# "em" is defined from parent script
em_lc <- tolower( em )

activity_data <- readData( "MED_OUT", "A.comb_activity" )

# ------------------------------------------------------------------------------
# 2. Create default sulfur content, ash retention, control percentage databases

# Create default control fraction database
default_ControlFrac <- activity_data
# The default for control percentage is 0
default_ControlFrac[,5:ncol(default_ControlFrac)] <- 0

#No unit for sulfur content percentage
default_ControlFrac$units <- 'percent'

# ------------------------------------------------------------------------------
# 4. Output    

# Write out all three default databases
writeData( default_ControlFrac, "MED_OUT", paste0( "B.", em ,"_", "ControlFrac_db") )


# Every script should finish with this line
logStop()
# END