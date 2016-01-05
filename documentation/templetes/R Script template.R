## Template Notes (Marked by ##)
## Fill out script info in section below.
##      - Input Files does not need to include header and function files.
## Alter neccessary fields in section "0":
##      - headers
##      - log_msg
##      - script_name
# ------------------------------------------------------------------------------
# Program Name: 
# Author(s): 
# Date Last Updated: 
# Program Purpose:     
# Input Files: 
# Output Files: 
# Notes: 1. 
#        2. 
# TODO: 
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Set working directory to the CEDS “input” directory and define PARAM_DIR as the
# location of the CEDS “parameters” directory, relative to the new working directory.
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
# provides logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R" ) # Any additional function files required
log_msg <- "short description of code" # First message to be printed to the log
script_name <- "xxxx.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Read in files
## use the readData() function (defined in IO_functions.R) for all input data
# 
# ------------------------------------------------------------------------------
# 2.  
# 
# ------------------------------------------------------------------------------
# X. Output
## use the writeData() function (defined in IO_functions.R) to output any data

## Every script should finish with this line-
## using system functions after calling logStop() causes errors
logStop()

# END
