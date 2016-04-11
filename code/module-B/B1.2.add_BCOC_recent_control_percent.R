#------------------------------------------------------------------------------
# Program Name: B1.2.add_BCOC_recent_EF.R
# Author: Leyang Feng
# Date Last Updated: 30 March 2016
# Program Purpose: Adding EF trends for BCOC control percentage for years after 2010
# Input Files: B.[BC/OC]_comb_EF_GAINS_EMF30
# Output Files: U.[BC/OC]_comb_ControlFrac_db.csv
# Notes: 

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
log_msg <- "Adding EF trends for BCOC control percentage for years after 2010" # First message to be printed to the log
script_name <- "B1.2.add_BCOC_recent_EF.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "OC"

# ------------------------------------------------------------------------------
# 1. Read in files and do preliminary setup

gains_ef_db <- readData( 'DIAG_OUT', paste0( 'B.', em, '_comb_EF_GAINS_EMF30' ) )
# ------------------------------------------------------------------------------
# 2. Recent year ( after 2010 ) %control calculation 

# define recent years 
recent_years <- as.character( 2010 : 2014 )

# extract emissions for recent years 
gains_recent <- gains_ef_db[ , paste0( 'X', recent_years ) ]

# construct ef matrix using 2010 ef following layout of gains_recent 
gains_2010 <- matrix( rep( gains_recent$X2010 , length( recent_years ) ), ncol = length( recent_years ) )

# calculate the recent year %control
recent_controls <- 1 - ( gains_recent / gains_2010 )

# change negative values into 0. Negative values means in GAINS EMF30 data, EF(after2010) > EF(2010)
recent_controls[ recent_controls < 0 ] <- 0
recent_controls[ is.na( recent_controls ) ] <- 0

# add the layout
recent_controls <- cbind( gains_ef_db[ , c( 'iso', 'sector', 'fuel', 'units' ) ], recent_controls )

# ------------------------------------------------------------------------------
# 3. Write output

writeData( recent_controls , "DEFAULT_EF_PARAM", paste0( "B.",em,"_Recent_GAINS_control_percent" ) )

# Every script should finish with this line
logStop()

# END




