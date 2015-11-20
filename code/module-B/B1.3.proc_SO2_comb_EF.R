#------------------------------------------------------------------------------
# Program Name: B1.3.proc_SO2_comb_EF.R.R
# Authors: Leyang Feng
# Date Last Updated: Nov 9, 2015
# Program Purpose: Use sulfur content, ash retention, control percentage database to 
# generate sulfur EF database corresponding to the fuels and sectors in A.comb_activity.csv
# Input Files: A.comb_activity.csv, B.SO2_S_Content_db.csv, 
#              B.SO2_S_AshRet_db.csv, B.SO2_S_ControlFrac_db.csv   
# Output Files: B.[em]_com_EF_db.csv
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
log_msg <- "Extrapolating default emissions factors to full dataset" # First message to be printed to the log
script_name <- "B1.3.proc_SO2_comb_EF.R"

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

S_content <- readData( "MED_OUT", "B.SO2_S_Content_db" )
ash_ret <- readData( "MED_OUT", "B.SO2_AshRet_db" )
control_frac <- readData( "MED_OUT", "B.SO2_ControlFrac_db" )
layout <- activity_data <- readData( "MED_OUT", "A.comb_activity" )
# ------------------------------------------------------------------------------
# 2.Combine S_Content, Ash_Ret, and control_frac to generate sulfur EF database

S_content[,5:ncol(S_content)] <- lapply(S_content[,5:ncol(S_content)], as.numeric)
ash_ret[,5:ncol(ash_ret)] <- lapply(ash_ret[,5:ncol(ash_ret)], as.numeric)
control_frac[,5:ncol(control_frac)] <- lapply(control_frac[,5:ncol(control_frac)], as.numeric)

# Set output layout
layout[,5:ncol(layout)] <- 0
layout$units <- NA

# Do layout check for each database
check_1 <- identical(layout[,1:3],S_content[,1:3])
check_2 <- identical(layout[,1:3],ash_ret[,1:3])
check_3 <- identical(layout[,1:3],control_frac[,1:3])
check_4 <- identical(ncol(layout),ncol(S_content))
check_5 <- identical(ncol(layout),ncol(ash_ret))
check_6 <- identical(ncol(layout),ncol(control_frac))
checkls <- c(check_1,check_2,check_3,check_4,check_5,check_6)

if (F %in% checkls == F){
  layout[,5:ncol(layout)] <- S_content[,5:ncol(S_content)]*2*(1-ash_ret[,5:ncol(ash_ret)])*(1-control_frac[,5:ncol(control_frac)])
} else {
  print('Cannot generate SO2_EF_db. The iso/fuel/sector in each database my not be consistant. ')
  }

default_efs <- layout

# ------------------------------------------------------------------------------
# 3. Output    

# Write out all three default databases
writeData( default_efs, "MED_OUT", paste0( "B.", em ,"_", "comb", "_EF_db" ) )

# Every script should finish with this line
logStop()
# END


