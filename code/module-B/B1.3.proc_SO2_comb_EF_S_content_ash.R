#------------------------------------------------------------------------------
# Program Name: B1.3.proc_SO2_comb_EF_S_content_ash.R.R
# Authors: Leyang Feng
# Date Last Updated: Nov 9, 2015
# Program Purpose: Use sulfur content, ash retention database to
# generate sulfur EF database corresponding to the fuels and sectors in A.comb_activity.csv
# Input Files: A.comb_activity.csv, B.SO2_S_Content_db.csv,
#              B.SO2_S_AshRet_db.csv,
# Output Files: B.[em]_comb_EF_db.csv
# Notes:
# TODO:
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R" ) # Additional function files required.
log_msg <- "Extrapolating default emissions factors to full dataset" # First message to be printed to the log
script_name <- "B1.3.proc_SO2_comb_EF_S_content_ash.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )
# ------------------------------------------------------------------------------
# 1. Read in files
# for running directly from R - defines emission species (normally from command line)
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"

S_content <- readData( "MED_OUT", "B.SO2_S_Content_db" )
ash_ret <- readData( "MED_OUT", "B.SO2_AshRet_db" )

layout <- readData( "MED_OUT", "A.comb_activity" )
# ------------------------------------------------------------------------------
# 2.Combine S_Content, Ash_Ret to generate sulfur EF database

# sort
S_content <- S_content[ with( S_content, order( iso, sector, fuel ) ), ]
ash_ret <- ash_ret[ with( ash_ret, order( iso, sector, fuel ) ), ]
layout <- layout[ with( layout, order( iso, sector, fuel ) ), ]

# make sure values are numeric
S_content[,X_emissions_years] <- lapply(S_content[,X_emissions_years], as.numeric)
ash_ret[,X_emissions_years] <- lapply(ash_ret[,X_emissions_years], as.numeric)

# Set output layout
layout[,X_emissions_years] <- 0
layout$units <- 'kt/kt'

# Do layout check for each database
check_1 <- all.equal(layout[,c('iso','sector','fuel')],S_content[,c('iso','sector','fuel')], check.attributes = FALSE)
check_2 <- all.equal(layout[,c('iso','sector','fuel')],ash_ret[,c('iso','sector','fuel')], check.attributes = FALSE)
check_3 <- identical(ncol(layout),ncol(S_content))
check_4 <- identical(ncol(layout),ncol(ash_ret))
checkls <- c(check_1,check_2,check_3,check_4)

if (F %in% checkls == F){
  layout[,X_emissions_years] <- S_content[,X_emissions_years]*2*(1-ash_ret[,X_emissions_years])
}else {
  stop('Cannot generate EF_db. The iso/fuel/sector in each database my not be consistant. ')
  }

default_efs <- layout

# ------------------------------------------------------------------------------
# 3. Output

# Write out all three default databases
writeData( default_efs, "MED_OUT", paste0( "B.", em ,"_", "comb", "_EF_db" ) )

# Every script should finish with this line
logStop()
# END
