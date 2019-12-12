#------------------------------------------------------------------------------
# Program Name: B1.3.proc_comb_EF_control_percent.R.R
# Authors: Leyang Feng, Rachel Hoesly
# Date Last Updated: Nov 9, 2015
# Program Purpose: Use sulfur content, ash retention, control percentage database to
# generate sulfur EF database corresponding to the fuels and sectors in A.final_comb_activity_modern.csv
# Input Files: B.[em]_ControlFrac_db.csv, B.[em]_comb_EF_db.csv
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
log_msg <- "Encorporating control percentage into EF" # First message to be printed to the log
script_name <- "B1.3.proc_comb_EF_control_percent.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )
# ------------------------------------------------------------------------------
# 1. Read in files
# for running directly from R - defines emission species (normally from command line)
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "NOx"

control_frac <- readData( "MED_OUT", paste0("B.",em,'_ControlFrac_db' ))
EF_db <- readData("MED_OUT", paste0( "B.", em ,"_", "comb", "_EF_db" ))

# ------------------------------------------------------------------------------
# 2.Combine S_Content, Ash_Ret, and control_frac to generate sulfur EF database

# sort
control_frac <- control_frac[ with( control_frac, order( iso, sector, fuel ) ), ]
EF_db <- EF_db[ with( EF_db, order( iso, sector, fuel ) ), ]

# make sure values are numeric
control_frac[,X_emissions_years] <- lapply(control_frac[,X_emissions_years], as.numeric)
EF_db[,X_emissions_years] <- lapply(EF_db[,X_emissions_years], as.numeric)

if(anyNA( EF_db )) stop(paste('NAs in EF data base for',em,'please check.'))
if(anyNA( control_frac )) stop(paste('NAs in EF data base for',em,'please check.'))


# Set output EF_db

# Do EF_db check for each database
check_1 <- all.equal(EF_db[,c('iso','sector','fuel')],control_frac[,c('iso','sector','fuel')], check.attributes = FALSE)
check_2 <- identical(ncol(EF_db),ncol(control_frac))
checkls <- c(check_1,check_2)

if (F %in% checkls == F){
  EF_db[,X_emissions_years] <- EF_db[,X_emissions_years]*(1-control_frac[,X_emissions_years])
} else {
  stop('Cannot generate EF_db. The iso/fuel/sector in each database my not be consistant. ')
}

default_efs <- EF_db

# ------------------------------------------------------------------------------
# 3. Output

# Write out all three default databases
writeData( default_efs, "MED_OUT", paste0( "B.", em ,"_", "comb", "_EF_db" ) )

# Every script should finish with this line
logStop()
# END
