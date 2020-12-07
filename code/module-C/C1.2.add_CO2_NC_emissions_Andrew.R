#------------------------------------------------------------------------------
# Program Name: C1.2.add_CO2_NC_emissions_Andrew.R
# Author: Linh Vu, Hamza Ahsan
# Date Last Modified: December 3, 2020
# Program Purpose: Add Andrew CO2 cement emissions data
# Input Files: E.CO2_Andrew_Cement.csv, C.[em]_NC_emissions_db.csv
# Output Files: C.[em]_NC_emissions_db.csv, C.Andrew_NC_Emissions_[em].csv
# Notes:
# TODO:
#-------------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "timeframe_functions.R", "common_data.R", "data_functions.R",
              "process_db_functions.R" ) # Additional function files required.
log_msg <- "Processing Andrew CO2 emissions data" # First message to be printed to the log
script_name <- "C1.2.add_CO2_NC_emissions_Andrew.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers, common_data = FALSE )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "CO2"

# ------------------------------------------------------------------------------
# 1. Input

# Stop script if running for unsupported species
if ( em %!in% c('CO2') ) {
  stop (paste( 'Unsupported emission species', em, '. Remove from script
               list in C1.2.add_NC_emissions.R'))
}

Andrew <- readData( "MED_OUT", "E.CO2_Andrew_Cement" )

# Fuels to add to db from Andrew and corresponding CEDS sectors
Andrew_fuel <- c( "cement_production" )
Andrew_fuel_sector <- c( "2A1_Cement-production" )

# ------------------------------------------------------------------------------
# 2. Put Andrew into iso-sector-fuel-units-years format
X_Andrew_years <- paste0( "X", start_year:end_year)
Andrew_added <- filter( Andrew, fuel %in% Andrew_fuel )
Andrew_added$units <- "kt"
Andrew_added$sector <- NA

for ( i in seq_along( Andrew_fuel ) )
  Andrew_added$sector[ Andrew_added$fuel == Andrew_fuel[[ i ]] ] <- Andrew_fuel_sector[[ i ]]
Andrew_added$fuel <- "process"
Andrew_added <- Andrew_added[ c( "iso", "sector", "fuel", "units", X_Andrew_years ) ]

# ------------------------------------------------------------------------------
# 3. Output
addToEmissionsDb( Andrew_added, em = em, type = 'NC', ext_backward = FALSE, ext_forward = FALSE )
writeData( Andrew_added, domain = "DIAG_OUT", fn = paste0( "C.Andrew_NC_Emissions_",em ) )

logStop()
