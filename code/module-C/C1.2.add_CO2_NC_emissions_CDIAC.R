#------------------------------------------------------------------------------
# Program Name: C1.2.add_CO2_NC_emissions_CDIAC.R
# Author: Linh Vu
# Date Last Modified: 27 Jul 2016
# Program Purpose: Add CDIAC cement production and flaring emissions for CO2
# Input Files: E.CO2_CDIAC_inventory.csv, C.[em]_NC_emissions_db.csv
# Output Files: C.[em]_NC_emissions_db.csv
# Notes:
# TODO:
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "timeframe_functions.R", "common_data.R", "data_functions.R",
              "process_db_functions.R" ) # Additional function files required.
log_msg <- "Processing CDIAC non-combustion emissions data" # First message to be printed to the log
script_name <- "C1.2.add_CO2_NC_emissions_CDIAC.R"

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

cdiac <- readData( "MED_OUT", "E.CO2_CDIAC_inventory" )

# Fuels to add to db from cdiac and corresponding CEDS sectors
cdiac_fuel <- c( "cement_production" )
cdiac_fuel_sector <- c( "2A1_Cement-production" )

# ------------------------------------------------------------------------------
# 2. Put cdiac into iso-sector-fuel-units-years format
X_cdiac_years <- paste0( "X", start_year:cdiac_end_year_cement)
cdiac_added <- filter( cdiac, fuel %in% cdiac_fuel )
cdiac_added <- cdiac_added[-which(cdiac_added$iso == 'global'),] # added
cdiac_added$units <- "kt"
cdiac_added$sector <- NA
cdiac_added[, X_cdiac_years ] <- cdiac_added[, X_cdiac_years ] * conversionFactor_C_CO2  # C to CO2

for ( i in seq_along( cdiac_fuel ) )
  cdiac_added$sector[ cdiac_added$fuel == cdiac_fuel[[ i ]] ] <- cdiac_fuel_sector[[ i ]]
cdiac_added$fuel <- "process"
cdiac_added <- cdiac_added[ c( "iso", "sector", "fuel", "units", X_cdiac_years ) ]

# ------------------------------------------------------------------------------
# 3. Output
addToEmissionsDb( cdiac_added, em = em, type = 'NC', ext_backward = FALSE, ext_forward = FALSE )
writeData( cdiac_added, domain = "DIAG_OUT", fn = paste0( "C.CDIAC_NC_Emissions_",em ) )

logStop()
