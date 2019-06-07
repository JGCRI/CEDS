# ------------------------------------------------------------------------------
# Program Name: H2.1.base_EFs.R
# Author: Rachel Hoesly
# Program Purpose: Create base database to extend EFs backward
# Input Files: H.", em, "_total_EFs_adjusted-sector
# Output Files:
# TODO: change default to NA and have a better check method for EF drivers
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R") # Additional function files may be required.
log_msg <- "Creating database for CEDS EFs extension before 1960" # First message to be printed to the log
script_name <- "H2.1.base_EFs.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "BC"

# ---------------------------------------------------------------------------
# 1. Load Data

ceds_EFs <- readData( 'MED_OUT', paste0( "H.", em, "_total_EFs_adjusted-sector" )  )
extension_drivers_EF<- readData("EXT_IN", 'CEDS_historical_extension_methods_EF' )
extension_drivers_activity <- readData("EXT_IN", 'CEDS_historical_extension_drivers_activity' )

# ---------------------------------------------------------------------------
# 2. Check EF methods

# Expand fuels - all-comb
expand <- extension_drivers_EF[which(extension_drivers_EF$fuel == 'all-comb' ) ,]
extension_drivers_EF <- extension_drivers_EF[which(extension_drivers_EF$fuel != 'all-comb' ) ,]
comb_fuels <- c('biomass', 'hard_coal','brown_coal','coal_coke','natural_gas','heavy_oil','diesel_oil','light_oil')
for (i in seq_along(comb_fuels)){
  expand$fuel <- rep(comb_fuels[i], times= nrow(expand) )
  extension_drivers_EF <- rbind( extension_drivers_EF, expand )
}

# Check methods
valid_methods <- c( 'constant', "Emissions-trend" , "EF-trend" , 'EF-converge', 'default', NA)

if( any( extension_drivers_EF$method %!in% valid_methods) ) stop(
          'Invalid methods in CEDS_historical_extension_methods_EF. Please Check' )


# ---------------------------------------------------------------------------
# 3. Extend Data frame

ceds_EF_extended <- ceds_EFs
ceds_EF_extended[ paste0('X', historical_pre_extension_year: (historical_end_extension_year-1))] <- ceds_EFs$X1960
ceds_EF_extended <- ceds_EF_extended[ c( 'iso' , 'sector' , 'fuel' , 'units' , X_extended_years ) ]

# TODO: change default to NA and have a better check method for EF drivers

# ---------------------------------------------------------------------------
# 4. Output

 writeData( ceds_EF_extended, "MED_OUT" , paste0('H.',em,'_total_EFs_extended_db'))

 logStop()
