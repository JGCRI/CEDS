#------------------------------------------------------------------------------
# Program Name: A6.3.extended_default_activity.R
# Author: Presley Muwan
# Date Last Updated: 20 March 2018
# Program Purpose: Combines the historically extended  combustion data with
#                extended biomass and IMO shipping data to complete historical
#                energy combustion data.
# Input Files: A.CEDS_combustion_activity_coal_by_sector.csv, A.CEDS_combustion_activity_natural_gas_by_sector.csv,
#              A.CEDS_combustion_activity_petroleum_by_sector.csv
# Output Files: A.total_default_activity
# Notes:

# ------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c('data_functions.R', 'common_data.R') # Additional function files required.
log_msg <- paste0( "Combining extended combustion data" ) # First message to be printed to the log
script_name <- "A6.3.extended_default_activity.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------------
# 0.5 Load Packages, Define Functions
loadPackage('tools')

# Create a function that can be applied to source all child scripts for the given
# emissions type.
MODULE <- "../code/module-A/"
source_child <- function( file_name ){ source( paste( MODULE, file_name, sep = "" ) ) }

# ---------------------------------------------------------------------------
# 1. Load Data

A.NG_activity <- readData( 'MED_OUT',paste0("A.CEDS_combustion_activity_coal_by_sector.csv") , meta = F)
A.petroleum_activity <- readData( 'MED_OUT',paste0("A.CEDS_combustion_activity_natural_gas_by_sector.csv") , meta = F)
A.coal_activity <- readData( 'MED_OUT',paste0("A.CEDS_combustion_activity_petroleum_by_sector.csv") , meta = F)
A.biomass_activity <- readData( 'MED_OUT', "A.CEDS_default_actvity_biomass")
ceds_activity <- readData( 'MED_OUT', paste0( 'A.total_activity' ) )
shipping_fuel <- readData( 'MED_OUT', 'A.intl_shipping_en' )

# ---------------------------------------------------------------------------
# 2. Extend Data frame

ceds_activity[ paste0('X', historical_pre_extension_year: 1959)] <- NA
ceds_activity <- ceds_activity[ c( 'iso' , 'sector' , 'fuel' , 'units' , X_extended_years ) ]
ceds_activity[ which( ceds_activity$sector == '1A3di_International-shipping'), paste0('X',1750:1959) ] <- 0

# ---------------------------------------------------------------------------
# 3. Add Shipping Fuel
ceds_activity <- replaceValueColMatch( ceds_activity, shipping_fuel,
                                       x.ColName = paste0('X',1750:1959),
                                       match.x = c('iso','sector','fuel','units'),
                                       addEntries = F)


# Natural gas and petroleum have data from 1750:1849 (but coal does)
# Those years are added to NG and Petroleum, and initialized with zero
coal_and_biomass_years <- paste0('X',1750:1849)
A.NG_activity[ , coal_and_biomass_years ] <- 0
A.petroleum_activity[ , coal_and_biomass_years ] <- 0
# ----------------------------------------------------------------------------

non_comb_activity <- readData( "A.NC_activity", domain = "MED_OUT" )
non_comb_activity[ , paste0("X", 1750:1959)] <- NA

A.total_default_activity <- bind_rows( A.coal_activity, A.NG_activity,
                                       A.petroleum_activity, A.biomass_activity,
                                       non_comb_activity )

# ----------------------------------------------------------------------------

# Write out the data
writeData( A.total_default_activity , "MED_OUT", "A.total_default_activity_extended" )

logStop()
# END
