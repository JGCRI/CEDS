#------------------------------------------------------------------------------
# Program Name: A6.4.extended_default_activity.R
# Author: Rachel Hoesly
# Date Last Updated: 20 March 2018
# Program Purpose: Combines the historically extended combustion data (defaults) into one data frame:
#                  - Coal, Natural Gas, Oil
#                  - Residential Biomass (Fernandez) (previously extended in earlier mod A script)
#                  - Shipping (IMO) (previously extended in earlier mod A script)
# Input Files: A.CEDS_combustion_activity_coal_by_sector.csv, A.CEDS_combustion_activity_natural_gas_by_sector.csv,
#              A.CEDS_combustion_activity_petroleum_by_sector.csv
# Output Files: A.combustion_default_activity_extended
# Notes: industrial biomass and other biomass need to be added

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
script_name <- "A6.4.extended_default_activity.R"

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

A.natural_gas_extended <- readData( 'MED_OUT',paste0("A.CEDS_combustion_activity_extended_natural_gas") , meta = F)
A.petroleum_extended <- readData( 'MED_OUT',paste0("A.CEDS_combustion_activity_extended_petroleum") , meta = F)
A.coal_extended <- readData( 'MED_OUT',paste0("A.CEDS_combustion_activity_extended_coal") , meta = F)
A.residential_biomass_extended <- readData('MED_OUT','A.residential_biomass_full')
# A.industrial_biomass_extended <- readData('MED_OUT','A.industrial_biomass_extended')
# A.other_biomass_extended <- readData('MED_OUT','A.other_biomass_extended')
ceds_modern_activity <- readData( 'MED_OUT', paste0( 'A.total_activity' ) )
shipping_fuel <- readData( 'MED_OUT', 'A.intl_shipping_en' )

iea_start_year <- readData( 'ENERGY_IN' , 'IEA_iso_start_data')

# ---------------------------------------------------------------------------
# 2. Extend Data frame template
ceds_activity  <- ceds_modern_activity
ceds_activity[ paste0('X', historical_pre_extension_year: 1959)] <- NA
ceds_activity <- ceds_activity[ c( 'iso' , 'sector' , 'fuel' , 'units' , X_extended_years ) ]


# ---------------------------------------------------------------------------
# 3. Add Extended coal, oil, gas fuels

# write function to replace values based on IEA years

add_extended_activity_by_iea <- function(new_data, a.ceds_activity = ceds_activity){
    iea1971 <- iea_start_year %>%
        filter(start_year == 1971) %>%
        pull(iso)

    iea1960 <- iea_start_year %>%
        filter(start_year == 1960) %>%
        pull(iso)

    activity1960 <-  replaceValueColMatch( ceds_activity,
                                           new_data %>% filter ( iso %in% iea1960),
                                         x.ColName = paste0('X',1750:1959),
                                         match.x = c('iso','sector','fuel'),
                                         addEntries = F)
    activity1971 <-  replaceValueColMatch( activity1960,
                                           new_data %>% filter ( iso %in% iea1971),
                                         x.ColName = paste0('X',1750:1970),
                                         match.x = c('iso','sector','fuel'),
                                         addEntries = F)
     return(activity1971)
}

ceds_activity <- add_extended_activity_by_iea(A.natural_gas_extended) %>%
    add_extended_activity_by_iea(A.petroleum_extended) %>%
    add_extended_activity_by_iea(A.coal_extended)


# ---------------------------------------------------------------------------
# 3. Add Shipping Fuel
ceds_activity[ which( ceds_activity$sector == '1A3di_International-shipping'), paste0('X',1750:1959) ] <- NA

ceds_activity <- replaceValueColMatch( ceds_activity, shipping_fuel,
                                       x.ColName = paste0('X',1750:1959),
                                       match.x = c('iso','sector','fuel','units'),
                                       addEntries = F)

# ---------------------------------------------------------------------------
# 3. Add Extended Residential Biomass (Fernandes data)

residential_biomass <- A.residential_biomass_extended[,c('iso','year','units','ceds_tot_final')]
residential_biomass$X_year <- paste0('X',residential_biomass$year)
residential_biomass <- cast(residential_biomass, iso + units ~ X_year, value = 'ceds_tot_final',
                fun.aggregate = sum)
residential_biomass$fuel <- 'biomass'
residential_biomass$sector <- '1A4b_Residential'


ceds_activity <- replaceValueColMatch(ceds_activity, residential_biomass,
                                 x.ColName = paste0('X',1750:1959),
                                 match.x = c('iso','sector','fuel'),
                                 addEntries = FALSE)

# ---------------------------------------------------------------------------
# 3. Add Industrial Biomass

# ---------------------------------------------------------------------------
# 3. Add Other Biomass



# ----------------------------------------------------------------------------

non_comb_activity <- readData( "A.NC_activity", domain = "MED_OUT" )
non_comb_activity[ , paste0("X", 1750:1959)] <- NA

A.total_default_activity <- bind_rows( A.coal_activity, A.NG_activity,
                                       A.petroleum_activity, A.biomass_activity,
                                       non_comb_activity )

# ----------------------------------------------------------------------------

# Write out the data
writeData( A.total_default_activity , "MED_OUT", "A.combustion_default_activity_extended" )

logStop()
# END
