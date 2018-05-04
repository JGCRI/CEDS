#------------------------------------------------------------------------------
# Program Name: A6.4.extended_default_activity.R
# Author: Rachel Hoesly
# Date Last Updated: 20 March 2018
# Program Purpose: Combines the historically extended combustion data (defaults) into one data frame:
#                  - Coal, Natural Gas, Oil
#                  - Residential Biomass (Fernandez) (previously extended in earlier mod A script)
#                  - Shipping (IMO) (previously extended in earlier mod A script)
#                  - Industrial and Other Bionass
# Input Files: A.CEDS_combustion_activity_extended_natural_gas.csv,
#              A.CEDS_combustion_activity_extended_coal.csv,
#              A.CEDS_combustion_activity_extended_petroleum.csv
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

    A.natural_gas_extended <- readData( 'MED_OUT', "A.comb_activity_extended_natural_gas" , meta = F)
    A.petroleum_extended <- readData( 'MED_OUT', "A.comb_activity_extended_petroleum" , meta = F)
    A.coal_extended <- readData( 'MED_OUT', "A.comb_activity_extended_coal" , meta = F)
    A.residential_biomass_extended <- readData('MED_OUT','A.residential_biomass_full')
    A.industrial_biomass_extended <- readData('MED_OUT','A.industrial_biomass_extended')
    A.other_biomass_extended <- readData('MED_OUT','A.other_biomass_extended')
    ceds_comb_activity <- readData( 'MED_OUT', paste0( 'A.comb_activity' ) )
    shipping_fuel <- readData( 'MED_OUT', 'A.intl_shipping_en' )

    iea_start_year <- readData( 'ENERGY_IN' , 'IEA_iso_start_data')

# ---------------------------------------------------------------------------
# 2. Extend Dataframe template

    ceds_comb_extended  <- ceds_comb_activity
    ceds_comb_extended[ paste0('X', historical_pre_extension_year: 1959)] <- NA
    ceds_comb_extended <- ceds_comb_extended[ c( 'iso' , 'sector' , 'fuel' , 'units' , X_extended_years ) ]


# ---------------------------------------------------------------------------
# 3. Add Extended coal, oil, gas fuels

# write function to replace values based on IEA years
    add_extended_activity_by_iea <- function(new_data, a.ceds_comb_extended = ceds_comb_extended){
        iea1971 <- iea_start_year %>%
            filter(start_year == 1971) %>%
            pull(iso)

        iea1960 <- iea_start_year %>%
            filter(start_year == 1960) %>%
            pull(iso)

        activity1960 <-  replaceValueColMatch( ceds_comb_extended,
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
# Add extended coal, oil, gas data
    ceds_comb_extended <- add_extended_activity_by_iea(A.natural_gas_extended) %>%
            add_extended_activity_by_iea(A.petroleum_extended) %>%
            add_extended_activity_by_iea(A.coal_extended)


# ---------------------------------------------------------------------------
# 3. Add Shipping Fuel
    ceds_comb_extended[ which( ceds_comb_extended$sector == '1A3di_International-shipping'), paste0('X',1750:1959) ] <- NA

    ceds_comb_extended <- replaceValueColMatch( ceds_comb_extended, shipping_fuel,
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


    ceds_comb_extended <- replaceValueColMatch(ceds_comb_extended, residential_biomass,
                                     x.ColName = paste0('X',1750:1959),
                                     match.x = c('iso','sector','fuel'),
                                     addEntries = FALSE)

# ---------------------------------------------------------------------------
# 3. Add Industrial Biomass
    ceds_comb_extended <- add_extended_activity_by_iea(A.industrial_biomass_extended)

# ---------------------------------------------------------------------------
# 3. Add Other Biomass

    ceds_comb_extended <- add_extended_activity_by_iea(A.other_biomass_extended)

# ----------------------------------------------------------------------------
# Arrange and check for NAs
    ceds_comb_extended <- ceds_comb_extended %>%
        select(iso, sector, fuel, units, one_of(X_extended_years)) %>%
        arrange(iso, sector, fuel)

    test <- ceds_comb_extended %>%
        filter(is.na(X1750))

    any(is.na(ceds_comb_extended))
    # For now, replace NAs with zero
    # Should change this when industrial bimass is added
    ceds_comb_extended <- replace(ceds_comb_extended, ceds_comb_extended==NA, 0)

# ----------------------------------------------------------------------------

# Write out the data
writeData( ceds_comb_extended , "MED_OUT", "A.comb_default_activity_extended" )

logStop()
# END
