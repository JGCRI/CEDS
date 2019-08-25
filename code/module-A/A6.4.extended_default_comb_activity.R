#------------------------------------------------------------------------------
# Program Name: A6.4.extended_default_activity.R
# Author: Rachel Hoesly
# Date Last Updated: 20 March 2018
# Program Purpose: Combines the historically extended combustion data (defaults)
#                  into one data frame:
#                  - Coal, Natural Gas, Oil
#                  - Residential Biomass (Fernandez) (previously extended in earlier mod A script)
#                  - Shipping (IMO) (previously extended in earlier mod A script)
#                  - Industrial and Other Biomass
# Input Files: A.comb_activity_extended_natural_gas.csv,
#              A.comb_activity_extended_coal.csv,
#              A.comb_activity_extended_oil.csv,
#              A.residential_biomass_full.csv,
#              A.industrial_biomass_extended.csv,
#              A.other_biomass_extended.csv,
#              A.default_comb_activity_with_other.csv,
#              A.intl_shipping_en.csv,
#              IEA_iso_start_data.csv
# Output Files: A.combustion_default_activity_extended.csv
# Notes: industrial biomass and other biomass need to be added
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
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

# -----------------------------------------------------------------------------
# 0.5 Load Packages, Define Functions
loadPackage('tools')

# Create a function that can be applied to source all child scripts for the given
# emissions type.
MODULE <- "../code/module-A/"
source_child <- function( file_name ){ source( paste( MODULE, file_name, sep = "" ) ) }


# ---------------------------------------------------------------------------
# 1. Load Data

A.coal_extended        <- readData('MED_OUT', "A.comb_activity_extended_coal", meta = F)
A.oil_extended         <- readData('MED_OUT', "A.comb_activity_extended_oil", meta = F)
A.natural_gas_extended <- readData('MED_OUT', "A.comb_activity_extended_natural_gas", meta = F)

A.other_biomass_extended       <- readData('MED_OUT', 'A.other_biomass_extended')
A.industrial_biomass_extended  <- readData('MED_OUT', 'A.industrial_biomass_extended')
A.residential_biomass_extended <- readData('MED_OUT', 'A.residential_biomass_full')

shipping_fuel      <- readData('MED_OUT', 'A.intl_shipping_en')
ceds_comb_activity <- readData('MED_OUT', 'A.default_comb_activity_with_other')
iea_start_year     <- readData('ENERGY_IN', 'IEA_iso_start_data')


# 2. Extend Dataframe template ------------------------------------------------

    extension_years <- paste0('X', historical_pre_extension_year:1959)
    ceds_comb_extended  <- ceds_comb_activity
    ceds_comb_extended[extension_years] <- NA
    ceds_comb_extended <- ceds_comb_extended[ c( 'iso', 'sector', 'fuel', 'units', X_extended_years ) ]

# Fill in 'global" iso with zeros. Will replace international shipping later
    ceds_comb_extended[ ceds_comb_extended$iso == 'global' , paste0('X',1750:1959) ] <- 0

# 3. Add Extended coal, oil, gas fuels ----------------------------------------

# write function to replace values based on IEA years
    add_extended_activity_by_iea <- function(new_data,
                                             a.ceds_comb_extended = ceds_comb_extended){
        iea1971 <- iea_start_year %>%
            filter(start_year == 1971) %>%
            pull(iso)

        iea1960 <- iea_start_year %>%
            filter(start_year == 1960) %>%
            pull(iso)

        activity1960 <-  replaceValueColMatch( a.ceds_comb_extended,
                                               new_data %>% filter ( iso %in% iea1960),
                                             x.ColName = paste0('X',1750:1959),
                                             match.x = c('iso','sector','fuel'),
                                             addEntries = F)
        activity1971 <-  replaceValueColMatch( activity1960,
                                               new_data %>% filter ( iso %in% iea1971),
                                             x.ColName = paste0('X',1750:1970),
                                             match.x = c('iso','sector','fuel'),
                                             addEntries = F)

        if( nrow(activity1971) != nrow(a.ceds_comb_extended) ) stop()

        return(activity1971)
    }

# Add extended coal, oil, gas data
    ceds_comb_extended <- add_extended_activity_by_iea(A.natural_gas_extended)
    ceds_comb_extended <- add_extended_activity_by_iea(A.oil_extended)
    ceds_comb_extended <- add_extended_activity_by_iea(A.coal_extended)



# 4. Add Shipping Fuel --------------------------------------------------------
# overwrite historical extension with NAs
    ceds_comb_extended[ ceds_comb_extended$sector == '1A3di_International-shipping' &
                            ceds_comb_extended$iso == 'global'    , paste0('X',1750:1959) ] <- NA
# international shipping biomass to zero
    ceds_comb_extended[ ceds_comb_extended$sector == '1A3di_International-shipping' &
                            ceds_comb_extended$fuel %in% c( 'biomass','brown_coal','coal_coke','light_oil','natural_gas' )  ,
                        paste0('X',1750:1959) ] <- 0

    ceds_comb_extended <- replaceValueColMatch( ceds_comb_extended, shipping_fuel,
                                           x.ColName = extension_years,
                                           match.x = c('iso','sector','fuel','units'),
                                           addEntries = F)


# 5. Add Extended Residential Biomass (Fernandes data) ------------------------

    residential_biomass <- A.residential_biomass_extended %>%
        dplyr::select( iso, year, units, ceds_tot_final ) %>%
        tidyr::spread( year, ceds_tot_final ) %>%
        dplyr::mutate( fuel = 'biomass', sector = '1A4b_Residential' ) %>%
        dplyr::rename_all( make.names )

    ceds_comb_extended <- replaceValueColMatch(ceds_comb_extended,
                                               residential_biomass,
                                               x.ColName = extension_years,
                                               match.x = c('iso','sector','fuel'),
                                               addEntries = FALSE)
    # Fill in biomass - pse with zero
    ceds_comb_extended[ ceds_comb_extended$sector == '1A4b_Residential' &
                            ceds_comb_extended$iso == 'pse' &
                            ceds_comb_extended$fuel == 'biomass'   , paste0('X',1750:1959) ] <-0

# 6. Add Industrial Biomass ---------------------------------------------------------------
    ceds_comb_extended <- add_extended_activity_by_iea(A.industrial_biomass_extended,
                                                       ceds_comb_extended)


# 7. Add Other Biomass ---------------------------------------------------------------

    ceds_comb_extended <- add_extended_activity_by_iea(A.other_biomass_extended,
                                                       ceds_comb_extended)

# 8. Extended other transformation ---------------------------------------------

    other_transformation_extended <- A.coal_extended  %>%
        bind_rows(A.oil_extended) %>%
        bind_rows(A.natural_gas_extended) %>%
        filter( sector %in% c("1A1bc_Other-transformation", "1A1bc_Other-feedstocks")) %>%
        arrange(iso, fuel, sector)

    ceds_comb_extended <- add_extended_activity_by_iea(other_transformation_extended ,
                                                       ceds_comb_extended)
# 9. Arrange and check for NAs ---------------------------------------------------------------

    other_transformation <- ceds_comb_extended %>%
        filter(sector %in% c("1A1bc_Other-transformation", "1A1bc_Other-feedstocks"))

    ceds_comb_extended <- ceds_comb_extended %>%
        filter(!(fuel %in% c('biomass','light_oil','diesel_oil','heavy_oil') &
                sector %in% c("1A1bc_Other-transformation", "1A1bc_Other-feedstocks"))) %>%
        select(iso, sector, fuel, units, one_of(X_extended_years)) %>%
        arrange(iso, sector, fuel)

    # Check for NAs
    if( anyNA( ceds_comb_extended ) ) stop('NAs in final extended combustion data. Please Check')

# 10. Write out the data ---------------------------------------------------------------

    writeData( ceds_comb_extended , "MED_OUT", "A.comb_default_activity_extended" )
  
    logStop()

# END
