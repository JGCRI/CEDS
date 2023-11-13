#------------------------------------------------------------------------------
# Program Name: A3.1.IEA_BP_data_extension_detailed.R
# Authors Names: Rachel Hoesly
# Date Last Modified: 20 March 2023
# Program Purpose: Reads in detailed BP petroleum data and extended IEA data
#                  (extended using aggregate BP data). Recalculates
#                  trends for some detailed oil products and replaces those
#                  in the data
# Input Files: "A.IEA_BP_energy_ext", MCL, "bp-stats-review-2022-oil-by-product",
#               map - "BP_detailed_extension"
# Output Files: A.IEA_BP_energy_ext_detailed.csv
# Notes: When new data comes out, this BP sheet needs to be altered and saved
#               as a csv. Easier todo it by hand in excel (just simple
#               formatting)
#        Extension here uses 2 methods:
#           - the BP growth method: standard CEDS trend method where the ratio of
#           growth in the trend data is applied to the data to be extended
#           - unit conversion method: the bp data is "converted" to the units of
#           the IEA data
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the IEA "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R", "process_db_functions.R",
              "bp_extension_functions.R") # Additional function files required.
log_msg <- "Deatiled projection of petroleum IEA data from more recent BP statistics" # First message to be printed to the log
script_name <- "A3.1.IEA_BP_data_extension_detailed.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# -----------------------------------------------------------------------------------------
# 2.

oil_densities <- tibble(Product = c('Light distillates',
                                    '    of which: gasoline',
                                    '    of which: naphtha',
                                    'Middle distillates',
                                    '    of which: diesel/gasoil',
                                    '    of which: jet/kerosene',
                                    'Fuel oil',
                                    'Others',
                                    '    of which: ethane and LPG'),
                        Density = c(1400,
                                    1350,
                                    1448,
                                    1200,
                                    1185,
                                    1246,
                                    1081,
                                    2400,
                                    2400)) %>% mutate(Unit = 'Liters per ton')

# BP growth limit
BP_growth_limit <- 5

# Define Threshold for small number problem
IEA_BP_ratio_limit <- 3

# Number of years to average IEA:bp ratio over
no_ratio_years <- 5


# ------------------------------------------------------------------------------
# 1. Read in files
activity <- readData( "MED_OUT", "A.IEA_BP_energy_ext" )
MCL <- readData( "MAPPINGS", "Master_Country_List" )
detailed_bp_data <- readData( "ENERGY_IN", "bp-stats-review-2022-oil-by-product", skip = 2)
detailed_bp_map <- readData( "MAPPINGS", "BP_detailed_extension", domain_extension = "energy/")


# ------------------------------------------------------------------------------
# 2. Process BP Data

# convert thousand barrels per day to kt
# 158.987 Liters per barrel
# The converted value is not used in the bp extension, but can be useful for debugging
# for some sectors (not good for all sectors depends on the mapping between
# detailed fuels)
detailed_bp_data_kt <- detailed_bp_data %>%
    filter(!is.na(BP_Oil_product)) %>%
    select(BP_Oil_product, Product, paste0('X',2010:BP_last_year)) %>%
    gather(year,value,-BP_Oil_product, -Product) %>%
    left_join(oil_densities) %>%
    filter(!is.na(value)) %>%
    filter(!is.na(Density)) %>%
    mutate(kt = value* 1000 *365 *158.987 / Density /1000) %>%
    select(-value) %>%
    spread(year, kt) %>%
    mutate(units = 'kt')

# ------------------------------------------------------------------------------
# 2. Set up data for Extension

# IEA data to trend
IEA_petroleum_data <- activity %>%
    select(iso, sector, fuel, units, X_IEA_years) %>%
    filter(fuel %in% c("diesel_oil","light_oil","heavy_oil")) %>%
    left_join(MCL %>% select(iso, BP_Oil_product) %>% unique() ) %>% #make sure the mapping doesn't duplicate
    arrange(iso, sector, fuel)
# BP data in the
BP_trend_data <- IEA_petroleum_data %>%
    select(iso, sector, fuel) %>%
    mutate(units = 'kt') %>%
    left_join(detailed_bp_map) %>%
    left_join(MCL %>% select(iso, BP_Oil_product) %>% unique ) %>%
    left_join(detailed_bp_data) %>%
    select(iso, sector, fuel, units, Product, BP_Oil_product, paste0('X',(BP_first_year - 6):BP_last_year)) %>%
    arrange(iso, sector, fuel)

check_iea_bp(IEA_petroleum_data, BP_trend_data)

# ------------------------------------------------------------------------------
# 2. Extend with Growth Method
# Growth method: IEA data is extended using the trend of the BP data. BP trend is
# calculated with IEA(n+1) = IEA(n)* BP(n+1)/BP(n) where BP(n+1) is the data in the next
# year and BP(n) is the data in year n.

IEA_extended_growth <- extend_iea_growth_detailed(iea = IEA_petroleum_data,
                                          bp = BP_trend_data,
                                          bp_growth_limit = BP_growth_limit)

# ------------------------------------------------------------------------------
# 3. Extend with BP-IEA units method
# Create a BP to IEA "conversion factor" to create a BP trend in "IEA" units
# mean(iea/bp)*bp_trend = estimated bp trend in IEA units


IEA_BP_trend_conversion <- iea_to_bp_conversion_factor(iea = IEA_extended_growth,
                                                       bp = BP_trend_data)


BP_trend_IEA_unit <- bp_to_iea_trend(bp = BP_trend_data,
                                     iea_to_bp_conversion = IEA_BP_trend_conversion)

# ------------------------------------------------------------------------------
# 4. Extension Comparison and Correction
# Compare the BP growth methods and the BP-IEA unit conversion method. Set a
# limit for the tolerance of the ratio between the 2 trends.

IEA_extended_correction <- extension_correction(iea = IEA_extended_growth,
                     iea_bp_trend_conversion = IEA_BP_trend_conversion,
                     bp_trend_iea_unit = BP_trend_IEA_unit,
                     iea_bp_ratio_limit =  IEA_BP_ratio_limit)

# ------------------------------------------------------------------------------
# 4. Final processing

# Replace corrected oil data in activity data

activity_not_oil <- activity %>%
    filter(fuel %!in% c("diesel_oil","light_oil","heavy_oil") )
activity_oil <- activity %>%
    filter(fuel %in% c("diesel_oil","light_oil","heavy_oil") )

# Check to ensure that all oil activity is corrected
if (nrow(activity_oil) != nrow(IEA_extended_correction)) {stop('Some data was dropped in the detailed BP oil extension')}

detailed_bp_extension_final <- activity_not_oil %>%
    bind_rows(IEA_extended_correction) %>%
    arrange(iso, sector, fuel)

# Check to ensure that all activity is in the final output

if( !identical(activity %>% select(iso, sector, fuel) %>% arrange(iso, sector, fuel),
               detailed_bp_extension_final %>% select(iso, sector, fuel) %>% arrange(iso, sector, fuel))){
    stop('Some data was dropped in the detailed BP oil extension.')
}

# ------------------------------------------------------------------------------
# 5. Output

# Final Output
writeData(detailed_bp_extension_final, domain = "MED_OUT", fn = "A.IEA_BP_energy_ext_detailed.csv")

logStop()

