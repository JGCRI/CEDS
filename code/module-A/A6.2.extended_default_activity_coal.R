# ------------------------------------------------------------------------------------------
# Program Name: A6.2.extended_default_activity_coal.R
# Author: Presley Muwan, Rachel Hoesly
# Date Last Updated: 4 June 2019
# Program Purpose: Create the default activity coal data for CEDS following these three steps:
#                 * Merge IEA coal data (2014 - 1960/1971) to UNSD coal data (1959/1970 - 1950)
#                   and extend the merged data from 1950 to 1850 using CDIAC data.
#                 * Disaggregate the merge data by iso-fuel
#                 * Disaggregate the merged data by CEDS iso-fuel-Sector
# Input Files: CDA1_UNSD_Energy_Final_Consumption_by_Ctry.csv, A.default_comb_activity_with_other.csv,
#              E.CO2_CDIAC_inventory.csv, A.full_default_sector_shares.csv,
#              IEA_iso_start_data.csv, IEA_product_fuel.csv
# Output Files:  A.comb_activity_extended_coal.csv
# -----------------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c("data_functions.R","process_db_functions.R", "default_activity_functions.R") # Additional function files may be required.
log_msg <- "Extending Coal data with bond and IEA" # First message to be printed to the log
script_name <- "A6.2.extended_default_activity_coal.R"

source(paste0(PARAM_DIR, "header.R"))
initialize(script_name, log_msg, headers)

#-------------------------------------------------------------------------------------
# 1. Read in Files

UNSD_Energy_Final_Consumption_all <- readData('EXT_IN',"CDA1_UNSD_Energy_Final_Consumption_by_Ctry" , meta = F)
A.comb_activity_all <- readData('MED_OUT', paste0("A.default_comb_activity_with_other") , meta = F)
cdiac_fuel_all <- readData('MED_OUT' , 'E.CO2_CDIAC_inventory')
full_default_sector_shares_all <- readData('MED_OUT', 'A.full_default_sector_shares')
iea_start_year_all <- readData('ENERGY_IN' , 'IEA_iso_start_data')
iea_energy_mapping <- readData("MAPPINGS", domain_extension = "energy/" , "IEA_product_fuel", meta = F)

#-------------------------------------------------------------------------------------
# 2. Define Variables and Filter and Process inputs

# Define fuels
ceds_extension_fuels <- c('brown_coal','coal_coke','hard_coal')
aggregate_fuel_name <- 'coal'
cdiac_fuel_name <- 'solid_fuels'
default_fuel_share <- data.frame(fuel = ceds_extension_fuels,
                                  breakdown = c(0,0,1))

# Process UN data
# Filter out countries that have lines of data, but all zero
UN_countries <- UNSD_Energy_Final_Consumption_all %>%
    filter(fuel %in% ceds_extension_fuels) %>%
    group_by(iso) %>%
    summarize_if(is.numeric, sum) %>%
    mutate(sum_all = rowSums(.[grep("X", names(.))], na.rm = TRUE)) %>% # sum rows over all columns
    filter(sum_all>0) %>%
    pull(iso)

UNSD_Energy_Final_Consumption <- UNSD_Energy_Final_Consumption_all %>%
    filter(fuel %in% ceds_extension_fuels,
            iso %in% UN_countries)

# Filter Input data for fuels and zero data
# Filter Input data for fuels
# other_transformation <- other_transformation_all %>%
#     filter(fuel %in% ceds_extension_fuels) %>%
#     filter(iso != 'global ')
A.default_comb_activity_with_other <- A.comb_activity_all %>%
    filter(fuel %in% ceds_extension_fuels) %>%
    filter(iso != 'global ')
cdiac_fuel <- cdiac_fuel_all %>%
    filter(fuel %in% cdiac_fuel_name) %>%
    filter(iso != 'global ')
full_default_sector_shares <- full_default_sector_shares_all %>%
    filter(fuel %in% ceds_extension_fuels) %>%
    filter(iso != 'global ')
iea_start_year <- iea_start_year_all %>%
    filter(iso != 'global')

# Filter combustion data for coal fuels
A.default_comb_activity_with_other <- A.default_comb_activity_with_other%>% filter(fuel %in% ceds_extension_fuels)

#-------------------------------------------------------------------------------------
# 1. process_and_combine_un_ced_data function is called to format CEDS and UN data
#   into a common format, and the merged together. The merged data is then extended
#   backward from 1950 to 1750, by CDIAC
#
# Extend the coal data by CDIAC
printLog(paste("Extending aggregate ", aggregate_fuel_name, ' with UN and CDIAC data back to 1750'))

ceds_un_extended_data <- merge_extend_UN_CEDS_data(a.CEDS_data = A.default_comb_activity_with_other,
                                                    a.CDIAC_data = cdiac_fuel,
                                                    a.UN_data = UNSD_Energy_Final_Consumption,
                                                    a.ceds_extension_fuels = ceds_extension_fuels,
                                                    a.extension_start_year = 1750,
                                                    a.iea_start_years = iea_start_year,
                                                    a.iea_end_year = end_year,
                                                    a.aggregate_fuel = aggregate_fuel_name,
                                                    a.CDIAC_fuel = cdiac_fuel_name)

#-----------------------------------------------------------------------------------
# 2. Compute the fuel Percentage break down
#    take not of coutries with no UN data: For extend only from their IEA years
printLog('Disaggregating total coal into fuel types')

# Call function to disaggregate by fuel
all_disaggregate_fuel <- fuel_breakdown(a.UN_data = UNSD_Energy_Final_Consumption,
                                         a.CEDS_UN_aggregate = ceds_un_extended_data$un_ceds ,
                                         a.CEDS_only_aggregate = ceds_un_extended_data$ceds_only,
                                         a.CEDS_comb_with_other = A.default_comb_activity_with_other,
                                         a.ceds_extension_fuels = ceds_extension_fuels,
                                         a.extension_start_year = 1750,
                                         a.aggregate_fuel = aggregate_fuel_name,
                                         a.default_fuel_share = default_fuel_share,
                                         a.UN_start = 1950,
                                         a.iea_start_years = iea_start_year)

# ------------------------------------------------------------------------------------------------
# 3. CEDS fuel_types into sector split.
#
#    Disaggregate_to_CEDS_Sectors function is called to disaggregrate fuel values into CEDS sectors.
#   The default break down values (sector ratios) are read from default_sector_breakdown csv file

printLog('Disaggregating fuel_types into sector split')

CEDS_default_actvity <- sector_breakdown(fuel_totals = all_disaggregate_fuel,
                                         sector_shares = full_default_sector_shares_all,
                                         iea_start_years = iea_start_year,
                                         ceds_extension_fuels = ceds_extension_fuels)

# ------------------------------------------------------------------------------------------------
# 4. Arrange final df
CEDS_default_actvity_final <- CEDS_default_actvity %>%
    mutate(units = 'kt') %>%
    arrange(iso, sector, fuel, units)

#-----------------------------------------------------------------------------------------------
# 5. Print output
writeData(CEDS_default_actvity_final , "MED_OUT", "A.comb_activity_extended_coal")

logStop()
# END
