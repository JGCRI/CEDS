# ------------------------------------------------------------------------------------------
# Program Name: A6.2.default_activity_coal.R
# Author: Presley Muwan, Rachel Hoesly
# Date Last Updated: 18 Feb 2018
# Program Purpose: Create the default activity coal data for CEDS following these three steps:
#                 * Merge IEA coal data (2014 - 1960/1971) to UNSD coal data (1959/1970 - 1950)
#                   and extend the merged data from 1950 to 1850 using CDIAC data.
#                 * Disaggregate the merge data by iso-fuel
#                 * Disaggregate the merged data by CEDS iso-fuel-Sector
#
# Output Files:  A.CEDS_default_actvity_coal
# -----------------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R","process_db_functions.R", "default_activity_functions.R") # Additional function files may be required.
log_msg <- "Extending Coal data with bond and IEA" # First message to be printed to the log
script_name <- "A5.4.default_activity_coal.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

#-------------------------------------------------------------------------------------
# 1. Read in Files

    UNSD_Energy_Final_Consumption <- readData( 'EXT_IN',"CDA1_UNSD_Energy_Final_Consumption_by_Ctry" , meta = F)
    other_transformation <- readData( 'MED_OUT','A.Other_transformation_fuel' )
    A.comb_activity <- readData( 'MED_OUT', paste0("A.comb_activity") , meta = F)
    iea_start_year <- readData( 'ENERGY_IN' , 'IEA_iso_start_data')
    iea_energy_mapping <- readData( "MAPPINGS", domain_extension = "Energy/" , "IEA_product_fuel", meta = F )
    final_sector_shares_all <- readData( 'MED_OUT', 'A.final_sector_shares')
    cdiac_solid_fuel <- readData( 'MED_OUT' , 'E.CO2_CDIAC_inventory')

#-------------------------------------------------------------------------------------
# 2. Define Variables and filter inputs

    ceds_extension_fuels <- c('brown_coal','coal_coke','hard_coal')
    final_sector_shares <- final_sector_shares_all %>%
        filter( fuel %in% ceds_extension_fuels)

#-------------------------------------------------------------------------------------
# Process CEDS Combustion data
x_years <- names(A.comb_activity)[grepl("X", names(A.comb_activity))]

# Extract CEDS Reported coal
A.comb_activity <- A.comb_activity[ which( A.comb_activity$fuel %in% ceds_extension_fuels ) ,
                                    c('iso','fuel', 'units', 'sector', x_years ) ]

# Add "Other Coal" record to the rows of Energy data
A.comb_activity_with_other <- do.call(rbind, list(A.comb_activity, other_transformation ) )

#-------------------------------------------------------------------------------------
# 1. process_and_combine_un_ced_data function is called to format CEDS and UN data
#   into a common format, and the merged together. The merged data is then extended
#   backward from 1950 to 1750, by CDIAC
#
# Extend the coal data by CDIAC
printLog('Extending Total Coal Values with CDIAC')


# Call function to extend process and combine ceds and cdiac data
##CR: a function should never have 'and' in the name as it should fundamentally
##  only do one thing.
ceds_un_coal_data <- process_and_combine_un_ced_data( A.comb_activity_with_other, cdiac_solid_fuel,
                                                      UNSD_Energy_Final_Consumption,
                                                      extension_start_year = 1750,
                                                      ceds_extension_fuels = ceds_extension_fuels,
                                                      iea_start_years = iea_start_year,
                                                      iea_end_year = end_year )

#-----------------------------------------------------------------------------------
# 2. Compute the fuel Percentage break down
#    take not of coutries with no UN data: For extend only from their IEA years
printLog('Disaggregating total coal into fuel types')

# Call function to disaggregate by fuel
CED_UN_coal_disaggregated_fuel <- Fuel_break_down( ceds_un_coal_data$un_ceds ,
                                                   ceds_un_coal_data$ceds_only,
                                                   A.comb_activity_with_other,
                                                   UNSD_Energy_Final_Consumption,
                                                   ceds_extension_fuels,
                                                   extension_start_year = 1750,
                                                   cdiac_solid_fuel )

# ------------------------------------------------------------------------------------------------
# 3. CEDS fuel_types into sector split.
#
#    Disaggregate_to_CEDS_Sectors function is called to disaggregrate fuel values into CEDS sectors.
#   The default break down values (sector ratios) are read from default_sector_breakdown csv file

printLog('Disaggregating fuel_types into sector split')

CEDS_default_actvity_coal <- Disaggregate_to_CEDS_Sectors( a.comb_activity_data = A.comb_activity_with_other,
                                                           CED_UN_disaggregated_by_fuel = CED_UN_coal_disaggregated_fuel,
                                                           default_sector_breakdown = final_sector_shares,
                                                           ceds_extension_fuels = ceds_extension_fuels,
                                                           agg_fuel_name = "coal",
                                                           all_countries = unique( CED_UN_coal_disaggregated_fuel$iso ),
                                                           iea_start_years = iea_start_year )

CEDS_default_actvity_coal_combustion <- CEDS_default_actvity_coal %>% filter(sector %!in% c("1A1bc_Other-transformation","1A1bc_Other-feedstock"))
CEDS_default_actvity_coal_other <- CEDS_default_actvity_coal %>% filter(sector %in% c("1A1bc_Other-transformation","1A1bc_Other-feedstock"))

#----------------------------------------------------------------------------------------------
# 4. Produce data by fuel and by country
x_names <- names(CEDS_default_actvity_coal)[grepl("X",names(CEDS_default_actvity_coal))]
##CR: dplyr::group_by + summarise much faster than aggregate
CED_UN_coal_by_iso_all <- aggregate(CEDS_default_actvity_coal[x_names], list(
                                iso = CEDS_default_actvity_coal$iso), FUN = sum)

CED_UN_coal_by_fuel_combustion <- aggregate(CEDS_default_actvity_coal_combustion[x_names], list(
                       iso = CEDS_default_actvity_coal_combustion$iso,
                       fuel = CEDS_default_actvity_coal_combustion$fuel), FUN = sum)

CED_UN_coal_by_iso_combustion <- aggregate(CEDS_default_actvity_coal_combustion[x_names], list(
                      iso = CEDS_default_actvity_coal_combustion$iso), FUN = sum)


CED_UN_coal_by_fuel_other <- aggregate(CEDS_default_actvity_coal_other[x_names], list(
                        iso = CEDS_default_actvity_coal_other$iso,
                        fuel = CEDS_default_actvity_coal_other$fuel), FUN = sum)

CED_UN_coal_by_iso_other <- aggregate(CEDS_default_actvity_coal_other[x_names], list(
                        iso = CEDS_default_actvity_coal_other$iso), FUN = sum)
#-----------------------------------------------------------------------------------------------
# 3. Print output
writeData( CEDS_default_actvity_coal_combustion , "MED_OUT", "A.CEDS_combustion_activity_coal_by_sector" )
writeData( CED_UN_coal_by_fuel_combustion , "MED_OUT", "A.CEDS_combustion_activity_coal_by_fuel" )
writeData( CED_UN_coal_by_iso_combustion , "MED_OUT", "A.CEDS_combustion_activity_coal_by_country" )

writeData( CED_UN_coal_by_iso_all , "MED_OUT", "A.CEDS_combustion_activity_coal_all_by_iso" )

writeData( CEDS_default_actvity_coal_other , "MED_OUT", "A.CEDS_combustion_activity_coal_other_by_sector" )
writeData( CED_UN_coal_by_fuel_other , "MED_OUT", "A.CEDS_combustion_activity_coal_other_by_fuel" )
writeData( CED_UN_coal_by_iso_other , "MED_OUT", "A.CEDS_combustion_activity_coal_other_by_country" )

logStop()
# END
