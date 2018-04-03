# ------------------------------------------------------------------------------
# Program Name: A5.4.default_actvity_natural_gas.R
# Author: Presley Muwan
# Date Last Updated: 30 July 2017
# Program Purpose: Create the default activity natural gas data for CEDS following these three steps:
#                 * Merge IEA coal data (2014 - 1960/1971) to UNSD coal data (1959/1970 - 1950)
#                   and extend the merged data from 1950 to 1850 using CDIAC data.
#                 * Disaggregate the merge data by iso-fuel
#                 * Disaggregate the merged data by CEDS iso-fuel-Sector
#
# Output Files:  A.CEDS_default_actvity_natural_gas
# ---------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R","process_db_functions.R", "default_activity_functions.R") # Additional function files may be required.
log_msg <- "Extending Coal data with bond and IEA" # First message to be printed to the log
script_name <- "A5.4.default_actvity_natural_gas.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )


# Year Selection
extension_start_year <- 1850
extension_end_year <- 1970

#-------------------------------------------------------------------------------------
# 1. Read in Files

UNSD_Energy_1950_1990_Final_Consumption <- readData( 'EXT_IN',"CDA1_UNSD_Energy_Final_Consumption_by_Ctry" , meta = F)

cdiac_liquid_fuel <- readData( 'MED_OUT' , 'E.CO2_CDIAC_inventory')

A.comb_activity <- readData( 'MED_OUT',paste0("A.comb_activity") , meta = F)

iea_other_ng <- readData( 'MED_OUT','A.IEA_CEDS_natural_gas_difference' )

iea_start_year <- readData( 'ENERGY_IN' , 'IEA_iso_start_data')

iea_energy_mapping <- readData( "MAPPINGS", domain_extension = "Energy/" , "IEA_product_fuel", meta = F )

H.final_percentage_breakdown_ng <- readData( 'MED_OUT' , 'A.final_sector_shares_natural_gas')

#-------------------------------------------------------------------------------------
# 2. Process CEDS Combustion data
x_years <- names(A.comb_activity)[grepl("X", names(A.comb_activity))]

# Extend IEA other coal to last CEDS year (extend last IEA year constantly)
iea_other_ng[ X_BP_years ] <- iea_other_ng[ X_IEA_end_year ]

# Add "Other Coal" record to the rows of Energy data
A.comb_activity <- bind_rows( A.comb_activity , iea_other_ng[c('iso',paste0( 'X' , 1960 : end_year ) ) ] )

extension_fuel_category <- 'natural_gas' # natural_gas, coal, or petroleum

# A. Process Ceds Combustion data
ceds_extension_fuels <- c('natural_gas') # the ceds fuels in the fuel category


# Extract CEDS Reported coal
A.comb_activity <- A.comb_activity[ which( A.comb_activity$fuel %in% ceds_extension_fuels ) ,
                                    c('iso','fuel', 'units', 'sector', x_years ) ]

iea_other_ng <-  disaggregate_iea_other_by_fuel(A.comb_activity, iea_other_ng)

# Add "Other Coal" record to the rows of Energy data
A.comb_activity_with_other <- do.call(rbind, list( A.comb_activity , iea_other_ng) )

# Extend the natural_gas data by CDIAC
printLog('Extending Total natural_gas Values with CDIAC')


#----------------------------------------------------------------------------
# 1. process_and_combine_un_ced_data function is called to format CEDS and UN data
#   into a common format, and the merged together. The mergeed data is then extended
#   backward from 1950 to 1850, by CDIAC
#
# Extend the coal data by CDIAC
ceds_un_ng_data <- process_and_combine_un_ced_data( ceds_fuel_data = A.comb_activity_with_other,
                                                    cdiac_data = cdiac_liquid_fuel,
                                                    un_consumption_data = UNSD_Energy_1950_1990_Final_Consumption,
                                                    extension_start_year = 1850,
                                                    ceds_extension_fuels = ceds_extension_fuels,
                                                    iea_start_years = iea_start_year,
                                                    iea_end_year = end_year,
                                                    cdiac_end_year = 2014)

#-----------------------------------------------------------------------------------
# 2. Compute the fuel Percentage break down
#    TODO--take note of coutries with no UN data: For extend only from their IEA years
printLog('Disaggregating total natural_gas into fuel types')

# Call function to disaggregate by fuel
CED_UN_natural_gas_disaggregated_fuel <- Fuel_break_down( CED_UN_fuel_Data_extended = ceds_un_ng_data$un_ceds,
                                                          CED_only_data_extended = ceds_un_ng_data$ceds_only,
                                                          ceds_iea_original_by_fuel = A.comb_activity_with_other,
                                                          unsd_energy_consumption_data =  UNSD_Energy_1950_1990_Final_Consumption,
                                                          ceds_extension_fuels,
                                                          extension_start_year = 1850,
                                                          aggregated_fuel = extension_fuel_category,
                                                          cdiac_fuel = cdiac_liquid_fuel)



# ------------------------------------------------------------------------------------------------
# 3. Disaggregating fuel_types into sector split.
#
#    Disaggregate_to_CEDS_Sectors function is called to disaggregrate fuel values into CEDS sectors.
#    The default break down values (sector ratios) are read from default_sector_breakdown csv file
printLog('Disaggregating fuel_types into sector split')


CEDS_default_actvity_natural_gas <- Disaggregate_to_CEDS_Sectors( a.comb_activity_data = A.comb_activity_with_other,
                                                                  CED_UN_disaggregated_by_fuel = CED_UN_natural_gas_disaggregated_fuel,
                                                                  default_sector_breakdown = H.final_percentage_breakdown_ng,
                                                                  ceds_extension_fuels = ceds_extension_fuels,
                                                                  agg_fuel_name = extension_fuel_category,
                                                                  all_countries = unique( CED_UN_natural_gas_disaggregated_fuel$iso ),
                                                                  iea_start_years = iea_start_year,
                                                                  extension_start_year = 1850)


#----------------------------------------------------------------------------------------------
# Produce data by fuel and by country
x_names <- names(CEDS_default_actvity_natural_gas)[grepl("X",names(CEDS_default_actvity_natural_gas))]
CED_UN_ng_by_fuel <- aggregate(CEDS_default_actvity_natural_gas[x_names], list(
                        iso = CEDS_default_actvity_natural_gas$iso,
                        fuel = CEDS_default_actvity_natural_gas$fuel), FUN = sum)

CED_UN_ng_by_iso <- aggregate(CEDS_default_actvity_natural_gas[x_names], list(
                      iso = CEDS_default_actvity_natural_gas$iso), FUN = sum)

#-----------------------------------------------------------------------------------------------
#Print output
writeData( CEDS_default_actvity_natural_gas , "MED_OUT", "A.CEDS_combustion_activity_natural_gas_by_sector.csv" )
writeData( CED_UN_ng_by_fuel , "MED_OUT", "A.CEDS_combustion_activity_natural_gas_by_fuel" )
writeData( CED_UN_ng_by_iso , "MED_OUT", "A.CEDS_combustion_activity_natural_gas_by_country" )

logStop()
# END
