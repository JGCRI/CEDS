#------------------------------------------------------------------------------
# Program Name: A3.1.IEA_BP_data_extension.R
# Authors Names: Tyler Pitkanen, Rachel Hoesly, Linh Vu
# Date Last Modified: 26 March 2023
# Program Purpose: Reads in BP data for years not yet covered by IEA data
#                  Alters BP data to agree with IEA data labels
#                  Adds recent BP-projected data to historical years data
# Input Files: A.comb_othertrans_activity.csv, BP_energy_data.xlsx,
#              Master_Country_List.csv, Master_Fuel_Sector_List.xlsx,
# Output Files: A.IEA_BP_sum_comparison.csv, A.IEA_BP_trend_comparison.csv,
#               A.IEA_BP_energy_ext.csv
# Notes: IEA_years, BP_years, end_year and X_ variants defined in common_data.R

#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R", "process_db_functions.R" ,
              "bp_extension_functions.R") # Additional function files required.
log_msg <- "Projection of IEA data from more recent BP statistics" # First message to be printed to the log
script_name <- "A3.1.IEA_BP_data_extension.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# -----------------------------------------------------------------------------------------
# 2. Define Options/Constants

# BP growth limit
BP_growth_limit <- 5

# Define Threshold for small number problem
IEA_BP_ratio_limit <- 3

# Number of years to average IEA:bp ratio over
no_ratio_years <- 5

# ------------------------------------------------------------------------------
# 1. Read in files
iea_data_full <- readData( "MED_OUT", "A.comb_othertrans_activity" )

MCL <- readData( "MAPPINGS", "Master_Country_List" )
fuel_list <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Fuels" )

# Read in BP energy data
printLog( c("Reading in BP energy consumption data."))
bp_energy_data <- readData( "ENERGY_IN",BP_data_file_name, ".xlsx",  skip = 2)

# ------------------------------------------------------------------------------
# 2. Clean/Aggregate Energy Data

printLog( c("Aggregating IEA energy consumption data."))
# IEA data is more detailed than BP data, so aggregate IEA data to BP regions
#   We will calculate fuel use trends for these regions and apply them to
#   IEA's individual countries.

oil_fuels <- c( fuel_list[fuel_list$aggregated_fuel == "oil", "fuel"], 'oil' )
gas_fuels <- fuel_list[fuel_list$aggregated_fuel == "gas", "fuel"]
coal_fuels <- c( fuel_list[fuel_list$aggregated_fuel == "coal", "fuel"], 'coal' )
biomass_fuels <- fuel_list[fuel_list$aggregated_fuel == "biomass", "fuel"]
other_fuels <- subset( unique( iea_data_full$fuel ), unique( iea_data_full$fuel )
                       %!in% c( oil_fuels, gas_fuels, coal_fuels, biomass_fuels ) )

IEA_agg_oil <- iea_data_full %>%
    filter(fuel %in% oil_fuels) %>%
    group_by(iso) %>%
    summarize_at(vars(starts_with('X')), sum)
IEA_full_oil <- iea_data_full %>%
    filter(fuel %in% oil_fuels) %>%
    arrange(iso, sector, fuel)

IEA_agg_gas <- iea_data_full %>%
    filter(fuel %in% gas_fuels) %>%
    group_by(iso) %>%
    summarize_at(vars(starts_with('X')), sum)
IEA_full_gas <- iea_data_full %>%
    filter(fuel %in% gas_fuels) %>%
    arrange(iso, sector, fuel)

IEA_agg_coal <- iea_data_full %>%
    filter(fuel %in% coal_fuels) %>%
    group_by(iso) %>%
    summarize_at(vars(starts_with('X')), sum)
IEA_full_coal <- iea_data_full %>%
    filter(fuel %in% coal_fuels) %>%
    arrange(iso, sector, fuel)

# BP
printLog( c("Cleaning BP energy consumption data."))
BP_oil_clean <- bp_energy_data[[ getBPSheetNumber( "oil", "consumption", "tonnes", bp_energy_data ) ]] %>%
    rename_at(vars(as.character(1965:BP_last_year)), ~ paste0('X',1965:BP_last_year)) %>%
    mutate(BPName = `Million tonnes`) %>%  # This could be more robust
    select(BPName, paste0('X',1965:BP_last_year)) %>%
    filter(!is.na(X1965)) %>%
    left_join(MCL %>% select(iso, BPName) %>% unique) %>%
    select(iso, BPName, paste0('X',1965:BP_last_year)) %>%
    filter(!is.na(iso))
BP_agg_oil <- IEA_agg_oil %>%
    select(iso) %>%
    left_join(BP_oil_clean)
check_iea_bp(IEA_agg_oil, BP_agg_oil)
BP_full_oil <- IEA_full_oil %>%
    select(iso,sector, fuel) %>%
    left_join(BP_oil_clean)
check_iea_bp(IEA_full_oil, BP_full_oil)

BP_gas_clean <- bp_energy_data[[ getBPSheetNumber( "gas", "consumption", "EJ", bp_energy_data ) ]]%>%
    rename_at(vars(as.character(1965:BP_last_year)), ~ paste0('X',1965:BP_last_year)) %>%
    mutate(BPName = `Exajoules`) %>%  # This could be more robust
    select(BPName, paste0('X',1965:BP_last_year)) %>%
    filter(!is.na(X1965)) %>%
    left_join(MCL %>% select(iso, BPName) %>% unique) %>%
    select(iso, BPName, paste0('X',1965:BP_last_year)) %>%
    filter(!is.na(iso))
BP_agg_gas <- IEA_agg_gas %>%
    select(iso) %>%
    left_join(BP_gas_clean)
check_iea_bp(IEA_agg_gas, BP_agg_gas)
BP_full_gas <- IEA_full_gas %>%
    select(iso,sector, fuel) %>%
    left_join(BP_gas_clean)
check_iea_bp(IEA_full_gas, BP_full_gas)

BP_coal_clean <- bp_energy_data[[ getBPSheetNumber( "coal", "consumption", "EJ", bp_energy_data ) ]]%>%
    rename_at(vars(as.character(1965:BP_last_year)), ~ paste0('X',1965:BP_last_year)) %>%
    mutate(BPName = `Exajoules`) %>%  # This could be more robust
    select(BPName, paste0('X',1965:BP_last_year)) %>%
    filter(!is.na(X1965)) %>%
    left_join(MCL %>% select(iso, BPName) %>% unique) %>%
    select(iso, BPName, paste0('X',1965:BP_last_year)) %>%
    filter(!is.na(iso))
BP_agg_coal <- IEA_agg_coal %>%
    select(iso) %>%
    left_join(BP_coal_clean)
check_iea_bp(IEA_agg_coal, BP_agg_coal)
BP_full_coal <- IEA_full_coal %>%
    select(iso, sector, fuel) %>%
    left_join(BP_coal_clean)
check_iea_bp(IEA_full_coal, BP_full_coal)

# ------------------------------------------------------------------------------
# 3. Extend with Growth Method

IEA_extended_growth_oil <- extend_iea_growth_aggregate(iea = IEA_full_oil,
                                                       iea_aggregate = IEA_agg_oil,
                                                       bp = BP_agg_oil,
                                                       bp_growth_limit = BP_growth_limit,
                                                       fuel = 'oil')
IEA_extended_growth_gas <- extend_iea_growth_aggregate(iea = IEA_full_gas,
                                                       iea_aggregate = IEA_agg_gas,
                                                       bp = BP_agg_gas,
                                                       bp_growth_limit = BP_growth_limit,
                                                       fuel = 'gas')
IEA_extended_growth_coal <- extend_iea_growth_aggregate(iea = IEA_full_coal,
                                                       iea_aggregate = IEA_agg_coal,
                                                       bp = BP_agg_coal,
                                                       bp_growth_limit = BP_growth_limit,
                                                       fuel = 'coal')

# ------------------------------------------------------------------------------
# 4. Extend with BP IEA units method
# Create a BP to IEA "conversion factor" to create a bp trend in "IEA" units
# mean(iea/bp)*bp_trend = estimated bp trend in IEA units

#coal
IEA_BP_trend_conversion_oil <- iea_to_bp_conversion_factor(iea = IEA_extended_growth_oil,
                                                           bp = BP_full_oil)
BP_trend_IEA_unit_oil <- bp_to_iea_trend(bp = BP_full_oil,
                                     iea_to_bp_conversion = IEA_BP_trend_conversion_oil)
#gas
IEA_BP_trend_conversion_gas <- iea_to_bp_conversion_factor(iea = IEA_extended_growth_gas,
                                                           bp = BP_full_gas)
BP_trend_IEA_unit_gas <- bp_to_iea_trend(bp = BP_full_gas,
                                         iea_to_bp_conversion = IEA_BP_trend_conversion_gas)
#coal
IEA_BP_trend_conversion_coal <- iea_to_bp_conversion_factor(iea = IEA_extended_growth_coal,
                                                           bp = BP_full_coal)
BP_trend_IEA_unit_coal <- bp_to_iea_trend(bp = BP_full_coal,
                                         iea_to_bp_conversion = IEA_BP_trend_conversion_coal)

# ------------------------------------------------------------------------------
# 5. Extension Comparison and Correction

IEA_extended_correction_oil <- extension_correction(iea = IEA_extended_growth_oil,
                                                    iea_bp_trend_conversion = IEA_BP_trend_conversion_oil,
                                                    bp_trend_iea_unit = BP_trend_IEA_unit_oil,
                                                    iea_bp_ratio_limit =  IEA_BP_ratio_limit,
                                                    fuel = 'oil')
IEA_extended_correction_gas <- extension_correction(iea = IEA_extended_growth_gas,
                                                    iea_bp_trend_conversion = IEA_BP_trend_conversion_gas,
                                                    bp_trend_iea_unit = BP_trend_IEA_unit_gas,
                                                    iea_bp_ratio_limit =  IEA_BP_ratio_limit,
                                                    fuel = 'gas')
IEA_extended_correction_coal <- extension_correction(iea = IEA_extended_growth_coal,
                                                    iea_bp_trend_conversion = IEA_BP_trend_conversion_coal,
                                                    bp_trend_iea_unit = BP_trend_IEA_unit_coal,
                                                    iea_bp_ratio_limit =  IEA_BP_ratio_limit,
                                                    fuel = 'coal')

# ------------------------------------------------------------------------------
# 6. Extend Other Fuels

IEA_biomass_extended <- iea_data_full %>%
    filter(fuel %in% biomass_fuels)
IEA_biomass_extended[X_BP_years] <- do.call("cbind", replicate(length(X_BP_years), IEA_biomass_extended[X_IEA_end_year], simplify = FALSE))

IEA_other_extended <- iea_data_full %>%
    filter(fuel %in% other_fuels)
IEA_other_extended[X_BP_years] <- do.call("cbind", replicate(length(X_BP_years), IEA_other_extended[X_IEA_end_year], simplify = FALSE))

# ------------------------------------------------------------------------------
# 7. Aggregate Extended Data

IEA_BP_ext <- IEA_extended_correction_oil %>%
    bind_rows(IEA_extended_correction_gas) %>%
    bind_rows(IEA_extended_correction_coal) %>%
    bind_rows(IEA_biomass_extended) %>%
    bind_rows(IEA_other_extended) %>%
    arrange(iso, sector, fuel)


# ------------------------------------------------------------------------------
# 8. Output

# Add comments for each table
comments.A.energy_data_extension <- c( paste0( "IEA energy statistics",
                                               " by intermediate sector / intermediate fuel / historical year,",
                                               " extendForwarded with BP energy statistics for latest BP years" ) )

# write extended energy data
writeData( IEA_BP_ext, domain = "MED_OUT", fn = "A.IEA_BP_energy_ext",
           comments = comments.A.energy_data_extension )

logStop()
