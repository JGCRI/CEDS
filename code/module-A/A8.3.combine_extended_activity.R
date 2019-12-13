#------------------------------------------------------------------------------
# Program Name: A8.3.combine_extended_activity.R
# Author: Rachel Hoesly
# Date Last Updated: November 4, 2019
# Program Purpose: To combine combustion and non-combustion activity data
#                  into one data frame. Modern combustion data is also subset, and
#                  total energy consumption files by aggregate fossil fuel (coal, oil,
#                  gas) are created.
# Input Files: A.comb_int_shipping_adjusted, A.NC_default_activity_extended.csv,
#              Master_Fuel_Sector_List.xlsx
# Output Files: A.total_activity_extended.csv, A.final_comb_activity_modern.csv,
#               A.activity_extended_coal.csv, A.activity_extended_natural_gas.csv,
#               A.activity_extended_oil.csv
# Notes:
# ------------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if( "input" %in% dir( ) ) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( 'data_functions.R', 'common_data.R' ) # Additional function files required.
log_msg <- paste0( "Combining extended combustion data" ) # First message to be printed to the log
script_name <- "A8.3.combine_extended_activity.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 1. Load Data

A.comb_extended <- readData( 'MED_OUT','A.comb_int_shipping_adjusted' )
A.NC_default_activity_extended <- readData( 'MED_OUT','A.NC_default_activity_extended' )
MSL <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Sectors" )

# ---------------------------------------------------------------------------
# 2. Combine combustion and non combustion extended activity data

# All data, all years
total <- A.comb_extended %>%
    dplyr::filter( sector %!in% c( '1A1bc_Other-transformation','1A1bc_Other-feedstocks' ) ) %>%
    dplyr::bind_rows( A.NC_default_activity_extended ) %>%
    dplyr::arrange( iso, sector, fuel )

# Modern era energy data
ceds_comb_modern <- total %>%
    dplyr::filter( sector %!in% c( "1A1bc_Other-feedstocks", "1A1bc_Other-transformation" ) )

column_names <-  c( "iso", "sector", "fuel", "units" )
modern_years <- 1960 : BP_last_year
X_modern_years <- paste0( "X", modern_years )

ceds_comb_modern <-  ceds_comb_modern[ c( column_names,  X_modern_years ) ]

# Keep only combustion sectors
combustion_sectors <- c( MSL[ which( MSL$activity %in% c( 'Energy_Combustion' ) ), 'sector' ] )

ceds_comb_modern <- ceds_comb_modern %>%
    dplyr::filter( sector %in% combustion_sectors )

# ---------------------------------------------------------------------------
# 3. Other Output

# Total fossil fuel by aggregate fuel
total_coal <- A.comb_extended %>%
    dplyr::filter( fuel %in% c( 'hard_coal','brown_coal','coal_coke' ) ) %>%
    dplyr::mutate( fuel = 'coal' ) %>%
    dplyr::group_by( iso, fuel, units ) %>%
    dplyr::summarise_if( is.numeric, sum )

total_natural_gas <- A.comb_extended %>%
    dplyr::filter( fuel %in% c( 'natural_gas' ) ) %>%
    dplyr::group_by( iso, fuel, units ) %>%
    dplyr::summarise_if( is.numeric, sum )

total_oil <- A.comb_extended %>%
    dplyr::filter( fuel %in% c( 'light_oil','diesel_oil','heavy_oil','oil' ) ) %>%
    dplyr::mutate( fuel = 'oil' ) %>%
    dplyr::group_by( iso,units ) %>%
    dplyr::summarise_if( is.numeric, sum )

# Other_tranformation
other_tranformation <- A.comb_extended %>%
    dplyr::filter( sector %in% "1A1bc_Other-transformation" )

other_feedstocks <- A.comb_extended %>%
    dplyr::filter( sector %in% "1A1bc_Other-feedstocks" )

# ----------------------------------------------------------------------------
# 4. Write out the data
writeData( total , "MED_OUT", "A.total_activity_extended" )
writeData( ceds_comb_modern , "MED_OUT", "A.final_comb_activity_modern" )

writeData( total_coal , "MED_OUT", "A.activity_extended_coal" )
writeData( total_natural_gas , "MED_OUT", "A.activity_extended_natural_gas" )
writeData( total_oil , "MED_OUT", "A.activity_extended_oil" )

logStop()
# END
