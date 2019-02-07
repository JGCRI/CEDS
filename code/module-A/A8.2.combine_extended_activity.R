#------------------------------------------------------------------------------
# Program Name: A8.2.combine_extended_activity.R
# Author: Rachel Hoesly
# Date Last Updated: 07 February 2019
# Program Purpose:
# Input Files: A.comb_user_added.csv
#              A.NC_default_activity_extended.csv
# Output Files: A.total_activity_extended.csv
#               A.activity_extended_coal.csv
#               A.activity_extended_natural_gas.csv
#               A.activity_extended_oil.csv
# Notes:

# ------------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c('data_functions.R', 'common_data.R') # Additional function files required.
log_msg <- paste0( "Combining extended combustion data" ) # First message to be printed to the log
script_name <- "A8.2.combine_extended_activity.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 1. Load Data

A.comb_extended <- readData('MED_OUT','A.comb_user_added')
A.NC_default_activity_extended <- readData('MED_OUT','A.NC_default_activity_extended')


# ---------------------------------------------------------------------------
# 2. Combine combustion and non combustion extended activity data

A.comb_extended <- A.comb_extended %>%
    dplyr::mutate(units = 'kt') %>%
    dplyr::rename( sector = CEDS_sector, fuel = CEDS_fuel ) %>%
    dplyr::select( -agg_sector, -agg_fuel)

total <- A.comb_extended %>%
    dplyr::filter(sector %!in% c('1A1bc_Other-transformation','1A1bc_Other-feedstocks') ) %>%
    dplyr::bind_rows( A.NC_default_activity_extended ) %>%
    dplyr::arrange(iso, sector, fuel)


# 10. Other Output ---------------------------------------------------------------------

# Total fossil fuel by aggregate fuel
total_coal <- A.comb_extended %>%
    filter(fuel %in% c('hard_coal','brown_coal','coal_coke')) %>%
    mutate(fuel = 'coal') %>%
    group_by(iso, fuel, units) %>%
    summarise_if(is.numeric, sum)
total_natural_gas <- A.comb_extended %>%
    filter(fuel %in% c('natural_gas')) %>%
    group_by(iso, fuel, units) %>%
    summarise_if(is.numeric, sum)
total_oil <- A.comb_extended %>%
    filter(fuel %in% c('light_oil','diesel_oil','heavy_oil','oil')) %>%
    mutate(fuel = 'oil') %>%
    group_by(iso,units) %>%
    summarise_if(is.numeric, sum)

# other_tranformation
other_tranformation <- A.comb_extended %>%
    filter(sector %in% "1A1bc_Other-transformation")
other_feedstocks <- A.comb_extended %>%
    filter(sector %in% "1A1bc_Other-feedstocks")

# ----------------------------------------------------------------------------

# Write out the data
writeData( total , "MED_OUT", "A.total_activity_extended" )

writeData( total_coal , "MED_OUT", "A.activity_extended_coal" )
writeData( total_natural_gas , "MED_OUT", "A.activity_extended_natural_gas" )
writeData( total_oil , "MED_OUT", "A.activity_extended_oil" )

logStop()
# END