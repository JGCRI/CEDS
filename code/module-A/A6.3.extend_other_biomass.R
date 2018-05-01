# ------------------------------------------------------------------------------
# Program Name: A6.3.extend_other_biomass.R
# Authors: Rachel Hoesly, Caleb Braun
# Program Purpose: Extend other Biomass back with population to zero by certain date.
#
# Output Files:''
# TODO: extend with Bond data
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R","process_db_functions.R") # Additional function files may be required.
log_msg <- "Extending other biomass activity_data before 1960 with Bond data" # First message to be printed to the log
script_name <- "A6.3.extend_other_biomass.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 1. Load files

bond_ctypes = c(rep("text", 5), rep("numeric", 4), "skip") # last column contains value in last cell: set option to skip
bond_historical <- readData( "EM_INV", domain_extension = "Bond-BCOC/" ,"160227_SPEW_BCOCemission", ".xlsx", column_types = bond_ctypes )

iso_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_country_map", meta = F )
fuel_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_fuel_map", meta = F )
sector_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_sector_map", meta = F )
iea_start_year <- readData( 'ENERGY_IN' , 'IEA_iso_start_data', meta = F )
un_pop <- readData( "MED_OUT" , 'A.UN_pop_master' )

ceds_comb_activity <- readData( 'MED_OUT', paste0( 'A.total_activity' ) )
# ---------------------------------------------------------------------------
# 2. Select sectors, script options

other_sectors <- c('1A1a_Electricity-autoproducer','1A1a_Electricity-public',
                   '1A1a_Heat-production', '1A3ai_International-aviation',
                   '1A3aii_Domestic-aviation', '1A3b_Road', '1A3c_Rail',
                   '1A3dii_Domestic-navigation', '1A3eii_Other-transp',
                   '1A4a_Commercial-institutional', '1A4c_Agriculture-forestry-fishing',
                   '1A5_Other-unspecified')

activity <- ceds_comb_activity
activity[paste0('X',1750:1959)] <- NA

# Year to which other biomass goes to zero
zero_year <- 1900
end_extension_year <- 1970

# process un population
un_pop$X_year <- paste0( "X" , un_pop$year)
un_pop$pop <- as.numeric(un_pop$pop)
population <- cast( un_pop[which ( un_pop$year %in% historical_pre_extension_year:end_year ) , ] ,
                    iso ~ X_year, value = 'pop')

# ---------------------------------------------------------------------------
# 4. Extend with population

# select other biomass
other_biomass <- activity[which( activity$fuel == 'biomass' &
                  activity$sector %in% other_sectors),]
other_biomass[, paste0('X',historical_pre_extension_year: zero_year)] <- 0

# extend with population
other_biomass <- extend_data_on_trend_range(driver_trend = population,
                                            input_data = other_biomass,
                                        start = zero_year,
                                        end = end_extension_year,
                                        id_match.driver = c('iso'),
                                        id_match.input = c('iso','sector'))
# Slowly Blend to Zero
biomass_extension_years <- zero_year:end_extension_year

# percent ( year n )
other_biomass_extended <- other_biomass
for ( n in seq_along( biomass_extension_years)){
  ceds_fraction <- (n-1)*(1/(length(biomass_extension_years)-1))
  zero_fraction <- 1-ceds_fraction
  ceds_split <- other_biomass_extended[,paste0('X',biomass_extension_years[n])]
  zero_split <- rep(0,times = length(ceds_split))

  other_biomass_extended[,paste0('X',biomass_extension_years[n])] <- zero_split*zero_fraction + ceds_split*ceds_fraction

}
# ---------------------------------------------------------------------------
# 5. Add to Activity Database


other_biomass_extended <- other_biomass_extended %>%
    select(iso, sector, fuel, units, one_of(X_extended_years))

# ---------------------------------------------------------------------------
# 4. Write to database

  writeData( activity, "MED_OUT" , 'A.other_biomass_extended')

logStop()
