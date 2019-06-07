# ------------------------------------------------------------------------------
# Program Name:  A6.3.extend_other_biomass.R
# Authors:       Rachel Hoesly, Caleb Braun
# Last Modified: May, 2018
# Purpose:       Extend other Biomass back with population to zero by certain
#                date.
#
# Input Files:   A.total_activity, A.UN_pop_master
# Output Files:  A.industrial_biomass_extended
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Read in additional header files and start the script log
headers <- "data_functions.R"
log_msg <- "Extending other biomass activity_data before 1960 with Bond data"
script_name <- "A6.3.extend_other_biomass.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )


# ---------------------------------------------------------------------------
# 1. Load files

un_pop   <- readData( "MED_OUT", 'A.UN_pop_master' )
activity <- readData( 'MED_OUT', 'A.total_activity' )


# ---------------------------------------------------------------------------
# 2. Select sectors, script options

other_sectors <- c('1A1a_Electricity-autoproducer',
                   '1A1a_Electricity-public',
                   '1A1a_Heat-production',
                   '1A3ai_International-aviation',
                   '1A3aii_Domestic-aviation',
                   '1A3b_Road',
                   '1A3c_Rail',
                   '1A3dii_Domestic-navigation',
                   '1A3eii_Other-transp',
                   '1A4a_Commercial-institutional',
                   '1A4c_Agriculture-forestry-fishing',
                   '1A5_Other-unspecified')

activity[paste0('X',1750:1959)] <- NA

end_extension_year <- 1970
zero_year <- 1900 # year to which other biomass goes to zero

# process UN population
population <- un_pop %>%
    dplyr::select( iso, year, pop ) %>%
    dplyr::filter( year %in% historical_pre_extension_year:end_year ) %>%
    tidyr::spread( year, pop ) %>%
    dplyr::rename_all( make.names )


# ---------------------------------------------------------------------------
# 3. Extend with population

# select other biomass
other_biomass <- activity[activity$fuel == 'biomass' &
                          activity$sector %in% other_sectors, ]
other_biomass[ , paste0('X', historical_pre_extension_year: zero_year)] <- 0

# extend with population
other_biomass <- extend_data_on_trend_range(driver_trend = population,
                                            input_data = other_biomass,
                                            start = zero_year,
                                            end = end_extension_year,
                                            id_match.driver = c('iso'),
                                            id_match.input = c('iso','sector'))
# slowly blend to zero
biomass_extension_years <- zero_year:end_extension_year
biomass_extension_Xyears <- paste0( 'X', biomass_extension_years )

# percent ( year n )
other_biomass_extended <- other_biomass
for ( n in seq_along( biomass_extension_years ) ) {
  ceds_fraction <- (n-1)*(1/(length(biomass_extension_years)-1))
  zero_fraction <- 1-ceds_fraction
  ceds_split <- other_biomass_extended[, biomass_extension_Xyears[n]]
  zero_split <- rep(0,times = length(ceds_split))

  other_biomass_extended[ , biomass_extension_Xyears[n]] <- zero_split*zero_fraction + ceds_split*ceds_fraction
}


# ---------------------------------------------------------------------------
# 4. Replace NAs with zero, arrange

# Some remaining NAs (small countries with no data)
other_biomass_extended[is.na(other_biomass_extended)] <- 0

other_biomass_extended <- other_biomass_extended %>%
    select(iso, sector, fuel, units, one_of(X_extended_years))

if( anyNA( other_biomass_extended[paste0('X',1750:1959)] ) )
    stop('NAs in in extended other biomass data. Please Check.')

# ---------------------------------------------------------------------------
# 5. Write to database

writeData( other_biomass_extended, "MED_OUT", 'A.other_biomass_extended')

logStop()
