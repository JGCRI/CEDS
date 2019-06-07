#------------------------------------------------------------------------------
# Program Name: A1.2.Fernandes_biomass.R
# Author: Linh Vu
# Date Last Updated: 7 June 2019
# Program Purpose: This program produces 1850-2013 time series of residential biomass
#                  consumption by country and fuel type from Fernandes biofuels data.
#                  The program also produces fuel-weighted biomass conversion factors
#                  (TJ/kt) by country and year.
# Input Files: Fernandes_Biofuels_9.xlsx, Master_Country_List.csv, A.UN_pop_master.csv,
#              biomass_heat_content.xlsx, Fernandes_proxy_country_mapping.csv
# Output Files: A.Fernandes_residential_biomass.csv, A.Fernandes_biomass_conversion.csv
# Notes: 1. Fernandes provide residential biomass consumption by country and fuel type
#           for 1850-2000.
#        2. Most corrections to the original data (e.g. fix gaps, fill in missing
#           data) are done on rural per-capita basis.
#        3. Where original data is unavailable, use rural per-capita value of a
#           proxy country. For split-up countries, use the value of the composite
#           country before the split.
#        4. Assume minimum of 0.25 kt/rural population Dom Total FW for all years
#           before 1900. Phase in this minimum from 1900 to 1940 (so minimum of
#           zero in 1940).
#        5. If biomass for one year is zero or drops substantially, replace with
#           biomass of the next available year. Extend last non-zero biomass year
#           forward in time.
#        6. Aside from the biomass time series, also calculate residential
#           fuel-weighted biomass conversion factors (TJ/kt) by country.
# TODO: 1. Move Fernandes_years to common_data.R?
#       2. TODO itmesfrom the code review.
#
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
  PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
  headers <- c( "data_functions.R", "analysis_functions.R", "timeframe_functions.R" ) # Any additional function files required
  log_msg <- "Read Fernandes biofuels data"
  script_name <- "A1.2.Fernandes_biomass.R"

  source( paste0( PARAM_DIR, "header.R" ) )
  initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Read raw input files and define useful values
  library( "zoo" )

  # The guess_max prevents some columns from being read in as numeric when there
  # are notes at the bottom that cause warnings
  input <- readData( "ENERGY_IN", "Fernandes_Biofuels_9", ".xlsx",
                     sheet_selection = 2:11, guess_max = 1000 )
  Master_Country_List <- readData( "MAPPINGS", "Master_Country_List" )

# Read and process population data to have rural population
  pop_master <- readData( "MED_OUT", "A.UN_pop_master" ) %>%
    filter( scenario %in% c( "Estimates", "Medium fertility" ) ) %>%
    select( iso, year, pop, urban_share ) %>%
    distinct()
  pop_master$rural_pop <- pop_master$pop * ( 1 - pop_master$urban_share )

# Energy-weight conversion factors by biomass type
  heat_content <- readData( "GEN_IN", "biomass_heat_content", ".xlsx", sheet_selection = "main" ) %>%
    select( fuel, heating_value, units )

# Proxy country mapping for Fernandes
  proxy_mapping <- readData( "MAPPINGS", "Fernandes_proxy_country_mapping" )

# Define useful values
# TODO: Move Fernandes_years to common_data.R?
  ktoe_to_TJ <- 41.8680000  # TJ/ktoe
  Fernandes_regions <- c( "North America", "Latin America", "Africa",
                          "Western Europe", "Eastern Europe/FSU", "Middle East",
                          "South Asia", "East Asia", "Southeast Asia", "Oceania" )
  Fernandes_years <- seq( 1850, 2000 )
  X_Fernandes_years <- paste0( "X", Fernandes_years )

# Biomass per capita threshold, expressed as ratio of current year consumption
# over next year consumption. If pc biomass for one year is zero or drops under
# the threshold relative to the following year, replace it with pc biomass of
# the next available year (with nonzero biomass).
  THRESHOLD <- .5

# ------------------------------------------------------------------------------
# 2. Read and reformat input
# processSingleSheet(): takes one sheet of input in df, returns processed df
  processSingleSheet <- function( i ){  # i: sheet index

  # Read sheet, keep relevant columns rename columns
    df <- input[[ i ]][ 1:152 ]
    df$units <- names( df )[[ 1 ]]  # slice out units from cell (1, 1)
    names( df ) <- c( "fuel", X_Fernandes_years, "units" )

  # Drop aggregation rows (everything after "TOTALS...") and blank rows
    df <- df[ 1: (grep( "TOTALS", df$fuel ) - 1 ), ] %>% filter( !is.na( fuel ) )

  # Some rows of df$fuel contain country name info (generally row 1 and
  # all rows immediately after "Total" rows). Slice out country names from
  # these rows and copy to a separate column
    rows <- c( 1, grep( "^Total$", df$fuel ) + 1, nrow( df ) + 1 ) %>% unique()
    countries <- df$fuel[ rows[ -length( rows ) ] ]  # all country names
    rep_times <- rows - lag( rows )
    rep_times <- rep_times[ -1 ]
    df$country <- rep( countries, rep_times )

  # Drop non-data and aggregated rows
    df <- filter( df, fuel %!in% countries, fuel != "Total" )

    return( df )
  }

# Read all data sheets into a df; clean up
  raw <- lapply( seq_along( input ), processSingleSheet )
  raw <- do.call( rbind,  raw ) %>%
    select_( .dots = c( "country", "units", "fuel", X_Fernandes_years ) )
  raw <- filter( raw, country != "USSR-not included" )
  raw$country <- str_trim( raw$country )
  raw[, X_Fernandes_years ] <- sapply( raw[, X_Fernandes_years ],
                                       function( x ){ as.numeric( as.character( x ) ) } )
  raw$fuel <- str_trim( raw$fuel )
  raw$units <- "kt"

# Reshape from wide to long format; drop rows with NA and 0 biomass
### TODO: moving towards tidyr functions: gather, spread
  raw <- melt( raw, measure.vars = X_Fernandes_years )
  names( raw )[ names( raw ) %in% c( "variable", "value" ) ] <- c( "year", "consumption" )
  raw$year <- as.character( raw$year )
  raw$year <- xYearToNum( raw$year )
  raw <- filter( raw, consumption > 0 )

# Keep only domestic/residential biomass. If a country has urban/rural
# Dom FW split, keep only Dom Total FW to avoid double counting
  raw <- filter( raw, grepl( "Dom", fuel ),
                 fuel %!in% c( "Dom Urban FW", "Dom Rural FW" ) )

# Add islands/territories to their corresponding IEA countries/regions
  mapped <- raw
  mapped$country[ mapped$country == "Bosnia" ] <- "Bosnia and Herzegovina"   # IEA: Bosnia and Herzegovina
  mapped$country[ mapped$country == "Monaco" ] <- "France"                   # IEA: France
  mapped <- group_by( mapped, country, units, fuel, year ) %>%
    dplyr::summarise( consumption = sum( consumption ) )

# Drop all countries without ISO (i.e. those not in Master_Country_List)
  mapped$iso <- Master_Country_List$iso[ match( mapped$country, Master_Country_List$Country_Name, nomatch = NA ) ]  # add ISO

# Diagnostics: What Fern countries are not in Master_Country_List?
# Comoro Islands, St. Helena: not in IEA, ok to leave out;
# Other Asia, Other L America: composite regions, will not use
  in_Fern_not_in_MCL <- filter( mapped, is.na( iso ) ) %>%
    unique() %>%
    dplyr::arrange( country, year, fuel )

# Drop countries with no ISO
  mapped <- filter( mapped, !is.na( iso ) )

# Calculate pc biomass consumption, using rural population when rural
# population is available and nonzero, and using total population otherwise
# What countries do not have rural population for at least 1 year
# (that is not the result of 0 total population)?
# aia, bmu, cym, gib, hkg, mac, mco, nru, sgp, sxm, vat
  no_rural <- unique( pop_master$iso[ is.na( pop_master$urban_share ) |
                                        pop_master$urban_share == 1 ] )

# Create variable pop2 = rural pop if rural_pop available and nonzero,
# = total pop otherwise
  pop_master$rural_pop[ pop_master$iso %in% no_rural ] <-
    pop_master$pop[ pop_master$iso %in% no_rural ]
  pop_master <- select( pop_master, iso, year, rural_pop ) %>%
    unique()
  names( pop_master ) <- c( "iso", "year", "pop2" )

# Compute pc consumption
  mapped_pc <- merge( mapped, pop_master, all.x = T ) %>%
    dplyr::mutate( consumption_pc = consumption / pop2 ) %>%
    select( -pop2, -consumption )

# ------------------------------------------------------------------------------
# 3. Make estimates for split-up countries and countries with no Fernandes
# -- Split-up countries include:
#         Czechoslovakia (csk): split into Czech (cze) and Slovakia (svk)
#         Serbia and Montenegro (scg): split into Serbia (srb), Montenegro (mne),
#                                      and later Kosovo (srb (Kosovo))
#         Yugoslavia: split into Bosnia and Herzegovina (bih), Croatia (hrv),
#                     Macedonia (mkd), Slovenia (svn), Serbia (srb),
#                     Montenegro (mne), and later Kosovo (srb (Kosovo))
# -- For member countries (after a split) and Master_Country_List countries with no
#    Fernandes, use per-capita value of a proxy country where original data is
#    unavailable. Proxy for member countries is the composite country before the split
#    (for srb, mne, and srb (Kosovo), use scg's per-capita). See Fernandes_proxy_country_mapping.csv
#    for complete proxy country mapping.
# -- Do this estimates by fuel and based on rural per-capita biomass (or per-capita
#    if rural population unavailable).

# Make a template of all Master_Country_List countries, Fernandes fuels and all years, from beginning
# of Fernandes to X_IEA_end_year
  full <- merge( unique( select( Master_Country_List, iso, country = Country_Name, region = Region ) ),
                 data.frame( year = unique( c( Fernandes_years, IEA_years ) ) ), all = T ) %>%
    merge( data.frame( unique( mapped_pc[ c( "fuel", "units" ) ] ) ), all = T ) %>%
    merge( mapped_pc, all.x = T )  # add Fernandes data to template

# Change NA consumption_pc to 0
  full$consumption_pc[ is.na( full$consumption_pc ) ] <- 0

# For countries/years without original Fernandes data (per-capita consumption is 0),
# use per-capita value of a proxy country
  proxied <- filter( full, iso %in% proxy_mapping$iso ) %>%
    merge( select( proxy_mapping, iso, iso_proxy ), all.x = T ) %>%
    merge( select( full, fuel, year, iso_proxy = iso,
                   consumption_pc_proxy = consumption_pc ), all.x = T )
  proxied$consumption_pc[ proxied$consumption_pc == 0 ] <-
    proxied$consumption_pc_proxy[ proxied$consumption_pc == 0 ]

# Bind back proxied values
  full_proxied <- bind_rows( filter( full, iso %!in% proxy_mapping$iso ),
                             select( proxied, -iso_proxy, -consumption_pc_proxy ) ) %>%
    dplyr::arrange( iso, year )

# ------------------------------------------------------------------------------
# 4. Apply minimum rural pc biomass
#   Assume minimum of 0.25 kt/rural population Dom Total FW for all years
#   before 1900. Phase in this minimum from 1900 to 1940 (so minimum of zero
#   in 1940)

# Define values used in routine
  MIN_CONSUMPTION_PC_HIST <- .25
  CONVERGENCE_YEAR <- 1900
  PHASE_OUT_YEAR <- 1940

# a. Create df of minimum rural pc biomass
  min_biomass <- data.frame( CONVERGENCE_YEAR = MIN_CONSUMPTION_PC_HIST,
                             PHASE_OUT_YEAR = 0 )
  names( min_biomass ) <- c( CONVERGENCE_YEAR, PHASE_OUT_YEAR )
  min_biomass <- interpolate( min_biomass, CONVERGENCE_YEAR, PHASE_OUT_YEAR )
  min_biomass <- melt( min_biomass, measure = names( min_biomass ) )
  names( min_biomass ) <- c( "year", "consumption_pc_min" )
  min_biomass$year <- as.numeric( as.character( min_biomass$year ) )
  min_biomass <- bind_rows( min_biomass,
                            data.frame( year = seq( min( mapped_pc$year ), CONVERGENCE_YEAR - 1 ),
                                        consumption_pc_min = MIN_CONSUMPTION_PC_HIST ) )
  min_biomass$fuel <- "Dom Total FW"

# b. If Fernandes falls below minimum value, bring up to minimum value
  full_min_fixed <- merge( full_proxied, min_biomass, all.x = T )
  full_min_fixed$consumption_pc_min[ is.na( full_min_fixed$consumption_pc_min ) ] <- 0
  full_min_fixed$consumption_pc[ is.na( full_min_fixed$consumption_pc ) ] <- 0
  full_min_fixed$consumption_pc[ full_min_fixed$consumption_pc < full_min_fixed$consumption_pc_min ] <-
    full_min_fixed$consumption_pc_min[ full_min_fixed$consumption_pc < full_min_fixed$consumption_pc_min ]
  full_min_fixed$consumption_pc_min <- NULL

# ------------------------------------------------------------------------------
# 5. Fix discontinuities and extend Fernandes to cover all emissions years
# -- If pc biomass for one year is zero or drops substantially, replace with pc
#    biomass of the next available year. In pseudo-mathematical notation: If
#    consumption_pc_i is 0, or consumption_pc_(i + 1) is not 0 and
#    consumption_pc_i/consumption_pc_(i + 1) < threshold, replace consumption_pc_i
#    with consumption_pc_(i + 1), where i denotes the current year.
# -- Extend last non-zero per-capita Fernandes forward.

# Iterate over each ISO + fuel combination to fix discontinuities
  full_min_fixed <- dplyr::arrange( full_min_fixed, iso, fuel, dplyr::desc( year ) )
  full_discont_fixed <- ddply( full_min_fixed, .( iso, fuel ), function( df ){
    for ( i_year in seq_along( X_Fernandes_years )[-1] ){
      if( df$consumption_pc[[ i_year - 1]] > 0 &
          df$consumption_pc[[ i_year ]] / df$consumption_pc[[ i_year - 1 ]] < THRESHOLD )
      { df$consumption_pc[[ i_year ]] <- df$consumption_pc[[ i_year - 1 ]] }
    } # END for loop
    return( df )
    } )

# Now the only 0 left should be end years -- extend last nonzero pc consumption forward.
  full_discont_fixed$consumption_pc[ full_discont_fixed$consumption_pc == 0 ] <- NA
  full_discont_fixed <- dplyr::arrange( full_discont_fixed, iso, fuel, year ) %>%
    ddply( .(iso, fuel),
           function( df ){ within( df, { consumption_pc <- na.locf( consumption_pc, na.rm = F ) } ) } )
  full_discont_fixed$consumption_pc[ is.na( full_discont_fixed$consumption_pc ) ] <- 0

# Re-compute total consumption from pc
  full_discont_fixed <- merge( full_discont_fixed, pop_master, all.x = T ) %>%
    dplyr::mutate( consumption = consumption_pc * pop2 ) %>%
    select( -pop2 )

# Diagnostics: Fernandes per-capita biomass
  Fern_pc_wide <- group_by( full_discont_fixed, iso, country, region, units, year ) %>%
    dplyr::summarise( consumption = sum( consumption ), consumption_pc = sum( consumption_pc ) ) %>%
    cast( iso + country + region + units ~ year, value = "consumption_pc" )

# ------------------------------------------------------------------------------
# 6. Calculate residential fuel-weighted biomass conversion factors by country
# Note: Exclude Dom Charcoal from computation since IEA Charcoal is already in kt
  Fernandes_biomass_conversion <- filter( full_discont_fixed, fuel != "Dom Charcoal" ) %>%
    select( -units ) %>%
    merge( heat_content ) %>%
    group_by( iso, units, year ) %>%
    dplyr::summarise( heating_value = weighted.mean( heating_value, consumption ) ) %>%
    data.frame()

# Convert ktoe/kt to TJ/kt
  Fernandes_biomass_conversion$heating_value <- Fernandes_biomass_conversion$heating_value * ktoe_to_TJ
  Fernandes_biomass_conversion$units <- "TJ/kt"

# Cast to wide format
### TODO: moving towards tidyr functions: gather, spread
  Fernandes_biomass_conversion <- filter( Fernandes_biomass_conversion, year %in% IEA_years ) %>%
    cast( iso + units ~ year, value = "heating_value" )

# ------------------------------------------------------------------------------
# 7. Write output
  Fernandes_biomass_conversion[ is.na( Fernandes_biomass_conversion ) ] <- ""
  full_discont_fixed[ is.na( full_discont_fixed ) ] <- ""
  writeData( full_discont_fixed, "MED_OUT", "A.Fernandes_residential_biomass" )
  writeData( Fernandes_biomass_conversion, "MED_OUT", "A.Fernandes_biomass_conversion" )

# Diagnostics
  writeData( Fern_pc_wide, "DIAG_OUT", "A.Fernandes_biomass_pc" )

  logStop()
