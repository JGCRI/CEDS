#------------------------------------------------------------------------------
# Program Name: C1.3.proc_NC_user_added_wastewater.R
# Author: Linh Vu
# Date Last Updated: 20 June 2016
# Program Purpose: Process wastewater treatment % to produce default NH3 NC emissions
#                   Outputs will be read in by C1.3.proc_NC_emissions_user_added.R
#                   and overwrite existing emissions
# Input Files:  UN_Percentage_WW_Treatment.xlsx, OECD_Percentage_WW_Treatment.xlsx,
#               E.NH3_REAS_inventory.csv, auxiliary_ww_treatment_trend.csv
#               Master_Country_List.csv, A.UN_pop_master.csv,
#               wastewater_proxy_mapping.csv, Per-Capita_Protein_and_WW_NH3.csv
# Output Files:	C.NH3_NC_wastewater_emissions_user_added.csv
# Notes:
#     Default Wastewater sector emissions are taken to include emissions from untreated waste.
#     NH3 emissions from untreated waste are taken to be far higher than that from treatment
#     facilities, therefore default NH3 emissions are taken to be:
#         Emissions = emissions per-capita * population * (1 - wastewater_treatment_ratio)
#     where wastewater_treatment_ratio is ratio of population with wastewater treatment.
#
#     The default emissions factor is taken to be 1.6 kg/capita globally in recent years,
#     scaled by average nitrogen uptake over time.
#
#     Data for wastewater treatment percentages are incomplete, so proxy countries are
#     used where direct data was not available. Wastewater treatment percentage is assumed to
#     increase over time, and 0 in 1900 for all countries (or 0 in 1970 for countries with
#     no 1970 original data).

# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R", "interpolation_extension_functions.R",
              "timeframe_functions.R" ) # Additional function files required.
log_msg <- "Process wastewater treatment % to produce NH3 NC emissions and EF" # First message to be printed to the log
script_name <- "C1.3.proc_NC_user_added_wastewater.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "NH3"

# ------------------------------------------------------------------------------
# 0.5 Define functions for later use
  library( "zoo" )

# proxyExtendAbs(): Function to extend wastewater treatment % of a country based on
# trend (absolute difference) of a proxy country
# @params:
#     iso_ctry: iso code of the country to be extended
#     iso_proxy: iso code of the proxy country
#     df: wide-format df containing data for both iso_ctry and iso_proxy
# Returns: wide-format df containing extended data of iso_ctry
# Note: Values may be out of range [0, 100] after extending
  proxyExtendAbs <- function( df, iso_ctry, iso_proxy ) {
    df <- filter( df, iso %in% c( iso_ctry, iso_proxy ) )
    first <- first_available_yr$year[ first_available_yr$iso == iso_ctry ]
    last <- last_available_yr$year[ last_available_yr$iso == iso_ctry ]
    adj <- filter( df, iso == iso_proxy )
    out <- filter( df, iso == iso_ctry )
    if ( first > first_ww_yr ) {
      adj[, paste0( "X", first_ww_yr:(first-1) ) ] <- adj[, paste0( "X", first_ww_yr:(first-1) ) ] - adj[, paste0( "X", first ) ]
      out[, paste0( "X", first_ww_yr:(first-1) ) ] <- out[, paste0( "X", first ) ] + adj[, paste0( "X", first_ww_yr:(first-1) ) ]
    }
    if ( last < last_ww_yr ) {
      adj[, paste0( "X", (last+1):last_ww_yr ) ] <- adj[, paste0( "X", (last+1):last_ww_yr ) ] - adj[, paste0( "X", last ) ]
      out[, paste0( "X", (last+1):last_ww_yr ) ] <- out[, paste0( "X", last ) ] + adj[, paste0( "X", (last+1):last_ww_yr ) ]
    }
    return( out )
  }

# proxyExtendRel(): Function to extend wastewater treatment % of a country based on
# trend (relative/ratio) of a proxy country
# @params:
#     iso_ctry: iso code of the country to be extended
#     iso_proxy: iso code of the proxy country
#     df: wide-format df containing data for both iso_ctry and iso_proxy
# Returns: wide-format df containing extended data of iso_ctry
  proxyExtendRel <- function( df, iso_ctry, iso_proxy ){
    df <- filter( df, iso %in% c( iso_ctry, iso_proxy ) )
    first <- first_available_yr$year[ first_available_yr$iso == iso_ctry ]
    last <- last_available_yr$year[ last_available_yr$iso == iso_ctry ]
    adj <- filter( df, iso == iso_proxy )
    out <- filter( df, iso == iso_ctry )
    if ( first > first_ww_yr ) {
      adj[, paste0( "X", first_ww_yr:(first-1) ) ] <- adj[, paste0( "X", first_ww_yr:(first-1) ) ] / adj[, paste0( "X", first ) ]
      out[, paste0( "X", first_ww_yr:(first-1) ) ] <- out[, paste0( "X", first ) ] * adj[, paste0( "X", first_ww_yr:(first-1) ) ]
    }
    if ( last < last_ww_yr ) {
      adj[, paste0( "X", (last+1):last_ww_yr ) ] <- adj[, paste0( "X", (last+1):last_ww_yr ) ] / adj[, paste0( "X", last ) ]
      out[, paste0( "X", (last+1):last_ww_yr ) ] <- out[, paste0( "X", last ) ] * adj[, paste0( "X", (last+1):last_ww_yr ) ]
    }
    return( out )
  }



# proxyReplace(): Function to replace wastewater treatment % of a country by copying
# data of a proxy country
# @params:
#     iso_ctry: iso code of the country to be extended
#     iso_proxy: iso code of the proxy country
#     df: wide-format df containing data for both iso_ctry and iso_proxy
# Returns: wide-format df containing extended data of iso_ctry
  proxyReplace <- function( df, iso_ctry, iso_proxy ) {
    out <- filter( df, iso == iso_ctry )
    proxy <- filter( df, iso == iso_proxy )
    X_years <- names( out )[ grepl( "X", names( out ) ) ]
    out[, X_years ] <- proxy[, X_years ]
    return( out )
  }

# smoothSingleCountry(): Takes df of single country data. Corrects wastewater
# treatment % so that it does not rise going back in time:
# if ww_percent[t-1] < ww_percent[t], replace ww_percent[t-1] with ww_percent[t]
  smoothSingleCountry <- function( df ){
    df <- dplyr::arrange( df, dplyr::desc( year ) )
    if ( nrow( df ) >= 2 ){
      for ( i in seq( 2, nrow( df ) ) ){
        if ( df$ww_percent[ i ] > df$ww_percent[ i - 1 ] )
          df$ww_percent[ i ] <- df$ww_percent[ i - 1 ]
      }
    }
    return( df )
  }

# ------------------------------------------------------------------------------
# 1. Read data

if ( em == "NH3" ) {  # only run script for NH3

  UN_ww_input <- readData( "ACTIVITY_IN", "UN_Percentage_WW_Treatment", ".xlsx",
                       sheet_selection = "Data", domain_extension = "wastewater/" )
  OECD_ww_input <- readData( "ACTIVITY_IN", "OECD_Percentage_WW_Treatment", ".xlsx",
                       sheet_selection = "Data", domain_extension = "wastewater/" )
  REAS_ww_input <- readData( "MED_OUT", paste0( "E.", em, "_REAS_inventory" ) )
  aux_ww_input <- readData( "ACTIVITY_IN", "auxiliary_ww_treatment_trend", ".csv",
                            domain_extension = "wastewater/" )

  proxy_map <- readData( "MAPPINGS", "wastewater_proxy_mapping" )
  Master_Country_List <- readData( "MAPPINGS", "Master_Country_List" )
  UN_pop <- readData( "MED_OUT", "A.UN_pop_master" )

  NH3_em_pc <- readData( "ACTIVITY_IN", "Per-Capita_Protein_and_WW_NH3", domain_extension = "wastewater/" )[ 5:150, c( 1, 4 )]

# ------------------------------------------------------------------------------
# 2. Process and combine wastewater treatment % data
# Produce wastewater treatment % series from UN, OECD and REAS data.
# Smooth out series so that wastewater treatment % does not rise going back in time.

# Process UN
  UN_ww <- UN_ww_input[ c( 1, 4, 5 ) ]
  names( UN_ww ) <- c( "country", "year", "ww_percent_UN" )
  UN_ww$year <- as.numeric( UN_ww$year )
  UN_ww$ww_percent_UN <- as.numeric( UN_ww$ww_percent_UN )
  UN_ww <- filter( UN_ww, !is.na( ww_percent_UN ) )
  UN_ww$country[ UN_ww$country == "China, Hong Kong SAR" ] <- "Hong Kong, China"
  UN_ww$country[ UN_ww$country == "Venezuela (Bolivarian Republic of)" ] <- "Venezuela"
  UN_ww$country[ UN_ww$country == "The Former Yugoslav Rep. of Macedonia" ] <- "Macedonia"
  UN_ww$country[ UN_ww$country == "Korea, Republic of" ] <- "Republic of Korea"
  UN_ww$country[ UN_ww$country == "Republic of Moldova" ] <- "Moldova"
  UN_ww$country[ grepl( "union", UN_ww$country ) ] <- "Reunion"
  UN_ww$iso <- Master_Country_List$iso[ match( UN_ww$country, Master_Country_List$Country_Name)]
  UN_ww <- filter( UN_ww, !is.na( iso ) ) %>% select( -country )

# Process OECD
  OECD_ww <- OECD_ww_input[ c( 1, 6, 7 ) ]
  names( OECD_ww ) <- c( "iso", "year", "ww_percent_OECD" )
  OECD_ww$iso <- tolower( OECD_ww$iso )
  OECD_ww <- filter( OECD_ww, iso %in% Master_Country_List$iso )

# Process REAS: Compute implied % wastewater treatment
  REAS_ww <- filter( REAS_ww_input, sector == "ncomb-others-waste_water" ) %>%
    select( iso, units, X2000:X2008 ) %>% melt( id = c( "iso", "units" ) )
  names( REAS_ww ) <- c( "iso", "units", "year", "em" )
  REAS_ww$year <- xYearToNum( REAS_ww$year )
  REAS_ww <- merge( REAS_ww, select( UN_pop, iso, year, pop ), all.x = T )
  REAS_ww <- dplyr::mutate( REAS_ww, pop_no_ww = em / 1.6E-3,  # assume 1.6 kg/person/year i.e. 1.6E-3 kt/thous. people/year
                     ww_percent_REAS = ( 1 - pop_no_ww / pop )* 100 ) %>%
    select( iso, year, ww_percent_REAS )

# Process Auxiliary trends
  aux_ww <- melt( aux_ww_input, id = c( "iso", "country" ) )
  names( aux_ww ) <- c( "iso", "country", "year", "ww_percent" )
  aux_ww$year <- xYearToNum( aux_ww$year )
  aux_ww <- filter( aux_ww, !is.na( ww_percent ) )

# Combine: Use OECD where available, then fill with UN and REAS
  ww <- merge( UN_ww, OECD_ww, all = T ) %>% merge( REAS_ww, all = T ) %>%
    dplyr::arrange( iso, year )
  diag_overlap <- filter( ww, ( !is.na( ww_percent_UN ) & !is.na( ww_percent_OECD ) ) |
                    ( !is.na( ww_percent_UN ) & !is.na( ww_percent_REAS ) ) |
                    ( !is.na( ww_percent_REAS ) & !is.na( ww_percent_OECD ) ) )  # diagnostic: check overlapping
  ww$ww_percent <- ww$ww_percent_OECD
  ww$ww_percent[ is.na( ww$ww_percent ) ] <- ww$ww_percent_UN[ is.na( ww$ww_percent ) ]
  ww$use_REAS[ is.na( ww$ww_percent ) ] <- T
  ww$ww_percent[ is.na( ww$ww_percent ) ] <- ww$ww_percent_REAS[ is.na( ww$ww_percent ) ]

  # Diagnostics: Write out countries using REAS and either OECD/UN (note chn 2000, 2004 and sgp 2009)
  diag_REAS <- group_by( ww, iso ) %>% filter( anyNA( use_REAS ) & any( use_REAS ) ) %>%
    dplyr::arrange( iso, year )

# Manually remove REAS data for sgp to avoid jumps
  ww_smooth <- ww
  ww_smooth$ww_percent[ ww_smooth$iso == "sgp" & ww_smooth$use_REAS ] <- NA
  ww_smooth <- filter( ww_smooth, !is.na( ww_percent ) )

# Add auxiliary
  ww_smooth <- bind_rows( ww_smooth, select( aux_ww, -country ) )

# Correct jumps: Going back in time, if ww_percent[t-1] < ww_percent[t], replace ww_percent[t-1]
# with ww_percent[t]
  ww_smooth <- ddply( ww_smooth, .(iso), smoothSingleCountry ) %>%
    select( iso, year, ww_percent ) %>% dplyr::arrange( iso, year )

# Diagnostics: Write out unextended wastewater treatment %
  ww_unextended <- filter( ww_smooth, iso %!in% aux_ww$iso )
  ww_unextended$country <- Master_Country_List$Country_Name[
    match( ww_unextended$iso, Master_Country_List$iso ) ]
  ww_unextended <- cast( ww_unextended, iso+country~year, value = "ww_percent" )

# ------------------------------------------------------------------------------
# 3. Extend wastewater treatment percentage time series
# Phase in assumptions to extend wastewater series to all emissions years and
# for all countries in Master_Country_List.
#
# Countries without data or with incomplete data may be extended based on trend
# of a proxy country, replaced with data from a proxy country, or have assumptions
# manually placed in. Extension instructions are detailed in wastewater_proxy_map.csv.
# The order of extension is:
#   1) manually place in assumption
#   2, 3) extend based on trend (absolute difference) of proxy country
#   4) extend based on relative trend of proxy country
#   5) replace with trend of proxy country.
#
# All countries have the last non-NA years carried forward.
#
# Countries without 1970 data are linearly extended backwards to 0 by 1970. All countries
# are linearly extended backwards to 0 by 1900.

  printLog( "Extend wastewater treatment pathway ..." )

# Find first and last data years for each country
  ww_years <- seq( min( ww_smooth$year ), max( ww_smooth$year ) )
  X_ww_years <- paste0( "X", ww_years )
  first_available_yr <- filter( ww_smooth, iso %in% proxy_map$iso, !is.na( ww_percent ) ) %>%
    group_by( iso ) %>% filter( year == min( year ) ) %>% select( iso, year )
  last_available_yr <- filter( ww_smooth, iso %in% proxy_map$iso, !is.na( ww_percent) ) %>%
    group_by( iso ) %>% filter( year == max( year ) ) %>% select( iso, year )
  first_ww_yr <- min( ww_years )
  last_ww_yr <- max( ww_years )

# Interpolate wastewater treatment % between available data years
  ww_interp <- data.frame( iso = unique( ww_smooth$iso ) ) %>%
    merge( data.frame( year = ww_years ) ) %>%
    merge( select( ww_smooth, iso, year, ww_percent ), all.x = T )
  ww_interp$year <- paste0( "X", ww_interp$year )
  ww_interp$region <- Master_Country_List$Region[ match( ww_interp$iso, Master_Country_List$iso ) ]
  ww_interp <- cast( ww_interp, iso + region ~ year, value = "ww_percent" )
  ww_interp[ X_ww_years ] <- interpolate_NAs( ww_interp[ X_ww_years ] )
  ww_interp$iso <- as.character( ww_interp$iso )

# Add all Master_Country_List countries to ww_interp (as template for proxy map)
  to_add <- filter( Master_Country_List, iso %!in% ww_interp$iso ) %>% select( iso, region = Region ) %>% unique()
  ww_interp_full <- dplyr::mutate( ww_interp, hasData = 1 )
  ww_interp_full <- bind_rows( ww_interp_full, to_add )
  ww_interp_full$country <- Master_Country_List$Country_Name[ match( ww_interp_full$iso, Master_Country_List$iso ) ]
  ww_interp_full <- ww_interp_full[ c( "iso", "country", "region", "hasData", X_ww_years ) ] %>% data.frame()

# Process by priority order specified in proxy_map
  # Priority 1: Manually place in assumptions
  # Typically this means putting in anchor data points for countries with no direct data.
  # Later steps will extend these anchor points to produce full series.
  proxy_map_1 <- filter( proxy_map, priority == 1 ) %>% melt( measure = X_ww_years ) %>%
    filter( !is.na( value ) )
  ww_interp_full_1 <- melt( ww_interp_full, measure = X_ww_years )
  ww_interp_full_1$replace_val <- proxy_map_1$value[
    match( paste( ww_interp_full_1$iso, ww_interp_full_1$variable ),
           paste( proxy_map_1$iso, proxy_map_1$variable ) ) ]
  # Print warning if there are assumptions where original data exists
  collision <- !is.na( ww_interp_full_1$value ) & !is.na( ww_interp_full_1$replace_val )
  collision <- paste( ww_interp_full_1$iso[ collision ], ww_interp_full_1$variable[ collision ] )
  if ( length( collision ) > 0 ){
    warning( paste( "Attempting to place assumptiong where data exists. No assumptions placed for the following:\n",
                    paste( collision, collapse = "\n" ) ) )
  }
  ww_interp_full_1$value[ is.na( ww_interp_full_1$value) & !is.na( ww_interp_full_1$replace_val ) ] <-
    ww_interp_full_1$replace_val[ is.na( ww_interp_full_1$value) & !is.na( ww_interp_full_1$replace_val ) ]
  ww_interp_full_1$replace_val <- NULL
  ww_interp_full_1 <- cast( ww_interp_full_1 )
  # Interpolate again
  ww_interp_full_1[ X_ww_years ] <- interpolate_NAs( ww_interp_full_1[ X_ww_years ] )

  # Priority 2-3: Extend backward to 1970 by following absolute trend of proxy country:
  #     Country[t = t_start-1] = Country[t_start] + ( Proxy[t_start-1] - Proxy[t_start] )
  # Some countries are first extended then later used as proxy for other countries, so need
  # to extend in two different rounds.

  # Priority 2: Proxy-extend absolute trend of base countries (i.e. countries that will be used as proxy)
  proxy_map_2 <- filter( proxy_map, priority == 2 ) %>% dplyr::arrange( iso )
  ww_interp_full_2 <- filter( ww_interp_full_1, iso %!in% proxy_map_2$iso )
  extended <- filter( ww_interp_full_1, iso %in% proxy_map_2$iso ) %>% dplyr::arrange( iso )
  for ( i in seq_along( extended[ , 1] ) ) {
    iso_ctry <- proxy_map_2$iso[[ i ]]
    iso_proxy <- proxy_map_2$proxy[[ i ]]
    out <- proxyExtendAbs( ww_interp_full_1, iso_ctry, iso_proxy )
    ww_interp_full_2 <- bind_rows( ww_interp_full_2, out )
  }
  ww_interp_full_2 <- dplyr::arrange( ww_interp_full_2, iso ) %>% data.frame()
  ww_interp_full_2[ X_ww_years ][ ww_interp_full_2[ X_ww_years ] < 0 ] <- 0
  ww_interp_full_2[ X_ww_years ][ ww_interp_full_2[ X_ww_years ] > 100 ] <- 100

  # Priority 3: Proxy-extend absolute trend of non-base countries
  # (i.e. countries that will not be used as proxy)
  proxy_map_3 <- filter( proxy_map, priority == 3 ) %>% dplyr::arrange( iso )
  ww_interp_full_3 <- filter( ww_interp_full_2, iso %!in% proxy_map_3$iso )
  extended <- filter( ww_interp_full_2, iso %in% proxy_map_3$iso ) %>% dplyr::arrange( iso )
  for ( i in seq_along( extended[ , 1] ) ) {
    iso_ctry <- proxy_map_3$iso[[ i ]]
    iso_proxy <- proxy_map_3$proxy[[ i ]]
    out <- proxyExtendAbs( ww_interp_full_2, iso_ctry, iso_proxy )
    ww_interp_full_3 <- bind_rows( ww_interp_full_3, out )
  }
  ww_interp_full_3 <- dplyr::arrange( ww_interp_full_3, iso ) %>% data.frame()
  ww_interp_full_3[ X_ww_years ][ ww_interp_full_3[ X_ww_years ] < 0 ] <- 0
  ww_interp_full_3[ X_ww_years ][ ww_interp_full_3[ X_ww_years ] > 100 ] <- 100

  # Priority 4: Proxy-extend following relative trend of proxy country
  proxy_map_4 <- filter( proxy_map, priority == 4 ) %>% dplyr::arrange( iso )
  ww_interp_full_4 <- filter( ww_interp_full_3, iso %!in% proxy_map_4$iso )
  extended <- filter( ww_interp_full_3, iso %in% proxy_map_4$iso ) %>% dplyr::arrange( iso )
  for ( i in seq_along( extended[ , 1] ) ) {
    iso_ctry <- proxy_map_4$iso[[ i ]]
    iso_proxy <- proxy_map_4$proxy[[ i ]]
    out <- proxyExtendRel( ww_interp_full_3, iso_ctry, iso_proxy )
    ww_interp_full_4 <- bind_rows( ww_interp_full_4, out )
  }
  ww_interp_full_4 <- dplyr::arrange( ww_interp_full_4, iso ) %>% data.frame()

  # Priority 5: Replace with data of specified proxy country
  proxy_map_5 <- filter( proxy_map, priority == 5 ) %>% dplyr::arrange( iso )
  ww_interp_full_5 <- filter( ww_interp_full_4, iso %!in% proxy_map_5$iso )
  extended <- filter( ww_interp_full_4, iso %in% proxy_map_5$iso ) %>% dplyr::arrange( iso )
  for ( i in seq_along( extended[ , 1] ) ) {
    iso_ctry <- proxy_map_5$iso[[ i ]]
    iso_proxy <- proxy_map_5$proxy[[ i ]]
    out <- proxyReplace( ww_interp_full_4, iso_ctry, iso_proxy )
    ww_interp_full_5 <- bind_rows( ww_interp_full_5, out )
  }
  ww_interp_full_5 <- dplyr::arrange( ww_interp_full_5, iso ) %>% data.frame()

# Add all emissions years
  ww_interp_ext <- ww_interp_full_5
  ww_interp_ext[ X_extended_years[ X_extended_years %!in% X_ww_years ] ] <- NA
  ww_interp_ext <- ww_interp_ext[ c( "iso", "country", "region", X_extended_years ) ]

# Extend last non-NA year forward
  ww_interp_fwd <- melt( ww_interp_ext, id = c( "iso", "country", "region" ) ) %>%
    dplyr::arrange( iso, variable )
  ww_interp_fwd <- ddply( ww_interp_fwd, .(iso), function( df ){
    within( df, { value <- na.locf( value, na.rm = F ) } )
  })
  ww_interp_fwd <- cast( ww_interp_fwd ) %>% dplyr::arrange( iso )

# For countries with no 1970 data, linearly extrapolate to 0 by 1970.
# For chn only, linearly extrapolate to 0 by 1980
  ww_interp_bwd <- ww_interp_fwd
  ww_interp_bwd$X1970[ is.na( ww_interp_bwd$X1970 ) ] <- 0
  ww_interp_bwd$X1980[ ww_interp_bwd$iso == "chn" ] <- 0
  ww_interp_bwd[ X_extended_years ] <- interpolate_NAs( ww_interp_bwd[ X_extended_years ] )

# For all countries, linearly extrapolate to 0 by 1900
  ww_interp_bwd[ paste0( "X", min( extended_years ):1900 ) ] <- 0
  ww_interp_bwd[ X_extended_years ] <- interpolate_NAs( ww_interp_bwd[ X_extended_years ] )

# Drop auxiliary trends
  ww_interp_bwd <- filter( ww_interp_bwd, iso %!in% aux_ww$iso )

# ------------------------------------------------------------------------------
# 4. Compute default NH3 emissions and EFs
# Default NH3 emissions are computed from NH3 emissions per-capita estimates
# (provided in Per-Capita_Protein_and_WW_NH3.csv), population data, and
# wastewater treatment % (calculated above):
#     emissions = emissions per-capita * population * (1 - wastewater treatment ratio)
# Default NH3 EFs:
#     EF = emission per-capita * (1 - wastewater treatment ratio)

  ww_interp_bwd_long <- melt( ww_interp_bwd, id = c( "iso", "country", "region" ) )
  names( ww_interp_bwd_long ) <- c( "iso", "country", "region", "year", "ww_treatment_percent" )
  ww_interp_bwd_long$year <- xYearToNum( ww_interp_bwd_long$year )

# Extend NH3 per-capita emissions to all emissions years
  names( NH3_em_pc ) <- c( "year", "NH3_pc" )
  NH3_em_pc$NH3_pc <- as.numeric( as.character( NH3_em_pc$NH3_pc ) )
  NH3_em_pc_ext <- merge( data.frame( year = extended_years ), 
                       NH3_em_pc, all = T ) %>% dplyr::arrange( year )
  NH3_em_pc_ext$NH3_pc <- na.locf( NH3_em_pc_ext$NH3_pc, na.rm = F )
  NH3_em_pc_ext$NH3_pc <- na.locf( NH3_em_pc_ext$NH3_pc, na.rm = F, fromLast = T )

# Compute default emissions
  default <- merge( ww_interp_bwd_long, select( UN_pop, iso, year, pop ), all.x = T ) %>%  # pop in thous. people
    merge( NH3_em_pc_ext, all.x = T )  # NH3_pc in kg/person
  default <- dplyr::mutate( default, emissions = NH3_pc*pop*(100-ww_treatment_percent)*1E-5,   # emissions in kt
                     EF = NH3_pc*(100-ww_treatment_percent)*1E-5 )
  default$em_units <- "kt"
  default$EF_units <- "kt/1000"
  default$sector <- "5D_Wastewater-handling"
  default$fuel <- "process"
  default <- filter( default, iso %!in% c( "yug", "ussr", "global" ) )
  default <- select( default, iso, sector, fuel, em_units, EF_units, year, emissions, EF )
  default$year <- paste0( "X", default$year )

# Cast to wide
  default_em <- select( default, iso, sector, fuel, units = em_units, year, value = emissions ) %>%
    cast( iso + sector + fuel + units ~ year )
  default_EF <- select( default, iso, sector, fuel, units = EF_units, year, value = EF ) %>%
    cast( iso + sector + fuel + units ~ year )

# Only keep emissions years
  default_em_out <- select_( default_em, .dots = c( "iso", "sector", "fuel", "units", X_emissions_years ) )

# ------------------------------------------------------------------------------
# 5. Output
  writeData( default_em_out, "DEFAULT_EF_IN", domain_extension = "non-combustion-emissions/",
             fn = paste0( "C.", em, "_NC_wastewater_emissions_user_added" ) )
  writeData( default_em, "MED_OUT", paste0( "C.", em, "_NC_emissions_wastewater_full" ) )
  writeData( ww_interp_bwd, "DIAG_OUT", "C.wastewater_treatment_percent" )
  writeData( ww_unextended, "DIAG_OUT", "C.wastewater_treatment_percent_unextended" )
  writeData( default_EF, "DIAG_OUT", paste0( "C.", em, "_EF_wastewater" ) )
}

logStop()
