#------------------------------------------------------------------------------
# Program Name: A2.2.IEA_biomass_fix.R
# Author: Linh Vu
# Date Last Updated: 3 May 2016
# Program Purpose:  This program corrects system (IEA) residential biomass using
#                   EIA, Fernandes et al. 2007, and Denier van der Gon et al. 2015
#                   (European) biomass data.
#                   The program also produces 1700-IEA_end_year residential biomass series
#                   for all CEDS countries.
# Input Files:     A.en_stat_sector_fuel.csv, A.Fernandes_residential_biomass,
#                  Europe_wooduse_Europe_TNO_4_Steve.xlsx, A.UN_pop_master.csv,
#                  EIA_Table_10.2a_Renewable_Energy_Consumption___Residential_and_Commercial_Sectors.xlsx,
#                  IEA_biomass_double_counting.xlsx
# Output Files:    A.en_biomass_fix.csv, A.residential_biomass_full.csv
# Notes:
# TODO:
#
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R", "timeframe_functions.R" ) # Any additional function files required
log_msg <- "Refine CEDS residential biomass"
script_name <- "A2.2.IEA_biomass_fix.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )


# ------------------------------------------------------------------------------
# 1. Read raw input files and define useful values
    loadPackage( "zoo" )

# Read population
    pop_master <- readData( "MED_OUT", "A.UN_pop_master" )

# Read energy data
    IEA_en <- readData( "MED_OUT", "A.en_stat_sector_fuel" )
    Fern_biomass <- readData( "MED_OUT", "A.Fernandes_residential_biomass" )
    Eur_biomass <- readData( "ENERGY_IN", "Europe_wooduse_Europe_TNO_4_Steve", ".xlsx",
                              sheet_selection = "Data", skip = 3 )[ c( 1, 5 ) ]
    EIA_biomass <- readData( "ENERGY_IN", "EIA_Table_10.2a_Renewable_Energy_Consumption___Residential_and_Commercial_Sectors",
                             ".xlsx", sheet_selection = "Annual Data", skip = 10 )[ c( 1, 4 ) ]

# Read biomass double-counting correction
    IEA_correction <- readData( "ENERGY_IN", "IEA_biomass_double_counting", ".xlsx" )

# Read/define conversion factors
    # 2005 TJ/kt for USA and European countries
    Master_Country_List <- readData( "MAPPINGS", "Master_Country_List" )
    kt_to_TJ <- readData( "MED_OUT", "A.Fernandes_biomass_conversion" )
    kt_to_TJ$region <- Master_Country_List$Region[ match( kt_to_TJ$iso, Master_Country_List$iso ) ]
    kt_to_TJ <- select( kt_to_TJ, iso, units, region, X2005 ) %>%
      filter( iso == "usa" | grepl( "Europe", region ) )
    kt_to_TJ <- mean( kt_to_TJ$X2005, na.rm = T )

    tBtu_to_TJ <- 0.94782 * 10^3 # source: eia.gov/cfapps/ipdbproject/docs/unitswithpetro.cfm


# ------------------------------------------------------------------------------
# 2. Combine IEA, EIA, Fernandes, and Europe biomass into one df
# Prepare population data
    pop_master <- filter( pop_master, scenario == "Estimates" ) %>%
      dplyr::mutate( rural_pop = pop * ( 1 - urban_share ) )  # compute rural pop

    # What countries have 0 or NA rural pop for at least 1 year (that is
    # not the result of 0 total population)?
    no_rural <- unique( pop_master$iso[ is.na( pop_master$urban_share ) |
                                          pop_master$urban_share == 1 ] )

    # Create variable pop2 = rural pop if rural_pop available and nonzero,
    # = total pop otherwise
    pop_master$rural_pop[ pop_master$iso %in% no_rural ] <-
      pop_master$pop[ pop_master$iso %in% no_rural ]
    pop_master <- select( pop_master, iso, year, rural_pop ) %>% unique()
    names( pop_master ) <- c( "iso", "year", "pop2" )

# Slice out IEA residential biomass
    IEA_biomass <- filter( IEA_en, sector == "1A4b_Residential", fuel == "biomass" ) %>%
      melt( measure = X_IEA_years )
    names( IEA_biomass )[ names( IEA_biomass ) %in% c( "variable", "value" ) ] <- c( "year", "IEA" )
    IEA_biomass$year <- xYearToNum( IEA_biomass$year )

# Aggregate Fernandes biomass by country
    Fern_biomass <- group_by( Fern_biomass, iso, units, year ) %>%
      dplyr::summarise( Fern = sum( consumption ) )

# Convert Europe biomass to standard format
    names( Eur_biomass ) <- c( "iso", "Eur" )
    Eur_biomass <- filter( Eur_biomass, !is.na( iso ) )
    Eur_biomass$iso[ Eur_biomass$iso == "yug" ] <- "scg"  # Serbia and Montenegro
    Eur_biomass$iso <- tolower( Eur_biomass$iso )
    Eur_biomass$Eur <- Eur_biomass$Eur / kt_to_TJ  # convert from TJ to kt
    Eur_biomass$units <- "kt"
    Eur_biomass$year <- 2005

# Convert EIA data (US only) to standard format
    names( EIA_biomass ) <- c( "year", "EIA" )
    EIA_biomass <- filter( EIA_biomass, !is.na( year ) )
    EIA_biomass$EIA <- as.numeric( as.character( EIA_biomass$EIA ) )
    EIA_biomass$iso <- "usa"
    EIA_biomass$EIA <- EIA_biomass$EIA * tBtu_to_TJ  # trillion Btu to TJ
    EIA_biomass$EIA <- EIA_biomass$EIA / kt_to_TJ  # TJ to kt
    EIA_biomass$units <- "kt"

# Merge all 4 datasets and keep only emissions years
    biomass_0 <- merge( IEA_biomass, Fern_biomass, all = T ) %>%
      merge( Eur_biomass, all.x = T ) %>%
      merge( EIA_biomass, all.x = T ) %>%
      filter( year %in% emissions_years )

    # Change 0 biomass to NA
    biomass_0$IEA[ biomass_0$IEA == 0 ] <- NA
    biomass_0$Fern[ biomass_0$Fern == 0 ] <- NA
    biomass_0$EIA[ biomass_0$EIA == 0 ] <- NA
    biomass_0$Eur[ biomass_0$Eur == 0 ] <- NA


# ------------------------------------------------------------------------------
# 3. Decide which source to use for each country
# General rules:
# -- Use EIA data for USA.
# -- Where Europe is available and differs from IEA by more than 30% (note Europe data
#    is only available 2005), print out diagnostics to decide whether to use IEA or
#    Fernandes.
# -- For remaining countries, use Fernandes if IEA is smaller than Fernandes by more
#    than 75%, for N = 5 years in a row centered at 2000.
# -- For remaining countries, use IEA.
    printLog( "Combine IEA, EIA, Fernandes, and European biomass")

# Define values for routine
    N <- 5
    N_CENTER_YEAR <- 2000
    MIN_IEA_OVER_FERN <- .25
    MIN_IEA_OVER_EUR <- .7
    MAX_IEA_OVER_EUR <- 10/7

# First decide what database to use for each country
    # Substitute EIA for IEA for USA
    biomass_1 <- biomass_0
    biomass_1$IEA[ biomass_1$iso == "usa" ] <- biomass_1$EIA[ biomass_1$iso == "usa" ]

    # Calculate IEA/Fern and IEA/Eur ratio
    biomass_1 <- dplyr::mutate( biomass_1, IEA_over_Fern = IEA / Fern,
                         IEA_over_Eur = IEA / Eur )

    # If IEA differs from Eur by over 30%, take notes to decide later whether to use
    # IEA or Fernandes
    iso_Eur <- filter( biomass_1, IEA_over_Eur < MIN_IEA_OVER_EUR |
                          IEA_over_Eur > MAX_IEA_OVER_EUR )
    iso_Eur <- unique( iso_Eur$iso )
    iso_rest <- setdiff( unique( biomass_1$iso ), iso_Eur )

    # For remaining countries, use Fernandes if IEA is > 75% smaller than Fernandes
    # (evaluated for N years, centered at N_CENTER_YEAR)
      # Pick N years closest to N_CENTER_YEAR
    iso_Fern <- filter( biomass_1, iso %in% iso_rest ) %>%
      dplyr::mutate( d = abs( year - N_CENTER_YEAR ) )  # distance from center year
    iso_Fern$d[ is.na( iso_Fern$IEA_over_Fern ) ] <- NA  # skip years with NA IEA/Fern ratio
    iso_Fern <- dplyr::arrange( iso_Fern, d )
    iso_Fern <- by( iso_Fern, iso_Fern$iso, head, n = N )
    iso_Fern <- Reduce( rbind, iso_Fern )

      # Use Fernandes if IEA_over_Fern is under threshold for all N years
    iso_Fern <- group_by( iso_Fern, iso ) %>%
      filter( all( IEA_over_Fern < MIN_IEA_OVER_FERN, na.rm = T ) )
    iso_Fern <- unique( iso_Fern$iso )

    # For remaining countries, use IEA
    iso_IEA <- setdiff( iso_rest, iso_Fern )

# Compute pc biomass (using pop2)
    biomass_1 <- merge( biomass_1, pop_master, all.x = T ) %>%
      dplyr::mutate( IEA_pc = IEA / pop2, Fern_pc = Fern / pop2, Eur_pc = Eur / pop2 )

# Diagnostics: Decide whether to use IEA or Fernandes for countries in iso_Eur
    diag_Eur <- filter( biomass_1, iso %in% iso_Eur ) %>%
      select( iso, year, pop2, IEA, Eur, Fern, IEA_pc, Eur_pc, Fern_pc )
    # Resolution: Use Fern for aze, rus, svk, ukr; use IEA for geo, irl, mda, swe
    iso_Fern <- c( iso_Fern, "aze", "rus", "svk", "ukr" )
    iso_IEA <- c( iso_IEA, "geo", "irl", "mda", "swe" )
    rm( iso_Eur, iso_rest )

# Keep relevant columns
    biomass_1 <- select( biomass_1, iso, units, year, pop2, IEA, Fern, Eur,
                         IEA_pc, Fern_pc, Eur_pc )

# ------------------------------------------------------------------------------
# 4. Correct gaps in IEA residential biomass
# -- Use rural per capita biomass for detecting/correcting gaps, and compute back
#   total biomass after done correcting.
# -- If IEA is missing or breaks, use Fernandes growth trend to go back in time.
#   IEA should eventually converge to Fernandes.
# -- Note there are two types of gaps: missing (NA) biomass, or breaks (year-to-year
#   ratio exceeding a specified threshold). Missing biomass could be for edge years
#   (most countries), or inner years (for che and deu).
# -- Note that Fernandes has been corrected for discontinuities, now has full series.
    printLog( "Correct gaps in IEA residential biomass" )

# Define values used in routine
    MIN_IEA_YR_TO_YR <- .5  # for residential biomass
    MAX_IEA_YR_TO_YR <- 2
    YEAR_SEQUENCE <- seq( 1969, 1971 )  # for computing maximum limit in 4c

# Slice out countries that use IEA data
    biomass_1_IEA <- filter( biomass_1, iso %in% iso_IEA )

    # Prepare for 4a
    biomass_1_IEA$IEA_pc_ext_a <- biomass_1_IEA$IEA_pc  # will change in 4a
    biomass_1_IEA$flag_a <- NA  # ==1 if changed in 4a
    biomass_1_IEA <- dplyr::arrange( biomass_1_IEA, iso, dplyr::desc( year ) )

# 4a. To fix NA: Starting from most recent year and going backwards in time, if IEA
#   is missing, extrapolate using IEA/Fernandes ratio of the average of the 3 years
#   immediately following (after) the break. Cascade IEA if needed. Result is full
#   IEA series.
    biomass_1a_IEA <- biomass_1_IEA

# Diagnostic: What countries have NA IEA?
    diag_IEA_NA <- filter( biomass_1a_IEA, is.na( IEA_pc ) ) %>%
      group_by( iso ) %>%
      dplyr::mutate( diff = year - lead( year ), first_yr = min( year ) )

    # What countries have irregular NA?
    # che: 1983-1992; deu: 1960-1969, 1986-1989; pol: 1990-1992
    # kwt: 1960-1970, 1997-2013
    diag_IEA_NA_irreg <- filter( diag_IEA_NA, first_yr > min( IEA_years ) | diff != 1 )
    diag_IEA_NA_irreg <- filter( diag_IEA_NA, iso %in% diag_IEA_NA_irreg$iso )

# kwt has NA IEA for the rightmost edge years (1997-IEA_end_year) -- extend average
# of the last 3 available years (1994-1996) forward
    kwt_avg <- filter( biomass_1a_IEA, iso == "kwt", year %in% seq( 1994, 1996 ) )
    kwt_avg <- mean( kwt_avg$IEA_pc )
    biomass_1a_IEA$flag_a[ biomass_1a_IEA$iso == "kwt" & biomass_1a_IEA$year >= 1997 ] <- 1
    biomass_1a_IEA$IEA_pc_ext_a[ biomass_1a_IEA$iso == "kwt" & biomass_1a_IEA$year >= 1997 ] <- kwt_avg

# Make a df of last (most recent) year with NA IEA
    last_IEA_NA <- filter( biomass_1a_IEA, is.na( IEA_pc_ext_a ) ) %>%
      group_by( iso ) %>%
      dplyr::mutate( last_yr = max( year ) ) %>%
      select( iso, last_yr ) %>% unique()

# For now, loop backwards from most recent years to fill in NA. The following
# loop works because the 3 rightmost edge years are not NA
    biomass_1a_IEA_iso <- unique( biomass_1a_IEA$iso )
    biomass_1a_IEA <- ddply( biomass_1a_IEA, .( iso ), function( df ){
      for( i_year in seq_along( df$year ) ){
        if( is.na( df$IEA_pc_ext_a[[ i_year ]] ) ){
          df$flag_a[[ i_year ]] <- 1
          df$IEA_pc_ext_a[[ i_year ]] <- df$Fern_pc[[ i_year ]] *
            ( df$IEA_pc_ext_a[[ i_year - 1 ]] + df$IEA_pc_ext_a[[ i_year - 2 ]] +
                df$IEA_pc_ext_a[[ i_year - 3 ]] ) /
            ( df$Fern_pc[[ i_year - 1 ]] + df$Fern_pc[[ i_year - 2 ]] +
                df$Fern_pc[[ i_year - 3 ]] )
        }
      }
      return( df )
    } )

# Prepare for 4b
    biomass_1a_IEA <- dplyr::arrange( biomass_1a_IEA, iso, year )
    biomass_1a_IEA$IEA_pc_ext_b <- biomass_1a_IEA$IEA_pc_ext_a  # will change in 4b
    biomass_1a_IEA$flag_b <- NA  # ==1 if changed in 4b


# 4b. To fix breaks: Going backwards in time, find the last (most recent) year where
#   IEA year-to-year ratio exceeds the specified threshold. Extend this year and all
#   all earlier years backwards using 3-year-average IEA/Fernandes ratio (using the
#   extrapolated IEA series from the previous step).
# Diagnostics: What countries have IEA yr-to-yr ratio exceeding threshold?
    diag_IEA_break <- group_by( biomass_1a_IEA, iso ) %>%
      dplyr::mutate( IEA_yr_to_yr = IEA_pc_ext_a / lead( IEA_pc_ext_a ),
              logical = ( IEA_yr_to_yr < MIN_IEA_YR_TO_YR |
                            IEA_yr_to_yr > MAX_IEA_YR_TO_YR ) ) %>%
      group_by( iso ) %>%
      filter( any( logical ) )

# Find year of the last (most recent) break
    last_IEA_break <- filter( diag_IEA_break, logical ) %>%
      group_by( iso ) %>%
      dplyr::mutate( last_yr = max( year ) ) %>%
      select( iso, last_yr ) %>% unique()

# Make a df of 3-year-average ratio of IEA/Fernandes
    ratio_3yr <- dplyr::arrange( diag_IEA_break, iso, year ) %>%
      group_by( iso ) %>%
      dplyr::mutate( IEA_lead = lead( IEA_pc_ext_a ), Fern_lead = lead( Fern_pc ),
              IEA_lead2 = lead( IEA_lead ), Fern_lead2 = lead( Fern_lead ),
              IEA_lead3 = lead( IEA_lead2 ), Fern_lead3 = lead( Fern_lead2 ) )
    ratio_3yr$IEA_over_Fern_3yr <- rowMeans( ratio_3yr[, c( "IEA_lead", "IEA_lead2",
                                                            "IEA_lead3" ) ], na.rm = T ) /
      rowMeans( ratio_3yr[, c( "Fern_lead", "Fern_lead2", "Fern_lead3" ) ], na.rm = T )
    ratio_3yr <- select( ratio_3yr, iso, year, IEA_over_Fern_3yr )

# For all years before and including year of the most recent break, extend back using
# IEA/Fernandes 3-year-average ratio
    IEA_break_ext <- merge( diag_IEA_break, ratio_3yr ) %>% merge( last_IEA_break )
    IEA_break_ext$flag_b[ IEA_break_ext$year <= IEA_break_ext$last_yr ] <- 1
    IEA_break_ext$IEA_pc_ext_b[ IEA_break_ext$year <= IEA_break_ext$last_yr ] <-
      IEA_break_ext$Fern_pc[ IEA_break_ext$year <= IEA_break_ext$last_yr ] *
      IEA_break_ext$IEA_over_Fern_3yr[ IEA_break_ext$year <= IEA_break_ext$last_yr ]
    IEA_break_ext <- select_( IEA_break_ext, .dots = names( biomass_1a_IEA ) )

# Update biomass_1_IEA with extended IEA
    biomass_1b_IEA <- bind_rows( filter( biomass_1a_IEA, iso %!in% IEA_break_ext$iso ),
                                IEA_break_ext )

# Prepare for 4c
    biomass_1b_IEA <- dplyr::arrange( biomass_1b_IEA, iso, year )
    biomass_1b_IEA$IEA_pc_ext_c <- biomass_1b_IEA$IEA_pc_ext_b  # will change in 4c
    biomass_1b_IEA$flag_c <- NA  # ==1 if changed in 4c

    # Make a logical column is_ext == T if IEA_pc has been extrapolated
    biomass_1b_IEA$is_ext <- is.na( biomass_1b_IEA$IEA_pc ) |
      biomass_1b_IEA$IEA_pc_ext_c != biomass_1b_IEA$IEA_pc


# 4c. To keep extrapolated IEA under control: If extrapolated IEA exceeds a maximum
#   limit (taken to be max of IEA and Fernandes 3-year average for 1969-1971 and for
#   3 years after the last (most recent) gap (missing or break, whichever is later),
#   replace extrapolated IEA with this maximum limit.
# Find max IEA and Fernandes for 1969-1971
    max_3yr <- select( biomass_1b_IEA, iso, year, IEA_pc, Fern_pc ) %>%
      filter( year %in% YEAR_SEQUENCE ) %>%
      group_by( iso ) %>%
      dplyr::summarise( IEA_pc = mean( IEA_pc, na.rm = T ),
                 Fern_pc = mean( Fern_pc, na.rm = T ) )
    max_3yr$max_3yr <- apply( max_3yr[, 2:3 ], 1, max, na.rm = T)

# Find max IEA and Fernandes for 3 years following (after) a gap (missing or break,
# whichever is later/more recent
    last_IEA_gap <- bind_rows( last_IEA_NA, last_IEA_break ) %>% group_by( iso ) %>%
      filter( last_yr == max( last_yr ) )  # last year with either break or missing IEA
    max_post_gap <- select( biomass_1b_IEA, iso, year, IEA_pc, Fern_pc ) %>%
      merge( last_IEA_gap ) %>%
      filter( year < last_yr + 4, year > last_yr  ) %>%  # keep 3 years following gap
      group_by( iso ) %>%
      dplyr::summarise( IEA_pc = mean( IEA_pc, na.rm = T ),
                 Fern_pc = mean( Fern_pc, na.rm = T ) )
    max_post_gap$max_post_gap <- apply( max_post_gap[, 2:3 ], 1, max, na.rm = T)

# Combine max_post_gap and max_3yr to find max_IEA_pc_ext
    max_IEA_pc_ext <- merge( select( max_3yr, iso, max_3yr ),
                             select( max_post_gap, iso, max_post_gap ), all = T )
    max_IEA_pc_ext$max_IEA_pc_ext <- apply( max_IEA_pc_ext[, c( "max_3yr",
                                                                "max_post_gap" ) ],
                                            1, max, na.rm = T )

# If extrapolated IEA_pc > max_IEA_pc_ext, replace with max_IEA_pc_ext
    biomass_1c_IEA <- merge( biomass_1b_IEA, max_IEA_pc_ext, all.x = T )
    biomass_1c_IEA$max_IEA_pc_ext[ is.na( biomass_1c_IEA$max_IEA_pc_ext ) ] <- Inf
    biomass_1c_IEA$flag_c[ biomass_1c_IEA$is_ext &
                             biomass_1c_IEA$IEA_pc_ext_b > biomass_1c_IEA$max_IEA_pc_ext ] <- 1
    biomass_1c_IEA$IEA_pc_ext_c[ biomass_1c_IEA$is_ext &
                                 biomass_1c_IEA$IEA_pc_ext_b > biomass_1c_IEA$max_IEA_pc_ext ] <-
      biomass_1c_IEA$max_IEA_pc_ext[ biomass_1c_IEA$is_ext &
                                   biomass_1c_IEA$IEA_pc_ext_b > biomass_1c_IEA$max_IEA_pc_ext ]


# 4d. Make final IEA dataset
    biomass_IEA_final <- select_( biomass_1c_IEA,
                                  .dots = c( names( biomass_1 ), "IEA_pc_ext_a", "flag_a",
                                             "IEA_pc_ext_b", "flag_b", "IEA_pc_ext_c", "flag_c",
                                             "is_ext" ) )
    biomass_IEA_final$IEA_pc_ext_final <- biomass_IEA_final$IEA_pc_ext_c

# Compute back total biomass and clean up
    biomass_IEA_final$IEA_ext_total <- biomass_IEA_final$IEA_pc_ext_final * biomass_IEA_final$pop2

# ------------------------------------------------------------------------------
# 5. Merge all sources to compile final 1700-IEA_end_year residential biomass series
    printLog( "Compile 1700-IEA_end_year residential biomass series" )

# Define values
    CONVERGENCE_YEAR <- 1920  # what year to converge to Fernandes?
    LAST_IEA_YEAR <- min( IEA_years )  # 1960
    LAST_FERN_YEAR <- min( Fern_biomass$year )  # 1850
    biomass_years <- seq( min( pop_master$year ), max( biomass_IEA_final$year ) )  # 1700-IEA_end_year

# Combine IEA and Fern to get 1960-IEA_end_year series
    added_Fern <- filter( biomass_1, iso %in% iso_Fern, year %in% emissions_years ) %>%
      select( iso, year, units, ceds = Fern )
    biomass_final <- bind_rows( select( biomass_IEA_final, iso, year, units,
                                        ceds = IEA_ext_total, is_ext ),
                                added_Fern )

# Add rows for all biomass_years and column of Fernandes biomass
    biomass_final_ext <- merge( data.frame( iso = unique( biomass_final$iso ) ),
                                data.frame( year = biomass_years ) ) %>%
      merge( select( biomass_final, iso, year, ceds, is_ext ), all.x = T ) %>%
      merge( Fern_biomass, all.x = T )
    biomass_final_ext$units <- "kt"

# Compute rural per-capita biomass (global, pse, ussr and yug won't have population -- that's ok)
    biomass_final_ext <- merge( biomass_final_ext, pop_master, all.x = T ) %>%
      dplyr::mutate( ceds_pc_orig = ceds / pop2, Fern_pc = Fern / pop2 )

# For 1850-1920, just copy Fernandes values to CEDS
    Fern_years <- biomass_final_ext$year <= CONVERGENCE_YEAR &
      biomass_final_ext$year >= LAST_FERN_YEAR
    biomass_final_ext$ceds_pc_ext <- biomass_final_ext$ceds_pc_orig
    biomass_final_ext$ceds_pc_ext[ Fern_years ] <- biomass_final_ext$Fern_pc[ Fern_years ]


# For 1921-IEA_end_year, IEA should be adjusted to converge to Fernandes by 1920. For each country,
# define start_yr to be the last year where extrapolated IEA falls below Fernandes (if
# applicable) and LAST_IEA_YEAR otherwise. Interpolate start_yr delta so that IEA rural
# per-capita matches Fernandes in 1920, and take
#     ceds[yr] = max( Fern[yr] + (ceds[start_yr] - Fern[start_yr]) * (yr - 1920)/(start_yr - 1920),
#                     min( ceds[start_yr], Fern[yr] )
# For 1700-1849, just carry 1850 Fernandes rural per-capita backward.
    # Find the last (most recent) year where extrapolated IEA drops below Fernandes
    last_IEA_below_Fern <- filter( biomass_IEA_final, is_ext ) %>%
      filter( IEA_pc_ext_final < Fern_pc ) %>%
      group_by( iso ) %>%
      dplyr::mutate( last_yr = max( year ) ) %>%
      select( iso, last_yr ) %>% unique()

    # Add column start_yr to biomass_final_ext
    biomass_final_ext$start_yr <- last_IEA_below_Fern$last_yr[
      match( biomass_final_ext$iso, last_IEA_below_Fern$iso ) ]
    biomass_final_ext$start_yr[ is.na( biomass_final_ext$start_yr ) |
                                  biomass_final_ext$start_yr < LAST_IEA_YEAR ] <- LAST_IEA_YEAR

    # Extend and adjust ceds_pc
    biomass_final_ext <- dplyr::arrange( biomass_final_ext, iso, year )
    biomass_final_ext$ceds_pc_ext_adj <- biomass_final_ext$ceds_pc_ext
    scaled <- filter( biomass_final_ext, year <= start_yr )
    scaled <- ddply( scaled, .(iso), function( df ) {
      interp_years <- df$year > CONVERGENCE_YEAR & df$year <= df$start_yr
      max_years <- interp_years & df$iso %in% last_IEA_below_Fern$iso  #LV
      within( df, {
        # extend ceds_pc by interpolating start_yr delta
        ceds_pc_ext[ interp_years ] <- Fern_pc[ interp_years ] +
          ( ceds_pc_ext[ year == start_yr ] - Fern_pc[ year == start_yr ] ) *
          ( year[ interp_years ] - CONVERGENCE_YEAR ) /
          ( start_yr[ year == start_yr ] - CONVERGENCE_YEAR )

        # adjust ceds_pc_ext by comparing with min( ceds[start_yr], Fern[yr] )
        ceds_pc_ext_adj[ interp_years ] <- ceds_pc_ext[ interp_years ]
        ceds_pc_ext_adj[ max_years ] <- pmax( ceds_pc_ext[ max_years ],
                                          pmin( Fern_pc[ max_years ],
                                          ceds_pc_ext[ max_years & year == start_yr ] ) )

        # carry 1850 values backward
        ceds_pc_ext <- na.locf( ceds_pc_ext, fromLast = T )
        ceds_pc_ext_adj <- na.locf( ceds_pc_ext_adj, fromLast = T )
      })
    })

    biomass_final_ext <- bind_rows( filter( biomass_final_ext, year > start_yr ), scaled ) %>%
      dplyr::arrange( iso, year )
    biomass_final_ext$is_adj <- biomass_final_ext$ceds_pc_ext != biomass_final_ext$ceds_pc_ext_adj

# Compute back total biomass
    biomass_final_ext <- dplyr::mutate( biomass_final_ext, ceds_tot_final = ceds_pc_ext_adj * pop2 )

# Add source note
    biomass_final_ext$src <- "Fernandes"

    # countries using IEA for 1960-IEA_end_year
    IEA_used <- biomass_final_ext$year %in% IEA_years & biomass_final_ext$iso %in% iso_IEA
    biomass_final_ext$src[ IEA_used ] <- "IEA"
    biomass_final_ext$src[ IEA_used & biomass_final_ext$is_ext ] <-
      "IEA - extrapolated/adjusted using Fernandes trend"

    # countries using delta-interpolated IEA for 1921-IEA_end_year
    IEA_delta_interp <- biomass_final_ext$year > CONVERGENCE_YEAR &
      biomass_final_ext$year <= biomass_final_ext$start_yr & biomass_final_ext$iso %in% iso_IEA
    biomass_final_ext$src[ IEA_delta_interp ] <-
      paste0( "IEA - interpolated ", biomass_final_ext$start_yr[ IEA_delta_interp ],
              " delta to match Fernandes per-capita by ", CONVERGENCE_YEAR )

    # countries using min-adjusted interpolated IEA for 1921-IEA_end_year
    biomass_final_ext$src[ IEA_delta_interp & biomass_final_ext$is_adj ] <-
      paste0( "IEA - taken to be min of IEA ",
              biomass_final_ext$start_yr[ IEA_delta_interp & biomass_final_ext$is_adj ],
              " and Fernandes ", biomass_final_ext$year[ IEA_delta_interp & biomass_final_ext$is_adj ] )

    # replace IEA with EIA for USA
    biomass_final_ext$src[ biomass_final_ext$iso == "usa" ] <-
      gsub( "IEA", "EIA", biomass_final_ext$src[ biomass_final_ext$iso == "usa" ] )

    # countries without data
    biomass_final_ext$src[ biomass_final_ext$iso %in% c( "global", "pse", "ussr", "yug" ) ] <- NA

# Clean up
    biomass_final_ext <- select( biomass_final_ext, iso, year, units, pop2,
                                 Fern_pc, ceds_pc_orig, ceds_pc_ext,
                                 ceds_pc_final = ceds_pc_ext_adj, ceds_tot_final, src )

# ------------------------------------------------------------------------------
# 6. Detect potential double counting in IEA unspecified biomass
# IEA residential and unspecified biomass might be correlated (i.e. one drops as the
# other rises). If so, unspecified biomass needs to be adjusted if residential biomass
# is increased.
#
# The input file IEA_biomass_double_counting.xlsx shows where residential and
# unspecified biomass are correlated. The decision column identifies countries/
# years where unspecified biomass needs to be adjusted. The .xlsx is created from
# A.IEA_biomass_double_counting.csv which is generated by the following diagnostic
# block. A warning will print if IEA_biomass_double_counting.xlsx needs to be updated.
#
# The actual adjustment amount is computed after the diagnostic block.

# Diagnostics: Where are residential and unspecified biomass correlated?
    printLog( "Detect potential biomass double counting")

    # Define threshold values
    MIN_IEA_YR_TO_YR_UNSPEC <- .8 # for unspecified biomas
    MAX_IEA_YR_TO_YR_UNSPEC <- 1.25

    # Slice out IEA countries that have both residential and unspecified biomass
    IEA_res_unspec <- filter( IEA_en, fuel == "biomass", iso %in% iso_IEA,
                              sector %in% c( "1A4b_Residential", "1A5_Other-unspecified" ) )
    IEA_res_unspec <- group_by( IEA_res_unspec, iso ) %>%
      filter( length( sector ) == 2 ) %>%
      select( -fuel, -units ) %>%
      data.frame()

    # Compute res and unspec year-to-year change (ratio and absolute difference)
    IEA_res_unspec <- melt( IEA_res_unspec, id = c( "iso", "sector" ) )
    IEA_res_unspec <- cast( IEA_res_unspec, iso + variable ~ sector )
    names( IEA_res_unspec ) <- c( "iso", "year", "res", "unspec" )
    IEA_res_unspec <- dplyr::arrange( IEA_res_unspec, iso, year ) %>%
      group_by( iso ) %>%
      dplyr::mutate( res_ratio = res / lag( res ),
              unspec_ratio = unspec / lag( unspec ),
              res_diff = res - lag( res ),
              unspec_diff = unspec - lag( unspec ),
              res_flag = res_ratio <= MIN_IEA_YR_TO_YR | res_ratio >= MAX_IEA_YR_TO_YR,
              unspec_flag = unspec_ratio <= MIN_IEA_YR_TO_YR_UNSPEC |
                unspec_ratio >= MAX_IEA_YR_TO_YR_UNSPEC )

    # Keep rows where res and unspec change abnormally in opposite direction
    IEA_res_unspec_flag <- filter( IEA_res_unspec, res_flag, unspec_flag,
                                   res_diff * unspec_diff < 0 )

    # Write out countries with correlated res and unspec
    # flag == T where res and unspec changes abnormally in opposite direction
    IEA_res_unspec_out <- select( IEA_res_unspec, -res_flag, -unspec_flag ) %>%
      filter( iso %in% IEA_res_unspec_flag$iso ) %>%
      dplyr::arrange( iso, year )
    IEA_res_unspec_out$flag <- NA
    IEA_res_unspec_out$flag[ paste0( IEA_res_unspec_out$iso, IEA_res_unspec_out$year ) %in%
                               paste0( IEA_res_unspec_flag$iso, IEA_res_unspec_flag$year ) ] <- 1
    IEA_res_unspec_out$year <- as.character( IEA_res_unspec_out$year )

    # Above result should be identical to what is read in from IEA_biomass_double_counting.xlsx
    # If a warning prints, please review/update IEA_biomass_double_counting.xlsx
    if ( !isTRUE( all.equal( IEA_res_unspec_out[ c( 1:4 ) ], IEA_correction[ c( 1:4 ) ] ) ) )
      warning( "Input values have changed. Please review diagnostic-output/A.IEA_biomass_adjustment.csv, update input/energy/IEA_biomass_double_counting.xlsx, then re-run the system." )

# Compute adjustment made to IEA residential biomass. This amount will be
# subtracted from unspecified biomass.
    printLog( "Compute adjustments to IEA unspecified biomass" )

    IEA_correction <- filter( IEA_correction, !is.na( decision ),
                              !grepl( "ignore", decision ) )
    IEA_adj <- select( biomass_final_ext, iso, units, year, ceds_tot_final ) %>%
      filter( year %in% IEA_years )
    IEA_adj$IEA <- biomass_IEA_final$IEA[ match( paste0( IEA_adj$iso, IEA_adj$year ),
                                                 paste0( biomass_IEA_final$iso, biomass_IEA_final$year ) ) ]
    IEA_adj$year <- paste0( "X", IEA_adj$year )

    # IEA for USA is actually EIA, so replace with IEA first
    IEA_usa <- filter( IEA_en, sector == "1A4b_Residential", fuel == "biomass", iso == "usa" ) %>%
      melt( measure = X_IEA_years )
    IEA_adj$IEA[ IEA_adj$iso == "usa" ] <- IEA_usa$value[ match(
      IEA_adj$year[ IEA_adj$iso == "usa" ], IEA_usa$variable ) ]

    # Compute adjustment
    IEA_adj$IEA[ is.na( IEA_adj$IEA ) ] <- 0
    IEA_adj <- dplyr::mutate( IEA_adj, adj = ceds_tot_final - IEA )  %>%
      filter( paste( iso, year ) %in%
                paste( IEA_correction$iso, IEA_correction$year ) )

    # Add all years and cast to wide format
    IEA_adj_wide <- merge( data.frame( year = X_IEA_years ),
                           data.frame( iso = unique( IEA_adj$iso ) ) ) %>%
      merge( IEA_adj, all = T ) %>%
      cast( iso ~ year, value = "adj" )
    IEA_adj_wide[ is.na( IEA_adj_wide ) ] <- 0

# ------------------------------------------------------------------------------
# 7. Make final biomass dataset
# First create rows of 1A4b_Residential biomass in IEA_en if not already exist
    iso_w_res <- filter( IEA_en, sector == "1A4b_Residential", fuel == "biomass" )
    iso_no_res <- setdiff( IEA_en$iso, iso_w_res$iso )
    IEA_en_adj <- bind_rows( IEA_en, data.frame(
      iso = iso_no_res, sector = "1A4b_Residential", fuel = "biomass", units = "kt" ) ) %>%
      dplyr::arrange( iso, sector, fuel )

# Replace residential biomass in CEDS with data in biomass_final_ext
    biomass_replaced <- select( biomass_final_ext, iso, year, units, ceds_tot_final ) %>%
      filter( year %in% IEA_years )
    biomass_replaced$year <- paste0( "X", biomass_replaced$year )
    biomass_replaced_wide <- cast( biomass_replaced, iso + units ~ year, value = "ceds_tot_final" )
    IEA_en_adj[ IEA_en_adj$sector == "1A4b_Residential" & IEA_en_adj$fuel == "biomass", X_IEA_years ] <-
      biomass_replaced_wide[ match(
        IEA_en_adj$iso[ IEA_en_adj$sector == "1A4b_Residential" & IEA_en_adj$fuel == "biomass" ],
        biomass_replaced_wide$iso ), X_IEA_years ]

# Adjust CEDS unspecified biomass
    # Subtract adjustment to residential biomass from unspecified biomass
    IEA_en_unspec <- filter( IEA_en_adj, sector == "1A5_Other-unspecified", fuel == "biomass",
                             iso %in% IEA_adj_wide$iso )
    IEA_en_unspec[ X_IEA_years ] <- IEA_en_unspec[ X_IEA_years ] - IEA_adj_wide[ X_IEA_years ]

    # If unspecified biomass < 0 after adjustment, bring up to 0
    IEA_en_unspec[ IEA_en_unspec < 0 ] <- 0

    # Bind back with other sectors/fuels
    IEA_en_adj <- bind_rows( filter( IEA_en_adj, ! (sector == "1A5_Other-unspecified" & fuel == "biomass" &
                                     iso %in% IEA_adj_wide$iso ) ), IEA_en_unspec ) %>%
      dplyr::arrange( iso, sector, fuel ) %>% data.frame( )

# Diagnostics: Compare residential/unspecified biomass before and after adjustment
    res_unspec <- filter( IEA_en, sector %in% c( "1A4b_Residential", "1A5_Other-unspecified" ),
                          fuel == "biomass", iso %in% IEA_adj_wide$iso ) %>%
      melt( measure = X_IEA_years ) %>%
      cast( iso + fuel + units + variable ~ sector )
    names( res_unspec ) <- c( "iso", "fuel", "units", "year", "1A4b_Residential", "1A5_Other-unspecified" )

    res_unspec_adj <- filter( IEA_en_adj, sector %in% c( "1A4b_Residential", "1A5_Other-unspecified" ),
                          fuel == "biomass", iso %in% IEA_adj_wide$iso ) %>%
      melt( measure = X_IEA_years ) %>%
      cast( iso + fuel + units + variable ~ sector )
    names( res_unspec_adj ) <- c( "iso", "fuel", "units", "year", "1A4b_Residential-ADJUSTED", "1A5_Other-unspecified-ADJUSTED" )

    res_unspec_comp <- merge( res_unspec, res_unspec_adj )

# ------------------------------------------------------------------------------
# 8. Write output
    biomass_final_ext[ is.na( biomass_final_ext ) ] <- ""
    biomass_IEA_final[ is.na( biomass_IEA_final ) ] <- ""
    IEA_res_unspec_out[ is.na( IEA_res_unspec_out ) ] <- ""

    writeData( IEA_en_adj, "MED_OUT", "A.en_biomass_fix" )
    writeData( biomass_final_ext, "MED_OUT", "A.residential_biomass_full" )

# Diagnostics
    writeData( biomass_IEA_final, "DIAG_OUT", "A.residential_biomass_IEA_ext" )
    writeData( IEA_res_unspec_out, "DIAG_OUT", "A.IEA_biomass_double_counting" )
    writeData( res_unspec_comp, "DIAG_OUT", "A.IEA_biomass_adjustment" )

logStop()
