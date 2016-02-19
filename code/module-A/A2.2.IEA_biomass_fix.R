#------------------------------------------------------------------------------
# Program Name: A2.2.IEA_biomass_fix.R
# Author: Linh Vu
# Date Last Updated: 18 February 2016
# Program Purpose:  This program corrects system (IEA) residential biomass using 
#                   EIA, Fernandes, and Visschedijk (European) biomass data
# Input Files:     A.en_stat_sector_fuel.csv, A.Fernandes_residential_biomass, 
#                  Visschedijk_wooduse_Europe_TNO_4_Steve.xlsx, A.UN_pop_master.csv, 
#                  EIA_Table_10.2a_Renewable_Energy_Consumption___Residential_and_Commercial_Sectors.xlsx
# Output Files:    A.en_stat_sector_fuel.csv, A.residential_biomass_full.csv
# Notes:
# TODO: Fix any possible double counting due to adjustments in biomass consumption
#   
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Set working directory to the CEDS "input" directory and define PARAM_DIR as the
# location of the CEDS "parameters" directory, relative to the new working directory.
dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
for ( i in 1:length( dirs ) ) {
  setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
  wd <- grep( 'CEDS/input', list.dirs(), value = T )
  if ( length( wd ) > 0 ) {
    setwd( wd[ 1 ] )
    break
  }
}
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files - 
# provides logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R", "timeframe_functions.R" ) # Any additional function files required
log_msg <- "Combine system's IEA residential biomass with EIA, Fernandes and Visschedijk (European) data"
script_name <- "A2.2.IEA_biomass_fix.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )


# ------------------------------------------------------------------------------
# 1. Read raw input files and define useful values
    pop_master <- readData( "MED_OUT", "A.UN_pop_master", meta = F )

# Read energy data
    IEA_en <- readData( "MED_OUT", "A.en_stat_sector_fuel", meta = F )
    Fern_biomass <- readData( "MED_OUT", "A.Fernandes_residential_biomass", meta = F )
    Viss_biomass <- readData( "ENERGY_IN", "Visschedijk_wooduse_Europe_TNO_4_Steve", ".xlsx", 
                              sheet_selection = "Data", skip_rows = 2, meta = F )[ c( 1, 5 ) ]
    EIA_biomass <- readData( "ENERGY_IN", "EIA_Table_10.2a_Renewable_Energy_Consumption___Residential_and_Commercial_Sectors", 
                             ".xlsx", sheet_selection = "Annual Data", skip_rows = 10, meta = F )[ c( 1, 4 ) ]

# Define conversion factors
    kt_to_TJ <- 13.79960448  # 2005 for European countries and USA (TJ/kt)
    tBtu_to_TJ <- 0.94782 * 10^3 # source: eia.gov/cfapps/ipdbproject/docs/unitswithpetro.cfm

    
# ------------------------------------------------------------------------------
# 2. Combine IEA, EIA, Fernandes, and Viss biomass into one df
# Prepare population data
    pop_master <- filter( pop_master, scenario == "Estimates" ) %>%
      mutate( rural_pop = pop * ( 1 - urban_share ) )  # compute rural pop
    
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

# Aggregate IEA residential biomass by country
    IEA_biomass <- filter( IEA_en, sector == "1A4b_Residential", fuel == "biomass" ) %>%
      melt( measure = X_IEA_years )
    names( IEA_biomass )[ names( IEA_biomass ) %in% c( "variable", "value" ) ] <- c( "year", "IEA" )
    IEA_biomass$year <- xYearToNum( IEA_biomass$year )
    IEA_biomass <- group_by( IEA_biomass, iso, units, year ) %>%
      summarise( IEA = sum( IEA ) ) 

# Aggregate Fernandes biomass by country
    Fern_biomass <- group_by( Fern_biomass, iso, units, year ) %>%
      summarise( Fern = sum( consumption ) )

# Convert Viss biomass to standard format
    names( Viss_biomass ) <- c( "iso", "Viss" )
    Viss_biomass <- filter( Viss_biomass, !is.na( iso ) )
    Viss_biomass$iso[ Viss_biomass$iso == "yug" ] <- "scg"  # Serbia and Montenegro
    Viss_biomass$iso <- tolower( Viss_biomass$iso )
    Viss_biomass$Viss <- Viss_biomass$Viss / kt_to_TJ  # convert from TJ to kt
    Viss_biomass$units <- "kt"
    Viss_biomass$year <- 2005
    
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
      merge( Viss_biomass, all.x = T ) %>%
      merge( EIA_biomass, all.x = T ) %>%
      filter( year %in% emissions_years )

    # Change 0 biomass to NA
    biomass_0$IEA[ biomass_0$IEA == 0 ] <- NA
    biomass_0$Fern[ biomass_0$Fern == 0 ] <- NA
    biomass_0$EIA[ biomass_0$EIA == 0 ] <- NA
    biomass_0$Viss[ biomass_0$Viss == 0 ] <- NA


# ------------------------------------------------------------------------------
# 3. Decide which source to use for each country
# General rules:
# -- Use EIA data for USA.
# -- Where Viss is available and differs from IEA by more than 30% (note Viss is 
#    only available 2005), print out diagnostics to decide whether to use IEA or
#    Fernandes.
# -- For remaining countries, use Fernandes if IEA is smaller than Fernandes by more  
#    than 75%, for N = 5 years in a row centered at 2000.
# -- For remaining countries, use IEA.

# Define values for routine
    N <- 5
    N_CENTER_YEAR <- 2000
    MIN_IEA_OVER_FERN <- .25
    MIN_IEA_OVER_VISS <- .7
    MAX_IEA_OVER_VISS <- 10/7
    
# First decide what database to use for each country
    # Substitute EIA for IEA for USA
    biomass_1 <- biomass_0
    biomass_1$IEA[ biomass_1$iso == "usa" ] <- biomass_1$EIA[ biomass_1$iso == "usa" ]
    
    # Calculate IEA/Fern and IEA/Viss ratio
    biomass_1 <- mutate( biomass_1, IEA_over_Fern = IEA / Fern, 
                         IEA_over_Viss = IEA / Viss )
    
    # If IEA differs from Viss by over 30%, take notes to decide later whether to use
    # IEA or Fernandes
    iso_Viss <- filter( biomass_1, IEA_over_Viss < MIN_IEA_OVER_VISS | 
                          IEA_over_Viss > MAX_IEA_OVER_VISS )
    iso_Viss <- unique( iso_Viss$iso )
    iso_rest <- setdiff( unique( biomass_1$iso ), iso_Viss )

    # For remaining countries, use Fernandes if IEA is > 75% smaller than Fernandes
    # (evaluated for N years, centered at N_CENTER_YEAR)
      # Pick N years closest to N_CENTER_YEAR
    iso_Fern <- filter( biomass_1, iso %in% iso_rest ) %>%
      mutate( d = abs( year - N_CENTER_YEAR ) )  # distance from center year
    iso_Fern$d[ is.na( iso_Fern$IEA_over_Fern ) ] <- NA  # skip years with NA IEA/Fern ratio
    iso_Fern <- arrange( iso_Fern, d )
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
      mutate( IEA_pc = IEA / pop2, Fern_pc = Fern / pop2, Viss_pc = Viss / pop2 )
    
# Diagnostics: Decide whether to use IEA or Fernandes for countries in iso_Viss
    diag_Viss <- filter( biomass_1, iso %in% iso_Viss ) %>%
      select( iso, year, pop2, IEA, Viss, Fern, IEA_pc, Viss_pc, Fern_pc )
    # Resolution: Use Fern for aze, rus, svk, ukr; use IEA for geo, irl, mda, swe
    iso_Fern <- c( iso_Fern, "aze", "rus", "svk", "ukr" )
    iso_IEA <- c( iso_IEA, "geo", "irl", "mda", "swe" )
    rm( iso_Viss, iso_rest )

# Take note of which source is used for which country
    biomass_1$src <- NA
    biomass_1$src[ biomass_1$iso %in% iso_Fern ] <- "Fernandes"
    biomass_1$src[ biomass_1$iso %in% iso_IEA ] <- "IEA 2015"
    biomass_1$src[ biomass_1$iso == "usa" ] <- "EIA"
    
# Keep relevant columns
    biomass_1 <- select( biomass_1, iso, units, year, pop2, IEA, Fern, Viss, 
                         IEA_pc, Fern_pc, Viss_pc, src ) 
    
# ------------------------------------------------------------------------------
# 4. Correct gaps in IEA_pc
# General rules:
# -- Use rural per capita biomass for detecting/correcting gaps, and compute back
#   total biomass after done correcting.
# -- If IEA is missing or breaks, use Fernandes growth trend to go back in time.
# -- Try 2 IEA break criteria: 50% year to year; or 50% over 2 year (count 
#   discontinuity as in the first year). [TODO]
# -- Note there are two types of gaps: missing (NA) biomass, or breaks (year-to-year
#   ratio exceeding a specified threshold). Missing biomass could be for edge years
#   (most countries), or inner years (for che and deu).
# -- Note that Fernandes has been corrected for discontinuities, now has full series.
#    
# General procedures for correcting IEA_pc:
# -- To fix NA: Starting from most recent year and going backwards in time, if IEA
#   is missing, extrapolate using IEA/Fernandes ratio of the average of the 3 years
#   immediately following (after) the break. Cascade IEA if needed. Result is full 
#   IEA series.
# -- To fix breaks: Going backwards in time, find the last (most recent) year where 
#   IEA year-to-year ratio exceeds the specified threshold. Extend this year and all
#   all earlier years backwards using 3-year-average IEA/Fernandes ratio (using the 
#   extrapolated IEA series from the previous step).
# -- To keep extrapolated IEA under control: If extrapolated IEA exceeds a maximum 
#   limit (taken to be max of IEA and Fernandes 3-year average for 1969-1971 and for 
#   3 years after the last (most recent) gap (missing or break, whichever is later), 
#   replace extrapolated IEA with this maximum limit.
# -- To make IEA converge to Fernandes eventually: Go backwards in time and find the 
#   last (most recent) year where extrapolated IEA drops below Fernandes, replace 
#   this year and all earlier years with Fernandes.
    
# Define values used in routine
    MIN_IEA_YR_TO_YR <- .5
    MAX_IEA_YR_TO_YR <- 2
    YEAR_SEQUENCE <- seq( 1969, 1971 )  # for computing maximum limit in 4c
    
# Slice out countries that use IEA data
    biomass_1_IEA <- filter( biomass_1, iso %in% iso_IEA )
    
    # Prepare for 4a
    biomass_1_IEA$IEA_pc_ext_a <- biomass_1_IEA$IEA_pc  # will change in 3a
    biomass_1_IEA$flag_a <- NA  # ==1 if changed in 3a
    biomass_1_IEA <- arrange( biomass_1_IEA, iso, desc( year ) )

# 4a. To fix NA: Starting from most recent year and going backwards in time, if IEA
#   is missing, extrapolate using IEA/Fernandes ratio of the average of the 3 years
#   immediately following (after) the break. Cascade IEA if needed. Result is full 
#   IEA series.
    biomass_1a_IEA <- biomass_1_IEA
    
# Diagnostic: What countries have NA IEA? 
    diag_IEA_NA <- filter( biomass_1a_IEA, is.na( IEA_pc ) ) %>%
      group_by( iso ) %>%
      mutate( diff = year - lead( year ), first_yr = min( year ) )
    
    # What countries have irregular NA?
    # che: 1983-1992; deu: 1960-1969, 1986-1989; pol: 1990-1992
    # kwt: 1960-1970, 1997-2013
    diag_IEA_NA_irreg <- filter( diag_IEA_NA, first_yr > 1960 | diff != 1 )
    diag_IEA_NA_irreg <- filter( diag_IEA_NA, iso %in% diag_IEA_NA_irreg$iso )
    
# kwt has NA IEA for the rightmost edge years (1997-2013) -- extend average
# of the last 3 available years (1994-1996) forward
    kwt_avg <- filter( biomass_1a_IEA, iso == "kwt", year %in% seq( 1994, 1996 ) )
    kwt_avg <- mean( kwt_avg$IEA_pc )
    biomass_1a_IEA$flag_a[ biomass_1a_IEA$iso == "kwt" & biomass_1a_IEA$year >= 1997 ] <- 1
    biomass_1a_IEA$IEA_pc_ext_a[ biomass_1a_IEA$iso == "kwt" & biomass_1a_IEA$year >= 1997 ] <- kwt_avg

# Make a df of last (most recent) year with NA IEA
    last_IEA_NA <- filter( biomass_1a_IEA, is.na( IEA_pc_ext_a ) ) %>%
      group_by( iso ) %>%
      mutate( last_yr = max( year ) ) %>%
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
    biomass_1a_IEA <- arrange( biomass_1a_IEA, iso, year )
    biomass_1a_IEA$IEA_pc_ext_b <- biomass_1a_IEA$IEA_pc_ext_a  # will change in 3b
    biomass_1a_IEA$flag_b <- NA  # ==1 if changed in 3b
    
    
# 4b. To fix breaks: Going backwards in time, find the last (most recent) year where 
#   IEA year-to-year ratio exceeds the specified threshold. Extend this year and all
#   all earlier years backwards using 3-year-average IEA/Fernandes ratio (using the 
#   extrapolated IEA series from the previous step).
# Diagnostics: What countries have IEA yr-to-yr ratio exceeding threshold?
    diag_IEA_break <- group_by( biomass_1a_IEA, iso ) %>%
      mutate( IEA_yr_to_yr = IEA_pc_ext_a / lead( IEA_pc_ext_a ), 
              logical = ( IEA_yr_to_yr < MIN_IEA_YR_TO_YR | 
                            IEA_yr_to_yr > MAX_IEA_YR_TO_YR ) ) %>%
      group_by( iso ) %>%
      filter( any( logical ) )
    
# Find year of the last (most recent) break
    last_IEA_break <- filter( diag_IEA_break, logical ) %>%
      group_by( iso ) %>%
      mutate( last_yr = max( year ) ) %>%
      select( iso, last_yr ) %>% unique()
    
# Make a df of 3-year-average ratio of IEA/Fernandes
    ratio_3yr <- arrange( diag_IEA_break, iso, year ) %>%
      group_by( iso ) %>%
      mutate( IEA_lead = lead( IEA_pc_ext_a ), Fern_lead = lead( Fern_pc ), 
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
    biomass_1b_IEA <- arrange( biomass_1b_IEA, iso, year )
    biomass_1b_IEA$IEA_pc_ext_c <- biomass_1b_IEA$IEA_pc_ext_b  # will change in 3c
    biomass_1b_IEA$flag_c <- NA  # ==1 if changed in 3c
    
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
      summarise( IEA_pc = mean( IEA_pc, na.rm = T ), 
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
      summarise( IEA_pc = mean( IEA_pc, na.rm = T ), 
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

# Prepare for 4d
    biomass_1c_IEA <- select_( biomass_1c_IEA, .dots = names( biomass_1b_IEA ) )
    biomass_1c_IEA$IEA_pc_ext_d <- biomass_1c_IEA$IEA_pc_ext_c  # will change in 3d
    biomass_1c_IEA$flag_d <- NA  # ==1 if changed in 3d
    
    
# 4d. To make IEA converge to Fernandes eventually: Go backwards in time and find the 
#   last (most recent) year where extrapolated IEA drops below Fernandes, replace 
#   this year and all earlier years with Fernandes.
# For each country, find most recent year where extrapolated IEA_pc_ext < Fern_pc
    last_IEA_below_Fern <- filter( biomass_1c_IEA, is_ext ) %>%
      filter( IEA_pc_ext_c < Fern_pc ) %>% 
      group_by( iso ) %>%
      mutate( last_yr = max( year ) ) %>%
      select( iso, last_yr ) %>% unique()
    
# Replace all years before last_yr with Fernandes
    biomass_1d_IEA <- merge( biomass_1c_IEA, last_IEA_below_Fern, all.x = T )
    biomass_1d_IEA$flag_d[ !is.na( biomass_1d_IEA$last_yr ) & 
                             biomass_1d_IEA$year <= biomass_1d_IEA$last_yr ] <- 1
    biomass_1d_IEA$IEA_pc_ext_d[ !is.na( biomass_1d_IEA$last_yr ) & 
                                 biomass_1d_IEA$year <= biomass_1d_IEA$last_yr ] <- 
      biomass_1d_IEA$Fern_pc[ !is.na( biomass_1d_IEA$last_yr ) & 
                                biomass_1d_IEA$year <= biomass_1d_IEA$last_yr ] 

# Keep relevant columns and sort
    biomass_1d_IEA <- select_( biomass_1d_IEA, .dots = names( biomass_1c_IEA ) ) %>%
      arrange( iso, year )

# 4e. Make final IEA dataset
    biomass_IEA_final <- select_( biomass_1d_IEA, 
                              .dots = c( names( biomass_1 ), "IEA_pc_ext_a", "flag_a", 
                                         "IEA_pc_ext_b", "flag_b", "IEA_pc_ext_c", "flag_c", 
                                         "IEA_pc_ext_d", "flag_d", "is_ext" ) )
    biomass_IEA_final$IEA_pc_ext_final <- biomass_IEA_final$IEA_pc_ext_d
    biomass_IEA_final$is_ext[ biomass_IEA_final$is_ext ] <- 1
    biomass_IEA_final$is_ext[ biomass_IEA_final$is_ext == 0 ] <- NA
    
# Compute back total biomass
    biomass_IEA_final$IEA_ext_total <- biomass_IEA_final$IEA_pc_ext_final * biomass_IEA_final$pop2
    
# ------------------------------------------------------------------------------
# 5. Make final biomass dataset
# Combine IEA and Fern    
    added_Fern <- filter( biomass_1, iso %in% iso_Fern, year %in% emissions_years ) %>%
      select( iso, units, year, pop2, src, biomass_pc = Fern_pc, biomass_tot = Fern )
    biomass_final <- bind_rows( select( biomass_IEA_final, iso, units, year, 
                                        pop2, src, biomass_pc = IEA_pc_ext_final, 
                                        biomass_tot = IEA_ext_total ),
                                added_Fern )
    biomass_final$year <- paste0( "X", biomass_final$year )

# Replace residential biomass in IEA_en with data in biomass_final
    biomass_final_wide <- cast( biomass_final, iso + units ~ year, value = "biomass_tot" )
    IEA_en[ IEA_en$sector == "1A4b_Residential" & IEA_en$fuel == "biomass", X_IEA_years ] <- 
      biomass_final_wide[ match( 
        IEA_en$iso[ IEA_en$sector == "1A4b_Residential" & IEA_en$fuel == "biomass" ], 
        biomass_final_wide$iso ), X_IEA_years ] 
    
# ------------------------------------------------------------------------------
# 6. Write output
    biomass_IEA_final[ is.na( biomass_IEA_final ) ] <- ""
    biomass_final[ is.na( biomass_final ) ] <- ""
    
    writeData( IEA_en, "MED_OUT", "A.en_stat_sector_fuel" )
    writeData( biomass_final, "MED_OUT", "A.residential_biomass_full" )

# Diagnostics
    writeData( biomass_IEA_final, "DIAG_OUT", "A.residential_biomass_IEA_ext" )
    
logStop()
