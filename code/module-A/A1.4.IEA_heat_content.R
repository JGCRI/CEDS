# Program Name: A1.4.IEA_heat_content.R
# Author: Linh Vu, Rachel Hoesly
# Date Last Updated: September 5, 2019
# Program Purpose: Computes weighted average heat content from IEA Conversion Factors
#                  by country, year and fuel type. Currently doing this for coal.
# Input Files: OECD_Conversion_Factors_Full.csv, NonOECD_Conversion_Factors_Full.csv,
#              IEA_product_fuel.csv, Master_Country_List.csv, A.IEA_en_stat_ctry_hist
# Output Files:  A.coal_heat_content.csv
# Notes: This script handles iso+fuel+year duplicates by summing. This works
#        because all duplicates are currently disaggregated countries and
#        cover different years. The script will print warnings and produce a diagnostic
#        A.coal_heat_content_duplicates.csv when this is no longer the case. If
#        so, revise the handling process to avoid discontinuities.
# TODO:
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
  PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
  headers <- c( 'data_functions.R', 'common_data.R' )
  log_msg <- "Calculating coal heat content from IEA conversion factors"  # First message to be printed to the log
  script_name <- 'A1.4.IEA_heat_content.R'

  source( paste0( PARAM_DIR, "header.R" ) )
  initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 1. Input
  conversion_OECD <- readData( "ENERGY_IN", "OECD_Conversion_Factors_Full" )
  conversion_NonOECD <- readData( "ENERGY_IN", "NonOECD_Conversion_Factors_Full" )
  activity_data <- readData( "MED_OUT", "A.IEA_en_stat_ctry_hist" )

  IEA_product_fuel <- readData( "EN_MAPPINGS", "IEA_product_fuel" )
  MCL <- readData( "MAPPINGS", "Master_Country_List" )

# ---------------------------------------------------------------------------
# 2. Define useful variables and functions

  fuels <- c('hard_coal','coal_coke','brown_coal')

# define function to correct coal shares over IEA data
  fix_hard_coal_shares <- function ( df ) {

  # define years for which to correct
    extend_years <- paste0( 'X', 1960 : 1977 )
  # if statement - if need to correct
    if ( "Hard coal" %in% df$PRODUCT ) {
    if ( df[ which( df$PRODUCT == 'Hard coal' ),  'X1977' ] != 0 &
         df[ which( df$PRODUCT == 'Hard coal' ),  'X1978' ] == 0 ) {
    # seperate lines to leave, extend, and use in calucations
      out <- df[ which( df$X1978 == 0 & df$PRODUCT != 'Hard coal' ) , ]
      to_extend <- df[ which( df$X1978 != 0 & df$X1977 == 0 ) , ]
      hard_coal <- df[ which( df$PRODUCT == 'Hard coal' ) , ]
    # extend 1978 ratio of disaggregate fuels over hard coal value
      extended <- to_extend
      extended[ extend_years ] <- do.call( "rbind", replicate( nrow( extended ), hard_coal[ extend_years ], simplify = FALSE ) ) *
        do.call( "cbind", replicate( length( extend_years ) ,  to_extend[ 'X1978' ] , simplify = FALSE ) )
    # set hard coal to zero
      hard_coal[ extend_years ] <- 0
    # combine all rows
      corrected_all <- rbind( out, extended, hard_coal ) %>%
        dplyr::arrange( PRODUCT )
    #renormalize to one
      renormalized_shares <- calculate_shares( input_data = corrected_all,
                                               id_columns = c( 'iso', 'fuel' ),
                                               target_column = 'PRODUCT' )
      return( renormalized_shares )
    } } else ( return( df ) ) }

  fix_brown_coal_shares <- function ( df ) {
  # define years for which to correct
    extend_years <- paste0( 'X', 1960 : 1977 )
  # if statement - if need to correct
    if ( "Brown coal" %in% df$PRODUCT ) {
      if ( df[ which( df$PRODUCT == 'Brown coal' ),  'X1977' ] != 0 &
           df[ which( df$PRODUCT == 'Brown coal' ),  'X1978' ] == 0 ) {
      # seperate lines to leave, extend, and use in calucations
        out <- df[ which( df$X1978 == 0 & df$PRODUCT != 'Brown coal' ) , ]
        to_extend <- df[ which( df$X1978 != 0 & df$X1977 == 0 ) , ]
        brown_coal <- df[ which( df$PRODUCT == 'Brown coal' ) , ]
      # extend 1978 ratio of disaggregate fuels over hard coal value
        extended <- to_extend
        extended[ extend_years ] <- do.call( "rbind", replicate( nrow( extended ), brown_coal[ extend_years ], simplify = FALSE ) ) *
          do.call( "cbind", replicate( length( extend_years ) ,  to_extend[ 'X1978' ] , simplify = FALSE ) )
      # set hard coal to zero
        brown_coal[ extend_years ] <- 0
      # combine all rows
        corrected_all <- rbind( out, extended, brown_coal ) %>% dplyr::arrange( PRODUCT )
      # renormalize to one
        renormalized_shares <- calculate_shares( input_data = corrected_all,
                                                 id_columns = c( 'iso', 'fuel' ),
                                                 target_column = 'PRODUCT' )

        return( renormalized_shares )
      } } else ( return( df ) ) }

# ---------------------------------------------------------------------------
# 3. Calculate shares of fuel for each CEDS fuel (for weighted average)
# Combine into one df, convert to numeric

  # Keep data for only IEA years
  id_cols <- names( conversion_OECD )[1:3]
  conversion_all <- dplyr::bind_rows( conversion_OECD, conversion_NonOECD ) %>%
    dplyr::select( id_cols, X_IEA_years )

  conversion_all[ , X_IEA_years ] <- suppressWarnings( lapply( conversion_all[ , X_IEA_years ], function( x ) { as.numeric( as.character( x ) ) } ) )

# Conversion years
  X_IEA_years <-  dplyr::select( conversion_all, contains ('X') ) %>%
    names

# Clean up mapping
  IEA_product_fuel$product_conversion <- gsub( " \\(.*", "", IEA_product_fuel$product )

# Select relevant flows
  conversion_all <- filter( conversion_all, FLOW %in% c( "Average net calorific value" ) )
  conversion_all$fuel <- IEA_product_fuel$fuel[ match( conversion_all$PRODUCT, IEA_product_fuel$product_conversion ) ]
  conversion_all <- filter( conversion_all, fuel %in% fuels )

# Constantly extend heat value forward and back
  conversion_extended <- conversion_all
  conversion_extended[ X_IEA_years ] <- t( na.locf( t( conversion_extended[ X_IEA_years ] ) ) )
  conversion_extended[ X_IEA_years ] <- t( na.locf( t( conversion_extended[ X_IEA_years ] ) , fromLast = T ) )

# Calculate shares of energy data for countries by fuel type
  coal_data <- filter( activity_data, FLOW == 'DOMSUP' ) %>%
    left_join( IEA_product_fuel[ c( 'product', 'fuel' ) ] , by = c( 'PRODUCT' = 'product' ) ) %>%
    dplyr::group_by( iso, PRODUCT, fuel ) %>%
    dplyr::summarise_if( is.numeric, sum ) %>%
    filter( fuel %in% fuels ) %>%
    unite( "iso_fuel", c( iso, fuel ), sep = "-" )

  coal_shares <- calculate_shares( input_data = coal_data,
                                   id_columns= 'iso_fuel',
                                   target_column = 'PRODUCT',
                                   replace_with_zeros = T ) %>%
    separate( "iso_fuel", c( "iso", "fuel" ), sep = "-" )
  coal_shares$PRODUCT <- gsub( " \\(.*", "", coal_shares$PRODUCT )
  coal_shares[ paste0( 'X', BP_years ) ] <- coal_shares[ paste0( 'X', IEA_end_year ) ]

  coal_shares <- dplyr::arrange( coal_shares, iso, fuel, PRODUCT )

# Correct coal_shares.
# Stems from discontinuity in IEA data (reporting before 1978)  - many fuels switch to being reported as "hard coal"

# make a list of data frames, one for each country
  hard_coal_shares <- filter( coal_shares, fuel == 'hard_coal' )
  brown_coal_shares <- filter( coal_shares, fuel == 'brown_coal' )

  hard_shares_list <- lapply( X = unique( coal_shares$iso ) , FUN = function( x )  hard_coal_shares[ which( hard_coal_shares$iso == x ), ] )
  brown_shares_list <- lapply( X = unique( coal_shares$iso ) , FUN = function( x )  brown_coal_shares[ which( brown_coal_shares$iso == x ), ] )

  hard_coal_shares_list <- lapply( X = hard_shares_list, FUN = fix_hard_coal_shares )
  brown_coal_shares_list <- lapply( X = brown_shares_list, FUN = fix_brown_coal_shares )

# combine corrected hard_coal shares with other fuels
  coal_shares_corrected <- rbind( filter( coal_shares, fuel %!in% c( 'hard_coal', 'brown_coal' ) ),
                                  do.call( 'rbind', hard_coal_shares_list ),
                                  do.call( 'rbind', brown_coal_shares_list ) ) %>%
    dplyr::arrange( iso, fuel, PRODUCT )

# ---------------------------------------------------------------------------
# 4. Calculate weighted average heat content

# Map heat content to iso
  conversion_extended_iso <- conversion_extended
  conversion_extended_iso$iso <- MCL[ match( conversion_extended_iso$COUNTRY, MCL$IEAName ), 'iso' ]
  conversion_extended_iso <- conversion_extended_iso[ c( 'iso', 'fuel', 'PRODUCT', X_IEA_years ) ]
  conversion_extended_iso <- conversion_extended_iso %>%
    drop_na( iso, fuel, PRODUCT )

# Combine Conversion and energy shares into same df
  combined_df <- left_join( conversion_extended_iso, coal_shares_corrected, by = c( 'iso', 'fuel', 'PRODUCT' ) )
  combined_df[ X_IEA_years ] <- combined_df[ paste0( X_IEA_years , '.x' ) ] * combined_df[ paste0( X_IEA_years , '.y' ) ]

  weighted_average_heat_content <- combined_df[ c( 'iso', 'fuel', X_IEA_years ) ] %>%
    group_by( iso, fuel ) %>%
    summarise_all( funs( sum( ., na.rm = T ) ) )

  hc_coal <- weighted_average_heat_content %>%
    dplyr::mutate( units = 'kJ/kg' ) %>%
    replace( . == 0, NA ) %>%
    dplyr::distinct() # drop duplicates

# Drop rows of all NAs
  hc_coal_all <- hc_coal[ rowSums( is.na( hc_coal ) ) != length( X_IEA_years ), ]

# Remaining iso+fuel duplicates seem mostly composite/broken up countries that cover
# separate years, so okay to combine by summing these rows.
  hc_coal_all <- group_by( hc_coal_all, iso, fuel, units ) %>%
    summarise_all( funs( mean( ., na.rm = T ) ) ) %>%
    ungroup()

# Interpolate NAs, extend last non-NAs forward/backward
  hc_coal_all_ext <- data.frame( hc_coal_all )
  hc_coal_all_ext[ , X_IEA_years ] <- interpolate_NAs( hc_coal_all_ext[ , X_IEA_years ] )
  hc_coal_all_ext <- melt( hc_coal_all_ext, id = c( "iso", "fuel", "units" ) )
  hc_coal_all_ext <- ddply( hc_coal_all_ext, .( iso, fuel, units ), function( df ) {
    df$value <- na.locf( df$value, na.rm = F )
    df$value <- na.locf( df$value, na.rm = F, fromLast = T )
    return( df )
  } )
  hc_coal_all_ext <- cast( hc_coal_all_ext )

  # Constantly extend heat value forward
  X_extended_years <- X_BP_years
  hc_coal_all_ext_forward <- hc_coal_all_ext %>%
    dplyr::mutate_at( X_extended_years, funs( identity ( !!rlang::sym( X_IEA_end_year ) ) ) )


# ---------------------------------------------------------------------------
# 5. Output
  writeData( hc_coal_all_ext_forward, "MED_OUT", "A.coal_heat_content" )

  logStop()
