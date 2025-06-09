# Program Name: A1.4.IEA_heat_content.R
# Author: Linh Vu, Rachel Hoesly, Patrick O'Rourke
# Date Last Updated: May 3, 2020
# Program Purpose: Computes weighted average heat content from IEA Conversion Factors
#                  by country, year and fuel type. Currently doing this for coal only.
# Input Files: OECD_and_NonOECD_Conversion_Factors_Full.csv,
#              A.IEA_en_stat_ctry_hist.csv, IEA_product_fuel.csv,
#              Master_Country_List.csv
# Output Files:  A.coal_heat_content.csv
# Notes: 1) This script handles iso + fuel + year duplicates by summing. This works
#        because all duplicates are isos which are also part of IEA composite regions.
#        The summing allows for a wider coverage of conversion factors (in terms of years),
#        as the composite regions have data for different years than the disaggregated isos.
#        2) Heating values are held constant with 1978 values for prior years where
#        data becomes less detailed going back in time.
# -----------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
  PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
  headers <- c( 'data_functions.R', 'common_data.R' )
  log_msg <- "Calculating coal heat content from IEA conversion factors..."  # First message to be printed to the log
  script_name <- 'A1.4.IEA_heat_content.R'

  source( paste0( PARAM_DIR, "header.R" ) )
  initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 1. Input
  conversion_OECD_and_NonOECD <- readData( "ENERGY_IN", "OECD_and_NonOECD_Conversion_Factors_Full" )
  activity_data <- readData( "MED_OUT", "A.IEA_en_stat_ctry_hist" )

  IEA_product_fuel <- readData( "EN_MAPPINGS", "IEA_product_fuel" )
  MCL <- readData( "MAPPINGS", "Master_Country_List" )

# ---------------------------------------------------------------------------
# 2. Define useful variables and functions

# Relevant CEDS fuels to calculate weighted average heating values for
  fuels <- c('hard_coal','coal_coke','brown_coal')

# Units for object -fuels-. These are stripped from the data and IEA_product_fuel.csv
# map in order to correctly merge with IEA World Conversion Factor data.
  IEA_units_to_remove <- c( "kt", "TJ-net", "TJ-gross" )

# Define function to correct hard coal shares over IEA data
# Note: This deals with the fact that going back in time data for certain IEA PRODUCTS
#       is no longer available beginning in 1977, while the aggregate "Hard coal (if no detail)"
#       as well as the "Brown coal (if no detail)" IEA PRODUCTS become available. Shares for 1978
#       are utilized back in time to correct this (this assumes similar fuel shares would exist from
#       1960-1977)
  fix_hard_coal_shares <- function ( df ) {

  # Define years for which to correct
    extend_years <- paste0( 'X', IEA_start_year : 1977 )

  # If statement - if need to correct
    if ( "Hard coal (if no detail)" %in% df$PRODUCT ) {
    if ( df[ which( df$PRODUCT == 'Hard coal (if no detail)' ),  'X1977' ] != 0 &
         df[ which( df$PRODUCT == 'Hard coal (if no detail)' ),  'X1978' ] == 0 ) {

    # Separate lines to leave, extend, and use in calucations
      out <- df[ which( df$X1978 == 0 & df$PRODUCT != 'Hard coal (if no detail)' ) , ]
      to_extend <- df[ which( df$X1978 != 0 & df$X1977 == 0 ) , ]
      # TODO: This seems to be dropping PRODUCTS which are not 0 in 1978 and not 0 in 1977 (for instance, BKB and gas coke in aus)
      #       See GIT issue #229
      hard_coal <- df[ which( df$PRODUCT == 'Hard coal (if no detail)' ) , ]

    # Extend 1978 ratio of disaggregate fuels over hard coal value
      extended <- to_extend
      extended[ extend_years ] <- do.call( "rbind", replicate( nrow( extended ), hard_coal[ extend_years ], simplify = FALSE ) ) *
        do.call( "cbind", replicate( length( extend_years ) ,  to_extend[ 'X1978' ] , simplify = FALSE ) )

    # Set hard coal to zero
      hard_coal[ extend_years ] <- 0

    # Combine all rows
    # TODO: This seems to be dropping PRODUCTS which are not 0 in 1978 and not 0 in 1977 (for instance, BKB and gas coke in aus)
    #       See GIT issue #229
      corrected_all <- dplyr::bind_rows( out, extended, hard_coal ) %>%
        dplyr::arrange( PRODUCT )

    # Renormalize to one
      renormalized_shares <- calculate_shares( input_data = corrected_all,
                                               id_columns = c( 'iso', 'fuel' ),
                                               target_column = 'PRODUCT' )
      return( renormalized_shares )
    } } else ( return( df ) ) }


# Define function to correct brown coal shares over IEA data
# Note: This deals with the fact that going back in time data for certain IEA PRODUCTS
#       is no longer available beginning in 1977, while the aggregate "Hard coal (if no detail)"
#       as well as the "Brown coal (if no detail)" IEA PRODUCTS become available. Shares for 1978
#       are utilized back in time to correct this (this assumes similar fuel shares would exist from
#       1960-1977)
# TODO: This function could be combined with fix_hard_coal_shares above (have a parameter
#       to account for whether brown or hard coal shares are being fixed)
  fix_brown_coal_shares <- function ( df ) {
  # TODO: This function may also drop certain PRODUCTS, as is occuring for hard_coal, since they are similar functions
  #       See GIT issue #229

  # Define years for which to correct
    extend_years <- paste0( 'X', IEA_start_year : 1977 )

  # If statement - if need to correct
    if ( "Brown coal (if no detail)" %in% df$PRODUCT ) {
      if ( df[ which( df$PRODUCT == 'Brown coal (if no detail)' ),  'X1977' ] != 0 &
           df[ which( df$PRODUCT == 'Brown coal (if no detail)' ),  'X1978' ] == 0 ) {

      # Separate lines to leave, extend, and use in calucations
        out <- df[ which( df$X1978 == 0 & df$PRODUCT != 'Brown coal (if no detail)' ) , ]
        to_extend <- df[ which( df$X1978 != 0 & df$X1977 == 0 ) , ]
        brown_coal <- df[ which( df$PRODUCT == 'Brown coal (if no detail)' ) , ]

      # Extend 1978 ratio of disaggregate fuels over hard coal value
        extended <- to_extend
        extended[ extend_years ] <- do.call( "rbind", replicate( nrow( extended ), brown_coal[ extend_years ], simplify = FALSE ) ) *
          do.call( "cbind", replicate( length( extend_years ) ,  to_extend[ 'X1978' ] , simplify = FALSE ) )

      # Set hard coal to zero
        brown_coal[ extend_years ] <- 0

      # Combine all rows
        corrected_all <- dplyr::bind_rows( out, extended, brown_coal ) %>%
            dplyr::arrange( PRODUCT )

      # Renormalize to one
        renormalized_shares <- calculate_shares( input_data = corrected_all,
                                                 id_columns = c( 'iso', 'fuel' ),
                                                 target_column = 'PRODUCT' )

        return( renormalized_shares )

      }

    } else ( return( df ) ) }

# ---------------------------------------------------------------------------
# 3. Calculate shares of fuel for each CEDS fuel (for weighted average)
# Combine into one df, convert to numeric

# Keep data for only IEA years, convert 0's to NA
  id_cols <- names( conversion_OECD_and_NonOECD )[ 1 : 3 ] # Assumes that COUNTRY, FLOW, PRODUCT are the first 3 columns
  conversion_all <- conversion_OECD_and_NonOECD %>%
    dplyr::select( all_of(id_cols), all_of(X_IEA_years) ) %>%
    dplyr::mutate_at( .vars = X_IEA_years, .funs = funs( if_else( . == 0, NA_real_, . ) ) )

  conversion_all[ , X_IEA_years ] <- suppressWarnings( lapply( conversion_all[ , X_IEA_years ], function( x ) { as.numeric( as.character( x ) ) } ) )

# Clean up mapping
  IEA_units_to_remove_reformatted <- paste0( " \\(", IEA_units_to_remove, "\\)" )  # Add a space and parenthesis in front of the units, and a parenthesis after the units
  IEA_units_to_remove_collapsed <- str_c( IEA_units_to_remove_reformatted, collapse = "|" ) # Collapse all of the unit patterns into 1 string for gsub, separated by "|", indicating 'or'
  IEA_product_fuel <- IEA_product_fuel %>%
      dplyr::mutate( product_conversion = gsub( IEA_units_to_remove_collapsed, "", product ) )

# Select relevant flows
  conversion_all <- dplyr::filter( conversion_all, FLOW %in% c( "Average net calorific value" ) )
  conversion_all$fuel <- IEA_product_fuel$fuel[ match( conversion_all$PRODUCT, IEA_product_fuel$product_conversion ) ]

  un_mapped_fuels <- conversion_all %>%
      dplyr::filter( is.na( fuel ) ) %>%
      dplyr::select( PRODUCT )

  if( unique( un_mapped_fuels$PRODUCT ) != "Crude/NGL/feedstocks/non-crude (if no detail)" ){

      unique_unmapped_conv_fac_prods <- unique( un_mapped_fuels$PRODUCT )
      unique_unmapped_conv_fac_prods <- subset( unique_unmapped_conv_fac_prods,
                                                unique_unmapped_conv_fac_prods != "Crude/NGL/feedstocks/non-crude (if no detail)" )

      printLog( unique_unmapped_conv_fac_prods )
      stop( "The above products in OECD_and_NonOECD_Conversion_Factors_Full.csv were not mapped to CEDS fuels. See ", script_name )

  }

 # The only PRODUCT not mapped above now is "Crude/NGL/feedstocks/non-crude (if no detail)", as IEA Energy Stats. and Conversion Factors have different
 # PRODUCTS (Energy Stats =  "Crude/NGL/feedstocks (if no detail)" vs. Conversion Factors = "Crude/NGL/feedstocks/non-crude (if no detail)".
 # This is not currently an issue, as liquid fuels are not currently dealt with here.
  conversion_all <- dplyr::filter( conversion_all, fuel %in% fuels )

# Constantly extend heat value forward and back
  conversion_extended <- conversion_all
  conversion_extended[ X_IEA_years ] <- t( na.locf( t( conversion_extended[ X_IEA_years ] ) ) )
  conversion_extended[ X_IEA_years ] <- t( na.locf( t( conversion_extended[ X_IEA_years ] ) , fromLast = T ) )

# Calculate shares of energy data for countries by fuel type
  coal_data <- dplyr::filter( activity_data, FLOW == 'DOMSUP' ) %>%
    dplyr::left_join( IEA_product_fuel[ c( 'product', 'fuel' ) ] , by = c( 'PRODUCT' = 'product' ) ) %>%
    dplyr::group_by( iso, PRODUCT, fuel ) %>%
    dplyr::summarise_if( is.numeric, sum ) %>%
    dplyr::filter( fuel %in% fuels ) %>%
    unite( "iso_fuel", c( iso, fuel ), sep = "-" )

  coal_shares <- calculate_shares( input_data = coal_data,
                                   id_columns= 'iso_fuel',
                                   target_column = 'PRODUCT',
                                   replace_with_zeros = T ) %>%
    separate( "iso_fuel", c( "iso", "fuel" ), sep = "-" )

  coal_shares <- coal_shares %>%
      dplyr::ungroup( ) %>%
      dplyr::mutate( PRODUCT = gsub( IEA_units_to_remove_collapsed, "", PRODUCT ) ) # Remove units and their parentheses from shares,
                                                                                    # so that matches with conversion factor PRODUCTS

# Extend coal shares to the last CEDS year, if necessary
  if( last( IEA_years ) < last( emissions_years ) ){

      IEA_end_year_to_CEDS_end_year <- paste0( "X", IEA_end_year : end_year )
      years_to_extend_to <- subset( IEA_end_year_to_CEDS_end_year,
                                    IEA_end_year_to_CEDS_end_year %!in% X_IEA_years )


      #gets numeric values for the years that must be appended to the dataframe
      start_year <- as.numeric(str_extract(X_IEA_end_year, "\\d+")) + 1
      end_year <- as.numeric(str_extract(last(years_to_extend_to), "\\d+"))

      ##Sets new columns based on the start and end year from IEA_end_year and year_columns respectively
      new_columns <- stats::setNames(
          #first argument is this replicate function, which returns identical copies of the X_IEA_end_year column
          #for an amount of instances denoted by end_year - start_year + 1
          replicate(end_year - start_year + 1, coal_shares[[X_IEA_end_year]], simplify = FALSE),
          #These are the new names for each column
          paste0("X", start_year:end_year)
      )

      coal_shares <- coal_shares %>%
          #!!!new_columns ensures that each column is added separately (it separates new_columns)
          tibble::add_column(!!!new_columns)

     # OLD CODE
     # coal_shares <- coal_shares %>%
     #     dplyr::mutate_at( years_to_extend_to, funs( identity ( !!rlang::sym( X_IEA_end_year ) ) ) )

  }

  coal_shares <- dplyr::arrange( coal_shares, iso, fuel, PRODUCT )

# Correct coal_shares.
# Stems from discontinuity in IEA data (reporting before 1978)  - many fuels switch to being reported as "hard coal"

# Make a list of data frames, one for each country
  hard_coal_shares <- dplyr::filter( coal_shares, fuel == 'hard_coal' )
  brown_coal_shares <- dplyr::filter( coal_shares, fuel == 'brown_coal' )

  hard_shares_list <- lapply( X = unique( coal_shares$iso ) , FUN = function( x )  hard_coal_shares[ which( hard_coal_shares$iso == x ), ] )
  brown_shares_list <- lapply( X = unique( coal_shares$iso ) , FUN = function( x )  brown_coal_shares[ which( brown_coal_shares$iso == x ), ] )

  hard_coal_shares_list <- lapply( X = hard_shares_list, FUN = fix_hard_coal_shares )
  brown_coal_shares_list <- lapply( X = brown_shares_list, FUN = fix_brown_coal_shares )

# Combine corrected hard_coal shares with other fuels
  coal_shares_corrected <- rbind( dplyr::filter( coal_shares, fuel %!in% c( 'hard_coal', 'brown_coal' ) ),
                                  do.call( 'rbind', hard_coal_shares_list ),
                                  do.call( 'rbind', brown_coal_shares_list ) ) %>%
    dplyr::arrange( iso, fuel, PRODUCT )

# ---------------------------------------------------------------------------
# 4. Calculate weighted average heat content

# Map heat content to CEDS isos
  MCL_clean <- MCL %>%
      dplyr::select( iso, IEAName ) %>%
      dplyr::filter( !is.na( IEAName ) ) %>%
      dplyr::distinct( ) %>%
      dplyr::rename( COUNTRY = IEAName )

  conversion_extended_iso <- conversion_extended %>%
      dplyr::left_join( MCL_clean, by = "COUNTRY" ) %>%
      dplyr::filter( !is.na( iso ) ) # Filter out regions which don't get mapped (such as the 'Memo' regions)

# Check for duplicated iso + FLOW + PRODUCT + fuel combinations - If they exist for the
# IEA composite Former Soviet Union and Former Yugoslavia, then use the FSU and FYU aggregate region
# heating values only if a given PRODUCT has an NA for the iso specific heating value
    conversion_extended_unique_combos <- conversion_extended_iso %>%
      dplyr::select( iso, FLOW, PRODUCT, fuel ) %>%
      dplyr::distinct( )

  if( nrow( conversion_extended_unique_combos ) < nrow( conversion_extended_iso ) ){

#     Check if the duplicates are for Former Soviet Union and Former Yugoslavia subregions only
      MCL_clean_FSU_FYug <- MCL_clean %>%
          dplyr::filter( COUNTRY %in% c( FSU_IEA_composite_name, FYUG_IEA_composite_name ) ) # As defined in common_data.R

      conversion_extended_iso_duplicates <- conversion_extended_iso %>%
          dplyr::select( iso, FLOW, PRODUCT )

      conversion_extended_iso_duplicates <- conversion_extended_iso_duplicates[ duplicated( conversion_extended_iso_duplicates[ , c( "iso", "FLOW", "PRODUCT" ) ] ),]

      conversion_extended_iso_duplicates <- unique( conversion_extended_iso_duplicates$iso )

      if( any( conversion_extended_iso_duplicates %!in% MCL_clean_FSU_FYug$iso ) ){

          stop( "There are duplicated iso + FLOW + PRODUCT combinations in the iso mapped IEA World Conversion Factor data ",
                "for isos not included in the composite IEA regions for the Former Soviet Union and Former Yugoslavia. ",
                "This should not occur. See ", script_name, "..." )

      }

#     For FSU and FYU duplicated isos, use the FSU and FYU heating values only if a given PRODUCT has an NA
#     for the iso
      conversion_extended_FSU_FYU_comp <- conversion_extended_iso %>%
        dplyr::filter( iso %in% conversion_extended_iso_duplicates ) %>%
        dplyr::filter( COUNTRY %in% c( FSU_IEA_composite_name, FYUG_IEA_composite_name ) ) %>%
        tidyr::gather( key = year, value = comp_heating_value, X_IEA_years ) %>%
        dplyr::rename( comp_region = COUNTRY )

      conversion_extended_FSU_FYU_fixed <- conversion_extended_iso %>%
        dplyr::filter( iso %in% conversion_extended_iso_duplicates ) %>%
        dplyr::filter( COUNTRY %!in% c( FSU_IEA_composite_name, FYUG_IEA_composite_name ) ) %>%
        tidyr::gather( key = year, value = iso_heating_value, X_IEA_years ) %>%
        dplyr::left_join( conversion_extended_FSU_FYU_comp,
                          by = c( "FLOW", "PRODUCT", "fuel", "iso", "year" ) ) %>%
        dplyr::mutate( fixed_heating_value = if_else( is.na( iso_heating_value ) & !is.na( comp_heating_value ),
                                                      comp_heating_value, iso_heating_value ) ) %>%
        dplyr::select( -iso_heating_value, -comp_region, -comp_heating_value ) %>%
        tidyr::spread( year, fixed_heating_value )

#     Add corrected FSU and FYU iso data back to the full data set of heating values
      conversion_extended_iso <- conversion_extended_iso %>%
        dplyr::filter( iso %!in% conversion_extended_iso_duplicates ) %>%
        dplyr::filter( COUNTRY %!in% c( FSU_IEA_composite_name, FYUG_IEA_composite_name ) ) %>%
        dplyr::bind_rows( conversion_extended_FSU_FYU_fixed )

  }

  conversion_extended_iso <- conversion_extended_iso[ c( 'iso', 'fuel', 'PRODUCT', X_IEA_years ) ]
  conversion_extended_iso <- conversion_extended_iso %>%
    drop_na( iso, fuel, PRODUCT )

# Combine Conversion and energy shares into same df
  combined_df <- dplyr::left_join( conversion_extended_iso, coal_shares_corrected, by = c( 'iso', 'fuel', 'PRODUCT' ) )
  combined_df[ X_IEA_years ] <- combined_df[ paste0( X_IEA_years , '.x' ) ] * combined_df[ paste0( X_IEA_years , '.y' ) ]

  weighted_average_heat_content <- combined_df %>%
    dplyr::select( 'iso', 'fuel', all_of(X_IEA_years) ) %>%
    dplyr::group_by( iso, fuel ) %>%
    dplyr::summarise_all( list( ~sum( ., na.rm = T ) ) )

  coal <- weighted_average_heat_content %>%
    dplyr::mutate( units = 'kJ/kg' ) %>%
    replace( . == 0, NA ) %>%
    dplyr::distinct( ) # Drop duplicates

# Drop rows of all NAs
  coal_all <- coal[ rowSums( is.na( coal ) ) != length( X_IEA_years ), ]

# Check for duplicated iso+fuel heating values - These should not exist at this point
  coal_all_ids <- coal_all %>%
     dplyr::select( iso, fuel, units ) %>%
     dplyr::distinct( )

  if( nrow( coal_all_ids ) != nrow( coal_all ) ){

     stop( "There are duplicated heating values being produced (by iso + fuel) in ", script_name,
          ". Duplicates should not exist, please check..." )

  }

# Interpolate NAs, extend last non-NAs forward/backward
  coal_all_ext <- data.frame( coal_all )
  coal_all_ext[ , X_IEA_years ] <- interpolate_NAs( coal_all_ext[ , X_IEA_years ] )

  coal_all_ext <- coal_all_ext %>%
      tidyr::gather( key = variable, value = value, X_IEA_years )

  coal_all_ext <- ddply( coal_all_ext, .( iso, fuel, units ), function( df ) {
    df$value <- na.locf( df$value, na.rm = F )
    df$value <- na.locf( df$value, na.rm = F, fromLast = T )
    return( df )
  } )

  coal_all_ext <- coal_all_ext %>%
      tidyr::spread( variable, value )

# Constantly extend heat value forward, if needed, through final CEDS year
  if( last( IEA_years ) < last( emissions_years ) ){

      #gets numeric values for the years that must be appended to the dataframe
      start_year <- as.numeric(str_extract(X_IEA_end_year, "\\d+")) + 1
      end_year <- as.numeric(str_extract(last(years_to_extend_to), "\\d+"))

      ##Sets new columns based on the start and end year from IEA_end_year and year_columns respectively
      new_columns <- stats::setNames(
          #first argument is this replicate function, which returns identical copies of the X_IEA_end_year column
          #for an amount of instances denoted by end_year - start_year + 1
          replicate(end_year - start_year + 1, coal_all_ext[[X_IEA_end_year]], simplify = FALSE),
          #These are the new names for each column
          paste0("X", start_year:end_year)
      )

      coal_all_ext_forward <- coal_all_ext %>%
          #!!!new_columns ensures that each column is added separately (it separates new_columns)
          tibble::add_column(!!!new_columns)

      #OLD CODE
      #coal_all_ext_forward <- coal_all_ext %>%
       # dplyr::mutate_at( years_to_extend_to, funs( identity ( !!rlang::sym( X_IEA_end_year ) ) ) )

  }else(
      coal_all_ext_forward <- coal_all_ext )

# ---------------------------------------------------------------------------
# 5. Output
  writeData( coal_all_ext_forward, "MED_OUT", "A.coal_heat_content" )

  logStop()
