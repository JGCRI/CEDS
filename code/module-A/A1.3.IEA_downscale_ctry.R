#------------------------------------------------------------------------------
# Program Name: A1.3.IEA_downscale_ctry.R
# Author's Name: Page Kyle, for GCAM; modified for use in CEDS project by
#                Steve Smith, Emily Voelker, Tyler Pitkanen, Jon Seibert, and
#                Linh Vu, Patrick O'Rourke
# Date Last Modified: May 3, 2020
# Program Purpose: Reads in the initial IEA energy data.
#				   Splits the composite data into individual countries.
# 				   Maps the aggregate coal consumption in earlier years to
#                  specific coal types.
# Input Files: A.UN_pop_master.csv, OECD_and_NonOECD_E_Stat.csv,
#              Master_Country_List.csv, E.CO2_CDIAC_inventory,
#              IEA_product_fuel.csv
# Output Files: A.IEA_en_stat_ctry_hist.csv, A.IEA_en_stat_adjusted.csv,
#               A.IEA_en_stat_adjust_diff.csv
# Notes: IEA_years and X_IEA_years are now defined in common_data.R, and currently
#        range from 1960-2017 (IEA_v2019)
# TODO: Add read-in of documentation data for this files
# TODO: Ultimately do a more sophisticated country split using population data
#       in a few places. (Unless its already doing this.)
#       (PR Comment From Patrick): Currently we are downscaling using CDIAC for the
#                                composite "aggregate" regions. FSU uses ratios.
# TODO:	Ultimately modify this to use more detailed data on coking coal
#       consumption instead of using a constant ratio for the split.
# TODO: Add additional energy data adjustment options and modularize adjustment
#       calculations.
# TODO: Handle small iso disaggregation (for instance, cuw from 2012 onwards
#       is for Curacao Island only. Prior to 2012 covers the entire territory
#       of the former Netherlands Antilles.)
#-------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "timeframe_functions.R",
                  "interpolation_extension_functions.R" )  # Additional function files required.
    log_msg <- "Initial processing of IEA energy statistics for all countries and historical years..."  # First message to be printed to the log
    script_name <- "A1.3.IEA_downscale_ctry.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 0.5. Define script constants and functions, load Packages

# Define composite IEA region names (these typically change between IEA versions,
# and are downscaled within this script). They are defined in common_data.R
    IEA_composite_regions <- c( FSU_IEA_composite_name, FYUG_IEA_composite_name,
                                Other_African_composite_name, Other_Americas_composite_name,
                                Other_Asia_composite_name )

# extendAdjValues(): Interpolate and extend IEA "energy-data-adjustment" values
# Params:  data frame containing base adjustment values. Should have the following format:
#               COUNTRY, FLOW, PRODUCT: as in IEA Energy Stat
#               method: multiplier, replace_value
#               ext_backward: percentage, none [default: none]
#               ext_forward: percentage, none [default: none].
#               Xyears: adjustment values for base years (ordered)
# Returns:  data frame containing adjustment values, interpolated between base years
#           and extended according to ext_backward and ext_forward flags.
# Note:     Method "replace_value" is currently configured to only allow for
#           ext_backward and ext_forward to be set to "none".
# TODO: add ext_forward = "absolute" and method = "replace"
    extendAdjValues <- function( df ) {
        id <- names( df )[ !grepl( "X", names( df ) ) ]
        valid_method <- c( "multiplier", "replace_value" )
        valid_ext <- c( "percentage", "none" )

    # Validate inputs
        if ( any( df$method %!in% valid_method ) ) {
            stop( "Invalid method -- must be multiplier or replace_value." )
        }
        if ( any( df$ext_backward %!in% valid_ext ) ) {
            warning( "Invalid ext_backward -- must be percentage or none. Default chosen: none. ")
            df$ext_backward[ df$ext_backward %!in% valid_ext ] <- "none"
        }
        if ( any( df$ext_forward %!in% valid_ext ) ) {
            warning( "Invalid ext_forward -- must be percentage or none. Default chosen: none." )
            df$ext_forward[ df$ext_forward %!in% valid_ext ] <- "none"
        }

    # Interpolate between Xyears
        df <- interpolateValues( df )

    # Get backward and forward extension range
        first_yr <- head( names( df )[ grepl( "X", names( df ) ) ], n = 1 ) %>% xYearToNum()
        bwd_range <- c()
        if ( first_yr > min( IEA_years ) )
            bwd_range <- paste0( "X", seq( min( IEA_years ), first_yr - 1 ) )
        last_yr <- tail( names( df )[ grepl( "X", names( df ) ) ], n = 1 ) %>% xYearToNum()
        fwd_range <- c()
        if ( last_yr < max( IEA_years ) )
            fwd_range <- paste0( "X", seq( last_yr + 1, max( IEA_years ) ) )

    # Add columns for all IEA years
        df[ , bwd_range ] <- NA
        df[ , fwd_range ] <- NA
        df <- df[ , c( id, X_IEA_years ) ]

    # Extend backward by method specified in ext_backward
        first_X_yr <- paste0( "X", first_yr )

        df <- df %>%
            dplyr::mutate_at( .vars = bwd_range, .funs = funs( as.numeric( .) ) ) %>%
            dplyr::mutate_at( .vars = bwd_range, .funs = funs(
                if_else( method == "replace_value", .,
                if_else( method == "multiplier" & ext_backward == "none", 1, . ) ) ) )

        if ( any( df$ext_backward == "percentage" ) )
            df[ df$ext_backward == "percentage", bwd_range ] <-
                df[ df$ext_backward == "percentage", first_X_yr ]

    # Extend forward by method specified in ext_foward
        last_X_yr <- paste0( "X", last_yr )

        df <- df %>%
            dplyr::mutate_at( .vars = fwd_range, .funs = funs( as.numeric( .) ) ) %>%
            dplyr::mutate_at( .vars = fwd_range, .funs = funs(
                if_else( method == "replace_value", .,
                if_else( method == "multiplier" & ext_forward == "none", 1, . ) ) ) )

        if ( any( df$ext_forward == "percentage" ) )
            df[ df$ext_forward == "percentage", fwd_range ] <-
                df[ df$ext_forward == "percentage", last_X_yr ]

        return( df )

    }

# ------------------------------------------------------------------------------
# 1. Load files
# "Invalid factor" warnings can be ignored- hence the warning suppressor code.
    w <- getOption( "warn" )
    options( warn = -1 )	# suppress the warning about columns names and appending

    un_pop <- readData( "MED_OUT", "A.UN_pop_master" )
    A.IEAfull <- readData( "ENERGY_IN", "OECD_and_NonOECD_E_Stat", ".csv" )
    MCL <- readData( "MAPPINGS", "Master_Country_List" )
    IEA_product_fuel <- readData( "EN_MAPPINGS", "IEA_product_fuel" )
    cdiac_total <- readData( "MED_OUT", "E.CO2_CDIAC_inventory" )

    adj_list <- list.files( path = "energy/energy-data-adjustment", pattern = "*.csv" )
    adj_list <- tools::file_path_sans_ext( adj_list )
    adj_list <- adj_list[ !grepl( "metadata", adj_list ) ]  # remove metadata
    adj_list <- lapply( adj_list, FUN = readData, domain = "ENERGY_IN", domain_extension = "energy-data-adjustment/" )

    options( warn = w )

# ------------------------------------------------------------------------------
# 2. Prep population, CDIAC data for country splitting

# Process UN Population Data
    un_pop$X_year <- paste0( "X", un_pop$year )
    un_pop$pop <- as.numeric( un_pop$pop )

    population <- un_pop %>%
        dplyr::filter( year %in% historical_pre_extension_year:end_year ) %>%
        dplyr::select( iso, X_year, pop ) %>%
        tidyr::spread( X_year, pop )

# CDIAC: extend last year of data out constantly.
#   This script does not use cdiac data for current trends, only for disaggregating IEA
#   'other' regions, so it's okay to constantly extend those emissions values
    cdiac_total[ , paste0( 'X', cdiac_end_year:IEA_end_year ) ] <- cdiac_total[ paste0( 'X', cdiac_end_year ) ]

# Add population trend for biomass. Cdiac does not include biomass data
    biomass_pop <- population
    biomass_pop$fuel <- 'biomass'
    biomass_pop <- biomass_pop[ c( 'iso', 'fuel', paste0( 'X', cdiac_start_year:IEA_end_year ) ) ]

    cdiac_merge <- dplyr::bind_rows( cdiac_total,
                               biomass_pop[ c( 'iso', 'fuel', paste0( 'X', cdiac_start_year:IEA_end_year ) ) ] )

# All but international bunkers, match by fuels
    cdiac_trend_fuel <- merge( IEA_product_fuel[ c( 'product', 'cdiac_fuel' ) ], cdiac_merge,
                               by.x = 'cdiac_fuel', by.y = 'fuel',
                               all = T )

    cdiac_trend_fuel <- cdiac_trend_fuel[ complete.cases( cdiac_trend_fuel[ , c( 'product', 'iso', 'cdiac_fuel' ) ] ), ]
    cdiac_trend_fuel <- cdiac_trend_fuel[ , c( "iso", "cdiac_fuel", "product", paste0( "X", start_year:IEA_end_year ) ) ]
    names(cdiac_trend_fuel) <- c( 'iso', 'cdiac_fuel' , 'PRODUCT', paste0( "X", start_year:IEA_end_year ) )

# International bunkers, match with FLOW
    bunker_flows <- c( 'MARBUNK', 'AVRBUNK' )
    cdiac_trend_MARbunkers <- cdiac_total[ which( cdiac_total$fuel == 'bunker_fuels' ), ]
    cdiac_trend_MARbunkers$FLOW <- 'MARBUNK'

    cdiac_trend_AVbunkers <- cdiac_total[ which( cdiac_total$fuel == 'bunker_fuels' ), ]
    cdiac_trend_AVbunkers$FLOW <- 'AVRBUNK'

    cdiac_trend_bunkers <- dplyr::bind_rows( cdiac_trend_MARbunkers, cdiac_trend_AVbunkers )
    cdiac_trend_bunkers <- cdiac_trend_bunkers[ , c( 'iso', 'fuel', 'FLOW', paste0( "X", start_year:IEA_end_year ) ) ]

# ------------------------------------------------------------------------------
# 3. Preparatory IEA data Calculations
#    This section adjusts and separates data as necessary to handle issues
#    like aggregate countries and capitalization inconsistencies. Uses
#    energy-data-adjustment instructions to make additional corrections.

# Subset only the relevant years (and convert NAs to 0, if NAs exist)
  	IEA_IDcodes  <- c( "COUNTRY", "FLOW", "PRODUCT" )
  	A.IEAfull <- A.IEAfull %>%
  	    dplyr::select( all_of(IEA_IDcodes), all_of(X_IEA_years) ) %>%
  	    dplyr::mutate_at( .vars = X_IEA_years,
  	                      .funs = funs( if_else( is.na( . ), 0, . ) ) )

# Adjust IEA energy stat according to instruction files in energy-data-adjustment folder
  	if ( length( adj_list ) > 0 ) {
      	printLog( "Adjusting IEA Energy Statistics..." )

  	# Extend adjustment values to all IEA years
    	adj_en_ext <- lapply( adj_list, FUN = extendAdjValues )
    	adj_en_ext <- do.call( dplyr::bind_rows, adj_en_ext )

    # Separate multiplier vs. replace_value methods
    	adj_en_ext_multiplier <- adj_en_ext %>%
    	    dplyr::filter( method == "multiplier" )

    	adj_en_ext_replace_value <- adj_en_ext %>%
    	    dplyr::filter( method == "replace_value" )

  	# Keep only ID and Xyear columns
    	adj_en_ext_multiplier <- adj_en_ext_multiplier[ , c( IEA_IDcodes, grep( "X", names( adj_en_ext_multiplier ), value = T ) ) ]
    	adj_en_ext_replace_value <- adj_en_ext_replace_value[ , c( IEA_IDcodes, grep( "X", names( adj_en_ext_replace_value ), value = T ) ) ]

    # Process multiplier method adjustments
    if( nrow( adj_en_ext_multiplier ) > 0 ){
      	# Adjustment files may repeat COUNTRY+FLOW+PRODUCT, so multiply adjustment
        # values by COUNTRY+FLOW+PRODUCT and year (note that method=multiplier)
    	adj_en_ext_multiplier <- dplyr::group_by( adj_en_ext_multiplier, COUNTRY, FLOW, PRODUCT ) %>%
      	        dplyr::summarise_all( prod )

      	# Make adjustment to relevant COUNTRY+FLOW+PRODUCT
      	    IEA_to_adjust <- dplyr::filter( A.IEAfull, paste0( COUNTRY, FLOW, PRODUCT ) %in%
      	                             paste0( adj_en_ext_multiplier$COUNTRY, adj_en_ext_multiplier$FLOW, adj_en_ext_multiplier$PRODUCT ) ) %>%
      	      dplyr::arrange( COUNTRY, FLOW, PRODUCT )
      	    adj_en_ext_multiplier <- dplyr::filter( adj_en_ext_multiplier, paste0( COUNTRY, FLOW, PRODUCT ) %in%
      	                          paste0( IEA_to_adjust$COUNTRY, IEA_to_adjust$FLOW, IEA_to_adjust$PRODUCT ) ) %>%
      	      dplyr::arrange( COUNTRY, FLOW, PRODUCT )

      	    IEA_adjusted <- IEA_to_adjust

      	    IEA_adjusted[ , X_IEA_years ] <- IEA_adjusted[ , X_IEA_years ] * adj_en_ext_multiplier[ , X_IEA_years ]
      	    A.IEAfull <- dplyr::filter( A.IEAfull, paste0( COUNTRY, FLOW, PRODUCT ) %!in%
      	                          paste0( adj_en_ext_multiplier$COUNTRY, adj_en_ext_multiplier$FLOW, adj_en_ext_multiplier$PRODUCT ) ) %>%
      	      dplyr::bind_rows( IEA_adjusted ) %>%
      	      dplyr::arrange( COUNTRY, FLOW, PRODUCT )

      	    IEA_adjust_diff <- IEA_adjusted

    } else {

        IEA_to_adjust <- A.IEAfull %>% dplyr::slice( 0 ) # Create now incase method multiplier is not used, but method replace_value is
        IEA_adjusted <- A.IEAfull %>% dplyr::slice( 0 ) # Create now incase method multiplier is not used, but method replace_value is
        IEA_adjust_diff <- A.IEAfull %>% dplyr::slice( 0 ) # Create now incase method multiplier is not used, but method replace_value is

    }

    # Process replace_value method adjustments
    # TODO Replace the loop with an apply statement
    if( nrow( adj_en_ext_replace_value ) > 0 ){

        for( row in 1: nrow( adj_en_ext_replace_value ) ){

      	    all_columns <- colnames( adj_en_ext_replace_value )

      	    replace_adjustment <- adj_en_ext_replace_value %>%
      	        dplyr::slice( row ) %>%
      	        dplyr::select_if( function( x ){ !all( is.na( x ) ) } )

      	    replace_adjustment_years <- grep( "X", names( replace_adjustment ), value = T )


      	    IEA_to_adjust_replace <- dplyr::filter( A.IEAfull, paste0( COUNTRY, FLOW, PRODUCT ) %in%
      	                                            paste0( replace_adjustment$COUNTRY, replace_adjustment$FLOW, replace_adjustment$PRODUCT ) )

      	    IEA_to_adjust <- IEA_to_adjust %>%
      	        dplyr::bind_rows( IEA_to_adjust_replace ) %>%
      	        dplyr::arrange( COUNTRY, FLOW, PRODUCT )

      	    IEA_adjusted_replace <- IEA_to_adjust_replace %>%
      	        dplyr::select( -all_of(replace_adjustment_years) ) %>%
      	        dplyr::left_join( replace_adjustment, by = c( "COUNTRY", "FLOW", "PRODUCT" ) ) %>%
      	        dplyr::select( all_of(all_columns) )

      	    IEA_adjusted <- IEA_adjusted %>%
      	        dplyr::bind_rows( IEA_adjusted_replace ) %>%
      	        dplyr::arrange( COUNTRY, FLOW, PRODUCT )

      	    IEA_adjust_diff <- IEA_adjust_diff %>%
      	        dplyr::bind_rows( IEA_adjusted_replace ) %>%
      	        dplyr::arrange( COUNTRY, FLOW, PRODUCT )

      	    A.IEAfull <- A.IEAfull %>%
      	        dplyr::filter( paste0( COUNTRY, FLOW, PRODUCT ) %!in%
      	                       paste0( IEA_adjusted_replace$COUNTRY, IEA_adjusted_replace$FLOW, IEA_adjusted_replace$PRODUCT ) ) %>%
      	        dplyr::bind_rows( IEA_adjusted_replace ) %>%
      	        dplyr::arrange( COUNTRY, FLOW, PRODUCT )

        }

    }

  	# Diagnostics: What is the amount changed?
  	    IEA_adjust_diff[ , X_IEA_years ] <- IEA_adjust_diff[ , X_IEA_years ] -
  	                                       IEA_to_adjust[ , X_IEA_years ]

  	 }

# Deal with composite regions (e.g. Other Africa) and historical countries
#     (Former Soviet Union and Former Yugoslavia) that get broken

# Split the country mapping table into composite regions and single-countries
	  IEA_composite <- subset( MCL, IEAName %in% IEA_composite_regions )

	  IEA_single <- subset( MCL, IEAName %!in% IEA_composite$IEAName )

# Split IEA energy statistics into table of single countries and composite
#   regions (keeping only desired composite regions)
	  A.IEAcomp <- subset( A.IEAfull, COUNTRY %in% IEA_composite$IEAName )
	  A.IEAsingle <- subset( A.IEAfull, COUNTRY %in% IEA_single$IEAName )
	  A.IEAsingle$iso <- IEA_single$iso[ match( A.IEAsingle$COUNTRY,
	                                            IEA_single$IEAName ) ]

# ------------------------------------------------------------------------------
# 4. First process FSU and former Yugoslavia

# Subset countries that are being downscaled in certain years using historical
#   energy data in a specified year
# Former Soviet Union and Yugoslavia: use specified flows for each product

# The downscaling, except for FSU exception below, is as follows:
# E(country, year, sector, fuel) =
#        (E(country, 1990, sector, fuel) / E(FSU, 1990, sector, fuel) ) *
#        E(FSU, year, sector, fuel)
#
# This method depends on the reporting categories being consistent between the FSU and
# the reporting after 1990 for the constituent countries. This is not the case for FSU
# energy transformation sectors, so those are transformed as a block, as noted below
#
# The IEA data has many inter-sectoral inconsistencies between the USSR and
#   separated countries thereafter. Results in unrealistic fuel shares.

  	USSR_Yug_years   <- IEA_years[ IEA_years < 1990 ]
  	X_USSR_Yug_years <- paste( "X", USSR_Yug_years, sep = "" )
  	postUSSR_Yug_years   <- IEA_years[ IEA_years >= 1990 ]
  	X_postUSSR_Yug_years <- paste( "X", postUSSR_Yug_years, sep = "" )
  	A.USSR_Yug  <- subset( A.IEAcomp, COUNTRY %in% c(
              FSU_IEA_composite_name,
              FYUG_IEA_composite_name ) )

# Re-map the forms of coal from the historical years--called "if no detail"
#   (this is before 1978)--to the relevant coal types for matching with the
#   more recent years. Extend back to IEA_start_year (1960).
    X_no_detail_coal_years <- paste( "X", IEA_start_year : 1977, sep = "" )

# Hard coal needs to split proportionally between coking coal and other
#   bituminous coal to minimize bias from different country-wise shares of the
#   two fuel types. Note that anthracite is not considered in these regions.

# This split cannot be used as an indicator of coking coal consumption. Coal
#   coke consumption is available directly.

# NOTE: using round() to avoid NA's for any values whose base value is >1e6

    split_factor <- round( A.USSR_Yug$X1978[ A.USSR_Yug$PRODUCT ==
            "Other bituminous coal (kt)" ] /
        ( A.USSR_Yug$X1978[ A.USSR_Yug$PRODUCT == "Coking coal (kt)" ] +
          A.USSR_Yug$X1978[ A.USSR_Yug$PRODUCT ==
            "Other bituminous coal (kt)" ] + 1e-3 ), 2 )

    A.USSR_Yug[ A.USSR_Yug$PRODUCT == "Other bituminous coal (kt)",
        X_no_detail_coal_years ] <- A.USSR_Yug[ A.USSR_Yug$PRODUCT ==
        "Hard coal (if no detail) (kt)", X_no_detail_coal_years ] * split_factor

  	A.USSR_Yug[ A.USSR_Yug$PRODUCT == "Coking coal (kt)",
          X_no_detail_coal_years ] <- A.USSR_Yug[ A.USSR_Yug$PRODUCT ==
          "Hard coal (if no detail) (kt)", X_no_detail_coal_years ] -
          A.USSR_Yug[ A.USSR_Yug$PRODUCT == "Other bituminous coal (kt)",
              X_no_detail_coal_years ]

  	A.USSR_Yug[ A.USSR_Yug$PRODUCT == "Hard coal (if no detail) (kt)",
          X_no_detail_coal_years ] <- 0

# Brown coal is simpler, as lignite is the only relevant fuel (sub-bituminous
#   coal is not considered in these regions)
  	A.USSR_Yug[ A.USSR_Yug$PRODUCT == "Lignite (kt)", X_no_detail_coal_years ] <-
  	     A.USSR_Yug[ A.USSR_Yug$PRODUCT == "Brown coal (if no detail) (kt)",
  	                 X_no_detail_coal_years ]

  	A.USSR_Yug[ A.USSR_Yug$PRODUCT == "Brown coal (if no detail) (kt)",
                X_no_detail_coal_years ] <- 0

  	A.USSR_Yug_ctry <- subset( A.IEAsingle, iso %in%
  	                             IEA_composite$iso[ IEA_composite$IEAName %in%
  	                                                  c( FSU_IEA_composite_name,
  	                                                     FYUG_IEA_composite_name ) ] )
  	A.USSR_Yug_ctry$IEAcomp <- IEA_composite$IEAName[
  	                             match( A.USSR_Yug_ctry$iso, IEA_composite$iso ) ]


# Use data from 1990 to estimate what fractions of overall FSU or Yug data
#   belong to their sub-countries

# Data for Non-specified primary biofuels/waste disappears in 1990 when biofuels
#   start to be categorized in greater detail. Data during FSU and Yug years
#   most likely comes from Primary solid biofuels, so use the data for that
#   product to determine 1990 shares

# NOTE: Some combinations of products and flows go uncounted starting in 1990
#   and their shares cannot be calculated. Note this in the meta-data
    meta_names <- c( "Data.Type", "Emission", "Region", "Sector", "Start.Year",
                               "End.Year", "Source.Comment" )

    meta_note <- c( "Energy Consumption", "NA", "Former Soviet Union", "Consumption",
                    1971, "1990", paste0( "Natural gas liquids (kt) in final consumption go uncounted ",
                    "before 1990 since shares cannot be calculated from 1990 values" ) )

    source_info <- script_name

    addMetaData( meta_note, meta_names, source_info )

    meta_note <- c( "Energy Consumption", "NA", "Former Yugoslavia", "Consumption",
                    1971, "1990", paste0( "Natural gas liquids (kt) in final consumption go uncounted ",
                    "before 1990 since shares cannot be calculated from 1990 values" ) )


    addMetaData( meta_note, meta_names, source_info )

# Take the combination of PRODUCT and FLOW columns, and keep only the rows that
#   contain the combination of PRODUCT and FLOW in IEA_product_downscaling
    A.USSR_Yug_FLOW_PROD <- A.USSR_Yug_ctry %>%
        dplyr::select( IEAcomp, FLOW, PRODUCT, X1990 ) %>%
        dplyr::group_by( IEAcomp, FLOW, PRODUCT ) %>%
        dplyr::summarise_all( list( ~sum( ., na.rm = T ) ) ) %>%
        dplyr::ungroup( ) %>%
        dplyr::arrange( PRODUCT, FLOW, IEAcomp ) %>%
        as.data.frame( )

    A.USSR_Yug_FLOW_PROD <- A.USSR_Yug_FLOW_PROD[
                                A.USSR_Yug_FLOW_PROD$X1990 != 0 |
                                A.USSR_Yug_FLOW_PROD$PRODUCT == "Non-specified primary biofuels/waste (TJ-net)", ]

    A.USSR_Yug_ctry_FLOW_PROD <- subset( A.USSR_Yug_ctry,
                                         paste( PRODUCT, FLOW ) %in%
                                         paste( A.USSR_Yug_FLOW_PROD$PRODUCT,
                                                A.USSR_Yug_FLOW_PROD$FLOW ) )

# Calculate the share of each country within the larger USSR aggregate in 1990
    A.USSR_Yug_ctry_FLOW_PROD$X1990_share <-
        A.USSR_Yug_ctry_FLOW_PROD$X1990 / A.USSR_Yug_FLOW_PROD$X1990[
            match( paste( A.USSR_Yug_ctry_FLOW_PROD$IEAcomp,
                    A.USSR_Yug_ctry_FLOW_PROD$FLOW,
                    A.USSR_Yug_ctry_FLOW_PROD$PRODUCT ),
			      paste( A.USSR_Yug_FLOW_PROD$IEAcomp,
                    A.USSR_Yug_FLOW_PROD$FLOW,
                    A.USSR_Yug_FLOW_PROD$PRODUCT ) ) ]

# Copy shares for Primary solid biofuels to Non-specified primary biofuels/waste
    spec_bio_shares <- subset( A.USSR_Yug_ctry_FLOW_PROD,
        ( PRODUCT == "Primary solid biofuels (TJ-net)" ) &
        ( FLOW %in% c( "DOMSUP", "FINCONS" ) ), c( iso, FLOW, PRODUCT, X1990_share ) )
    A.USSR_Yug_ctry_FLOW_PROD$X1990_share[
        A.USSR_Yug_ctry_FLOW_PROD$PRODUCT ==
        "Non-specified primary biofuels/waste (TJ-net)" &
            ( A.USSR_Yug_ctry_FLOW_PROD$FLOW %in%
            c( "DOMSUP", "FINCONS" ) ) ] <- spec_bio_shares$X1990_share

# Replace NAs with 0.
  	A.USSR_Yug_ctry_FLOW_PROD[ is.na( A.USSR_Yug_ctry_FLOW_PROD ) ] <- 0

# Calculate the fuel use of individual countries during USSR years = total use
#   of composite region multiplied by the country-wise shares in 1990
	  A.USSR_Yug_ctry_stat <- A.USSR_Yug_ctry[ c( "iso", "FLOW", "PRODUCT",
                                                "IEAcomp" ) ]

  	A.USSR_Yug_ctry_stat[ X_USSR_Yug_years ] <- A.USSR_Yug[ match(
              paste( A.USSR_Yug_ctry_stat$IEAcomp,
                     A.USSR_Yug_ctry_stat$FLOW,
                     A.USSR_Yug_ctry_stat$PRODUCT ),
              paste( A.USSR_Yug$COUNTRY,
                     A.USSR_Yug$FLOW,
                     A.USSR_Yug$PRODUCT ) ),
              X_USSR_Yug_years ] *
          A.USSR_Yug_ctry_FLOW_PROD$X1990_share[ match(
              paste( A.USSR_Yug_ctry_stat$iso,
                     A.USSR_Yug_ctry_stat$FLOW,
                     A.USSR_Yug_ctry_stat$PRODUCT ),
        			paste( A.USSR_Yug_ctry_FLOW_PROD$iso,
                     A.USSR_Yug_ctry_FLOW_PROD$FLOW,
                     A.USSR_Yug_ctry_FLOW_PROD$PRODUCT ) ) ]

# Replace post-USSR Yugoslavia years with the original data.
  	A.USSR_Yug_ctry_stat[ X_postUSSR_Yug_years ] <-
         A.USSR_Yug_ctry[ X_postUSSR_Yug_years ]

# Replace NAs with 0.
# Make sure that rows with NAs are not needed.
	  A.USSR_Yug_ctry_stat[ is.na( A.USSR_Yug_ctry_stat ) ] <- 0

# -----------------------------------------------------------------------------
# 5. Fix scaling of Former Soviet Union (FSU) transformation sector

# Here the reporting is not consistent between the FSU years and afterward. So instead of
# by sector, the scaling is performed using the sum of the main transformation sectors
# (ELEC, HEAT, CHP) as follows:
# E(country, year, sector, fuel) =
#        ( E(country, 1990, sector, fuel) / E(country, 1990, STransF, fuel) ) *
#        ( E(country, 1990, STransF, fuel) /  E(FSU, 1990, STransF, fuel) ) *
#        E(FSU, year, STransF, fuel)
#
# Where STransF = the sum of ELEC, HEAT, + CHP (MAIN only, not Autoproducer)
# variable transf_flows below
#
# The first ratio is the 1990 fraction of transformation consumption in a specific
# transformation sub-sector, the section ratio is that country's fraction of transformation
# consumption for the whole FSU, and the final term is the FSU tranformation consumption
# for the given year.
#
# Note that the country 1990 transformation sum cancels, so the calculation can be
# simplified to:
# E(country, year, sector, fuel) =
#        ( E(country, 1990, sector, fuel) / E(FSU, 1990, STransF, fuel) ) *
#        E(FSU, year, STransF, fuel)

# Note that it appears that autoproduction before 1990 was included in the industrial
# sector, whereas this is split out afterward. This is not directly accounted for, however
# all industrial energy consumption should be put somewhere in the downscaling.

# Define temporary new name for data frame
    fsu_data <- A.USSR_Yug_ctry_stat
    fsu_years <- IEA_start_year : 1990
    fsu_X_years <- paste0( 'X', fsu_years )
    fsu_replace_years <- IEA_start_year : 1989
    fsu_X_replace_years <- paste0( 'X', fsu_replace_years )

# These are the only flows that require re-scaling, for the moment
    transf_flows <- c( "MAINELEC", "MAINHEAT", "MAINCHP" )

# Calculate/sum FSU tranformation sector by year and fuel
# E(FSU, year, STransF, fuel)
    fsu_transf_data <- fsu_data[ which( fsu_data$FLOW %in% transf_flows &
                                        fsu_data$IEAcomp %in% FSU_IEA_composite_name ), ]

    A.FSU_trasf_shares <- fsu_transf_data %>%
        dplyr::select( PRODUCT, all_of(fsu_X_years) ) %>%
        dplyr::group_by( PRODUCT ) %>%
        dplyr::summarise_all( list( ~sum( ., na.rm = T ) ) ) %>%
        dplyr::ungroup( ) %>%
        as.data.frame( )

# Calculate the share of each country within the FSU aggregate tranformation sector in 1990
# ( E(country, 1990, sector, fuel) / E(FSU, 1990, STransF, fuel) )
    fsu_transf_data$X1990_share <- fsu_transf_data$X1990 /
                                      A.FSU_trasf_shares[ match( paste( fsu_transf_data$PRODUCT ),
                                                                 paste( A.FSU_trasf_shares$PRODUCT ) ),
                                                          'X1990' ]

    fsu_transf_data[ is.na( fsu_transf_data$X1990_share ), 'X1990_share' ] <- 0

# Calculate Scaled Value
# ( E(country, 1990, sector, fuel) / E(FSU, 1990, STransF, fuel) ) * E(FSU, year, STransF, fuel)

    fsu_transf_data_corrected <- fsu_transf_data
    fsu_transf_data_corrected [ fsu_X_replace_years ] <- fsu_transf_data_corrected$X1990_share *
                                                           A.FSU_trasf_shares[
                                                              match( fsu_transf_data_corrected$PRODUCT,
                                                                     A.FSU_trasf_shares$PRODUCT ),
                                                              fsu_X_replace_years ]

# Replace with new data
    fsu_data <- replaceValueColMatch( fsu_data,fsu_transf_data_corrected,
                                      x.ColName = fsu_X_replace_years,
                                      match.x = c( 'iso', 'FLOW', 'PRODUCT' ),
                                      addEntries = FALSE )

# Put it back
    A.USSR_Yug_ctry_stat <- fsu_data

# -----------------------------------------------------------------------------
# 6. Now process composite regions
#    Composite regions where CDIAC data is used to downscale energy to countries
#    over all historical years

#   A. Subset composite regions
    other_countries <- c( Other_African_composite_name, Other_Americas_composite_name, Other_Asia_composite_name )
	A.IEA_others  <- subset( A.IEAcomp, COUNTRY %in% other_countries )

#   B. Add iso - same as IEA name for "other_countries"
    A.IEA_others$iso <- A.IEA_others$COUNTRY

#   C. Split between bunker fuel and other fuel use (use different cdiac data to disaggregate)
    bunker_flows <- c( 'MARBUNK', 'AVRBUNK' )
    A.IEA_others_fuel <- A.IEA_others[ which( A.IEA_others$FLOW %!in% bunker_flows ), ]
    A.IEA_others_bunkers <- A.IEA_others[ which( A.IEA_others$FLOW %in% bunker_flows ), ]

#   D. Disaggregate other_fuel
#       i.) Disaggregate other African countries
        A.IEA_others_fuel_africa <- disaggregate_country( original_data = A.IEA_others_fuel,
                                                trend_data = cdiac_trend_fuel,
                                                trend_match_cols = c( 'iso', 'PRODUCT' ),
                                                combined_iso = Other_African_composite_name,
                                                disaggregate_iso = unique( MCL[ which( MCL$IEAName == Other_African_composite_name ),'iso' ] ),
                                                dis_end_year = IEA_end_year,
                                                dis_start_year = start_year,
                                                method = 2,
                                                id_cols = T,
                                                remove_aggregate = T,
                                                write_over_values = F,
                                                allow_dropped_data = T )

#       ii.)  Disaggregate other Asian countries
        A.IEA_others_fuel_asia <- disaggregate_country( original_data = A.IEA_others_fuel_africa,
                                                trend_data = cdiac_trend_fuel,
                                                trend_match_cols = c( 'iso', 'PRODUCT' ),
                                                combined_iso = Other_Asia_composite_name,
                                                disaggregate_iso = unique( MCL[ which( MCL$IEAName == Other_Asia_composite_name ),'iso' ] ),
                                                dis_end_year = IEA_end_year,
                                                dis_start_year = start_year,
                                                method = 2,
                                                id_cols = T,
                                                remove_aggregate = T,
                                                write_over_values = F,
                                                allow_dropped_data = T )

#       iii.) Disaggregate other non-OECD American countries
#             Note: Suriname is included within this IEA region through 1999, afterwards it is within
#             its own region (IEA World energy Statistics: Database Documentation (2019 edition), page 46)

#           a.) Diaggregate through 1999 with Suriname included
            A.IEA_others_fuel_asia_to_1999 <- A.IEA_others_fuel_asia %>%
                dplyr::select( all_of(IEA_IDcodes), "iso", paste0( "X", start_year : 1999 ) )

            A.IEA_others_fuel_americas_to_1999 <- disaggregate_country( original_data = A.IEA_others_fuel_asia_to_1999,
                                                trend_data = cdiac_trend_fuel,
                                                trend_match_cols = c( 'iso', 'PRODUCT' ),
                                                combined_iso = Other_Americas_composite_name,
                                                disaggregate_iso = unique( MCL[ which(
                                                    MCL$IEAName == Other_Americas_composite_name | MCL$iso == "sur" ),'iso' ] ),
                                                dis_end_year = 1999,
                                                dis_start_year = start_year,
                                                method = 2,
                                                id_cols = T,
                                                remove_aggregate = T,
                                                write_over_values = F,
                                                allow_dropped_data = T )

#           b.) Subset Suriname  data through 1999 - to be added to all IEA data later in processing
            A.IEA_others_fuel_suriname_to_1999 <- A.IEA_others_fuel_americas_to_1999 %>%
                dplyr::filter( iso == "sur" )

            A.IEA_others_fuel_americas_to_1999 <- A.IEA_others_fuel_americas_to_1999 %>%
                dplyr::filter( iso != "sur" )

#           c.) Disaggregate 2000-2017 without Suriname, as data for this region exists for 2000 - 2017
            A.IEA_others_fuel_asia_2000_to_end_year <- A.IEA_others_fuel_asia %>%
                dplyr::select( all_of(IEA_IDcodes), "iso", paste0( "X", 2000 : IEA_end_year ) )

            A.IEA_others_fuel_americas_2000_to_end_year <- disaggregate_country( original_data = A.IEA_others_fuel_asia_2000_to_end_year,
                                                trend_data = cdiac_trend_fuel,
                                                trend_match_cols = c( 'iso', 'PRODUCT' ),
                                                combined_iso = Other_Americas_composite_name,
                                                disaggregate_iso = unique( MCL[ which(
                                                    MCL$IEAName == Other_Americas_composite_name ),'iso' ] ),
                                                dis_end_year = IEA_end_year,
                                                dis_start_year = 2000,
                                                method = 2,
                                                id_cols = T,
                                                remove_aggregate = T,
                                                write_over_values = F,
                                                allow_dropped_data = T )

#           d.) Combine the two other Americas data frames, without Suriname
            A.IEA_others_fuel_americas <- A.IEA_others_fuel_americas_to_1999 %>%
                dplyr::left_join( A.IEA_others_fuel_americas_2000_to_end_year, by = c( IEA_IDcodes, "iso" ) )

#   E. Disaggregate bunker fuel
#       i.) Disaggregate other African countries
        A.IEA_others_bunkers_africa <- disaggregate_country( original_data = A.IEA_others_bunkers,
                                                trend_data = cdiac_trend_bunkers,
                                                trend_match_cols = c( 'iso', 'FLOW' ),
                                                combined_iso = Other_African_composite_name,
                                                disaggregate_iso = unique( MCL[ which( MCL$IEAName == Other_African_composite_name ),'iso' ] ),
                                                dis_end_year = IEA_end_year,
                                                dis_start_year = start_year,
                                                method = 2,
                                                id_cols = T,
                                                remove_aggregate = T,
                                                write_over_values = F )

#       ii.)  Disaggregate other Asian countries
        A.IEA_others_bunkers_asia <- disaggregate_country( original_data =  A.IEA_others_bunkers_africa,
                                                trend_data = cdiac_trend_bunkers,
                                                trend_match_cols = c( 'iso', 'FLOW' ),
                                                combined_iso = Other_Asia_composite_name,
                                                disaggregate_iso = unique( MCL[ which( MCL$IEAName == Other_Asia_composite_name ),'iso' ] ),
                                                dis_end_year = IEA_end_year,
                                                dis_start_year = start_year,
                                                method = 2,
                                                id_cols = T,
                                                remove_aggregate = T,
                                                write_over_values = F )

#       iii.) Disaggregate non-OECD American countries
#             Note: Suriname is included within this IEA region through 1999, afterwards it is within
#             its own region (IEA World energy Statistics: Database Documentation (2019 edition), page 46)

#           a.) Diaggregate through 1999 with Suriname included
            A.IEA_others_bunkers_asia_to_1999 <- A.IEA_others_bunkers_asia %>%
                dplyr::select( all_of(IEA_IDcodes), "iso", paste0( "X", start_year : 1999 ) )

            A.IEA_others_bunkers_americas_to_1999 <- disaggregate_country( original_data = A.IEA_others_bunkers_asia_to_1999,
                                                trend_data = cdiac_trend_bunkers,
                                                trend_match_cols = c( 'iso', 'FLOW' ),
                                                combined_iso = Other_Americas_composite_name,
                                                disaggregate_iso = unique( MCL[ which( MCL$IEAName == Other_Americas_composite_name | MCL$iso == "sur" ),'iso' ] ),
                                                dis_end_year = 1999,
                                                dis_start_year = start_year,
                                                method = 2,
                                                id_cols = T,
                                                remove_aggregate = T,
                                                write_over_values = F )

#           b.) Subset Suriname  data through 1999 - to be added to all IEA data later in processing
            A.IEA_others_bunkers_suriname_to_1999 <- A.IEA_others_bunkers_americas_to_1999 %>%
                dplyr::filter( iso == "sur" )

            A.IEA_others_bunkers_americas_to_1999 <- A.IEA_others_bunkers_americas_to_1999 %>%
                dplyr::filter( iso != "sur" )

#           c.) Disaggregate 2000-2017 without Suriname, as data for this region exists for 2000 - 2017
            A.IEA_others_bunkers_asia_2000_to_end_year <- A.IEA_others_bunkers_asia %>%
                dplyr::select( all_of(IEA_IDcodes), "iso", paste0( "X", 2000 : IEA_end_year ) )

            A.IEA_others_bunkers_americas_2000_to_end_year <- disaggregate_country( original_data = A.IEA_others_bunkers_asia_2000_to_end_year,
                                                trend_data = cdiac_trend_bunkers,
                                                trend_match_cols = c( 'iso', 'FLOW' ),
                                                combined_iso = Other_Americas_composite_name,
                                                disaggregate_iso = unique( MCL[ which( MCL$IEAName == Other_Americas_composite_name ),'iso' ] ),
                                                dis_end_year = IEA_end_year,
                                                dis_start_year = 2000,
                                                method = 2,
                                                id_cols = T,
                                                remove_aggregate = T,
                                                write_over_values = F )

#           d.) Combine the two other Americas data frames, without Suriname
            A.IEA_others_bunkers_americas <- A.IEA_others_bunkers_americas_to_1999 %>%
                dplyr::left_join( A.IEA_others_bunkers_americas_2000_to_end_year, by = c( IEA_IDcodes, "iso" ) )

#   F. Combine the composite region data frames (others_fuel and others_bunkers)
    A.Others_ctry_stat <- dplyr::bind_rows( A.IEA_others_fuel_americas, A.IEA_others_bunkers_americas )

#   G. Combine the Suriname data frames (up to 1999 - others_fuel and others_bunkers)
    A.Others_suriname_stat_to_1999 <- dplyr::bind_rows( A.IEA_others_fuel_suriname_to_1999,
                                                        A.IEA_others_bunkers_suriname_to_1999)

# -----------------------------------------------------------------------------
# 7. Now combine data into one database

# A. Combine data on Suriname (data pre-2000 from disaggregated non-OECD
#   American countries and 2000 - IEA_end_year within A.Others_ctry_stat)

#       i.) Subset Suriname data from A.IEAsingle - 2000 through IEA_end_year
        A.Others_suriname_stat_2000_to_end_year <- A.IEAsingle %>%
            dplyr::filter( iso == "sur" ) %>%
            dplyr::select( all_of(IEA_IDcodes), paste0( "X", 2000 : IEA_end_year ), iso )

        A.IEAsingle <- A.IEAsingle %>%
            dplyr::filter( iso != "sur" )

#       ii.)  Combine the Suriname data
        suriname_IEA_name <- unique( MCL %>% dplyr::filter( iso == "sur" ) %>% dplyr::select( IEAName ) )

        A.Suriname_stat <- A.Others_suriname_stat_to_1999 %>%
            dplyr::mutate( COUNTRY = paste( suriname_IEA_name ) ) %>%  # Set pre-2000 data COUNTRY to "Suriname" from Other_Americas_composite_name
            dplyr::left_join( A.Others_suriname_stat_2000_to_end_year, by = c( IEA_IDcodes, "iso" ) )

# B. Subset final energy statistics to only the rows that aren't zero in all years
  	A.IEAsingle <- A.IEAsingle[ rowSums( A.IEAsingle[
          X_IEA_years ] ) != 0, ]
  	A.IEAsingle_noUSSR_Yug <- subset( A.IEAsingle, iso %!in%
          A.USSR_Yug_ctry_stat$iso )

  	A.USSR_Yug_ctry_stat <- A.USSR_Yug_ctry_stat[ rowSums(
          A.USSR_Yug_ctry_stat[ X_IEA_years ] ) != 0, ]
  	A.Others_ctry_stat <- A.Others_ctry_stat[ rowSums(
          A.Others_ctry_stat[ X_IEA_years ] ) != 0, ]
  	A.Suriname_stat <- A.Suriname_stat[ rowSums(
  	    A.Suriname_stat[ X_IEA_years ] ) != 0, ]

# C. Combine the country-level data tables and write out energy balances
# Use iso codes rather than IEA's country names
  	IEA_isoID <- c( "iso", "FLOW", "PRODUCT" )
  	A.IEA_en_stat_ctry_hist <- dplyr::bind_rows(
  	    A.IEAsingle_noUSSR_Yug[ c( IEA_isoID, X_IEA_years ) ],
      	A.USSR_Yug_ctry_stat[ c( IEA_isoID, X_IEA_years ) ],
      	A.Others_ctry_stat[ c( IEA_isoID, X_IEA_years ) ],
      	A.Suriname_stat[ c( IEA_isoID, X_IEA_years ) ] )

# -----------------------------------------------------------------------------
# 8. Output Final IEA Energy Data

# Add comments for each table
    comments.A.IEA_en_stat_ctry_hist <- c( paste0( "IEA energy statistics ",
        "downscaled to 216 countries (iso / FLOW / PRODUCT / historical year)" ),
        "Units = kt, TJ or GWh" )

# Write table as CSV file
	  writeData( A.IEA_en_stat_ctry_hist, "MED_OUT", "A.IEA_en_stat_ctry_hist",
               comments = comments.A.IEA_en_stat_ctry_hist )

# Write diagnostics
  	if ( exists( "IEA_adjusted" ) ) {
    	  writeData( IEA_adjusted, "DIAG_OUT", "A.IEA_en_stat_adjusted" )
    	  writeData( IEA_adjust_diff, "DIAG_OUT", "A.IEA_en_stat_adjust_diff" )
    }

# Every script should finish with this line
	  logStop()

#END
