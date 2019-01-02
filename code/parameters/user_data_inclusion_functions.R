#------------------------------------------------------------------------------
# Program Name: user_data_inclusion_functions.R
# Author: Ben Goldstein, Caleb Braun
# Date Last Updated: December, 2018
# Program Purpose: Contains functions for including pre-processed user-defined
#                  energy extension data. This file focuses mainly on the
#                  functionality for the actual integration of user and default
#                  datasets, leaving the pre-processing of either for other
#                  functions.
#
# Notes: See also user_extension_instr_processing.R and user_data_processing.R
# -----------------------------------------------------------------------------


# includeUserData
#
# Normalize, disaggregate, and then incorporate the user-defined data into the
# default activity data, returning a list with both the data and diagnostics.
#
# Args:
#    usrdata: a subsetted dataframe of user-specified data
#    default_data: a dataframe of unchanged activity data extracted from
#      all_activity
#    Xyears: the year range of data processing
#    keep_total_cols: the columns whose aggregate value should not change
#    filename: Root filename for the data, currently unused
#    specified_breakdowns: Whether to use user-defined breakdowns, currently
#      unused
includeUserData <- function( usrdata, default_data, Xyears, keep_total_cols,
                             filename, specified_breakdowns, all_activity ) {

    CEDS_cols <- getCEDSAggCols()
    usrdata_cols <- intersect( CEDS_cols, names( usrdata ) )

    # Disaggregate user data to lowest level based on default shares
    usrdata_disagg <- disaggregate( usrdata, default_data, usrdata_cols, all_activity )

    # Essentially an "update_join" --> Perhaps this should be rewritten in the near future
    unnormalized <- replaceValueColMatch( default_data, usrdata_disagg, x.ColName = Xyears,
                                          match.x = CEDS_cols, addEntries = FALSE )

    # Normalize data when you are maintaining the totals for the higher
    # aggregate group (if all userdata_cols are not within keep_total_cols)
    if( all( usrdata_cols %in% keep_total_cols) ) {
        normalized <- unnormalized
        warning_diagnostics <- paste("Normalization did not occur as user did not",
                                     "specify that they wanted to maintain totals",
                                     "at a higher aggregate group.")
    }
    else {
        normalized <- normalize( default_data, usrdata_disagg, keep_total_cols,
                                 usrdata_cols, Xyears, unnormalized, all_activity )

        diagnostics <- normalized$diagnostics
        normalized <- normalized$data

        # Call the generateWarnings function to diagnose how well we did retaining column sums
        warning_diagnostics <- generateWarnings( Xyears, normalized, default_data,
                                                 diagnostics)
    }

    # Essentially an "update_join" --> Perhaps this should be rewritten in the near future
    all_activity_data <- replaceValueColMatch( all_activity_data, normalized,
                                               x.ColName = Xyears,
                                               match.x = CEDS_cols,
                                               addEntries = FALSE )

    rows_changed <- sum( apply( normalized != default_data, 1, any ) )
    diagnostics <- data.frame( rows_changed, warning_diagnostics )

    return( list( all_data = all_activity_data, diagnostics = diagnostics ) )
}

# normalize
#
# Handles normalizing data and disaggregating data based on percentage breakdowns.
#
# Normalization, the process of modifying rows that weren't directly specified
# in order to retain aggregate information, should only happen if both:
#   a) a whole group was not specified and
#   b) no manual override of normalization was called
#
# Args:
#    default_data: a dataframe of unchanged activity data extracted from
#      all_activity
#    usrdata_disagg: a subsetted dataframe of user-specified data
#    keep_total_cols: the columns specifiying the aggregation level to normalize
#      to; the groupings defined by these columns will retain their default
#      total values
#    usrdata_cols: the id columns of the user data
#    Xyears: the year range of data processing
#    unnormalized: the combined default activity data and user data; this should
#      just contain replaced raw values, with no other processing
#    all_activity_data: the dataframe holding all activity
#
# Returns:
#   A list containing the adjusted data (all_activity_data) and information
#     about the normalization, including any warnings (diagnostics)
normalize <- function( default_data, usrdata_disagg, keep_total_cols,
                       usrdata_cols, Xyears, unnormalized, all_activity_data ) {
    # Initialize diagnostic info
    diagnostics <- list( negatives = FALSE, need_user_spec = FALSE )

    CEDS_cols <- getCEDSAggCols(iso = TRUE)

    # Select the rows from the original data which were not directly changed
    default_to_change <- default_data %>%
        semi_join(usrdata, by = keep_total_cols) %>%
        anti_join(usrdata, by = usrdata_cols ) %>%
        select(CEDS_cols, Xyears)

    # If all the rows were directly changed then we've specified data for an
    # entire aggregate group and therefore can't normalize
    if ( nrow( default_to_change ) == 0 ) {
        return( list( data = unnormalized, diagnostics = diagnostics ) )
    }

    # All calculations are just done with the year columns. Take those
    # columns and replace the values with their proportion of the total
    # value for the whole year. This provides a fraction representing how
    # much of a difference each row will need to absorb; it says what
    # percent of the unedited aggregate group it is responsible for. Note
    # that 'drop = F' keeps data as dataframe even if indexing one year.
    pct_of_disagg_group <- default_to_change[ , Xyears, drop = F ] %>%
        as.matrix() %>%
        prop.table( margin = 2 )

    # If all of the data in a year (that wasn't user-specified) is 0, this
    # is like having a "whole group" -- we do not need to (cannot) normalize
    # in this case. We will not compare these rows in the warnings check.
    all_zero_years <- names( which ( colSums( pct_of_disagg_group ) == 0 ) )

    # Create vectors for the sums of activity in the group for each year
    # before and after including the user data (pre-normalization).
    year_totals <- colSums( default_data[ , Xyears, drop = F ] )
    new_year_totals <- colSums( unnormalized[ , Xyears, drop = F ] )

    # Likely, adding new data will change the sum of the aggregate group;
    # the point of normalization is to eliminate this change. The variable
    # annual_diffs stores the yearly difference between pre- and post-
    # inclusion data, for each aggregate group, for each year.
    annual_diffs <- year_totals - new_year_totals

    # Disaggregate data if all pct_of_disagg_group == 0
    normalize_aggregate <- default_to_change %>%
        group_by_at( keep_total_cols ) %>%
        summarise_at( Xyears, sum )
    normalize_aggregate[ , Xyears ] <- normalize_aggregate[ , Xyears, drop = F ] + annual_diffs

    # Select the rows from the original data which were not directly changed
    default_to_change_4_disagg <- default_data %>%
        semi_join(usrdata, by = keep_total_cols) %>%
        anti_join(usrdata, by = usrdata_cols )

    normalized <- disaggregate( normalize_aggregate, default_to_change_4_disagg,
                                keep_total_cols, all_activity_data )

    # If the total provided for only the user-defined columns exceeds the
    # original total for the entire aggregate group, data will be normalized
    # to negative emissions. We need to force this to zero but make a note
    # of it (the negatives boolean will generate a warning later on).
    if ( any( normalized < 0 ) ) {
        normalized[ ( normalized < 0 ) ] <- 0
        diagnostics$negatives <- T
    }

    # Add the normalized data into the dataframe containing the user-defined
    # data. After this, it has both the user-defined data and normalized
    # versions of the rows which are not user defined
    normalized_final <- unnormalized %>%
        replaceValueColMatch( normalized, Xyears, Xyears, CEDS_cols, CEDS_cols, addEntries = FALSE)

    return( list( data = normalized_final, diagnostics = diagnostics ) )
}


# handle_nan_breakdowns
#
# This function addresses the case where user-specified emissions
# replace zero-emissions cells. The user data cannot be disaggregated using
# factors of zero, so the percent breakdowns must be calculated by other means.
#
# Three options are tried:
#   1. Use data from other years to interpolate breakdowns:
#      If other years in the same row have non-zero values, they can be used to
#      determine the breakdowns for the years with zero values being replaced.
#      In this case, the breakdowns are linearly interpolated.
#   2. Interpolate using data from global default dataset:
#      This is done in the case that the entire row in question contains zeros.
#      In this case, the breakdowns are calculated from the default activity.
#   3. Disaggregate with equal values:
#      If the first two options didn't find values to interpolate breakdowns
#      from, just breakdown evenly across all disaggregation levels. There is
#      likely something wrong in this case, so throw a warning.
#
# Args:
#  disagg_pct_breakdowns: df of proportions at the most disaggregate level
#  global_data: global activity data just in case one cannot interpolate
#  Xyears: years we need the breakdown for
#  agg_id_cols: the columns specifying the aggregate group for the breakdowns
#
# Returns:
handleNanBreakdowns <- function( disagg_pct_breakdown, global_data, Xyears, agg_id_cols ) {
    disagg_breakdown_nans <- is.nan.df( disagg_pct_breakdown[ , Xyears ] )

    # We don't need to handle NaNs if there aren't any
    if ( !any( disagg_breakdown_nans ) ) {
        return(disagg_pct_breakdown)
    }

    # Extract just the breakdown rows that contain NaNs
    any_nan <- disagg_pct_breakdown[ apply( disagg_breakdown_nans, 1, any ), ]

    all_years <- isXYear( names( disagg_pct_breakdown ) )

    for ( row_num in 1:nrow(any_nan) ) {
        current_row <- any_nan[row_num, ]

        # Try option 1 (see function documentation)
        if ( !all( is.nan.df( current_row[ , all_years ] ) ) ) {
            current_row_no_NaN <- interpBreakdowns( current_row, all_years,
                                                    nan_replace_zeroes = TRUE )
        }
        # Try options 2 and 3
        else {
            current_row_no_NaN <- breakdownFromGlobal( current_row, global_data,
                                                       Xyears, agg_id_cols )
        }

        # overwrite the rows to replace (r2r) with the new breakdowns
        r2r <- disagg_pct_breakdown$CEDS_fuel %in% current_row_no_NaN$CEDS_fuel &
            disagg_pct_breakdown$CEDS_sector %in% current_row_no_NaN$CEDS_sector
        disagg_pct_breakdown[ r2r, Xyears ] <- current_row_no_NaN[ Xyears ]
    }

    # specified breakdowns are currently not supported by the CEDS data system
    specified_breakdowns <- FALSE

    if ( any( specified_breakdowns ) ) {
        tryCatch({
            user_breakdown <- readData( paste0( filename, "-breakdowns" ),
                                        domain = "EXT_IN",
                                        domain_extension = "user-defined-energy/" )
            user_breakdown <- as.data.frame(user_breakdown)
        }, error = function(e) {
            stop( paste( "No user specified breakdown found for", filename ) )
        })

        # Do we still need this check?
        if ( !all.na( user_breakdown ) ) {
            if ( any( colnames( user_breakdown ) == "all" ) ) {
                # use the "all" column for every year
                user_breakdown[ , Xyears ] <- user_breakdown$all
            }
            # Confirm that there are year headers and merge them in
            years_to_merge <- colnames( user_breakdown )[ which(
                isXYear( colnames( user_breakdown ) ) ) ]

            disagg_pct_breakdown[ which( paste0( disagg_pct_breakdown$CEDS_fuel,
                                                 disagg_pct_breakdown$CEDS_sector,
                                                 disagg_pct_breakdown$iso ) %in%
                                             paste0( user_breakdown$CEDS_fuel,
                                                     user_breakdown$CEDS_sector,
                                                     user_breakdown$iso ) ),
                                  c( "CEDS_fuel", "CEDS_sector", "iso",
                                     years_to_merge ) ] <-
                user_breakdown[ , c( "CEDS_fuel", "CEDS_sector", "iso",
                                     years_to_merge ) ]
            # Check to make sure everything still adds up to 100%
            double_check_breakdowns <- ddply( disagg_pct_breakdown, agg_cols_to_breakdown,
                                              function(x) colSums( x[ Xyears ] ) )

            if ( any( double_check_breakdowns[ , Xyears ] %!in%
                      c( 1,0 ) ) ) {
                # Join to the disaggregates, and then divide
                correction_factors <- left_join( disagg_data_changed[ , agg_cols_to_breakdown ],
                                                 double_check_breakdowns,
                                                 by = agg_cols_to_breakdown )
                disagg_pct_breakdown[ , Xyears ] <-
                    disagg_pct_breakdown[ , Xyears ] /
                    correction_factors[ , Xyears ]
                disagg_pct_breakdown[ is.nan.df( disagg_pct_breakdown ) ]<- 0
            }
        }
    }

    return(disagg_pct_breakdown)
}


# generateWarnings
#
# Brief: A helper function for normalizeAndIncludeData. Analyzes the output and
#        prepares warning diagnostics based on how much data was changed.
# params:
#    Xyears: the year range of data processing
#    disagg_data_changed: the disaggregate product of normalization and update
#    data_to_use: a dataframe of unchanged activity data extracted from all_activity
#    negatives: a logical indicating whether any negative values were reset to 0
generateWarnings <- function ( Xyears, disagg_data_changed, data_to_use,
                               diagnostics ) {

    # Exclude those years for which there is no non-user-specified data from
    # generating warnings
    years_to_compare <- Xyears

    need_user_spec <- diagnostics$need_user_spec
    negatives <- diagnostics$negatives

    if ( need_user_spec ) {
        return( "User-specified percent breakdowns are needed; no global defaults." )
    }

    warning_diag <- NA

    # Assign rounding convention
    normalized_rounded <- round( colSums( disagg_data_changed[ , years_to_compare, drop = FALSE ] ), 3 )
    default_data_rounded <- round( colSums( data_to_use[ , years_to_compare, drop = FALSE ] ), 3 )

    # Check if data sums are the same for normalized data and default_data
   if ( any( normalized_rounded != default_data_rounded ) ) {

        cols_not_retained <- sum( normalized_rounded != default_data_rounded )
        warning_diag <- paste0( cols_not_retained, "/", length( Xyears ),
                                  " column sums were not retained." )
        if ( negatives ) {
            warning_diag <- paste( warning_diag,
            "some energy data has been coerced to 0 as the user provided",
            "a specified total which exceeded an aggregate group total in the default data." )
          }
    }

    return( warning_diag )
}


# enforceContinuity
#
# Prevent discontinuity between user-specified input and default activity data.
#
# Calculates final_activity based on the unchanged and changed versions of the
# activity dataframe, and a dataframe storing continuity "factors" that holds
# which cells need what proportion.
#
# Args:
#   activity: The activity list, which stores three things: changed activity
#     data, unchanged activity data, and continuity factors
#   yearsAllowed: The Xyears in the current run of the system
#
# Returns:
#   final_activity_data, a dataframe storing continuous activity data
enforceContinuity <- function( activity, yearsAllowed ) {
    # Initialize a dataframe to hold the results
    final_activity_data <- activity$all_activity_data

    # Calculation. For cells that don't need smoothing,
    #                 final activity data = all_activity data
    # For cells that do need smoothing,
    #                 final = (changed * factor) + (unchanged * (1-factor))
    factors <- activity$continuity_factors[ , yearsAllowed ]
    final_activity_data[ , yearsAllowed ] <-
        ( activity$all_activity_data[ , yearsAllowed ] * factors ) +
        ( activity$old_activity_data[ , yearsAllowed ] * ( 1 - factors ) )

    return( final_activity_data )
}


# addContinuityFactors
#
# Brief: Creates a dataframe storing the "continuity factors", which tell
#        what cells will need to be made continuous and what factor of scaling
#        they require.
# Params:
#   activity: the list holding activity
#   instructions: The master instruction list. This will be used to identify
#                 the beginning and end of each row of data, so as to enforce
#                 continuity at dataframe boundaries
#   interval_len: an optional value storing how many years should be made
#                 continuous at each edge, if possible.
# Returns:
#   the activity, now holding a continuity_factors dataframe
addContinuityFactors <- function( activity, instructions, all_yrs, interval_len = 7 ) {

    cfs <- activity$all_activity_data
    cfs[ , all_yrs ] <- 1

    for ( row_num in seq_along( nrow( instructions ) ) ) {
        # Select the row for initializating continuity
        this.row <- instructions[ row_num, ]
        s_year <- this.row$start_year
        e_year <- this.row$end_year

        # Determine the years of the data
        len_data_years <- sum( paste0( "X", s_year:e_year ) %in% all_yrs )

        # If the number of years of data is less than twice the length of the
        # continuity interval, the interval will need to be reduced.
        if ( len_data_years < interval_len * 2 )
            continuity_interval <- floor( len_data_years / 2 )
        else
            continuity_interval <- interval_len

        # Determine the aggregation columns for this instruction
        ceds_cols <- c( "iso", "CEDS_sector", "CEDS_fuel", "agg_sector", "agg_fuel" )
        join_cols <- c( ceds_cols[ !is.na( this.row[ , ceds_cols ] ) ] )

        # Extract the subset of disaggregated rows corresponding to the data and
        # confirm that the data actually exists.
        rows_to_adjust <- dplyr::left_join( this.row[ , join_cols ], cfs,
                                            by = join_cols ) %>%
                          dplyr::select( ceds_cols, dplyr::everything() )
        if ( anyNA( rows_to_adjust ) ) {
            stop( paste( "Error in instruction:\n\t",
                  paste( this.row[ join_cols ], collapse = " " ), "\n",
                  "No default data found for iso/fuel/sector specified" ) )
        }

        # The continuity step is by how much each value will increase each year
        # (ends at 1)
        continuity_step <- 1 / continuity_interval
        continuity_vals <- 1:continuity_interval * continuity_step

        # Enforce continuity at beginning and end of dataset, as specified by
        # the instruction. Note that `historical_pre_extension_year` and
        # `historical_end_extension_year` are global CEDS variables.
        if ( this.row$start_continuity && s_year > historical_pre_extension_year ) {
            interval_end <- s_year + continuity_interval - 1
            year_range <- paste0( "X", s_year:interval_end )
            rows_to_adjust[ , year_range ] <- rep( continuity_vals, each = NROW( rows_to_adjust ) )
        }
        if ( this.row$end_continuity && e_year < historical_end_extension_year ) {
            interval_end <- e_year - continuity_interval + 1
            year_range <- paste0( "X", e_year:interval_end )
            rows_to_adjust[ , year_range ] <- rep( continuity_vals, each = NROW( rows_to_adjust ) )
        }

        # Incorporate the new adjusted rows into the created dataframe
        cfs[ cfs$iso         %in% rows_to_adjust$iso &
             cfs$CEDS_sector %in% rows_to_adjust$CEDS_sector &
             cfs$CEDS_fuel   %in% rows_to_adjust$CEDS_fuel, ] <- rows_to_adjust
    }

    activity$continuity_factors <- cfs
    return( activity )
}


# breakdownFromGlobal
#
# Given a level of aggregation, calculate how much each disaggregated row makes
# up of the total, based on values for all countries
#
# If no global percent breakdowns are found, breakdown equally across all rows.
#
# Params:
#   pct_breakdown: an all-zero subset of percentage breakdowns
#   global_data: the dataset of global values to retrieve breakdowns from
#   Xyears: the year columns to replace with global breakdowns
#   agg_id_cols: the id columns definining the aggregate group that we are
#     breaking down (that is, the yearly sum of rows with matching id columns
#     will be 1)
breakdownFromGlobal <- function( pct_breakdown, global_data, Xyears, agg_id_cols ) {
    stopifnot("iso" %!in% agg_id_cols)

    global_filtered <- global_data %>%
        dplyr::select( -iso ) %>%
        dplyr::semi_join( pct_breakdown, by = agg_id_cols )

    if ( nrow( global_filtered ) == 0 ) {
        warning( "No breakdowns found for aggregate data" )
        global_totals[ , Xyears ] <- 1
    } else {
        global_totals <- global_filtered %>%
            dplyr::group_by_if( is.character ) %>%
            dplyr::summarise_all( sum )
    }

    # If all the rows have zero values for all years, give each year a
    # uniform value of 1 across all rows. This causes them to be divided evenly
    # among the breakdown categories by default, but is likely inaccurate so
    # generates a warning that the user needs to specify breakdowns.
    all_years <- names( global_totals )[ isXYear( names( global_totals ) ) ]
    if ( all( colSums( global_totals[ , all_years ] ) == 0 ) ) {
        global_totals[ , Xyears ] <- 1
    }

    # Calculate new percent breakdowns by dividing each row by its group's total
    new_breakdowns <- global_totals %>%
        dplyr::group_by_at( agg_id_cols ) %>%
        dplyr::mutate_at( all_years, prop.table ) %>%
        dplyr::ungroup()

    # If only some years had zero values for their aggregate total, interpolate
    # the breakdowns from years with useful data
    if ( any( is.nan.df( new_breakdowns[ , Xyears ] ) ) ) {
        new_breakdowns <- interpBreakdowns( new_breakdowns, all_years, TRUE )
    }

    join_cols <- intersectNames( pct_breakdown, new_breakdowns )
    join_cols <- setdiff( join_cols, all_years )

    replaceValueColMatch( pct_breakdown, new_breakdowns, x.ColName = Xyears,
                          match.x = join_cols, addEntries = F )
}

# interpBreakdowns
#
# For rows that have some but not all zero cells (some 0 rows available)
# we can use interpolate_NA() to fill in percent breakdowns linearly,
# making sure to retain a total of 100%.
interpBreakdowns <- function( pct_breakdown, Xyears, nan_replace_zeroes = FALSE ) {

    breakdowns_to_correct <- pct_breakdown[ , Xyears ]

    if (nan_replace_zeroes){
        # All columns containing all NaN will be replaced with NA, so we can use
        # interpolate_NAs() to replace them
        zcols <- colSums( is.nan.df(breakdowns_to_correct )) > 0

    } else {
        # All columns containing all zeros will be replaced with NA, so we can use
        # interpolate_NAs() to replace them
        zcols <- colSums( breakdowns_to_correct ) == 0
    }

    breakdowns_to_correct[ , zcols ] <- NA_real_

    # If the first or last year is NA this will make complete interpolation
    # impossible. We will therefore come up with a default if it is NA; we will
    # use the nearest cell with values.
    if ( zcols[1] ) {
        first_non_NA <- min( which( !zcols ) )
        breakdowns_to_correct[ , 1 ] <- breakdowns_to_correct[ , first_non_NA ]
    }
    if ( zcols[ length( zcols ) ] ) {
        last_non_NA <- max( which( !zcols ) )
        breakdowns_to_correct[ , length( zcols ) ] <- breakdowns_to_correct[ , last_non_NA ]
    }

    # Interpolate and assign the values to new_percent_breakdowns
    pct_breakdown[ , Xyears ] <- interpolate_NAs( breakdowns_to_correct )

    return( pct_breakdown )
}


# Map aggregate level ID number to column names
#
# Original notes:
# For each aggregate level, column names are specified that will guide
# normalization and processing. Because of this flexible architecture,
# specific aggregate levels can be easily adjusted or added without changing
# the design of the function. However, there may only be one column in
# cols_given that isn't present in normalizeTo; you can only ever normalize
# by a single "level" of aggregation. This is a law of the function, not of
# the process, and may be worth changing down the line.
#
# Args:
#   agg_level: Integer representing the aggregation level of a CEDS dataset
#
# Returns:
#   A character vector of the id columns corresponding given aggregation level
aggLevelToCols <- function(agg_level) {
    switch(agg_level,
        c("iso", "agg_fuel"),                                           # 1
        c("iso", "agg_fuel", "CEDS_fuel"),                              # 2
        c("iso", "agg_fuel", "CEDS_fuel", "agg_sector"),                # 3
        c("iso", "agg_fuel", "CEDS_fuel", "agg_sector", "CEDS_sector"), # 4
        c("iso", "agg_fuel", "agg_sector", "CEDS_sector"),              # 5
        c("iso", "agg_fuel", "agg_sector")                              # 6
    )
}
aggLevelToNormalize <- function(agg_level) {
    switch(agg_level,
        c("iso"),                                          # 1
        c("iso", "agg_fuel"),                              # 2
        c("iso", "agg_fuel", "agg_sector"),                # 3
        c("iso", "agg_fuel", "agg_sector", "CEDS_sector"), # 4
        c("iso", "agg_fuel", "agg_sector"),                # 5
        c("iso", "agg_fuel")                               # 6
    )
}


# Identify the aggregation level ID of a CEDS dataset
#
# Performs a simple check of column names to determine the data frame's level of
# aggregation. The CEDS level of aggregation is the most disaggregated allowed,
# and is specified by CEDS_COLS. Returns 0 if the level is unidentifiable.
#
# Args:
#    df: The dataframe whose level you wish to identify
#    na.rm: Remove aggregation columns that only contain NAs?
#
# Returns:
#   An integer representing the aggregation level of a CEDS dataset
identifyLevel <- function( df, na.rm = FALSE ) {
    CEDS_COLS <- getCEDSAggCols()

    agg_cols <- dplyr::select_if( df, funs( !na.rm | !all.na( . ) ) )
    agg_cols <- dplyr::intersect( CEDS_COLS, names( agg_cols ) )

    switch( paste( agg_cols, collapse = " " ),
        "agg_fuel"                                  = 1, # most aggregate
        "agg_fuel CEDS_fuel"                        = 2,
        "agg_fuel CEDS_fuel agg_sector"             = 3,
        "agg_fuel CEDS_fuel agg_sector CEDS_sector" = 4, # most disaggregate
        "agg_fuel agg_sector CEDS_sector"           = 5,
        "agg_fuel agg_sector"                       = 6,
        0
    )
}

getCEDSAggCols <- function(iso = FALSE) {
    cols <- c( "agg_fuel", "CEDS_fuel", "agg_sector", "CEDS_sector" )
    if ( iso ) {
        return( c( "iso", cols ) )
    } else {
        return( cols )
    }
}