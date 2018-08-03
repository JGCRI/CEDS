#------------------------------------------------------------------------------
# Program Name: user_data_inclusion_functions.R
# Author: Ben Goldstein, Caleb Braun
# Date Last Updated: January, 2018
# Program Purpose: Contains functions for including pre-processed user-defined
#                  energy extension data. This file focuses mainly on the
#                  functionality for the actual integration of user and default
#                  datasets, leaving the pre-processing of either for other
#                  functions.
# Functions Defined:
# Notes: See also user_extension_instr_processing.R and user_data_processing.R
# ------------------------------------------------------------------------------------

# normalizeAndIncludeData
# Brief: A functions for processing user extension data. It is flexible to each aggregate
#        level. It handles normalizing data and disaggregating data based on
#        already-present percentage breakdowns.
# params:
#    Xyears: the year range of data processing
#    data_to_use: a dataframe of unchanged activity data extracted from all_activity
#    user_dataframe_subset: a subsetted dataframe of user-specified data
#    all_activity_data: the dataframe holding all activity
#    override_normalization: a manual option for disabling normalization
#    agg_level: an integer indicating the aggregate level of the data given
#
#    TODO: Split this into many more functions

normalizeAndIncludeData <- function( Xyears, data_to_use, user_dataframe_subset,
                                     all_activity_data, override_normalization,
                                     agg_level, filename, specified_breakdowns ) {

    override_normalization <- any( override_normalization, na.rm = T )

    cols_given <- aggLevelToCols(agg_level)
    if(is.null(cols_given)) stop( paste( "agg_level", agg_level, "not supported" ) )

    # This will not be used at level 1 but is necessary for creating the identifier_col
    normalizeTo <- aggLevelToNormalize(agg_level)

    # The "identifier column" is the column that will be unique among all rows
    # once we have aggregated to this level.
    id_col <- dplyr::setdiff(cols_given, normalizeTo)

    # Aggregate the pre-update data to match the user data level of aggregation
    activity_agg_to_level <- ddply( data_to_use, cols_given,
                                    function(x) colSums( x[ Xyears ] ) )

    # Add the user data (in a new copy of the agg dataframe)
    rows_to_replace <- activity_agg_to_level[[id_col]] %in% user_dataframe_subset[[id_col]]
    act_agg_changed <- activity_agg_to_level
    act_agg_changed[ rows_to_replace, Xyears ] <- user_dataframe_subset[ , Xyears ]

    # If all the rows were directly changed then we've specified data for an
    # entire aggregate group and therefore can't normalize
    whole_group <- all( rows_to_replace )

    # Initialize warning info
    negatives <- F
    all_zero_years <- NA
    need_user_spec <- F

    # TODO: Break this out into separate function
    #
    # Normalization, the process of modifying rows that weren't directly
    # specified in order to retain aggregate information, should only happen if
    # both:
    #   a) a whole group was not specified and
    #   b) no manual override of normalization was called.
    if ( !whole_group & !override_normalization ) {

        # Select the rows from the original data which were not directly changed
        data_to_correct <- activity_agg_to_level[ !rows_to_replace,
                                                  c( cols_given, Xyears ) ]

        # All calculations are just done with the year columns. Take those
        # columns and replace the values with their proportion of the total
        # value for the whole year. This provides a fraction representing how
        # much of a difference each row will need to absorb; it says what
        # percent of the unedited aggregate group it is responsible for.
        pct_of_agg_group <- data_to_correct[ , Xyears, drop = F ] %>%
                            as.matrix() %>%
                            prop.table( margin = 2 )
        pct_of_agg_group[ is.nan( pct_of_agg_group ) ] <- 0

        # If all of the data in a year (that wasn't user-specified) is 0, this
        # is like having a "whole group" -- we do not need to (cannot) normalize
        # in this case. We will not compare these rows in the warnings check.
        all_zero_years <- names( which ( colSums( pct_of_agg_group ) == 0 ) )

        # Create vectors for the sums of activity in the group for each year
        # before and after including the user data (pre-normalization). Note
        # that 'drop = F' keeps data as dataframe even if indexing one year.
        year_totals <- colSums( activity_agg_to_level[ , Xyears, drop = F ] )
        new_year_totals <- colSums( act_agg_changed[ , Xyears, drop = F ] )

        # Likely, adding new data will change the sum of the aggregate group;
        # the point of normalization is to eliminate this change. The variable
        # annual_diffs stores the yearly difference between pre- and post-
        # inclusion data, for each aggregate group, for each year.
        annual_diffs <- year_totals - new_year_totals
        annual_diffs <- sweep( pct_of_agg_group, 2, annual_diffs, "*" )

        # Simply add the adjustments to the original data
        normalized <- data_to_correct[ , Xyears, drop = F ] + annual_diffs

        # If the total provided for only the user-defined columns exceeds the
        # original total for the entire aggregate group, data will be normalized
        # to negative emissions. We need to force this to zero but make a note
        # of it (the negatives boolean will generate a warning later on).
        if ( any( normalized < 0 ) ) {
            normalized[ ( normalized < 0 ) ] <- 0
            negatives <- T
        }

        # Add the normalized data into the dataframe containing the user-defined
        # data. After this, it has both the user-defined data and normalized
        # versions of the undefined rows
        act_agg_changed[ !rows_to_replace, Xyears ] <- normalized
    }

    disagg_data_changed <- data_to_use
    # Create a dataframe of group totals arranged correspondingly to the
    # disaggregate data
    join_cols <- c( "iso", "agg_fuel", "CEDS_fuel", "agg_sector", "CEDS_sector" )
    agg_group_totals_unchanged <- left_join( data_to_use[ , join_cols ],
                                             activity_agg_to_level, by = cols_given )
    agg_group_totals_changed   <- left_join( data_to_use[ , join_cols ],
                                             act_agg_changed, by = cols_given )

# Calculate the percentage of the aggregate group that each row used to make up
    disagg_pct_breakdown <- data_to_use
    disagg_pct_breakdown[ , Xyears ] <- data_to_use[ , Xyears ] /
                                        agg_group_totals_unchanged[ , Xyears ]

    disagg_pct_breakdown[ is.nan.df( disagg_pct_breakdown ) ] <- 0

# TODO: Break this out into separate function
#
# This section of the function addresses the case where user-specified emissions
# replace zero-emissions cells. The user data cannot be disaggregated using
# factors of zero, so the percent breakdowns must be calculated by other means.
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

    if ( any( activity_agg_to_level[ , Xyears ] == 0 &
              act_agg_changed[ , Xyears ] != 0 ) ) {

        # zero_cells holds a boolean data frame used to identify cells which
        # will have this issue (changed value is > 0 but original value is 0)
        orig_zeros <- activity_agg_to_level[ , Xyears, drop = F ] == 0
        chgd_zeros <- act_agg_changed[ , Xyears, drop = F ] == 0
        zero_cells <- cbind( act_agg_changed[ , cols_given ], orig_zeros & !chgd_zeros )
        any_zeros <- zero_cells[ apply( zero_cells[ , Xyears ], 1, any ), ]

        for ( row_num in 1:nrow(any_zeros) ) {
            current_row <- any_zeros[row_num, cols_given]
            bdown_4_row <- dplyr::left_join( current_row, disagg_pct_breakdown,
                                             by = cols_given )

            if ( all( bdown_4_row[ , Xyears ] == 0 ) ) {
                bdown_4_row <- breakdownFromGlobal(bdown_4_row, Xyears,
                                                   cols_given, join_cols)
            }
            else if ( any( bdown_4_row[ , Xyears ] == 0 ) ) {
                bdown_4_row <- interpBreakdowns( bdown_4_row, Xyears )
            }

            # overwrite the rows to replace (r2r) with the new breakdowns
            r2r <- disagg_pct_breakdown$CEDS_fuel %in% bdown_4_row$CEDS_fuel &
                   disagg_pct_breakdown$CEDS_sector %in% bdown_4_row$CEDS_sector
            disagg_pct_breakdown[ r2r, ] <- bdown_4_row
        }
    }


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
        if ( !all( is.na( user_breakdown ) ) ) {
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
            double_check_breakdowns <- ddply( disagg_pct_breakdown, cols_given,
                                      function(x) colSums( x[ Xyears ] ) )

            if ( any( double_check_breakdowns[ , Xyears ] %!in%
                                                            c( 1,0 ) ) ) {
            # Join to the disaggregates, and then divide
                correction_factors <- left_join( disagg_data_changed[ , cols_given ],
                                                 double_check_breakdowns,
                                                 by = cols_given )
                disagg_pct_breakdown[ , Xyears ] <-
                        disagg_pct_breakdown[ , Xyears ] /
                        correction_factors[ , Xyears ]
               disagg_pct_breakdown[ is.nan.df( disagg_pct_breakdown ) ]<- 0
            }
        }
    }


# Multiply these percentages by the new values to get updated versions
    disagg_data_changed[ , Xyears ] <- disagg_pct_breakdown[ , Xyears ] *
                                       agg_group_totals_changed[ , Xyears ]
    if ( agg_level == 4 ) {
        disagg_data_changed <- act_agg_changed
        need_user_spec <- F
    }

# Call the generateWarnings function to diagnose how well we did retaining column sums
    if ( agg_level != 1 ) {
        warning_diagnostics <- generateWarnings( Xyears, disagg_data_changed,
                                                 data_to_use, negatives, whole_group,
                                                 override_normalization, all_zero_years,
                                                 need_user_spec)
    } else {
        warning_diagnostics <- NA
    }

    all_activity_data[ which( all_activity_data$iso %in% disagg_data_changed$iso &
                              all_activity_data$CEDS_fuel %in% disagg_data_changed$CEDS_fuel &
                              all_activity_data$CEDS_sector %in% disagg_data_changed$CEDS_sector ), Xyears ] <-
    disagg_data_changed[ , Xyears ]

    rows_changed <- sum( apply( disagg_data_changed != data_to_use, 1, any ) )
    diagnostics <- data.frame( rows_changed, warning_diagnostics )

    return( list( all_data = all_activity_data, diagnostics = diagnostics ) )
}

#------------------------------------------------------------------------------
# generateWarnings
# Brief: A helper function for normalizeAndIncludeData. Analyzes the output and
#        prepares warning diagnostics based on how much data was changed.
# params:
#    Xyears: the year range of data processing
#    disagg_data_changed: the disaggregate product of normalization and update
#    data_to_use: a dataframe of unchanged activity data extracted from all_activity
#    negatives: a logical indicating whether any negative values were reset to 0
#    override_normalization: a logical indicating whether normalization was
#                            manually skipped
generateWarnings <- function ( Xyears, disagg_data_changed, data_to_use,
                               negatives, whole_group, override_normalization,
                               all_zero_years, need_user_spec ) {

    # Exclude those years for which there is no non-user-specified data from
    # generating warnings
    years_to_compare <- Xyears
    if ( length( all_zero_years ) > 0 ) {
        years_to_compare <- Xyears[ Xyears %!in% all_zero_years ]
    }

    if ( length( years_to_compare ) == 0 ) {
        return( NA )
    }

    if ( need_user_spec ) {
        return( "User-specified percent breakdowns are needed; no global defaults." )
    }

    warning_diag <- NA
    if ( !override_normalization && !whole_group ) {
        if ( length( years_to_compare ) > 1 ) {
            if ( any( round( colSums( disagg_data_changed[ , years_to_compare ] ), 3 ) !=
                      round( colSums( data_to_use[ , years_to_compare ] ), 3 ) ) ) {

              cols_not_retained <- sum( round( colSums( disagg_data_changed[ , years_to_compare ] ), 3 ) !=
                                        round( colSums( data_to_use[ , years_to_compare ] ), 3 ) )
              warning_diag <- paste0( cols_not_retained, "/", length( Xyears ),
                                      " column sums were not retained.")
              if ( negatives ) {
                  warning_diag <- paste0( warning_diag,
                                          " A specified total exceeded an aggregate group total." )
              }

            }
        } else {
            if ( any( round( sum( disagg_data_changed[ , years_to_compare ] ), 3 )
                      != round( sum( data_to_use[ , years_to_compare ] ), 3 ) ) ) {
              warning_diag <- "1 column sum was not retained."
              if ( negatives ) {
                  warning_diag <- paste0( warning_diag, "A specified total exceeded an aggregate group total.")
              }
           }
        }
    } else if ( override_normalization ) {
        warning_diag <- "Manual override of normalization"
    } else if ( whole_group ) {
        warning_diag <- "Whole group provided; aggregate sums overwritten"
    }
    return( warning_diag )
}


#------------------------------------------------------------------------------
# sumAllActivityByFuelSector
# Brief: A helper function for enforcing percentage breakdowns in all-zero rows.
#        Given a row (containing all zero values), find all other rows in the
#        default dataset with the same sector and fuel. Then, find the sum of
#        the data for that fuel and sector, which helps create a global default
#        percentage breakdown for a given aggregation category.
#
#        TODO: Remove global variable 'all_activity_data' as a default parameter
sumAllActivityByFuelSector <- function( guide_row, years = Xyears, data = all_activity_data ) {

    df_to_sum <- dplyr::filter( data, CEDS_sector == guide_row[["CEDS_sector"]],
                                      CEDS_fuel   == guide_row[["CEDS_fuel"]] )
    df_to_sum <- df_to_sum[ , c( "iso", "CEDS_sector", "CEDS_fuel",
                                 "agg_sector", "agg_fuel", years ) ]

    df_to_sum[ is.na( df_to_sum ) ] <- 0

    return( colSums( df_to_sum[ , years, drop = F ] ) )
}


#------------------------------------------------------------------------------
# enforceContinuity
# Brief: Used for preventing discontinuity between user-specified input and
#          default activity data.
# Purpose: calculates final_activity based on the unchanged and changed versions
#          of the activity dataframe, and a dataframe storing continuity "factors"
#          that holds which cells need what proportion.
# Params:
#   activity: the activity list, which stores three things: changed activity
#            data, unchanged activity data, and continuity factors
#   yearsAllowed: the Xyears in the current run of the system
# Returns: final_activity_data, a dataframe storing continuous activity data
enforceContinuity <- function( activity, yearsAllowed ) {

# Initialize a dataframe to hold the results
    final_activity_data <- activity$all_activity_data

# Calculation. For cells that don't need smoothing,
#                 final activity data = all_activity data
# For cells that do need smoothing,
#                 final = (changed * factor) + (unchanged * (1-factor))
    final_activity_data[ , yearsAllowed ] <-
                           ( activity$all_activity_data[ , yearsAllowed ] *
                             activity$continuity_factors[ , yearsAllowed ] ) +
                           ( activity$old_activity_data[ , yearsAllowed ] *
                           ( 1 - activity$continuity_factors[ , yearsAllowed ] ) )

# Return the result
    return( final_activity_data )
}



#------------------------------------------------------------------------------
# initContinuityFactors
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
# Returns: the activity, now holding a continuity_factors dataframe

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
        if ( any( is.na( rows_to_adjust ) ) ) {
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


#------------------------------------------------------------------------------
# Given a level of aggregation, calculate how much each disaggregated row makes
# up of the total, based on values for all countries
#
# This should only be done for data that has no non-zero years available. If no
# global percent breakdowns are found, breakdown equally across all rows.
#
# Params:
#   pct_breakdown:  an all-zero subset of percentage breakdowns
#   Xyears:         years to consider
#   join_cols:      columns present in data's most disaggregated form
#
breakdownFromGlobal <- function( pct_breakdown, Xyears, cols_given, join_cols ) {

    # Use the sumAllActivityByFuelSector function to generate totals for each
    # fuel x sector, ignoring iso.
    global_totals <- apply( pct_breakdown, 1, sumAllActivityByFuelSector, Xyears )
    global_totals <- t( global_totals )

    # If all the rows STILL have zero values for all years, give each year a
    # uniform value of 1 across all rows. This causes them to be divided evenly
    # among the breakdown categories by default, but is likely inaccurate so
    # generates a warning that the user needs to specify breakdowns.
    #
    # If only some rows still have zero values, interpolate from years with data
    if ( all( colSums( global_totals ) == 0 ) ) {
        global_totals[ , Xyears ] <- 1
        warning( "No breakdowns found for aggregate data" )
    }
    else if ( any( colSums( global_totals ) == 0 ) ) {
        global_totals <- data.frame( pct_breakdown[ join_cols ], global_totals )
        return( interpBreakdowns( global_totals, Xyears ) )
    }

    # Calculate new percent breakdowns by dividing each row by its group's total
    new_breakdown <- prop.table( global_totals, margin = 2 )
    new_breakdown <- data.frame( pct_breakdown[ join_cols ], new_breakdown )

    return( new_breakdown )
}

#------------------------------------------------------------------------------
# For rows that have some but not all zero cells (some 0 rows available)
# we can use interpolate_NA() to fill in percent breakdowns linearly,
# making sure to retain a total of 100%.
interpBreakdowns <- function( pct_breakdown, Xyears ) {
    breakdowns_to_correct <- pct_breakdown[ , Xyears ]

    # All columns containing all zeros will be replaced with NA, so we can use
    # interpolate_NAs() to replace them
    zcols <- colSums( breakdowns_to_correct ) == 0
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


# For each aggregate level, column names are specified that will guide
# normalization and processing. Because of this flexible architecture,
# specific aggregate levels can be easily adjusted or added without changing
# the design of the function. However, there may only be one column in
# cols_given that isn't present in normalizeTo; you can only ever normalize
# by a single "level" of aggregation. This is a law of the function, not of
# the process, and may be worth changing down the line.
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

#------------------------------------------------------------------------------
# identifyLevel
# Brief: Performs a simple check of column names to determine the data frame's
#        level of aggregation. The CEDS level of aggregation is the most
#        disaggregated allowed, and is specified by CEDS_COLS.
# params:
#    df:        the dataframe whose level you wish to identify
#    na.rm:     remove aggregation columns that only contain NAs?
identifyLevel <- function ( df, na.rm = FALSE ) {
    CEDS_COLS <- c( "agg_fuel", "CEDS_fuel", "agg_sector", "CEDS_sector" )

    agg_cols <- dplyr::select_if( df, funs( !na.rm | !all( is.na( . ) ) ) )
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
