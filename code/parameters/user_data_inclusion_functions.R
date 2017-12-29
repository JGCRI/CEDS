#------------------------------------------------------------------------------
# Program Name: user_data_inclusion_functions.R
# Author: Ben Goldstein
# Date Last Updated: 11 July 2017
# Program Purpose:
# Input Files:
# Output Files: None
# Functions Defined:
# Notes:
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

normalizeAndIncludeData <- function( Xyears, data_to_use, user_dataframe_subset,
                                     all_activity_data, override_normalization,
                                     agg_level, filename, specified_breakdowns ) {

    override_normalization <- any( override_normalization, na.rm = T )

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

    # Separate out those original columns which are not directly changed
    data_to_correct <- activity_agg_to_level[ !rows_to_replace, c( cols_given, Xyears ) ]

    # If all the rows were directly changed then we've specified data for an
    # entire aggregate group and therefore can't normalize
    whole_group <- all( rows_to_replace )

    # Initialize warning info
    negatives <- F
    all_zero_years <- NA
    need_user_spec <- F

    # Normalization, the process of modifying rows that weren't directly
    # specified in order to retain aggregate information, should only happen if
    # both:
    #   a) a whole group was not specified and
    #   b) no manual override of normalization was called.
    if ( !whole_group & !override_normalization ) {

        # All calculations are just done with the year columns. Take those
        # columns and replace the values with their proportion of the total
        # value for the whole year. This provides a fraction representing how
        # much of a difference each row ill need to absorb; it says what percent
        # of the unedited aggregate group it is responsible for.
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
    agg_group_totals_unchanged <- left_join( disagg_data_changed[ , join_cols ],
                                             activity_agg_to_level, by = cols_given )
    agg_group_totals_changed   <- left_join( disagg_data_changed[ , join_cols ],
                                             act_agg_changed, by = cols_given )

# Calculate the percentage of the aggregate group that each row used to make up
    disagg_pct_breakdown <- data_to_use
    disagg_pct_breakdown[ , Xyears ] <- data_to_use[ , Xyears ] /
                                        agg_group_totals_unchanged[ , Xyears ]

    disagg_pct_breakdown[ is.nan.df(disagg_pct_breakdown) ] <- 0

# This section of the function addresses an issue where user-specified emissions
# would fail to replace zero-emissions cells, because they would be
# disaggregated using factors of zero. It creates percent breakdowns one of a
# few ways; they will be explained as they appear (all contained in the
# following if statements).
    if ( any( activity_agg_to_level[ , Xyears ] == 0 &
              act_agg_changed[ , Xyears ] != 0 ) ) {

        # zero_cells holds a boolean data frame used to identify cells which
        # will have this issue (changed value is > 0 but original value is 0)
        orig_zeros <- activity_agg_to_level[ , Xyears, drop = F ] == 0
        chgd_zeros <- act_agg_changed[ , Xyears, drop = F ] == 0
        zero_cells <- cbind( act_agg_changed[ , cols_given ], orig_zeros & !chgd_zeros )

        # Create two dataframes: all_zeros (all cells cause the issue) and
        # some_zeros (those rows that have some, but not all, cells causing the
        # issue)
        any_zeros <- zero_cells[ apply( zero_cells[ , Xyears ], 1, any ), ]
        all_zeros <- zero_cells[ apply( zero_cells[ , Xyears ], 1, all ), ]
        some_zeros <- dplyr::setdiff( any_zeros, all_zeros )

        # if ( length( Xyears ) > 1 ) {
        #     zero_cells <- act_agg_changed[ rowSums( act_agg_changed[, Xyears] ) > 0, ]
        #     zero_cells[ , Xyears ] <- activity_agg_to_level[ rowSums( act_agg_changed[, Xyears] ) > 0, Xyears ] == 0
        #
        #     any_zeros <- zero_cells[ apply( zero_cells[ , Xyears ], 1, any ), ]
        #     all_zeros <- zero_cells[ apply( zero_cells[ , Xyears ], 1, all ), ]
        # } else {
        #     zero_cells <- act_agg_changed[ which( act_agg_changed[, Xyears] > 0 ), ]
        #     zero_cells[ , Xyears ] <- activity_agg_to_level[ which( act_agg_changed[, Xyears] > 0 ),
        #                                                  Xyears ] == 0
        #     all_zeros <- zero_cells[ which( zero_cells[, Xyears] ), ]
        #     any_zeros <- all_zeros
        # }
        #
        # not_all_zeros <- rbind( all_zeros, any_zeros )
        # not_all_zeros <- not_all_zeros[ !duplicated( not_all_zeros ) &
        #                                 seq( nrow( not_all_zeros ) ) > nrow( all_zeros ), ]


    # For rows that have some but not all zero cells (some 0 rows available): we
    # can use interpolate_NA() to fill in percent breakdowns linearly, retaining
    # a total of 100 %
        if ( nrow( some_zeros ) > 0 ) {
        # Each row in some_zeros represents an aggregate row that needs to be
        # disaggregated; since we need to deal with the totals of each we will
        # iterate through them
            for ( row_num in 1:nrow( some_zeros ) ) {
            # Retrieve the row in question
                operating_row <- some_zeros[ row_num, ]
            # retrieve the percent breakdowns corresponding to this aggregate row
                breakdowns_to_correct <- disagg_pct_breakdown
                for ( col in cols_given ) {
                    breakdowns_to_correct <- breakdowns_to_correct[ breakdowns_to_correct[ , col ]
                                                       == operating_row[ , col ], ]
                }

            # All columns containing all zeros will be replaced with NA, so we
            # can use interpolate_NAs() to replace them
                breakdowns_to_correct[ , 5 + which( colSums( breakdowns_to_correct[ , Xyears] ) == 0 ) ] <- NA
                new_percent_breakdowns <- breakdowns_to_correct

            # If the first or last year is NA this will make complete
            # interpolation impossible. We will therefore come up with a default
            # if it is NA; we will use the nearest cell with values.
                if ( all( is.na( breakdowns_to_correct[ , Xyears[1] ] ) ) ) {
                    first_non_NA <- min( which( !is.na( breakdowns_to_correct[ 1, Xyears ] ))) + 5
                    breakdowns_to_correct[ , Xyears[1] ] <- breakdowns_to_correct[, first_non_NA]
                }
                if ( all( is.na( breakdowns_to_correct[ , Xyears[ length(Xyears) ] ] ) ) ) {
                    first_non_NA <- max( which( !is.na( breakdowns_to_correct[ 1, Xyears ] ))) + 5
                    breakdowns_to_correct[ , Xyears[ length(Xyears) ] ] <-
                            breakdowns_to_correct[, first_non_NA]
                }

            # Interpolate and assign the values to new_percent_breakdowns
                new_percent_breakdowns[ , Xyears ] <- interpolate_NAs( breakdowns_to_correct[ , Xyears ] )

            }

        # Replace into the disagg_pct_breakdown master dataframe
            disagg_pct_breakdown[ which( disagg_pct_breakdown$CEDS_fuel %in% new_percent_breakdowns$CEDS_fuel &
                                         disagg_pct_breakdown$CEDS_sector %in% new_percent_breakdowns$CEDS_sector ), ] <-
                    new_percent_breakdowns
        }

    # For rows that have no non-zero years available: we will use global percent
    # breakdowns for this row as a default.
        if ( nrow( all_zeros ) > 0 ) {
            # Isolate the rows in all_zeros (row names should have persisted)
            breakdowns_to_correct <- disagg_pct_breakdown[ row.names( all_zeros ) ]
            # breakdowns_to_correct <- disagg_pct_breakdown
            # for ( col in cols_given ) {
            #     breakdowns_to_correct <- breakdowns_to_correct[ breakdowns_to_correct[, col]
            #                                                     %in% all_zeros[, col], ]
            # }

        # Use the sumAllActivityByFuelSector function to generate global totals
        # for each fuel x sector, ignoring iso
            for ( i in 1:nrow( breakdowns_to_correct ) ) {
                breakdowns_to_correct[ i, Xyears ] <-
                         sumAllActivityByFuelSector( breakdowns_to_correct[ i, ] )
            }

            totals_by_agg_group <- ddply( breakdowns_to_correct, cols_given,
                                          function(x) colSums( x[ Xyears ] ) )

        # If there are some rows that STILL have zeros, we'll divide them
        # evenly among the breakdown categories and generate a warning that
        # the user needs to specify breakdowns
            if ( any( totals_by_agg_group[ , Xyears ] == 0 ) ) {

                # Extract the rows to address with this fix
                zrows <- rowSums( totals_by_agg_group[ , Xyears, drop = F ] ) == 0
                problem_rows <- totals_by_agg_group[ zrows, ]
                # if ( length( Xyears ) > 1 ) {
                #     problem_rows <- totals_by_agg_group[ which( rowSums(
                #                  totals_by_agg_group[ , Xyears ] ) == 0 ), ]
                # } else {
                #     problem_rows <- totals_by_agg_group[ which(
                #                  totals_by_agg_group[ , Xyears ] == 0 ), ]
                # }

            # Extract the corresponding disaggregated rows
                problem_bd <- disagg_pct_breakdown
                for ( col in cols_given ) {
                    problem_bd <- problem_bd[ problem_bd[, col]
                                              %in% totals_by_agg_group[, col], ]
                }
            # Assign the disaggregated rows a uniform "1" so division is
            # performed evenly
                problem_bd[ , Xyears ] <- 1

            # Replace new values into the disagg pct breakdown df
                disagg_pct_breakdown[ which( disagg_pct_breakdown$CEDS_fuel %in% problem_bd$CEDS_fuel &
                                         disagg_pct_breakdown$CEDS_sector %in% problem_bd$CEDS_sector ), ] <-
                    problem_bd
            # Re-calculate totals
                totals_by_agg_group <- ddply( breakdowns_to_correct, cols_given,
                                          function(x) colSums( x[ Xyears ] ) )
            # Make a note of the fact that the user needs to specify their own
            # pct breakdowns (will be passed to warning-generating function)
                need_user_spec <- T
            }

        # Arrange the totals by the breakdown rows they affect
            totals_by_agg_group <- left_join( breakdowns_to_correct[ c( "iso", "CEDS_fuel",
                                                                        "CEDS_sector", "agg_sector",
                                                                        "agg_fuel" ) ],
                                              totals_by_agg_group, by = cols_given )
        # Calculate new_percent_breakdowns by dividing each row by its group's total
            new_percent_breakdowns <- totals_by_agg_group
            new_percent_breakdowns[ , Xyears ] <- breakdowns_to_correct[ , Xyears ] /
                                                  totals_by_agg_group[ , Xyears ]

        # ???
            disagg_pct_breakdown[ which( disagg_pct_breakdown$CEDS_fuel %in% new_percent_breakdowns$CEDS_fuel &
                                         disagg_pct_breakdown$CEDS_sector %in% new_percent_breakdowns$CEDS_sector ), ] <-
                    new_percent_breakdowns
        }
    }


    if ( any( specified_breakdowns ) ) {
        tryCatch({
            user_breakdown <- readData( paste0( filename, "-breakdowns" ),
                                        domain = "EXT_IN",
                                        domain_extension = "user-defined-energy/" )
            user_breakdown <- as.data.frame(user_breakdown)
        }, error = function(e) {
            stop( paste0( "No user specified breakdown found for ", filename ) )
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

    return( list( 'all_data' = all_activity_data, 'diagnostics' = diagnostics ) )
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
generateWarnings <- function ( Xyears, disagg_data_changed,
                               data_to_use, negatives, whole_group,
                               override_normalization, all_zero_years,
                               need_user_spec ) {

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
# A helper function for enforcing percentage breakdowns in all-zero rows.
#    Helps create a global default percentage breakdown for a given aggregation category.
sumAllActivityByFuelSector <- function( guide_row, years = Xyears, data = all_activity_data ) {

    fuel_row <- as.character( guide_row[ which( names( guide_row ) == "CEDS_fuel" ) ] )
    sector_row <- as.character( guide_row[ which( names( guide_row ) == "CEDS_sector" ) ] )

    df_to_sum <- data %>%
                  filter( CEDS_sector %in% sector_row ) %>%
                  filter( CEDS_fuel %in% fuel_row )
    df_to_sum <- df_to_sum[ , c( "iso", "CEDS_sector",
                                 "CEDS_fuel", "agg_sector",
                                 "agg_fuel", years ) ]

    df_to_sum[ is.na( df_to_sum ) ] <- 0

    if ( length( years ) > 1 ) {
        return( colSums( df_to_sum[ , years ] ) )
    } else {
        return( sum( df_to_sum[ , years ] ) )
    }


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
#   activity: the list holding activity and factors
#   instructions: The master instruction list. This will be used to identify
#                 the beginning and end of each row of data, so as to enforce
#                 continuity at dataframe boundaries
#   default_continuity_interval: an optional value storing how many years
#                 should be made continuous at each edge, if possible.
# Returns: the activity, now holding a continuity_factors dataframe

initContinuityFactors <- function( activity, instructions, yearsAllowed,
                                   default_continuity_interval = 7 ) {

    activity$continuity_factors <- activity$all_activity_data
    activity$continuity_factors[ , yearsAllowed ] <- 1

    for ( row_num in 1:nrow( instructions ) ) {
    # Select the row for initializating continuity
        this.row <- instructions[ row_num, ]
        start_year <- this.row$start_year
        end_year <- this.row$end_year
        continuity_interval <- default_continuity_interval
    # Determine the years of the data
        data_years <- paste0( "X", start_year:end_year )
        data_years <- data_years[ data_years %in% names( activity$all_activity_data ) ]
    # If the data year length is less than twice the continuity interval, the interval will need to be reduced.
        if ( length( data_years ) < continuity_interval * 2 ) {
            continuity_interval <- floor( length( data_years ) / 2 )
        }

    # Determine which columns are present (bypass needing agg_level)
        ok_cols <- c( "iso", "CEDS_fuel", "CEDS_sector", "agg_sector", "agg_fuel" )
        cols_given <- names( instructions )[ !is.na( this.row ) & names( instructions ) %in% ok_cols ]

    # Extract the subset of disaggregated rows corresponding to the data
        rows_to_adjust <- activity$continuity_factors
        for ( col in cols_given ) {
            rows_to_adjust <- rows_to_adjust[ rows_to_adjust[, col]
                                      %in% this.row[, col], ]
        }

    # The continuity step is by how much each value will increase each year
    # (ends at 1)
        continuity_step <- 1 / continuity_interval
        continuity_vals <- 1:continuity_interval * continuity_step

        # If continuity enforcement is required at the beginning of this dataset:
        if ( this.row$start_continuity && start_year > historical_pre_extension_year ) {
            interval_end <- start_year + continuity_interval - 1
            year_range <- paste0( "X", start_year:interval_end )
            rows_to_adjust[ , year_range ] <- rep( continuity_vals, each = NROW( rows_to_adjust ) )

        }

        # If continuity enforcement is required at the end of this dataset:
        if ( this.row$end_continuity && end_year < historical_end_extension_year ) {
            interval_end <- end_year - continuity_interval + 1
            year_range <- paste0( "X", end_year:interval_end )
            rows_to_adjust[ , year_range ] <- rep( continuity_vals, each = NROW( rows_to_adjust ) )
        }


    # Incorporate the new adjusted rows into the created dataframe
        activity$continuity_factors[ activity$continuity_factors$iso %in% rows_to_adjust$iso &
                                     activity$continuity_factors$CEDS_sector %in% rows_to_adjust$CEDS_sector &
                                     activity$continuity_factors$CEDS_fuel %in% rows_to_adjust$CEDS_fuel, ] <-
                        rows_to_adjust

    }

    return( activity )
}

