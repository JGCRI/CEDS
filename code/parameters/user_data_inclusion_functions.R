#------------------------------------------------------------------------------
# Program Name: user_data_inclusion_functions.R
# Author: Ben Goldstein
# Date Last Updated: 14 June 2017
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
                                     agg_level ) {
  
# The any() function returns NA if no elements are true but some are NA. We do a
# quick overwrite if this is the case so that we can use
# any(override_normalization) as a logical element without fear.
    if ( is.na( any( override_normalization ) ) ) {
        override_normalization <- F
    }

# For each aggregate level, column names are specified that will guide 
# normalization and processing. Because of this flexible architecture, specific 
# aggregate levels can be easily adjusted or added without changing the design 
# of the function. However, there may only be one column in cols_given that
# isn't present in normalizeTo; you can only ever normalize by a single "level"
# of aggregation. This is a law of the function, not of the process, and may be
# worth changing down the line.
    if ( agg_level == 1 ) {
        override_normalization <- T
        normalizeTo <- c( "iso" ) # This will not be used at level 1 
                                  # but is necessary for creating the identifier_col
        cols_given <- c( "agg_fuel", "iso" )
    } else if ( agg_level == 2 ) {
        normalizeTo <- c( "agg_fuel", "iso" )
        cols_given <- c( "agg_fuel", "CEDS_fuel", "iso" )
    } else if ( agg_level == 3 ) {
        normalizeTo <- c( "agg_fuel", "agg_sector", "iso" )
        cols_given <- c( "agg_fuel", "CEDS_fuel", "agg_sector", "iso" )
    } else if ( agg_level == 4 ) {
        normalizeTo <- c( "agg_fuel", "agg_sector", "CEDS_sector", "iso" )
        cols_given <- c( "agg_fuel", "CEDS_fuel", "agg_sector", 
                         "CEDS_sector", "iso" )
    } else if ( agg_level == 5 ) {
        normalizeTo <- c( "agg_fuel", "agg_sector", "iso" )
        cols_given <- c( "agg_fuel", "agg_sector", "CEDS_sector", "iso" )
    } else if ( agg_level == 6 ) {
        normalizeTo <- c( "agg_fuel", "iso" )
        cols_given <- c( "agg_fuel", "agg_sector", "iso" )
    } else {
        stop( paste0( "agg_level ", agg_level, " not supported" ) )
    }
  
# The "identifier column" is that column that will be unique among all
# rows once aggregated to this level.
    identifier_col <- cols_given[ which( cols_given %!in% normalizeTo)]
    
# Aggregate the pre-update data to match the user data level of aggregation
    activity_agg_to_level <- ddply( data_to_use, cols_given, 
                                     function(x) colSums( x[ Xyears ] ) )
    
# Add the user data (in a new copy of the agg dataframe)
    act_agg_changed <- activity_agg_to_level
    act_agg_changed[ which( act_agg_changed[[identifier_col]] %in%
                                   user_dataframe_subset[[identifier_col]] ), Xyears ] <-
                user_dataframe_subset[ , Xyears ]
    
# Separate out those original columns which are not directly changed
    data_to_correct <- activity_agg_to_level[ which( activity_agg_to_level[[identifier_col]] %!in%
                                                      user_dataframe_subset[[identifier_col]] ),
                                                      c( cols_given, Xyears ) ]
# If there are no rows that weren't directly changed, then we've specified a
# whole group (data for an entire aggregate group) and can't normalize
    whole_group <- ( nrow( data_to_correct ) == 0 )
    
# Initialize warning info
    negatives <- F
    all_zero_years <- NA
# The block of code that deals with normalization (the process of modifying rows
# that weren't directly specified in order to retain aggregate information) will
# only be called if a) a whole group was not specified and b) no manual override
# of normalization was called.
    if ( !whole_group && !any( override_normalization ) ) {
      
        pct_of_agg_group <- data_to_correct
        
    # Create a vector containing unaltered sums of activity in the group for each year
        if ( length( Xyears ) > 1 ) {
            year_totals <- colSums( activity_agg_to_level[ , Xyears ] )
            year_totals_user_rows <- colSums( activity_agg_to_level[ which( activity_agg_to_level[[ identifier_col ]] %in%
                                                         user_dataframe_subset[[ identifier_col ]] ), Xyears ] )
        } else {
        # due to a flaw in colSums, instances where there is only one column are separated
            year_totals <- sum( activity_agg_to_level[ , Xyears ] )
            year_totals_user_rows <- sum( activity_agg_to_level[ which( activity_agg_to_level[[identifier_col]] %in%
                                                         user_dataframe_subset[[identifier_col]] ), Xyears ] )
        }
        
    # Find the sum of only rows which need to be normalized (were not specified)
        year_totals_non_user_rows <- pct_of_agg_group
        year_totals_non_user_rows[ 1, Xyears ] <- year_totals - year_totals_user_rows
        
    # This data is stored in a dataframe of multiple identical rows for the
    # purposes of mathematical operation
        if ( nrow( year_totals_non_user_rows ) > 1 ) {
            year_totals_non_user_rows[ 2:nrow(year_totals_non_user_rows), Xyears ] <- year_totals_non_user_rows[ 1, Xyears ]
        } 
        
    # Divide each unedited row by the sum of these rows. This provides a
    # fraction representing how much of a difference each row will need to
    # absorb; it says what percent of the unedited aggregate group it is
    # responsible for
        pct_of_agg_group[ , Xyears ] <- data.matrix( pct_of_agg_group[ , Xyears ] ) /
                                        data.matrix( year_totals_non_user_rows[ , Xyears ] ) 
        pct_of_agg_group[ is.nan.df( pct_of_agg_group ) ] <- 0
        
    # IF all of the data in a year (that wasn't user-specified) is 0, this is
    # like having a "whole group"--we do not need to (cannot) normalize in this
    # case. We will not compare these rows in the warnings check.
        all_zero_years <- colnames( pct_of_agg_group[, Xyears] ) [ which( colSums(pct_of_agg_group[ , Xyears] == 0) == nrow(pct_of_agg_group) ) ]
        
    # Calculate the sum of the data post-inclusion and pre-normalization
        if ( length( Xyears ) > 1 ) {
            new_year_totals <- colSums( act_agg_changed[ , Xyears ] )
        } else {
            new_year_totals <- sum( act_agg_changed[ , Xyears ] )
        }
        
    # Likely, adding new data will change the sum of the aggregate group; the
    # point of normalization is to eliminate this change. Annual diffs stores
    # the yearly difference between pre- and post-inclusion data
        annual_diffs <- pct_of_agg_group
        annual_diffs[ 1, Xyears ] <- year_totals - new_year_totals
        if ( nrow( year_totals_non_user_rows ) > 1 ) {
            annual_diffs[ 2:nrow(annual_diffs), Xyears ] <- 
                  as.numeric( annual_diffs[ 1, Xyears ] ) 
        }
    # Each row linearly absorbs a percentage of the total difference
        sums_to_add <- pct_of_agg_group
        sums_to_add[ , Xyears ] <- data.matrix( pct_of_agg_group[ , Xyears ] ) *
                                   data.matrix( annual_diffs[ , Xyears ] )
        
    # "Corrected data" sums are added
        corrected_data <- data_to_correct
        corrected_data[ , Xyears ] <- data.matrix( data_to_correct[ , Xyears ] ) + 
                                          data.matrix( sums_to_add[ , Xyears ] )
        
    # If the total provided for only the user-defined columns exceeds the 
    # original total for the entire aggregate group, data will be normalized to 
    # negative emissions. We need to force this to zero but make a note of it
    # (the negatives boolean will generate a warning later on)
        if ( any( corrected_data < 0 ) ) {
            corrected_data[ ( corrected_data < 0 ) ] <- 0
            negatives <- T
        }
        
    # This dataframe now contains the user-defined data and normalized versions
    # of the undefined rows
        act_agg_changed[ which( act_agg_changed[[ identifier_col ]] %!in%
                                  user_dataframe_subset[[ identifier_col ]]), ] <- corrected_data 
    }
    
    disagg_data_changed <- data_to_use
    
# Our changed data needs to be on the lowest level of aggregation. We'll do this
# by calculating the factor by which each row at the aggregate level was
# changed, then apply this to any disaggregate row that's a part of the
# corresponding aggregate value.
    disaggregation_factors <- act_agg_changed
    disaggregation_factors[ , Xyears ] <- act_agg_changed[ , Xyears ] / 
                              activity_agg_to_level[ , Xyears ]
    
    disaggregation_factors[ is.nan.df( disaggregation_factors ) ] <- 0
    disaggregation_factors[ act_agg_changed != 0 & activity_agg_to_level == 0 ] <- 1
    
### Okay, so here's the problem. Unless we have pct breakdowns for data, if
### there are 0 emissions in a category that now has emissions, they're going to
### get replaced. This is an issue. Manual override doesn't fix because there's
### no way to inform the breakdowns. We're going to have to come up with a whole
### thing to handle this case, but for now, 0 anywhere means 0.
    
# Arrange disaggregation factors to the correct rows
    disaggregation_factors <- left_join( disagg_data_changed[ , c( "iso", 
                                                                   "CEDS_fuel", 
                                                                   "CEDS_sector", 
                                                                   "agg_fuel", 
                                                                   "agg_sector" ) ], 
                                         disaggregation_factors,
                                         by = cols_given
                                      )
    
# Apply disaggregation factors
    disagg_data_changed[ , Xyears ] <- disagg_data_changed[ , Xyears ] *
                                        disaggregation_factors[ , Xyears ]
    
# Call the generateWarnings function to diagnose how well we did retaining column sums
    if ( agg_level != 1 ) {
        warning_diagnostics <- generateWarnings( Xyears, disagg_data_changed,
                                                 data_to_use, negatives, whole_group,
                                                 override_normalization, all_zero_years )
    } else {
        warning_diagnostics <- NA
    }


    all_activity_data[ which( all_activity_data$iso %in% disagg_data_changed$iso &
                                    all_activity_data$CEDS_fuel %in% disagg_data_changed$CEDS_fuel &
                                    all_activity_data$CEDS_sector %in% disagg_data_changed$CEDS_sector ), Xyears ] <-
          disagg_data_changed[, Xyears]
    
    activity_environment$all_activity_data <- all_activity_data
    
    rows_changed <- sum( apply( disagg_data_changed != data_to_use, 1, any ) )
    diagnostics <- data.frame( rows_changed, warning_diagnostics )
    
}

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
                               override_normalization, all_zero_years ) {
  
# Exclude those years for which there is no non-user-specified data from generating warnings
    years_to_compare <- Xyears[ which( Xyears %!in% all_zero_years ) ]
    
    if ( length( years_to_compare ) == 0 ) {
       return( NA )
    }
    
    warning_diag <- NA
    if ( !any( override_normalization ) && !whole_group ) {
        if ( length( years_to_compare ) > 1 ) {
            if ( any( round( colSums( disagg_data_changed[ , years_to_compare ] ), 3 ) != 
                      round( colSums( data_to_use[ , years_to_compare ] ), 3 ) ) ) {
              
              cols_not_retained <- sum( round( colSums( disagg_data_changed[ , years_to_compare ] ), 3 ) != 
                                        round( colSums( data_to_use[ , years_to_compare ] ), 3 ) )
              warning_diag <- paste0( cols_not_retained, " column sums were not retained.")
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
    } else if ( any( override_normalization ) ) {
        warning_diag <- "Manual override of normalization"
    } else if ( whole_group ) {
        warning_diag <- "Whole group provided; aggregate sums overwritten"
    } 
    return( warning_diag )
}










