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
#    whole_group: a logical option specifying whether an entire aggregate group is being processed
#    override_normalization: a manual option for disabling normalization 
#    agg_level: an integer indicating the aggregate level of the data given

normalizeAndIncludeData <- function( Xyears, data_to_use, user_dataframe_subset, 
                                     all_activity_data, whole_group, override_normalization,
                                     agg_level ) {
  
    if ( is.na( any( override_normalization ) ) ) {
        override_normalization <- F
    }

    if ( agg_level == 1 ) {
        override_normalization <- T
        normalizeTo <- c( "iso" ) # This will not be used but is necessary for creating the identifier_col
        cols_given <- c( "agg_fuel", "iso" )
    } else if ( agg_level == 2 ) {
        normalizeTo <- c( "agg_fuel", "iso" )
        cols_given <- c( "agg_fuel", "CEDS_fuel", "iso" )
    } else if ( agg_level == 3 ) {
        normalizeTo <- c( "agg_fuel", "agg_sector", "iso" )
        cols_given <- c( "agg_fuel", "CEDS_fuel", "agg_sector", "iso" )
    } else if ( agg_level == 4 ) {
        normalizeTo <- c( "agg_fuel", "agg_sector", "CEDS_sector", "iso" )
        cols_given <- c( "agg_fuel", "CEDS_fuel", "agg_sector", "CEDS_sector", "iso" )
    } else if ( agg_level == 5 ) {
        normalizeTo <- c( "agg_fuel", "agg_sector", "iso" )
        cols_given <- c( "agg_fuel", "agg_sector", "CEDS_sector", "iso" )
    } else if ( agg_level == 6 ) {
        normalizeTo <- c( "agg_fuel", "iso" )
        cols_given <- c( "agg_fuel", "agg_sector", "iso" )
    } else {
        stop( paste0( "agg_level ", agg_level, " not supported" ) )
    }
  
    identifier_col <- cols_given[ which( cols_given %!in% normalizeTo)]
    
    
    activity_agg_to_level <- ddply( data_to_use, cols_given, 
                                     function(x) colSums( x[ Xyears ] ) )
    
    act_agg_changed <- activity_agg_to_level
    
    act_agg_changed[ which( act_agg_changed[[identifier_col]] %in%
                                   user_dataframe_subset[[identifier_col]] ), Xyears ] <-
                user_dataframe_subset[ , Xyears ]
    
    data_to_correct <- activity_agg_to_level[ which( activity_agg_to_level[[identifier_col]] %!in%
                                                      user_dataframe_subset[[identifier_col]] ),
                                                      c( cols_given, Xyears ) ]
    
    is_whole_group <- ( nrow( data_to_correct ) == 0 )
    negatives <- F

    if ( !whole_group && !any( override_normalization ) ) {
      
        pct_of_agg_group <- data_to_correct
        
        if ( length( Xyears ) > 1 ) {
            year_totals <- colSums( activity_agg_to_level[ , Xyears ] )
            year_totals_user_rows <- colSums( activity_agg_to_level[ which( activity_agg_to_level[[identifier_col]] %in%
                                                         user_dataframe_subset[[identifier_col]] ), Xyears ] )
        } else {
            year_totals <- sum( activity_agg_to_level[ , Xyears ] )
            year_totals_user_rows <- sum( activity_agg_to_level[ which( activity_agg_to_level[[identifier_col]] %in%
                                                         user_dataframe_subset[[identifier_col]] ), Xyears ] )
        }
        
        year_totals_non_user_rows <- year_totals
        
        year_totals_non_user_rows <- pct_of_agg_group
        year_totals_non_user_rows[ 1, Xyears ] <- year_totals - year_totals_user_rows
        
        if ( nrow( year_totals_non_user_rows ) > 1 ) {
            year_totals_non_user_rows[ 2:nrow(year_totals_non_user_rows), Xyears ] <- year_totals_non_user_rows[ 1, Xyears ]
        } 
        
        pct_of_agg_group[ , Xyears ] <- data.matrix( pct_of_agg_group[ , Xyears ] ) /
                                        data.matrix( year_totals_non_user_rows[ , Xyears ] ) 
        pct_of_agg_group[ is.nan.df( pct_of_agg_group ) ] <- 0
        
        if ( length( Xyears ) > 1 ) {
            new_year_totals <- colSums( act_agg_changed[ , Xyears ] )
        } else {
            new_year_totals <- sum( act_agg_changed[ , Xyears ] )
        }
        
        annual_diffs <- pct_of_agg_group
        annual_diffs[ 1, Xyears ] <- year_totals - new_year_totals
        if ( nrow( year_totals_non_user_rows ) > 1 ) {
            annual_diffs[ 2:nrow(year_totals_non_user_rows), Xyears ] <- 
                  rep( annual_diffs[ 1, Xyears ], each=nrow(year_totals_non_user_rows) - 1 )
        }
        sums_to_add <- pct_of_agg_group
        sums_to_add[ , Xyears ] <- data.matrix( pct_of_agg_group[ , Xyears ] ) *
                                   data.matrix( annual_diffs[ , Xyears ] )
        
        corrected_data <- data_to_correct
        corrected_data[ , Xyears ] <- data.matrix( data_to_correct[ , Xyears ] ) + 
                                          data.matrix( sums_to_add[ , Xyears ] )
        
        if ( any( corrected_data < 0 ) ) {
            corrected_data[ ( corrected_data < 0 ) ] <- 0
            negatives <- T
        }
        
        act_agg_changed[ which( act_agg_changed[[ identifier_col ]] %!in%
                                  user_dataframe_subset[[ identifier_col ]]), ] <- corrected_data 
    }
    
    disagg_data_changed <- data_to_use
    
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
    
    disaggregation_factors <- left_join( disagg_data_changed[ , c( "iso", 
                                                                   "CEDS_fuel", 
                                                                   "CEDS_sector", 
                                                                   "agg_fuel", 
                                                                   "agg_sector" ) ], 
                                         disaggregation_factors,
                                         by = cols_given
                                      )
    disagg_data_changed[ , Xyears ] <- disagg_data_changed[ , Xyears ] *
                                        disaggregation_factors[ , Xyears ]
    
    if ( any( override_normalization ) ) {
        warning_diagnostics <- "Manual override of normalization"
    } else {
        warning_diagnostics <- generateWarnings( Xyears, disagg_data_changed,
                                                 data_to_use, negatives,
                                                 override_normalization )
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
                               data_to_use, negatives, 
                               override_normalization ) {
    warning_diag <- NA
    if ( !any( override_normalization ) ) {
        if ( length( Xyears ) > 1 ) {
            if ( any( round( colSums( disagg_data_changed[ , Xyears ] ), 3 ) != 
                      round( colSums( data_to_use[ , Xyears ] ), 3 ) ) ) {

              if ( negatives ) {
                  warning_diag <- "A specified total exceeded an aggregate group total."
              } else {
                  warning_diag <- "Column sums were not retained."
              }
              
            }
        } else {
            if ( any( round( sum( disagg_data_changed[ , Xyears ] ), 3 ) 
                      != round( sum( data_to_use[ , Xyears ] ), 3 ) ) ) {
              if ( negatives ) {
                  warning_diag <- "A specified total exceeded an aggregate group total."
              } else {
                  warning_diag <- "Column sums were not retained."
              }
           }
        }
    }
    return( warning_diag )
}










