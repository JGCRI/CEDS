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


# normalizeAndIncludeDataL6
# Brief: One of four main-loop functions for processing user extension data. This
#        handles agg level 6, for data that specifies agg fuel, agg sector, and iso.
#        It disaggregates to CEDS fuel x CEDS sector and normalizes to agg fuel only.
# params:
#    Xyears: the year range of data processing
#    data_to_use: a dataframe of unchanged activity data extracted from all_activity
#    user_dataframe_subset: a subsetted dataframe of user-specified data
#    all_activity_data: the dataframe holding all activity
#    whole_group: a logical option specifying whether an entire aggregate group is being processed
#    override_normalization: a manual option for disabling normalization      
    normalizeAndIncludeDataL6 <- function( Xyears, data_to_use, user_dataframe_subset, 
                                           all_activity_data, whole_group, override_normalization ) {
      
        if ( is.na( any( override_normalization ) ) ) {
            override_normalization <- F
        }

        activity_agg_to_l6 <- ddply( data_to_use, 
                                     c( "iso", "agg_sector", "agg_fuel" ), 
                                     function(x) colSums( x[ Xyears ] ) )
        
        act_agg_to_l6_changed <- activity_agg_to_l6
        act_agg_to_l6_changed[ which( act_agg_to_l6_changed$agg_sector %in% 
                                      user_dataframe_subset$agg_sector ), Xyears ] <- 
                    user_dataframe_subset[ , Xyears ]
      
    # Create a total to calculate percents of differences that the non-user-edited sectors need to absorb
        year_totals <- activity_agg_to_l6[ 1, ]
        pct_of_agg_group <- activity_agg_to_l6[ which( activity_agg_to_l6$agg_sector %!in% 
                                                         user_dataframe_subset$agg_sector ), 
                                                c( colnames( year_totals[ which( !isXYear( 
                                                  colnames( year_totals ) ) ) ] ), Xyears ) ]
        data_to_correct <- activity_agg_to_l6[ which( activity_agg_to_l6$agg_sector %!in% user_dataframe_subset$agg_sector )
                                               , c( colnames( year_totals[ which( !isXYear( 
                                                 colnames( year_totals ) ) ) ] ), Xyears ) ]
        
        whole_group <- ( nrow(pct_of_agg_group) == 0 )
        
        if ( !whole_group && !any( override_normalization ) ) {
             if (nrow( activity_agg_to_l6) > 1 ) {
                year_totals[ 1, Xyears ] <- colSums( activity_agg_to_l6[ , Xyears ] ) 
             } else {
                year_totals[ 1, Xyears ] <- sum( activity_agg_to_l6[ , Xyears ] )
             }
          
             year_totals_non_user_data <- year_totals[ , c( colnames( year_totals[ which( !isXYear( 
                                                           colnames( year_totals ) ) ) ] ), Xyears ) ]
             year_totals_non_user_data[ , Xyears ] <- year_totals[ , Xyears ] - 
                                                      colSums( activity_agg_to_l6[ which( activity_agg_to_l6$agg_sector %in% 
                                                                                 user_dataframe_subset$agg_sector ), 
                                                                          Xyears ] )
        
        # Calculate percents that each non-edited sector needs to absorb
            year_totals_non_user_data[ 2:( nrow( pct_of_agg_group ) ), ] <- year_totals_non_user_data[ 1, ]
            
            pct_of_agg_group[ , Xyears ] <- data.matrix( pct_of_agg_group[ , Xyears ] ) / 
                                            data.matrix( year_totals_non_user_data[ , Xyears ] )
            pct_of_agg_group[ is.nan.df( pct_of_agg_group ) ] <- 0
            
            new_year_totals <- act_agg_to_l6_changed[ 1, ]
            new_year_totals[ 1, Xyears ] <- colSums( act_agg_to_l6_changed[ , Xyears ] )
            
            annual_diffs <- year_totals[ 1, Xyears ] - new_year_totals[ , Xyears ]
            annual_diffs[ 2:( nrow( pct_of_agg_group ) ), ] <- annual_diffs[ 1, ]
            
            sums_to_add <- pct_of_agg_group
            sums_to_add[ , Xyears ] <- data.matrix( pct_of_agg_group[ , Xyears ] ) * 
                                       data.matrix( annual_diffs[ , Xyears ] )
            
            corrected_data <- data_to_correct
            corrected_data[ , Xyears ] <- data.matrix( data_to_correct[ , Xyears ] ) + 
                                          data.matrix( sums_to_add[ , Xyears ] )
            
        # Add corrected data, resulting in a dataframe with the new agg sums
            act_agg_to_l6_changed[ which( act_agg_to_l6_changed$agg_sector %!in% 
                                            user_dataframe_subset$agg_sector), ] <-
                            corrected_data
        }
      
    # Now we need to apply the changes above linearly to the disaggregate cells.
        disagg_data_changed <- data_to_use
        
        disaggregation_factors <- act_agg_to_l6_changed
        disaggregation_factors[ , Xyears ] <- act_agg_to_l6_changed[ , Xyears ] / 
                                              activity_agg_to_l6[ , Xyears ]
        
        disaggregation_factors <- left_join( disagg_data_changed[ , c( "iso", "CEDS_fuel", "CEDS_sector", "agg_fuel", "agg_sector" ) ], 
                                             disaggregation_factors, 
                                             by = c( "iso", "agg_sector" ) )
        
        disagg_data_changed[ , Xyears ] <- disagg_data_changed[ , Xyears ] * 
                                           disaggregation_factors[ , Xyears ]
        
    # Replace any values that were 0s in the old dataset with 0s
        disagg_data_changed[ is.nan.df( disagg_data_changed ) & data_to_use == 0 ] <- 0
        
        warning_diag <- NA
        if ( !any( override_normalization ) ) {
            if ( length( Xyears ) > 1 ) {
                if ( any( round( colSums( disagg_data_changed[ , Xyears ] ), 3 ) != 
                          round( colSums( data_to_use[ , Xyears ] ), 3 ) ) ) {
                    warning( "Column sums were not retained \n[this will eventually be an error once we fix the situation with 0s]" )
                    warning_diag <- "Col sums not retained"
                }
            } else {
                if ( any( round( sum( disagg_data_changed[ , Xyears ] ), 3 ) != 
                          round( sum( data_to_use[ , Xyears ] ), 3 ) ) ) {
                    warning( "Column sums were not retained \n[this will eventually be an error once we fix the situation with 0s]" )
                    warning_diag <- "Col sums not retained"
                }             
            }
        }
        
        all_activity_data[ which( all_activity_data$iso %in% disagg_data_changed$iso &
                                    all_activity_data$agg_fuel %in% disagg_data_changed$agg_fuel &
                                    all_activity_data$agg_sector %in% disagg_data_changed$agg_sector ), Xyears ] <-
          disagg_data_changed[, Xyears]  ### We will maybe not re-add this data into the main dataframe... discuss later
        
        activity_environment$all_activity_data <- all_activity_data

    # Count the number of rows that have a changed value
        rows_changed <- sum( apply( disagg_data_changed != data_to_use, 1, any ) ) 
        diagnostics <- data.frame( rows_changed, warning_diag )
        return( diagnostics )
        
    }
 
    
# normalizeAndIncludeDataL5
# Brief: One of four main-loop functions for processing user extension data. This
#        handles agg level 5, for data that specifies agg fuel, CEDS sector, and iso.
#        It disaggregates to CEDS  fuel and normalizes to agg fuel x agg sector.
# params:
#    Xyears: the year range of data processing
#    data_to_use: a dataframe of unchanged activity data extracted from all_activity
#    user_dataframe_subset: a subsetted dataframe of user-specified data
#    all_activity_data: the dataframe holding all activity
#    whole_group: a logical option specifying whether an entire aggregate group is being processed
#    override_normalization: a manual option for disabling normalization      
    normalizeAndIncludeDataL5 <- function( Xyears, data_to_use, user_dataframe_subset, 
                                           all_activity_data, whole_group, override_normalization ) {
      
        if ( is.na( any( override_normalization ) ) ) {
            override_normalization <- F
        }

        activity_agg_to_l5 <- ddply( data_to_use, 
                                     c( "iso", "CEDS_sector", "agg_sector", "agg_fuel" ), 
                                     function(x) colSums( x[ Xyears ] ) )
        
        act_agg_to_l5_changed <- activity_agg_to_l5
        act_agg_to_l5_changed[ which( act_agg_to_l5_changed$CEDS_sector %in% 
                                      user_dataframe_subset$CEDS_sector ), Xyears ] <- 
                    user_dataframe_subset[ , Xyears ]
      
    # Create a total to calculate percents of differences that the non-user-edited sectors need to absorb
        year_totals <- activity_agg_to_l5[ 1, ]
        pct_of_agg_group <- activity_agg_to_l5[ which( activity_agg_to_l5$CEDS_sector %!in% 
                                                         user_dataframe_subset$CEDS_sector ), 
                                                c( colnames( year_totals[ which( !isXYear( 
                                                  colnames( year_totals ) ) ) ] ), Xyears ) ]
        data_to_correct <- activity_agg_to_l5[ which( activity_agg_to_l5$CEDS_sector %!in% user_dataframe_subset$CEDS_sector )
                                               , c( colnames( year_totals[ which( !isXYear( 
                                                 colnames( year_totals ) ) ) ] ), Xyears ) ]
        
        whole_group <- ( nrow(pct_of_agg_group) == 0 )

        if ( !whole_group && !any( override_normalization ) ) {
          
             if ( nrow( activity_agg_to_l5 ) > 1 ) {
                year_totals[ 1, Xyears ] <- colSums( activity_agg_to_l5[ , Xyears ] ) 
             } else {
                year_totals[ 1, Xyears ] <- sum( activity_agg_to_l5[ , Xyears ] )
             }             
             
             year_totals_non_user_data <- year_totals[ , c( colnames( year_totals[ which( !isXYear( 
                                                           colnames( year_totals ) ) ) ] ), Xyears ) ]
             year_totals_non_user_data[ , Xyears ] <- year_totals[ , Xyears ] - 
                                                      colSums( activity_agg_to_l5[ which( activity_agg_to_l5$CEDS_sector %in% 
                                                                                 user_dataframe_subset$CEDS_sector ), 
                                                                          Xyears ] )
        
        # Calculate percents that each non-edited sector needs to absorb
            year_totals_non_user_data[ 2:nrow( pct_of_agg_group ), ] <- year_totals_non_user_data[ 1, ]
            
            pct_of_agg_group[ , Xyears ] <- data.matrix( pct_of_agg_group[ , Xyears ] ) / 
                                            data.matrix( year_totals_non_user_data[ , Xyears ] )
            pct_of_agg_group[ is.nan.df( pct_of_agg_group ) ] <- 0
            
            new_year_totals <- act_agg_to_l5_changed[ 1, ]
            new_year_totals[ 1, Xyears ] <- colSums( act_agg_to_l5_changed[ , Xyears ] )
            
            annual_diffs <- year_totals[ 1, Xyears ] - new_year_totals[ , Xyears ]
            annual_diffs[ 2:( nrow( pct_of_agg_group ) ), ] <- annual_diffs[ 1, ]
            
            sums_to_add <- pct_of_agg_group
            sums_to_add[ , Xyears ] <- data.matrix( pct_of_agg_group[ , Xyears ] ) * 
                                       data.matrix( annual_diffs[ , Xyears ] )
            
            corrected_data <- data_to_correct
            corrected_data[ , Xyears ] <- data.matrix( data_to_correct[ , Xyears ] ) + 
                                          data.matrix( sums_to_add[ , Xyears ] )
            
        # Add corrected data, resulting in a dataframe with the new agg sums
            act_agg_to_l5_changed[ which( act_agg_to_l5_changed$CEDS_sector %!in% 
                                            user_dataframe_subset$CEDS_sector), ] <-
                            corrected_data
        }
      
    # Now we need to apply the changes above linearly to the disaggregate cells.
        disagg_data_changed <- data_to_use
        
        disaggregation_factors <- act_agg_to_l5_changed
        disaggregation_factors[ , Xyears ] <- act_agg_to_l5_changed[ , Xyears ] / 
                                              activity_agg_to_l5[ , Xyears ]
        
        disaggregation_factors <- left_join( disagg_data_changed[ , c( "iso", "CEDS_fuel", "CEDS_sector" ) ], 
                                             disaggregation_factors, 
                                             by = c( "iso", "CEDS_sector" ) )
        
        disagg_data_changed[ , Xyears ] <- disagg_data_changed[ , Xyears ] * 
                                           disaggregation_factors[ , Xyears ]
        
    # Replace any values that were 0s in the old dataset with 0s
        disagg_data_changed[ is.nan.df( disagg_data_changed ) & data_to_use == 0 ] <- 0
        
        warning_diag <- NA
        if ( !any( override_normalization ) ) {
            if ( length( Xyears ) > 1 ) {
                if ( any( round( colSums( disagg_data_changed[ , Xyears ] ), 3 ) != 
                          round( colSums( data_to_use[ , Xyears ] ), 3 ) ) ) {
                    warning( "Column sums were not retained \n[this will eventually be an error once we fix the situation with 0s]" )
                    warning_diag <- "Col sums not retained"
                }
            } else {
                if ( any( round( sum( disagg_data_changed[ , Xyears ] ), 3 ) != 
                          round( sum( data_to_use[ , Xyears ] ), 3 ) ) ) {
                    warning( "Column sums were not retained \n[this will eventually be an error once we fix the situation with 0s]" )
                    warning_diag <- "Col sums not retained"
                }             
            }
        }
        
        all_activity_data[ which( all_activity_data$iso %in% disagg_data_changed$iso &
                                    all_activity_data$agg_fuel %in% disagg_data_changed$agg_fuel &
                                    all_activity_data$CEDS_sector %in% disagg_data_changed$CEDS_sector ), Xyears ] <-
          disagg_data_changed[, Xyears]  ### We will maybe not re-add this data into the main dataframe... discuss later
        
        activity_environment$all_activity_data <- all_activity_data

    # Count the number of rows that have a changed value
        rows_changed <- sum( apply( disagg_data_changed != data_to_use, 1, any ) ) 
        diagnostics <- data.frame( rows_changed, warning_diag )
        return( diagnostics )
        
    }
 
# normalizeAndIncludeDataL4
# Brief: One of four main-loop functions for processing user extension data. This
#        handles agg level 4, the most specific level; there is a normalization
#        component but no disaggregation process.
# params:
#    Xyears: the year range of data processing
#    data_to_use: a dataframe of unchanged activity data extracted from all_activity
#    user_dataframe_subset: a subsetted dataframe of user-specified data
#    all_activity_data: the dataframe holding all activity
#    whole_group: a logical option specifying whether an entire aggregate group is being processed
#    override_normalization: a manual option for disabling normalization
    normalizeAndIncludeDataL4 <- function( Xyears, data_to_use, user_dataframe_subset, 
                                           all_activity_data, whole_group, override_normalization = F ) {
        if ( is.na( any( override_normalization ) ) ) {
            override_normalization <- F
        }
        
        year_totals <- data_to_use[ 1, ]
        pct_of_agg_group <- data_to_use[ which( data_to_use$CEDS_fuel %!in% user_dataframe_subset$CEDS_fuel )
                                 , c( colnames( year_totals[ which( !isXYear( 
                                   colnames(year_totals) ) ) ] ), Xyears ) ]
        
        whole_group <- ( nrow(pct_of_agg_group) == 0 )

        if ( !whole_group && !any( override_normalization ) ) {
        # Calculate percent breakdowns for the category.
        # First, get the sum of the category faor each year.
            if ( length( Xyears ) > 1 ) { 
                year_totals[ 1, Xyears ] <- colSums( data_to_use[ , Xyears ] )
                year_totals_non_user_data <- year_totals[ , c( colnames( year_totals[ which( !isXYear( 
                  colnames( year_totals ) ) ) ] ), Xyears ) ]
                year_totals_non_user_data[ , Xyears ] <- year_totals[ , Xyears ] - 
                  colSums( data_to_use[ which( data_to_use$iso %in% user_dataframe_subset$iso &
                                        data_to_use$CEDS_sector %in% user_dataframe_subset$CEDS_sector &
                                        data_to_use$CEDS_fuel %in% user_dataframe_subset$CEDS_fuel )
                               , Xyears ] )
            } else { 
                year_totals[ 1, Xyears ] <- sum( data_to_use[ , Xyears ] )
                year_totals_non_user_data <- year_totals[ , c( colnames( year_totals[ which( !isXYear( 
                  colnames( year_totals ) ) ) ] ), Xyears ) ]
                year_totals_non_user_data[ , Xyears ] <- year_totals[ , Xyears ] - 
                  sum( data_to_use[ which( data_to_use$iso %in% user_dataframe_subset$iso &
                                        data_to_use$CEDS_sector %in% user_dataframe_subset$CEDS_sector &
                                        data_to_use$CEDS_fuel %in% user_dataframe_subset$CEDS_fuel )
                               , Xyears ] )
            }
    
        # Determine higher-level percentage breakdown. Because this is agg. 
        # level 4, we can be confident that each row (if unique) is of a higher 
        # level of aggregation than the data being added, so we don't have to
        # group; we can just operate on rows.
            
        # Determine what percent of the agg group minus our row each row made up

            if ( nrow( pct_of_agg_group ) > 1 ) {
                year_totals_non_user_data[ 2:( nrow( pct_of_agg_group ) ), ] <- 
                                  year_totals_non_user_data[ 1, ]
            }
            pct_of_agg_group[ , Xyears ] <- data.matrix( pct_of_agg_group[ , Xyears ] ) / 
                                              data.matrix( year_totals_non_user_data[ , Xyears ] )
        }
        
    # Create a new dataframe that and replace the old row of data with the
    # new row, to calculate new year totals
        data_changed <- data_to_use[ , c( colnames( year_totals[ which( !isXYear( 
                                     colnames( year_totals ) ) ) ] ), Xyears ) ]
        data_changed <- replaceValueColMatch( data_changed, user_dataframe_subset,
                                             x.ColName = Xyears, 
                                             match.x = c( "iso", "CEDS_sector", "CEDS_fuel" ),
                                             addEntries = T )
        
        new_year_totals <- data_changed[ 1, ]
        if ( length( Xyears ) > 1) {
            new_year_totals[ 1, Xyears ] <- colSums( data_changed[ , Xyears ] )
            annual_diffs <- year_totals[ 1, Xyears ] - new_year_totals[ , Xyears ]
            if ( nrow( pct_of_agg_group ) > 1 ) {
                annual_diffs[ 2:( nrow( data_to_use ) - 1), ] <- annual_diffs[ 1, ]
            }
            sums_to_add <- pct_of_agg_group
            if ( !whole_group ) sums_to_add[ , Xyears ] <- data.matrix( pct_of_agg_group[ , Xyears ] ) * 
                                                                  data.matrix( annual_diffs[ , Xyears ] )
        } else {
            new_year_totals[ 1, Xyears ] <- sum( data_changed[ , Xyears ] )
            annual_diffs <- year_totals[ 1, Xyears ] - new_year_totals[ , Xyears ]
            if ( nrow( pct_of_agg_group ) > 1 ) {
                annual_diffs[ 2:(nrow(data_to_use) - 1) ] <- annual_diffs
            }
            sums_to_add <- pct_of_agg_group
            if ( !whole_group ) {
              sums_to_add[ , Xyears ] <- 
                            data.matrix( pct_of_agg_group[ , Xyears ] ) * 
                            data.matrix( annual_diffs )
            }
        }
        
        
    # Calculate the values that needed to be added to each non-specified cell
    #   in order to preserve the total activity by aggregate fuel for this iso/sector
        
    # Add these values into the old data
        data_to_correct <- data_to_use[ which( data_to_use$CEDS_fuel %!in% user_dataframe_subset$CEDS_fuel )
                                        , c( colnames( year_totals[ which( !isXYear( 
                                          colnames( year_totals ) ) ) ] ), Xyears ) ]
        corrected_data <- data_to_correct
        if ( !whole_group && !any( override_normalization ) ) {
            corrected_data[ , Xyears ] <- 
                    data.matrix( data_to_correct[ , Xyears ] ) + 
                    data.matrix( sums_to_add[ , Xyears ] )
        }
        
        data_changed[ which( data_to_use$CEDS_fuel %!in% user_dataframe_subset$CEDS_fuel ), ] <-
                corrected_data
        
        data_changed[ is.nan.df( data_changed ) ] <- 0
        data_changed[ is.na( data_changed ) ] <- 0
        
    # Return an error if column sums didn't come out the same (rounded to 3
    # decimal places to allow for e-14 processing discrepencies that were
    # occuring during testing)
        warning_diag <- NA
        if ( !any( override_normalization ) ) {
            if ( length( Xyears ) > 1 ) {
                if ( any( round( colSums( data_changed[ , Xyears ] ), 3 ) != 
                          round( colSums( data_to_use[ , Xyears ] ), 3 ) ) ) {
                    if( !whole_group ) {
                        warning( "Aggregate sums were not retained" )
                        warning_diag <- "Col sums not retained"
                    } else {
                        warning( "Aggregate sums were not retained due to whole-group overwrite" )
                        warning_diag <- "Whole-group overwrite"
                    }
                } 
                if ( any( colSums( data_changed[ , Xyears ] ) < 0 )  ) {
                  data_changed[ which(data_changed[ , Xyears ] < 0 ), Xyears ] <- 0
                  warning( "Some negative values were created during normalization. Coercing to zeros." )
                }             
            } else {
                if ( any( round( sum( data_changed[ , Xyears ] ), 3 ) != 
                          round( sum( data_to_use[ , Xyears ] ), 3 ) ) ) {
                    warning( "Aggregate sums were not retained" )
                    warning_diag <- "Col sums not retained"
                } 
                if ( any( sum( data_changed[ , Xyears ] ) < 0 ) ) {
                    data_changed[ which( data_changed[ , Xyears ] < 0 ), Xyears ] <- 0
                    warning( "Some negative values were created during normalization. Coercing to zeros." )
                    warning_diag <- "Negative values created"
                }                   
            }
        }

        all_activity_data[ which( all_activity_data$iso %in% data_changed$iso &
                                  all_activity_data$agg_fuel %in% data_changed$agg_fuel &
                                  all_activity_data$CEDS_sector %in% data_changed$CEDS_sector ), Xyears ] <-
                  data_changed[ , Xyears ]  ### We will maybe not re-add this data into the main dataframe... discuss later
    
    # Count the number of rows that have a changed value
        if ( length( Xyears ) > 1 ) {
            rows_changed <- sum( apply( data_changed[ , Xyears ] != data_to_use[ , Xyears ], 1, any ) ) 
        } else {
            rows_changed <- sum( any( data_changed[ , Xyears ] != data_to_use[ , Xyears ] ) )
        }
        
        activity_environment$all_activity_data <- all_activity_data

        diagnostics <- data.frame( rows_changed, warning_diag )
        return( diagnostics )
    }
    
# normalizeAndIncludeDataL3
# Brief: One of four main-loop functions for processing user extension data. This
#        handles agg level 3, for data that specifies agg sector, CEDS fuel, and iso.
#        It disaggregates to CEDS sector and normalizes to agg fuel x agg sector.
# params:
#    Xyears: the year range of data processing
#    data_to_use: a dataframe of unchanged activity data extracted from all_activity
#    user_dataframe_subset: a subsetted dataframe of user-specified data
#    all_activity_data: the dataframe holding all activity
#    whole_group: a logical option specifying whether an entire aggregate group is being processed
#    override_normalization: a manual option for disabling normalization      
    normalizeAndIncludeDataL3 <- function( Xyears, data_to_use, user_dataframe_subset, 
                                           all_activity_data, whole_group, override_normalization ) {
      
        if ( is.na( any( override_normalization ) ) ) {
            override_normalization <- F
        }

        activity_agg_to_l3 <- ddply( data_to_use, 
                                     c( "iso", "CEDS_fuel", "agg_fuel", "agg_sector" ), 
                                     function(x) colSums( x[ Xyears ] ) )
        
        act_agg_to_l3_changed <- activity_agg_to_l3
        act_agg_to_l3_changed[ which( act_agg_to_l3_changed$CEDS_fuel %in% 
                                      user_dataframe_subset$CEDS_fuel ), Xyears ] <- 
                    user_dataframe_subset[ , Xyears ]
      
    # Create a total to calculate percents of differences that the non-user-edited sectors need to absorb
        year_totals <- activity_agg_to_l3[ 1, ]
        pct_of_agg_group <- activity_agg_to_l3[ which( activity_agg_to_l3$CEDS_fuel %!in% 
                                                         user_dataframe_subset$CEDS_fuel ), 
                                                c( colnames( year_totals[ which( !isXYear( 
                                                  colnames( year_totals ) ) ) ] ), Xyears ) ]
        data_to_correct <- activity_agg_to_l3[ which( activity_agg_to_l3$CEDS_fuel %!in% user_dataframe_subset$CEDS_fuel )
                                               , c( colnames( year_totals[ which( !isXYear( 
                                                 colnames( year_totals ) ) ) ] ), Xyears ) ]
        
        if ( !whole_group && !any( override_normalization ) ) {
            year_totals[ 1, Xyears ] <- colSums( activity_agg_to_l3[ , Xyears ] )
            year_totals_non_user_data <- year_totals[ , c( colnames( year_totals[ which( !isXYear( 
                                                           colnames( year_totals ) ) ) ] ), Xyears ) ]
            year_totals_non_user_data[ , Xyears ] <- year_totals[ , Xyears ] - 
                                                      activity_agg_to_l3[ which( activity_agg_to_l3$CEDS_fuel %in% 
                                                                                 user_dataframe_subset$CEDS_fuel ), 
                                                                          Xyears ]
        
        # Calculate percents that each non-edited sector needs to absorb
            year_totals_non_user_data[ 2:( nrow( pct_of_agg_group ) - 1 ), ] <- year_totals_non_user_data[ 1, ]
            
            pct_of_agg_group[ , Xyears ] <- data.matrix( pct_of_agg_group[ , Xyears ] ) / 
                                            data.matrix( year_totals_non_user_data[ , Xyears ] )
            pct_of_agg_group[ is.nan.df( pct_of_agg_group ) ] <- 0
            
            new_year_totals <- act_agg_to_l3_changed[ 1, ]
            new_year_totals[ 1, Xyears ] <- colSums( act_agg_to_l3_changed[ , Xyears ] )
            
            annual_diffs <- year_totals[ 1, Xyears ] - new_year_totals[ , Xyears ]
            annual_diffs[ 2:( nrow( act_agg_to_l3_changed ) - 1), ] <- annual_diffs[ 1, ]
            
            sums_to_add <- pct_of_agg_group
            sums_to_add[ , Xyears ] <- data.matrix( pct_of_agg_group[ , Xyears ] ) * 
                                       data.matrix( annual_diffs[ , Xyears ] )
            
            corrected_data <- data_to_correct
            corrected_data[ , Xyears ] <- data.matrix( data_to_correct[ , Xyears ] ) + 
                                          data.matrix( sums_to_add[ , Xyears ] )
            
        # Add corrected data, resulting in a dataframe with the new agg sums
            act_agg_to_l3_changed[ which( act_agg_to_l3_changed$CEDS_fuel %!in% user_dataframe_subset$CEDS_fuel), ] <-
                            corrected_data
        }
      
    # Now we need to apply the changes above linearly to the disaggregate cells.
        disagg_data_changed <- data_to_use
        
        disaggregation_factors <- act_agg_to_l3_changed
        disaggregation_factors[ , Xyears ] <- act_agg_to_l3_changed[ , Xyears ] / 
                                              activity_agg_to_l3[ , Xyears ]
        
        disaggregation_factors <- left_join( disagg_data_changed[ , c( "iso", "CEDS_fuel", "CEDS_sector" ) ], 
                                             disaggregation_factors, 
                                             by = c( "iso", "CEDS_fuel" ) )
        
        disagg_data_changed[ , Xyears ] <- disagg_data_changed[ , Xyears ] * 
                                           disaggregation_factors[ , Xyears ] 
        
    # Replace any values that were 0s in the old dataset with 0s
        disagg_data_changed[ is.nan.df( disagg_data_changed ) & data_to_use == 0 ] <- 0
        
        warning_diag <- NA
        if ( !any( override_normalization ) ) {
            if ( length( Xyears ) > 1 ) {
                if ( any( round( colSums( disagg_data_changed[ , Xyears ] ), 3 ) != 
                          round( colSums( data_to_use[ , Xyears ] ), 3 ) ) ) {
                    warning( "Column sums were not retained \n[this will eventually be an error once we fix the situation with 0s]" )
                    warning_diag <- "Col sums not retained"
                }
            } else {
                if ( any( round( sum( disagg_data_changed[ , Xyears ] ), 3 ) != 
                          round( sum( data_to_use[ , Xyears ] ), 3 ) ) ) {
                    warning( "Column sums were not retained \n[this will eventually be an error once we fix the situation with 0s]" )
                    warning_diag <- "Col sums not retained"
                }             
            }
        }
        
        all_activity_data[ which( all_activity_data$iso %in% disagg_data_changed$iso &
                                    all_activity_data$agg_fuel %in% disagg_data_changed$agg_fuel &
                                    all_activity_data$CEDS_sector %in% disagg_data_changed$CEDS_sector ), Xyears ] <-
          disagg_data_changed[, Xyears]  ### We will maybe not re-add this data into the main dataframe... discuss later
        
        activity_environment$all_activity_data <- all_activity_data

    # Count the number of rows that have a changed value
        rows_changed <- sum( apply( disagg_data_changed != data_to_use, 1, any ) ) 
        diagnostics <- data.frame( rows_changed, warning_diag )
        return( diagnostics )
        
    }
    
# normalizeAndIncludeDataL2
# Brief: One of four main-loop functions for processing user extension data. This
#        handles agg level 2, for data that specifies agg CEDS fuel and iso.
#        It disaggregates to CEDS sector x CEDS fuel and normalizes to agg fuel.
# params:
#    Xyears: the year range of data processing
#    data_to_use: a dataframe of unchanged activity data extracted from all_activity
#    user_dataframe_subset: a subsetted dataframe of user-specified data
#    all_activity_data: the dataframe holding all activity
#    whole_group: a logical option specifying whether an entire aggregate group is being processed
#    override_normalization: a manual option for disabling normalization      
    normalizeAndIncludeDataL2 <- function( Xyears, data_to_use, user_dataframe_subset, 
                                           all_activity_data, whole_group, override_normalization ) {
      
        if ( is.na( any( override_normalization ) ) ) {
            override_normalization <- F
        }
    # At agg level 2, we need to:
        # Calculate total CEDS_fuel over all sectors
        # Replace values for our sectors at agg level
        # Normalize fuels over all sectors 
        # Replace all values down     
        activity_agg_to_l2 <- ddply( data_to_use, c( "iso", "CEDS_fuel", "agg_fuel" ), function(x) colSums( x[ Xyears ] ) )
        act_agg_to_l2_changed <- activity_agg_to_l2
        act_agg_to_l2_changed[ which( act_agg_to_l2_changed$CEDS_fuel %in% user_dataframe_subset$CEDS_fuel ), Xyears ] <- 
                                          user_dataframe_subset[ , Xyears ]

        year_totals <- activity_agg_to_l2[ 1, ]
        
        data_to_correct <- activity_agg_to_l2[ which( activity_agg_to_l2$CEDS_fuel %!in% user_dataframe_subset$CEDS_fuel )
                                        , c( colnames( year_totals[ which( !isXYear( 
                                          colnames(year_totals) ) ) ] ), Xyears ) ]

        if ( !whole_group && !any( override_normalization ) ) {
        # Create a total to calculate percents of differences that the non-user-edited sectors need to absorb
            year_totals[ 1, Xyears ] <- colSums( activity_agg_to_l2[ , Xyears ] )
            year_totals_non_user_data <- year_totals[ , c( colnames( year_totals[ which( !isXYear( 
              colnames( year_totals ) ) ) ] ), Xyears ) ]
            year_totals_non_user_data[ , Xyears ] <- year_totals[ , Xyears ] - 
                  activity_agg_to_l2[ which( activity_agg_to_l2$CEDS_fuel %in% 
                                             user_dataframe_subset$CEDS_fuel ), 
                                      Xyears ]
            
        # Calculate percents that each non-edited sector needs to absorb
            pct_of_agg_group <- activity_agg_to_l2[ which( activity_agg_to_l2$CEDS_fuel %!in% 
                                                             user_dataframe_subset$CEDS_fuel ), 
                                                    c( colnames( year_totals[ which( !isXYear( 
                                                      colnames( year_totals ) ) ) ] ), Xyears ) ]
            
            year_totals_non_user_data[ 2:( nrow( pct_of_agg_group ) - 1 ), ] <- 
                                        year_totals_non_user_data[ 1, ]
            
            
            pct_of_agg_group[ , Xyears ] <- data.matrix( pct_of_agg_group[ , Xyears ] ) / 
                                          data.matrix( year_totals_non_user_data[ , Xyears ] )
            pct_of_agg_group[ is.nan.df( pct_of_agg_group ) ] <- 0
            
            new_year_totals <- act_agg_to_l2_changed[ 1, ]
            new_year_totals[ 1, Xyears ] <- colSums( act_agg_to_l2_changed[ , Xyears ] )
            
            annual_diffs <- year_totals[ 1, Xyears ] - new_year_totals[ , Xyears ]
            annual_diffs[ 2:( nrow( act_agg_to_l2_changed ) - 1 ), ] <- annual_diffs[ 1, ]
            
            sums_to_add <- pct_of_agg_group
            sums_to_add[ , Xyears ] <- data.matrix( pct_of_agg_group[ , Xyears ] ) * 
                                       data.matrix( annual_diffs[ , Xyears ] )
            corrected_data <- data_to_correct
            corrected_data[ , Xyears ] <- data.matrix( data_to_correct[ , Xyears ] ) + 
                                          data.matrix( sums_to_add[ , Xyears ] )
        # Add corrected data, resulting in a dataframe with the new agg sums
            act_agg_to_l2_changed[ which( act_agg_to_l2_changed$CEDS_fuel %!in% 
                                        user_dataframe_subset$CEDS_fuel ), ] <- corrected_data

        }
        
    
        
    # Now we need to apply the changes above linearly to the disaggregate cells.
        disagg_data_changed <- data_to_use
        
        disaggregation_factors <- act_agg_to_l2_changed
        disaggregation_factors[ , Xyears ] <- act_agg_to_l2_changed[ , Xyears ] / 
                                              activity_agg_to_l2[ , Xyears ]
        
        disaggregation_factors <- left_join( disagg_data_changed[ , c( "iso", "CEDS_fuel" ) ], 
                                             disaggregation_factors, 
                                             by = c( "iso", "CEDS_fuel" ) )
        
        disagg_data_changed[ , Xyears ] <- disagg_data_changed[ , Xyears ] * disaggregation_factors[ , Xyears ] 
        
    ### An issue that's occurring--that I haven't addressed--is that if
    ### a cell had 0 to begin with, it will end up with 0 or NaN even if
    ### we've directly given it different data. We need a whole
    ### disaggregation procedure here to fix this, which takes its
    ### percent breakdowns fromthe nearest row with data (at least
    ### that's my proposal)
        
    # Replace any values that were 0s in the old dataset with 0s
        disagg_data_changed[ is.nan.df( disagg_data_changed ) & data_to_use == 0 ] <- 0
        
        warning_diag <- NA
        if ( !any( override_normalization ) ) {
            if ( length( Xyears ) > 1 ) {
                if ( any( round( colSums( disagg_data_changed[ , Xyears ] ), 3 ) != 
                          round( colSums( data_to_use[ , Xyears ] ), 3 ) ) ) {
                    warning( "Column sums were not retained 
                             \n[this will eventually be an error once we fix the situation with 0s]" )
                    warning_diag <- "Col sums not retained"
                }
            } else {
                if ( any( round( colSums( disagg_data_changed[ , Xyears ] ), 3 ) 
                          != round( colSums( data_to_use[ , Xyears ] ), 3 ) ) ) {
                    warning("Column sums were not retained 
                            \n[this will eventually be an error once we fix the situation with 0s]")
                    warning_diag <- "Col sums not retained"
                }           
            }
        }
        all_activity_data[ which( all_activity_data$iso %in% disagg_data_changed$iso &
                                    all_activity_data$agg_fuel %in% disagg_data_changed$agg_fuel &
                                    all_activity_data$CEDS_sector %in% disagg_data_changed$CEDS_sector ), Xyears ] <-
          disagg_data_changed[ , Xyears ]  ### We will maybe not re-add this data into the main dataframe... discuss later
        
    # Count the number of rows that have a changed value
        rows_changed <- sum( apply( disagg_data_changed != data_to_use, 1, any ) ) 
        activity_environment$all_activity_data <- all_activity_data

        diagnostics <- data.frame( rows_changed, warning_diag )
        return( diagnostics )
    }
    
# normalizeAndIncludeDataL1
# Brief: One of four main-loop functions for processing user extension data. This
#        handles agg level 1, for data that specifies agg fuel and iso.
#        It disaggregates to CEDS sector x CEDS fuel. It does not normalize.
# params:
#    Xyears: the year range of data processing
#    data_to_use: a dataframe of unchanged activity data extracted from all_activity
#    user_dataframe_subset: a subsetted dataframe of user-specified data
#    all_activity_data: the dataframe holding all activity
#    whole_group: a logical option specifying whether an entire aggregate group is being processed

    normalizeAndIncludeDataL1 <- function( Xyears, data_to_use, user_dataframe_subset, 
                                                                    all_activity_data ) {
        
        grouping_cols <- c( "iso", "agg_fuel" )
        data_to_use[ , Xyears ][ is.na( data_to_use[ , Xyears ] ) ] <- 0  
        
        if ( length( Xyears ) > 1 ) { 
            yearly_totals_unchanged <- colSums( data_to_use[ , Xyears] )
        } else yearly_totals_unchanged( sum( data_to_use[ , Xyears ] ) )
        
        adjustment_factors <- user_dataframe_subset[ , Xyears ] / yearly_totals_unchanged
        adjustment_factors[ 2:nrow( data_to_use ), ] <- adjustment_factors
        
        data_changed <- data_to_use
        data_changed[ , Xyears ] <- data_changed[ , Xyears ] * adjustment_factors
        
        if ( length( Xyears ) > 1 ) {
            if ( any( round( colSums( data_changed[ , Xyears ] ), 3 ) != 
                      round( colSums( user_dataframe_subset[ , Xyears ] ), 3 ) ) ) {
                stop( "Data were not scaled properly, don't match user-defined data" )
            }
        } else {
            if ( any( round( sum( data_changed[ , Xyears ] ), 3 ) != round( sum( user_dataframe_subset[ , Xyears ] ), 3 ) ) ) {
                stop( "Data were not scaled properly, don't match user-defined data" )
            }
        }
        
    
        all_activity_data[ which( all_activity_data$iso %in% data_changed$iso &
                                    all_activity_data$agg_fuel %in% data_changed$agg_fuel &
                                    all_activity_data$CEDS_sector %in% data_changed$CEDS_sector ), Xyears ] <-
          data_changed[ , Xyears ]  ### We will maybe not re-add this data into the main dataframe... discuss later
        
        warning_diag <- NA # Level 1 can't return a warning
    # Count the number of rows that have a changed value
        rows_changed <- sum( apply( data_changed != data_to_use, 1, any ) ) 
        
        activity_environment$all_activity_data <- all_activity_data
        
        diagnostics <- data.frame( rows_changed, warning_diag )
        return( diagnostics )
    }