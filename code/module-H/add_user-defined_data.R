#------------------------------------------------------------------------------
# Program Name: user_data_proc_pseudocode.R
# Author: Ben Goldstein
# Date Last Updated: 14 June 2017
# Program Purpose: To process user-defined datasets for use in the historical
#                  energy extension. 
# Input Files: U.*.csv, U.*-instructions.xslx, U.*-mapping.xslx
# Output Files: None
# Functions Defined: mapToCEDS, interpolateData, interpolateByTrend
# Notes: 
### TODO: If the function gives an error, we should not end the whole system,
###       all we need to do is reject this user-defined dataset and proceed
###       to the next one... Unless this would make it hard for the user
###       to see that their changes are being rejected?
# ------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
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

# Get emission species first so can name log appropriately
    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[1]
    if ( is.na( em ) ) em <- "CO2"

# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R" ,"emissions_scaling_functions.R" , "analysis_functions.R", 
                  "interpolation_extension_functions.R", "user_data_processing.R",
                  "user_extension_instr_processing.R") # Additional function files required.
    log_msg <- paste0( "Calling inventory emission scaling stripts" ) # First message to be printed to the log
    script_name <- paste0( "step2_adding_data_pseudocode.R" )
    
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

    
# ------------------------------------------------------------------------------------
# 0.5 Define functions
    
# identifyLevel
# Brief: This is a helper function for the add_user-defined_data script.
#        It performs a simple check of column names to determine the data
#        frame's level of aggregation.
# params:
#    dataframe: the dataframe whose level you wish to identify
    identifyLevel <- function ( dataframe ) {
        
        column_names <- colnames(dataframe)
        categorical <- column_names[ which( !isXYear(column_names) ) ]
        
        if ( "CEDS_sector" %in% column_names ) return(4)
        else if ( "agg_sector" %in% column_names & 
                  "CEDS_fuel" %in% column_names ) return(3)
        else if ( "CEDS_fuel" %in% column_names ) return(2)
        else if ( "agg_fuel" %in% column_names ) return(1)
        
        return(0)
    }
    
# is.invalid
# Brief: This is a helper function for the add_user-defined_data script.
#        Combination check for if a value is null, NA, or NaN
# params:
#    x: some non-list object
    is.invalid = function(x) {
      return(is.null(x) || is.na(x) || is.nan(x))
    }
    
# is.nan.df
# Brief: This is a helper function for the add_user-defined_data script.
#        Makes up for the fact that R has no built-in vectorized check 
#        for NaN.
# params:
#    x: a dataframe that may or may not contain NaN values
    is.nan.df <- function(x) {
        do.call(cbind, lapply(x, is.nan)) 
    }

# retrieveUserDataframeSubset
# Brief: This is a helper function for the add_user-defined_data script.
#        Subsets a user-specified dataset based on user-specified instructions,
#        so the only data processed is the data specified in instructions.
# params:
#    user_dataframe: 
#    working_instructions:
    retrieveUserDataframeSubset <- function( user_dataframe, working_instructions ) {
        
    # initialize a subset dataframe
        user_dataframe_subset <- user_dataframe[ which( user_dataframe$iso %in% 
                                                          working_instructions$iso ), ]
        
    # Subset the dataframe based on which columns are present and filled out in the dataframe
        if ( !is.invalid( working_instructions$CEDS_sector ) && working_instructions$CEDS_sector != 'all' ) {
          user_dataframe_subset <- user_dataframe_subset[ which ( user_dataframe_subset$CEDS_sector %in%
                                                                    working_instructions$CEDS_sector ), ]
        } else if ( !is.invalid( working_instructions$agg_sector ) && working_instructions$agg_sector != 'all' ) {
          user_dataframe_subset <- user_dataframe_subset[ which ( user_dataframe_subset$agg_sector %in%
                                                                    working_instructions$agg_sector ), ]
        }
        
        if ( !is.invalid( working_instructions$CEDS_fuel ) && working_instructions$CEDS_fuel != 'all' ) {
          user_dataframe_subset <- user_dataframe_subset[ which ( user_dataframe_subset$CEDS_fuel %in% 
                                                                    working_instructions$CEDS_fuel ), ]
        } else if ( !is.invalid( working_instructions$agg_fuel ) && working_instructions$agg_fuel != 'all' ) {
          user_dataframe_subset <- user_dataframe_subset[ which ( user_dataframe_subset$agg_fuel %in%
                                                                    working_instructions$agg_fuel ), ]
        }
        return( user_dataframe_subset )
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

    # At agg level 2, we need to:
        # Aggregate by CEDS_fuel
        # Replace values for our sectors at agg level
        # Normalize over all sectors
        # Replace all values down
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
        }
        
    # Add corrected data, resulting in a dataframe with the new agg sums
        act_agg_to_l3_changed[ which( act_agg_to_l3_changed$CEDS_fuel %!in% user_dataframe_subset$CEDS_fuel), ] <-
          corrected_data
        
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

        if ( !whole_group && !any( override_normalization ) ){
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
        }
        
    
    # Add corrected data, resulting in a dataframe with the new agg sums
        act_agg_to_l2_changed[ which( act_agg_to_l2_changed$CEDS_fuel %!in% 
                                        user_dataframe_subset$CEDS_fuel ), ] <- corrected_data
        
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
    

# ------------------------------------------------------------------------------------
# 1. Read in data

    MSL <- readData( "Master_Sector_Level_map", domain = "MAPPINGS" )
    colnames( MSL )[ which( colnames( MSL ) == 'working_sectors_v1' ) ] <- 'CEDS_sector' 
    MCL <- readData( "Master_Country_List", domain = "MAPPINGS" )
    MFL <- readData( "Master_Fuel_Sector_List", domain = "MAPPINGS", extension = ".xlsx",
                    sheet_selection = "Fuels" )
    comb_or_NC <- readData( "Master_Fuel_Sector_List", domain = "MAPPINGS", extension = ".xlsx",
                           sheet_selection = "Sectors" )
    comb_sectors_only <- comb_or_NC$sector[ which( comb_or_NC$type == "comb" ) ]
    

# Gather default activity data
    default_activity <- readData( 'MED_OUT', paste0( 'H.', em, '_total_activity_extended_db' ) , meta = F) ### Eventually this will not require an emissions species.
    colnames( default_activity )[ 1:3 ] <- c( "iso", "CEDS_sector", "CEDS_fuel" )
    default_activity_mapped <- mapToCEDS( default_activity, MSL, MCL, MFL, aggregate = F )

# ------------------------------------------------------------------------------------
# 2. Collect user-defined inputs and begin processing data loop

# Initialize an environment to track activity data
    activity_environment <- new.env()
    
# This script only operates on combustion emissions, so reduce the data to that form
    all_activity_data <- default_activity_mapped[ which( default_activity_mapped$CEDS_sector %in% comb_sectors_only ), ]
    activity_environment$all_activity_data <- all_activity_data
    
# Read instructions files and create a procedure list
    instructions <- readInUserInstructions()
     instructions[ , c( "iso", "CEDS_fuel", "agg_fuel" ) ] <- left_join( instructions[ , c( "iso", "CEDS_fuel" ) ], 
                                                            MFL[ , c( "aggregated_fuel", "fuel" ) ], 
                                                            by = c( "CEDS_fuel" = "fuel" ) )

# This will store the final form of each instruction used, for diagnostics
    rows_completed <- instructions[ 0, ]
    
# This integer will track which batch number we're on, for informing diagnostics
    batch <- 0
    
    old.file <- "NULL"
    
    while ( nrow( instructions ) > 0 ) {
        
        batch <- batch + 1
    
    # Select the first instruction in the list for processing
        working_instructions <- instructions[ 1, ]
        instructions <- instructions[ -1, ]
        
        Xyears <- paste0( "X", working_instructions$start_year:working_instructions$end_year )
      
    # Get the first dataset to operate on. 
        new.file <-working_instructions$data_file
        
        if ( old.file != new.file ) {
            dataframe_interp_instructions <- readData( paste0 ( "user-defined-energy/", 
                                                                new.file, "-instructions"),
                                                       domain = "EXT_IN", extension = ".xlsx", 
                                                       sheet_selection = "Interpolation_instructions" )
            user_dataframe <- processUserDefinedData( new.file, dataframe_interp_instructions, MSL, MCL, MFL )
        }
            
    # Extract the data from the dataframe that will refer to the specific
    # categories and years as defined by the
        user_dataframe_subset <- retrieveUserDataframeSubset( user_dataframe, working_instructions )
        agg_level <- identifyLevel( user_dataframe )
        
    # Identify other instructions in the "batch" that will neeed to be
    # aggreagated as one.
        
        if ( agg_level == 4 ) {
            
            batch_data_instructions <- instructions[ which( !is.na( instructions$L4_CEDS_sector ) & 
                                                            instructions$iso %in% user_dataframe_subset$iso &
                                                            instructions$CEDS_sector %in% user_dataframe_subset$CEDS_sector &
                                                            instructions$agg_fuel
                                                                        %in% user_dataframe_subset$agg_fuel ), ] 
            instructions <- instructions[ which( is.na( instructions$L4_CEDS_sector ) |
                                                    instructions$iso %!in% user_dataframe_subset$iso |
                                                    instructions$CEDS_sector %!in% user_dataframe_subset$CEDS_sector |
                                                    instructions$agg_fuel %!in% user_dataframe_subset$agg_fuel ), ]

            
            
        } else if ( agg_level == 3 ) {
            batch_data_instructions <- instructions[ which( !is.na( instructions$L4_CEDS_sector ) & 
                                                              instructions$iso %in% user_dataframe_subset$iso &
                                                              instructions$agg_sector %in% user_dataframe_subset$agg_sector &
                                                              instructions$agg_fuel
                                                            %in% user_dataframe_subset$agg_fuel ), ]
            instructions <- instructions[ which( is.na( instructions$L4_CEDS_sector ) |
                                                   instructions$iso %!in% user_dataframe_subset$iso |
                                                   instructions$agg_sector %!in% user_dataframe_subset$agg_sector |
                                                   instructions$agg_fuel %!in% user_dataframe_subset$agg_fuel ), ]

        } else if ( agg_level %in% 1:2 ) {
            batch_data_instructions <- instructions[ which( !is.na( instructions$L4_CEDS_sector ) &
                                                            instructions$iso %in% user_dataframe_subset$iso &
                                                            instructions$agg_fuel %in% user_dataframe_subset$agg_fuel #&
                                                            ), ] ### A line of code that confirms that they're only grabbing instructions at the same level.
            instructions <- instructions[ which( is.na( instructions$L4_CEDS_sector ) |
                                                 instructions$iso %!in% user_dataframe_subset$iso |
                                                 instructions$agg_fuel %!in% user_dataframe_subset$agg_fuel ), ]
        }

        batch_instructions_overlap <- batch_data_instructions[ which( 
                              batch_data_instructions$start_year < working_instructions$end_year &
                              batch_data_instructions$end_year > working_instructions$start_year ), ]
        batch_instructions_no_overlap <- batch_data_instructions[ which( 
                              batch_data_instructions$start_year > working_instructions$end_year |
                              batch_data_instructions$end_year < working_instructions$start_year ), ]
        instructions <- rbind( instructions, batch_instructions_no_overlap )
        batch_data_instructions <- batch_instructions_overlap
        
    # If there are data in our instructions that will be in the current group's batch:
        if ( nrow( batch_data_instructions ) > 0 ) {
          
        # If our years haven't been properly subdivided by year, we'll need to
        #     subdivide the whole batch and return back to the beginning of the loop
            if ( any( batch_data_instructions$start_year != working_instructions$start_year ) ||
                 any( batch_data_instructions$end_year != working_instructions$end_year ) ) {
                
                year_breaks <- unique( c( batch_data_instructions$start_year,
                                          working_instructions$start_year,
                                          batch_data_instructions$end_year + 1,
                                          working_instructions$end_year + 1 ) ) %>%
                                  sort()
                
                whole_batch <- rbind( working_instructions, batch_data_instructions )
                new_division_batch <- whole_batch[ 0, ]
                
                for (i in 1:(length(year_breaks)-1)) {
                    new_year_span <- year_breaks[ i ]:( year_breaks[ i+1 ] - 1 )
                    
                    rows_to_segment <- whole_batch[ which( whole_batch$start_year <=
                                                                  max( new_year_span ) &
                                                           whole_batch$end_year >=
                                                                  min( new_year_span ) ), ]
                    rows_to_segment$start_year <- min( new_year_span )
                    rows_to_segment$end_year <- max( new_year_span )
                    
                    new_division_batch <- rbind( new_division_batch, rows_to_segment )
                }
                
                instructions <- rbind( instructions, new_division_batch )
                next
            }
        
        # If our years are properly subdivided, we should re-retrieve our
        #   user_defined_data dataframe. This may require drawing on multiple
        #   source files.
            working_instructions <- rbind( working_instructions, batch_data_instructions )
            user_dataframe_subset <- user_dataframe_subset[0,]
            for ( file in unique( working_instructions$data_file ) ) { 
              
                dataframe_interp_instructions <- readData( paste0 ( "user-defined-energy/", 
                                                                    file, "-instructions"),
                                                           domain = "EXT_IN", extension = ".xlsx", 
                                                           sheet_selection = "Interpolation_instructions" )
                user_dataframe <- processUserDefinedData( file, dataframe_interp_instructions, MSL, MCL, MFL )
                
                user_dataframe_subset <- rbind( user_dataframe_subset, 
                                                retrieveUserDataframeSubset( user_dataframe, 
                                                                             working_instructions ) )
                
            }
          

              
        }
        
        
    # Identify the rows that will need adjusting. If the aggregation level is the lowest,
    #   this will be any rows that match to your row, and any row in the next-highest aggregation
    #   GROUP. For example, If I'm adjusting ger-coal_coke-1A1, I will need to edit all the
    #   ger-coal_coke-1A rows.
    #   If we're dealing with data on not the lowest aggregation level, you need any cells that
    #   can map to this cell.
    # Basically, in all cases: figure out what level you're on, 
    #   then grab any rows that would be in the same group as yours one level up.
    
        if ( agg_level == 4 ) {
            data_to_use <- all_activity_data[ which( all_activity_data$iso %in% user_dataframe_subset$iso &
                                                     all_activity_data$CEDS_sector %in% user_dataframe_subset$CEDS_sector &
                                                     all_activity_data$agg_fuel %in% MFL$aggregated_fuel[ which( MFL$fuel %in%
                                                                                                    user_dataframe_subset$CEDS_fuel ) ]
                                                     ), c( colnames( all_activity_data[ which( !isXYear( 
                                                       colnames( all_activity_data ) ) ) ] ), Xyears ) ]
            
        } else if (agg_level == 3) {
            data_to_use <- all_activity_data[ which( all_activity_data$iso %in% user_dataframe_subset$iso &
                                                     all_activity_data$agg_sector %in% user_dataframe_subset$agg_sector &
                                                     all_activity_data$agg_fuel == MFL$aggregated_fuel[ which(  MFL$fuel %in% 
                                                                                                    user_dataframe_subset$CEDS_fuel ) ] 
                                                     ), c( colnames( all_activity_data[ which( !isXYear( 
                                                       colnames(all_activity_data) ) ) ] ), Xyears ) ]
        } else if (agg_level == 2 || agg_level == 1) {
            data_to_use <- all_activity_data[ which( all_activity_data$iso %in% user_dataframe_subset$iso&
                                                     all_activity_data$agg_fuel %in% user_dataframe_subset$agg_fuel
                                                     ), c( colnames( all_activity_data[ which( !isXYear( 
                                                       colnames(all_activity_data) ) ) ] ), Xyears ) ]
        } 
        
        whole_group = F
        if ( nrow( data_to_use ) == nrow( user_dataframe_subset ) ) {
            whole_group = T
        }

        if ( agg_level == 1 ) {
            diagnostics <- normalizeAndIncludeDataL1( Xyears, data_to_use, user_dataframe_subset, 
                                                      all_activity_data )
        } else if ( agg_level == 2 ) {
            diagnostics <- normalizeAndIncludeDataL2( Xyears, data_to_use, user_dataframe_subset, 
                                                      all_activity_data, whole_group, 
                                                      as.logical( working_instructions$override_normalization ) )
        } else if ( agg_level == 3 ) {
            diagnostics <- normalizeAndIncludeDataL3( Xyears, data_to_use, user_dataframe_subset, 
                                                      all_activity_data, whole_group, 
                                                      as.logical( working_instructions$override_normalization ) )
        } else if ( agg_level == 4 ) {
            diagnostics <- normalizeAndIncludeDataL4( Xyears, data_to_use, user_dataframe_subset, 
                                                      all_activity_data, whole_group, 
                                                      as.logical( working_instructions$override_normalization ) )
        }
        
        working_instructions$batch_id <- batch
        working_instructions$nrow_changed <- diagnostics$rows_changed
        working_instructions$warnings <- diagnostics$warning_diag
        
        old.file <- new.file
        rows_completed <- rbind( rows_completed, working_instructions )
        
    }

    writeData( rows_completed, domain = "DIAG_OUT", fn = "user-ext-data_diagnostics")

    
