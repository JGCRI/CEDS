#------------------------------------------------------------------------------
# Program Name: user_data_proc_pseudocode.R
# Author: Ben Goldstein
# Date Last Updated: 21 June 2017
# Program Purpose: To process user-defined datasets for use in the historical
#                  energy extension.
# Input Files: U.*, U.*-instructions, U.*-mapping
# Output Files: None
# Functions Defined: mapToCEDS, interpolateData, interpolateByTrend
# Notes:
# ------------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# processUserDefinedData()
# Brief:
#   1. Cleans the user data replacing NAs with zero and casting
#      all data columns to numeric
#   2. Maps the user data to the proper (CEDS) format
#   3. Interpolates the data to fill missing years
# Params:
#   filename: The non-extension part of the file containing user data
#   MSL: Master sector list (default)
#   MCL: Master country list (default)
#   MFL: Master fuel list (default)
# Returns: the data in filename as a mapped and interpolated dataframe
    processUserDefinedData <- function( filename, MSL = NULL, MCL = NULL, MFL = NULL) {

        # If master mapping files were not provided, they need to be read in.
        # MSL = Master Sector List
        # MCL = Master Country List
        # MFL = Master Fuel List
        if ( is.null( MSL ) ) {
            MSL <- readData( "Master_Sector_Level_map", domain = "MAPPINGS" )
            names( MSL )[ which( names( MSL ) == 'working_sectors_v1' ) ] <- 'CEDS_sector'
        }
        if ( is.null( MCL ) )
            MCL <- readData("Master_Country_List", domain = "MAPPINGS")
        if ( is.null( MFL ) )
            MFL <- readData("Master_Fuel_Sector_List", domain = "MAPPINGS",
                            extension = ".xlsx", sheet_selection = "Fuels")

        # Read in the interpolation instructions, saved with the default filename
        interp_instr <- NA
        tryCatch({
            interp_instr <- readData( paste0 ( "user-defined-energy/",
                                      filename, "-instructions"),
                                      domain = "EXT_IN", extension = ".xlsx",
                                      sheet_selection = "Interpolation_instructions" )
        }, error = function(x) {
            warning( paste0( "No sheet named 'Interpolation_instructions' found in ",
                             filename, "-instructions.xlsx" ) )
        })

        # Read in the actual user data file
        usr_data <- readData( filename,
                              domain = "EXT_IN",
                              domain_extension = "user-defined-energy/")

        # Take advantage of the isXYear function to isolate which columns are
        # available. Since data is not yet interpolated we can't use the range.
        Xyears <- colnames( usr_data )[ which( isXYear( colnames( usr_data ) ) ) ]

        # Cast all year columns to be numeric.
        if ( nrow( usr_data ) > 1 ) {
            usr_data[ , Xyears ] <- as.data.frame( sapply( usr_data[ , Xyears ],
                                                           as.numeric ) )
        } else {
            usr_data[ , Xyears ] <- sapply( usr_data[ , Xyears ], as.numeric )
        }

        # Replace all NA values with 0s
        usr_data[ is.na( usr_data ) ] <- 0

        # Read in each potential mapping sheet. Some sheets may not be provided,
        # which is okay as mapToCEDS can handle NULL values for the sheets.
        mappings <- readData( paste0( "user-defined-energy/", filename, "-mapping" ),
                              domain = "EXT_IN",
                              extension = ".xlsx" )

        # Map the user data to CEDS format
        mapped_df <- mapToCEDS( usr_data, MSL, MCL, MFL,
                                iso_map         = mappings$iso,
                                agg_sector_map  = mappings$agg_sector,
                                CEDS_sector_map = mappings$CEDS_sector,
                                agg_fuel_map    = mappings$agg_fuel,
                                CEDS_fuel_map   = mappings$CEDS_fuel )

        # Interpolate the mapped data
        interp_df <- interpolateData( mapped_df, interp_instr, MSL, MCL, MFL )

        return( interp_df )
    }

#------------------------------------------------------------------------------
# mapToCEDS()
# Brief: Maps a user dataframe to CEDS level categories
# Params:
#   dataframe: The user dataframe
#   MSL: Master sector list (default)
#   MCL: Master country list (default)
#   MFL: Master fuel list (default)
#   iso_map, agg_sector_map, CEDS_sector_map, agg_fuel_map, CEDS_fuel_map:
#        Specified mapping files corresponding to sheets in a mapping
#        spreadsheet. Should have one column that's the CEDS column and one
#        that matches the column in the user dataframe.
# Returns: the mapped dataframe
    mapToCEDS <- function( dataframe, MSL, MCL, MFL, iso_map = NULL,
                           agg_sector_map = NULL, CEDS_sector_map = NULL,
                           agg_fuel_map = NULL, CEDS_fuel_map = NULL, aggregate = TRUE ) {
    # Divide the present column names into
        dataframe_categories <- colnames( dataframe )[ which( !isXYear( colnames( dataframe ) ) ) ]
        Xyears <- colnames( dataframe )[ which( isXYear( colnames( dataframe ) ) ) ]

    # If the iso is not already in the data, map to it
        if ( 'iso' %!in% dataframe_categories ) {
            if ( is.null( iso_map ) ) {
                stop( "Error in mapToCEDS: iso not in data, but no iso map provided" )
            }

        # Merge the datasets together. This assumes that the name of the non-iso country column
        #   in each dataset is the same. Can we make this assumption? It seems like yes, as that's
        #   the point of a mapping file.
            dataframe_w_iso <- left_join( dataframe, iso_map )  ### Do anti-join first, print a warning with the unmatchable country-level data
            if ( nrow( dataframe_w_iso ) != nrow( dataframe ) ) {
                stop( "Double-mapped isos are not allowed." )
            }
            dataframe <- dataframe_w_iso

        # Throw out data that has NAs for iso
            dataframe <- dataframe[ which( !is.na( dataframe$iso ) ), ]
        }

        printLog( "mapToCEDS: iso mapping complete" )

    # If a CEDS sector map was provided:
        if ( !is.null( CEDS_sector_map ) |
                       'CEDS_sector' %in% dataframe_categories ) {
            if ( 'CEDS_sector' %!in% dataframe_categories ) {
            # same join, same assumptions
                dataframe_w_sector <- dplyr::left_join( dataframe, CEDS_sector_map )
                if ( nrow( dataframe_w_sector ) != nrow( dataframe ) ) {
                    stop( "Double-mapped sectors are not allowed." )
                }
                dataframe <- dataframe_w_sector

            # Throw out data that has NAs for CEDS_sector
                dataframe <- dataframe[ which( !is.na( dataframe$CEDS_sector ) ), ]
            }

        # if a CEDS sector map was provided but an agg sector map was not,
        #   we can automatically map to agg sector based on CEDS sectors and MSL
            if ( is.null( agg_sector_map ) ) {
                if ( 'agg_sector' %!in% dataframe_categories ) {
                    MSL_to_map <- unique( MSL[ , c( "CEDS_sector", "aggregate_sectors" ) ] )
                    colnames( MSL_to_map ) <- c( "CEDS_sector", "agg_sector" )
                    dataframe <- dplyr::left_join( dataframe, MSL_to_map )
                }
            }
        }

    # There are three possibilities. If neither CEDS nor agg sector maps are provided,
    #   agg_sector_map never gets assigned. If only CEDS_sector_map is provided,
    #   agg_sector_map happens above. If agg_sector_map is provided, it will be
    #   mapped below.
        if ( !is.null( agg_sector_map ) ) {
            if ( 'agg_sector' %!in% dataframe_categories ) {

            # same join, same assumptions
                dataframe_w_aggsec <- dplyr::left_join( dataframe, agg_sector_map )
                if ( nrow( dataframe_w_aggsec ) != nrow( dataframe ) ) {
                    stop( "Double-mapped sectors are not allowed." )
                }
                dataframe <- dataframe_w_aggsec

            # Throw out data that has NAs for CEDS_sector
                dataframe <- dataframe[ which( !is.na( dataframe$agg_sector ) ), ]

            }
        }

        # If a CEDS fuel map was provided:
        if ( !is.null( CEDS_fuel_map ) |
             'CEDS_fuel' %in% dataframe_categories ) {
            if ( 'CEDS_fuel' %!in% dataframe_categories ) {
              # same join, same assumptions
              dataframe_w_fuel <- dplyr::left_join( dataframe, CEDS_fuel_map )
              if ( nrow( dataframe_w_fuel ) != nrow( dataframe ) ) {
                  stop( "Double-mapped sectors are not allowed." )
              }
              dataframe <- dataframe_w_fuel

              # Throw out data that has NAs for CEDS_fuel
              dataframe <- dataframe[ which( !is.na( dataframe$CEDS_fuel ) ), ]
          }

          # if a CEDS fuel map was provided but an agg fuel map was not,
          #   we can automatically map to agg fuel based on CEDS fuel and MSL
          if ( is.null( agg_fuel_map ) ) {
            if ( 'agg_fuel' %!in% dataframe_categories ) {
              MFL_to_map <- MFL[ , c( "fuel", "aggregated_fuel" ) ]
              colnames( MFL_to_map ) <- c( "CEDS_fuel", "agg_fuel" )
              dataframe <- dplyr::left_join( dataframe, MFL_to_map )
            }
          }
        }

        # There are three possibilities. If neither CEDS nor agg sector maps are provided,
        #   agg_sector_map never gets assigned. If only CEDS_sector_map is provided,
        #   agg_sector_map happens above. If agg_sector_map is provided, it will be
        #   mapped below.
        if ( !is.null( agg_fuel_map ) ) {
          if ( 'agg_fuel' %!in% dataframe_categories ) {

            # same join, same assumptions
            dataframe_w_aggfuel <- dplyr::left_join( dataframe, agg_fuel_map )
            if ( nrow( dataframe_w_aggfuel ) != nrow( dataframe ) ) {
              stop( "Double-mapped fuels are not allowed." )
            }
            dataframe <- dataframe_w_aggfuel

            # Throw out data that has NAs for CEDS_sector
            dataframe <- dataframe[ which( !is.na( dataframe$agg_fuel ) ), ]

          }
        }


    # The pseudocode for agg_fuel and CEDS_fuel mapping should be identical to the
    #   sectoral mapping, so I won't reproduce it. Imagine that what follows occurs
    #   after fuel mapping is complete.

        present_columns <- colnames( dataframe ) [ which ( colnames( dataframe ) %in%
                                                        c( "iso", "agg_sector", "CEDS_sector",
                                                           "agg_fuel", "CEDS_fuel" ) ) ]

        dataframe <- dataframe[ , c( present_columns, Xyears ) ]

    # Aggregate data frames by categories
        if ( aggregate ) {
            dataframe <- ddply( dataframe, present_columns, function(x) colSums( x[ Xyears ] ) )
        }

        printLog( "Done with mapping file to CEDS categories" )

        return( dataframe )

    }


#------------------------------------------------------------------------------
# interpolateData()
# Purpose: A function defining standard interpolation methodology. This assumes that 0
#          values are not holes; only missing values or NAs are holes.
# Params:
#   df: A dataframe of user data that has been mapped to CEDS
#   interp_instr: The interpolation instructions parsed from the instructions
#                 file corresponding to the data
# Returns:
#------------------------------------------------------------------------------
    interpolateData <- function( df, interp_instr, MSL, MCL, MFL ) {

    # Check if the data has any holes

        # We can assume that all columns are in this form as the data has
        # already been processed by mapping
        non_years <- c("iso", "agg_sector", "CEDS_sector", "agg_fuel", "CEDS_fuel")
        non_year_cols <- names( df )[ names( df ) %in% non_years ]
        year_cols <- names( df ) [ isXYear( names( df ) ) ]

        if ( is.null( interp_instr ) ) {
            min_year <- substr( min( year_cols ), 2, 5 )
            max_year <- substr( max( year_cols ), 2, 5 )
        } else {
            min_year <- min(interp_instr$start_year)
            max_year <- max(interp_instr$end_year)
        }

        # The range of years specified by the instructions file
        X_data_years <- paste0( "X", min_year:max_year )

        # Add years that are specified in the instructions but aren't in the data
        needed_Xyears <- X_data_years[ X_data_years %!in% year_cols ]
        df[ , needed_Xyears ] <- NA

        # Remove years that are in the data but aren't specified by the instructions
        extra_Xyears <- year_cols[ year_cols %!in% X_data_years ]
        df[ , extra_Xyears ] <- NULL # this used to be NA, will that cause probs?

        # Rebuild the dataframe with the correct year columns
        df <- df[ , c( non_year_cols, X_data_years ) ]

        # Checks if any row in the dataframe contains NA. In the event that there
        # are no NAs we can return the data matched to the intended years.
        if ( !any( apply ( df, 1, function(r) any( is.na(r) ) ) ) ) {
            return( df )
        }

    # The data has holes, so we have to continue on and fill the gaps.

        # Confirm that a valid method was specified. Currently the only valid
        # methods are match_to_trend, and linear.
        valid_methods <- c( "match_to_trend", "linear" )

        if ( is.null( interp_instr ) || interp_instr$method == "linear" ) {
            # CEDS already has a function for linear interpolation.
            final_df <- df
            final_df[ , X_data_years ] <- interpolate_NAs( final_df[ , X_data_years])
        } else if ( interp_instr == "match_to_trend" ) {
            # Execute trend-matching function
            final_df <- interpolateByTrend( df,
                                            interp_instr$matching_file_name,
                                            interp_instr$domain,
                                            MSL, MCL, MFL )
        } else {
            # Instructions exist but the method is invalid
            warning( paste0( "Specified interpolation method '",
                             interp_instr$method, "' is invalid; using linear" ) )
            final_df <- df
            final_df[ , X_data_years ] <- interpolate_NAs( final_df[ , X_data_years ] )
        }

        return( final_df )

    }

#------------------------------------------------------------------------------
# interpolateByTrend()
# This function takes a dataframe and the info to read in a matching file,
#    then
    interpolateByTrend <- function( dataframe, match_filename, match_domain, MSL, MCL, MFL ) {


    # Read in the trend that we'll match our values to next to each gap
        dataToMatch <- readData( file_name = paste0("user-defined-energy/", match_filename ), domain = match_domain )

        non_year_cols <- colnames( dataframe ) [ which (colnames( dataframe ) %in%
                                                        c( "iso", "agg_sector", "CEDS_sector",
                                                           "agg_fuel", "CEDS_fuel" ) ) ]
        dataToMatch <- mapToCEDS( dataToMatch, MSL, MCL, MFL, iso_map = MCL, CEDS_sector_map = MSL )

    # Trim the trend data to only those rows that correspond to a row in the dataframe
        for ( col in non_year_cols ) {
            dataToMatch <- dataToMatch[ which( dataToMatch[[ col ]] %in% dataframe[[ col ]] ), ]
        }


    # Check that the specified trend data has data for all the years desired
        all_Xyears <- colnames( dataframe )[ which( colnames( dataframe ) %in% paste0( "X", 1750:2016 ) ) ]
        if ( any( all_Xyears %!in% colnames( dataToMatch ) ) ) {
            stop( "Error: Trend data did not have years available for specified date range." )
        }

    # Assume the data is uniformly in need of interpolation (all rows have holes
    #     in the same years). Identify the years for which there is data.
        row1_year_data <- dataframe[ 1, all_Xyears ]
        years_with_data <- colnames( row1_year_data )[ which( !is.na( row1_year_data ) ) ]

    # Trim trend data to the years of the dataframe
        dataToMatch <- dataToMatch[ , c( non_year_cols, all_Xyears ) ]

    # Initialize a multiplier dataframe
        multiplier <- dataframe

    # All yearly data set to NA
        multiplier[ , all_Xyears ] <- NA

    # Multiplier for years with original data set as given data / trend data
        multiplier[ years_with_data ] <- dataframe[ years_with_data ] / dataToMatch [ years_with_data ]
        multiplier[ all_Xyears ] <- replace( multiplier[ all_Xyears ], multiplier[ all_Xyears ] == Inf, 1 )
    # Linearly interpolate the multiplier
        multiplier[ , all_Xyears ] <- interpolate_NAs( multiplier[ , all_Xyears ] )  ### Thing to discuss: right now if a trend has a 0 value, that value will end up at 0 even if the data says otherwise.
                                                                                     ###   Should we go in and change this, maybe replace all 0s (or even all values) that we already had data for
                                                                                     ###   with the original data just to be safe? probably yes

    # Apply the multiplier to the data; result is interpolated data
        interpolated_data <- dataToMatch
        interpolated_data[ , all_Xyears ] <- dataToMatch[ , all_Xyears ] * multiplier[ , all_Xyears ]
        return( interpolated_data )
    }



#------------------------------------------------------------------------------
# processTrendData()
# Purpose: This function converts dataframes whose instructions indicate they
#          should be used as trend data to raw data that can be processed. It
#          writes out the processed data and adds a new line of instructions
#          to the master instructions list pointing to the new data rather than
#          the specified trend data.
# Params:
#   instructions: the list of instructions generated in the main body of the routine
#   all_activity_data: the source data to get trended
# Returns: An instructions file containing pointers to the output raw data
#          generated by matching data to trend
    processTrendData <- function( instructions, all_activity_data ) {

        trend_instructions <- instructions[ which( instructions$use_as_trend ), ]

        # Don't do anything if no instructions should be used as trend data
        if ( nrow( trend_instructions ) == 0) {
            return(instructions)
        }

        # Remove all instructions being used as trend data
        instructions <- instructions[ which( !instructions$use_as_trend ), ]

        # Because we'll have to write out a new file for each row, it is unrealistic
        # to plan on doing this vectorized; a for loop is necessary.
        for ( row_num in 1:nrow( trend_instructions ) ) {
            Xyears <- paste0( "X", trend_instructions[row_num, ]$start_year:
                                   trend_instructions[row_num, ]$end_year)

            # Call the processUserDefinedData function, which will execute mapping
            # and interpolation as necessary
            user_dataframe <- processUserDefinedData( trend_instructions[row_num, ]$data_file,
                                                      MSL, MCL, MFL )

            # Extract the data from the dataframe that will refer to the specific
            # categories and years as defined by the
            user_specified_trend <- subsetUserData( user_dataframe, trend_instructions[row_num, ] )
            agg_level <- identifyLevel( user_specified_trend )
            # Using the agg_level, identify the important columns
            if ( agg_level == 1 ) {
                cols_given <- c( "agg_fuel", "iso" )
            } else if ( agg_level == 2 ) {
                cols_given <- c( "agg_fuel", "CEDS_fuel", "iso" )
            } else if ( agg_level == 3 ) {
                cols_given <- c( "agg_fuel", "CEDS_fuel", "agg_sector", "iso" )
            } else if ( agg_level == 4 ) {
                cols_given <- c( "agg_fuel", "CEDS_fuel", "agg_sector",
                                 "CEDS_sector", "iso" )
            } else if ( agg_level == 5 ) {
                cols_given <- c( "agg_fuel", "agg_sector", "CEDS_sector", "iso" )
            } else if ( agg_level == 6 ) {
                cols_given <- c( "agg_fuel", "agg_sector", "iso" )
            }

            # Extract the activity data for this period
            data_to_get_trended <- all_activity_data
            for (col in cols_given) {
                data_to_get_trended <- data_to_get_trended[ data_to_get_trended[ , col ]
                                                            == user_specified_trend[ , col ], ]
            }

            # Aggregate the data to the trend's aggregate level. Since the
            # output is new data, there will be no need to disaggregate.
            data_to_get_trended <- ddply( data_to_get_trended, cols_given,
                                          function(x) colSums( x[ Xyears ] ) )

            # Identify a fraction which can be used to bring the data in the
            # match year to be the same. The idea behind the match year is that
            # the "trend" will all become relative to a single value from the
            # original dataframe. The original trend is completely overwritten
            # except for the single absolute value.
            match_year <- paste0( "X", trend_instructions[ row_num, ]$match_year )
            scaling_fraction <- data_to_get_trended[ , match_year ] /
                user_specified_trend[ , match_year ]

            # Compute the trended data, as described above.
            matched_trend <- user_specified_trend
            matched_trend[ , Xyears ] <- user_specified_trend[ , Xyears ] * scaling_fraction

            # We have the trend. We now need to get the data into a state where it
            # can be handled by the system as raw data. To do this, we need to a)
            # add a new line to instructions, and b) write out a new .csv file.
            new_instruction <- trend_instructions[ row_num, ]
            # This data no longer needs to be used as a trend as it has been
            # converted to activity data.
            new_instruction$use_as_trend <- F
            # Create the name of the new/ouput file
            new_instruction$data_file <- paste0( "Data_from_trend_",
                                                 trend_instructions[ row_num, ]$data_file,
                                                 "-", row_num )
            # This file will not need to be mapped or interpolated. This switch
            # will allow us to not create a *-instructions or *-mapping file.
            new_instruction$bypass_processing <- T

            # Write out the new file.
            writeData( matched_trend,
                       domain = "EXT_IN", domain_extension = "user-defined-energy/",
                       fn = new_instruction$data_file )
            # Add the new instruction back into the main df.
            instructions <- rbind( instructions, new_instruction )
        }

        # Return the new version of the instructions.
        return( instructions )
    }



#------------------------------------------------------------------------------
# getRowsForAdjustment()
# Purpose: This function fetches the data from the extended default activity
#          that needs adjustment based on the new user data provided.
#
#          Identify the rows that will need adjusting. If the aggregation level
#          is the lowest, this will be any rows that match to your row, and any
#          row in the next-highest aggregation GROUP. For example, If I'm
#          adjusting usa-coal_coke-1A1, I will need to edit all the
#          usa-coal_coke-1A rows. If we're dealing with data on not the lowest
#          aggregation level, you need any cells that can map to this cell.
#
#          Basically, in all cases: figure out what level you're on, then grab
#          any rows that would be in the same group as yours one level up.
# Params:
#   all_activity_data: the source data that needs adjusting
#   usrdata: the new data that will replace values in all_activity_data
#   MFL: the Master Fuel List from mappings/Master_Fuel_Sector_List.xlsx
#   agg_level: the aggregation level of the current instruction
# Returns: the rows that need adjusting
#          generated by matching data to trend
getRowsForAdjustment <- function( all_activity_data, usrdata, MFL, agg_level ) {

    data_to_adjust <- dplyr::filter( all_activity_data, iso %in% usrdata$iso )
    cols <- c( names( data_to_adjust )[!sapply( names( data_to_adjust ), isXYear )], Xyears )
    data_to_adjust <- data_to_adjust[ , cols]

    if ( agg_level %in% c(1, 2, 5, 6) )
        data_to_adjust <- dplyr::filter( data_to_adjust,
                                         agg_fuel %in% usrdata$agg_fuel)
    if ( agg_level %in% c(3, 5) )
        data_to_adjust <- dplyr::filter( data_to_adjust,
                                         agg_sector %in% usrdata$agg_sector )
    if ( agg_level == 4 )
        data_to_adjust <- dplyr::filter( data_to_adjust,
                                         CEDS_sector %in% usrdata$CEDS_sector )
    if (agg_level %in% c(3, 4))
        data_to_adjust <- dplyr::filter( data_to_adjust,
                                         agg_fuel %in% MFL$aggregated_fuel[MFL$fuel %in% usrdata$CEDS_fuel] )

    return( data_to_adjust )
}