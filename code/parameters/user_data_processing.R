#------------------------------------------------------------------------------
# Program Name: user_data_processing.R
# Authors: Ben Goldstein, Caleb Braun
# Date Last Updated: 12 July 2018
# Program Purpose: Define some helper functions for processing user-defined
#                  datasets for use in the historical energy extension.
# -----------------------------------------------------------------------------

#------------------------------------------------------------------------------
# readInUserData()
# Brief: Given a root filename, reads in an extension data file, or optionally
#        an associated instructions, mapping, or breakdowns file.
# Params:
#   fname: The filename without extension
#   yearsAllowed: A vector of the CEDS Xyears; any input years outside this
#                 range are filtered out
#   ftype: A string to add on to fname that specifies a support (non-data) file
#
# Returns: A data frame containing the contents of the file that are in the CEDS
#          year range.
readInUserData <- function( fname, yearsAllowed, ftype = NULL ) {
    # Read in files
    fpath <- paste0( "user-defined-energy/", fname, ftype )
    if ( !is.null( ftype ) && ftype == "-mapping" )
        user_df <- tryCatch(
            readData( fpath, domain = "EXT_IN", extension = ".xlsx" ),
            error = function(e) NULL )
    else
        user_df <- readData( fpath, domain = "EXT_IN" )

    # Filter out any years not in the CEDS range
    bad_years <- isXYear( names( user_df ) ) &
                 names( user_df ) %!in% yearsAllowed

    if ( any( bad_years ) ) {
        warning( paste("Some data in", fname, "are not in the",
                       "allowed CEDS years and will be ignored") )
        user_df <- user_df[ !bad_years ]
    }

    return( user_df )
}

#------------------------------------------------------------------------------
# procUsrData()
# Brief:
#   1. Cleans the user data replacing NAs with zero and casting
#      all data columns to numeric
#   2. Maps the user data to the proper (CEDS) format
#   3. Interpolates the data to fill missing years
# Params:
#   usr_data: A dataframe of user-defined data.
#   proc_instr: The instructions for processing the user-defined data.
#   mappings: The instructions for mapping the user-defined data to CEDS format.
#             If NULL, data is expected to be already correctly mapped.
#   MSL: Master CEDS sector list.
#   MCL: Master CEDS country (iso) list.
#   MFL: Master CEDS fuel list.
#   trend_data: If interpolating to trend, a dataframe containing that trend
# Returns: the data in usr_data as a mapped and interpolated dataframe
procUsrData <- function( usr_data, proc_instr, mappings,
                         MSL, MCL, MFL, trend_data = NULL ) {

    # Get year columns and cast them as numeric
    year_cols <- names( usr_data )[ isXYear( names( usr_data ) ) ]
    usr_data <- dplyr::mutate_at( usr_data, year_cols, as.numeric )

    # mappings is expected as a list of data frames, but if it isn't we can
    # pull out the name of the data frame from the column names
    if ( is.data.frame( mappings ) ) {
        mappings_name <- intersect( names( mappings ), names( proc_instr ) )
        mappings <- setNames( list( mappings ), mappings_name )
    }

    # Test if `sector_map` parameter is defined and has any character values.
    # If it does, we need to
    if ( !is.invalid( proc_instr$sector_map ) ) {
        maps <- list( mappings$iso, mappings$CEDS_fuel, mappings$agg_fuel )
        mcols <- c( 'iso', 'CEDS_fuel', 'agg_fuel' )
        usr_data <- joinUserMaps( usr_data, maps, mcols )
        usr_data <- disaggUserSectors( usr_data, proc_instr, trend_data )
    }

    # Clean, map, validate, and then interpolate the user data
    interp_df <- mapToCEDS( usr_data, MSL, MFL,
                            iso_map         = mappings$iso,
                            agg_sector_map  = mappings$agg_sector,
                            CEDS_sector_map = mappings$CEDS_sector,
                            agg_fuel_map    = mappings$agg_fuel,
                            CEDS_fuel_map   = mappings$CEDS_fuel ) %>%
                 validateUserData( proc_instr ) %>%
                 interpolateData( proc_instr, MSL, MCL, MFL, trend_data )

    if ( identifyLevel( interp_df ) == 0 )
        stop("Fuel type not found in user data")

    return( interp_df )
}


# ------------------------------------------------------------------------
# mapToCEDS()
# Brief: Maps a user dataframe to CEDS level categories
# Params:
#   dataframe: The user dataframe
#   MSL: Master sector list (default)
#   MFL: Master fuel list (default)
#   aggregate: Boolean - should the data be aggregated after joining?
#   iso_map, agg_sector_map, CEDS_sector_map, agg_fuel_map, CEDS_fuel_map:
#        Specified mapping files corresponding to sheets in a mapping
#        spreadsheet. Should have one column that's the CEDS column and one
#        that matches the column in the user dataframe.
# Returns: the mapped dataframe
    mapToCEDS <- function( usrdata, MSL, MFL, aggregate = TRUE, iso_map = NULL,
                           agg_sector_map = NULL, CEDS_sector_map = NULL,
                           agg_fuel_map = NULL, CEDS_fuel_map = NULL ) {

        # Make sure iso is accounted for
        if ( is.null( iso_map ) & 'iso' %!in% names( usrdata ) )
            stop( "iso not in data, but no iso map provided" )

        # The only accepted non-year columns (order matters)
        CEDS_COLS <- c( "iso", "agg_sector", "CEDS_sector", "agg_fuel", "CEDS_fuel" )
        maps <- list( iso_map, agg_sector_map, CEDS_sector_map, agg_fuel_map, CEDS_fuel_map )

        usrdata <- joinUserMaps( usrdata, maps, CEDS_COLS )

        # Join in the higher agg level column, if not already mapped by user
        usrdata <- addAggregateCol( usrdata, 'sector', MSL, 'aggregate_sectors', 'CEDS_sector' )
        usrdata <- addAggregateCol( usrdata, 'fuel',   MFL, 'aggregated_fuel', 'fuel' )

        # Order the results correctly
        Xyears <- names( usrdata )[ isXYear( names( usrdata ) ) ]
        agg_cols <- names( usrdata )[ names( usrdata ) %in% CEDS_COLS ]
        # dplyr::select( usrdata, dplyr::one_of( CEDS_COLS, Xyears ) )
        usrdata <- usrdata[ , c( agg_cols, Xyears ) ]

        # Aggregate data frames by categories
        if ( aggregate ) {
            usrdata <- ddply( usrdata, agg_cols, function(x) colSums( x[ Xyears ] ) )
        }

        return( usrdata )
    }


#------------------------------------------------------------------------------
# interpolateData()
# Purpose: A function defining standard interpolation methodology. This assumes
#          that 0 values are not holes; only missing values or NAs are holes.
# Params:
#   df: A dataframe of user data that has been mapped to CEDS
#   interp_instr: The interpolation instructions parsed from the instructions
#                 file corresponding to the data
#   MSL: Master sector list
#   MCL: Master country list
#   MFL: Master fuel list
#   trend_data: If interpolating to trend, a dataframe containing that trend
# Returns:
    interpolateData <- function( df, interp_instr, MSL, MCL, MFL, trend = NULL ) {

        # X_data_years is the range of years specified by the instructions file
        min_year <- min( interp_instr$start_year )
        max_year <- max( interp_instr$end_year )
        X_data_years <- paste0( "X", min_year:max_year )

        # Subset data years to correct range and fill in missing years with NAs
        df <- filterToYearRange( df, X_data_years )
        df[ , X_data_years[ X_data_years %!in% names( df ) ] ] <- NA
        df <- df[ order( names( df ) ) ]

        # Checks if any row in the dataframe contains NA. In the event that
        # there are no NAs we can return the data matched to the intended years.
        if ( !any( is.na( df ) ) ) return( df )

        # If we didn't return, the data has holes that need interpolating. First
        # find out what method to use, then call the corresponding interpolation
        # function.
        #
        # TODO: Figure out what to do in the case that interp_instr specifies
        #       different methods for different instructions
        # Idea:
        #   1. filter to specific interp type:
        #      mtt <- dplyr::filter(interp_instr, method == "match_to_default")
        #   2. get rows in df that match that specific method:
        #      join_cols <- aggLevelToCols(identifyLevel(df))
        #   3. Interpolate whole df with that method, but only replace rows that
        #      specified that method
        #   4. Repeat with all methods
        method <- unique( interp_instr$method )
        if ( length( method ) > 1 )
            stop( "Multiple interpolation methods is currently unsupported" )

        if ( method == "linear" ) {
            # CEDS already has a function for linear interpolation.
            df[ , X_data_years ] <- interpolate_NAs( df[ , X_data_years ] )
        }
        else if ( method == "match_to_default" ) {
            df <- interpolateByTrend( df, trend )
        }
        else if ( method == "match_to_trend" ) {
            # Execute trend-matching function
            # TODO: Error checking
            trend <- readData( interp_instr$match_file_name, domain = "EXT_IN",
                               domain_extension = "user-defined-energy/" )
            trend <- mapToCEDS( trend, MSL, MFL, iso_map = MCL,
                                CEDS_sector_map = MSL )
            df <- interpolateByTrend( df, trend )
        }
        else {
            # Instructions exist but the method is invalid
            stop( paste( "Interpolation method '", method, "' not supported" ) )
        }

        return( df )
    }

#------------------------------------------------------------------------------
# interpolateByTrend()
# This function takes an incomplete dataframe and data to use as a trend. It
# matches to the trend and fills in values.
#
# Note: It is assumed that the data is uniformly in need of interpolation (all
# rows have holes in the same years).
    interpolateByTrend <- function( usrdata, trend_data ) {

        agg_cols <- c( "iso", "agg_sector", "CEDS_sector", "agg_fuel", "CEDS_fuel" )
        agg_cols <- names( usrdata )[ names( usrdata ) %in% agg_cols ]
        all_Xyears <- names( usrdata )[ isXYear( names( usrdata ) ) ]

        # Trim the trend data to only those rows and columns that correspond to
        # entries in the usrdata dataframe
        trend_data <- filterToYearRange( trend_data, all_Xyears)
        trend_data <- dplyr::left_join( usrdata[ , agg_cols ], trend_data,
                                        by = agg_cols) %>%
                      ddply( agg_cols, function(x) colSums( x[ all_Xyears ] ) )

        # Multiply the trend data by the correct ratios such that all user data
        # present is included as-is, and all missing user data is interpolated.
        multiplier <- usrdata[ , all_Xyears ] / trend_data[ , all_Xyears ]
        multiplier[ multiplier == Inf ] <- 1
        multiplier <- interpolate_NAs( multiplier )
        trend_data[ , all_Xyears ] <- trend_data[ , all_Xyears ] * multiplier

        return( trend_data )
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

        stop("Custom trend data is not currently supported.")

        trend_instructions <- instructions[ instructions$use_as_trend, ]

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

            # Call the procUsrData function, which will execute mapping and
            # interpolation as necessary
            user_dataframe <- procUsrData( trend_instructions[row_num, ]$data_file,
                                                      MSL, MCL, MFL )

            # Extract the data from the dataframe that will refer to the specific
            # categories and years as defined by the
            user_specified_trend <- subsetUserData( user_dataframe, trend_instructions[row_num, ] )
            agg_level <- identifyLevel( user_specified_trend )

            # Using the agg_level, identify the important columns
            cols_given <- aggLevelToCols( agg_level )

            # Extract the activity data for this period
            # TODO: use join instead of for loop
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

#------------------------------------------------------------------------------
# filterToYearRange( df, X_data_years )
# Purpose: Filters a CEDS-mapped dataframe to a range of years
# Params:
#   df: the source data to filter
#   X_data_years: the range of years to filter to
# Returns: the filtered dataframe
filterToYearRange <- function( df, X_data_years ) {

    # We can assume that all columns are in this form as the data has already
    # been processed by mapping.
    # TODO: Check if mapToCEDS removes extra non-year columns, and if so remove
    #       the non_years list.
    non_years <- c("iso", "agg_sector", "CEDS_sector", "agg_fuel", "CEDS_fuel")
    non_year_cols <- names( df )[ names( df ) %in% non_years ]
    year_cols <- names( df ) [ isXYear( names( df ) ) ]

    # Validate years are ok
    if ( min( X_data_years ) < min( year_cols ) )
        stop("Specified start year earlier than years provided in data")
    if ( max( X_data_years ) > max( year_cols ) )
        stop("Specified end year later than years provided in data")

    # Rebuild dataframe with only years specified by the instructions
    cols_in_range <- year_cols[ year_cols %in% X_data_years ]
    final_df <- df[ , c( non_year_cols, cols_in_range ) ]

    return( final_df )
}

# subsetUserData
# Purpose: Subsets a user-specified dataset based on user-specified
#          instructions, checking for validity and removing any irrelevant data.
# Params:
#    user_df: Entire user dataset associated with the given instruction(s)
#    instructions: Dataframe of instructions
# Returns: A subset of user_df
subsetUserData <- function( user_df, instructions ) {

    # Determine which aggregation columns are present in the instructions
    na_cols <- colSums( !is.na( instructions ) ) == 0
    yr_cols <- setdiff( names( user_df ), names( instructions ) )
    agg_cols <- intersect( names( user_df ), names( instructions[ !na_cols ] ) )

    # Initialize a subset dataframe based on matching isos between the user
    # instructions and actual data. Then aggregate the user data to the same
    # level of detail specified in the instructions
    subset <- dplyr::select( user_df, agg_cols, yr_cols ) %>%
              dplyr::filter( iso %in% instructions$iso ) %>%
              dplyr::group_by_at( agg_cols ) %>%
              dplyr::summarise_all( sum ) %>%
              dplyr::ungroup() %>% data.frame()

    # Subset the dataframe based on which columns are specified in the
    # instructions
    if ( !is.invalid( subset$CEDS_sector ) ) {
      subset <- subset[ subset$CEDS_sector %in% instructions$CEDS_sector, ]
    } else if ( !is.invalid( instructions$agg_sector ) ) {
      subset <- subset[ subset$agg_sector %in% instructions$agg_sector, ]
    }

    if ( !is.invalid( instructions$CEDS_fuel ) ) {
      subset <- subset[ subset$CEDS_fuel %in% instructions$CEDS_fuel, ]
    }
    else if ( !is.invalid( instructions$agg_fuel ) ) {
      subset <- subset[ subset$agg_fuel %in% instructions$agg_fuel, ]
    }

    # Error checks
    validateUserData( subset, instructions )
}


#------------------------------------------------------------------------------
# validateUserData
# Purpose: Ensures user data matches specifications in Trend_instructions sheet
# Params:
#   df: a dataframe that has been mapped to CEDS
#   trend_instr: the Trend_instructions sheet
# Returns: the validated dataframe
validateUserData <- function( df, trend_instr ) {

    if ( nrow( df ) == 0 ) {
        err <- paste( as.character( trend_instr[ 1, ] ), collapse = " " )
        stop(paste("No provided data matches instruction:\n", err))
    }

    # Go through instructions line by line
    apply( trend_instr, 1, function( instr ) {
        instr <- as.data.frame( t( instr ), stringsAsFactors = FALSE )
        instr <- instr[ 1, !is.na( instr ) ]
        join_cols <- dplyr::intersect( names( instr ), names( df ) )
        instr_data <- dplyr::left_join( instr[ join_cols ], df, by = join_cols )

        err <- paste0( "Error in instruction:\n\t",
                        paste( instr[ join_cols ], collapse = " "), "\n " )

        # Tests!
        if ( all( is.na( instr_data[ , isXYear( names( instr_data ) ) ] ) ) )
            stop( paste( err, "No data found for instruction"))
        if ( paste0( "X", instr$start_year ) %!in% names( instr_data ) )
            stop( paste( err, "Start year earlier than any year in data" ) )
        if ( paste0( "X", instr$end_year ) %!in% names( instr_data ) )
            stop( paste( err, "End year later than any year in data" ) )
        if ( any( is.na( instr_data[[ paste0( "X", instr$start_year ) ]] ) ) )
            stop( paste( err, "Some data is NA at start year" ) )
        if ( any( is.na( instr_data[[ paste0( "X", instr$end_year ) ]] ) ) )
            stop( paste( err, "Some data is NA at end year" ) )
    })

    return( df )
}


#------------------------------------------------------------------------------
# validateUserMap
# Purpose: Ensures a user mapping file follows the correct format
# Params:
#   user_map: a dataframe containing the mapping information
#   trend_instr: the Trend_instructions sheet
# Returns: the validated dataframe (allowing use in a chain)
validateUserMap <- function( user_map, CEDS_COLS ) {

    if ( is.null( user_map ) ) return( NULL )

    usr_cols <- names( user_map )
    col_name <- usr_cols[ usr_cols %in% CEDS_COLS ]
    map_key <- usr_cols[ usr_cols %!in% CEDS_COLS ][1]

    if ( length( col_name ) != 1 )
        stop( paste( c( "ambiguous mapping:", usr_cols ), collapse = " " ) )

    map_type <- tail( strsplit( col_name, '_' )[[1]], 1 )

    if ( length( usr_cols ) < 2 )
        stop( paste( "not enough columns in", map_type, "map" ) )
    if ( length( usr_cols ) > 2 )
        warning( paste( "extra columns found in ", map_type, "map; only using",
                        "columns", col_name, "and", map_key ) )
    if ( length( unique( user_map[[ map_key ]] ) ) != nrow( user_map ) )
        stop( paste0( "double-mapped ", map_type, "s are not allowed" ) )

    return( user_map )
}


# Disaggregate the instructions before starting to process the user data. Then
# use the new instructions to spread the user data over all its disaggregations.
# From there we do the breakdown calculation and re-write of user data.
#
# 1. Map instructions agg_sector to correct agg_sector (left join on agg_sector)
# 2. Rename old agg_sector to agg_sector_inv and keep CEDS_sector as the new one
# 3. Join with default data
mapToUserSectors <- function( df, default_activity ) {
    if ( !is.character( df$sector_map ) ) return( df )
    stopifnot( all( c( 'iso', 'agg_fuel', 'agg_sector' ) %in% names( df ) ) )

    user_sector_maps <- unique( df$sector_map )
    user_sector_maps <- user_sector_maps[ !is.na( user_sector_maps ) ]

    df_mapped <- lapply( user_sector_maps, function( map ) {
        map_no_csv <- sub( '.csv', '', map, fixed = T )
        sector_map <- readInUserData( map_no_csv, NULL )
        stopifnot( 'CEDS_sector' %in% names( sector_map ) )
        stopifnot( ncol( sector_map ) == 2 )
        join_col <- setdiff( names( sector_map ), 'CEDS_sector' )

        df_subset <- df %>%
            dplyr::filter( sector_map == map )

        # TODO: Add check to see if we need to join on CEDS_fuel instead of
        #       only allowing agg_fuel
        if ( !all( is.na( df_subset$CEDS_fuel ) ) )
            stop( "Custom sector mapping with CEDS_fuel instructions currently",
                  " not supported")

        id_columns <- c( 'iso', 'agg_fuel', 'CEDS_sector' )
        df_subset %>%
            dplyr::select( -CEDS_sector, -CEDS_fuel ) %>%
            dplyr::left_join( sector_map, by = c( "agg_sector" = join_col ) ) %>%
            dplyr::rename( agg_sector_inv = agg_sector ) %>%
            dplyr::mutate( agg_sector_join_col = join_col ) %>%
            dplyr::semi_join( default_activity[ id_columns ], by = id_columns )
    })

    df_mapped <- do.call( rbind, df_mapped )

    df %>%
        dplyr::filter( is.na( sector_map ) ) %>%
        dplyr::bind_rows( df_mapped )
}


disaggUserSectors <- function( usr_data, proc_instr, default_activity ) {
    user_sector_maps <- unique( proc_instr$sector_map )
    if ( length( user_sector_maps ) != 1 )
        stop( paste( "File", proc_instr$data_file[1], "must all use one sector",
                     "map" ) )

    join_col <- unique( proc_instr$agg_sector_join_col[1] )
    stopifnot( length( join_col ) == 1 )

    usr_data_mapped <- proc_instr %>%
        dplyr::select( iso, CEDS_sector, agg_fuel, agg_sector_inv ) %>%
        dplyr::left_join( usr_data, by = c( "iso", "agg_fuel",
                                            "agg_sector_inv" = join_col ) )

    disagg_all <- usr_data_mapped %>%
        dplyr::left_join( default_activity, by = c( "iso", "agg_fuel",
                                                    "CEDS_sector" ) ) %>%
        dplyr::filter( !is.na( CEDS_fuel ) ) %>%
        dplyr::select( iso, agg_sector_inv, agg_sector, agg_fuel, CEDS_sector,
                       CEDS_fuel, ends_with( '.x' ), ends_with ( '.y' ) )

    disagg <- disagg_all %>%
        dplyr::group_by( iso, agg_sector_inv, agg_fuel ) %>%
        dplyr::mutate_at( vars( matches( 'X\\d{4}\\.y' ) ), prop.table ) %>%
        dplyr::mutate_at( vars( matches( 'X\\d{4}\\.y' ) ),
                          funs( if_else( is.nan( . ), 0, . ) ) ) %>%
        dplyr::ungroup()

    dplyr::bind_cols(
        dplyr::select( disagg, iso, agg_sector, agg_fuel, CEDS_sector, CEDS_fuel ),
        dplyr::select( disagg, matches( 'X\\d{4}\\.x' ) ) *
            dplyr::select( disagg, matches( 'X\\d{4}\\.y' ) ) ) %>%
    dplyr::rename_all( funs( sub( '\\.(x|y)$', '', . ) ) )

}


joinUserMaps <- function(usrdata, maps, default_cols) {
    # Ensure mapping files are correctly formatted
    sapply( maps, validateUserMap, default_cols )

    # Extract the unmapped non-year column names
    user_cnames <- names( usrdata )[ !isXYear( names( usrdata ) ) ]

    # Loop through maps and join with data (if the column is not already
    # present)
    for ( i in seq_along( maps ) ) {
        mp <- maps[[i]]
        map_col <- default_cols[i]

        if ( !is.null( mp ) & map_col %!in% user_cnames ) {
            # Merge the datasets together and throw out data that has NAs
            key_col <- names( mp )[ names( mp ) != map_col ][1]
            usrdata <- dplyr::left_join( usrdata, mp, by = key_col ) %>%
                       dplyr::filter( !is.na( UQ( as.name( map_col ) ) ) )
        }
    }

    return( usrdata )
}


preprocUserData <- function( instructions ) {
    if ( !is.null( instructions$preprocessing_script ) ) {
        preproc <- unique( instructions$preprocessing_script )
        preproc <- preproc[ !is.na( preproc ) ]
        preproc <- paste0( "extension/user-defined-energy/", preproc)
        sapply( preproc, source, local = T, chdir = T )
        instructions$preprocessing_script <- NULL
    }

    return( instructions )
}


# Join in the higher agg level column, if not already mapped by user
addAggregateCol <- function( df, type, map, map_agg_col, map_disagg_col ) {
    stopifnot( type %in% c( 'sector', 'fuel' ) )
    disagg_col <- paste0( 'CEDS_', type )
    agg_col <- paste0( 'agg_', type )
    if ( disagg_col %!in% names( df ) ) return( df )

    map <- unique( map[ , c( map_disagg_col, map_agg_col ) ] )
    names( map ) <- c( disagg_col, agg_col )

    if ( agg_col %!in% names( df ) ) {
        rowsMissingAgg <- df
        rowsAlreadyAgg <- NULL
    } else {
        rowsMissingAgg <- df[ is.na( df[ agg_col ] ) & !is.na( df[ disagg_col ] ), ]
        rowsAlreadyAgg <- setdiff( df, rowsMissingAgg )
        rowsMissingAgg[ agg_col ] <- NULL
    }

    fixed_df <- rowsMissingAgg %>%
        dplyr::left_join( map, by = disagg_col ) %>%
        dplyr::bind_rows( rowsAlreadyAgg )

    stopifnot( nrow( fixed_df ) == nrow( df ) )

    return( fixed_df )
}
