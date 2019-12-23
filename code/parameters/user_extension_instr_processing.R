#------------------------------------------------------------------------------
# Program Name: user_extension_instr_processing.R
# Authors: Ben Goldstein, Caleb Braun, Patrick O'Rourke
# Date Last Updated: November 5, 2019
# Program Purpose: Provides functions for the add_user-defined_data script that
#                  help process the instructions for handling user-defined
#                  datasets.
# ------------------------------------------------------------------------------------


# Order instructions
#
# Sort user instructions to ensure that all actions are performed in the right
# order. The rows are arranged in ascending order by:
#   1. priority
#   2. CEDS_sector
#   3. CEDS_fuel
#   4. start_year
#
# Args:
#   instructions: Processed instructions (see processInstructions())
#
# Returns:
#   The ordered instructions
orderInstructions <- function( instructions ) {
    dplyr::arrange( instructions, priority, CEDS_sector, CEDS_fuel, start_year )
}


# Get instruction filenames
#
# Retrieve instruction files from the user_energy_input directory. They are
# all specified with the name [filename]-instructions.csv
#
# Returns:
#   Vector of filenames of all instructions files to be processed
getInstructionFilenames <- function() {
    USER_DOM <- "energy/user-defined-energy/user_energy_input"

    # Get a list of all the files in the directory
    files_present <- list.files( USER_DOM )
    if ( !length( files_present ) )
        message( "No user defined energy files found; using default data" )

    files <- grep( ".+-instructions.csv", files_present, value = T )
    files <- sub( ".csv", "", files )
    return( files )
}


# Read in user instructions
#
# Searches the user_energy_input directory for instructions, filters out non-
# combustion data, and adds defaults.
#
# Returns:
#   A list of lists. Outer list is named the base filename of the instructions.
#   Inner lists contain two dataframes, one named Trend_instructions and the
#   other Interpolation_instructions.
readInUserInstructions <- function() {
    instr_names <- getInstructionFilenames()
    if ( length( instr_names ) == 0 ) return( NULL )
    instr_files <- paste0( instr_names ) %>%
                   sapply( readData, domain = "USER_EN_IN", simplify = F ) %>%
                   setNames( sub( "-instructions", "", instr_names ) )

    return( instr_files )
}


# Prepare the raw trend instructions for use in the main processing loop
#
# Gives a single dataframe with columns for all CEDS ids (currently: iso,
# agg_fuel, CEDS_fuel, agg_sector, CEDS_sector) and all instruction parameters.
#   - If instruction is for disaggregate data, aggregate id columns will be
#     automatically added. For example, if CEDS_fuel is 'coal_coke', the
#     agg_fuel column will be automatically filled in as 'coal'.
#   - If instruction is for aggregate data, the disaggregate id columns are set
#     to NA.
#   - If an instruction parameter is not specified, it will be set to the
#     default value
#
# For a description of allowed instruction parameters, please see the official
# wiki: https://github.com/JGCRI/CEDS/wiki/User_Guide#32-use-instructions-options
#
# Args:
#   comb_sectors: Vector of combustion sectors
#   MSL: Master sector mapping file
#   MFL: Master fuel mapping file
#   default_activity: Extended default activity data
#
# Returns:
#   A dataframe containing all user instructions
processInstructions <- function( comb_sectors, MSL, MFL, default_activity ) {

    # Get list of all '-instructions.csv' files in the user_energy_input dir
    instructions <- readInUserInstructions()

    # Put all instructions into single dataframe with uniform columns
    instructions <- cleanInstructions( instructions, comb_sectors, MSL, MFL )

    # Preprocess any data that needs it
    instructions <- preprocUserData( instructions )

    # Add defaults for optional use instructions.
    # TODO: add options for use_as_trend and match_year
    opts <- list( priority = NA_integer_,
                  use_as_trend = FALSE,
                  #match_year = integer(0),
                  start_continuity = TRUE,
                  end_continuity = TRUE,
                  specified_breakdowns = FALSE )
    opts_present <- intersectNames( instructions, opts )
    opts_missing <- setdiff( names( opts ), opts_present )

    # Replace missing options with their default values
    sapply( opts_missing, function(o) {
        instructions[[o]] <<- opts[[o]]
    })

    # Cast existing options to the correct type and replace NAs
    sapply( opts_present, function(o) {
        instructions[[o]][ is.na( instructions[[o]] ) ] <<- opts[[o]]
        class( instructions[[o]] ) <<- class( opts[[o]] )
    })

    instructions <- instructions %>%
        mapToUserSectors( default_activity ) %>%
        addAggregateCol( 'sector', MSL, 'aggregate_sectors', 'CEDS_sector' )

    # Special case: If iso is NA ('all' gets replaced to NA as well), repeat
    # instructions for all isos.
    if ( any( is.na( instructions$iso ) ) ) {
        all_isos <- unique( default_activity$iso )
        instructions <- instructions %>%
            dplyr::filter( is.na( iso ) ) %>%
            dplyr::slice( rep( 1:n(), each = length( all_isos ) ) ) %>%
            dplyr::mutate( iso = rep( all_isos, n() / length( all_isos ) ) ) %>%
            dplyr::bind_rows( dplyr::filter( instructions, !is.na( iso ) ) )
    }

    return( instructions )
}


# Extract batch instructions
# TODO: update documentation below, I believe this is inaccurate
# Given a dataframe of instructions and a dataframe of energy extension values,
# return the rows from the instructions dataframe that apply to the user data.
#
# Args:
#   instructions: Processed instructions (see processInstructions())
#   usrdf: Processed data for the energy extension (see procUsrData())
#   sy: Start year
#   ey: End year
#
# Returns:
#   The instructions filtered to only the rows that apply to the user data
extractBatchInstructions <- function( working_instructions, instructions, sy, ey ) {
    CEDS_COLS <- getCEDSAggCols()

    # Filters the instructions to only the ones with corresponding data in the
    # user's instruction.
    # Files only need to be batched if their year ranges overlap.

    stopifnot(length(working_instructions$keep_total_cols) == 1)

        # Define what variables are needed to filter instructions by for batching
    matches <- working_instructions$keep_total_cols[[1]]

        # If keep_total_cols is set to "NA", then batch with other instructions for
        # the iso which are "NA" for the parameter
    if( working_instructions$keep_total_cols == "NA" ){

        working_instructions <- working_instructions %>%
            dplyr::mutate( keep_total_cols = as.character( keep_total_cols ) )

        instructions <- instructions %>%
            dplyr::mutate( keep_total_cols = as.character( keep_total_cols ) )

        matches <- "keep_total_cols"

    }

    instructions <- instructions %>%
        dplyr::semi_join( working_instructions, by = c( matches, "iso" ) ) %>%
        dplyr::filter( start_year <= ey, end_year >= sy )

    return( instructions )
}


# Remove any non-combustion data
#
# Removes all non-combustion data, as that is not supported
#
# Args:
#   df: Data needing filtering
#   comb_sectors_only: Vector of combustion sectors to filter to
#
# Returns:
#   The filtered combustion data
removeNonComb <- function( df, comb_sectors_only ) {
    num_instructions <- nrow( df )
    df <- dplyr::filter( df, is.na( CEDS_sector ) |
                             CEDS_sector %in% comb_sectors_only )

    num_removed <- num_instructions - nrow( df )
    if ( num_removed > 0 ) {
        warning( paste( num_removed, "instruction line(s) were rejected as",
                        "non-combustion sectors" ) )
    }

    return( df )
}


# Clean instructions for user-added data
#
# Converts list of raw instruction files into a standardized data.frame
#
# Args:
#   instructions: A list of data.frames of user instructions
cleanInstructions <- function( instructions, comb_sectors_only, MSL, MFL ) {
    # Make sure instructions are a given as a list of data.frames
    stopifnot( all( sapply( instructions, is.data.frame ) ) )

    # Define CEDS_cols
    CEDS_cols <- getCEDSAggCols()

    # Extract the trend instructions, add the file they came from, and map to
    # the standard CEDS format

    instruction_list <- lapply( seq_along( instructions ), function( i ) {

        # Extract and add source file
        instruction_df <- instructions[[i]]
        instr_dfile <- names( instructions )[i]
        instruction_df$data_file <- instr_dfile

        # Map
        mapped <- mapToCEDS( instruction_df, MSL, MFL, aggregate = F )
        instruction_df[ names( mapped ) ] <- mapped

        # Add in any aggregation levels the user has not provided (iso and agg_fuel
        # are required and should be present from calling mapToCEDS).
        # Note that an instruction requesting all sectors or fuels to be included is
        # the same as leaving out that column.
        stopifnot( c( "iso", "agg_fuel" ) %in% names( instruction_df ) )
        instruction_df[ setdiff( CEDS_cols, names( instruction_df ) ) ] <- NA_character_

        # If agg_sector is set to "all" then ensure that exclude_int_bunkers is either
        # TRUE, FALSE, or is.invalid. If exclude_int_bunkers is invalid or not equal to
        # TRUE or FALSE, set it to TRUE by default (default to remove int. bunkers from
        # total consumption data)
        instruction_df_with_aggsector <- instruction_df

            # If any values for agg_sector are NA, then replace the value with the string
            # "MISSING" temporarily
            if( any( is.na( instruction_df_with_aggsector$agg_sector ) ) ){

                instruction_df_with_aggsector <- instruction_df_with_aggsector %>%
                    dplyr::mutate( agg_sector = if_else( is.na( agg_sector ), "MISSING",
                                                         agg_sector ) )

            }

            # If the instructions file does not have the column exclude_int_bunkers or if
            # the column exists but some rows have NA values, provide the string "MISSING"
            # temporarily
            if( is.null( instruction_df_with_aggsector$exclude_int_bunkers ) ){

                instruction_df_with_aggsector$exclude_int_bunkers <- "MISSING"

                }

            instruction_df_with_aggsector <- instruction_df_with_aggsector %>%
                dplyr::mutate( exclude_int_bunkers = as.character( exclude_int_bunkers )) %>%  # convert all exclude_int_bunker values to characters temporarily
                dplyr::mutate( exclude_int_bunkers = toupper( exclude_int_bunkers) ) # converts text to upper case (i.e. if the user provides "false", it is converted to "FALSE")

            if( any( is.na( instruction_df_with_aggsector$exclude_int_bunkers ) ) ){

                instruction_df_with_aggsector <- instruction_df_with_aggsector %>%
                    dplyr::mutate( exclude_int_bunkers = if_else( is.na( exclude_int_bunkers ),
                                                                  "MISSING", exclude_int_bunkers ) )

            }

            # If any rows have agg_sector set to "all" and have exclude_int_bunkers set to
            # "MISSING" warn the user that the default value of true will be provided for those rows,
            # meaning that the int. bunkers will be excluded from the total fuel consumption data
            total_consump_missing_int_bunk_instructions <- instruction_df_with_aggsector %>%
                dplyr::filter( agg_sector == "all", exclude_int_bunkers == "MISSING" )

            if( nrow( total_consump_missing_int_bunk_instructions ) != 0 ){

                warning( "User instructions for ", instr_dfile,
                         ".csv indicate that the user is providing CEDS with supplementary ",
                         "TOTAL consumption data for at least one fuel, as -agg_sector- ",
                         "was set to -all- at least once. However, no values were provided in the same ",
                         "row(s) for column -exclude_int_bunkers-...  System is setting the ",
                         "values of -exclude_int_bunkers- for these rows to the default value of TRUE ",
                         " -- International bunkers will not be included within the disaggregation of the ",
                         "user's total fuel consumption data...." )

                instruction_df_with_aggsector <- instruction_df_with_aggsector %>%
                    dplyr::mutate( exclude_int_bunkers = if_else( agg_sector == "all" &
                                                                  exclude_int_bunkers == "MISSING",
                                                                  "TRUE", exclude_int_bunkers ) )

            }

            # If any rows have agg_sector set to "all" and have exclude_int_bunkers set to a value
            # that isn't equal to "T, "F", "TRUE", or "FALSE, warn the user that the default
            # value of true will be provided for that row as the value provided is not an available
            # option, meaning that the int. bunkers will be excluded from the total fuel consumption data
            total_consump_wrong_int_bunk_instructions <- instruction_df_with_aggsector %>%
                dplyr::filter( agg_sector == "all",
                               exclude_int_bunkers %!in% c( "T", "TRUE", "F","FALSE" ) )


            if(  nrow( total_consump_wrong_int_bunk_instructions ) != 0 ){

                warning( "User instructions for ", instr_dfile,
                         ".csv indicate that the user is providing CEDS with supplementary ",
                         "TOTAL consumption data for at least one fuel, as -agg_sector- ",
                         "was set to -all- at least once. However, the values ",
                         "provided for the column -exclude_int_bunkers-in the same rows were ",
                         "not provided as either TRUE or FALSE... The system is setting the ",
                         "values of -exclude_int_bunkers- for these rows to the default value of TRUE ",
                         " -- International bunkers will not be included within the disaggregation of the ",
                         "user's total fuel consumption data...." )

                instruction_df_with_aggsector <- instruction_df_with_aggsector %>%
                    dplyr::mutate( exclude_int_bunkers = if_else( agg_sector == "all" &
                                                                  exclude_int_bunkers %!in% c( "T", "TRUE", "F","FALSE" ),
                                                                  "TRUE", exclude_int_bunkers ) )

            }

            not_total_consump_missing_int_bunk_instructions <- instruction_df_with_aggsector %>%
                dplyr::filter( agg_sector != "all", exclude_int_bunkers == "MISSING" )

            if( nrow( not_total_consump_missing_int_bunk_instructions ) != 0 ){

                instruction_df_with_aggsector <- instruction_df_with_aggsector %>%
                    dplyr::mutate( exclude_int_bunkers = if_else( agg_sector != "all" &
                                                                  exclude_int_bunkers == "MISSING",
                                                                  NA_character_, exclude_int_bunkers ) )

            }

        # If agg_sector is not set to "all" then ensure exclude_int_bunkers is set to NA_character_,
        # as exclude_int_bunkers only corresponds to total fuel consumption data.
        # 1) Warn the user if they provided a value for exclude_int_bunkers
        # 2) If agg_sector is not currently set to "all", then set exclude_int_bunkers to NA_character_
        # 4) Set agg_sector values that are "MISSING" to NA_character_
        not_total_consump_includes_int_bunk_instructions <- instruction_df_with_aggsector %>%
            dplyr::filter( agg_sector != "all", !is.na( exclude_int_bunkers ) )


        if( nrow( not_total_consump_includes_int_bunk_instructions ) != 0 ){

            warning( "User instructions for ", instr_dfile,
                     ".csv indicate that the user has attempted to provide a value for ",
                     "-exclude_int_bunkers- in at least one row where -agg_sector- is not ",
                     "provided or not set to -all-. The system is ignoring these values ",
                     "as values for -exclude_int_bunkers- are only needed when agg_sector ",
                     "is set to -all-, indicating that TOTAL fuel consumption ",
                     "data has been provided by the user." )

            instruction_df_with_aggsector <- instruction_df_with_aggsector %>%
                dplyr::mutate( exclude_int_bunkers = if_else( agg_sector != "all" &
                                                              !is.na( exclude_int_bunkers ),
                                                              NA_character_, exclude_int_bunkers ) )

        }

        instruction_df_with_aggsector <- instruction_df_with_aggsector %>%
            dplyr::mutate( agg_sector = if_else( agg_sector == "MISSING", NA_character_, agg_sector ) )

        # Convert exclude_int_bunkers to class "logical"
        instruction_df_with_aggsector <- instruction_df_with_aggsector %>%
            dplyr::mutate( exclude_int_bunkers = as.logical( exclude_int_bunkers ) )

        instruction_df <- instruction_df_with_aggsector

        # Stop if any values for exclude_int_bunkers are still not TRUE or FALSE or NA
        instruction_df_wrong_exclude_intstructions <- instruction_df %>%
            dplyr::filter( !is.na( exclude_int_bunkers ) &
                           exclude_int_bunkers %!in% c( TRUE, FALSE, T, F ) )

        if( nrow( instruction_df_wrong_exclude_intstructions ) != 0 ){

            stop( paste0 ( "All values for exclude_int_bunkers should be either NA, T,TRUE, F, or FALSE. ",
                  "See the function cleanInstructions..." ) )

        }

        # Stop if any rows which do not have "all" for agg_sector contain TRUE or FALSE
        # for exclude_int_bunkers
        check_non_total_consumption_data <- instruction_df %>%
            dplyr::filter( agg_sector != "all", exclude_int_bunkers %in% c( TRUE, FALSE, T, F ) )

        if( nrow( check_non_total_consumption_data ) != 0 ){

            stop( paste0( "There are non-NA Values for exclude_int_bunkers for rows where ",
                          "agg_sector is not equal to -all-. This shouldn't occur. See the ",
                          "function cleanInstructions..." ) )

        }

        # Stop if missing iso and remove any invalid instructions (missing iso)
        invld_instr <- is.invalid( instruction_df$iso )
        instruction_df <- instruction_df[ !invld_instr, ]
        if ( any( invld_instr ) )
            stop ( paste0( sum( invld_instr ) , " instruction(s) invalid in ",
                           instr_dfile, "-instructions.csv", " , missing iso." ) )

        instruction_df[ instruction_df == 'all' ] <- NA_character_

        instruction_df <- removeNonComb( instruction_df, comb_sectors_only )

        # Stop if missing keep_total_cols column in instructions file
        if ( is.null( instruction_df$keep_total_cols) )
            stop( paste0 ( instr_dfile, "-instructions.csv is missing keep_total_cols",
                          " - stop and add this column as it is required." ) )

        # If keep_total_cols is not of class character, make it so
        if( !is.character( instruction_df$keep_total_cols ) ){

            instruction_df <- instruction_df %>%
                dplyr::mutate( keep_total_cols = as.character( keep_total_cols ) )

        }

        # Split aggregation levels into a vector
        instruction_df <- instruction_df %>%
            dplyr::mutate( keep_total_cols = if_else( is.na( keep_total_cols ), "NA",
                                                      keep_total_cols ) ) %>%
            dplyr::mutate( keep_total_cols = strsplit( keep_total_cols, "," ) ,
                           keep_total_cols = lapply( keep_total_cols, gsub, pattern = " ",
                                                    replacement = "" ) )

        # Map on higher aggregation levels based on what is provided
        # and test for appropriate normalization columns
        for ( i in seq_len( nrow( instruction_df ) ) ) {

            row <- instruction_df[i,]

            keep_total_cols <- row$keep_total_cols[[1]]

            if ( "CEDS_fuel" %in% keep_total_cols ) {
                keep_total_cols <- union( "agg_fuel", keep_total_cols )
            }

            if ( "CEDS_sector" %in% keep_total_cols ) {
                keep_total_cols <- union( "agg_sector", keep_total_cols )
            }

            if( !( all( keep_total_cols %in% c( "NA", CEDS_cols ) ) ) ) {
                stop( paste0 ( instr_dfile, "-instructions.csv includes columns in keep_total_cols",
                                " that are not supported." ) )
            }

            # Need to test for if provide a fuel and sector level which is less detailed
            # than keep_total_cols, as data will not be normalized in this case
            user_cols <- CEDS_cols[ !( is.na( row[ , CEDS_cols ] ) ) ]

            if ( ! ( all(keep_total_cols %in% c( "NA", user_cols ) ) ) ){
                stop( paste0 ( instr_dfile, "-instructions.csv includes more detail in keep_total_cols",
                               " than is provided by the user data itself." ) )
            }

            instruction_df$keep_total_cols[[i]] <- keep_total_cols
        }

        # Remove any invalid instructions (missing agg_fuel)
        invld_instr <- is.invalid( instruction_df$agg_fuel)
        instruction_df <- instruction_df[ !invld_instr, ]
        if ( any( invld_instr ) )
            stop ( paste0( sum( invld_instr ) , " instruction(s) invalid in ",
                             instr_dfile, "-instructions.csv", " , missing agg_fuel" ) )
        instruction_df

    } )

    # Combine instructions into a single data frame
    all_instructions <- rbind.fill( instruction_list )

    return( all_instructions )

}

