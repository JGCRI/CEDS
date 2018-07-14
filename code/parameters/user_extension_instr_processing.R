#------------------------------------------------------------------------------
# Program Name: user_extension_instr_processing.R
# Author: Ben Goldstein
# Date Last Updated: 26 January 2018
# Program Purpose: Provides functions for the add_user-defined_data script that
#                  help process the instructions for handling user-defined
#                  datasets.
# Input Files: U.*, U.*-instructions, U.*-mapping
# Output Files: None
# ------------------------------------------------------------------------------------


# orderInstructions
# Sorts user instructions ensure that all actions are performed in the right
# order. The rows are arranged in ascending order by:
#   1. priority
#   2. CEDS_sector
#   3. CEDS_fuel
#   4. start_year
orderInstructions <- function( instructions ) {
    dplyr::arrange( instructions, priority, CEDS_sector, CEDS_fuel, start_year )
}


# getInstructionFilenames
# Instruction files are specified as all files in the user-defined-energy
# directory with the name [filename]-instructions.csv
getInstructionFilenames <- function() {
    USER_DOM <- "extension/user-defined-energy/"

    # Get a list of all the files in the directory
    files_present <- list.files( USER_DOM )
    if ( !length( files_present ) )
        message( "No user defined energy files found; using default data" )

    files <- grep( ".+-instructions.csv", files_present, value = T )
    files <- sub( ".csv", "", files )
    return( files )
}


# readInUserInstructions
# Searches the user-defined-energy directory for instructions, filters out non-
# combustion data, and adds defaults.
#
# Returns a list of lists. Outer list is named the base filename of the
# instructions. Inner lists contain two dataframes, one named Trend_instructions
# and the other Interpolation_instructions.
readInUserInstructions <- function() {

    instr_names <- getInstructionFilenames()
    if ( length( instr_names ) == 0 ) return( NULL )
    instr_files <- paste0( "user-defined-energy/" , instr_names ) %>%
                   sapply( readData, domain = "EXT_IN", simplify = F ) %>%
                   setNames( sub( "-instructions", "", instr_names ) )

    return( instr_files )
}


# processInstructions
# Prepares the raw trend instructions for use in the main processing loop.
# Outputs a dataframe containing all user instructions
processInstructions <- function( comb_sectors, MSL, MFL, default_activity ) {

    # Get list of all '-instructions.csv' files in the user-defined-energy dir
    instructions <- readInUserInstructions()

    # Puts all instructions into single dataframe with uniform columns
    instructions <- cleanInstructions( instructions, comb_sectors, MSL, MFL )

    # Preprocess any data that needs it
    instructions <- preprocUserData( instructions )

    # Add defaults for optional use instructions.
    # TODO: add options for use_as_trend and match_year
    opts <- list( priority = NA_integer_,
                  override_normalization = FALSE,
                  use_as_trend = FALSE,
                  #match_year = integer(0),
                  start_continuity = TRUE,
                  end_continuity = TRUE,
                  specified_breakdowns = FALSE )
    opts_present <- intersect( names( instructions ), names ( opts ) )
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

    instructions <- mapToUserSectors( instructions, default_activity ) %>%
        addAggregateCol( 'sector', MSL, 'aggregate_sectors', 'CEDS_sector' )

    return( instructions )
}


# extractBatchInstructions
# Given a dataframe of instructions and a dataframe of energy extension values,
# return the rows from the instructions dataframe that apply to the user data.
extractBatchInstructions <- function( instructions, usrdf, agg_level ) {

    if ( agg_level == 1 | agg_level == 2 ) {
        matches <- "agg_fuel"
        missing <- "agg_sector"
    }
    else if ( agg_level == 3 )
        matches <- c( "agg_fuel", "agg_sector" )
    else if ( agg_level == 4 )
        matches <- c( "CEDS_sector", "agg_fuel" )
    else if ( agg_level == 5 )
        matches <- c( "agg_fuel", "agg_sector" )
    else if ( agg_level == 6 ) {
        matches <- "agg_fuel"
        missing <- "CEDS_sector"
    }
    else
        return( NULL )

    # Filters the instructions to only the ones with corresponding data in the
    # user's data set. Always filter to match iso.
    for ( column in c("iso", matches) ) {
        instructions <- dplyr::filter(instructions, UQ(as.name(column)) %in% usrdf[[column]])
    }

    if ( is.character( missing ) )
        instructions <- dplyr::filter(instructions, is.na(UQ(as.name(missing))))

    return( instructions )
}


# removeNonComb
# Removes any non-combustion data, as that is not supported.
removeNonComb <- function( df, comb_sectors_only ) {
    num_instructions <- nrow( df )
    df <- dplyr::filter( df, is.na( CEDS_sector ) |
                             CEDS_sector %in% comb_sectors_only)
    num_removed = num_instructions - nrow( df )

    if ( num_removed > 0 ) {
        warning( paste( num_removed, "instruction line(s) were rejected as",
                        "non-combustion sectors" ) )
    }
    return( df )
}


cleanInstructions <- function( instructions, comb_sectors_only, MSL, MFL ) {
    # Make sure instructions are a given as a list of data.frames
    stopifnot( all( sapply( instructions, is.data.frame ) ) )

    # Extract the trend instructions, add the file they came from, and map to
    # the standard CEDS format
    instruction_list <- lapply( seq_along( instructions ), function( i ) {
        # Extract and add source file
        instruction <- instructions[[i]]
        instr_dfile <- names( instructions )[i]
        instruction$data_file <- instr_dfile

        # Map
        mapped <- mapToCEDS( instruction, MSL, MFL, aggregate = F )
        instruction[ names( mapped ) ] <- mapped

        # Remove any invalid instructions
        invld_instr <- is.na( instruction$iso ) | is.na( instruction$agg_fuel )
        instruction <- instruction[ !invld_instr, ]
        if ( any( invld_instr ) )
            warning( paste0( sum( invld_instr ), " instruction(s) invalid in ",
                             instr_dfile, "-instructions.csv" ) )
        instruction
    })

    # Combine instructions into a single data frame
    all_instructions <- rbind.fill( instruction_list )

    # Add in any aggregation levels the user has not provided (iso and agg_fuel
    # are required and should be present from calling mapToCEDS).
    # Note that an instruction requesting all sectors or fuels to be included is
    # the same as leaving out that column.
    stopifnot( c( "iso", "agg_fuel" ) %in% names( all_instructions ) )
    add_cols <- c( "agg_sector", "CEDS_fuel", "CEDS_sector" )
    all_instructions[ setdiff( add_cols, names( all_instructions ) ) ] <- NA
    all_instructions[ all_instructions == 'all' ] <- NA
    all_instructions <- removeNonComb( all_instructions, comb_sectors_only )

    return( all_instructions )
}

