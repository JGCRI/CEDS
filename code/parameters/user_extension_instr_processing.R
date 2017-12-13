#------------------------------------------------------------------------------
# Program Name: user_extension_instr_processing.R
# Author: Ben Goldstein
# Date Last Updated: 9 June 2017
# Program Purpose: To process user-defined datasets for use in the historical
#                  energy extension.
# Input Files: U.*, U.*-instructions, U.*-mapping
# Output Files: None
# Functions Defined:
# Notes:
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

# getInstructionLevel
getInstructionLevel <- function(all_instr, level) {
    if (level %in% names(all_instr)) {
        instructions <- all_instr[ which( !is.na( all_instr[[level]] ) ), ] %>%
            dplyr::arrange(iso) %>%
            dplyr::arrange(-end_year) %>%
            dplyr::arrange(-priority)
        return(instructions)
    }
    else {
        return(NULL)
    }

}

# getInstructionFilenames
getInstructionFilenames <- function() {
    USER_DOM <- "../input/extension/user-defined-energy/"

    # Get a list of all the files in the directory
    files_present <- list.files( USER_DOM )
    if ( length( files_present ) == 0 ) {
        message("No user defined energy files found; using default data")
    }

    # The regex "^(?!~\\$)" ignores "~$": Excel's autosave files
    files <- grep( "^(?!~\\$).*-instructions", files_present, perl = T, value = T )
    return ( files )
}

# readInUserInstructions
# Searches the user-defined-energy directory for instructions, filters out non-
# combustion data, and adds defaults.
readInUserInstructions <- function () {

    comb_or_NC <- readData( "Master_Fuel_Sector_List", domain = "MAPPINGS",
                            extension = ".xlsx", sheet_selection = "Sectors" )
    comb_sectors_only <- comb_or_NC$sector[ which( comb_or_NC$type == "comb" ) ]
    instruction_files <- getInstructionFilenames()
    all_instructions <- NULL

    for ( file in instruction_files ) {
        data_file <- gsub( "-instructions.xlsx", "", file )

        use_instructions <- readData( paste0( "user-defined-energy/", data_file, "-instructions" ),
                                      domain = "EXT_IN", extension = '.xlsx',
                                      sheet_selection = "Trend_instructions" )
        use_instructions$data_file <- data_file

        # Remove any non-combustion data as that is not supported
        if ( "CEDS_sector" %in% names( use_instructions ) ) {
            nc_rows <- dplyr::filter( use_instructions,
                                      !is.na(CEDS_sector) & CEDS_sector %!in% comb_sectors_only)
            if ( nrow( nc_rows ) > 0 ) {
                use_instructions <- dplyr::setdiff( use_instructions, nc_rows )
                warning( paste0( nrow( nc_rows )," instruction line(s) were rejected as non-combustion sectors" ) )
            }
        }

        all_instructions <- rbind.fill( all_instructions, use_instructions )
    }

    # First, determine batches. How will we indicate that a group of items is in a batch?
    ### The solution is probably just to sort them in a way that batched items are together...
    ### Okay, what makes a batch? A batch is any items that are a) on the same aggregation level
    ###    and b) belong to the same aggregate category.

    level_1_instructions <- getInstructionLevel(all_instructions, "agg_fuel")
    level_2_instructions <- getInstructionLevel(all_instructions, "CEDS_fuel")
    level_3_instructions <- getInstructionLevel(all_instructions, "agg_sector")
    level_4_instructions <- getInstructionLevel(all_instructions, "CEDS_sector")

    all_instructions <- rbind( level_1_instructions, level_2_instructions, level_3_instructions, level_4_instructions )
    all_instructions <- unique( all_instructions )

    # TODO: add options for bypass_processing, use_as_trend, and match_year
    # Add defaults for optional use instructions.
    opts <- list( #priority = integer(0),
                  override_normalization = FALSE,
                  use_as_trend = FALSE,
                  #match_year = integer(0),
                  start_continuity = TRUE,
                  end_continuity = TRUE,
                  specified_breakdowns = FALSE )
    opts_present <- intersect( names( all_instructions ), names ( opts ) )
    opts_missing <- setdiff( names( opts ), opts_present )

    # Replace missing options with their default values
    sapply( opts_missing, function(o) {
        all_instructions[[o]] <<- opts[[o]]
    })

    # Cast existing options to the correct type and replace NAs
    sapply( opts_present, function(o) {
        all_instructions[[o]][is.na(all_instructions[[o]])] <<- opts[[o]]
        class(all_instructions[[o]]) <<- class(opts[[o]])
    })

    all_instructions$bypass_processing <- F

    return ( all_instructions )
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
    # user's data set.
    for ( column in c("iso", matches) ) {
        instructions <- dplyr::filter(instructions, UQ(as.name(column)) %in% usrdf[[column]])
    }

    for ( column in missing ) {
        instructions <- dplyr::filter(instructions, is.na(UQ(as.name(column))))
    }

    return (instructions)
}



