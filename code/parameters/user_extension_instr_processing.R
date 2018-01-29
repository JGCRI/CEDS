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


# getInstructionLevel
getInstructionLevel <- function(all_instr, level) {
    if ( level %in% names( all_instr ) ) {
        instructions <- all_instr[ !is.na( all_instr[[ level ]] ), ] %>%
            dplyr::arrange(iso) %>%
            dplyr::arrange(-end_year) %>%
            dplyr::arrange(-priority)
        return(instructions)
    }
    else {
        return(NULL)
    }
}


# getInstructionFilenames (without the .xlsx extension)
getInstructionFilenames <- function() {
    USER_DOM <- "../input/extension/user-defined-energy/"

    # Get a list of all the files in the directory
    files_present <- list.files( USER_DOM )
    if ( length( files_present ) == 0 ) {
        message("No user defined energy files found; using default data")
    }

    # The regex "^(?!~\\$)" ignores "~$": Excel's autosave files
    files <- grep( "^(?!~\\$).*-instructions", files_present, perl = T, value = T )
    files <- sub( ".xlsx", "", files ) 
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
    instr_files <- paste0( "user-defined-energy/" , instr_names ) %>% 
                   sapply( readData, domain = "EXT_IN", extension = ".xlsx",
                           simplify = F) %>% 
                   setNames( sub( "-instructions", "", instr_names ) )
    
    return( instr_files )
}

# processTrendInstructions
# Prepares the raw trend instruction for use in the main processing loop.
# Outputs a dataframe containing all user instructions
processTrendInstructions <- function( instructions, comb_sectors_only, MSL, MFL ) {
   
    # Extract the trend instructions, add the file they came from, and map to
    # the standard CEDS format
    instruction_list <- lapply( seq_along( instructions ), function( i ) {
        instruction <- instructions[[i]]$Trend_instructions
        instr_dfile <- names( instructions )[i]
        instruction$data_file <- instr_dfile
        
        mapped <- mapToCEDS( instruction, MSL, MFL, aggregate = F )
        instruction[ names( mapped ) ] <- mapped
        return( instruction )
    })

    # Combine all instructions into a single dataframe
    all_instructions <- rbind.fill( instruction_list )

    # Add in any aggregation levels the user has not provided
    all_cols <- c( "iso", "agg_fuel", "agg_sector", "CEDS_fuel", "CEDS_sector" )
    all_instructions[ all_cols %!in% names( all_instructions ) ] <- NA
    
    all_instructions <- removeNonComb( all_instructions, comb_sectors_only )
    
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

    # TODO: allow this to be a real argument?
    all_instructions$bypass_processing <- F

    return( all_instructions )
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
# Removes any non-combustion data, as that is not supported
removeNonComb <- function( df, comb_sectors_only ) {
    num_instructions <- nrow( df )
    df <- dplyr::filter( df, is.na( CEDS_sector ) |
                             CEDS_sector %in% comb_sectors_only )
    num_removed = num_instructions - nrow( df )
    
    if ( num_removed > 0 ) {
        warning( paste( num_removed, "instruction line(s) were rejected as",
                        "non-combustion sectors" ) )
    }
    return( df )    
}