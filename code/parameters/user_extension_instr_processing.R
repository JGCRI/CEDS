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
# Sorts the instructions in order to ensure that all actions are performed in the right order
    orderInstructions <- function( instructions ) {

        instructions <- instructions %>%
                        dplyr::arrange( start_year ) %>%
                        dplyr::arrange( CEDS_fuel ) %>%
                        dplyr::arrange( CEDS_sector ) %>%
                        dplyr::arrange( priority )

        return( instructions )
    }

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

    readInUserInstructions <- function () {
    # At this point in the process, data has been maped & cleaned.
    # We need to build an instructions dataframe to pass forward.
        USER_DOM <- "../input/extension/user-defined-energy/"

    # Get a list of all the files in the directory
        files_present <- list.files( USER_DOM )
        if ( length( files_present ) == 0 ) {
            stop("No user defined energy files found")
        }

        # The regex "^(?!~\\$)" ignores "~$": Excel's autosave files
        instruction_files <- grep( "^(?!~\\$).*-instructions", files_present, perl = T, value = T )
        comb_or_NC <- readData( "Master_Fuel_Sector_List", domain = "MAPPINGS", extension = ".xlsx",
                                                        sheet_selection = "Sectors" )
        comb_sectors_only <- comb_or_NC$sector[ which( comb_or_NC$type == "comb" ) ]

    # For each file in the instructions folder, read it in and collect important info:
    #    Name of the file minus "-instructions" tells us what data it's connected to
    #    Year range
    #    Iso, sector, fuel affected (in CEDS form--I think it's ok to demand this be mapped for us)
    #    Type of data (final totals? trend? etc)
    #    Priority?

        all_instructions <- NULL

        for ( file in instruction_files ) {
            data_file <- gsub( "-instructions.xlsx", "", file )

            use_instructions <- readData( paste0( "user-defined-energy/", data_file, "-instructions" ), domain = "EXT_IN", extension = '.xlsx',
                                          sheet_selection = "Trend_instructions" )

            if ( "CEDS_sector" %in% colnames( use_instructions ) ) {
                all_rows <- nrow( use_instructions )
                use_instructions <- use_instructions[ which ( use_instructions$CEDS_sector %in% comb_sectors_only |
                                                                is.na(use_instructions$CEDS_sector)), ]
                if ( nrow( use_instructions ) != all_rows ) {
                    warning( paste0( all_rows-nrow( use_instructions )," instruction line(s) were rejected as non-combustion sectors" ) )
                }
            }

            if ( is.null( all_instructions ) ) {
                all_instructions <- use_instructions
                all_instructions$data_file <- data_file
            }
            else {
                all_instructions <- rbind.fill( all_instructions, use_instructions )
                all_instructions$data_file[ which( is.na( all_instructions$data_file ) ) ] <- data_file
            }
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

        if ( "start_continuity" %!in% colnames(all_instructions) ) {
            all_instructions$start_continuity <- T
        } else {
            all_instructions$start_continuity <- as.logical(all_instructions$start_continuity)
        }
        if ( "end_continuity" %!in% colnames(all_instructions) ) {
            all_instructions$end_continuity <- T
        } else {
            all_instructions$end_continuity <- as.logical(all_instructions$end_continuity)
        }
        all_instructions$start_continuity[ is.na(all_instructions$start_continuity) ] <- T
        all_instructions$end_continuity[ is.na(all_instructions$end_continuity) ] <- T


        if ( "override_normalization" %!in% colnames(all_instructions) ) {
            all_instructions$override_normalization <- T
        } else {
            all_instructions$override_normalization <- as.logical(all_instructions$override_normalization)
        }
        all_instructions$override_normalization[ is.na(all_instructions$override_normalization) ] <- F

        if ( "use_as_trend" %!in% colnames(all_instructions) ) {
            all_instructions$use_as_trend <- F
        } else {
            all_instructions$use_as_trend <- as.logical(all_instructions$use_as_trend)
        }
        all_instructions$use_as_trend[ is.na( all_instructions$use_as_trend ) ] <- F

        if ( "specified_breakdowns" %!in% colnames(all_instructions) ) {
            all_instructions$specified_breakdowns <- F
        } else {
            all_instructions$specified_breakdowns <- as.logical(all_instructions$specified_breakdowns)
        }
        all_instructions$specified_breakdowns[ is.na( all_instructions$specified_breakdowns ) ] <- F

        all_instructions$bypass_processing <- F

        return ( all_instructions )
    }


