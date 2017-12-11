#------------------------------------------------------------------------------
# Program Name: user_data_proc_pseudocode.R
# Author: Ben Goldstein
# Date Last Updated: 11 July 2017
# Program Purpose: To process user-defined datasets for use in the historical
#                  energy extension.
# Input Files: U.*.csv, U.*-instructions.xslx, U.*-mapping.xslx
# Output Files: None
# Notes:
### TODO: If the function gives an error, we should not end the whole system,
###       all we need to do is reject this user-defined dataset and proceed
###       to the next one... Unless this would make it hard for the user
###       to see that their changes are being rejected?
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
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
    if ( is.na( em ) ) em <- "BC"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R" ,"emissions_scaling_functions.R" , "analysis_functions.R",
                  "interpolation_extension_functions.R", "user_data_processing.R",
                  "user_extension_instr_processing.R", "user_data_inclusion_functions.R" ) # Additional function files required.
    log_msg <- paste0( "Calling inventory emission scaling stripts" ) # First message to be printed to the log
    script_name <- paste0( "add_user-defined_data.R" )

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

        agg_level <- names(dataframe)

        if (      "CEDS_sector" %in% agg_level &
                  "CEDS_fuel"   %in% agg_level ) return( 4 )
        else if ( "CEDS_sector" %in% agg_level ) return( 5 )
        else if ( "CEDS_fuel"   %in% agg_level &
                  "agg_sector"  %in% agg_level ) return( 3 )
        else if ( "CEDS_fuel"   %in% agg_level ) return( 2 )
        else if ( "agg_sector"  %in% agg_level ) return( 6 )
        else if ( "agg_fuel"    %in% agg_level ) return( 1 )
        else return(0)
    }

# is.invalid
# Brief: This is a helper function for the add_user-defined_data script.
#        Combination check for if a value is null, NA, or NaN
# params:
#    x: some non-list object
    is.invalid = function(x) {
      return( is.null(x) || is.na(x) || is.nan(x) )
    }

# is.nan.df
# Brief: This is a helper function for the add_user-defined_data script.
#        Makes up for the fact that R has no built-in vectorized check
#        for NaN.
# params:
#    x: a dataframe that may or may not contain NaN values
    is.nan.df <- function(x) {
        do.call( cbind, lapply(x, is.nan) )
    }

# subsetUserData
# Brief: This is a helper function for the add_user-defined_data script.
#        Subsets a user-specified dataset based on user-specified instructions,
#        so the only data processed is the data specified in instructions.
# params:
#    user_df:
#    working_instructions:
    subsetUserData <- function( user_df, working_instructions ) {

    # initialize a subset dataframe
        subset <- user_dataframe[ which( user_dataframe$iso %in% working_instructions$iso ), ]

    # Subset the dataframe based on which columns are present and filled out in the dataframe
        if ( !is.invalid( working_instructions$CEDS_sector ) && working_instructions$CEDS_sector != 'all' ) {
          subset <- subset[ which ( subset$CEDS_sector %in% working_instructions$CEDS_sector ), ]
        } else if ( !is.invalid( working_instructions$agg_sector ) && working_instructions$agg_sector != 'all' ) {
          subset <- subset[ which ( subset$agg_sector %in% working_instructions$agg_sector ), ]
        }

        if ( !is.invalid( working_instructions$CEDS_fuel ) && working_instructions$CEDS_fuel != 'all' ) {
          subset <- subset[ which ( subset$CEDS_fuel %in% working_instructions$CEDS_fuel ), ]
        }
        else if ( !is.invalid( working_instructions$agg_fuel ) && working_instructions$agg_fuel != 'all' ) {
          subset <- subset[ which ( subset$agg_fuel %in% working_instructions$agg_fuel ), ]
        }
        return( subset )
    }

# ------------------------------------------------------------------------------------
# 1. Read in data

    MSL <- readData( "Master_Sector_Level_map", domain = "MAPPINGS" )
    names( MSL )[ names( MSL ) == 'working_sectors_v1' ] <- 'CEDS_sector'
    MCL <- readData( "Master_Country_List", domain = "MAPPINGS" )
    MFL <- readData( "Master_Fuel_Sector_List", domain = "MAPPINGS",
                     extension = ".xlsx", sheet_selection = "Fuels" )
    comb_or_NC <- readData( "Master_Fuel_Sector_List", domain = "MAPPINGS",
                            extension = ".xlsx", sheet_selection = "Sectors" )
    comb_sectors_only <- comb_or_NC$sector[ comb_or_NC$type == "comb" ]


# Gather default activity data
    default_activity <- readData( 'MED_OUT', paste0( 'H.', em, '_total_activity_extended_db' ) , meta = F) ### Eventually this will not require an emissions species.
    colnames( default_activity )[ 1:3 ] <- c( "iso", "CEDS_sector", "CEDS_fuel" )
    default_activity_mapped <- mapToCEDS( default_activity, MSL, MCL, MFL, aggregate = F )

    # We only operates on combustion emissions, so reduce the data to that form
    combustion_rows <- default_activity_mapped$CEDS_sector %in% comb_sectors_only
    all_activity_data <- default_activity_mapped[which(combustion_rows), ]

# ------------------------------------------------------------------------------------
# 2. Collect user-defined inputs and prepare processing loop

# Read instructions files and create a procedure list
    instructions <- readInUserInstructions()

    # The user may not have instructions covering all aggregation levels
    sapply(c("agg_fuel", "agg_sector", "CEDS_fuel", "CEDS_sector"), function(l) {
        if (l %!in% names(instructions)) instructions[[l]] <<- NA
    })

    # If the user data specifies CEDS_fuel but not the aggregate fuel type,
    # join in the aggregated fuel
    aggNA <- which(is.na(instructions$agg_fuel) & !is.na(instructions$CEDS_fuel))
    if (length(aggNA) > 0) {
        instructions[aggNA, ] <- instructions[aggNA, ] %>%
            dplyr::left_join(MFL[ , c("aggregated_fuel", "fuel")],
                             by = c("CEDS_fuel" = "fuel")) %>%
            dplyr::mutate(agg_fuel = aggregated_fuel) %>%
            dplyr::select(-aggregated_fuel)
    }


# Initialize script variables
    # Years the user is allowed to add data to
    yearsAllowed <- colnames( all_activity_data )[ isXYear(colnames( all_activity_data ))]

    # Master list used to track activity data. Contains three dataframes:
    # 1. all_activity_data: changed activity data
    # 2. old_activity_data: unchanged (the original) activity data
    # 3. continuity_factors
    activity <- list()
    activity$all_activity_data <- all_activity_data
    activity$old_activity_data <- all_activity_data
    activity <- initializeContinuityFactors( activity, instructions, yearsAllowed )

    # The user provided instructions for all supplemental data
    instructions <- processTrendData( instructions, all_activity_data )

    # This will store the final form of each instruction used, for diagnostics
    rows_completed <- instructions[ 0, ]

    # This integer will track which batch number we're on, for informing diagnostics
    batch <- 0

# ------------------------------------------------------------------------------------
# 3. Execute processing loop


    while ( nrow( instructions ) > 0 ) {

        all_activity_data <- activity$all_activity_data

        batch <- batch + 1

        # Select and remove the last instruction in the list for processing
        instructions <- orderInstructions( instructions )
        working_instructions <- instructions[ nrow( instructions ), ]
        instructions <- instructions[ -nrow( instructions ), ]

        Xyears <- paste0( "X", working_instructions$start_year:working_instructions$end_year )

        # Filter out any years not in the CEDS range
        if ( any( Xyears %!in% yearsAllowed )) {
            warning(paste("Some data in", working_instructions$data_file,
                          "are not in the allowed CEDS years and will be ignored"))
            Xyears <- Xyears[ which( Xyears %in% yearsAllowed ) ]
        }

        if ( working_instructions$bypass_processing ) {
            user_dataframe <- readData( working_instructions$data_file, domain = "EXT_IN",
                                        domain_extension = "user-defined-energy/" )
        } else {
            # The processUserDefinedData function executes mapping and
            # interpolation as necessary
            user_dataframe <- processUserDefinedData( working_instructions$data_file,
                                                      MSL, MCL, MFL )
        }

        # Extract the rows from the user's dataframe refers to the specific
        # categories and years as defined by the current instructions
        user_dataframe_subset <- subsetUserData( user_dataframe, working_instructions )

        agg_level <- identifyLevel( user_dataframe )

        # Identify other instructions in the "batch" that will need to be aggregated as one.
        if ( agg_level == 4 ) {
            batch_data_instructions <- instructions[ which( instructions$iso %in% user_dataframe_subset$iso &
                                                            instructions$CEDS_sector %in% user_dataframe_subset$CEDS_sector &
                                                            instructions$agg_fuel
                                                                        %in% user_dataframe_subset$agg_fuel ), ]
            instructions <- instructions[ which( instructions$iso %!in% user_dataframe_subset$iso |
                                                    instructions$CEDS_sector %!in% user_dataframe_subset$CEDS_sector |
                                                    instructions$agg_fuel %!in% user_dataframe_subset$agg_fuel ), ]

        } else if ( agg_level == 3 ) {
            batch_data_instructions <- instructions[ which( instructions$iso %in% user_dataframe_subset$iso &
                                                            instructions$agg_sector %in% user_dataframe_subset$agg_sector &
                                                            instructions$agg_fuel
                                                                        %in% user_dataframe_subset$agg_fuel ), ]
            instructions <- instructions[ which( instructions$iso %!in% user_dataframe_subset$iso |
                                                   instructions$agg_sector %!in% user_dataframe_subset$agg_sector |
                                                   instructions$agg_fuel %!in% user_dataframe_subset$agg_fuel ), ]
        } else if ( agg_level == 6 ) {
            batch_data_instructions <- instructions %>%
                                       dplyr::filter( iso      %in% user_dataframe_subset$iso,
                                                      agg_fuel %in% user_dataframe_subset$agg_fuel,
                                                      is.na( CEDS_sector ) )
            instructions <- rbind( instructions %>% filter( iso %!in% user_dataframe_subset$iso ),
                                   instructions %>% filter( agg_fuel %!in% user_dataframe_subset$agg_fuel ),
                                   instructions %>% filter( !is.na( CEDS_sector ) ) )
            instructions <- unique( instructions )
        } else if ( agg_level %in% c( 1, 2 ) ) {
            batch_data_instructions <- instructions %>% filter( iso %in% user_dataframe_subset$iso ) %>%
                                                    filter( agg_fuel %in% user_dataframe_subset$agg_fuel ) %>%
                                                    filter( is.na( agg_sector ) )
            instructions <- rbind( instructions %>% filter( iso %!in% user_dataframe_subset$iso ),
                                   instructions %>% filter( agg_fuel %!in% user_dataframe_subset$agg_fuel ) )
                                   instructions %>% filter( !is.na( agg_sector ) )
            instructions <- unique( instructions )
        } else if ( agg_level == 5 ) {
            batch_data_instructions <- instructions %>% filter( iso %in% user_dataframe_subset$iso ) %>%
                                                    filter( agg_fuel %in% user_dataframe_subset$agg_fuel ) %>%
                                                    filter( agg_sector %in% user_dataframe_subset$agg_sector )
            instructions <- rbind( instructions %>% filter( iso %!in% user_dataframe_subset$iso ),
                                   instructions %>% filter( agg_fuel %!in% user_dataframe_subset$agg_fuel ),
                                   instructions %>% filter( agg_sector %!in% user_dataframe_subset$agg_sector ) )
            instructions <- unique( instructions )
        }

    # Files only need to be batched if their year ranges overlap.
        batch_instructions_overlap <- batch_data_instructions[ which(
                              batch_data_instructions$start_year < working_instructions$end_year &
                              batch_data_instructions$end_year > working_instructions$start_year ), ]

    # Extract those that don't overlap and bind them back into instructions.
        batch_instructions_no_overlap <- batch_data_instructions[ which(
                              batch_data_instructions$start_year > working_instructions$end_year |
                              batch_data_instructions$end_year < working_instructions$start_year ), ]
        instructions <- rbind( batch_instructions_no_overlap, instructions )  ### Should sort instructions at this point

        batch_data_instructions <- batch_instructions_overlap

    # If there are data in our instructions that will be in the current group's batch:
        if ( nrow( batch_data_instructions ) > 0 ) {

        # If our years haven't been properly subdivided by year, we'll need to
        # subdivide the whole batch and return back to the beginning of the
        # loop. The goal of this process is to be able to process subdivisions
        # of datasets that only partially overlap.
            if ( length( unique( c(batch_data_instructions$start_year,
                                   working_instructions$start_year) ) ) > 1 ||
                 length( unique(  c(batch_data_instructions$end_year,
                                    working_instructions$end_year)  ) ) > 1 ) {
            # Identify all the breaks that will need to occur (each unique start
            # and end year)
                year_breaks <- unique( c( batch_data_instructions$start_year,
                                          working_instructions$start_year,
                                          batch_data_instructions$end_year + 1,
                                          working_instructions$end_year + 1 ) ) %>%
                                  sort()
            # Combine the working with the rest of the batch
                whole_batch <- rbind( working_instructions, batch_data_instructions )
                new_division_batch <- whole_batch[ 0, ]

            # For each break: create a new instruction for any instruction that
            # encompasses this year range
                for ( i in 1:( length( year_breaks ) - 1 ) ) {
                    new_year_span <- year_breaks[ i ]:( year_breaks[ i+1 ] - 1 )

                    rows_to_segment <- whole_batch[ which( whole_batch$start_year <=
                                                                  max( new_year_span ) &
                                                           whole_batch$end_year >=
                                                                  min( new_year_span ) ), ]
                    if ( i != 1 ) {
                        rows_to_segment$start_continuity[ which( rows_to_segment$start_year != year_breaks[ i ] ) ] <- F
                    }
                    if ( i != length( year_breaks ) - 1 ) {
                        rows_to_segment$end_continuity[ which( rows_to_segment$end_year != year_breaks[ i + 1 ] - 1 ) ] <- F
                    }

                    rows_to_segment$start_year <- min( new_year_span )
                    rows_to_segment$end_year <- max( new_year_span )

                    new_division_batch <- rbind( new_division_batch, rows_to_segment )

                }


            # Tack all the newly-divided instructions onto the instructions df
                instructions <- rbind( new_division_batch, instructions )
                next
            }
            print("Got here")
        # If our years are properly subdivided, we should re-retrieve our
        #   user_defined_data dataframe. This may require drawing on multiple
        #   source files.
            working_instructions <- rbind( working_instructions, batch_data_instructions )
            user_dataframe_subset <- user_dataframe_subset[ 0, ]
            for ( row_num in 1:nrow( working_instructions ) ) {
                file <- working_instructions$data_file[ row_num ]
                bypass <- working_instructions$bypass_processing[ row_num ]
            # Process the data if necessary...
                if ( !bypass ) {
                # call the processUserDefinedData function, which will execute mapping
                # and interpolation as necessary
                    user_dataframe <- processUserDefinedData( file, MSL, MCL, MFL )
            # Otherwise, read in the raw file...
                } else {
                    user_dataframe <- readData( file, domain = "EXT_IN",
                                                domain_extension = "user-defined-energy/" )
                }
            # ...and append the relevant part to the dataframe
                user_dataframe_subset <- rbind( subsetUserData( user_dataframe,
                                                                      working_instructions ) )
            }
        }

    # Identify the rows that will need adjusting. If the aggregation level is the lowest,
    #   this will be any rows that match to your row, and any row in the next-highest aggregation
    #   GROUP. For example, If I'm adjusting usa-coal_coke-1A1, I will need to edit all the
    #   usa-coal_coke-1A rows.
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
        } else if ( agg_level == 2 || agg_level == 1 ) {
            data_to_use <- all_activity_data[ which( all_activity_data$iso %in% user_dataframe_subset$iso &
                                                     all_activity_data$agg_fuel %in% user_dataframe_subset$agg_fuel
                                                     ), c( colnames( all_activity_data[ which( !isXYear(
                                                       colnames(all_activity_data) ) ) ] ), Xyears ) ]
        } else if ( agg_level == 5 ) {
            data_to_use <- all_activity_data %>% filter( agg_fuel %in% user_dataframe_subset$agg_fuel ) %>%
                                                 filter( iso %in% user_dataframe_subset$iso ) %>%
                                                 filter( agg_sector %in% user_dataframe_subset$agg_sector )
            data_to_use <- data_to_use[ , c(colnames( all_activity_data[ which( !isXYear(
                                                       colnames(all_activity_data) ) ) ] ), Xyears ) ]
        } else if ( agg_level == 6 ) {
            data_to_use <- all_activity_data %>% filter( agg_fuel %in% user_dataframe_subset$agg_fuel ) %>%
                                                 filter( iso %in% user_dataframe_subset$iso )
            data_to_use <- data_to_use[ , c(colnames( all_activity_data[ which( !isXYear(
                                                       colnames(all_activity_data) ) ) ] ), Xyears ) ]
        }

    # Initialize diagnostics as NA, so if the function fails or returns nothing
    # it will still exist
        diagnostics <- NA

    # Execute the normalizeAndIncludeData function in
    # user_data_inclusion_functions. This is the main point of the program; it
    # will normalize, disaggregate, and then incorporate the user-defined data
    # into activity$all_activity_data
        normalized <- normalizeAndIncludeData( Xyears, data_to_use, user_dataframe_subset,
                                               all_activity_data,
                                               working_instructions$override_normalization,
                                               agg_level, working_instructions$data_file,
                                               as.logical( working_instructions$specified_breakdowns ) )

        diagnostics <- normalized$diagnostics
        activity$all_activity_data <- normalized$all_data

    # Tack on some diagnostics to the working instructions dataframe for
    # diagnostic output
        working_instructions$batch_id <- batch
        working_instructions$agg_level <- agg_level

        if ( is.data.frame( diagnostics ) ) {
            working_instructions$nrow_changed <- diagnostics$rows_changed
            working_instructions$warnings <- diagnostics$warning_diag
        } else {
            working_instructions$nrow_changed <- 0
            working_instructions$warnings <- "Normalize and Include function not called"
        }
    # Add working instructions to rows_completed, which will be a diagnostic for
    # reviewing what changes occurred
        rows_completed <- rbind( rows_completed, working_instructions )
    }

# ------------------------------------------------------------------------------------
# 4. Write out the diagnostic data
    writeData( rows_completed, domain = "DIAG_OUT", fn = "user-ext-data_diagnostics" )

    final_activity <- enforceContinuity( activity, yearsAllowed )

    writeData( final_activity, domain = "MED_OUT", paste0("H.", em,"-total-activity-TEST"))
    writeData( final_activity[which(final_activity$iso == 'deu'), ], domain = "MED_OUT", paste0("H.", em,"-total-activity-TEST-small"))
    default_short <- default_activity[which(default_activity$iso == 'deu'), ]
    writeData( default_short, domain = "MED_OUT", paste0("H.", em,"-total-activity-original-short"))

    logStop()

#END


