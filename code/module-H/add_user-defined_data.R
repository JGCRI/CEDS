#------------------------------------------------------------------------------
# Program Name: add_user-defined_data.R
# Authors: Ben Goldstein, Caleb Braun
# Date Last Updated: January 2018
# Program Purpose: To process user-defined datasets for use in the historical
#                  energy extension.
# Input Files: U.*.csv, U.*-instructions.xslx, U.*-mapping.xslx
# Output Files: None
# Notes: Relies on funcitons from the following files:
#   - parameters/user_data_inclusion_functions.R
#   - parameters/user_data_processing.R
#   - parameters/user_extension_instr_processing.R
# TODO: Move data functions in this file to data_functions.R
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# 0. Read in global settings and headers
# TODO: Change this to match the new format in master
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
                  "user_extension_instr_processing.R", "user_data_inclusion_functions.R" ) # Additional function files required.
    log_msg <- paste0( "Calling inventory emission scaling stripts" ) # First message to be printed to the log
    script_name <- paste0( "add_user-defined_data.R" )

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )


# ------------------------------------------------------------------------------------
# 0.5 Define functions

# is.invalid
# Brief: This is a helper function for the add_user-defined_data script.
#        Combination check for if a value is null, NA, or NaN
# params:
#    x: some non-list object
    is.invalid <- function(x) {
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

# ------------------------------------------------------------------------------------
# 1. Read in data and filter out non-combustion data

    MSL <- readData( "Master_Sector_Level_map", domain = "MAPPINGS" )
    MCL <- readData( "Master_Country_List", domain = "MAPPINGS" )
    MFL <- readData( "Master_Fuel_Sector_List", domain = "MAPPINGS", extension = ".xlsx" )
    comb_or_NC <- MFL$Sectors
    comb_sectors_only <- comb_or_NC$sector[ comb_or_NC$type == "comb" ]
    MFL <- MFL$Fuels
    names( MSL )[ names( MSL ) == 'working_sectors_v1' ] <- 'CEDS_sector'

    # Gather default activity data
    default_activity <- readData( 'MED_OUT', paste0( 'H.', em, '_total_activity_extended_db' ) , meta = F) ### Eventually this will not require an emissions species.
    names( default_activity )[ 1:3 ] <- c( "iso", "CEDS_sector", "CEDS_fuel" )
    default_mapped <- mapToCEDS( default_activity, MSL, MFL, aggregate = F )

    # We only operate on combustion emissions, so reduce the data to that form
    combustion_rows <- default_mapped$CEDS_sector %in% comb_sectors_only
    all_activity_data <- default_mapped[ combustion_rows, ]

    # I don't know why there are NAs, but for now let's zero them out:
    all_activity_data[is.na(all_activity_data)] <- 0

# ------------------------------------------------------------------------------------
# 2. Collect user-defined inputs and prepare processing loop

    # Read instructions files that give the user-provided instructions for all
    # supplemental data.
    all_instr <- readInUserInstructions()
    instructions <- processInstructions( all_instr, comb_sectors_only, MSL, MFL )

    # TODO: Figure out how/where/why to process trend data without roundabout
    #       file writing and bypassing processing
    # instructions <- processTrendData( instructions, all_activity_data )

# Initialize script variables
    # Years the user is allowed to add data to
    all_yrs <- names( all_activity_data )[ isXYear( names( all_activity_data ) ) ]

    # Master list used to track activity data. Contains three dataframes:
    # 1. all_activity_data:    changed activity data
    # 2. old_activity_data:    unchanged (the original) activity data
    # 3. continuity_factors:   percent weight given to unchanged data
    activity <- list()
    activity$all_activity_data <- all_activity_data
    activity$old_activity_data <- all_activity_data
    activity <- addContinuityFactors( activity, instructions, all_yrs )


    # The lists usr_files and map_files hold the user provided data and the
    # associated mapping files. They are used as lookup tables, with the base
    # filename (e.g. without '-mapping' and '.csv') as the key.
    filenames <- unique( instructions$data_file )
    map_files <- sapply( filenames, readInUserData, all_yrs, '-mapping' )
    usr_files <- sapply( filenames, function( data_file ) {
        procUsrData( readInUserData( data_file, all_yrs ),
                     all_instr[[ data_file ]], map_files[[ data_file ]],
                     MSL, MCL, MFL, all_activity_data )
    })

    # This stores the final form of each instruction used, for diagnostics
    rows_completed <- instructions[ 0, ]

    # This integer tracks which batch number we're on, for informing diagnostics
    batch <- 0

# ------------------------------------------------------------------------------
# 3. Execute processing loop

    while ( nrow( instructions ) > 0 ) {
        # Update variables for each run of the loop
        batch <- batch + 1
        all_activity_data <- activity$all_activity_data
        instructions <- orderInstructions( instructions ) # Sort instructions

        # Process the last row (lowest priority, most aggregate) first
        working_instructions <- instructions[ nrow( instructions ), ]
        instructions <- instructions[ -nrow( instructions ), ]

        # Get the actual data referred to by this instruction
        data_file <- working_instructions$data_file
        user_dataframe <- usr_files[[ data_file ]]

        # Extract the rows from the user's dataframe refering to the specific
        # categories and years as defined by the current instruction
        usrdata <- subsetUserData( user_dataframe, working_instructions )

        s_year <- working_instructions$start_year
        e_year <- working_instructions$end_year
        Xyears <- all_yrs[ all_yrs %in% paste0( "X", s_year:e_year ) ]

        # Identify other instructions in the "batch" that will need to be
        # aggregated as one. Files only need to be batched if their year ranges
        # overlap.
        agg_level <- identifyLevel( user_dataframe )
        batch_instructions <- instructions %>%
                              extractBatchInstructions( usrdata, agg_level ) %>%
                              dplyr::filter( start_year < e_year, end_year > s_year)

        # Remove the batch instructions from the master instruction dataframe
        instructions <- dplyr::setdiff( instructions, batch_instructions )

        # Process the batch of instructions (if there is a batch)
        if ( nrow( batch_instructions ) > 0 ) {

            # If our years haven't been properly subdivided by year, we'll need
            # to subdivide the whole batch and return back to the beginning of
            # the loop. The goal of this process is to be able to process
            # subdivisions of datasets that only partially overlap.
            if ( length( unique( c( batch_instructions$start_year, s_year ) ) ) > 1 ||
                 length( unique( c( batch_instructions$end_year, e_year ) ) ) > 1 ) {
                # Identify all the breaks that will need to occur (each unique
                # start and end year)
                year_breaks <- unique( c( batch_instructions$start_year, s_year,
                                          batch_instructions$end_year + 1,
                                          e_year + 1 ) ) %>%
                               sort()
                # Combine the working with the rest of the batch
                whole_batch <- rbind( working_instructions, batch_instructions )
                new_division_batch <- whole_batch[ 0, ]

                # For each break: create a new instruction for any instruction
                # that encompasses this year range
                for ( i in 1:( length( year_breaks ) - 1 ) ) {
                    new_year_span <- year_breaks[ i ]:( year_breaks[ i+1 ] - 1 )

                    rows_to_segment <- whole_batch[ whole_batch$start_year <=
                                                      max( new_year_span ) &
                                                    whole_batch$end_year >=
                                                      min( new_year_span ), ]
                    if ( i != 1 ) {
                        rows_to_segment$start_continuity[ rows_to_segment$start_year != year_breaks[ i ] ] <- F
                    }
                    if ( i != length( year_breaks ) - 1 ) {
                        rows_to_segment$end_continuity[ rows_to_segment$end_year != year_breaks[ i + 1 ] - 1 ] <- F
                    }

                    rows_to_segment$start_year <- min( new_year_span )
                    rows_to_segment$end_year <- max( new_year_span )

                    new_division_batch <- rbind( new_division_batch, rows_to_segment )
                }

                # Tack all the newly-divided instructions onto the instructions df
                instructions <- rbind( new_division_batch, instructions )
                next
            }

            # From here we know our years are properly subdivided, so we should
            # re-retrieve our user_defined_data dataframe. This may require
            # drawing on multiple source files.
            working_instructions <- rbind( working_instructions, batch_instructions )
            usrdata <- usrdata[ 0, ]
            for ( row_num in 1:nrow( working_instructions ) ) {
                data_file <- working_instructions$data_file[ row_num ]
                user_dataframe <- usr_files[[ data_file ]]

                # Append the relevant part to the dataframe
                usrdata <- rbind( subsetUserData( user_dataframe,
                                                  working_instructions ) )
            }
        }

        data_to_use <- getRowsForAdjustment(all_activity_data, usrdata, MFL, agg_level)

        # Call the normalizeAndIncludeData function. This is the main point of
        # the program; it will normalize, disaggregate, and then incorporate the
        # user-defined data, returning a list with both the data and diagnostics
        normalized <- normalizeAndIncludeData( Xyears, data_to_use, usrdata,
                                               all_activity_data,
                                               working_instructions$override_normalization,
                                               agg_level, data_file,
                                               working_instructions$specified_breakdowns )

        activity$all_activity_data <- normalized$all_data
        diagnostics <- normalized$diagnostics

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
    #writeData( rows_completed, domain = "DIAG_OUT", fn = "user-ext-data_diagnostics" )

    final_activity <- enforceContinuity( activity, all_yrs )

    writeData( final_activity, domain = "MED_OUT", paste0("H.", em,"-total-activity-TEST"))
    final_short <- final_activity %>% dplyr::filter(iso == 'usa', agg_sector == '1A1_Energy-transformation', agg_fuel == 'coal')
    default_short <- default_mapped %>% dplyr::filter(iso == 'usa', agg_sector == '1A1_Energy-transformation', agg_fuel == 'coal')
    View(default_short[, c(1:5, 250:270)])
    final_short <- final_short[, c(1:5, 250:270)]
    View(final_short)

    #writeData( final_activity[which(final_activity$iso == 'deu'), ], domain = "MED_OUT", paste0("H.", em,"-total-activity-TEST-small"))
    #final_short <- final_activity[ final_activity$iso == 'deu', ]
    #default_short <- default_activity[which(default_activity$iso == 'deu' & default_activity$CEDS_fuel != 'process'), ]
    #writeData( default_short, domain = "MED_OUT", paste0("H.", em,"-total-activity-original-short"))


# Result comparisons ------------------------------------------------------

    # samecols <- "units"
    # for (c in names(final_short)) {
    #     if (identical(final_short[[c]], default_short[[c]])) {
    #         samecols <- c(samecols, c)
    #     }
    # }
    # samecols <- samecols[samecols %!in% c("agg_sector", "agg_fuel", "CEDS_sector", "CEDS_fuel", "iso")]
    # original <- default_short[ , -which(names(default_short) %in% samecols)]
    # changed <- final_short[ , -which(names(final_short) %in% samecols)]
    #
    # orig_rows <- dplyr::filter(original, X1937 != changed$X1937)
    # changed_rows <- dplyr::filter(changed, X1937 != original$X1937) %>% dplyr::select(-agg_fuel, -agg_sector)
    #
    # sum(original$X1937[grepl('coal', original$CEDS_fuel)])
    # sum(changed$X1937[changed$agg_fuel == 'coal'])
    #
    # sapply(final_short[final_short$agg_fuel == 'coal', paste0('X', 1941:1945)], sum)

    logStop()

#END


