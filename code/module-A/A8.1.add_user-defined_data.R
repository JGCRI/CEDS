#------------------------------------------------------------------------------
# Program Name:    A8.1.add_user-defined_data.R
# Authors:         Ben Goldstein, Caleb Braun, Patrick O'Rourke
# Last Updated:    January 2019
# Program Purpose: To process user-defined datasets for use in the historical
#                  energy extension. See Section 3 of the CEDS User Guide
#                  (https://github.com/JGCRI/CEDS-dev/wiki/User-Guide) for
#                  more details.
#
# Input Files:  U.*.csv, U.*-instructions.csv, U.*-mapping.xslx
# Output Files: A.comb_user_added.csv
# Notes: Relies on functions from the following files:
#   - parameters/user_data_inclusion_functions.R
#   - parameters/user_data_processing.R
#   - parameters/user_extension_instr_processing.R
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# 0. Read in global settings and headers
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files
headers <- c( "data_functions.R", "user_data_diagnostics.R",
              "user_data_processing.R", "user_extension_instr_processing.R",
              "user_data_inclusion_functions.R" )
log_msg <- "Adding user-defined energy data."
script_name <- "A8.1.add_user-defined_data.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

DIAGNOSTIC_CHARTS <- T


# ------------------------------------------------------------------------------------
# 1. Read in data and filter out non-combustion data

MSL <- readData( "Master_Sector_Level_map", domain = "MAPPINGS" )
MCL <- readData( "Master_Country_List",     domain = "MAPPINGS" )
MFL <- readData( "Master_Fuel_Sector_List", domain = "MAPPINGS", extension = ".xlsx" )

# Process reference data to make adding supplemental data easier
all_sectors <- MFL$Sectors
othr_sectors <- all_sectors[ all_sectors$category == "other", "sector" ]
comb_sectors <- all_sectors[ all_sectors$category == "combustion", "sector" ]
comb_sectors <- c( othr_sectors, comb_sectors )
MFL <- MFL$Fuels
MSL <- dplyr::rename( MSL, CEDS_sector = working_sectors_v1 )

# Remove non-combustion activity from the default data then map it to the
# standard CEDS format
all_activity_data <- readData( 'MED_OUT', 'A.comb_default_activity_extended', meta = F ) %>%
    dplyr::rename( CEDS_sector = sector, CEDS_fuel = fuel ) %>%
    dplyr::filter( CEDS_sector %in% comb_sectors ) %>%
    mapToCEDS( MSL, MFL, aggregate = F )

# Special case for oil 1A1bc_Other-transformation and 1A1bc_Other-feedstocks
# This data is aggreagated to the agg_fuel level by default, so we place a fill
# value in for the CEDS_fuel.
all_activity_data <- all_activity_data %>%
    dplyr::mutate(CEDS_fuel = if_else( CEDS_fuel == 'oil' & CEDS_sector %in% othr_sectors,
                                        'AGGREGATE', CEDS_fuel ),
                  agg_fuel = if_else( CEDS_fuel == 'AGGREGATE', 'oil', agg_fuel ) )

stopifnot( !anyNA( all_activity_data ) ) # Data should all be valid

# -----------------------------------------------------------------------
# 2. Collect user-defined inputs and initialize script variables

# Read in the user-provided instructions for all supplemental data.
instructions <- processInstructions( comb_sectors, MSL, MFL, all_activity_data )

# TODO: Figure out how/where/why to process trend data without roundabout
#       file writing and bypassing processing
# instructions <- processTrendData( instructions, all_activity_data )

# Years the user is allowed to add data to
all_yrs <- names( all_activity_data )[ isXYear( names( all_activity_data ) ) ]

# Master list used to track activity data. Contains three dataframes:
# 1. all_activity_data:    changed activity data
# 2. old_activity_data:    unchanged (the original) activity data
# 3. continuity_factors:   percent weight given to unchanged data
activity <- list( all_activity_data = all_activity_data,
                  old_activity_data = all_activity_data )
activity <- addContinuityFactors( activity, instructions, all_yrs )

# The lists usr_files and map_files hold the user provided data and the
# associated mapping files. They are used as lookup tables, with the base
# filename (e.g. without '-mapping' and '.csv') as the key.
filenames <- unique( instructions$data_file )
map_files <- sapply( filenames, readInUserData, all_yrs, '-mapping', simplify = F )
usr_files <- sapply( filenames, function( data_file ) {
    procUsrData( readInUserData( data_file, all_yrs ),
                 instructions[ instructions$data_file == data_file, ],
                 map_files[[ data_file ]], MSL, MCL, MFL, all_activity_data )
}, simplify = F )

# Write out comparisons of input data to the default data
if ( DIAGNOSTIC_CHARTS ) {
    for ( i in seq_along( usr_files ) ) {
        compareToDefault( usr_files[[i]], all_activity_data,
                          names( usr_files )[[i]] )
    }
}


# This stores the final form of each instruction used, for diagnostics,
# and will be further defined within section 3.
rows_completed <- NULL

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

    # Identify other instructions in the "batch" that will need to be aggregated
    # as one. Files only need to be batched if their year ranges overlap.
    batch_instructions <- extractBatchInstructions( working_instructions,
                                                    instructions, s_year, e_year )

    # Remove the batch instructions from the master instruction dataframe
    anti_join_cols <- grep("keep_total_cols", names(instructions), invert = TRUE, value = TRUE)
    instructions <- dplyr::anti_join(instructions, batch_instructions,
                                       by = anti_join_cols)

    # Process the batch of instructions (if there is a batch)
    if ( nrow( batch_instructions ) > 0 ) {

        # If our years haven't been properly subdivided by year, we'll need to
        # subdivide the whole batch and return back to the beginning of the
        # loop. The goal of this process is to be able to process subdivisions
        # of datasets that only partially overlap.
        if ( length( unique( c( batch_instructions$start_year, s_year ) ) ) > 1 ||
             length( unique( c( batch_instructions$end_year, e_year ) ) ) > 1 ) {
            # Identify all the breaks that will need to occur (each unique start
            # and end year)
            year_breaks <- unique( c( batch_instructions$start_year, s_year,
                                      batch_instructions$end_year + 1,
                                      e_year + 1 ) ) %>%
                           sort()
            # Combine the working with the rest of the batch
            whole_batch <- rbind( working_instructions, batch_instructions )
            new_division_batch <- whole_batch[ 0, ]

            # For each break: create a new instruction for any instruction
            # that encompasses this year range
            for ( i in seq_along( year_breaks ) ) {
                if ( i == length( year_breaks ) ) break

                year_span_min <- year_breaks[ i ]
                year_span_max <- year_breaks[ i + 1 ] - 1

                rows_to_segment <- whole_batch[ whole_batch$start_year <= year_span_max &
                                                whole_batch$end_year   >= year_span_min, ]
                if ( i != 1 ) {
                    new_syear <- rows_to_segment$start_year != year_breaks[ i ]
                    rows_to_segment$start_continuity[ new_syear ] <- F
                }
                if ( i != length( year_breaks ) - 1 ) {
                    new_eyear <- rows_to_segment$end_year != year_breaks[ i + 1 ] - 1
                    rows_to_segment$end_continuity[ new_eyear ] <- F
                }

                rows_to_segment$start_year <- year_span_min
                rows_to_segment$end_year <- year_span_max

                new_division_batch <- rbind( new_division_batch, rows_to_segment )
            }

            # Tack all the newly-divided instructions onto the instructions df
            instructions <- rbind( new_division_batch, instructions )

            # Restart the while loop with new instructions
            next
        }

        # From here we know our years are properly subdivided, so we should
        # re-retrieve the data pointed to by the instructions. This may require
        # drawing on multiple source files.
        working_instructions <- rbind( working_instructions, batch_instructions )
        data_srcs <- unique( working_instructions$data_file )

        # Get relevant data from each source file, then combine into one df
        usrdata <- do.call( rbind, lapply( data_srcs, function( data_file ) {
            instr_rows <- working_instructions$data_file == data_file
            instr <- working_instructions[ instr_rows, ]
            subsetUserData( usr_files[[ data_file ]], instr )
        }))
    }

    data_to_use <- getRowsForAdjustment(all_activity_data, usrdata, Xyears)

    # The normalizeAndIncludeData is the main point of this script; it will
    # normalize, disaggregate, and then incorporate the user-defined data,
    # returning a list with both the data and diagnostics
    normalized <- includeUserData(usrdata, data_to_use, Xyears, working_instructions$keep_total_cols[[1]],
                                  data_file, working_instructions$specified_breakdowns,
                                  all_activity_data)

    activity$all_activity_data <- normalized$all_data
    diagnostics <- normalized$diagnostics

    # Tack on some diagnostics to the working instructions dataframe for
    # diagnostic output
    working_instructions$batch_id     <- batch
    working_instructions$warnings     <- diagnostics$warning_diagnostics
    working_instructions$nrow_changed <- diagnostics$rows_changed

    # Add working instructions to rows_completed, which will be a diagnostic for
    # reviewing what changes occurred
     rows_completed <- rbind( rows_completed, working_instructions )


}


# ------------------------------------------------------------------------------
# 4. Write out the final results

final_activity <- enforceContinuity( activity, all_yrs )

# Final diagnostics
rows_changed <- split( rows_completed, rows_completed$data_file )
id_cols <- aggLevelToCols( identifyLevel( final_activity ) )

invisible( lapply( rows_changed, function( df ) {
    agg_cols <- aggLevelToCols( identifyLevel( df, na.rm = T ) )
    source_name <- paste0( df$data_file[1], '-PROC' )

    # Add 5 year padding to plot range
    plot_yrs <- range( df$start_year, df$end_year ) + c( -20, 20 )
    plot_yrs <- intersect( paste0( 'X', plot_yrs[1]:plot_yrs[2] ), all_yrs )

    df[ agg_cols ] %>%
        dplyr::left_join( final_activity, by = agg_cols ) %>%
        dplyr::select( one_of( id_cols, plot_yrs ) ) %>%
        compareToDefault( activity$old_activity_data, source_name )
}) )

writeData( rows_completed, domain = "DIAG_OUT", "A.user_added_changed_rows",
           domain_extension = "user-data/" )
writeData( final_activity, domain = "MED_OUT",  "A.comb_user_added" )

logStop()
