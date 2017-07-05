#------------------------------------------------------------------------------
# Program Name: user_data_proc_pseudocode.R
# Author: Ben Goldstein
# Date Last Updated: 5 July 2017
# Program Purpose: To process user-defined datasets for use in the historical
#                  energy extension. 
# Input Files: U.*.csv, U.*-instructions.xslx, U.*-mapping.xslx
# Output Files: None
# Notes: 
### TODO: If the function gives an error, we should not end the whole system,
###       all we need to do is reject this user-defined dataset and proceed
###       to the next one... Unless this would make it hard for the user
###       to see that their changes are being rejected?
# ------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
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
    
# identifyLevel
# Brief: This is a helper function for the add_user-defined_data script.
#        It performs a simple check of column names to determine the data
#        frame's level of aggregation.
# params:
#    dataframe: the dataframe whose level you wish to identify
    identifyLevel <- function ( dataframe ) {
        
        column_names <- colnames(dataframe)
        categorical <- column_names[ which( !isXYear(column_names) ) ]
        
        if ( "CEDS_sector" %in% column_names ) {
            if ( "CEDS_fuel" %in% column_names ) return( 4 )
            return( 5 )
        }
        else if ( "agg_sector" %in% column_names & 
                  "CEDS_fuel" %in% column_names ) return( 3 )
        else if ( "CEDS_fuel" %in% column_names ) return( 2 )
        else if ( "agg_sector" %in% column_names) return( 6 )
        else if ( "agg_fuel" %in% column_names ) return( 1 )
        
        return(0)
    }
    
# is.invalid
# Brief: This is a helper function for the add_user-defined_data script.
#        Combination check for if a value is null, NA, or NaN
# params:
#    x: some non-list object
    is.invalid = function(x) {
      return(is.null(x) || is.na(x) || is.nan(x))
    }
    
# is.nan.df
# Brief: This is a helper function for the add_user-defined_data script.
#        Makes up for the fact that R has no built-in vectorized check 
#        for NaN.
# params:
#    x: a dataframe that may or may not contain NaN values
    is.nan.df <- function(x) {
        do.call(cbind, lapply(x, is.nan)) 
    }

# retrieveUserDataframeSubset
# Brief: This is a helper function for the add_user-defined_data script.
#        Subsets a user-specified dataset based on user-specified instructions,
#        so the only data processed is the data specified in instructions.
# params:
#    user_dataframe: 
#    working_instructions:
    retrieveUserDataframeSubset <- function( user_dataframe, working_instructions ) {
        
    # initialize a subset dataframe
        user_dataframe_subset <- user_dataframe[ which( user_dataframe$iso %in% 
                                                          working_instructions$iso ), ]
        
    # Subset the dataframe based on which columns are present and filled out in the dataframe
        if ( !is.invalid( working_instructions$CEDS_sector ) && working_instructions$CEDS_sector != 'all' ) {
          user_dataframe_subset <- user_dataframe_subset[ which ( user_dataframe_subset$CEDS_sector %in%
                                                                    working_instructions$CEDS_sector ), ]
        } else if ( !is.invalid( working_instructions$agg_sector ) && working_instructions$agg_sector != 'all' ) {
          user_dataframe_subset <- user_dataframe_subset[ which ( user_dataframe_subset$agg_sector %in%
                                                                    working_instructions$agg_sector ), ]
        }
        
        if ( !is.invalid( working_instructions$CEDS_fuel ) && working_instructions$CEDS_fuel != 'all' ) {
          user_dataframe_subset <- user_dataframe_subset[ which ( user_dataframe_subset$CEDS_fuel %in% 
                                                                    working_instructions$CEDS_fuel ), ]
        } else if ( !is.invalid( working_instructions$agg_fuel ) && working_instructions$agg_fuel != 'all' ) {
          user_dataframe_subset <- user_dataframe_subset[ which ( user_dataframe_subset$agg_fuel %in%
                                                                    working_instructions$agg_fuel ), ]
        }
        return( user_dataframe_subset )
    }

# ------------------------------------------------------------------------------------
# 1. Read in data

    MSL <- readData( "Master_Sector_Level_map", domain = "MAPPINGS" )
    colnames( MSL )[ which( colnames( MSL ) == 'working_sectors_v1' ) ] <- 'CEDS_sector' 
    MCL <- readData( "Master_Country_List", domain = "MAPPINGS" )
    MFL <- readData( "Master_Fuel_Sector_List", domain = "MAPPINGS", extension = ".xlsx",
                    sheet_selection = "Fuels" )
    comb_or_NC <- readData( "Master_Fuel_Sector_List", domain = "MAPPINGS", extension = ".xlsx",
                           sheet_selection = "Sectors" )
    comb_sectors_only <- comb_or_NC$sector[ which( comb_or_NC$type == "comb" ) ]
    

# Gather default activity data
    default_activity <- readData( 'MED_OUT', paste0( 'H.', em, '_total_activity_extended_db' ) , meta = F) ### Eventually this will not require an emissions species.
    colnames( default_activity )[ 1:3 ] <- c( "iso", "CEDS_sector", "CEDS_fuel" )
    default_activity_mapped <- mapToCEDS( default_activity, MSL, MCL, MFL, aggregate = F )

# ------------------------------------------------------------------------------------
# 2. Collect user-defined inputs and begin processing data loop

# Initialize an environment to track activity data
    activity_environment <- new.env()
    
# This script only operates on combustion emissions, so reduce the data to that form
    all_activity_data <- default_activity_mapped[ which( default_activity_mapped$CEDS_sector %in% comb_sectors_only ), ]
    
    yearsAllowed <- colnames( all_activity_data )[ isXYear(colnames( all_activity_data ))]
    
    activity_environment$all_activity_data <- all_activity_data
    
# Read instructions files and create a procedure list
    instructions <- readInUserInstructions()
    instructions[ which( is.na( instructions$agg_fuel ) ), c( "iso", "CEDS_fuel", "agg_fuel" ) ] <- 
                                        left_join( instructions[ which( is.na(instructions$agg_fuel)), c( "iso", "CEDS_fuel" ) ], 
                                                            MFL[ , c( "aggregated_fuel", "fuel" ) ], 
                                                            by = c( "CEDS_fuel" = "fuel" ) ) ### Do a row check

# This will store the final form of each instruction used, for diagnostics
    rows_completed <- instructions[ 0, ]
    
# This integer will track which batch number we're on, for informing diagnostics
    batch <- 0
    
    old.file <- "NULL"
    
    while ( nrow( instructions ) > 0 ) {
        
        batch <- batch + 1
    
    # Select the first instruction in the list for processing
        working_instructions <- instructions[ 1, ]
        instructions <- instructions[ -1, ]
        
        Xyears <- paste0( "X", working_instructions$start_year:working_instructions$end_year )
        Xyears <- Xyears[ which( Xyears %in% yearsAllowed )]
      
    # Get the first dataset to operate on. 
        new.file <- working_instructions$data_file
        
        if ( old.file != new.file ) {
            dataframe_interp_instructions <- readData( paste0 ( "user-defined-energy/", 
                                                                new.file, "-instructions"),
                                                       domain = "EXT_IN", extension = ".xlsx", 
                                                       sheet_selection = "Interpolation_instructions" )
            user_dataframe <- processUserDefinedData( new.file, dataframe_interp_instructions, MSL, MCL, MFL )
            old.file <- new.file
        }
            
    # Extract the data from the dataframe that will refer to the specific
    # categories and years as defined by the
        user_dataframe_subset <- retrieveUserDataframeSubset( user_dataframe, working_instructions )
        agg_level <- identifyLevel( user_dataframe )
        
    # Identify other instructions in the "batch" that will neeed to be
    # aggreagated as one.
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
            batch_data_instructions <- instructions %>% filter( iso %in% user_dataframe_subset$iso ) %>%
                                                    filter( agg_fuel %in% user_dataframe_subset$agg_fuel ) %>%
                                                    filter( is.na( CEDS_sector ) )
            instructions <- rbind( instructions %>% filter( iso %!in% user_dataframe_subset$iso ),
                                   instructions %>% filter( agg_fuel %!in% user_dataframe_subset$agg_fuel ),
                                   instructions %>% filter( !is.na( CEDS_sector ) ) )
            instructions <- unique(instructions)
        } else if ( agg_level %in% c(1, 2) ) {
            batch_data_instructions <- instructions %>% filter( iso %in% user_dataframe_subset$iso ) %>%
                                                    filter( agg_fuel %in% user_dataframe_subset$agg_fuel ) %>%
                                                    filter( is.na( agg_sector ) )
            instructions <- rbind( instructions %>% filter( iso %!in% user_dataframe_subset$iso ),
                                   instructions %>% filter( agg_fuel %!in% user_dataframe_subset$agg_fuel ) )
                                   instructions %>% filter( !is.na( agg_sector ) )
            instructions <- unique(instructions)
        } else if ( agg_level == 5 ) {
            batch_data_instructions <- instructions %>% filter( iso %in% user_dataframe_subset$iso ) %>%
                                                    filter( agg_fuel %in% user_dataframe_subset$agg_fuel ) %>%
                                                    filter( agg_sector %in% user_dataframe_subset$agg_sector )
            instructions <- rbind( instructions %>% filter( iso %!in% user_dataframe_subset$iso ),
                                   instructions %>% filter( agg_fuel %!in% user_dataframe_subset$agg_fuel ),
                                   instructions %>% filter( agg_sector %!in% user_dataframe_subset$agg_sector ) )
            instructions <- unique(instructions)
        }

        batch_instructions_overlap <- batch_data_instructions[ which( 
                              batch_data_instructions$start_year < working_instructions$end_year &
                              batch_data_instructions$end_year > working_instructions$start_year ), ]
        batch_instructions_no_overlap <- batch_data_instructions[ which( 
                              batch_data_instructions$start_year > working_instructions$end_year |
                              batch_data_instructions$end_year < working_instructions$start_year ), ]
        instructions <- rbind( instructions, batch_instructions_no_overlap )
        batch_data_instructions <- batch_instructions_overlap
    # If there are data in our instructions that will be in the current group's batch:
        if ( nrow( batch_data_instructions ) > 0 ) {
          
        # If our years haven't been properly subdivided by year, we'll need to
        #     subdivide the whole batch and return back to the beginning of the loop
            if ( any( batch_data_instructions$start_year != working_instructions$start_year ) ||
                 any( batch_data_instructions$end_year != working_instructions$end_year ) ) {
                
                year_breaks <- unique( c( batch_data_instructions$start_year,
                                          working_instructions$start_year,
                                          batch_data_instructions$end_year + 1,
                                          working_instructions$end_year + 1 ) ) %>%
                                  sort()
                
                whole_batch <- rbind( working_instructions, batch_data_instructions )
                new_division_batch <- whole_batch[ 0, ]
                
                for (i in 1:(length(year_breaks)-1)) {
                    new_year_span <- year_breaks[ i ]:( year_breaks[ i+1 ] - 1 )
                    
                    rows_to_segment <- whole_batch[ which( whole_batch$start_year <=
                                                                  max( new_year_span ) &
                                                           whole_batch$end_year >=
                                                                  min( new_year_span ) ), ]
                    rows_to_segment$start_year <- min( new_year_span )
                    rows_to_segment$end_year <- max( new_year_span )
                    
                    new_division_batch <- rbind( new_division_batch, rows_to_segment )
                }
                
                instructions <- rbind( instructions, new_division_batch )
                next
            }
        
        # If our years are properly subdivided, we should re-retrieve our
        #   user_defined_data dataframe. This may require drawing on multiple
        #   source files.
            working_instructions <- rbind( working_instructions, batch_data_instructions )
            user_dataframe_subset <- user_dataframe_subset[0,]
            for ( file in unique( working_instructions$data_file ) ) { 
              
                dataframe_interp_instructions <- readData( paste0 ( "user-defined-energy/", 
                                                                    file, "-instructions"),
                                                           domain = "EXT_IN", extension = ".xlsx", 
                                                           sheet_selection = "Interpolation_instructions" )
                user_dataframe <- processUserDefinedData( file, dataframe_interp_instructions, MSL, MCL, MFL )
                
                user_dataframe_subset <- retrieveUserDataframeSubset( user_dataframe, 
                                                                            working_instructions )
            }
        }
        
    # Identify the rows that will need adjusting. If the aggregation level is the lowest,
    #   this will be any rows that match to your row, and any row in the next-highest aggregation
    #   GROUP. For example, If I'm adjusting ger-coal_coke-1A1, I will need to edit all the
    #   ger-coal_coke-1A rows.
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
        } else if (agg_level == 2 || agg_level == 1) {
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
        
        whole_group = F
        if ( nrow( data_to_use ) == nrow( user_dataframe_subset ) ) { ### This is the wrong way to check this
            whole_group = T
        }
        
        diagnostics <- NA
        
        if ( agg_level == 1 ) {
            diagnostics <- normalizeAndIncludeDataL1( Xyears, data_to_use, user_dataframe_subset, 
                                                      all_activity_data )
        } else if ( agg_level == 2 ) {
            diagnostics <- normalizeAndIncludeDataL2( Xyears, data_to_use, user_dataframe_subset, 
                                                      all_activity_data, whole_group, 
                                                      as.logical( working_instructions$override_normalization ) )
        } else if ( agg_level == 3 ) {
            diagnostics <- normalizeAndIncludeDataL3( Xyears, data_to_use, user_dataframe_subset, 
                                                      all_activity_data, whole_group, 
                                                      as.logical( working_instructions$override_normalization ) )
        } else if ( agg_level == 4 ) {
            diagnostics <- normalizeAndIncludeDataL4( Xyears, data_to_use, user_dataframe_subset, 
                                                      all_activity_data, whole_group, 
                                                      as.logical( working_instructions$override_normalization ) )
        } else if ( agg_level == 5 ) {
            diagnostics <- normalizeAndIncludeDataL5( Xyears, data_to_use, user_dataframe_subset, 
                                                      all_activity_data, whole_group, 
                                                      as.logical( working_instructions$override_normalization ) )
        } else if ( agg_level == 6 ) {
            diagnostics <- normalizeAndIncludeDataL6( Xyears, data_to_use, user_dataframe_subset, 
                                          all_activity_data, whole_group, 
                                          as.logical( working_instructions$override_normalization ) )
        }
        
        working_instructions$batch_id <- batch
        working_instructions$agg_level <- agg_level
        if (!is.na(diagnostics)) {
            working_instructions$nrow_changed <- diagnostics$rows_changed
            working_instructions$warnings <- diagnostics$warning_diag
        } else {
            working_instructions$nrow_changed <- 0
            working_instructions$warnings <- "Normalize and Include function not called"
        }
        
        rows_completed <- rbind( rows_completed, working_instructions )
        
    }

    writeData( rows_completed, domain = "DIAG_OUT", fn = "user-ext-data_diagnostics")

    
