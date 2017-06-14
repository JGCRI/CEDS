#------------------------------------------------------------------------------
# Program Name: user_data_proc_pseudocode.R
# Author: Ben Goldstein
# Date Last Updated: 14 June 2017
# Program Purpose: To process user-defined datasets for use in the historical
#                  energy extension. 
# Input Files: U.*, U.*-instructions, U.*-mapping
# Output Files: None
# Functions Defined: mapToCEDS, interpolateData, interpolateByTrend
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
                  "interpolation_extension_functions.R", "user_data_proc_pseudocode.R",
                  "instruction_process_pseudocode.R") # Additional function files required.
    log_msg <- paste0( "Calling inventory emission scaling stripts" ) # First message to be printed to the log
    script_name <- paste0( "step2_adding_data_pseudocode.R" )
    
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

    
# ------------------------------------------------------------------------------------
# 0.5 Define functions
    
    identifyLevel <- function ( dataframe ) {
        
        column_names <- colnames(dataframe)
        categorical <- column_names[ which( !isXYear(column_names) ) ]
        
        if ( "CEDS_sector" %in% column_names ) return(4)
        else if ( "agg_sector" %in% column_names & 
                  "CEDS_fuel" %in% column_names ) return(3)
        else if ( "CEDS_fuel" %in% column_names ) return(2)
        else if ( "agg_fuel" %in% column_names ) return(1)
        
        return(0)
    }
    
    is.invalid = function(x) {
      return(is.null(x) || is.na(x) || is.nan(x))
    }

# ------------------------------------------------------------------------------------
# 1. Read in data

    MSL <- readData("Master_Sector_Level_map", domain = "MAPPINGS")
    colnames( MSL )[ which( colnames( MSL ) == 'detailed_sectors' ) ] <- 'CEDS_sector' 
    MCL <- readData("Master_Country_List", domain = "MAPPINGS")
    MFL <- readData("Master_Fuel_Sector_List", domain = "MAPPINGS", extension = ".xlsx",
                    sheet_selection = "Fuels")

# Gather default activity data
    default_activity <- readData( 'MED_OUT', paste0('H.', em, '_total_activity_extended_db' ) , meta = F) ### Eventually this will not require an emissions species.
    colnames( default_activity )[1:3] <- c("iso","CEDS_sector","CEDS_fuel")
    default_activity_mapped <- mapToCEDS( default_activity, MSL, MCL, MFL, aggregate = F)
    default_activity_mapped$units <- default_activity$units

# ------------------------------------------------------------------------------------
# 2. Collect user-defined inputs and begin processing data loop
    
    all_activity_data <- default_activity_mapped
    
# Read instructions files and create a procedure list
    instructions <- readInUserInstructions()
    old.file <- "NULL"
    
    while (nrow(instructions) > 0) {
        
        Xyears <- paste0("X", instructions$start_year[1]:instructions$end_year[1])
      
    # Get the first dataset to operate on. ### For now, we will just do them in order until we commit to a priority method,
    #     although priority could take effect in the readInUserInstructions() function by ordering them?
        new.file <-instructions$data_file[1]
        
        if (old.file != new.file) {
            dataframe_interp_instructions <- readData( paste0 ("user-defined-energy/", 
                                                               new.file, "-instructions"),
                                                       domain = "EXT_IN", extension = ".xlsx", 
                                                       sheet_selection = "Interpolation_instructions")
            user_dataframe <- processUserDefinedData( new.file, dataframe_interp_instructions, MSL, MCL, MFL )
        }
            
    # Extract the data from the dataframe that will refer to the specific
    # categories and years as defined by the
        user_dataframe_subset <- user_dataframe
        user_dataframe_subset <- user_dataframe_subset[ which( user_dataframe_subset$iso == 
                                                                 instructions$iso[1] ), ]
        if ( !is.invalid( instructions$CEDS_sector[1] ) && instructions$CEDS_sector[1] != 'all' ) {
            user_dataframe_subset <- user_dataframe_subset[ which ( user_dataframe_subset$CEDS_sector == 
                                                                    instructions$CEDS_sector[1] ), ]
        } else if ( !is.invalid( instructions$agg_sector[1] ) && instructions$agg_sector != 'all' ) {
            user_dataframe_subset <- user_dataframe_subset[ which ( user_dataframe_subset$agg_sector ==
                                                                    instructions$agg_sector[1] ), ]
        }
        
        if ( !is.invalid( instructions$CEDS_fuel[1] ) && instructions$CEDS_fuel[1] != 'all' ) {
            user_dataframe_subset <- user_dataframe_subset[ which ( user_dataframe_subset$CEDS_fuel == 
                                                                    instructions$CEDS_fuel[1] ), ]
        } else if ( !is.invalid( instructions$agg_fuel[1] ) && instructions$agg_fuel[1] != 'all' ) {
            user_dataframe_subset <- user_dataframe_subset[ which ( user_dataframe_subset$agg_fuel ==
                                                                    instructions$agg_fuel[1] ), ]
        }
            
    # Identify the rows that will need adjusting. If the aggregation level is the lowest,
    #   this will be any rows that match to your row, and any row in the next-highest aggregation
    #   GROUP. For example, If I'm adjusting ger-coal_coke-1A1, I will need to edit all the
    #   ger-coal_coke-1A rows.
    #   If we're dealing with data on not the lowest aggregation level, you need any cells that
    #   can map to this cell.
    # Basically, in all cases: figure out what level you're on, 
    #   then grab any rows that would be in the same group as yours one level up.
        
        agg_level <- identifyLevel( user_dataframe )
        if ( agg_level == 4 ) {
            data_to_use <- all_activity_data[ which( all_activity_data$iso == instructions$iso[1] &
                                                     all_activity_data$CEDS_sector == instructions$CEDS_sector[1] &
                                                     all_activity_data$agg_fuel == MFL$aggregated_fuel[which( instructions$CEDS_fuel[1] == MFL$fuel)]
                                                     ), c( colnames( all_activity_data[ which( !isXYear( 
                                                       colnames(all_activity_data) ) ) ] ), Xyears ) ]
        } else if (agg_level == 3) {
            data_to_use <- all_activity_data[ which( all_activity_data$iso == instructions$iso[1] &
                                                     all_activity_data$CEDS_sector == instructions$CEDS_sector
                                                     ), c( colnames( all_activity_data[ which( !isXYear( 
                                                       colnames(all_activity_data) ) ) ] ), Xyears ) ]
        } else if (agg_level == 2) {
            data_to_use <- all_activity_data[ which( all_activity_data$iso == instructions$iso[1] &
                                                     all_activity_data$agg_sector == instructions$agg_sector
                                                     ), c( colnames( all_activity_data[ which( !isXYear( 
                                                       colnames(all_activity_data) ) ) ] ), Xyears ) ]
        } else if (agg_level == 1) {
            data_to_use <- all_activity_data[ which( all_activity_data$iso == instructions$iso[1] ), 
                                              c( colnames( all_activity_data[ which( !isXYear( 
                                                 colnames(all_activity_data) ) ) ] ), Xyears ) ]          
        }
        
    # We have all cells that we need to operate on. Cells that are part of 
    # the aggregate group need to be adjusted based on the current percent 
    # breakdowns. If not at level four, diaggregated cells that match our
    # criteria also need to be handled. We'll work on aggregation cases
    # one-by-one.
        
        if (agg_level == 4) {
            
        # Calculate percent breakdowns for the category.
        # First, get the sum of the category for each year.
            year_totals <- data_to_use[1,]
            year_totals[1, Xyears] <- colSums(data_to_use[, Xyears])
            year_totals_non_user_data <- year_totals[ , c( colnames( year_totals[ which( !isXYear( 
                                       colnames(year_totals) ) ) ] ), Xyears ) ]

        # Determine higher-level percentage breakdown. Because this is agg. 
        # level 4, we can be confident that each row (if unique) is of a higher 
        # level of aggregation than the data being added, so we don't have to
        # group; we can just operate on rows.
            
        # Determine what percent of the agg group minus our row each row made up
            year_totals_non_user_data[, Xyears] <- year_totals[, Xyears] - 
                                        data_to_use[ which( data_to_use$iso %in% user_dataframe_subset$iso &
                                                            data_to_use$CEDS_sector %in% user_dataframe_subset$CEDS_sector &
                                                            data_to_use$CEDS_fuel %in% user_dataframe_subset$CEDS_fuel )
                                        , Xyears]
            year_totals_non_user_data[ 2:(nrow(data_to_use) - 1), ] <- year_totals_non_user_data[1,]  ### Rework how I get year_totals so I can start out with a matrix of the right size
            
            pct_of_agg_group <- data_to_use[ which( data_to_use$CEDS_fuel %!in% user_dataframe_subset$CEDS_fuel)
                                             , c( colnames( year_totals[ which( !isXYear( 
                                               colnames(year_totals) ) ) ] ), Xyears ) ]
            pct_of_agg_group[, Xyears] <- data.matrix(pct_of_agg_group[, Xyears]) / data.matrix(year_totals_non_user_data[, Xyears])
            
        # Create a new dataframe that and replace the old row of data with the
        # new row, to calculate new year totals
            data_changed <- data_to_use[ , c( colnames( year_totals[ which( !isXYear( 
                                         colnames(year_totals) ) ) ] ), Xyears ) ]
            data_changed <- replaceValueColMatch( data_changed, user_dataframe_subset,
                                                 x.ColName = Xyears, match.x = c("iso","CEDS_sector","CEDS_fuel"),
                                                 addEntries = T )
            
            new_year_totals <- data_changed[1,]
            new_year_totals[1, Xyears] <- colSums( data_changed[ , Xyears ] )
            
            annual_diffs <- year_totals[ 1, Xyears ] - new_year_totals[, Xyears]
            annual_diffs[ 2:(nrow(data_to_use) - 1), ] <- annual_diffs[1,]
            
        # Calculate the values that needed to be added to each non-specified cell
        #   in order to preserve the total activity by aggregate fuel for this iso/sector
            sums_to_add <- pct_of_agg_group
            sums_to_add[ , Xyears] <- data.matrix(pct_of_agg_group[, Xyears]) * data.matrix(annual_diffs[, Xyears])
            
        # Add these values into the old data
            data_to_correct <- data_to_use[ which( data_to_use$CEDS_fuel %!in% user_dataframe_subset$CEDS_fuel)
                                            , c( colnames( year_totals[ which( !isXYear( 
                                              colnames(year_totals) ) ) ] ), Xyears ) ]
            corrected_data <- data_to_correct
            corrected_data[, Xyears] <- data.matrix(data_to_correct[, Xyears]) + data.matrix(sums_to_add[, Xyears])
            
            data_changed[ which( data_to_use$CEDS_fuel %!in% user_dataframe_subset$CEDS_fuel), ] <-
                    corrected_data
            
        # Return an error if column sums didn't come out the same (rounded to 3
        # decimal places to allow for e-14 processing discrepencies that were
        # occuring during testing)
            if (any(round(colSums(data_changed[, Xyears]), 3) != round(colSums(data_to_use[,Xyears]), 3))) {
                stop( "Aggregate sums were not retained" )
            }
            
            all_activity_data[ which( all_activity_data$iso %in% data_changed$iso &
                                      all_activity_data$agg_fuel %in% data_changed$agg_fuel &
                                      all_activity_data$CEDS_sector %in% data_changed$CEDS_sector), Xyears] <-
                      data_changed[, Xyears]  ### We will maybe not re-add this data into the main dataframe... discuss later
            
        } else if ( agg_level == 1 ) {
            
        # We have to do all the things we did at level 4 on the highest 
        # level, and then use those operations to calculate factors by which
        # we will adjust all lower-level cells. First let's create that
        # highest-level dataset.
            
            grouping_cols <- c("iso", "agg_fuel")
            data_to_use[, Xyears][is.na(data_to_use[, Xyears])] <- 0  ### I'm replacing NAs with 0s here. Is this allowed? Or is this a false representation of how we want to do sums?
            aggregated_data_unchanged <- ddply( data_to_use, grouping_cols, function(x) colSums(x[Xyears]) ) ### Also, I'm trying to normalize across different units here... hmm. I think maybe we exclude process? You know what? We might not even need to normalize on level 1. We might have to do trickle-down only.
          
            
        }
        
        
        
        
        
        
        
        
        
        
        old.file <- new.file
        instructions <- instructions[ -1, ]
    }

    
    
    



    
    
    
    

