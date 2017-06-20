#------------------------------------------------------------------------------
# Program Name: user_data_proc_pseudocode.R
# Author: Ben Goldstein
# Date Last Updated: 9 June 2017
# Program Purpose: To process user-defined datasets for use in the historical
#                  energy extension. 
# Input Files: U.*, U.*-instructions, U.*-mapping
# Output Files: None
# Functions Defined: 
# Notes: 
# ------------------------------------------------------------------------------------
    
    orderInstructions <- function( instructions ) {
        
        instructions <- instructions[ order(instructions$start_year) ]
        
        return()
    }
    
    readInUserInstructions <- function () {
    # At this point in the process, data has been maped & cleaned.
    # We need to build an instructions dataframe to pass forward.
        USER_DOM <- "../input/extension/user-defined-energy/"
        
    # Get a list of all the files in the directory
        files_present <- list.files(USER_DOM)
        instruction_files <- files_present[ grep("-instructions", files_present) ]
        comb_or_NC <- readData("Master_Fuel_Sector_List", domain = "MAPPINGS", extension = ".xlsx",
                                                        sheet_selection = "Sectors")
        comb_sectors_only <- comb_or_NC$sector[ which( comb_or_NC$type == "comb") ]
        
    # For each file in the instructions folder, read it in and collect important info:
    #    Name of the file minus "-instructions" tells us what data it's connected to
    #    Year range
    #    Iso, sector, fuel affected (in CEDS form--I think it's ok to demand this be mapped for us)
    #    Type of data (final totals? trend? etc)
    #    Priority?
        
        all_instructions <- NULL
        
        for (file in instruction_files) {
            data_file <- gsub("-instructions.xlsx", "", file)
    
            use_instructions <- readData( paste0( "user-defined-energy/", data_file, "-instructions"), domain = "EXT_IN", extension = '.xlsx',
                                          sheet_selection = "Trend_instructions")
            
            if ("CEDS_sector" %in% colnames(use_instructions)) {
                all_rows <- nrow(use_instructions)
                use_instructions <- use_instructions[ which ( use_instructions$CEDS_sector %in% comb_sectors_only ), ]
                if (nrow(use_instructions) != all_rows) {
                    warning(paste0(all_rows-nrow(use_instructions)," instruction line(s) were rejected as non-combustion sectors"))
                }
            }
            
            if ( is.null( all_instructions ) ) {
                all_instructions <- use_instructions
                all_instructions$data_file <- data_file
            }
            else {
                all_instructions <- rbind.fill(all_instructions, use_instructions)
                all_instructions$data_file[ which( is.na( all_instructions$data_file) ) ] <- data_file
            }
        }
        
        # First, determine batches. How will we indicate that a group of items is in a batch?
        ### The solution is probably just to sort them in a way that batched items are together...
        ### Okay, what makes a batch? A batch is any items that are a) on the same aggregation level
        ###    and b) belong to the same aggregate category.
        
        
        level_1_instructions <- all_instructions[ which( !is.na(all_instructions$L1_agg_fuel)),] %>%
                                    arrange(iso) %>%
                                    arrange(-end_year)
        level_2_instructions <- all_instructions[ which( !is.na(all_instructions$L2_CEDS_fuel)),]%>%
                                    arrange(iso) %>%
                                    arrange(-end_year)
        level_3_instructions <- all_instructions[ which( !is.na(all_instructions$L3_agg_sector)),]%>%
                                    arrange(iso) %>%
                                    arrange(-end_year)
        level_4_instructions <- all_instructions[ which( !is.na(all_instructions$L4_CEDS_sector)),] %>%
                                    arrange(iso) %>%
                                    arrange(-end_year)
        
        all_instructions <- rbind(level_1_instructions, level_2_instructions, level_3_instructions, level_4_instructions)
        
        
        return ( all_instructions )
    }
        

    