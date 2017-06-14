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
    
    
    readInUserInstructions <- function () {
    # At this point in the process, data has been maped & cleaned.
    # We need to build an instructions dataframe to pass forward.
        USER_DOM <- "../input/extension/user-defined-energy/"
        
    # Get a list of all the files in the directory
        files_present <- list.files(USER_DOM)
        instruction_files <- files_present[ grep("-instructions", files_present) ]
        
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
            
            if ( is.null( all_instructions ) ) {
                all_instructions <- use_instructions
                all_instructions$data_file <- data_file
            }
            else {
                all_instructions <- rbind.fill(all_instructions, use_instructions)
                all_instructions$data_file[ which( is.na( all_instructions$data_file) ) ] <- data_file
            }
        }
        
        
        return ( all_instructions )
    }
        
