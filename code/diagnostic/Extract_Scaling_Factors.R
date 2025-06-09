#------------------------------------------------------------------------------
# Program Name: Extract Scaling Factors
# Authors: Rachel Hoesly
# Date Last Updated: Jan 2, 2025
# Program Purpose: pulls out the scaling factors for all species for a specific sector
# Input Files:   F.em_Scaling_Factors_sector_<inventory>.csv
# Output Files:
# Notes:
# TODO:
# ------------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R" ) # Any additional function files required
log_msg <- "Performing some scaling diagnostics" # First message to be printed to the log
script_name <- "Scaling Diagnostics.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

MED_OUT <- '../intermediate-output/'

# ------------------------------------------------------------------------------
# 1. Script Options


inventory <- "EDGAR"
scaling_sectors <- 'all'

# ------------------------------------------------------------------------------
# 2. Load Files

files_list <- list.files("../diagnostic-output") %>%
    as_tibble() %>%
    filter(str_detect(value, paste0('Scaling_Factors_Scaling_sectors_',inventory))) %>%
    pull(value)

readData_em <- function(file){
    readData(domain = "DIAG_OUT", file_name = file) %>%
    mutate(em = file)
}

scaling_factors_in <- lapply(files_list, readData_em) %>%
    do.call(bind_rows, .)
scaling_factors_in$inventory <- inventory

SF <- scaling_factors_in %>%
    mutate(em = str_sub(em, 3,8)) %>%
    mutate(location = str_locate(em, "_")[,1]-1) %>%
    mutate(em = str_sub(em, 1,location)) %>%
    select(inventory, em, iso, scaling_sector, contains('X'))

if(scaling_sector != 'all'){

    possible_scaling_sectors <- SF$scaling_sector %>% unique
    if(any( ! scaling_sectors %in% possible_scaling_sectors) ){

        stop('Some selected scaling_sectors are not in possible scaling sectors. Check variable.')
    }

    SF <- SF %>%
        filter(scaling_sector %in% scaling_sectors)
}

# ------------------------------------------------------------------------------
# 4. Output
writeData(SF, domain = 'DIAG_OUT', fn = paste0('extracted_scaling_factors_',inventory), meta = F)


