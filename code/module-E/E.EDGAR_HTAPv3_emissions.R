# ------------------------------------------------------------------------------
# Program Name: E.EDGAR_HTAPv3_emissions.R
# Author(s): Rachel Hoesly
# Date Last Updated: 20 February 2024
# Program Purpose: Produces diagnostic summary figures of global emissions for four inventories
# Input Files:
# Output Files: Figures in the diagnostic-output
# Notes:
# TODO: 1.
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if( "input" %in% dir( ) ) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", 'common_data.R', 'IO_functions.R' ) # Additional function files may be required.
log_msg <- "Process EDGAR_HTAPv3" # First message to be printed to the log
script_name <- "EDGAR_HTAPv3_emissions.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"

# -------------------------------------------------------
# 1. Define parameters for inventory-specific script

inventory_data_file <- paste0("EDGAR_HTAPv3_", em)
inv_data_folder <- "EM_INV"
subfolder_name <- 'EDGAR_HTAPv3/'
inv_name <- 'EDGAR_HTAPv3' #for naming diagnostic files
inv_years <- c(2000:2018)

# ---------------------------------------------------------------------------
# 2. Read in data

EDGAR_HTAP_species <- c('BC','OC','CO','NH3','NMVOC','NOx','SO2')

if(em %in% EDGAR_HTAP_species){
#Read in the EDGAR_HTAP files
inv_in <- readData(domain = "EM_INV",
                       domain_extension = subfolder_name,
                       file_name = inventory_data_file,
                       extension = ".xlsx",
                       sheet_selection = inventory_data_file)

# Edgar_HTAPv3 data is in Mg which is equal to 1 metric tonne
inv_data <- inv_in %>%
    as_tibble() %>%
    select(Country, Sector, Year, Annual) %>%
    mutate(Country = str_replace(Country,'CHN_HKG_MAC','CHN'))%>%
    mutate(Country = str_replace(Country,'USA_PRI_VIR','USA')) %>%
    mutate(Country = str_replace(Country,'SRB_MNE_KOS','SRB')) %>%
    mutate(iso = tolower(Country)) %>%
    dplyr::rename(sector = Sector) %>%
    mutate(year = paste0('X',Year)) %>%
    mutate(Annual = Annual/1000) %>% # convert from metric tonne (or Mega grams) to kt
    mutate(unit = 'kt') %>%
    select(-Country,-Year) %>%
    spread(year, Annual) %>%
    select(iso, sector, unit, starts_with('X')) %>%
    arrange(iso, sector)

# print out Korea data - (only country we scale EDGAR HTAP to)
inv_data_kor <- inv_data %>%
    filter(iso == 'kor')

    }else{

#blank data frame for species that aren't in the inventory
    inv_data <- data.frame( )
    inv_data_kor <- data.frame( )

    }

# ------------------------------------------------------------------------------
# 3. Write out standard form inventory

writeData( inv_data, domain = "MED_OUT",
           paste0( 'E.', em, '_', inv_name, '_inventory' ) )

writeData( inv_data_kor, domain = "MED_OUT",
           paste0( 'E.', em, '_', inv_name, '_kor_inventory' ) )

logStop()
# END








