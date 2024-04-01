#------------------------------------------------------------------------------
# Program Name: E.Argentina_emissions.R
# Authors' Names: Rachel Hoesly, Steve Smith
# Date Last Modified: Feb 27, 2024
# Program Purpose: To read in & reformat Argentina emissions inventory data
#                  This data only contains data from 1990 - 2011, missing data
#                  from 2000 and 2010. Units are initially in metric tonnes
# Input Files: Argentina_Translation.xlsx, Argentina Inventario 1990-2012-ipcc1996.xlsx,
# Output Files: E.[em]_ARG_inventory.csv
# Notes:
# TODO: Re-write read-in so that order of years is taken from input data instead of assumed.
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Get emission species first so can name log appropriately
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[1]
if ( is.na( em ) ) em <- "CO"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'common_data.R', "data_functions.R",
                  "emissions_scaling_functions.R", "analysis_functions.R" ) # Additional function files required.
    log_msg <- "Initial reformatting of Argentina emissions" # First message to be printed to the log
    script_name <- "E.Argentina_emissions.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )


# ------------------------------------------------------------------------------
# 0.5. Define parameters for inventory specific script
    em_temp = em  # Variable to use for interacting with emissions file
    if ( em_temp == "NMVOC" ) {
        em_temp <- "COVDM"
    }

    inv_data_folder <- "EM_INV"
    subfolder_name <- 'Argentina/'
    inv_name <- 'ARG' #for naming diagnostic files
    inv_years<-c( 1990:2020 )

# -------------------------------------------
# 1. Reading in translation file and the first sheet of Argentina Inventory

    em_species <- c('NOx','CO','NMVOC','SO2')

# If em species, then read in and process
    if ( em %in%  em_species  ) {

# Read in Argentina inventory
    inv_file <- 'Argentina emissions 1990-2022'
    sheet_name_1 <- "Energy - Precursors"
    sheet_name_2 <- "IPPU - Precursors"

    inv_data_in <- readData( inv_data_folder, inv_file, ".xlsx",
                             domain_extension = subfolder_name,
                             sheet_selection = sheet_name_1) %>%
        bind_rows(readData( inv_data_folder, inv_file, ".xlsx",
                            domain_extension = subfolder_name,
                            sheet_selection = sheet_name_2 )) %>%
        as_tibble()


# -------------------------------------------
# Process data

inv_data <- inv_data_in %>%
        mutate(iso = 'arg') %>%
        mutate(unit = 'kt') %>%
        mutate(sector = `IPCC category`) %>%
        mutate(year = paste0('X',Year)) %>%
        select('iso', 'sector', 'unit', 'year', em_temp) %>%
        dplyr::rename(value = matches(em_temp)) %>%
        mutate(value = as.numeric(value)) %>%
        group_by(iso, sector, unit, year) %>%
        summarise_if(is.numeric, sum, na.rm = TRUE) %>%
        spread(year, value)

# -------------------------------------------
# Write out blank df if no inventory data exists for given emissions
    } else {
        inv_data <- data.frame()
    }

# ------------------------------------------------------------------------------
# 2. Write standard form inventory

    writeData( inv_data , domain = "MED_OUT",
               paste0( 'E.', em, '_', inv_name, '_inventory' ) )

# Every script should finish with this line
    logStop()
# END
