#------------------------------------------------------------------------------
# Program Name: E.China_emissions_MEIC_2018.R
# Authors' Names: Erin McDuffie (adapted from E.China_emissions.R)
# Date Last Modified: July 15, 2019
# Program Purpose: To read in and reformat China emissions inventory data
# This data only contains data from 2010-2017.
# Units are initially in Tg, converted to kt
# Input Files: Fig3_MEIC_detail_Zheng_etal_2018.xlsx
# Output Files: E.[em]_CHN_2018_inventory.csv, E.[em]_MEIC_2018_inventory_country_total.csv
# Notes:
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Get emission species first so can name log appropriately
    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "BC"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'common_data.R', "data_functions.R",
              "emissions_scaling_functions.R", "analysis_functions.R" ) # Additional function files required.
    log_msg <- "Secondary reformatting of China emissions" # First message to be printed to the log
    script_name <- "E.China_emissions__MEIC_2018.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define parameters for inventory-specific script
    inventory_data_file <- 'Fig3_MEIC_detail_Zheng_etal_2018'
    inv_data_folder <- "EM_INV"
    subfolder_name <- 'China/'
    inv_name <- 'CHN_2018'
    inv_years <- c( 2010 : 2017 )

# Run if em is not CH4 - MEIC update does not have CH4
if( em %!in% c("CH4", "N2O") ){

# ------------------------------------------------------------------------------
# 2. Inventory in Standard Form (iso-sector-fuel-years, iso-sector-years, etc)

# Import Sheets containing selected species data.
    sheet_name <- em
    inv_data_sheet <- readData( inv_data_folder, inventory_data_file,
                                ".xlsx", sheet_selection = sheet_name,
                                domain_extension = subfolder_name, skip=3)

# Reformat data into sector by year data frame
    sectors <- names( inv_data_sheet )[ 2 : ncol( inv_data_sheet ) ]
    years <- paste0( "X", inv_data_sheet[ 1:nrow(inv_data_sheet) , 1 ] )
    inv_data_species <- data.frame( sectors )
    names( inv_data_species ) <- "sector"

    for (i in 1:length( inv_years ) ) {

        irow <- which(inv_data_sheet == inv_years[ i ])  # Find the row of the given inventory year
        inv_data_species$newcol <- t( inv_data_sheet[irow, 2:ncol(inv_data_sheet)] ) # Make new column with yearly data
        colnames(inv_data_species)[i+1] <- paste0("X", inv_data_sheet[ irow, 1 ] )   # Rename column with year

    }

# Clean rows and columns to standard format
    inv_data_species$iso <- 'chn'
    inv_data_species <- inv_data_species[ , c( 'iso', 'sector', years ) ]

# Make numeric
    inv_data_species[ , years ] <- sapply( inv_data_species[ , years ], as.numeric )

# Convert from Tg to kt
    inv_data_species[ , years ] <- as.matrix( inv_data_species[ , years ] ) * 1000

# ------------------------------------------------------------------------------
#  2.5 Find BC and OC emissions
    # aggregate and print out
    #units are in Mg
    #filter BC and OC to just transportation sector for now:
    if (em %in% c('BC','OC')) {

    inv_data_species_agg <- inv_data_species %>%
        gather(year, emissions, -iso, -sector) %>%
        mutate(emissions = emissions/1000) %>% # convert to Gg = kt
        mutate(units = 'kt') %>%
        group_by(iso, year, units) %>%
        dplyr::summarize(emissions = sum(emissions)) %>%
        spread(year, emissions)

    writeData( inv_data_species_agg, domain = "MED_OUT",
               paste0('E.',em,'_', inv_name, '_inventory_country_total'))

        inv_data_species <- inv_data_species %>% filter(sector == "Transportation")
    }

# ------------------------------------------------------------------------------
# 3. If em is CH4 - create a blank data frame (MEIC update does not have CH4 data)

} else {

    Note <- c( "No MEIC update data available." )
    inv_data_species <- dplyr::tibble( Note )

}

# ------------------------------------------------------------------------------
# 4. Write out standard form inventory
    writeData( inv_data_species, domain = "MED_OUT",
               paste0( 'E.', em, '_', inv_name, '_inventory' ) )


    # Write out inventory country totals
    # Remove sectors not scaled in module F

    # no sectors removed in module F. Write out sector and iso data.
    writeData( inv_data_species, domain = "DIAG_OUT", domain_extension = "country-inventory-compare/",
               paste0('inventory_',em,'_', inv_name))

    if (em %!in% c("BC","OC","CH4","N2O")){
         country_total <- inv_data_species %>%
            select(-sector)%>%
            group_by(iso) %>%
            summarize_each(funs(sum))

    writeData( country_total, domain = "MED_OUT",
               paste0('E.',em,'_', inv_name, '_inventory_country_total'))
    }

    logStop( )

# END

