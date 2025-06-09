# ------------------------------------------------------------------------------
# Program Name: E.Australia_emission_2018Update.R
# Author(s): Erin McDuffie, based on original from Leyang Feng
#
# Depreciated file - here in case we want to extract this for comparison
#
# Date Last Updated: September 17, 2020
# Program Purpose: To read in and reformat Australia NPI data.
# Input Files: [em]_Australia_UNFCCC_and_NPI.xlsx
# Output Files: E.[EM]_Australia_inventory.csv, E.[EM]_Australia_inventory_country_total.csv
# Notes: Only process Australia NPI data for year 2000, 2006, 2012
# TODO:
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Get emission species first so can name log appropriately
    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[1]
    if ( is.na( em ) ) em <- "BC"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'common_data.R', "data_functions.R",
                  "analysis_functions.R" ) # Additional function files required.
    log_msg <- "Generating Australia emission inventory data" # First message to be printed to the log
    script_name <- paste0( em, "-E.Australia_emission_2018Update.R" )

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define parameters for inventory-specific script

# For each Module E script, define the following parameters:
# Inventory parameters. Provide the inventory and mapping file names, the
#   mapping method (by sector, fuel, or both), and the regions covered by
#   the inventory (as a vector of iso codes)
    inventory_data_file <- paste0( em, '_Australia_UNFCCC_and_NPI_2018Update' )
    inv_data_folder <- "EM_INV"
    subfolder_name <- 'Australia/'
    sector_fuel_mapping <- 'Australia_scaling_mapping'
    mapping_method <- 'sector'
    inv_name <- 'AUS_2018' #for naming diagnostic files
    region <- c( "aus" )
    inv_years<-c( 2001:2021 )
    #inv_years<-c( 2000, 2006, 2012 )

# ------------------------------------------------------------------------------
# 2 Inventory in Standard Form (iso-sector-fuel-years, iso-sector-years, etc)

    if ( em %!in% c( 'SO2', 'NOx', 'NMVOC', 'CO','NH3' ) ) {
    # Output a blank df for unsupported species
        inv_data_clean <- data.frame( )

    } else {
        file_path <- filePath( inv_data_folder, inventory_data_file,
                               extension = ".xlsx",
                               domain_extension = subfolder_name )
        inv_data = data.frame()
        # Import each year (new sheet) and add to data frame
        for ( i in seq_along( inv_years ) ) {
            sheet_name <- paste0(toString( inv_years[ i ] - 1 ),'-',toString( inv_years[ i ] ))
            inv_data_sheet <- readData( inv_data_folder,
                                        domain_extension = subfolder_name,
                                        inventory_data_file , ".xlsx",
                                        sheet_selection = sheet_name )
            #select correct columns, rename, and add new year data to tQotal inv. data frame
            keep_columns <- c( "Source", "Air (kg)" )
            inv_singleyear <- subset( inv_data_sheet, select = keep_columns )
            colnames( inv_singleyear ) <- c( 'sector', paste0( 'X', inv_years[ i ] ) )
            if ( i == 1 ) {
                inv_data <- inv_singleyear
                names( inv_data ) <- c( 'sector', paste0( 'X', inv_years[ i ] ) )
            } else {
            #     #make sure that each new column has the same sectors (since each sheet has different sector orders)
                inv_data <- dplyr::full_join(inv_data,inv_singleyear,by="sector")
                }
        }

        # Clean rows and columns to standard format
        # Rename cols; add iso cols
        inv_data_clean <- inv_data
        years <- paste( "X", inv_years, sep="" )

        # Remove rows with all NAs
     #   remove.na <- which( apply( inv_data_clean[ , years ],
     #                              1, function( x ) all.na( x ) ) )
     #   inv_data_clean2 <- inv_data_clean[ -remove.na, ]

        # Make numeric, convert from kg to kt, replace nan with 0, combine duplicate rows
        inv_data_clean[ , years ] <- sapply( inv_data_clean[ , years ], as.numeric )
        inv_data_clean[ , years ] <- as.matrix( inv_data_clean[ , years ] ) / 1000000
        inv_data_clean[ is.na( inv_data_clean ) ] <- 0
        inv_data_clean <- ddply( inv_data_clean, "sector", numcolwise(sum))
        inv_data_clean$iso <- 'aus'
        inv_data_clean <- inv_data_clean[ , c( 'iso', 'sector', years ) ]

        # write country totals

        country_total <- inv_data_clean %>%
            filter(!sector %in% c("Biogenics [*]",
                                  "Burning(fuel red., regen., agric.)/ Wildfires [*]",
                                  "Services to Air Transport [*]",
                                  "Aeroplanes [*]" ))

        writeData( country_total, domain = "DIAG_OUT", domain_extension = "country-inventory-compare/",
                   paste0('inventory_',em,'_', inv_name))

        if (length(country_total) > 0){
            country_total <- country_total %>%
                select(-sector)%>%
                group_by(iso) %>%
                summarize_each(funs(sum))

            writeData( country_total, domain = "MED_OUT",
                       paste0('E.',em,'_', inv_name, '_inventory_country_total'))
        }
    }

# ------------------------------------------------------------------------------
# 5. Output
# write standard form inventory
    writeData( inv_data_clean, domain = "MED_OUT",
               paste0( 'E.', em, '_', inv_name, '_inventory' ) )

# Every script should finish with this line
    logStop()
# END


