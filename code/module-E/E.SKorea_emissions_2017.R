# ------------------------------------------------------------------------------
# Program Name: E.SKorea_emissions_2017.R
# Author(s): Leyang Feng, Andrea Mott
# Date Last Updated: March 26, 2021
# Program Purpose: To read in and reformat South Korea emissions data.
# Input Files: Air_pollutants_South_Korea_1999_2017_translation.xlsx
# Output Files: E.[EM]_KOR2017_inventory.csv, E.[EM]_KOR2017_inventory_country_total.csv
# Notes: inventory methodology changed in year 2009, making many values 0.
        # Changed all values for 2009 to 0 in line 96 for continuity.
# TODO:
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Get emission species first so can name log appropriately
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[1]
if ( is.na( em ) ) em <- "NOx"

em.read <- em
if (em %in% c ('BC','OC')) em.read <- "PM10"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'common_data.R', "data_functions.R", "analysis_functions.R","emissions_scaling_functions.R" ) # Additional function files required.
    log_msg <- "Generating South Korean emission inventory data" # First message to be printed to the log
    script_name <- paste0( em, "-E.SKorea_emission_2017.R" )

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define parameters for inventory specific script

# Stop script if running for unsupported species
#if ( em %!in% c( 'SO2', 'NOx', 'NMVOC', 'CO' ) ) {
#  stop (paste( ' South Korea scaling is not supported for emission species', em, 'remove from script
#               list in F1.1.inventory_scaling.R') )
#}

# For each Module E script, define the following parameters:
# Inventory parameters. Provide the inventory and mapping file names, the
#   mapping method (by sector, fuel, or both), and the regions covered by
#   the inventory (as a vector of iso codes)
    inventory_data_file <- 'Air_pollutants_South_Korea_1999_2017_translation'
    subfolder_name <- 'Korea/'
    inv_data_folder <- "EM_INV"
    sector_fuel_mapping <- 'S_Korea_scaling_mapping'
    mapping_method <- 'sector'
    inv_name <- 'KOR2017' #for naming diagnostic files
    region <- c( "kor" )
    inv_years<-c( 1999:2017 )

# ------------------------------------------------------------------------------
# 2. Read in the inventory

    if ( em.read %!in% c( 'SO2', 'NOx', 'NMVOC', 'CO','NH3','PM2.5','PM10','BC','OC' ) ) {
    # write out a dummy file for unsupported species
        inv_data_species <- data.frame( )

    } else {
    # Import Sheets containing 1999:2017

        for (year in inv_years) {
            sheet_name <- as.character(year)
            inv_data_sheet_one <- readData( inv_data_folder, inventory_data_file,
                                            ".xlsx",
                                            sheet_selection = sheet_name,
                                            domain_extension = subfolder_name )

    # 3. Convert to standard format

    # Trim each dataframe to the desired rows and rename
        keep_columns <- c( 'Source middle category','NOx','SOx','CO', 'PM10' , 'VOC', 'NH3')
        col_names <- c( 'Sector','NOx','SO2','CO', 'PM10' , 'NMVOC', 'NH3')
        dfyear <- subset( inv_data_sheet_one, select = keep_columns )
        colnames( dfyear) <- col_names
        dfyear[is.na(dfyear)] = 0

    # Sum by Source
        dfyear <- dfyear %>%
            gather(non.co2, value, -c(Sector)) %>%
            group_by( Sector, non.co2) %>%
            dplyr::mutate(value = sum(value) ) %>%
            distinct( Sector, non.co2, value ) %>%
            filter(non.co2 == em.read) %>%
            mutate(Year = year)

        assign(paste0("df",year),dfyear)
}

        # inventory methodology changed in year 2009, making many values 0.
        # Change all values to 0 for continuity.
        df2009$value <- 0

    # Make sure each df contains all sectors, even those with 0 emissions for
    # that year.
    # TODO: make this more resilient (lapply, sapply, create loop?)

        inv_data_species <- df2017 %>%
            bind_rows(df2016) %>%
            bind_rows(df2015) %>%
            bind_rows(df2014) %>%
            bind_rows(df2013) %>%
            bind_rows(df2012) %>%
            bind_rows(df2011) %>%
            bind_rows(df2010) %>%
            bind_rows(df2009) %>%
            bind_rows(df2008) %>%
            bind_rows(df2007) %>%
            bind_rows(df2006) %>%
            bind_rows(df2005) %>%
            bind_rows(df2004) %>%
            bind_rows(df2003) %>%
            bind_rows(df2002) %>%
            bind_rows(df2001) %>%
            bind_rows(df2000) %>%
            bind_rows(df1999) %>%
            spread(key = Year, value = value) %>%
            ungroup() %>%
            select(-non.co2)
        inv_data_species[is.na(inv_data_species)] = 0

    # Convert from kg to kt
    # TODO: make this by year range (currently manually added)
        names(inv_data_species)[2:20] <- paste0('X',inv_years)
        inv_data_species[ , paste0( 'X', inv_years ) ] <-
          as.matrix( inv_data_species[ , paste0( 'X', inv_years ) ] ) / 1000000

    # Clean rows and columns to standard format
        inv_data_species$iso <- 'kor'
        inv_data_species$unit <- 'kt'
        colnames(inv_data_species)[1] <- "sector"
        inv_data_species <- inv_data_species[ , c( 'iso', 'sector', 'unit',
                                                   paste0( 'X', inv_years ) ) ]

    # Write out inventory country totals
            # PM10 inventory total not relevant.
            # Remove sectors not in CEDS country-level.
            # Note: country total for 2009 is 0 (removed due to discontinuities)
           if (em != "PM10")
               {country_total <- inv_data_species %>%
                filter( !sector %in% c("Aviation",
                                       "Forest fire and fire") )

                   writeData( country_total, domain = "DIAG_OUT", domain_extension = "country-inventory-compare/",
                              paste0('inventory_',em,'_', inv_name))

               country_total <- country_total %>%
                select(-c(sector,unit))%>%
                group_by(iso) %>%
                summarize_each(funs(sum))

            writeData( country_total, domain = "MED_OUT",
                       paste0('E.',em,'_', inv_name, '_inventory_country_total'))
           }
    }

# ------------------------------------------------------------------------------
    # Find BC and OC emissions

    if (em %in% c ('BC','OC') ) {

    # Define parameters for BC and OC specific script

    ceds_sector <- "1A3b_Road"
    inv_iso <- "kor"
    PM <- "PM10"

    # Read in scaling mapping file and filter transportation sectors
    mapping_file <- readData("SCALE_MAPPINGS", "S_Korea_scaling_mapping.csv")
    mapping_file <- mapping_file %>%
        filter(str_detect(road_flag,"Road"))
    inv_sector_name <- mapping_file$inv_sector

    # Match formatting from PM2.5 inventory to BC/OC script
    X_inv_years <- paste0("X",inv_years)
    inv_data_sheet <- inv_data_species %>% select(-unit)
    inv_data_sheet[is.na(inv_data_sheet)] = 0

    # Calculate BC and OC emissions

        inv_data_sheet <- F.Estimate_BC_OC_emissions(em,PM,inv_iso,ceds_sector,inv_sector_name,X_inv_years)
        inv_data_species <- inv_data_sheet
    }

# ------------------------------------------------------------------------------
# 4. Output
# write standard form inventory
    writeData( inv_data_species , domain = "MED_OUT",
               paste0( 'E.', em, '_', inv_name, '_inventory' ) )

# Every script should finish with this line

    logStop()
# END

