# ------------------------------------------------------------------------------
# Program Name: E.Australia_NPI_emissions.R
# Author(s): Erin McDuffie, based on original from Leyang Feng, Steven Smith
# Date Last Updated: September 17, 2020
# Program Purpose: To read in and reformat Australia NPI data.
# Input Files: [em]_Australia_NPI.xlsx (old, don't update), Aus_total_air_emissions_by_sector.csv
# Output Files: E.[EM]_Australia_inventory.csv, E.[EM]_Australia_inventory_country_total.csv
# TODO:
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Get emission species first so can name log appropriately
    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[1]
    if ( is.na( em ) ) em <- "SO2"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'common_data.R', "data_functions.R",
                  "analysis_functions.R" ) # Additional functionfiles required.
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
    inventory_data_file <- paste0( 'Aus_total_air_emissions_by_sector' )
    inv_data_folder <- "EM_INV"
    subfolder_name <- 'Australia/'
    inv_name <- 'AUS_NPI' #for naming diagnostic files
    region <- c( "aus" )
    inv_years<-c( 1999:2022 )

# ------------------------------------------------------------------------------
# 2 Inventory in Standard Form (iso-sector-fuel-years, iso-sector-years, etc)

# For scaling, new code focuses on processing point source time series for AUS NPI
# Only sectors dominated by point sources are scaled (elec and metals processing)

# Note that we also want country totals to be written out for comparison, so will also
# read in one year of NPI summary files to extract diffuse emissions using previous code

    diffuse_inv_data = data.frame()
    if ( em %!in% c( 'SO2', 'NOx', 'NMVOC', 'CO','NH3' ) ) {
    # Output a blank df for unsupported species
        inv_data_clean <- data.frame( )
    } else {
        file_path <- filePath( inv_data_folder, inventory_data_file,
                               extension = ".csv",
                               domain_extension = subfolder_name )
        inv_data_sheet <- readData( inv_data_folder,
                                        domain_extension = subfolder_name,
                                        inventory_data_file , ".csv",
                                        sheet_selection = sheet_name )
        # Select relevant emission species
        inv_data_selected <- inv_data_sheet %>% filter( emission == em ) %>% select(-substance_id,-emission)

        # Re-process into calendar years
        # Year indicates row with the 2nd half of the named year
        inv_data_selected$year = substr(inv_data_selected$report_year, 0, 4)
        inv_data_selected$year <- as.numeric(inv_data_selected$year)
        if ( max(inv_data_selected$year) != max(inv_years) ) {
            stop( paste( 'inv_year and last data year do not match for ', em,' in ',inv_name,
                         ', Please check', script_name ) )
        }
        inv_data_selected$report_year <- NULL
        inv_data_selected$air_total_emission_kg <- as.numeric(inv_data_selected$air_total_emission_kg)
        inv_data_selected$iso <- "aus"

        # Now want to average to get calendar year values
        shifted_data <- inv_data_selected %>% dplyr::mutate(year = year + 1 ) %>%
                   mutate(air_total_emission_kg_yr2 = air_total_emission_kg ) %>% select(-air_total_emission_kg)
        annualized_data <- left_join(inv_data_selected, shifted_data) %>%
                    # Remove first calendar year since data is incomplete
                    filter(!year %in% min(inv_data_selected$year) ) %>%
                    # Double any values missing for 2nd half of specified year (these are generally smaller emissions and likely not used)
                    mutate( air_total_emission_kg_yr2 = ifelse(is.na(air_total_emission_kg_yr2), air_total_emission_kg, air_total_emission_kg_yr2) ) %>%
                    # Now take average of two parts
                    mutate( air_total_emission_kg = (air_total_emission_kg_yr2 + air_total_emission_kg)/2 ) %>% select(-air_total_emission_kg_yr2)

        # Change to wide format
        inv_data_wide <- tidyr::pivot_wider(annualized_data, names_from = year, values_from = air_total_emission_kg)

        # Add note for metadata
        meta_names <- c( "Data.Type", "Emission", "Region", "Sector", "Start.Year",
                         "End.Year", "Source.Comment" )
        meta_note <- c( "Emissions", "All", "aus", "1A2g+2C4_Non-Ferrous-metals",
                        min(inv_years), max(inv_years), paste( "Data has been shifted to represent calendar years instead of native AUS reporting year.") )
        source_info <- script_name
        addMetaData( meta_note, meta_names, source_info )

        # For SO2, write summary time series used for adjusting off-line metal smelting estimates
        # within CEDS_Data
        if (em == "SO2") {
            inv_data_temp <- inv_data_wide
            names(inv_data_temp)[names(inv_data_temp) == 'primary_anzsic_class_name'] <- 'sector'

            inv_data_temp$sector <- '1A2g+2C4_Non-Ferrous-other-metals'
            SO2_non_ferrous_smelting_noAl <- inv_data_temp %>%
                filter(primary_anzsic_class_code %in% c("2133","2139","2141") ) %>%
                select(-primary_anzsic_class_code) %>%
                group_by(iso, sector) %>% summarize_if (is.numeric, sum)

            # Note 1998/1999 data is missing some key non-ferrous metals (but elec looks ok)
            # so put NA's for those categories for 1999, but can use 1999 for elec.
            inv_data_wide$'1999'[inv_data_wide$primary_anzsic_class_code == '2133'] <- '0'
            inv_data_wide$'1999'[inv_data_wide$primary_anzsic_class_code == '2139'] <- '0'
            inv_data_wide$'1999'[inv_data_wide$primary_anzsic_class_code == '2141'] <- '0'

            inv_data_temp$sector <- '1A2g+2C3_Aluminum-production'
            SO2_non_ferrous_Al_smelting <- inv_data_temp %>%
                filter(primary_anzsic_class_code %in% c("2131","2132") ) %>%
                select(-primary_anzsic_class_code) %>%
                group_by(iso, sector) %>% summarize_if (is.numeric, sum)
            inv_data_wide$'1999'[inv_data_wide$primary_anzsic_class_code == '2131'] <- '0'
            inv_data_wide$'1999'[inv_data_wide$primary_anzsic_class_code == '2132'] <- '0'

            writeData(SO2_non_ferrous_smelting_noAl, domain = "MED_OUT",
                      paste0('E.',em,'_', inv_name, '_non_ferrous_smelting_noAl'))
            writeData(SO2_non_ferrous_Al_smelting, domain = "MED_OUT",
                      paste0('E.',em,'_', inv_name, '_non_ferrous_smelting_Al'))
        }

        # Clean rows and columns to standard format
        # Rename cols; add iso cols
        inv_data_clean <- inv_data_wide %>% select(-primary_anzsic_class_code)
        names(inv_data_clean)[names(inv_data_clean) == 'primary_anzsic_class_name'] <- 'sector'
        years <- paste( "X", inv_years, sep="" )
        names(inv_data_clean) <- c("sector","iso",years)

         # Make numeric, convert from kg to kt, replace nan with 0, combine duplicate rows
        inv_data_clean[ , years ] <- sapply( inv_data_clean[ , years ], as.numeric )
        inv_data_clean[ , years ] <- as.matrix( inv_data_clean[ , years ] ) / 1000000
        inv_data_clean[ is.na( inv_data_clean ) ] <- 0
        inv_data_clean <- ddply( inv_data_clean, "sector", numcolwise(sum))
        inv_data_clean$iso <- 'aus'
        inv_data_clean <- inv_data_clean[ , c( 'iso', 'sector', years ) ]

        # Now read in one year for the individual species inventory files so that
        # can add to get approximate total country emissions
        inventory_data_file <- paste0( em, '_Australia_NPI' )
        diffuse_inv_years<-c( 2021 )
        # Import one year and add to data frame
        i = 1
            sheet_name <- paste0(toString( diffuse_inv_years[ i ] - 1 ),'-',toString( diffuse_inv_years[ i ] ))
            inv_data_sheet <- readData( inv_data_folder,
                                        domain_extension = subfolder_name,
                                        inventory_data_file , ".xlsx",
                                        sheet_selection = sheet_name )
            #select correct columns, rename, and add new year data to tQotal inv. data frame
            keep_columns <- c( "Source", "Air (kg)" )
            inv_singleyear <- subset( inv_data_sheet, select = keep_columns )
            colnames( inv_singleyear ) <- c( 'sector', paste0( 'X', diffuse_inv_years[ i ] ) )
            diffuse_inv_data <- inv_singleyear
            names( diffuse_inv_data ) <- c( 'sector', paste0( 'X', diffuse_inv_years[ i ] ) )

        diffuse_inv_data$iso <- 'aus'
        for (year in years) {
            diffuse_inv_data[[year]] <- diffuse_inv_data$X2021
        }

    }

# ------------------------------------------------------------------------------
# 5. Output
# write standard form inventory
    writeData( inv_data_clean, domain = "MED_OUT",
               paste0( 'E.', em, '_', inv_name, '_inventory' ) )

# write country totals
# Need to read in at least one NPI year to get background diffuse emission estimates

    if ( em %in% c( 'SO2', 'NOx', 'NMVOC', 'CO','NH3' ) ) {
        diffuse_sectors <- diffuse_inv_data$sector
        diffuse_sectors <- diffuse_sectors[grepl("\\*", diffuse_sectors)]
        diffuse_inv_data <- diffuse_inv_data %>% filter(sector %in% diffuse_sectors)
        diffuse_inv_data[ , years ] <- sapply( diffuse_inv_data[ , years ], as.numeric )
        diffuse_inv_data[ , years ] <- as.matrix( diffuse_inv_data[ , years ] ) / 1000000
        diffuse_inv_data[ is.na( diffuse_inv_data ) ] <- 0

        diffuse_inv_data <- diffuse_inv_data %>%
            filter(!sector %in% c("Biogenics [*]",
                                  "Burning(fuel red., regen., agric.)/ Wildfires [*]",
                                  "Services to Air Transport [*]",
                                  "Aeroplanes [*]" ))

        # Approximation of country total, given that diffuse emissions have not time trends
        country_total <- full_join(diffuse_inv_data,inv_data_clean)
        country_total <- country_total[ , c( 'iso', 'sector', years ) ]

        # Remove 1999 since removed incompete SO2 data
        country_total$'X1999' <- NULL

        writeData( country_total, domain = "DIAG_OUT", domain_extension = "country-inventory-compare/",
                   paste0('inventory_',em,'_', inv_name))

        if (length(country_total) > 0){
            country_sum <- country_total %>%
                select(-sector)%>%
                group_by(iso) %>%
                summarize_each(funs(sum))

            writeData( country_sum, domain = "MED_OUT",
                       paste0('E.',em,'_', inv_name, '_inventory_country_total'))
        }
    }
# Every script should finish with this line
    logStop()
# END


