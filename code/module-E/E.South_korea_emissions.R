#------------------------------------------------------------------------------
# Program Name: E.South_Korea_emissions.R
# Authors' Names: Rachel Hoesly
# Program Purpose: Read emissions estimate South Korea inventory file
# Note:
# Input Files: Air pollutants_South Korea_1999_2012_Korean font.xlsx
# Output Files: F.[em]Korea_inventory.csv
# Notes:
# TODO:
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'common_data.R', "data_functions.R",
                  "emissions_scaling_functions.R" ) # Additional function files required.
    log_msg <- "test inventory data" # First message to be printed to the log
    script_name <- "E.South_korea_emissions.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[1]
    if ( is.na( em ) ) em <- "NMVOC"

# ------------------------------------------------------------------------------
# 0.5. Define parameters for inventory specific script

# Stop script if running for unsupported species
    if ( em %!in% c( 'SO2', 'NOx', 'CO', 'NMVOC','NH3','BC' ) ) {
      stop( paste( 'SKorea scaling is not supported for emission species', em,
                   'remove from script list in F1.1.inventory_scaling.R' ) )
    }
# There are PM emissions to estimate OC emissions - but currently not scaling so need to
# finish the estimates below later
    em_temp = em
# South Korea only reports SOx which is similar to SO2.
    if ( em_temp == "SO2" ) {
        em_temp <- "SOx"
    }
    if ( em_temp == "NMVOC" ) {
        em_temp <- "VOCs"
    }
    if ( em_temp == "NMVOC" ) {
        em_temp <- "VOCs"
    }
    if(em %in% c ('OC')) {
        em_temp <- "PM-10"
    }

# ------------------------------------------------------------------------------
# 1. Read in initial input

# Inventory-specific parameters
    inv_data_folder <- "EM_INV"
    inv_years <- c( 1999:2018 )
    inventory_filename <- 'Korea_CAPSS_Emissions'
    translation_filename <- 'Korea_CAPSS_Emissions_Translation'
    subfolder_name <- 'Korea/'
    inv_name <- 'KOR'  # For naming diagnostic files

    # inventory is in metric tons

    if ( em_temp == "BC" ) {
        inv_years <- c( 2014:2018 ) }

# Translation Map
    translation_map <- readData( inv_data_folder, domain_extension =
                                     subfolder_name, translation_filename , ".xlsx",
                                 sheet_selection = "Translation")
# ------------------------------------------------------------------------------
# 2. Read in inventory data

# Read in inventory data
    read_inventory_list <- list()
    for (i in seq_along(inv_years)){
        read_in_data <- readData( inv_data_folder, domain_extension =
                                                       subfolder_name,
                                    inventory_filename , ".xlsx", to_numeric = TRUE,
                                    sheet_selection =  as.character(inv_years[i]))
        names(read_in_data) <- read_in_data[1,]
        read_in_data <- read_in_data[-1,]

    read_inventory_list[[i]]  <- read_in_data %>%
        mutate(year = inv_years[i]) %>%
        left_join(translation_map) %>%
        select(sector, year, em_temp) %>%
        dplyr::rename(value = any_of(em_temp)) %>%
        mutate(value = as.numeric(value)) %>%
        filter(!is.na(sector))

    }

# ------------------------------------------------------------------------------
# 3. Process Inventory Data

# create list for na replacement
    na_correction <- as.list(rep(0, length(inv_years)) )
    names(na_correction) <- inv_years

# final process data
    inv_data <- do.call(bind_rows, read_inventory_list) %>%
        as_tibble() %>%
        mutate(iso = 'kor') %>%
        mutate(unit = 'kt') %>%
        mutate(value = as.numeric(value)/1000) %>% #convert from ton to kt
        spread(year, value)  %>%
        replace_na(na_correction) %>%
        select(iso, sector, everything())

# ------------------------------------------------------------------------------
# 4. Emission Specific corrections

# Process OC
# This currently doesn't work - need to fix later
    if (em %in% c ('OC') ) {

        # Define parameters for BC and OC specific script

        ceds_sector <- "1A3b_Road"
        inv_iso <- "kor"
        PM <- "PM10"

        # Read in scaling mapping file and filter transportation sectors
        mapping_file <- readData("SCALE_MAPPINGS", "S_Korea_scaling_mapping.csv") %>%
            filter(str_detect(road_flag,"Road"))
        inv_sector_name <- mapping_file$inv_sector

        # Match formatting from PM2.5 inventory to BC/OC script
        X_inv_years <- paste0("X",inv_years)
        inv_data_sheet <- inv_data

        # Calculate BC and OC emissions

        inv_data_sheet <- F.Estimate_BC_OC_emissions(em,PM,inv_iso,ceds_sector,inv_sector_name,X_inv_years)
        inv_data <- inv_data_sheet
    }

# VOC energy transport and storage correction
# Split "Energy transport and storage" between road and non road proportionally
    if(em == 'NMVOC'){
        #calculate road percent of Energy transport and storage
        road_nonroad_ratio <- inv_data %>%
            filter(sector %in% c('Road transportation','Non Road transportation')) %>%
            gather(year, value,-iso,-sector,-unit) %>%
            spread(sector, value) %>%
            mutate(road_percent = `Road transportation`/(`Non Road transportation`+`Road transportation`)) %>%
            select(iso, year, road_percent)

        #calculation share of energy transport to be added to road and non road
        split_energy_transport <- road_nonroad_ratio %>%
            left_join(inv_data %>% filter(sector == "Energy transport and storage") %>%
                          gather(year, value,-iso,-sector,-unit) %>%
                          select(year, value) ) %>%
            mutate(`Road transportation` = value*road_percent) %>%
            mutate(`Non Road transportation` = value*(1-road_percent)) %>%
            select(year, `Road transportation`, `Non Road transportation`) %>%
            gather(sector, energy_split,-year)

        #add energy transport share to road and non road sectors
        new_road_nonroad <- inv_data %>%
            filter(sector %in% c('Road transportation','Non Road transportation')) %>%
            gather(year, value,-iso,-sector,-unit) %>%
            left_join(split_energy_transport) %>%
            mutate(new_value = value+energy_split) %>%
            select(iso, sector, year, unit, new_value) %>%
            spread(year, new_value)

        #replace old road and non road with new values that include energy transport and storage
        inv_data <- inv_data %>%
            filter(sector %!in% c('Road transportation','Non Road transportation')) %>%
            bind_rows(new_road_nonroad)

    }

# write standard form inventory
    writeData( inv_data, domain = "MED_OUT",
               paste0( 'E.', em, '_', inv_name, '_inventory' ) )
    inventory_data_file <- paste0( 'E.', em, '_', inv_name, '_inventory' )
    inv_data_folder <- 'MED_OUT'


# Every script should finish with this line
    logStop()
# END
