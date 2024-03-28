# ------------------------------------------------------------------------------
# Program Name: Compare_inventory_totals_to_CEDS_GAINS_EDGAR.R
# Author: Andrea Mott, Harrison Suchyta
# Date Last Updated: February 4, 2021
# Program Purpose: Generate comparisons between inventory country totals and
#               CEDS, GAINS, EDGAR, EDGAR-HTAPv3, and REAS. Removes international
#               shipping and air/forest fires etc
# Input Files: [em]_total_CEDS_emissions.csv
#              'E.', em,'_',inv,'_inventory_country_total'
#              Master_Country_List.csv, emf-30_ctry_map.csv,
#              emf-30_comparison_sector_map-comb_vs_process.csv
# Output Files: Compare_inventory_to_CEDS_[em].pdf
#               Compare_inventory_to_CEDS_[em].csv
#               CEDS_[em]_[inv]_compare_to_inv.csv

# TODO: 1. eventually add in BC, OC, and GHGs
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if( "input" %in% dir( ) ) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", 'common_data.R', 'IO_functions.R' ) # Additional function files may be required.
log_msg <- "Comparing invenotry emissions to CEDS, GAINS, EDGAR, and REAS..." # First message to be printed to the log
script_name <- "Compare_inventory_to_CEDS_GAINS_EDGAR.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "BC"

#flag for whether or not to include HTAP in comparisons
add_HTAP <- TRUE

# Inventory Plot Labels
CEDS_label <- 'CEDS_current_update'
EDGAR_label <- 'EDGARv6.0'
EDGAR_HTAP_label <- 'EDGAR_HTAPv3'
GAINS_label <- 'GAINS (ECLIPSE V6b CLE)'
REAS_label <- 'REASv3.2'


# Color Palette

purple <- "#9882FF"
dark_blue <- "#4477FD"
marigold <- "#FFCF00"
red <- "#DC2653"
green <- "#18C18C"
black <- "#000000"
orange <- "#FB7624"

# Plot
plot_start <- 1990

# ---------------------------------------------------------------------------
# 1. Run for relevant inventory emissions

em_list <- c(  'SO2','NOx', 'NH3', 'NMVOC', 'CO', 'BC')

if (em %in% em_list) {

# ---------------------------------------------------------------------------
# 2. Inventory data

    MCL <- readData( domain = 'MAPPINGS', file_name = 'Master_Country_List' )

# Load in country inventory totals

    # Emissions present in inventory data
    # TODO: eventually read BC, OC, and GHGs
        em_list_us <- c( 'SO2', 'NOx', 'NH3', 'NMVOC', 'CO')
        em_list_jpn <- c( 'SO2', 'NOx', 'NH3', 'NMVOC', 'CO')
        em_list_can <- c( 'SO2', 'NOx', 'NH3','NMVOC', 'CO', 'BC')
        em_list_old_can <- c( 'SO2', 'NOx', 'NH3','NMVOC', 'CO')
        em_list_kor <- c( 'CO', 'NH3', 'NMVOC', 'NOx', 'SO2', 'BC')
        em_list_kor_old <- c( 'CO', 'NH3', 'NMVOC', 'NOx', 'SO2')
        em_list_EMEP <- c( 'CO', 'NH3', 'NMVOC', 'NOx', 'SO2', 'BC')
        em_list_twn <- c( 'CO', 'NMVOC', 'NOx', 'SO2', 'BC')  # Taiwan doesn't have NH3
        em_list_MEIC <- c( 'CO', 'NH3', 'NMVOC', 'NOx', 'SO2','BC')
        em_list_aus <- c( 'CO', 'NH3', 'NMVOC', 'NOx', 'SO2')
        em_list_old_chn <- c( 'CO', 'NH3', 'NMVOC', 'NOx', 'SO2')

        # set emission species and set flags
        # em <- em_list[h]

        us_em_flag <- em %in% em_list_us
        jpn_em_flag <- em %in% em_list_jpn
        can_em_flag <- em %in% em_list_can
        can_old_em_flag <- em %in% em_list_old_can
        kor_em_flag <- em %in% em_list_kor
        kor_old_em_flag <- em %in% em_list_kor_old
        EMEP_em_flag <- em %in% em_list_EMEP
        twn_em_flag <- em %in% em_list_twn
        MEIC_em_flag <- em %in% em_list_MEIC
        chn_old_em_flag <- em %in% em_list_old_chn
        aus_em_flag <- em %in% em_list_aus

    # Read in inventory data
    if ( us_em_flag == T ) {
        inv_emissions_US <- readData( domain = 'MED_OUT', file_name =  paste0( 'E.', em,'_',"US",'_inventory_country_total'))
        inv_emissions_US <- subset(inv_emissions_US, select = -c(34:35))

    } else {
        printLog( paste0( em, ' is not supported by CEDS, dummy data created. ' ) )
        inv_emissions_US <- data.frame( iso = "usa")
    }

    if ( jpn_em_flag == T ) {
        inv_emissions_jpn <- readData( domain = 'MED_OUT', file_name =  paste0( 'E.', em,'_',"Japan",'_inventory_country_total'))
    } else {
        printLog( paste0( em, ' is not supported by CEDS, dummy data created. ' ) )
        inv_emissions_jpn <- data.frame( iso = "jpn")
    }

    if ( can_em_flag == T ) {
        inv_emissions_can <- readData( domain = 'MED_OUT', file_name =  paste0( 'E.', em,'_',"CAN_2021",'_inventory_country_total'))
    } else {
        printLog( paste0( em, ' is not supported by CEDS, dummy data created. ' ) )
        inv_emissions_can <- data.frame( iso = "can")
    }

    if ( kor_em_flag == T ) {
        inv_emissions_kor <- readData( domain = 'MED_OUT', file_name =  paste0( 'E.', em,'_',"KOR",'_inventory_country_total'))
        inv_emissions_kor$X2009 <- NA
    } else {
        printLog( paste0( em, ' is not supported by CEDS, dummy data created. ' ) )
        inv_emissions_kor <- data.frame( iso = "can")
    }

    if ( EMEP_em_flag == T ) {
        inv_emissions_EMEP <- readData( domain = 'MED_OUT', file_name =  paste0( 'E.', em,'_',"EMEP_NFR14",'_inventory_country_total'))
        inv_emissions_EMEP[is.na(inv_emissions_EMEP)] = 0
        # note: replacing NAs with 0 means it sums everything, not just the columns that don't have NAs.

    } else {
        printLog( paste0( em, ' is not supported by CEDS, dummy data created. ' ) )
        inv_emissions_EMEP <- data.frame( iso = "EMEP")
    }

    if ( twn_em_flag == T ) {
        inv_emissions_twn <- readData( domain = 'MED_OUT', file_name =  paste0( 'E.', em,'_',"TWN",'_inventory_country_total'))
    } else {
        printLog( paste0( em, ' is not supported by CEDS, dummy data created. ' ) )
        inv_emissions_twn <- data.frame( iso = "twn")
    }

    if ( MEIC_em_flag == T ) {
        inv_emissions_MEIC <- readData( domain = 'MED_OUT', file_name =  paste0( 'E.', em,'_',"CHN_2018",'_inventory_country_total'))
    } else {
        printLog( paste0( em, ' is not supported by CEDS, dummy data created. ' ) )
        inv_emissions_MEIC <- data.frame( iso = "chn")
    }

    if ( aus_em_flag == T ) {
        inv_emissions_aus <- readData( domain = 'MED_OUT', file_name =  paste0( 'E.', em,'_',"AUS_2018",'_inventory_country_total'))
    } else {
        printLog( paste0( em, ' is not supported by CEDS, dummy data created. ' ) )
        inv_emissions_aus <- data.frame( iso = "aus")
    }

    # Map EMEP to East and West regions. Remove EMEP's non-Europe regions.
    Master_Country_List <- readData( domain = 'MAPPINGS', file_name = 'Master_Country_List' )
    master_country <- Master_Country_List %>%
        select(c(iso, Region))
    # cut off years before 1990 since data becomes increasingly incomplete
    inv_emissions_East_West_Europe <- inv_emissions_EMEP[c('iso',paste0('X',1990:2021))] %>%
        left_join(master_country %>% unique(), by = c("iso")) %>%
        filter(Region %in% c("Eastern Europe","Western Europe")) %>%
        select(iso, Region, starts_with('X')) %>%
        unique() %>%
        select(-iso) %>%
        dplyr::rename(iso = Region) %>%
        group_by(iso) %>%
        summarize_if(is.numeric, sum)


    # Bind inventory rows together.
    inv_total <- bind_rows(inv_emissions_jpn, inv_emissions_US, inv_emissions_can, inv_emissions_kor,
                           inv_emissions_East_West_Europe,inv_emissions_MEIC,inv_emissions_twn, inv_emissions_aus)
#inv_emissions_aus
    # change to long
    country_inventory_long <- inv_total %>%
        select(iso, contains('X')) %>%
        gather(year, inventory, -c(iso))

# ---------------------------------------------------------------------------
printLog("Load old inventory data")
    # Load in old inventory data

    if ( can_old_em_flag == T ) {
        inv_emissions_can_old <- readData( domain = 'MED_OUT', file_name =  paste0( 'E.', em,'_',"CAN_to2011",'_inventory_country_total'))
    } else {
        printLog( paste0( em, ' is not supported by CEDS, dummy data created. ' ) )
        inv_emissions_can_old <- data.frame( iso = "can_old", X2010 = NA)
    }

    if ( chn_old_em_flag == T ) {
        inv_emissions_chn_old <- readData( domain = 'MED_OUT', file_name =  paste0( 'E.', em,'_',"CHN",'_inventory_country_total'))
    } else {
        printLog( paste0( em, ' is not supported by CEDS, dummy data created. ' ) )
        inv_emissions_chn_old <- data.frame( iso = "chn_old")
    }
    if ( kor_old_em_flag == T ) {
        inv_emissions_kor_old <- readData( domain = 'MED_OUT', file_name =  paste0( 'E.', em,'_',"KOR2017",'_inventory_country_total'))
    } else {
        printLog( paste0( em, ' is not supported by CEDS, dummy data created. ' ) )
        inv_emissions_kor_old <- data.frame( iso = "kor_old")
    }

    old_inv_total <- bind_rows(inv_emissions_can_old, inv_emissions_chn_old,inv_emissions_kor_old)

    # change to long
    old_inventory_long <- old_inv_total %>%
        gather(year, Old_Inventory, -c(iso)) %>%
        mutate(year = as.numeric(sub('X','',year))) %>%
        filter(!is.na(Old_Inventory))


# ---------------------------------------------------------------------------
# 3. CEDS
    printLog("Load CEDS data")
    # Load in CEDS country totals
    ceds_emissions <- readData( domain = 'MED_OUT', file_name =  paste0( em,'_total_CEDS_emissions' ) )

    # remove 1A3dii_Domestic-navigation from USA and Canada
    # ceds_emissions <- ceds_emissions %>%
    #     filter(!iso %in% c("usa","can") | !sector == "1A3dii_Domestic-navigation")

    # CEDS sectors that are not inventory specific
    ceds_remove_sectors <- c( "1A3ai_International-aviation",
                              "1A3di_International-shipping",
                              '1A3aii_Domestic-aviation',
                              '7A_Fossil-fuel-fires',
                              '3F_Agricultural-residue-burning-on-fields',
                              '11A_Volcanoes',
                              '11B_Forest-fires',
                              '11C_Other-natural',
                              '6B_Other-not-in-total')

    # remove sectors
    ceds_emissions <- ceds_emissions[ -which( ceds_emissions$sector %in% ceds_remove_sectors ), ]

    # filter sectors and isos of CEDS to East and Western European countries.
    ceds_East_West_Europe_by_iso <- ceds_emissions %>%
        left_join(master_country, by = c("iso")) %>%
        filter(Region %in% c("Eastern Europe","Western Europe")) %>%
        unique() %>%
        group_by(iso,Region) %>%
        summarize_if(is.numeric, sum, na.rm = TRUE)

    ceds_East_West_Europe <- ceds_emissions %>%
        left_join(master_country, by = c("iso")) %>%
        filter(Region %in% c("Eastern Europe","Western Europe")) %>%
        unique %>%
        select(-c(iso,fuel,units)) %>%
        dplyr::rename(iso = Region) %>%
        group_by(iso,sector) %>%
        summarize_each(funs(sum))

     # ---
            # write out diagnostic of CEDS by iso and sector after sectors removed.
            ceds_by_iso_sector <- ceds_emissions %>%
                select(-c(fuel,units)) %>%
                group_by(iso,sector) %>%
                summarize_each(funs(sum))
            ceds_by_iso_sector <- bind_rows(ceds_East_West_Europe,ceds_by_iso_sector)

            # keep only the isos:
            ceds_by_iso_sector_inv_isos <- ceds_by_iso_sector %>%
                filter(iso %in% c("usa","jpn","Eastern Europe","Western Europe", "twn", "kor","can","aus"))

            # cut off years before 1980 for incomplete data
                ceds_by_iso_sector_inv_isos <- subset(ceds_by_iso_sector_inv_isos, select = -c(3:232))

            # write out CEDS data, filtered by inventory
                inv_list <- c("usa","jpn","Eastern Europe","Western Europe", "twn", "kor","can","aus")
                for (i in seq_along(inv_list)) {
                    inv <- inv_list[i]
                    df <- ceds_by_iso_sector_inv_isos %>%
                        filter(iso == inv)
                    writeData( df , domain = "DIAG_OUT", domain_extension = "country-inventory-compare/",
                           paste0( "CEDS_", em,'_',inv,"_compare_to_inv" ) )
                }

            # ---

    ceds_emissions_East_West_Europe <- ceds_East_West_Europe %>%
        select(-sector) %>%
        group_by(iso) %>%
        summarize_each(funs(sum))

    # Clean CEDS emissions
    ceds <- ceds_emissions %>%
        dplyr::select( -c(fuel,units,sector )) %>%
        dplyr::group_by( iso ) %>%
        summarize_each(funs(sum))

    ceds <- bind_rows(ceds, ceds_emissions_East_West_Europe)

    # Make long format
    ceds_long <- ceds %>%
        gather(year, CEDS, -c(iso))

    #------------
    #world data
    ceds_world_regions <- ceds_emissions %>%
                          left_join(MCL, by = 'iso') %>%
                          select(-c(iso,fuel,units)) %>%
                          group_by(Region) %>%
                          dplyr::summarize_if(is.numeric,sum,na.rm = TRUE)%>%
                          select(Region,everything()) %>%
                          gather(year,CEDS, -c(Region))

# ---------------------------------------------------------------------------
# 4. GAINS
    printLog("Load GAINS data")
# Load in GAINS emissions
    # Define em string to use for GAINS data
    em_use <- em
    if ( em == "NMVOC" ){ em_use <- "VOC" }

    # Define other settings for GAINS data
    domain_use <- "EM_INV"
    domain_ext_use <- "GAINS/"

    emissions_file_name <- paste0( "Global by region-detail_emf30_", em_use, "_wSE" )
    if( em == "SO2" ){ emissions_file_name <- gsub( "_wSE", "_v2_wSE", emissions_file_name ) }

    emissions_rows_to_skip <- 9

    #   Read in GAINS data
    gains_emissions <- readData( domain = domain_use, domain_extension = domain_ext_use,
                                 file_name = emissions_file_name, skip = emissions_rows_to_skip )

    # CEDS emissions and master country list
    MFSL <- readData( domain = "MAPPINGS", file_name = "Master_Fuel_Sector_List", extension = ".xlsx",
                      sheet_selection = "Sectors" )

    # Other mapping files
    ctry_map <- readData( domain = 'MAPPINGS',  domain_extension = 'GAINS/',
                          file_name ='emf-30_ctry_map' )

# Process GAINS data

    # choose sectors to remove
    gains_remove_sectors <- c( 'SUM' )

    # Remove leading white space from GAINS data and rename sector column
        gains_emissions2 <- gains_emissions %>%
            dplyr::rename( Sector = 'EMF30.kt' ) %>%
            dplyr::select( Region, Sector, all_of(X_GAINS_years) ) %>% # ALl GAINS years
            # GAINS data can have tabs in front of all characters in the ID columns, which would need to be removed
            dplyr::mutate_at( .vars = c( "Region", "Sector" ), .funs = funs( gsub( "\t", "", . ) ) )

    # Remove sectors and replace NAs with 0
            gains_emissions2 <- gains_emissions2[ -which( gains_emissions2$Sector %in% gains_remove_sectors ), ]
            gains_emissions2[is.na(gains_emissions2)] = 0

    # Filter by regions. Match regions to those isos for graphing.
         gains <- gains_emissions2 %>%
             filter(Region %in% c("WEST_EURO","CENT_EURO","JAPA_WHOL","CHIN_PLUS","USAM_WHOL","CANA_WHOL")) %>%
             select(-Sector) %>%
             dplyr::group_by( Region ) %>%
             summarize_each(funs(sum))
         gains_Regions <- c("can","Eastern Europe",'chn',"jpn","usa","Western Europe")
         gains_w_correct_regions <- cbind(gains, gains_Regions)
         gains <- gains_w_correct_regions %>%
             dplyr::rename(iso = "gains_Regions") %>%
             select(-Region)

    # Make long format
         gains_long <- gather(gains,year, GAINS, -c(iso))
         gains_long$year <- as.numeric(sub('X','',gains_long$year))
         gains_long$iso <- as.character(gains_long$iso)

    #---------
    #World data

    #Read in mapping file
    gains_mapping_index <- read.csv('mappings/GAINS_to_master_map.csv')

    #Gets rid of row with NA value for Region
    gains_emissions_no_all_sum <- gains_emissions[!is.na(gains_emissions$Region),]

    #Replace GAINS regions with master regions
    gains_global_data <- gains_emissions_no_all_sum  %>%
                         filter(EMF30.kt != 'SUM') %>%
                         left_join(gains_mapping_index, by = 'Region') %>%
                         select(-c(Region,EMF30.kt)) %>%
                         dplyr::rename(Region = master_region) %>%
                         dplyr::mutate_at(vars(-("Region")),~replace(., is.na(.), 0)) %>%
                         select(Region,everything()) %>%
                         group_by(Region) %>%
                         dplyr::summarize_if(is.numeric,sum,na.rm = TRUE)%>%
                         gather(year,GAINS, -c(Region))
    #sub out the X's and filter to before 2020
    gains_global_data$year <- as.numeric(sub('X','',gains_global_data$year))
    gains_global_data <- filter(gains_global_data,gains_global_data$year <= 2020)

# ---------------------------------------------------------------------------
# 5. EDGAR
    printLog("Load EDGAR data")
# 1.2 Read in and load files for EDGAR

    edgar_in <- readData( domain = 'MED_OUT', file_name =  paste0( 'E.',em,'_EDGAR' ) )

    # Combine European isos into eastern and western Europe.
    edgar_all_isos <- edgar_in %>%
        left_join(master_country, by = c("iso")) %>%
        filter(Region != "Eastern Europe" | Region != "Western Europe") %>%
        group_by(iso, Region) %>%
        summarize_if(is.numeric, sum)
    edgar_europe <- edgar_all_isos %>%
        filter(Region %in% c("Eastern Europe","Western Europe")) %>%
        select(-iso) %>%
        group_by(Region) %>%
        summarize_if(is.numeric, sum) %>%
        ungroup() %>%
        dplyr::rename(iso = Region)
    edgar <- edgar_all_isos %>%
        select(-Region) %>%
        bind_rows(edgar_europe)

    # Make long
    edgar_long <- edgar %>% gather( year, EDGAR, -iso) %>%
        mutate(year = str_replace(year, "X","") %>% as.numeric)

    #---
    #World data
    edgar_world <- edgar_in %>%
        left_join(master_country, by = c("iso"))%>%
                   group_by(Region) %>%
                   dplyr::summarize_if(is.numeric,sum,na.rm = TRUE)%>%
                   select(Region,everything()) %>%
                   gather(year, EDGAR, -c(Region))


# ---------------------------------------------------------------------------
# 6. REAS
    printLog("Load REAS data")
    # Load in REAS_32 inventory data
    REAS_emissions <- readData( domain = 'MED_OUT', file_name =  paste0( 'E.',em,'_REAS32_inventory' ) )

    # Filter by inventory isos (kor, jpn, and twn)
    REAS <- REAS_emissions %>%
        filter(sector == "TOTAL") %>%
        filter(iso %in% c("kor","twn","jpn")) %>%
        select(-c(units,sector))

    # Make long
    REAS_long <- REAS %>%
        gather(year, REAS, -c(iso)) %>%
        mutate(year = as.numeric(sub('X','',year))) %>%
        mutate(REAS = as.numeric(REAS))

    #-------
    #world data
    REAS_world <- REAS_emissions %>%
                  left_join(MCL, by = 'iso') %>%
                  filter(sector != 'TOTAL') %>%
                  select(-c(sector,units,iso)) %>%
                  group_by(Region) %>%
                  dplyr::summarize_if(is.numeric,sum,na.rm = TRUE)%>%
                  select(Region,everything()) %>%
                  gather(year,REAS, -c(Region)) %>%
                  filter(Region == 'Asia')

# ---------------------------------------------------------------------------
# 7. EDGAR-HTAPv3

if(add_HTAP == TRUE){
    EDGAR_HTAP_in <- readData( domain = 'MED_OUT', file_name =  paste0( 'E.',em,'_EDGAR_HTAPv3_inventory' ) )

    # Combine European isos into eastern and western Europe.
    EDGAR_HTAP_all_isos <- EDGAR_HTAP_in %>%
        left_join(master_country, by = c("iso")) %>%
        filter(sector %!in% c('HTAPv3_2.2_International_Aviation','HTAPv3_1_International_Shipping')) %>%
        filter(Region != "Eastern Europe" | Region != "Western Europe") %>%
        group_by(iso, Region) %>%
        summarize_if(is.numeric, sum, na.rm = TRUE)
    EDGAR_HTAP_europe <- EDGAR_HTAP_all_isos %>%
        filter(Region %in% c("Eastern Europe","Western Europe")) %>%
        select(-iso) %>%
        group_by(Region) %>%
        summarize_if(is.numeric, sum, na.rm = TRUE) %>%
        ungroup() %>%
        dplyr::rename(iso = Region)
    EDGAR_HTAP <- EDGAR_HTAP_all_isos %>%
        select(-Region) %>%
        bind_rows(edgar_europe)

    # Make long
    EDGAR_HTAP_long <- EDGAR_HTAP %>%
        gather( year, EDGAR_HTAP, -iso) %>%
        mutate(year = str_replace(year, "X","") %>% as.numeric)

    #---
    #World data
    EDGAR_HTAP_world <- EDGAR_HTAP_in %>%
        filter(sector %!in% c('HTAPv3_2.2_International_Aviation','HTAPv3_1_International_Shipping')) %>%
        filter(iso %!in% c('sea','air')) %>%
        left_join(master_country, by = c("iso")) %>%
        mutate(Region = ifelse(iso == 'aia','Latin America',Region)) %>%
        mutate(Region = ifelse(iso == 'myt','Western Europe',Region)) %>%
        mutate(Region = ifelse(iso == 'shn','Western Europe',Region)) %>%
        group_by(Region) %>%
        dplyr::summarize_if(is.numeric,sum,na.rm = TRUE)%>%
        select(Region,everything()) %>%
        gather(year, EDGAR_HTAP, -c(Region))%>%
        mutate(year = str_replace(year, "X","") %>% as.numeric)

    unmatched_EDGAR_HTAP_isos <- EDGAR_HTAP_long %>%
        left_join(master_country, by = c("iso"))%>%
        group_by(Region) %>%
        filter(is.na(Region)) %>%
        filter(iso %!in% c('Eastern Europe','Western Europe')) %>%
        pull(iso)

    if(length(unmatched_EDGAR_HTAP_isos)>0){
        stop(paste0('Unmapped countries to regions in EDGAR_HTAP inventory.
                    Check mapping or corrections. ',unmatched_EDGAR_HTAP_isos))
    }
}

# ---------------------------------------------------------------------------
# 8. Prepare country inventories for plotting
    printLog("Process data for plotting.")
    # Combine all inventories into one data frame
    combined <- country_inventory_long %>%
        left_join(ceds_long, by = c("iso","year")) %>%
        mutate(year = as.numeric(sub('X','',year))) %>%
        left_join(edgar_long, by = c("iso","year")) %>%
        left_join(gains_long, by = c("iso","year")) %>%
        left_join(REAS_long, by = c("iso","year")) %>%
        left_join(old_inventory_long, by = c("iso", "year"))

    if(add_HTAP == TRUE){
        combined_w_EDGAR_HTAP <- left_join(combined, EDGAR_HTAP_long, by = c("iso","year"))
        # Make long
        combined_long <- gather(combined_w_EDGAR_HTAP, Inventory, value, -c(iso,year))
    }else{
        # Make long
        combined_long <- gather(combined_w_oldinv, Inventory, value, -c(iso,year))
    }


    # Add em column and rename value column to total emissions
    combined_long$em <- em
    combined_long <- combined_long %>% dplyr::rename(total_emissions = value)

    # Diagnostics:
        # Print out plot csv
        combined_wide <- combined_long %>%
            filter(!is.na(total_emissions)) %>%
            group_by(iso, Inventory) %>%
            tidyr::spread(year, total_emissions)

        writeData( combined_wide , domain = "DIAG_OUT", domain_extension = "country-inventory-compare/",
                   paste0("Compare_inventory_to_CEDS_", em ) )

#-------------------------------------------------------------------------------
# 9. Prepare global inventories for plotting

        #Removes the X's in the years
        edgar_world$year <- as.numeric(sub('X','',edgar_world$year))
        REAS_world$year <- as.numeric(sub('X','',REAS_world$year))
        ceds_world_regions$year <- as.numeric(sub('X','',ceds_world_regions$year))
        ceds_world_regions <- filter(ceds_world_regions, year >= 1950) %>% filter(Region != 'Global')

        #combines all the data into one dtaframe
        global_combined <- left_join(ceds_world_regions, gains_global_data, by = c("Region","year"))
        global_combined_w_edgar <- left_join(global_combined, edgar_world, by = c("Region","year"))
        global_combined_w_reas <- left_join(global_combined_w_edgar, REAS_world, by = c("Region","year"))

        #add HTAP data if present
        if(add_HTAP == TRUE){
            global_combined_w_EDGAR_HTAP <- left_join(global_combined_w_reas, EDGAR_HTAP_world, by = c("Region","year"))
            #make long
            global_combined_long <- global_combined_w_EDGAR_HTAP %>% gather(Inventory,value,-c(Region,year)) %>% dplyr::rename(total_emissions = value)
        } else{
            #make long
            global_combined_long <- global_combined_w_reas %>% gather(Inventory,value,-c(Region,year)) %>% dplyr::rename(total_emissions = value)
        }


# -------------------------------------------------------------------
# 10. plotting
        printLog("Plot Comparisons")
# rename isos for cleaner graphs
    combined_long$iso <- gsub("aus", "Australia", combined_long$iso)
    combined_long$iso <- gsub("can", "Canada", combined_long$iso)
    combined_long$iso <- gsub("jpn", "Japan", combined_long$iso)
    combined_long$iso <- gsub("chn", "China", combined_long$iso)
    combined_long$iso <- gsub("kor", "South Korea", combined_long$iso)
    combined_long$iso <- gsub("twn", "Taiwan", combined_long$iso)
    combined_long$iso <- gsub("usa", "USA", combined_long$iso)

    combined_long$Inventory <- gsub("inventory", "Country_inventory", combined_long$Inventory)

    #filter out Australia country inventory data
    combined_long <- combined_long %>% dplyr::filter(iso != 'Australia' | Inventory != 'Country_inventory')

# Plot
pdf(paste0('../diagnostic-output/country-inventory-compare/Compare_inventory_to_CEDS_',em,'.pdf'),width=11,height=9.5,paper='special', onefile=TRUE)


#original script
plot <- ggplot(combined_long, aes(x = year, y = total_emissions, color = Inventory, linetype = Inventory, shape = Inventory)) +
    geom_point(data = subset(combined_long, Inventory == "Country_inventory"), size = 2) +
    geom_point(data = subset(combined_long, Inventory == "Old_Inventory"), size = 2) +
    geom_line(data = subset(combined_long, Inventory == "CEDS"), size = 1) +
    geom_line(data = subset(combined_long, Inventory == "EDGAR"), size = 1) +
    geom_line(data = subset(combined_long, Inventory == "EDGAR_HTAP"), size = 1) +
    geom_point(data = subset(combined_long, Inventory =='GAINS'), size = 2) +
    geom_line(data = subset(combined_long, Inventory =='REAS'), size = 1) +
    scale_y_continuous(limits=c(0,max(combined_long$total_emissions)), labels = scales::comma) +
    labs(x= "" , y= paste(em ,'Emissions [Gg/yr]') )+
    xlim(plot_start,NA)+
    facet_wrap(~iso, scales = "free", nrow = 3) +
    ggtitle( paste(em)) +
    theme(panel.background=element_blank(),
          panel.grid.minor = element_line(colour="gray95"),
          panel.grid.major = element_line(colour="gray88")) +
    scale_color_manual(name = 'Inventory',
                       values = c('Country_inventory'= dark_blue,
                                  'Old_Inventory'= dark_blue,
                                  "CEDS"= red,
                                  'EDGAR'= green,
                                  'EDGAR_HTAP'=purple,
                                  'GAINS'= black,
                                  'REAS'= marigold),
                       labels = c('Country_inventory'= 'National Inventory',
                                  'Old_Inventory'= 'Old National Inventory',
                                  "CEDS"= CEDS_label,
                                  'EDGAR'= EDGAR_label,
                                  'EDGAR_HTAP'= EDGAR_HTAP_label,
                                  'GAINS'= GAINS_label,
                                  'REAS'= REAS_label)) +
    scale_linetype_manual(name= 'Inventory',
                          values = c('Country_inventory' = 0,
                                     'Old_Inventory' = 0,
                                     "CEDS" = 1,
                                     'EDGAR'= 1,
                                     'EDGAR_HTAP'= 1,
                                     'GAINS'= 0,
                                     'REAS'= 1),
                          labels = c('Country_inventory'= 'National Inventory',
                                     'Old_Inventory'= 'Old National Inventory',
                                     "CEDS"= CEDS_label,
                                     'EDGAR'= EDGAR_label,
                                     'EDGAR_HTAP'= EDGAR_HTAP_label,
                                     'GAINS'= GAINS_label,
                                     'REAS'= REAS_label)) +
    scale_shape_manual(name = "Inventory",
                       values = c("CEDS" = 32,
                                  "Country_inventory" = 16,
                                  "EDGAR" = 32,
                                  "EDGAR_HTAP" = 32,
                                  "GAINS" = 16,
                                  "Old_Inventory" = 1,
                                  "REAS" = 32 ),
                       labels = c('Country_inventory'= 'National Inventory',
                                  'Old_Inventory'= 'Old National Inventory',
                                  "CEDS"= CEDS_label,
                                  'EDGAR'= EDGAR_label,
                                  'EDGAR_HTAP'= EDGAR_HTAP_label,
                                  'GAINS'= GAINS_label,
                                  'REAS'= REAS_label))+
    theme(legend.position = "bottom")

plot_world <- ggplot(global_combined_long, aes(x = year, y = total_emissions, color = Inventory, linetype = Inventory, shape = Inventory)) +
    geom_line(data = subset(global_combined_long, Inventory == "CEDS"), size = 1) +
    geom_line(data = subset(global_combined_long, Inventory == "EDGAR"), size = 1) +
    geom_point(data = subset(global_combined_long, Inventory =='GAINS'), size = 2) +
    geom_line(data = subset(global_combined_long, Inventory == 'EDGAR_HTAP'), size = 1) +
    geom_line(data = subset(global_combined_long, Inventory =='REAS'), size = 1) +
    scale_y_continuous(limits=c(0,max(global_combined_long$total_emissions)), labels = scales::comma) +
    labs(x= "" , y= paste(em ,'Emissions [Gg/yr]') )+
    xlim(plot_start,NA)+
    facet_wrap(~Region, scales = "free", nrow = 3) +
    ggtitle( paste(em)) +
    theme(panel.background=element_blank(),
          panel.grid.minor = element_line(colour="gray95"),
          panel.grid.major = element_line(colour="gray88")) +
    scale_color_manual(name = 'Inventory',
                       values = c('CEDS'= red,
                                  'EDGAR'= green,
                                  'GAINS'= black,
                                  'EDGAR_HTAP'=purple,
                                  'REAS'= marigold),
                       labels = c('CEDS'= CEDS_label,
                                  'EDGAR'= EDGAR_label,
                                  'GAINS'= GAINS_label,
                                  'EDGAR_HTAP'=EDGAR_HTAP_label,
                                  'REAS'= REAS_label)) +
    scale_linetype_manual(name= 'Inventory',
                          values = c('CEDS' = 1,
                                     'EDGAR'= 1,
                                     'GAINS'= 0,
                                     'EDGAR_HTAP'= 1,
                                     'REAS'= 1),
                          labels = c('CEDS'= CEDS_label,
                                     'EDGAR'= EDGAR_label,
                                     'GAINS'= GAINS_label,
                                     'EDGAR_HTAP'=EDGAR_HTAP_label,
                                     'REAS'= REAS_label)) +
    scale_shape_manual(name = "Inventory",
                       values = c("CEDS" = 32,
                                  "EDGAR" = 32,
                                  "GAINS" = 16,
                                  "EDGAR_HTAP" = 32,
                                  "REAS" = 32 ),
                       labels = c('CEDS'= CEDS_label,
                                  'EDGAR'= EDGAR_label,
                                  'GAINS'= GAINS_label,
                                  'EDGAR_HTAP'=EDGAR_HTAP_label,
                                  'REAS'= REAS_label))+
    theme(legend.position = "bottom")

plot(plot)
plot(plot_world)
dev.off()

} else {printLog( paste0( em, ' is not in invenotry to CEDS comparison. ' ) )}

logStop()

# END
