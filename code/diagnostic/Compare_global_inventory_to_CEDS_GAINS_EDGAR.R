# ------------------------------------------------------------------------------
# Program Name: Compare_inventory_totals_to_CEDS_GAINS_EDGAR.R
# Author: Andrea Mott, Harrison Suchyta
# Date Last Updated: January 4, 2023
# Program Purpose: Generate comparisons between inventory country totals and
#               CEDS, GAINS, EDGAR, EDGAR-HTAPv3, and REAS.
# Input Files: [em]_total_CEDS_emissions.csv
#              'E.', em,'_',inv,'_inventory_country_total'
#              Master_Country_List.csv, emf-30_ctry_map.csv,
#              emf-30_comparison_sector_map-comb_vs_process.csv
# Output Files: Compare_inventory_to_CEDS_[em].pdf
#               Compare_inventory_to_CEDS_[em].csv
#               CEDS_[em]_[inv]_compare_to_inv.csv

# TODO: 1. eventually add in BC, OC, and GHGs
# ---------------------------------------------------------------------------
setwd('C:/users/such559/Documents/CEDS-Dev')
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if( "input" %in% dir( ) ) "code/parameters/" else "../code/parameters/"

#read in mapping file to organize countries
mapping_world_region <- read.csv('input/mappings/Master_Country_List.csv') %>%
           subset(select = c(iso,Region))

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", 'common_data.R', 'IO_functions.R' ) # Additional function files may be required.
log_msg <- "Comparing invenotry emissions to CEDS, GAINS, EDGAR, and REAS..." # First message to be printed to the log
script_name <- "Compare_inventory_to_CEDS_GAINS_EDGAR.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "NMVOC"

#flag for whether or not to include HTAP in comparisons
add_HTAP <- TRUE

# ---------------------------------------------------------------------------
# 1. Run for relevant inventory emissions

em_list <- c(  'SO2','NOx', 'NH3', 'NMVOC', 'CO')

if (em %in% em_list) {

# ---------------------------------------------------------------------------
# 2. Inventory data

# Load in country inventory totals

    # Emissions present in inventory data
    # TODO: eventually read BC, OC, and GHGs
        em_list_us <- c( 'SO2', 'NOx', 'NH3', 'NMVOC', 'CO')
        em_list_jpn <- c( 'SO2', 'NOx', 'NH3', 'NMVOC', 'CO')
        em_list_can <- c( 'SO2', 'NOx', 'NH3','NMVOC', 'CO')
        em_list_kor <- c( 'CO', 'NH3', 'NMVOC', 'NOx', 'SO2')
        em_list_EMEP <- c( 'CO', 'NH3', 'NMVOC', 'NOx', 'SO2')
        em_list_twn <- c( 'CO', 'NMVOC', 'NOx', 'SO2')  # Taiwan doesn't have NH3
        em_list_MEIC <- c( 'CO', 'NH3', 'NMVOC', 'NOx', 'SO2')
        em_list_aus <- c( 'CO', 'NH3', 'NMVOC', 'NOx', 'SO2')
        em_list_old_chn <- c( 'CO', 'NH3', 'NMVOC', 'NOx', 'SO2')

        # set emission species and set flags
        # em <- em_list[h]

        us_em_flag <- em %in% em_list_us
        jpn_em_flag <- em %in% em_list_jpn
        can_em_flag <- em %in% em_list_can
        can_old_em_flag <- em %in% em_list_can
        kor_em_flag <- em %in% em_list_kor
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
        inv_emissions_can <- readData( domain = 'MED_OUT', file_name =  paste0( 'E.', em,'_',"CAN_2018",'_inventory_country_total'))
    } else {
        printLog( paste0( em, ' is not supported by CEDS, dummy data created. ' ) )
        inv_emissions_can <- data.frame( iso = "can")
    }

    if ( kor_em_flag == T ) {
        inv_emissions_kor <- readData( domain = 'MED_OUT', file_name =  paste0( 'E.', em,'_',"KOR2017",'_inventory_country_total'))
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
    inv_emissions_East_West_Europe <- inv_emissions_EMEP %>%
        left_join(master_country, by = c("iso")) %>%
        select(-iso) %>%
        dplyr::rename(iso = Region) %>%
        group_by(iso) %>%
        summarize_each(funs(sum)) %>%
        filter(iso %in% c("Eastern Europe","Western Europe"))

    # cut off years before 1990 since data becomes increasingly incomplete
    inv_emissions_East_West_Europe <- subset(inv_emissions_East_West_Europe, select = -c(2:11))

    # Bind inventory rows together.
    inv_total <- bind_rows(inv_emissions_jpn, inv_emissions_US, inv_emissions_can, inv_emissions_kor,
                           inv_emissions_East_West_Europe,inv_emissions_MEIC,inv_emissions_twn, inv_emissions_aus)
#,inv_emissions_aus
    # change to long
    inventory_long <- gather(inv_total,year, inventory, -c(iso))
    inventory_long$year <- as.numeric(sub('X','',inventory_long$year))

    if (add_HTAP == TRUE) {

        #establish more specific rows to remove for HTAP plots
        rows_to_remove <- which(inventory_long$iso == 'Eastern Europe' & inventory_long$year <= 1995,arr.ind=TRUE)
        inventory_long <- inventory_long[-rows_to_remove,]
        if(em == "NH3"){
            rows_to_remove <- which(inventory_long$iso == 'usa' & inventory_long$year <= 2002)
            inventory_long <- inventory_long[-rows_to_remove,] }
    }

# ---------------------------------------------------------------------------

    # Load in old inventory data

    if ( can_old_em_flag == T ) {
        inv_emissions_can_old <- readData( domain = 'MED_OUT', file_name =  paste0( 'E.', em,'_',"CAN_to2011",'_inventory_country_total'))
    } else {
        printLog( paste0( em, ' is not supported by CEDS, dummy data created. ' ) )
        inv_emissions_can <- data.frame( iso = "can_old")
    }

    if ( chn_old_em_flag == T ) {
        inv_emissions_chn_old <- readData( domain = 'MED_OUT', file_name =  paste0( 'E.', em,'_',"CHN",'_inventory_country_total'))
    } else {
        printLog( paste0( em, ' is not supported by CEDS, dummy data created. ' ) )
        inv_emissions_chn_old <- data.frame( iso = "chn_old")
    }

    old_inv_total <- bind_rows(inv_emissions_can_old, inv_emissions_chn_old)

    # change to long
    old_inventory_long <- gather(old_inv_total,year, Old_Inventory, -c(iso))
    old_inventory_long$year <- as.numeric(sub('X','',old_inventory_long$year))


# ---------------------------------------------------------------------------
# 3. CEDS
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
    ceds_East_West_Europe <- ceds_emissions %>%
        left_join(master_country, by = c("iso")) %>%
        select(-c(iso,fuel,units)) %>%
        dplyr::rename(iso = Region) %>%
        group_by(iso,sector) %>%
        summarize_each(funs(sum)) %>%
        filter(iso %in% c("Eastern Europe","Western Europe"))

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
    ceds_long <- gather(ceds,year, CEDS_v_2021_04_21, -c(iso))
    ceds_long$year <- as.numeric(sub('X','',ceds_long$year))

    #------------
    #world data
    ceds_world_regions <- ceds_emissions %>%
                          left_join(mapping_world_region, by = 'iso') %>%
                          select(-c(iso,fuel,units)) %>%
                          group_by(Region) %>%
                          dplyr::summarize_if(is.numeric,sum,na.rm = TRUE)%>%
                          select(Region,everything()) %>%
                          gather(year,CEDS_v_2021_04_21, -c(Region))

# ---------------------------------------------------------------------------
# 4. GAINS

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
         gains_long <- gather(gains,year, 'GAINS_(ECLIPSE_V6b_CLE)', -c(iso))
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
                         gather(year,'GAINS_(ECLIPSE_V6b_CLE)', -c(Region))
    #sub out the X's and filter to before 2020
    gains_global_data$year <- as.numeric(sub('X','',gains_global_data$year))
    gains_global_data <- filter(gains_global_data,gains_global_data$year <= 2020)

# ---------------------------------------------------------------------------
# 5. EDGAR 5.0

# 1.2 Read in and load files for EDGAR 5.0

    # Construct sheet name, for BC and OC the sheet name is slightly different
    edgar_sheet_name <- paste0( "v5.0_EM_" ,em, "_IPCC2006" )
    edgar_file_name <- paste0( "v50_" ,em, "_1970_2015" )
    if ( em %in% c( 'CO2' ) ) { edgar_file_name <- "v50_CO2_excl_short-cycle_org_C_1970_2018" }

    # Read in the edgar data
    edgar_emissions <- readData( domain = "EM_INV",
                                 domain_extension = "EDGAR_5.0/",
                                 file_name = edgar_file_name,
                                 extension = ".xls",
                                 sheet_selection = edgar_sheet_name,
                                 skip = 8 )
    edgar_emissions[edgar_emissions == "NULL"] <- 0

    # Edgar sectors to remove
    edgar_remove_sectors <- c( '1.A.3.a', #domestic aviation
                               '4.F', #agricultural waste
                               '1.C.1', #international aviation,
                               '1.C.2', #international shipping,
                               '7.A') # fossil fuel fires

    # Remove domestic navigation for usa and can
        # edgar_emissions <- edgar_emissions %>%
        #     filter(!ISO_A3 %in% c("USA","CAN") | !IPCC == "1.A.3.d")

# 4.3 process EDGAR data

    # Clean rows and columns to standard format.
    edgar <- edgar_emissions %>%
        select(-c(`IPCC-Annex`,`World Region`,Name,IPCC_description))

    # Drop sectors
    edgar <- edgar[ -which( edgar$IPCC %in% edgar_remove_sectors ), ]
    edgar <- edgar %>% dplyr::select(-IPCC)

    # Make numeric
    edgar[,2:ncol(edgar)] <- lapply(2:ncol(edgar),function(x) as.numeric(edgar[[x]]))

    # Sum across isos, filter by relevant isos
    edgar_sum <- edgar %>%
        group_by(ISO_A3) %>%
        summarize_each(funs(sum)) %>%
        dplyr::rename(iso = ISO_A3)
    edgar_sum$iso <- tolower( edgar_sum$iso )

    # Combine European isos into eastern and western Europe.
    edgar_all_isos <- edgar_sum %>%
        left_join(master_country, by = c("iso"))
    edgar_europe <- edgar_all_isos %>%
        filter(Region %in% c("Eastern Europe","Western Europe")) %>%
        select(-iso)%>%
        group_by(Region) %>%
        summarize_each(funs(sum)) %>%
        dplyr::rename(iso = Region)
    edgar <- edgar_all_isos %>%
        filter(Region != "Eastern Europe" | Region != "Western Europe") %>%
        bind_rows(edgar_europe)

    # Make long
    edgar_long <- gather(edgar, year, EDGAR_5.0, -c(iso))
    edgar_long$year <- as.numeric(edgar_long$year)
    edgar_long$EDGAR_5.0 <- as.numeric(edgar_long$EDGAR_5.0)

    #---
    #World data
    edgar_world <- edgar %>%
                   group_by(Region) %>%
                   dplyr::summarize_if(is.numeric,sum,na.rm = TRUE)%>%
                   select(Region,everything()) %>%
                   gather(year,EDGAR_5.0, -c(Region))
# ---------------------------------------------------------------------------
# 6. EDGAR 6.1

    # read in the files for EDGAR 6.1
    # Construct sheet name, for BC and OC the sheet name is slightly different
    edgar_v6_sheet_name <- paste0( em, "_IPCC2006" )
    edgar_v6_file_name <- paste0( em, "_1970_2018" )

    # Read in the edgar data
    edgar_v6_emissions <- readData( domain = "EM_INV",
                                 domain_extension = "EDGAR/",
                                 file_name = edgar_v6_file_name,
                                 extension = ".xlsx",
                                 sheet_selection = edgar_v6_sheet_name,
                                 skip = 9 ) %>%
                         replace(is.na(.),0)
    edgar_v6_emissions[edgar_v6_emissions == "NULL"] <- 0

    # Edgar sectors to remove
    edgar_v6_remove_sectors <- c( '1.A.3.a', #domestic aviation
                               '4.F', #agricultural waste
                               '1.C.1', #international aviation,
                               '1.C.2', #international navigation,
                               '7.A') # fossil fuel fires
    # 4.3 process EDGAR data

    # Clean rows and columns to standard format.
    edgar_v6 <- edgar_v6_emissions %>%
        select(-c(C_group_IM24_sh,Name,IPCC_annex,ipcc_code_2006_for_standard_report_name,Substance,fossil_bio)) %>%
        dplyr::rename(IPCC = ipcc_code_2006_for_standard_report)

    # Drop sectors
    edgar_v6 <- edgar_v6[ -which( edgar_v6$IPCC %in% edgar_v6_remove_sectors ), ]
    edgar_v6 <- edgar_v6 %>% dplyr::select(-IPCC)

    # Make numeric
    edgar_v6[,2:ncol(edgar_v6)] <- lapply(2:ncol(edgar_v6),function(x) as.numeric(edgar_v6[[x]]))

    #Remove "_Y" and add "X" to year numbers
    names( edgar_v6 ) <- sub("Y_", "", names( edgar_v6 ))
    names( edgar_v6 ) <- c( names( edgar_v6[ 1 ] ), paste0( "X", names( edgar_v6[ 2 : ncol(edgar_v6) ] ) ) )

    # Sum across isos, filter by relevant isos
    edgar_v6_sum <- edgar_v6 %>%
        group_by(Country_code_A3) %>%
        summarize_each(funs(sum)) %>%
        dplyr::rename(iso = Country_code_A3)
    edgar_v6_sum$iso <- tolower( edgar_v6_sum$iso )

    # Combine European isos into eastern and western Europe.
    edgar_v6_all_isos <- edgar_v6_sum %>%
        left_join(master_country, by = c("iso"))
    edgar_v6_europe <- edgar_v6_all_isos %>%
        filter(Region %in% c("Eastern Europe","Western Europe")) %>%
        select(-iso)%>%
        group_by(Region) %>%
        summarize_each(funs(sum)) %>%
        dplyr::rename(iso = Region)
    edgar_v6 <- edgar_v6_all_isos %>%
        filter(Region != "Eastern Europe" | Region != "Western Europe") %>%
        bind_rows(edgar_v6_europe)

    #get rid of X's before years
    names( edgar_v6 ) <- sub("X", "", names( edgar_v6 ))

    # Make long
    edgar_v6_long <- gather(edgar_v6, year, EDGAR_6.1, -c(iso))
    edgar_v6_long$year <- as.numeric(edgar_v6_long$year)
    edgar_v6_long$EDGAR_6.1 <- as.numeric(edgar_v6_long$EDGAR_6.1)

    #world data
    edgar_v6_world <- edgar_v6 %>%
        group_by(Region) %>%
        dplyr::summarize_if(is.numeric,sum,na.rm = TRUE)%>%
        select(Region,everything()) %>%
        gather(year,EDGAR_6.1, -c(Region))

    #make the year column a double
    edgar_v6_world$year <- as.double(edgar_v6_world$year)


# ---------------------------------------------------------------------------
# 7. REAS

    # Load in REAS_32 inventory data
    REAS_emissions <- readData( domain = 'MED_OUT', file_name =  paste0( 'E.',em,'_REAS32_inventory' ) )

    # Filter by inventory isos (kor, jpn, and twn)
    REAS <- REAS_emissions %>%
        filter(sector == "TOTAL") %>%
        filter(iso %in% c("kor","twn","jpn")) %>%
        select(-c(units,sector))

    # Make long
    REAS_long <- gather(REAS, year, REAS_32, -c(iso))
    REAS_long$year <- as.numeric(sub('X','',REAS_long$year))
    REAS_long$REAS_32 <- as.numeric(REAS_long$REAS_32)

    #-------
    #world data
    REAS_world <- REAS_emissions %>%
                  left_join(mapping_world_region, by = 'iso') %>%
                  filter(sector != 'TOTAL') %>%
                  select(-c(sector,units,iso)) %>%
                  group_by(Region) %>%
                  dplyr::summarize_if(is.numeric,sum,na.rm = TRUE)%>%
                  select(Region,everything()) %>%
                  gather(year,REAS_32, -c(Region)) %>%
                  filter(Region == 'Asia')

# ---------------------------------------------------------------------------
# 8. EDGAR-HTAPv3

if(add_HTAP == TRUE){
    #HTAP sectors to remove
    remove_HTAP_sectors <- c('HTAPv3_8.1_Agricultural_waste_burning',
                             'HTAPv3_1_International_Shipping',
                             'HTAPv3_2.1_Domestic_Aviation',
                             'HTAPv3_2.2_International_Aviation')

    #Read in the EDGAR_HTAP files
    emission_file <- paste0("EDGAR_HTAPv3_", em)
    inventory_data_file <- "../emissions-inventories"
    Edgar_HTAP <- readData(domain = "EM_INV",
                           domain_extension = "HTAPv3_TIMESERIES/",
                           file_name = emission_file,
                           extension = ".csv") %>%
                           filter(!Sector %in% remove_HTAP_sectors)
    Edgar_HTAP %>% subset(select = c(Data_provider,Substance,Year,Country,Sector,Annual)) %>%
                   group_by(Year,Country) %>%
                   summarize_if(is.numeric,sum,na.rm = TRUE) %>%
                   mutate(Country = str_replace(Country,'CHN_HKG_MAC','CHN'))%>%
                   mutate(Country = str_replace(Country,'USA_PRI_VIR','USA')) %>%
                   mutate(Country = str_replace(Country,'SRB_MNE_KOS','SRB')) -> EDGAR_HTAP_long
    #NOTE: Serbia, Montenegro, and Kosovo are lumped into one country, for the
    #       for the purposes of this script they are all changed to just Serbia
    #       since there is not


    #Get rid of unnecessary columns, rearrange and rename the remaining columns,
    #and convert country names to lowercase
   # EDGAR_HTAP_long <- subset(Edgar_HTAP, select = -c(Data_provider))
    EDGAR_HTAP_long %>% select(Country,Year,Annual)
    colnames(EDGAR_HTAP_long) <- c("year","iso","EDGAR_HTAPv3")
    EDGAR_HTAP_long$iso <- tolower(EDGAR_HTAP_long$iso)
    EDGAR_HTAP_long$EDGAR_HTAPv3 <- as.numeric(as.character(EDGAR_HTAP_long$EDGAR_HTAPv3)) / 1000

    Edgar_for_world_unprocessed <- EDGAR_HTAP_long

    EDGAR_HTAP_long <- EDGAR_HTAP_long %>%
        left_join(mapping_world_region, by = 'iso')

    EDGAR_HTAP_long$iso[EDGAR_HTAP_long$Region == 'Eastern Europe'] <- 'Eastern Europe'
    EDGAR_HTAP_long$iso[EDGAR_HTAP_long$Region == 'Western Europe'] <- 'Western Europe'

    #sum so there are no repeat isos (especially for Western and Eastern Europr)
    EDGAR_HTAP_long <- EDGAR_HTAP_long %>%
                       group_by(year,iso) %>%
                       dplyr::summarise(EDGAR_HTAPv3 = sum(EDGAR_HTAPv3))

    #------
    #world data
    Edgar_HTAP_long_for_world <- Edgar_for_world_unprocessed

    Edgar_HTAP_world <- Edgar_HTAP_long_for_world %>%
                        left_join(mapping_world_region, by = 'iso') %>%
                        subset(select =-c(iso)) %>%
                        select(Region,everything())
    Edgar_HTAP_world <- Edgar_HTAP_world[!is.na(Edgar_HTAP_world$Region),] %>%
                        group_by(Region,year) %>%
                        dplyr::summarize_if(is.numeric,sum,na.rm = TRUE)
}
# ---------------------------------------------------------------------------
# 9. Prepare country inventories for plotting

    #list of isos to include in the final combined_long
    iso_to_include <- c('jpn','can','kor','Western Europe','chn','twn','aus','Eastern Europe','usa')

    # Combine all inventories into one data frame
    combined <- left_join(ceds_long,inventory_long, by = c("iso","year")) %>%
        filter(iso %in% iso_to_include)
    combined_w_edgar <- left_join(combined, edgar_long, by = c("iso","year"))
    combined_w_GAINS <- left_join(combined_w_edgar, gains_long, by = c("iso","year"))
    combined_w_REAS <- left_join(combined_w_GAINS, REAS_long, by = c("iso","year"))
    combined_w_edgar_6 <- left_join(combined_w_REAS, edgar_v6_long, by = c("iso","year"))
    combined_w_oldinv <- left_join(combined_w_edgar_6, old_inventory_long, by = c("iso", "year"))

    if(add_HTAP == TRUE){
        combined_w_edgar_htap <- left_join(combined_w_oldinv, EDGAR_HTAP_long, by = c("iso","year"))
        # Make long
        combined_long <- gather(combined_w_edgar_htap, Inventory, value, -c(iso,year)) %>%
            filter(year >= 1990) %>%
            unique() #get rid of repeats

    }else{
        # Make long
        combined_long <- gather(combined_w_oldinv, Inventory, value, -c(iso,year)) %>%
            unique() #get rid of repeats
    }


    # Add em column and rename value column to total emissions
    combined_long$em <- em
    combined_long <- combined_long %>% dplyr::rename(total_emissions = value)

    # Diagnostics:
        # Print out plot csv
        combined_wide <- combined_long %>%
            group_by(iso, Inventory) %>%
            tidyr::spread(year, total_emissions)

        writeData( combined_wide , domain = "DIAG_OUT", domain_extension = "country-inventory-compare/",
                   paste0("Compare_inventory_to_CEDS_", em ) )

#-------------------------------------------------------------------------------
# 10. Prepare global inventories for plotting

        #Removes the X's in the years
        edgar_world$year <- as.numeric(sub('X','',edgar_world$year))
        REAS_world$year <- as.numeric(sub('X','',REAS_world$year))
        ceds_world_regions$year <- as.numeric(sub('X','',ceds_world_regions$year))
        ceds_world_regions <- filter(ceds_world_regions, year >= 1950) %>% filter(Region != 'Global')

        #combines all the data into one dtaframe
        global_combined <- left_join(ceds_world_regions, gains_global_data, by = c("Region","year"))
        global_combined_w_edgar <- left_join(global_combined, edgar_world, by = c("Region","year"))
        global_combined_w_edgar_6 <- left_join(global_combined_w_edgar, edgar_v6_world, by = c("Region","year"))
        global_combined_w_reas <- left_join(global_combined_w_edgar_6, REAS_world, by = c("Region","year"))

        #add HTAP data if present
        if(add_HTAP == TRUE){
            global_combined_w_edgar_htap <- left_join(global_combined_w_reas, Edgar_HTAP_world, by = c("Region","year"))
            #make long
            global_combined_long <- global_combined_w_edgar_htap %>%
                gather(Inventory,value,-c(Region,year)) %>%
                dplyr::rename(total_emissions = value) %>%
                filter(year >= 1990)

        } else{
            #make long
            global_combined_long <- global_combined_w_reas %>% gather(Inventory,value,-c(Region,year)) %>% dplyr::rename(total_emissions = value)
        }

# -------------------------------------------------------------------
# 11. plotting

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
pdf(paste0('../diagnostic-output/country-inventory-compare/Compare_inventory_to_CEDS_',em,'.pdf'),height = 11,width = 9.5,paper='special', onefile=TRUE)


#original script
plot <- ggplot(combined_long, aes(x = year, y = total_emissions, color = Inventory, linetype = Inventory, shape = Inventory)) +
    geom_point(data = subset(combined_long, Inventory == "Country_inventory"), size = 1) +
    geom_point(data = subset(combined_long, Inventory == "Old_Inventory"), size = 1) +
    geom_line(data = subset(combined_long, Inventory == "CEDS_v_2021_04_21"), size = 1) +
    geom_line(data = subset(combined_long, Inventory == "EDGAR_5.0"), size = 1) +
    geom_line(data = subset(combined_long, Inventory == "EDGAR_HTAPv3"), size = 1) +
    geom_line(data = subset(combined_long, Inventory == "EDGAR_6.1"), size = 1) +
    geom_point(data = subset(combined_long, Inventory =='GAINS_(ECLIPSE_V6b_CLE)'), size = 1) +
    geom_line(data = subset(combined_long, Inventory =='REAS_32'), size = 1) +
    scale_y_continuous(limits=c(0,max(combined_long$total_emissions)), labels = scales::comma) +
    labs(x= "" , y= paste(em ,'Emissions [Gg/yr]') )+
    facet_wrap(~iso, scales = "free", nrow = 3) +
    ggtitle( paste(em)) +
    theme(panel.background=element_blank(),
          panel.grid.minor = element_line(colour="gray95"),
          panel.grid.major = element_line(colour="gray88")) +
    theme(text = element_text(size = 13)) +
    theme(strip.text.x = element_text(size = 15)) +
    geom_point(size=1.5) +

    scale_color_manual(name = 'Inventory',
                       values = c('Country_inventory'= "#1E88E5",
                                  'Old_Inventory'= "#1E88E5",
                                  'CEDS_v_2021_04_21'= "#D81B60",
                                  'EDGAR_5.0'= "#BF7618",
                                  'EDGAR_HTAPv3'='#000000',
                                  'EDGAR_6.1' = '#FFC107',
                                  'GAINS_(ECLIPSE_V6b_CLE)'= "#000000",
                                  'REAS_32'= "#7A2884")) +
    scale_linetype_manual(name= 'Inventory',
                          values = c('Country_inventory' = 0,
                                     'Old_Inventory' = 0,
                                     'CEDS_v_2021_04_21' = 1,
                                     'EDGAR_5.0'= 2,
                                     'EDGAR_HTAPv3'= 1,
                                     'EDGAR_6.1' = 1,
                                     'GAINS_(ECLIPSE_V6b_CLE)'= 0,
                                     'REAS_32'= 1)) +
    scale_shape_manual(name = "Inventory",
                       values = c("Country_inventory" = 16,
                                  "Old_Inventory" = 1,
                                  "CEDS_v_2021_04_21" = 32,
                                  "EDGAR_5.0" = 32,
                                  "EDGAR_HTAPv3" = 32,
                                  'EDGAR_6.1' = 32,
                                  "GAINS_(ECLIPSE_V6b_CLE)" = 3,
                                  "REAS_32" = 32 ))+
    theme(legend.position = "bottom") +
    theme(plot.margin = margin(t = 10,
                               b = 10,
                               l = 15,
                               r = 15))

plot_world <- ggplot(global_combined_long, aes(x = year, y = total_emissions, color = Inventory, linetype = Inventory, shape = Inventory)) +
    geom_line(data = subset(global_combined_long, Inventory == "CEDS_v_2021_04_21"), size = 1) +
    geom_line(data = subset(global_combined_long, Inventory == "EDGAR_5.0"), size = 1) +
    geom_point(data = subset(global_combined_long, Inventory =='GAINS_(ECLIPSE_V6b_CLE)'), size = 1.5) +
    geom_line(data = subset(global_combined_long, Inventory == 'EDGAR_HTAPv3'), size = 1) +
    geom_line(data = subset(global_combined_long, Inventory == "EDGAR_6.1"), size = 1) +
    geom_line(data = subset(global_combined_long, Inventory =='REAS_32'), size = 1) +
    scale_y_continuous(limits=c(0,max(global_combined_long$total_emissions)), labels = scales::comma) +
    labs(x= "" , y= paste(em ,'Emissions [Gg/yr]') )+
    facet_wrap(~Region, scales = "free", nrow = 3) +
    ggtitle( paste(em)) +
    theme(panel.background=element_blank(),
          panel.grid.minor = element_line(colour="gray95"),
          panel.grid.major = element_line(colour="gray88")) +
    theme(text = element_text(size = 13)) +
    theme(strip.text.x = element_text(size = 15)) +
    geom_point(size=1.5) +

    scale_color_manual(name = 'Inventory',
                       values = c('CEDS_v_2021_04_21'= "#D81B60",
                                  'EDGAR_5.0'= "#BF7618",
                                  'GAINS_(ECLIPSE_V6b_CLE)'= "#00dee6",
                                  'EDGAR_HTAPv3'='#000000',
                                  'EDGAR_6.1' = '#FFC107',
                                  'REAS_32'= "#7A2884")) +
    scale_linetype_manual(name= 'Inventory',
                          values = c('CEDS_v_2021_04_21' = 1,
                                     'EDGAR_5.0'= 2,
                                     'GAINS_(ECLIPSE_V6b_CLE)'= 0,
                                     'EDGAR_HTAPv3'= 1,
                                     'EDGAR_6.1' = 1,
                                     'REAS_32'= 1)) +
    scale_shape_manual(name = "Inventory",
                       values = c("CEDS_v_2021_04_21" = 32,
                                  "EDGAR_5.0" = 32,
                                  "GAINS_(ECLIPSE_V6b_CLE)" = 16,
                                  "EDGAR_HTAPv3" = 32,
                                  'EDGAR_6.1' = 32,
                                  "REAS_32" = 32 ))+
    theme(legend.position = "bottom") +
    theme(plot.margin = margin(t = 10,
                               b = 10,
                               l = 15,
                               r = 15))

plot(plot)
plot(plot_world)
dev.off()

} else {printLog( paste0( em, ' is not in invenotry to CEDS comparison. ' ) )}

logStop()

# END
