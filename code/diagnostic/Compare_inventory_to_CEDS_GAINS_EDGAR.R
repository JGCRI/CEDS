# ------------------------------------------------------------------------------
# Program Name: Compare_inventory_totals_to_CEDS_GAINS_EDGAR.R
# Author: Andrea Mott
# Date Last Updated: April 26, 2021
# Program Purpose: Generate comparisons between inventory country totals and
#               CEDS, GAINS, EDGAR, and REAS.
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
if ( is.na( em ) ) em <- "NOx"

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
                           inv_emissions_East_West_Europe,inv_emissions_MEIC,inv_emissions_twn,inv_emissions_aus)

    # change to long
    inventory_long <- gather(inv_total,year, inventory, -c(iso))

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
    ceds_long <- gather(ceds,year, CEDS, -c(iso))

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
         gains_long <- gather(gains,year, GAINS_(ECLIPSE_V6b_CLE), -c(iso))
         gains_long$year <- as.numeric(sub('X','',gains_long$year))
         gains_long$iso <- as.character(gains_long$iso)

# ---------------------------------------------------------------------------
# 5. EDGAR

# 1.2 Read in and load files for EDGAR

    # Construct sheet name, for BC and OC the sheet name is slightly different
    edgar_sheet_name <- paste0( "v5.0_EM_" ,em, "_IPCC2006" )
    edgar_file_name <- paste0( "v50_" ,em, "_1970_2015" )
    if ( em %in% c( 'CO2' ) ) { edgar_file_name <- "v50_CO2_excl_short-cycle_org_C_1970_2018" }

    # Read in the edgar data
    edgar_emissions <- readData( domain = "EM_INV",
                                 domain_extension = "EDGAR/",
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
                               '6.C', #residential waste incineration
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

# ---------------------------------------------------------------------------
# 6. REAS

    # Load in REAS_32 inventory data
    REAS_emissions <- readData( domain = 'MED_OUT', file_name =  paste0( 'E.',em,'_REAS32_inventory' ) )

    # Filter by inventory isos (kor and twn)
    REAS <- REAS_emissions %>%
        filter(sector == "TOTAL") %>%
        filter(iso %in% c("kor","twn")) %>%
        select(-c(units,sector))

    # Make long
    REAS_long <- gather(REAS, year, REAS_32, -c(iso))
    REAS_long$year <- as.numeric(sub('X','',REAS_long$year))
    REAS_long$REAS_32 <- as.numeric(REAS_long$REAS_32)

# ---------------------------------------------------------------------------
# 7. Prepare inventories for plotting

    # Combine all inventories into one data frame
    combined <- left_join(inventory_long, ceds_long, by = c("iso","year"))
    combined$year <- as.numeric(sub('X','',combined$year))
    combined_w_edgar <- left_join(combined, edgar_long, by = c("iso","year"))
    combined_w_GAINS <- left_join(combined_w_edgar, gains_long, by = c("iso","year"))
    combined_w_REAS <- left_join(combined_w_GAINS, REAS_long, by = c("iso","year"))
    combined_w_oldinv <- left_join(combined_w_REAS, old_inventory_long, by = c("iso", "year"))

    # Make long
    combined_long <- gather(combined_w_oldinv, Inventory, value, -c(iso,year))

    # Add em column and rename value column to total emissions
    combined_long$em <- em
    combined_long <- combined_long %>% dplyr::rename(total_emissions = value)

    # Diagnostics:
        # Print out plot csv
        combined_wide <- combined_long %>%
            group_by(iso, Inventory) %>%
            spread(year, total_emissions)

        writeData( combined_wide , domain = "DIAG_OUT", domain_extension = "country-inventory-compare/",
                   paste0("Compare_inventory_to_CEDS_", em ) )

# -------------------------------------------------------------------
# 8. plotting

# rename isos for cleaner graphs
    combined_long$iso <- gsub("aus", "Australia", combined_long$iso)
    combined_long$iso <- gsub("can", "Canada", combined_long$iso)
    combined_long$iso <- gsub("jpn", "Japan", combined_long$iso)
    combined_long$iso <- gsub("chn", "China", combined_long$iso)
    combined_long$iso <- gsub("kor", "South Korea", combined_long$iso)
    combined_long$iso <- gsub("twn", "Taiwan", combined_long$iso)
    combined_long$iso <- gsub("usa", "USA", combined_long$iso)

    combined_long$Inventory <- gsub("inventory", "Country_inventory", combined_long$Inventory)

# Plot
pdf(paste0('../diagnostic-output/country-inventory-compare/Compare_inventory_to_CEDS_',em,'.pdf'),width=9.5,height=9.5,paper='special', onefile=F)

#original script
plot <- ggplot(combined_long, aes(x = year, y = total_emissions, color = Inventory, linetype = Inventory, shape = Inventory)) +
    geom_point(data = subset(combined_long, Inventory == "Country_inventory"), size = 1) +
    geom_point(data = subset(combined_long, Inventory == "Old_Inventory"), size = 1) +
    geom_line(data = subset(combined_long, Inventory == "CEDS"), size = 1) +
    geom_line(data = subset(combined_long, Inventory == "EDGAR_5.0"), size = 1) +
    geom_point(data = subset(combined_long, Inventory =='GAINS_(ECLIPSE_V6b_CLE)'), size = 1) +
    geom_line(data = subset(combined_long, Inventory =='REAS_32'), size = 1) +
    scale_y_continuous(limits=c(0,max(combined_long$total_emissions)), labels = scales::comma) +
    labs(x= "" , y= paste(em ,'Emissions [Gg/yr]') )+
    facet_wrap(~iso, scales = "free") +
    ggtitle( paste(em)) +
    theme(panel.background=element_blank(),
          panel.grid.minor = element_line(colour="gray95"),
          panel.grid.major = element_line(colour="gray88")) +

    scale_color_manual(name = 'Inventory',
                       values = c('Country_inventory'= "#0066ff",
                                  'Old_Inventory'= "#0066ff",
                                  'CEDS'= "#f75f55",
                                  'EDGAR_5.0'= "#73e600",
                                  'GAINS_(ECLIPSE_V6b_CLE)'= "#00dee6",
                                  'REAS_32'= "#C77CFF")) +
    scale_linetype_manual(name= 'Inventory',
                          values = c('Country_inventory' = 0,
                                     'Old_Inventory' = 0,
                                     'CEDS' = 1,
                                     'EDGAR_5.0'= 1,
                                     'GAINS_(ECLIPSE_V6b_CLE)'= 0,
                                     'REAS_32'= 1)) +
    scale_shape_manual(name = "Inventory",
                    values = c("CEDS" = 32,
                               "Country_inventory" = 16,
                               "EDGAR_5.0" = 32,
                               "GAINS_(ECLIPSE_V6b_CLE)" = 16,
                               "Old_Inventory" = 1,
                               "REAS_32" = 32 ))





print(plot)
dev.off()

} else {printLog( paste0( em, ' is not in invenotry to CEDS comparison. ' ) )}

logStop()

# END
