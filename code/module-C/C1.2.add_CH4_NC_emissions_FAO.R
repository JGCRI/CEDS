# ------------------------------------------------------------------------------
# Program Name: C1.2.add_CH4_NC_emissions_FAO.R
# Authors: Rachel Hoesly, Harrison Suchyta
# Date Last Modified: 24 February 2023
# Program Purpose: Use the package FAOSTAT to retrieve methane emissions
#             data for agriculture.
# Input Files: Master_Coutnry_list.csv, FAO_methane_API.csv
# Output Files: C.CH4_NC_emissions_agriculture.csv
# TODO:
# Notes:
# -----------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Universal header file - provides logging, file support, etc.
    headers <- c( "common_data.R","data_functions.R", "analysis_functions.R",
                  "process_db_functions.R") # Additional function files required.
    log_msg <- paste0( "Sulfite and Sulfate data processing from FAO pulp and paper",
                       "processing data" ) # First message to be printed to the log
    script_name <- "C.1.2.add_CH4_NC_emissions_FAO.R"
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# -----------------------------------------------------------------------------
# 1. Load Data

    FAO_country_map <- readData( "MAPPINGS", "FAO_country_mapping", meta = F )
    FAO_in <-  readData('EM_INV' ,'FAO/FAOSTAT_methane_1961-2022')
    un_pop <- readData( "MED_OUT" , 'A.UN_pop_master' )

# -----------------------------------------------------------------------------

    FAO_start_year <- FAO_in %>% pull(Year) %>% min()
    FAO_end_year <- FAO_in %>% pull(Year) %>% max()
    X_FAO_end_year <- paste0('X', FAO_end_year)
    X_FAO_years <- paste0('X', FAO_start_year:FAO_end_year)

# -----------------------------------------------------------------------------
# 3. Process FAO methane data

FAO <- FAO_in %>%
      left_join(FAO_country_map, by = c('Area.Code..M49.' = 'FAO_Country_Code')) %>%
       select(iso,Year,Item,Value) %>%
       dplyr::rename(sector = Item) %>%
       filter(!is.na(iso), Year %in% extended_years) %>%
       dplyr::mutate(Year = paste0('X',Year)) %>% unique() %>%
       mutate(as.character(Value)) %>%
       filter(Value != 0) %>%
       # full_join( expand.grid(iso = FAO_country_map$iso %>% unique,
       #                        sector = c('Manure Management', 'Rice Cultivation', 'Enteric Fermentation'))) %>%
       cast(iso+sector~Year, value = 'Value', sum)

FAO$sector <- mapvalues(FAO$sector,
          c('Manure Management', 'Rice Cultivation', 'Enteric Fermentation'),
          c('3B_Manure-management', '3D_Rice-Cultivation', '3E_Enteric-fermentation'))

#      Carry Forward value to last year
FAO_extended <- FAO
FAO_extended[ X_FAO_years ] <- t(na.locf(t(FAO_extended[ X_FAO_years ])))

# -----------------------------------------------------------------------------
# 4. Country Splitting

#   Reformat UN population data
un_pop$X_year <- paste0( "X" , un_pop$year)
un_pop$pop <- as.numeric(un_pop$pop)
population <- cast( un_pop[which ( un_pop$year %in% historical_pre_extension_year:end_year ) , ] ,
                    iso ~ X_year, value = 'pop')

#   Disaggregate csk data
FAO_csk <- disaggregate_country(original_data = FAO_extended,
                                id_cols = c('iso','sector'),
                                trend_data = population,
                                trend_match_cols = 'iso',
                                combined_iso = 'csk',
                                disaggregate_iso = c("cze","svk"),
                                dis_end_year= 1992,
                                dis_start_year = 1961)

#   Disaggregate USSR data
FAO_ussr <- disaggregate_country(original_data = FAO_csk,
                                 id_cols = c('iso','sector'),
                                 trend_data = population,
                                 trend_match_cols = 'iso',
                                 combined_iso = 'ussr',
                                 disaggregate_iso = c("arm","aze","blr","est","geo","kaz",
                                                      "kgz","ltu","lva","mda","rus","tjk","tkm","ukr","uzb"),
                                 dis_end_year= 1991,
                                 dis_start_year = 1961)
#   Disaggregate blx data
FAO_blx <- disaggregate_country(original_data = FAO_ussr,
                                id_cols = c('iso','sector'),
                                trend_data = population,
                                trend_match_cols = 'iso',
                                combined_iso = 'blx',
                                disaggregate_iso = c("bel","lux"),
                                dis_end_year= 1999,
                                dis_start_year = 1961)


#   Disaggregate yug data
FAO_yug <- disaggregate_country(original_data = FAO_blx,
                                id_cols = c('iso','sector'),
                                trend_data = population,
                                trend_match_cols = 'iso',
                                combined_iso = "yug",
                                disaggregate_iso = c( "svn", "mkd","hrv","bih"),
                                dis_end_year= 1991,
                                dis_start_year = 1961, allow_dropped_data = T)

# -----------------------------------------------------------------------------
# 5. Final Processing

FAO_out <- FAO_yug

#   Add units and fuel variables
FAO_out <- FAO_out %>%
    dplyr::mutate(units = 'kt') %>%
    dplyr::mutate(fuel = 'process')

#   Reorder columns of interest
FAO_out <- FAO_out[c('iso','sector','fuel','units',X_FAO_years)]
# FAO_out[is.na(FAO_out)] <- 0

# --------------------------------------------------------------------------------
# 6. Output
  addToEmissionsDb_overwrite(FAO_out,em='CH4',type='NC')

  writeData( FAO_out, domain = "MED_OUT", fn = "C.CH4_NC_emissions_agriculture")

  logStop()
# END
