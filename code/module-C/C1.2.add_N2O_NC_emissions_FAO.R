# ------------------------------------------------------------------------------
# Program Name: C1.2.add_N2O_NC_emissions_FAO.R
# Authors: Patrick O'Rourke, Noah Prime
# Date Last Modified: 10 April 2023
# Program Purpose: Use the package FAOSTAT to retrieve N2O emissions
#             data for agriculture.
# Input Files: Master_Country_List.csv, FAO_N2O_API_manure_management.csv,
#              FAO_N2O_API_manure_pasture, FAO_N2O_API_ synthetic_fertilizer,
#              FAO_N2O_API_manure_appplied, FAO_N2O_API_organic_soil_cultivation
# Output Files: C.N2O_NC_emissions_agriculture.csv
# To Do:
# - addToEmissionsDb_overwrite why isn't this working?
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
    script_name <- "C.1.2.add_N2O_NC_emissions_FAO.R"
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# -----------------------------------------------------------------------------
# 1. Load Data
#    A.) Load the Master Country List for mapping
     FAO_country_map <- readData( "MAPPINGS", "FAO_country_mapping", meta = F )

#    B.) Load FAO manure management data file
     FAO_mm <-  readData('EM_INV' , domain_extension = 'FAO/', 'FAOSTAT_N2O_manure_management_1961-2022', meta = FALSE)

#    C.) Load FAO manure left on pasture data file
     FAO_mp <-  readData('EM_INV' , domain_extension = 'FAO/', 'FAOSTAT_N2O_manure_pasture_1961-2022', meta = FALSE)

#    D.) Load FAO Synthetic Fertilizers data file
     FAO_sf <-  readData('EM_INV' , domain_extension = 'FAO/', 'FAOSTAT_N2O_synthetic_fertilizer_1961-2022', meta = FALSE)

#    E.) Load FAO Manure applied to Soils data file
     FAO_ma <-  readData('EM_INV' , domain_extension = 'FAO/', 'FAOSTAT_N2O_manure_applied_1961-2022', meta = FALSE)

#    F.) Load FAO Cultivation of Organic Soils data file
     FAO_cos <-  readData('EM_INV' , domain_extension = 'FAO/', 'FAOSTAT_N2O_cropland_organic_drained_soils_1990-2023', meta = FALSE)

#    G.) Load UN Population data file
     un_pop <- readData( "MED_OUT" , 'A.UN_pop_master', meta = FALSE)
# -----------------------------------------------------------------------------

    FAO_start_year <- FAO_mm %>% pull(Year) %>% min()
    FAO_end_year <- bind_rows( FAO_mm, FAO_mp, FAO_sf, FAO_cos) %>% pull(Year) %>% max()
    X_FAO_end_year <- paste0('X', FAO_end_year)
    X_FAO_years <- paste0('X', FAO_start_year:FAO_end_year)

# -----------------------------------------------------------------------------
# 2. Process FAO N2O data

#   A.) Process FAO manure management data (CEDS sector 3B_Manure-management)

#      Sum FAO_mm and FAO_mp
       FAO_manure_summed <- FAO_mm
       FAO_add <- ddply(rbind(FAO_mm, FAO_mp), .(Area, Year), summarize, summed_value = sum(Value))
       FAO_manure_summed <- dplyr::left_join(FAO_manure_summed, FAO_add, by = c("Area" = "Area", "Year" = "Year"))

# FAO Manure

       FAO_manure <- FAO_manure_summed %>%
#      Map over CEDS iso codes
            left_join(FAO_country_map, by = c('Area.Code..M49.' = 'FAO_Country_Code')) %>%
#      Rename "Domain" variable "sector"
            dplyr::rename(sector = Domain) %>%
#      Map sectors to CEDS sectors
            mutate(sector = "3B_Manure-management") %>%
#      Convert years into CEDS format
            mutate(Year = as.character(Year)) %>%
            mutate(Year = paste0('X','',Year)) %>%
#      Reorder columns of interest
            select(iso, sector, Year, summed_value) %>%
#      Delete countries that did not map over
            dplyr::filter(!is.na(iso)) %>%
#      Spread the data to wide format
            tidyr::spread(Year, summed_value)


#   B.) Process FAO soil data (CEDS sector 3D_Soil-emissions)

#      Sum FAO_cos and FAO_ma and FAO_sf
       FAO_soil_summed <- rbind(FAO_ma, FAO_cos, FAO_sf)
       FAO_soil_summed <- FAO_soil_summed[!duplicated(FAO_soil_summed[c("Year","Area")]),]
       FAO_soil_add <- ddply(rbind(FAO_ma, FAO_cos, FAO_sf), .(Area, Year), summarize, summed_value = sum(Value))
       FAO_soil_summed <- dplyr::left_join(FAO_soil_summed, FAO_soil_add, by = c("Area" = "Area", "Year" = "Year"))

#      Map over CEDS iso codes
       FAO_soil <- FAO_soil_summed %>%
#      Map over CEDS iso codes
           left_join(FAO_country_map, by = c('Area.Code..M49.' = 'FAO_Country_Code')) %>%
#      Rename "Domain" variable "sector"
           dplyr::rename(sector = Domain) %>%
#      Map sectors to CEDS sectors
           mutate(sector = "3D_Soil-emissions") %>%
#      Convert years into CEDS format
           mutate(Year = as.character(Year)) %>%
           mutate(Year = paste0('X','',Year)) %>%
#      Reorder columns of interest
           select(iso, sector, Year, summed_value) %>%
#      Delete countries that did not map over
           dplyr::filter(!is.na(iso)) %>%
#      Spread the data to wide format
           tidyr::spread(Year, summed_value)

#   C.) Combine the soil and manure data into one data frame

#      Row bind the data frame

      if(FAO_end_year != 2023){ stop('Need to check this code - hard coded for 2023 - check new data.')}
# A lot of the Soil emissions data for 2023 is discontinuous - zero out for now
       FAO <- bind_rows(FAO_manure, FAO_soil) %>%
           arrange(iso, sector) %>%
#       some zero last years in the data - make NA, to carry fowrad the last value below
           mutate( X2023 = NA)

#      Carry Forward value to last year
       FAO_extended <- FAO
       FAO_extended[ X_FAO_years ] <- t(na.locf(t(FAO_extended[ X_FAO_years ])))

# -----------------------------------------------------------------------------
# 3. Country Splitting


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
                                   dis_start_year = 1961)

    FAO_yug[FAO_yug == 0] <- NA

# Carry values forward

    #      Carry Forward value to last year
    FAO_yug[ X_FAO_years ] <- t(na.locf(t(FAO_yug[ X_FAO_years ])))

# -----------------------------------------------------------------------------
# 4. Final Processing

  FAO_out <- FAO_yug

#   Add units and fuel variables
    FAO_out <- FAO_out %>%
        dplyr::mutate(units = 'kt') %>%
        dplyr::mutate(fuel = 'process')

#   Reorder columns of interest
    FAO_out <- FAO_out[c('iso','sector','fuel','units',X_FAO_years)]
    FAO_out[is.na(FAO_out)] <- 0

# --------------------------------------------------------------------------------
# 5. Output
  addToEmissionsDb_overwrite(FAO_out,em='N2O',type='NC')

  writeData( FAO_out, domain = "MED_OUT", fn = "C.N2O_NC_emissions_agriculture")

  logStop()

# END
