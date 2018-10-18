# ------------------------------------------------------------------------------
# Program Name: C1.2.add_N2O_NC_emissions_FAO.R
# Authors: Patrick O'Rourke
# Date Last Modified: 12 October 2018
# Program Purpose: Use the package FAOSTAT to retrieve N2O emissions
#             data for agriculture.
# Input Files: Master_Country_List.csv, FAO_N2O_API_manure_management.csv,
#              FAO_N2O_API_manure_pasture, FAO_N2O_API_ synthetic_fertilizer,
#              FAO_N2O_API_manure_appplied, FAO_N2O_API_organic_soil_cultivation
# Output Files: C.N2O_NC_emissions_agriculture.csv
# To Do:
# - addToEmissionsDb_overwrite why isn't this working?
# - final check of output being correct
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
     MCL <- readData( "MAPPINGS", "Master_Country_List", meta = F )

#    B.) Load FAO manure management data file
     FAO_mm <-  readData('EM_INV' , domain_extension = 'FAO_N2O/', 'FAO_N2O_API_manure_management')

#    C.) Load FAO manure left on pasture data file
     FAO_mp <-  readData('EM_INV' , domain_extension = 'FAO_N2O/', 'FAO_N2O_API_manure_pasture')

#    D.) Load FAO Synthetic Fertilizers data file
     FAO_sf <-  readData('EM_INV' , domain_extension = 'FAO_N2O/', 'FAO_N2O_API_ synthetic_fertilizer')

#    E.) Load FAO Manure applied to Soils data file
     FAO_ma <-  readData('EM_INV' , domain_extension = 'FAO_N2O/', 'FAO_N2O_API_manure_appplied')

#    F.) Load FAO Cultivation of Organic Soils data file
     FAO_cos <-  readData('EM_INV' , domain_extension = 'FAO_N2O/', 'FAO_N2O_API_organic_soil_cultivation')

#    G.) Load UN Population data file
     un_pop <- readData( "MED_OUT" , 'A.UN_pop_master' )
# -----------------------------------------------------------------------------
# 2. Process FAO N2O data

#   A.) Process FAO manure management data (CEDS sector 3B_Manure-management)

#      Sum FAO_mm and FAO_mp
       FAO_manure_summed <- FAO_mm
       FAO_add <- ddply(rbind(FAO_mm, FAO_mp), .(Area, Year), summarize, summed_value = sum(Value))
       FAO_manure_summed <- dplyr::left_join(FAO_manure_summed, FAO_add, by = c("Area" = "Area", "Year" = "Year"))


#      Map over CEDS iso codes
       FAO_manure <- FAO_manure_summed %>%
            left_join(MCL[c('iso','FAO_Country_Code')], by = c('Area.Code' = 'FAO_Country_Code')) %>%

#      Remove columns that are not needed
            select(-Domain.Code, -Area.Code, -Area, -Element.Code, -Element, -Item.Code, -Item, -Year.Code,
                   -Unit, -Value, -Flag, -Flag.Description) %>%

#      Rename "Domain" variable "sector"
            dplyr::rename(sector = Domain) %>%

#      Map sectors to CEDS sectors
            mutate(sector = "3B_Manure-management") %>%

#      Convert years into CEDS format
            mutate(Year = as.character(Year)) %>%
            mutate(Year = paste0('X','',Year))

#      Reorder columns of interest
       FAO_manure <- FAO_manure[, c("iso", "sector", "Year", "summed_value")]

#      Delete duplicated rows before spreading data to wide format (duplicated when iso is present
#      more than once in MCL)
       FAO_manure <- dplyr::distinct(FAO_manure)

#      Delete countries that did not map over
       FAO_manure <- dplyr::filter(FAO_manure, !is.na(iso))

#      Spread the data to wide format
       FAO_manure <- tidyr::spread(FAO_manure, Year, summed_value)

#      Remove Years not needed (2015, 2016)
       FAO_manure <- dplyr::select(FAO_manure, -X2015, -X2016)

#   B.) Process FAO soil data (CEDS sector 3D_Soil-emissions)

#      Sum FAO_cos and FAO_ma and FAO_sf
       FAO_soil_summed <- rbind(FAO_ma, FAO_cos, FAO_sf)
       FAO_soil_summed <- FAO_soil_summed[!duplicated(FAO_soil_summed[c("Year","Area")]),]
       FAO_soil_add <- ddply(rbind(FAO_ma, FAO_cos, FAO_sf), .(Area, Year), summarize, summed_value = sum(Value))
       FAO_soil_summed <- dplyr::left_join(FAO_soil_summed, FAO_soil_add, by = c("Area" = "Area", "Year" = "Year"))

#      Map over CEDS iso codes
       FAO_soil <- FAO_soil_summed %>%
           left_join(MCL[c('iso','FAO_Country_Code')], by = c('Area.Code' = 'FAO_Country_Code')) %>%

#      Remove columns that are not needed
           select(-Domain.Code, -Area.Code, -Area, -Element.Code, -Element, -Item.Code, -Item, -Year.Code,
                  -Unit, -Value, -Flag, -Flag.Description) %>%

#      Rename "Domain" variable "sector"
           dplyr::rename(sector = Domain) %>%

#      Map sectors to CEDS sectors
           mutate(sector = "3D_Soil-emissions") %>%

#      Convert years into CEDS format
           mutate(Year = as.character(Year)) %>%
           mutate(Year = paste0('X','',Year))

#      Reorder columns of interest
       FAO_soil <- FAO_soil[, c("iso", "sector", "Year", "summed_value")]

#      Delete duplicated rows before spreading data to wide format (duplicated when iso is present
#      more than once in MCL, or if not mapped to CEDS iso and has the same value as another country
#      that did not get mapped for a given year)
       FAO_soil <- dplyr::distinct(FAO_soil)

#      Delete countries that did not map over
       FAO_soil <- dplyr::filter(FAO_soil, !is.na(iso))

#      Spread the data to wide format
       FAO_soil <- tidyr::spread(FAO_soil, Year, summed_value)

#      Remove Years not needed (2015, 2016)
       FAO_soil <- dplyr::select(FAO_soil, -X2015, -X2016)

#   C.) Combine the oil and manure data into one data frame

#      Row bind the data frame
       FAO <- rbind(FAO_manure, FAO_soil)

#      Sort the data frame by iso, sector
       FAO <- dplyr::arrange(FAO, iso, sector)

# -----------------------------------------------------------------------------
# 3. Country Splitting

#   List countries that will be split and how they will be split
    list(czech.slovkia = c(1961, 1993, "csk","cze","svk"),
       bel.lux = c(1961, 2000, "blx","bel","lux"),
       yugo = c(1961, 1992, "yug", "scg", "svn", "mkd","hrv","bih"),
       ser.mont = c(1961, 2006, "scg","srb","mne"),
       soviet = c(1961, 1992, "USSR","arm","aze","blr","est","geo","kaz",
                  "kgz","ltu","lva","mda","rus","tjk","tkm","ukr","uzb"))


#   Reformat UN population data
    un_pop$X_year <- paste0( "X" , un_pop$year)
    un_pop$pop <- as.numeric(un_pop$pop)
    population <- cast( un_pop[which ( un_pop$year %in% historical_pre_extension_year:end_year ) , ] ,
                      iso ~ X_year, value = 'pop')

#   Disaggregate csk data
    FAO_csk <- disaggregate_country(original_data = FAO,
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
                                   disaggregate_iso = c("scg", "svn", "mkd","hrv","bih"),
                                   dis_end_year= 1991,
                                   dis_start_year = 1961)
#   Disaggregate scg data
    FAO_scg <- disaggregate_country(original_data = FAO_yug,
                                  id_cols = c('iso','sector'),
                                  trend_data = population,
                                  trend_match_cols = 'iso',
                                  combined_iso = "scg",
                                  disaggregate_iso = c("srb","mne"),
                                  dis_end_year= 2005,
                                  dis_start_year = 1961)
# -----------------------------------------------------------------------------
# 4. Final Processing

  FAO_out <- FAO_scg

#   Carry foward last value to end year
    FAO_last <- max( as.numeric ( gsub('X','',names ( FAO_out ) ) ), na.rm = T )
    years <- paste0('X',1961:end_year)
    add_years <- years[ years %!in% names( FAO_out ) ]
    FAO_out[ add_years ] <- FAO_out[ paste0('X', FAO_last ) ]

#   Add units and fuel variables
    FAO_out <- FAO_out %>%
        dplyr::mutate(units = 'kt') %>%
        dplyr::mutate(fuel = 'process')

#   Reorder columns of interest
    FAO_out <- FAO_out[c('iso','sector','fuel','units',years)]

# --------------------------------------------------------------------------------
# 5. Output
  addToEmissionsDb_overwrite(FAO_out,em='N2O',type='NC')

  writeData( FAO_out, domain = "MED_OUT", fn = "C.N2O_NC_emissions_agriculture")

  logStop()

# END
