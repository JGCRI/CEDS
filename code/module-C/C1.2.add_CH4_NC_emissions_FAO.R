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

  MCL <- readData( "MAPPINGS", "Master_Country_List", meta = F )
  FAO_API_old <-  readData('EM_INV' ,'FAO_methane_API')
  FAO_API <-  readData('EM_INV' ,'FAO_methane')
  un_pop <- readData( "MED_OUT" , 'A.UN_pop_master' )

# -----------------------------------------------------------------------------
# 2. Retrive and process data from FAOSTAT - uncomment to update, then run script.

    # FAO_querey <- readData('MAPPINGS','FAO_methane_query')
    # FAO_list <- with(FAO_querey,
    #      getFAOtoSYB(name = sector, domainCode = domainCode,
    #      itemCode = itemCode, elementCode = elementCode,
    #      useCHMT = TRUE, outputFormat = "wide"))
    # writeData(FAO_list[[1]],'EM_INV' ,'FAO_methane_API')
    # FAO_API <-  readData('EM_INV' ,'FAO_methane_API')

# -----------------------------------------------------------------------------
# 3. Process FAO methane data

# Remove countries with limited or inconsistent data, use default instead
#fao_data_remove <- c('Aruba') # arb has only one datapoint, so stay with EDGAR default

#DELETE CHUNK
# FAO_new <- FAO_API_old %>%
#        filter( FAOST_CODE %!in% fao_data_remove) %>%
#        left_join(MCL[c('iso','FAO_Country_Code')], by = c('FAOST_CODE' = 'FAO_Country_Code')) %>%
#        select(-FAOST_CODE) %>%
#        melt(id.vars = c('iso','Year')) %>%
#        dplyr::mutate(value = as.numeric(as.character(value))) %>%
#        dplyr::mutate(variable = gsub('X','',variable)) %>%
#        dplyr::mutate(variable = gsub('\\.','-',variable)) %>%
#        filter(!is.na(iso), Year %in% extended_years) %>%
#        dplyr::mutate(Year = paste0('X',Year)) %>% unique %>%
#        cast(iso+variable~Year) %>%
#        dplyr::rename(sector = variable)

FAO <- FAO_API %>%
       dplyr::rename(FAO_Country_Code_2021 = Area.Code..M49.) %>%
       left_join(MCL[c('iso','Country_Name','FAO_Country_Code_2021')], by = 'FAO_Country_Code_2021') %>%
       select(iso,Year,Item,Value) %>%
       dplyr::rename(sector = Item) %>%
       filter(!is.na(iso), Year %in% extended_years) %>%
       dplyr::mutate(Year = paste0('X',Year)) %>% unique() %>%
       mutate(as.character(Value)) %>%
       cast(iso+sector~Year, value = 'Value', sum)


# -----------------------------------------------------------------------------
# 4. Country Splitting
  list(czech.slovkia = c(1961, 1993, "csk","cze","svk"),
       bel.lux = c(1961, 2000, "blx","bel","lux"),
       yugo = c(1961, 1992, "yug", "scg", "svn", "mkd","hrv","bih"),
       ser.mont = c(1961, 2006, "scg","srb","mne"),
       soviet = c(1961, 1992, "USSR","arm","aze","blr","est","geo","kaz",
                  "kgz","ltu","lva","mda","rus","tjk","tkm","ukr","uzb"))


  un_pop$X_year <- paste0( 'X', un_pop$year)
  un_pop$pop <- as.numeric(un_pop$pop)
  population <- cast( un_pop[which ( un_pop$year %in% historical_pre_extension_year:end_year ) , ] ,
                      iso ~ X_year, value = 'pop')


  FAO_csk <- disaggregate_country(original_data = FAO,
                                  id_cols = c('iso','sector'),
                                  trend_data = population,
                                  trend_match_cols = 'iso',
                                  combined_iso = 'csk',
                                  disaggregate_iso = c("cze","svk"),
                                  dis_end_year= 1992,
                                  dis_start_year = 1961)
  FAO_ussr <- disaggregate_country(original_data = FAO_csk,
                                  id_cols = c('iso','sector'),
                                  trend_data = population,
                                  trend_match_cols = 'iso',
                                  combined_iso = 'ussr',
                                  disaggregate_iso = c("arm","aze","blr","est","geo","kaz",
                                                       "kgz","ltu","lva","mda","rus","tjk","tkm","ukr","uzb"),
                                  dis_end_year= 1991,
                                  dis_start_year = 1961)
  FAO_blx <- tryCatch(
      {
          FAO_blx <- disaggregate_country(original_data = FAO_ussr,
                                          id_cols = c('iso','sector'),
                                          trend_data = population,
                                          trend_match_cols = 'iso',
                                          combined_iso = 'blx',
                                          disaggregate_iso = c("bel","lux"),
                                          dis_end_year= 1999,
                                          dis_start_year = 1961)

          return(FAO_blx)
      },
      error=function(cond) {
          message(paste("Warning: The blx iso does not appear to exist in the FAO data"))
          message(cond)
          FAO_blx <- FAO_ussr
          return(FAO_blx)
      }
  )

  FAO_yug <- disaggregate_country(original_data = FAO_blx,
                                   id_cols = c('iso','sector'),
                                   trend_data = population,
                                   trend_match_cols = 'iso',
                                   combined_iso = "yug",
                                   disaggregate_iso = c("scg", "svn", "mkd","hrv","bih"),
                                   dis_end_year= 1991,
                                   dis_start_year = 1961)
  FAO_scg <- disaggregate_country(original_data = FAO_yug,
                                  id_cols = c('iso','sector'),
                                  trend_data = population,
                                  trend_match_cols = 'iso',
                                  combined_iso = "scg",
                                  disaggregate_iso = c("srb","mne"),
                                  dis_end_year= 2005,
                                  dis_start_year = 1961)
# -----------------------------------------------------------------------------
# 5. Final Processing

  FAO_out <- FAO_scg
  # carry foward last value to end year
  FAO_last <- max( as.numeric ( gsub('X','',names ( FAO_out ) ) ), na.rm = T )
  years <- paste0('X',1961:end_year)
  add_years <- years[ years %!in% names( FAO_out ) ]
  FAO_out[ add_years ] <- FAO_out[ paste0('X', FAO_last ) ]

  FAO_out <- FAO_out %>%
    dplyr::mutate(units = 'kt') %>%
    dplyr::mutate(fuel = 'process')

  FAO_out <- FAO_out[c('iso','sector','fuel','units',years)]

# --------------------------------------------------------------------------------
# 6. Output
  addToEmissionsDb_overwrite(FAO_out,em='CH4',type='NC')

  writeData( FAO_out, domain = "MED_OUT", fn = "C.CH4_NC_emissions_agriculture")

  logStop()
# END
