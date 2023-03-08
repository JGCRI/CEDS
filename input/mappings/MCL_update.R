# ------------------------------------------------------------------------------
# Program Name: MCL_update.R
# Authors: Harrison Suchyta
# Date Last Modified: 01 March 2023
# Program Purpose: add updated FAO numbers to the MCL
# Input Files: Master_Coutnry_list.csv, FAO_methane_API.csv
# Output Files: Master_Country_List.csv
# TODO:
# Notes:
# -----------------------------------------------------------------------------


#Read in Master Country List
MCL <- readData( "MAPPINGS", "Master_Country_List_comp", meta = F )
#Read in the Methane API
FAO_API <-  readData('EM_INV' ,'FAO_methane')

#Change names to be consistent with MCL, join the dataframes together
FAO_API_renamed <- FAO_API %>%
    dplyr::mutate(Area = gsub('Czechia','Czech Republic',Area)) %>%
    dplyr::mutate(Area = gsub('Republic of Moldova','Moldova',Area)) %>%
    dplyr::mutate(Area = gsub('Russian Federation','Russia',Area)) %>%
    dplyr::mutate(Area = gsub('USSR','Soviet Union',Area)) %>%
    dplyr::mutate(Area = gsub('North Macedonia','Macedonia',Area)) %>%
    dplyr::mutate(Area = gsub('Yugoslav SFR','Yugoslavia',Area)) %>%
    dplyr::mutate(Area = gsub('Bolivia (Plurinational State of)','Bolivia',Area, fixed = TRUE)) %>%
    dplyr::mutate(Area = gsub('China, Hong Kong SAR','Hong Kong, China',Area)) %>%
    dplyr::mutate(Area = gsub('China, Macao SAR','Macao',Area)) %>%
    dplyr::mutate(Area = gsub('China, mainland','China',Area)) %>%
    dplyr::mutate(Area = gsub('China, Taiwan Province of','Chinese Taipei',Area)) %>%
    dplyr::mutate(Area = gsub("CÃ´te d'Ivoire",'Cote dIvoire',Area)) %>%
    dplyr::mutate(Area = gsub("Democratic People's Republic of Korea",'Democratic Peoples Republic of Korea',Area)) %>%
    dplyr::mutate(Area = gsub("Democratic Republic of the Congo",'Democratic Republic of Congo',Area)) %>%
    dplyr::mutate(Area = gsub("Iran (Islamic Republic of)",'Islamic Republic of Iran',Area, fixed = TRUE)) %>%
    dplyr::mutate(Area = gsub("Lao People's Democratic Republic",'Laos',Area)) %>%
    dplyr::mutate(Area = gsub("Micronesia (Federated States of)",'Federated States of Micronesia',Area, fixed = TRUE)) %>%
    dplyr::mutate(Area = gsub("Myanmar",'Myanmar (Burma)',Area)) %>%
    dplyr::mutate(Area = gsub("Saint Vincent and the Grenadines",'Saint Vincent And Grenadines',Area)) %>%
    dplyr::mutate(Area = gsub("Sudan (former)",'Sudan',Area, fixed = TRUE)) %>%
    dplyr::mutate(Area = gsub("Syrian Arab Republic",'Syria',Area)) %>%
    dplyr::mutate(Area = gsub("United Kingdom of Great Britain and Northern Ireland",'United Kingdom',Area)) %>%
    dplyr::mutate(Area = gsub("United Republic of Tanzania",'Tanzania',Area)) %>%
    dplyr::mutate(Area = gsub("United States of America",'United States',Area)) %>%
    dplyr::mutate(Area = gsub("Venezuela (Bolivarian Republic of)",'Venezuela',Area, fixed = TRUE)) %>%
    dplyr::mutate(Area = gsub("Viet Nam",'Vietnam',Area)) %>%
    select(Area.Code..M49.,Area) %>%
    unique() %>%
    dplyr::rename(FAO_Country_Code_2021 = Area.Code..M49.)

    rename_MCL <- MCL %>%
        left_join(FAO_API_renamed, by = c('Country_Name' = 'Area')) %>%
        write.csv('Master_Country_List.csv')

