# Program Name: E.CAN_emissions_2021.R
# Authors' Names: Tyler Pitkanen, Jon Seibert, Rachel Hoesly, Steve Smith, Huong Nguyen, Andrea Mott, Harrison Suchyta
# Date Last Modified: Jan 17, 2023
# Program Purpose: To read in & reformat Canada emissions inventory data.
#                  This file uses the newer format used since 2022. This data
#                  only extends back to 1990, so older data is still used back
#                  to 1985 in a separate scaling operation. This newer data
#                  should be used last so that any discrepancies are resolved in
#                  favor of the newer data.
# Input Files: EN_APEI_Can_Prov_Terr.csv
# Output Files: E.[em]_CAN_2021_inventory.csv, E.[em]_CAN_2018_inventory_country_total.csv
# Notes:
# TODO: Re-write read-in so that order of years is taken from input data instead of assumed.
# ------------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
  PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# # Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
  headers <- c( 'common_data.R', "data_functions.R",
                "emissions_scaling_functions.R", "analysis_functions.R",
                "interpolation_extension_functions.R" ) # Additional function files required.
  log_msg <- "Initial reformatting of Canada emissions (newer data)" # First message to be printed to the log
  script_name <- "E.CAN_emissions_2021.R"

  source( paste0( PARAM_DIR, "header.R" ) )
  initialize( script_name, log_msg, headers )


  # Get emission species first so can name log appropriately
  args_from_makefile <- commandArgs( TRUE )
  em <- args_from_makefile[ 1 ]
  if ( is.na( em ) ) em <- "CH4"

  # ------------------------------------------------------------------------------
  # 1. Define parameters for inventory specific script
  inventory_data_file <- 'EN_APEI-Can-Prov_Terr'
  subfolder_name <- 'Canada/'
  inv_data_folder <- "EM_INV"
  inv_name <- 'CAN_2021' # For naming diagnostic files
  inv_years <- c( 1990:2021 )
  # Because this data comes read in as reversed.
  inv_years_reversed <- c( 2021:1990 )

  # Define function for processing raw data

  output_data <- function(em2) {
      #read in the raw data
      raw_data <- list() #empty list
      raw_data_path <- paste0('emissions-inventories/Canada/raw_2021/',em2,'/')
      temp <- list.files(path = raw_data_path, pattern = paste0('APEI*'))
      temp <- temp[ !grepl("metadata", temp) ]
      for(i in 1:length(temp)) {raw_data[[i]] <- assign(paste0(em2,'_',i),read.csv(file = paste0(raw_data_path,temp[i]), stringsAsFactors = FALSE)%>%
                                                            select(Source = 1, everything()) %>%
                                                            gather("year","value",-c('Source','Sector','SubSector')))}
      #combine all the raw data into one table, remove duplicate rows and totals
      #for each sector and subsector
      raw_data_total <- bind_rows(raw_data) %>%
          distinct() %>%
          filter(Source != 'Grand total') %>%
          filter(Sector != '') %>%
          filter(SubSector != '')

      #condense the Source and Sector columns into one column, get rid of the "/"
      #and transform the values from characters to doubles
      output_sector_sub_combined <- transform(raw_data_total, sector_subsector=paste(Source,Sector,sep="_"))
      output_sector_sub_combined$sector_subsector <-gsub("/","",output_sector_sub_combined$sector_subsector, fixed=TRUE)
      output_sector_sub_combined$value <- as.double(output_sector_sub_combined$value)

      #Summarise each sector_subsector column, convert to kt, and get into wide form
      output_final <- output_sector_sub_combined %>%
          mutate(value = value / 1000,
                 iso = 'can') %>%
          dplyr::rename(sector = sector_subsector) %>%
          dplyr::group_by(iso,sector,year) %>%
          dplyr::summarise(value = sum(value)) %>%
          spread(key = 'year', value = 'value')

      return(output_final)
  }

  # ------------------------------------------------------------------------------
  # 2. Reformat raw data files

  #Makes sure only present data is processed
  if( em %!in% c("CO2", "CH4", "N2O") ){

      if (em %in% c ('BC','OC')) {

          # Write out the APEI BC inventory country total, which will be different than
          # the BC estimate using PM2.5 data
          if (em == "BC") {

              country_total <- output_data(em)
              country_total <- country_total %>%
                  filter( !sector %in% c("Fires_Prescribed Burning",
                                         "Fires_Structural Fires",
                                         "Transportation and Mobile Equipment_Air Transportation",
                                         "Transportation and Mobile Equipment_Marine Transportation"
                  )) %>%
                  ungroup() %>%
                  dplyr::select(-sector) %>%
                  group_by(iso) %>%
                  summarise_at(vars(-group_cols(),),sum)

              writeData( country_total, domain = "MED_OUT",
                         paste0('E.',em,'_', inv_name, '_APEI_inventory_country_total'))

          }

          em2 <- 'PM25'
      } else {em2 <- em}

      output_final <- output_data(em2)

      # Find BC and OC emissions

      # Define parameters for BC and OC specific script
      # Read in scaling mapping file and filter transportation sectors
      ceds_sector <- "1A3b_Road"
      inv_iso <- "can"
      PM <- "PM25"
      mapping_file <- readData("SCALE_MAPPINGS", "CAN_2018_scaling_mapping.csv")
      mapping_file <- mapping_file %>%
          filter(str_detect(road_flag,"Road"))

      inv_sector_name <- mapping_file$inv_sector
      X_inv_years <- paste0("X",inv_years) # put inventory years in BC/OC script format
      inv_data_sheet <- output_final       # rename for BC/OC script input

      # Calculate BC and OC emissions

      if (em %in% c("BC","OC") ) {

          output_final <- F.Estimate_BC_OC_emissions(em, PM, inv_iso,ceds_sector,inv_sector_name,X_inv_years)

      }

  } else {Note <- c( "No CAN data available." )
  output_final <- dplyr::tibble( Note )}


  # 3. Write standard form inventory-

  writeData(output_final, domain = "MED_OUT",
            paste0('E.', em, '_', inv_name, '_inventory'))


  # Write out inventory country totals
  # remove sectors for CEDS comparison
  if (length(output_final)>1 ) {
      country_total <- output_final %>%
          filter( !sector %in% c("Fires_Prescribed Burning",
                                 "Fires_Structural Fires",
                                 "Transportation and Mobile Equipment_Air Transportation",
                                 "Transportation and Mobile Equipment_Marine Transportation"
          ))

      writeData( country_total, domain = "DIAG_OUT", domain_extension = "country-inventory-compare/",
                 paste0('inventory_',em,'_', inv_name))

      #get rid of "sector" column and sum all the yearly data together
      country_total <- country_total %>%
          ungroup() %>%
          dplyr::select(-sector) %>%
          group_by(iso) %>%
          summarise_at(vars(-group_cols(),),sum)

      writeData( country_total, domain = "MED_OUT",
                 paste0('E.',em,'_', inv_name, '_inventory_country_total'))

  } else {Note <- c( "No CAN data available." )
  output_final <- dplyr::tibble( Note )}


  # Every script should finish with this line
  logStop()
  # END
