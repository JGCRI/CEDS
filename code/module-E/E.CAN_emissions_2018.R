# Program Name: E.CAN_emissions_2018.R
# Authors' Names: Tyler Pitkanen, Jon Seibert, Rachel Hoesly, Steve Smith, Huong Nguyen, Andrea Mott
# Date Last Modified: Sept 07, 2016
# Program Purpose: To read in & reformat Canada emissions inventory data.
#                  This file uses the newer format used since 2018. This data
#                  only extends back to 1990, so older data is still used back
#                  to 1985 in a separate scaling operation. This newer data
#                  should be used last so that any discrepancies are resolved in
#                  favor of the newer data.
# Input Files: EN_APEI_Can_Prov_Terr.csv
# Output Files: E.[em]_CAN_inventory.csv
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
  script_name <- "E.CAN_emissions_2017Update.R"

  source( paste0( PARAM_DIR, "header.R" ) )
  initialize( script_name, log_msg, headers )


# Get emission species first so can name log appropriately
  args_from_makefile <- commandArgs( TRUE )
  em <- args_from_makefile[ 1 ]
  if ( is.na( em ) ) em <- "CH4"
  em.read <- em
  if( em == "NOx" ) em.read <- "NOx (t)"
  if( em == "SO2" ) em.read <- "SOX (t)"
  if( em == "NMVOC" ) em.read <- "VOC (t)"
  if( em == "CO" ) em.read <- "CO (t)"
  if( em == "NH3" ) em.read <- "NH3 (t)"
  if (em %in% c ('BC','OC')) em.read <- "PM25 (t)"

  # # Stop script if running for unsupported emissions species
  # if ( em %!in% c( 'BC', 'CO', 'NH3', 'NMVOC', 'NOx',
  #                  'OC', 'SO2' ) ) {
  #   stop ( paste( 'CAN script is not supported for emission species', em ) )
  # }

# ------------------------------------------------------------------------------
# 1. Define parameters for inventory specific script
  inventory_data_file <- 'EN_APEI-Can-Prov_Terr'
  subfolder_name <- 'Canada/'
  inv_data_folder <- "EM_INV"
  inv_name <- 'CAN_2018' # For naming diagnostic files
  inv_years <- c( 1990:2018 )
  # Because this data comes read in as reversed.
  inv_years_reversed <- c( 2018:1990 )

# ------------------------------------------------------------------------------

# 2. Inventory in Standard Form (iso-sector-fuel-years, iso-sector-years, etc)

  file_path <- filePath( inv_data_folder, inventory_data_file,
                         extension = ".csv",
                         domain_extension = subfolder_name )

# Process given emission if inventory data exists

  if ( file.exists( file_path ) ) {
    # Import Inventory
    inv_data <- readData( inv_data_folder,
                                domain_extension = subfolder_name,
                                inventory_data_file , ".csv",
                                sheet_selection = sheet_name )
    # Write out blank df if no inventory data exists for given emission
  } else {
    inv_data <- data.frame()
  }

    # Remove double counting (remove rows in R that have a "y" under "is it a total", which is same as removing N/A from the subsector columns)
    inv_data_sheet <- inv_data[!is.na(inv_data$Subsector),]

    # select valuable columns
    inv_data_sheet <- select(inv_data_sheet,Region,Source,Sector,Year,"SOX..t.","NOX..t.","CO..t.","NH3..t.","VOC..t.","PM25..t.")
    inv_data_sheet$iso <- "can"
    inv_data_sheet <- dplyr::rename(inv_data_sheet,
                                             sector=Source,subsector = Sector,
                                             SO2=SOX..t.,
                                             NOx=NOX..t.,
                                             CO =CO..t.,
                                             NH3=NH3..t.,
                                             NMVOC = VOC..t.,
                                             PM25= PM25..t.)

    # Set NAs to 0
    inv_data_sheet[is.na(inv_data_sheet)] <- 0

    # Run if em is not CO2, CH4, or N20 - CAN does not have these
    if( em %!in% c("CO2", "CH4", "N2O") ){

    # em_list <- c( "SO2","NOx","CO","NH3","NMVOC","BC","OC")
    # for( n in em_list){
    #   em <- paste0( n )

    # differentiate BC and OC from PM2.5, but keep variable em for BC/oC script in part 3
    if (em %in% c ('BC','OC')) {
      em2 <- 'PM25'
    } else {em2 <- em}

      # aggregate emissions by region, sector, subsector, and year
      output <- inv_data_sheet %>%
        dplyr::select( c("iso", "sector", "subsector", "Year", em2 ) ) %>%
        dplyr::group_by( iso, sector, subsector, Year ) %>%
        dplyr::mutate( em2 = sum( get( em2 ) ) ) %>%
        dplyr::distinct( iso, sector, subsector, Year, em2) %>%
        tidyr::spread( key = "Year", value = "em2")

      # combined sector and subsector into one column and rename as "sector" for mapping
      output_sector_sub_combined <- transform(output, sector_subsector=paste(sector,subsector,sep="_"))
      output_sector_sub_combined$sector_subsector <-gsub(" / ","",output_sector_sub_combined$sector_subsector, fixed=TRUE)
      output_final <- subset(output_sector_sub_combined, select = -c(sector,subsector))
      output_final <- dplyr::rename(output_final,sector = sector_subsector)

      # Make numeric
      output_final[ , paste0( 'X', inv_years ) ] <-
        sapply( output_final[ , paste0( 'X', inv_years ) ],
                as.numeric )
      # Convert from tonnes to kt
      output_final[ , paste0( 'X', inv_years ) ] <-
        as.matrix( output_final[ , paste0( 'X', inv_years ) ] ) / 1000

# ------------------------------------------------------------------------------
# Find BC and OC emissions

    # Define parameters for BC and OC specific script
    # Read in scaling mapping file and filter transportation sectors

    ceds_sector <- "1A3b_Road"
    inv_iso <- "can"
    PM <- "PM25"
    mapping_file <- readData("SCALE_MAPPINGS", "CAN_2018_scaling_mapping.csv")
    mapping_file <- mapping_file %>%
      filter(str_detect(scaling_sector,"Road"))

    inv_sector_name <- mapping_file$inv_sector

    X_inv_years <- paste0("X",inv_years) # put inventory years in BC/OC script format
    inv_data_sheet <- output_final       # rename for BC/OC script input


    # Calculate BC and OC emissions

    if (em %in% c("BC","OC") ) {

        em_emissions <- F.Estimate_BC_OC_emissions(em, PM, inv_iso,ceds_sector,inv_sector_name,X_inv_years)
        output_final <- em_emissions

    }

# -----------------------------------------------------------------------------
    } else {Note <- c( "No CAN data available." )
    output_final <- dplyr::tibble( Note )}


# 3. Write standard form inventory-

    writeData(output_final, domain = "MED_OUT",
              paste0('E.', em, '_', inv_name, '_inventory'))



# Every script should finish with this line
    logStop()
    # END


