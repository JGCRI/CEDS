#------------------------------------------------------------------------------
# Program Name: C1.2.Fugitive-petr-and-gas_default_process_emissions.R
# Author: Leyang Feng, Patrick O'Rourke
# Date Last Modified: June 12, 2020
# Program Purpose: Generates default process emissions for CEDS "1B2_Fugitive-petr-and-gas"
#                  using EDGAR and ECLIPSE data
# Input Files: C.[em]_ECLIPSE_flaring_emissions_extended.csv,
#              relevant EDGAR emissions data ( EDGAR v5 = v50_[em]_1970_[edgar_end_year].xls )
#              NC_EDGAR_sector_mapping.csv
# Output Files: C.[em]_Fugitive-petr-and-gas_default_process_emissions.csv
# Notes:
# TODO:
#-------------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
    headers <- c( 'data_functions.R' ) # Any additional function files required
    log_msg <- "Generating flaring default process emissions..." # First message to be printed to the log
    script_name <- "C1.2.Fugitive-petr-and-gas_default_process_emissions.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define emission species and read in files

# Define emissions species variable
    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "CO"

# Read in the extended flaring data
    flaring <- readData( "MED_OUT", file_name = paste0( 'C.', em, '_ECLIPSE_flaring_emissions_extended' ) )

# Read in EDGAR non-combustion sector mapping file
    NC_sector_map <- readData( "MAPPINGS", "NC_EDGAR_sector_mapping" )

# Read in EDGAR data

#   Define EDGAR version using and other input settings
    vn <- "5.0"

#   Defineinput domain
    domain_use <- "EM_INV"
    domain_ext <- "EDGAR/"

#   Read in EDGAR v4.2 - original CEDS release used this only for CH4
    if( as.numeric( vn ) == 4.2 ){

      EDGAR_raw <- readData( domain = domain_use, domain_extension = domain_ext,
                             file_name = paste0( "EDGAR", gsub( "[.]", "", vn ), "_", em  ) )

#     If em is CH4, read in additional EDGAR v4.2 file
      if ( em == 'CH4' ) {

        skip_rows <- 9
        extension_use <- ".xlsx"
        EDGAR_FT_raw <- readData( domain = domain_use,  domain_extension = domain_ext,
                                  file_name = paste0( 'v', gsub( "[.]", "", vn ), "FT_", em, "_2000_2010" ),
                                  extension = extension_use, sheet_selection = 1, skip = skip_rows )

      }

#   Read in EDGAR v4.3 - original CEDS released used this for all ems other than CH4 (and CO2, since this
#   script is not run for CO2)
    } else if( as.numeric( vn ) == 4.3 ){

      skip_rows <- 8
      extension_use <- ".xlsx"
      sheet_name <- paste0( 'NEW_v', vn, '_EM_', em, '_ref' )
      EDGAR_raw <- readData( domain = domain_use, domain_extension = domain_ext,
                             file_name = paste0( 'JRC_PEGASOS_', em, '_TS_REF' ),
                             extension = extension_use, sheet_selection = sheet_name, skip = skip_rows )

#   Readin EDGAR v5
    } else if( as.numeric( vn ) == 5 ){

      fn <- c( paste0( "v",  gsub( "[.]", "", vn ), "_", em, "_", EDGAR_start_year, "_",
                       EDGAR_end_year ), ".xls")
      sheet_to_use <- paste0( "v", vn, "_EM_", em, "_IPCC1996" )
      skip_rows <- 9

#     EDGAR v5 has a special file naming convention for CO2
      if( em == "CO2" ){

        EDGAR_CO2_real_end_year <- 2018
        fn <- c( paste0( "v",  gsub( "[.]", "", vn ), "_", em, "_", "excl_short-cycle_org_C_",
                         EDGAR_start_year, "_", EDGAR_CO2_real_end_year ), ".xls")

      }

      EDGAR_raw <- readData( domain = domain_use, domain_extension = domain_ext,
                         file_name = fn[ 1 ], extension = fn[ 2 ],
                         sheet_selection = sheet_to_use, skip = skip_rows,
                         missing_value = c( "", "NULL" ) )

#   Naming pattern for other EDGAR versions
    } else {

      stop( script_name, " has not been formatted to process Edgar v", vn, ". Please ",
            "reformat the script and rerun." )

    }

# ------------------------------------------------------------------------------
# 2. Pre-processing

# Define EDGAR fugitive oil and gas sector
    EDGAR_fug_oil_gas_mapping <- 	NC_sector_map %>%
      dplyr::filter( ceds_sector == "1B2_Fugitive-petr-and-gas" )

    EDGAR_fug_oil_gas_sec <- sort( unique( EDGAR_fug_oil_gas_mapping$edgar_sector ) )
    EDGAR_fug_oil_gas_sec_desc <- sort( unique( EDGAR_fug_oil_gas_mapping$edgar_descr ) )

# Define the output years
    all_Xyear <- paste0( 'X', EDGAR_start_year : end_year )

# Extract year_list in flaring data
    flaring_Xyear <- colnames( flaring )[ grep( 'X', colnames( flaring ) ) ]

# Extract year_list in EDGAR data
  edgar_year <- colnames( EDGAR_raw )[ c( grep( '19', colnames( EDGAR_raw ) ), grep( '20', colnames( EDGAR_raw ) ) ) ]
  if ( vn %in% c( "4.3", "5.0" ) ){ edgar_Xyear <- paste0( 'X', edgar_year ) }
  if ( em == "4.2" ){ edgar_Xyear <- edgar_year }
  edgar_year <- as.numeric( sub( 'X', "", edgar_year ) )

# Define final CEDS fuel, sector, and units
  final_fuel <- 'process'
  final_sector <- '1B2_Fugitive-petr-and-gas'
  final_units <- 'kt'

# If methane and EDGAR version is v4.2, process EDGAR FT and combine with edgar
if ( em == 'CH4' & vn == "4.2" ){

  edgarFT_year <- colnames( EDGAR_FT_raw )[ c( grep( '19', colnames( EDGAR_FT_raw ) ), grep( '20', colnames( EDGAR_FT_raw ) ) ) ]
  edgarFT_Xyear <- paste0 ( 'X' , edgarFT_year )
  edgarFT <- EDGAR_FT_raw
  names(edgarFT)[ which( names( edgarFT ) %in% edgarFT_year ) ] <- edgarFT_Xyear
  FT_ext_year <- edgarFT_Xyear[ which ( edgarFT_Xyear %!in% edgar_Xyear ) ]
  EDGAR_raw <- left_join(EDGAR_raw, edgarFT[ c( 'ISO_A3' , 'IPCC' , FT_ext_year ) ])
  edgar_Xyear <- c(edgar_Xyear, FT_ext_year)

}

# Extract years that are not in EDGAR data
    edgar_missing_Xyear <- all_Xyear[ which( all_Xyear %!in% edgar_Xyear ) ]

# Generate a dummy layout using flaring data in case there's no desired info in edgar_raw
    dummy_layout <- flaring[ , c( 'iso', 'sector', all_Xyear ) ]
    dummy_layout[ , all_Xyear ] <- 0

# Clean EDGAR data
    if( vn %in% c( "4.3", "5.0" ) ){ edgar <- EDGAR_raw[ , c('ISO_A3', 'IPCC', 'IPCC_description', edgar_year ) ] }
    if( vn == "4.2" ){ edgar <- EDGAR_raw[ , c( 'ISO_A3', 'IPCC', 'IPCC_description', edgar_Xyear ) ] }
    edgar$ISO_A3 <- tolower( edgar$ISO_A3 )
    colnames( edgar ) <- c( 'iso', 'sector', 'sector_description', edgar_Xyear )

# Remove rows with all NA's
    edgar <- edgar[ apply( X = edgar[ , edgar_Xyear ],
                           MARGIN = 1, function( x ) ( !all.na( x ) ) ) ,]

# Turn NAs to zeros
    edgar[ is.na( edgar ) ] <- 0

# Make negative emissions zero
    neg_rows <- apply( edgar[, edgar_Xyear ], 1, function( row ) any( row < 0 ) )
    edgar_neg <- edgar[ neg_rows, ]
    edgar[ edgar < 0 ] <- 0

# Extend EDGAR to flaring years using the last available EDGAR year (2015 for all ems other than CO2 in EDGAR v5)
    edgar[ , edgar_missing_Xyear ] <- edgar[ last( edgar_Xyear ) ]
    edgar <- edgar[ edgar$sector %in% EDGAR_fug_oil_gas_sec, ]
    edgar <- edgar[ !is.na( edgar$iso ), ]
    edgar <- edgar[ , c( 'iso', 'sector', 'sector_description', all_Xyear ) ]

#     Split EDGAR data into three parts
#     As of EDGAR v5, there are 5 1B2 sub-sectors in EGDAR: Fugitive emissions from oil and gas (fossil fuels OR biofuels)
#                                                           Fugitive emissions from gaseous fuels (fossil fuels OR biofuels)
#                                                           Fugitive emissions from liquid fuels
#     For most of the EDGAR data ( except for CH4 and NMVOC ), only Fugitive emissions from oil and gas exists.
#     Eventually, we want the flaring default process emissions to be generated as ( in the next section ):
#     1B2_Fugitive-petr-and-gas default emissions = max( ECLIPSE, EDGAR 1B2_ Fugitive emissions from oil and gas ) +
#                                                   EDGAR 1B2_Fugitive emissions from gaseous fuels +
#                                                   EDGAR 1B2_ Fugitive emissions from liquid fuels
#     So we split the EDGAR data here first

#   Extract edgar Fugitive emissions from oil and gas
    edgar_oil_string <- 'oil and gas'
    edgar_fug_oil_sectors <- grep( edgar_oil_string,  EDGAR_fug_oil_gas_sec_desc, value = TRUE  )
    edgar_fug_oil_sectors <- gsub( " \\s*\\([^\\)]+\\)", "",  edgar_fug_oil_sectors ) %>%
      unique( )

    edgar_gas_string <- 'gaseous fuels'
    edgar_fug_gas_sectors <- grep( edgar_gas_string,  EDGAR_fug_oil_gas_sec_desc, value = TRUE  )
    edgar_fug_gas_sectors <- gsub( " \\s*\\([^\\)]+\\)", "",  edgar_fug_gas_sectors ) %>%
      unique( )

    edgar_liquid_string <- 'liquid fuels'
    edgar_fug_liq_sectors <- grep( edgar_gas_string,  EDGAR_fug_oil_gas_sec_desc, value = TRUE  )
    edgar_fug_liq_sectors <- gsub( " \\s*\\([^\\)]+\\)", "",  edgar_fug_liq_sectors ) %>%
      unique( )

    edgar_oil_gas <- edgar %>%
      dplyr::filter( sector_description %in% edgar_fug_oil_sectors ) %>%
      dplyr::select( iso, sector, all_of(all_Xyear) ) %>%
      dplyr::mutate( sector = if_else( grepl( "x", sector ), # Remove the "x" from biofuel fugitive sector string
                                       gsub( "x", "", sector ), sector ) ) %>%
      dplyr::group_by( iso, sector ) %>%
      dplyr::summarise_all( list( ~sum(., na.rm = T ) ) ) %>% # Aggregate across all relevant 1B2 sectors
      dplyr::ungroup( )

    edgar_gas_fuel <- edgar %>%
      dplyr::filter( sector_description %in% edgar_fug_gas_sectors ) %>%
      dplyr::select( iso, sector, all_of(all_Xyear) )  %>%
      dplyr::mutate( sector = if_else( grepl( "x", sector ), # Remove the "x" from biofuel fugitive sector string
                                       gsub( "x", "", sector ), sector ) ) %>%
      dplyr::group_by( iso, sector ) %>%
      dplyr::summarise_all( list( ~sum(., na.rm = T ) ) ) %>% # Aggregate across all relevant 1B2 sectors
      dplyr::ungroup( )

    edgar_liquid_fuel <- edgar %>%
      dplyr::filter( sector_description %in% edgar_fug_liq_sectors ) %>%
      dplyr::select( iso, sector, all_of(all_Xyear) )  %>%
      dplyr::mutate( sector = if_else( grepl( "x", sector ), # Remove the "x" from biofuel fugitive sector string
                                       gsub( "x", "", sector ), sector ) ) %>%
      dplyr::group_by( iso, sector ) %>%
      dplyr::summarise_all( list( ~sum(., na.rm = T ) ) ) %>% # Aggregate across all relevant 1B2 sectors
      dplyr::ungroup( )

#   If no '1B2-Fugitive emissions from oil and gas' in certain species use the dummy_layout
    if( dim( edgar_oil_gas )[ 1 ] == 0 ){ edgar_oil_gas <- dummy_layout }

#   Extend flaring and edgar_oil_gas to have all countries, and provide countries which are added
#   values of 0 for all years
    flaring_data <- flaring[ , c( 'iso', all_Xyear ) ]
    edgar_data <- edgar_oil_gas[ , c( 'iso', all_Xyear ) ]
    merge_table <- merge( flaring_data, edgar_data, by = 'iso', all = T )
    merge_table[ is.na( merge_table ) ] <- 0

#   Generate falring and EDGAR comparing matrices
    flaring_mat <- merge_table[ , paste0( all_Xyear, '.x' ) ]
    edgar_mat <- merge_table[ , paste0( all_Xyear, '.y' ) ]

# ------------------------------------------------------------------------------
# 3. Generates the flaring default process emissions
#    The final flaring default process emissions takes the max value between EDGAR
#    '1B2-Fugitive emissions from oil and gas'  and flaring extended

    comparing_mat <- flaring_mat >= edgar_mat

    flaring_mask <- ifelse( comparing_mat == T, 1, 0 )
    edgar_mask <- ifelse( comparing_mat  == T, 0, 1 )

    emissions_mat <- flaring_mat * flaring_mask + edgar_mat * edgar_mask

    emissions <- cbind( merge_table$iso, emissions_mat )
    colnames( emissions ) <- c( 'iso', all_Xyear )
    emissions$fuel <- final_fuel
    emissions$sector <- final_sector
    emissions$units <- final_units

    emissions <- emissions[ , c( 'iso', 'sector', 'fuel', 'units', all_Xyear ) ]

# 3.1 optional routine if '1B2-Fugitive emissions from gaseous fuels' does exits
    if ( dim( edgar_gas_fuel )[ 1 ] != 0 ) {

      edgar_gas_fuel <- edgar_gas_fuel %>%
        dplyr::mutate( fuel = final_fuel,
                       sector = final_sector,
                       units = final_units ) %>%
        dplyr::select( iso, sector, fuel, units, all_of(all_Xyear) )

      emissions <- emissions %>%
        dplyr::mutate( iso = as.character( iso ) ) %>%
        dplyr::bind_rows( edgar_gas_fuel ) %>%
        dplyr::group_by( iso, sector, fuel, units ) %>%
        dplyr::summarise_all( list( ~sum(., na.rm = T ) ) ) %>%
        dplyr::ungroup( )

    }

# 3.2 optional routine if '1B2-Fugitive emissions from liquid fuels' does exits
    if ( dim( edgar_liquid_fuel )[ 1 ] != 0 ) {

      edgar_liquid_fuel <- edgar_liquid_fuel %>%
        dplyr::mutate( fuel = final_fuel,
                       sector = final_sector,
                       units = final_units ) %>%
        dplyr::select( iso, sector, fuel, units, all_of(all_Xyear) )

      emissions <- emissions %>%
        dplyr::mutate( iso = as.character( iso ) ) %>%
        dplyr::bind_rows( edgar_liquid_fuel ) %>%
        dplyr::group_by( iso, sector, fuel, units ) %>%
        dplyr::summarise_all( list( ~sum(., na.rm = T ) ) ) %>%
        dplyr::ungroup( )

    }

# -----------------------------------------------------------------------------
# 4. Write output

#   Add meta note
    meta_names <- c( "Data.Type", "Emission", "Region", "Sector", "Start.Year",
                     "End.Year", "Source.Comment")
    meta_note <- c( "Default process emissions for 1B2c_Venting-flaring-oil-gas",
                    em, "All", "1B2_Fugitive-petr-and-gas", EDGAR_start_year, end_year,
                    paste0( "Max value between EDGAR", vn,  "data and extended ",
                            "ECLIPSE flaring emissions is taken as default process ",
                            "emissions for country-year combinations for sector ",
                            "1B2c_Venting-flaring-oil-gas" ) )
    source_info <- script_name
    addMetaData( meta_note, meta_names, source_info )

#   Output fugitive petr. and gas default emissions
    writeData( emissions , "DEFAULT_EF_IN", domain_extension = 'non-combustion-emissions/',
               paste0( "C.", em, "_Fugitive-petr-and-gas_default_process_emissions" ) )

# Every script should finish with this line:
    logStop()

