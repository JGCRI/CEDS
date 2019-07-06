#------------------------------------------------------------------------------
# Program Name: C1.2.Fugitive-petr-and-gas_default_process_emissions.R
# Author: Leyang Feng
# Date Last Modified: June 28, 2016
# Program Purpose: Generates default process emissions for 1B2c_Venting-flaring-oil-gas
#                  using part of EDGAR JRC PEGASOS data
# Input Files: C.[em]_ECLIPSE_flaring_emissions_extended.csv,
# Output Files: C.[em]_Fugitive-petr-and-gas default process.csv
# Notes:
# TODO:
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
    headers <- c( 'data_functions.R' ) # Any additional function files required
    log_msg <- "Generating flaring default process emissions" # First message to be printed to the log
    script_name <- "C1.2.Fugitive-petr-and-gas_default_process_emissions.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define emission species and read in files

# Define emissions species variable
    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "CO"

    MODULE_C <- "../code/module-C/"

# read in the extended flaring data
    flaring <- readData( "MED_OUT", file_name = paste0( 'C.', em, '_ECLIPSE_flaring_emissions_extended' ) )
# for non-methane read in the EDGAR JRC PEGASOS data
if ( em != 'CH4' )
    EDGAR_raw <- readData( 'EM_INV', domain_extension = "EDGAR/", file_name = paste0( 'JRC_PEGASOS_', em, '_TS_REF' ),
                           extension = ".xlsx", sheet_selection = 1, skip = 8 )
# for methane read in the EDGAR 4.2 and Edgar fast track
    if ( em == 'CH4' ) {
    EDGAR_raw <- readData( "EM_INV", file_name = '/EDGAR/EDGAR42_CH4' )
    EDGAR_FT_raw <- readData( "EM_INV", file_name = '/EDGAR/v42FT_CH4_2000_2010',
                              extension = ".xlsx", sheet_selection = 1, skip = 9 )
    }

# ------------------------------------------------------------------------------
# 2. Pre-processing

# set the output years
    all_Xyear <- paste0( 'X', 1970 : end_year )

# extract year_list in flaring data
    flaring_Xyear <- colnames( flaring )[ grep( 'X', colnames( flaring ) ) ]

# extract year_list in edgar data
  edgar_year <- colnames( EDGAR_raw )[ c( grep( '19', colnames( EDGAR_raw ) ), grep( '20', colnames( EDGAR_raw ) ) ) ]
  if ( em != 'CH4' ) edgar_Xyear <- paste0( 'X', edgar_year )
  if ( em == 'CH4' ) {edgar_Xyear <- edgar_year
  edgar_year <- as.numeric(sub('X',"",edgar_year))}

# if methane, process EDGAR FT and combine with edgar
if ( em == 'CH4' ){
  edgarFT_year <- colnames( EDGAR_FT_raw )[ c( grep( '19', colnames( EDGAR_FT_raw ) ), grep( '20', colnames( EDGAR_FT_raw ) ) ) ]
  edgarFT_Xyear <- paste0 ( 'X' , edgarFT_year )
  edgarFT <- EDGAR_FT_raw
  names(edgarFT)[ which( names( edgarFT ) %in% edgarFT_year ) ] <- edgarFT_Xyear
  FT_ext_year <- edgarFT_Xyear[ which ( edgarFT_Xyear %!in% edgar_Xyear ) ]
  EDGAR_raw <- left_join(EDGAR_raw, edgarFT[ c( 'ISO_A3' , 'IPCC' , FT_ext_year ) ])
  edgar_Xyear <- c(edgar_Xyear, FT_ext_year)
}

# extract years that are not in edgar data
    edgar_missing_Xyear <- all_Xyear[ which( all_Xyear %!in% edgar_Xyear ) ]

# gnenerate a dummy layout using flaring data in case there's no desired info in edgar_raw
    dummy_layout <- flaring[ , c( 'iso', 'sector', all_Xyear ) ]
    dummy_layout[ , all_Xyear ] <- 0

# cleaning edgar data
    if ( em != 'CH4' ) edgar <- EDGAR_raw[ , c('ISO_A3', 'IPCC', 'IPCC_description', edgar_year ) ]
    if ( em == 'CH4' ) edgar <- EDGAR_raw[ , c('ISO_A3', 'IPCC', 'IPCC_description', edgar_Xyear ) ]
    edgar$ISO_A3 <- tolower( edgar$ISO_A3 )
    colnames( edgar ) <- c( 'iso', 'sector', 'sector_description', edgar_Xyear )

    # extend edgar to flaring years using edgar 2010 data
    edgar[ , edgar_missing_Xyear ] <- edgar[ last(edgar_Xyear) ]
    edgar <- edgar[ edgar$sector == '1B2', ]
    edgar <- edgar[ !is.na( edgar$iso ), ]
    edgar[ is.na( edgar ) ] <- 0
    edgar <- edgar[ , c( 'iso', 'sector', 'sector_description', all_Xyear ) ]

    # split edgar data into three parts
    # there are three 1B2 sub-sectors in EGDAR: Fugitive emissions from oil and gas;
    #                                           Fugitive emissions from gaseous fuels;
    #                                           Fugitive emissions from liquid fuels
    # for most of the edgar data ( except NMVOC ), only Fugitive emissions from oil and gas exists.
    # but eventually, we want the flaring default process emissions to be generated as ( in the next section ):
    #   1B2_Fugitive-petr-and-gas default emissions = max( ECLIPSE, EDGAR 1B2_ Fugitive emissions from oil and gas ) +
    #                                                 EDGAR 1B2_Fugitive emissions from gaseous fuels +
    #                                                 EDGAR 1B2_ Fugitive emissions from liquid fuels
    # so we split the edgar data in here first

    # extract edgar Fugitive emissions from oil and gas
    edgar_oil_gas <- edgar[ edgar$sector_description == 'Fugitive emissions from oil and gas', c( 'iso', 'sector', all_Xyear ) ]
    edgar_gas_fuel <- edgar[ edgar$sector_description == 'Fugitive emissions from gaseous fuels', c( 'iso', 'sector', all_Xyear ) ]
    edgar_liquid_fuel <- edgar[ edgar$sector_description == 'Fugitive emissions from liquid fuels', c( 'iso', 'sector', all_Xyear ) ]

    # if no '1B2-Fugitive emissions from oil and gas' in certain species use the dummy_layout
    if ( dim( edgar_oil_gas )[ 1 ] == 0 ) { edgar_oil_gas <- dummy_layout }

# extend flaring and edgar_oil_gas to have all countries
    flaring_data <- flaring[ , c( 'iso', all_Xyear ) ]
    edgar_data <- edgar_oil_gas[ , c( 'iso', all_Xyear ) ]
    merge_table <- merge( flaring_data, edgar_data, by = 'iso', all = T )
    merge_table[ is.na( merge_table ) ] <- 0
    # generate falring and edgar comparing matrices
    flaring_mat <- merge_table[ , paste0( all_Xyear, '.x' ) ]
    edgar_mat <- merge_table[ , paste0( all_Xyear, '.y' ) ]

# ------------------------------------------------------------------------------
# 3. Generates the flaring default process emissions
#    The final flaring default process emissions takes the max value between EDGAR '1B2-Fugitive emissions from oil and gas'  and flaring extended

    comparing_mat <- flaring_mat >= edgar_mat

    flaring_mask <- ifelse( comparing_mat == T, 1, 0 )
    edgar_mask <- ifelse( comparing_mat  == T, 0, 1 )

    emissions_mat <- flaring_mat * flaring_mask + edgar_mat * edgar_mask

    emissions <- cbind( merge_table$iso, emissions_mat )
    colnames( emissions ) <- c( 'iso', all_Xyear )
    emissions$fuel <- 'process'
    emissions$sector <- '1B2_Fugitive-petr-and-gas'
    emissions$units <- 'kt'

    emissions <- emissions[ , c( 'iso', 'sector', 'fuel', 'units', all_Xyear ) ]

# 3.1 optional routine if '1B2-Fugitive emissions from gaseous fuels' does exits
    if ( dim( edgar_gas_fuel )[ 1 ] != 0 ) {
      for ( iso in edgar_gas_fuel$iso ) {
        emissions[ emissions$iso == iso, all_Xyear ] <- emissions[ emissions$iso == iso, all_Xyear ] +
          edgar_gas_fuel[ edgar_gas_fuel$iso == iso, all_Xyear ]
        }
      }

# 3.2 optional routine if '1B2-Fugitive emissions from liquid fuels' does exits
    if ( dim( edgar_liquid_fuel )[ 1 ] != 0 ) {
      for ( iso in edgar_liquid_fuel$iso ) {
        emissions[ emissions$iso == iso, all_Xyear ] <- emissions[ emissions$iso == iso, all_Xyear ] +
          edgar_liquid_fuel[ edgar_liquid_fuel$iso == iso, all_Xyear ]
        }
      }

# -----------------------------------------------------------------------------
# 4. Write output
    meta_names <- c( "Data.Type", "Emission", "Sector", "Start.Year", "End.Year", "Source.Comment")
    meta_note <- c( "default process emissions for 1B2c_Venting-flaring-oil-gas", em, "1B2_Fugitive-petr-and-gas",
        "1970", end_year, "Max value between EDGAR JRC PEGASOS data and extended ECLIPSE flaring emissions is taken as default process emissions for country-year combination for sector 1B2c_Venting-flaring-oil-gas" )
    addMetaData( meta_note, meta_names )

    writeData( emissions , "DEFAULT_EF_IN", domain_extension = 'non-combustion-emissions/' ,paste0( "C.", em, "_Fugitive-petr-and-gas_default_process_emissions" ) )

# Every script should finish with this line:
    logStop()
