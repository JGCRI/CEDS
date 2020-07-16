# ----------------------------------------------------------------------------
# Program Name: A2.1.IEA_en_bal.R
# Author's Name: Page Kyle, for GCAM; modified for use in CEDS Project by
#               Steve Smith, Emily Voelker, Tyler Pitkanen, Jon Seibert,
#               Rachel Hoesly, Linh Vu, Patrick O'Rourke
# Date Last Modified: July 16, 2020
# Program Purpose: Aggregates IEA energy statistics to CEDS sectors and fuels
# Input Files: A.IEA_en_stat_ctry_hist.csv, IEA_flow_sector.csv,
#              IEA_product_fuel.csv, IEA_process_sectors.csv, IEA_process_coal.csv,
#              IEA_energy_balance_factor.csv, Master_Fuel_Sector_List.xlsx,
#              A.Fernandes_biomass_conversion.csv
# Output Files: A.en_stat_sector_fuel.csv, A.en_balance_dropped_data.csv,
#               A.en_balance_non_process_fuels_process_sectors.csv
# Notes:
# TODO:
#   -- Adjust biomass TJ/kt conversion factors for regions that use industrial
#   biomass fuels other than wood
# -----------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"


# Call standard script header function to read in universal header files -
#   provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "analysis_functions.R" ) # Additional function files required.
    log_msg <- paste0( "Historical energy balances from IEA aggregated to CEDS ",
                       "sectors and fuels..." ) # First message to be printed to the log
    script_name <- "A2.1.IEA_en_bal.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Read in files
    A.IEA_en_stat_ctry_hist_full <- readData( "MED_OUT", "A.IEA_en_stat_ctry_hist" )
    IEA_flow_sector <- readData( "EN_MAPPINGS", "IEA_flow_sector" )
    IEA_product_fuel <- readData( "EN_MAPPINGS", "IEA_product_fuel" )[ , c( 'product', 'fuel' ) ]
    IEA_process_sectors <- readData( "EN_MAPPINGS", "IEA_process_sectors" )
    IEA_process_coal_sectors <-readData( "EN_MAPPINGS", "IEA_process_coal")
    MSL <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Sectors" )
    IEA_energy_balance_factor <- readData( "ENERGY_IN", "IEA_energy_balance_factor" )
    biomass_conversion <- readData( "MED_OUT", "A.Fernandes_biomass_conversion" )

# ------------------------------------------------------------------------------
# 2. Check that IEA mappings and balance factor files are complete

# Check that IEA flow sector and product fuel mappings are complete
    sectorCheck( IEA_flow_sector )
    fuelCheck( IEA_product_fuel )

# Check that IEA_energy_balance_factor has all energy production and
# transformation sectors defined, and that no other FLOWS are within the file
  unique_balance_factor_flows <- sort( unique( IEA_energy_balance_factor$FLOW ) )

  energy_transf_and_prod_flows <- IEA_flow_sector %>%
    dplyr::filter( energy_production_and_transformation_flag == "en_prod_transf" )
  energy_transf_and_prod_flows <- sort( unique( energy_transf_and_prod_flows$flow_code ) )

  if( any( unique_balance_factor_flows %!in% energy_transf_and_prod_flows ) ){

      extra_en_prod_transf_flow <- subset( unique_balance_factor_flows, unique_balance_factor_flows %!in%
                                     energy_transf_and_prod_flows )

      stop( "The following energy production and transformation FLOWS are within IEA_energy_balance_factor.csv ",
            "but are not defined within IEA_flow_sector.csv as being energy production or transformation sectors: ",
            extra_en_prod_transf_flow )

  } else if ( any( energy_transf_and_prod_flows %!in% unique_balance_factor_flows ) ){

      extra_en_prod_transf_flow <- subset( energy_transf_and_prod_flows, energy_transf_and_prod_flows %!in%
                                           unique_balance_factor_flows )

      stop( "The following energy production and transformation FLOWS are missing from IEA_energy_balance_factor.csv: ",
            extra_en_prod_transf_flow )

  } else {

    printLog( "All energy production and transformation FLOWS have been defined in IEA_energy_balance_factor.csv..." )

  }

# Check that IEA_energy_balance_factor.csv has all neccessary products defined
# for each energy production and transformation FLOW.
  unique_balance_factor_flow_product_combinations <- IEA_energy_balance_factor %>%
    dplyr::select( FLOW, PRODUCT ) %>%
    dplyr::filter( PRODUCT != "-",
                   PRODUCT != "0" )

  relevant_flow_product_combinations <- A.IEA_en_stat_ctry_hist_full %>%
    dplyr::filter( FLOW %in% energy_transf_and_prod_flows ) %>%
    dplyr::filter_at( X_IEA_years, any_vars( !is.na( .) ) ) %>% # Filter out rows which are all NA
    dplyr::filter_at( X_IEA_years, any_vars( . != 0 ) ) %>%  # Filter out rows which are all 0
    dplyr::select( FLOW, PRODUCT ) %>%
    dplyr::distinct( )

  flow_prod_test1 <- dplyr::setdiff( relevant_flow_product_combinations,
                                     unique_balance_factor_flow_product_combinations )

  if( nrow( flow_prod_test1 ) != 0 ){

    file_name <- "IEA_energy_balance_factor-potential_missing_combinations"

    meta_names <- c( "Data.Type", "Emission", "Region", "Sector", "Start.Year",
                     "End.Year", "Source.Comment" )

    meta_note <- c( "Energy Consumption", "NA", "All", "Energy Production and Transformation",
                    IEA_start_year, IEA_end_year,
                    paste0( "Potential IEA FLOW and PRODUCT combinations for energy transformation ",
                            "and production sectors which may be missing from IEA_energy_balance_factor.csv..." ) )

    source_info <- paste0( file_name, ".csv" )

    addMetaData( meta_note, meta_names, source_info )

    writeData( flow_prod_test1, domain = "DIAG_OUT", fn = file_name )

    stop( "IEA_energy_balance_factor.csv appears to be missing certain energy transformation ",
          "and production FLOW + PRODUCT combinations which exist in A.IEA_en_stat_ctry_hist_full.csv ",
          "with non-zero or non-NA values. Please see ", file_name, ".csv..." )

  }

  all_flow_product_combinations <- A.IEA_en_stat_ctry_hist_full %>%
    dplyr::filter( FLOW %in% energy_transf_and_prod_flows ) %>%
    dplyr::select( FLOW, PRODUCT ) %>%
    dplyr::distinct( )

  flow_prod_test2 <- dplyr::setdiff( unique_balance_factor_flow_product_combinations,
                                     all_flow_product_combinations )

  if( nrow( flow_prod_test2 ) != 0 ){

    file_name <- "IEA_energy_balance_factor-unnecessary_combinations"


    printLog( paste0( "IEA_energy_balance_factor.csv appears to contain certain energy transformation ",
              "and production FLOW + PRODUCT combinations which do not exist in A.IEA_en_stat_ctry_hist_full.csv. ",
              "These could be removed from IEA_energy_balance_factor. ", "Please see ", file_name, ".csv... " ) )

    meta_names <- c( "Data.Type", "Emission", "Region", "Sector", "Start.Year",
                     "End.Year", "Source.Comment" )

    meta_note <- c( "Energy Consumption", "NA", "All", "Energy Production and Transformation",
                    IEA_start_year, IEA_end_year,
                    paste0( "IEA FLOW and PRODUCT combinations for energy transformation ",
                            "and production sectors which do not exist in A.IEA_en_stat_ctry_hist_full.csv. ",
                            "but are within ", file_name, ".csv..." ) )

    source_info <- paste0( file_name, ".csv" )

    addMetaData( meta_note, meta_names, source_info )

    writeData( flow_prod_test2, domain = "DIAG_OUT", fn = file_name )

  }

# ------------------------------------------------------------------------------
# 3. Map IEA to CEDS sectors.
#    Map IEA to CEDS fuels and perform Unit Conversions

    printLog( "Matching intermediate sector names and intermediate fuel names into IEA energy statistics..." )

# Add CEDS sector names
    A.IEA_en_stat_ctry_hist <- A.IEA_en_stat_ctry_hist_full %>%
        dplyr::left_join( IEA_flow_sector[ , c( "flow_code", "sector" ) ], by = c( "FLOW" = "flow_code" ) )

# Add CEDS fuels
    A.IEA_en_stat_ctry_hist <- A.IEA_en_stat_ctry_hist %>%
        dplyr::left_join( IEA_product_fuel[ , c( "fuel", "product" ) ], by = c( "PRODUCT" = "product" ) )

# Separate by units in preparation for unit conversion
    TJ_products <- na.omit( IEA_product_fuel[ grep( "TJ",
                                                     IEA_product_fuel[ , "product" ] ), c( "product", "fuel" ) ] )

# Biomass
# Use Fernandez conversion factors for residential biomass and wood default
# for non-residential biomass
# TODO: Adjust conversion factors for regions that use industrial biomass fuels other than wood
    TJ_to_kt_biomass <- TJ_products[ TJ_products$fuel == "biomass", ]

    # Residential biomass consumption
    # These take into account different regional biomass types (wood, ag residue, etc.)
    A.IEA_en_stat_ctry_hist[ A.IEA_en_stat_ctry_hist$PRODUCT %in% TJ_to_kt_biomass$product &
                               grepl( "1A4b_Residential", A.IEA_en_stat_ctry_hist$sector ), X_IEA_years ] <-
      A.IEA_en_stat_ctry_hist[ A.IEA_en_stat_ctry_hist$PRODUCT %in% TJ_to_kt_biomass$product &
                                 grepl( "1A4b_Residential", A.IEA_en_stat_ctry_hist$sector ), X_IEA_years ] /
      biomass_conversion[ match(
        A.IEA_en_stat_ctry_hist$iso[ A.IEA_en_stat_ctry_hist$PRODUCT %in% TJ_to_kt_biomass$product &
                                       grepl( "1A4b_Residential", A.IEA_en_stat_ctry_hist$sector ) ],
        biomass_conversion$iso ), X_IEA_years ]

    # Use standard conversion factor for other sectors
    A.IEA_en_stat_ctry_hist[ A.IEA_en_stat_ctry_hist$PRODUCT %in% TJ_to_kt_biomass$product &
                               !grepl( "1A4b_Residential", A.IEA_en_stat_ctry_hist$sector ), X_IEA_years ] <-
      A.IEA_en_stat_ctry_hist[ A.IEA_en_stat_ctry_hist$PRODUCT %in% TJ_to_kt_biomass$product &
                                 !grepl( "1A4b_Residential", A.IEA_en_stat_ctry_hist$sector ), X_IEA_years ] /
      conversionFactor_biomass_kt_TJ

# Natural Gas
    TJ_to_kt_natural_gas <- unique( TJ_products$product[ grepl( "natural_gas", TJ_products$fuel ) ] )

    TJ_to_kt_natural_gas_list_no_tjnet <- grep( "TJ-net",  TJ_to_kt_natural_gas,
                                                value = TRUE , invert = TRUE )  # The invert selects all IEA_TJ_gas without "TJ-net" units
    TJ_to_kt_natural_gas_list_tjnet <- grep( "TJ-net",  TJ_to_kt_natural_gas, value = TRUE )

    A.IEA_en_stat_ctry_hist <- A.IEA_en_stat_ctry_hist %>%
        tidyr::gather( key = years, value = energy_consumption, X_IEA_years) %>%
        dplyr::mutate( energy_consumption = if_else( PRODUCT %in% TJ_to_kt_natural_gas_list_no_tjnet,
                                                     energy_consumption / conversionFactor_naturalgas_TJ_per_kt_Gross,
                                            if_else( PRODUCT %in% TJ_to_kt_natural_gas_list_tjnet,
                                                     energy_consumption / conversionFactor_naturalgas_TJ_per_kt_Net,
                                                     energy_consumption ) ) ) %>%
        tidyr::spread( years, energy_consumption ) %>%
        dplyr::select( iso, FLOW, PRODUCT, X_IEA_years, sector, fuel )

# Add units to dataframe
    A.IEA_en_stat_ctry_hist$units <- "kt"

# ------------------------------------------------------------------------------
# 4. Fix Energy Balance
#    Use energy balance correction factors and conversions to correct the data

    A.IEA_en_stat_ctry_hist_units <- A.IEA_en_stat_ctry_hist
    printLog( "Correcting IEA energy balance and negative values..." )

# Add IEA flow conversion factor
    A.IEA_en_stat_ctry_hist_units <- A.IEA_en_stat_ctry_hist_units %>%
        dplyr::left_join( IEA_flow_sector[, c( "conversion", "flow_code" ) ], by = c( "FLOW" = "flow_code" ) )

# Add IEA balance correction
     A.IEA_en_stat_ctry_hist_units <- A.IEA_en_stat_ctry_hist_units %>%
         dplyr::left_join( IEA_energy_balance_factor, by = c( "FLOW", "PRODUCT" ) )

# Keep the sign of IEA Flow conversion factor. Don't retain numeric value
    A.IEA_en_stat_ctry_hist_units$conversion <-
                A.IEA_en_stat_ctry_hist_units$conversion / abs( A.IEA_en_stat_ctry_hist_units$conversion )

# Replace balance_correction NA's with 1 so we don't get NAs when applying the conversion factor to data
    A.IEA_en_stat_ctry_hist_units[ which( is.na( A.IEA_en_stat_ctry_hist_units$balance_correction ) ), 'balance_correction' ] <-
          rep_len( x = 1, length.out = length( A.IEA_en_stat_ctry_hist_units[
                                               which( is.na( A.IEA_en_stat_ctry_hist_units$balance_correction ) ),
                                               'balance_correction' ] ) )

# Correct (+/-) Energy Balance
    A.IEA_en_stat_ctry_hist_units[ X_IEA_years ] <- A.IEA_en_stat_ctry_hist_units[ X_IEA_years ] *
        A.IEA_en_stat_ctry_hist_units$conversion * A.IEA_en_stat_ctry_hist_units$balance_correction

# Replace NAs with 0 in units
    A.IEA_en_stat_ctry_hist_units[ is.na( A.IEA_en_stat_ctry_hist_units ) ] <- 0

# Replace 0s with NA in sectors and fuels
    A.IEA_en_stat_ctry_hist_units[ A.IEA_en_stat_ctry_hist_units$sector == 0, 'sector' ] <- NA
    A.IEA_en_stat_ctry_hist_units[ A.IEA_en_stat_ctry_hist_units$fuel == 0, 'fuel' ] <- NA

# Drop rows with all zero or negative data (negative energy balance, outputs in energy tranformation)
   logical <- A.IEA_en_stat_ctry_hist_units[ , X_IEA_years ] <= 0
   drop.row <- apply ( logical, MARGIN = 1, FUN = all )
   DroppedData <- A.IEA_en_stat_ctry_hist_units[ drop.row, ]
   A.IEA_en_stat_ctry_hist_units <- A.IEA_en_stat_ctry_hist_units[ !drop.row, ]

# ------------------------------------------------------------------------------
# 5. Energy Process Drivers and Diagnostics - energy data needed for activity/process driver
#    Input is the unit-corrected A.IEA_en_stat_ctry_hist_units
#    Output is same data in CEDS sector and fuels instead of product and flows.
#    Data not corresponding to CEDS sector/fuel is dropped

    printLog( "Correcting energy production sectors used for activity/driver data..." )
    A.IEA_en_stat_process <- A.IEA_en_stat_ctry_hist_units

# Assign correct ceds sector/fuel
    A.IEA_en_stat_process <- replaceValueColMatch( A.IEA_en_stat_process,
                                                   IEA_process_sectors,
                                                   x.ColName = c( 'sector', 'fuel' ),
                                                   y.ColName = c('activity','fuel'),
                                                   match.x = c( 'FLOW', 'PRODUCT' ),
                                                   match.y = c( 'FLOW', 'PRODUCT' ),
                                                   addEntries = FALSE )

# Assign coal processes to sector/fuel
    A.IEA_process_coal <- replaceValueColMatch( A.IEA_en_stat_ctry_hist_units,
                                                IEA_process_coal_sectors,
                                                x.ColName = c( 'sector', 'fuel' ),
                                                y.ColName = c( 'activity', 'fuel' ),
                                                match.x = c( 'FLOW', 'PRODUCT' ),
                                                match.y = c( 'FLOW', 'PRODUCT' ),
                                                addEntries = FALSE )

# Data that doesn't map to a CEDS sector/fuel and extra columns
    DroppedData <- rbind.fill( DroppedData,
                               A.IEA_en_stat_process[ !complete.cases( A.IEA_en_stat_process[ , c( 'sector', 'fuel' ) ] )
                                                      && A.IEA_process_coal$fuel != "process", ] )

# Drop A.IEA_en_stat_process data that doesn't map to a CEDS sector/fuel and extra columns
    A.IEA_en_stat_process <- A.IEA_en_stat_process[ , c( 'iso', 'sector', 'fuel', 'units', X_IEA_years ) ]
    A.IEA_en_stat_process <- A.IEA_en_stat_process[ complete.cases( A.IEA_en_stat_process ), ]

# Drop A.IEA_process_coal data that doesn't map to a CEDS sector/fuel, extra columns and non-coal process records
    A.IEA_process_coal <- A.IEA_process_coal[ complete.cases( A.IEA_process_coal ) & A.IEA_process_coal$fuel == "process", ]
    A.IEA_process_coal <- A.IEA_process_coal[ , c( 'iso', 'sector', 'fuel', 'units', X_IEA_years ) ]

# Bind the process_coal data frame and the en_start_process data frames
    A.IEA_en_stat_process <- dplyr::bind_rows( A.IEA_en_stat_process, A.IEA_process_coal )

# ------------------------------------------------------------------------------
# 6. Other Data Cleaning and aggregation by CEDS fuel
#    Aggregate based on fuel/sector/iso/units
#    Separate process sectors

    printLog( "Performing other data cleaning..." )

# Aggregate by relevant categories
    printLog( "Aggregating energy statistics by CEDS iso, sector, and fuel..." )

    A.en_stat_sector_fuel <- A.IEA_en_stat_process %>%
        dplyr::group_by( iso, sector, fuel, units ) %>%
        dplyr::summarize_all( sum, na.rm = TRUE ) %>%
        dplyr::ungroup( )

# Check Combustion Sectors vs Process Sectors - If non process fuels for process sectors, seperate and
#   Write to diagnostic file
    combustion_sectors <- c(MSL[ which( MSL$activity %in% c('Energy_Combustion')), 'sector' ],
                            '1A1bc_Other-transformation','1A1bc_Other-feedstocks')

    combustion_data_process_sectors <- A.en_stat_sector_fuel[ which( A.en_stat_sector_fuel$sector %!in% combustion_sectors & A.en_stat_sector_fuel$fuel != 'process' ), ]

    if ( nrow( combustion_data_process_sectors ) ) {
        unique <- unique(combustion_data_process_sectors[ , c( 'sector', 'fuel' ) ] )
        writeData( combustion_data_process_sectors, domain = "DIAG_OUT",
                   fn = "A.en_balance_non_process_fuels_process_sectors",
                   comments = paste0( 'Combustion Data in process sectors, that are not specifically ',
                                      'mapped to a process sector activity in mappings/energy/IEA_process_sectors.csv' ) )
        warning( paste( paste( 'IEA data contains non-zero energy data for CEDS fuels other than "process" ',
                         'for process sectors. Removing from IEA data: ' ),
                      paste( unique$sector, unique$fuel, sep = '-', collapse = ', ') ) )

        A.en_stat_sector_fuel <- A.en_stat_sector_fuel[ -which( A.en_stat_sector_fuel$sector %!in% combustion_sectors & A.en_stat_sector_fuel$fuel != 'process' ), ]
    } # END if

# Reorder the columns for clarity
    A.en_stat_sector_fuel <-
        A.en_stat_sector_fuel[ c( 'iso', 'sector','fuel','units', X_IEA_years ) ]

# -----------------------------------------------------------------------------
# 7. Output

# Add comments for each table
    comments.A.en_stat_sector_fuel <- c( paste0( "Energy statistics",
            " by intermediate sector / intermediate fuel / historical year" ),
        "Units = kt or TJ depending on fuel type" )

# Write diagnostic output
    writeData( DroppedData, domain = "DIAG_OUT",
               fn = "A.en_balance_dropped_data",
               comments = comments.A.en_stat_sector_fuel )

# Write main output
    writeData( A.en_stat_sector_fuel, domain = "MED_OUT",
               fn = "A.en_stat_sector_fuel",
               comments = comments.A.en_stat_sector_fuel )

# Every script should finish with this line:
    logStop()

# END
