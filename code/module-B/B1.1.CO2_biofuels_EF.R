# ----------------------------------------------------------------------------
# Program Name: B1.1.CO2_biofuels_EF.R
# Author's Name: Rachel Hoesly, Patrick O'Rourke
# Date Last Modified: April 25, 2020
# Program Purpose: This file processes CO2 biofuel combustion emissions factors
#                  and removes the fraction of liquid and gas biofuels from EFs
#                  for liquid and gas fuels.
# Input Files: A.IEA_en_stat_ctry_hist.csv, B.CO2_comb_EF_db.csv,
#              IEA_product_fuel.csv, IEA_flow_sector.csv, IEA_energy_balance_factor.csv
# Output Files: B.CO2_comb_EF_db.csv
# Notes:
# TODO: There are other modifications to the IEA data which could be handled here
#       as well to improve default CO2 combustion EFs (see A2.1.IEA_en_bal.R).
#       Alternatively, A2.1.IEA_en_bal.R could output a processed version of IEA data
#       which could be utilized here.
# TODO: Non-OECD countries only have fractions moved beginning in 1971 currently.

# ------------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "analysis_functions.R" ) # Additional function files required.
    log_msg <- paste0( "Removing liquid biofuels from CO2 combustion emissions factors..." ) # First message to be printed to the log
    script_name <- "B1.1.CO2_biofuels_EF.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "CO2"

# Stop script if running for unsupported species
    if ( em %!in% c( 'CO2' ) ) {
        stop (paste( 'Not supported for emission species', em, '...remove from script
                     list in B1.1.base_comb_EF.R and/or makefile...' ) )
    }

# ------------------------------------------------------------------------------
# 1. Read in files

# IEA processed historical flows df
    IEA_energy <- readData( 'MED_OUT', 'A.IEA_en_stat_ctry_hist' )

# Initialized combustion emissions factors df
    CO2_ef <- readData( 'MED_OUT', paste0( "B.", em, "_comb_EF_db" ) )

# IEA mapping files and balance correction factor
    IEA_product_fuel <- readData( "EN_MAPPINGS", "IEA_product_fuel" )
    IEA_flow_sector <- readData( "EN_MAPPINGS", "IEA_flow_sector" )
    IEA_energy_balance_factor <- readData( "ENERGY_IN", "IEA_energy_balance_factor" )

# ------------------------------------------------------------------------------
# 2. Calculate biofuels fraction

# Clean IEA_flow_sector map
    IEA_flow_sector_clean <- IEA_flow_sector %>%
        dplyr::rename( FLOW = flow_code ) %>%
        dplyr::select( FLOW, sector )

# Define CEDS fuels which have at least 1 IEA PRODUCT which is a biofuel (not including
# CEDS fuel "biomass")
  CEDS_fuels_with_biofuels <- IEA_product_fuel %>%
      dplyr::filter( biofuel_flag == 1,
                     fuel != "biomass" )

# Map fuels, sectors, and biofuels notation to IEA energy
#   Remove FLOWS which aren't in CEDS.
#   Remove PRODUCTS which map to biomass (as biomass has a CO2 EF of 0), or
#          which map to a CEDS fuel which has no PRODUCTS flagged as biofuels (this will leave
#          oil and gas CEDS fuels)
    IEA_energy <- IEA_energy %>%
        dplyr::left_join( IEA_product_fuel[, c( "product", "fuel", "biofuel_flag" )],
                          by = c( "PRODUCT" = "product" ) ) %>%
        dplyr::rename( bio_flag = biofuel_flag ) %>%
        dplyr::left_join( IEA_flow_sector_clean, by = "FLOW" ) %>%
        dplyr::select( iso, FLOW, sector, PRODUCT, fuel, bio_flag, X_IEA_years ) %>%
        dplyr::filter( !is.na( sector ),
                       fuel != "biomass",
                       fuel %in% unique( CEDS_fuels_with_biofuels$fuel ) )

# Convert remaining IEA gas PRODUCTS from TJ (gross and net) to kt
  TJ_ng_products <- na.omit( IEA_product_fuel[ grep( "TJ",
                             IEA_product_fuel[ , "product" ] ), c( "product", "fuel" ) ] )
  TJ_ng_products <- unique( TJ_ng_products$product[ grepl( "natural_gas", TJ_ng_products$fuel ) ] )
  TJ_to_kt_natural_gas_list_no_tjnet <- grep( "TJ-net",  TJ_ng_products,
                                               value = TRUE , invert = TRUE )  # The invert selects all IEA_TJ_gas without "TJ-net" units
  TJ_to_kt_natural_gas_list_tjnet <- grep( "TJ-net",  TJ_ng_products, value = TRUE )

  IEA_energy_kt <- IEA_energy %>%
    tidyr::gather( key = years, value = energy_consumption, X_IEA_years ) %>%
    dplyr::mutate( energy_consumption = if_else( PRODUCT %in% TJ_to_kt_natural_gas_list_no_tjnet,
                                                 energy_consumption / conversionFactor_naturalgas_TJ_per_kt_Gross,
                                        if_else( PRODUCT %in% TJ_to_kt_natural_gas_list_tjnet,
                                                 energy_consumption / conversionFactor_naturalgas_TJ_per_kt_Net,
                                                 energy_consumption ) ) ) %>%
    tidyr::spread( years, energy_consumption ) %>%
    dplyr::mutate( units = "kt" ) %>%
    dplyr::select( iso, FLOW, sector, PRODUCT, fuel, bio_flag, units, X_IEA_years )

# Check that there are not liquid IEA products in TJ
  TJ_oil_products <- na.omit( IEA_product_fuel[ grep( "TJ",
                              IEA_product_fuel[ , "product" ] ), c( "product", "fuel" ) ] )
  TJ_oil_products <- TJ_oil_products %>%
      dplyr::filter( fuel %in% c( "diesel_oil", "heavy_oil", "light_oil" ) )

  if( nrow( TJ_oil_products ) > 0 ){

      stop( script_name, " is not configured to handle converting oil based IEA PRODUCTS from TJ to kt. ",
            "Please modify ", script_name, "..." )

  }

# Do balance factor correction (fix negative values to positive values - based on A2.1.IEA_en_bal.R)
  IEA_energy_sign_corrected <- IEA_energy_kt %>%
      dplyr::left_join( IEA_flow_sector[, c( "conversion", "flow_code" ) ], by = c( "FLOW" = "flow_code" ) ) %>%
      dplyr::left_join( IEA_energy_balance_factor, by = c( "FLOW", "PRODUCT" ) ) %>%
      dplyr::mutate( conversion = conversion / abs( conversion ) ,
                     balance_correction = if_else( is.na( balance_correction ), 1,
                                                   as.numeric( balance_correction ) ) ) %>%
      dplyr::mutate_at( .vars = X_IEA_years, .funs = funs( . * conversion * balance_correction ) )

# Aggregate IEA flows by iso, fuel, and sector for biofuels...
  IEA_aggregate_biofuels <- IEA_energy_sign_corrected %>%
      dplyr::filter( bio_flag == 1 ) %>%
      dplyr::select( iso, sector, fuel, X_IEA_years ) %>%
      dplyr::group_by( iso, sector, fuel ) %>%
      dplyr::summarise_all( funs( sum( ., na.rm = TRUE ) ) ) %>%
      dplyr::ungroup( ) %>%
      dplyr::arrange( fuel, sector, iso  )

# ...and for all fuels.
  IEA_aggregate <- IEA_energy_sign_corrected %>%
      dplyr::select( iso, sector, fuel, X_IEA_years ) %>%
      dplyr::group_by( iso, sector, fuel ) %>%
      dplyr::summarise_all( funs( sum( ., na.rm = TRUE ) ) ) %>%
      dplyr::ungroup( ) %>%
      dplyr::arrange( fuel, sector, iso  )

# Calculate fraction of biofuels for each year & country
  IEA_aggregate_biofuels_long <- IEA_aggregate_biofuels %>%
      tidyr::gather( key = year, value = en_cons_bio, X_IEA_years )

  IEA_aggregate_long <- IEA_aggregate %>%
      tidyr::gather( key = year, value = en_cons_total, X_IEA_years )

  IEA_biofuels_fraction <- IEA_aggregate_biofuels_long %>%
      dplyr::left_join( IEA_aggregate_long, by = c( "iso", "sector", "fuel", "year" ) ) %>%
      dplyr::mutate( fraction = en_cons_bio / en_cons_total ) %>%
      dplyr::select( -en_cons_bio, -en_cons_total ) %>%
      tidyr::spread( year, fraction )

# Fix biomass fraction
#   Remove rows which are all NA or all NaN
#   Replace NA and NaN values with 0
  IEA_biofuels_fraction_fixed <- IEA_biofuels_fraction %>%
      dplyr::filter_at( .vars = X_IEA_years, any_vars( !is.invalid( . ) ) ) # is.invalid indicates if NULL, NA, or NaN

  IEA_biofuels_fraction_fixed <-
      replace( IEA_biofuels_fraction_fixed, is.na( IEA_biofuels_fraction_fixed ), 0 )

# Extend forward to final CEDS year, if necessary
  if( last( IEA_years ) < last( emissions_years ) ){

      X_extension_years <- X_emissions_years[ X_emissions_years %!in% X_IEA_years ]

      IEA_biofuels_fraction_fixed <- IEA_biofuels_fraction_fixed %>%
          dplyr::mutate_at( .vars = X_extension_years, .funs = funs( identity ( !!rlang::sym( X_IEA_end_year ) ) ) )

  }

# ------------------------------------------------------------------------------
# 3. Remove biofuels fraction

# Calculate final CO2 emissions factors
    final_ef <- CO2_ef[ , c( "iso", "sector", "fuel", "units" ) ]

    multiplier <- CO2_ef %>%
      dplyr::select( iso, sector, fuel, units ) %>%
      dplyr::left_join( IEA_biofuels_fraction_fixed, by = c( "iso", "sector", "fuel" ) )

    multiplier <- replace( multiplier, is.na( multiplier ), 0 )

    final_ef[ X_emissions_years ] <-
        CO2_ef[ X_emissions_years ] * ( 1 - multiplier[ X_emissions_years ] )

# -----------------------------------------------------------------------------
# 5. Output

# Add comments for each table
    comment.final_ef <- paste0( 'Base CO2 combustion EFs taking into account',
                               ' the fraction of liquid and gas fuels that',
                               ' are from biofuels' )

    writeData( final_ef, "MED_OUT", paste0( "B.", em, "_comb_EF_db" ),
               comments = comment.final_ef)

# Every script should finish with this line:
    logStop()
