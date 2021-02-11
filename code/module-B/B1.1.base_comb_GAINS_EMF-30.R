# ---------------------------------------------------------------------------
# Program Name: B1.1.base_comb_GAINS_EMF-30.R
# Author: Rachel Hoesly, Linh Vu, Patrick O'Rourke
# Date Last Updated: July 15, 2020
# Program Purpose: Generate base combustion emission factors from global GAINS EMF-30 data.
#                  CO2 are just used as diagnostics, while SO2, BC, and OC are used for
#                  recent year control percents.
# Input Files: Global by region-detail_emf30_[activity/em]_noSE.csv
#              OECD_and_NonOECD_Conversion_Factors_Full.csv, IEA_product_fuel.csv,
#              emf-30_ctry_map.csv, emf-30_fuel_sector_map.csv
# Output Files: B1.2.energy_conversion_factors.csv, B1.1.Europe_heat_content_IEA.csv,
#               B.[em]_NC_EF_GAINS_EMF30.csv, B.[em]_comb_EF_GAINS_EMF30.csv,
#               B.[em]_GAINS_EF_ratios.csv
# Notes: EFS have constant extension backwards and forwards within mutual years
#        for GAINS and CEDS
# TODO: (Future:) For SO2, create post 2010 S control trends instead of EFs
#       (only writes to diagnostic now).
# TODO: (Future:) Use all conversion years, instead of just 2010
# TODO: (Future:) Heat content should be fuel consumption weighted by GAINS region and product.
#                 When this is done, we can then use the IEA_product_fuel.csv map instead of
#                 hard coding the IEA products in here.
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'process_db_functions.R', 'data_functions.R',
                  'interpolation_extension_functions.R', 'common_data.R',
                  'analysis_functions.R' ) # Additional function files may be required.

    log_msg <- "Processing GAINS EMF data in order to create default combustion emissions..." # First message to be printed to the log
    script_name <- 'B1.1.base_comb_GAINS_EMF-30.R'

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "CH4"

# Stop script if running for unsupported species
    if ( em %!in% c( "BC", "CH4", "CO", "CO2", "NH3", "NMVOC", "NOx", "OC", "SO2" ) ) {

      stop ( paste( 'GAINS EMF is not supported for emission species', em,
                    'remove from script list in B1.1.base_comb_EF.R',
                    'and/or makefile...' ) )

    }

# ---------------------------------------------------------------------------
# 0.5. Load Packages and Define functions

# Define heat_contents function used in section 2. This function returns the
# average heat content of an IEA fuel from the column in a conversion matrix
    calc_heat_content <- function( conversion, IEAfuel ) {

        conversion <- conversion[ , IEAfuel ]

        unit_multiplier <- sum( colSums( conversion, na.rm = T ) ) /
                           length( na.omit( unlist( conversion ) ) )

        return( unit_multiplier )

    }

# Define function used in section 4. For each CEDS fuel, this function converts
# energy to kg fuel based on the given heat content, for fuels in kJ.
# Fuel mass in kt = fuel energy * 10^-6 / heat content (kJ/kg)
    convert_to_kg_of_fuel <- function( df_in, heat_content_df, fuel_use ){

      df_in[ df_in$fuel %in% fuel_use & df_in$units == "kJ", X_GAINS_years ] <-
        ( df_in[ df_in$fuel %in% fuel_use & df_in$units == "kJ", X_GAINS_years ] /
            heat_content_df[ which( heat_content_df$fuel == fuel_use ),
                             'heat_content' ] ) * 10^-6

      df_out <- df_in

      return( df_out )

    }

# ---------------------------------------------------------------------------
# 1. Load Data and define other useful script constants

# Define em string to use for GAINS data
    em_use <- em
    if ( em == "NMVOC" ){ em_use <- "VOC" }

# Define other settings for GAINS data
  domain_use <- "EM_INV"
  domain_ext_use <- "GAINS/"

  emissions_file_name <- paste0( "Global by region-detail_emf30_", em_use, "_wSE" )
  if( em == "SO2" ){ emissions_file_name <- gsub( "_wSE", "_v2_wSE", emissions_file_name ) }

  activity_file_Name <-  paste0( "Global by region-detail_emf30_activity_wSE" )

  activity_rows_to_skip <- 7
  emissions_rows_to_skip <- 9

# Read in GAINS data
    GAINS_emissions <- readData( domain = domain_use, domain_extension = domain_ext_use,
                                 file_name = emissions_file_name, skip = emissions_rows_to_skip )

    GAINS_activities <- readData( domain = domain_use, domain_extension = domain_ext_use,
                                 file_name = activity_file_Name, skip = activity_rows_to_skip )

# Read in GAINS mapping files
    GAINS_ctry_map <- readData( domain = 'MAPPINGS', domain_extension = domain_ext_use,
                                file_name ='emf-30_ctry_map' )

    GAINS_fuel_sector_map <- readData( domain = 'MAPPINGS', domain_extension = domain_ext_use,
                                       file_name = 'emf-30_fuel_sector_map' )

# Read in IEA heat content conversion factors for OECD and non-OECD countries
    OECD_and_NonOECD_Conversion <- readData( "ENERGY_IN", "OECD_and_NonOECD_Conversion_Factors_Full" )

# Read in IEA product to CEDS fuel map
    IEA_product_fuel_map <- readData( "EN_MAPPINGS", "IEA_product_fuel" )

# Define the year of IEA World Conversion Factors data to use
    CONVERSION_YEAR_TO_USE <- "X2010"

# ---------------------------------------------------------------------------
# 2. Calculate GAINS heat content
#    TODO: (Future:) Here heat content should be fuel consumption weighted by GAINS region
#    The IEA data provides conversions (heat content) for a variety of flows and fuels
#    and for most countries. Take only EU countries and develop a conversion factor
#    by taking a weighted average of fuels brought together into CEDS fuels.
#    units of heat_content - kJ/kg

    printLog( 'Calculating Gains heat content...' )

# Just European Countries in Gains data.
    EU <- c( "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
             "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
             "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
             "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovak Republic",
             "Slovenia", "Spain", "Sweden", "United Kingdom" )

    Flows <- c( "NCV of imports", "Average net calorific value" )

# Initial reformatting of GAINS data
    OECD_and_NonOECD_Conversion <- OECD_and_NonOECD_Conversion %>%
        dplyr::select( COUNTRY, FLOW, PRODUCT, all_of(CONVERSION_YEAR_TO_USE) ) %>%
        tidyr::spread( PRODUCT, CONVERSION_YEAR_TO_USE )

# Subset only EU countries and only specific flows
    conversion <- OECD_and_NonOECD_Conversion[ OECD_and_NonOECD_Conversion$COUNTRY %in% EU &
                                   OECD_and_NonOECD_Conversion$FLOW %in% Flows, ]

# Convert conversion factors to numeric values (non-numeric and 0 placeholders become NA)
#   This data is output to the diagnostic-output directory
    conversion[ , 3:ncol( conversion ) ] <-
        suppressWarnings( lapply( conversion[ , 3:ncol( conversion ) ],
                                  as.numeric ) )

    all_conversion_columns <- colnames( conversion )
    product_columns <- subset( all_conversion_columns, all_conversion_columns %!in% c( "FLOW", "COUNTRY" ) )
    conversion_fixed <- conversion %>%
        dplyr::mutate_at( .vars = product_columns, .funs = funs( if_else( . == 0, NA_real_, . ) ) )

# Using the function defined in Section 0.5 and IEA fuels to create a unit
# conversion multiplier for the CEDS fuel equivalents in the GAINS data.
# TODO: (Future:) Here heat content should fuel consumption weighted by GAINS region and PRODUCT
# TODO: (Future:) We can use the IEA product fuel mapping file once we are weighting by GAINS region and PRODUCT

#   Note: "Brown coal (if no  other detail) (kt)" is not included below,
#         as values for this IEA product are all NA or 0 during GAINS years
#         in IEA v2019.
    browncoal <- c( "Sub-bituminous coal", "Lignite" ) # TODO: (Future:)  Missing: "Peat (kt)", "Peat products (kt)"
                                                       # TODO: (Future:)  "Sub-bituminous coal" CEDS hard_coal?

    mult_browncoal <- calc_heat_content( conversion_fixed, browncoal )

#   Note: "Hard coal (if no  other detail) (kt)" is not included below,
#         as values for this IEA product are all NA or 0 during GAINS years
#         in IEA v2019.
    hardcoal <- c( "Anthracite", "Other bituminous coal" ) # TODO: (Future:) Missing: "Coking coal (kt)", "Sub-bituminous coal (kt)",
                                                           #                          "Patent fuel (kt)", "Gas coke (kt)",
                                                           #                          "Coal tar (kt)", "BKB (kt)"

    mult_hardcoal <- calc_heat_content( conversion_fixed, hardcoal )

    coalcoke <- c( "Coke oven coke", "Coking coal" )

    mult_coalcoke <- calc_heat_content( conversion_fixed, coalcoke )

    lightoil <- c( "Motor gasoline excl. biofuels", "Aviation gasoline", "Gasoline type jet fuel",
                   "Kerosene type jet fuel excl. biofuels", "Other kerosene" ) # TODO: (Future:) Missing: "Refinery feedstocks (kt)", "Additives/blending components (kt)",
                                                                               #                          "Other hydrocarbons (kt)", "Ethane (kt)",
                                                                               #                          "Liquefied petroleum gases (LPG) (kt)", "Bio jet kerosene (kt)",
                                                                               #                          "Naphtha (kt)", "White spirit & SBP (kt)", "Biogasoline (kt)"
                                                                               #                          "Other liquid biofuels (kt)"
    mult_lightoil <- calc_heat_content( conversion_fixed, lightoil )

    heavy_oil <- c( "Crude oil", "Fuel oil" ) # TODO: (Future:) Missing: "Oil shale and oil sands (kt)", "Crude/NGL/feedstocks (if no detail) (kt)",
                                              #                          "Bitumen (kt)", "Paraffin waxes (kt)", "Petroleum coke (kt)",
                                              #                          "Other oil products (kt)"
    mult_heavy_oil <- calc_heat_content( conversion_fixed, heavy_oil )

# IEA does not have conversion factors for biomass, so assume value for wood from GAINS fuel wood value in common_data.R
    KJ_per_MJ <- 1000
    mult_biomass <- GAINS_conversionFactor_fuelwood_MJ_per_kg * KJ_per_MJ

# Use default natural gas covnersion factor as well
# GAINS is based on IEA energy statistics and, therefore, units are TJ-net
    mult_NaturalGas <- conversionFactor_naturalgas_TJ_per_kt_Net * 1000 # NG heat content in kJ/kg

# These have a single column of data and therefore the function is not necessary;
#   Execute a single sum
#   TODO: (Future:) We can use the IEA product fuel mapping file one we weight by PRODUCT and GAINS regions.
    Diesel <- c( "Gas/diesel oil excl. biofuels" ) # TODO: (Future:) Missing: "Biodiesels (kt)"
    mult_Diesel <- sum( conversion_fixed[ , Diesel ], na.rm = T ) /
                   length( na.omit( conversion_fixed[ , Diesel ] ) )

# Combine all calculated heat contents into a single dataframe
# This data is output to the diagnostic-output directory
    GAINS_heat_content <- data.frame( fuel = c( 'brown_coal', 'hard_coal',     # TODO (Future) This could be taken from IEA_prod_fuel
                                                'coal_coke', 'biomass',
                                                'light_oil', 'natural_gas',
                                                'diesel_oil', 'heavy_oil' ),
                                     heat_content = c( mult_browncoal,
                                                       mult_hardcoal,
                                                       mult_coalcoke,
                                                       mult_biomass,
                                                       mult_lightoil,
                                                       mult_NaturalGas,
                                                       mult_Diesel,
                                                       mult_heavy_oil ) )
    GAINS_heat_content$units <- "kJ/kg"

# ---------------------------------------------------------------------------
# 3. Map activity and emissions to CEDS sectors and fuels.

    printLog( "Mapping and aggregating GAINS data to CEDS fuels and sectors..." )

# Select years and fix column names
    GAINS_emissions <- GAINS_emissions %>%
      dplyr::rename( Sector = 'EMF30.kt' ) %>%
      dplyr::select( Region, Sector, all_of(X_GAINS_years) ) %>%
      # GAINS data can have tabs in front of all characters in the ID columns, which would need to be removed
      dplyr::mutate_at( .vars = c( "Region", "Sector" ), .funs = funs( gsub( "\t", "", . ) ) )

    GAINS_activities <- GAINS_activities %>%
      dplyr::rename( Sector = "EMF30.sector",
                     Unit = "EMF30.unit" ) %>%
      dplyr::select( Region, Sector, Unit, all_of(X_GAINS_years) ) %>%
      # GAINS activity data has tabs in front of all characters in the ID columns, which must be removed
      dplyr::mutate_at( .vars = c( "Region", "Sector", "Unit" ), .funs = funs( gsub( "\t", "", . ) ) )

# Map to CEDS isos
    emissions_ceds <- GAINS_emissions %>%
      dplyr::left_join( GAINS_ctry_map, by = "Region" )

# Check that GAINS sectors which should be all NA have no values for GAINS emissions data
  GAINS_fuel_sector_map_no_val_secs <- GAINS_fuel_sector_map %>%
    dplyr::filter( Notes == "not mapped as has no values" )

  GAINS_fuel_sector_map_no_val_secs <- sort( unique( GAINS_fuel_sector_map_no_val_secs$emf_sector ) )

  emissions_ceds_na_val_secs <- emissions_ceds %>%
    dplyr::filter( Sector %in% GAINS_fuel_sector_map_no_val_secs ) %>%
    dplyr::filter_at( X_GAINS_years, any_vars( !is.na( . ) ) ) # Filter for rows with at least 1 non-NA value

  if( nrow( emissions_ceds_na_val_secs ) > 1 ){

    printLog( paste( GAINS_fuel_sector_map_no_val_secs, sep = ", " ) )

    stop( paste0( "GAINS emissions data has non-NA values for sectors which are specified in ",
                  "the sector fuel mapping file to have only NA-values. Please fix mapping file ",
                  "for the above sectors..." ) )

  }

# Map emissions to (and aggregate by) CEDS sector and fuel
    emissions_ceds <- emissions_ceds %>%
      # Drop rows which are all NAs before mapping to CEDS isos, sectors, and fuels and then aggregating,
      # so rows of all zeroes don't appear
      dplyr::filter_at( X_GAINS_years, any_vars( !is.na( . ) ) ) %>%
      dplyr::left_join( GAINS_fuel_sector_map, by = c( "Sector" = "emf_sector" ) ) %>%
      dplyr::rename( sector = ceds_sector, fuel = ceds_fuel ) %>%
      dplyr::filter( !is.na( iso ), !is.na( sector ), !is.na( fuel ) ) %>%
      dplyr::select( sector, fuel, Region, country_name, iso, emf_number, all_of(X_GAINS_years) ) %>%
      dplyr::group_by( sector, fuel, Region, country_name, iso, emf_number ) %>%
      # Summarize only if all values aren't NA, if NA return NA. This ensures that if all values are NA
      # the value 0 isn't returned
      dplyr::summarise_all( funs( . = if_else( all( is.na( . ) ), NA_real_, sum( ., na.rm = TRUE ) ) ) ) %>%
      dplyr::ungroup( ) %>%
      dplyr::rename_at( .vars = vars( contains( "_." ) ),
                        .funs = funs( sub( "_.", "", . ) ) )

    emissions_ceds <- emissions_ceds %>%
      dplyr::mutate( units = "kt" ) %>% # GAINS emissions data is already in kt (of the given em)
      dplyr::select( iso, sector, fuel, units, fuel, all_of(X_GAINS_years) ) %>% # Trim columns
      dplyr::distinct( ) # Remove duplicatations that arise when multiple country_names
                         # map to 1 CEDS iso (e.g. USA & United States Minor Outlying Islands
                         # both map to CEDS USA iso)

# CO2 data from GAINS is reported in Mt rather than kt, so multiply CO2 by 1000 to convert Mt to kt
  if( em == 'CO2' ){

      printLog( "Converting GAINS CO2 data from Mt to kt (only CO2 is provided in Mt from GAINS..." )

      emissions_ceds <- emissions_ceds %>%
        dplyr::mutate_at( .vars = X_GAINS_years, .funs = funs( . * 1000 ) )

  }

# Map CEDS countries and regions to activities
    activities_ceds <- GAINS_activities %>%
      dplyr::left_join( GAINS_ctry_map, by = "Region" )

# Check that GAINS sectors which should be all NA have no values for GAINS activity data
    activities_ceds_na_val_secs <- activities_ceds %>%
      dplyr::filter( Sector %in% GAINS_fuel_sector_map_no_val_secs ) %>%
      dplyr::filter_at( X_GAINS_years, any_vars( !is.na( . ) ) ) # Filter for rows with at least 1 non-NA value

    if( nrow( activities_ceds_na_val_secs ) > 1 ){

      printLog( paste( GAINS_fuel_sector_map_no_val_secs, sep = ", " ) )

      stop( paste0( "GAINS activity data has non-NA values for sectors which are specified in ",
                    "the sector fuel mapping file to have only NA-values. Please fix mapping file ",
                    "for the above sectors..." ) )

    }

# Map activities to (and aggregate by) CEDS sector and fuel
    activities_ceds <- activities_ceds %>%
      # Drop rows which are all NAs before mapping to CEDS sectors and fuels and aggregating,
      # so rows of all zeroes don't appear
      dplyr::filter_at( X_GAINS_years, any_vars( !is.na( . ) ) ) %>%
      dplyr::left_join( GAINS_fuel_sector_map, by = c( "Sector" = "emf_sector" ) ) %>%
      dplyr::rename( sector = ceds_sector, fuel = ceds_fuel ) %>%
      dplyr::filter( !is.na( iso ), !is.na( sector ), !is.na( fuel ) ) %>%
      dplyr::select( sector, fuel, Region, Unit, country_name, iso, emf_number, all_of(X_GAINS_years) ) %>%
      dplyr::group_by( sector, fuel, Region, Unit, country_name, iso, emf_number ) %>%
      # Summarize only if all values aren't NA, if NA return NA. This ensures that if all values are NA
      # the value 0 isn't returned
      dplyr::summarise_all( funs( . = if_else( all( is.na( . ) ), NA_real_, sum( ., na.rm = TRUE ) ) ) ) %>%
      dplyr::ungroup( ) %>%
      dplyr::rename_at( .vars = vars( contains( "_." ) ),
                        .funs = funs( sub( "_.", "", . ) ) )

    activities_ceds <- activities_ceds %>%
      dplyr::rename( units = Unit ) %>%
      dplyr::select( iso, sector, fuel, units, fuel, all_of(X_GAINS_years) ) %>% # Trim columns
      dplyr::distinct( ) # Remove duplicatations that arise when multiple country_names
                         # map to 1 CEDS iso (e.g. USA & United States Minor Outlying Islands
                         # both map to CEDS USA iso)

# ---------------------------------------------------------------------------
# 4. Convert energy units to mass units (Activity)

    printLog( "Converting energy units to mass units..." )

# Check if any activity data has units 'bln m3'. This should only occur for GAINS
# petr. and gas 'losses' sectors, which are treated as process emissionsin CEDS. Currently
# the GAINS activity data is all NA for petr. and gas 'losses', so add a stop in
# case that changes (if data is no longer NA for these sectors, it will exist in
# the data at this point and need converting for CEDS diagnostic of GAINS process emission
# EFs.

    if( any( activities_ceds$units == "bln m3" ) ){

      stop( script_name, " has not been configured to convert GAINS activity data from ",
            "-bln m3- to -kJ-." )

    }

# Convert PetaJoules into kiloJoules
    activities_ceds <- activities_ceds %>%
      dplyr::mutate_at( .vars = X_GAINS_years, .funs = funs(
                        if_else( units == "PJ", . * 10^12, .  ) ) ) %>%
      dplyr::mutate( units = if_else( units == "PJ", "kJ", units ) )

# For each CEDS fuel, convert energy to kg fuel based on the given heat content.
# fuel mass in kt = fuel energy * 10^-6 / heat content (kJ/kg)

#   biomass convert
    activities_ceds <- convert_to_kg_of_fuel( activities_ceds, GAINS_heat_content, "biomass" )

#   brown_coal convert
    activities_ceds <- convert_to_kg_of_fuel( activities_ceds, GAINS_heat_content, "brown_coal" )

#   hard_coal convert
    activities_ceds <- convert_to_kg_of_fuel( activities_ceds, GAINS_heat_content, "hard_coal" )

#   coal_coke convert
    activities_ceds <- convert_to_kg_of_fuel( activities_ceds, GAINS_heat_content, "coal_coke" )

#   diesel_oil convert
    activities_ceds <- convert_to_kg_of_fuel( activities_ceds, GAINS_heat_content, "diesel_oil" )

#   heavy_oil convert
    activities_ceds <- convert_to_kg_of_fuel( activities_ceds, GAINS_heat_content, "heavy_oil" )

#   light_oil convert
    activities_ceds <- convert_to_kg_of_fuel( activities_ceds, GAINS_heat_content, "light_oil" )

#   natural_gas convert
    activities_ceds <- convert_to_kg_of_fuel( activities_ceds, GAINS_heat_content, "natural_gas" )

#   Label units as kg for values which were converted from kj
    activities_ceds[ which( activities_ceds$units == 'kJ' ), 'units' ] <- 'kt'

# Convert mass units to kt
    activities_ceds[ which( activities_ceds$units == 'Mt' ), X_GAINS_years ] <-
           activities_ceds[ which( activities_ceds$units == 'Mt' ), X_GAINS_years ] *
           10^3
    activities_ceds[ which( activities_ceds$units == 'Mt' ), 'units' ] <- 'kt'

# ---------------------------------------------------------------------------
# 5. Interpolate Data through current year

# Aggregate activities and emissions to CEDS sectors and fuels
# TODO: PR Comment: This only really necessary once the conversion from 'bln m3' to 'PJ'
#       is made (and when there is activity data for losses)
    activities_ceds <- activities_ceds %>%
      dplyr::group_by( iso, sector, fuel, units ) %>%
    # Summarize only if all values aren't NA, if NA return NA. This ensures that
    # if all values are NA the value 0 isn't returned
      dplyr::summarise_all( funs( . = if_else( all( is.na( . ) ), NA_real_, sum( ., na.rm = TRUE ) ) ) ) %>%
      dplyr::ungroup( ) %>%
      dplyr::rename_at( .vars = vars( contains( "_." ) ),
                        .funs = funs( sub( "_.", "", . ) ) )

    emissions_ceds <- emissions_ceds %>%
      dplyr::group_by( iso, sector, fuel, units ) %>%
      # Summarize only if all values aren't NA, if NA return NA. This ensures that if all values are NA
      # the value 0 isn't returned
      dplyr::summarise_all( funs( . = if_else( all( is.na( . ) ), NA_real_, sum( ., na.rm = TRUE ) ) ) ) %>%
      dplyr::ungroup( ) %>%
      dplyr::rename_at( .vars = vars( contains( "_." ) ),
                        .funs = funs( sub( "_.", "", . ) ) )

# Interpolate annual values
    printLog( "Interpolating GAINS emission and activity data..." )

    activities_ceds <- as.data.frame( activities_ceds )
    activities_ceds_interp <- interpolateValues( activities_ceds )

    emissions_ceds <- as.data.frame( emissions_ceds )

    # Replace zeros with Inf so that the interpolateValues function in Step 6 can
    # interpolate between GAINS years, as opposed to CEDS years. The interpolateValues
    # function below will replace the values between the GAINS years with Inf/NaN,
    # essentially flagging them so that they are interpolated correctly later.
    # For instance, the values between 2006 and 2014 will be replaced with Inf/NaN,
    # and will then be interpolated using GAINS data from 2005 and 2015.
    emissions_ceds <- replace( emissions_ceds, emissions_ceds == 0, Inf )
    emissions_ceds_interp <- interpolateValues( emissions_ceds )

# Create some Xyear lists
    ceds_years <- GAINS_start_year : end_year
    X_ceds_years <- paste0( 'X', ceds_years )

    emf_years <- GAINS_start_year : GAINS_end_year
    X_emf_years <- paste0( 'X', emf_years )

# ---------------------------------------------------------------------------
# 6. Calculate Emission Factors

    printLog( 'Calculating emission factors...' )

# Merge the activity and emissions data frames
    combined <- merge( activities_ceds_interp,
                       emissions_ceds_interp,
                       by = c( 'iso', 'sector', 'fuel' ),
                       suffixes = c( ".a", ".e" ), all = T )

# Calculate EFs by dividing emissions years by activity years
    combined[ X_emf_years ] <- combined[ paste0( X_emf_years, '.e' ) ] /
                               combined[ paste0( X_emf_years, '.a' ) ]

# Update units based on emissions and activity units
    combined$units <- paste0( combined$units.e, '/', combined$units.a )

# Check for non-numbers and infinite values and replace with NA
    combined <- replace( combined, combined == Inf, NA )
    combined <- replace( combined, is.na( combined ), NA )

# Keep only rows which aren't completely NA for all years
    combined <- removeNARows( combined, X_emf_years ) %>%
        dplyr::select( iso, sector, fuel, units, all_of(X_emf_years) )

# Interpolate internal NAs
    combined_interp <- interpolateValues( combined )

# Extend emissions forwards and backwards in time (constant extension)
# Without extension many EFS would be NA (missing activity data,
#                   or missing emissions data (or both) for a given year ).
    gainsEMF30_all <- extend_and_interpolate( combined_interp, X_emf_years )

# Retain GAINS years which are in CEDS years
    gainsEMF30_all <- gainsEMF30_all[ , c( 'iso', 'sector', 'fuel',
                                     'units', X_ceds_years ) ]

# ---------------------------------------------------------------------------
# 7. Correct Emission Factors

# Remove coal_coke if SO2 emissions. EMF contains only coal data, and while coal coke
# has similar EF's as other coals for other emission species, coal coke EFs differ from other
# coal types for SO2.
    if ( em == 'SO2' ) {

        gainsEMF30_all <- gainsEMF30_all[ -which( gainsEMF30_all$fuel == 'coal_coke' ), ]

    }

# Brown coal has lower heat content so ends up having a higher default EF when converting
# kg/PJ to kg/kg. To correct, give brown coal EF the same value as hard coal for most
# emissions, and 70% hard coal value for NOx.
#
# This is because brown coal has a lower NOx EF per unit weight than hard coal.
# The ratio ranges from 0.6 for pulverized coal units to 0.8 for Spreader stokers
# (Source: US EPA, AP-42), so we use 0.7 average value

# Prepare brown coal raw data
    gains_brown_coal <- dplyr::filter( gainsEMF30_all, fuel == "brown_coal" ) %>%
                        dplyr::arrange( iso, sector )

# Prepare hard coal raw data
    gains_hard_coal <- dplyr::filter( gainsEMF30_all, fuel == "hard_coal" ) %>%
                       dplyr::arrange( iso, sector )

# Replace brown coal EF with hard coal EF
    gains_brown_coal[ , X_ceds_years ] <- gains_hard_coal[ , X_ceds_years ]

# Only execute the special additional change if em is NOx
    if ( em == "NOx" ){ gains_brown_coal[, X_ceds_years ] <-
                           0.7 * gains_brown_coal[ , X_ceds_years ] }

# Replace new brown coal values into main EF dataframe
    gainsEMF30_all <- dplyr::filter( gainsEMF30_all, fuel != "brown_coal" ) %>%
                      dplyr::bind_rows( gains_brown_coal ) %>%
                      dplyr::arrange( iso, sector, fuel )

# Separate process and combustion
    process_gainsEMF30 <- gainsEMF30_all[ which( gainsEMF30_all$fuel ==
                                                   'process' ), ]
    gainsEMF30_comb <- gainsEMF30_all[ gainsEMF30_all$fuel %!in% 'process', ]

# For any given iso-sector combination, replace low values (< 0.001 * std. dev) with
# median value of that year. The low value threshold was determined by trial and error
# and captured the unrealistically low EFs for Asia-stan countries.
    gainsEMF30_comb <- gainsEMF30_comb %>%
        group_by(iso, sector) %>%
        mutate_at(vars(starts_with("X")), funs(if_else(length(.)>1 & .<0.001*sd(.),NA_real_ ,.))) %>%
        mutate_at(vars(starts_with("X")), funs(if_else(is.na(.),median(., na.rm = TRUE),.))) %>%
        ungroup()

# ---------------------------------------------------------------------------
# 8. Diagnostics

    gains_diagnostics <- gainsEMF30_all

# Ratio of the emissions factors in the last CEDS year to the EF in the last
# EDGAR inventory year (currently 2015 for all ems other than CO2, which has an
# end year of 2018 - as of 6/12/20)
    gains_diagnostics$ratio <- gains_diagnostics[[X_end_year]] /
                               gains_diagnostics[ , paste0( "X", EDGAR_end_year ) ]

# Only write output for extreme ratios (greater than 1.5, less than 0.5)
    gains_diagnostics <- gains_diagnostics[ which( gains_diagnostics$ratio > 1.5 |
                                                   gains_diagnostics$ratio < .5 ), ]

# ---------------------------------------------------------------------------
# 9. Output

# Write the IEA conversion data to diagnostic output
    writeData( conversion_fixed, domain = "DIAG_OUT",
               fn = "B1.2.energy_conversion_factors" )

# Write the process EF data to diagnostic output
# TODO: (Future:) There are duplicated iso + sector + fuel here for CH4. This problem existed in old CEDS version
#       using IEA_v2015. This output doesn't appear to be used anywhere, so can be addressed later.
    writeData( process_gainsEMF30, domain = "DIAG_OUT",
               fn = paste0( 'B.', em, '_NC_EF_GAINS_EMF30' ) )

# Write data of all heat contents to intermediate output
    writeData( GAINS_heat_content, domain = "MED_OUT", fn = "B1.1.Europe_heat_content_IEA" )

# Write extreme ratios of EFs in last CEDS year to EF in last inventory year
    writeData( gains_diagnostics, domain = "DIAG_OUT", fn = paste0( "B.", em, "_GAINS_EF_ratios" ) )

# For CO2 don't use global GAINS data, but write to diagnostic.
# For other species, write to intermediate-output to be used for base combustion emission factors
# ( BC, OC, and SO2 use GAINS data for recent control percents, and thus are also output to
# the intermediate-output directory)
    if ( em %in% c( 'CO2' ) ) {

        writeData( gainsEMF30_comb, domain = "DIAG_OUT",
                   fn = paste0( 'B.', em, '_comb_EF_GAINS_EMF30' ) )

    } else {

        writeData( gainsEMF30_comb, domain = "MED_OUT",
                   fn = paste0( 'B.', em, '_comb_EF_GAINS_EMF30' ) )

    }

    logStop()

# END
