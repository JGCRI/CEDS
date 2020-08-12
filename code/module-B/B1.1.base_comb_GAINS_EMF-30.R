# ---------------------------------------------------------------------------
# Program Name: B1.1.base_comb_GAINS_EMF-30.R
# Author: Rachel Hoesly, Linh Vu, Patrick O'Rourke
# Date Last Updated: June 12, 2020
# Program Purpose: Generate base emission factors from global GAINS EMF-30 data.
#                  SO2, BC, OC, CO2 are just used as diagnostics.
# Input Files: GAINS_EMF30_EMISSIONS_extended_Ev5a_CLE_Nov2015.xlsx,
#              GAINS_EMF30_ACTIVITIES_extended_Ev5a_Nov2015.xlsx,
#              OECD_and_NonOECD_Conversion_Factors_Full.csv, IEA_product_fuel.csv,
#              emf-30_ctry_map.csv, emf-30_fuel_sector_map.csv
# Output Files: B1.2.energy_conversion_factors.csv, B1.1.Europe_heat_content_IEA.csv,
#               B.[em]_NC_EF_GAINS_EMF30.csv, B.[em]_comb_EF_GAINS_EMF30.csv,
#               B.[em]_GAINS_EF_ratios.csv
# Notes: transportation_rail only hase a 2020 values, so interpolated values are
#           constant extended back from 2020 to 2011
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

    log_msg <- "Processing GAINS EMF-30 data" # First message to be printed to the log
    script_name <- 'B1.1.base_comb_GAINS_EMF-30.R'

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "CH4"

# Stop script if running for unsupported species
    if ( em %!in% c('SO2','NOx','NMVOC','BC','OC','CH4','CO','CO2') ) {
      stop ( paste( 'GAINS EMF-30 is not supported for emission species', em,
                    'remove from script list in B1.2.add_comb_EF.R',
                    'and/or makefile' ) )
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
# energy to kg fuel based on the given heat content.
# fuel mass in kt = fuel energy * 10^-6 / heat content (kJ/kg)
    convert_to_kg_of_fuel <- function( df_in, heat_content_df, fuel_use ){

        df_in[ df_in$fuel %in% fuel_use, X_GAINS_years ] <-
            ( df_in[ df_in$fuel %in% fuel_use, X_GAINS_years ] /
                  heat_content_df[ which( heat_content_df$fuel == fuel_use ),
                                   'heat_content' ] ) * 10^-6

        df_out <- df_in

        return( df_out )

    }

# ---------------------------------------------------------------------------
# 1. Load Data and define other useful script constants
# Define activities sheet
    a.sheet <- 'Air pollutants'
    if ( em %in% c( 'CH4', "CO2" ) ){ a.sheet <- tolower( em ) }

# Define emissions sheet
    e.sheet <- em
    if ( em == 'NMVOC' ){ e.sheet <- 'VOC' }

# Read in GAINS data
    GAINS_emissions <- readData( domain = 'EM_INV', domain_extension = 'GAINS/',
                           file_name = 'GAINS_EMF30_EMISSIONS_extended_Ev5a_CLE_Nov2015',
                           ".xlsx", sheet_selection = e.sheet )
    GAINS_activities <- readData( domain = 'EM_INV', domain_extension = 'GAINS/',
                             file_name ='GAINS_EMF30_ACTIVITIES_extended_Ev5a_Nov2015',
                             ".xlsx", sheet_selection = a.sheet )

# Read in mapping files
    GAINS_ctry_map <- readData( domain = 'MAPPINGS', domain_extension = 'GAINS/',
                          file_name ='emf-30_ctry_map' )
    GAINS_fuel_sector_map <- readData( domain = 'MAPPINGS', domain_extension = 'GAINS/',
                                 file_name = 'emf-30_fuel_sector_map' )

# Read in heat content conversion factors for OECD and non-OECD countries
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

    printLog( 'Calculating Gains Heat Content' )

# Just European Countries in Gains data.
    EU <- c( "Czech Republic", "Denmark", "Estonia", "Finland", "France",
             "Germany", "Greece", "Hungary", "Ireland", "Italy", "Luxembourg",
             "Netherlands", "Poland", "Portugal", "Slovak Republic", "Slovenia",
             "Spain", "Sweden", "United Kingdom", "Belgium", "Latvia",
             "Lithuania", "Austria", "Romania", "Croatia", "Bulgaria",
             "Malta", "Cyprus" )
    Flows <- c( "NCV of imports", "Average net calorific value" )

# Initial reformatting of GAINS data
    OECD_and_NonOECD_Conversion <- OECD_and_NonOECD_Conversion %>%
        dplyr::select( COUNTRY, FLOW, PRODUCT, CONVERSION_YEAR_TO_USE ) %>%
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

# IEA does not have covnersion factors for biomass, so assume value for wood from GAINS fuel wood value in common_data.R
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
                                                'biomass', 'light_oil',
                                                'natural_gas', 'diesel_oil',
                                                'heavy_oil' ),
                                     heat_content = c( mult_browncoal,
                                                       mult_hardcoal,
                                                       mult_biomass,
                                                       mult_lightoil,
                                                       mult_NaturalGas,
                                                       mult_Diesel,
                                                       mult_heavy_oil ) )
    GAINS_heat_content$units <- "kJ/kg"

# ---------------------------------------------------------------------------
# 3. Map activity and emissions to CEDS sectors and fuels.

    printLog("Mapping and aggregating to CEDS fuels and sectors...")

# Select years
    GAINS_emissions <- GAINS_emissions[ , c( 'Region', 'Sector', GAINS_years ) ]
    GAINS_activities <- GAINS_activities[ , c('Region', 'Sector', 'Unit', GAINS_years ) ]

# Convert to fuel and sector
    emissions_ceds <- merge( GAINS_emissions, GAINS_ctry_map,
                             by.x = 'Region',
                             by.y = 'emf_name', all = TRUE )

# Map emissions to sector and fuel
    emissions_ceds <- mapCEDS_sector_fuel( mapping_data = emissions_ceds,
                                           mapping_file = GAINS_fuel_sector_map,
                                           data_match_col = 'Sector',
                                           map_match_col = 'emf_sector',
                                           map_merge_col = c( 'ceds_sector',
                                                              'ceds_fuel' ),
                                           new_col_names = c( 'sector', 'fuel' ),
                                           level_map_in = 'working_sectors_v1',
                                           level_out = 'working_sectors_v1',
                                           aggregate = TRUE,
                                           aggregate_col = GAINS_years,
                                           oneToOne = FALSE,
                                           agg.fun = sum )

# Drop incomplete cases of emissions rows
    emissions_ceds <- emissions_ceds[ complete.cases( emissions_ceds ), ]
    emissions_ceds$units <- 'kt'

# Only CO2 is reported in Tg rather than Gg, so multiply by 10^3 to convert Tg to Gg
    if( em == 'CO2' ){ emissions_ceds[ , GAINS_years ] <- emissions_ceds[ , GAINS_years ] * 10^3 }

# Trim to only desired columns
    emissions_ceds <- emissions_ceds[ , c( 'iso', 'sector', 'fuel',
                                           'units', GAINS_years ) ]
# Rename to Xyear format
    names( emissions_ceds ) <- c( 'iso', 'sector', 'fuel', 'units', X_GAINS_years )

# Map CEDS countries and regions to activitiesd
    activities_ceds <- GAINS_activities %>%
        dplyr::left_join( GAINS_ctry_map, by = c( "Region" = "emf_name" ) )

# Map CEDS sectors and fuels to activity
    activities_ceds <- mapCEDS_sector_fuel( mapping_data = activities_ceds,
                                            mapping_file = GAINS_fuel_sector_map,
                                            data_match_col = 'Sector',
                                            map_match_col = 'emf_sector',
                                            map_merge_col = c( 'ceds_sector',
                                                               'ceds_fuel' ),
                                            new_col_names = c( 'sector',
                                                               'fuel' ),
                                            level_map_in = 'working_sectors_v1',
                                            level_out = 'working_sectors_v1',
                                            aggregate = TRUE,
                                            aggregate_col = GAINS_years,
                                            oneToOne = FALSE,
                                            agg.fun = sum )

# Drop incomplete cases
    activities_ceds <- activities_ceds[ complete.cases( activities_ceds ), ]

# Drop unnecessary columns and convert to Xyears
    activities_ceds <- activities_ceds[ , c( 'iso', 'sector', 'fuel',
                                             'Unit', GAINS_years ) ]
    names( activities_ceds ) <- c( 'iso', 'sector', 'fuel', 'units', X_GAINS_years )

# ---------------------------------------------------------------------------
# 4. Convert energy units to mass units (Activity)

    printLog( "Converting energy units to mass units..." )

# Convert PetaJoules into kiloJoules
    activities_ceds[ which( activities_ceds$units == 'PJ' ), X_GAINS_years ] <-
          activities_ceds[ which( activities_ceds$units == 'PJ' ), X_GAINS_years ] *
          10^12
    activities_ceds[ which( activities_ceds$units == 'PJ' ), 'units' ] <- 'kJ'

# For each CEDS fuel, convert energy to kg fuel based on the given heat content.
# fuel mass in kt = fuel energy * 10^-6 / heat content (kJ/kg)

#   biomass convert
    activities_ceds <- convert_to_kg_of_fuel( activities_ceds, GAINS_heat_content, "biomass" )

#   brown_coal convert
    activities_ceds <- convert_to_kg_of_fuel( activities_ceds, GAINS_heat_content, "brown_coal" )

#   hard_coal convert
    activities_ceds <- convert_to_kg_of_fuel( activities_ceds, GAINS_heat_content, "hard_coal" )

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
    activities_ceds <- activities_ceds %>%
        dplyr::group_by( iso, sector, fuel, units ) %>%
        dplyr::summarize_all( funs( sum( ., na.rm = TRUE ) ) ) %>%
        dplyr::ungroup( ) %>%
        as.data.frame( )

    emissions_ceds <- emissions_ceds %>%
        dplyr::group_by( iso, sector, fuel, units ) %>%
        dplyr::summarize_all( funs( sum( ., na.rm = TRUE ) ) ) %>%
        dplyr::ungroup( ) %>%
        as.data.frame( )

    printLog( "Interpolating and extending data..." )

# Interpolate annual values
    activities_ceds_interp <- interpolateValues( activities_ceds )
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

# Keep only rows which aren't completely NA for all years
    combined <- removeNARows( combined, X_emf_years ) %>%
        dplyr::select( iso, sector, fuel, units, X_emf_years )

# Trim unnecessary and duplicate columns
    gainsEMF30_all <- combined[ , c( 'iso', 'sector', 'fuel',
                                     'units', X_ceds_years ) ]

# Check for non-numbers and infinite values and replace with NA
    gainsEMF30_all <- replace( gainsEMF30_all, gainsEMF30_all == Inf, NA )
    gainsEMF30_all <- replace( gainsEMF30_all, is.na( gainsEMF30_all ), NA )


# ---------------------------------------------------------------------------
# 7. Calculate Emission Factors

# Remove coal_coke if SO2 emissions. EMF note only coal, and coal coke has similar EF's
# as other coals for other emission species, except SO2.
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

    Xyears <- names( gainsEMF30_all )[ grepl( "X", names( gainsEMF30_all ) ) ]

# Prepare brown coal raw data
    gains_brown_coal <- dplyr::filter( gainsEMF30_all, fuel == "brown_coal" ) %>%
                        dplyr::arrange( iso, sector )

# Prepare hard coal raw data
    gains_hard_coal <- dplyr::filter( gainsEMF30_all, fuel == "hard_coal" ) %>%
                       dplyr::arrange( iso, sector )

# Replace brown coal EF with hard coal EF
    gains_brown_coal[ , Xyears ] <- gains_hard_coal[ , Xyears ]

# Only execute the change if em is NOx
    if ( em == "NOx" ){ gains_brown_coal[, Xyears ] <-
                           0.7 * gains_brown_coal[ , Xyears ] }

# Replace new brown coal values into main EF dataframe
    gainsEMF30_all <- dplyr::filter( gainsEMF30_all, fuel != "brown_coal" ) %>%
                      dplyr::bind_rows( gains_brown_coal ) %>%
                      dplyr::arrange( iso, sector, fuel )

# Separate process and combustion
    process_gainsEMF30 <- gainsEMF30_all[ which( gainsEMF30_all$fuel ==
                                                   'process' ), ]
    gainsEMF30_comb <- gainsEMF30_all[ gainsEMF30_all$fuel %!in% 'process', ]

# ---------------------------------------------------------------------------
# 8. Diagnostics

    gains_diagnostics <- gainsEMF30_all

# Ratio of the emissions factors in the last CEDS year to the EF in the last
# EDGAR inventory year (currently 2015 for all ems other than CO2, which has an
# end year of 2018 - as of 6/12/20)
    gains_diagnostics$ratio <- gains_diagnostics[[X_end_year]] /
                               gains_diagnostics[ , paste0( "X", EDGAR_end_year ) ]

# Only write output for extreme ratios
    gains_diagnostics <- gains_diagnostics[ which( gains_diagnostics$ratio > 1.5 |
                                                   gains_diagnostics$ratio < .5 ), ]

# ---------------------------------------------------------------------------
# 9. Output

# Write the IEA conversion data to diagnostic output
    writeData( conversion_fixed, domain = "DIAG_OUT",
               fn = "B1.2.energy_conversion_factors" )

# Write the process EF data to diagnostic output
# TODO: (Future:) There are duplicated iso + sector + fuel here for CH4. This problem existed in the master
#       brnach with IEA_v2015. This output doesn't appear to be used anywhere, so can be addressed later.
    writeData( process_gainsEMF30, domain = "DIAG_OUT",
               fn = paste0( 'B.', em, '_NC_EF_GAINS_EMF30' ) )

# Write data of all heat contents to intermediate output
    writeData( GAINS_heat_content, domain = "MED_OUT", fn = "B1.1.Europe_heat_content_IEA" )

# Write extreme ratios of EFs in last CEDS year to EF in last inventory year
    writeData( gains_diagnostics, domain = "DIAG_OUT", fn = paste0( "B.", em, "_GAINS_EF_ratios" ) )

# For SO2, BC, OC, CH4, CO2 don't use global GAINS data, but write to diagnostic
# for other species, write to intermediate-output to be used for base emission factors
    if ( em %in% c( 'SO2', 'BC', 'OC', 'CO2' ) ) {

        writeData( gainsEMF30_comb, domain = "DIAG_OUT",
                   fn = paste0( 'B.', em, '_comb_EF_GAINS_EMF30' ) )

    } else {

        writeData( gainsEMF30_comb, domain = "MED_OUT",
                   fn = paste0( 'B.', em, '_comb_EF_GAINS_EMF30' ) )

    }

    logStop()

# END
