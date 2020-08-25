# Program Name: B1.1.base_CO2_comb_EF.R
# Author: Linh Vu, Rachel Hoesly, Steve Smith, Patrick O'Rourke
# Date Last Updated: May 14, 2020
# Program Purpose: Generate base CO2 combustion emission factors
# Input Files: A.coal_heat_content.csv, CO2_base_EF.xlsx,
#              A.final_comb_activity_modern.csv, Master_Country_List.csv,
#              IEA_product_fuel.csv
# Output Files: B.[em]_comb_EF_db.csv, B.[em]_comb_EF_non-bunker.csv,
#               B.CO2_comb_EF_db-values_set_to_maxEF.csv,
#               B.CO2_max_possible_comb_EF_by_fuel.csv
# Notes:
# TODO: script TODOs
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'data_functions.R' ) # Additional function files may be required.
    log_msg <- "Generating base CO2 combustion emission factors..."
    script_name <- 'B1.1.base_CO2_comb_EF.R'

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "CO2"

# Stop script if running for unsupported species
    if ( em %!in% c('CO2') ) {
      stop (paste( 'Not supported for emission species', em, 'remove from script
                   list in B1.1.base_comb_EF.R and/or makefile...' ))
    }

# ---------------------------------------------------------------------------
# 1. Load Data

# Load coal heat content from Module A
    coal_heat_content <- readData( "MED_OUT", "A.coal_heat_content" )

# Load CDIAC/EIA emissions coefficients for CO2
    emission_coefficient <- readData( "DEFAULT_EF_IN", "CO2_base_EF", ".xlsx",
                                      sheet_selection = "Emission_Coefficient" )

# Load CDIAC/EIA oxidization fractions
    fraction_oxidized <- readData( "DEFAULT_EF_IN", "CO2_base_EF", ".xlsx",
                                   sheet_selection = "Fraction_Oxidized" )

# Load base activity data from Module A
    activity_data <- readData( "MED_OUT", "A.final_comb_activity_modern" )

# Load mapping files
    MCL <- readData( "MAPPINGS", "Master_Country_List" )
    IEA_product_fuel <- readData( "MAPPINGS", "IEA_product_fuel",
                                  domain_extension = "energy/" )

# Initialize categories of fuels/sectors
    conversion_fuels <- c( 'hard_coal', 'brown_coal' )
    no_conversion_fuels <- c( "light_oil", "heavy_oil", "diesel_oil",
                              "natural_gas", 'coal_coke')
    bunker_sectors <- c( '1A3aii_Domestic-aviation',
                         '1A3ai_International-aviation',
                         '1A3di_International-shipping',
                         '1A3dii_Domestic-navigation' )

# ---------------------------------------------------------------------------
# 2. Unit conversions

# Convert coal heat content from kJ/kg to kJ/kt
    coal_heat_content[ coal_heat_content$units == "kJ/kg",
                                         X_emissions_years ] <-
        coal_heat_content[ coal_heat_content$units == "kJ/kg",
                                             X_emissions_years ] * 10^6
    coal_heat_content$units[ coal_heat_content$units == "kJ/kg" ] <- "kJ/kt"

# If any units were not converted to kt from kg, throw an error. This error
# indicates that some original units were in a form unacounted for by this
# script.
    if ( any( coal_heat_content$units != "kJ/kt" ) |
         any( emission_coefficient$units[ emission_coefficient$cdiac_fuel %in%
                                          conversion_fuels ] != "kt CO2/kJ" ) ){

      stop( paste0( "Units mismatched. Check that coal heat content ",
                    "is kJ/kt and CDIAC solid fuels EF is kt/kJ in ", script_name, "..." ) )

    }

# Convert gas_fuels EF from kt/TJ to kt/kt
# Gas_fuels EF from CDIAC is TJ gross (HHV), so use appropriate  conversion factor
    emission_coefficient$Emission_Coefficient[ emission_coefficient$fuel %in%
                                                 "natural_gas" ] <-
      emission_coefficient$Emission_Coefficient[ emission_coefficient$fuel %in%
                                                   "natural_gas" ] *
      conversionFactor_naturalgas_TJ_per_kt_Gross  # kt/TJ * TJ/kt = kt/kt
    emission_coefficient$units[ emission_coefficient$fuel %in% "natural_gas" ] <-
             "kt CO2/kt"

# ---------------------------------------------------------------------------
# 3. Calculate Emission Factor
#    For Solid Fuels
#    Oxidized_Emission_Coefficient = Emission_Coefficient (kg CO2/kJ fuel) *
#                      Fraction_Oxidized * Energy_Content (kJ fuel/kt fuel)
#    For Liquid Fuels
#    Oxidized_Emission_Coefficient = Emission_Coefficient (kg CO2/kt fuel) *
#                                    Fraction_Oxidized

# Generate a blank template
    ef_data <- activity_data[ c( 'iso', 'sector', 'fuel' ) ]

# Fill in db with Emission Coefficients:
# 1. Match by fuel defaults
    default <-
        emission_coefficient[ which( emission_coefficient$iso == 'default' &
                                     emission_coefficient$sector == 'default' ), ]
    ef_data$Emission_Coefficient <-  default[ match( ef_data$fuel,
                                                     default$fuel ),
                                              'Emission_Coefficient' ]

# 2. Match by iso and fuel
    iso_default <-
        emission_coefficient[ which( emission_coefficient$iso == 'default' &
                                     emission_coefficient$iso != 'default' ), ] # TODO: This line is nonsense; sector should be default, not iso
    ef_data <- replaceValueColMatch( ef_data,
                                    iso_default,
                                    x.ColName = 'Emission_Coefficient',
                                    match.x = c( 'iso', 'fuel' ),
                                    addEntries = F )

# 3. Match by sector and fuel
    sector_default <-
        emission_coefficient[ which( emission_coefficient$iso == 'default' &
                                     emission_coefficient$sector != 'default' ), ]

    ef_data<- replaceValueColMatch( ef_data,
                                    sector_default,
                                    x.ColName = 'Emission_Coefficient',
                                    match.x = c( 'sector', 'fuel' ),
                                    addEntries = F )

# 4. Match by iso, sector & fuel
    iso_sector_fuel <-
        emission_coefficient[ which( emission_coefficient$iso != 'default' &
                                     emission_coefficient$sector != 'default' ), ]
    ef_data <- replaceValueColMatch( ef_data,
                                     iso_sector_fuel,
                                     x.ColName = 'Emission_Coefficient',
                                     match.x = c( 'iso', 'sector', 'fuel' ),
                                     addEntries = F )

# Fill in db with Fraction Oxidized
# 1. Match by fuel defaults
    default <-
        fraction_oxidized[ which( fraction_oxidized$iso == 'default' &
                                  fraction_oxidized$sector == 'default' ), ]

    ef_data$fraction_oxidized <-  fraction_oxidized[ match( ef_data$fuel,
                                                            default$fuel ),
                                                     'Fraction_Oxidized' ]

# 2. Match by iso and fuel
    iso_default <-
        fraction_oxidized[ which( fraction_oxidized$sector == 'default' &
                                  fraction_oxidized$iso != 'default' ), ]

    ef_data <- replaceValueColMatch( ef_data,
                                     iso_default,
                                     x.ColName = 'fraction_oxidized',
                                     y.ColName = 'Fraction_Oxidized',
                                     match.x = c( 'iso', 'fuel' ),
                                     addEntries = F )

# 3. Match by sector and fuel
    sector_default <-
        fraction_oxidized[ which( fraction_oxidized$iso == 'default' &
                                  fraction_oxidized$sector != 'default' ), ]
    ef_data<- replaceValueColMatch( ef_data,
                                    sector_default,
                                    x.ColName = 'fraction_oxidized',
                                    y.ColName = 'Fraction_Oxidized',
                                    match.x = c( 'sector', 'fuel' ),
                                    addEntries = F)

# 4. Match by iso, sector & fuel
  iso_sector_fuel <-
      fraction_oxidized[ which( fraction_oxidized$iso != 'default' &
                                fraction_oxidized$sector != 'default' ), ]
  ef_data <- replaceValueColMatch( ef_data,
                                   sector_default,
                                   x.ColName = 'fraction_oxidized',
                                   y.ColName = 'Fraction_Oxidized',
                                   match.x = c( 'iso', 'sector', 'fuel' ),
                                   addEntries = F )

# Multiply emission coefficient by oxidation fraction to calculate
# emissions factors
    ef_data$EF <- ef_data$fraction_oxidized * ef_data$Emission_Coefficient
    ef_data[ , X_emissions_years ] <- NA

# Fill in trend with constant EFs for liquid fuels; these do not need to be
# converted with heat content
    ef_data[ which( ef_data$fuel %in% no_conversion_fuels ), X_emissions_years ] <-
        matrix( rep( ef_data$EF[ which( ef_data$fuel %in% no_conversion_fuels ) ],
                     each = length( X_emissions_years ) ),
               ncol = length( X_emissions_years ) , byrow = T )

# Fill in zero for biomass
    ef_data[ which( ef_data$fuel == 'biomass' ),
             c( "Emission_Coefficient",
                "fraction_oxidized",
                'EF', X_emissions_years ) ] <- 0

# ---------------------------------------------------------------------------
# 4. Conversion for solid fuel using coal heat content

# Convert solid_fuels EF from kt/kJ to kt/kt, using country/type/year-specific coal heat content
    ef_data[ which( ef_data$fuel %in% conversion_fuels ), X_emissions_years ] <-
        ef_data[ which( ef_data$fuel %in% conversion_fuels ), 'EF' ] *
        coal_heat_content[ match( paste( ef_data[ which( ef_data$fuel %in%
                                                         conversion_fuels ), 'iso' ],
                                         ef_data[ which( ef_data$fuel %in%
                                                         conversion_fuels ), 'fuel' ] ),
                                  paste( coal_heat_content$iso,
                                         coal_heat_content$fuel ) ),
                           X_emissions_years ]  # TODO: This could be split up into several lines for clarity.

# ---------------------------------------------------------------------------
# 5. Fix any outlier emission factors
#    Set maximum emission factors by fuel type and check that no EFs exceed these  values.

# Maximum EF = carbon content times conversion from C -> CO2 (since we are working on a
# mass basis for fuels in CEDS)
# Therefore, setting the  maximum EF translattes to selecting a carbon fraction
#
# The aim here is to pick a maximum value for each category to make sure inconsistancies or
# other data errors in the above calcuations do not result in an unrealistic emission factor.

# Petroleum Products
# For medium and light oils, we assume a maximum carbon content of 87%, slightly higher than
# any of the values in Table A-63 in the US Inventory of U.S. Greenhouse Gas Emissions and Sinks: 1990-2017
# (April 2019) EPA 430-R-19-001.

# For heavy oil, we select  92.3% which is the value appropriate for petroleum coke

# For hard coal and coal coke we use 98% as the upper end value for anthracite
# For brown coal we use 69% (lower limit for sub-bituminous coal) as the upper limit for lignite
# Both values drawn from: Bell, D.A., Towler, B.F. and Fan, M. (2010) "Chapter 1. The Nature of Coal",
# in Coal gasification and its applications. William Andrew.
# These are extreme upper end emission factors by weight since ash, sulfur, and water
# content are not taken into account.

# For gaseous fuels we use 82.7%, the carbon content of Isobutane (and n-butane)
# From Table A-64 EPA(2019)

# Create template dataframe for these values
max_CO2_ef_by_fuel <- ef_data %>%
    dplyr::select( fuel, X_emissions_years ) %>%
    dplyr::mutate( max = pmax( !!!rlang::syms( X_emissions_years ) ) ) %>%
    dplyr::select( -X_emissions_years ) %>%
    dplyr::group_by( fuel ) %>%
    dplyr::summarise_all( list( ~max( ., na.rm = T ) ) ) %>%
    dplyr::rename( maxCO2EF = max )

# Add values as described above
max_CO2_ef_by_fuel <- max_CO2_ef_by_fuel %>%
    dplyr::mutate( maxCO2EF = if_else( fuel == "heavy_oil",
        92.3 / 100 * (15.9994*2+12.011)/12.011, maxCO2EF ) ) %>%
    dplyr::mutate( maxCO2EF = if_else( fuel == "diesel_oil",
        87.0 / 100 * (15.9994*2+12.011)/12.011, maxCO2EF ) ) %>%
    dplyr::mutate( maxCO2EF = if_else( fuel == "light_oil",
        87.0 / 100 * (15.9994*2+12.011)/12.011, maxCO2EF ) ) %>%
    dplyr::mutate( maxCO2EF = if_else( fuel == "hard_coal",
        98.0 / 100 * (15.9994*2+12.011)/12.011, maxCO2EF ) ) %>%
    dplyr::mutate( maxCO2EF = if_else( fuel == "coal_coke",
        98.0 / 100 * (15.9994*2+12.011)/12.011, maxCO2EF ) ) %>%
    dplyr::mutate( maxCO2EF = if_else( fuel == "brown_coal",
        69.0 / 100 * (15.9994*2+12.011)/12.011, maxCO2EF ) ) %>%
    dplyr::mutate( maxCO2EF = if_else( fuel == "natural_gas",
        82.7 / 100 * (15.9994*2+12.011)/12.011, maxCO2EF ) ) %>%
     dplyr::mutate( maxCO2EF = if_else( fuel == "biomass",
        0.0 / 100 * (15.9994*2+12.011)/12.011, maxCO2EF ) )

# Set any EF to their maximum possilble value if they are larger than the
# given maximum
ef_data_max_EF_applied <- ef_data %>%
  dplyr::left_join( max_CO2_ef_by_fuel, by = "fuel" ) %>%
  dplyr::mutate_at( .vars = X_emissions_years, .funs = funs(
    if_else( . > maxCO2EF, maxCO2EF, . ) ) ) %>%
  dplyr::select( -maxCO2EF )

# If some EFs were larger than their maximum possible value,
# inform the user, utilize the corrected EFs, and create a diagnostic df
# of the original EFs which were changed.
if( !identical( ef_data_max_EF_applied, ef_data ) ){

  printLog( paste0( "Some default CO2 combustion emissions factors have been set to their maximum ",
            "possible values, as assumed in ", script_name, " - Section 5. ",
            "See ", paste0( "B.", em, "_comb_EF_db-values_set_to_maxEF" ), ".csv..." ) )

  original_efs <- ef_data %>%
    tidyr::gather( key = year, value = EF, X_emissions_years )

  new_efs <- ef_data_max_EF_applied %>%
    tidyr::gather( key = year, value = EF, X_emissions_years )

  original_efs_which_changed <- dplyr::setdiff( original_efs, new_efs ) %>%
    tidyr::spread( year, EF )

  ef_data <- ef_data_max_EF_applied

} else {

  printLog( "No default CO2 emission factors were larger than maximum possible",
            "values by CEDS fuel as assumed in", script_name, "- therefore none were corrected..." )

}

# ---------------------------------------------------------------------------
# 6. Final processing

# Clean the emissions factors dataframe for output
    ef_data[ is.na( ef_data ) ] <- 0
    ef_data$units <- "kt/kt"  # kt CO2/kt
    ef_data <- ef_data[ , c( "iso", "sector",
                             "fuel", "units",
                             X_emissions_years ) ]
    ef_data <- dplyr::arrange( ef_data, iso, sector, fuel )

# Prepare diagnostic output: country-specific EF for non-bunker sectors
    ef_non_bunker <- ef_data %>%
        dplyr::filter( sector %!in% bunker_sectors ) %>%
        dplyr::select( -sector ) %>%
        dplyr::distinct( )

# Final sanity check to make sure that no sectors are duplicated
    if ( any( duplicated( ef_non_bunker[ c( "iso", "fuel", "units" ) ] ) ) ){

      warning( "There are duplicates in non-bunker sector CO2 EFs." )

    }

# ---------------------------------------------------------------------------
# 7. Output

#   Save base CO2 combustion EFs
    writeData( ef_data, "MED_OUT", paste0( "B.", em, "_comb_EF_db" ) )

#   Save diagnostic of maximum possible CO2 EFs by CEDS fuel
    writeData( max_CO2_ef_by_fuel, "DIAG_OUT", paste0( "B.", em, "_max_possible_comb_EF_by_fuel" ) )

#   Save diagnostic of original value CO2 EFs that were set to the maximum
#   possible values, by CEDS fuel (if it exists)
    if( exists( "original_efs_which_changed" ) ){

      writeData( original_efs_which_changed, "DIAG_OUT",
                 paste0( "B.", em, "_comb_EF_db-values_set_to_maxEF" ) )

    }

#   Save diagnostic coutnry-specific EF for non-bunker sectors
    writeData( ef_non_bunker, "DIAG_OUT", paste0( "B.", em, "_comb_EF_non-bunker" ) )

    logStop()

# END
