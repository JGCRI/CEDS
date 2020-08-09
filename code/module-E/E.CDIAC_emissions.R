# ------------------------------------------------------------------------------
# Program Name: E.CDIAC_emissions.R
# Author(s): Rachel Hoesly, Linh Vu, Patrick O'Rourke
# Date Last Updated: August 2, 2020
# Program Purpose: To read in & reformat CDIAC emissions data.
# Input Files: A.UN_pop_master.csv, CDIAC_national_1751_2011.csv, CDIAC_country_map.csv
#              Master_Country_List,csv, USGS_Commodity_Summaries_Cement_Production.xlsx
# Output Files: E.CO2_CDIAC_inventory.csv, E.CO2_CDIAC_Cement.csv, E.CO2_CDIAC_Total_CO2.csv,
#               E.CO2_CDIAC_liquid_and_gas.csv, E.CO2_CDIAC_solid_fuel.csv,
#               E.CO2_CDIAC_solid_fuel_cumulative.csv,
#               E.CO2_CDIAC_by_figure_region_CDIAC_fuel.csv,
#               E.CO2_CDIAC_by_iso_CDIAC_fuel.csv, E.CDIAC_cement_EF.csv,
#               E.USGS_cement_production.csv
# Notes: Cement data is extended to last available year for USGS cement data. Other data ends at last CDIAC year, with zeros afterward.
# TODO: 1) Some outputs may need to be fixed. For instance, there are numerous outputs
#       which have 0 emissions for 2012-2015, which is really an artifact of the fact
#       that we extended CDIAC cement emissions, but not emissions for other sources in CDIAC
#       (therefore, the extra years should be removed or set to NA for non-cement emissions):
#           - cdiac_final - all sectors other than cement (this is misleading and could cause issues)
#           - cdiac_total - emissions are zero from 2012-2015.... these years can be removed
#           - cdiac_solid_fuel - emisisons are zero from 2012-2015.... these years can be removed
#           - cdiac_solid_fuel_cumulative - cumulative emissions are present from 2012-2015, but set to 2011 values
#             since emissions are 0 from 2012-2015, meaning cumulative emissions don't change
#        2) E.CDIAC_cement_EF.csv also is missing data for asm, gum, pri, ssd, and vir, and has inf and NaN values
#        which could be fixed by using the extend_and_interpolate function (after setting theses values to NA)
#        3) Clean up years (use objects)
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R" ) # Any additional function files required
    log_msg <- "Read and format CDIAC emissions" # First message to be printed to the log
    script_name <- "E.CDIAC_emissions.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# -----------------------------------------------------------------------------------------------------------
# 0.5 Define Functions

# Define function to replicate a row, x, n times and convert to a matrix
    rep.row <- function( x, n ){ matrix( rep( x, each = n ), nrow = n ) }

# Define function to process individual USGS cement production tab
    procUSGS <- function( df ) {

#   Trim trailing whitespace
        df$Country <- gsub( "\\s+$", "", df$Country )

#   Remove non-characters
        df$Country <- gsub( "[^a-zA-Z ]", "", df$Country )

#   Force colnames to syntactically valid names
        names( df ) <- make.names( names( df ) )

#   Identify any years
        e_years <- grepl( "^X.*e$", names( df ) )

#   Get rid of the e in the year variable
        names( df )[ e_years ] <- gsub( "e", "", names( df )[ e_years ] )

#   Drop any NA countries
        df <- dplyr::filter( df, !is.na( Country ) )

#   Map isos
        df$iso <- usgs_ctry_map$iso[ match( df$Country, usgs_ctry_map$Country ) ]

#   Coerce value columns to numeric
        df <- dplyr::mutate_at( df, vars( matches( 'X\\d{4}' ) ), as.numeric )

        return( df )
    }

# Define function to disaggregate isos which do not have CDIAC data provided directly in any CDIAC time period,
#    but have their data contained within an aggregated CDIAC iso. Downscaling to these isos
#    will be done with UN population data

    disagg_CDIAC_iso_in_aggIso <- function( agg_iso_region, disagg_isos_in_agg_region,
                                            CDIAC_df_in, population_data, CDIAC_years_use ){

        agg_region_all_isos <- c( disagg_isos_in_agg_region, agg_iso_region)

        no_per_capita_data_years <- paste0( "X", cdiac_start_year : 1949 )

        fuels_of_interest <- unique( CDIAC_df_in$fuel)
        fuels_of_interest_no_percap <- subset( fuels_of_interest, fuels_of_interest != "per_capital_CO2" ) # Remove per_capita_CO2 since this will get fixed
        fuels_of_interest_final <- c( fuels_of_interest_no_percap, "CDIAC_derived_population" ) # Add derived CDIAC population

#       Calculate aggregate UN region population
        agg_region_iso_populations <- population_data %>%
            dplyr::filter( iso %in% agg_region_all_isos ) %>%
            dplyr::select( iso, CDIAC_years_use ) %>%
            tidyr::gather( key = Years, value = Population, CDIAC_years_use ) %>%
            dplyr::filter( Years %in% extended_CDIAC_years_with_Xs )

        aggregated_population <- agg_region_iso_populations %>%
            dplyr::select( -iso ) %>%
            dplyr::group_by( Years ) %>%
            dplyr::summarise_all( funs( sum (., na.rm = TRUE) ) ) %>%
            dplyr::ungroup( ) %>%
            dplyr::rename( Agg_region_population = Population)

#       Calculate UN population share for each sub-iso
        agg_region_iso_pop_shares <- agg_region_iso_populations %>%
            dplyr::left_join( aggregated_population, by = "Years" ) %>%
            dplyr::mutate( iso_share_of_agg_region_pop = Population / Agg_region_population ) %>%
            dplyr::select( -Population, -Agg_region_population  )

#       Subset the agg region being downscaled in the CDIAC data
        cdiac_agg_region_of_interest <- CDIAC_df_in %>%
            dplyr::filter( iso == agg_iso_region ) %>%

#       Create variable "CDIAC_derived_population" and remove "per_capital_CO2" fuel type - will need to be fixed after
#       downscaling - set CDIAC_derived_population to 0 for 1750-1949 (since this is a temporary variable,
#       and no per capita data in original CDIAC for those years)
            tidyr::spread( fuel, Emissions ) %>%
            dplyr::mutate( CDIAC_derived_population = ( 1 / ( per_capital_CO2 * ( 1/ Total_CO2 ) ) ),
                           CDIAC_derived_population = if_else( Years %in% no_per_capita_data_years,
                                                               0, CDIAC_derived_population ) ) %>%
            dplyr::select( -per_capital_CO2 ) %>%
            tidyr::gather( key = fuel, value = Emissions, fuels_of_interest_final )

#       Duplicate the agg regions Emissions data for each disagg iso
        cdiac_agg_region_aggIso_AND_disaggIsos <- cdiac_agg_region_of_interest

        for( isos in disagg_isos_in_agg_region  ){

            cdiac_diagg_iso_add <- cdiac_agg_region_aggIso_AND_disaggIsos %>%
                dplyr::mutate( iso = isos )

            cdiac_agg_region_aggIso_AND_disaggIsos <- dplyr::bind_rows( cdiac_agg_region_aggIso_AND_disaggIsos,
                                                                        cdiac_diagg_iso_add ) %>%
                dplyr::distinct( )

        }

#       Multiply population share by agg region emissions for each sub-iso and sector combination
        cdiac_agg_region_downscaled <- cdiac_agg_region_aggIso_AND_disaggIsos %>%
            dplyr::left_join( agg_region_iso_pop_shares, by = c( "iso", "Years" ) ) %>%
            dplyr::mutate( Down_Scaled_Emissions = Emissions * iso_share_of_agg_region_pop ) %>%
            dplyr::select( -Emissions, -iso_share_of_agg_region_pop ) %>%
            dplyr::rename( Emissions = Down_Scaled_Emissions )

#       Check that there are no NAs for new downscaled emissions
        if( any( is.na( cdiac_agg_region_downscaled$Emissions ) ) == TRUE ){

            stop( paste0( "Some downscaled emissions are now NA. Check population and emissions data.") )

        } else{

            printLog( "There are no NAs for emissions after downscaling", agg_iso_region, "to the following isos:",
                      agg_region_all_isos )

        }

#       Check that diagg regions summed = agg region data for each sector - roundedto 9 decimals
        cdiac_agg_region_downscaled_reaggregated_for_check <- cdiac_agg_region_downscaled %>%
            dplyr::select( -iso ) %>%
            dplyr::group_by( fuel, Years ) %>%
            dplyr::summarise_all( funs( sum (., na.rm = TRUE) ) ) %>%
            dplyr::arrange( fuel, Years ) %>%
            dplyr::mutate( Emissions = round( Emissions, digits = 9 ) ) %>%
            dplyr::ungroup( )

        cdiac_agg_region_of_interest_for_check <- cdiac_agg_region_of_interest %>%
            dplyr::select( -iso ) %>%
            dplyr::arrange( fuel, Years ) %>%
            dplyr::mutate( Emissions = round( Emissions, digits = 9 ) )

        identical_after_downscale <- all.equal(cdiac_agg_region_downscaled_reaggregated_for_check,cdiac_agg_region_of_interest_for_check)

        if( identical_after_downscale != TRUE ){

            stop( paste0( "Downscaled emissions do not equal aggregate region emissions for all fuels. Check population, emissions data, ",
                          "and disagg_CDIAC_iso_in_aggIso function") )

        } else{

            printLog( "Downscaled CDIAC emissions summed for -", agg_region_all_isos,
                      "- equals aggregate CDIAC emissions for all fuels for the original agg_region -",
                      agg_iso_region, "- after downscaling with A.UN_pop_master.csv." )

        }

#       Calculate new per_capital_CO2 variable = Total_CO2 / CDIAC_derived_population,
#       set values to 0 before 1950 (since no per capita data in original CDIAC for those years)
        cdiac_agg_region_downscaled_final <- cdiac_agg_region_downscaled %>%
            tidyr::spread( fuel, Emissions ) %>%
            dplyr::mutate(  per_capital_CO2 = Total_CO2 / CDIAC_derived_population,
                            per_capital_CO2 = if_else( Years %in% no_per_capita_data_years,
                                                       0, per_capital_CO2 ) ) %>%
            dplyr::select( -CDIAC_derived_population ) %>%
            tidyr::gather( key = fuel, value = Emissions, fuels_of_interest )

#       Filter out original agg region data from CDIAC emissions and replace with disaggregate data
        cdiac_split_final_no_agg_region_with_disagg_isos <- CDIAC_df_in %>%
            dplyr::filter( iso != agg_iso_region ) %>%
            dplyr::bind_rows( cdiac_agg_region_downscaled_final )

        return( cdiac_split_final_no_agg_region_with_disagg_isos )

    }

# -----------------------------------------------------------------------------------------------------------
# 1. Read in files

# CDIAC inventory
    cdiac_read <- readData( 'EM_INV', domain_extension = "CDIAC/",
                            'CDIAC_national_1751_2011', missing_value = '.' )

# Mapping files
    MCL <- readData( "MAPPINGS", "Master_Country_List" )
    cdiac_country_map <- readData( 'EM_INV', domain_extension = "CDIAC/",
                                   'CDIAC_country_map' )

# UN population from Module A
    un_pop <- readData( "MED_OUT", 'A.UN_pop_master' )

    usgs_sheets <- c( "2002", "2005", "2007", "2010", "2011", "2013", "2016" )

# USGS data
    usgs_data_in <- readData( "ACTIVITY_IN",
                              "USGS_Commodity_Summaries_Cement_Production",
                              ".xlsx", sheet_selection = usgs_sheets,
                              missing_value = c("--", "XX", "W", "(5)") )

# USGS mapping
    usgs_ctry_map <- readData( "ACTIVITY_IN",
                               "USGS_Commodity_Summaries_Cement_Production",
                               ".xlsx", sheet_selection = "mapping" )

# -----------------------------------------------------------------------------------------------------------
# 2. Formatting Data to ceds format

    cdiac_start_year <- cdiac_start_year + 1 #  common_data object + 1 year
    final_units <- 'kt-C'

# Process UN population
    un_pop$X_year <- paste0( "X", un_pop$year )
    un_pop$pop <- as.numeric( un_pop$pop )

# Cast to wide by year
    population <-
          cast( un_pop[ which( un_pop$year %in%
                               historical_pre_extension_year:end_year ), ],
                iso ~ X_year, value = 'pop' )

# Process CDIAC data
    cdiac_fuel_wide <- cdiac_read
    cdiac_fuel_wide$units <- final_units

# Drop first two rows
    cdiac_fuel_wide <- cdiac_fuel_wide[ -1:-2, ]
    cdiac_fuels <- c( 'Total_CO2', 'solid_fuels', 'liquid_fuels',
                      'gas_fuels', 'cement_production',
                      'gas_flaring', 'per_capital_CO2', 'bunker_fuels' )

# Apply fuel names as column headers
    names( cdiac_fuel_wide )[ 3:10 ] <- cdiac_fuels

# Create an Xyear row
    cdiac_fuel_wide$X_year <- paste0( 'X', cdiac_fuel_wide$Year )

# Make values numeric
    cdiac_fuel_wide <- cdiac_fuel_wide %>%
        dplyr::mutate_at( c( 'Year', cdiac_fuels ), as.numeric )

# Add iso
    cdiac_fuel_wide$iso <- cdiac_country_map[ match( cdiac_fuel_wide$Nation,
                                                     cdiac_country_map$CDIAC ),
                                              'iso' ]

# Aggregate all fuel columns by iso/year/unit
    cdiac_fuel_wide <- aggregate( cdiac_fuel_wide[ cdiac_fuels ],
                                  by = list( iso = cdiac_fuel_wide$iso,
                                             year = cdiac_fuel_wide$Year,
                                             X_year = cdiac_fuel_wide$X_year,
                                             units = cdiac_fuel_wide$units ),
                                  FUN = sum, na.rm = T )

# Reshape to standard ceds format, select post 1850 years
    cdiac_long <- melt( cdiac_fuel_wide,
                        id = c( 'iso', 'year', 'X_year', 'units' ) )
    names( cdiac_long )[ which( names( cdiac_long ) == 'variable' ) ] <- 'fuel'

# Cast CDIAC to wide
    cdiac_year_wide <- cast( cdiac_long, iso + fuel + units ~ X_year )
    cdiac_year_wide[ is.na( cdiac_year_wide ) ] <- 0

# ------------------------------------------------------------------------------
# 3. Remove negative CDIAC values, extend to 1750

# Grab CDIAC id cols
    id_cdiac <- cdiac_year_wide[ , 1:3 ]

# Grab CDIAC non-ID cols
    years_cdiac <- cdiac_year_wide[ , 4:ncol( cdiac_year_wide ) ]

# Set any values less than 0 to NA
    years_cdiac[ years_cdiac < 0 ] <- NA

# Approximate NAs by interpolation
    years_cdiac <- as.data.frame( t( apply( years_cdiac,
                                            MARGIN = 1, na.approx ) ),
                                  stringsAsFactors = FALSE )

# Copy column headers from the original df
    names( years_cdiac ) <- names( cdiac_year_wide )[ 4:ncol( cdiac_year_wide ) ]

# Bind interpolated values with the original id cols
    cdiac_corrected <- cbind( id_cdiac, years_cdiac )

# Extend 1751 value to 1750
    cdiac_corrected$X1750 <- cdiac_corrected$X1751

# Force fuel to character
    cdiac_corrected$fuel <- as.character( cdiac_corrected$fuel )
    cdiac_start_year

# Obtain 1750
    cdiac_start_year <- cdiac_start_year - 1 # Reset start year to 1750, common_data.R object value
    X_cdiac_years <- paste0( 'X', cdiac_start_year : cdiac_end_year )

# -----------------------------------------------------------------------------
# 4. Split Countries

#    This section is for isos which have CDIAC data provided for certain
#    time periods, but in other time periods their data is within an aggregated iso.
#    Example: Serbia -  CDIAC provides data for Serbia from 2006-2011, while
#    data for this iso is counted within the CDIAC region "YUGOSLAVIA (MONTENEGRO & SERBIA)"
#    from 1992-2005, and within the CDIAC region "YUGOSLAVIA (FORMER SOCIALIST FEDERAL REPUBLIC)"
#    from 1880-1991

#    For each country, use the disaggregate_country function defined in
#    data_functions.R to split aggregate country fuels. Use population breakdown
#    to split data.

# FSU
    cdiac_ussr_corrected <-
              disaggregate_country( original_data = cdiac_corrected,
                                    id_cols = c( 'iso', 'fuel' ),
                                    trend_data = population,
                                    trend_match_cols = 'iso',
                                    combined_iso = 'USSR',
                                    dis_end_year = 1991,
                                    disaggregate_iso = c( 'aze', 'arm', 'blr',
                                                          'est', 'geo', 'kaz',
                                                          'kgz', 'lva', 'ltu',
                                                          'mda', 'tjk', 'tkm',
                                                          'ukr', 'uzb', 'rus' ),
                                    write_over_values = T )

# Yugoslavia
    cdiac_yug_corrected <-
              disaggregate_country( original_data = cdiac_ussr_corrected,
                                    id_cols = c( 'iso', 'fuel' ),
                                    trend_data = population,
                                    trend_match_cols = 'iso',
                                    combined_iso = 'yug',
                                    dis_end_year = 1991,
                                    disaggregate_iso = c( 'bih', 'hrv', 'mkd',
                                                          'svn', 'scg' ),
                                    allow_dropped_data = T )

# Serbia, Montenegro, and Kosovo
    cdiac_scg_corrected <-
              disaggregate_country( original_data = cdiac_yug_corrected,
                                    id_cols = c( 'iso', 'fuel' ),
                                    trend_data = population,
                                    trend_match_cols = 'iso',
                                    combined_iso = 'scg',
                                    dis_end_year = 2005,
                                    disaggregate_iso = c( 'srb', 'mne' ) )

# Czechoslovakia
    cdiac_csk_corrected <-
              disaggregate_country( original_data = cdiac_scg_corrected,
                                    id_cols = c( 'iso', 'fuel' ),
                                    trend_data = population,
                                    trend_match_cols = 'iso',
                                    combined_iso = 'csk',
                                    dis_end_year = 1991,
                                    disaggregate_iso = c( 'cze', 'svk' ) )

# East and West Pakistan
    cdiac_pak_corrected <-
              disaggregate_country( original_data = cdiac_csk_corrected,
                                    id_cols = c( 'iso', 'fuel' ),
                                    trend_data = population,
                                    trend_match_cols = 'iso',
                                    combined_iso = 'EAST_WEST_PAKISTAN',
                                    dis_end_year = 1971,
                                    disaggregate_iso = c( 'pak', 'bgd' ) )

# United Korea
    cdiac_kor_corrected <-
              disaggregate_country( original_data = cdiac_pak_corrected,
                                    id_cols = c( 'iso', 'fuel' ),
                                    trend_data = population,
                                    trend_match_cols = 'iso',
                                    combined_iso = 'UNITED_KOREA',
                                    dis_end_year = 1944,
                                    ratio_start_year = 1948,
                                    disaggregate_iso = c( 'prk','kor' ) )

# French Equatorial Africa
    cdiac_FrEqAf_corrected <-
              disaggregate_country( original_data = cdiac_kor_corrected,
                                    id_cols = c( 'iso', 'fuel' ),
                                    trend_data = population,
                                    trend_match_cols = 'iso',
                                    combined_iso = 'FRENCH_EQUATORIAL_AFRICA',
                                    dis_end_year = 1958,
                                    disaggregate_iso = c( 'caf', 'cog',
                                                          'gab', 'tcd' )  ,
                                    allow_dropped_data = T )

# French West Africa
    cdiac_FrWeAf_corrected <-
              disaggregate_country( original_data = cdiac_FrEqAf_corrected,
                                    id_cols = c( 'iso', 'fuel' ),
                                    trend_data = population,
                                    trend_match_cols = 'iso',
                                    combined_iso = 'FRENCH_WEST_AFRICA',
                                    method = 2,
                                    dis_end_year = 1957,
                                    disaggregate_iso = c( 'mrt', 'sen', 'mli',
                                                          'gin', 'civ', 'bfa',
                                                          'ben', 'ner' ) )

# Rwanda-Urundi
    cdiac_RU_corrected <-
              disaggregate_country( original_data = cdiac_FrWeAf_corrected,
                                    id_cols = c( 'iso', 'fuel' ),
                                    trend_data = population,
                                    trend_match_cols = 'iso',
                                    combined_iso = 'RWANDA-URUNDI',
                                    dis_end_year = 1961,
                                    disaggregate_iso = c( 'rwa', 'bdi' ),
                                    allow_dropped_data = T )

# Netherland Antiliies and Aruba
    cdiac_NAR_corrected <-
              disaggregate_country( original_data = cdiac_RU_corrected,
                                    id_cols = c( 'iso', 'fuel' ),
                                    trend_data = population,
                                    trend_match_cols = 'iso',
                                    combined_iso = 'NETHERLAND_ANTILLES_AND_ARUBA',
                                    dis_end_year = 1985,
                                    dis_start_year = 1926,
                                    disaggregate_iso = c( 'ant', 'abw' ) )

# Netherland Antillies
    cdiac_NA_corrected <-
              disaggregate_country( original_data = cdiac_NAR_corrected,
                                    id_cols = c( 'iso', 'fuel' ),
                                    trend_data = population,
                                    trend_match_cols = 'iso',
                                    combined_iso = 'ant',
                                    dis_end_year = 2011,
                                    ratio_range_length = 2,
                                    disaggregate_iso = c( 'cuw', 'sxm' ) )

# Rhodesia Nyasaland
    cdiac_RN_corrected <-
              disaggregate_country( original_data = cdiac_NA_corrected,
                                    id_cols = c( 'iso', 'fuel' ),
                                    trend_data = population,
                                    trend_match_cols = 'iso',
                                    combined_iso = 'RHODESIA-NYASALAND',
                                    dis_end_year = 1963,
                                    disaggregate_iso = c( 'zmb', 'mwi' ),
                                    allow_dropped_data = T )

# Leeward Islands
    cdiac_LI_corrected <-
              disaggregate_country( original_data = cdiac_RN_corrected,
                                    id_cols = c( 'iso', 'fuel' ),
                                    trend_data = population,
                                    trend_match_cols = 'iso',
                                    combined_iso = 'LEEWARD ISLANDS',
                                    dis_end_year = 1956,
                                    dis_start_year = 1950,
                                    disaggregate_iso = c( 'kna', 'atg' ),
                                    allow_dropped_data = T )

# -----------------------------------------------------------------------------------------------------------
# 5. Split additional isos

#    This section is for isos which do not have CDIAC data provided directly in any CDIAC time period,
#    but have their data contained within an aggregated CDIAC iso. Downscaling to these isos
#    will be done with UN population data
#    Example: USA CDIAC data contains data for the American Samoa, Guam, Puerto Rico, U.S. Virgin Islands,
#             and the USA (proper, 50 states)

    extended_CDIAC_years_with_Xs <- paste0( "X", cdiac_start_year : cdiac_end_year )
    CDIAC_CEDS_years <- paste0( "X", cdiac_start_year : end_year )

#   Remove original Puerto Rican data, as it is only available for 1920
    cdiac_Puerto_Rico <- cdiac_LI_corrected %>%
        dplyr::filter( iso == "pri" )

    cdiac_split_final_no_pri <- cdiac_LI_corrected %>%
        dplyr::filter( iso != "pri" ) %>%
        tidyr::gather( key = "Years", value = "Emissions", extended_CDIAC_years_with_Xs )


# Fix USA data - disaggregate to asm, gum, vir, pri, and usa (https://cdiac.ess-dive.lbl.gov/trends/emis/tre_usa.html)
    agg_region_disagg_isos <- c( "asm", "gum", "vir", "pri" )
    agg_region <- "usa"

    cdiac_split_usa_Fixed <- disagg_CDIAC_iso_in_aggIso ( agg_region, agg_region_disagg_isos,
                                         cdiac_split_final_no_pri, population, CDIAC_CEDS_years )

# Fix Serbia data - disaggregate to srb and srb (Kosovo)
    agg_region_disagg_isos <- c( "srb (kosovo)" )
    agg_region <- "srb"

    cdiac_split_srb_Fixed <- disagg_CDIAC_iso_in_aggIso ( agg_region, agg_region_disagg_isos,
                                                          cdiac_split_usa_Fixed, population, CDIAC_CEDS_years )

# Fix Sudan data - disaggregate to sdn and ssd
    agg_region_disagg_isos <- c( "ssd" )
    agg_region <- "sdn"

    cdiac_split_sdn_Fixed <- disagg_CDIAC_iso_in_aggIso ( agg_region, agg_region_disagg_isos,
                                                          cdiac_split_srb_Fixed, population, CDIAC_CEDS_years )


# Cast back into wide format
    cdiac_split_final <- cdiac_split_sdn_Fixed %>%
        tidyr::spread( Years, Emissions )

# -----------------------------------------------------------------------------------------------------------
# 6. Add zero values for nations too small for CDIAC
#    Add blank entries for any countries not in CDIAC

# Define countries to add to cdiac data
    non_cdaic_countries <- MCL$iso[ MCL$iso %!in%
                                    unique( cdiac_split_final$iso ) ]

# Only keep countries with 1 for the final_data_flag in the MCL, Kosovo, and Guam
# TODO: when Kosovo and Guam issue in Master Country List is resolved, the call to their isos below can be removed
    non_cdaic_countries <-
          non_cdaic_countries[ non_cdaic_countries %in%
                               MCL[ which( MCL$final_data_flag == 1 | MCL$iso %in% c( "srb (kosovo)", "gum" ) ), 'iso' ] ]

    non_cdaic_countries <- unique( non_cdaic_countries )

# Drop "global" country
# TODO: These provides 0 values for the following isos: esh, tkl. Look into whether or not theses isos are contained
#       within an aggregate CDIAC iso that could be broken out with UN population data
    non_cdaic_countries <- non_cdaic_countries[ non_cdaic_countries %!in%
                                                  'global' ]

# Create a df with all iso/fuel combos to add
    add_zeros <- data.frame( iso = rep( non_cdaic_countries,
                                        each = length( cdiac_fuels ) ),
                             fuel = rep( cdiac_fuels,
                                         times = length( non_cdaic_countries ) ),
                             units = final_units )

    add_zeros[ X_cdiac_years ] <- 0

# Combine needed rows to CDIAC, so even countries w/o data are represented
    cdiac_disaggregated <- dplyr::bind_rows( add_zeros, cdiac_split_final )

# Clean and reformat
    cdiac_disaggregated$units <- final_units
    cdiac_disaggregated <- arrange_( cdiac_disaggregated,
                                     c( 'iso', 'fuel', 'units',
                                        X_cdiac_years ) )

# -----------------------------------------------------------------------------------------------------------
# 7. Corrections
#    Manual overwrite of data discontinuities using interpolation overwrites

    cdiac_smooth <- cdiac_disaggregated

# CORRECTIONS-1950 discontinuity, linear interpolate between 1952 and last zero value
# make nonzeros NA
    cdiac_smooth[ which( cdiac_smooth$iso %in% c( 'abw', 'arg', 'bhr', 'cuw',
                                                  'tto', 'irn', 'ven', 'brn',
                                                  'kwt' ) &
                         cdiac_smooth$fuel == 'liquid_fuels' ),
                  paste0( 'X', historical_pre_extension_year:1951 ) ] <-
      replace( cdiac_smooth[ which( cdiac_smooth$iso %in% c( 'abw', 'arg', 'bhr',
                                                             'cuw', 'tto', 'irn',
                                                             'ven', 'brn', 'kwt' ) &
                                    cdiac_smooth$fuel == 'liquid_fuels' ),
                             paste0( 'X', historical_pre_extension_year:1951 ) ],
                ( cdiac_smooth[ which( cdiac_smooth$iso %in%
                                           c( 'abw', 'arg', 'bhr', 'cuw', 'tto',
                                              'irn', 'ven', 'brn', 'kwt' ) &
                                       cdiac_smooth$fuel == 'liquid_fuels' ),
                         paste0( 'X', historical_pre_extension_year:1951 ) ] ) != 0,
                 NA )

# Replace NAs in 1750 with 0 so we can interpolate (there is an edge value)
    cdiac_smooth[ which( is.na( cdiac_smooth$X1750 ) ), 'X1750' ] <- 0

# Various country-specific corrections; set areas to smooth to NA so that we can interpolate
    cdiac_smooth[ which( cdiac_smooth$iso == 'kwt' ),
                  paste0( 'X', 1960:1969 ) ] <- NA
    cdiac_smooth[ which( cdiac_smooth$iso == 'sau' ),
                  paste0( 'X', 1947 ) ] <- NA
    cdiac_smooth[ which( cdiac_smooth$iso == 'irn' ),
                  paste0( 'X', 1953:1954 ) ] <- NA
    cdiac_smooth[ which( cdiac_smooth$iso == 'irq' ),
                  paste0( 'X', 1949:1955 ) ] <- NA
    cdiac_smooth[ which( cdiac_smooth$iso == 'mex' ),
                  paste0( 'X', 1912:1938 ) ] <- NA

# Interpolate over the NA regions where smoothing was needed
    cdiac_smooth[ X_cdiac_years ] <-
             interpolate_NAs( cdiac_smooth[ , X_cdiac_years ] )

    cdiac_smooth[ which( cdiac_smooth$iso == 'abw' ), ] <-
             replace( cdiac_smooth[ which( cdiac_smooth$iso == 'abw' ), ],
                      is.na( cdiac_smooth[ which( cdiac_smooth$iso == 'abw' ), ] ),
                      0 )

# -------------------------------------------------------------------------------
# 8. Recalcuate total CO2 after corrections
#    Sum all emissions per country and add these sums to the df as Total_CO2

    cdiac_final <- cdiac_smooth

# Drop "Total_CO2" fuels
    cdiac_final <- cdiac_final[ which( cdiac_final$fuel %!in% 'Total_CO2' ), ]

# Prepare a sum df
    cdiac_sum <- cdiac_final[ which( cdiac_final$fuel %in%
                                       c( "solid_fuels", "liquid_fuels",
                                          "gas_fuels", "cement_production",
                                          "gas_flaring", "bunker_fuels" ) ), ]
# Sum emissions by iso to obtain total CO2
    total_CO2 <- aggregate( cdiac_final[ X_cdiac_years ],  ### Should this be cdiac_sum?
                            by = list( iso = cdiac_final$iso ),
                            FUN = sum )
    total_CO2$fuel <- 'Total_CO2'

# Bind this value into
    cdiac_final <- dplyr::bind_rows( cdiac_final, total_CO2 )

# -----------------------------------------------------------------------------------------------------------
# 9. Add entry for "liquid and gas fuels"

# Separate out liquid fuels and gas fuels
    cdiac_liquid_and_gas <-
                 cdiac_final[ which( cdiac_final$fuel %in%
                                     c( 'liquid_fuels', 'gas_fuels' ) ), ]

# Sum liquid/gas by country
    cdiac_liquid_and_gas <-
                 aggregate( cdiac_liquid_and_gas[X_cdiac_years],
                            by = list( iso = cdiac_liquid_and_gas$iso ),
                            FUN = sum )

# Add fuel and unit tags
    cdiac_liquid_and_gas$fuel <- 'liquid_and_gas_fuels'
    cdiac_liquid_and_gas$units <- final_units

# Reorder columns
    cdiac_liquid_and_gas <- cdiac_liquid_and_gas[ , c( 'iso', 'fuel',
                                                       'units', X_cdiac_years ) ]

# Incorporate "liquid and gas" into cdiac_final
    cdiac_final <- dplyr::bind_rows( cdiac_final, cdiac_liquid_and_gas )

# Sort and organize
    cdiac_final <- cdiac_final[ , c( 'iso', 'fuel', X_cdiac_years ) ]
    cdiac_final <- cdiac_final[ with( cdiac_final, order( iso, fuel ) ), ]

# -----------------------------------------------------------------------------------------------------------
# 10. Extend CDIAC cement emissions using USGS cement production data
#    Routine to make USGS country mapping

# Use the procUSGS function to handle all usgs cement datasheets
    usgs <- lapply( usgs_data_in, procUSGS )

# Extract all isos/countries into a df list
    all_countries <- lapply( usgs, function( df )
                                   return( df[ c( "iso", "Country" ) ] ) )

# Incorporate all countries into a single dataframe
    all_countries <- do.call( "rbind", all_countries ) %>%
                                              unique()

# Select only isos
    all_iso <- dplyr::filter( all_countries, !is.na( iso ) ) %>%
                                        dplyr::select( iso ) %>%
                                             unique()
# Sort isos
    all_iso <- sort( all_iso$iso )

# Find all countries with no isos
    unmatched_countries <- dplyr::filter( all_countries, is.na( iso ) ) %>%
                                               dplyr::select( Country ) %>%
                                                        unique()
    unmatched_countries <- sort( unmatched_countries$Country )

# Print a warning if some countries weren't mapped
    if ( length( unmatched_countries ) > 0 ) {
      unmatched_countries <- paste0( "\"", unmatched_countries, "\"" )
      warning( paste0( "The following countries were unmatched.
                        Please update mapping:\n",
                       paste( unmatched_countries, collapse = ", " ) ) )
    }

# Make template df with all iso and years
    all_years <- lapply( usgs, function( df )
                           return( names( df )[ grepl( "X", names( df ) ) ] ) )

# Add all years to a list
    all_years <- Reduce( c, all_years ) %>%
                               unique() %>%
                                 sort()

# Create a template for cement, years by iso
    cement <- merge( data.frame( iso = all_iso ),
                     data.frame( year = all_years ), all = T )

# Add a blank NA column
    cement$value <- NA

# Add data to template. Iterate through each df list in USGS
    for ( i in seq_along( usgs ) ) {
        df <- usgs[[ i ]]
        df$Country <- NULL
    # Melt to long by iso
        df <- melt( df, id = "iso" ) %>%
           filter( !is.na( value ) )
    # Set column names
        names( df )[ names( df ) == "variable" ] <- "year"
        names( df )[ names( df ) == "value" ] <- "value_new"
    # Merge into template
        cement <- merge( cement, df, all.x = T )
        cement$value[ is.na( cement$value ) ] <-
          cement$value_new[ is.na( cement$value ) ]
        cement$value_new <- NULL
    }

# Clean cement data and cast to wide
    cement$units <- "kt"
    cement <- cast( cement, iso + units ~ year )
    Xyears <- names( cement )[ grepl( "X", names( cement ) ) ]
    cement <- cement[ rowSums( is.na( cement[, ] ) ) < length( Xyears ), ]  # Drop rows of all NA

# Give all scg cement before 2005 to srb
    cement[ cement$iso == "srb", paste0( "X", 1998:2005 ) ] <-
      cement[ cement$iso == "scg", paste0( "X", 1998:2005 ) ]
    cement <- dplyr::filter( cement, iso != "scg" )

# Make all NA 2013 production zero (sgp only)
    cement$X2013[ is.na( cement$X2013 ) ] <- 0

# Disaggregate 2014/2015 production based on 2013 shares
# Find iso/years without 2014 data, and select the corresponding 2013 data
    shares <- dplyr::filter( cement, is.na( X2014 ) ) %>%
                          dplyr::select( iso, X2013 )

# Find what pct of 2013 each country holds
    shares$ratio <- shares$X2013 / sum( shares$X2013 )

# Cement data to add to 2014 is the same proportion that the country had of 2013
# data
    cement_other_X2014_X2015 <- dplyr::filter( cement, iso == "OTHER" ) %>%
                                          dplyr::select( X2014, X2015 ) %>%
                  merge( dplyr::select( shares, iso, ratio ), all = T ) %>%
              dplyr::mutate( X2014 = X2014*ratio, X2015 = X2015*ratio )

# Deselect 2015 and 2014 in original df data and replace with new values
    cement_other <- dplyr::filter( cement, is.na( X2014 ) ) %>%
                    dplyr::select( -X2014, -X2015 ) %>%
                    merge( dplyr::select( cement_other_X2014_X2015, -ratio ) )

# Combine all data with cement_other
    cement_all <- dplyr::filter( cement, !is.na( X2014 ), iso != "OTHER" ) %>%
                                              dplyr::bind_rows( cement_other ) %>%
                                                     dplyr::arrange( iso )

# Interpolate NAs
    cement_all[ , Xyears ] <- interpolate_NAs( cement_all[ , Xyears ] )

# Make remaining edge NAs 0
    cement_all[ is.na( cement_all ) ] <- 0

# Calculate EFs over time using USGS cement production
    X_cement_years <- paste0( "X", 1998:2011 )
    X_cement_ext_years <- paste0( "X", 2012:2015 )

# Extract X CDIAC years
    X_cdiac_years_ext <- paste0( 'X', cdiac_start_year:cdiac_end_year_cement )

# Get cement_production from CDIAC and arrange by iso
    cdiac_ext <- dplyr::filter( cdiac_final, fuel == "cement_production",
                         iso %in% cement_all$iso ) %>%
                 dplyr::arrange( iso )

# Get USGS cement data corresponding to CDIAC data
    cement_prod <- dplyr::filter( cement_all, iso %in% cdiac_ext$iso ) %>%
                   dplyr::arrange( iso )

# EF dataframe
    cement_ef <- cdiac_ext

# Calculate emissions factors as CDIAC emissions over cement activity
    cement_ef[ , X_cement_years ] <- cdiac_ext[ , X_cement_years ] /
                                     cement_prod[ , X_cement_years ]

# Extend cement emissions to 2015 using production and 2009-2011 average EFs
    cement_ef$avg <- rowMeans( cement_ef[ paste0( "X", 2009:2011 ) ], na.rm = T )
    cdiac_ext[ , X_cement_ext_years ] <- cement_prod[ , X_cement_ext_years ] *
                                         cement_ef$avg

# Diagnostic cement EF: use only selected columns to write out
    cement_ef_diag <- cement_ef[ c( "iso", "fuel", X_cement_years ) ]

# Combine extended data back
    cdiac_final <- dplyr::filter( cdiac_final, paste( iso, fuel ) %!in%
                           paste( cdiac_ext$iso, cdiac_ext$fuel ) ) %>%
                   dplyr::bind_rows( cdiac_ext ) %>% dplyr::arrange( iso, fuel ) %>%
                                                       data.frame()

# Set NA values to zero
    cdiac_final[ is.na( cdiac_final ) ] <- 0

# -----------------------------------------------------------------------------
# 11. Summary
# non combustion
    cdiac_cement <- cdiac_final[ which( cdiac_final$fuel %in%
                                          c( "cement_production" ) ), ]
    cdiac_total <- cdiac_final[ which( cdiac_final$fuel %in%
                                          c( "Total_CO2") ), ]

# Figure region and cdiac fuel
    cdiac_region_fuel <- cdiac_final

# Map figure region to CDIAC
    cdiac_region_fuel$Figure_Region <- MCL[ match( cdiac_region_fuel$iso, MCL$iso ),
                                            "Figure_Region" ]

# Aggregate to figure region
    cdiac_region_fuel <- aggregate( cdiac_region_fuel[ X_cdiac_years ],
                                    by = list( Figure_Region =
                                                 cdiac_region_fuel$Figure_Region,
                                               fuel = cdiac_region_fuel$fuel ),
                                    FUN = sum )
    cdiac_region_fuel <- cdiac_region_fuel[ with( cdiac_region_fuel,
                                                  order( Figure_Region, fuel ) ), ]

# Aggregate to iso and fuel
    cdiac_iso_fuel <- cdiac_final
    cdiac_iso_fuel <- aggregate( cdiac_iso_fuel[ X_cdiac_years ],
                                    by = list( iso = cdiac_iso_fuel$iso,
                                               fuel = cdiac_iso_fuel$fuel ),
                                    FUN = sum )
    cdiac_iso_fuel <- cdiac_iso_fuel[ with( cdiac_iso_fuel,
                                            order( iso, fuel ) ), ]

# Obtain only solid fuels
    cdiac_solid_fuel <- cdiac_final[ which( cdiac_final$fuel ==
                                                   'solid_fuels' ), ]

# Add up all solid fuels
    cdiac_solid_fuel_cumulative <- melt( cdiac_solid_fuel,
                                         id = c( "iso", "fuel" ) )
    cdiac_solid_fuel_cumulative <- dplyr::arrange( cdiac_solid_fuel_cumulative,
                                                   iso, fuel, variable ) %>%
                                  ddply( .( iso, fuel ), function( df ) {
                                      df$value <- cumsum( df$value )
                                      return( df )
                                  } )
    cdiac_solid_fuel_cumulative <- cast( cdiac_solid_fuel_cumulative )
    cdiac_solid_fuel_cumulative$fuel <- "solid_fuels_cumulative"

# CDIAC global total for each category
    cdiac_fuel_cats <- unique( cdiac_final$fuel )
    cdiac_cats_total <-
        sapply( cdiac_fuel_cats, function( cdiac_fuel_cat ) {
            temp_cat_data <- subset( cdiac_final,
                                     cdiac_final$fuel == cdiac_fuel_cat )
            temp_cat_agg <- aggregate( temp_cat_data[ , X_cdiac_years_ext ],
                                       by = list( temp_cat_data$fuel ),
                                       FUN = sum )
        } )
    cdiac_cats_total <- t( cdiac_cats_total )
    cdiac_cats_total <- as.data.frame( cdiac_cats_total, row.names = F )
    colnames( cdiac_cats_total )[ 1 ] <- 'fuel'
    cdiac_cats_total$iso <- 'global'
    cdiac_cats_total <- cdiac_cats_total[ , c( 'iso', 'fuel',
                                               X_cdiac_years_ext ) ]

# Add the totals back to cdiac_final as global iso
    cdiac_final <- rbind( cdiac_final, cdiac_cats_total )
    cdiac_final <- cdiac_final[ order( cdiac_final$iso ), ]

# -----------------------------------------------------------------------------------------------------------
# 12. Output

# Intermediate output
    writeData( cdiac_final, domain = "MED_OUT",
               fn = paste0( "E.CO2_CDIAC_inventory" ) )
    writeData( cdiac_cement, domain = "MED_OUT",
               fn = paste0( "E.CO2_CDIAC_Cement" ) )
    writeData( cdiac_total, domain = "MED_OUT",
               fn = paste0( "E.CO2_CDIAC_Total_CO2" ) )
    writeData( cdiac_liquid_and_gas, domain = "MED_OUT",
               fn = paste0( "E.CO2_CDIAC_liquid_and_gas" ) )
    writeData( cdiac_solid_fuel, domain = "MED_OUT",
               fn = paste0( "E.CO2_CDIAC_solid_fuel" ) )
    writeData( cdiac_solid_fuel_cumulative, domain = "MED_OUT",
               fn = paste0( "E.CO2_CDIAC_solid_fuel_cumulative" ) )

# Diagnostic output
    writeData( cdiac_region_fuel, domain = "DIAG_OUT",
               fn = "E.CO2_CDIAC_by_figure_region_CDIAC_fuel")
    writeData( cdiac_iso_fuel, domain = "DIAG_OUT",
               fn = "E.CO2_CDIAC_by_iso_CDIAC_fuel" )
    writeData( cement_ef_diag, "DIAG_OUT", "E.CDIAC_cement_EF" )
    writeData( cement_all, "DIAG_OUT", "E.USGS_cement_production" )

    logStop()

# END
