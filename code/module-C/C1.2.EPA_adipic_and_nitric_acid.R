# ------------------------------------------------------------------------------
# Program Name: C1.2.EPA_adipic_and_nitric_acid.R
# Author(s): Patrick O'Rourke
# Date Last Modified: January 16, 2020
# Program Purpose: To reformat EPA adipic and nitric acid production process emissions.
# Input Files: EPA_nonCO2_GHG-proj-data-annex_Sept2019.csv, EPA_nonCO2_GHG-country_map.csv,
#              Master_Country_List.csv,
# Output Files: C.N2O_EPA_NC_adipic_and_nitric_acid.csv
# Notes:
# TODO:
# -----------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if( "input" %in% dir( ) ) "code/parameters/" else "../code/parameters/"

# Universal header file - provides logging, file support, etc.
headers <- c( "common_data.R","data_functions.R", "analysis_functions.R",
              "process_db_functions.R", 'timeframe_functions.R') # Additional function files required.
log_msg <- paste0( "Processing EPA adipic and nitric acid default process emissions data..." ) # First message to be printed to the log
script_name <- "C1.2.EPA_adipic_and_nitric_acid.R"
source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Settings and script constants

# Define emissions species variable
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "N2O"

# ------------------------------------------------------------------------------
# 2. Input

# US EPA non-CO2 emissions estimates and projections
EPA_non_CO2 <- readData( "EM_INV", 'EPA_nonCO2_GHG-proj-data-annex_Sept2019',
                         domain_ext = "USA/" )

# US EPA non-CO2 emissions country map
EPA_non_CO2_country_map <- readData( "MAPPINGS", 'EPA_nonCO2_GHG-country_map',
                          domain_ext = "US_EPA/" )

# Master Country List mapping file
Master_Country_List <- readData( "MAPPINGS", "Master_Country_List" )

# ------------------------------------------------------------------------------
# 3. Check mapping file, set script constants

# Check EPA region mapping file for CEDS isos
iso_check( data_to_check = EPA_non_CO2_country_map,
           data_to_check_iso_colname = "iso",
           provided_data_needs_all_ceds_isos = T,
           provided_data_contains_unique_isos_only = T )

# EPA historical and BAU projected data years.
# "Projections results are a “business-as-usual” (BAU) scenario with emission rates consistent with
#  historical levels and do not include future effects of policy changes."
# Source: US EPA, "Global Non-CO2 Greenhouse Gas Emission Projections & Mitigation 2015–2050",
#  September 2019, page 2.
#  https://www.epa.gov/sites/production/files/2019-09/documents/epa_non-co2_greenhouse_gases_rpt-epa430r19010.pdf
EPA_NON_CO2_HISTORICAL_START_YEAR <- 1990
EPA_NON_CO2_HISTORICAL_END_YEAR <- 2015
EPA_NON_CO2_BAU_PROJECTION_START_YEAR <- 2016
EPA_NON_CO2_BAU_PROJECTION_END_YEAR <- 2050

ALL_EPA_NON_CO2_YEARS <- EPA_NON_CO2_HISTORICAL_START_YEAR : EPA_NON_CO2_BAU_PROJECTION_END_YEAR

if( end_year > EPA_NON_CO2_HISTORICAL_END_YEAR ){

    printLog( "EPA non-CO2 GHG business-as-usual projections will be used for default adipic and nitric process",
              "emissions from", EPA_NON_CO2_BAU_PROJECTION_START_YEAR, "to", paste0( end_year, "," ), "since the historical data",
              "ends in", EPA_NON_CO2_HISTORICAL_END_YEAR, "..." )

}

# EPA historical and BAU/ on-the-books years within CEDS years
EPA_YEARS_USING <- subset( ALL_EPA_NON_CO2_YEARS,
                           ALL_EPA_NON_CO2_YEARS %in% c( start_year : end_year ) )

X_EPA_YEARS_USING <- paste0( "X", EPA_YEARS_USING )

# EPA N2O GWP (AR4)
# Source: US EPA, "Global Non-CO2 Greenhouse Gas Emission Projections & Mitigation 2015–2050",
# September 2019, page 4.
# https://www.epa.gov/sites/production/files/2019-09/documents/epa_non-co2_greenhouse_gases_rpt-epa430r19010.pdf
# "Global Non-CO2 Greenhouse Gas Emission Projections & Marginal Abatement Cost Analysis: Methodology Documentation",
# September 2019, page 3-3
# https://www.epa.gov/sites/production/files/2019-09/documents/nonco2_methodology_report.pdf
EPA_N2O_GWP <- 298

# kt per MMT (million metric tonnes, or mega tonnes)
KT_PER_MMT <- 1000

# ------------------------------------------------------------------------------
# 4. Process the EPA EPA_non_CO2 data

# Filter for emissions species of interest and acid production sectors
epa_acid_disagg_sectors <- c( "Adipic", "Nitric" )

EPA_acid_emissions <- EPA_non_CO2 %>%
    dplyr::filter( source == "NitricAdipic",
                   subsource %in% epa_acid_disagg_sectors ) %>%
    dplyr::filter( gas ==  em )

# Retain data up to end_year in global_settings
EPA_acid_emissions_relevant_yrs <- EPA_acid_emissions %>%
    dplyr::filter( year %in% EPA_YEARS_USING )

# Map EPA regions to CEDS isos. Stop if any EPA regions are mapped to NA for CEDS iso & have emissions
# for the acid production sectors
EPA_acids_mapped <- EPA_acid_emissions_relevant_yrs %>%
    dplyr::left_join( EPA_non_CO2_country_map, by = c( "country" =  "EPA_nonCO2_GHG" ) ) %>%
    dplyr::select( -Note )

EPA_acids_region_check <- EPA_acids_mapped %>%
    dplyr::filter( is.na( iso ) )

if( any( EPA_acids_region_check$value > 0 ) ){

    stop( "An EPA non-CO2 GHG region has been mapped to NA for CEDS iso, but has nitric and/or adipic ",
          "acid production process emissions. Please check the EPA non-CO2 GHG data and mapping file..." )

}

# Filter out regions mapped to NA, and aggregate by sector, year, and iso
EPA_acids_region_aggregated <- EPA_acids_mapped %>%
    dplyr::filter( !is.na( iso ) ) %>%
    dplyr::select( -country, -sector ) %>%
    dplyr::group_by( source, subsource, gas, year, unit, iso ) %>%
    dplyr::summarize_all( funs( sum( ., na.rm = TRUE ) ) ) %>%
    dplyr::ungroup( )

# Convert emissions to kt of the gas in queestion, instead of MMTCO2e,
# fix units, and rename sectors
EPA_acids_kt <- EPA_acids_region_aggregated %>%
    dplyr::mutate( value = value / EPA_N2O_GWP * KT_PER_MMT,
                   unit = "kt",
                   year = paste0( "X", year ) ) %>%
    dplyr::rename( units = unit,
                   sector = subsource ) %>%
    dplyr::select( iso, sector, units, year, value ) %>%
    tidyr::spread( year, value )

# Rename sectors and define fuel as "process"
EPA_CEDS_sectors <- EPA_acids_kt %>%
    dplyr::mutate( sector = if_else( sector == "Nitric", "2B2_Chemicals-Nitric-acid",
                            if_else( sector == "Adipic", "2B3_Chemicals-Adipic-acid", NA_character_ ) ),
                    fuel = "process" ) %>%
    dplyr::select( iso, sector, fuel, units, X_EPA_YEARS_USING )

# ------------------------------------------------------------------------------
# 4. Add missing isos with values of 0 for emissions - assumes EPA data is a complete estimate
#    of world Nitric and Adipic N2O emissions

#   Subset isos which aren't final CEDS isos and have emissions other than 0 in any year
MCL_final_isos <- Master_Country_List %>%
    dplyr::filter( final_data_flag == 1 | iso %in% c( "gum", "srb (kosovo)" ) ) %>%  # TODO: when this issue ("gum", "srb (kosovo)" ) is fixed in the MCL this rule can be removed
    dplyr::select( iso ) %>%
    dplyr::distinct( )

#  If any final CEDS isos are not in the EPEA data add them to the data frame
#  Currently added with values all equal to zero (assumes EPA data is a complete estimate
#    of world Nitric and Adipic N2O emissions)
#    TODO: Disaggregate smaller isos from larger isos if applicable (if they are known to produce nitric
#          or adipic acid and their emissions are counted within a larger iso currently)
final_isos_not_in_EPA_data <- subset( MCL_final_isos$iso, MCL_final_isos$iso %!in% EPA_CEDS_sectors$iso &
                                                          MCL_final_isos$iso != "global" )

if( length( final_isos_not_in_EPA_data ) > 0 ){

#   Define function to add isos to the emissions data for each relevant sector
#   TODO: A function similar to this is used in a few scripts (such as mod. C Edgar processing)
#         They could probably use a more flexible version of the same function.
    add_isos_to_EPA <- function( iso_in ){

        new_iso <- EPA_CEDS_sectors %>%
            dplyr::mutate( iso = paste0( iso_in ) ) %>%
            dplyr::mutate_at( X_EPA_YEARS_USING, funs( identity( 0 ) ) ) %>%
            dplyr::distinct( )
    }

#   Add each missing iso(s) with all NA entries
    new_isos <- lapply( final_isos_not_in_EPA_data, add_isos_to_EPA ) %>%
        dplyr::bind_rows(  )

    EPA_CEDS_sectors <- EPA_CEDS_sectors %>%
        dplyr::bind_rows( new_isos )

}

#   Check that the emissions data now only has final CEDS isos
    EPA__final_unique_isos <- EPA_CEDS_sectors %>%
        dplyr::select( iso ) %>%
        dplyr::distinct( )

    if( nrow( EPA__final_unique_isos ) < nrow( MCL_final_isos %>% dplyr::filter( iso != "global" ) ) ){

        stop( "EPA emissions data is missing at least one CEDS final isos. See ", script_name )

    } else if( nrow( EPA__final_unique_isos ) > nrow( MCL_final_isos %>% dplyr::filter( iso != "global" ) ) ){

        stop( "EPA emissons data contains at least one iso which is not a final CEDS isos. See ", script_name )

    }

#   Add sectors that are missing for certain isos as all NAs, if necessary
    EPA_CEDS_sectors_iso_sector_combos <- EPA_CEDS_sectors %>%
        expand( iso, sector )

    if( nrow( EPA_CEDS_sectors_iso_sector_combos ) > nrow( EPA_CEDS_sectors ) ) {

        EPA_CEDS_sectors <- EPA_CEDS_sectors %>%
            dplyr::full_join( EPA_CEDS_sectors_iso_sector_combos, by = c( "iso", "sector" ) ) %>%
            dplyr::mutate( fuel = "process", units = "kt" ) %>%
            dplyr::mutate_at( .vars = X_EPA_YEARS_USING, funs( if_else( is.na( . ), 0, . ) ) )

    }

# ------------------------------------------------------------------------------
# 5. Output data

# Save EPA adipic and nitric acid emissions data
writeData( EPA_CEDS_sectors, domain = "MED_OUT",
           fn = paste0( "C.", em,  "_EPA_NC_adipic_and_nitric_acid" ) )

logStop( )
# END


