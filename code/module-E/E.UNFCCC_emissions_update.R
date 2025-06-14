# ------------------------------------------------------------------------------
# Program Name: E.UNFCCC_emissions_update.R
# Author(s): Patrick O'Rourke
# Date Last Updated: April 16, 2019
# Program Purpose: To read in and reformat UNFCCC emissions data.
# Input Files: [em]_Annual_Net emissions_removals_in_kt.csv, Master_Country_List.csv
# Output Files: E.[em]_UNFCCC_inventory_update.csv
# Notes: UNFCCC  Emissions are provided from 1990-2016.
# TODO: Check if 2016 data ("Last Inventory Year (2016)" column in unprocessed data) is
#       in fact historical data.

# ------------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "analysis_functions.R" ) # Any additional function files required
    log_msg <- "Initial reformatting of the UNFCCC emissions inventories..." # First message to be printed to the log
    script_name <- "E.UNFCCC_emissions_update.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "CO2"

# -----------------------------------------------------------------------------------------------------------

# 0.5 Settings

if( ! ( em %in% c( 'SO2', 'CO', 'NMVOC', 'NOx', 'CO2', 'CH4', 'N2O', 'NH3' ) ) ){

    printLog ( paste0 ( "Selected em is not in E.UNFCCC_emissions_update.R and will",
                        " not be run. Dummy file will be generated for this em..." ) )

} else{

    UNFCCC_years <- paste0( 1990:2021 )
    UNFCCC_years_with_Xs <- paste0( "X", UNFCCC_years )

# -----------------------------------------------------------------------------------------------------------

# 1. Read in files

    UNFCCC <- readData( "UNFCCC__NEW_IN",  paste0(em, "_Annual_Net_emissions_removals_in_kt" ), header = FALSE )

    MCL <- readData( "MAPPINGS", "Master_Country_List" ) %>%
        dplyr::select( iso, UNFCCC ) %>%
        dplyr::distinct( ) %>%
        dplyr::filter( !( is.na ( UNFCCC ) ) )

# -----------------------------------------------------------------------------------------------------------

# 2. Formatting Data

# Fix column names, add units column, remove rows not needed
colnames(UNFCCC) = UNFCCC[ 3, ]
colnames(UNFCCC)[1] = "UNFCCC"
if ( UNFCCC[4,4] == "kt") {
    UNFCCC$units <- UNFCCC[4,4] # Select unit from units row
} else {
    stop("Check units and/or format in UNFCCC data.")
}
# TODO: Remove hard coded last inventory year below
UNFCCC_clean <- UNFCCC %>%
    dplyr::slice( -( 1:4 ) ) %>%
    dplyr::rename( "2021" = "Last Inventory Year (2021)", sector = Year ) %>%
    dplyr::mutate( UNFCCC = str_replace(UNFCCC, 'Türkiye', 'Turkey')) %>%


# Add X's to years, and fix emissions values (remove commas, make numeric, and remove values that aren't numbers)
    tidyr::gather( key = years, value = Emissions, UNFCCC_years ) %>%
    dplyr::mutate( years = paste0( "X", years ),
                   Emissions_new = gsub(",", "", Emissions),
                   Emissions_new = as.numeric( Emissions_new ) ) %>%
    dplyr::select( -Emissions ) %>%
    dplyr::filter( ! (is.na ( sector ) ) ) %>%
    tidyr::spread( years, Emissions_new ) %>%
    dplyr::select( UNFCCC, sector, units, UNFCCC_years_with_Xs ) %>%

# Map to CEDS isos
    dplyr::left_join( MCL, by = "UNFCCC" ) %>%
    dplyr::select( iso, UNFCCC, sector, units, UNFCCC_years_with_Xs ) %>%

# Fix sector names - remove words after sectors, then remove "." if it is the final character in a sector
#                   (sectors 1, 2, 3...), fix sectors that were originally only words (int. aviation and bunkers...)
    dplyr::mutate( new_sector_names_remove_text = if_else( sector %in% c( "International Aviation", "International Bunkers",
                                                                          "International Navigation", "Multilateral Operations",
                                                                          "Waste Incineration with Energy Recovery included as Biomass",
                                                                          "Waste Incineration with Energy Recovery included as Fossil Fuels" ),
                                                           sector, gsub(" .*", "", sector ) ) ) %>%
    dplyr::mutate( new_sector_names_remove_ending_period = if_else( grepl('..$', new_sector_names_remove_text ),
                                                                    gsub("[.]$", "", new_sector_names_remove_text ),
                                                                    new_sector_names_remove_text ) ) %>%
    dplyr::select( iso, new_sector_names_remove_ending_period, units, UNFCCC_years_with_Xs ) %>%
    dplyr::rename( sector = new_sector_names_remove_ending_period ) %>%

# Remove rows which are NA for all years
    filter_at( vars( UNFCCC_years_with_Xs ), any_vars(! (is.na( . ) ) ) )

# ------------------------------------------------------------------------------
# 3. Removed "Bad" Data

    if (em == 'CH4'){

        remove_iso <- NULL

    } else if (em == 'N2O'){

        remove_iso <- NULL

    } else {

      remove_iso <- NULL

    }

    UNFCCC_final <- UNFCCC_clean %>%
        dplyr::filter( !( iso %in% remove_iso ) )

}

# ------------------------------------------------------------------------------

#4. Dummy files - If length( UNFCCC) == 0 or if UNFCCC_final doesn't exist
#                 (if there is no data to process for this emissions species), create a dummy file.

    if( exists( "UNFCCC_final" ) == "FALSE" ){

      UNFCCC_final <- data.frame()


    } else if( length( UNFCCC_final ) == 0 ){

      UNFCCC_final <- data.frame()

    } else {

      UNFCCC_final <- UNFCCC_final

    }

# ------------------------------------------------------------------------------

# 5. Meta Data



# ------------------------------------------------------------------------------

# 6. Output

    writeData( UNFCCC_final, domain = "MED_OUT",
               fn = paste0( "E.", em, "_UNFCCC_update_inventory" ), meta = TRUE )

# Every script should finish with this line
    logStop()

# END
