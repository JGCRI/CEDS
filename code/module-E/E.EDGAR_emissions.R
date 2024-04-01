# ------------------------------------------------------------------------------
# Program Name:E.EDGAR_emissions.R
# Author(s): Noah Prime, Harrison Suchyta, Rachel Hoesly
# Date Last Modified: May 3, 2023
# Program Purpose: Intended to reformat EDGAR default emissions data and add it to
#                  the data base for the relevant emissions species
# Input Files: relevant EDGAR emissions data ( EDGAR = [em]_1970_[edgar_end_year].xls )
#           This script processes EDGARv6.1 data.
# Output Files: E.EDGAR_Emissions_[em].csv
# TODO:
#
# Notes:
# -----------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Universal header file - provides logging, file support, etc.
headers <- c( "common_data.R","data_functions.R", "analysis_functions.R",
              "process_db_functions.R", 'timeframe_functions.R') # Additional function files required.
log_msg <- paste0( "Processing EDGAR non-combustion default emissions data..." ) # First message to be printed to the log
script_name <- "E.EDGAR_emissions.R"
source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Settings

# Define emissions species variable
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "CO2"

# Input domain
domain <- "EM_INV"
domain_ext <- "EDGAR/"

# --------------------------------------------------------------------------------
# NOTE
# If EDGAR emissions are updated, make sure constants are updated in common_data.R
# --------------------------------------------------------------------------------
if( em %in% c('CH4','N2O','CO2') ) {
     EDGAR_end_year = EDGAR_end_year_GHG # GHG Emissions are provided for more years than air pollutants
}

EDGAR_years <- EDGAR_start_year : EDGAR_end_year
X_EDGAR_years <- paste0( 'X', EDGAR_start_year : EDGAR_end_year )


# ------------------------------------------------------------------------------
# 2. Input

# File settings for EDGARv6.1 Air pollutants and EDGARv8 GHG
fn <- c( paste0( em, "_", EDGAR_start_year, "_",
                 EDGAR_end_year ), ".xlsx")

if( em == 'CH4') fn [1] <- "EDGAR_CH4_1970_2022"
if( em == 'N2O') fn [1] <- "EDGAR_N2O_1970_2022"
if( em == 'CO2') fn [1] <- "IEA_EDGAR_CO2_1970_2022"

sheet_to_use <- paste0(em, "_IPCC1996" )

if( em %in% c('CH4','N2O','CO2') ) {
    sheet_to_use <- "IPCC 1996"
}

rows_to_skip <- 9

# Read in EDGAR data
edgar_in <- readData( domain, domain_extension = domain_ext,
                   file_name = fn[ 1 ], extension = fn[ 2 ],
                   sheet_selection = sheet_to_use, skip = rows_to_skip,
                   missing_value = c( "", "NULL" ) )

# ------------------------------------------------------------------------------
# 3. Reformatting

# Add iso, units, group by sector
# combine fossil and bio emissions for each year/sector
# convert to long form for processeing

edgar_long <- edgar_in %>%
    mutate(iso = tolower(Country_code_A3)) %>%
    dplyr::rename(sector = ipcc_code_1996_for_standard_report,
                  sector_description = ipcc_code_1996_for_standard_report_name) %>%
    mutate(units = 'kt') %>%   # Define units as kt (as EDGAR data is in Gg, which is the same as kt)
    dplyr::select( iso, sector, units, fossil_bio, sector_description, starts_with("Y_")) %>%
    tidyr::gather(year, value, -iso, -sector, -units, -fossil_bio, -sector_description) %>%
    mutate(year = sub("Y_",'X',year)) %>%
    mutate(value = as.numeric(value)) %>%
    replace_na(list(value = 0)) %>%
    dplyr::group_by( iso, sector, units, sector_description, year ) %>% # we want both bio and fossil emissions for most sectors
    dplyr::summarize(value = sum(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    mutate(value = ifelse(value <0, 0, value)) # replace negative emissions with zero

# back to wide form
edgar_wide <- edgar_long %>%
    tidyr::spread(year, value)

# Remove rows with all NA's
edgar_wide <- edgar_wide[ apply( X = edgar_wide[ , X_EDGAR_years ],
                       MARGIN = 1, function( x ) ( !all( x == 0 ) ) ) ,]

# Check for negative emissions
check <- edgar_long %>%
    filter(value < 0) %>%
    pull(value)

if( length(check)>0 ) stop('Check negative emissions in EDGAR processing')

# ------------------------------------------------------------------------------

# 12. Output

# write formatted EDGAR data to intermediate-output
writeData(edgar_wide, domain = "MED_OUT", fn = paste0( "E.", em, "_EDGAR" ))

logStop( )

# END