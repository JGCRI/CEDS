# ------------------------------------------------------------------------------
# Program Name:E.EDGAR_emissions.R
# Author(s): Noah Prime, Harrison Suchyta
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
if ( is.na( em ) ) em <- "CH4"

# Input domain
domain <- "EM_INV"
domain_ext <- "EDGAR/"

# Global constants defined in common_data.R
EDGAR_years <- EDGAR_start_year : EDGAR_end_year
X_EDGAR_years <- paste0( 'X', EDGAR_start_year : EDGAR_end_year )


# ------------------------------------------------------------------------------
# 2. Input

# File settings for EDGARv6.1
fn <- c( paste0( em, "_", EDGAR_start_year, "_",
                 EDGAR_end_year ), ".xlsx")

if( em == 'CH4') fn [1] <- "v60_CH4_1970_2018"
if( em == 'N2O') fn [1] <- "v60_N2O_1970_2018"
if( em == 'CO2') fn [1] <- "IEA_EDGAR_CO2_1970-2021"

sheet_to_use <- paste0(em, "_IPCC1996" )

rows_to_skip <- 9
if( em == 'CO2') rows_to_skip <- 10

print('here')
# Read in EDGAR data
edgar <- readData( domain, domain_extension = domain_ext,
                   file_name = fn[ 1 ], extension = fn[ 2 ],
                   sheet_selection = sheet_to_use, skip = rows_to_skip,
                   missing_value = c( "", "NULL" ) )

# ------------------------------------------------------------------------------
# 3. Reformatting

# Add iso column and group sector column with it at the end
edgar$iso <- tolower( edgar[ , "Country_code_A3" ] )
edgar <- edgar %>%
    dplyr::rename(sector = ipcc_code_1996_for_standard_report,
                  sector_description = ipcc_code_1996_for_standard_report_name)

# Define units as kt (as EDGAR data is in Gg, which is the same as kt)
edgar$units <- 'kt'


# Remove unnecessary columns and arrange (iso-sector-units-sector description-data)
len <- ncol( edgar )
edgar <- edgar %>%
    select( iso, sector, units, sector_description, all_of(9: (len - 2)) ) %>%
    arrange(iso,sector)


# Remove Y's and add X's to the column names of years
len <- ncol ( edgar )
names( edgar ) <- sub("Y_", "", names( edgar ))
names( edgar ) <- c( names( edgar[ 1 : 4 ] ), paste0( "X", names( edgar[ 5 : len ] ) ) )


# Convert data to class numeric, from class character
edgar <- edgar %>%
    dplyr::mutate_at( .vars =  X_EDGAR_years,
                      .funs = list( ~as.numeric( . ) ) )


# Remove rows with all NA's
edgar <- edgar[ apply( X = edgar[ , X_EDGAR_years ],
                       MARGIN = 1, function( x ) ( !all.na( x ) ) ) ,]

# Turn NAs to zeros
edgar <- edgar %>%
    mutate_at( X_EDGAR_years,  ~replace(., is.na(.), 0) )


# Make negative emissions zero
neg_rows <- apply( edgar[, X_EDGAR_years ], 1, function( row ) any( row < 0 ) )
edgar_neg <- edgar[ neg_rows, ]
edgar[ edgar < 0 ] <- 0




# ------------------------------------------------------------------------------

# 12. Output

# write formatted EDGAR data to intermediate-output
writeData(edgar, domain = "MED_OUT", fn = paste0( "E.", em, "_EDGAR" ))

logStop( )

# END