# ------------------------------------------------------------------------------
# Program Name:E.EDGAR_emissions.R
# Author(s): Noah Prime
# Date Last Modified: June 21, 2021
# Program Purpose: Intended to reformat EDGAR default emissions data and add it to
#                  the data base for the relevant emissions species
# Input Files: relevant EDGAR emissions data ( EDGAR v5 = v50_[em]_1970_[edgar_end_year].xls )
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
if ( is.na( em ) ) em <- "N2O"


# EDGAR data version number
vn <- "5.0"

# Input domain
domain <- "EM_INV"
domain_ext <- "EDGAR/"


# Define EDGAR years
# CO2 end year in v5 is 2018, else 2015
if( em == "CO2" ){

    EDGAR_end_year <- EDGAR_end_year_CO2

}

# Global constants defined in common_data.R
EDGAR_years <- EDGAR_start_year : EDGAR_end_year
X_EDGAR_years <- paste0( 'X', EDGAR_start_year : EDGAR_end_year )


# ------------------------------------------------------------------------------
# 2. Input

# File settings for EDGAR v5
fn <- c( paste0( "v",  gsub( "[.]", "", vn ), "_", em, "_", EDGAR_start_year, "_",
                 EDGAR_end_year ), ".xls")
sheet_to_use <- paste0( "v", vn, "_EM_", em, "_IPCC1996" )
rows_to_skip <- 9

# EDGAR v5 has a special file naming convention for CO2
if( em == "CO2" ){

    fn <- c( paste0( "v",  gsub( "[.]", "", vn ), "_", em, "_", "excl_short-cycle_org_C_",
                     EDGAR_start_year, "_", EDGAR_end_year ), ".xls")

}



# Read in EDGAR data
edgar <- readData( domain, domain_extension = domain_ext,
                   file_name = fn[ 1 ], extension = fn[ 2 ],
                   sheet_selection = sheet_to_use, skip = rows_to_skip,
                   missing_value = c( "", "NULL" ) )




# ------------------------------------------------------------------------------
# 3. Reformatting

# Add iso column and group sector column with it at the end
edgar$iso <- tolower( edgar[ , "ISO_A3" ] )
edgar <- edgar %>%
            dplyr::rename(sector = IPCC,
                          sector_description = IPCC_description)

# Define units as kt (as EDGAR data is in Gg, which is the same as kt)
edgar$units <- 'kt'


# Remove unnecessary columns and arrange (iso-sector-units-sector description-data)
len <- ncol( edgar )
edgar <- edgar %>%
    select( iso, sector, units, sector_description, all_of(7: (len - 2)) ) %>%
    arrange(iso,sector)


# Add X's to the column names of years
len <- ncol ( edgar )
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
writeData(edgar, domain = "MED_OUT", fn = paste0( "E.", em, "_EDGAR_v5" ))

logStop( )

# END