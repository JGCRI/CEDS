#------------------------------------------------------------------------------
# Program Name: B1.2.add_BCOC_recent_control_percent.R
# Author: Leyang Feng
# Date Last Updated: July 15, 2020
# Program Purpose: Adding EF trends for BCOC control percentage for years after 2010
#                  ( the final BOND year)
# Input Files: B.[em]_comb_EF_GAINS_EMF30.csv
# Output Files: B.[em]_Recent_GAINS_control_percent.csv
# Notes:

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "analysis_functions.R" ) # Additional function files required.
    log_msg <- "Adding EF trends for BCOC control percentage for years after 2010" # First message to be printed to the log
    script_name <- "B1.2.add_BCOC_recent_control_percent.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "BC"

# ------------------------------------------------------------------------------
# 1. Read in files and do preliminary setup

# Read in the GAINS EMF-30 combustion emissions factors
    gains_ef_db <- readData( 'MED_OUT', paste0( 'B.', em, '_comb_EF_GAINS_EMF30' ) )

# ------------------------------------------------------------------------------
# 2. Recent years ( after 2010 ) %control calculation

# Define recent years
    first_recent_year <- 2010
    recent_years <- as.character( first_recent_year : end_year )

# Extract emissions for recent years
    gains_recent <- gains_ef_db[ , paste0( 'X', recent_years ) ]

# Construct ef matrix using 2010 ef following layout of gains_recent
    gains_2010 <- matrix( rep( gains_recent[ , paste0( "X", first_recent_year ) ] , length( recent_years ) ),
                          ncol = length( recent_years ) )

# Calculate the recent year %control
    recent_controls <- 1 - ( gains_recent / gains_2010 )

# Change negative values into 0. Negative values means in GAINS EMF30 data, EF(after2010) > EF(2010)
    recent_controls[ recent_controls < 0 ] <- 0
    recent_controls[ is.na( recent_controls ) ] <- 0

# Add the layout
    recent_controls <- cbind( gains_ef_db[ , c( 'iso', 'sector',
                                                'fuel', 'units' ) ],
                              recent_controls )

# ------------------------------------------------------------------------------
# 3. Write output

    writeData( recent_controls , "DEFAULT_EF_PARAM",
               paste0( "B.", em, "_Recent_GAINS_control_percent" ) )

# Every script should finish with this line
    logStop( )

# END
