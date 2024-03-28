#------------------------------------------------------------------------------
# Program Name: B1.2.add_SO2_recent_control_percent.R
# Author: Leyang Feng, Noah Prime
# Date Last Updated: March 22, 2024
# Program Purpose: Adding EF trends for SO2 control percentage for recent years,
# Input Files: B.[em]_comb_EF_GAINS_EMF30, B.[em]_ControlFrac_db,
#              SO2_control_frac_last_inv_year.csv, SO2_control_frac_exclude_fuel_sector.csv
# Output Files: B.[em]_ControlFrac_db.csv
# Notes:

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R" ) # Additional function files required.
log_msg <- "Adding EF trends for SO2 control percentage
            for years after inventory data" # First message to be printed to the log
script_name <- "B1.2.add_SO2_recent_control_percent.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"

# ------------------------------------------------------------------------------
# 1. Read in files and do preliminary setup

# Read in GAINS EFs
gains_ef_db <- readData( 'MED_OUT', paste0( 'B.', em, '_comb_EF_GAINS_EMF30' ) )

# Read in mapping files of last inventory year
last_inv_year_csv <- readData( 'MAPPINGS',
                               'SO2_control_frac_last_inv_year',
                               extension = '.csv', sheet_selection = 1 )

last_inv_year_csv <- last_inv_year_csv %>% select(-c(Inv_file,Notes))

exclude_sector_fuel_combination <-
    readData( 'MAPPINGS', 'SO2_control_frac_exclude_fuel_sector' )

# Read in the control percentage db
control_db <- readData( 'MED_OUT', paste0( 'B.', em, '_ControlFrac_db' ) )
# ------------------------------------------------------------------------------
# 2. Recent year Gains EF ratio calculation
# Equation: Ratio = EF( GAINS_year ) / EF( lastinvyear)

# Define recent years
recent_years <- as.character( min( last_inv_year_csv$last_inv_year ) : end_year )
recent_Xyears <- paste0( 'X', recent_years )

# Remove undesired sector fuel combination from gains_ef_db
gains_ef_db <- gains_ef_db %>%
    dplyr::select(fuel, sector) %>%
    # Set diff so only fuel/sector combinations we want to keep
    dplyr::setdiff(exclude_sector_fuel_combination) %>%
    dplyr::left_join(gains_ef_db, by = c('fuel', 'sector'))

# Extract countries with data in the last_inv_year_csv dataframe
gains_ef_db <- gains_ef_db %>%
    dplyr::filter( iso %in% last_inv_year_csv$iso )

# Extract recent year gains ef
gains_recent <- gains_ef_db %>%
    dplyr::select( iso, sector, fuel, units, all_of(recent_Xyears) )

# Last year EF dataframe
gains_last_year_ef <- gains_recent %>%
    tidyr::gather(year, ef, all_of(recent_Xyears)) %>%
    dplyr::mutate( year = as.numeric(gsub('X', '', year)) ) %>%
    dplyr::right_join( last_inv_year_csv, by = c('iso', 'year' = 'last_inv_year') ) %>%
    dplyr::select( iso, sector, fuel, year, ef ) %>%
    dplyr::rename( last_ef = ef, last_inv_year = year )

# Calculate ratios, set to zero before/including last inv year
gains_ef_ratios <- gains_recent %>%
    tidyr::gather(year, ef, all_of(recent_Xyears)) %>%
    dplyr::mutate( year = as.numeric(gsub('X', '', year)) ) %>%
    dplyr::left_join( gains_last_year_ef, by = c('iso', 'sector', 'fuel') ) %>%
    dplyr::mutate( ef_ratio = ef / last_ef ) %>%
    # dplyr::mutate( ef_ratio = (year > last_inv_year) * ef_ratio ) %>%
    # Set ratio EF reduction ratio, or 1 if before last inventory year to do nothing
    dplyr::mutate( ef_ratio = if_else(year > last_inv_year, ef_ratio, NaN) ) %>%
    dplyr::select(iso, sector, fuel, year, last_inv_year, ef_ratio)

# Make ratios greater than 1 equal to 1?
# So not allowing for EFs to increase
gains_ef_ratios <- gains_ef_ratios %>%
    dplyr::mutate( ef_ratio = if_else(ef_ratio > 1, 1, ef_ratio) )

# Convert back to wide format and write out for diagnostics
gains_ef_ratios_wide <- gains_ef_ratios %>%
    dplyr::mutate( year = paste0('X', year) ) %>%
    tidyr::spread( year, ef_ratio )

# ------------------------------------------------------------------------------
# 3. Recent year% control calculation
# Calculate recent controls
# Equations: Control % = 1 - ( 1 - Control % ) * R
#            R = EF( GAINS_year ) / EF( GAINS_lastinvyear)
#            R is calculated as recent_ratio
# 1 - R - R*Control%
# 1 - R * (1-Control %)

# Control db long format
control_db_long <- control_db %>%
    tidyr::gather(year, control_pct, dplyr::starts_with('X') ) %>%
    dplyr::mutate( year = as.numeric(gsub('X', '', year)) )

# Merge ratios with Control DB and calculate new control percentages
final_control <- gains_ef_ratios %>%
    # Merge with full control frac database
    dplyr::right_join( control_db_long, by = c('iso', 'sector', 'fuel', 'year') ) %>%
    # Calculate new control percent
    dplyr::mutate( new_control_pct = 1 - ef_ratio * (1 - control_pct) ) %>%
    # Update control frac database where not NA
    dplyr::mutate( control_pct = if_else(is.na(new_control_pct), control_pct, new_control_pct) ) %>%
    # Convert back to proper wide format and with required columns
    dplyr::select( iso, sector, fuel, units, year, control_pct ) %>%
    dplyr::mutate( year = paste0('X', year) ) %>%
    tidyr::spread( year, control_pct )

# ------------------------------------------------------------------------------
# 5. Write out and Stop

# Diagnostics
writeData( gains_ef_ratios, 'DIAG_OUT', paste0( 'B1.2.add_SO2_controls_diagnostic' ) )

# Output
writeData( final_control, 'MED_OUT', paste0( 'B.', em, '_ControlFrac_db' ) )

# Every script should finish with this line
logStop()

# END