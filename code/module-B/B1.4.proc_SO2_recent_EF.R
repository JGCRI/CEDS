#------------------------------------------------------------------------------
# Program Name: B1.4.proc_SO2_recent_EF.R
# Author: Leyang Feng
# Date Last Updated: 30 March 2016
# Program Purpose: Adding EF trends for SO2 control percentage for recent years
# Input Files: B.SO2_comb_EF_GAINS_EMF30
# Output Files: U.SO2_comb_ControlFrac_db.csv
# Notes: 

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Before we can load headers we need some paths defined. They may be provided by
#   a system environment variable or may have already been set in the workspace.
dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
for ( i in 1:length( dirs ) ) {
  setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
  wd <- grep( 'CEDS/input', list.dirs(), value = T )
  if ( length( wd ) > 0 ) {
    setwd( wd[ 1 ] )
    break
  }
}
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R" ) # Additional function files required.
log_msg <- "Adding EF trends for SO2 control percentage for years after inventory data" # First message to be printed to the log
script_name <- "B1.4.proc_SO2_recent_EF.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"

# ------------------------------------------------------------------------------
# 1. Read in files and do preliminary setup

gains_ef_db <- readData( 'DIAG_OUT', paste0( 'B.', em, '_comb_EF_GAINS_EMF30' ) )
last_inv_year_by_country <- readData( 'MAPPINGS', 'EF_last_inv_year' )
exclude_sector_fuel_combination <- readData( 'MAPPINGS', 'SO2_exclude_fuel_sector' )

# data cleaning
last_inv_year_by_country <- last_inv_year_by_country[ !is.na( last_inv_year_by_country$iso ), ] 
# ------------------------------------------------------------------------------
# 2. Recent year ( after 2010 ) %control calculation 

# define recent years 
recent_years <- as.character( min( last_inv_year_by_country$last_inv_year ) : 2014 )

# remove undesired sector fuel combination from gains_ef_db
for ( row_index in 1 : nrow( exclude_sector_fuel_combination ) ) {
  gains_ef_db <- gains_ef_db[ !( gains_ef_db$sector == exclude_sector_fuel_combination[ row_index, ]$sector & 
                                   gains_ef_db$fuel == exclude_sector_fuel_combination[ row_index, ]$fuel ), ]
}

# extract countrie in the last_inv_year_by_country with its EF
gains_ef_db <- gains_ef_db[ gains_ef_db$iso %in% last_inv_year_by_country$iso, ]

# extract the recent years gains ef
gains_recent <- gains_ef_db[ , c( 'iso', 'sector', 'fuel', 'units', paste0( 'X', recent_years ) ) ] 

# extract the last year ef value for each country 
last_year_ef_list <- c( )
for ( row_index in 1 : nrow( gains_recent ) ) {
  iso_name <- gains_recent[ row_index, 1 ] 
  last_inv_year <- last_inv_year_by_country[ last_inv_year_by_country$iso == iso_name, 2 ]  
  last_year_ef <- gains_recent[ row_index, paste0( 'X', last_inv_year ) ]
  last_year_ef_list <- c( last_year_ef_list, last_year_ef )
  }
  # make into matrix form 
  gains_last_year_ef <- matrix( rep( last_year_ef_list, length( recent_years ) ), ncol = length( recent_years ) )

# calculate the recent year %control
recent_controls <- 1 - ( gains_recent[ , 5 : ncol( gains_recent ) ] / gains_last_year_ef )

# generate a mask that pick out the %control needes to be recalculated 
last_year_mask <- data_frame( iso = gains_recent$iso )
for ( year in recent_years ) {
  mask_or_not_list <- c( )
  for ( each_iso in last_year_mask$iso ) {
    last_year <- last_inv_year_by_country[ last_inv_year_by_country$iso == each_iso, ]$last_inv_year
    current_year <- as.numeric( year )
    mask_or_not <- ifelse( current_year < last_year, 0, 1 )
    mask_or_not_list <- c( mask_or_not_list, mask_or_not )
  }
  last_year_mask <- cbind( last_year_mask, mask_or_not_list )
}
colnames( last_year_mask ) <- c( 'iso', paste0( 'X', recent_years ) )

# apply the mask 
recent_controls <- recent_controls * last_year_mask[ , 2 :ncol( last_year_mask ) ]

# change negative values into 0. Negative values means in GAINS EMF30 data, EF(after2010) > EF(2010)
recent_controls[ recent_controls < 0 ] <- 0
recent_controls[ is.na( recent_controls ) ] <- 0

# add the layout
recent_controls <- cbind( gains_recent[ , c( 'iso', 'sector', 'fuel', 'units' ) ], recent_controls )

# ------------------------------------------------------------------------------
# 3. Write output

writeData( recent_controls , "DEFAULT_EF_PARAM", paste0( "U.",em,"_comb_ControlFrac_db" ) )

# Every script should finish with this line
logStop()

# END