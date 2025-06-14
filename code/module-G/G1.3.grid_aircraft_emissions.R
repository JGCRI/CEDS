# ------------------------------------------------------------------------------
# Program Name: G1.3.grid_aircraft_emissions.R
# Authors: Leyang Feng, Caleb Braun, Noah Prime
# Date Last Updated: June 22, 2023
# Program Purpose: Grid aggregated emissions into NetCDF grids for aircraft emissions
# Input Files:  MED_OUT:    CEDS_[em]_emissions_by_country_CEDS_sector_[CEDS_version].csv OR
#                           subregional/CEDS_[em]_emissions_by_country_CEDS_sector_[CEDS_version].csv
# Output Files: MED_OUT:    gridded-emissions/CEDS_[em]_AIR_anthro_[year]_0.5_[CEDS_version].nc
#                           gridded-emissions/CEDS_[em]_AIR_anthro_[year]_0.5_[CEDS_version].csv
#               DIAG_OUT:   G.[em]_AIR_emissions_checksum_comparison_diff.csv
#                           G.[em]_AIR_emissions_checksum_comparison_per.csv
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Read in universal header files, support scripts, and start logging
headers <- c( 'data_functions.R', 'gridding_functions.R', 'nc_generation_functions.R' )
log_msg <- "Gridding anthropogenic aircraft emissions"
source( paste0( PARAM_DIR, "header.R" ) )
initialize( "G1.3.grid_aircraft_emissions.R", log_msg, headers )


# ------------------------------------------------------------------------------
# 0.5 Initialize gridding setups

# Select whether gridding seasonality will be "gridded" or "global" (i.e., carbon monitoring)
seasonality_profile <- "global"

# Define emissions species variable
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
res <- as.numeric( args_from_makefile[ 2 ] ) # introducing second command line argument as gridding resolution
if ( is.na( em ) ) em <- "SO2"
if ( is.na( res ) ) res <- 0.5 # default gridding resolution of 0.5

# X years
x_years <- paste0('X', historical_pre_extension_year:end_year)

# Month columns made globaly available
month_columns <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

# Set tolerance for a warning and/or error if checksum differences are too large
# This works on the percentage difference checksums, so set a threshold where if there are
# percent differences which exceeds the threshold, we will give warning or error.
warning_tol <- 0.05     # 0.05% i.e. 0.0005
error_tol <- 1          # 1% i.e. 0.01

# Set up directories
output_dir          <- filePath( "MED_OUT",  "gridded-emissions/",     extension = "" )
total_grid_dir      <- filePath( "DIAG_OUT", "total-emissions-grids/", extension = "" )
proxy_dir           <- filePath( "GRIDDING", "proxy/",                 extension = "" )
proxy_backup_dir    <- filePath( "GRIDDING", "proxy-backup/",          extension = "" )
mask_dir            <- filePath( "GRIDDING", "mask/",                  extension = "" )
seasonality_dir     <- filePath( "GRIDDING", "seasonality/",           extension = "" )
final_emissions_dir <- filePath( "FIN_OUT",  "current-versions/",      extension = "" )

# Initialize the gridding parameters
gridding_initialize( grid_resolution = res,
                     start_year = 1750,
                     end_year = end_year,
                     load_masks = T,
                     load_seasonality_profile = T )


# ------------------------------------------------------------------------------
# 1. Read in files

# Read in the emission data; the flag GRID_SUBREGIONS is set in global_settings.R
# and indicates whether or not to use subregional emissions data.
if ( GRID_SUBREGIONS ) {
  pattern <- paste0( ".*", em, '_subnational.*' )
} else {
  pattern <- paste0( ".*_", em, '_emissions_by_country_CEDS_sector.*' )
}

target_filename <- list.files( final_emissions_dir, pattern )
target_filename <- tools::file_path_sans_ext( target_filename )
stopifnot( length( target_filename ) == 1 )
emissions <- readData( "FIN_OUT", domain_extension = "current-versions/", target_filename )

# Read in mapping files
ceds_gridding_mapping <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'CEDS_sector_to_gridding_sector_mapping', meta = FALSE )
proxy_mapping         <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'proxy_mapping', meta = FALSE )
seasonality_mapping   <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'seasonality_mapping', meta = FALSE )
checksum_tols         <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'checksums_error_tolerance', meta = FALSE )
seasonality_profiles  <- readData( 'MED_OUT', file_name = paste0(em, '_seasonality_mapping') )

# ------------------------------------------------------------------------------
# 2. Pre-processing

# a) Convert the emission data from CEDS working sectors to CEDS level 1 gridding sector
# b) Drop non-matched sectors
# c) Drop non-AIR sectors
# d) Aggregate the emissions at the gridding sectors
# e) Fix names
gridding_emissions <- ceds_gridding_mapping %>%
    dplyr::select( CEDS_working_sector, CEDS_int_gridding_sector_short ) %>%
    dplyr::inner_join( emissions, by = c( 'CEDS_working_sector' = 'sector' ) ) %>%
    dplyr::filter( !is.na( CEDS_int_gridding_sector_short ) ) %>%
    dplyr::filter( CEDS_int_gridding_sector_short == 'AIR' ) %>%
    dplyr::rename( sector = CEDS_working_sector ) %>%
    dplyr::arrange( sector, iso ) %>%
    as.data.frame()

proxy_files <- list( primary = list.files( proxy_dir ), backup = list.files( proxy_backup_dir ) )

#Extend to last year
proxy_mapping <- extendProxyMapping( proxy_mapping )
seasonality_mapping <- extendSeasonalityMapping( seasonality_mapping )

# 2.1 Get global seasonality mapping to 1750 ----------------------------------

# Get iso/sector/year combinations not present in seasonality profiles
global_seasonality_iso_sector_combos <- seasonality_profiles %>%
    dplyr::select(iso, sector) %>%
    dplyr::distinct()
global_seasonality_iso_sector_combos[x_years] <- NA
global_seasonality_iso_sector_year_combos <- global_seasonality_iso_sector_combos %>%
    tidyr::gather( year, value, all_of(x_years) ) %>%
    dplyr::mutate( year = as.numeric(gsub('X', '', year)) ) %>%
    dplyr::select( iso, sector, year )
extension_combos <- global_seasonality_iso_sector_year_combos %>%
    dplyr::setdiff( seasonality_profiles %>% dplyr::select(iso,sector,year) )

# Get the earliest profile for each iso/sector
earliest_year_profiles <- seasonality_profiles %>%
    dplyr::group_by(iso, sector, CEDS_int_gridding_sector_short, CEDS_final_gridding_sector_short ) %>%
    dplyr::filter(year == min(year)) %>%
    dplyr::select(-year) %>%
    dplyr::ungroup()

# Map that profile to each iso/sector/year in the extension
extension_seasonality_mapping <- extension_combos %>%
    dplyr::right_join( earliest_year_profiles, by = c('iso', 'sector'))

# Stack extension with recent years
full_seasonality_profiles <- dplyr::bind_rows(seasonality_profiles, extension_seasonality_mapping) %>%
    dplyr::arrange( iso, sector, year )

# Diagnostics, seasonality profiles should sum to 1
seasonality_profile_diagnostics <- full_seasonality_profiles %>%
    dplyr::mutate(total = rowSums(.[month_columns])) %>%
    dplyr::select(iso, sector, year, data_source, total) %>%
    dplyr::arrange(total)

# Make sure profiles sum to 1
full_seasonality_profiles <- full_seasonality_profiles %>%
    dplyr::mutate(total = rowSums(.[month_columns])) %>%
    dplyr::mutate_at(month_columns, ~./total)

# 2.3 Create monthly emissions by iso/year/int gridding sector -----------------
global_seasonality_emissions_profiles <- gridding_emissions %>%
    tidyr::gather( year, emissions, all_of(x_years) ) %>%
    dplyr::mutate( year = as.numeric(gsub('X', '', year)) ) %>%
    dplyr::right_join( full_seasonality_profiles, by = c('iso', 'sector', 'year', 'CEDS_int_gridding_sector_short') ) %>%
    tidyr::drop_na()

# Apply seasonality and aggregate to CEDS int gridding sector
global_seasonality_emissions <- global_seasonality_emissions_profiles %>%
    dplyr::mutate_at(month_columns, ~.*emissions ) %>%
    dplyr::group_by( iso, CEDS_int_gridding_sector_short, year ) %>%
    dplyr::summarise_at(month_columns, sum) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(total = rowSums(.[month_columns])) %>%
    dplyr::mutate_at(month_columns, ~./total)

global_seasonality_emissions[is.na(global_seasonality_emissions)] <- 0

# ------------------------------------------------------------------------------
# 3. Gridding and writing output data

# For now, the gridding routine uses nested for loops to go through every years
# gases and sectors. May consider to take away for loop for sectors and keep year loops
# for future parallelization

printLog( paste( 'Start', em, 'gridding for each year' ) )

for ( year in year_list ) {
  # grid one years aircraft emissions
  AIR_grid <- grid_one_year_air( year, em, grid_resolution, gridding_emissions, proxy_mapping, proxy_files )

  # Check for negative values in grids and print out error message
  if (any(AIR_grid < 0)) {
      stop(paste("Negative value(s) found in", em, year))
  }

  # generate nc file for gridded one years emissions,
  # a checksum file is also generated along with the nc file
  # which summarize the emissions in mass by sector by month.
  # Note: either "gridded" or "global" seasonality profile must be selected earlier in the script
  if (seasonality_profile == "gridded") {
      generate_final_grids_nc_aircraft( AIR_grid, output_dir, grid_resolution, year, em, seasonality_mapping, seasonality_profile )
  } else if (seasonality_profile == "global") {
      generate_final_grids_nc_aircraft( AIR_grid, output_dir, grid_resolution, year, em, global_seasonality_emissions, seasonality_profile)
  }
}

# -----------------------------------------------------------------------------
# 4. Diagnostic: checksum

# The checksum process uses the checksum files generated along the nc file
# for all gridding years then compare with the input emissions at
# final gridding sector level for each year.
# The comparisons are done in two ways: absolute difference and percentage difference

printLog( 'Start checksum check' )

# calculate global total emissions by sector by year
gridding_emissions_fin <- ceds_gridding_mapping %>%
    dplyr::select( CEDS_int_gridding_sector_short, CEDS_final_gridding_sector_short ) %>%
    dplyr::distinct() %>%
    dplyr::right_join( gridding_emissions, by = c( 'CEDS_int_gridding_sector_short' = 'sector' ) ) %>%
    dplyr::group_by( CEDS_final_gridding_sector_short ) %>%
    dplyr::summarise_at( vars( starts_with( 'X' ) ), sum ) %>%
    dplyr::ungroup() %>%
    dplyr::rename( sector = CEDS_final_gridding_sector_short ) %>%
    dplyr::arrange( sector )

# consolidate different checksum files to have total emissions by sector by year
checksum_df <- list.files( output_dir, paste0( '_', em, '_AIR_anthro.*[.]csv' ), full.names = TRUE ) %>%
    lapply( read.csv ) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by( sector, year ) %>%
    dplyr::summarise( value = sum( value ) ) %>%
    dplyr::ungroup() %>%
    tidyr::spread( year, value ) %>%
    dplyr::rename_all( make.names ) %>%
    dplyr::arrange( sector )

# comparison
X_year_list <- paste0( 'X', year_list )
diag_diff_df <- cbind( checksum_df$sector, abs( gridding_emissions_fin[ X_year_list ] - checksum_df[ X_year_list ] ) )
diag_per_df <- cbind( checksum_df$sector, ( diag_diff_df[ X_year_list ] / gridding_emissions_fin[ X_year_list ] ) * 100 )
diag_per_df[ is.nan.df( diag_per_df ) ] <- NA
diag_per_df <- diag_per_df %>%
  dplyr::rename('sector' = 'checksum_df$sector')
diag_diff_df <- diag_diff_df %>%
  dplyr::rename('sector' = 'checksum_df$sector')


# -----------------------------------------------------------------------------
# 5. Write-out

out_name <- paste0( 'G.', em, '_AIR_emissions_checksum_comparison_diff' )
writeData( diag_diff_df, "DIAG_OUT", out_name )
out_name <- paste0( 'G.', em, '_AIR_emissions_checksum_comparison_per' )
writeData( diag_per_df, "DIAG_OUT", out_name )


# -----------------------------------------------------------------------------
# 6. Checksum warnings
# Get max deviation per sector, and combine that with user defined tolerance mapping file
error_check_df <- diag_per_df %>%
  tidyr::gather('year', 'em', X_year_list) %>%
  dplyr::group_by(sector) %>%
  na.omit() %>%
  dplyr::summarise( M = max(em) ) %>%
  dplyr::left_join(checksum_tols, by = 'sector')

{
print('===============================================================================')
print('===============================================================================')
# Warn / Error for any sectors that warrant it
throw_error <- FALSE
for(i in 1:dim(error_check_df)[1]){
  if(error_check_df[i, 'M'] > error_check_df[i, 'warning_tol']){
    print(paste0('Warning: Checksum diagnostics have found deviations in the ', error_check_df[i, 'sector'], ' sector beyond ', error_check_df[i, 'warning_tol'], '%.'))
  }
  if(error_check_df[i, 'M'] > error_check_df[i, 'error_tol']){
    print(paste0('ERROR: Checksum diagnostics have found deviations in the ', error_check_df[i, 'sector'], ' sector beyond ', error_check_df[i, 'error_tol'], '%.'))
    throw_error <- TRUE
  }
}
if(throw_error){
  print('===============================================================================')
  print('===============================================================================')
  stop('Checksum Deviation Error')
}

print('No checksum deviations above the set error thresholds.')
print('===============================================================================')
print('===============================================================================')
}


# END -------------------------------------------------------------------------
# Run negative grid cell check
printLog( 'Running diagnostic to check for negative grid cell values ...' )
source( "../code/diagnostic/diag_negative_grid_check_aviation" )

logStop( )
