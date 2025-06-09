# ------------------------------------------------------------------------------
# Program Name: G1.1.grid_bulk_emissions.R
# Authors: Leyang Feng, Caleb Braun, Noah Prime
# Date Last Updated: June 22, 2023
# Program Purpose: Grid aggregated emissions into NetCDF grids for bulk emissions (excluding AIR)
# Input Files:  MED_OUT:    CEDS_[em]_emissions_by_country_CEDS_sector_[CEDS_version].csv OR
#                           subregional/CEDS_[em]_emissions_by_country_CEDS_sector_[CEDS_version].csv
# Output Files: MED_OUT:    gridded-emissions/CEDS_[em]_anthro_[year]_0.5_[CEDS_version].nc
#                           gridded-emissions/CEDS_[em]_anthro_[year]_0.5_[CEDS_version].csv
#               DIAG_OUT:   CEDS_[em]_anthro_[year]_TOTAL_0.5_[CEDS_version].nc
#                           CEDS_[em]_anthro_[year]_TOTAL_0.5_[CEDS_version].csv
#                           CEDS_[em]_anthro_[year]_TOTAL_monthly_[CEDS_version].nc
#                           CEDS_[em]_anthro_[year]_TOTAL_monthly_[CEDS_version].csv
#                           G.[em]_bulk_emissions_checksum_comparison_diff.csv
#                           G.[em]_bulk_emissions_checksum_comparison_per.csv
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Read in universal header files, support scripts, and start logging
headers <- c( 'data_functions.R', 'gridding_functions.R', 'nc_generation_functions.R',
              'point_source_util_functions.R' )
log_msg <- "Gridding anthropogenic emissions (excluding AIR) "
source( paste0( PARAM_DIR, "header.R" ) )
initialize( "G1.1.grid_bulk_emissions.R", log_msg, headers )
if ( grid_remove_iso != "" ) printLog( paste("Gridding will exclude",grid_remove_iso) )

# ------------------------------------------------------------------------------
# 0.5 Initialize gridding setups

# Define emissions species variable
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
res <- as.numeric( args_from_makefile[ 2 ] ) # introducing second command line argument as gridding resolution
if ( is.na( em ) ) em <- "SO2"
if ( is.na( res ) ) res <- 0.5 # default gridding resolution of 0.5

# Load raster library (for some reason not read in by default through renv)
library('raster')

# Whether or not to perform downscaling
if (res < 0.5) {
    fine_res <- res
    downscale <- TRUE
} else if (res == 0.5) {
    downscale <- FALSE
}

# X years
x_years <- paste0('X', historical_pre_extension_year:end_year)

# Month columns made globaly available
month_columns <<- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

# Set tolerance for a warning and/or error if checksum differences are too large
# This works on the percentage difference checksums, so set a threshold where if there are
# percent differences which exceeds the threshold, we will give warning or error.
warning_tol <- 0.05     # 0.05% i.e. 0.0005
error_tol <- 1          # 1% i.e. 0.01

# Set up directories
output_dir          <- filePath( "MED_OUT",  "gridded-emissions/",     extension = "" )
total_grid_dir      <- filePath( "DIAG_OUT", "total-emissions-grids/", extension = "" )
# SHP, AIR, and TANK sectors proxy pre-installed in different location
SAT_proxy_dir       <- filePath( "GRIDDING", "proxy/",                 extension = "" )
proxy_dir           <- filePath( "MED_OUT",  "final_generated_proxy/", extension = "" )
proxy_backup_dir    <- filePath( "GRIDDING", "proxy-backup/",          extension = "" )
mask_dir            <- filePath( "GRIDDING", "mask/",                  extension = "" )
seasonality_dir     <- filePath( "GRIDDING", "seasonality/",           extension = "" )
final_emissions_dir <- filePath( "FIN_OUT",  'current-versions',       extension = "" )
prev_versions_dir   <- filePath( "FIN_OUT",  'diagnostics/grids-to-compare/',       extension = "" )
intermediate_output <- filePath( "MED_OUT",  '',                       extension = "" )
point_source_dir    <- filePath( 'MED_OUT', 'full_point_source_scaled_yml', extension = "" )
# Downscaling dirs
downscaling_output_dir          <- filePath( "MED_OUT",  "gridded-emissions_01/",           extension = "" )
fine_proxy_dir                  <- filePath( "MED_OUT",  "final_generated_proxy_0.1/",      extension = "" )
fine_proxy_backup_dir           <- filePath( "GRIDDING", "proxy-backup-01/",                extension = "" )

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
# Need total emissions for checksums
total_emissions <- readData( "FIN_OUT", domain_extension = "current-versions/", target_filename )
# Need emissions without point source for gridding
target_filename <- list.files( intermediate_output, pattern )
target_filename <- tools::file_path_sans_ext( target_filename )
stopifnot( length( target_filename ) == 1 )
emissions <- readData( "MED_OUT", domain_extension = "", target_filename )

# If defined, remove emissions from one iso from gridding
if ( grid_remove_iso != "" ) {
  emissions <- dplyr::mutate_at( emissions, vars( all_of(X_extended_years) ),
                                 list( ~ifelse( iso == grid_remove_iso, 0, . )))

  total_emissions <- dplyr::mutate_at( total_emissions, vars( all_of(X_extended_years) ),
                                       list( ~ifelse( iso == grid_remove_iso, 0, . )))
}

# Read in mapping files
# the location index indicates the location of each region mask in the 'world' matrix
# TODO: fix metadata readin so that works again for these
location_index               <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'country_location_index_05', meta = FALSE )
ceds_gridding_mapping        <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'CEDS_sector_to_gridding_sector_mapping', meta = FALSE )
proxy_mapping                <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'proxy_mapping', meta = FALSE )
seasonality_mapping          <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'seasonality_mapping', meta = FALSE )
proxy_substitution_mapping   <- readData( 'MED_OUT', paste0( em, '_proxy_substitution_mapping'), meta = FALSE )
sector_name_mapping          <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'CEDS_gridding_sectors', meta = FALSE )
sector_name_mapping          <- unique( sector_name_mapping[ , c( 'CEDS_fin_sector', 'CEDS_fin_sector_short' ) ] )
edgar_sector_replace_mapping <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'EDGAR_sector_replace_mapping', meta = F )
expanded_sectors_map         <- readData( domain = 'MAPPINGS', file_name = 'old_to_new_sectors', extension = '.csv', meta = FALSE )
checksum_tols                <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'checksums_error_tolerance', meta = FALSE )
master_country_list          <- readData( 'MAPPINGS', file_name = 'Master_Country_List')
seasonality_profiles         <- readData( 'MED_OUT', file_name = paste0(em, '_seasonality_mapping') )
proxy_mapping_downscale      <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'proxy_mapping_downscale', meta = FALSE )


# Update CEDS gridding mapping with new expanded sectors
ceds_gridding_mapping <- ceds_gridding_mapping %>%
  dplyr::left_join(expanded_sectors_map, by = c('CEDS_working_sector' = 'ceds_sector')) %>%
  dplyr::mutate( CEDS_working_sector = ifelse(is.na(new_sector), CEDS_working_sector, new_sector) ) %>%
  dplyr::select( -new_sector)

# Read in point source yml files

# point source yml files
point_source_files <- list.files( paste0(point_source_dir, '/', em ), '*.yml' )

if(length(point_source_files) == 0 ){
    x_years_df <- data.frame(matrix(ncol = length(x_years), nrow = 0))
    colnames(x_years_df) <- x_years
    point_source_df <- data.frame( id = character(), name = character(), location = character(),
                                   latitude = double(), longitude = double(), units = character(),
                                   CEDS_sector = character(), EDGAR_sector = character(), fuel = character(),
                                   iso = character(), build_year = integer(), description = character(),
                                   date = character(), species = character(), data_source = character())
    point_source_df <- cbind(point_source_df, x_years_df)
} else{
    # List of point source data frames
    point_source_list <- lapply( point_source_files, read_yml_all_ems,
                                paste0(point_source_dir, '/', em ) )

    # As data frame
    point_source_df <- do.call( rbind, point_source_list )
    point_source_df <- point_source_df %>%
        dplyr::mutate_at( vars(latitude, longitude, all_of(x_years)), as.numeric )
}

# 1.1 Checksum input file (diagnostic) --------------------------------------------------

# Emissions from the total CEDS inventory
input_ems <- ceds_gridding_mapping %>%
    dplyr::select(CEDS_working_sector, CEDS_final_gridding_sector_short) %>%
    dplyr::distinct() %>%
    dplyr::right_join( total_emissions, by = c('CEDS_working_sector' = 'sector')) %>%
    dplyr::group_by(CEDS_final_gridding_sector_short) %>%
    dplyr::summarise_at(x_years, sum) %>%
    # dplyr::select(CEDS_final_gridding_sector_short, X2022) %>%
    dplyr::ungroup()

# Non point source emissions
non_ps_ems <- ceds_gridding_mapping %>%
    dplyr::select(CEDS_working_sector, CEDS_final_gridding_sector_short) %>%
    dplyr::distinct() %>%
    dplyr::right_join( emissions, by = c('CEDS_working_sector' = 'sector')) %>%
    dplyr::group_by(CEDS_final_gridding_sector_short) %>%
    dplyr::summarise_at(x_years, sum) %>%
    # dplyr::select(CEDS_final_gridding_sector_short, X2022) %>%
    dplyr::ungroup()

# Point source emissions
ps_ems <- ceds_gridding_mapping %>%
    dplyr::select(CEDS_working_sector, CEDS_final_gridding_sector_short) %>%
    dplyr::distinct() %>%
    dplyr::right_join( point_source_df, by = c('CEDS_working_sector' = 'CEDS_sector')) %>%
    dplyr::group_by(CEDS_final_gridding_sector_short) %>%
    dplyr::summarise_at(x_years, sum) %>%
    # dplyr::select(CEDS_final_gridding_sector_short, X2022) %>%
    dplyr::ungroup()

# Non point source emissions + point source emissions
total_ems <- dplyr::bind_rows(non_ps_ems, ps_ems) %>%
    dplyr::group_by(CEDS_final_gridding_sector_short) %>%
    dplyr::summarise_at(x_years, sum) %>%
    # dplyr::summarise(X2022 = sum(X2022)) %>%
    # dplyr::select(CEDS_final_gridding_sector_short, X2022) %>%
    dplyr::ungroup()

# Difference from CEDS aggregate and the sum of the serperated emissions (ps and non ps)
input_diff <- dplyr::bind_rows( input_ems, total_ems ) %>%
    dplyr::group_by(CEDS_final_gridding_sector_short) %>%
    dplyr::summarise_at(x_years, diff) %>%
    dplyr::ungroup()

if( sum( abs( input_diff[x_years] ) ) > 0.001 ){
    stop("Point source emissions and non-point source emissions do not add up to CEDS aggregate. Try cleaning point sources and running module-P again.")
}


# ------------------------------------------------------------------------------
# 2. Pre-processing

# a) Match CEDS working sector with CEDS int gridding sector
# b) Drop non-matched sectors
# d) Fix names
# e) Remove AIR sector in data
gridding_emissions <- ceds_gridding_mapping %>%
    dplyr::select( CEDS_working_sector, CEDS_int_gridding_sector_short ) %>%
    dplyr::inner_join( emissions, by = c( 'CEDS_working_sector' = 'sector' ) ) %>%
    dplyr::filter( !is.na( CEDS_int_gridding_sector_short ) ) %>%
    dplyr::filter( !(CEDS_int_gridding_sector_short %in% c('AIR') ) ) %>%
    dplyr::rename( sector = CEDS_working_sector ) %>%
    dplyr::arrange( sector, iso ) %>%
    as.data.frame()

# a) Convert the emission data from CEDS working sectors to CEDS level 1 gridding sector
# b) Drop non-matched sectors
# c) Aggregate the emissions at the gridding sectors
# d) Fix names
# e) Remove AIR sector in data
checksum_emissions <- ceds_gridding_mapping %>%
    dplyr::select( CEDS_working_sector, CEDS_int_gridding_sector_short ) %>%
    dplyr::inner_join( total_emissions, by = c( 'CEDS_working_sector' = 'sector' ) ) %>%
    dplyr::filter( !is.na( CEDS_int_gridding_sector_short ) ) %>%
    dplyr::group_by( iso, CEDS_int_gridding_sector_short ) %>%
    dplyr::summarise_at( paste0( 'X', year_list ), sum ) %>%
    dplyr::ungroup() %>%
    dplyr::rename( sector = CEDS_int_gridding_sector_short ) %>%
    dplyr::filter( sector != 'AIR' ) %>%
    dplyr::arrange( sector, iso ) %>%
    as.data.frame()

# 2.1 Proxy and gridded seasonality files ---------------------------------------

# Move pre-installed proxy to final_proxy folder (no overwriting)
pre_installed <- list.files(SAT_proxy_dir)
file.copy( from = paste0(SAT_proxy_dir, pre_installed),
           to = paste0(proxy_dir, pre_installed),
           overwrite = FALSE )

# List of proxy files
proxy_files <- list( primary = list.files( proxy_dir ), backup = list.files( proxy_backup_dir ) )

#Extend to last year
proxy_mapping <- extendProxyMapping( proxy_mapping )
seasonality_mapping <- extendSeasonalityMapping( seasonality_mapping )

# If downscaling
if(downscale){
    # Proxy extension and files list
    proxy_files_downscale <- list( primary = list.files( fine_proxy_dir ), backup = list.files( fine_proxy_backup_dir ) )
    proxy_mapping_downscale <- extendProxyMapping( proxy_mapping_downscale )

    # Grid dimension to normalize over (must be integer since we are working with whole
    # cells and not partial). For example, if downscaling from 0.5 deg to 0.25deg, each
    # single cell of the 0.5 grid will map to a 2x2 grid on the 0.25 grid (0.5/0.25=2).
    # Therefore, we need to normalize every (non-overlapping) 2x2 grid array on the
    # 0.25 grid.
    norm_dim <- res/fine_res
    if(round(norm_dim) != norm_dim){
        stop( "norm_dim must be an integer")
    }

    # Generate grid cell area arrays for both resolutions
    gridcell_area_coarse <- grid_area( res, all_lon = T )
    gridcell_area_fine <- grid_area( fine_res, all_lon = T )
}

# 2.2 Get regional seasonality mapping to 1750 ----------------------------------

# Get iso/sector/year combinations not present in seasonality profiles
regional_seasonality_iso_sector_combos <- seasonality_profiles %>%
    dplyr::select(iso, sector) %>%
    dplyr::distinct()
regional_seasonality_iso_sector_combos[x_years] <- NA
regional_seasonality_iso_sector_year_combos <- regional_seasonality_iso_sector_combos %>%
    tidyr::gather( year, value, all_of(x_years) ) %>%
    dplyr::mutate( year = as.numeric(gsub('X', '', year)) ) %>%
    dplyr::select( iso, sector, year )
extension_combos <- regional_seasonality_iso_sector_year_combos %>%
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
regional_seasonality_emissions_profiles <- gridding_emissions %>%
    tidyr::gather( year, emissions, all_of(x_years) ) %>%
    dplyr::mutate( year = as.numeric(gsub('X', '', year)) ) %>%
    dplyr::right_join( full_seasonality_profiles, by = c('iso', 'sector', 'year', 'CEDS_int_gridding_sector_short') ) %>%
    tidyr::drop_na() # This essentially removes AIR sector (which was brought in from regional seasonality)

# Apply seasonality and aggregate to CEDS int gridding sector
regional_seasonality_emissions <- regional_seasonality_emissions_profiles %>%
    dplyr::mutate_at(month_columns, ~.*emissions ) %>%
    dplyr::group_by( iso, CEDS_int_gridding_sector_short, year ) %>%
    dplyr::summarise_at(month_columns, sum) %>%
    dplyr::ungroup()

# 2.4 Get data with gridded seasonality ----------------------------------------

# Get combinations of iso/sector/year covered in regional seasonality profiles
regional_combos <- full_seasonality_profiles %>%
    dplyr::select( iso, sector, year )

# Anti join emissions data with what's present in regional combos
# Aggregate to CEDS int gridding sector
gridded_seasonality_emissions <- gridding_emissions %>%
    tidyr::gather( year, value, all_of(x_years) ) %>%
    dplyr::mutate( year = as.numeric(gsub('X', '', year)) ) %>%
    dplyr::anti_join( regional_combos, by = c('iso', 'sector', 'year') ) %>%
    dplyr::mutate( year = paste0('X', year) ) %>%
    tidyr::spread( year, value ) %>%
    dplyr::group_by( iso, CEDS_int_gridding_sector_short ) %>%
    dplyr::summarise_at( x_years, sum ) %>%
    dplyr::ungroup()

# 2.5 Checksums diagnostics ----------------------------------------

# Combine gridded seasonality emissions and regional seasonality emissions
wide_regional_seasonality_emissions <- regional_seasonality_emissions %>%
    dplyr::mutate(total = rowSums(.[month_columns])) %>%
    dplyr::select(iso, CEDS_int_gridding_sector_short, year, total) %>%
    dplyr::mutate( year = paste0('X', year) ) %>%
    tidyr::spread(year, total)
sum_seasonality_emissions <- dplyr::bind_rows(wide_regional_seasonality_emissions, gridded_seasonality_emissions) %>%
    dplyr::group_by(CEDS_int_gridding_sector_short) %>%
    dplyr::summarise_at( x_years, sum ) %>%
    dplyr::ungroup()

# Take the difference between sum of above and the input gridding emissions
diff_df <- gridding_emissions %>%
    dplyr::group_by(CEDS_int_gridding_sector_short) %>%
    dplyr::summarise_at( x_years, sum ) %>%
    dplyr::ungroup() %>%
    dplyr::bind_rows(sum_seasonality_emissions) %>%
    dplyr::group_by(CEDS_int_gridding_sector_short) %>%
    dplyr::summarise_at( x_years, diff )

# Write out diagnositc file
writeData(diff_df, domain = 'DIAG_OUT', fn = 'G.seasonality_split_checksums', meta=FALSE)

# Fail if can't pass this fairly weak threshold for equality:
# sum of all years and sectors can't have combined difference
# of greater than 0.00001
if( sum(abs(diff_df[x_years])) > 1e-5 ){
    stop('Regional seasonality gridding emissions and gridded seasonality emissions do not sum up to total')
}


# 2.6 Separating Point Source Data ---------------------------------------------

# Get point source emissions for each month using regional seasonality profiles
regional_seasonality_point_sources <- point_source_df %>%
    tidyr::gather( year, value, all_of(x_years) ) %>%
    dplyr::select( id, iso, CEDS_sector, latitude, longitude, year, value ) %>%
    dplyr::mutate( year = as.numeric(gsub('X', '', year)) ) %>%
    dplyr::right_join( full_seasonality_profiles,
                       by = c( 'iso', 'CEDS_sector' = 'sector', 'year' ) ) %>%
    stats::na.omit() %>%
    dplyr::mutate_at(month_columns, ~.*value ) %>%
    dplyr::select( id, iso, CEDS_sector, CEDS_final_gridding_sector_short, latitude, longitude, year, all_of(month_columns))

# Get combinations of regional data
regional_point_source_combos <- regional_seasonality_point_sources %>%
    dplyr::select( id, iso, CEDS_sector, year ) %>%
    dplyr::distinct()

# Get point sources without regional seasonality profiles
gridded_seasonality_point_sources <- point_source_df %>%
    tidyr::gather( year, value, all_of(x_years) ) %>%
    dplyr::select( id, iso, CEDS_sector, latitude, longitude, year, value ) %>%
    dplyr::mutate( year = as.numeric(gsub('X', '', year)) ) %>%
    dplyr::anti_join( regional_point_source_combos, by = c('id', 'iso', 'CEDS_sector', 'year') ) %>%
    dplyr::left_join( ceds_gridding_mapping, by = c('CEDS_sector' = 'CEDS_working_sector') ) %>%
    dplyr::select( id, iso, CEDS_sector, CEDS_final_gridding_sector_short, latitude, longitude, year, value)

# 2.7 Checksums diagnostics (Point Sources) ----------------------------------------

# If point sources not present, skip

if( nrow(point_source_df) > 0 ){
    # Get total emissions by final gridding sector for point sources using regional seasonality profiles
    regional_ps_agg <- regional_seasonality_point_sources %>%
        dplyr::mutate(total = rowSums(.[month_columns])) %>%
        dplyr::select( id, year, CEDS_final_gridding_sector_short, iso, total ) %>%
        dplyr::mutate( year = paste0('X', year) ) %>%
        tidyr::spread(year, total) %>%
        dplyr::group_by( CEDS_final_gridding_sector_short ) %>%
        dplyr::summarise_at(x_years, sum) %>%
        dplyr::ungroup()
    # Get total emissions by final gridding sector for point sources using gridded seasonality profiles
    gridded_ps_agg <- gridded_seasonality_point_sources %>%
        dplyr::mutate(year = paste0('X', year) ) %>%
        tidyr::spread( year, value ) %>%
        dplyr::group_by( CEDS_final_gridding_sector_short ) %>%
        dplyr::summarise_at(x_years, sum) %>%
        dplyr::ungroup()
    # Get total emissions by final gridding sector from both together
    sum_ps_agg <- dplyr::bind_rows(regional_ps_agg, gridded_ps_agg) %>%
        dplyr::group_by( CEDS_final_gridding_sector_short ) %>%
        dplyr::summarise_at( x_years, sum ) %>%
        dplyr::ungroup()
    # Get total emissions by final gridding sector from input data
    total_ps_agg <- point_source_df %>%
        dplyr::left_join( ceds_gridding_mapping, by = c('CEDS_sector' = 'CEDS_working_sector') ) %>%
        dplyr::group_by( CEDS_final_gridding_sector_short ) %>%
        dplyr::summarise_at(x_years, sum) %>%
        dplyr::ungroup()

    # Take the difference between sum of above and the input point sources emissions
    diff_df <- dplyr::bind_rows(sum_ps_agg, total_ps_agg) %>%
        dplyr::group_by( CEDS_final_gridding_sector_short ) %>%
        dplyr::summarise_at( x_years, diff )

    # Write out diagnositc file
    writeData(diff_df, domain = 'DIAG_OUT', fn = paste0( 'G.', em, '_point-source_seasonality_split_checksums'), meta=FALSE)

    # Fail if can't pass this fairly weak threshold for equality:
    # sum of all years and sectors can't have combined difference
    # of greater than 0.00001
    if( sum(abs(diff_df[x_years])) > 1e-5 ){
        stop('Point source regional seasonality gridding emissions and gridded seasonality emissions do not sum up to total')
    }
} else{
    # Write out diagnositc file
    writeData(data.frame(column = paste0('no point sources for ', em)), domain = 'DIAG_OUT', fn = paste0( 'G.', em, 'point-source_seasonality_split_checksums'), meta=FALSE)
}

# 3. Saving Previous Versions To Compare ---------------------------------------

# List of species auto-compared
save_em_list <- c('SO2', 'CO2', 'NOx')

# List of annual grids, first the ones broken up by sector, then the totals
previous_versions_by_sector <- list.files(output_dir, pattern = glob2rx('CEDS_*_anthro_*_0.5.nc'))
previous_versions_total <- list.files(total_grid_dir, pattern = glob2rx('CEDS_*_anthro_*.nc'))

# Function which moves the most recent grid to previous grid versions directory
# Works by species, and list of grids (ie by sector grids or total grids)
get_most_recent_grid <- function(species, grid_list, input_directory){
    # Get list of grids available for given species
    available_grids <- grep(species, grid_list, value = TRUE)

    # Return if no grids available
    if(length(available_grids) == 0) return(FALSE)

    # Get years of available grids
    year_list <- unlist(lapply(available_grids, function(x) as.numeric(strsplit(x, '_')[[1]][4])))

    # Most Recent Year
    max_year <- max(year_list)

    # Getting File For Most recent year for species
    final_grid <- grep(max_year, available_grids, value = TRUE)

    # Copy file to previous grid versions directory
    file.copy( from = paste0(input_directory, final_grid),
               to = paste0(prev_versions_dir, final_grid),
               overwrite = TRUE )

    return(TRUE)

}

# Copy the files (return list says whether or not a file was moved for the given species)
move_files_by_sector <- lapply(save_em_list, get_most_recent_grid, previous_versions_by_sector, output_dir)
move_files_total <- lapply(save_em_list, get_most_recent_grid, previous_versions_total, total_grid_dir)


# ------------------------------------------------------------------------------
# 4. Gridding and writing output data


# For now, the gridding routine uses nested for loops to go through every years
# gases and sectors. Future work could parallelize the year loop.


printLog( paste( 'Gridding', em, 'emissions for each year...' ) )

pb <- txtProgressBar(min = 0, max = length(year_list), style = 3)

start_time <- Sys.time()
for ( year in year_list ) {
    setTxtProgressBar(pb, year - min(year_list))

    # grid one year's emissions (non point sources)
    int_grids_list <- grid_one_year( year,
                                     em,
                                     grid_resolution,
                                     gridded_seasonality_emissions,
                                     regional_seasonality_emissions,
                                     location_index,
                                     proxy_mapping,
                                     proxy_substitution_mapping,
                                     proxy_files,
                                     ceds_gridding_mapping,
                                     seasonality_mapping )


    # Grid one year's point source emissions
    final_point_source_grids_list <- grid_point_sources_one_year( em,
                                                                  year,
                                                                  regional_seasonality_point_sources,
                                                                  gridded_seasonality_point_sources,
                                                                  grid_resolution )

    # generate nc file for each year's gridded emissions,
    # a checksum file is also generated along with the nc file
    # which summarize the emissions in mass by sector by month.
    final_grids <- generate_final_grids_nc( int_grids_list,
                                            final_point_source_grids_list,
                                            output_dir,
                                            grid_resolution,
                                            year,
                                            em,
                                            sector_name_mapping )

    # Check for negative values in grids and print out error message
    if (any(final_grids$AGR < 0)) {
        stop(paste("Negative value(s) found in", em, year))
    } else if (any(final_grids$ENE < 0)) {
        stop(paste("Negative value(s) found in", em, year))
    } else if (any(final_grids$IND < 0)) {
        stop(paste("Negative value(s) found in", em, year))
    } else if (any(final_grids$TRA < 0)) {
        stop(paste("Negative value(s) found in", em, year))
    } else if (any(final_grids$RCO < 0)) {
        stop(paste("Negative value(s) found in", em, year))
    } else if (any(final_grids$SLV < 0)) {
        stop(paste("Negative value(s) found in", em, year))
    } else if (any(final_grids$WST < 0)) {
        stop(paste("Negative value(s) found in", em, year))
    } else if (any(final_grids$SHP < 0)) {
        stop(paste("Negative value(s) found in", em, year))
    }

    # diagnostic: generate total emissions grid for one year
    generate_annual_total_emissions_grids_nc( total_grid_dir, final_grids,
                                              grid_resolution, year, em )

    # diagnostic: generate total emissions grid for one year monthly
    generate_monthly_total_emissions_grids_nc( total_grid_dir, final_grids,
                                               grid_resolution, year, em )

    # Downscale
    if(downscale & (year >= downscale_start_year)){
        # Intermediate sector names
        sector_list <- names(int_grids_list)

        start_time <- Sys.time()
        # Initialize sector list
        int_grids_list_fine <- downscale_year(em, year, sector_list, int_grids_list,
                                              proxy_files_downscale, proxy_mapping_downscale,
                                              fine_proxy_dir, fine_proxy_backup_dir,
                                              fine_res, norm_dim,
                                              gridcell_area_coarse, gridcell_area_fine)
        end_time <- Sys.time()
        end_time - start_time

        # Grid one year's point source emissions (fine resolution)
        final_point_source_grids_list <- grid_point_sources_one_year( em,
                                                                      year,
                                                                      regional_seasonality_point_sources,
                                                                      gridded_seasonality_point_sources,
                                                                      fine_res )

        # generate nc file for each year's gridded emissions,
        # a checksum file is also generated along with the nc file
        # which summarize the emissions in mass by sector by month.
        final_grids_fine <- generate_final_grids_nc( int_grids_list_fine,
                                                     final_point_source_grids_list,
                                                     downscaling_output_dir,
                                                     fine_res,
                                                     year,
                                                     em,
                                                     sector_name_mapping )
    }

}

close(pb)

end_time <- Sys.time()
end_time - start_time


# -----------------------------------------------------------------------------
# 5. Diagnostic: checksum

# The checksum process uses the checksum files generated along the nc file
# for all gridding years then compare with the input emissions at
# final gridding sector level for each year.
# The comparisons are done in two ways: absolute difference and percentage difference

printLog( 'Start checksum check' )

# calculate global total emissions by sector by year
gridding_emissions_fin <- ceds_gridding_mapping %>%
    dplyr::select( CEDS_int_gridding_sector_short, CEDS_final_gridding_sector_short ) %>%
    dplyr::distinct() %>%
    dplyr::right_join( checksum_emissions, by = c( 'CEDS_int_gridding_sector_short' = 'sector' ) ) %>%
    dplyr::group_by( CEDS_final_gridding_sector_short ) %>%
    dplyr::summarise_at( vars( starts_with( 'X' ) ), sum ) %>%
    dplyr::ungroup() %>%
    dplyr::rename( sector = CEDS_final_gridding_sector_short ) %>%
    dplyr::arrange( sector )

# consolidate different checksum files to have total emissions by sector by year
checksum_df <- list.files( output_dir, paste0( '_', em, '_anthro.*[.]csv' ), full.names = TRUE ) %>%
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
colnames(diag_per_df)[1] <- 'sector'
colnames(diag_diff_df)[1] <- 'sector'

# Downscale checksums
if(downscale){
    # Total emissions by sector by year from grids
    checksum_df_downscale <- list.files( downscaling_output_dir, paste0( '_', em, '_anthro_.*_0.1.csv' ), full.names = TRUE ) %>%
        lapply( read.csv ) %>%
        dplyr::bind_rows() %>%
        dplyr::group_by( sector, year ) %>%
        dplyr::summarise( value = sum( value ) ) %>%
        dplyr::ungroup() %>%
        tidyr::spread( year, value ) %>%
        dplyr::rename_all( make.names ) %>%
        dplyr::arrange( sector )

    # Take absolute difference and percentage difference
    X_year_list_downscale <- paste0( 'X', downscale_start_year:end_year )
    diag_diff_df_downscale <- cbind( checksum_df_downscale['sector'], abs( gridding_emissions_fin[ X_year_list_downscale ] - checksum_df_downscale[ X_year_list_downscale ] ) )
    diag_per_df_downscale <- cbind( checksum_df_downscale['sector'], ( diag_diff_df_downscale[ X_year_list_downscale ] / gridding_emissions_fin[ X_year_list_downscale ] ) * 100 )
    diag_per_df_downscale[ is.nan.df( diag_per_df_downscale ) ] <- NA
    colnames(diag_per_df_downscale)[1] <- 'sector'
    colnames(diag_diff_df_downscale)[1] <- 'sector'
}


# -----------------------------------------------------------------------------
# 6. Write-out

# Default res
out_name <- paste0( 'G.', em, '_bulk_emissions_checksum_comparison_diff' )
writeData( diag_diff_df, "DIAG_OUT", out_name )
out_name <- paste0( 'G.', em, '_bulk_emissions_checksum_comparison_per' )
writeData( diag_per_df, "DIAG_OUT", out_name )

# Fine res
if(downscale){
    # Save checksum files
    out_name <- paste0( 'G.', em, '_bulk_emissions_checksum_comparison_diff_01' )
    writeData( diag_diff_df_downscale, "DIAG_OUT", out_name )
    out_name <- paste0( 'G.', em, '_bulk_emissions_checksum_comparison_per_01' )
    writeData( diag_per_df_downscale, "DIAG_OUT", out_name )
}


# -----------------------------------------------------------------------------
# 7. Checksum warnings
# Get max deviation per sector, and combine that with user defined tolerance mapping file
error_check_df <- diag_per_df %>%
  tidyr::gather('year', 'em', X_year_list) %>%
  dplyr::group_by(sector) %>%
  stats::na.omit() %>%
  dplyr::summarise( M = max(em) ) %>%
  dplyr::left_join(checksum_tols, by = 'sector')

# In brackets to print together in correct order
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

# Downscale checksums
if(downscale){
    # Get max deviation per sector, and combine that with user defined tolerance mapping file
    error_check_df_downscale <- diag_per_df_downscale %>%
        tidyr::gather('year', 'em', X_year_list_downscale) %>%
        dplyr::group_by(sector) %>%
        na.omit() %>%
        dplyr::summarise( M = max(em) ) %>%
        dplyr::left_join(checksum_tols, by = 'sector')

    # In brackets to print together in correct order
    {
        print('===============================================================================')
        print('===============================================================================')
        # Warn / Error for any sectors that warrant it
        throw_error <- FALSE
        for(i in 1:dim(error_check_df_downscale)[1]){
            if(error_check_df_downscale[i, 'M'] > error_check_df_downscale[i, 'warning_tol']){
                print(paste0('Warning: Checksum diagnostics have found deviations in the ', error_check_df_downscale[i, 'sector'], ' sector beyond ', error_check_df_downscale[i, 'warning_tol'], '%.'))
            }
            if(error_check_df_downscale[i, 'M'] > error_check_df_downscale[i, 'error_tol']){
                print(paste0('ERROR: Checksum diagnostics have found deviations in the ', error_check_df_downscale[i, 'sector'], ' sector beyond ', error_check_df_downscale[i, 'error_tol'], '%.'))
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
}

# -----------------------------------------------------------------------------
# Run negative grid cell check
printLog( 'Running diagnostic to check for negative grid cell values ...' )
source( "../code/diagnostic/diag_negative_grid_check.R" )

# 8. END
logStop()
