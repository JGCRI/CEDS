# ------------------------------------------------------------------------------
# Program Name: G1.2.grid_subVOC_emissions.R
# Authors: Leyang Feng, Caleb Braun, Noah Prime
# Date Last Updated: June 22, 2023
# Program Purpose: Grid aggregated emissions into NetCDF grids for subVOCs
# Input Files:  MED_OUT:    CEDS_NMVOC_emissions_by_country_CEDS_sector_[CEDS_version].csv
# Output Files: MED_OUT:    gridded-emissions/CEDS_[VOCID]_anthro_[year]_0.5_[CEDS_version].nc
#                           gridded-emissions/CEDS_[VOCID]_anthro_[year]_0.5_[CEDS_version].csv
#               DIAG_OUT:   G.[VOCID]_bulk_emissions_checksum_comparison_per.csv
#                           G.[VOCID]_bulk_emissions_checksum_comparison_diff.csv
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Read in universal header files, support scripts, and start logging
headers <- c( 'data_functions.R', 'gridding_functions.R', 'nc_generation_functions.R', 'point_source_util_functions.R' )
log_msg <- "VOC speciation gridding "
source( paste0( PARAM_DIR, "header.R" ) )
initialize( "G1.2.grid_subVOC_emissions.R", log_msg, headers )


# ------------------------------------------------------------------------------
# 0.5 Initialize gridding setups

# Define emissions species variable
args_from_makefile <- commandArgs( TRUE )
VOC_em <- args_from_makefile[ 1 ]
if ( is.na( VOC_em ) ) VOC_em <- "VOC01"
res <- as.numeric( args_from_makefile[ 2 ] ) # introducing second command line argument as gridding resolution
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

# Other constants
em <- 'NMVOC'
month_columns <<- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

# Set tolerance for a warning and/or error if checksum differences are too large
# This works on the percentage difference checksums, so set a threshold where if there are
# percent differences which exceeds the threshold, we will give warning or error.
warning_tol <- 0.05     # 0.05% i.e. 0.0005
error_tol <- 1          # 1% i.e. 0.01

# Set up directories
output_dir          <- filePath( "MED_OUT",  "gridded-emissions/",     extension = "" )
total_grid_dir      <- filePath( "DIAG_OUT", "total-emissions-grids/", extension = "" )
SAT_proxy_dir       <- filePath( "GRIDDING", "proxy/",                 extension = "" )
proxy_dir           <- filePath( "MED_OUT",  "final_generated_proxy/", extension = "" )
proxy_backup_dir    <- filePath( "GRIDDING", "proxy-backup/",          extension = "" )
mask_dir            <- filePath( "GRIDDING", "mask/",                  extension = "" )
seasonality_dir     <- filePath( "GRIDDING", "seasonality/",           extension = "" )
final_emissions_dir <- filePath( "FIN_OUT",  "current-versions/",      extension = "" )
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
}

# Read in mapping files
# the location index indicates the location of each region mask in the 'world' matrix
location_index             <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'country_location_index_05', meta = F )
ceds_gridding_mapping      <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'CEDS_sector_to_gridding_sector_mapping', meta = F )
proxy_mapping              <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'proxy_mapping', meta = F )
seasonality_mapping        <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'seasonality_mapping', meta = F )
proxy_substitution_mapping   <- readData( 'MED_OUT', paste0( em, '_proxy_substitution_mapping'), meta = FALSE )
sector_name_mapping        <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'CEDS_gridding_sectors', meta = F )
sector_name_mapping        <- unique( sector_name_mapping[ , c( 'CEDS_fin_sector', 'CEDS_fin_sector_short' ) ] )
VOC_ratios                 <- readData( 'GRIDDING', domain_extension = "gridding_mappings/", 'VOC_ratio_AllSectors', meta = F )
VOC_names                  <- readData( 'GRIDDING', domain_extension = "gridding_mappings/", 'VOC_id_name_mapping', meta = F )
edgar_sector_replace_mapping <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'EDGAR_sector_replace_mapping', meta = F )
expanded_sectors_map         <- readData( domain = 'MAPPINGS', file_name = 'old_to_new_sectors', extension = '.csv', meta = FALSE )
checksum_tols                <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'checksums_error_tolerance', meta = FALSE )
seasonality_profiles         <- readData( 'MED_OUT', file_name = paste0(em, '_seasonality_mapping') )
proxy_mapping_downscale      <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'proxy_mapping_downscale', meta = FALSE )

# Update CEDS gridding mapping with new expanded sectors
ceds_gridding_mapping <- ceds_gridding_mapping %>%
  dplyr::left_join(expanded_sectors_map, by = c('CEDS_working_sector' = 'ceds_sector')) %>%
  dplyr::mutate( CEDS_working_sector = ifelse(is.na(new_sector), CEDS_working_sector, new_sector) ) %>%
  dplyr::select( -new_sector)

# point source yml files
point_source_files <- list.files( paste0(point_source_dir, '/', em ), '*.yml' )

if(length(point_source_files) == 0 ){
  cols <- c('id', 'name', 'location', 'longitude', 'latitude', 'units', 'CEDS_sector',
            'EDGAR_sector', 'fuel', 'iso', 'build_year', 'description', 'date', 'species',
            'data_source', x_years)
  point_source_df <- data.frame(matrix(ncol = length(cols), nrow = 0))
  colnames(point_source_df) <- cols
} else{
  # List of point source data frames
  point_source_list <- lapply( point_source_files, read_yml_all_ems,
                                paste0(point_source_dir, '/', em ) )

  # As data frame
  point_source_df <- do.call( rbind, point_source_list )
  point_source_df <- point_source_df %>%
      dplyr::mutate_at( vars(latitude, longitude, all_of(x_years)), as.numeric )
}

# ------------------------------------------------------------------------------
# 2. Pre-processing

# 2.0 Apply VOC ratio to emissions

# Get VOC ratios for given VOC emission species
VOC_ratios <- dplyr::select( VOC_ratios, iso, sector, !!VOC_em )

# Apply ratio to emissions (by CEDS int gridding sector)
voc_emissions <- ceds_gridding_mapping %>%
    dplyr::select( CEDS_working_sector, CEDS_int_gridding_sector_short ) %>%
    # Join with emissions data with sector mapping
    dplyr::inner_join( emissions, by = c( 'CEDS_working_sector' = 'sector' ) ) %>%
    # Remove AIR, and sectors with no matching gridding sector
    dplyr::filter( !is.na( CEDS_int_gridding_sector_short ) ) %>%
    dplyr::filter( !(CEDS_int_gridding_sector_short %in% c('AIR') ) ) %>%
    # Join with VOC ratios
    dplyr::inner_join( VOC_ratios, by = c( 'iso', 'CEDS_int_gridding_sector_short' = 'sector' ) ) %>%
    dplyr::rename( sector = CEDS_working_sector, voc_ratio = !!VOC_em ) %>%
    # Apply ratio to time series
    dplyr::mutate_at(x_years, ~.*voc_ratio ) %>%
    dplyr::select(-voc_ratio)

checksum_emissions <- ceds_gridding_mapping %>%
    dplyr::select( CEDS_working_sector, CEDS_int_gridding_sector_short ) %>%
    dplyr::inner_join( total_emissions, by = c( 'CEDS_working_sector' = 'sector' ) ) %>%
    dplyr::filter( !is.na( CEDS_int_gridding_sector_short ) ) %>%
    dplyr::group_by( iso, CEDS_int_gridding_sector_short ) %>%
    dplyr::summarise_at( x_years, sum ) %>%
    dplyr::ungroup() %>%
    dplyr::rename( sector = CEDS_int_gridding_sector_short ) %>%
    dplyr::filter( sector != 'AIR' ) %>%
    dplyr::arrange( sector, iso ) %>%
    as.data.frame()

# apply VOC ratio (multiply all years by ratio column)
checksum_emissions <- checksum_emissions %>%
    dplyr::inner_join( VOC_ratios, by = c( 'iso', 'sector' ) ) %>%
    dplyr::rename( voc_ratio = !!VOC_em ) %>%
    # Apply ratio to time series
    dplyr::mutate_at(x_years, ~.*voc_ratio ) %>%
    dplyr::select(-voc_ratio)

# apply VOC ratio to point sources (multiply all years by ratio column)
point_source_df <- point_source_df %>%
    dplyr::mutate( CEDS_sector = as.character(CEDS_sector),
                   iso = as.character(iso) ) %>%
    dplyr::inner_join( ceds_gridding_mapping, by = c('CEDS_sector' = 'CEDS_working_sector') ) %>%
    # Remove AIR, and sectors with no matching gridding sector
    dplyr::filter( !is.na( CEDS_int_gridding_sector_short ) ) %>%
    dplyr::filter( !(CEDS_int_gridding_sector_short %in% c('AIR') ) ) %>%
    # Join with VOC ratios
    dplyr::inner_join( VOC_ratios, by = c('iso', 'CEDS_int_gridding_sector_short' = 'sector') ) %>%
    dplyr::rename( voc_ratio = !!VOC_em ) %>%
    # Apply ratio to time series
    dplyr::mutate_at(x_years, ~.*voc_ratio ) %>%
    dplyr::select(-voc_ratio)

# 2.1 Get regional seasonality mapping to 1750 ----------------------------------

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


# 2.2 Proxy and gridded seasonality files ---------------------------------------

# Move pre-installed proxy to final_proxy folder (no overwriting)
pre_installed <- list.files(SAT_proxy_dir)
file.copy( from = paste0(SAT_proxy_dir, pre_installed),
           to = paste0(proxy_dir, pre_installed),
           overwrite = FALSE )

# List of proxy files
proxy_files <- list( primary = list.files( proxy_dir ), backup = list.files( proxy_backup_dir ) )

# Extend mappings to last year
proxy_mapping <- extendProxyMapping( proxy_mapping )
seasonality_mapping <- extendSeasonalityMapping( seasonality_mapping )
full_seasonality_profiles <- extend_iso_seasonality_profile_mapping(seasonality_profiles, x_years)

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

# 2.3 Create monthly emissions by iso/year/int gridding sector -----------------
regional_seasonality_emissions_profiles <- voc_emissions %>%
    tidyr::gather( year, emissions, all_of(x_years) ) %>%
    dplyr::mutate( year = as.numeric(gsub('X', '', year)) ) %>%
    dplyr::right_join( full_seasonality_profiles, by = c('iso', 'sector', 'year', 'CEDS_int_gridding_sector_short') )

# Apply seasonality and aggregate to CEDS int gridding sector
regional_seasonality_emissions <- regional_seasonality_emissions_profiles %>%
    dplyr::mutate_at(month_columns, ~.*emissions ) %>%
    dplyr::group_by( iso, CEDS_int_gridding_sector_short, year ) %>%
    dplyr::summarise_at(month_columns, sum) %>%
    dplyr::ungroup() %>%
    stats::na.omit()

# 2.4 Get data with gridded seasonality ----------------------------------------

# Get combinations of iso/sector/year covered in regional seasonality profiles
regional_combos <- full_seasonality_profiles %>%
    dplyr::select( iso, sector, year )

# Anti join emissions data with what's present in regional combos
# Aggregate to CEDS int gridding sector
gridded_seasonality_emissions <- voc_emissions %>%
    tidyr::gather( year, value, all_of(x_years) ) %>%
    dplyr::mutate( year = as.numeric(gsub('X', '', year)) ) %>%
    dplyr::anti_join( regional_combos, by = c('iso', 'sector', 'year') ) %>%
    dplyr::mutate( year = paste0('X', year) ) %>%
    tidyr::spread( year, value ) %>%
    dplyr::group_by( iso, CEDS_int_gridding_sector_short ) %>%
    dplyr::summarise_at( x_years, sum ) %>%
    dplyr::ungroup() %>%
    stats::na.omit()

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
diff_df <- voc_emissions %>%
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
    writeData(diff_df, domain = 'DIAG_OUT', fn = paste0( 'G.', VOC_em, 'point-source_seasonality_split_checksums'), meta=FALSE)

    # Fail if can't pass this fairly weak threshold for equality:
    # sum of all years and sectors can't have combined difference
    # of greater than 0.00001
    if( sum(abs(diff_df[x_years])) > 1e-5 ){
        stop('Point source regional seasonality gridding emissions and gridded seasonality emissions do not sum up to total')
    }

} else{
    # Write out diagnositc file
    writeData(data.frame(column = paste0('no point sources for ', VOC_em)), domain = 'DIAG_OUT', fn = paste0( 'G.', VOC_em, 'point-source_seasonality_split_checksums'), meta=FALSE)
}

# ------------------------------------------------------------------------------
# 3. Gridding and writing output data

# Create directory in intermediate-output for grids without point sources to be saved
incomplete_grid_dir <- '../intermediate-output/incomplete-grids/'
dir.create( incomplete_grid_dir, showWarnings = FALSE)

# For now, the gridding routine uses nested for loops to go through every years
# gases and sectors. May consider to take away for loop for sectors and keep year loops
# for future parallelization

printLog( paste( 'Start', VOC_em, 'gridding for each year' ) )

pb <- txtProgressBar(min = 0, max = length(year_list), style = 3)

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

    # generate nc file for gridded one years subVOC emissions,
    # a checksum file is also generated along with the nc file
    # which summarize the emissions in mass by sector by month.
    # TODO: EDIT THIS AKIN TO BULK EMISSIONS
    final_grids <- generate_final_grids_nc_subVOC( int_grids_list,
                                    final_point_source_grids_list,
                                    output_dir,
                                    grid_resolution,
                                    year,
                                    em = 'NMVOC',
                                    VOC_em = VOC_em,
                                    VOC_names,
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

    # Downscale
    if(downscale & (year >= downscale_start_year)){
        # Intermediate sector names
        sector_list <- names(int_grids_list)

        # Initialize sector list
        int_grids_list_fine <- downscale_year(em, year, sector_list, int_grids_list,
                                              proxy_files_downscale, proxy_mapping_downscale,
                                              fine_proxy_dir, fine_proxy_backup_dir,
                                              fine_res, norm_dim,
                                              gridcell_area_coarse, gridcell_area_fine)

        # Grid one year's point source emissions (fine resolution)
        final_point_source_grids_list <- grid_point_sources_one_year( em,
                                                                      year,
                                                                      regional_seasonality_point_sources,
                                                                      gridded_seasonality_point_sources,
                                                                      fine_res )

        # generate nc file for each year's gridded emissions,
        # a checksum file is also generated along with the nc file
        # which summarize the emissions in mass by sector by month.
        final_grids_fine <- generate_final_grids_nc_subVOC( int_grids_list_fine,
                                                     final_point_source_grids_list,
                                                     downscaling_output_dir,
                                                     fine_res,
                                                     year,
                                                     em,
                                                     VOC_em,
                                                     VOC_names,
                                                     sector_name_mapping )
    }

}

close(pb)


# -----------------------------------------------------------------------------
# 4. Diagnostic: checksum
# TODO: Add downscaling checksum

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
checksum_df <- list.files( output_dir, paste0( '_', VOC_em, '_anthro.*0\\.5[.]csv' ), full.names = TRUE ) %>%
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

# Downscale checksums
if(downscale){
    # Total emissions by sector by year from grids
    checksum_df_downscale <- list.files( downscaling_output_dir, paste0( '_', VOC_em, '_anthro_.*_0.1.csv' ), full.names = TRUE ) %>%
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
# 5. Write-out

out_name <- paste0( 'G.', VOC_em, '_anthro_checksum_comparison_diff' )
writeData( diag_diff_df, "DIAG_OUT", out_name )
out_name <- paste0( 'G.', VOC_em, '_anthro_checksum_comparison_per' )
writeData( diag_per_df, "DIAG_OUT", out_name )

# Fine res
if(downscale){
    # Save checksum files
    out_name <- paste0( 'G.', VOC_em, '_anthro_checksum_comparison_diff_01' )
    writeData( diag_diff_df_downscale, "DIAG_OUT", out_name )
    out_name <- paste0( 'G.', VOC_em, '_anthro_checksum_comparison_per_01' )
    writeData( diag_per_df_downscale, "DIAG_OUT", out_name )
}


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
logStop()
