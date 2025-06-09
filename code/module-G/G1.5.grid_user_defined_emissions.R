# ------------------------------------------------------------------------------
# Program Name: G1.5.grid_user_defined_emissions.R
# Authors: Noah Prime
# Date Last Updated: June 22, 2023
# Program Purpose: Grid aggregated emissions defined in custom_fuels_to_grid.csv
#                  (where we specify a fuel or combination of fuels to grid) into
#                  NetCDF grids for bulk emissions
# Input Files:  MED_OUT:    [em]_total_CEDS_emissions.csv
#               input/gridding/gridding_mappings:   CEDS_gridding_sector_selection.csv
# Output Files: MED_OUT:    gridded-emissions/CEDS_[em]_solidbiofuel_anthro_[year]_0.5_[CEDS_version].nc
#                           gridded-emissions/CEDS_[em]_solidbiofuel_anthro_[year]_0.5_[CEDS_version].csv
#               DIAG_OUT:   G.[em]_solidfuel_emissions_checksum_comparison_diff.csv
#                           G.[em]_solidfuel_emissions_checksum_comparison_per.csv
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Read in universal header files, support scripts, and start logging
headers <- c( 'data_functions.R', 'gridding_functions.R', 'nc_generation_functions.R',
              'point_source_util_functions.R')
log_msg <- "Gridding anthropogenic biomass emissions (excluding AIR) "
source( paste0( PARAM_DIR, "header.R" ) )
initialize( "G1.5.grid_user_defined_emissions.R", log_msg, headers )

library( yaml )

# ------------------------------------------------------------------------------
# 0.5 Initialize gridding setups

# Define emissions species variable
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"
res <- as.numeric( args_from_makefile[ 2 ] ) # introducing second command line argument as gridding resolution
if ( is.na( res ) ) res <- 0.5 # default gridding resolution of 0.5
fine_res <- 0.1

# Whether or not to perform downscaling
downscale <- FALSE
downscale_start_year <- 1980

# Set tolerance for a warning and/or error if checksum differences are too large
# This works on the percentage difference checksums, so set a threshold where if there are
# percent differences which exceeds the threshold, we will give warning or error.
warning_tol <- 0.05     # 0.05% i.e. 0.0005
error_tol <- 1          # 1% i.e. 0.01

# X years
x_years <- paste0('X', historical_pre_extension_year:end_year)

# Month columns made globaly available
month_columns <<- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

# Set up directories
output_dir          <- filePath( "MED_OUT",  "gridded-emissions/",     extension = "" )
total_grid_dir      <- filePath( "DIAG_OUT", "total-emissions-grids/", extension = "" )
proxy_dir           <- filePath( "MED_OUT",  "final_generated_proxy/", extension = "" )
proxy_backup_dir    <- filePath( "GRIDDING", "proxy-backup/",          extension = "" )
# SHP, AIR, and TANK sectors proxy pre-installed in different location
SAT_proxy_dir       <- filePath( "GRIDDING", "proxy/",                 extension = "" )
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
gridding_initialize( grid_resolution = 0.5,
                     start_year = 1750,
                     end_year = end_year,
                     load_masks = T,
                     load_seasonality_profile = T )


# ------------------------------------------------------------------------------
# 1. Read in files

# CEDS inventory without point sources
emissions <- readData( "MED_OUT", paste0( em, '_total_CEDS_emissions_no_point_sources'  ) )

# Need total emissions for checksums
total_emissions <- readData( "MED_OUT", paste0( em, '_total_CEDS_emissions'  ) )

# read in user selected gridding sectors
fuels_to_grid <- readData( "GRIDDING", domain_extension = "gridding_mappings/", file_name = "custom_fuels_to_grid.csv" )
fuel_list <- fuels_to_grid %>%
    dplyr::filter( !is.na(file_description) ) %>%
    dplyr::select(CEDS_fuel) %>%
    dplyr::pull()

# read in some mapping files
# read in the region location index, which indicates the location of each region mask in the 'world' matrix
location_index <- readData( "GRIDDING", domain_extension = "gridding_mappings/", file_name =  "country_location_index_05", meta = FALSE )
# read in the CEDS gridding sector mapping
ceds_gridding_mapping <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', file_name = 'CEDS_sector_to_gridding_sector_mapping', meta = FALSE )
# read in the proxy mapping
proxy_mapping <- readData( domain = "GRIDDING", domain_extension = "gridding_mappings/", 'proxy_mapping', meta = FALSE )
# read in the seasonality mapping
seasonality_mapping <- readData( domain = "GRIDDING", domain_extension = "gridding_mappings/", 'seasonality_mapping', meta = FALSE )
# read in the proxy substitution mapping
proxy_substitution_mapping   <- readData( 'MED_OUT', paste0( em, '_proxy_substitution_mapping'), meta = FALSE )
# read in CEDS final gridding sector name list
sector_name_mapping <- readData( domain = 'GRIDDING', domain_extension = "gridding_mappings/", 'CEDS_gridding_sectors', meta = FALSE )
sector_name_mapping <- unique( sector_name_mapping[ , c( "CEDS_fin_sector", "CEDS_fin_sector_short" ) ] )
# EDGAR grid sectors -> CEDS int gridding sectors
edgar_sector_replace_mapping <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'EDGAR_sector_replace_mapping', meta = F )
# Non-expanded sectors -> expanded sectors
expanded_sectors_map         <- readData( domain = 'MAPPINGS', file_name = 'old_to_new_sectors', extension = '.csv', meta = FALSE )
# Tolerances for checksums to give warning or throw error
checksum_tols                <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'checksums_error_tolerance', meta = FALSE )
# Seasonality profiles for regional seasonality data
seasonality_profiles         <- readData( 'MED_OUT', file_name = paste0(em, '_seasonality_mapping') )
# Proxy mapping for downscaling routine
proxy_mapping_downscale      <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'proxy_mapping_downscale', meta = FALSE )

# Update CEDS gridding mapping with new expanded sectors
ceds_gridding_mapping <- ceds_gridding_mapping %>%
  dplyr::left_join(expanded_sectors_map, by = c('CEDS_working_sector' = 'ceds_sector')) %>%
  dplyr::mutate( CEDS_working_sector = ifelse(is.na(new_sector), CEDS_working_sector, new_sector) ) %>%
  dplyr::select( -new_sector)

# Read in point source yml files
# point source yml files
point_source_files <- list.files( paste0(point_source_dir, '/', em ), '*.yml' )

if(length(point_source_files) == 0){
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

  # Only keep point sources using one of the given CEDS fuels
  point_source_df <- point_source_df %>%
      dplyr::filter( fuel %in% fuel_list )
}

# ------------------------------------------------------------------------------
# 2. Pre-processing

# 2.0 Selecting fuel and aggregating to int gridding sector ---------------------------------------

# extract file descriptor from user
file_descr <- fuels_to_grid %>%
    stats::na.omit() %>%
    dplyr::select(file_description) %>%
    dplyr::distinct() %>%
    dplyr::pull()

# check that only one file descriptor is used
if( length(file_descr) != 1){
    stop( 'Expecting just one file description' )
}


# For each iso & sector, getting emissions for user specified fuels
# a.) Convert the emissions data from CEDS working sectors to CEDS level 1 gridding sector
# b.) Dop non-matched sectors
# c.) Select only user specified fuels by right joining data to fuels_to_grid
# d.) Aggregate the missions at the gridding sectors and order by sector, iso
gridding_emissions <- ceds_gridding_mapping %>%
    dplyr::select( CEDS_working_sector, CEDS_int_gridding_sector_short ) %>%
    dplyr::inner_join( emissions, by = c( 'CEDS_working_sector' = 'sector' ) ) %>%
    dplyr::filter( !is.na( CEDS_int_gridding_sector_short ), CEDS_int_gridding_sector_short != 'AIR' ) %>%
    dplyr::rename("sector" = "CEDS_working_sector") %>%
    dplyr::right_join( fuels_to_grid %>%
                           dplyr::filter( !is.na(file_description) ) %>%
                           dplyr::select( CEDS_fuel ),
                       by = c( 'fuel' = 'CEDS_fuel' ) ) %>%
    dplyr::select( -fuel ) %>%
    dplyr::group_by( iso, sector, CEDS_int_gridding_sector_short ) %>%
    dplyr::summarise_at( x_years, sum ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange( CEDS_int_gridding_sector_short, iso ) %>%
    dplyr::filter( !is.na( CEDS_int_gridding_sector_short ) ) %>%
    as.data.frame()

# Emissions not used for gridding but for checksum diagnostic files
checksum_emissions <- ceds_gridding_mapping %>%
    dplyr::select( CEDS_working_sector, CEDS_int_gridding_sector_short ) %>%
    dplyr::inner_join( total_emissions, by = c( 'CEDS_working_sector' = 'sector' ) ) %>%
    dplyr::filter( !is.na( CEDS_int_gridding_sector_short ), CEDS_int_gridding_sector_short != 'AIR' ) %>%
    dplyr::rename("sector" = "CEDS_working_sector") %>%
    dplyr::right_join( fuels_to_grid %>%
                           dplyr::filter( !is.na(file_description) ) %>%
                           dplyr::select( CEDS_fuel ),
                       by = c( 'fuel' = 'CEDS_fuel' ) ) %>%
    dplyr::select( -fuel ) %>%
    dplyr::group_by( iso, sector, CEDS_int_gridding_sector_short ) %>%
    dplyr::summarise_at( x_years, sum ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange( CEDS_int_gridding_sector_short, iso ) %>%
    dplyr::filter( !is.na( CEDS_int_gridding_sector_short ) ) %>%
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
    gridcell_area_orig <- grid_area( res, all_lon = T )
    gridcell_area_proxy <- grid_area( fine_res, all_lon = T )
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
    dplyr::ungroup() %>%
    tidyr::drop_na()

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

# Get total emissions by final gridding sector for point sources using regional seasonality profiles
regional_ps_agg <- regional_seasonality_point_sources %>%
    dplyr::mutate(total = rowSums(.[month_columns])) %>%
    dplyr::select( id, year, CEDS_final_gridding_sector_short, iso, total ) %>%
    dplyr::mutate( year = paste0('X', year) ) %>%
    tidyr::spread(year, total)
if(nrow(regional_ps_agg) > 0){
    regional_ps_agg <- regional_ps_agg %>%
        dplyr::group_by( CEDS_final_gridding_sector_short ) %>%
        dplyr::summarise_at(x_years, sum) %>%
        dplyr::ungroup()
}

# Get total emissions by final gridding sector for point sources using gridded seasonality profiles
gridded_ps_agg <- gridded_seasonality_point_sources %>%
    dplyr::mutate(year = paste0('X', year) ) %>%
    tidyr::spread( year, value )
if(nrow(gridded_ps_agg) > 0){
    gridded_ps_agg <- gridded_ps_agg %>%
        dplyr::group_by( CEDS_final_gridding_sector_short ) %>%
        dplyr::summarise_at(x_years, sum) %>%
        dplyr::ungroup()
}

# Get total emissions by final gridding sector from both together
sum_ps_agg <- dplyr::bind_rows(regional_ps_agg, gridded_ps_agg)
if(nrow(regional_ps_agg) > 0 | nrow(gridded_ps_agg) > 0){
    sum_ps_agg <- sum_ps_agg %>%
        dplyr::group_by( CEDS_final_gridding_sector_short ) %>%
        dplyr::summarise_at( x_years, sum ) %>%
        dplyr::ungroup()
}

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
writeData(diff_df, domain = 'DIAG_OUT', fn = 'G.point-source_seasonality_split_checksums', meta=FALSE)

# Fail if can't pass this fairly weak threshold for equality:
# sum of all years and sectors can't have combined difference
# of greater than 0.00001
if(nrow(diff_df) > 0){
    if( sum(abs(diff_df[x_years])) > 1e-5 ){
        stop('Point source regional seasonality gridding emissions and gridded seasonality emissions do not sum up to total')
    }
}

# ------------------------------------------------------------------------------
# 3. Gridding and writing output data

# For now, the gridding routine uses nested for loops to go through every years
# gases and sectors. Future work could parallelize the year loop.

printLog( paste( 'Gridding', em, 'emissions for each year...' ) )

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


    # generate nc file for gridded one years emissions,
    # a checksum file is also generated along with the nc file
    # which summarize the emissions in mass by sector by month.
    # TODO: EDIT THIS AKIN TO BULK EMISSIONS
    final_grids <- generate_final_grids_nc_user_specified( int_grids_list, final_point_source_grids_list,
                                                           output_dir, grid_resolution, year, em,
                                                           sector_name_mapping, file_descr )

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
                                              fine_res, norm_dim, gridcell_area_proxy)

        # Grid one year's point source emissions (fine resolution)
        final_point_source_grids_list <- grid_point_sources_one_year( em,
                                                                      year,
                                                                      regional_seasonality_point_sources,
                                                                      gridded_seasonality_point_sources,
                                                                      fine_res )

        # generate nc file for each year's gridded emissions,
        # a checksum file is also generated along with the nc file
        # which summarize the emissions in mass by sector by month.
        # TODO: EDIT THIS AKIN TO BULK EMISSIONS
        final_grids_fine <- generate_final_grids_nc_user_specified( int_grids_list_fine,
                                                                    final_point_source_grids_list,
                                                                    downscale_output_dir,
                                                                    fine_res,
                                                                    year,
                                                                    em,
                                                                    sector_name_mapping,
                                                                    file_descr )
    }
}

close(pb)


# -----------------------------------------------------------------------------
# 4. Checksum
# TODO: Add downscaling checksum
printLog( 'Start checksum check' )

# calculate global total emissions by sector by year
gridding_emissions_fin <- ceds_gridding_mapping %>%
    dplyr::select( CEDS_int_gridding_sector_short, CEDS_final_gridding_sector_short ) %>%
    dplyr::distinct() %>%
    dplyr::right_join( checksum_emissions, by = "CEDS_int_gridding_sector_short" ) %>%
    dplyr::group_by( CEDS_final_gridding_sector_short ) %>%
    dplyr::summarise_at( vars( starts_with( 'X' ) ), sum ) %>%
    dplyr::ungroup() %>%
    dplyr::rename( sector = CEDS_final_gridding_sector_short ) %>%
    dplyr::arrange( sector )

# manually add in all 0 lines for AGR, SLV, and WST because they do have emissions from fuel biomass
temp_matrix <- matrix( 0, 3, length( year_list ) )
temp_df <- data.frame( temp_matrix )
temp_df$sector <- c( 'AGR', 'SLV', 'WST' )
names( temp_df ) <- c( paste0( 'X', year_list ), 'sector' )
temp_df <- temp_df[ ,c( 'sector', paste0( 'X', year_list ) ) ]
gridding_emissions_fin <- rbind( gridding_emissions_fin, temp_df)
gridding_emissions_fin <- gridding_emissions_fin[ order( gridding_emissions_fin$sector ), ]

# consolidate different checksum files to have total emissions by sector by year
checksum_df <- list.files( output_dir, paste0( '_', em, '_', file_descr, '_.*[.]csv' ), full.names = TRUE ) %>%
# checksum_df <- list.files( output_dir, paste0( '_', em, '_user_specified_.*[.]csv' ), full.names = TRUE ) %>%
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


# -----------------------------------------------------------------------------
# 5. Write-out

out_name <- paste0( 'G.', em, '_user_specified_emissions_checksum_comparison_diff' )
writeData( diag_diff_df, "DIAG_OUT", out_name )
out_name <- paste0( 'G.', em, '_user_specified_emissions_checksum_comparison_per' )
writeData( diag_per_df, "DIAG_OUT", out_name )


# -----------------------------------------------------------------------------
# 6. Checksum warnings
# Get max deviation per sector, and combine that with user defined tolerance mapping file
error_check_df <- diag_per_df %>%
  tidyr::gather('year', 'em', X_year_list) %>%
  dplyr::group_by(sector) %>%
  stats::na.omit() %>%
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
