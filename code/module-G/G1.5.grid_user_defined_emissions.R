# ------------------------------------------------------------------------------
# Program Name: G1.5.grid_user_defined_emissions.R
# Authors: Noah Prime
# Date Last Updated: June 22, 2023
# Program Purpose: Grid aggregated emissions defined in user defined file into NetCDF grids for bulk emissions
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

# Set tolerance for a warning and/or error if checksum differences are too large
# This works on the percentage difference checksums, so set a threshold where if there are
# percent differences which exceeds the threshold, we will give warning or error.
warning_tol <- 0.05     # 0.05% i.e. 0.0005
error_tol <- 1          # 1% i.e. 0.01

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

# Update CEDS gridding mapping with new expanded sectors
ceds_gridding_mapping <- ceds_gridding_mapping %>%
  dplyr::left_join(expanded_sectors_map, by = c('CEDS_working_sector' = 'ceds_sector')) %>%
  dplyr::mutate( CEDS_working_sector = ifelse(is.na(new_sector), CEDS_working_sector, new_sector) ) %>%
  dplyr::select( -new_sector)

# Read in point source yml files
# point source yml files
point_source_files <- list.files( paste0(point_source_dir, '/', em ), '*.yml' )

if(length(point_source_files) == 0){
  cols <- c('id', 'name', 'location', 'longitude', 'latitude', 'units', 'CEDS_sector',
            'EDGAR_sector', 'fuel', 'iso', 'build_year', 'description', 'date', 'species',
            'data_source', paste0('X', 1750:2019))
  point_source_df <- data.frame(matrix(ncol = length(cols), nrow = 0))
  colnames(point_source_df) <- cols
} else{
  # List of point source data frames
  point_source_list <- lapply( point_source_files, read_yml_all_ems,
                              paste0(point_source_dir, '/', em ) )

  # As data frame
  point_source_df <- do.call( rbind, point_source_list )
  point_source_df <- point_source_df %>%
      dplyr::mutate_at( vars(latitude, longitude, X1750:X2019), as.numeric )

  # Only keep point sources using one of the given CEDS fuels
  point_source_df <- point_source_df %>%
      dplyr::filter( fuel %in% fuel_list )
}

# ------------------------------------------------------------------------------
# 2. Pre-processing

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
    dplyr::rename( sector = CEDS_int_gridding_sector_short ) %>%
    dplyr::right_join( fuels_to_grid %>%
                           dplyr::filter( !is.na(file_description) ) %>%
                           dplyr::select( CEDS_fuel ),
                       by = c( 'fuel' = 'CEDS_fuel' ) ) %>%
    dplyr::select( -fuel, -CEDS_working_sector ) %>%
    dplyr::group_by( iso, sector ) %>%
    dplyr::summarise_at( paste0( 'X', year_list ), sum ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange( sector, iso ) %>%
    dplyr::filter( !is.na( sector ) ) %>%
    as.data.frame()

# Emissions not used for gridding but for checksum diagnostic files
checksum_emissions <- ceds_gridding_mapping %>%
    dplyr::select( CEDS_working_sector, CEDS_int_gridding_sector_short ) %>%
    dplyr::inner_join( total_emissions, by = c( 'CEDS_working_sector' = 'sector' ) ) %>%
    dplyr::filter( !is.na( CEDS_int_gridding_sector_short ), CEDS_int_gridding_sector_short != 'AIR' ) %>%
    dplyr::rename( sector = CEDS_int_gridding_sector_short ) %>%
    dplyr::right_join( fuels_to_grid %>%
                           dplyr::filter( !is.na(file_description) ) %>%
                           dplyr::select( CEDS_fuel ),
                       by = c( 'fuel' = 'CEDS_fuel' ) ) %>%
    dplyr::select( -fuel, -CEDS_working_sector ) %>%
    dplyr::group_by( iso, sector ) %>%
    dplyr::summarise_at( paste0( 'X', year_list ), sum ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange( sector, iso ) %>%
    dplyr::filter( !is.na( sector ) ) %>%
    as.data.frame()

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

# ------------------------------------------------------------------------------
# 3. Gridding and writing output data

# For now, the gridding routine uses nested for loops to go through every years
# gases and sectors. Future work could parallelize the year loop.

printLog( paste( 'Gridding', em, 'emissions for each year...' ) )

pb <- txtProgressBar(min = 0, max = length(year_list), style = 3)

for ( year in year_list ) {
    setTxtProgressBar(pb, year - min(year_list))

    # grid one years emissions
    int_grids_list <- grid_one_year( year, em, grid_resolution, gridding_emissions, location_index,
                                     proxy_mapping, proxy_substitution_mapping, proxy_files )

    # TODO: Should save intermediate point-source-less grids into incomplete grids
    #       if we want to downscale these grids later

    # generate nc file for gridded one years emissions,
    # a checksum file is also generated along with the nc file
    # which summarize the emissions in mass by sector by month.
    generate_final_grids_nc_user_specified( int_grids_list, output_dir, grid_resolution, year,
                                            em, sector_name_mapping, seasonality_mapping,
                                            file_descr, ceds_gridding_mapping, edgar_sector_replace_mapping,
                                            point_source_df )
}

close(pb)


# -----------------------------------------------------------------------------
# 4. Checksum
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


# -----------------------------------------------------------------------------
# 5. Write-out 

out_name <- paste0( 'G.', em, '_user_specified_emissions_checksum_comparison_diff' )
writeData( diag_diff_df, "DIAG_OUT", out_name )
out_name <- paste0( 'G.', em, '_user_specified_emissions_checksum_comparison_per' )
writeData( diag_per_df, "DIAG_OUT", out_name )
diag_per_df <- diag_per_df %>%
  dplyr::rename('sector' = 'checksum_df$sector')
diag_diff_df <- diag_diff_df %>%
  dplyr::rename('sector' = 'checksum_df$sector')


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
