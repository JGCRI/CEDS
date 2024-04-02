# Header --------------------------------------------------------------------
# Program Name: G4.1.downscale_bulk_emissions.R
# Authors: Hamza Ahsan, Noah Prime
# Date Last Updated: June 22, 2023
# Program Purpose: Generate annual NetCDF grid files at 0.1 deg resolution
# Input Files:
#   - Matrices (.Rd files) of annual sectoral grids at 0.5 deg resolution with no point sources
#   - Proxy files at 0.1 deg reesolution with no point sources
#   - Seasonality profiles
# Output Files:
#   - Annual NetCDF files at 0.1 deg resolution
# Notes: Works only for bulk emissions at the moment


# 0. Read in global settings and headers --------------------------------------

# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Read in universal header files, support scripts, and start logging
headers <- c( 'data_functions.R', 'gridding_functions.R', 'nc_generation_functions.R',
              '../diagnostic/diag_utility_functions.R', 'point_source_util_functions.R')
log_msg <- "Generating downscaled gridded data"
source( paste0( PARAM_DIR, "header.R" ) )
initialize( "G4.1.downscale_bulk_emissions.R", log_msg, headers )

# Define emissions species and gridding resolution
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"

# Load raster library (for some reason not read in by default through renv)
library('raster')

# Set tolerance for a warning and/or error if checksum differences are too large
# This works on the percentage difference checksums, so set a threshold where if there are
# percent differences which exceeds the threshold, we will give warning or error.
warning_tol <- 0.05     # 0.05% i.e. 0.0005
error_tol <- 1          # 1% i.e. 0.01


# 0.5 Initialize gridding setups --------------------------------------------

# Degree of original and new resolution grids
orig_res <- 0.5
new_res <- 0.1

# Year to start downscaling
start_year <- 2000


# Set up directories
input_dir           <- filePath( "MED_OUT",  "incomplete-grids/",              extension = "" )
output_dir          <- filePath( "MED_OUT",  "gridded-emissions_01/",           extension = "" )
proxy_dir           <- filePath( "MED_OUT",  "final_generated_proxy_0.1/",      extension = "" )
proxy_backup_dir    <- filePath( "GRIDDING", "proxy-backup-01/",                extension = "" )
point_source_dir    <- filePath( 'MED_OUT', paste0( 'full_point_source_scaled_yml/', em, '/'), extension = '' )
final_emissions_dir <- filePath( "FIN_OUT",  'current-versions/',                       extension = "" )
seasonality_dir     <- filePath( "GRIDDING", "seasonality/",           extension = "" )
mask_dir            <- filePath( "GRIDDING", "mask/",                  extension = "" )

# Initialize the gridding parameters
gridding_initialize( grid_resolution = new_res,
                     start_year = start_year,
                     end_year = end_year,
                     load_masks = T,
                     load_seasonality_profile = T )

# 1. Read in files ------------------------------------------------------

# 1.1 Mapping and proxy files -------------------------------------------
proxy_mapping       <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'proxy_mapping_downscale', meta = FALSE )
sector_name_mapping <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'CEDS_gridding_sectors', meta = FALSE )
sector_name_mapping <- distinct(sector_name_mapping[ , c( 'CEDS_fin_sector', 'CEDS_fin_sector_short' ) ])
ceds_gridding_mapping        <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'CEDS_sector_to_gridding_sector_mapping', meta = FALSE )
edgar_sector_replace_mapping <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/',
                                          'EDGAR_sector_replace_mapping', meta = F )
seasonality_mapping          <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'seasonality_mapping', meta = FALSE )
proxy_files         <- list( primary = list.files( proxy_dir ), backup = list.files( proxy_backup_dir ) )
expanded_sectors_map         <- readData( domain = 'MAPPINGS', file_name = 'old_to_new_sectors', extension = '.csv', meta = FALSE )
# Tolerances for checksums to give warning or throw error
checksum_tols                <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'checksums_error_tolerance', meta = FALSE )


# Update CEDS gridding mapping with new expanded sectors
ceds_gridding_mapping <- ceds_gridding_mapping %>%
  dplyr::left_join(expanded_sectors_map, by = c('CEDS_working_sector' = 'ceds_sector')) %>%
  dplyr::mutate( CEDS_working_sector = ifelse(is.na(new_sector), CEDS_working_sector, new_sector) ) %>%
  dplyr::select( -new_sector)


# 1.2 Extend proxy mapping file ------------------------------------------

# Extend proxy mapping to end year
last_proxy_data_year <- as.numeric( max( proxy_mapping$year ) )
last_proxy_data_year_string <- paste( last_proxy_data_year )
extra_years_needed <- (last_proxy_data_year+1):end_year
extra_years_needed_string <- paste( extra_years_needed )
final_years <- start_year:end_year
final_years_string <- paste( final_years )

# Extend seasonality mapping
seasonality_mapping <- extendSeasonalityMapping( seasonality_mapping )

# Supply days in month and global grid area at new resolution
days_in_month <- c( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 )
global_grid_area <- grid_area( new_res, all_lon = T )

# Proxy mapping file updated to support all years
proxy_mapping <- proxy_mapping %>%
    tidyr::spread( year, proxybackup_file ) %>%
    dplyr::mutate_at( extra_years_needed_string, funs( identity (  !!rlang::sym( last_proxy_data_year_string ) ) ) ) %>%
    tidyr::gather( key = year, value = proxybackup_file, all_of(final_years_string) ) %>%
    dplyr::arrange( em, sector, year, proxy_file ) %>%
    dplyr::select( em, sector, year, proxy_file, proxybackup_file ) %>%
    dplyr::filter( !is.na( proxybackup_file ) )


# 1.3 Read in point source yml files ---------------------------------------

# point source yml files
point_source_files <- list.files( point_source_dir, '*.yml' )

if(length(point_source_files) == 0 ){
  cols <- c('id', 'name', 'location', 'longitude', 'latitude', 'units', 'CEDS_sector',
            'EDGAR_sector', 'fuel', 'iso', 'build_year', 'description', 'date', 'species',
            'data_source', paste0('X', 1750:2019))
  point_source_df <- data.frame(matrix(ncol = length(cols), nrow = 0))
  colnames(point_source_df) <- cols
} else{
    # List of point source data frames
    point_source_list <- lapply( point_source_files,
                                read_yml_all_ems,
                                point_source_dir )

    # As data frame
    point_source_df <- do.call( rbind, point_source_list )
    point_source_df <- point_source_df %>%
        dplyr::mutate_at( vars(latitude, longitude, X1750:X2019), as.numeric )
}


# 1.4 Files for checksums ----------------------------------------------

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

# TOTAL EMISSIONS CEDS INVENTORY
total_emissions <- readData( "FIN_OUT", domain_extension = "current-versions/", target_filename )


# 2. Downscaling function ---------------------------------------------------


# Downscale grid
downscale_grid <- function( grid, reference, fact ){
    # Reference grid as raster (and aggregated)
    reference_raster <- raster( reference )
    reference_raster_agg <- aggregate( reference_raster, fact = fact, fun = sum )

    # Base resolution grid as raster
    grid_raster <- raster( grid )

    # normalize proxy by norm_dim x norm_dim grid chunks
    reference_norm <- reference_raster / disaggregate( reference_raster_agg, fact )

    # Multiply grid by proportions
    out_grid <- disaggregate( grid_raster, fact ) * reference_norm

    # Replace NA with 0
    out_grid[ is.na( out_grid ) ] <- 0

    # Unused emissions
    unused <- grid_raster * ( reference_raster_agg == 0 )
    sum_unused <- cellStats( unused, sum )

    return( list( 'grid' = as.matrix( out_grid ), 'unused' = as.matrix( unused ), 'sum_unused' = sum_unused ) )
}



# 3. Iterate through years and downscale -----------------------------------

# Grid dimension to normalize over (must be integer since we are working with whole
# cells and not partial). For example, if downscaling from 0.5 deg to 0.25deg, each
# single cell of the 0.5 grid will map to a 2x2 grid on the 0.25 grid (0.5/0.25=2).
# Therefore, we need to normalize every (non-overlapping) 2x2 grid array on the
# 0.25 grid.
norm_dim <- orig_res/new_res
if(round(norm_dim) != norm_dim){
    stop( "norm_dim must be an integer")
}

# Generate grid cell area arrays for both resolutions
gridcell_area_orig <- grid_area( orig_res, all_lon = T )
gridcell_area_proxy <- grid_area( new_res, all_lon = T )

# List of sectors to downscale
sector_list <- c("AGR", "ENE", "IND", "TRA", "RCOO", 'RCORC', "SLV", "SHP", "WST")
# Final sector list
final_sector_list <- c("AGR", "ENE", "IND", "TRA", "RCO", "SLV", "SHP", "WST")

# Progress bar as we iterate through the years
pb <- txtProgressBar(min = 0, max = length(final_years) * length(sector_list), style = 3)
stepi <- 0

for(year in final_years){

    # Initialize sector list
    monthly_totals <- list()
    final_grids <- list()

    # 3.2 Iterate through sectors and downscale -----------------------------------
    for(sector in sector_list) {

        # Load grid to downscale
        file_name <- paste( em, sector, year, '0.5', sep = '_' )
        input_file <- paste0( input_dir, file_name, '.Rd' )
        x <- load( input_file )
        input_grid <- get( x )

        # Load proxy
        # Group together subsector proxies
        if (sector %in% "ENE") {
            # Load primary proxy file
            ELEC_proxy <- get_proxy( em, year, "ELEC", proxy_mapping, proxy_files, proxy_type = 'primary' )
            ETRN_proxy <- get_proxy( em, year, "ETRN", proxy_mapping, proxy_files, proxy_type = 'primary' )
            FFFI_proxy <- get_proxy( em, year, "FFFI", proxy_mapping, proxy_files, proxy_type = 'primary' )

            primary_proxy <- ELEC_proxy + ETRN_proxy + FFFI_proxy

            # Load backup proxy file (doesn't matter which subsector since all have population as backup)
            primary_backup <- get_proxy( em, year, "ELEC", proxy_mapping, proxy_files, proxy_type = 'backup' )

        } else if (sector %in% "IND") {
            # Load primary proxy file
            INDC_proxy <- get_proxy( em, year, "INDC", proxy_mapping, proxy_files, proxy_type = 'primary' )
            INPU_proxy <- get_proxy( em, year, "INPU", proxy_mapping, proxy_files, proxy_type = 'primary' )

            primary_proxy <- INDC_proxy + INPU_proxy

            # Load backup proxy file
            primary_backup <- get_proxy( em, year, "INDC", proxy_mapping, proxy_files, proxy_type = 'backup' )

        } else if (sector %in% "TRA") {
            # Load primary proxy file
            NRTR_proxy <- get_proxy( em, year, "NRTR", proxy_mapping, proxy_files, proxy_type = 'primary' )
            ROAD_proxy <- get_proxy( em, year, "ROAD", proxy_mapping, proxy_files, proxy_type = 'primary' )

            primary_proxy <- NRTR_proxy + ROAD_proxy

            # Load backup proxy file
            primary_backup <- get_proxy( em, year, "NRTR", proxy_mapping, proxy_files, proxy_type = 'backup' )

        } else if (sector %in% c('RCORC','RCOO') ){
            # Load primary proxy file
            primary_proxy <- get_proxy( em, year, 'RCO', proxy_mapping, proxy_files, proxy_type = 'primary' )

            # Load backup proxy file
            primary_backup <- get_proxy( em, year, 'RCO', proxy_mapping, proxy_files, proxy_type = 'backup' )
        } else {
            # Load primary proxy file
            primary_proxy <- get_proxy( em, year, sector, proxy_mapping, proxy_files, proxy_type = 'primary' )

            # Load backup proxy file
            primary_backup <- get_proxy( em, year, sector, proxy_mapping, proxy_files, proxy_type = 'backup' )
        }

        # Load global proxy (all ones) which is the same as backup proxy for shipping
        default_proxy <- get_proxy( em, year, "SHP", proxy_mapping, proxy_files, proxy_type = 'backup' )

        # Convert from flux to annual kt
        input_grid <- input_grid * gridcell_area_orig * 365 * 24 * 60 * 60 * 0.000001

        # Downscale routine

        # Use proxy to downscale
        downscale1 <- downscale_grid( input_grid, primary_proxy, norm_dim )

        # If emissions from grid were lost due to proxy being zero, disaggregate
        # left-over emissions using backup proxy
        if( downscale1$sum_unused > 0 ){

            # Use back up proxy to downscale
            downscale2 <- downscale_grid( downscale1$unused, primary_backup, norm_dim )
            # Add left out emissions back to full grid
            downscale1$grid <- downscale1$grid + downscale2$grid

            # If still the back-up proxy leaves out emissions, use default global
            # proxy. This is a grid of all ones, so it just uniformly distributes
            # over the fine cells.
            if( downscale2$sum_unused > 0 ){

                # Downscale using default global proxy
                downscale3 <- downscale_grid( downscale2$unused, default_proxy, norm_dim )
                # Add left out emissions to full grid
                downscale1$grid <- downscale1$grid + downscale3$grid

            }

        }


        # Convert from mass back to flux (kt to kg m-2 s-1)
        flux <- downscale1$grid / ( gridcell_area_proxy * ( 365 * 24 * 60 * 60 ) * 0.000001 )

        # Add point sources
        flux_with_sources <- add_point_sources( flux, em, sector, year, new_res, ceds_gridding_mapping, edgar_sector_replace_mapping, point_source_df )

        # Add seasonality
        flux_with_seasonality <- add_seasonality( flux_with_sources, em, sector, year, days_in_month, new_res, seasonality_mapping )

        # Add to list of final grids
        final_grids[[sector]] <- flux_with_seasonality

        # Monthly totals
        monthly_tot <- sum_monthly_em( flux_with_seasonality, em, sector, year, days_in_month, global_grid_area, seasonality_mapping )

        # Add to list of monthly totals
        monthly_totals[[sector]] <- monthly_tot

        stepi <- stepi + 1
        setTxtProgressBar(pb, stepi)

    }

    # Combine RCORC and RCOO intermediate sectors into final sector RCO
    monthly_totals[['RCO']] <- data.frame( em = em, sector = 'RCO', year = year, month = 1 : 12, units = 'kt',
                                           value = monthly_totals[['RCORC']]$value + monthly_totals[['RCOO']]$value, stringsAsFactors = F )
    final_grids[['RCO']] <- final_grids[['RCORC']] + final_grids[['RCOO']]


    # 3.3 Create output NetCDF files -----------------------------------

    # Write out csv file of all monthly totals
    monthly_totals <- within.list(monthly_totals, rm('RCORC', 'RCOO'))
    total_month_em <- do.call(rbind, monthly_totals)
    summary_name <- paste0( output_dir, 'CEDS_', em, '_anthro_', year, '_', new_res, '.csv' )
    write.csv( total_month_em, file = summary_name, row.names = F )

    # NetCDF generation starts here
    lons <- seq( -180 + new_res / 2, 180 - new_res / 2, new_res )
    lats <- seq( -90 + new_res / 2, 90 - new_res / 2, new_res )
    base_days <- ( year - 1750 ) * 365
    time <- floor( c( 15.5, 45, 74.5, 105, 135.5, 166, 196.5, 227.5, 258, 288.5, 319, 349.5 ) )
    time <- time + base_days
    londim <- ncdim_def( "lon", "degrees_east", as.double( lons ), longname = 'longitude' )
    latdim <- ncdim_def( "lat", "degrees_north", as.double( lats ), longname = 'latitude' )
    timedim <- ncdim_def( "time", paste0( "days since 1750-01-01 0:0:0" ), as.double( time ),
                          calendar = '365_day', longname = 'time' )
    dim_list <- list( londim, latdim, timedim )
    lon_bnds_data <- cbind( seq( -180, ( 180 - new_res ), new_res ),
                            seq( ( -180 + new_res ), 180, new_res ) )
    lat_bnds_data <- cbind( seq( -90, (90 - new_res) , new_res),
                            seq( ( -90 + new_res ), 90, new_res ) )
    bnds <- 1 : 2
    bndsdim <- ncdim_def( "bnds", '', as.integer( bnds ), longname = 'bounds', create_dimvar = F )
    time_bnds_data <- cbind( c( 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 ),
                             c( 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 ) )
    time_bnds_data <- time_bnds_data + base_days

    data_unit <- 'kg m-2 s-1'
    missing_value <- 1.e20

    # Define nc variables
    AGR <- ncvar_def( 'AGR', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$CEDS_fin_sector_short == 'AGR', 'CEDS_fin_sector' ], prec = 'float', compression = 5 )
    ENE <- ncvar_def( 'ENE', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$CEDS_fin_sector_short == 'ENE', 'CEDS_fin_sector' ], prec = 'float', compression = 5 )
    IND <- ncvar_def( 'IND', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$CEDS_fin_sector_short == 'IND', 'CEDS_fin_sector' ], prec = 'float', compression = 5 )
    TRA <- ncvar_def( 'TRA', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$CEDS_fin_sector_short == 'TRA', 'CEDS_fin_sector' ], prec = 'float', compression = 5 )
    RCO <- ncvar_def( 'RCO', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$CEDS_fin_sector_short == 'RCO', 'CEDS_fin_sector' ], prec = 'float', compression = 5 )
    SLV <- ncvar_def( 'SLV', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$CEDS_fin_sector_short == 'SLV', 'CEDS_fin_sector' ], prec = 'float', compression = 5 )
    SHP <- ncvar_def( 'SHP', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$CEDS_fin_sector_short == 'SHP', 'CEDS_fin_sector' ], prec = 'float', compression = 5 )
    WST <- ncvar_def( 'WST', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$CEDS_fin_sector_short == 'WST', 'CEDS_fin_sector' ], prec = 'float', compression = 5 )

    lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
    lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
    time_bnds <- ncvar_def( 'time_bnds', '', list( bndsdim, timedim ), prec = 'double' )

    # Generate nc file name
    nc_file_name <- paste0( 'CEDS_', em, '_anthro_', year, '_', new_res, '.nc' )

    # Generate the var_list
    variable_list <- list( AGR, ENE, IND, TRA, RCO, SLV, SHP, WST, lat_bnds, lon_bnds, time_bnds )

    # create new nc file
    nc_new <- nc_create( paste0( output_dir, nc_file_name ), variable_list, force_v4 = T )

    ncvar_put( nc_new, AGR, rotate_lat_lon( list( final_grids[['AGR']] ) )[[1]] )
    ncvar_put( nc_new, ENE, rotate_lat_lon( list( final_grids[['ENE']] ) )[[1]] )
    ncvar_put( nc_new, IND, rotate_lat_lon( list( final_grids[['IND']] ) )[[1]] )
    ncvar_put( nc_new, TRA, rotate_lat_lon( list( final_grids[['TRA']] ) )[[1]] )
    ncvar_put( nc_new, SLV, rotate_lat_lon( list( final_grids[['SLV']] ) )[[1]] )
    ncvar_put( nc_new, WST, rotate_lat_lon( list( final_grids[['WST']] ) )[[1]] )
    ncvar_put( nc_new, SHP, rotate_lat_lon( list( final_grids[['SHP']] ) )[[1]] )
    ncvar_put( nc_new, RCO, rotate_lat_lon( list( final_grids[['RCO']] ) )[[1]] )

    ncvar_put( nc_new, lon_bnds, t( lon_bnds_data ) )
    ncvar_put( nc_new, lat_bnds, t( lat_bnds_data ) )
    ncvar_put( nc_new, time_bnds, t( time_bnds_data ) )

    add_dimension_attributes( nc_new )

    # attributes for variables
    final_sectors <- c()
    for ( each_var in final_sector_list ) {
        ncatt_put( nc_new, each_var, 'cell_methods', 'time: mean' )
        ncatt_put( nc_new, each_var, 'missing_value', 1e+20, prec = 'float' )
    }

    add_global_attributes( nc_new, title = paste( 'annual emissions of', em ) )

    global_total_emission <- sum( total_month_em$value ) * 0.001
    ncatt_put( nc_new, 0, 'global_total_emission', paste0( round( global_total_emission, 2 ), ' Tg/year' ) )

    # species information
    reporting_info <- data.frame( em_species = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC', 'CO2', 'CH4', 'N2O' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass', 'Mass flux of CO2', 'Mass flux of CH4', 'Mass flux of N2O' ), stringsAsFactors = F )
    info_line <- reporting_info %>% dplyr::filter( em_species == em ) %>% dplyr::select( info ) %>% pull()
    ncatt_put( nc_new, 0, 'reporting_unit', info_line )

    # close nc_new
    nc_close( nc_new)

}
close(pb)



# 4. Checksum -------------------------------------------------------------
year_list <- final_years

# Relevant Inventory emissions
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

# Inventory global total emissions by sector by year
gridding_emissions_fin <- ceds_gridding_mapping %>%
    dplyr::select( CEDS_int_gridding_sector_short, CEDS_final_gridding_sector_short ) %>%
    dplyr::distinct() %>%
    dplyr::right_join( checksum_emissions, by = c( 'CEDS_int_gridding_sector_short' = 'sector' ) ) %>%
    dplyr::group_by( CEDS_final_gridding_sector_short ) %>%
    dplyr::summarise_at( vars( starts_with( 'X' ) ), sum ) %>%
    dplyr::ungroup() %>%
    dplyr::rename( sector = CEDS_final_gridding_sector_short ) %>%
    dplyr::arrange( sector )

# Total emissions by sector by year from grids
checksum_df <- list.files( output_dir, paste0( '_', em, '_anthro_.*_0.1.csv' ), full.names = TRUE ) %>%
    lapply( read.csv ) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by( sector, year ) %>%
    dplyr::summarise( value = sum( value ) ) %>%
    dplyr::ungroup() %>%
    tidyr::spread( year, value ) %>%
    dplyr::rename_all( make.names ) %>%
    dplyr::arrange( sector )

# Take absolute difference and percentage difference
X_year_list <- paste0( 'X', year_list )
diag_diff_df <- cbind( checksum_df['sector'], abs( gridding_emissions_fin[ X_year_list ] - checksum_df[ X_year_list ] ) )
diag_per_df <- cbind( checksum_df['sector'], ( diag_diff_df[ X_year_list ] / gridding_emissions_fin[ X_year_list ] ) * 100 )
diag_per_df[ is.nan.df( diag_per_df ) ] <- NA

# Save checksum files
out_name <- paste0( 'G.', em, '_bulk_emissions_checksum_comparison_diff_01' )
writeData( diag_diff_df, "DIAG_OUT", out_name )
out_name <- paste0( 'G.', em, '_bulk_emissions_checksum_comparison_per_01' )
writeData( diag_per_df, "DIAG_OUT", out_name )



# -----------------------------------------------------------------------------
# 5. Checksum warnings
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


# End ---------------------------------------------------------------------
logStop()
