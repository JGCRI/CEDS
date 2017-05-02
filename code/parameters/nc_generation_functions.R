#------------------------------------------------------------------------------
# Program Name: nc_generation_functions.R
# Author's Name: Leyang Feng
# Date Last Modified: June 30 2016
# Program Purpose: NetCDF generation related functions for gridding routine.  
# Note: 
# TODO: 
# 
# ------------------------------------------------------------------------------

# Special Packages
loadPackage( 'ncdf4' ) 
loadPackage( 'sp' )
loadPackage( 'geosphere' )

# =====================================================================
# NetCDF related functions
# -------------------------------------------------
# generate_final_grids_nc
# Brief: 
# Dependencies: 
# Author: Leyang Feng
# parameters:   
# return:  
# input files: 
# output: 
generate_final_grids_nc <- function( int_grids_list,
                                     output_dir, 
                                     grid_resolution, 
                                     year, 
                                     em, 
                                     sector_name_mapping,
                                     seasonality_mapping ) {
  
  # 0 
  global_grid_area <- grid_area( grid_resolution, all_lon = T )
  flux_factor <- 1000000 / global_grid_area / ( 365 * 24 * 60 * 60 )
  days_in_month <- c( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 )
  
  # 1
  AGR_proc_grid <- int_grids_list$AGR_int_grid
  ENE_proc_grid <- int_grids_list$ELEC_int_grid + int_grids_list$ETRN_int_grid + int_grids_list$FFFI_int_grid + int_grids_list$FLR_int_grid
  IND_proc_grid <- int_grids_list$INDC_int_grid + int_grids_list$INPU_int_grid
  TRA_proc_grid <- int_grids_list$NRTR_int_grid + int_grids_list$ROAD_int_grid
  RCORC_proc_grid <- int_grids_list$RCORC_int_grid
  RCOO_proc_grid <- int_grids_list$RCOO_int_grid
  SLV_proc_grid <- int_grids_list$SLV_int_grid
  WST_proc_grid <- int_grids_list$WST_int_grid
  SHP_proc_grid <- int_grids_list$SHP_int_grid + int_grids_list$TANK_int_grid
  
  # 2
  AGR_fin_grid <- add_seasonality( AGR_proc_grid, em, 'AGR', year, days_in_month, grid_resolution, seasonality_mapping ) 
  ENE_fin_grid <- add_seasonality( ENE_proc_grid, em, 'ENE', year, days_in_month, grid_resolution, seasonality_mapping ) 
  IND_fin_grid <- add_seasonality( IND_proc_grid, em, 'IND', year, days_in_month, grid_resolution, seasonality_mapping ) 
  TRA_fin_grid <- add_seasonality( TRA_proc_grid, em, 'TRA', year, days_in_month, grid_resolution, seasonality_mapping ) 
  SLV_fin_grid <- add_seasonality( SLV_proc_grid, em, 'SLV', year, days_in_month, grid_resolution, seasonality_mapping ) 
  WST_fin_grid <- add_seasonality( WST_proc_grid, em, 'WST', year, days_in_month, grid_resolution, seasonality_mapping ) 
  SHP_fin_grid <- add_seasonality( SHP_proc_grid, em, 'SHP', year, days_in_month, grid_resolution, seasonality_mapping ) 
  RCORC_fin_grid <- add_seasonality( RCORC_proc_grid, em, 'RCORC', year, days_in_month, grid_resolution, seasonality_mapping ) 
  RCOO_fin_grid <- add_seasonality( RCOO_proc_grid, em, 'RCOO', year, days_in_month, grid_resolution, seasonality_mapping ) 
  
  RCO_fin_grid <- RCORC_fin_grid + RCOO_fin_grid
  
  # 3
  AGR_month_em <- sum_monthly_em( AGR_fin_grid, em, 'AGR', year, days_in_month, global_grid_area, seasonality_mapping )
  ENE_month_em <- sum_monthly_em( ENE_fin_grid, em, 'ENE', year, days_in_month, global_grid_area, seasonality_mapping )
  IND_month_em <- sum_monthly_em( IND_fin_grid, em, 'IND', year, days_in_month, global_grid_area, seasonality_mapping )
  TRA_month_em <- sum_monthly_em( TRA_fin_grid, em, 'TRA', year, days_in_month, global_grid_area, seasonality_mapping )
  SLV_month_em <- sum_monthly_em( SLV_fin_grid, em, 'SLV', year, days_in_month, global_grid_area, seasonality_mapping )
  WST_month_em <- sum_monthly_em( WST_fin_grid, em, 'WST', year, days_in_month, global_grid_area, seasonality_mapping )
  SHP_month_em <- sum_monthly_em( SHP_fin_grid, em, 'SHP', year, days_in_month, global_grid_area, seasonality_mapping )
  RCORC_month_em <- sum_monthly_em( RCORC_fin_grid, em, 'RCORC', year, days_in_month, global_grid_area, seasonality_mapping )
  RCOO_month_em <- sum_monthly_em( RCOO_fin_grid, em, 'RCOO', year, days_in_month, global_grid_area, seasonality_mapping )
  
  RCO_month_em <- data.frame( em = em, sector = 'RCO', year = year, month = 1 : 12, units = 'kt', 
                              value = RCORC_month_em$value + RCOO_month_em$value, stringsAsFactors = F )
  total_month_em <- rbind( AGR_month_em, ENE_month_em, IND_month_em, TRA_month_em, 
                           SLV_month_em, WST_month_em, SHP_month_em, RCORC_month_em, RCOO_month_em )
  
  # NetCDF generation starts here
  fin_sector_list <- c( 'AGR', 'ENE', 'IND', 'TRA', 'SLV', 'WST', 'SHP', 'RCO' )
  
  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
  #base_days <- as.numeric( strptime( paste0( year, '0101' ), format = '%Y%m%d', tz = 'UTC' ) - strptime( "17500101", format = "%Y%m%d", tz = 'UTC' ) )
  base_days <- ( year - 1750 ) * 365 
  time <- floor( c( 15.5, 45, 74.5, 105, 135.5, 166, 196.5, 227.5, 258, 288.5, 319, 349.5 ) )
  time <- time + base_days 
  londim <- ncdim_def( "lon", "degrees_east", as.double( lons ), longname = 'longitude' )
  latdim <- ncdim_def( "lat", "degrees_north", as.double( lats ), longname = 'latitude' )
  timedim <- ncdim_def( "time", paste0( "days since 1750-01-01 0:0:0" ), as.double( time ),  
                        calendar = '365_day', longname = 'time' )
  dim_list <- list( londim, latdim, timedim )
  lon_bnds_data <- cbind( seq( -180, ( 180 - grid_resolution ), grid_resolution ), 
                          seq( ( -180 + grid_resolution ), 180, grid_resolution ) )
  lat_bnds_data <- cbind( seq( -90, (90 - grid_resolution) , grid_resolution), 
                          seq( ( -90 + grid_resolution ), 90, grid_resolution ) )
  bnds <- 1 : 2
  bndsdim <- ncdim_def( "bnds", '', as.integer( bnds ), longname = 'bounds', create_dimvar = F )
  time_bnds_data <- cbind( c( 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 ),
                      c( 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 ) )
  time_bnds_data <- time_bnds_data + base_days				  
  
  data_unit <- 'kg m-2 s-1'  
  missing_value <- 1.e20
  
  # define nc variables
  AGR <- ncvar_def( 'AGR', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$CEDS_fin_sector_short == 'AGR', 'CEDS_fin_sector' ], prec = 'float', compression = 5 )
  ENE <- ncvar_def( 'ENE', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$CEDS_fin_sector_short == 'ENE', 'CEDS_fin_sector' ], prec = 'float', compression = 5 )
  IND <- ncvar_def( 'IND', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$CEDS_fin_sector_short == 'IND', 'CEDS_fin_sector' ], prec = 'float', compression = 5 )
  TRA <- ncvar_def( 'TRA', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$CEDS_fin_sector_short == 'TRA', 'CEDS_fin_sector' ], prec = 'float', compression = 5 )
  SLV <- ncvar_def( 'SLV', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$CEDS_fin_sector_short == 'SLV', 'CEDS_fin_sector' ], prec = 'float', compression = 5 )
  WST <- ncvar_def( 'WST', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$CEDS_fin_sector_short == 'WST', 'CEDS_fin_sector' ], prec = 'float', compression = 5 )
  SHP <- ncvar_def( 'SHP', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$CEDS_fin_sector_short == 'SHP', 'CEDS_fin_sector' ], prec = 'float', compression = 5 )
  RCO <- ncvar_def( 'RCO', data_unit, dim_list, missval = missing_value, longname = sector_name_mapping[ sector_name_mapping$CEDS_fin_sector_short == 'RCO', 'CEDS_fin_sector' ], prec = 'float', compression = 5 )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( bndsdim, timedim ), prec = 'double' )
  
  # generate nc file name
  # object version stamp is from global environment
  nc_file_name <- paste0( 'CEDS_', em, '_anthro_', year, '_', grid_resolution, '_', version_stamp, '.nc' ) 
  
  # generate the var_list 
  variable_list <- list( AGR, ENE, IND, TRA, SLV, WST, SHP, RCO, lat_bnds, lon_bnds, time_bnds )

  # create new nc file
  nc_new <- nc_create( paste0( output_dir, nc_file_name ), variable_list, force_v4 = T )
  
  # put nc variables into the nc file
  for ( i in seq_along( time ) ) {
  ncvar_put( nc_new, AGR, t( flip_a_matrix( AGR_fin_grid[ , , i ] ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  ncvar_put( nc_new, ENE, t( flip_a_matrix( ENE_fin_grid[ , , i ] ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  ncvar_put( nc_new, IND, t( flip_a_matrix( IND_fin_grid[ , , i ] ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  ncvar_put( nc_new, TRA, t( flip_a_matrix( TRA_fin_grid[ , , i ] ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  ncvar_put( nc_new, SLV, t( flip_a_matrix( SLV_fin_grid[ , , i ] ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  ncvar_put( nc_new, WST, t( flip_a_matrix( WST_fin_grid[ , , i ] ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  ncvar_put( nc_new, SHP, t( flip_a_matrix( SHP_fin_grid[ , , i ] ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  ncvar_put( nc_new, RCO, t( flip_a_matrix( RCO_fin_grid[ , , i ] ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  }
  ncvar_put( nc_new, lon_bnds, t( lon_bnds_data ) )
  ncvar_put( nc_new, lat_bnds, t( lat_bnds_data ) )
  ncvar_put( nc_new, time_bnds, t( time_bnds_data ) )
  
  # nc variable attributes
  # attributes for dimensions
  ncatt_put( nc_new, "lon", "axis", "X" )
  ncatt_put( nc_new, "lon", "standard_name", "longitude" )
  ncatt_put( nc_new, "lon", "bounds", "lon_bnds" )
  ncatt_put( nc_new, "lat", "axis", "Y" )
  ncatt_put( nc_new, "lat", "standard_name", "latitude" )
  ncatt_put( nc_new, "lat", "bounds", "lat_bnds" )
  ncatt_put( nc_new, "time", "standard_name", "time" )
  ncatt_put( nc_new, "time", "axis", "T" )
  ncatt_put( nc_new, "time", "bounds", "time_bnds" )
  # attributes for variables
  for ( each_var in fin_sector_list ) {
    ncatt_put( nc_new, each_var, 'cell_methods', 'time: mean' )
    ncatt_put( nc_new, each_var, 'missing_value', 1e+20, prec = 'float' )
  }
  # nc global attributes
  #ncatt_put( nc_new, 0, 'IMPORTANT', 'FOR TEST ONLY, DO NOT USE' )
  ncatt_put( nc_new, 0, 'title', paste0('Annual Emissions of ', em ) )
  ncatt_put( nc_new, 0, 'institution_id', 'PNNL-JGCRI' )
  ncatt_put( nc_new, 0, 'institution', 'Pacific Northwest National Laboratory - JGCRI' )
  ncatt_put( nc_new, 0, 'activity_id', 'input4MIPs' )
  ncatt_put( nc_new, 0, 'Conventions', 'CF-1.6' )
  ncatt_put( nc_new, 0, 'creation_date', as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%Y-%m-%dT%H:%M:%SZ' ) ) )
  ncatt_put( nc_new, 0, 'data_structure', 'grid' )
  ncatt_put( nc_new, 0, 'frequency', 'mon' )
  ncatt_put( nc_new, 0, 'realm', 'atmos' )
  ncatt_put( nc_new, 0, 'source', paste0( 'CEDS ', 
                                          as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%m-%d-%Y' ) ),
                                          ': Community Emissions Data System (CEDS) for Historical Emissions' ) )
  ncatt_put( nc_new, 0, 'source_id', paste0( 'CEDS-', as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%m-%d-%Y' ) ) ) )
  ncatt_put( nc_new, 0, 'further_info_url', 'http://www.globalchange.umd.edu/ceds/' )
  #ncatt_put( nc_new, 0, 'license', 'FOR TESTING AND REVIEW ONLY' )
  ncatt_put( nc_new, 0, 'history', 
             paste0( as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%d-%m-%Y %H:%M:%S %p %Z' ) ),
                     '; College Park, MD, USA') )
  #ncatt_put( nc_new, 0, 'comment', 'Test Dataset for CMIP6' )
  ncatt_put( nc_new, 0, 'data_usage_tips', 'Note that these are monthly average fluxes of all oxidized fossil carbon. CO2 from solid and liquid biofuel combustion is not included.' )
  ncatt_put( nc_new, 0, 'host', 'TBD' )
  ncatt_put( nc_new, 0, 'contact', 'ssmith@pnnl.gov' )
  ncatt_put( nc_new, 0, 'references', 'http://www.geosci-model-dev.net/special_issue590.html' )
  ncatt_put( nc_new, 0, 'dataset_category', 'emissions' )
  ncatt_put( nc_new, 0, 'dataset_version_number', '1.0.0' )
  ncatt_put( nc_new, 0, 'grid_label', 'gr' )
  ncatt_put( nc_new, 0, 'grid_resolution', '50 km' )
  ncatt_put( nc_new, 0, 'grid', '0.5x0.5 degree latitude x longitude' )
  ncatt_put( nc_new, 0, 'mip_era', 'CMIP6' )
  ncatt_put( nc_new, 0, 'target_mip', 'CMIP' )
  
  global_total_emission <- sum( total_month_em$value ) * 0.001
  ncatt_put( nc_new, 0, 'global_total_emission', paste0( round( global_total_emission, 2 ), ' Tg/year' ) )
  
  # species information 
  species_info <- data.frame( species = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC', 'CO2' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass', 'Mass flux of CO2' ), stringsAsFactors = F )
  info_line <- species_info[ species_info$species == em, ]$info 
  ncatt_put( nc_new, 0, 'reporting_unit', info_line )
  
  # close nc_new
  nc_close( nc_new)
  
  # additional section: write a summary and check text
  summary_name <- paste0( output_dir, 'CEDS_', em, '_anthro_', year, '_', grid_resolution, '_', version_stamp, '.csv' )
  write.csv( total_month_em, file = summary_name, row.names = F )
}

# -------------------------------------------------
# annual2chunk
# Brief: generate a fliped matrix by a given matrix
# Dependencies: 
# Author: Leyang Feng
# parameters: output_format - ALL: output both singleVar format and multiVar format; singleVar: output only singleVar format; multiVar: output only multiVar format   
# return: 
# input files: 
# output: 
annual2chunk <- function( em, grid_resolution, gridtype = NULL, chunk_start_years, chunk_end_years, chunk_count_index, input_dir, output_dir, VOC_chunk, VOC_info = NULL, CEDS_version = 'YYYY-MM-DD', output_format = NULL ) {
  
  # -------------------------------------------------
  # 1. define 3 functions nested in annual2chunk()
    # (1) -----
    # createMultiVarnc
    # Brief: INTERNAL FUNCTION USED IN annual2chunk()
    #        ALL VARIABLES ARE INHERITED FROM UPPER LAYER FUNCTION
    #        SHOULD NOT BE CALLED DIRECTLY
    #        create multi-variable nc file   
createMultiVarnc <- function( ) {
   
  # the new nc generation routine begins here 
  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
  time <- time_array 
  londim <- ncdim_def( "lon", "degrees_east", as.double( lons ), longname = 'longitude' )
  latdim <- ncdim_def( "lat", "degrees_north", as.double( lats ), longname = 'latitude' )
  timedim <- ncdim_def( "time", paste0( "days since 1750-01-01 0:0:0" ), as.double( time ),  
                        calendar = '365_day', longname = 'time' )
  dim_list <- list( londim, latdim, timedim )
  lon_bnds_data <- cbind( seq( -180, ( 180 - grid_resolution ), grid_resolution ), 
                          seq( ( -180 + grid_resolution ), 180, grid_resolution ) )
  lat_bnds_data <- cbind( seq( -90, (90 - grid_resolution) , grid_resolution), 
                          seq( ( -90 + grid_resolution ), 90, grid_resolution ) )
  bnds <- 1 : 2
  bndsdim <- ncdim_def( "bnds", '', as.integer( bnds ), longname = 'bounds', create_dimvar = F )
  time_bnds_data <- time_bnds_array
  
  # define unit and missing value 
  data_unit <- 'kg m-2 s-1'  
  missing_value <- 1.e20
  var_name_list <- c( 'AGR', 'ENE', 'IND', 'TRA', 'RCO', 'SLV', 'WST', 'SHP' )
  
  # define nc variables
  AGR <- ncvar_def( 'AGR', 'kg m-2 s-1', dim_list, missval = missing_value, longname= 'Agriculture' , prec = 'float', compression = 5 )
  ENE <- ncvar_def( 'ENE', 'kg m-2 s-1', dim_list, missval = missing_value, longname= 'Energy Sector' , prec = 'float', compression = 5 )
  IND <- ncvar_def( 'IND', 'kg m-2 s-1', dim_list, missval = missing_value, longname= 'Industrial Sector' , prec = 'float', compression = 5 )
  TRA <- ncvar_def( 'TRA', 'kg m-2 s-1', dim_list, missval = missing_value, longname= 'Transportation Sector' , prec = 'float', compression = 5 )
  RCO <- ncvar_def( 'RCO', 'kg m-2 s-1', dim_list, missval = missing_value, longname= 'Residential, Commercial, Other' , prec = 'float', compression = 5 )
  SLV <- ncvar_def( 'SLV', 'kg m-2 s-1', dim_list, missval = missing_value, longname= 'Solvents production and application' , prec = 'float', compression = 5 )
  WST <- ncvar_def( 'WST', 'kg m-2 s-1', dim_list, missval = missing_value, longname= 'Waste' , prec = 'float', compression = 5 )
  SHP <- ncvar_def( 'SHP', 'kg m-2 s-1', dim_list, missval = missing_value, longname= 'International Shipping' , prec = 'float', compression = 5 )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( bndsdim, timedim ), prec = 'double' )
  
  # generate nc file name
  ver_date <- paste0( paste0( 'v', CEDS_version ) )
  nc_file_name <- paste0( output_dir, em, '-em-', gsub( '_', '-', gridtype ), 
                          '_input4MIPs_emissions_CMIP_CEDS-', ver_date,
                          '_gr', '_',  
                          chunk_start_years[ chunk_count_index ], '01', '-', 
                          chunk_end_years[ chunk_count_index ], '12','.nc' ) 
  if ( gridtype == 'SOLID_BIOFUEL_anthro' ) {
    nc_file_name <- paste0( output_dir, em, '-em-', gsub( '_', '-', gridtype ), 
                            '_input4MIPs_emissions_CMIP_CEDS-', ver_date, '-supplemental-data',
                            '_gr', '_',  
                            chunk_start_years[ chunk_count_index ], '01', '-', 
                            chunk_end_years[ chunk_count_index ], '12','.nc' ) 
    }                        
  
  
  
  if ( VOC_chunk == T ) { 
    nc_file_name <- paste0( output_dir, gsub( '_', '-', gridtype ), '-em-', 'speciated-VOC', 
                              '_input4MIPs_emissions_CMIP_CEDS-', ver_date, '-supplemental-data',
                              '_gr', '_',  
                              chunk_start_years[ chunk_count_index ], '01', '-', 
                              chunk_end_years[ chunk_count_index ], '12','.nc' ) 
  }  
  
  # generate the var_list 
  variable_list <- list( AGR, ENE, IND, TRA, RCO, SLV, WST, SHP, lat_bnds, lon_bnds, time_bnds )
  
  # create new nc file
  nc_new <- nc_create( nc_file_name, variable_list, force_v4 = T )
  
  # put nc variables into the nc file
  for ( i in seq_along( time ) ) {
    ncvar_put( nc_new, AGR, AGR_array[ , , i ] , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
    ncvar_put( nc_new, ENE, ENE_array[ , , i ] , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
    ncvar_put( nc_new, IND, IND_array[ , , i ] , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
    ncvar_put( nc_new, TRA, TRA_array[ , , i ] , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
    ncvar_put( nc_new, RCO, RCO_array[ , , i ] , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
    ncvar_put( nc_new, SLV, SLV_array[ , , i ] , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
    ncvar_put( nc_new, WST, WST_array[ , , i ] , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
    ncvar_put( nc_new, SHP, SHP_array[ , , i ] , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  }
  ncvar_put( nc_new, lon_bnds, t( lon_bnds_data ) )
  ncvar_put( nc_new, lat_bnds, t( lat_bnds_data ) )
  ncvar_put( nc_new, time_bnds, time_bnds_data )
  
  # nc variable attributes
  # attributes for dimensions
  ncatt_put( nc_new, "lon", "axis", "X" )
  ncatt_put( nc_new, "lon", "standard_name", "longitude" )
  ncatt_put( nc_new, "lon", "bounds", "lon_bnds" )
  ncatt_put( nc_new, "lat", "axis", "Y" )
  ncatt_put( nc_new, "lat", "standard_name", "latitude" )
  ncatt_put( nc_new, "lat", "bounds", "lat_bnds" )
  ncatt_put( nc_new, "time", "standard_name", "time" )
  ncatt_put( nc_new, "time", "axis", "T" )
  ncatt_put( nc_new, "time", "bounds", "time_bnds" )
  # attributes for variables
  for ( each_var in var_name_list ) {
    ncatt_put( nc_new, each_var, 'cell_methods', 'time: mean' )
    ncatt_put( nc_new, each_var, 'missing_value', 1e+20, prec = 'float' )
  }
  # nc global attributes
  #ncatt_put( nc_new, 0, 'IMPORTANT', 'FOR TEST ONLY, DO NOT USE' )
  ncatt_put( nc_new, 0, 'institution_id', 'PNNL-JGCRI' )
  ncatt_put( nc_new, 0, 'institution', 'Pacific Northwest National Laboratory - JGCRI' )
  ncatt_put( nc_new, 0, 'activity_id', 'input4MIPs' )
  ncatt_put( nc_new, 0, 'Conventions', 'CF-1.6' )
  ncatt_put( nc_new, 0, 'creation_date', as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%Y-%m-%dT%H:%M:%SZ' ) ) )
  ncatt_put( nc_new, 0, 'data_structure', 'grid' )
  ncatt_put( nc_new, 0, 'frequency', 'mon' )
  ncatt_put( nc_new, 0, 'realm', 'atmos' )
  ncatt_put( nc_new, 0, 'source', paste0( 'CEDS ', 
                                          as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%m-%d-%Y' ) ),
                                          ': Community Emissions Data System (CEDS) for Historical Emissions' ) )
  ncatt_put( nc_new, 0, 'source_id', paste0( 'CEDS-', as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%m-%d-%Y' ) ) ) )
  ncatt_put( nc_new, 0, 'further_info_url', 'http://www.globalchange.umd.edu/ceds/' )
  #ncatt_put( nc_new, 0, 'license', 'FOR TESTING AND REVIEW ONLY' )
  ncatt_put( nc_new, 0, 'history', 
             paste0( as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%d-%m-%Y %H:%M:%S %p %Z' ) ),
                     '; College Park, MD, USA') )
  #ncatt_put( nc_new, 0, 'comment', 'Test Dataset for CMIP6' )
  ncatt_put( nc_new, 0, 'data_usage_tips', 'Note that these are monthly average fluxes.' )
  ncatt_put( nc_new, 0, 'contact', 'ssmith@pnnl.gov' )
  ncatt_put( nc_new, 0, 'references', 'http://www.geosci-model-dev.net/special_issue590.html' )
  ncatt_put( nc_new, 0, 'dataset_category', 'emissions' )
  ncatt_put( nc_new, 0, 'dataset_version_number', CEDS_version )
  ncatt_put( nc_new, 0, 'grid_label', 'gr' )
  ncatt_put( nc_new, 0, 'grid_resolution', '50 km' )
  ncatt_put( nc_new, 0, 'grid', '0.5x0.5 degree latitude x longitude' )
  ncatt_put( nc_new, 0, 'mip_era', 'CMIP6' )
  ncatt_put( nc_new, 0, 'target_mip', 'CMIP' )
  ncatt_put( nc_new, 0, 'external_variables', 'gridcell_area' )
  ncatt_put( nc_new, 0, 'table_id', 'input4MIPs' )
  # write first/last year global total emission into metadata
  if ( first_year != last_year ) {
    ncatt_put( nc_new, 0, paste0( 'global_total_emission_', first_year ), first_year_global_total_emission_value )
    ncatt_put( nc_new, 0, paste0( 'global_total_emission_', last_year ), last_year_global_total_emission_value )
  } else {
    ncatt_put( nc_new, 0, paste0( 'global_total_emission_', first_year ), first_year_global_total_emission_value )
  }
  
  # below attributes may change depending on gridtype 
  if ( gridtype == 'anthro' ) {
    ncatt_put( nc_new, 0, 'title', paste0( 'Annual Emissions of ', em ) )
    ncatt_put( nc_new, 0, 'product', 'primary-emissions-data' )
    ncatt_put( nc_new, 0, 'variable_id', paste0( em, '-em-anthro') )
    species_info <- data.frame( species = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass'), stringsAsFactors = F )
    info_line <- species_info[ species_info$species == em, ]$info 
    ncatt_put( nc_new, 0, 'reporting_unit', info_line )
  }
  if ( gridtype == 'SOLID_BIOFUEL_anthro' ) {
    ncatt_put( nc_new, 0, 'title', paste0( 'Annual Solid Biofuel Emissions of ', em ) )
    species_info <- data.frame( species = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass'), stringsAsFactors = F )
    info_line <- species_info[ species_info$species == em, ]$info 
    ncatt_put( nc_new, 0, 'reporting_unit', info_line )
    ncatt_put( nc_new, 0, 'product', 'supplementary-emissions-data' )
    ncatt_put( nc_new, 0, 'variable_id', paste0( em, '-em-solid-biofuel') )
    ncatt_put( nc_new, 0, 'note', 'These are a subset of anthro emissions and SHOULD NOT BE ADDED to the anthro emissions data. These are supplemental data only.' )
  }
  if ( VOC_chunk == T ) {
    VOC_name <- VOC_info[ VOC_info$VOC_id == VOC_id, 'VOC_name' ]
    ncatt_put( nc_new, 0, 'title', paste0( 'Annual Emissions of ', VOC_id, ' - ', VOC_name ) ) 
    molecular_weight <- VOC_info[ VOC_info$VOC_id == VOC_id, 'molecular.weight' ]
    ncatt_put( nc_new, 0, 'product', 'supplementary-emissions-data' )
    ncatt_put( nc_new, 0, 'variable_id', paste0( VOC_name, '-em-speciated-VOCs') )
    ncatt_put( nc_new, 0, 'molecular_weight', molecular_weight, prec = 'float' )
    ncatt_put( nc_new, 0, 'molecular_weight_unit', 'g mole-1' )
  }
  
  # close nc_new
  nc_close( nc_new )
  
  return( nc_file_name )
}

    # (2) -----
    # createSingleVarnc
    # Brief: INTERNAL FUNCTION USED IN annual2chunk()
    #        ALL VARIABLES ARE INHERITED FROM UPPER LAYER FUNCTION
    #        SHOULD NOT BE CALLED DIRECTLY
    #        create single variable nc file   
createSingleVarnc <- function( ) {

# -----------------
# 1. create flat data block 
# the order of sectors is as below: 
#  0: Agriculture; 
#  1: Energy Sector; 
#  2: Industrial Sector; 
#  3: Transportation Sector; 
#  4: Residential, Commercial, Other; 
#  5: Solvents production and application; 
#  6: Waste; 
#  7: International Shipping

  flat_dim <- c( data_dim[1], data_dim[2], 8 , data_dim[3] )
  flat_data <- array( dim = flat_dim )
  for ( i in 1 : flat_dim[4] ) {
    flat_data[ , , 1 , i ] <- AGR_array[ , , i ]
    flat_data[ , , 2 , i ] <- ENE_array[ , , i ]
    flat_data[ , , 3 , i ] <- IND_array[ , , i ]
    flat_data[ , , 4 , i ] <- TRA_array[ , , i ]
    flat_data[ , , 5 , i ] <- RCO_array[ , , i ]
    flat_data[ , , 6 , i ] <- SLV_array[ , , i ]
    flat_data[ , , 7 , i ] <- WST_array[ , , i ]
    flat_data[ , , 8 , i ] <- SHP_array[ , , i ]
  }

# -----------------
# 2. generate nc file

  # the new nc generation routine begins here 
  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
  time <- time_array 
  sectors <- 0 : 7 
  londim <- ncdim_def( "lon", "degrees_east", as.double( lons ), longname = 'longitude' )
  latdim <- ncdim_def( "lat", "degrees_north", as.double( lats ), longname = 'latitude' )
  timedim <- ncdim_def( "time", paste0( "days since 1750-01-01 0:0:0" ), as.double( time ),  
                        calendar = '365_day', longname = 'time', unlim = T )
  sectordim <- ncdim_def( "sector", "", sectors, longname = 'sector' )
  dim_list <- list( londim, latdim, sectordim, timedim )
  lon_bnds_data <- cbind( seq( -180, ( 180 - grid_resolution ), grid_resolution ), 
                          seq( ( -180 + grid_resolution ), 180, grid_resolution ) )
  lat_bnds_data <- cbind( seq( -90, (90 - grid_resolution) , grid_resolution), 
                          seq( ( -90 + grid_resolution ), 90, grid_resolution ) )
  bnds <- 1 : 2
  bndsdim <- ncdim_def( 'bound', '', as.integer( bnds ), longname = 'bound', create_dimvar = F )
  time_bnds_data <- time_bnds_array
  sector_bnds_data <- cbind( seq( -0.5, 6.5, 1 ),
                             seq( 0.5, 7.5, 1 ) )
  
  # generate nc file name
  ver_date <- paste0( paste0( 'v', CEDS_version ) )
  ver_date_parts <- unlist( strsplit( CEDS_version, split = '-') )
  source_date <- paste0( ver_date_parts[2], '-',ver_date_parts[3], '-', ver_date_parts[1] ) 
  source_value <- paste0( 'CEDS ', source_date, ': Community Emissions Data System (CEDS) for Historical Emissions' )
  source_id_value <- paste0( 'CEDS-', source_date )
  nc_file_name <- paste0( output_dir, em, '-em-', gsub( '_', '-', gridtype ),  
                          '_input4MIPs_emissions_CMIP_CEDS-', ver_date,
                          '_gr', '_',  
                          chunk_start_years[ chunk_count_index ], '01', '-', 
                          chunk_end_years[ chunk_count_index ], '12','.nc' ) 
  
  # generate flat_var variable name 
  flat_var_name <- paste0( em, '_em_', gridtype )
  flat_var_longname <- flat_var_name
  variable_id_value <- flat_var_name
  if ( gridtype == 'SOLID_BIOFUEL_anthro' ) {
    nc_file_name <- paste0( output_dir, em, '-em-', gsub( '_', '-', gridtype ), 
                            '_input4MIPs_emissions_CMIP_CEDS-', ver_date, '-supplemental-data',
                            '_gr', '_',  
                            chunk_start_years[ chunk_count_index ], '01', '-', 
                            chunk_end_years[ chunk_count_index ], '12','.nc' ) 
	flat_var_name <- paste0( em, '_em_', gridtype )
    flat_var_longname <- flat_var_name
	variable_id_value <- flat_var_name
	}                        
  
  if ( VOC_chunk == T ) { 
    nc_file_name <- paste0( output_dir, gsub( '_', '-', gridtype ), '-em-', 'speciated-VOC', 
                              '_input4MIPs_emissions_CMIP_CEDS-', ver_date, '-supplemental-data',
                              '_gr', '_',  
                              chunk_start_years[ chunk_count_index ], '01', '-', 
                              chunk_end_years[ chunk_count_index ], '12','.nc' ) 
	flat_var_name <- paste0( gridtype, '_em_', 'speciated_VOC' )
    flat_var_longname <- flat_var_name  
	variable_id_value <- flat_var_name
  }  
  
  # define unit and missing value 
  data_unit <- 'kg m-2 s-1'  
  missing_value <- 1.e20
  dataset_version_number_value <- paste0( CEDS_version ) 
  
  # define nc variables  
  flat_var <- ncvar_def( flat_var_name, data_unit, dim_list, missval = missing_value, compression = 5 )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( bndsdim, timedim ), prec = 'double' )
  sector_bnds <- ncvar_def( 'sector_bnds', '', list( bndsdim, sectordim ), prec = 'double' )
  
  # generate the var_list
  variable_list <- list( flat_var, lat_bnds, lon_bnds, time_bnds, sector_bnds ) 
  
  # create new nc file
  nc_new <- nc_create( nc_file_name, variable_list, force_v4 = T )
  
  # put nc variables into the nc file
  for ( i in seq_along( time ) ) {
    ncvar_put( nc_new, flat_var, flat_data[ , , , i ], start = c( 1, 1, 1, i ), count = c( -1, -1, -1, 1 ) )  
  }
  ncvar_put( nc_new, lon_bnds, t( lon_bnds_data ) )
  ncvar_put( nc_new, lat_bnds, t( lat_bnds_data ) )
  ncvar_put( nc_new, time_bnds, time_bnds_data )
  ncvar_put( nc_new, sector_bnds, t( sector_bnds_data ) )
  
  # nc variable attributes
  # attributes for dimensions
  ncatt_put( nc_new, "lon", "axis", "X" )
  ncatt_put( nc_new, "lon", "standard_name", "longitude" )
  ncatt_put( nc_new, "lon", "bounds", "lon_bnds" )
  ncatt_put( nc_new, "lon", "realtopology", "circular" )
  ncatt_put( nc_new, "lon", "modulo", 360.0, prec = 'double' )
  ncatt_put( nc_new, "lon", "topology", "circular" )
  ncatt_put( nc_new, "lat", "axis", "Y" )
  ncatt_put( nc_new, "lat", "standard_name", "latitude" )
  ncatt_put( nc_new, "lat", "bounds", "lat_bnds" )
  ncatt_put( nc_new, "lat", "realtopology", "linear" ) 
  ncatt_put( nc_new, "time", "standard_name", "time" )
  ncatt_put( nc_new, "time", "axis", "T" )
  ncatt_put( nc_new, "time", "bounds", "time_bnds" )
  ncatt_put( nc_new, "time", "realtopology", "linear" )
  ncatt_put( nc_new, "sector", "ids", "0: Agriculture; 1: Energy; 2: Industrial; 3: Transportation; 4: Residential, Commercial, Other; 5: Solvents production and application; 6: Waste; 7: International Shipping" )
  ncatt_put( nc_new, "sector", "bounds", "sector_bnds" )
  # attributes for variables
  ncatt_put( nc_new, flat_var_name, 'cell_methods', 'time: mean' )
  ncatt_put( nc_new, flat_var_name, 'missing_value', 1e+20, prec = 'float' )
  ncatt_put( nc_new, flat_var_name, 'long_name', flat_var_longname )
  # nc global attributes
  #ncatt_put( nc_new, 0, 'IMPORTANT', 'FOR TEST ONLY, DO NOT USE' )
  ncatt_put( nc_new, 0, 'institution_id', 'PNNL-JGCRI' )
  ncatt_put( nc_new, 0, 'institution', 'Pacific Northwest National Laboratory - JGCRI' )
  ncatt_put( nc_new, 0, 'activity_id', 'input4MIPs' )
  ncatt_put( nc_new, 0, 'Conventions', 'CF-1.6' )
  ncatt_put( nc_new, 0, 'creation_date', as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%Y-%m-%dT%H:%M:%SZ' ) ) )
  ncatt_put( nc_new, 0, 'data_structure', 'grid' )
  ncatt_put( nc_new, 0, 'frequency', 'mon' )
  ncatt_put( nc_new, 0, 'realm', 'atmos' )
  ncatt_put( nc_new, 0, 'source', source_value )
  ncatt_put( nc_new, 0, 'source_id', source_id_value )
  ncatt_put( nc_new, 0, 'further_info_url', 'http://www.globalchange.umd.edu/ceds/' )
  #ncatt_put( nc_new, 0, 'license', 'FOR TESTING AND REVIEW ONLY' )
  ncatt_put( nc_new, 0, 'history', 
             paste0( as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%d-%m-%Y %H:%M:%S %p %Z' ) ),
                     '; College Park, MD, USA') )
  #ncatt_put( nc_new, 0, 'comment', 'Test Dataset for CMIP6' )
  ncatt_put( nc_new, 0, 'data_usage_tips', 'Note that these are monthly average fluxes.' )
  ncatt_put( nc_new, 0, 'contact', 'Steven J. Smith; ssmith@pnnl.gov' )
  ncatt_put( nc_new, 0, 'references', 'Hoesly, R. M., Smith, S. J., Feng, L., Klimont, Z., Janssens-Maenhout, G., Pitkanen, T., Seibert, J. J., Vu, L., Andres, R. J., Bolt, R. M., Bond, T. C., Dawidowski, L., Kholod, N., Kurokawa, J.-I., Li, M., Liu, L., Lu, Z., Moura, M. C. P., O\'Rourke, P. R., and Zhang, Q.: Historical (1750-2014) anthropogenic emissions of reactive gases and aerosols from the Community Emission Data System (CEDS), Geosci. Model Dev. Discuss., doi:10.5194/gmd-2017-43, in review, 2017.' )
  ncatt_put( nc_new, 0, 'dataset_category', 'emissions' )
  ncatt_put( nc_new, 0, 'dataset_version_number', dataset_version_number_value )
  ncatt_put( nc_new, 0, 'grid_label', 'gn' )
  ncatt_put( nc_new, 0, 'nominal_resolution', '50 km' )
  ncatt_put( nc_new, 0, 'grid', '0.5x0.5 degree latitude x longitude' )
  ncatt_put( nc_new, 0, 'mip_era', 'CMIP6' )
  ncatt_put( nc_new, 0, 'target_mip', 'CMIP' )
  ncatt_put( nc_new, 0, 'external_variables', 'gridcell_area' )
  ncatt_put( nc_new, 0, 'table_id', 'input4MIPs' )
  # write first/last year global total emission into metadata
  if ( first_year != last_year ) {
    ncatt_put( nc_new, 0, paste0( 'global_total_emission_', first_year ), first_year_global_total_emission_value )
    ncatt_put( nc_new, 0, paste0( 'global_total_emission_', last_year ), last_year_global_total_emission_value )
  } else {
    ncatt_put( nc_new, 0, paste0( 'global_total_emission_', first_year ), first_year_global_total_emission_value )
  }
  
  # below attributes may change depending on gridtype 
  if ( gridtype == 'anthro' ) {
    ncatt_put( nc_new, 0, 'title', paste0( 'Annual Anthropogenic Emissions of ', em, ' prepared for input4MIPs' ) )
    ncatt_put( nc_new, 0, 'product', 'primary-emissions-data' )
    ncatt_put( nc_new, 0, 'variable_id', variable_id_value )
    species_info <- data.frame( species = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass'), stringsAsFactors = F )
    info_line <- species_info[ species_info$species == em, ]$info 
    ncatt_put( nc_new, 0, 'reporting_unit', info_line )
    ncatt_put( nc_new, 0, 'comment', 'This data supersedes v2016-06-18 and v2016-07-26 data versions. See README file at the project web site.' )
    }
  if ( gridtype == 'SOLID_BIOFUEL_anthro' ) {
    ncatt_put( nc_new, 0, 'title', paste0( 'Annual Solid Biofuel Emissions of ', em ) )
    species_info <- data.frame( species = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass'), stringsAsFactors = F )
    info_line <- species_info[ species_info$species == em, ]$info 
    ncatt_put( nc_new, 0, 'reporting_unit', info_line )
    ncatt_put( nc_new, 0, 'product', 'supplementary-emissions-data' )
    ncatt_put( nc_new, 0, 'variable_id', variable_id_value )
    ncatt_put( nc_new, 0, 'note', 'These are a subset of anthro emissions and SHOULD NOT BE ADDED to the anthro emissions data. These are supplemental data only.' )
  }
  if ( VOC_chunk == T ) {
    VOC_name <- VOC_info[ VOC_info$VOC_id == VOC_id, 'VOC_name' ]
    ncatt_put( nc_new, 0, 'title', paste0( 'Annual Emissions of ', VOC_id, ' - ', VOC_name ) ) 
    molecular_weight <- VOC_info[ VOC_info$VOC_id == VOC_id, 'molecular.weight' ]
    ncatt_put( nc_new, 0, 'product', 'supplementary-emissions-data' )
    ncatt_put( nc_new, 0, 'variable_id', variable_id_value )
    ncatt_put( nc_new, 0, 'molecular_weight', molecular_weight, prec = 'float' )
    ncatt_put( nc_new, 0, 'molecular_weight_unit', 'g mole-1' )
  }
  
  # close nc_new
  nc_close( nc_new )
  
  return( nc_file_name )
}

    # (3) -----
    # createAnthroChecksum
    # Brief: INTERNAL FUNCTION USED IN annual2chunk()
    #        ALL VARIABLES ARE INHERITED FROM UPPER LAYER FUNCTION
    #        SHOULD NOT BE CALLED DIRECTLY
    #        create checksum file for the nc generated   
createAnthroChecksum <- function( nc_file_name ) {
  # additional routine for generating checksum file
  global_grid_area <- grid_area( grid_resolution, all_lon = T )
  flux_factor <- 1000000 / global_grid_area / ( 365 * 24 * 60 * 60 )
  flux_factor <- t( flip_a_matrix( flux_factor ) )
  Days_in_Month <- c( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 )
  
  total_em_list <- c( )
  total_em_list <- c( total_em_list, unlist( lapply( 1 : dim( AGR_array ) [ 3 ], function( i ) { sum( AGR_array[ , , i ] / flux_factor / ( 365 / Days_in_Month[ ifelse( i %% 12 == 0, 12, i %% 12 ) ] ) ) } ) ) )
  total_em_list <- c( total_em_list, unlist( lapply( 1 : dim( ENE_array ) [ 3 ], function( i ) { sum( ENE_array[ , , i ] / flux_factor / ( 365 / Days_in_Month[ ifelse( i %% 12 == 0, 12, i %% 12 ) ] ) ) } ) ) )
  total_em_list <- c( total_em_list, unlist( lapply( 1 : dim( IND_array ) [ 3 ], function( i ) { sum( IND_array[ , , i ] / flux_factor / ( 365 / Days_in_Month[ ifelse( i %% 12 == 0, 12, i %% 12 ) ] ) ) } ) ) )
  total_em_list <- c( total_em_list, unlist( lapply( 1 : dim( TRA_array ) [ 3 ], function( i ) { sum( TRA_array[ , , i ] / flux_factor / ( 365 / Days_in_Month[ ifelse( i %% 12 == 0, 12, i %% 12 ) ] ) ) } ) ) )
  total_em_list <- c( total_em_list, unlist( lapply( 1 : dim( RCO_array ) [ 3 ], function( i ) { sum( RCO_array[ , , i ] / flux_factor / ( 365 / Days_in_Month[ ifelse( i %% 12 == 0, 12, i %% 12 ) ] ) ) } ) ) )
  total_em_list <- c( total_em_list, unlist( lapply( 1 : dim( SLV_array ) [ 3 ], function( i ) { sum( SLV_array[ , , i ] / flux_factor / ( 365 / Days_in_Month[ ifelse( i %% 12 == 0, 12, i %% 12 ) ] ) ) } ) ) )
  total_em_list <- c( total_em_list, unlist( lapply( 1 : dim( WST_array ) [ 3 ], function( i ) { sum( WST_array[ , , i ] / flux_factor / ( 365 / Days_in_Month[ ifelse( i %% 12 == 0, 12, i %% 12 ) ] ) ) } ) ) )
  total_em_list <- c( total_em_list, unlist( lapply( 1 : dim( SHP_array ) [ 3 ], function( i ) { sum( SHP_array[ , , i ] / flux_factor / ( 365 / Days_in_Month[ ifelse( i %% 12 == 0, 12, i %% 12 ) ] ) ) } ) ) )
  
  sector_list <- c( 'AGR', 'ENE', 'IND', 'TRA', 'RCO', 'SLV', 'WST', 'SHP' )
  checksum_sector_list <- unlist( lapply( sector_list, function( sector ) { sectors <- rep( sector, dim( AGR_array )[ 3 ]  ) } ) )
  checksum_month_list <- rep( 1 : 12, ( dim( AGR_array )[ 3 ] / 12 * length( sector_list ) ) )
  checksum_year_list  <- rep( unlist( lapply( chunk_start_years[ chunk_count_index ] : chunk_end_years[ chunk_count_index ], function( year ) { years <- rep( year, 12 ) } ) ), length( sector_list ) )
  checksum_unit_list <- rep( 'kt', length( checksum_sector_list ) ) 
  checksum_em_list <- rep( em, length( checksum_sector_list ) )
  if ( VOC_chunk == T ) { 
  VOC_name <- VOC_info[ VOC_info$VOC_id == VOC_id, 'VOC_name' ]
  checksum_em_list <- rep( paste0( VOC_id, '-', VOC_name ), length( checksum_sector_list ) )
  }
  layout <- data.frame( year = checksum_year_list, em = checksum_em_list, sector = checksum_sector_list, month = checksum_month_list, global_total = total_em_list, units = checksum_unit_list, stringsAsFactors = F )
  
  summary_name <- paste0( substr( nc_file_name, 1, ( nchar( nc_file_name) - 3 ) ), '.csv' )
  write.csv( layout, file = summary_name, row.names = F )
}
  
  # ------------------------------------------------
  # 2. the code for annual2chunk() starts here
  # create filename pattern using grid type 
  filename_patterns <- paste0( '_', em, '_', gridtype, '_', ( chunk_start_years[ chunk_count_index ] : chunk_end_years[ chunk_count_index ] ), '.*nc' )
  if ( VOC_chunk == T ) { 
    VOC_id <- unlist( strsplit( gridtype, split = '-' ) ) [ 1 ]
    filename_patterns <- paste0( gridtype, '_anthro_', ( chunk_start_years[ chunk_count_index ] : chunk_end_years[ chunk_count_index ] ), '.*nc' )
  }
  # generate file list in the input dir 
  nc_file_list <- c( )
  for ( filename_pattern in filename_patterns ) { 
    matched_filename <- list.files( input_dir, pattern = filename_pattern )
    nc_file_list <- c( nc_file_list, matched_filename )
  }
  
  # read in each nc file and extract variables 
  # create ampty arries for storage
  AGR_array <- c( )
  ENE_array <- c( )
  IND_array <- c( )
  TRA_array <- c( )
  RCO_array <- c( )
  SLV_array <- c( )
  WST_array <- c( )
  SHP_array <- c( )
  time_array <- c( )
  time_bnds_array <- c( )
  # go through each nc files, extract the variables and append to storage array 
  for ( nc_file in nc_file_list ) {
    # open the nc file 
    nc_temp <- nc_open( paste0( input_dir, '/', nc_file ) ) 
    # extract the data for variables 
    AGR_temp_data <- ncvar_get( nc_temp, 'AGR' )
    ENE_temp_data <- ncvar_get( nc_temp, 'ENE' )
    IND_temp_data <- ncvar_get( nc_temp, 'IND' )
    TRA_temp_data <- ncvar_get( nc_temp, 'TRA' )
    RCO_temp_data <- ncvar_get( nc_temp, 'RCO' )
    SLV_temp_data <- ncvar_get( nc_temp, 'SLV' )
    WST_temp_data <- ncvar_get( nc_temp, 'WST' )
    SHP_temp_data <- ncvar_get( nc_temp, 'SHP' )
    time_temp_data <- ncvar_get( nc_temp, 'time' )
    time_bnds_temp_data <- ncvar_get( nc_temp, 'time_bnds' )
    # close the nc file 
    nc_close( nc_temp )
    # append the data to storage array 
    AGR_array <- c( AGR_array, AGR_temp_data )
    ENE_array <- c( ENE_array, ENE_temp_data )
    IND_array <- c( IND_array, IND_temp_data )
    TRA_array <- c( TRA_array, TRA_temp_data )
    RCO_array <- c( RCO_array, RCO_temp_data )
    SLV_array <- c( SLV_array, SLV_temp_data )
    WST_array <- c( WST_array, WST_temp_data )
    SHP_array <- c( SHP_array, SHP_temp_data )
    time_array <- c( time_array, time_temp_data )
    time_bnds_array <- c( time_bnds_array, time_bnds_temp_data )
  }
  
  # extract values of global_total_emission attribute from first year and last year 
  # value from first year 
  first_year_nc_file_name <- nc_file_list[ 1 ]
  first_year <- chunk_start_years[ chunk_count_index ] 
  nc_temp <- nc_open( paste0( input_dir, '/', first_year_nc_file_name ) ) 
  first_year_global_total_emission_value <- ncatt_get( nc_temp, 0, 'global_total_emission' )[[ 2 ]]
  nc_close( nc_temp )
  # value from last year 
  last_year_nc_file_name <- nc_file_list[ length( nc_file_list ) ]
  last_year <- chunk_end_years[ chunk_count_index ]
  nc_temp <- nc_open( paste0( input_dir, '/', last_year_nc_file_name ) ) 
  last_year_global_total_emission_value <- ncatt_get( nc_temp, 0, 'global_total_emission' )[[ 2 ]]
  nc_close( nc_temp )
  
  # reshape the array
  # calculate how many years in the duration 
  years_in_current_chunk <- chunk_end_years[ chunk_count_index ] - chunk_start_years[ chunk_count_index ] + 1 
  # define dim for 3-D data 
  data_dim <- c( dim( AGR_temp_data )[ 1 : 2 ], 12 * years_in_current_chunk ) # the dim for AGR_temp_data should always be the same in each nc file 
  # time array has no need to be reshaped -- it's a one dimensional vector 
  time_bnds_dim <- c( dim( time_bnds_temp_data )[ 1 ], 12 * years_in_current_chunk )
  # reshaping
  dim( AGR_array ) <- data_dim 
  dim( ENE_array ) <- data_dim 
  dim( IND_array ) <- data_dim 
  dim( TRA_array ) <- data_dim 
  dim( RCO_array ) <- data_dim 
  dim( SLV_array ) <- data_dim 
  dim( WST_array ) <- data_dim 
  dim( SHP_array ) <- data_dim 
  time_array <- time_array
  dim( time_bnds_array ) <- time_bnds_dim 
  
  if ( output_format == 'singleVar' ) {
    # generate single variable nc file 
    nc_file_name_singleval <- createSingleVarnc( )
    # generate checksum file for single variable nc
    createAnthroChecksum( nc_file_name_singleval )
  }
  
  if ( output_format == 'multiVar' ) {
    # generate multi-variable nc file 
    nc_file_name_multivar <- createMultiVarnc( )
    # generate checksum file for multi-variable nc
    createAnthroChecksum( nc_file_name_multivar )
  }
  
  if ( output_format == 'ALL' ) {
    # generate multi-variable nc file 
    nc_file_name_multivar <- createMultiVarnc( )
    # generate single variable nc file 
    nc_file_name_singleval <- createSingleVarnc( )
  
    # generate checksum file for multi-variable nc
    createAnthroChecksum( nc_file_name_multivar )
    # generate checksum file for single variable nc
    createAnthroChecksum( nc_file_name_singleval )
  }
  
  }
  
# -------------------------------------------------
# generate_annual_total_emissions_grids_nc
# Brief: generate a fliped matrix by a given matrix
# Dependencies: 
# Author: Leyang Feng
# parameters: x - the matrix to be fliped  
# return: a fliped matrix 
# input files: 
# output: 
generate_annual_total_emissions_grids_nc <- function( output_dir, int_grids_list, grid_resolution, year, em ) {
  
  # 0 set up some basics for later use
  global_grid_area <- grid_area( grid_resolution, all_lon = T )
  flux_factor <- 1000000 / global_grid_area / ( 365 * 24 * 60 * 60 )
    
  # 1 adding all sector's grid together so generate total emission grid 
  
  # convert flux back to mass 
  total_grid_mass <- lapply( int_grids_list, '/', flux_factor )
  total_grid_mass <- Reduce( '+', total_grid_mass )
  
  total_sum_in_kt <- sum( total_grid_mass )
  
  total_grid_flux <- total_grid_mass * flux_factor 
  
  # generate the nc file 
  
  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
  londim <- ncdim_def( "lon", "degrees_east", as.double( lons ), longname = 'longitude' )
  latdim <- ncdim_def( "lat", "degrees_north", as.double( lats ), longname = 'latitude' )
  dim_list <- list( londim, latdim )
  lon_bnds_data <- cbind( seq( -180, ( 180 - grid_resolution ), grid_resolution ), 
                          seq( ( -180 + grid_resolution ), 180, grid_resolution ) )
  lat_bnds_data <- cbind( seq( -90, (90 - grid_resolution) , grid_resolution), 
                          seq( ( -90 + grid_resolution ), 90, grid_resolution ) )
  bnds <- 1 : 2
  bndsdim <- ncdim_def( "bnds", '', as.integer( bnds ), longname = 'bounds', create_dimvar = F )

  data_unit <- 'kg m-2 s-1'  
  
  # define nc variables
  missing_value <- 1.e20
  long_name <- 'Global total emissions '
  total_emission <- ncvar_def( 'total_emission', data_unit, dim_list, missval = missing_value, longname = long_name , prec = 'float', compression = 5 )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
  
  # generate nc file name
  nc_file_name <- paste0( output_dir, 'CEDS_', em, '_anthro_', year, '_', 'TOTAL', '_', grid_resolution, '_', version_stamp, '.nc' ) 
  
  # generate the var_list 
  variable_list <- list( total_emission, lat_bnds, lon_bnds ) 

  # create new nc file
  nc_new <- nc_create( nc_file_name, variable_list, force_v4 = T )
  
  # put nc variables into the nc file
  ncvar_put( nc_new, total_emission, t( flip_a_matrix( total_grid_flux ) ) )
  ncvar_put( nc_new, lon_bnds, t( lon_bnds_data ) )
  ncvar_put( nc_new, lat_bnds, t( lat_bnds_data ) )

  
  # nc variable attributes
  # attributes for dimensions
  ncatt_put( nc_new, "lon", "axis", "X" )
  ncatt_put( nc_new, "lon", "standard_name", "longitude" )
  ncatt_put( nc_new, "lon", "bounds", "lon_bnds" )
  ncatt_put( nc_new, "lat", "axis", "Y" )
  ncatt_put( nc_new, "lat", "standard_name", "latitude" )
  ncatt_put( nc_new, "lat", "bounds", "lat_bnds" )
  # attributes for variables
  ncatt_put( nc_new, total_emission, 'cell_methods', 'time: mean' )
  ncatt_put( nc_new, total_emission, 'missing_value', 1e+20, prec = 'float' )
  # nc global attributes
  #ncatt_put( nc_new, 0, 'IMPORTANT', 'FOR TEST ONLY, DO NOT USE' )
  ncatt_put( nc_new, 0, 'title', paste0('Annual Emissions of ', em ) )
  ncatt_put( nc_new, 0, 'institution_id', 'PNNL-JGCRI' )
  ncatt_put( nc_new, 0, 'institution', 'Pacific Northwest National Laboratory - JGCRI' )
  ncatt_put( nc_new, 0, 'activity_id', 'input4MIPs' )
  ncatt_put( nc_new, 0, 'Conventions', 'CF-1.6' )
  ncatt_put( nc_new, 0, 'creation_date', as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%Y-%m-%dT%H:%M:%SZ' ) ) )
  ncatt_put( nc_new, 0, 'data_structure', 'grid' )
  ncatt_put( nc_new, 0, 'frequency', 'mon' )
  ncatt_put( nc_new, 0, 'realm', 'atmos' )
  ncatt_put( nc_new, 0, 'source', paste0( 'CEDS ', 
                                          as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%m-%d-%Y' ) ),
                                          ': Community Emissions Data System (CEDS) for Historical Emissions' ) )
  ncatt_put( nc_new, 0, 'source_id', paste0( 'CEDS-', as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%m-%d-%Y' ) ) ) )
  ncatt_put( nc_new, 0, 'further_info_url', 'http://www.globalchange.umd.edu/ceds/' )
  #ncatt_put( nc_new, 0, 'license', 'FOR TESTING AND REVIEW ONLY' )
  ncatt_put( nc_new, 0, 'history', 
             paste0( as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%d-%m-%Y %H:%M:%S %p %Z' ) ),
                     '; College Park, MD, USA') )
  #ncatt_put( nc_new, 0, 'comment', 'Test Dataset for CMIP6' )
  ncatt_put( nc_new, 0, 'data_usage_tips', 'Note that these are monthly average fluxes of all oxidized fossil carbon. CO2 from solid and liquid biofuel combustion is not included.' )
  ncatt_put( nc_new, 0, 'host', 'TBD' )
  ncatt_put( nc_new, 0, 'contact', 'ssmith@pnnl.gov' )
  ncatt_put( nc_new, 0, 'references', 'http://www.geosci-model-dev.net/special_issue590.html' )
  ncatt_put( nc_new, 0, 'dataset_category', 'emissions' )
  ncatt_put( nc_new, 0, 'dataset_version_number', '1.0.0' )
  ncatt_put( nc_new, 0, 'grid_label', 'gr' )
  ncatt_put( nc_new, 0, 'grid_resolution', '50 km' )
  ncatt_put( nc_new, 0, 'grid', '0.5x0.5 degree latitude x longitude' )
  ncatt_put( nc_new, 0, 'mip_era', 'CMIP6' )
  ncatt_put( nc_new, 0, 'target_mip', 'CMIP' )

  global_total_emission <- total_sum_in_kt * 0.001
  ncatt_put( nc_new, 0, 'global_total_emission', paste0( round( global_total_emission, 2 ), ' Tg/year' ) )
  # species information 
  species_info <- data.frame( species = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC', 'CO2', 'CH4' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass', 'Mass flux of CO2', 'Mass flux of CH4' ), stringsAsFactors = F )
  info_line <- species_info[ species_info$species == em, ]$info 
  ncatt_put( nc_new, 0, 'reporting_unit', info_line )
  
    
  # close nc_new
  nc_close( nc_new)
 
  # additional section: write a summary and check text

  summary_table <- data.frame( year = year, em = em,  
                               global_total = total_sum_in_kt,
                               unit = 'kt', stringsAsFactors = F )

  summary_name <- paste0( output_dir, 'CEDS_', em, '_anthro_', year, '_', 'TOTAL', '_', grid_resolution, '_', version_stamp, '.csv' )
  write.csv( summary_table, file = summary_name, row.names = F )
}
