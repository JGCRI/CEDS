# ------------------------------------------------------------------------------
# Program Name: nc_generation_functions.R
# Authors: Leyang Feng, Caleb Braun
# Date Last Modified: March 25 2019
# Program Purpose: NetCDF generation related functions for gridding routine.
# ------------------------------------------------------------------------------

# Special Packages
loadPackage( 'ncdf4' )
loadPackage( 'sp' )
loadPackage( 'geosphere' )

# ====================== Annual grids generation functions =====================

# generate_final_grids_nc
# Brief: generate annual nc grids for bulk emissions
# Dependencies:
# Author: Leyang Feng
# parameters: int_grids_list - the list contains intermediate grids
#             output_dir - the output dir
#             grid_resolution - the gridding resolution
#             year - the current gridding year
#             em - the gridding emission species
#             sector_name_mapping - the mapping contains final sectors short names and long names
#             seasonality_mapping  - the seasonality mapping file
# return: null
# input files: null
# output: CEDS_[em]_anthro_[year]_0.5.nc
#         CEDS_[em]_anthro_[year]_0.5.csv
generate_final_grids_nc <- function( int_grids_list,
                                     output_dir,
                                     grid_resolution,
                                     year,
                                     em,
                                     sector_name_mapping,
                                     seasonality_mapping ) {

  # 0 set up basics
  global_grid_area <- grid_area( grid_resolution, all_lon = T )
  days_in_month <- c( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 )

  # 1 extract int grids in the list
  AGR_proc_grid <- int_grids_list$AGR_int_grid
  ENE_proc_grid <- int_grids_list$ELEC_int_grid + int_grids_list$ETRN_int_grid + int_grids_list$FFFI_int_grid + int_grids_list$FLR_int_grid
  IND_proc_grid <- int_grids_list$INDC_int_grid + int_grids_list$INPU_int_grid
  TRA_proc_grid <- int_grids_list$NRTR_int_grid + int_grids_list$ROAD_int_grid
  RCORC_proc_grid <- int_grids_list$RCORC_int_grid
  RCOO_proc_grid <- int_grids_list$RCOO_int_grid
  SLV_proc_grid <- int_grids_list$SLV_int_grid
  WST_proc_grid <- int_grids_list$WST_int_grid
  SHP_proc_grid <- int_grids_list$SHP_int_grid + int_grids_list$TANK_int_grid

  # 2 add seasonality for each final grids
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

  # 3 calculate checksum values
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
                           SLV_month_em, WST_month_em, SHP_month_em, RCO_month_em )

  # NetCDF generation starts here
  fin_sector_list <- c( 'AGR', 'ENE', 'IND', 'TRA', 'SLV', 'WST', 'SHP', 'RCO' )

  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
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
  nc_file_name <- paste0( 'CEDS_', em, '_anthro_', year, '_', grid_resolution, '.nc' )

  # generate the var_list
  variable_list <- list( AGR, ENE, IND, TRA, SLV, WST, SHP, RCO, lat_bnds, lon_bnds, time_bnds )

  # create new nc file
  nc_new <- nc_create( paste0( output_dir, nc_file_name ), variable_list, force_v4 = T )

  # put nc variables into the nc file
  ncvar_put( nc_new, AGR, rotate_lat_lon( list( AGR_fin_grid ) )[[1]] )
  ncvar_put( nc_new, ENE, rotate_lat_lon( list( ENE_fin_grid ) )[[1]] )
  ncvar_put( nc_new, IND, rotate_lat_lon( list( IND_fin_grid ) )[[1]] )
  ncvar_put( nc_new, TRA, rotate_lat_lon( list( TRA_fin_grid ) )[[1]] )
  ncvar_put( nc_new, SLV, rotate_lat_lon( list( SLV_fin_grid ) )[[1]] )
  ncvar_put( nc_new, WST, rotate_lat_lon( list( WST_fin_grid ) )[[1]] )
  ncvar_put( nc_new, SHP, rotate_lat_lon( list( SHP_fin_grid ) )[[1]] )
  ncvar_put( nc_new, RCO, rotate_lat_lon( list( RCO_fin_grid ) )[[1]] )

  ncvar_put( nc_new, lon_bnds, t( lon_bnds_data ) )
  ncvar_put( nc_new, lat_bnds, t( lat_bnds_data ) )
  ncvar_put( nc_new, time_bnds, t( time_bnds_data ) )

  add_dimension_attributes( nc_new )

  # attributes for variables
  for ( each_var in fin_sector_list ) {
    ncatt_put( nc_new, each_var, 'cell_methods', 'time: mean' )
    ncatt_put( nc_new, each_var, 'missing_value', 1e+20, prec = 'float' )
  }

  add_global_attributes( nc_new, title = paste( 'annual emissions of', em ) )

  global_total_emission <- sum( total_month_em$value ) * 0.001
  ncatt_put( nc_new, 0, 'global_total_emission', paste0( round( global_total_emission, 2 ), ' Tg/year' ) )

  # species information
  reporting_info <- data.frame( em = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC', 'CO2', 'CH4' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass', 'Mass flux of CO2', 'Mass flux of CH4' ), stringsAsFactors = F )
  info_line <- reporting_info[ reporting_info$em == em, 'info' ]
  ncatt_put( nc_new, 0, 'reporting_unit', info_line )

  # close nc_new
  nc_close( nc_new)

  # additional section: write a summary and check text
  summary_name <- paste0( output_dir, 'CEDS_', em, '_anthro_', year, '_', grid_resolution, '.csv' )
  write.csv( total_month_em, file = summary_name, row.names = F )
}


# generate_final_grids_nc_subVOC
# Brief: generate annual nc grids for subVOC emissions
# Dependencies:
# Author: Leyang Feng
# parameters: int_grids_list - the list contains intermediate grids
#             output_dir - the output dir
#             grid_resolution - the gridding resolution
#             year - the current gridding year
#             em - the gridding emission species NMVOC
#             VOC_em - VOC ID
#             VOC_names - VOC name mapping file
#             sector_name_mapping - the mapping contains final sectors short names and long names
#             seasonality_mapping  - the seasonality mapping file
# return: null
# input files: null
# output: CEDS_[VOCID]_anthro_[year]_0.5_[CEDS_version].nc
#         CEDS_[VOCID]_anthro_[year]_0.5_[CEDS_version].csv
generate_final_grids_nc_subVOC <- function( int_grids_list,
                                            output_dir,
                                            grid_resolution,
                                            year,
                                            em = 'NMVOC',
                                            VOC_em,
                                            VOC_names,
                                            sector_name_mapping,
                                            seasonality_mapping ) {

  # 0
  VOC_name <- VOC_names[ VOC_names$VOC_id == VOC_em, 'VOC_name' ]
  VOC_mw <- VOC_names[ VOC_names$VOC_id == VOC_em, 'molecular.weight' ]
  global_grid_area <- grid_area( grid_resolution, all_lon = T )
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

  RCO_month_em <- data.frame( em = VOC_em, sector = 'RCO', year = year, month = 1 : 12, units = 'kt',
                              value = RCORC_month_em$value + RCOO_month_em$value, stringsAsFactors = F )
  total_month_em <- rbind( AGR_month_em, ENE_month_em, IND_month_em, TRA_month_em,
                           SLV_month_em, WST_month_em, SHP_month_em, RCO_month_em )

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
  nc_file_name <- paste0( 'CEDS_', VOC_em, '_anthro_', year, '_', grid_resolution, '_', version_stamp, '.nc' )

  # generate the var_list
  variable_list <- list( AGR, ENE, IND, TRA, SLV, WST, SHP, RCO, lat_bnds, lon_bnds, time_bnds )

  # create new nc file
  nc_new <- nc_create( paste0( output_dir, nc_file_name ), variable_list, force_v4 = T )

  # put nc variables into the nc file
  for ( i in seq_along( time ) ) {
  ncvar_put( nc_new, AGR, rotate_a_matrix( AGR_fin_grid[ , , i ] ), start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  ncvar_put( nc_new, ENE, rotate_a_matrix( ENE_fin_grid[ , , i ] ), start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  ncvar_put( nc_new, IND, rotate_a_matrix( IND_fin_grid[ , , i ] ), start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  ncvar_put( nc_new, TRA, rotate_a_matrix( TRA_fin_grid[ , , i ] ), start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  ncvar_put( nc_new, SLV, rotate_a_matrix( SLV_fin_grid[ , , i ] ), start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  ncvar_put( nc_new, WST, rotate_a_matrix( WST_fin_grid[ , , i ] ), start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  ncvar_put( nc_new, SHP, rotate_a_matrix( SHP_fin_grid[ , , i ] ), start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  ncvar_put( nc_new, RCO, rotate_a_matrix( RCO_fin_grid[ , , i ] ), start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  }
  ncvar_put( nc_new, lon_bnds, t( lon_bnds_data ) )
  ncvar_put( nc_new, lat_bnds, t( lat_bnds_data ) )
  ncvar_put( nc_new, time_bnds, t( time_bnds_data ) )

  add_dimension_attributes( nc_new )

  # attributes for variables
  for ( each_var in fin_sector_list ) {
    ncatt_put( nc_new, each_var, 'cell_methods', 'time: mean' )
    ncatt_put( nc_new, each_var, 'missing_value', 1e+20, prec = 'float' )
  }

  add_global_attributes( nc_new, title = paste0( 'Annual Emissions of ', VOC_em, '-', VOC_name ) )

  # Add VOC specific attributes
  ncatt_put( nc_new, 0, 'molecular weight', VOC_mw, prec = 'float' )
  ncatt_put( nc_new, 0, 'molecular weight unit', 'g mole-1' )
  ncatt_put( nc_new, 0, 'VOC_name', VOC_name )

  global_total_emission <- sum( total_month_em$value ) * 0.001
  ncatt_put( nc_new, 0, 'global_total_emission', paste0( round( global_total_emission, 2 ), ' Tg/year' ) )

  # species information
  reporting_info <- data.frame( em = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC', 'CO2', 'CH4' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass', 'Mass flux of CO2', 'Mass flux of CH4' ), stringsAsFactors = F )
  info_line <- reporting_info[ reporting_info$em == em, 'info' ]
  ncatt_put( nc_new, 0, 'reporting_unit', info_line )

  # close nc_new
  nc_close( nc_new)

  # additional section: write a summary and check text
  summary_name <- paste0( output_dir, 'CEDS_', VOC_em, '_anthro_', year, '_', grid_resolution, '.csv' )
  write.csv( total_month_em, file = summary_name, row.names = F )
}


# generate_final_grids_nc_solidbiofuel
# Brief: generate annual nc grids for solid biofuel emissions
# Dependencies:
# Author: Leyang Feng
# parameters: int_grids_list - the list contains intermediate grids
#             output_dir - the output dir
#             grid_resolution - the gridding resolution
#             year - the current gridding year
#             em - the gridding emission species
#             sector_name_mapping - the mapping contains final sectors short names and long names
#             seasonality_mapping  - the seasonality mapping file
# return: null
# input files:
# output: CEDS_[em]_solidbiofuel_anthro_[year]_0.5_[CEDS_version].nc
#         CEDS_[em]_solidbiofuel_anthro_[year]_0.5_[CEDS_version].csv
generate_final_grids_nc_solidbiofuel <- function( int_grids_list,
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
  all_zero_grid <- matrix( 0, 180 / grid_resolution, 360 / grid_resolution )

  AGR_int_grid <- int_grids_list$AGR_int_grid
  ELEC_int_grid <- int_grids_list$ELEC_int_grid
  ETRN_int_grid <- int_grids_list$ETRN_int_grid
  FFFI_int_grid <- int_grids_list$FFFI_int_grid
  FLR_int_grid <- int_grids_list$FLR_int_grid
  INDC_int_grid <- int_grids_list$INDC_int_grid
  INPU_int_grid<- int_grids_list$INPU_int_grid
  NRTR_int_grid <- int_grids_list$NRTR_int_grid
  ROAD_int_grid <- int_grids_list$ROAD_int_grid
  RCORC_int_grid <- int_grids_list$RCORC_int_grid
  RCOO_int_grid <- int_grids_list$RCOO_int_grid
  SLV_int_grid <- int_grids_list$SLV_int_grid
  WST_int_grid <- int_grids_list$WST_int_grid
  SHP_int_grid <- int_grids_list$SHP_int_grid
  TANK_int_grid <- int_grids_list$TANK_int_grid

  if( is.null( AGR_int_grid  ) ) { AGR_int_grid   <- all_zero_grid }
  if( is.null( ELEC_int_grid ) ) { ELEC_int_grid  <- all_zero_grid }
  if( is.null( ETRN_int_grid ) ) { ETRN_int_grid  <- all_zero_grid }
  if( is.null( FFFI_int_grid ) ) { FFFI_int_grid  <- all_zero_grid }
  if( is.null( FLR_int_grid  ) ) { FLR_int_grid   <- all_zero_grid }
  if( is.null( INDC_int_grid ) ) { INDC_int_grid  <- all_zero_grid }
  if( is.null( INPU_int_grid ) ) { INPU_int_grid  <- all_zero_grid }
  if( is.null( NRTR_int_grid ) ) { NRTR_int_grid  <- all_zero_grid }
  if( is.null( ROAD_int_grid ) ) { ROAD_int_grid  <- all_zero_grid }
  if( is.null( RCORC_int_grid) ) { RCORC_int_grid <- all_zero_grid }
  if( is.null( RCOO_int_grid ) ) { RCOO_int_grid  <- all_zero_grid }
  if( is.null( SLV_int_grid  ) ) { SLV_int_grid   <- all_zero_grid }
  if( is.null( WST_int_grid  ) ) { WST_int_grid   <- all_zero_grid }
  if( is.null( SHP_int_grid  ) ) { SHP_int_grid   <- all_zero_grid }
  if( is.null( TANK_int_grid ) ) { TANK_int_grid  <- all_zero_grid }

  AGR_proc_grid <- AGR_int_grid
  ENE_proc_grid <- ELEC_int_grid + ETRN_int_grid + FFFI_int_grid + FLR_int_grid
  IND_proc_grid <- INDC_int_grid + INPU_int_grid
  TRA_proc_grid <- NRTR_int_grid + ROAD_int_grid
  RCORC_proc_grid <- RCORC_int_grid
  RCOO_proc_grid <- RCOO_int_grid
  SLV_proc_grid <- SLV_int_grid
  WST_proc_grid <- WST_int_grid
  SHP_proc_grid <- SHP_int_grid + TANK_int_grid

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
                           SLV_month_em, WST_month_em, SHP_month_em, RCO_month_em )

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
  nc_file_name <- paste0( 'CEDS_', em, '_solidbiofuel_anthro_', year, '_', grid_resolution, '.nc' )

  # generate the var_list
  variable_list <- list( AGR, ENE, IND, TRA, SLV, WST, SHP, RCO, lat_bnds, lon_bnds, time_bnds )

  # create new nc file
  nc_new <- nc_create( paste0( output_dir, nc_file_name ), variable_list, force_v4 = T )

  # put nc variables into the nc file
  for ( i in seq_along( time ) ) {
  ncvar_put( nc_new, AGR, rotate_a_matrix( AGR_fin_grid[ , , i ] ), start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  ncvar_put( nc_new, ENE, rotate_a_matrix( ENE_fin_grid[ , , i ] ), start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  ncvar_put( nc_new, IND, rotate_a_matrix( IND_fin_grid[ , , i ] ), start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  ncvar_put( nc_new, TRA, rotate_a_matrix( TRA_fin_grid[ , , i ] ), start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  ncvar_put( nc_new, SLV, rotate_a_matrix( SLV_fin_grid[ , , i ] ), start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  ncvar_put( nc_new, WST, rotate_a_matrix( WST_fin_grid[ , , i ] ), start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  ncvar_put( nc_new, SHP, rotate_a_matrix( SHP_fin_grid[ , , i ] ), start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  ncvar_put( nc_new, RCO, rotate_a_matrix( RCO_fin_grid[ , , i ] ), start = c( 1, 1, i ), count = c( -1, -1, 1 ) )
  }
  ncvar_put( nc_new, lon_bnds, t( lon_bnds_data ) )
  ncvar_put( nc_new, lat_bnds, t( lat_bnds_data ) )
  ncvar_put( nc_new, time_bnds, t( time_bnds_data ) )

  add_dimension_attributes( nc_new )

  # attributes for variables
  for ( each_var in fin_sector_list ) {
    ncatt_put( nc_new, each_var, 'cell_methods', 'time: mean' )
    ncatt_put( nc_new, each_var, 'missing_value', 1e+20, prec = 'float' )
  }

  add_global_attributes( nc_new, title = paste( 'Annual Emissions of', em, '(solid biofuel)' ) )

  global_total_emission <- sum( total_month_em$value ) * 0.001
  ncatt_put( nc_new, 0, 'global_total_emission', paste0( round( global_total_emission, 2 ), ' Tg/year' ) )

  # species information
  reporting_info <- data.frame( em = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC', 'CO2', 'CH4' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass', 'Mass flux of CO2', 'Mass flux of CH4' ), stringsAsFactors = F )
  info_line <- reporting_info[ reporting_info$em == em, 'info' ]
  ncatt_put( nc_new, 0, 'reporting_unit', info_line )

  # close nc_new
  nc_close( nc_new)

  # additional section: write a summary and check text
  summary_name <- paste0( output_dir, 'CEDS_', em, '_solidbiofuel_anthro_', year, '_', grid_resolution, '.csv' )
  write.csv( total_month_em, file = summary_name, row.names = F )
}


# generate_final_grids_nc_aircraft
# Brief: generate annual nc grids for aircraft emissions
# Dependencies:
# Author: Leyang Feng
# parameters: AIR_em_global - aircraft annual grid
#             output_dir - the output dir
#             grid_resolution - the gridding resolution
#             year - the current gridding year
#             em - the gridding emission species
#             sector_name_mapping - the mapping contains final sectors short names
# return:
# input files:
# output:
generate_final_grids_nc_aircraft <- function( AIR_em_global,
                                     output_dir,
                                     grid_resolution,
                                     year,
                                     em,
                                     seasonality_mapping ) {

  # 0
  global_grid_area <- grid_area( grid_resolution, all_lon = T )
  flux_factor <- 1000000 / global_grid_area / ( 365 * 24 * 60 * 60 )
  days_in_month <- c( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 )

  # 1
  AIR_proc_grid <- AIR_em_global

  # 2
  AIR_fin_grid <- add_seasonality( AIR_proc_grid, em, 'AIR', year, days_in_month, grid_resolution, seasonality_mapping )

  # 3
  AIR_month_em <- sum_monthly_em( AIR_fin_grid, em, 'AIR', year, days_in_month, global_grid_area, seasonality_mapping )

  total_month_em <- AIR_month_em

 # NetCDF generation starts here
  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
  levs <- seq( 0.305, 14.945, 0.61 )
  base_days <- ( year - 1750 ) * 365
  time <- floor( c( 15.5, 45, 74.5, 105, 135.5, 166, 196.5, 227.5, 258, 288.5, 319, 349.5 ) )
  time <- time + base_days
  londim <- ncdim_def( "lon", "degrees_east", as.double( lons ), longname = 'longitude' )
  latdim <- ncdim_def( "lat", "degrees_north", as.double( lats ), longname = 'latitude' )
  levdim <- ncdim_def( "level", "km", as.double ( levs ), longname = 'altitude' )
  timedim <- ncdim_def( "time", paste0( "days since 1750-01-01 0:0:0" ), as.double( time ),
                        calendar = '365_day', longname = 'time' )
  dim_list <- list( londim, latdim, levdim, timedim )
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

  AIR <- ncvar_def( 'AIR', data_unit, dim_list, missval = missing_value, longname = 'Aircraft', prec = 'float', compression = 5  )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( bndsdim, timedim ), prec = 'double' )

  # generate nc file name
  nc_file_name <- paste0( 'CEDS_', em, '_AIR_anthro_', year, '_', grid_resolution, '.nc' )

  # generate the var_list
  variable_list <- list( AIR, lat_bnds, lon_bnds, time_bnds )

  # create new nc file
  nc_new <- nc_create(  paste0( output_dir, nc_file_name ), variable_list, force_v4 = T )

  # put nc variables into the nc file
  # transpose and flip the data first
  air_dim <- dim( AIR_fin_grid )
  AIR_array <- array( dim = c( air_dim[ 2 ], air_dim[ 1 ], air_dim[ 3 ], air_dim[ 4 ] ) )
  for ( i in 1 : air_dim[ 4 ] ) {
    for ( j in 1 : air_dim[ 3 ] ) {
	  AIR_array[ , , j, i ] <- rotate_a_matrix( AIR_fin_grid[ , , j, i ] )
	}
  }
  # then put the data into nc
  ncvar_put( nc_new, AIR, AIR_array )
  ncvar_put( nc_new, lon_bnds, t( lon_bnds_data ) )
  ncvar_put( nc_new, lat_bnds, t( lat_bnds_data ) )
  ncvar_put( nc_new, time_bnds, t( time_bnds_data ) )

  add_dimension_attributes( nc_new, air = TRUE )

  # attributes for variables
  ncatt_put( nc_new, 'AIR', 'cell_methods', 'time: mean' )
  ncatt_put( nc_new, 'AIR', 'missing_value', 1e+20, prec = 'float' )

  add_global_attributes( nc_new, title = paste( 'Annual Aircraft Emissions of', em ) )

  global_total_emission <- sum( total_month_em$value ) * 0.001
  ncatt_put( nc_new, 0, 'global_total_emission', paste0( round( global_total_emission, 2 ), ' Tg/year' ) )

  # species information
  reporting_info <- data.frame( em = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC', 'CO2', 'CH4' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass', 'Mass flux of CO2', 'Mass flux of CH4' ), stringsAsFactors = F )
  info_line <- reporting_info[ reporting_info$em == em, 'info' ]
  ncatt_put( nc_new, 0, 'reporting_unit', info_line )

  # close nc_new
  nc_close( nc_new)

  # additional section: write a summary and check text
  summary_name <- paste0( output_dir, 'CEDS_', em, '_AIR_anthro_', year, '_', grid_resolution, '.csv' )
  write.csv( total_month_em, file = summary_name, row.names = F )
}


# ====================== Chunk grids generation functions ======================

# singleVarChunking_bulkemissions
# Brief: generate multi-year emissions chunks for bulk emissions
# Dependencies:
# Author: Leyang Feng
# parameters: em
#             grid_resolution
#             chunk_start_years - a list of start years for each chunk
#             chunk_end_years - a list of start years for each chunk
#             chunk_count_index - the index of the chunk
#             input_dir
#             output_dir
# return: null
# input files: null
# output: [em]-em-anthro_input4MIPs_emissions_CMIP_CEDS-[CEDS_grid_version]_gn_[time_range].nc
singleVarChunking_bulkemissions <- function( em,
                                             grid_resolution,
                                             chunk_start_years,
                                             chunk_end_years,
                                             chunk_count_index,
                                             input_dir,
                                             output_dir ) {

  year_1st <- chunk_start_years[ chunk_count_index ]
  year_last <- chunk_end_years[ chunk_count_index ]
  chunk_yr_range <- year_1st:year_last

  # get in grids file list for current chunking
  fin_grid_list <- paste( 'CEDS', em, 'anthro', chunk_yr_range, '0.5.nc', sep = '_' )
  yearly_grid_files <- file.path( input_dir, fin_grid_list )
  if ( !all( file.exists( yearly_grid_files ) ) ) {
      stop( "Cannot find all files need for chunk ", year_1st, "-", year_last )
  }

  # read in each nc file and extract variables
  #
  # Compile the yearly values from each NetCDF file and stack them into a
  # final output array. This array will be written as the data values for the
  # output NetCDF.
  #
  # The order of sectors is as below:
  #  0: Agriculture;
  #  1: Energy Sector;
  #  2: Industrial Sector;
  #  3: Transportation Sector;
  #  4: Residential, Commercial, Other;
  #  5: Solvents production and application;
  #  6: Waste;
  #  7: International Shipping
  sectors <- c( 'AGR', 'ENE', 'IND', 'TRA', 'RCO', 'SLV', 'WST', 'SHP' )
  n_chunk_years <- ( chunk_end_years - chunk_start_years + 1 )[ chunk_count_index ]
  n_months <- 12 * n_chunk_years

  flat_array <- NULL
  time_array <- numeric( n_months )
  time_bnds_array <- array( dim = c( 2, n_months ) )
  current_month <- 1

  for ( fin_grid in fin_grid_list ) {
      nc <- nc_open( file.path( input_dir, fin_grid ) )

      # Build the final array if this is the first pass through. Note that the
      # dimension of the first sector should always be the same as the other
      # sectors within this file and across all other.
      if ( current_month == 1 ) {
          grid_dims <- nc[[ c( 'var', sectors[[1]], 'size' ) ]]
          flat_array <- array( dim = c( grid_dims[ 1:2 ], length( sectors ), n_months ) )

          MD_global_total_emission_1st_year <- ncatt_get( nc, 0, 'global_total_emission' )[[ 2 ]]
      }

      # Extract values of global_total_emission attribute from first year and last year
      if ( current_month == n_months - 12 + 1 ) {
          MD_global_total_emission_last_year <- ncatt_get( nc, 0, 'global_total_emission' )[[ 2 ]]
      }

      # Put the data into our pre-built arrays in 12-month chunks
      month_range <- current_month:(current_month + 12 - 1)
      for ( i in seq_along( sectors ) ) {
          flat_array[ , , i, month_range ] <- ncvar_get( nc, sectors[ i ] )
      }

      time_array[ month_range ] <- ncvar_get( nc, 'time' )
      time_bnds_array[ , month_range ] <- ncvar_get( nc, 'time_bnds' )

      current_month <- current_month + 12

      nc_close( nc )
  }

  # generate nc file

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
  filename_version_tag <- paste0( 'CEDS-', GRIDDING_VERSION )
  MD_source_value <- paste0( filename_version_tag, ': Community Emissions Data System (CEDS) for Historical Emissions' )
  MD_source_id_value <- filename_version_tag
  FN_source_id_value <- MD_source_id_value
  FN_variable_id_value <- paste0( em, '-em-anthro' )
  nc_file_name <- paste0( output_dir,
                          FN_variable_id_value,
                          '_input4MIPs_emissions_CMIP_',
                          FN_source_id_value,
                          '_gn_',
                          chunk_start_years[ chunk_count_index ], '01', '-', chunk_end_years[ chunk_count_index ], '12',
                          '.nc' )

  # generate flat_var variable name
  MD_variable_id_value <- paste0( em, '_em_anthro' ) # identical with FN_variable_id_value except the '-'
  flat_var_name <- MD_variable_id_value
  flat_var_longname <- paste0( em, ' Anthropogenic Emissions' )

  # define unit and missing value
  data_unit <- 'kg m-2 s-1'
  missing_value <- 1.e20
  MD_dataset_version_number_value <- GRIDDING_VERSION

  # define nc variables
  flat_var <- ncvar_def( flat_var_name, data_unit, dim_list, missval = missing_value, longname = flat_var_longname, compression = 5 )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( bndsdim, timedim ), prec = 'double' )
  sector_bnds <- ncvar_def( 'sector_bnds', '', list( bndsdim, sectordim ), prec = 'double' )

  # generate the var_list
  variable_list <- list( flat_var, lat_bnds, lon_bnds, time_bnds, sector_bnds )

  # create new nc file
  nc_new <- nc_create( nc_file_name, variable_list, force_v4 = T )

  # put nc variables into the nc file
  printLog( paste( 'Writing', nc_file_name ) )

  # write the data in 6 pieces to allow for lower-memory devices
  for ( sub_range in seq( n_months / 6, n_months, length.out = 6 ) ) {
      m1 <- 1 + sub_range - n_months / 6
      m2 <- sub_range
      st <- c( 1, 1, 1, m1 )
      ct <- c( -1, -1, -1, n_months / 6)
      ncvar_put( nc_new, flat_var, flat_array[ , , , m1:m2 ], start = st, count = ct )
  }
  ncvar_put( nc_new, lon_bnds, t( lon_bnds_data ) )
  ncvar_put( nc_new, lat_bnds, t( lat_bnds_data ) )
  ncvar_put( nc_new, time_bnds, time_bnds_data )
  ncvar_put( nc_new, sector_bnds, t( sector_bnds_data ) )

  title <- paste( 'Annual Anthropogenic Emissions of', em, 'prepared for input4MIPs' )
  add_global_attributes_single_var( nc_new, title, flat_var_name, MD_dataset_version_number_value, MD_source_value,
                                    MD_source_id_value, MD_variable_id_value, air = FALSE, primary = TRUE )

  # write first/last year global total emission into metadata
  if ( year_1st != year_last ) {
    ncatt_put( nc_new, 0, paste0( 'global_total_emission_', year_1st ),  MD_global_total_emission_1st_year )
    ncatt_put( nc_new, 0, paste0( 'global_total_emission_', year_last ), MD_global_total_emission_last_year )
  } else {
    ncatt_put( nc_new, 0, paste0( 'global_total_emission_', year_1st ),  MD_global_total_emission_1st_year )
  }
  # some other metadata
  ncatt_put( nc_new, 0, 'data_usage_tips', 'Note that these are monthly average fluxes.' )
  reporting_info <- data.frame( em = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC', 'CO2', 'CH4' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass', 'Mass flux of CO2', 'Mass flux of CH4' ), stringsAsFactors = F )
  info_line <- reporting_info[ reporting_info$em == em, 'info' ]
  ncatt_put( nc_new, 0, 'reporting_unit', info_line )

  # close nc_new
  nc_close( nc_new )

  # additional checksum output
  fin_csv_list <- gsub( '.nc', '.csv', fin_grid_list, fixed = T )
  checksum_df_list <- lapply( file.path( input_dir, fin_csv_list ), read.csv, stringsAsFactors = F )
  checksum_df <- do.call( 'rbind', checksum_df_list )
  checksum_file_name <- gsub( '.nc', '.csv', nc_file_name, fixed = T )
  write.csv( checksum_df, checksum_file_name, row.names = F )
}


# singleVarChunking_subVOCemissions
# Brief: generate multi-year emissions chunks for subVOC emissions
# Dependencies:
# Author: Leyang Feng
# parameters: VOC_em - VOC id
#             grid_resolution
#             chunk_start_years - a list of start years for each chunk
#             chunk_end_years - a list of start years for each chunk
#             chunk_count_index - the index of the chunk
#             input_dir
#             output_dir
#             VOC_names - VOC name mapping file
# return:
# input files:
# output: [VOCID]-acids-em-speciated-VOC-anthro_input4MIPs_emissions_CMIP_CEDS-[CEDS_grid_version]_supplement-data_gn_[time_range].nc
singleVarChunking_subVOCemissions <- function( VOC_em,
                                               grid_resolution,
                                               chunk_start_years,
                                               chunk_end_years,
                                               chunk_count_index,
                                               input_dir,
                                               output_dir,
                                               VOC_names ) {

  year_1st <- chunk_start_years[ chunk_count_index ]
  year_last <- chunk_end_years[ chunk_count_index ]
  chunk_yr_range <- year_1st:year_last

  # get in grids file list for current chunking
  fin_grid_list <- paste( 'CEDS', VOC_em, 'anthro', chunk_yr_range, '0.5.nc', sep = '_' )
  yearly_grid_files <- file.path( input_dir, fin_grid_list )
  if ( !all( file.exists( yearly_grid_files ) ) ) {
      stop( "Cannot find all files need for chunk ", year_1st, "-", year_last )
  }

  # read in each nc file and extract variables
  # create empty arrays for storage
  # TODO: This way of growing arrays is slow and bad, re-write with array size set before the loop.
  AGR_array <- ENE_array <- IND_array <- TRA_array <- RCO_array <- SLV_array <- WST_array <- SHP_array <- c( )
  time_array <- time_bnds_array <- c( )

  # go through each nc files, extract the variables and append to storage array
  for ( fin_grid in fin_grid_list ) {
    # open the nc file
    nc_temp <- nc_open( file.path( input_dir, fin_grid ) )
    # extract the data for variables
    AGR_fin_block <- ncvar_get( nc_temp, 'AGR' )
    ENE_fin_block <- ncvar_get( nc_temp, 'ENE' )
    IND_fin_block <- ncvar_get( nc_temp, 'IND' )
    TRA_fin_block <- ncvar_get( nc_temp, 'TRA' )
    RCO_fin_block <- ncvar_get( nc_temp, 'RCO' )
    SLV_fin_block <- ncvar_get( nc_temp, 'SLV' )
    WST_fin_block <- ncvar_get( nc_temp, 'WST' )
    SHP_fin_block <- ncvar_get( nc_temp, 'SHP' )
    time_fin_block <- ncvar_get( nc_temp, 'time' )
    time_bnds_fin_block <- ncvar_get( nc_temp, 'time_bnds' )
    # close the nc file
    nc_close( nc_temp )
    # append the data to storage array
    AGR_array <- c( AGR_array, AGR_fin_block )
    ENE_array <- c( ENE_array, ENE_fin_block )
    IND_array <- c( IND_array, IND_fin_block )
    TRA_array <- c( TRA_array, TRA_fin_block )
    RCO_array <- c( RCO_array, RCO_fin_block )
    SLV_array <- c( SLV_array, SLV_fin_block )
    WST_array <- c( WST_array, WST_fin_block )
    SHP_array <- c( SHP_array, SHP_fin_block )
    time_array <- c( time_array, time_fin_block )
    time_bnds_array <- c( time_bnds_array, time_bnds_fin_block )
  }

  # extract values of global_total_emission attribute from first year and last year
  # value from first year
  fin_grid_1st <- fin_grid_list[ 1 ]
  nc_temp <- nc_open( file.path( input_dir, fin_grid_1st ) )
  MD_global_total_emission_1st_year <- ncatt_get( nc_temp, 0, 'global_total_emission' )[[ 2 ]]
  nc_close( nc_temp )
  # value from last year
  fin_grid_last <- fin_grid_list[ length( fin_grid_list ) ]
  nc_temp <- nc_open( file.path( input_dir, fin_grid_last ) )
  MD_global_total_emission_last_year <- ncatt_get( nc_temp, 0, 'global_total_emission' )[[ 2 ]]

  # reshape the array
  # calculate how many years in the duration
  years_in_current_chunk <- chunk_end_years[ chunk_count_index ] - chunk_start_years[ chunk_count_index ] + 1
  # define dim for 3-D data
  array_dim <- c( dim( AGR_fin_block )[ 1 : 2 ], 12 * years_in_current_chunk ) # the dim for AGR_temp_data should always be the same in each nc file
  # time array has no need to be reshaped -- it's a one dimensional vector
  time_bnds_dim <- c( dim( time_bnds_fin_block )[ 1 ], 12 * years_in_current_chunk )
  # reshaping
  dim( AGR_array ) <- array_dim
  dim( ENE_array ) <- array_dim
  dim( IND_array ) <- array_dim
  dim( TRA_array ) <- array_dim
  dim( RCO_array ) <- array_dim
  dim( SLV_array ) <- array_dim
  dim( WST_array ) <- array_dim
  dim( SHP_array ) <- array_dim
  time_array <- time_array
  dim( time_bnds_array ) <- time_bnds_dim

  # create flat data block
  # the order of sectors is as below:
  #  0: Agriculture;
  #  1: Energy Sector;
  #  2: Industrial Sector;
  #  3: Transportation Sector;
  #  4: Residential, Commercial, Other;
  #  5: Solvents production and application;
  #  6: Waste;
  #  7: International Shipping

  flat_dim <- c( array_dim[ 1 ], array_dim[ 2 ], 8 , array_dim[ 3 ] )
  flat_array <- array( dim = flat_dim )
  flat_array[ , , 1 , ] <- AGR_array
  flat_array[ , , 2 , ] <- ENE_array
  flat_array[ , , 3 , ] <- IND_array
  flat_array[ , , 4 , ] <- TRA_array
  flat_array[ , , 5 , ] <- RCO_array
  flat_array[ , , 6 , ] <- SLV_array
  flat_array[ , , 7 , ] <- WST_array
  flat_array[ , , 8 , ] <- SHP_array

  # generate nc file

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
  filename_version_tag <- paste0( 'CEDS-', GRIDDING_VERSION )
  MD_source_value <- paste0( filename_version_tag, ': Community Emissions Data System (CEDS) for Historical Emissions' )
  MD_source_id_value <- paste0( filename_version_tag, '-supplemental-data' )
  FN_source_id_value <- MD_source_id_value
  VOC_name <- VOC_names[ VOC_names$VOC_id == VOC_em, 'VOC_name' ]
  VOC_name_10_dig <- substr( VOC_name, 1, 10 )
  VOC_name_10_dig <- gsub( '_', '-', VOC_name_10_dig, fixed = T )
  VOC_mw <- VOC_names[ VOC_names$VOC_id == VOC_em, 'molecular.weight' ]
  FN_variable_id_value <- paste0( VOC_em, '-', VOC_name_10_dig, '-em-speciated-VOC-anthro' )
  nc_file_name <- paste0( output_dir,
                          FN_variable_id_value,
                          '_input4MIPs_emissions_CMIP_',
                          FN_source_id_value,
                          '_gn_',
                          chunk_start_years[ chunk_count_index ], '01', '-', chunk_end_years[ chunk_count_index ], '12',
                          '.nc' )

  # generate flat_var variable name
  MD_variable_id_value <- gsub( '-', '_', FN_variable_id_value, fixed = T )
  flat_var_name <- MD_variable_id_value
  flat_var_longname <- paste0( VOC_em, ' ', VOC_name, ' Anthropogenic Emissions' )

  # define unit and missing value
  data_unit <- 'kg m-2 s-1'
  missing_value <- 1.e20
  MD_dataset_version_number_value <- GRIDDING_VERSION

  # define nc variables
  flat_var <- ncvar_def( flat_var_name, data_unit, dim_list, missval = missing_value, longname = flat_var_longname, compression = 5 )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( bndsdim, timedim ), prec = 'double' )
  sector_bnds <- ncvar_def( 'sector_bnds', '', list( bndsdim, sectordim ), prec = 'double' )

  # generate the var_list
  variable_list <- list( flat_var, lat_bnds, lon_bnds, time_bnds, sector_bnds )

  # create new nc file
  nc_new <- nc_create( nc_file_name, variable_list, force_v4 = T )

  # put nc variables into the nc file
  ncvar_put( nc_new, flat_var, flat_array )
  ncvar_put( nc_new, lon_bnds, t( lon_bnds_data ) )
  ncvar_put( nc_new, lat_bnds, t( lat_bnds_data ) )
  ncvar_put( nc_new, time_bnds, time_bnds_data )
  ncvar_put( nc_new, sector_bnds, t( sector_bnds_data ) )

  title <- paste( 'Annual Anthropogenic Emissions of', VOC_em, VOC_name, 'prepared for input4MIPs' )
  add_global_attributes_single_var( nc_new, title, flat_var_name, MD_dataset_version_number_value, MD_source_value,
                                    MD_source_id_value, MD_variable_id_value, air = FALSE, primary = FALSE )

  # write first/last year global total emission into metadata
  if ( year_1st != year_last ) {
    ncatt_put( nc_new, 0, paste0( 'global_total_emission_', year_1st ),  MD_global_total_emission_1st_year )
    ncatt_put( nc_new, 0, paste0( 'global_total_emission_', year_last ), MD_global_total_emission_last_year )
  } else {
    ncatt_put( nc_new, 0, paste0( 'global_total_emission_', year_1st ),  MD_global_total_emission_1st_year )
  }
  # some other metadata
  ncatt_put( nc_new, 0, 'VOC_name', VOC_name )
  ncatt_put( nc_new, 0, 'molecular_weight', VOC_mw, prec = 'float' )
  ncatt_put( nc_new, 0, 'molecular_weight_unit', 'g mole-1' )

  # close nc_new
  nc_close( nc_new )

  # additional checksum output
  fin_csv_list <- gsub( '.nc', '.csv', fin_grid_list, fixed = T )
  checksum_df_list <- lapply( file.path( input_dir, fin_csv_list ), read.csv, stringsAsFactors = F )
  checksum_df <- do.call( 'rbind', checksum_df_list )
  checksum_df$em <- VOC_em
  checksum_file_name <- gsub( '.nc', '.csv', nc_file_name, fixed = T )
  write.csv( checksum_df, checksum_file_name, row.names = F )
}


# singleVarChunking_solidbiofuelemissions
# Brief: generate multi-year emissions chunks for solid biofuel emissions
# Dependencies:
# Author: Leyang Feng
# parameters: em
#             grid_resolution
#             chunk_start_years - a list of start years for each chunk
#             chunk_end_years - a list of start years for each chunk
#             chunk_count_index - the index of the chunk
#             input_dir
#             output_dir
# return: null
# input files: null
# output: [em]-em-SOLID-BIOFUEL-anthro_input4MIPs_emissions_CMIP_CEDS-[CEDS_grid_version]_supplement-data_gn_[time_range].nc
singleVarChunking_solidbiofuelemissions <- function( em,
                                                     grid_resolution,
                                                     chunk_start_years,
                                                     chunk_end_years,
                                                     chunk_count_index,
                                                     input_dir,
                                                     output_dir ) {

  year_1st <- chunk_start_years[ chunk_count_index ]
  year_last <- chunk_end_years[ chunk_count_index ]
  chunk_yr_range <- year_1st:year_last

  # get in grids file list for current chunking
  fin_grid_list <- paste( 'CEDS', em, 'solidbiofuel_anthro', chunk_yr_range, '0.5.nc', sep = '_' )
  yearly_grid_files <- file.path( input_dir, fin_grid_list )
  if ( !all( file.exists( yearly_grid_files ) ) ) {
      stop( "Cannot find all files need for chunk ", year_1st, "-", year_last )
  }

  # read in each nc file and extract variables
  # create empty arrays for storage
  # TODO: This way of growing arrays is slow and bad, re-write with array size set before the loop.
  AGR_array <- ENE_array <- IND_array <- TRA_array <- RCO_array <- SLV_array <- WST_array <- SHP_array <- c( )
  time_array <- time_bnds_array <- c( )

  # go through each nc files, extract the variables and append to storage array
  for ( fin_grid in fin_grid_list ) {
    # open the nc file
    nc_temp <- nc_open( file.path( input_dir, fin_grid ) )
    # extract the data for variables
    AGR_fin_block <- ncvar_get( nc_temp, 'AGR' )
    ENE_fin_block <- ncvar_get( nc_temp, 'ENE' )
    IND_fin_block <- ncvar_get( nc_temp, 'IND' )
    TRA_fin_block <- ncvar_get( nc_temp, 'TRA' )
    RCO_fin_block <- ncvar_get( nc_temp, 'RCO' )
    SLV_fin_block <- ncvar_get( nc_temp, 'SLV' )
    WST_fin_block <- ncvar_get( nc_temp, 'WST' )
    SHP_fin_block <- ncvar_get( nc_temp, 'SHP' )
    time_fin_block <- ncvar_get( nc_temp, 'time' )
    time_bnds_fin_block <- ncvar_get( nc_temp, 'time_bnds' )
    # close the nc file
    nc_close( nc_temp )
    # append the data to storage array
    AGR_array <- c( AGR_array, AGR_fin_block )
    ENE_array <- c( ENE_array, ENE_fin_block )
    IND_array <- c( IND_array, IND_fin_block )
    TRA_array <- c( TRA_array, TRA_fin_block )
    RCO_array <- c( RCO_array, RCO_fin_block )
    SLV_array <- c( SLV_array, SLV_fin_block )
    WST_array <- c( WST_array, WST_fin_block )
    SHP_array <- c( SHP_array, SHP_fin_block )
    time_array <- c( time_array, time_fin_block )
    time_bnds_array <- c( time_bnds_array, time_bnds_fin_block )
  }

  # extract values of global_total_emission attribute from first year and last year
  # value from first year
  fin_grid_1st <- fin_grid_list[ 1 ]
  nc_temp <- nc_open( file.path( input_dir, fin_grid_1st ) )
  MD_global_total_emission_1st_year <- ncatt_get( nc_temp, 0, 'global_total_emission' )[[ 2 ]]
  nc_close( nc_temp )
  # value from last year
  fin_grid_last <- fin_grid_list[ length( fin_grid_list ) ]
  nc_temp <- nc_open( file.path( input_dir, fin_grid_last ) )
  MD_global_total_emission_last_year <- ncatt_get( nc_temp, 0, 'global_total_emission' )[[ 2 ]]

  # reshape the array
  # calculate how many years in the duration
  years_in_current_chunk <- chunk_end_years[ chunk_count_index ] - chunk_start_years[ chunk_count_index ] + 1
  # define dim for 3-D data
  array_dim <- c( dim( AGR_fin_block )[ 1 : 2 ], 12 * years_in_current_chunk ) # the dim for AGR_temp_data should always be the same in each nc file
  # time array has no need to be reshaped -- it's a one dimensional vector
  time_bnds_dim <- c( dim( time_bnds_fin_block )[ 1 ], 12 * years_in_current_chunk )
  # reshaping
  dim( AGR_array ) <- array_dim
  dim( ENE_array ) <- array_dim
  dim( IND_array ) <- array_dim
  dim( TRA_array ) <- array_dim
  dim( RCO_array ) <- array_dim
  dim( SLV_array ) <- array_dim
  dim( WST_array ) <- array_dim
  dim( SHP_array ) <- array_dim
  time_array <- time_array
  dim( time_bnds_array ) <- time_bnds_dim

  # create flat data block
  # the order of sectors is as below:
  #  0: Agriculture;
  #  1: Energy Sector;
  #  2: Industrial Sector;
  #  3: Transportation Sector;
  #  4: Residential, Commercial, Other;
  #  5: Solvents production and application;
  #  6: Waste;
  #  7: International Shipping

  flat_dim <- c( array_dim[ 1 ], array_dim[ 2 ], 8 , array_dim[ 3 ] )
  flat_array <- array( dim = flat_dim )
  flat_array[ , , 1 , ] <- AGR_array
  flat_array[ , , 2 , ] <- ENE_array
  flat_array[ , , 3 , ] <- IND_array
  flat_array[ , , 4 , ] <- TRA_array
  flat_array[ , , 5 , ] <- RCO_array
  flat_array[ , , 6 , ] <- SLV_array
  flat_array[ , , 7 , ] <- WST_array
  flat_array[ , , 8 , ] <- SHP_array

  # generate nc file

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
  filename_version_tag <- paste0( 'CEDS-', GRIDDING_VERSION )
  MD_source_value <- paste0( filename_version_tag, ': Community Emissions Data System (CEDS) for Historical Emissions' )
  MD_source_id_value <- paste0( filename_version_tag, '-supplemental-data' )
  FN_source_id_value <- MD_source_id_value
  FN_variable_id_value <- paste0( em, '-em-SOLID-BIOFUEL-anthro' )
  nc_file_name <- paste0( output_dir,
                          FN_variable_id_value,
                          '_input4MIPs_emissions_CMIP_',
                          FN_source_id_value,
                          '_gn_',
                          chunk_start_years[ chunk_count_index ], '01', '-', chunk_end_years[ chunk_count_index ], '12',
                          '.nc' )

  # generate flat_var variable name
  MD_variable_id_value <- paste0( em, '_em_SOLID_BIOFUEL_anthro' )
  flat_var_name <- MD_variable_id_value
  flat_var_longname <- paste0( em, ' SOLID BIOFUEL Anthropogenic Emissions - Supplemental Data' )


  # define unit and missing value
  data_unit <- 'kg m-2 s-1'
  missing_value <- 1.e20
  MD_dataset_version_number_value <- GRIDDING_VERSION

  # define nc variables
  flat_var <- ncvar_def( flat_var_name, data_unit, dim_list, missval = missing_value, longname = flat_var_longname, compression = 5 )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( bndsdim, timedim ), prec = 'double' )
  sector_bnds <- ncvar_def( 'sector_bnds', '', list( bndsdim, sectordim ), prec = 'double' )

  # generate the var_list
  variable_list <- list( flat_var, lat_bnds, lon_bnds, time_bnds, sector_bnds )

  # create new nc file
  nc_new <- nc_create( nc_file_name, variable_list, force_v4 = T )

  # put nc variables into the nc file
  ncvar_put( nc_new, flat_var, flat_array )
  ncvar_put( nc_new, lon_bnds, t( lon_bnds_data ) )
  ncvar_put( nc_new, lat_bnds, t( lat_bnds_data ) )
  ncvar_put( nc_new, time_bnds, time_bnds_data )
  ncvar_put( nc_new, sector_bnds, t( sector_bnds_data ) )

  title <- paste( 'Annual SOLID BIOFUEL Anthropogenic Emissions of', em, 'prepared for input4MIPs' )
  add_global_attributes_single_var( nc_new, title, flat_var_name, MD_dataset_version_number_value, MD_source_value,
                                    MD_source_id_value, MD_variable_id_value, air = FALSE, primary = FALSE )

  # write first/last year global total emission into metadata
  if ( year_1st != year_last ) {
    ncatt_put( nc_new, 0, paste0( 'global_total_emission_', year_1st ),  MD_global_total_emission_1st_year )
    ncatt_put( nc_new, 0, paste0( 'global_total_emission_', year_last ), MD_global_total_emission_last_year )
  } else {
    ncatt_put( nc_new, 0, paste0( 'global_total_emission_', year_1st ),  MD_global_total_emission_1st_year )
  }
  # some other metadata
  ncatt_put( nc_new, 0, 'data_usage_tips', 'Note that these are monthly average fluxes. Supplemental data, emissions in this data file are already included in the primary data file. See README.' )
  reporting_info <- data.frame( em = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC', 'CO2', 'CH4' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass', 'Mass flux of CO2', 'Mass flux of CH4' ), stringsAsFactors = F )
  info_line <- reporting_info[ reporting_info$em == em, 'info' ]
  ncatt_put( nc_new, 0, 'reporting_unit', info_line )

  # close nc_new
  nc_close( nc_new )

  # additional checksum output
  fin_csv_list <- gsub( '.nc', '.csv', fin_grid_list, fixed = T )
  checksum_df_list <- lapply( file.path( input_dir, fin_csv_list ), read.csv, stringsAsFactors = F )
  checksum_df <- do.call( 'rbind', checksum_df_list )
  checksum_file_name <- gsub( '.nc', '.csv', nc_file_name, fixed = T )
  write.csv( checksum_df, checksum_file_name, row.names = F )
}


# singleVarChunking_aircraftemissions
# Brief: generate multi-year emissions chunks for aircraft emissions
# Dependencies:
# Author: Leyang Feng
# parameters: em
#             grid_resolution
#             chunk_start_years - a list of start years for each chunk
#             chunk_end_years - a list of start years for each chunk
#             chunk_count_index - the index of the chunk
#             input_dir
#             output_dir
# return: null
# input files: null
# output: [em]-em-AIR-anthro_input4MIPs_emissions_CMIP_CEDS-[CEDS_grid_version]_gn_[time_range].nc
singleVarChunking_aircraftemissions <- function( em,
                                                 grid_resolution,
                                                 chunk_start_years,
                                                 chunk_end_years,
                                                 chunk_count_index,
                                                 input_dir,
                                                 output_dir ) {
  year_1st <- chunk_start_years[ chunk_count_index ]
  year_last <- chunk_end_years[ chunk_count_index ]
  chunk_yr_range <- year_1st:year_last

  # get in grids file list for current chunking
  fin_grid_list <- paste( 'CEDS', em, 'AIR_anthro', chunk_yr_range, '0.5.nc', sep = '_' )
  yearly_grid_files <- file.path( input_dir, fin_grid_list )
  if ( !all( file.exists( yearly_grid_files ) ) ) {
      stop( "Cannot find all files need for chunk ", year_1st, "-", year_last )
  }

  n_chunk_years <- ( chunk_end_years - chunk_start_years + 1 )[ chunk_count_index ]
  n_months <- 12 * n_chunk_years

  AIR_array <- NULL
  time_array <- numeric( n_months )
  time_bnds_array <- array( dim = c( 2, n_months ) )
  current_month <- 1

  for ( fin_grid in fin_grid_list ) {
    nc <- nc_open( file.path( input_dir, fin_grid ) )

    # Build the final array if this is the first pass through.
    if ( current_month == 1 ) {
      grid_dims <- nc[[ c( 'var', 'AIR', 'size' ) ]]
      AIR_array <- array( dim = c( grid_dims[ 1:3 ], n_months ) )

      MD_global_total_emission_1st_year <- ncatt_get( nc, 0, 'global_total_emission' )[[ 2 ]]
    }

    # Extract values of global_total_emission attribute from first year and last year
    if ( current_month == n_months - 12 + 1 ) {
      MD_global_total_emission_last_year <- ncatt_get( nc, 0, 'global_total_emission' )[[ 2 ]]
    }

    # Put the data into our pre-built arrays in 12-month chunks
    month_range <- current_month:(current_month + 12 - 1)
    AIR_array[ , , , month_range ] <- ncvar_get( nc )

    time_array[ month_range ] <- ncvar_get( nc, 'time' )
    time_bnds_array[ , month_range ] <- ncvar_get( nc, 'time_bnds' )

    current_month <- current_month + 12

    nc_close( nc )
  }

  # generate nc file
  # the new nc generation routine begins here
  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
  levs <- seq( 0.305, 14.945, 0.61 )

  londim <- ncdim_def( "lon", "degrees_east", as.double( lons ), longname = 'longitude' )
  latdim <- ncdim_def( "lat", "degrees_north", as.double( lats ), longname = 'latitude' )
  levdim <- ncdim_def( "level", "km", as.double ( levs ), longname = 'altitude' )
  timedim <- ncdim_def( "time", paste0( "days since 1750-01-01 0:0:0" ), as.double( time_array ),
                        calendar = '365_day', longname = 'time' )
  dim_list <- list( londim, latdim, levdim, timedim )
  lon_bnds_data <- cbind( seq( -180, ( 180 - grid_resolution ), grid_resolution ),
                          seq( ( -180 + grid_resolution ), 180, grid_resolution ) )
  lat_bnds_data <- cbind( seq( -90, (90 - grid_resolution) , grid_resolution),
                          seq( ( -90 + grid_resolution ), 90, grid_resolution ) )
  bnds <- 1 : 2
  bndsdim <- ncdim_def( "bnds", '', as.integer( bnds ), longname = 'bounds', create_dimvar = F )
  time_bnds_data <- time_bnds_array

  # generate nc file name
  filename_version_tag <- paste0( 'CEDS-', GRIDDING_VERSION )
  MD_source_value <- paste0( filename_version_tag, ': Community Emissions Data System (CEDS) for Historical Emissions' )
  MD_source_id_value <- filename_version_tag
  FN_source_id_value <- MD_source_id_value
  FN_variable_id_value <- paste0( em, '-em-AIR-anthro' )
  nc_file_name <- paste0( output_dir,
                          FN_variable_id_value,
                          '_input4MIPs_emissions_CMIP_',
                          FN_source_id_value,
                          '_gn_',
                          chunk_start_years[ chunk_count_index ], '01', '-', chunk_end_years[ chunk_count_index ], '12',
                          '.nc' )

  # generate flat_var variable name
  MD_variable_id_value <- paste0( em, '_em_AIR_anthro' )
  AIR_var_name <- MD_variable_id_value
  AIR_var_longname <- paste0( em, ' Aircraft Anthropogenic Emissions' )

  # define unit and missing value
  data_unit <- 'kg m-2 s-1'
  missing_value <- 1.e20
  MD_dataset_version_number_value <- GRIDDING_VERSION

  # define nc variables
  AIR_var <- ncvar_def( AIR_var_name, data_unit, dim_list, missval = missing_value, longname = AIR_var_longname, compression = 5 )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( bndsdim, timedim ), prec = 'double' )

  # generate the var_list
  variable_list <- list( AIR_var, lat_bnds, lon_bnds, time_bnds )

  # create new nc file
  nc_new <- nc_create( nc_file_name, variable_list, force_v4 = T )

  # put nc variables into the nc file
  # There is a 2GB limit for writing (perhaps because of an older rCPP version
  # in ncdf4), so the values need to be written in chunks of time-steps
  write_chunk_size <- floor((2^31 - 1) / (prod(AIR_array[1:3]) * 8))
  time_start_idx <- 1
  while ( time_start_idx < AIR_array[4] ) {
      next_time_start <- min(time_start_idx + write_chunk_size, AIR_array[4] + 1)
      time_steps <- next_time_start - time_start_idx
      ncvar_put( nc_new, AIR_var, AIR_array[ , , , time_start_idx:(next_time_start - 1) ],
                 start = c( 1, 1, 1, time_start_idx ),
                 count = c( -1, -1, -1, time_steps ) )
      time_start_idx <- next_time_start
  }
  ncvar_put( nc_new, lon_bnds, t( lon_bnds_data ) )
  ncvar_put( nc_new, lat_bnds, t( lat_bnds_data ) )
  ncvar_put( nc_new, time_bnds, time_bnds_data )

  title <- paste( 'Annual Aircraft Anthropogenic Emissions of', em, 'prepared for input4MIPs' )
  add_global_attributes_single_var( nc_new, title, AIR_var_name, MD_dataset_version_number_value, MD_source_value,
                                    MD_source_id_value, MD_variable_id_value, air = TRUE, primary = TRUE )

  # write first/last year global total emission into metadata
  if ( year_1st != year_last ) {
    ncatt_put( nc_new, 0, paste0( 'global_total_emission_', year_1st ),  MD_global_total_emission_1st_year )
    ncatt_put( nc_new, 0, paste0( 'global_total_emission_', year_last ), MD_global_total_emission_last_year )
  } else {
    ncatt_put( nc_new, 0, paste0( 'global_total_emission_', year_1st ),  MD_global_total_emission_1st_year )
  }
  # some other metadata
  ncatt_put( nc_new, 0, 'data_usage_tips', 'Note that these are monthly average fluxes.' )
  reporting_info <- data.frame( em = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC', 'CO2', 'CH4' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass', 'Mass flux of CO2', 'Mass flux of CH4' ), stringsAsFactors = F )
  info_line <- reporting_info[ reporting_info$em == em, 'info' ]
  ncatt_put( nc_new, 0, 'reporting_unit', info_line )

  # close nc_new
  nc_close( nc_new )

  # additional checksum output
  fin_csv_list <- gsub( '.nc', '.csv', fin_grid_list, fixed = T )
  checksum_df_list <- lapply( file.path( input_dir, fin_csv_list ), read.csv, stringsAsFactors = F )
  checksum_df <- do.call( 'rbind', checksum_df_list )
  checksum_file_name <- gsub( '.nc', '.csv', nc_file_name, fixed = T )
  write.csv( checksum_df, checksum_file_name, row.names = F )
}


# singleVarChunking_extendedCH4bulk
# Brief: generate multi-year emissions chunks for bulk emissions
# Dependencies:
# Author: Leyang Feng
# parameters: em
#             grid_resolution
#             chunk_start_years - a list of start years for each chunk
#             chunk_end_years - a list of start years for each chunk
#             chunk_count_index - the index of the chunk
#             input_dir
#             output_dir
# return: null
# input files: null
# output: [em]-em-anthro_input4MIPs_emissions_CMIP_CEDS-[CEDS_grid_version]_gn_[time_range].nc

singleVarChunking_extendedCH4bulk <- function( em,
                                           grid_resolution,
                                           chunk_start_years,
                                           chunk_end_years,
                                           chunk_density,
                                           chunk_count_index,
                                           input_dir,
                                           output_dir ) {

  year_1st <- chunk_start_years[ chunk_count_index ]
  year_last <- chunk_end_years[ chunk_count_index ]

  # get in grids file list for current chunking
  filename_patterns <- paste0( 'CEDS_', em, '_anthro_', seq( year_1st, year_last, chunk_density ), '_0.5.nc' )
  fin_grid_list <- unlist( lapply( filename_patterns, function( filename_pattern ) {
    list.files( input_dir, pattern = filename_pattern )
  } ) )

  # read in each nc file and extract variables
  # create empty arrays for storage
  # TODO: This way of growing arrays is slow and bad, re-write with array size set before the loop.
  AGR_array <- ENE_array <- IND_array <- TRA_array <- RCO_array <- SLV_array <- WST_array <- SHP_array <- c( )
  time_array <- time_bnds_array <- c( )

  # go through each nc files, extract the variables and append to storage array
  for ( fin_grid in fin_grid_list ) {
    # open the nc file
    nc_temp <- nc_open( file.path( input_dir, fin_grid ) )
    # extract the data for variables
    AGR_fin_block <- ncvar_get( nc_temp, 'AGR' )
    ENE_fin_block <- ncvar_get( nc_temp, 'ENE' )
    IND_fin_block <- ncvar_get( nc_temp, 'IND' )
    TRA_fin_block <- ncvar_get( nc_temp, 'TRA' )
    RCO_fin_block <- ncvar_get( nc_temp, 'RCO' )
    SLV_fin_block <- ncvar_get( nc_temp, 'SLV' )
    WST_fin_block <- ncvar_get( nc_temp, 'WST' )
    SHP_fin_block <- ncvar_get( nc_temp, 'SHP' )
    time_fin_block <- ncvar_get( nc_temp, 'time' )
    time_bnds_fin_block <- ncvar_get( nc_temp, 'time_bnds' )
    # close the nc file
    nc_close( nc_temp )
    # append the data to storage array
    AGR_array <- c( AGR_array, AGR_fin_block )
    ENE_array <- c( ENE_array, ENE_fin_block )
    IND_array <- c( IND_array, IND_fin_block )
    TRA_array <- c( TRA_array, TRA_fin_block )
    RCO_array <- c( RCO_array, RCO_fin_block )
    SLV_array <- c( SLV_array, SLV_fin_block )
    WST_array <- c( WST_array, WST_fin_block )
    SHP_array <- c( SHP_array, SHP_fin_block )
    time_array <- c( time_array, time_fin_block )
    time_bnds_array <- c( time_bnds_array, time_bnds_fin_block )
  }

  # extract values of global_total_emission attribute from first year and last year
  # value from first year
  fin_grid_1st <- fin_grid_list[ 1 ]
  nc_temp <- nc_open( file.path( input_dir, fin_grid_1st ) )
  MD_global_total_emission_1st_year <- ncatt_get( nc_temp, 0, 'global_total_emission' )[[ 2 ]]
  nc_close( nc_temp )
  # value from last year
  fin_grid_last <- fin_grid_list[ length( fin_grid_list ) ]
  nc_temp <- nc_open( file.path( input_dir, fin_grid_last ) )
  MD_global_total_emission_last_year <- ncatt_get( nc_temp, 0, 'global_total_emission' )[[ 2 ]]

  # reshape the array
  # calculate how many years in the duration
  years_in_current_chunk <- ( chunk_end_years[ chunk_count_index ] - chunk_start_years[ chunk_count_index ] ) / chunk_density + 1
  # define dim for 3-D data
  array_dim <- c( dim( AGR_fin_block )[ 1 : 2 ], 12 * years_in_current_chunk ) # the dim for AGR_temp_data should always be the same in each nc file
  # time array has no need to be reshaped -- it's a one dimensional vector
  time_bnds_dim <- c( dim( time_bnds_fin_block )[ 1 ], 12 * years_in_current_chunk )
  # reshaping
  dim( AGR_array ) <- array_dim
  dim( ENE_array ) <- array_dim
  dim( IND_array ) <- array_dim
  dim( TRA_array ) <- array_dim
  dim( RCO_array ) <- array_dim
  dim( SLV_array ) <- array_dim
  dim( WST_array ) <- array_dim
  dim( SHP_array ) <- array_dim
  time_array <- time_array
  dim( time_bnds_array ) <- time_bnds_dim

  # create flat data block
  # the order of sectors is as below:
  #  0: Agriculture;
  #  1: Energy Sector;
  #  2: Industrial Sector;
  #  3: Transportation Sector;
  #  4: Residential, Commercial, Other;
  #  5: Solvents production and application;
  #  6: Waste;
  #  7: International Shipping

  flat_dim <- c( array_dim[ 1 ], array_dim[ 2 ], 8 , array_dim[ 3 ] )
  flat_array <- array( dim = flat_dim )
  flat_array[ , , 1 , ] <- AGR_array
  flat_array[ , , 2 , ] <- ENE_array
  flat_array[ , , 3 , ] <- IND_array
  flat_array[ , , 4 , ] <- TRA_array
  flat_array[ , , 5 , ] <- RCO_array
  flat_array[ , , 6 , ] <- SLV_array
  flat_array[ , , 7 , ] <- WST_array
  flat_array[ , , 8 , ] <- SHP_array

  # generate nc file

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
  filename_version_tag <- paste0( 'CEDS-', GRIDDING_VERSION )
  MD_source_value <- paste0( filename_version_tag, ': Community Emissions Data System (CEDS) for Historical Emissions' )
  MD_source_id_value <- paste0( filename_version_tag, '-supplemental-data' )
  FN_source_id_value <- MD_source_id_value
  FN_variable_id_value <- paste0( em, '-em-anthro' )
  nc_file_name <- paste0( output_dir,
                          FN_variable_id_value,
                          '_input4MIPs_emissions_CMIP_',
                          FN_source_id_value,
                          '_gn_',
                          chunk_start_years[ chunk_count_index ], '01', '-', chunk_end_years[ chunk_count_index ], '12',
                          '.nc' )

  # generate flat_var variable name
  MD_variable_id_value <- paste0( em, '_em_anthro' ) # identical with FN_variable_id_value except the '-'
  flat_var_name <- MD_variable_id_value
  flat_var_longname <- paste0( em, ' Anthropogenic Emissions' )

  # define unit and missing value
  data_unit <- 'kg m-2 s-1'
  missing_value <- 1.e20
  MD_dataset_version_number_value <- GRIDDING_VERSION

  # define nc variables
  flat_var <- ncvar_def( flat_var_name, data_unit, dim_list, missval = missing_value, longname = flat_var_longname, compression = 5 )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( bndsdim, timedim ), prec = 'double' )
  sector_bnds <- ncvar_def( 'sector_bnds', '', list( bndsdim, sectordim ), prec = 'double' )

  # generate the var_list
  variable_list <- list( flat_var, lat_bnds, lon_bnds, time_bnds, sector_bnds )

  # create new nc file
  nc_new <- nc_create( nc_file_name, variable_list, force_v4 = T )

  # put nc variables into the nc file
  ncvar_put( nc_new, flat_var, flat_array )
  ncvar_put( nc_new, lon_bnds, t( lon_bnds_data ) )
  ncvar_put( nc_new, lat_bnds, t( lat_bnds_data ) )
  ncvar_put( nc_new, time_bnds, time_bnds_data )
  ncvar_put( nc_new, sector_bnds, t( sector_bnds_data ) )

  title <- paste( 'Annual Anthropogenic Emissions of', em, 'prepared for input4MIPs' )
  add_global_attributes_single_var( nc_new, title, flat_var_name, MD_dataset_version_number_value, MD_source_value,
                                    MD_source_id_value, MD_variable_id_value, air = FALSE, primary = FALSE )

  # write first/last year global total emission into metadata
  if ( year_1st != year_last ) {
    ncatt_put( nc_new, 0, paste0( 'global_total_emission_', year_1st ),  MD_global_total_emission_1st_year )
    ncatt_put( nc_new, 0, paste0( 'global_total_emission_', year_last ), MD_global_total_emission_last_year )
  } else {
    ncatt_put( nc_new, 0, paste0( 'global_total_emission_', year_1st ),  MD_global_total_emission_1st_year )
  }
  # some other metadata
  ncatt_put( nc_new, 0, 'data_usage_tips', 'Note that these are monthly average fluxes.' )
  reporting_info <- data.frame( em = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC', 'CO2', 'CH4' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass', 'Mass flux of CO2', 'Mass flux of CH4' ), stringsAsFactors = F )
  info_line <- reporting_info[ reporting_info$em == em, 'info' ]
  ncatt_put( nc_new, 0, 'reporting_unit', info_line )

  # close nc_new
  nc_close( nc_new )

  # additional checksum output
  fin_csv_list <- gsub( '.nc', '.csv', fin_grid_list, fixed = T )
  checksum_df_list <- lapply( file.path( input_dir, fin_csv_list ), read.csv, stringsAsFactors = F )
  checksum_df <- do.call( 'rbind', checksum_df_list )
  checksum_file_name <- gsub( '.nc', '.csv', nc_file_name, fixed = T )
  write.csv( checksum_df, checksum_file_name, row.names = F )
}


# singleVarChunking_extendedCH4air
# Brief: generate multi-year emissions chunks for aircraft emissions
# Dependencies:
# Author: Leyang Feng
# parameters: em
#             grid_resolution
#             chunk_start_years - a list of start years for each chunk
#             chunk_end_years - a list of start years for each chunk
#             chunk_count_index - the index of the chunk
#             input_dir
#             output_dir
# return: null
# input files: null
# output: [em]-em-AIR-anthro_input4MIPs_emissions_CMIP_CEDS-[CEDS_grid_version]_gn_[time_range].nc
singleVarChunking_extendedCH4air <- function( em,
                                              grid_resolution,
                                              chunk_start_years,
                                              chunk_end_years,
                                              chunk_density,
                                              chunk_count_index,
                                              input_dir,
                                              output_dir ) {

  year_1st <- chunk_start_years[ chunk_count_index ]
  year_last <- chunk_end_years[ chunk_count_index ]

  # get in grids file list for current chunking
  filename_patterns <- paste0( 'CEDS_', em, '_AIR_anthro_', seq( year_1st, year_last, chunk_density ), '_0.5.nc' )
  fin_grid_list <- unlist( lapply( filename_patterns, function( filename_pattern ) {
    list.files( input_dir, pattern = filename_pattern )
  } ) )

  # read in each nc file and extract variables
  # create empty arrays for storage
  AIR_array <- c( )
  time_array <- c( )
  time_bnds_array <- c( )
  # go through each nc files, extract the variables and append to storage array
  for ( fin_grid in fin_grid_list ) {
    # open the nc file
    nc_temp <- nc_open( file.path( input_dir, fin_grid ) )
    # extract the data for variables
    AIR_fin_block <- ncvar_get( nc_temp, 'AIR' )
    time_fin_block <- ncvar_get( nc_temp, 'time' )
    time_bnds_fin_block <- ncvar_get( nc_temp, 'time_bnds' )
    # close the nc file
    nc_close( nc_temp )
    # append the data to storage array
    AIR_array <- c( AIR_array, AIR_fin_block )
    time_array <- c( time_array, time_fin_block )
    time_bnds_array <- c( time_bnds_array, time_bnds_fin_block )
  }

  # extract values of global_total_emission attribute from first year and last year
  # value from first year
  fin_grid_1st <- fin_grid_list[ 1 ]
  nc_temp <- nc_open( file.path( input_dir, fin_grid_1st ) )
  MD_global_total_emission_1st_year <- ncatt_get( nc_temp, 0, 'global_total_emission' )[[ 2 ]]
  nc_close( nc_temp )
  # value from last year
  fin_grid_last <- fin_grid_list[ length( fin_grid_list ) ]
  nc_temp <- nc_open( file.path( input_dir, fin_grid_last ) )
  MD_global_total_emission_last_year <- ncatt_get( nc_temp, 0, 'global_total_emission' )[[ 2 ]]
  nc_close( nc_temp )

  # reshape the array
  # calculate how many years in the duration
  years_in_current_chunk <- ( chunk_end_years[ chunk_count_index ] - chunk_start_years[ chunk_count_index ] ) / chunk_density + 1
  # define dim for 3-D data
  array_dim <- c( dim( AIR_fin_block )[ 1 : 3 ], 12 * years_in_current_chunk ) # the dim for AGR_temp_data should always be the same in each nc file
  # time array has no need to be reshaped -- it's a one dimensional vector
  time_bnds_dim <- c( dim( time_bnds_fin_block )[ 1 ], 12 * years_in_current_chunk )
  # reshaping
  dim( AIR_array ) <- array_dim
  time_array <- time_array
  dim( time_bnds_array ) <- time_bnds_dim

  # generate nc file
  # the new nc generation routine begins here
  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
  levs <- seq( 0.305, 14.945, 0.61 )
  time <-time_array
  londim <- ncdim_def( "lon", "degrees_east", as.double( lons ), longname = 'longitude' )
  latdim <- ncdim_def( "lat", "degrees_north", as.double( lats ), longname = 'latitude' )
  levdim <- ncdim_def( "level", "km", as.double ( levs ), longname = 'altitude' )
  timedim <- ncdim_def( "time", paste0( "days since 1750-01-01 0:0:0" ), as.double( time ),
                        calendar = '365_day', longname = 'time' )
  dim_list <- list( londim, latdim, levdim, timedim )
  lon_bnds_data <- cbind( seq( -180, ( 180 - grid_resolution ), grid_resolution ),
                          seq( ( -180 + grid_resolution ), 180, grid_resolution ) )
  lat_bnds_data <- cbind( seq( -90, (90 - grid_resolution) , grid_resolution),
                          seq( ( -90 + grid_resolution ), 90, grid_resolution ) )
  bnds <- 1 : 2
  bndsdim <- ncdim_def( "bnds", '', as.integer( bnds ), longname = 'bounds', create_dimvar = F )
  time_bnds_data <- time_bnds_array

  # generate nc file name
  filename_version_tag <- paste0( 'CEDS-', GRIDDING_VERSION )
  MD_source_value <- paste0( filename_version_tag, ': Community Emissions Data System (CEDS) for Historical Emissions' )
  MD_source_id_value <- paste0( filename_version_tag, '-supplemental-data' )
  FN_source_id_value <- MD_source_id_value
  FN_variable_id_value <- paste0( em, '-em-AIR-anthro' )
  nc_file_name <- paste0( output_dir,
                          FN_variable_id_value,
                          '_input4MIPs_emissions_CMIP_',
                          FN_source_id_value,
                          '_gn_',
                          chunk_start_years[ chunk_count_index ], '01', '-', chunk_end_years[ chunk_count_index ], '12',
                          '.nc' )

  # generate flat_var variable name
  MD_variable_id_value <- paste0( em, '_em_AIR_anthro' )
  AIR_var_name <- MD_variable_id_value
  AIR_var_longname <- paste0( em, ' Aircraft Anthropogenic Emissions' )

  # define unit and missing value
  data_unit <- 'kg m-2 s-1'
  missing_value <- 1.e20
  MD_dataset_version_number_value <- GRIDDING_VERSION

  # define nc variables
  AIR_var <- ncvar_def( AIR_var_name, data_unit, dim_list, missval = missing_value, longname = AIR_var_longname, compression = 5 )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( bndsdim, timedim ), prec = 'double' )
  rm( filename_patterns, AIR_fin_block, time_fin_block, time_bnds_fin_block )
  gc( )

  # generate the var_list
  variable_list <- list( AIR_var, lat_bnds, lon_bnds, time_bnds )

  # create new nc file
  nc_new <- nc_create( nc_file_name, variable_list, force_v4 = T )

  # put nc variables into the nc file
  # There is a 2GB limit for writing (perhaps because of an older rCPP version
  # in ncdf4), so the values need to be written in chunks of time-steps
  write_chunk_size <- floor((2^31 - 1) / (prod(array_dim[1:3]) * 8))
  time_start_idx <- 1
  while ( time_start_idx < array_dim[4] ) {
      next_time_start <- min(time_start_idx + write_chunk_size, array_dim[4] + 1)
      time_steps <- next_time_start - time_start_idx
      ncvar_put( nc_new, AIR_var, AIR_array[ , , , time_start_idx:(next_time_start - 1) ],
                 start = c( 1, 1, 1, time_start_idx ),
                 count = c( -1, -1, -1, time_steps ) )
      time_start_idx <- next_time_start
  }

  ncvar_put( nc_new, lon_bnds, t( lon_bnds_data ) )
  ncvar_put( nc_new, lat_bnds, t( lat_bnds_data ) )
  ncvar_put( nc_new, time_bnds, time_bnds_data )

  title <- paste( 'Annual Anthropogenic Emissions of', em, 'prepared for input4MIPs' )
  add_global_attributes_single_var( nc_new, title, AIR_var_name, MD_dataset_version_number_value, MD_source_value,
                                    MD_source_id_value, MD_variable_id_value, air = TRUE, primary = FALSE )

  # write first/last year global total emission into metadata
  if ( year_1st != year_last ) {
    ncatt_put( nc_new, 0, paste0( 'global_total_emission_', year_1st ),  MD_global_total_emission_1st_year )
    ncatt_put( nc_new, 0, paste0( 'global_total_emission_', year_last ), MD_global_total_emission_last_year )
  } else {
    ncatt_put( nc_new, 0, paste0( 'global_total_emission_', year_1st ),  MD_global_total_emission_1st_year )
  }
  # some other metadata
  ncatt_put( nc_new, 0, 'data_usage_tips', 'Note that these are monthly average fluxes.' )
  reporting_info <- data.frame( em = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC', 'CO2', 'CH4' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass', 'Mass flux of CO2', 'Mass flux of CH4' ), stringsAsFactors = F )
  info_line <- reporting_info[ reporting_info$em == em, 'info' ]
  ncatt_put( nc_new, 0, 'reporting_unit', info_line )

  # close nc_new
  nc_close( nc_new )

  # additional checksum output
  fin_csv_list <- gsub( '.nc', '.csv', fin_grid_list, fixed = T )
  checksum_df_list <- lapply( file.path( input_dir, fin_csv_list ), read.csv, stringsAsFactors = F )
  checksum_df <- do.call( 'rbind', checksum_df_list )
  checksum_file_name <- gsub( '.nc', '.csv', nc_file_name, fixed = T )
  write.csv( checksum_df, checksum_file_name, row.names = F )
}

# =================== Diagnostic grids generation functions ====================

# generate_annual_total_emissions_grids_nc
# Brief: generate total emissions grids without seasonality
# Dependencies:
# Author: Leyang Feng
# parameters: output_dir
#             int_grids_list - the list contains intermediate grids
#             grid_resolution
#             year
#             em
# return: null
# input files: null
# output: CEDS_[em]_anthro_[year]_TOTAL_0.5_[CEDS_version].nc
#         CEDS_[em]_anthro_[year]_TOTAL_0.5_[CEDS_version].csv
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
  ncvar_put( nc_new, total_emission, rotate_a_matrix( total_grid_flux ) )
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

  add_global_attributes( nc_new, title = paste( 'Annual Emissions of ', em ) )

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


# generate_monthly_total_emissions_grids_nc
# Brief: generate total emissions grids with seasonality
# Dependencies:
# Author: Leyang Feng
# parameters: output_dir
#             int_grids_list - the list contains intermediate grids
#             grid_resolution
#             year
#             em
#             seasoanlity_mapping - seasonality mapping file
# return: null
# input files: null
# output: CEDS_[em]_anthro_[year]_TOTAL_monthly_[CEDS_version].nc
#         CEDS_[em]_anthro_[year]_TOTAL_monthly_[CEDS_version].csv
generate_monthly_total_emissions_grids_nc <- function( output_dir,
                                                      int_grids_list,
                                                      grid_resolution,
                                                      year,
                                                      em,
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
                           SLV_month_em, WST_month_em, SHP_month_em, RCO_month_em )
  total_month_em <- aggregate( total_month_em$value,
                               by = list( total_month_em$em, total_month_em$year,
                                          total_month_em$month, total_month_em$units ),
                               FUN = sum )

  total_grid_month <- AGR_fin_grid + ENE_fin_grid + IND_fin_grid + TRA_fin_grid +
    SLV_fin_grid + WST_fin_grid + SHP_fin_grid + RCO_fin_grid

  # generate the nc file

  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
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
  long_name <- 'global monthly total emissions '
  total_emission <- ncvar_def( 'total_emission', data_unit, dim_list, missval = missing_value, longname = long_name , prec = 'float', compression = 5 )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( bndsdim, timedim ), prec = 'double' )

  # generate nc file name
  nc_file_name <- paste0( output_dir, 'CEDS_', em, '_anthro_', year, '_', 'TOTAL_monthly', '_', grid_resolution, '_', version_stamp, '.nc' )

  # generate the var_list
  variable_list <- list( total_emission, lat_bnds, lon_bnds, time_bnds )

  # create new nc file
  nc_new <- nc_create( nc_file_name, variable_list, force_v4 = T )

  # put nc variables into the nc file
  ncvar_put( nc_new, total_emission, rotate_lat_lon( list( total_grid_month ) )[[1]] )
  ncvar_put( nc_new, lon_bnds, t( lon_bnds_data ) )
  ncvar_put( nc_new, lat_bnds, t( lat_bnds_data ) )
  ncvar_put( nc_new, time_bnds, t( time_bnds_data ) )

  add_dimension_attributes( nc_new )

  # attributes for variables
  ncatt_put( nc_new, total_emission, 'cell_methods', 'time: mean' )
  ncatt_put( nc_new, total_emission, 'missing_value', 1e+20, prec = 'float' )

  add_global_attributes( nc_new, title = paste( 'Annual Emissions of ', em ) )

  global_total_emission <- sum( total_month_em$value ) * 0.001
  ncatt_put( nc_new, 0, 'global_total_emission', paste0( round( global_total_emission, 2 ), ' Tg/year' ) )
  # species information
  reporting_info <- data.frame( em = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC', 'CO2', 'CH4' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass', 'Mass flux of CO2', 'Mass flux of CH4' ), stringsAsFactors = F )
  info_line <- reporting_info[ reporting_info$em == em, 'info' ]
  ncatt_put( nc_new, 0, 'reporting_unit', info_line )


  # close nc_new
  nc_close( nc_new)

  # additional section: write a summary and check text
  summary_name <- paste0( output_dir, 'CEDS_', em, '_anthro_', year, '_', 'TOTAL_monthly', '_', grid_resolution, '_', version_stamp, '.csv' )
  write.csv( total_month_em, file = summary_name, row.names = F )
}


# ============================== Helper Functions ==============================

# add_dimension_attributes
#
# Brief: Add standard attributes for NetCDF dimensions
# Author: Caleb Braun
# Params: nc_new - An object of class ncdf4
#         air    - TRUE if there should be an attribute for air levels
add_dimension_attributes <- function( nc_new, air = FALSE ) {
  ncatt_put( nc_new, "lon", "axis", "X" )
  ncatt_put( nc_new, "lon", "standard_name", "longitude" )
  ncatt_put( nc_new, "lon", "bounds", "lon_bnds" )
  ncatt_put( nc_new, "lat", "axis", "Y" )
  ncatt_put( nc_new, "lat", "standard_name", "latitude" )
  ncatt_put( nc_new, "lat", "bounds", "lat_bnds" )
  ncatt_put( nc_new, "time", "standard_name", "time" )
  ncatt_put( nc_new, "time", "axis", "T" )
  ncatt_put( nc_new, "time", "bounds", "time_bnds" )

  if ( air ) {
    ncatt_put( nc_new, "level", "axis", "Z" )
  }
}


# add_global_attributes
#
# Brief: Add standard global attributes for NetCDF
# Author: Caleb Braun
# Params: nc_new - An object of class ncdf4
#         title  - Value of the 'title' global attribute
add_global_attributes <- function( nc_new, title ) {
  # nc global attributes
  now <- as.POSIXlt( Sys.time(), "UTC")
  # ********************************************************************************
  # EDIT THE METADATA BELOW TO BE APPROPRIATE FOR YOUR PROJECT
  # Search for the word "your" and replace with appropriate text
  #
  # NOTE- This data must currently be edited in two separate places within this file
  # ********************************************************************************
  ncatt_put( nc_new, 0, 'title', title )
  ncatt_put( nc_new, 0, 'institution', 'your institution full name, city, state, zip, country' )
  ncatt_put( nc_new, 0, 'institution_id', 'your institution abbreviation' )
  ncatt_put( nc_new, 0, 'activity_id', 'NA' )
  ncatt_put( nc_new, 0, 'Conventions', 'CF-1.6' )
  ncatt_put( nc_new, 0, 'creation_date', as.character( format( now, format = '%Y-%m-%dT%H:%M:%SZ' ) ) )
  ncatt_put( nc_new, 0, 'data_structure', 'grid' )
  ncatt_put( nc_new, 0, 'frequency', 'mon' )
  ncatt_put( nc_new, 0, 'realm', 'atmos' )
  ncatt_put( nc_new, 0, 'source', paste0( 'CEDS ', as.character( format( now, format = '%m-%d-%Y' ) ),
                                          ': Community Emissions Data System (CEDS) for Historical Emissions' ) )
  ncatt_put( nc_new, 0, 'source_id', paste0( 'CEDS-', as.character( format( now, format = '%m-%d-%Y' ) ) ) )
  ncatt_put( nc_new, 0, 'further_info_url', 'your.web.site' )
  ncatt_put( nc_new, 0, 'history', paste0( as.character( format( now, format = '%d-%m-%Y %H:%M:%S %p %Z' ) ),
                                           '; your city, state, country') )
#  Specific metadata for CO2 emissions
#  ncatt_put( nc_new, 0, 'data_usage_tips', 'Note that these are monthly average fluxes of all oxidized fossil carbon. CO2 from solid and liquid biofuel combustion is not included.' )
  ncatt_put( nc_new, 0, 'data_usage_tips', 'Note that these are monthly average fluxes.' )
  ncatt_put( nc_new, 0, 'host', 'TBD' )
  ncatt_put( nc_new, 0, 'contact', 'Your Name Here (your-email@where.you.are)' )
  ncatt_put( nc_new, 0, 'references', 'Your journal paper or other reference for this work.' )
  ncatt_put( nc_new, 0, 'dataset_category', 'emissions' )
  ncatt_put( nc_new, 0, 'dataset_version_number', '1.0.0' )
  ncatt_put( nc_new, 0, 'grid_label', 'gr' )
  ncatt_put( nc_new, 0, 'grid_resolution', '50 km' )
  ncatt_put( nc_new, 0, 'grid', '0.5x0.5 degree latitude x longitude' )
  ncatt_put( nc_new, 0, 'mip_era', 'post-CMIP6' )
# Used for generating data for CMIP
# ncatt_put( nc_new, 0, 'target_mip', 'CMIP' )
}


# add_global_attributes_single_var
#
# Brief: Add standard global attributes for NetCDF single var chunking
# Author: Caleb Braun
# Params: nc_new                            An object of class ncdf4
#         title                             Value of the 'title' global attribute
#         flat_var_name                     Standardized NC variable name
#         MD_dataset_version_number_value   Dataset version number
#         MD_source_value                   Name of the data source
#         MD_source_id_value                ID of the data source
#         MD_variable_id_value              ID of the NC variable
#         air                               Is the data aircraft emissions?
#         primary                           Is this a primary data product?
add_global_attributes_single_var <- function( nc_new, title, flat_var_name, MD_dataset_version_number_value,
                                              MD_source_value, MD_source_id_value, MD_variable_id_value,
                                              air = FALSE, primary = TRUE ) {
  # nc variable attributes
  # attributes for dimensions
  ncatt_put( nc_new, "lon", "axis", "X" )
  ncatt_put( nc_new, "lon", "bounds", "lon_bnds" )
  ncatt_put( nc_new, "lon", "modulo", 360.0, prec = 'double' )
  ncatt_put( nc_new, "lon", "realtopology", "circular" )
  ncatt_put( nc_new, "lon", "standard_name", "longitude" )
  ncatt_put( nc_new, "lon", "topology", "circular" )
  ncatt_put( nc_new, "lat", "axis", "Y" )
  ncatt_put( nc_new, "lat", "bounds", "lat_bnds" )
  ncatt_put( nc_new, "lat", "realtopology", "linear" )
  ncatt_put( nc_new, "lat", "standard_name", "latitude" )
  ncatt_put( nc_new, "time", "axis", "T" )
  ncatt_put( nc_new, "time", "bounds", "time_bnds" )
  ncatt_put( nc_new, "time", "realtopology", "linear" )
  ncatt_put( nc_new, "time", "standard_name", "time" )
  if ( !air ) {
    ncatt_put( nc_new, "sector", "bounds", "sector_bnds" )
    ncatt_put( nc_new, "sector", "ids", "0: Agriculture; 1: Energy; 2: Industrial; 3: Transportation; 4: Residential, Commercial, Other; 5: Solvents production and application; 6: Waste; 7: International Shipping" )
  }
  # attributes for variables
  ncatt_put( nc_new, flat_var_name, 'cell_methods', 'time: mean' )
  #ncatt_put( nc_new, flat_var_name, 'long_name', flat_var_longname )
  ncatt_put( nc_new, flat_var_name, 'missing_value', 1e+20, prec = 'float' )
  # ********************************************************************************
  # EDIT THE METADATA BELOW TO BE APPROPRIATE FOR YOUR PROJECT
  # Search for the word "your" and replace with appropriate text
  #
  # NOTE- This data must currently be edited in two separate places
  # ********************************************************************************
  # nc global attributes
  ncatt_put( nc_new, 0, 'Conventions', 'CF-1.6' )
  ncatt_put( nc_new, 0, 'activity_id', 'NA' )
  ncatt_put( nc_new, 0, 'comment', 'Research data produced using CEDS.' )
  ncatt_put( nc_new, 0, 'contact', 'Your Name Here (your-email@where.you.are)' )
  ncatt_put( nc_new, 0, 'creation_date', as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%Y-%m-%dT%H:%M:%SZ' ) ) )
  ncatt_put( nc_new, 0, 'data_structure', 'grid' )
  ncatt_put( nc_new, 0, 'dataset_category', 'emissions' )
  ncatt_put( nc_new, 0, 'dataset_version_number', MD_dataset_version_number_value )
  ncatt_put( nc_new, 0, 'external_variables', 'gridcell_area' )
  ncatt_put( nc_new, 0, 'frequency', 'mon' )
  ncatt_put( nc_new, 0, 'further_info_url', 'your.web.site' )
  ncatt_put( nc_new, 0, 'grid', '0.5x0.5 degree latitude x longitude' )
  ncatt_put( nc_new, 0, 'grid_label', 'gn' )
  ncatt_put( nc_new, 0, 'nominal_resolution', '50 km' )
  ncatt_put( nc_new, 0, 'history', paste0( as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%d-%m-%Y %H:%M:%S %p %Z' ) ), '; your city, state, country') )
  ncatt_put( nc_new, 0, 'institution', 'your institution full name, city, state, zip, country' )
  ncatt_put( nc_new, 0, 'institution_id', 'your institution abbreviation' )
  ncatt_put( nc_new, 0, 'mip_era', 'post-CMIP6' )
  ncatt_put( nc_new, 0, 'product', ifelse( primary, 'primary-emissions-data', 'supplementary-emissions-data' ) )
  ncatt_put( nc_new, 0, 'realm', 'atmos' )
  ncatt_put( nc_new, 0, 'references', 'Your journal paper or other reference for this work.' )
  ncatt_put( nc_new, 0, 'source', MD_source_value )
  ncatt_put( nc_new, 0, 'source_id', MD_source_id_value )
# Used for generating data for CMIP
#  ncatt_put( nc_new, 0, 'table_id', 'input4MIPs' )
#  ncatt_put( nc_new, 0, 'target_mip', 'CMIP' )
  ncatt_put( nc_new, 0, 'title', title )
  ncatt_put( nc_new, 0, 'variable_id', MD_variable_id_value )
}


# chunk_emissions
#
# Brief: Call a chunking function for any emissions type
# Author: Caleb Braun
# Params: singleVarChunkingFun  A function (ex. singleVarChunking_bulkemissions)
#         em                    Emissions species (ex. 'SO2')
#         ...                   Further arguments to pass on to singleVarChunkingFun
chunk_emissions <- function(singleVarChunkingFun, em, ... ) {
    # setup dirs
    input_dir  <- filePath( 'MED_OUT', 'gridded-emissions/', extension = "" )
    output_dir <- filePath( 'FIN_OUT', 'gridded-emissions/', extension = "" )

    # Chunking variables
    grid_resolution <- 0.5
# If chunking years should be different than the full dataset specify that here
#    start_year <- 1750
#    end_year <- 1850
    chunk_years <- 50

    # basic start year/end year check
    if ( end_year < start_year ) { stop( 'End year must not be earlier than start year.') }

    # calculate chunk start years
    total_years <- end_year - start_year + 1
    chunk_count <- ceiling( total_years / chunk_years  )

    # calculate chunk end years
    chunk_start_years <- start_year + (1:chunk_count - 1) * chunk_years
    chunk_end_years <- chunk_start_years + ( chunk_years - 1 )

    if ( chunk_end_years[ chunk_count ] > end_year ) {
      chunk_end_years[ chunk_count ] <- end_year
    }

    # start chunking
    printLog( paste( 'Start', em, 'grids chunking from', start_year, 'to', end_year ) )

    for ( chunk_count_index in seq_len(chunk_count) ) {
      singleVarChunkingFun( em, grid_resolution, chunk_start_years, chunk_end_years,
                            chunk_count_index, input_dir, output_dir, ... )
    }
}
