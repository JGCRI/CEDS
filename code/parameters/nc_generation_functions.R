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
# final_monthly_nc_output
# Brief: generate a fliped matrix by a given matrix
# Dependencies: 
# Author: Leyang Feng
# parameters: x - the matrix to be fliped  
# return: a fliped matrix 
# input files: 
# output: 
final_monthly_nc_output <- function( output_dir, grid_resolution, year, em_species, sector_list, sector_list_long, mass = F ) {
  
  #debug
  #em_species <- em
  #sector_list <- level3_sector_list
  #sector_list_long <- level3_sector_longname_list
  
  
  # 0 set up some basics for later use
  global_grid_area <- grid_area( grid_resolution, all_lon = T )
  flux_factor <- 1000000 / global_grid_area / ( 365 * 24 * 60 * 60 )
  Days_in_Month <- c( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 )
  
  # process emission grids from low level sectors to high level sectors 
  # and adding seasonality ( except RCO )
  for ( i in seq_along( sectorl1_em_global_list ) ) {
    grid_name <- names( sectorl1_em_global_list ) [ i ]
    assign( grid_name, sectorl1_em_global_list[[ i ]] )
    }
  
  # first, aggregate to higher level
  AGR_em_global_final <- AGR_em_global
  ENE_em_global_final <- ELEC_em_global + FFFI_em_global + ETRN_em_global + FLR_em_global
  SLV_em_global_final <- SLV_em_global
  WST_em_global_final <- WST_em_global
  SHP_em_global_final <- SHP_em_global  
  IND_em_global_final <- INDC_em_global + INPU_em_global
  TRA_em_global_final <- NRTR_em_global + ROAD_em_global
  # second, add seasonality
  temp_sector_list <- sector_list[ !sector_list == 'RCO' ]
  
  checksum_sector_list <- c( )
  checksum_total_emission_list <- c( )
  for ( sector in temp_sector_list ) {
    seasonality <- get_seasonalityFrac( em_species, sector, year )
    temp_annual_data_name <- paste0( sector, '_em_global_final' )
    annual_data <- get( temp_annual_data_name )
    temp_array <- array( dim = dim( seasonality ) )
    checksum_total_emission_each_month_list <- c( )
    for ( i in 1 : dim( temp_array )[ 3 ] ) {
      temp_array[ , , i ] <- annual_data * seasonality[, , i ] * ( 365 / Days_in_Month[ i ] )
      checksum_total_emission <- sum( temp_array[ , , i ] / flux_factor / ( 365 / Days_in_Month[ i ] ), na.rm = T )
      checksum_total_emission_each_month_list <- c( checksum_total_emission_each_month_list, checksum_total_emission )
    }
    assign( temp_annual_data_name, temp_array )
    checksum_total_emission_list <- c( checksum_total_emission_list, checksum_total_emission_each_month_list )
    checksum_sector_list <- c( checksum_sector_list, rep( sector, 12 ) )
    }
  # special treatment for RCO 
  seasonality <- get_seasonalityFrac( em_species, 'RCO', year )
  temp_array <- array( dim = dim( seasonality ) )
  checksum_total_emission_each_month_list <- c( )
  for ( i in 1 : dim( temp_array )[ 3 ] ) {
    temp_array[ , , i ] <- RCORC_em_global * seasonality[, , i ] * ( 365 / Days_in_Month[ i ] ) + RCOO_em_global * RCOO_seasonality[ , , i ] * ( 365 / Days_in_Month[ i ] )
    checksum_total_emission <- sum( temp_array[ , , i ] / flux_factor / ( 365 / Days_in_Month[ i ] ), na.rm = T )
    checksum_total_emission_each_month_list <- c( checksum_total_emission_each_month_list, checksum_total_emission )
    
  }
  RCO_em_global_final <- temp_array
  checksum_total_emission_list <- c( checksum_total_emission_list, checksum_total_emission_each_month_list )
  checksum_sector_list <- c( checksum_sector_list, rep( 'RCO', 12 ) )
  
  # NetCDF generation starts here
  data_list <- paste0( sector_list, '_em_global_final')
  
  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
  #base_days <- as.numeric( strptime( paste0( year, '0101' ), format = '%Y%m%d', tz = 'UTC' ) - strptime( "17500101", format = "%Y%m%d", tz = 'UTC' ) )
  base_days <- ( as.numeric( year ) - 1750 ) * 365 
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
  
  if ( mass == T ){
    data_unit <- 'kt'
  } else {
    data_unit <- 'kg m-2 s-1'  
  }
  
  # define nc variables
  left_part <- sector_list 
  mid_part <- ' <- '
  right_part <- paste0( 'ncvar_def( \'', left_part, '\', \'', data_unit, '\', dim_list, missval = missing_value, longname= \'' ,sector_list_long, '\' , prec = \'float\', compression = 5 )' )
  expressions <- paste0( left_part, mid_part, right_part )
  missing_value <- 1.e20
  eval( parse( text = expressions ) )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( bndsdim, timedim ), prec = 'double' )
  
  # generate nc file name
  ceds_version <- 'v'
  date_parts <- unlist( strsplit( as.character( Sys.Date() ), split = '-' ) ) 
  ver_date <- paste( ceds_version, date_parts[ 2 ], date_parts[ 3 ], date_parts[ 1 ], sep = '_' )
  nc_file_name <- paste0( output_dir, 'CEDS_', em_species, '_anthro_', year, '_', grid_resolution, '_', ver_date, '.nc' ) 
  
  # generate the var_list 
  var_list_expression <- c()
  for ( part in left_part ) {
    var_list_expression <- paste0( var_list_expression, ', ', part )
    }
  var_list_expression <- paste0( 'variable_list <- list( ', substr( var_list_expression, 3, nchar( var_list_expression) ), ', lat_bnds, lon_bnds, time_bnds )' )
  eval( parse( text = var_list_expression ) ) 

  
  # create new nc file
  nc_new <- nc_create( nc_file_name, variable_list, force_v4 = T )
  
  # put nc variables into the nc file
  for ( i in seq_along( time ) ) {
  expressions <- paste0( 'ncvar_put( nc_new, ', left_part, ' ,t( flip_a_matrix( ', data_list, '[ , , i ]',
                         ' ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )' )
  eval( parse( text = expressions ) )
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
  for ( each_var in sector_list ) {
    ncatt_put( nc_new, each_var, 'cell_methods', 'time: mean' )
    ncatt_put( nc_new, each_var, 'missing_value', 1e+20, prec = 'float' )
  }
  # nc global attributes
  #ncatt_put( nc_new, 0, 'IMPORTANT', 'FOR TEST ONLY, DO NOT USE' )
  ncatt_put( nc_new, 0, 'title', paste0('Annual Emissions of ', em_species ) )
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

  global_total_emission <- sum( checksum_total_emission_list ) * 0.001
  ncatt_put( nc_new, 0, 'global_total_emission', paste0( round( global_total_emission, 2), ' Tg/year' ) )
  
  # species information 
  species_info <- data.frame( species = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass'), stringsAsFactors = F )
  info_line <- species_info[ species_info$species == em, ]$info 
  ncatt_put( nc_new, 0, 'reporting_unit', info_line )
  
  # close nc_new
  nc_close( nc_new)
    
  # additional section: write a summary and check text

  summary_table <- data.frame( year = year, species = em_species, 
                               sector = checksum_sector_list,
                               month = rep( 1 : 12, length( sector_list ) ),
                               global_total = checksum_total_emission_list,
                               unit = 'kt' )
  ceds_version <- 'v'
  date_parts <- unlist( strsplit( as.character( Sys.Date() ), split = '-' ) ) 
  ver_date <- paste( ceds_version, date_parts[ 2 ], date_parts[ 3 ], date_parts[ 1 ], sep = '_' )
  summary_name <- paste0( output_dir, 'CEDS_', em_species, '_anthro_', year, '_', grid_resolution, '_', ver_date, '.csv' )
  write.csv( summary_table, file = summary_name, row.names = F )
}
# -------------------------------------------------
# final_monthly_nc_output_air
# Brief: generate a fliped matrix by a given matrix
# Dependencies: 
# Author: Leyang Feng
# parameters: x - the matrix to be fliped  
# return: a fliped matrix 
# input files: 
# output: 
final_monthly_nc_output_air <- function( output_dir, grid_resolution, year, em_species, sector = 'AIR', sector_long = 'Aircraft', mass = F ) {
  
  # 0 set up some basics for later use
  global_grid_area <- grid_area( grid_resolution, all_lon = T )
  flux_factor <- 1000000 / global_grid_area / ( 365 * 24 * 60 * 60 )
  Days_in_Month <- c( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 )
  
  # process emission grid from low level sectors to high level sector
  # For AIR the low level is the same as high level   
  # first, aggregate to higher level
  AIR_em_global_final <- AIR_em_global
  
  # if 0 emission from AIR_em_global_final, do a shortcut 
  if ( sum( AIR_em_global_final ) == 0 ) { 
    AIR_em_global_final <- array( 0, dim = c( dim( AIR_em_global_final ), 12 ) ) 
    checksum_total_emission_list <- rep( 0, 12 )
  # if emission exists, do adding seasonality profile 
  } else { 
  # second, add seasonality 
  seasonality <- get_seasonalityFrac( em_species, sector, year )
  temp_array <- array( dim = dim( seasonality ) )
  checksum_total_emission_each_month_list <- c( )
  for ( i in 1 : dim( temp_array )[ 4 ] ) {
    temp_array[ , , , i ] <- AIR_em_global_final * seasonality[ , , , i ] * ( 365 / Days_in_Month[ i ] )
	
	array_per_month <- lapply( seq( dim( temp_array )[ 3 ] ), function( j ) {
	                           temp_array[ , , j, i ] / flux_factor / ( 365 / Days_in_Month[ i ] )
	                           } )
	array_per_month <- array( unlist( array_per_month ), dim = c( 180 / grid_resolution, 360 / grid_resolution, dim( temp_array )[ 3 ] ) )						   
    checksum_total_emission <- sum( array_per_month, na.rm = T )
    checksum_total_emission_each_month_list <- c( checksum_total_emission_each_month_list, checksum_total_emission )
  }
  AIR_em_global_final <- temp_array
  checksum_total_emission_list <- checksum_total_emission_each_month_list
  }

  # NetCDF generation starts here  
  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
  levs <- seq( 0.305, 14.945, 0.61 )
  #base_days <- as.numeric( strptime( paste0( year, '0101' ), format = '%Y%m%d', tz = 'UTC' ) - strptime( "17500101", format = "%Y%m%d", tz = 'UTC' ) )
  base_days <- ( as.numeric( year ) - 1750 ) * 365 
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
  
  if ( mass == T ){
    data_unit <- 'kt'
  } else {
    data_unit <- 'kg m-2 s-1'  
  }
  
  # define nc variables
  missing_value <- 1.e20
  AIR <- ncvar_def( sector, data_unit, dim_list, missval = missing_value, longname = sector_long, prec = 'float', compression = 5  )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( bndsdim, timedim ), prec = 'double' )
  
  # generate nc file name
  ceds_version <- 'v'
  date_parts <- unlist( strsplit( as.character( Sys.Date() ), split = '-' ) ) 
  ver_date <- paste( ceds_version, date_parts[ 2 ], date_parts[ 3 ], date_parts[ 1 ], sep = '_' )
  nc_file_name <- paste0( output_dir, 'CEDS_', em_species, '_', sector, '_anthro_', year, '_', grid_resolution, '_', ver_date, '.nc' ) 
  
  # generate the var_list 
  variable_list <- list( AIR, lat_bnds, lon_bnds, time_bnds )

  # create new nc file
  nc_new <- nc_create( nc_file_name, variable_list, force_v4 = T )
  
  # put nc variables into the nc file
  # transpose and flip the data first 
  dims <- dim( AIR_em_global_final )
  temp_data <- array( dim = c( dims[ 2 ], dims[ 1 ], dims[ 3 ], dims[ 4 ] ) )
  for ( i in 1 : dims[ 4 ] ) {
    for ( j in 1: dims[ 3 ] ) {
	  temp_data[ , , j, i ] <- t( flip_a_matrix( AIR_em_global_final[ , , j, i ] ) ) 
	}
  }
  # then put the data into nc 
  for ( i in seq_along( time ) ) {
    ncvar_put( nc_new, AIR, temp_data[ , , , i ], start = c( 1, 1, 1, i ), count = c( -1, -1, -1, 1 ) )
  }
  # for ( i in seq_along( time ) ) {
    # for (  j in seq_along( levs ) ) {
	  # ncvar_put( nc_new, AIR, temp_data[ , , j, i ], start = c( 1, 1, j, i ), count = c( -1, -1, -1, 1 ) )
	# }
  # }
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
  ncatt_put( nc_new, "lon", "axis", "Z" )
  # attributes for variables
  ncatt_put( nc_new, 'AIR', 'cell_methods', 'time: mean' )
  ncatt_put( nc_new, 'AIR', 'missing_value', 1e+20, prec = 'float' )
  # nc global attributes
  #ncatt_put( nc_new, 0, 'IMPORTANT', 'FOR TEST ONLY, DO NOT USE' )
  ncatt_put( nc_new, 0, 'title', paste0('Annual Emissions of ', em_species ) )
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

  global_total_emission <- sum( checksum_total_emission_list ) * 0.001
  ncatt_put( nc_new, 0, 'global_total_emission', paste0( round( global_total_emission, 2), ' Tg/year' ) )
  # species information 
  species_info <- data.frame( species = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass'), stringsAsFactors = F )
  info_line <- species_info[ species_info$species == em, ]$info 
  ncatt_put( nc_new, 0, 'reporting_unit', info_line )
  
    
  # close nc_new
  nc_close( nc_new)
  
  # additional section: write a summary and check text

  summary_table <- data.frame( year = year, species = em_species, 
                               sector = sector,
                               month = rep( 1 : 12 ),
                               global_total = checksum_total_emission_list,
                               unit = 'kt' )
  ceds_version <- 'v'
  date_parts <- unlist( strsplit( as.character( Sys.Date() ), split = '-' ) ) 
  ver_date <- paste( ceds_version, date_parts[ 2 ], date_parts[ 3 ], date_parts[ 1 ], sep = '_' )
  summary_name <- paste0( output_dir, 'CEDS_', em_species, '_', sector, '_anthro_', year, '_', grid_resolution, '_', ver_date, '.csv' )
  write.csv( summary_table, file = summary_name, row.names = F )
}
# -------------------------------------------------
# final_monthly_nc_output_subVOCs
# Brief: generate a fliped matrix by a given matrix
# Dependencies: 
# Author: Leyang Feng
# parameters: x - the matrix to be fliped  
# return: a fliped matrix 
# input files: 
# output: 
final_monthly_nc_output_subVOCs <- function( output_dir, grid_resolution, year, em_species, sector_list, sector_list_longname, VOC_list, VOC_name_list, mass = F ) {
  
  # 0 set up some basics for later use
  global_grid_area <- grid_area( grid_resolution, all_lon = T )
  flux_factor <- 1000000 / global_grid_area / ( 365 * 24 * 60 * 60 )
  Days_in_Month <- c( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 )
  
  # each VOC generates a netcdf file so loop on VOCs
  for ( VOC in VOC_list ) {
  
    # process emission grids from low level sectors to high level sectors 
    # and adding seasonality ( except RCO )
    # first, aggregate to higher level
    exp <- paste0( 'AGR_', VOC, '_em_global_final <- AGR_', VOC, '_em_global' )
	  eval( parse( text = exp ) )
	  exp <- paste0( 'ENE_', VOC, '_em_global_final <- ELEC_', VOC, '_em_global + FFFI_', VOC, '_em_global + ETRN_', VOC, '_em_global + FLR_', VOC, '_em_global' )
	  eval( parse( text = exp ) )
    exp <- paste0( 'IND_', VOC, '_em_global_final <- INDC_', VOC, '_em_global + INPU_', VOC, '_em_global' )
	  eval( parse( text = exp ) )     
	  exp <- paste0( 'TRA_', VOC, '_em_global_final <- NRTR_', VOC, '_em_global + ROAD_', VOC, '_em_global' )
	  eval( parse( text = exp ) ) 
	  exp <- paste0( 'SLV_', VOC, '_em_global_final <- SLV_', VOC, '_em_global' )
	  eval( parse( text = exp ) )
	  exp <- paste0( 'WST_', VOC, '_em_global_final <- WST_', VOC, '_em_global' )
	  eval( parse( text = exp ) )
	  exp <- paste0( 'SHP_', VOC, '_em_global_final <- SHP_', VOC, '_em_global' )
	  eval( parse( text = exp ) )

	  # second, add seasonality
    temp_sector_list <- sector_list[ !sector_list == 'RCO' ]

    checksum_sector_list <- c( )
    checksum_total_emission_list <- c( )
    
    for ( sector in temp_sector_list ) {
    seasonality <- get_seasonalityFrac( em_species, sector, year )
    temp_annual_data_name <- paste0( sector, '_', VOC, '_em_global_final' )
    annual_data <- get( temp_annual_data_name )
    temp_array <- array( dim = dim( seasonality ) )
    checksum_total_emission_each_month_list <- c( )
    for ( i in 1 : dim( temp_array )[ 3 ] ) {
      temp_array[ , , i ] <- annual_data * seasonality[, , i ] * ( 365 / Days_in_Month[ i ] )
      checksum_total_emission <- sum( temp_array[ , , i ] / flux_factor / ( 365 / Days_in_Month[ i ] ), na.rm = T )
      checksum_total_emission_each_month_list <- c( checksum_total_emission_each_month_list, checksum_total_emission )
    }
    assign( temp_annual_data_name, temp_array )
    checksum_total_emission_list <- c( checksum_total_emission_list, checksum_total_emission_each_month_list )
    checksum_sector_list <- c( checksum_sector_list, rep( sector, 12 ) )
    }
	  
	  # special treatment for RCO 
    seasonality <- get_seasonalityFrac( em_species, 'RCO', year )
    temp_array <- array( dim = dim( seasonality ) )
    checksum_total_emission_each_month_list <- c( )
    
    RCORC_VOC_varname <- paste0( 'RCORC_', VOC, '_em_global' )
    RCOO_VOC_varname <- paste0( 'RCOO_', VOC, '_em_global' )
    for ( i in 1 : dim( seasonality )[ 3 ] ) {
      temp_array[ , , i] <- get( RCORC_VOC_varname ) * seasonality[, , i ] * ( 365 / Days_in_Month[ i ] )
                            + get( RCOO_VOC_varname ) * RCOO_seasonality[ , , i ] * ( 365 / Days_in_Month[ i ] )
      checksum_total_emission <- sum( temp_array[ , , i ] / flux_factor / ( 365 / Days_in_Month[ i ] ), na.rm = T )
      checksum_total_emission_each_month_list <- c( checksum_total_emission_each_month_list, checksum_total_emission )
      }
    RCO_VOC_varname <- paste0( 'RCO_', VOC, '_em_global_final')
    assign( RCO_VOC_varname, temp_array )
    checksum_total_emission_list <- c( checksum_total_emission_list, checksum_total_emission_each_month_list )
    checksum_sector_list <- c( checksum_sector_list, rep( 'RCO', 12 ) )
	  
    # start nc write routine
  	data_list <- paste0( sector_list, '_', VOC, '_em_global_final')
	  
  	
	  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
    lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
    #base_days <- as.numeric( strptime( paste0( year, '0101' ), format = '%Y%m%d', tz = 'UTC' ) - strptime( "17500101", format = "%Y%m%d", tz = 'UTC' ) )
    base_days <- ( as.numeric( year ) - 1750 ) * 365 
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
	
  if ( mass == T ){
    data_unit <- 'kt'
  } else {
    data_unit <- 'kg m-2 s-1'  
  }
  
  # define nc variables
  left_part <- sector_list 
  mid_part <- ' <- '
  right_part <- paste0( 'ncvar_def( \'', left_part, '\', \'', data_unit, '\', dim_list, missval = missing_value, longname= \'' ,sector_list_longname, '\' , prec = \'float\', compression = 5 )' )
  expressions <- paste0( left_part, mid_part, right_part )
  missing_value <- 1.e20
  eval( parse( text = expressions ) )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( bndsdim, timedim ), prec = 'double' )
  
  # generate nc file name
  ceds_version <- 'v'
  date_parts <- unlist( strsplit( as.character( Sys.Date() ), split = '-' ) ) 
  ver_date <- paste( ceds_version, date_parts[ 2 ], date_parts[ 3 ], date_parts[ 1 ], sep = '_' )
  VOC_species_name <- VOC_names$VOC_name[ which( VOC_names$VOC_id %in% VOC ) ] # retrieve the VOC real name 
  VOC_species_name <- substring( VOC_species_name, 1, 10 ) # cutoff at 10 characters
  nc_file_name <- paste0( output_dir, 'CEDS_', VOC, '-', VOC_species_name, 
                          '_anthro_', year, '_', grid_resolution, '_', ver_date, '.nc' ) 
  
  # generate the var_list 
  var_list_expression <- c()
  for ( part in left_part ) {
    var_list_expression <- paste0( var_list_expression, ', ', part )
    }
  var_list_expression <- paste0( 'variable_list <- list( ', substr( var_list_expression, 3, nchar( var_list_expression) ), ', lat_bnds, lon_bnds, time_bnds )' )
  eval( parse( text = var_list_expression ) ) 
  
  # create new nc file
  nc_new <- nc_create( nc_file_name, variable_list, force_v4 = T )
  
  # put nc variables into the nc file
  for ( i in seq_along( time ) ) {
  expressions <- paste0( 'ncvar_put( nc_new, ', left_part, ' ,t( flip_a_matrix( ', data_list, '[ , , i ]',
                         ' ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )' )
  eval( parse( text = expressions ) )
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
  for ( each_var in sector_list ) {
    ncatt_put( nc_new, each_var, 'cell_methods', 'time: mean' )
    ncatt_put( nc_new, each_var, 'missing_value', 1e+20, prec = 'float' )
  }
  # nc global attributes
  #ncatt_put( nc_new, 0, 'IMPORTANT', 'FOR TEST ONLY, DO NOT USE' )
  ncatt_put( nc_new, 0, 'title', paste0('Annual Emissions of ', VOC, ' - ', 
                                        VOC_names$VOC_name[ which( VOC_names$VOC_id %in% VOC ) ] ) )
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
  ncatt_put( nc_new, 0, 'host', 'TBD' )
  ncatt_put( nc_new, 0, 'contact', 'ssmith@pnnl.gov' )
  ncatt_put( nc_new, 0, 'references', 'http://www.geosci-model-dev.net/special_issue590.html' )
  ncatt_put( nc_new, 0, 'molecular weight', VOC_names$molecular.weight[ which( VOC_names$VOC_id %in% VOC ) ], prec = 'float' )
  ncatt_put( nc_new, 0, 'molecular weight unit', 'g mole-1' )
  ncatt_put( nc_new, 0, 'dataset_category', 'emissions' )
  ncatt_put( nc_new, 0, 'dataset_version_number', '1.0.0' )
  ncatt_put( nc_new, 0, 'grid_label', 'gr' )
  ncatt_put( nc_new, 0, 'grid_resolution', '50 km' )
  ncatt_put( nc_new, 0, 'grid', '0.5x0.5 degree latitude x longitude' )
  ncatt_put( nc_new, 0, 'mip_era', 'CMIP6' )
  ncatt_put( nc_new, 0, 'target_mip', 'CMIP' )

  global_total_emission <- sum( checksum_total_emission_list ) * 0.001
  ncatt_put( nc_new, 0, 'global_total_emission', paste0( round( global_total_emission, 2), ' Tg/year' ) )
  
  # close nc_new
  nc_close( nc_new)
  
  # additional section: write a summary and check text
  summary_table <- data.frame( year = year, species = em_species, 
                               sector = checksum_sector_list,
                               month = rep( 1 : 12, length( sector_list ) ),
                               global_total = checksum_total_emission_list,
                               unit = 'kt' )
  summary_name <- paste0( output_dir, 'CEDS_', VOC, '-', VOC_species_name, '_anthro_', year, '_', grid_resolution, '_', ver_date, '.csv' )
  write.csv( summary_table, file = summary_name, row.names = F )

  }
}
# -------------------------------------------------
# annual_total_emission_nc_output
# Brief: generate a fliped matrix by a given matrix
# Dependencies: 
# Author: Leyang Feng
# parameters: x - the matrix to be fliped  
# return: a fliped matrix 
# input files: 
# output: 
annual_total_emission_nc_output <- function( output_dir, grid_resolution, year, em_species, mass = F ) {
  
  # 0 set up some basics for later use
  global_grid_area <- grid_area( grid_resolution, all_lon = T )
  flux_factor <- 1000000 / global_grid_area / ( 365 * 24 * 60 * 60 )
    
  # 1 adding all sector's grid together so generate total emission grid 
  
  # convert flux back to mass 
  total_grid <- lapply( sectorl1_em_global_list, '/', flux_factor )
  total_grid <- Reduce( '+', total_grid )
  
  total_sum_in_kt <- sum( total_grid )
  
  if ( mass == F ) {
    total_grid <- total_grid * flux_factor 
  } else { total_grid <- total_grid }
  
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

  if ( mass == T ){
    data_unit <- 'kt'
  } else {
    data_unit <- 'kg m-2 s-1'  
  }
  
  # define nc variables
  missing_value <- 1.e20
  long_name <- 'Global total emissions '
  total_emission <- ncvar_def( 'total_emission', data_unit, dim_list, missval = missing_value, longname = long_name , prec = 'float', compression = 5 )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
  
  # generate nc file name
  ceds_version <- 'v'
  date_parts <- unlist( strsplit( as.character( Sys.Date() ), split = '-' ) ) 
  ver_date <- paste( ceds_version, date_parts[ 2 ], date_parts[ 3 ], date_parts[ 1 ], sep = '_' )
  nc_file_name <- paste0( output_dir, 'CEDS_', em_species, '_anthro_', year, '_', 'TOTAL', '_', grid_resolution, '_', ver_date, '.nc' ) 
  
  # generate the var_list 
  variable_list <- list( total_emission, lat_bnds, lon_bnds ) 

  # create new nc file
  nc_new <- nc_create( nc_file_name, variable_list, force_v4 = T )
  
  # put nc variables into the nc file
  ncvar_put( nc_new, total_emission, t( flip_a_matrix( total_grid ) ) )
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
  ncatt_put( nc_new, 0, 'title', paste0('Annual Emissions of ', em_species ) )
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
  ncatt_put( nc_new, 0, 'global_total_emission', paste0( round( global_total_emission, 2), ' Tg/year' ) )
  # species information 
  species_info <- data.frame( species = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass'), stringsAsFactors = F )
  info_line <- species_info[ species_info$species == em, ]$info 
  ncatt_put( nc_new, 0, 'reporting_unit', info_line )
  
    
  # close nc_new
  nc_close( nc_new)
 
  # additional section: write a summary and check text

  summary_table <- data.frame( year = year, species = em_species,  
                               global_total = total_sum_in_kt,
                               unit = 'kt' )
  ceds_version <- 'v'
  date_parts <- unlist( strsplit( as.character( Sys.Date() ), split = '-' ) ) 
  ver_date <- paste( ceds_version, date_parts[ 2 ], date_parts[ 3 ], date_parts[ 1 ], sep = '_' )
  summary_name <- paste0( output_dir, 'CEDS_', em_species, '_anthro_', year, '_', 'TOTAL', '_', grid_resolution, '_', ver_date, '.csv' )
  write.csv( summary_table, file = summary_name, row.names = F )
}
# -------------------------------------------------
# final_monthly_nc_output_biomass
# Brief: generate a fliped matrix by a given matrix
# Dependencies: 
# Author: Leyang Feng
# parameters: x - the matrix to be fliped  
# return: a fliped matrix 
# input files: 
# output: 
final_monthly_nc_output_biomass <- function( output_dir, grid_resolution, year, em_species, sector_list, sector_list_long, mass = F ) {
  
  # 0 set up some basics for later use
  global_grid_area <- grid_area( grid_resolution, all_lon = T )
  flux_factor <- 1000000 / global_grid_area / ( 365 * 24 * 60 * 60 )
  Days_in_Month <- c( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 )
  
  # process emission grids from low level sectors to high level sectors 
  # and adding seasonality ( except RCO )
  for ( i in seq_along( sectorl1_em_global_list ) ) {
    grid_name <- names( sectorl1_em_global_list ) [ i ]
    assign( grid_name, sectorl1_em_global_list[[ i ]] )
    }
  
  # first, aggregate to higher level
  # level1 sector FLR should be added to ENE_em_global_final but actually doesn't exist for fuel type biomass
  ENE_em_global_final <- ELEC_em_global
  SHP_em_global_final <- SHP_em_global  
  IND_em_global_final <- INDC_em_global
  TRA_em_global_final <- NRTR_em_global + ROAD_em_global
    # belwo three sector does not exist in biomass emissions 
  AGR_em_global_final <- matrix( 0, 180 / grid_resolution, 360 / grid_resolution )
  SLV_em_global_final <- matrix( 0, 180 / grid_resolution, 360 / grid_resolution )
  WST_em_global_final <- matrix( 0, 180 / grid_resolution, 360 / grid_resolution )
  # second, add seasonality
  temp_sector_list <- sector_list[ !sector_list == 'RCO' ]
  
  checksum_sector_list <- c( )
  checksum_total_emission_list <- c( )
  for ( sector in temp_sector_list ) {
    seasonality <- get_seasonalityFrac( em_species, sector, year )
    temp_annual_data_name <- paste0( sector, '_em_global_final' )
    annual_data <- get( temp_annual_data_name )
    temp_array <- array( dim = dim( seasonality ) )
    checksum_total_emission_each_month_list <- c( )
    for ( i in 1 : dim( temp_array )[ 3 ] ) {
      temp_array[ , , i ] <- annual_data * seasonality[, , i ] * ( 365 / Days_in_Month[ i ] )
      checksum_total_emission <- sum( temp_array[ , , i ] / flux_factor / ( 365 / Days_in_Month[ i ] ), na.rm = T )
      checksum_total_emission_each_month_list <- c( checksum_total_emission_each_month_list, checksum_total_emission )
    }
    assign( temp_annual_data_name, temp_array )
    checksum_total_emission_list <- c( checksum_total_emission_list, checksum_total_emission_each_month_list )
    checksum_sector_list <- c( checksum_sector_list, rep( sector, 12 ) )
    }
  # special treatment for RCO 
  seasonality <- get_seasonalityFrac( em_species, 'RCO', year )
  temp_array <- array( dim = dim( seasonality ) )
  checksum_total_emission_each_month_list <- c( )
  for ( i in 1 : dim( temp_array )[ 3 ] ) {
    temp_array[ , , i ] <- RCORC_em_global * seasonality[, , i ] * ( 365 / Days_in_Month[ i ] ) + RCOO_em_global * RCOO_seasonality[ , , i ] * ( 365 / Days_in_Month[ i ] )
    checksum_total_emission <- sum( temp_array[ , , i ] / flux_factor / ( 365 / Days_in_Month[ i ] ), na.rm = T )
    checksum_total_emission_each_month_list <- c( checksum_total_emission_each_month_list, checksum_total_emission )
    
  }
  RCO_em_global_final <- temp_array
  checksum_total_emission_list <- c( checksum_total_emission_list, checksum_total_emission_each_month_list )
  checksum_sector_list <- c( checksum_sector_list, rep( 'RCO', 12 ) )
  
############################################################################################################
    
  data_list <- paste0( sector_list, '_em_global_final')
  
  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
  #base_days <- as.numeric( strptime( paste0( year, '0101' ), format = '%Y%m%d', tz = 'UTC' ) - strptime( "17500101", format = "%Y%m%d", tz = 'UTC' ) )
  base_days <- ( as.numeric( year ) - 1750 ) * 365 
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
  
  if ( mass == T ){
    data_unit <- 'kt'
  } else {
    data_unit <- 'kg m-2 s-1'  
  }
  
  # define nc variables
  left_part <- sector_list 
  mid_part <- ' <- '
  right_part <- paste0( 'ncvar_def( \'', left_part, '\', \'', data_unit, '\', dim_list, missval = missing_value, longname= \'' ,sector_list_long, '\' , prec = \'float\', compression = 5 )' )
  expressions <- paste0( left_part, mid_part, right_part )
  missing_value <- 1.e20
  eval( parse( text = expressions ) )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( bndsdim, timedim ), prec = 'double' )
  
  # generate nc file name
  ceds_version <- 'v'
  date_parts <- unlist( strsplit( as.character( Sys.Date() ), split = '-' ) ) 
  ver_date <- paste( ceds_version, date_parts[ 2 ], date_parts[ 3 ], date_parts[ 1 ], sep = '_' )
  nc_file_name <- paste0( output_dir, 'CEDS_', em_species, '_SOLID_BIOFUEL_anthro_', year, '_', grid_resolution, '_', ver_date, '.nc' ) 
  
  # generate the var_list 
  var_list_expression <- c()
  for ( part in left_part ) {
    var_list_expression <- paste0( var_list_expression, ', ', part )
    }
  var_list_expression <- paste0( 'variable_list <- list( ', substr( var_list_expression, 3, nchar( var_list_expression) ), ', lat_bnds, lon_bnds, time_bnds )' )
  eval( parse( text = var_list_expression ) ) 

  
  # create new nc file
  nc_new <- nc_create( nc_file_name, variable_list, force_v4 = T )
  
  # put nc variables into the nc file
  for ( i in seq_along( time ) ) {
  expressions <- paste0( 'ncvar_put( nc_new, ', left_part, ' ,t( flip_a_matrix( ', data_list, '[ , , i ]',
                         ' ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )' )
  eval( parse( text = expressions ) )
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
  for ( each_var in sector_list ) {
    ncatt_put( nc_new, each_var, 'cell_methods', 'time: mean' )
    ncatt_put( nc_new, each_var, 'missing_value', 1e+20, prec = 'float' )
  }
  # nc global attributes
  #ncatt_put( nc_new, 0, 'IMPORTANT', 'FOR TEST ONLY, DO NOT USE' )
  ncatt_put( nc_new, 0, 'title', paste0('Annual Emissions of ', em_species ) )
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
  ncatt_put( nc_new, 0, 'host', 'TBD' )
  ncatt_put( nc_new, 0, 'contact', 'ssmith@pnnl.gov' )
  ncatt_put( nc_new, 0, 'references', 'http://www.geosci-model-dev.net/special_issue590.html' )
  ncatt_put( nc_new, 0, 'note', 'These are a subset of anthro emissions and SHOULD NOT BE ADDED to the anthro emissions data. These are supplemental data only.' )
  ncatt_put( nc_new, 0, 'dataset_category', 'emissions' )
  ncatt_put( nc_new, 0, 'dataset_version_number', '1.0.0' )
  ncatt_put( nc_new, 0, 'grid_label', 'gr' )
  ncatt_put( nc_new, 0, 'grid_resolution', '50 km' )
  ncatt_put( nc_new, 0, 'grid', '0.5x0.5 degree latitude x longitude' )
  ncatt_put( nc_new, 0, 'mip_era', 'CMIP6' )
  ncatt_put( nc_new, 0, 'target_mip', 'CMIP' )
  
  global_total_emission <- sum( checksum_total_emission_list ) * 0.001
  ncatt_put( nc_new, 0, 'global_total_emission', paste0( round( global_total_emission, 2), ' Tg/year' ) )
  
  # species information 
  species_info <- data.frame( species = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass'), stringsAsFactors = F )
  info_line <- species_info[ species_info$species == em, ]$info 
  ncatt_put( nc_new, 0, 'reporting_unit', info_line )
  
  # close nc_new
  nc_close( nc_new)

#########################################################################################################################
    
  # additional section: write a summary and check text

  summary_table <- data.frame( year = year, species = em_species, 
                               sector = checksum_sector_list,
                               month = rep( 1 : 12, length( sector_list ) ),
                               global_total = checksum_total_emission_list,
                               unit = 'kt' )
  ceds_version <- 'v'
  date_parts <- unlist( strsplit( as.character( Sys.Date() ), split = '-' ) ) 
  ver_date <- paste( ceds_version, date_parts[ 2 ], date_parts[ 3 ], date_parts[ 1 ], sep = '_' )
  summary_name <- paste0( output_dir, 'CEDS_', em_species, '_SOLID_BIOFUEL_anthro_', year, '_', grid_resolution, '_', ver_date, '.csv' )
  write.csv( summary_table, file = summary_name, row.names = F )
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
  ver_date <- paste0( paste0( 'v', CEDS_version, '-sectorDim' ) )
  ver_date_parts <- unlist( strsplit( CEDS_version, split = '-') )
  source_date <- paste0( ver_date_parts[2], '-',ver_date_parts[3], '-', ver_date_parts[1] ) 
  source_value <- paste0( 'CEDS ', source_date, ': Community Emissions Data System (CEDS) for Historical Emissions' )
  source_id_value <- paste0( 'CEDS-', source_date, '-sectorDim' )
  nc_file_name <- paste0( output_dir, em, '-em-', gsub( '_', '-', gridtype ),  
                          '_input4MIPs_emissions_CMIP_CEDS-', ver_date,
                          '_gr', '_',  
                          chunk_start_years[ chunk_count_index ], '01', '-', 
                          chunk_end_years[ chunk_count_index ], '12','.nc' ) 
  
  # generate flat_var variable name 
  flat_var_name <- paste0( em, '-em-', gsub( '_', '-', gridtype ) )
  flat_var_longname <- flat_var_name
  variable_id_value <- flat_var_name
  if ( gridtype == 'SOLID_BIOFUEL_anthro' ) {
    nc_file_name <- paste0( output_dir, em, '-em-', gsub( '_', '-', gridtype ), 
                            '_input4MIPs_emissions_CMIP_CEDS-', ver_date, '-supplemental-data',
                            '_gr', '_',  
                            chunk_start_years[ chunk_count_index ], '01', '-', 
                            chunk_end_years[ chunk_count_index ], '12','.nc' ) 
	flat_var_name <- paste0( em, '-em-', gsub( '_', '-', gridtype ) )
    flat_var_longname <- flat_var_name
	variable_id_value <- flat_var_name
	}                        
  
  if ( VOC_chunk == T ) { 
    nc_file_name <- paste0( output_dir, gsub( '_', '-', gridtype ), '-em-', 'speciated-VOC', 
                              '_input4MIPs_emissions_CMIP_CEDS-', ver_date, '-supplemental-data',
                              '_gr', '_',  
                              chunk_start_years[ chunk_count_index ], '01', '-', 
                              chunk_end_years[ chunk_count_index ], '12','.nc' ) 
	flat_var_name <- paste0( gsub( '_', '-', gridtype ), '-em-', 'speciated-VOC' )
    flat_var_longname <- flat_var_name  
	variable_id_value <- flat_var_name
  }  
  
  # define unit and missing value 
  data_unit <- 'kg m-2 s-1'  
  missing_value <- 1.e20
  dataset_version_number_value <- paste0( paste0( CEDS_version, '-sectorDim' ) )
  
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
  ncatt_put( nc_new, 0, 'contact', 'ssmith@pnnl.gov' )
  ncatt_put( nc_new, 0, 'references', 'http://www.geosci-model-dev.net/special_issue590.html' )
  ncatt_put( nc_new, 0, 'dataset_category', 'emissions' )
  ncatt_put( nc_new, 0, 'dataset_version_number', dataset_version_number_value )
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
    ncatt_put( nc_new, 0, 'variable_id', variable_id_value )
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
# annual2chunk_AIR
# Brief: generate a fliped matrix by a given matrix
# Dependencies: 
# Author: Leyang Feng
# parameters: x - the matrix to be fliped  
# return: a fliped matrix 
# input files: 
# output: 
annual2chunk_AIR <- function( em, grid_resolution, gridtype = 'AIR_anthro', chunk_start_years, chunk_end_years, chunk_count_index, input_dir, output_dir, CEDS_version = 'YYYY-MM-DD' ) {
  
  # create filename pattern using grid type 
  filename_patterns <- paste0( '_', em, '_', gridtype, '_', ( chunk_start_years[ chunk_count_index ] : chunk_end_years[ chunk_count_index ] ), '.*nc' )
  
  # generate file list in the input dir 
  nc_file_list <- c( )
  for ( filename_pattern in filename_patterns ) { 
    matched_filename <- list.files( input_dir, pattern = filename_pattern )
    nc_file_list <- c( nc_file_list, matched_filename )
  }
  
  # read in each nc file and extract variables 
  # create ampty arries for storage
  AIR_array <- c( )
  time_array <- c( )
  time_bnds_array <- c( )
  # go through each nc files, extract the variables and append to storage array 
  for ( nc_file in nc_file_list ) {
    # open the nc file 
    nc_temp <- nc_open( paste0( input_dir, '/', nc_file ) ) 
    # extract the data for variables 
    AIR_temp_data <- ncvar_get( nc_temp, 'AIR' )
    time_temp_data <- ncvar_get( nc_temp, 'time' )
    time_bnds_temp_data <- ncvar_get( nc_temp, 'time_bnds' )
    # close the nc file 
    nc_close( nc_temp )
    # append the data to storage array 
    AIR_array <- c( AIR_array, AIR_temp_data )
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
  data_dim <- c( dim( AIR_temp_data )[ 1 : 3 ], 12 * years_in_current_chunk ) # the dim for AGR_temp_data should always be the same in each nc file 
  # time array has no need to be reshaped -- it's a one dimensional vector 
  time_bnds_dim <- c( dim( time_bnds_temp_data )[ 1 ], 12 * years_in_current_chunk )
  # reshaping
  dim( AIR_array ) <- data_dim 
  time_array <- time_array
  dim( time_bnds_array ) <- time_bnds_dim 
  
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
  
  # define unit and missing value 
  data_unit <- 'kg m-2 s-1'  
  missing_value <- 1.e20
  var_name_list <- c( 'AIR' )
  
  # define nc variables
  AIR <- ncvar_def( 'AIR', 'kg m-2 s-1', dim_list, missval = missing_value, longname= 'Aircraft' , prec = 'float', compression = 5 )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( bndsdim, londim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( bndsdim, latdim ), prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( bndsdim, timedim ), prec = 'double' )
  
  # generate nc file name
  ver_date <- paste0( paste0( 'v', CEDS_version ) )
  ver_date_parts <- unlist( strsplit( CEDS_version, split = '-') )
  source_date <- paste0( ver_date_parts[2], '-',ver_date_parts[3], '-', ver_date_parts[1] ) 
  source_value <- paste0( 'CEDS ', source_date, ': Community Emissions Data System (CEDS) for Historical Emissions' )
  source_id_value <- paste0( 'CEDS-', source_date )
  variable_id_value <- paste0( em, '-em-', gsub( '_', '-', gridtype ) )
  nc_file_name <- paste0( output_dir, em, '-em-', gsub( '_', '-', gridtype ), 
                          '_input4MIPs_emissions_CMIP_CEDS-', ver_date,
                          '_gr', '_',  
                          chunk_start_years[ chunk_count_index ], '01', '-', 
                          chunk_end_years[ chunk_count_index ], '12','.nc' ) 
  
  # generate the var_list 
  variable_list <- list( AIR, lat_bnds, lon_bnds, time_bnds )
  
  # create new nc file
  nc_new <- nc_create( nc_file_name, variable_list, force_v4 = T )
  
  # put nc variables into the nc file
  for ( i in seq_along( time ) ) {
    ncvar_put( nc_new, AIR, AIR_array[ , , , i ], start = c( 1, 1, 1, i ), count = c( -1, -1, -1, 1 ) )
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
  ncatt_put( nc_new, 0, 'title', paste0( 'Annual Aircraft Emissions of ', em ) )
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
  
  ncatt_put( nc_new, 0, 'product', 'primary-emissions-data' )
  ncatt_put( nc_new, 0, 'variable_id', variable_id_value )
  species_info <- data.frame( species = c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3', 'BC', 'OC' ), info = c( 'Mass flux of SOx, reported as SO2', 'Mass flux of NOx, reported as NO2', 'Mass flux of CO', 'Mass flux of NMVOC (total mass emitted)', 'Mass flux of NH3', 'Mass flux of BC, reported as carbon mass', 'Mass flux of OC, reported as carbon mass'), stringsAsFactors = F )
  info_line <- species_info[ species_info$species == em, ]$info 
  ncatt_put( nc_new, 0, 'reporting_unit', info_line )
  
  # close nc_new
  nc_close( nc_new )
  
  # additional routine for generating checksum file
  global_grid_area <- grid_area( grid_resolution, all_lon = T )
  flux_factor <- 1000000 / global_grid_area / ( 365 * 24 * 60 * 60 )
  flux_factor <- t( flip_a_matrix( flux_factor ) )
  Days_in_Month <- c( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 )

  total_em_list <- unlist( lapply( 1 : dim( AIR_array )[ 4 ], function( i ) {
                     temp_block <- AIR_array[ , , , i ]
                     flux_convertor <- flux_factor * ( 365 / Days_in_Month[ ifelse( i %% 12 == 0, 12, i %% 12 ) ] )
    
                     block_sum <- sum( unlist( lapply( 1 : dim( temp_block )[ 3 ], function( j ){ 
                                    temp_slice <- temp_block[ , ,j ]
                                    temp_slice <- temp_slice / flux_convertor
                                    slice_sum <- sum( temp_slice, na.rm = T )
                                    return( slice_sum )
                                    } ) ) )

                     return( block_sum )
                     } ) )
  
  checksum_sector_list <- rep( 'AIR', dim( AIR_array )[ 4 ]  )
  checksum_month_list <- rep( 1 : 12, ( dim( AIR_array )[ 4 ] / 12 ) )
  checksum_year_list  <- unlist( lapply( chunk_start_years[ chunk_count_index ] : chunk_end_years[ chunk_count_index ], function( year ) { years <- rep( year, 12 ) } ) ) 
  checksum_unit_list <- rep( 'kt', length( checksum_sector_list ) ) 
  checksum_em_list <- rep( em, length( checksum_sector_list ) )
  layout <- data.frame( year = checksum_year_list, em = checksum_em_list, sector = checksum_sector_list, month = checksum_month_list, global_total = total_em_list, units = checksum_unit_list, stringsAsFactors = F )

  summary_name <- paste0( substr( nc_file_name, 1, ( nchar( nc_file_name) - 3 ) ), '.csv' )
  write.csv( layout, file = summary_name, row.names = F )
}