#------------------------------------------------------------------------------
# Program Name: G.gridding_functions.R
# Author's Name: Leyang Feng
# Date Last Modified: 18 January 2016
# Program Purpose: Core functions for module-G
# Note: 1. Functions in the script can be only used within module-G
#       2. Some R packages used in this script needs other libraries built in 
#          your operation system, contact Leyang Feng for any assistance
#       3. ncdf4 package is not available through CRAN, contact Leyang Feng
#          for assistance about this package 
# TODO: 1. make the checking function out of G.emiDistributing( iso )
#       2. fix hard-coded stuff within writeNC_globalExt function 
# 
# 
# ------------------------------------------------------------------------------

# Special Packages
loadPackage( 'ncdf4' ) 
loadPackage( 'sp' )
loadPackage( 'geosphere' )
# ------------------------------------------------------------------------------
# grid_one_country
# Brief: Generate a country's emission spatial distribution using proxy
# Dependencies: 
# Author: Leyang Feng
# parameters: iso - the three digits iso country code for the country 
#                   desired for emission spatial distribution     
# return: null 
# input files: country_location_index, emission, [iso]_mask
# output: [iso]_emis_spatial 

grid_one_country <- function( country, location_index, em_data, proxy, year, sector, em_species ) {
  
  # retrieve matrix indexes for iso for later proxy cropping
  row_col_index <- location_index[ location_index$iso == country, ]
  start_row <- row_col_index$start_row
  end_row <- row_col_index$end_row
  start_col <- row_col_index$start_col
  end_col <- row_col_index$end_col
      
  # retrieve the iso_mask from memory 
  mask_name <- paste0( country, '_mask')
  mask <- get( mask_name )
      
  # extract the proxy as the extent of the iso_mask
  proxy_masked <- proxy[ start_row : end_row, start_col : end_col ]
  
  # retrieve the iso's emission for later distributing 
  emission_value <- em_data[ em_data$iso == country, 2 ]
  
  # get boundary weighted proxy 
  weighted_proxy <- proxy_masked * mask
  
  #### Make this section into a function
  if ( sum( weighted_proxy ) == 0 & emission_value != 0 ) {
    message_line <- paste0( 'All zero proxy pattern with non-zero emission: ', country, ' ,', year, ' ,', sector, ' ,', em_species )
    summary_dir <- filePath( "MED_OUT", "", extension="")
    cat( message_line, file = paste0( summary_dir, 'Proxy_Emission_check.txt' ), append = TRUE, sep="\n" )
    }
  ####
  
  # normalize the weighted_proxy 
  norm_weighted_proxy <- weighted_proxy / sum( weighted_proxy )
  norm_weighted_proxy[ is.nan( norm_weighted_proxy ) ] <- 0    
  # distibute the iso's emission to it's spatial proxy 
  em_spatial <- emission_value * norm_weighted_proxy 
  
  # save the emis_spatial as iso_emis_spatial into memory
  em_name <- paste0( country, '_em_spatial' )
  assign( em_name, em_spatial, .GlobalEnv )
}

# ------------------------------------------------------------------------------
# aggregare_all_countries
# Brief: Aggregate each country's emission spatial distribution to global level
# Dependencies: 
# Author: Leyang Feng
# parameters: iso_list - a list of country iso code. For all countries in the list, 
#                        their emission spatial distibutions are desired to be aggregated
#                        to global level.  
#             mass = T - if TURE, the output will be in unit of 'kt'. If FALSE, 
#                        the unit of output will be converted to 'kg m-2 s-1'
# return: Global emission spatial distribution  
# input files: [iso]_emis_spatial
# output : [sector]_emis_global

aggregate_all_countries <- function( sector, country_list, location_index, grid_resolution, mass = T ) {
  # create a empty template grid for later aggregating
  grid_template <- matrix( 0, 180 / grid_resolution, 360 / grid_resolution ) 
  
  # aggregating
  for ( country in country_list ) {   # use for loop for efficient aggregating
    
    # retrieve matrix indexes for iso for later locating
    row_col_index <- location_index[ location_index$iso == country, ]
    start_row <- row_col_index$start_row
    end_row <- row_col_index$end_row
    start_col <- row_col_index$start_col
    end_col <- row_col_index$end_col
    
    # retrieve the iso_emis_spatial from memory 
    em_spatial_name <- paste0( country, '_em_spatial')
    em_spatial <- get( em_spatial_name )
    
    # add the emis_spatial to empty template grid
    grid_template[ start_row : end_row , start_col : end_col ] <- 
      grid_template[ start_row : end_row , start_col :end_col ] + em_spatial
  }
  
  # convert the unit from mass to flux if desired
  if ( mass != T ) {
    # generate the global_grid_area matrix for later calculation 
    global_grid_area <- grid_area( grid_resolution, all_lon = T )
    flux_factor <- 1000000 / global_grid_area / ( 365 * 24 * 60 * 60 )
    grid_template <- grid_template * flux_factor
  }
  
  return( grid_template )
}

# ------------------------------------------------------------------------------
# year_length
# Brief: Decide how many days in a given year
# Dependencies: 
# Author: Leyang Feng
# parameters: year   
# return: 365 or 366 
# input files: 
# output:  

year_length <- function( year ) {
  year <- as.numeric( year ) 
  ifelse( ( year %% 4 == 0 & year %% 100 != 0) | year %% 400 == 0, 366, 365 ) 
  }

# ------------------------------------------------------------------------------
# grid_area
# Brief: Compute areas of a column of grid cells for all latitude at 
#        desired resolution in square meters   
# Dependencies: 
# Author: Leyang Feng
# parameters:  grid_resolution - the resolution of desired grid cell
#              all_lon - by default the function only return a column of areas since 
#                        the grid cell areas will be only changing along latitude. 
#                        if TRUE, the function retures a grid cell area as a matrix 
#                        as global extent                
# return: grid cell areas 
# input files: 
# output: 

grid_area <- function( grid_resolution, all_lon = F ) {
  
  # generate a GridTopology object contains a column of grids for -90 to 90 in latitude
  offset <- c( 0, -90 + grid_resolution / 2 )
  gridres <- c( grid_resolution, grid_resolution )
  crnum <- c( 1, 180 / grid_resolution )
  grid_topo <- GridTopology( offset, gridres, crnum )
  
  # convert the GridTopology object into a Spatial object then calculate the area in square meters 
  polygon_topo <- as( grid_topo, 'SpatialPolygons' )
  grid_cell_area <- areaPolygon( polygon_topo ) 
  
  # generate a world grid area matrix if all_lon = T
  if ( all_lon == T) {
    rep_times <- 360 / grid_resolution 
    grid_cell_area <- rep( grid_cell_area, times = rep_times )
    dim( grid_cell_area ) <- c( 180 / grid_resolution, 360 / grid_resolution )
  }
  
  return( grid_cell_area )
}

# ------------------------------------------------------------------------------
# writeNC_globalExt 
# Brief: Write all sectors' emission into one netCDF file for one year one gas' data
# Dependencies: flip_a_matrix()
# Author: Leyang Feng
# parameters: output_dir - the folder to store all netCDF files 
#             year, gas, sector - infomation wused for generating the filename
#             emis_naming_pattern - the naming pattern used when generating each sector's 
#                                   emission in G.emiAggregate()
#             mass = T - decide the unite for each variable in the netCDF file 
# return:  
# input files: 
# output: CEDS_SO2_anthro_[year]_[resolution]_[version]_[month_day_year].nc 

writeNC_globalExt <- function( output_dir, grid_resolution, year, em_species, mass = F ) {
  
  # list all matrices to be written into the nc file 
  em_naming_pattern = '_em_global'
  data_list <- ls( pattern = em_naming_pattern, .GlobalEnv )
      
  # define nc dimensions
  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
  londim <- ncdim_def( "lon", "degrees_east", as.double( lons ), longname = 'longitude' )
  latdim <- ncdim_def( "lat", "degrees_north", as.double( lats ), longname = 'latitude' )
  dim_list <- list( londim, latdim )
      
  # define the data unit
  if ( mass == T ){
    data_unit <- 'kt'
  } else {
    data_unit <- 'kg m-2 s-1'  
  }
  
  # define nc variables
  left_part <- paste0( 'emis_', sector_list )
  mid_part <- ' <- '
  right_part <- paste0( 'ncvar_def( \'', left_part, '\', \'', data_unit, '\', dim_list, longname= \'' ,sector_longname_list, '\' , prec = \'float\', compression = 5 )' )
  expressions <- paste0( left_part, mid_part, right_part )
  eval( parse( text = expressions ) )
  
  # generate nc file name
  version <- 'v1'
  date_parts <- unlist( strsplit( as.character( Sys.Date() ), split = '-' ) ) 
  ver_date <- paste( version, date_parts[ 2 ], date_parts[ 3 ], date_parts[ 1 ], sep = '_' )
  nc_file_name <- paste0( output_dir, 'CEDS_', em_species, '_anthro_', year, '_', grid_resolution, '_', ver_date, '.nc' )
  
  # generate the var_list 
  var_list_expression <- c()
  for ( part in left_part ) {
    var_list_expression <- paste0( var_list_expression, ', ', part )
    }
  var_list_expression <- paste0( 'variable_list <- list( ', substr( var_list_expression, 3, nchar( var_list_expression) ), ' )' )
  eval( parse( text = var_list_expression ) ) 
      
  # create new nc file
  nc_new <- nc_create( nc_file_name, variable_list, force_v4 = T )
      
  # put nc variables into the nc file
  expressions <- paste0( 'ncvar_put( nc_new, ', left_part, ' ,t( flip_a_matrix(', data_list, ') ) )' )
  
  eval( parse( text = expressions ) )
      
  # nc variable attributes 
  #ncatt_put(ncout,"lon","comment","center_of_cell")
  #ncatt_put(ncout,"lat","comment","center_of_cell")
      
  # nc global attributes
  ncatt_put( nc_new, 0, 'IMPORTANT','FOR TEST ONLY, DO NOT USE' )
  ncatt_put( nc_new, 0, 'Title','Annual Emissions of SO2' )
  ncatt_put( nc_new, 0, 'Institution','PNNL - JGCRI' )
  ncatt_put( nc_new, 0, 'Contact and websit','http://www.globalchange.umd.edu/ceds/' )
      
  # close nc_new
  nc_close( nc_new)

  # additional section: write a summary and check text
  global_grid_area <- grid_area( grid_resolution, all_lon = T )
  global_total <- c()
  em_global_sectors <- c()
  for ( em_global_data in data_list ) {
    em_global_sector <- unlist( strsplit( em_global_data, split = '_' ) ) [ 1 ]
    eval( parse( text = paste0( 'temp_data <- ', em_global_data ) ) )
    temp_data <- temp_data * global_grid_area
    total <- sum( temp_data )
    global_total <- c( global_total, total )
    em_global_sectors <- c( em_global_sectors, em_global_sector)
    }
  summary_table <- data.frame( year = year, species = em_species, 
                               sector = em_global_sectors, 
                               global_total = global_total,
                               unit = 'kg s-1' )
  summary_name <- paste0( output_dir, 'CEDS_', em_species, '_anthro_', year, '_', grid_resolution, '_', ver_date, '.csv' )
  write.csv( summary_table, file = summary_name, row.names = F )
  }

# ------------------------------------------------------------------------------
# flip_a_matrix 
# Brief: generate a fliped matrix by a given matrix
# Dependencies: 
# Author: Leyang Feng
# parameters: x - the matrix to be fliped  
# return: a fliped matrix 
# input files: 
# output: 
flip_a_matrix <- function( x ) {
  apply( x, 2, rev )
}
# ------------------------------------------------------------------------------
# mask_avail_check 
# Brief: generate a fliped matrix by a given matrix
# Dependencies: 
# Author: Leyang Feng
# parameters: x - the matrix to be fliped  
# return: a fliped matrix 
# input files: 
# output: 
mask_avail_check <- function( emission_country_list, mask_country_list ){
  emission_country_list <- unique( emission_country_list)
  if ( FALSE %in% ( emission_country_list %in% mask_country_list ) == TRUE ) {
    country_drop_list <- emission_country_list[ which( !emission_country_list %in% mask_country_list )]
    return( country_drop_list )
	message( paste0( 'masks not available for: ', country_drop_list ) )
  } 
}
# ------------------------------------------------------------------------------
# get_proxy
# Brief: generate a fliped matrix by a given matrix
# Dependencies: filePath
# Author: Leyang Feng
# parameters: x - the matrix to be fliped  
# return: a fliped matrix 
# input files: 
# output: 
get_proxy <- function( em_species, year, sector ) {
  if ( as.numeric( year ) > 2008 ) {
    year <- as.character( 2008 )
  }
  sector <- tolower( sector )
  proxy_dir <- filePath( "GRIDDING", "", extension="", domain_extension = "proxy/")
  proxy_filename <- paste( em_species, year, sector, sep = '_')
  load( paste0( proxy_dir,'/',proxy_filename ) )
  proxy <- get( proxy_filename )
  rm( list = proxy_filename )
  return( proxy )
}
# ------------------------------------------------------------------------------
# grid_one_sector
# Brief: generate a fliped matrix by a given matrix
# Dependencies: 
# Author: Leyang Feng
# parameters: x - the matrix to be fliped  
# return: a fliped matrix 
# input files: 
# output: 
grid_one_sector <- function( sector, em_species, year, location_index, grid_resolution, em_data, mass ) {
  ###########Place holder for SHP #####################
  if ( sector == 'SHP' ) { 
    proxy <- matrix( 0, 180 / grid_resolution, 360 / grid_resolution )
  } else {
      proxy <- get_proxy( em_species, year, sector )
    }
  ############################################
  
  #########uncomment the following one line after the SHP got resolved
  #proxy <- get_proxy( em_species, year, sector )
  # extract emission for specific year and gas
  emission_year_sector <- subset( em_data, em_data$CEDS_grd_sector == sector,
                              c( 'iso', paste0( 'X', year ) ) )
  # if no mask available for certain country, generate a country drop list 
  country_drop_list <- mask_avail_check( emission_year_sector$iso, location_index$iso )
  # drop country if necessary 
  for ( country in country_drop_list ) {
	emission_year_sector <- emission_year_sector[ emission_year_sector$iso != country, ]
	}
  # extract current country_list from emission_year_sector
  country_list <- emission_year_sector$iso 
  # calling the G.emiDistribute function to do the scalling
  invisible( lapply( country_list, grid_one_country, location_index, emission_year_sector, proxy, year, sector, em_species ) )
  
  # aggregating
  output_name <- paste0( sector, '_em_global' )
  expressions <- paste0( 'temp_emission <<- aggregate_all_countries( ',sector, ', country_list, location_index, grid_resolution, mass = ', mass, ' )' )
  eval( parse( text = expressions ) )
  assign( output_name, temp_emission, .GlobalEnv )
  #left_part <- paste0( sector, 'em_global' )
  #mid_part <- ' <<- '
  #right_part <- paste0( 'aggregate_all_countries( ',sector, ', country_list, location_index, grid_resolution, mass = ', mass, ' )' )
  #expressions <- paste0( left_part, mid_part, right_part )
  #eval( parse( text = expressions ) )
}
# ------------------------------------------------------------------------------
# grid_one_sector
# Brief: generate a fliped matrix by a given matrix
# Dependencies: 
# Author: Leyang Feng
# parameters: x - the matrix to be fliped  
# return: a fliped matrix 
# input files: 
# output: 
grid_one_year <- function( em_species, year, em_data, location_index, sector_list, grid_resolution, mass = F ) { 
  X_year <<- paste0( 'X', year )
  emission_year <<- em_data[ c( 'iso', 'CEDS_grd_sector', X_year ) ]
  
  invisible( lapply( sector_list, grid_one_sector, em_species, year, location_index, grid_resolution, emission_year, mass ) )

}
# ------------------------------------------------------------------------------
# gridding_initialize
# Brief: generate a fliped matrix by a given matrix
# Dependencies: 
# Author: Leyang Feng
# parameters: x - the matrix to be fliped  
# return: a fliped matrix 
# input files: 
# output: 
gridding_initialize <- function( grid_resolution = 0.5,
                                 start_year = 1970,
                                 end_year = 2014,
                                 load_masks = T
                                 ){
  grid_resolution <<- 0.5
  message( paste0( 'Processing resolution: ', grid_resolution ) )
  start_year <<- 1970
  end_year <<- 2014
  year_list <<- as.character( seq( start_year, end_year ) )
  message( paste0( 'Gridding from year ', start_year, ' to year ', end_year ))
  
  # load country masks 
  mask_dir <- filePath( "GRIDDING", "", extension="", domain_extension = "mask/")
  mask_list <- list.files( mask_dir )
  invisible( lapply( mask_list, function( mask_list ) { load( paste0( mask_dir, mask_list), .GlobalEnv ) } ) )
  country_mask_initialized = T
  message( 'country_mask_initialized: ', country_mask_initialized )
  
  }
# -------------------------------------------------
# monthly_nc_output
# Brief: generate a fliped matrix by a given matrix
# Dependencies: 
# Author: Leyang Feng
# parameters: x - the matrix to be fliped  
# return: a fliped matrix 
# input files: 
# output: 
monthly_nc_output <- function( output_dir, grid_resolution, year, em_species, sector_list, mass = F ) {
  
  data_list <- paste0( sector_list, '_em_global')
      
  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
  time <- 1:12
  #lon_bounds <- cbind( seq( -180, 180 - grid_resolution, grid_resolution ), seq( -180 + grid_resolution, 180, grid_resolution ) )
  #lat_bounds <- cbind( seq( -90, 90 - grid_resolution, grid_resolution ), seq( -90 + grid_resolution, 90, grid_resolution ) )
  londim <- ncdim_def( "lon", "degrees_east", as.double( lons ), longname = 'longitude' )
  latdim <- ncdim_def( "lat", "degrees_north", as.double( lats ), longname = 'latitude' )
  timedim <- ncdim_def( "time", "days since 1750-01-01 0:0:0", as.double( time ), 
                        calendar = '365_days', longname = 'time' )
  #lon_bnds <- ncdim_def( "lon_bnds", " ", as.double( lon_bounds ), longname = 'bounds_longitude', create_dimvar = TRUE )
  #lat_bnds <- ncdim_def( "lat_bnds", " ", as.double( lat_bounds ), longname = 'bounds_latitude', create_dimvar = TRUE )
  dim_list <- list( londim, latdim, timedim )
  
  if ( mass == T ){
    data_unit <- 'kt'
  } else {
    data_unit <- 'kg m-2 s-1'  
  }
  
  # define nc variables
  left_part <- sector_list 
  mid_part <- ' <- '
  right_part <- paste0( 'ncvar_def( \'', left_part, '\', \'', data_unit, '\', dim_list, missval = missing_value, longname= \'' ,sector_longname_list, '\' , prec = \'float\', compression = 5, chunksizes = NA )' )
  expressions <- paste0( left_part, mid_part, right_part )
  missing_value <- 1.e20
  eval( parse( text = expressions ) )
  
  # generate nc file name
  version <- 'v1'
  date_parts <- unlist( strsplit( as.character( Sys.Date() ), split = '-' ) ) 
  ver_date <- paste( version, date_parts[ 2 ], date_parts[ 3 ], date_parts[ 1 ], sep = '_' )
  nc_file_name <- paste0( output_dir, 'CEDS_', em_species, '_anthro_', year, '_', grid_resolution, '_', ver_date, '.nc' ) 
  
  # generate the var_list 
  var_list_expression <- c()
  for ( part in left_part ) {
    var_list_expression <- paste0( var_list_expression, ', ', part )
    }
  var_list_expression <- paste0( 'variable_list <- list( ', substr( var_list_expression, 3, nchar( var_list_expression) ), ' )' )
  eval( parse( text = var_list_expression ) ) 

  # create new nc file
  nc_new <- nc_create( nc_file_name, variable_list, force_v4 = T )
  
  # put nc variables into the nc file
  for ( i in seq_along(time) ) {
  expressions <- paste0( 'ncvar_put( nc_new, ', left_part, ' ,t( flip_a_matrix( ', data_list,
                         ' / 12 ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )' )
  eval( parse( text = expressions ) )
  }
  
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
  # nc global attributes
  ncatt_put( nc_new, 0, 'IMPORTANT', 'FOR TEST ONLY, DO NOT USE' )
  ncatt_put( nc_new, 0, 'title', paste0('Annual Emissions of ', em_species ) )
  ncatt_put( nc_new, 0, 'institution_id', 'PNNL-JGCRI' )
  ncatt_put( nc_new, 0, 'institution', 'Pacific Northwest National Laboratory - JGCRI' )
  ncatt_put( nc_new, 0, 'activity_id', 'input4MIPs' )
  ncatt_put( nc_new, 0, 'Conventions', 'CF-1.6' )
  ncatt_put( nc_new, 0, 'creation_date', as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%Y-%m-%dT%H:%M:%SZ' ) ) )
  ncatt_put( nc_new, 0, 'data_structure', 'grid' )
  ncatt_put( nc_new, 0, 'frequency', 'mon' )
  ncatt_put( nc_new, 0, 'realm', 'atmos' )
  ncatt_put( nc_new, 0, 'source', 'CEDS 12-27-16: Community Emissions Data System (CEDS) for Historical Emissions' )
  ncatt_put( nc_new, 0, 'source_id', 'CEDS-12-27-16' )
  ncatt_put( nc_new, 0, 'further_info_url', 'http://www.globalchange.umd.edu/ceds/' )
  ncatt_put( nc_new, 0, 'license', 'FOR TESTING AND REVIEW ONLY' )
  ncatt_put( nc_new, 0, 'history', 
             paste0( as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%d-%m-%Y %H:%M:%S %p %Z' ) ),
                     '; College Park, MD, USA') )
  ncatt_put( nc_new, 0, 'comment', 'Test Dataset for CMIP6' )
  ncatt_put( nc_new, 0, 'data_usage_tips', 
             'These monthly average fluxes can be linearly interpolated to join fluxes of adjacent months. (This should be done in a manner that the monthly average given here is preserved.)' )
  ncatt_put( nc_new, 0, 'host', 'TBD' )
  ncatt_put( nc_new, 0, 'contact', 'ssmith@pnnl.gov' )
  ncatt_put( nc_new, 0, 'references', 'http://www.geosci-model-dev.net/special_issue590.html' )
  # attributes for variables
  for ( each_var in sector_list ) {
    ncatt_put( nc_new, each_var, 'cell_methods', 'time:mean' )
    ncatt_put( nc_new, each_var, 'missing_value', 1e+20, prec = 'float' )
  }
  
  # close nc_new
  nc_close( nc_new)
  
  # additional section: write a summary and check text
  global_grid_area <- grid_area( grid_resolution, all_lon = T )
  global_total <- c()
  em_global_sectors <- c()
  for ( em_global_data in data_list ) {
    em_global_sector <- unlist( strsplit( em_global_data, split = '_' ) ) [ 1 ]
    eval( parse( text = paste0( 'temp_data <- ', em_global_data ) ) )
    temp_data <- temp_data * global_grid_area
    total <- sum( temp_data )
    global_total <- c( global_total, total )
    em_global_sectors <- c( em_global_sectors, em_global_sector)
    }
  summary_table <- data.frame( year = year, species = em_species, 
                               sector = em_global_sectors, 
                               global_total = global_total,
                               unit = 'kg s-1' )
  summary_name <- paste0( output_dir, 'CEDS_', em_species, '_anthro_', year, '_', grid_resolution, '_', ver_date, '.csv' )
  write.csv( summary_table, file = summary_name, row.names = F )
}
# -------------------------------------------------
# monthly_nc_output
# Brief: generate a fliped matrix by a given matrix
# Dependencies: 
# Author: Leyang Feng
# parameters: x - the matrix to be fliped  
# return: a fliped matrix 
# input files: 
# output: 
final_monthly_nc_output <- function( output_dir, grid_resolution, year, em_species, sector_list, mass = F ) {
  
  data_list <- paste0( sector_list, '_em_global')
  
      
  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
  time <- 1:12
  #lon_bounds <- cbind( seq( -180, 180 - grid_resolution, grid_resolution ), seq( -180 + grid_resolution, 180, grid_resolution ) )
  #lat_bounds <- cbind( seq( -90, 90 - grid_resolution, grid_resolution ), seq( -90 + grid_resolution, 90, grid_resolution ) )
  londim <- ncdim_def( "lon", "degrees_east", as.double( lons ), longname = 'longitude' )
  latdim <- ncdim_def( "lat", "degrees_north", as.double( lats ), longname = 'latitude' )
  timedim <- ncdim_def( "time", "days since 1750-01-01 0:0:0", as.double( time ), 
                        calendar = '365_days', longname = 'time' )
  #lon_bnds <- ncdim_def( "lon_bnds", " ", as.double( lon_bounds ), longname = 'bounds_longitude', create_dimvar = TRUE )
  #lat_bnds <- ncdim_def( "lat_bnds", " ", as.double( lat_bounds ), longname = 'bounds_latitude', create_dimvar = TRUE )
  dim_list <- list( londim, latdim, timedim )
  
  if ( mass == T ){
    data_unit <- 'kt'
  } else {
    data_unit <- 'kg m-2 s-1'  
  }
  
  # define nc variables
  left_part <- sector_list 
  mid_part <- ' <- '
  right_part <- paste0( 'ncvar_def( \'', left_part, '\', \'', data_unit, '\', dim_list, missval = missing_value, longname= \'' ,sector_longname_list, '\' , prec = \'float\', compression = 5, chunksizes = NA )' )
  expressions <- paste0( left_part, mid_part, right_part )
  missing_value <- 1.e20
  eval( parse( text = expressions ) )
  
  # generate nc file name
  version <- 'v1'
  date_parts <- unlist( strsplit( as.character( Sys.Date() ), split = '-' ) ) 
  ver_date <- paste( version, date_parts[ 2 ], date_parts[ 3 ], date_parts[ 1 ], sep = '_' )
  nc_file_name <- paste0( output_dir, 'CEDS_', em_species, '_anthro_', year, '_', grid_resolution, '_', ver_date, '.nc' ) 
  
  # generate the var_list 
  var_list_expression <- c()
  for ( part in left_part ) {
    var_list_expression <- paste0( var_list_expression, ', ', part )
    }
  var_list_expression <- paste0( 'variable_list <- list( ', substr( var_list_expression, 3, nchar( var_list_expression) ), ' )' )
  eval( parse( text = var_list_expression ) ) 

  # create new nc file
  nc_new <- nc_create( nc_file_name, variable_list, force_v4 = T )
  
  # put nc variables into the nc file
  for ( i in seq_along(time) ) {
  expressions <- paste0( 'ncvar_put( nc_new, ', left_part, ' ,t( flip_a_matrix( ', data_list,
                         ' / 12 ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )' )
  eval( parse( text = expressions ) )
  }
  
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
  # nc global attributes
  ncatt_put( nc_new, 0, 'IMPORTANT', 'FOR TEST ONLY, DO NOT USE' )
  ncatt_put( nc_new, 0, 'title', paste0('Annual Emissions of ', em_species ) )
  ncatt_put( nc_new, 0, 'institution_id', 'PNNL-JGCRI' )
  ncatt_put( nc_new, 0, 'institution', 'Pacific Northwest National Laboratory - JGCRI' )
  ncatt_put( nc_new, 0, 'activity_id', 'input4MIPs' )
  ncatt_put( nc_new, 0, 'Conventions', 'CF-1.6' )
  ncatt_put( nc_new, 0, 'creation_date', as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%Y-%m-%dT%H:%M:%SZ' ) ) )
  ncatt_put( nc_new, 0, 'data_structure', 'grid' )
  ncatt_put( nc_new, 0, 'frequency', 'mon' )
  ncatt_put( nc_new, 0, 'realm', 'atmos' )
  ncatt_put( nc_new, 0, 'source', 'CEDS 12-27-16: Community Emissions Data System (CEDS) for Historical Emissions' )
  ncatt_put( nc_new, 0, 'source_id', 'CEDS-12-27-16' )
  ncatt_put( nc_new, 0, 'further_info_url', 'http://www.globalchange.umd.edu/ceds/' )
  ncatt_put( nc_new, 0, 'license', 'FOR TESTING AND REVIEW ONLY' )
  ncatt_put( nc_new, 0, 'history', 
             paste0( as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%d-%m-%Y %H:%M:%S %p %Z' ) ),
                     '; College Park, MD, USA') )
  ncatt_put( nc_new, 0, 'comment', 'Test Dataset for CMIP6' )
  ncatt_put( nc_new, 0, 'data_usage_tips', 
             'These monthly average fluxes can be linearly interpolated to join fluxes of adjacent months. (This should be done in a manner that the monthly average given here is preserved.)' )
  ncatt_put( nc_new, 0, 'host', 'TBD' )
  ncatt_put( nc_new, 0, 'contact', 'ssmith@pnnl.gov' )
  ncatt_put( nc_new, 0, 'references', 'http://www.geosci-model-dev.net/special_issue590.html' )
  # attributes for variables
  for ( each_var in sector_list ) {
    ncatt_put( nc_new, each_var, 'cell_methods', 'time:mean' )
    ncatt_put( nc_new, each_var, 'missing_value', 1e+20, prec = 'float' )
  }
  
  # close nc_new
  nc_close( nc_new)
  
  # additional section: write a summary and check text
  global_grid_area <- grid_area( grid_resolution, all_lon = T )
  global_total <- c()
  em_global_sectors <- c()
  for ( em_global_data in data_list ) {
    em_global_sector <- unlist( strsplit( em_global_data, split = '_' ) ) [ 1 ]
    eval( parse( text = paste0( 'temp_data <- ', em_global_data ) ) )
    temp_data <- temp_data * global_grid_area
    total <- sum( temp_data )
    global_total <- c( global_total, total )
    em_global_sectors <- c( em_global_sectors, em_global_sector)
    }
  summary_table <- data.frame( year = year, species = em_species, 
                               sector = em_global_sectors, 
                               global_total = global_total,
                               unit = 'kg s-1' )
  summary_name <- paste0( output_dir, 'CEDS_', em_species, '_anthro_', year, '_', grid_resolution, '_', ver_date, '.csv' )
  write.csv( summary_table, file = summary_name, row.names = F )
}
# -------------------------------------------------
# monthly_nc_output
# Brief: generate a fliped matrix by a given matrix
# Dependencies: 
# Author: Leyang Feng
# parameters: x - the matrix to be fliped  
# return: a fliped matrix 
# input files: 
# output: 
final_monthly_nc_output <- function( output_dir, grid_resolution, year, em_species, sector_list, mass = F ) {
  
  AGR_em_global_final <- AGR_em_global
  ENE_em_global_final <- ELEC_em_global + FFFI_em_global + ETRN_em_global
  IND_em_global_final <- INDC_em_global + INPU_em_global
  TRN_em_global_final <- NRTR_em_global + ROAD_em_global
  RCO_em_global_final <- RCO_em_global
  SOL_em_global_final <- SOL_em_global
  WST_em_global_final <- WST_em_global
  SHP_em_global_final <- SHP_em_global
  
  data_list <- paste0( sector_list, '_em_global_final')
  
  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
  time <- 1:12
  londim <- ncdim_def( "lon", "degrees_east", as.double( lons ), longname = 'longitude' )
  latdim <- ncdim_def( "lat", "degrees_north", as.double( lats ), longname = 'latitude' )
  timedim <- ncdim_def( "time", "days since 1750-01-01 0:0:0", as.double( time ), 
                        calendar = '365_days', longname = 'time' )
  dim_list <- list( londim, latdim, timedim )
  lon_bnds_data <- cbind( seq( -180, ( 180 - grid_resolution ), grid_resolution ), 
                          seq( ( -180 + grid_resolution ), 180, grid_resolution ) )
  lat_bnds_data <- cbind( seq( -90, (90 - grid_resolution) , grid_resolution), 
                          seq( ( -90 + grid_resolution ), 90, grid_resolution ) )
  bnds <- 1 : 2
  bndsdim <- ncdim_def( "bnds", '', as.integer( bnds ), longname = 'bounds', create_dimvar = F )
  time_bnds_data <- cbind( c( 1, 32, 62, 93, 122, 153, 183, 214, 245, 275, 306, 336 ),
                      c( 31, 61, 92, 121, 152, 182, 213, 244, 274, 305, 335, 366 ) )
  
  if ( mass == T ){
    data_unit <- 'kt'
  } else {
    data_unit <- 'kg m-2 s-1'  
  }
  
  # define nc variables
  left_part <- sector_list 
  mid_part <- ' <- '
  right_part <- paste0( 'ncvar_def( \'', left_part, '\', \'', data_unit, '\', dim_list, missval = missing_value, longname= \'' ,final_sector_longname_list, '\' , prec = \'float\', compression = 5 )' )
  expressions <- paste0( left_part, mid_part, right_part )
  missing_value <- 1.e20
  eval( parse( text = expressions ) )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( londim, bndsdim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( latdim, bndsdim ), prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( timedim, bndsdim ), prec = 'double' )
  
  # generate nc file name
  version <- 'v1'
  date_parts <- unlist( strsplit( as.character( Sys.Date() ), split = '-' ) ) 
  ver_date <- paste( version, date_parts[ 2 ], date_parts[ 3 ], date_parts[ 1 ], sep = '_' )
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
  for ( i in seq_along(time) ) {
  expressions <- paste0( 'ncvar_put( nc_new, ', left_part, ' ,t( flip_a_matrix( ', data_list,
                         ' / 12 ) ) , start = c( 1, 1, i ), count = c( -1, -1, 1 ) )' )
  eval( parse( text = expressions ) )
  }
  ncvar_put( nc_new, lon_bnds, lon_bnds_data )
  ncvar_put( nc_new, lat_bnds, lat_bnds_data )
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
  # nc global attributes
  ncatt_put( nc_new, 0, 'IMPORTANT', 'FOR TEST ONLY, DO NOT USE' )
  ncatt_put( nc_new, 0, 'title', paste0('Annual Emissions of ', em_species ) )
  ncatt_put( nc_new, 0, 'institution_id', 'PNNL-JGCRI' )
  ncatt_put( nc_new, 0, 'institution', 'Pacific Northwest National Laboratory - JGCRI' )
  ncatt_put( nc_new, 0, 'activity_id', 'input4MIPs' )
  ncatt_put( nc_new, 0, 'Conventions', 'CF-1.6' )
  ncatt_put( nc_new, 0, 'creation_date', as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%Y-%m-%dT%H:%M:%SZ' ) ) )
  ncatt_put( nc_new, 0, 'data_structure', 'grid' )
  ncatt_put( nc_new, 0, 'frequency', 'mon' )
  ncatt_put( nc_new, 0, 'realm', 'atmos' )
  ncatt_put( nc_new, 0, 'source', 'CEDS 12-27-16: Community Emissions Data System (CEDS) for Historical Emissions' )
  ncatt_put( nc_new, 0, 'source_id', 'CEDS-12-27-16' )
  ncatt_put( nc_new, 0, 'further_info_url', 'http://www.globalchange.umd.edu/ceds/' )
  ncatt_put( nc_new, 0, 'license', 'FOR TESTING AND REVIEW ONLY' )
  ncatt_put( nc_new, 0, 'history', 
             paste0( as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%d-%m-%Y %H:%M:%S %p %Z' ) ),
                     '; College Park, MD, USA') )
  ncatt_put( nc_new, 0, 'comment', 'Test Dataset for CMIP6' )
  ncatt_put( nc_new, 0, 'data_usage_tips', 
             'These monthly average fluxes can be linearly interpolated to join fluxes of adjacent months. (This should be done in a manner that the monthly average given here is preserved.)' )
  ncatt_put( nc_new, 0, 'host', 'TBD' )
  ncatt_put( nc_new, 0, 'contact', 'ssmith@pnnl.gov' )
  ncatt_put( nc_new, 0, 'references', 'http://www.geosci-model-dev.net/special_issue590.html' )
  # attributes for variables
  for ( each_var in sector_list ) {
    ncatt_put( nc_new, each_var, 'cell_methods', 'time:mean' )
    ncatt_put( nc_new, each_var, 'missing_value', 1e+20, prec = 'float' )
  }
  
  # close nc_new
  nc_close( nc_new)
  
  # additional section: write a summary and check text
  global_grid_area <- grid_area( grid_resolution, all_lon = T )
  global_total <- c()
  em_global_sectors <- c()
  for ( em_global_data in data_list ) {
    em_global_sector <- unlist( strsplit( em_global_data, split = '_' ) ) [ 1 ]
    eval( parse( text = paste0( 'temp_data <- ', em_global_data ) ) )
    temp_data <- temp_data * global_grid_area
    total <- sum( temp_data )
    global_total <- c( global_total, total )
    em_global_sectors <- c( em_global_sectors, em_global_sector)
    }
  summary_table <- data.frame( year = year, species = em_species, 
                               sector = em_global_sectors, 
                               global_total = global_total,
                               unit = 'kg s-1' )
  summary_name <- paste0( output_dir, 'CEDS_', em_species, '_anthro_', year, '_', grid_resolution, '_', ver_date, '.csv' )
  write.csv( summary_table, file = summary_name, row.names = F )
}
