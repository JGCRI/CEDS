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

grid_one_country <- function( country, location_index, em_data, proxy, proxy_backup, year, sector, em_species, proxy_replace_flag ) {
  
  # retrieve matrix indexes for iso for later proxy cropping
  row_col_index <- location_index[ location_index$iso == country, ]
  start_row <- row_col_index$start_row
  end_row <- row_col_index$end_row
  start_col <- row_col_index$start_col
  end_col <- row_col_index$end_col
      
  # retrieve the iso_mask from memory 
  mask_name <- paste0( country, '_mask')
  mask <- get( mask_name )
  
  # retreive the proxy_replace_flag for current country 
  flag_line <- proxy_replace_flag[ proxy_replace_flag$iso == country, ]
  country_flag <- flag_line$replace_flag
  
  # decide use proxy_backup or not 
  if ( country_flag == T ) {
    # extract the proxy as the extent of the iso_mask
    proxy_cropped <-proxy_backup[ start_row : end_row, start_col : end_col ]
    
    # write out replacement message 
    summary_dir <- filePath( "MED_OUT", "", extension="")
    
    
    # what if the backup proxy have all zero pattern 
    if ( sum(proxy_cropped * mask ) == 0 ) {
      message_line <- paste0( 'Backup proxy used but all zero pattern: ', country, ' ,', year, ' ,', sector, ' ,', em_species )
    } else {
      message_line <- paste0( 'Backup proxy used: ', country, ' ,', year, ' ,', sector, ' ,', em_species )  
      }
    cat( message_line, file = paste0( summary_dir, 'proxy_replacement_list.txt' ), append = TRUE, sep="\n" )
  } else {
    # extract the proxy as the extent of the iso_mask
    proxy_cropped <- proxy[ start_row : end_row, start_col : end_col ] 
    }
  
  # retrieve the iso's emission for later distributing 
  emission_value <- em_data[ em_data$iso == country, 2 ]
  
  # get boundary weighted proxy 
  weighted_proxy <- proxy_cropped * mask
  
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

aggregate_all_countries <- function( sector, country_list, location_index, grid_resolution, mass ) {
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
  if ( mass == F ) {
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
  } 
  return( country_drop_list )
	#message( paste0( 'masks not available for: ', country_drop_list ) )
  #summary_dir <- filePath( "MED_OUT", "", extension = "" )
  #cat( country_drop_list, file = paste0( summary_dir, 'dropped_countries.txt' ), append = TRUE, sep = "\n" )
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
    
    # use RCO proxy for sector RCOO and RCORC
    if ( sector == 'RCORC' ) { sector <- 'RCO' }
    if ( sector == 'RCOO' ) { sector <- 'RCO' }
    
    # specify the proxy_dir
    proxy_dir <- filePath( "GRIDDING", "", extension="", domain_extension = "proxy/")
    # list out all availiable proxies in proxy_dir
    proxy_list <- list.files( proxy_dir ) 
    
    # generate the proxy name want to load 
    proxy_filename <- paste( em_species, year, sector, sep = '_')
    
    # if the proxy desired is not in proxy_list, load population as proxy 
    if( ( proxy_filename %in% proxy_list ) == T ) {
      load( paste0( proxy_dir,proxy_filename ) )
      proxy <- get( proxy_filename )
      rm( list = proxy_filename )
    } else { 
      proxy_filename <- paste0( 'population_', year )
      proxy_dir <- filePath( "GRIDDING", "", extension="", domain_extension = "proxy_backup/")
      load( paste0( proxy_dir, proxy_filename ) )
      proxy <- get( proxy_filename )
      rm( list = proxy_filename )
    }
    
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
  
  # retrive the proxy data
  proxy <- get_proxy( em_species, year, sector )
  proxy_backup <- get_backup_proxy( year, type = 'population' )
  
  # extract emission for specific year and gas
  emissions_year_sector <- subset( em_data, em_data$CEDS_grd_sector == sector,
                              c( 'iso', paste0( 'X', year ) ) )
  # if no mask available for certain country, generate a country drop list 
  country_drop_list <- mask_avail_check( emissions_year_sector$iso, location_index$iso )
  summary_dir <- filePath( "MED_OUT", "", extension = "" )
  #cat( country_drop_list, file = paste0( summary_dir, 'dropped_countries.txt' ), append = TRUE, sep = "\n" )
  # drop country if necessary 
  for ( country in country_drop_list ) {
	emissions_year_sector <- emissions_year_sector[ emissions_year_sector$iso != country, ]
	}
  # extract current country_list from emissions_year_sector
  country_list <- emissions_year_sector$iso 
  # perform proxy_substitution_check. If for one perticular country, the flag is TRUE,
  # then use proxy_backup
  proxy_replace_flag <- proxy_substitution_check( country_list, location_index, emissions_year_sector, proxy )
  
  # if using population as proxy, no need to do proxy substitution 
  if ( as.numeric( year ) < 1970 ) {
    proxy_replace_flag[ proxy_replace_flag == T ] <- F
  }
  # calling the G.emiDistribute function to do the scalling
  invisible( lapply( country_list, grid_one_country, location_index, emissions_year_sector, proxy, proxy_backup, year, sector, em_species, proxy_replace_flag ) )

  # aggregating
  output_name <- paste0( sector, '_em_global' )
  expressions <- paste0( 'temp_emission <<- aggregate_all_countries( ',sector, ', country_list, location_index, grid_resolution, mass = ', mass, ' )' )
  eval( parse( text = expressions ) )
  assign( output_name, temp_emission, .GlobalEnv )

}
# ------------------------------------------------------------------------------
# grid_one_year
# Brief: generate a fliped matrix by a given matrix
# Dependencies: 
# Author: Leyang Feng
# parameters: x - the matrix to be fliped  
# return: a fliped matrix 
# input files: 
# output: 
grid_one_year <- function( em_species, year, em_data, location_index, sector_list, grid_resolution, mass = F ) { 
  X_year <<- paste0( 'X', year )
  emissions_year <<- em_data[ c( 'iso', 'CEDS_grd_sector', X_year ) ]
  
  sector_list <- sector_list[ !sector_list == 'SHP' ]
  invisible( lapply( sector_list, grid_one_sector, em_species, year, location_index, grid_resolution, emissions_year, mass ) )
  
  # SHP gridding
  proxy <- get_proxy( em_species, year, 'SHP' )
  emissions_year_SHP <- subset( emissions_year, emissions_year$CEDS_grd_sector == 'SHP',
                              c( 'iso', paste0( 'X', year ) ) )
  emission_value <- sum( emissions_year_SHP[, 2 ] )
  proxy_weighted <- proxy * global_mask 
  proxy_normlized <- proxy_weighted / sum( proxy_weighted )
  SHP_em_global <- proxy_normlized * emission_value
  if ( mass == F ) {
    # generate the global_grid_area matrix for later calculation 
    global_grid_area <- grid_area( grid_resolution, all_lon = T )
    flux_factor <- 1000000 / global_grid_area / ( 365 * 24 * 60 * 60 )
    SHP_em_global <<- SHP_em_global * flux_factor
  } else { SHP_em_global <<- SHP_em_global }
  
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
                                 load_masks = T, load_seasonality_profile = T
                                 ){
  # set up basics
  grid_resolution <<- grid_resolution 
  message( paste0( 'Processing resolution: ', grid_resolution ) )
  start_year <- start_year
  end_year <- end_year
  year_list <<- as.character( seq( start_year, end_year ) )
  message( paste0( 'Gridding from year ', start_year, ' to year ', end_year ) )
  
  # load country masks 
  mask_dir <- filePath( "GRIDDING", "", extension="", domain_extension = "mask/")
  mask_list <- list.files( mask_dir )
  if ( load_masks == T ) {
    invisible( lapply( mask_list, function( mask_list ) { load( paste0( mask_dir, mask_list), .GlobalEnv ) } ) )
    country_mask_initialized <- T
    message( 'Country mask initialized: ', country_mask_initialized )
  }
  
  # load seasonality profile
  seasonality_dir <- filePath( "GRIDDING", "", extension="", domain_extension = "seasonality/" )
  common_seasonality_list <- c( "AGR_NH3_seasonality", "AGR_seasonality", "ENE_seasonality", 
                                "IND_seasonality", "RCOO_seasonality", "RCORC_seasonality", 
                                "SHP_seasonality", "SLV_seasonality", "TRA_seasonality", "WST_seasonality" ) 
  invisible( lapply( common_seasonality_list, function( common_seasonality ) { load( paste0( seasonality_dir, common_seasonality ), .GlobalEnv ) } ) )
  seasonality_profile_initialized <- T
  message( 'Seasonality profile initialized: ', seasonality_profile_initialized )
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
# final_monthly_nc_output
# Brief: generate a fliped matrix by a given matrix
# Dependencies: 
# Author: Leyang Feng
# parameters: x - the matrix to be fliped  
# return: a fliped matrix 
# input files: 
# output: 
final_monthly_nc_output <- function( output_dir, grid_resolution, year, em_species, sector_list, sector_list_long, mass = F ) {
  
  # 0 set up some basics for later use
  global_grid_area <- grid_area( grid_resolution, all_lon = T )
  flux_factor <- 1000000 / global_grid_area / ( 365 * 24 * 60 * 60 )
  Days_in_Month <- c( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 )
  
  # process emission grids from low level sectors to high level sectors 
  # and adding seasonality ( except RCO )
  # first, aggregate to higher level
  AGR_em_global_final <- AGR_em_global
  ENE_em_global_final <- ELEC_em_global + FFFI_em_global + ETRN_em_global  
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
  
  data_list <- paste0( sector_list, '_em_global_final')
  
  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
  time <- c( 15.5, 45, 74.5, 105, 135.5, 166, 196.5, 227.5, 258, 288.5, 319, 349.5 )
  londim <- ncdim_def( "lon", "degrees_east", as.double( lons ), longname = 'longitude' )
  latdim <- ncdim_def( "lat", "degrees_north", as.double( lats ), longname = 'latitude' )
  timedim <- ncdim_def( "time", paste0( "days since ", year, "-01-01 0:0:0" ), as.double( time ), 
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
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( londim, bndsdim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( latdim, bndsdim ), prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( timedim, bndsdim ), prec = 'double' )
  
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
  # attributes for variables
  for ( each_var in sector_list ) {
    ncatt_put( nc_new, each_var, 'cell_methods', 'time:mean' )
    ncatt_put( nc_new, each_var, 'missing_value', 1e+20, prec = 'float' )
  }
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
  ncatt_put( nc_new, 0, 'source', paste0( 'CEDS ', 
                                          as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%m-%d-%Y' ) ),
                                          ': Community Emissions Data System (CEDS) for Historical Emissions' ) )
  ncatt_put( nc_new, 0, 'source_id', paste0( 'CEDS-', as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%m-%d-%Y' ) ) ) )
  ncatt_put( nc_new, 0, 'further_info_url', 'http://www.globalchange.umd.edu/ceds/' )
  ncatt_put( nc_new, 0, 'license', 'FOR TESTING AND REVIEW ONLY' )
  ncatt_put( nc_new, 0, 'history', 
             paste0( as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%d-%m-%Y %H:%M:%S %p %Z' ) ),
                     '; College Park, MD, USA') )
  ncatt_put( nc_new, 0, 'comment', 'Test Dataset for CMIP6' )
  ncatt_put( nc_new, 0, 'data_usage_tips', 'Note that these are monthly average fluxes.' )
  ncatt_put( nc_new, 0, 'host', 'TBD' )
  ncatt_put( nc_new, 0, 'contact', 'ssmith@pnnl.gov' )
  ncatt_put( nc_new, 0, 'references', 'http://www.geosci-model-dev.net/special_issue590.html' )

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
  summary_name <- paste0( output_dir, 'CEDS_', em_species, '_anthro_', year, '_', grid_resolution, '_', ver_date, '.csv' )
  write.csv( summary_table, file = summary_name, row.names = F )
}
# -------------------------------------------------
# get_seasonalityFrac
# Brief: generate a fliped matrix by a given matrix
# Dependencies: 
# Author: Leyang Feng
# parameters: x - the matrix to be fliped  
# return: a fliped matrix 
# input files: 
# output: 
get_seasonalityFrac <- function( em_species, sector, year ) {

  seasonality_dir <- filePath( "GRIDDING", "", extension="", domain_extension = "seasonality/" )
  total_seasonality_list <- list.files( seasonality_dir )
  
  seasonality_filename <- paste0( sector, '_', em_species, '_', year, '_seasonality' )
  if ( ( seasonality_filename %in% total_seasonality_list ) == F ) {
    if ( sector == 'AGR' & em_species == 'NH3' ) { em_species <- '_NH3' } else { em_species <- NULL }
    if ( sector == 'RCO' ) { sector <- 'RCORC' }
    seasonality_filename <- paste0( sector, em_species, '_seasonality')
  } else { load( paste0( seasonality_dir, seasonality_filename ) ) }

  seasonality <- get( seasonality_filename )
  return( seasonality )
}
# -------------------------------------------------
# get_backup_proxy
# Brief: generate a fliped matrix by a given matrix
# Dependencies: 
# Author: Leyang Feng
# parameters: x - the matrix to be fliped  
# return: a fliped matrix 
# input files: 
# output: 
get_backup_proxy <- function( year, type = 'population' ) {

  if ( type == 'population' ) { type_name <- 'population' }
  proxy_name <- paste0( type_name, '_', year )
  
  proxy_dir <- filePath( "GRIDDING", "", extension="", domain_extension = "proxy_backup/")

  load( paste0( proxy_dir,proxy_name ) )
  proxy_backup <- get( proxy_name )
  rm( list = proxy_name )
  return( proxy_backup )
}
# -------------------------------------------------
# proxy_substitution_check
# Brief: generate a fliped matrix by a given matrix
# Dependencies: em_proxy_ratio, flag_or_not
# Author: Leyang Feng
# parameters: x - the matrix to be fliped  
# return: a fliped matrix 
# input files: 
# output: 
proxy_substitution_check <- function( region_list, location_index, em_data, proxy ) {
  
  # generate a list of ratios for all regions 
  ratio_list <- unlist( lapply( region_list, em_proxy_ratio, em_data, location_index, proxy ) )
  
  # remove Inf in the list 
  ratio_list_noInf <- ifelse( is.infinite( ratio_list ), NA, ratio_list )
  
  # compute the median and max using INF removed ratio list 
  median_ratio <- median( ratio_list_noInf, na.rm = T )
  max_ratio <- max( ratio_list_noInf, na.rm = T )
  
  # generate a list of flag based on region_list, proxy substitution is needed if T
  flag_list <- unlist( lapply( ratio_list, substitue_or_not, max_ratio ) )
  
  # make the flag_list into a data frame 
  flag_df <- data.frame( iso = region_list, replace_flag  = flag_list )
  
  return( flag_df )
  }
# -------------------------------------------------
# em_proxy_ratio
# Brief: compute the ratio between a region's emission and its proxy. Note the input 
#        proxy is not normalized. 
# Dependencies: none
# Author: Leyang Feng
# parameters: region - the region whose ratio wants to be calculated
#             em_data - year-sector specified a list of emissions for all regions. Only 
#                       contains two columns: iso and emission
#             location_index - a data frame contains matix indexes for all regions 
#             proxy - the proxy wanted to be examined. Should be in global extent.   
# return: a ratio
# input files: none
# output: none
em_proxy_ratio <- function( region, em_data, location_index, proxy ) {
  
  # retrieve matrix index infromation from loction_index table
  region_index <- location_index[ location_index$iso == region, ]
  start_row <- region_index$start_row
  end_row <- region_index$end_row
  start_col <- region_index$start_col
  end_col <- region_index$end_col
  
  # retrieve the region_mask from memory 
  mask_name <- paste0( region, '_mask' )
  if ( exists( mask_name ) == T ) { 
    mask <- get( mask_name ) 
  } else {
	message( paste0( mask_name, ' is not loaded into memory ' ) ) 
    }
  
  # extract the proxy as the extent of the region_mask 
  proxy_cropped <- proxy[ start_row : end_row, start_col : end_col ]
  
  # make the proxy weighted 
  proxy_weighted <- proxy_cropped * mask
  
  # retrieve the region's emission
  emission_value <- em_data[ em_data$iso == region, 2 ]
  
  # compute em/proxy ratio
  em_proxy_ratio <- emission_value / sum( proxy_weighted )
  
  return( em_proxy_ratio )
}
# -------------------------------------------------
# flag_or_not
# Brief: decide whether region needs proxy substitution or not
# Dependencies: none
# Author: Leyang Feng
# parameters: ratio - emission/proxy ratio computed by previous step
#             max_ratio - max ratio computed by previous step 
# return: a flag which is TRUE of FALSE
# input files: none 
# output: none
substitue_or_not <- function( ratio, max_ratio ) {
  if ( is.infinite( ratio ) == T ) { flag <- T } # ratio=number/0, all zero proxy pattern for existing emission => substitution needed 
  else if ( is.nan( ratio) == T ) { flag <- F } # ratio = 0/0, all zero proxy pattern for non-existing emission => no substitution needed
  else if ( is.na( ratio ) == T ) { flag <- F } # similar to nan
  else if ( ratio == 0 ) { flag <- F } # ratio=0/number, zero emission => no substitution needed
  else { 
    if ( ratio > ( max_ratio / 1000 ) ) { flag <- T } # for outlier needs substitution
	else { flag <- F } # non-outlier doesn't need substitution 
  }
  return( flag )
}
# -------------------------------------------------
# grid_one_year_air
# Brief: decide whether region needs proxy substitution or not
# Dependencies: none
# Author: Leyang Feng
# parameters: ratio - emission/proxy ratio computed by previous step
#             max_ratio - max ratio computed by previous step 
# return: a flag which is TRUE of FALSE
# input files: none 
# output: none
grid_one_year_air <- function( em_species, year, em_data, grid_resolution, sector = 'AIR', mass = F ) {

  proxy <- get_proxy( em_species, year, sector )
  emission_value <- em_data[ , paste0( 'X', year )]
  
  proxy_normalized  <- proxy / sum( proxy )
  proxy_normalized [ is.na( proxy_normalized ) ] <- 0 
  AIR_em_global <- proxy_normalized  * emission_value
  if ( mass == F ) {
    # generate the global_grid_area matrix for later calculation 
    global_grid_area <- grid_area( grid_resolution, all_lon = T )
    flux_factor <- 1000000 / global_grid_area / ( 365 * 24 * 60 * 60 )
    for ( i in 1: dim( AIR_em_global )[ 3 ] ) {
	  AIR_em_global[ , , i ] <- AIR_em_global[ , , i ] * flux_factor
	}
	AIR_em_global <<- AIR_em_global
  } else { AIR_em_global <<- AIR_em_global }
}
# -------------------------------------------------
# final_monthly_nc_output
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
  checksum_sector_list <- rep( sector , 12 ) 
  
  
  lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
  lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
  levs <- seq( 0.305, 14.945, 0.61 )
  time <- c( 15.5, 45, 74.5, 105, 135.5, 166, 196.5, 227.5, 258, 288.5, 319, 349.5 )
  londim <- ncdim_def( "lon", "degrees_east", as.double( lons ), longname = 'longitude' )
  latdim <- ncdim_def( "lat", "degrees_north", as.double( lats ), longname = 'latitude' )
  levdim <- ncdim_def( "level", "km", as.double ( levs ), longname = 'altitude' ) 
  timedim <- ncdim_def( "time", paste0( "days since ", year, "-01-01 0:0:0" ), as.double( time ), 
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
  
  if ( mass == T ){
    data_unit <- 'kt'
  } else {
    data_unit <- 'kg m-2 s-1'  
  }
  
  # define nc variables
  missing_value <- 1.e20
  AIR <- ncvar_def( sector, data_unit, dim_list, missval = missing_value, longname = sector_long, prec = 'float', compression = 5  )
  lon_bnds <- ncvar_def( 'lon_bnds', '', list( londim, bndsdim ), prec = 'double' )
  lat_bnds <- ncvar_def( 'lat_bnds', '', list( latdim, bndsdim ), prec = 'double' )
  time_bnds <- ncvar_def( 'time_bnds', '', list( timedim, bndsdim ), prec = 'double' )
  
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
  # attributes for variables
  ncatt_put( nc_new, 'AIR', 'cell_methods', 'time:mean' )
  ncatt_put( nc_new, 'AIR', 'missing_value', 1e+20, prec = 'float' )
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
  ncatt_put( nc_new, 0, 'source', paste0( 'CEDS ', 
                                          as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%m-%d-%Y' ) ),
                                          ': Community Emissions Data System (CEDS) for Historical Emissions' ) )
  ncatt_put( nc_new, 0, 'source_id', paste0( 'CEDS-', as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%m-%d-%Y' ) ) ) )
  ncatt_put( nc_new, 0, 'further_info_url', 'http://www.globalchange.umd.edu/ceds/' )
  ncatt_put( nc_new, 0, 'license', 'FOR TESTING AND REVIEW ONLY' )
  ncatt_put( nc_new, 0, 'history', 
             paste0( as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%d-%m-%Y %H:%M:%S %p %Z' ) ),
                     '; College Park, MD, USA') )
  ncatt_put( nc_new, 0, 'comment', 'Test Dataset for CMIP6' )
  ncatt_put( nc_new, 0, 'data_usage_tips', 'Note that these are monthly average fluxes.' )
  ncatt_put( nc_new, 0, 'host', 'TBD' )
  ncatt_put( nc_new, 0, 'contact', 'ssmith@pnnl.gov' )
  ncatt_put( nc_new, 0, 'references', 'http://www.geosci-model-dev.net/special_issue590.html' )

  global_total_emission <- sum( checksum_total_emission_list ) * 0.001
  ncatt_put( nc_new, 0, 'global_total_emission', paste0( round( global_total_emission, 2), ' Tg/year' ) )
  
    
  # close nc_new
  nc_close( nc_new)
  
  # additional section: write a summary and check text

  summary_table <- data.frame( year = year, species = em_species, 
                               sector = sector,
                               month = rep( 1 : 12 ),
                               global_total = checksum_total_emission_list,
                               unit = 'kt' )
  summary_name <- paste0( output_dir, 'CEDS_', em_species, '_', sector, '_', '_anthro_', year, '_', grid_resolution, '_', ver_date, '.csv' )
  write.csv( summary_table, file = summary_name, row.names = F )
}
# -------------------------------------------------
# region_emCombine
# Brief: generate a fliped matrix by a given matrix
# Dependencies: 
# Author: Leyang Feng
# parameters: x - the matrix to be fliped  
# return: a fliped matrix 
# input files: 
# output: 
region_emCombine <- function( em_data, region_list_for_combination ) {
  dim_check <- dim( region_list_for_combination )
  if ( dim_check[ 1 ] == 0 ) {
    message( 'No region\'s emission needs to be combined to another region. ' )
    return( em_data )  
  } else {
    for ( i in 1 : dim_check[ 1 ] ) { # lopping through each row of the combine_list 
	  region_merge_to <- region_list_for_combination[ i, 1 ]
	  region_merge <- region_list_for_combination[ i, 2 ]
	  em_data[ em_data$iso == region_merge_to , ] [ paste0( 'X', year_list ) ] <- em_data[ em_data$iso == region_merge_to, ][ paste0( 'X', year_list ) ] + em_data[ em_data$iso == region_merge, ][ paste0( 'X', year_list ) ]
	  em_data <- em_data[ !em_data$iso == region_merge, ]  
    }
    return( em_data )
  }
}