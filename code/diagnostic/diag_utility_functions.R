# ------------------------------------------------------------------------------
# Program Name: diag_utility_functions.R
# Author(s): Leyang Feng
# Date Last Updated: April 26, 2021
# Program Purpose: Define functions for manipulating gridded data.
# Input Files:
# Output Files:
# Notes:
# TODO:
# ------------------------------------------------------------------------------
library( 'sp' )
library( 'raster' )
library( 'ncdf4' )
library( 'geosphere' )

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
# rasterize_GlobalExt
# Brief: generate a fliped matrix by a given matrix
# Dependencies:
# Author: Leyang Feng
# parameters: x - the matrix to be fliped
# return: a fliped matrix
# input files:
# output:
nc_var_list <- function( nc_file ) {
 var_list <- sapply( seq_along( nc_file$var ), function( i ) { nc_file$var[[ i ]]$name } )
 return( var_list )
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
# load_list
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

load_list <- function( name_list, path ) {
  invisible( lapply( name_list, function ( each_name ) {
    load( paste0( path, '/', each_name ), .GlobalEnv )
  } ) )
}
# ------------------------------------------------------------------------------
# check_list_na
# Brief: Compute areas of a column of grid cells for all latitude at
#        desired resolution in square meters
# Dependencies:
# Author: Leyang Feng
# parameters:
# return: grid cell areas
# input files:
# output:
check_list_na <- function( x ){
  logic_list <- unlist( lapply( seq_along( x ), function( i ) { T %in% is.na( x[[i]] ) } ) )
  return( logic_list )
}
# ------------------------------------------------------------------------------
# check_list_inf
# Brief: Compute areas of a column of grid cells for all latitude at
#        desired resolution in square meters
# Dependencies:
# Author: Leyang Feng
# parameters:
# return: grid cell areas
# input files:
# output:
check_list_inf <- function( x ){
  logic_list <- unlist( lapply( seq_along( x ), function( i ) { T %in% is.infinite( x[[i]] ) } ) )
  return( logic_list )
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
