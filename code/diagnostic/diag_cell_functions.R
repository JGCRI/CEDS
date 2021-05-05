# ------------------------------------------------------------------------------
# Program Name: diag_cell_functions.R
# Author(s): Leyang Feng
# Date Last Updated: April 26, 2021
# Program Purpose: Defines functions for manipulating matrices.
# Input Files:
# Output Files:
# Notes:
# TODO: Check to see if these functions already exist in the gridding system.
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# cellCenter2index:
# Convert cell center lat and lon into matrix row and col indeces
cellCenter2index <- function( lat = NULL, lon = NULL, resolution = 0.5 ) { 
  
  lut_lat <- data.frame( lat = seq( ( 90 - resolution/2 ), ( -90 + resolution/2 ), ( -1 * resolution ) ),
                         row = 1 : ( 180 / resolution ), 
                         stringsAsFactors = F ) 
  lut_lon <- data.frame( lon = seq( ( -180 + resolution/2 ), ( 180 - resolution/2 ), resolution ),  
                         col = 1 : ( 360 / resolution ),
                         stringsAsFactors = F )
  
  if ( lat %in% lut_lat$lat ) { 
    row_index <- lut_lat[ lut_lat$lat == lat, 'row' ]
    } else { stop( 'invalid lat value' ) }
  if ( lon %in% lut_lon$lon ) { 
    col_index <- lut_lon[ lut_lon$lon == lon, 'col' ]
    } else { stop( 'invalid lat value' ) }
  row_col <- c( row_index, col_index )
  return( row_col )
}   

# -----------------------------------------------------------------------------
# cellIndex2Center:
# Convert matrix row and col indeces into cell center lat and lon coordinates
cellIndex2Center <- function( row = NULL, col = NULL, resolution = 0.5 ) { 
  lut_lat <- data.frame( lat = seq( ( 90 - resolution/2 ), ( -90 + resolution/2 ), ( -1 * resolution ) ),
                         row = 1 : ( 180 / resolution ), 
                         stringsAsFactors = F ) 
  lut_lon <- data.frame( lon = seq( ( -180 + resolution/2 ), ( 180 - resolution/2 ), resolution ),  
                         col = 1 : ( 360 / resolution ),
                         stringsAsFactors = F )
  if ( row %in% lut_lat$row ) { 
    lat_center <- lut_lat[ lut_lat$row == row, 'lat' ]
    } else { stop( 'invalid lat value' ) }
  if ( col %in% lut_lon$col ) { 
    lon_center <- lut_lon[ lut_lon$col == col, 'lon' ]
    } else { stop( 'invalid lat value' ) }
  lat_lon <- c( lat_center, lon_center )
  return( lat_lon )  
} 

# -----------------------------------------------------------------------------
# convertCellIndex_122
# Convert 1 dimensional cell index into matrix row and col indeces 
convertCellIndex_122 <- function( one_dim_index = NULL, resolution = 0.5 ) {
  row_ind_mat <- matrix( rep( 1 : ( 180 / resolution ), 360 / resolution ), 
                         nrow = 180 / resolution, 
                         ncol = 360 / resolution )
  row <- row_ind_mat[ one_dim_index ]
  col_ind_mat <- matrix( rep( 1 : ( 360 / resolution ), 180 / resolution ), 
                         nrow = 180 / resolution,
                         ncol = 360 / resolution,
                         byrow = T ) 
  col <- col_ind_mat[ one_dim_index ]
  row_col <- c( row, col )
  return( row_col )
  } 


# -----------------------------------------------------------------------------
# convertCellIndex_221
# Convert matrix row and col indeces into 1 dimensional cell index 
convertCellIndex_221 <- function( row = NULL, col = NULL, resolution = 0.5 ) {
  ind_mat <- matrix( 1 : ( (180/resolution) * (360/resolution) ), 
                     nrow = 180 / resolution, 
                     ncol = 360 / resolution )
  one_dim_index <- ind_mat[ row, col ]
  return( one_dim_index )
  } 
