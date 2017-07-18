#-----------------------------------------------------------------------------------------------------------------
# Program Name: gridding_functions.R
# Author's Name: Leyang Feng
# Date Last Modified: June 30 2016
# Program Purpose: Core functions for emissions gridding routine
# Note: 1. Brief introduction of how things work in gridding routine:
#          There are three steps for gridding, (1) the emissions are gridded at country level
#          then aggregated to form a global grids. The core functions for this step are grid_one_iso and aggregate_all_isos. 
#          (2) Then the first step is repeated for all sectors. The core function for this 
#          step is grid_one_sector and its wraper grid_all_sectors.
#          (3) Then the list contains global grids for all sectors is returned by grid_one_year
#          The nested relationship of three steps's core functions is as below
#            grid_one_year
#              |----- grid_all_sectors (grid_one_sector)
#                                                |----------- grid_all_isos (grid_one_iso)
#                                                |----------- aggregate_all_isos
#       2. The variations of grid_one_xxx functions ( such as grid_one_country_subVOCs, grid_one_year_air ) 
#          serves specific gridding tasks.  
#       3. Functions in this script are categrized into different classes serving different purposes:
#          core gridding functions - gridding functions for general routine
#          specified gridding functions - gridding functions for specific gridding tasks. 
#          proxy functions - proxy related functions that serve core/specified gridding functions 
#          seasonality functions - seasonality related functions that serve core/specified gridding functions 
#          other supporting functions
# TODO: 
# ----------------------------------------------------------------------------------------------------------------

# Special Packages
loadPackage( 'ncdf4' ) 
loadPackage( 'sp' )
loadPackage( 'geosphere' )

# ==============================================================================
# core gridding functions
# ------------------------------------------------------------------------------
# grid_one_iso
# Brief: Generates a iso's emission spatial distribution using proxy.
#        The emissions are gridded in following steps: 
#        (1) Bounding information is retrived using iso from location_index table
#            ( bounding information: the iso's bounding box in terms of upper left corner and lower right corner 
#                                    matrix indeces in global extent )
#        (2) Retrieve the proxy substitution flag from proxy subsitution mapping.
#            If proxy substitution flag is TRUE then use backup proxy instead of proxy
#        (3) The proxy and proxy_backup (in global extent) passed by upper layer function is cropped using bounding information.
#        (4) Apply weighted iso mask to cropped proxy to make proxy only contains data for the iso and boundary weighted. 
#        (5) Normalize the weighted proxy
#        (6) Apply emission statistics over normalized proxy to get emission spatial distribution for the iso
#        (7) Return the spatial distribution (matrix)  
# Dependencies: 
# Author: Leyang Feng
# parameters: iso - the iso that its gridded emission is desired  
#             em - the current gridding emission species
#             sector - the current gridding secotr
#             year - the current gridding year
#             grid_resolution - the current gridding resolution 
#             gridding_emissions_sector - the emission data from gridding, passed by upper layer function 
#             location_index - mapping file contains iso matrix indices
#             proxy_substitution_mapping - mapping file contains proxy substitution flags
#             proxy - proxy used for gridding, passed by upper layer function
#             proxy_backup - backup proxy used for gridding, passed by upper layer function
# return: em_spatial_global - matrix, the emission spatial distribution for the iso
# input files: null
# output: null
grid_one_iso <- function( iso, 
                          em, 
                          sector, 
                          year, 
                          grid_resolution, 
                          gridding_emissions_sector, 
                          location_index, 
                          proxy_substitution_mapping,
                          proxy,
                          proxy_backup ) { 

  sub_flag <- proxy_substitution_mapping[ proxy_substitution_mapping$em == em &
                                            proxy_substitution_mapping$sector == sector &
                                            proxy_substitution_mapping$iso == iso, 'sub_flag' ]
  
  if ( length( sub_flag ) == 0  ) { proxy <- proxy } else { proxy <- proxy_backup }
  
  # retrieve matrix indexes for iso for later proxy cropping
  index_line <- location_index[ location_index$iso == iso, ]
  start_row <- index_line$start_row
  end_row <- index_line$end_row
  start_col <- index_line$start_col
  end_col <- index_line$end_col
      
  # retrieve the iso_mask from memory 
  mask_name <- paste0( iso, '_mask')
  mask <- get( mask_name )
  
  
  proxy_cropped <- proxy[ start_row : end_row, start_col : end_col ] 
  weighted_proxy <- proxy_cropped * mask
  norm_weighted_proxy <- weighted_proxy / sum( weighted_proxy )
  norm_weighted_proxy[ is.nan( norm_weighted_proxy ) ] <- 0   
  
  emissions_value <- gridding_emissions_sector[ gridding_emissions_sector$iso == iso, paste0( 'X', year ) ]
  
  iso_em_spatial <- emissions_value * norm_weighted_proxy 
  
  return( iso_em_spatial )
}

# ------------------------------------------------------------------------------
# aggregare_all_isos
# Brief: Aggregates all iso's emission into one global grid.
#        The aggregating is in following steps:
#        (1) Create an empty temoplate grid as global extent using grid_resolution
#        (2) For each iso's emission spatial distribution, retrieve the iso's bounding information and 
#            add the iso's grid into template grid
#        (3) Rename the template grid and return 
# Dependencies: 
# Author: Leyang Feng
# parameters: iso_list - a list of isos need to be aggregated
#             iso_em_spatial_list - the list of emission spatial distributions for all isos.   
#             location_index - location index table, contains matrix index information for the country in global extent
#             grid_resolution - gridding resolution  
#             flux_factor - factor used for converting from mass to flux, passed by upper layer function  
# return: em_spatial_global  - global emission spatial distribution for given sector.   
# input files: null
# output: null
aggregate_all_isos <- function( iso_list, iso_em_spatial_list, location_index, grid_resolution, flux_factor ) {
  
  # create a empty template grid for later aggregating 
  temp_grid <- matrix( 0, 180 / grid_resolution, 360 / grid_resolution )
  
  # aggregating
  for ( iso in iso_list ) {
    
	info_line <- location_index[ location_index$iso == iso, ]
    start_row <- info_line$start_row
    end_row <- info_line$end_row
    start_col <- info_line$start_col
    end_col <- info_line$end_col
    
	# add the em_spatial to empty template grid
	temp_grid[ start_row : end_row , start_col : end_col ] <- 
    temp_grid[ start_row : end_row , start_col :end_col ] + iso_em_spatial_list[[ iso ]]
  }
  
  # convert into flux
  temp_grid <- temp_grid * flux_factor
  em_spatial_global <- temp_grid 

  return( em_spatial_global )
}

# ------------------------------------------------------------------------------
# grid_one_sector
# Brief: Generates a global grid for a given sector. 
#        The function contains sevral different gridding stradagy for differnet sectors after a few common steps. 
#        (1) extract sectoral gridding emissions 
#        (2) load proxy and proxy_backup
#         if sector is 'SHP':
#           (1) extract the emission value
#           (2) load the mask (global_mask in this case)
#           (3) normlize proxy 
#           (4) apply the emission value onto the normlized proxy 
#           (5) convert into flux from mass
#         if sector is 'TANK':
#            do the same thing as 'SHP'
#         if sector is 'WST':
#            (1) process the proxy (fixed to population -- see get_proxy()) as rural population
#            (2) extract iso_list 
#            (3) call grid_one_iso for iso_list
#            (4) call aggregate_all_isos to get global grid 
#         for rest sectors: 
#            (1) extract iso_list 
#            (2) call grid_one_iso for iso_list
#            (3) call aggregate_all_isos to get global grid 
# Dependencies: get_proxy, grid_one_iso, aggregate_all_isos
# Author: Leyang Feng
# parameters: sector - the current gridding sector
#             em - the current gridding emission species   
#             grid_resolution - gridding resolution 
#             year - the current gridding year
#             gridding_emissions_xyear - the emission data used for gridding, passed by upper layer function  
#             location_index - location index table, contains matrix index information for the country in global extent
#             proxy_mapping - proxy mapping file 
#             proxy_substitution_mapping - proxy substitution mapping file    
# return: global_em_spatial - a global grid for given sector 
# input files: null
# output: null 

grid_one_sector <- function( sector, 
                             em, 
                             grid_resolution,
                             year, 
                             gridding_emissions_xyear, 
                             location_index, 
                             proxy_mapping, 
                             proxy_substitution_mapping ) {
  
  gridding_emissions_sector <- gridding_emissions_xyear[ gridding_emissions_xyear$sector == sector, ]
  
  proxy <- get_proxy( em, year, sector, proxy_mapping, proxy_type = 'primary' )
  proxy_backup <- get_proxy( em, year, sector, proxy_mapping, proxy_type = 'backup' )
  
  global_grid_area <- grid_area( grid_resolution, all_lon = T )
  flux_factor <- 1000000 / global_grid_area / ( 365 * 24 * 60 * 60 ) # from kt to kg m-2 s-1
  
  if ( sector == 'SHP' ) { 
    emissions_value <- sum( gridding_emissions_sector[ , paste0( 'X', year ) ] )
    
    mask <- global_mask
	  proxy_weighted <- proxy * mask 
    proxy_normlized <- proxy_weighted / sum( proxy_weighted )
    proxy_normlized[ is.na( proxy_normlized ) ] <- 0 
    global_em_spatial <- proxy_normlized * emissions_value

    # convert into flux 
    global_em_spatial <- global_em_spatial * flux_factor

  } 
  if ( sector == 'TANK' ) { # TANK emissions only exist in iso global
    emissions_value <- gridding_emissions_sector[ gridding_emissions_sector$iso == 'global', paste0( 'X', year ) ] 
    
    mask <- global_mask
	  proxy_weighted <- proxy * mask 
    proxy_normlized <- proxy_weighted / sum( proxy_weighted )
    proxy_normlized[ is.na( proxy_normlized ) ] <- 0 
    global_em_spatial <- proxy_normlized * emissions_value

    # convert into flux 
    global_em_spatial <- global_em_spatial * flux_factor
  }
  if ( sector == 'WST' ) { 
    # special treatment for proxy used for WST -- see wiki for more information 
    pop_density_cutoff <- 1000 / 2.59e+6 
    proxy <- proxy / global_grid_area
    proxy <- ifelse( proxy > pop_density_cutoff, pop_density_cutoff, proxy )
    proxy <- proxy * global_grid_area
    
    iso_list <- gridding_emissions_sector$iso 
    
    iso_em_spatial_list <- lapply( iso_list,
                                   grid_one_iso, 
                                   em, 
                                   sector, 
                                   year, 
                                   grid_resolution, 
                                   gridding_emissions_sector, 
                                   location_index, 
                                   proxy_substitution_mapping,
                                   proxy,
                                   proxy_backup )
    names( iso_em_spatial_list ) <- iso_list  
    
    global_em_spatial <- aggregate_all_isos( iso_list, iso_em_spatial_list, location_index, grid_resolution, flux_factor )
  }
  if ( sector %in% c( 'AGR', 'ELEC', 'ETRN', 'FFFI', 'FLR', 'INDC', 'INPU', 'NRTR', 'RCOO', 'RCORC', 'ROAD', 'SLV' ) ) {
    iso_list <- gridding_emissions_sector$iso 
    
    iso_em_spatial_list <- lapply( iso_list,
                                   grid_one_iso, 
                                   em, 
                                   sector, 
                                   year, 
                                   grid_resolution, 
                                   gridding_emissions_sector, 
                                   location_index, 
                                   proxy_substitution_mapping,
                                   proxy,
                                   proxy_backup )
    names( iso_em_spatial_list ) <- iso_list   
    global_em_spatial <- aggregate_all_isos( iso_list, iso_em_spatial_list, location_index, grid_resolution, flux_factor )
    }

  return( global_em_spatial )
}

# ------------------------------------------------------------------------------
# grid_all_sectors
# Brief: lapply wraper for grid_one_countriy 
# Dependencies: grid_one_sector
# Author: Leyang Feng
# parameters: sector_list - the sectors in the gridding emissions
#             rest see the grid_one_sector
# return: res_list - a list contains grid for each sector
# input files: null
# output: null
grid_all_sectors <- function( sector_list, 
                              em, 
                              grid_resolution, 
                              year,
                              gridding_emissions_xyear, 
                              location_index, 
                              proxy_mapping,
                              proxy_substitution_mapping ) {
  
  res_list <- lapply( sector_list, 
                      grid_one_sector, 
                      em, 
                      grid_resolution,
                      year,
                      gridding_emissions_xyear,
                      location_index, 
                      proxy_mapping, 
                      proxy_substitution_mapping ) 
  names( res_list ) <- paste0( sector_list, '_int_grid' )
  return( res_list )
}

# ------------------------------------------------------------------------------
# grid_one_year
# Brief: Generates one year's gridded emission
# Dependencies: grid_all_sectors 
# Author: Leyang Feng
# parameters: em - the current gridding emission species
#             year - the vurrent gridding year
#             grid_resolution - gridding resolution 
#             location_index - location index table, contains matrix index information for the country in global extent
#             proxy_mapping - proxy mapping file 
#             proxy_substitution_mapping - proxy substitution mapping file 
# return: sector_grids_list - a list contains one year's gridded emission for each sector
# input files: null
# output: null 
grid_one_year <- function( em, 
                           year, 
                           grid_resolution,
                           gridding_emissions, 
                           location_index, 
                           proxy_mapping, 
                           proxy_substitution_mapping ){ 
  
  current_x_year <- paste0( 'X', year )
  gridding_emissions_xyear <- gridding_emissions[ c( 'iso', 'sector', current_x_year ) ]
  
  sector_list <- sort( unique( gridding_emissions_xyear$sector ) )
  
  sector_grids_list <- grid_all_sectors( sector_list, 
                                         em, 
                                         grid_resolution,
                                         year, 
                                         gridding_emissions_xyear, 
                                         location_index, 
                                         proxy_mapping,
                                         proxy_substitution_mapping )
  
  return( sector_grids_list )
}

# ------------------------------------------------------------------------------
# grid_one_year_air
# Brief: Generates one year's gridded emission for sector AIR 
# Dependencies: get_proxy 
# Author: Leyang Feng
# parameters: em - the current gridding emission species
#             year - the current gridding year
#             grid_resolution - gridding resolution  
#             gridding_emissions - emissions df used for gridding
#             proxy_mapping - proxy mapping file
# return: AIR_global_em_spatial - a 3D array of distributed aircraft emissions
# input files: null
# output: null 
grid_one_year_air <- function( em, 
                               year, 
                               grid_resolution,
                               gridding_emissions, 
                               proxy_mapping ) { 
  
  current_x_year <- paste0( 'X', year )
  gridding_emissions_xyear <- gridding_emissions[ c( 'iso', 'sector', current_x_year ) ]
  
  global_grid_area <- grid_area( grid_resolution, all_lon = T )
  flux_factor <- 1000000 / global_grid_area / ( 365 * 24 * 60 * 60 )
  
  emissions_value <- unlist( gridding_emissions_xyear[ current_x_year ] )
  
  if ( emissions_value <= 0 ) { 
    AIR_global_em_spatial <- array( 0, dim = c( 180 / grid_resolution, 360 / grid_resolution, 25 ) ) 
  } else {
      proxy <- get_proxy( em, year, 'AIR', proxy_mapping, proxy_type = 'primary' )
      proxy_norm <- proxy / sum( proxy )
      AIR_global_em_spatial <- proxy_norm * emissions_value
      
      # convert to flux
      flux_factor_array <- array( rep( as.vector( flux_factor ), dim( AIR_global_em_spatial )[ 3 ]), dim = dim( AIR_global_em_spatial ) )
      AIR_global_em_spatial <- AIR_global_em_spatial * flux_factor_array 
  }

  return( AIR_global_em_spatial )
}

# ==============================================================================
# proxy functions
# ------------------------------------------------------------------------------
# get_proxy
# Brief: loads proxy for a emission species, year, and sector combination 
#        The function first find the proper proxy file for a emission species, year, sector combination
#        then read the proxy file from disk and load into current environment.  
#        Proxy mapping should be loaded into environment before calling the function 
# Dependencies: 
# Author: Leyang Feng
# parameters: em - the current gridding emission species
#             year - the current gridding year
#             sector - the current gridding sector  
#             proxy_mapping - proxy mapping file 
#             proxy_type - the type of proxy wanted to be load; valid options are 'promary' or another charactor
# return: proxy - the desired proxy 
# input files: null
# output: null
get_proxy <- function( em, year, sector, proxy_mapping, proxy_type = 'primary' ) {
    
    if ( proxy_type == 'primary' ) { 
      file_name <- proxy_mapping[ proxy_mapping$em == em & proxy_mapping$sector == sector & proxy_mapping$year == year, 'proxy_file' ]
      } else { 
      file_name <- proxy_mapping[ proxy_mapping$em == em & proxy_mapping$sector == sector & proxy_mapping$year == year, 'proxybackup_file' ] 
      }
    
    main_dir_file_list <- list.files( proxy_dir )
    if ( file_name %in% main_dir_file_list ) { 
      load( paste0( proxy_dir, file_name ) )
      proxy <- get( file_name )
      rm( list = file_name )
    } else { 
      load( paste0( proxy_backup_dir, file_name ) )
      proxy <- get( file_name )
      rm( list = file_name )        
    }
    
    return( proxy )
}

# ==============================================================================
# seasonality functions
# ------------------------------------------------------------------------------
# add_seasonality
# Brief: add seasoanlity to annual flux grids; different stradegies are adapted for different sectors 
# Dependencies: 
# Author: Leyang Feng
# parameters: annual_flux - the annual flux matrix that needs to add seasonality  
#             em - the current gridding emission species
#             sector - the current gridding sector
#             year - the current gridding year
#             days_in_month - days in month in a year, passed by upper layer function
#             grid_resolution - gridding resolution 
#             seasonality_mapping - seasonality mapping file 
# return: monthly_array - flux grids for each month 
# input files: null
# output: null
add_seasonality <- function( annual_flux, em, sector, year, days_in_month, grid_resolution, seasonality_mapping ) { 
  
  file_name <- seasonality_mapping[ seasonality_mapping$em == em & seasonality_mapping$sector == sector & seasonality_mapping$year == year, 'seasonality_file' ]
  
  if ( file_name %in% common_seasonality_list ) { # common_seasonality_list exsists in the global environment
    sea_fracs <- get( file_name ) 
    } else {
      load( paste0( seasonality_dir, file_name  ) )
      sea_fracs <- get( file_name )
      rm( list = file_name )
    }
  
  month_list <- 1 : 12 
  common_dim <- c( 180 / grid_resolution, 360 / grid_resolution, length( month_list ) )
  month_array <- array( unlist( lapply( days_in_month, rep, 360 * 720 ) ) , dim = common_dim )
  
  storage_array <- array( dim = common_dim ) 
  
  if ( sector == 'SHP' ) { 
    for ( i in month_list ) { 
      storage_array[ , , i ] <- annual_flux * sea_fracs[ , , i ] * 12
    }
  }
  if ( sector == 'AIR' ) { 
    storage_array <- array( dim = dim( sea_fracs ) ) 
    for ( i in month_list ) { 
      storage_array[ , , , i ] <- annual_flux * sea_fracs[ , , , i ] * 12
    }
  }
  if ( sector %in% c( 'AGR', 'ENE', 'IND', 'TRA', 'RCORC', 'RCOO', 'SLV', 'WST' ) ) { 
    sea_adj <- 365 / apply( sea_fracs * month_array * 12, c( 1, 2 ), sum ) 
    for ( i in month_list ) { 
      storage_array[ , , i ] <- annual_flux * sea_fracs[ , , i ] * 12 * sea_adj
    }
  }
  
  monthly_array <- storage_array  
  
  return( monthly_array )    
}

# ------------------------------------------------------------------------------
# sum_monthly_em
# Brief: calculate monthly emissions in kt from emission flux grids with seasonality; different stradegies are adapted for different sectors 
# Dependencies: 
# Author: Leyang Feng
# parameters: fin_grid - flux grid with seasonality 
#             em - the current gridding emission species
#             sector - the current gridding sector
#             year - the current gridding sector 
#             days_in_month - days in month in a year, passed by upper layer function
#             global_grid_area - grid area matrix, passed by upper layer function
#             seasonality_mapping - seasonality mapping file 
# return: monthly_em - a data frame of monthly emissions value in kt
# input files: null
# output: null
sum_monthly_em <- function( fin_grid, em, sector, year, days_in_month, global_grid_area, seasonality_mapping ) { 
  
  file_name <- seasonality_mapping[ seasonality_mapping$em == em & seasonality_mapping$sector == sector & seasonality_mapping$year == year, 'seasonality_file' ]
  
  if ( file_name %in% common_seasonality_list ) { # common_seasonality_list exsists in the global environment
    sea_fracs <- get( file_name ) 
    } else {
      load( paste0( seasonality_dir, file_name  ) )
      sea_fracs <- get( file_name )
      rm( list = file_name )
    }
  
  month_list <- 1 : 12 
  common_dim <- c( 180 / grid_resolution, 360 / grid_resolution, length( month_list ) )
  month_array <- array( unlist( lapply( days_in_month, rep, 360 * 720 ) ) , dim = common_dim )
  
  if ( sector == 'SHP' ) { 
    monthly_em_list <- lapply( month_list, function( i ) { 
      month_flux <- fin_grid[ , , i ]
      month_mass <- month_flux * global_grid_area * days_in_month[ i ] * 24 * 60 * 60  
      month_mass_value <- sum( month_mass, na.rm = T )
      month_mass_value <- month_mass_value * 0.000001 # from kg to kt
      out_df <- data.frame( em = em, sector = sector, year = year, month = i, units = 'kt', value = month_mass_value, stringsAsFactors = F  )
      } )
    monthly_em <- do.call( 'rbind', monthly_em_list )
  }
  if ( sector == 'AIR' ) { 
    monthly_em_list <- lapply( month_list, function( i ) { 
      month_flux <- fin_grid[ , , , i ]
      flux2mass_factor <- array( rep( as.vector( global_grid_area ), dim( month_flux )[ 3 ] ), dim = dim( month_flux ) ) * days_in_month[ i ] * 24 * 60 * 60 
      month_mass <- month_flux * flux2mass_factor
      month_mass_value <- sum( month_mass, na.rm = T )
      month_mass_value <- month_mass_value * 0.000001 # from kg to kt
      out_df <- data.frame( em = em, sector = sector, year = year, month = i, units = 'kt', value = month_mass_value, stringsAsFactors = F  )
      } )
    monthly_em <- do.call( 'rbind', monthly_em_list )
  }
  if ( sector %in% c( 'AGR', 'ENE', 'IND', 'TRA', 'RCORC', 'RCOO', 'SLV', 'WST' ) ) { 
    sea_adj <- 365 / apply( sea_fracs * month_array * 12, c( 1, 2 ), sum ) 
    monthly_em_list <- lapply( 1 : 12, function( i ) { 
      month_flux <- fin_grid[ , , i ]
      month_mass <- month_flux * global_grid_area * days_in_month[ i ] * 24 * 60 * 60
      month_mass_value <- sum( month_mass, na.rm = T )
      month_mass_value <- month_mass_value * 0.000001 # from kg to kt
      out_df <- data.frame( em = em, sector = sector, year = year, month = i, units = 'kt', value = month_mass_value, stringsAsFactors = F  )
      } )
    monthly_em <- do.call( 'rbind', monthly_em_list )
    }
  
  return( monthly_em )    
}

# ==============================================================================
# other supporting functions
# ------------------------------------------------------------------------------
# year_length
# Brief: return how many days in a given year. The function could be useful if the leap year is considered. 
# Dependencies: null
# Author: Leyang Feng
# parameters: year    
# return: 365 or 366 
# input files: null
# output: null 
year_length <- function( year ) {
  year <- as.numeric( year ) 
  ifelse( ( year %% 4 == 0 & year %% 100 != 0) | year %% 400 == 0, 366, 365 ) 
  }
  
# ------------------------------------------------------------------------------
# grid_area
# Brief: Compute areas of a column of grid cells for all latitude at 
#        desired resolution in square meters   
# Dependencies: null 
# Author: Leyang Feng
# parameters:  grid_resolution - the resolution of desired grid cell
#              all_lon - by default the function only return a column of areas since 
#                        the grid cell areas will be only changing along latitude. 
#                        if TRUE, the function retures a grid cell area as a matrix 
#                        as global extent                
# return: grid_cell_areas 
# input files: null
# output: null
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
# Dependencies: null 
# Author: Leyang Feng
# parameters: x - the matrix to be fliped  
# return: a fliped matrix 
# input files: null
# output: null
flip_a_matrix <- function( x ) {
  apply( x, 2, rev )
}

# ------------------------------------------------------------------------------
# gridding_initialize
# Brief: Initializing and load the necessary parameters and variables needed by gridding routine. 
#        Should be called at the begining of the gridding routine.  
# Dependencies: null 
# Author: Leyang Feng
# parameters: grid_resolution - grid resolution defined by user.
#             start_year - start year of gridding 
#             end_year - end year of gridding 
#             load_masks - load all country masks into global environment if TRUE
#             load_seasonality_profile - load all seasonality profile into global environment if TRUE ( because those all commonly used )
# return: null 
# input files: null
# output: null 
gridding_initialize <- function( grid_resolution = 0.5,
                                 start_year = 1970,
                                 end_year = 2014,
                                 load_masks = T, 
                                 load_seasonality_profile = T
                                 ){
  printLog( 'initializing gridding...' )
  
  # set up basics
  grid_resolution <<- grid_resolution 
  printLog( paste0( 'Processing resolution: ', grid_resolution ) )
  
  start_year <- start_year
  if ( em == 'CH4' ) { start_year <- 1970 }
  end_year <- end_year
  year_list <<- seq( start_year, end_year )
  printLog( paste0( 'Gridding from year ', start_year, ' to year ', end_year ) )
  
  # load country masks 
  if ( load_masks ) {
    mask_list <- list.files( mask_dir, pattern = '.*_mask' )
    invisible( lapply( mask_list, function( mask_list ) { load( paste0( mask_dir, mask_list), .GlobalEnv ) } ) )
    printLog( 'Country mask initialized' )
  }
  
  # load seasonality profile
  if ( load_seasonality_profile ) { 
    common_seasonality_list <<- c( "AGR_NH3_seasonality", "AGR_seasonality", "ENE_seasonality", 
                                  "IND_seasonality", "RCOO_seasonality", "RCORC_seasonality", 
                                  "SHP_BC_seasonality", "SHP_CO_seasonality", "SHP_NH3_seasonality", 
                                  "SHP_NMVOC_seasonality", "SHP_NOx_seasonality", "SHP_OC_seasonality", 
                                  "SHP_SO2_seasonality", "SLV_seasonality", "TRA_seasonality", "WST_seasonality"  ) 
    invisible( lapply( common_seasonality_list, function( common_seasonality ) { load( paste0( seasonality_dir, common_seasonality ), .GlobalEnv ) } ) )
    printLog( 'Seasonality profile initialized' )
    }
}


