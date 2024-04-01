#-----------------------------------------------------------------------------------------------------------------
# Program Name: gridding_functions.R
# Author's Name: Leyang Feng
# Date Last Modified: June 30 2016
# Program Purpose: Core functions for emissions gridding routine
# Note: 1. Brief introduction of how things work in gridding routine:
#          There are three steps for gridding:
#          (1) the emissions are gridded at country level then aggregated to
#          form a global grids. The core functions for this step are
#          grid_one_iso and aggregate_all_isos.
#          (2) Then the first step is repeated for all sectors. The core
#          function for this step is tor and its wraper grid_all_sectors.
#          (3) Then the list contains global grids for all sectors is returned
#          by grid_one_year.
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
library( 'ncdf4' )
library( 'sp' )
library( 'geosphere' )
library( 'Matrix' )
library( 'yaml' )

# ==============================================================================
# core gridding functions
# ------------------------------------------------------------------------------
# grid_one_iso
# Brief: Generates an iso's emission spatial distribution using proxy.
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

  #======================================================
  # # DEBUGGING
  # if(sum(norm_weighted_proxy) == 0){
  #   printLog(paste0('WARNING, UNFLAGGED ZERO PROXY: ', iso, ', ', sector, ', ', em))
  #   printLog('USING PROXY BACKUP UNEXPECTEDLY')
  #   proxy_cropped <- proxy_backup[ start_row : end_row, start_col : end_col ]
  #   weighted_proxy <- proxy_cropped * mask
  #   norm_weighted_proxy <- weighted_proxy / sum( weighted_proxy )
  #   norm_weighted_proxy[ is.nan( norm_weighted_proxy ) ] <- 0
  # }
  #======================================================

  emissions_value <- gridding_emissions_sector[ gridding_emissions_sector$iso == iso, paste0( 'X', year ) ]

  iso_em_spatial <- emissions_value * norm_weighted_proxy

  #======================================================
  # # DEBUGGING
  proxy_cropped <- proxy[ start_row : end_row, start_col : end_col ]
  weighted_proxy <- proxy_cropped * mask
  norm_weighted_proxy <- weighted_proxy / sum( weighted_proxy )
  norm_weighted_proxy[ is.nan( norm_weighted_proxy ) ] <- 0

  proxy_cropped_backup <- proxy_backup[ start_row : end_row, start_col : end_col ]
  weighted_proxy_backup <- proxy_cropped_backup * mask
  norm_weighted_proxy_backup <- weighted_proxy_backup / sum( weighted_proxy_backup )
  norm_weighted_proxy_backup[ is.nan( norm_weighted_proxy_backup ) ] <- 0

  sum_spatial <- sum(iso_em_spatial)
  if( typeof(all.equal(emissions_value, sum_spatial)) == 'character' ){
    print(paste0(iso, ' ,', em, ' ,', sector, ' ,', year, ' has total emisions: ', emissions_value, ' but distributed emissions: ', sum(iso_em_spatial), 
                '. Normalized proxy sum: ', sum(norm_weighted_proxy), ', and normalized proxy-backup sum: ', sum(norm_weighted_proxy_backup)))
  }
  #======================================================
  


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
#        This function contains several different gridding strategies for different sectors after a few common steps.
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
#           (1) process the proxy (fixed to population -- see get_proxy()) as rural population
#           (2) extract iso_list
#           (3) call grid_one_iso for iso_list
#           (4) call aggregate_all_isos to get global grid
#         for all other sectors:
#           (1) extract iso_list
#           (2) call grid_one_iso for iso_list
#           (3) call aggregate_all_isos to get global grid
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
                             global_grid_area,
                             year,
                             gridding_emissions_xyear,
                             location_index,
                             proxy_mapping,
                             proxy_substitution_mapping,
                             proxy_files ) {

  gridding_emissions_sector <- gridding_emissions_xyear[ gridding_emissions_xyear$sector == sector, ]

  proxy <- get_proxy( em, year, sector, proxy_mapping, proxy_files, proxy_type = 'primary' )
  proxy_backup <- get_proxy( em, year, sector, proxy_mapping, proxy_files, proxy_type = 'backup' )

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

  } else if ( sector == 'TANK' ) {
    # TANK emissions only exist in iso global
    emissions_value <- gridding_emissions_sector[ gridding_emissions_sector$iso == 'global', paste0( 'X', year ) ]

    #========================================================
    # DEBUGGING
    # temp <- gridding_emissions %>%
    #   dplyr::filter(sector == 'TANK') %>%
    #   dplyr::mutate(sum_all_years = rowSums(paste0('X', year_list)))
    #========================================================

    mask <- global_mask
	  proxy_weighted <- proxy * mask
    proxy_normlized <- proxy_weighted / sum( proxy_weighted )
    proxy_normlized[ is.na( proxy_normlized ) ] <- 0
    global_em_spatial <- proxy_normlized * emissions_value

    # convert into flux
    global_em_spatial <- global_em_spatial * flux_factor

  }else {
    stopifnot(sector %in% c( 'AGR', 'ELEC', 'ETRN', 'FFFI', 'FLR', 'INDC', 'INPU', 'NRTR', 'RCOO', 'RCORC', 'ROAD', 'SLV', 'WST' ) )
    iso_list <- gridding_emissions_sector$iso

    iso_em_spatial_list <- lapply( iso_list,
                                   grid_one_iso,
                                   em,
                                   sector,
                                   year,
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
                              global_grid_area,
                              year,
                              gridding_emissions_xyear,
                              location_index,
                              proxy_mapping,
                              proxy_substitution_mapping,
                              proxy_files ) {

  res_list <- lapply( sector_list,
                      grid_one_sector,
                      em,
                      grid_resolution,
                      global_grid_area,
                      year,
                      gridding_emissions_xyear,
                      location_index,
                      proxy_mapping,
                      proxy_substitution_mapping,
                      proxy_files )
  names( res_list ) <- paste0( sector_list, '_int_grid' )
  return( res_list )
}

# ------------------------------------------------------------------------------
# grid_one_year
# Brief: Generates one year's gridded emission
# Dependencies: grid_all_sectors
# Author: Leyang Feng
# parameters: year - the current gridding year
#             em - the current gridding emission species
#             grid_resolution - gridding resolution
#             location_index - location index table, contains matrix index information for the country in global extent
#             proxy_mapping - proxy mapping file
#             proxy_substitution_mapping - proxy substitution mapping file
# return: sector_grids_list - a list contains one year's gridded emission for each sector
# input files: null
# output: null
grid_one_year <- function( year,
                           em,
                           grid_resolution,
                           gridding_emissions,
                           location_index,
                           proxy_mapping,
                           proxy_substitution_mapping,
                           proxy_files ){

  current_x_year <- paste0( 'X', year )
  gridding_emissions_xyear <- gridding_emissions[ c( 'iso', 'sector', current_x_year ) ]

  sector_list <- sort( unique( gridding_emissions_xyear$sector ) )

  global_grid_area <- grid_area( grid_resolution, all_lon = T )

  sector_grids_list <- grid_all_sectors( sector_list,
                                         em,
                                         grid_resolution,
                                         global_grid_area,
                                         year,
                                         gridding_emissions_xyear,
                                         location_index,
                                         proxy_mapping,
                                         proxy_substitution_mapping,
                                         proxy_files )

  return( sector_grids_list )
}

# ------------------------------------------------------------------------------
# grid_one_year_air
# Brief: Generates one year's gridded emission for sector AIR
# Dependencies: get_proxy
# Author: Leyang Feng
# parameters: year - the current gridding year
#             em - the current gridding emission species
#             grid_resolution - gridding resolution
#             gridding_emissions - emissions df used for gridding
#             proxy_mapping - proxy mapping file
# return: AIR_global_em_spatial - a 3D array of distributed aircraft emissions
# input files: null
# output: null
grid_one_year_air <- function( year,
                               em,
                               grid_resolution,
                               gridding_emissions,
                               proxy_mapping,
                               proxy_files ) {

  current_x_year <- paste0( 'X', year )
  gridding_emissions_xyear <- gridding_emissions[ c( 'iso', 'sector', current_x_year ) ]

  global_grid_area <- grid_area( grid_resolution, all_lon = T )
  flux_factor <- 1000000 / global_grid_area / ( 365 * 24 * 60 * 60 )

  emissions_value <- unlist( gridding_emissions_xyear[ current_x_year ] )

  if ( emissions_value <= 0 ) {
    AIR_global_em_spatial <- array( 0, dim = c( 180 / grid_resolution, 360 / grid_resolution, 25 ) )
  } else {
    proxy <- get_proxy( em, year, 'AIR', proxy_mapping, proxy_files, proxy_type = 'primary' )
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
get_proxy <- function( em, year, sector, proxy_mapping, proxy_files, proxy_type = 'primary' ) {

  # use VOC proxy files for all sub-VOCs
  if ( em %!in% proxy_mapping$em ) {
    stop( paste( 'Could not find proxy mapping for emission', em ) )
  }

  proxy_info <- proxy_mapping %>%
    dplyr::filter( em == !!em, sector == !!sector, year == !!year )

  stopifnot( nrow( proxy_info ) == 1 )

  if ( proxy_type == 'primary' ) {
    file_name <- proxy_info$proxy_file
    file_name_re <- paste0( "^", file_name )

    proxy_root <- proxy_dir
    proxy_file <- grep( file_name_re, proxy_files$primary, value = T )
  }

  # If we want a backup proxy, or the primary proxy file can't be found,
  # look for a backup
  if ( proxy_type != 'primary' || length( proxy_file ) == 0 ) {
    file_name <- proxy_info$proxybackup_file
    file_name_re <- paste0( "^", file_name )

    proxy_root <- proxy_backup_dir
    proxy_file <- grep( file_name_re, proxy_files$backup, value = T )
  }

  # Check that we found one, and exactly one, proxy file
  if ( length( proxy_file ) != 1 ) {
    stop( paste( length( proxy_file ), "proxy files found in", proxy_root,
                 "for", em, year, sector ) )
  }

  proxy_var_name <- load( paste0( proxy_root, proxy_file ) )

  get( proxy_var_name )
}

# ==============================================================================
# seasonality functions
# ------------------------------------------------------------------------------
# add_seasonality
# Brief: add seasonality to annual flux grids; different strategies are adapted for different sectors
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

  # use VOC seasonality mapping for all sub-VOCs
  if ( em %!in% seasonality_mapping$em ) {
    VOC_burn_ratios <- readData( 'GRIDDING', 'VOC_ratio_BurnSectors',
                                 domain_extension = "gridding-mappings/" )
    VOC_ratios <- readData( 'GRIDDING', 'VOC_ratio_AllSectors',
                            domain_extension = "gridding-mappings/" )

    if ( em %in% c( names( VOC_ratios ), names( VOC_burn_ratios ) ) ) {
      em <- 'NMVOC'
    } else {
      stop( paste( 'Could not find proxy mapping for emission', em ) )
    }
  }

  file_name <- seasonality_mapping %>%
    dplyr::filter( em == !!em, sector == !!sector, year == !!year ) %>%
    dplyr::pull( 'seasonality_file' ) %>%
    grep( list.files( seasonality_dir ), value = T )

  sea_frac_var_name <- sub( '.Rd$', '', file_name )

  # common_seasonality_list exists in the global environment
  if ( file_name %!in% common_seasonality_list ) {
    load( paste0( seasonality_dir, file_name ) )
  }

  # Seasonality profile at 0.5 deg resolution
  sea_fracs <- get( sea_frac_var_name )

  #rm(list = sea_frac_var_name, envir = .GlobalEnv)

  # Dimensions of output grid
  month_list <- 1 : 12
  common_dim <- c( 180 / grid_resolution, 360 / grid_resolution, length( month_list ) )

  # Ratio of output grid to seasonality grid
  norm_dim <- common_dim[1] / dim(sea_fracs)[1]

  # If ratio not 1-1, need to create seasonality grid at correct resolution
  if( norm_dim != 1 ){
      sea_fracs_adj <- array( dim = common_dim )
      for( i in 1:common_dim[1] ){
          for( j in 1:common_dim[2] ){
              for( k in 1:common_dim[3] ){
                  sea_fracs_adj[i,j,k] <- sea_fracs[ceiling(i/norm_dim),ceiling(j/norm_dim),k]
              }
          }
      }

      # Replace seasonality profile with adjusted resolution grid
      sea_fracs <- sea_fracs_adj
  }

  # Empty output grid
  storage_array <- array( dim = common_dim )

  if ( sector == 'SHP' ) {
    storage_array <- sweep(sea_fracs, c(1, 2), annual_flux * 12, `*`)
  }
  if ( sector == 'AIR' ) {
    storage_array <- array( dim = dim( sea_fracs ) )
    for ( i in month_list ) {
      storage_array[ , , , i ] <- annual_flux * sea_fracs[ , , , i ] * 12
    }
  } else if ( sector %in% c( 'AGR', 'ENE', 'IND', 'TRA', 'RCORC', 'RCOO', 'SLV', 'WST' ) ) {
    sea_adj <- 365 / rowSums( sweep( sea_fracs, 3, days_in_month, `*`) * 12, dims = 2)
    storage_array <- sweep(sea_fracs, c(1, 2), annual_flux * sea_adj * 12, `*`)
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

  file_name <- seasonality_mapping %>%
      dplyr::filter( em == !!em, sector == !!sector, year == !!year ) %>%
      dplyr::pull( 'seasonality_file' ) %>%
      grep( list.files( seasonality_dir ), value = T )

  sea_frac_var_name <- sub( '.Rd$', '', file_name )

  # common_seasonality_list exists in the global environment
  if ( file_name %!in% common_seasonality_list ) {
      load( paste0( seasonality_dir, file_name ) )
  }

  sea_fracs <- get( sea_frac_var_name )

  month_list <- 1 : 12

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
# Dependencies: sp::GridTopology, geosphere::areaPolygon
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
# rotate_a_matrix
# Brief: rotate the input matrix 90 degrees clockwise (must be 2D)
# Dependencies: null
# Author: Caleb Braun
# parameters: x - the matrix to be rotated
# return: a rotated matrix
# input files: null
# output: null
rotate_a_matrix <- function( x, n = 1 ) {
  switch(1 + (n %% 4),
         x,
         t( x[ nrow(x):1, ] ),
         x[ nrow(x):1, ncol(x):1 ],
         t( x[ , ncol(x):1 ] )
  )
}


# ------------------------------------------------------------------------------
# rotate_lat_lon
# Brief: Rotate the first two dimensions (lon, lat) of a list containing arrays
#        or matrices. If given a list of lists, this function recurses until it
#        finds the arrays. All arrays in a list must have the same dimensions.
# Author: Caleb Braun
# parameters: grids - a list containing arrays or matrices
# return: a rotated 4d array
rotate_lat_lon <- function( grids, direction = 1 ) {
  stopifnot( is.list( grids ) )

  # If given list of lists, call recursively on sub-lists
  if ( !all( sapply( grids, is.array ) ) ) {
    return( lapply( grids, rotate_lat_lon, direction ) )
  }

  # Get array dimensions and ensure they are all the same
  grid_dims <- lapply( grids, dim )
  stopifnot( length( unique( grid_dims ) ) == 1 )
  grid_dims <- grid_dims[[1]]
  ndims <- length( grid_dims )
  stopifnot( ndims > 1 )

  if ( ndims == 2 ) {
    final_grids <- lapply( grids, rotate_a_matrix, direction )
  } else {
    final_grids <- lapply( grids, apply, 3:ndims, rotate_a_matrix, direction )
  }

  # Rotate first two dimensions as well and return
  lapply( final_grids, `dim<-`, c( grid_dims[ 2:1 ], grid_dims[ -1:-2 ] ) )
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
# TODO: Avoid the inferno, (http://www.burns-stat.com/pages/Tutor/R_inferno.pdf)
#       see chapter 6
gridding_initialize <- function( grid_resolution = 0.5,
                                 start_year = 1970,
                                 end_year = end_year,
                                 load_masks = T,
                                 load_seasonality_profile = T
                                 ){
  printLog( 'Initializing gridding...' )

  # set up basics
  grid_resolution <<- grid_resolution
  printLog( paste( 'Processing resolution:', grid_resolution ) )

  if ( em == 'CH4' ) { start_year <- 1970 }
  year_list <<- seq( start_year, end_year )
  printLog( paste( 'Gridding from year', start_year, 'to year', end_year ) )

  # load country masks
  if ( load_masks ) {
    mask_list <- list.files( mask_dir, pattern = '.*_mask' )
    invisible( lapply( mask_list, function( mask_list ) { load( paste0( mask_dir, mask_list), .GlobalEnv ) } ) )
    printLog( 'Country mask initialized' )
  }

  # load seasonality profile
  if ( load_seasonality_profile ) {
    common_seasonality_list <<- list.files( seasonality_dir, pattern = ".*_seasonality" )
    common_seasonality_list <<- grep( 'AIR', common_seasonality_list, invert = T, value = T )

    invisible( lapply( common_seasonality_list, function( common_seasonality ) {
      load( paste0( seasonality_dir, common_seasonality ), .GlobalEnv )
    } ) )
    printLog( 'Seasonality profile initialized' )
  }
}


# ------------------------------------------------------------------------------
# extendProxyMapping
# Brief: Extend pre-existing proxy mapping to last year needed by copying last year of data
# Dependencies: null
# Author: Steve Smith, Patrick O'Rourke
# parameters: proxy_mapping - proxy mapping dataframe
# return: extended proxy mapping dataframe
# input files: null
# output: null
# NOTE: Code assumes that current mapping file is complete for all years in the last year provided
# TODO: See if extendProxyMapping and extendSeasonalityMapping can be combined using scoped function variants
extendProxyMapping <- function( a_proxy_mapping ) {
  last_proxy_data_year <- as.numeric( max( a_proxy_mapping$year ) )
  last_proxy_data_year_string <- paste( last_proxy_data_year )

  extra_years_needed <- (last_proxy_data_year+1):end_year
  extra_years_needed_string <- paste( extra_years_needed )

  final_years <- 1750:end_year
  final_years_string <- paste( final_years )

  a_proxy_mapping_temp <- a_proxy_mapping %>%
    tidyr::spread( year, proxybackup_file ) %>%
    dplyr::mutate_at( extra_years_needed_string, funs( identity (  !!rlang::sym( last_proxy_data_year_string ) ) ) ) %>%
    tidyr::gather( key = year, value = proxybackup_file, all_of(final_years_string) ) %>%
    dplyr::arrange( em, sector, year, proxy_file ) %>%
    dplyr::select( em, sector,  year, proxy_file, proxybackup_file ) %>%
    dplyr::filter( !is.na( proxybackup_file ) )

  return(a_proxy_mapping_temp)
}

# ------------------------------------------------------------------------------
# extendSeasonalityMapping
# Brief: Extend pre-existing seasonality mapping to last year needed by copying last year of data
# Dependencies: null
# Author: Steve Smith, Patrick O'Rourke
# parameters: seasonality_mapping - seasonality mapping dataframe
# return: extended seasonality mapping dataframe
# input files: null
# output: null
# NOTE: Code assumes that current mapping file is complete for all years in the last year provided
# TODO: See if extendProxyMapping and extendSeasonalityMapping can be combined using scoped function variants
extendSeasonalityMapping <- function( a_seasonality_mapping ) {
  last_seasonality_data_year <- as.numeric( max( a_seasonality_mapping$year ) )
   last_seasonality_data_year_string <- paste( last_seasonality_data_year )

  extra_years_needed <- (last_seasonality_data_year+1):end_year
  extra_years_needed_string <- paste( extra_years_needed )

  final_years <- 1750:end_year
  final_years_string <- paste( final_years )

  a_seasonality_mapping <- a_seasonality_mapping %>%
    tidyr::spread( year, seasonality_file ) %>%
    dplyr::mutate_at( extra_years_needed_string, funs( identity (  !!rlang::sym( last_seasonality_data_year_string ) ) ) ) %>%
    tidyr::gather( key = year, value = seasonality_file, all_of(final_years_string) ) %>%
    dplyr::arrange( em, sector, year, seasonality_file ) %>%
    dplyr::select( em, sector,  year, seasonality_file ) %>%
    dplyr::filter( !is.na( seasonality_file ) )

  return( a_seasonality_mapping )
}


# ------------------------------------------------------------------------------
# generate_rural_population_proxy
# Brief: Extend pre-existing seasonality mapping to last year needed by copying last year of data
# Dependencies: Population proxy file population_<year>.Rd
# Author: Noah Prime
# parameters: year - year for which to generate capped rural population proxy
#             global_grid_area - matrix defined by the gridding resolution
# return: null
# input files: null
# output files: saves capped_rural_population_<year>.Rd to proxy directory
# NOTE: Code assumes that population proxy already exists for the given year
generate_rural_population_proxy <- function( year, global_grid_area, proxy_dir, backup_proxy_dir ){
    # loading population proxy files
    population_proxy_file <- paste0('population_',year,'.Rd')
    population_proxy <- get( load( paste0( backup_proxy_dir,population_proxy_file ) ) )

    # altering population proxy to be rural population proxy
    pop_density_cutoff <- 1000 / 2.59e+6 # Rural population density as defined by U.S. Department of Health and Human Services (see wiki)
    population_proxy <- population_proxy / global_grid_area # Note: global_grid_area defined in nc_generation_functions.R
    population_proxy <- ifelse( population_proxy > pop_density_cutoff, pop_density_cutoff, population_proxy )
    population_proxy <- population_proxy * global_grid_area

    # write new proxy files into proxy and backup-proxy
    save( population_proxy, file = paste0( proxy_dir, 'capped_rural_population_', year, '.Rd') )

}



# ------------------------------------------------------------------------------
# coordinates_to_index
# Brief: Converts lat,lon coordinates to the row an column of a grid of given
#        resolution
# Dependencies:
# Author: Noah Prime
# parameters:
#   - lat - latitude
#   - lon - longitude
#   - res - grid resolution
# return: row and column index
# input files: Null
# output files: Null
coordinates_to_index <- function( lat, lon, res = 0.1 ){
    # Check that lat, lon are within correct range
    if( !((lat >= -90) & (lat <= 90) & (lon >= -180) & (lon <= 180)) ){
        stop( 'Latitude must be between -90,90 and Longitude between -180,180' )
    }

    # List of possible decimal values at this resolution
    dec_list <- seq( res/2, 1, res )

    # Max row and column at given res
    max_row <- 180 / res
    max_col <- 360 / res

    # Decimal places of input lat, lon
    lat_dec <- abs( lat - trunc(lat) )
    lon_dec <- abs( lon - trunc(lon) )

    # Robust way of comparing floating points rather than using '=='
    elementwise.all.equal <- Vectorize(function(x, y) {isTRUE(all.equal(x, y))})

    # Get closest decimal values (could be tied if on border)
    lat_opts <- which( elementwise.all.equal( min(abs(dec_list-lat_dec)), abs(dec_list-lat_dec) ) )
    lon_opts <- which( elementwise.all.equal( min(abs(dec_list-lon_dec)), abs(dec_list-lon_dec) ) )

    # If on edge of latitude, take cell above
    if( lat < 0 ){
        lat_dec_new <- dec_list[ min(lat_opts) ]
    }else{
        lat_dec_new <- dec_list[ max(lat_opts) ]
    }

    # If on edge of longitude, take cell to the right
    if( lon < 0 ){
        lon_dec_new <- dec_list[ min(lon_opts) ]
    }else{
        lon_dec_new <- dec_list[ max(lon_opts) ]
    }

    # Final Latitude
    lat_fin <- sign( lat ) * ( abs( trunc( lat ) ) + lat_dec_new )

    # Final Longitude
    lon_fin <- sign( lon ) * ( abs( trunc( lon ) ) + lon_dec_new )

    # First cell lat for given res
    first_lat <- 90 - res/2

    # First cell lon for given res
    first_lon <- -180 + res/2

    # Cell row
    row <- ( ( first_lat - lat_fin ) / res + 1 ) %% max_row
    if( row == 0 ){
        row <- max_row
    }

    # Cell column
    col <- ( ( lon_fin - first_lon ) / res + 1 ) %% max_col
    if( col == 0 ){
        col <- max_col
    }

    return( c( round(row), round(col) ) )
}


# ------------------------------------------------------------------------------
# TODO:
#   Either add a new function: add_point_sources_subVOC, or alter this function
#   so to add NMVOC point source data to a grid at the ratio of a given subVOC
#   species.
# add_point_sources
# Brief: Add point sources to grid for given gridding sector, year,
# emission species, and gridding resolution
# Dependencies:
# Author: Noah Prime
# parameters:
#   - grid - Grid to add point source emission to
#   - em - Emission species
#   - sector - CEDS final gridding sector
#   - year
#   - grid_resolution
# return: Grid with point sources added
# input files: Point source files if found.
# output files: Null
add_point_sources <- function( grid, em, sector, year, grid_resolution,
                               CEDS_sector_mapping, CEDS_EDGAR_sector_mapping, ps_df ){

    # If no sources, return grid
    if(nrow(ps_df) == 0){ return(grid) }

    # Get list of CEDS sectors included in this gridding sector
    ceds_sectors <- CEDS_sector_mapping %>%
        dplyr::filter( CEDS_final_gridding_sector_short == sector ) %>%
        dplyr::select( CEDS_working_sector ) %>%
        unique() %>%
        pull()

    # Get every point source in this gridding sector. Take just the lat,lon and emission
    # for the given year
    emissions <- ps_df %>%
        dplyr::filter( CEDS_sector %in% ceds_sectors ) %>%
        dplyr::select( latitude, longitude, paste0('X',year) )


    # Return if no relevant point sources
    if(nrow(emissions) == 0){ return(grid) }

    # get global grid area object for given resolution
    global_grid_area <- grid_area( grid_resolution )

    # row, col of each emission
    coords <- t( apply(emissions, 1, function(x){coordinates_to_index(x[1],x[2],grid_resolution)}) )

    # emission converted to flux
    flux <- ( emissions[,paste0('X',year)]  * 1000000 ) / ( global_grid_area[coords[,1]] * 365 * 24 * 60 * 60 )

    # Create sparse matrix of point sources
    sparseMat <- sparseMatrix( i = coords[,1], j = coords[,2], x = flux, dims = dim(grid) )

    # Add sources to grid
    grid <- grid + as.matrix( sparseMat )

    return( grid )
}


# ------------------------------------------------------------------------------
# load_point_source_attributes
# Brief: Load in yml files for all point sources from specified species
#        but does not load time-series emissions. Returns empty data frame
#        if no sources present
# Dependencies:
# Author: Noah Prime
# parameters:
#   - source_path - path to where the yml files are saved
#   - em - emission species
# return: data frame of point sources with attributes
# input files: yml files
# output files: Null
load_point_source_attributes <- function( source_path, em ){

    # list of yml file names
    target_filename <- list.files( paste0( source_path, em ), "*.yml", full.names = TRUE )

    # Return empty dataframe if no sources
    if( length( target_filename ) == 0 ){
        return( data.frame( name = character(),
                            location = character(),
                            iso = character(),
                            emission = double(),
                            CEDS_sector = character(),
                            EDGAR_sector = character(),
                            latitude = double(),
                            longitude = double() ) )
    }

    # reading in
    yml_input <- lapply(target_filename, read_yaml)
    # convert to data frame
    source_data <- do.call( bind_rows,
                            lapply( yml_input, function( x ){
                                return( as.data.frame( x$attributes ) )
                            } ) )
    return( source_data )
}


# ------------------------------------------------------------------------------
# load_point_source_attributes
# Brief: Load in yml files for all point sources from specified species
#        including the time-series of emissions.
# Dependencies:
# Author: Noah Prime
# parameters:
#   - source_path - path to where the yml files are saved
#   - em - emission species
# return: data frame of point sources with attributes and time series
# input files: yml files
# output files: Null
load_point_sources <- function( source_path, em ){

    # list of yml file names
    target_filename <- list.files( paste0( source_path, em ), "*.yml", full.names = TRUE )

    # Return if no sources
    if( length( target_filename ) == 0 ){
        # Empty dataframe of attributes
        df <- data.frame( descirption = character(),
                          name = character(),
                          location = character(),
                          latitude = double(),
                          longitude = double(),
                          emission = double(),
                          units = character(),
                          CEDS_sector = character(),
                          EDGAR_sector = character(),
                          fuel = character(),
                          iso = character(),
                          build_year = integer()
                        )

        # Empty dataframe of time series
        Xyears <- paste0('X',1750:2019)
        Xyears_df <- data.frame( matrix(ncol = length(Xyears), nrow = 0) ) %>%
            dplyr::mutate_all(as.numeric)

        # Combine dataframes
        df <- cbind(df,Xyears_df)

        # Rename columns
        x <- c('description', 'name', 'location', 'longitude', 'latitude', 'emission',
               'units', 'CEDS_sector', 'EDGAR_sector', 'fuel', 'iso', 'build_year',
               paste0('X',1750:2019) )
        colnames( df ) <- x

        return( df )
    }

    # reading in
    yml_input <- lapply(target_filename, read_yaml)
    # convert to data frame
    source_data <- do.call( bind_rows,
                            lapply( yml_input, function( x ){
                                df.a <- as.data.frame( x$attributes )
                                colnames( df.a ) <- gsub( 'value.', 'X', colnames( df.a ) )
                                df.b <- as.data.frame( x$documentation )
                                df <- cbind( df.a, df.b )
                                df <- df %>%
                                    dplyr::select( description, name, location, longitude,
                                                   latitude, emission, units, CEDS_sector,
                                                   EDGAR_sector, fuel, iso, build_year,
                                                   X1750:X2019 )
                                return( df )
                            } ) )

    return( source_data )
}

# ------------------------------------------------------------------------------
# match_sources
# Brief: Finds matches between sources in the proxy grid, and the yml files
# Dependencies:
# Author: Noah Prime
# parameters:
#   - proxy - list of sources in proxy grid
#   - ymls  - list of sources in yml files
#   - params - data frame that contains parameters. This is read in from a file
# return: data frame of point sources with attributes appended with matching
#         coordinates in proxy grid
# input files: Null
# output files: Null
match_sources <- function( proxy, ymls, params, sector ){
    # Get only point sources from given sector
    ymls <- ymls %>%
        dplyr::filter( EDGAR_sector == sector)

    # Number of point sources input
    num_sources <- nrow( ymls )

    if( num_sources == 0 ){ return( ymls ) }

    # Empty vectors of corresponding latitudes, longitudes of EDGAR data
    # and column to indicate if a match is found, and if the source sector
    # is set to zero out surrounding cells
    proxy_lat <- numeric( num_sources )
    proxy_lon <- numeric( num_sources )
    match_found <- numeric( num_sources )
    to_zero <- numeric( num_sources )

    # Area for which a cell is considered a match
    if( nrow(params) == 0){
        search_buffer <- 0.5
    }else{
        search_buffer <- as.numeric( params$search_buffer )
    }

    # Loop through each point sources to find matches
    for( i in 1:num_sources ){

        # Source latitude, longitude, sector and emission species
        source_lat <- as.numeric( ymls$latitude[i] )
        source_lon <- as.numeric( ymls$longitude[i] )
        source_sector <- ymls$EDGAR_sector[i]

        # Whether or not this source is in a sector which is set to zero surrounding cells
        to_zero[i] <- params$zero_surrounding

        # Looking for a cell in EDGAR near enough to the source to be considered a match
        matching_coords <- proxy %>%
            dplyr::filter( lat <= source_lat + search_buffer, lat >= source_lat - search_buffer,
                           lon <= source_lon + search_buffer, lon >= source_lon - search_buffer ) %>%
            dplyr::arrange( dplyr::desc(em) ) %>%
            dplyr::select( lat, lon ) %>%
            as.matrix()

        # If a match not found, use the source lat/lon and set match found to FALSE
        # Otherwise use the matched cell coordinates with largest emissions
        # and set match found to TRUE
        if( is.na( matching_coords[1] ) ){
            proxy_lat[i] <- source_lat
            proxy_lon[i] <- source_lon
            match_found[i] <- FALSE
        }else{
            proxy_lat[i] <- matching_coords[1,1]
            proxy_lon[i] <- matching_coords[1,2]
            match_found[i] <- TRUE
        }

    }

    # Amending the results to the data frame
    ymls$proxy_lat <- proxy_lat
    ymls$proxy_lon <- proxy_lon
    ymls$match_found <- match_found

    return( ymls )

}


# ------------------------------------------------------------------------------
# zero_cells
# Brief: Zeros cells in a grid based on supplied point sources
# Dependencies:
# Author: Noah Prime
# parameters:
#   - grid - grid of emissions
#   - source_data  - data frame of sources with locations in grid, and whether to zero nearby cells
#   - em - emission species
#   - sector - EDGAR sector or FLR
# return: grid with desired cells set to zero
# input files: Null
# output files: Null
zero_cells <- function( grid, matched_sources, params, em, sector, res ){

    # Return grid if no matched sources
    if( nrow(matched_sources) == 0 ){ return(grid) }

    # coordinates of point sources in EDGAR grid for this emission and sector
    coords <- matched_sources %>%
        dplyr::filter( EDGAR_sector == sector ) %>%
        dplyr::select( proxy_lat, proxy_lon )

    # Possible that no point sources exist for given species/sector
    if( nrow(coords) == 0 ){ return( grid ) }

    # converting coordinates to cell index
    center_cells <- t( apply(coords, 1, function(x){ coordinates_to_index( x[1], x[2], res ) } ) )

    # zero out cells
    for( i in 1:nrow(center_cells) ){
        grid[ center_cells[i,1], center_cells[i,2] ] <- 0
    }

    # If not a sector to zero surroundings, return
    if( nrow(params) != 0 ){
        if( params$zero_surrounding == 0 ){
            return( grid )
        }else{
            # Specified buffer
            zero_cell_buffer = params$zero_buffer / res
        }
    }else{
        # Default buffer
        zero_cell_buffer = 1
    }

    # zeroing out surrounding cells
    for( i in 1:nrow(center_cells) ){
        center_lat <- center_cells[i,1]
        center_lon <- center_cells[i,2]

        for( r in (center_lat - zero_cell_buffer):(center_lat + zero_cell_buffer) ){
            for( c in (center_lon - zero_cell_buffer):(center_lon + zero_cell_buffer) ){
                grid[(r%%1800),(c%%3600)] <- 0
            }
        }
    }

    # return grid with sources removed
    return( grid )
}


# ------------------------------------------------------------------------------
# get_grid_point_source
# Brief: Finds the largest X cells in a grid
# Dependencies:
# Author: Noah Prime
# parameters:
#   - grid - global grid of values, presumably emission flux
#   - X - number of cells wanted to return ( 100 default
#   - res - resolution of global grid
# return: data frame of coordinates of largest cells and values
# input files: Null
# output files: Null
get_grid_point_source <- function( grid, X = 100, res ){

    # largest X cell emissions
    ems <- tail(sort(as.matrix( grid )), X)

    # indices of largest X cells emissions as vector
    indvec <- tail(order( grid ), X)
    # row
    row <- mod( indvec, 1800 )
    # column
    col <- ceiling( indvec / 1800 )
    # coordinates of largest X cell emissions
    coords <- index_to_coordinates( cbind(row,col), res )


    return( data.frame( em = ems, lat = coords[,1], lon = coords[,2]) )
}


# ------------------------------------------------------------------------------
# index_to_coordinates
# Brief: Returns the coordinates at the center of input cell
# Dependencies:
# Author: Noah Prime
# parameters:
#   - inds - vector of indexes, c( row, col )
#   - res - resolution of the global grid
# return: coordinates on -90,90 lat and -180,180 lon
# input files: Null
# output files: Null
index_to_coordinates <- function( inds, res ){
    row <- inds[,1]
    col <- inds[,2]

    # coordinates are given at middle of the cell. this is inline with
    # EDGAR procedure, although here we have lon on rang -180 to 180
    lat <- ( 90 + res / 2 ) - res * row
    lon <- -( 180 + res / 2 ) + res * col

    return( cbind(lat,lon) )
}


# ------------------------------------------------------------------------------
# remove_point_sources
# Brief: Match CEDS point source time series to proxy grid and zero cells of matches
# Dependencies:
# Author: Noah Prime
# parameters:
#   - grid - proxy grid to remove sources from
#   - sources - dataframe of point sources
#   - params - extra details by sector
#   - sector - sector of the proxy grid
#   - res - grid resolution
# return: coordinates on -90,90 lat and -180,180 lon
# input files: Null
# output files: Null
remove_point_sources <- function( grid, sources, params, sector, res ){
    if(nrow(sources) == 0){ return( grid ) }
    if(nrow(params) == 0){
        num <- 500
    }else{
        num <- params$num_sources
    }
    proxy_sources <- get_grid_point_source( grid, num, res )
    matched_sources <- match_sources( proxy_sources, sources, params, sector )
    new_grid <- zero_cells( grid, matched_sources, params, em, sector, res )
    return( new_grid )
}













