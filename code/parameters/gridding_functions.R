#-----------------------------------------------------------------------------------------------------------------
# Program Name: gridding_functions.R
# Author's Name: Leyang Feng
# Date Last Modified: June 30 2016
# Program Purpose: Core functions for emissions gridding routine
# Note: 1. Brief introduction of how things work in gridding routine:
#          There are three steps for gridding, (1) the emissions are gridded at country level
#          then aggregated to form a global grids. The core functions for this step are grid_one_country,
#          grid_all_countries and aggregate_all_regions. 
#          (2) Then the first step is repeated for all sectors. The core function for this 
#          step is grid_one_sector and it's wraper grid_all_sectors.
#          (3) Then the list contains global grids for all sectors is returned by grid_one_year
#          The nested relationship of three steps's core functions is as below
#            grid_one_year
#              |----- grid_all_sectors (grid_one_sector)
#                                                |----------- grid_all_countries (grid_one_country)
#                                                |----------- aggregate_all_countries
#       2. The variations of grid_one_xxx functions ( such as grid_one_country_subVOCs, grid_one_year_air ) 
#          serves specific gridding tasks. They might be specific to CEDS project.  
#       3. Functions in this script are categrized into different classes serving different purposes:
#          core gridding functions - gridding functions for general routine
#          specified gridding functions - gridding functions for specific gridding tasks. 
#          proxy functions - proxy related functions that serve core/specified gridding functions 
#          seasonality functions - seasonality related functions that serve core/specified gridding functions 
#          other supporting functions
#          mask related functions 
# TODO: 1. The use of the words 'country' and 'region' are mixed. Change all 'region's into 'country'.
#       2. Add grid_all_years function as the lapply wraper of grid_one_year
#       3. Extract the process of adding seasonality out of nc generation functions.  
#       4. Update the VOC related functions to remove the for loop
# ----------------------------------------------------------------------------------------------------------------

# Special Packages
loadPackage( 'ncdf4' ) 
loadPackage( 'sp' )
loadPackage( 'geosphere' )

# ==============================================================================
# core gridding functions
# ------------------------------------------------------------------------------
# grid_one_country
# Brief: Generates a country's emission spatial distribution using proxy.
#        The emissions are gridded in following steps: 
#        (1) Bounding information is retrived using country name from location_index table
#            ( bounding information: the country's bounding box in terms of upper left corner and lower right corner 
#                                    matrix indeces in global extent )
#        (2) Retrieve the proxy substitution flag using country name from proxy_replace_flag passed by upper layer function.
#            If proxy substitution flag is TRUE then use backup proxy instead of proxy
#        (3) The proxy and proxy_backup (in global extent) passed by upper layer function is cropped using bounding information.
#        (4) Apply weighted country mask to cropped proxy to make proxy only contains data for the country and boundary weighted. 
#        (5) Normalize the weighted proxy
#        (6) Apply emission statistics over normalized proxy to get emission spatial distribution for the country
#        (7) Return the spatial distribution (matrix)  
# Dependencies: 
# Author: Leyang Feng
# parameters: country - the country that its gridded emission is desired.  
#             location_index - location index table, contains matrix index information for the country in global extent
#             em_data - emission data processed by upper layer function ( grid_one_sector )
#             proxy - spatial proxy used for gridding ( passed by upper layer function )
#             proxy_backup - Backup spatial proxy used for gridding when proxy_replace_flag = T
#             year - The current gridding year.   
#             sector - The current gridding secotr.
#             em_species - The current emission species. 
#             proxy_replace_flag - Indicator of which proxy to use. Default is F so the proxy is used, otherwise use proxy_backup 
# return: em_spatial - a matrix 
# input files: null
# output: null
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
    replacement_message_file <- paste0( diagnostic_msg_dir, 'proxy_replacement_list.txt' )
	
    # what if the backup proxy have all zero pattern 
    if ( sum(proxy_cropped * mask ) == 0 ) {
      message_line <- paste0( 'Backup proxy used but all zero pattern: ', country, ' ,', year, ' ,', sector, ' ,', em_species )
    } else {
      message_line <- paste0( 'Backup proxy used: ', country, ' ,', year, ' ,', sector, ' ,', em_species )  
      }
    cat( message_line, file = replacement_message_file, append = TRUE, sep="\n" )
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
  
  return( em_spatial )
}


# ------------------------------------------------------------------------------
# grid_all_countries
# Brief: lapply wraper for grid_one_country. Generates a list of emission spatial distributions for given country list.
# Dependencies: grid_one_country
# Author: Leyang Feng
# parameters: country_list - a list of country that its emissions needs to be gridded  
#             see grid_one_country for descriptions of other parameters
# return: region_em_spatial_list - a list contains spatial distribution (matrix) for all countries in the country list.
# input files: null
# output: null
grid_all_countries <- function( country_list, grid_one_country, location_index, 
                              emissions_year_sector, proxy, proxy_backup, year, sector, 
                              em_species, proxy_replace_flag ) {
    
    region_em_spatial_list <- lapply( country_list, grid_one_country, location_index, emissions_year_sector, proxy, proxy_backup, year, sector, em_species, proxy_replace_flag )
    names( region_em_spatial_list ) <- paste0( country_list, '_em_spatal' )
    return( region_em_spatial_list )

}


# ------------------------------------------------------------------------------
# aggregare_all_countries
# Brief: Aggregates all country's emission into global grid.
#        The aggregating is in following steps:
#        (1) Create an empty temoplate grid as global extent using grid_resolution
#        (2) For each countries emission spatial distribution, retrieve the country's bounding information and 
#            add the country's grid into template grid
#        (3) Rename the template grid and return 
# Dependencies: 
# Author: Leyang Feng
# parameters: region_em_spatial_list - passed by grid_all_countries function.  
#             location_index - location index table, contains matrix index information for the country in global extent
#             grid_resolution - passed by upper layer function.  
#             mass - passed by upper layer function. Output in mass(kt) if TRUE, otherwise the output is in flux(kg m-2 s-1) 
# return: em_spatial_global  - global emission spatial distribution for given sector.   
# input files: null
# output: null
aggregate_all_regions <- function( region_em_spatial_list, location_index, 
                                   grid_resolution, mass ) {
  
  # create a empty template grid for later aggregating 
  temp_grid <- matrix( 0, 180 / grid_resolution, 360 / grid_resolution )
  
  # aggregating
  for ( i in seq_along( region_em_spatial_list ) ) { ## use for loop for efficient aggregating
    
	# get the region name
	  region_name <- names( region_em_spatial_list ) [ i ]
    region_name <- unlist( strsplit( region_name, split = '_' ) ) [ 1 ]
	
	# retrieve matrix indexes for iso for later locating
	  row_col_index <- location_index[ location_index$iso == region_name, ]
    start_row <- row_col_index$start_row
    end_row <- row_col_index$end_row
    start_col <- row_col_index$start_col
    end_col <- row_col_index$end_col
    
	# add the em_spatial to empty template grid
	temp_grid[ start_row : end_row , start_col : end_col ] <- 
    temp_grid[ start_row : end_row , start_col :end_col ] + region_em_spatial_list[[ i ]]
  }
  
  # convert the unit from mass to flux if desired
  if ( mass == F ) {
    
    # generate the global_grid_area matrix for later calculation 
    global_grid_area <- grid_area( grid_resolution, all_lon = T )
    flux_factor <- 1000000 / global_grid_area / ( 365 * 24 * 60 * 60 ) # from kt to kg/m-2/s-1
    temp_grid <- temp_grid * flux_factor
  }
  
  em_spatial_global <- temp_grid 

  return( em_spatial_global )
}


# ------------------------------------------------------------------------------
# grid_one_sector
# Brief: Generates a global grid for a given sector. 
#        The function is divided into two different routine by a if statement:
#          if the given scetor is not 'SHP', the function processes in following steps:
#            (1) Retrieve proxy by using combination of emission species, year, and sector
#            (2) Retrieve backup proxy by using year and type (default 'population' )
#            (3) Process emission data passed by the upper layer function
#            (4) do a mask availibility check for countries appeared in the emission data
#            (5) Drop the countries in the emission data but not having a country mask
#            (6) Run proxy_substitution_check and pass the result to grid_all_countries function
#            (7) Generate country_list and pass to grid_all_countries and do the gridding
#            (8) Do the aggregating
#            (9) Return the global grid to upper layer function
#          if the given sector is 'SHP', the funtion processes in following steps:
#               ( the gridding for sector 'SHP' is not by country so needes different routine )
#            (1) Load proxy using combination of emission species, year, and sector 
#            (2) Load the global_mask
#            (3) Apply the global_mask and normalize the proxy 
#            (4) Apply pre-processed emission statistics to normalized proxy 
#            (5) Return the global grid to upper layer function  
# Dependencies: get_proxy, get_backup_proxy, mask_avail_check, proxy_substitution_check, grid_all_countries,
#               aggregate_all_countries
# Author: Leyang Feng
# parameters: sector - the emission sector passed by upper layer function 
#             em_species - the emission species passed by upper layer function   
#             year - the year passed by upper layer function 
#             location_index - the country location index table 
#             grid_resolution - grid resolution 
#             em_data - the emission data pre-processed and passed by upper layer function 
#             mass - parameter passing to inner layer function aggregate_all_countries   
# return: a globa;l grid for given sector 
# input files: null
# output: null 
grid_one_sector <- function( sector, em_species, year, location_index, grid_resolution, em_data, mass ) {
  
  #debug
  #em_data <- emissions_year
  
  # for sectors other than shipping
  if ( sector != 'SHP' ) {
  # retrive the proxy data
  proxy <- get_proxy( em_species, year, sector )
  proxy_backup <- get_backup_proxy( year, type = 'population' )
  
  # extract emission for specific year and gas
  emissions_year_sector <- subset( em_data, em_data$CEDS_grd_sector == sector,
                              c( 'iso', paste0( 'X', year ) ) )
  # if no mask available for certain country, generate a country drop list 
  country_drop_list <- mask_avail_check( emissions_year_sector$iso, location_index$iso )
  
  # write out the country_drop_list to a file 
  #drop_country_list_file <- paste0( diagnostic_msg_dir, 'dropped_countries.txt' )
  #cat( country_drop_list, file = drop_country_list_file, append = TRUE, sep = "\n" )
  
  # drop country if necessary 
  for ( country in country_drop_list ) {
	emissions_year_sector <- emissions_year_sector[ emissions_year_sector$iso != country, ]
	}
  # extract current country_list from emissions_year_sector
  country_list <- emissions_year_sector$iso 
  # perform proxy_substitution_check. If for one perticular country, the flag is TRUE,
  # then use proxy_backup
  proxy_replace_flag <- proxy_substitution_check( country_list, location_index, emissions_year_sector, proxy )
  
  # calling the grid_one_country function to do the scalling
  region_em_spatial_list <- lapply( country_list, grid_one_country, location_index, emissions_year_sector, proxy, proxy_backup, year, sector, em_species, proxy_replace_flag )
  names( region_em_spatial_list ) <- paste0( country_list, '_em_spatal' )
  
  # aggregating
  global_em_spatial <- aggregate_all_regions( region_em_spatial_list, location_index, 
                                   grid_resolution, mass )
  
  # for shipping sector 
  } else {
    proxy <- get_proxy( em_species, year, sector )
    
	  emissions_year_sector <- subset( em_data, em_data$CEDS_grd_sector == sector,
                                c( 'iso', paste0( 'X', year ) ) )
	  emission_value <- sum( emissions_year_sector[, 2 ] )
    
	  mask <- global_mask
	  proxy_weighted <- proxy * mask 
    proxy_normlized <- proxy_weighted / sum( proxy_weighted )
    global_em_spatial <- proxy_normlized * emission_value
    if ( mass == F ) {
      # generate the global_grid_area matrix for later calculation 
      global_grid_area <- grid_area( grid_resolution, all_lon = T )
      flux_factor <- 1000000 / global_grid_area / ( 365 * 24 * 60 * 60 )
      global_em_spatial <- global_em_spatial * flux_factor
    } 
  
  }
  return( global_em_spatial )
  }


# ------------------------------------------------------------------------------
# grid_all_sectors
# Brief: lapply wraper for grid_one_countriy 
# Dependencies: grid_one_sector
# Author: Leyang Feng
# parameters: sector_list - a list of sectors.    
#             see grid_one_country for descriptions of other parameters
# return: sector_em_global_list - a list contains global grids (matrix) for each sector in the sector_list
# input files: null
# output: null
grid_all_sectors <- function( sector_list, grid_one_sector, em_species, year, 
                            location_index, grid_resolution, emissions_year, mass) {

  sector_em_global_list <- lapply( sector_list, grid_one_sector, em_species, year, location_index, grid_resolution, emissions_year, mass ) 
  names( sector_em_global_list ) <- paste0( sector_list, '_em_global' )
  return( sector_em_global_list )

}


# ------------------------------------------------------------------------------
# grid_one_year
# Brief: Generates one year's gridded emission for given sector list 
# Dependencies: grid_all_sectors 
# Author: Leyang Feng
# parameters: em_species - the emission species passed by user
#             year - the emission year passed by user
#             em_data - the pre-processed emission data in the gridding routine 
#             location_index - location index table, contains matrix index information for the country in global extent
#             sector_list - the sector list passed by user
#             grid_resolution - grid resolution  
#             mass - passing to inner layer function. output in mass(kt) if TRUE, otherwise the output is in flux(kg m-2 s-1) 
# return: sector_em_global_list - a list contains one year's gridded emission for given sector list 
# input files: null
# output: null 
grid_one_year <- function( em_species, year, em_data, location_index, sector_list, grid_resolution, mass = F ) { 
  
  #debug
  #em_species <- em
  #year <- year
  #em_data <- emissions_level1_sector
  #location_index <- country_location_index 
  #sector_list <- level1_sector_list
  #mass <- F
  
  X_year <- paste0( 'X', year )
  emissions_year <- em_data[ c( 'iso', 'CEDS_grd_sector', X_year ) ]
  
  sector_em_global_list <- lapply( sector_list, grid_one_sector, em_species, year, location_index, grid_resolution, emissions_year, mass ) 
  names( sector_em_global_list ) <- paste0( sector_list, '_em_global' )
  
  return( sector_em_global_list )
}


# ==============================================================================
# specified gridding functions
# ------------------------------------------------------------------------------
# grid_one_country_subVOCs
# Brief: Generates a country's all 23 VOC emission spatial distributions
#        The speciated VOC grids are generated in following steps: 
#          (1) Bounding information is retrived using country name from location_index table
#              ( bounding information: the country's bounding box in terms of upper left corner and lower right corner 
#                                      matrix indeces in global extent )
#          (2) Retrieve the VOC ratios for the given VOC, sector
#          (3) Retrieve the proxy substitution flag using country name from proxy_replace_flag passed by upper layer function.
#              If proxy substitution flag is TRUE then use backup proxy instead of proxy
#          (4) The proxy and proxy_backup (in global extent) passed by upper layer function is cropped using bounding information.
#          (5) Apply weighted country mask to cropped proxy to make proxy only contains data for the country and boundary weighted. 
#          (6) Normalize the weighted proxy
#          (7) Apply emission statistics over normalized proxy to get emission spatial distribution for the country
#          (8) For each VOC ( through a for loop ), apply the ratio to the grid generated by step (7).
#          (9) Save each VOC grid into global environment. 
# Dependencies: null 
# Author: Leyang Feng
# parameters: VOC_ratio_country - VOC ratios for the give4n country. Passed by upper layer functions. 
#             VOC_list - the list of VOCs passed by upper layer function
#             rest parameters refers to grid_one_country     
# return: 23 VOCs grids for the given country but no return for now. 
#         all outputs should be returned are saving to global environment directly, which should be changed. Added to TODO. 
# input files: null
# output: null 
grid_one_country_subVOCs <- function( country, location_index, em_data, proxy, proxy_backup, year, sector, em_species, VOC_ratio_country, VOC_list, proxy_replace_flag ) {
  
  # retrieve matrix indexes for iso for later proxy cropping
  row_col_index <- location_index[ location_index$iso == country, ]
  start_row <- row_col_index$start_row
  end_row <- row_col_index$end_row
  start_col <- row_col_index$start_col
  end_col <- row_col_index$end_col
  
  # retrieve VOC ratios for current country
  ratio_all <- VOC_ratio_country[ VOC_ratio_country$iso == country, 2 : ncol( VOC_ratio_country ) ]
  
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
    replacement_message_file <- paste0( diagnostic_msg_dir, 'proxy_replacement_list.txt' )
    
    # what if the backup proxy have all zero pattern 
    if ( sum( proxy_cropped * mask ) == 0 ) {
      message_line <- paste0( 'Backup proxy used but all zero pattern: ', country, ' ,', year, ' ,', sector, ' ,', em_species )
    } else {
      message_line <- paste0( 'Backup proxy used: ', country, ' ,', year, ' ,', sector, ' ,', em_species )  
      }
    #cat( message_line, file = replacement_message_file, append = TRUE, sep="\n" )
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
  
  # distribute emissions using each VOC ratio
  for ( VOC in VOC_list ) {
    em_spatial <- emission_value * norm_weighted_proxy * as.numeric( ratio_all[ which( VOC_list == VOC ) ] )
    temp_em_name <- paste0( country, '_', VOC, '_em_spatial' )
    assign( temp_em_name, em_spatial, .GlobalEnv )
  }
}


# ----------------------------------------------------------------------------------------
# aggregate_all_countries_subVOCs
# Brief: similar to aggregate_all_country function 
# Dependencies: null 
# Author: Leyang Feng
# parameters: VOC - the VOC that its grids for all countries wanted to be aggregated 
#             other parameters refer to aggregate_all_country function 
# return: Global emission spatial distribution for given VOC, saving to global environment directly, which should be changed. Added to TODO.
# input files: null
# output: null
aggregate_all_countries_subVOCs <- function( VOC, sector, country_list, location_index, grid_resolution, mass ) {
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
    em_spatial_name <- paste0( country, '_', VOC, '_em_spatial')
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
  
  tempname <- paste0( sector, '_', VOC, '_em_global' ) 
  assign( tempname, grid_template, .GlobalEnv )
  }
  
  
# -------------------------------------------------
# grid_2L
# Brief: Generates a country's 2L_Other-process-emissions emission spatial distribution speccificly for NMVOC.
#        Special function for CEDS NMVOC emissions  
# Dependencies: null 
# Author: Leyang Feng
# parameters:  X2L_emissions - preprocessed country specific sector 2L_Other-process-emissions emissions
#              year - the gridding year 
# return: em_spatial - the grid for 2L emissions
# input files: null 
# output: null
grid_2L <- function( X2L_emissions, year ) {
  
  emission_value <- X2L_emissions[ , paste0( 'X', year ) ]
  
  proxy_year <- '1996'
  proxy_name <- paste0( 'X2L', '_', proxy_year )
  load( paste0( proxy_dir, proxy_name ) )
  proxy <- get( proxy_name )
  rm( list = proxy_name )
   
  proxy_normalized <- proxy / sum( proxy )
  em_spatial <- proxy_normalized * emission_value
  
  return( em_spatial )
}


# ------------------------------------------------------------------------------
# grid_one_sector_subVOCs
# Brief: Similar to grid_one_sector function, also has two routines for 'SHP' sector and non-'SHP' sector. 
# Dependencies: get_proxy, get_backup_proxy, mask_avail_check, proxy_substitution_check, grid_one_country_subVOCs, aggregate_all_countries_subVOCs 
# Author: Leyang Feng
# parameters: VOC_ratio_table - a table listing out VOC ratios for country, VOC, sector combination. The table is read-in in the routine and pass by upper layer function  
#             VOC_list - a list of VOCs, passed by upper layer function 
# return: the grid for given sector for all VOCs. Saving to the global environment for now, should be changed. Added to TODO.  
# input files: null
# output: null 
grid_one_sector_subVOCs <- function( sector, em_species, year, location_index, grid_resolution, em_data, VOC_ratio_table, VOC_list, mass ) {
  
  # for sectors other than shipping
  if ( sector != 'SHP' ) {
  # extract VOC ratios for current sector
  ## VOC ratio sectors are not at itermediate level, so do some aggregation
  sector_final <- sector
  if ( sector == 'ELEC' | sector == 'FFFI' | sector == 'FLR' ) { sector_final = 'ENE' }
  if ( sector == 'INDC' | sector == 'INPU' ) { sector_final = 'IND' } 
  if ( sector == 'ROAD' | sector == 'NRTR' ) { sector_final = 'TRA' } 
  if ( sector == 'RCORC' | sector == 'RCOO') { sector_final = 'RCO' }
  
  VOC_ratio_country <- subset( VOC_ratio_table, VOC_ratio_table$CEDS_grd_sector == sector_final, c( 'iso', VOC_list ) )
  # retrieve proxy
  proxy <- get_proxy( em_species, year, sector )
  proxy_backup <- get_backup_proxy( year, type = 'population' )
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
  # perform proxy_substitution_check. If for one perticular country, the flag is TRUE,
  # then use proxy_backup
  proxy_replace_flag <- proxy_substitution_check( country_list, location_index, emission_year_sector, proxy )
  # calling the grid_one_country function to do the scalling
  invisible( lapply( country_list, grid_one_country_subVOCs, location_index, emission_year_sector, proxy, proxy_backup, year, sector, em_species, VOC_ratio_country, VOC_list, proxy_replace_flag ) )

  # aggregating
  invisible( lapply( VOC_list, aggregate_all_countries_subVOCs, sector, country_list, location_index, grid_resolution, mass ) )
  
  # remove country specific em_spatial variables from global environment
  for ( VOC in VOC_list ) {
    for( country in country_list ) {
      var_to_remove <- paste0( country, '_', VOC, '_em_spatial' )
      rm( list = var_to_remove, envir = as.environment( '.GlobalEnv' ) )
      }
    }
  
  # for SHP sector 
  } else {
    # there are two parts of shiping specificlly for NMVOC: shipping in general and 2L sector under region global 
    # extrac the VOC ratio for SHP and X2L from the ratio table 
  ratio_global <- subset( VOC_ratio_table, iso == 'global', c( 'iso', 'CEDS_grd_sector', VOC_list ) )
  ratio_SHP <- as.numeric( subset( ratio_global, CEDS_grd_sector == 'SHP', VOC_list ) )
  ratio_X2L <- as.numeric( subset( ratio_global, CEDS_grd_sector == 'X2L', VOC_list ) )
    
  # deal with SHp and X2L at the same time 
    
  # SHP proxy process
  proxy_SHP <- get_proxy( em_species, year, sector )
  mask_SHP <- global_mask 
  proxy_weighted_SHP <- proxy_SHP * mask_SHP 
  proxy_normalized_SHP <- proxy_weighted_SHP / sum( proxy_weighted_SHP )
  # X2L proxy process
  proxy_year <- '1996'  # we only have X2L proxy for year 1996 and apply this proxy for all years 
  proxy_X2L_name <- paste0( 'X2L', '_', proxy_year )
  load( paste0( proxy_dir, proxy_X2L_name ) )
  proxy_X2L <- get( proxy_X2L_name )
  rm( list = proxy_X2L_name )
  mask_X2L <- global_mask 
  proxy_weighted_X2L <- proxy_X2L * mask_X2L 
  proxy_normalized_X2L <- proxy_weighted_X2L / sum( proxy_weighted_X2L )
  
  # extract emission values 
  # SHP extraction 
  emissions_year_SHP <- subset( em_data, em_data$CEDS_grd_sector == 'SHP',
                                c( 'iso', paste0( 'X', year ) ) )
  emission_value_SHP <- sum( emissions_year_SHP[ , 2 ] ) 
  # X2L extraction 
  emissions_year_X2L <- subset( em_data, em_data$CEDS_grd_sector == 'X2L',
                                c( 'iso', paste0( 'X', year ) ) )
  emission_value_X2L <- sum( emissions_year_X2L[ , 2 ] ) 
  
    
  global_grid_area <- grid_area( grid_resolution, all_lon = T )
  flux_factor <- 1000000 / global_grid_area / ( 365 * 24 * 60 * 60 )
    
  # make em_spatial for each VOC for SHP and X2L separately and then combine( add ) thses two together 
  invisible( lapply( VOC_list, function( each_VOC ) {
    em_spatial_SHP <- emission_value_SHP * proxy_normalized_SHP * ratio_SHP[ which( VOC_list == each_VOC ) ]
    em_spatial_X2L <- emission_value_X2L * proxy_normalized_X2L * ratio_X2L[ which( VOC_list == each_VOC ) ]
    em_spatial <- em_spatial_SHP + em_spatial_X2L 
      
    if ( mass == F ) { 
      em_spatial <- em_spatial * flux_factor 
      }
    # the combined grid for SHP and X2L will be called as SHP sector grid 
    tempname <- paste0( sector, '_', each_VOC, '_em_global' ) 
    assign( tempname, em_spatial, .GlobalEnv )
    } ) )
    }
  }


# ------------------------------------------------------------------------------
# grid_one_year_subVOCs
# Brief: Similar to function grid_one_year, except it's designed for VOC gridding
# Dependencies:  grid_one_sector_subVOCs
# Author: Leyang Feng
# parameters: VOC_ratio_table - a table listing out VOC ratios for country, VOC, sector combination. The table is read-in in the routine and pass by upper layer function  
#             VOC_list - a list of VOCs. 
# return: gloabl grids for one year's emissions for all sector and all VOCs. Saving to the global environment for now, should be changed. Added to TODO.  
# input files: null 
# output: null
grid_one_year_subVOCs <- function( em_species, year, em_data, location_index, sector_list, grid_resolution, VOC_ratio_table, VOC_list, mass = F ) { 
  X_year <- paste0( 'X', year )
  emissions_year <- em_data[ c( 'iso', 'CEDS_grd_sector', X_year ) ]

  invisible( lapply( sector_list, grid_one_sector_subVOCs, em_species, year, location_index, grid_resolution, emissions_year, VOC_ratio_table, VOC_list, mass ) )
  
  }
  
  
# -------------------------------------------------
# grid_one_year_air
# Brief: Generates global grid for international aircraft emissions 
#        The emissions for international aircraft does not need to be gridded by country 
#        and since it's only one sector, sector level gridding function is also unnecessary. 
# Dependencies: none
# Author: Leyang Feng
# parameters: em_species - emission species
#             year - the emission year 
#             em_data - the emission data pre-processed in the routine  
#             grid_resolution - grid resolution 
#             sector - the gridding sector with default value 'AIR'
#             mass - output in mass(kt) if TRUE, otherwise the output is in flux(kg m-2 s-1)  
# return: AIR_em_global - global grid for aircraft emissions 
# input files: null 
# output: null
grid_one_year_air <- function( em_species, year, em_data, grid_resolution, sector = 'AIR', mass = F ) {

  proxy <- get_proxy( em_species, year, sector )
  emission_value <- em_data[ , paste0( 'X', year )]
  
  # if 0 emission for distrubuting, use a shortcut 
  if ( emission_value <= 0 ) { 
    AIR_em_global <- array( 0, dim = dim( proxy ) ) 
  
  # if emission exists, do gridding 
  } else { 
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
    AIR_em_global <- AIR_em_global
  } else { AIR_em_global <- AIR_em_global } 
}
  return( AIR_em_global )
}


# ==============================================================================
# proxy functions
# ------------------------------------------------------------------------------
# get_proxy
# Brief: Loads proxy a emission species, year, and sector combination 
#        The function first find the proper proxy file for a emission species, year, sector combination
#        then read the proxy file from disk and load into current environment.  
#        Proxy mapping should be loaded into environment before calling the function 
# Dependencies: 
# Author: Leyang Feng
# parameters: em_species - emission species
#             year - the emission year 
#             em_data - the emission data pre-processed in the routine  
# return: proxy - the desired proxy 
# input files: null
# output: null
get_proxy <- function( em_species, year, sector ) {
    
    # list out all availiable proxies in proxy_dir
    proxy_list <- list.files( proxy_dir )
    if ( 'README' %in% proxy_list )	{ proxy_list <- proxy_list[ proxy_list != 'README' ] }
    
    # generate the proxy name want to load and some adjustement
    year_num <- as.numeric( year ) 
    proxy_filename <- paste( em_species, year, sector, sep = '_')
    
    # retrieve the proxy file name from the proxy mapping
    file_to_load <- proxy_mapping[ proxy_mapping$combination == proxy_filename, 'proxy_name' ]
    
    # if the proxy desired is not in proxy_list, load population as proxy 
    if( ( file_to_load %in% proxy_list ) == T ) {
      load( paste0( proxy_dir, file_to_load ) )
      proxy <- get( file_to_load )
      rm( list = file_to_load )
    } else { 
      
      load( paste0( proxy_backup_dir, file_to_load ) )
      proxy <- get( file_to_load )
      rm( list = file_to_load )
    }
    
    return( proxy )
}


# -------------------------------------------------
# get_backup_proxy
# Brief: Loads backup proxy for a year with backup proxy type 
# Dependencies: 
# Author: Leyang Feng
# parameters: year - the emission year
#             type - the type of backup proxy. The defualt type is 'population'  
# return: proxy_backup - the backup proxy 
# input files: null
# output: null
get_backup_proxy <- function( year, type = 'population' ) {

  if ( type == 'population' ) { type_name <- 'population' }
  proxy_name <- paste0( type_name, '_', year )
  
  load( paste0( proxy_backup_dir,proxy_name ) )
  proxy_backup <- get( proxy_name )
  rm( list = proxy_name )
  return( proxy_backup )
}


# -------------------------------------------------
# proxy_substitution_check
# Brief: A proxy check routine that decides whether the proxy for the country needs to be replaced by backup proxy or not. 
# Dependencies: em_proxy_ratio, substitue_or_not
# Author: Leyang Feng
# parameters: region_list - a list of countries/regions that its proxy needs to be check 
#             location_index - location index table, contains matrix index information for the country in global extent
#             em_data - pre-processed emission data passed by grid_one_sector
#             proxy - proxy data passed by grid_one_sector  
# return: a dataframe contains country/region names and proxy substitution information 
# input files: null
# output: null 
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
#             em_data - pre-processed emission datqa. Passed by upper layer function 
#             location_location index table, contains matrix index information for the country in global extent.
#             proxy - proxy data passed by upper layer function    
# return: a ratio between a region's emission and its proxy
# input files: null
# output: null
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
# substitue_or_not
# Brief: decide whether region needs proxy substitution or not. 
# Dependencies: null
# Author: Leyang Feng
# parameters: ratio - emission/proxy ratio computed by previous step
#             max_ratio - max ratio computed by previous step 
# return: a flag which is TRUE of FALSE
# input files: null 
# output: null
substitue_or_not <- function( ratio, max_ratio ) {
  if ( is.infinite( ratio ) == T ) { flag <- T } # ratio=number/0, all zero proxy pattern for existing emission => substitution needed 
  else if ( is.nan( ratio) == T ) { flag <- F } # ratio = 0/0, all zero proxy pattern for non-existing emission => no substitution needed
  else if ( is.na( ratio ) == T ) { flag <- F } # similar to nan
  else if ( ratio == 0 ) { flag <- F } # ratio=0/number, zero emission => no substitution needed
  else { 
    if ( ratio > ( max_ratio / 100 ) ) { flag <- T } # for outlier needs substitution
	else { flag <- F } # non-outlier doesn't need substitution 
  }
  return( flag )
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
  mask_list <- list.files( mask_dir, pattern = '.*_mask' )
  if ( load_masks == T ) {
    invisible( lapply( mask_list, function( mask_list ) { load( paste0( mask_dir, mask_list), .GlobalEnv ) } ) )
    country_mask_initialized <- T
    message( 'Country mask initialized: ', country_mask_initialized )
  }
  
  # load seasonality profile
  
  common_seasonality_list <- c( "AGR_NH3_seasonality", "AGR_seasonality", "ENE_seasonality", 
                                "IND_seasonality", "RCOO_seasonality", "RCORC_seasonality", 
                                "SHP_seasonality", "SLV_seasonality", "TRA_seasonality", "WST_seasonality" ) 
  invisible( lapply( common_seasonality_list, function( common_seasonality ) { load( paste0( seasonality_dir, common_seasonality ), .GlobalEnv ) } ) )
  seasonality_profile_initialized <- T
  message( 'Seasonality profile initialized: ', seasonality_profile_initialized )
}


# -------------------------------------------------
# region_emCombine
# Brief: merge one country's emission into another target country
# Dependencies: null
# Author: Leyang Feng
# parameters: em_data - pre-process emission data from the routine.
#             region_list_for_combination - a data frame listing out the merge county and target country
# return: em_data - similar as input but the emissions of desired counties are merged.   
# input files: null
# output: null 
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


# ==============================================================================
# seasonalityfunctions
# ------------------------------------------------------------------------------
# get_seasonalityFrac
# Brief: Get seasonality fraction profile for a given emission species, year, sector combination 
#        The function finds the proper seasonality profile through seasonality mapping file which should be
#        loaded into global environment before calling this function.  
# Dependencies: null
# Author: Leyang Feng
# parameters: em_species - emission species
#             year - the emission year 
#             em_data - the emission data pre-processed in the routine  
# return: seasonality - a 3-D array contains seasonality information for each cell
# input files: null
# output: null
get_seasonalityFrac <- function( em_species, sector, year ) {

  total_seasonality_list <- list.files( seasonality_dir, pattern = '.*_seasonality' )
  
  seasonality_filename <- paste0( sector, '_', em_species, '_', year, '_seasonality' )
  file_to_load <- seasonality_mapping[ seasonality_mapping$combination == seasonality_filename, 'seasonality_name' ]
  
  load( paste0( seasonality_dir, file_to_load ) )
  seasonality <- get( file_to_load )
  rm( list = file_to_load )
  
  return( seasonality )
}


# ==============================================================================
# mask related functions
# ------------------------------------------------------------------------------
# mask_avail_check 
# Brief: check the availibilty of country mask of a given country list 
# Dependencies: null
# Author: Leyang Feng
# parameters: emission_country_list - the country list that to check with 
#             mask_country_list - a list of all availible country masks 
# return: country_drop_list - a list contains countries that does not have country mask 
# input files: null
# output: null 
mask_avail_check <- function( emission_country_list, mask_country_list ){

  emission_country_list <- unique( emission_country_list)
  if ( ( FALSE %in% ( emission_country_list %in% mask_country_list ) ) == TRUE ) {
    country_drop_list <- emission_country_list[ which( !emission_country_list %in% mask_country_list )]
  } 
  return( country_drop_list )
  
}


