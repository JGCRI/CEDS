# -------------------------------------------------
# monthly_nc_output
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
	  exp <- paste0( 'ENE_', VOC, '_em_global_final <- ELEC_', VOC, '_em_global + FFFI_', VOC, '_em_global + ETRN_', VOC, '_em_global' )
	  eval( parse( text = exp ) )
    exp <- paste0( 'IND_', VOC, '_em_global_final <- INDC_', VOC, '_em_global + INPU_', VOC, '_em_global' )
	  eval( parse( text = exp ) )     
	  exp <- paste0( 'TRA_', VOC, '_em_global_final <- NRTR_', VOC, '_em_global + ROAD_', VOC, '_em_global' )
	  eval( parse( text = exp ) ) 
	  exp <- paste0( 'SLV_', VOC, '_em_global_final <- SLV_', VOC, '_em_global' )
	  eval( parse( text = exp ) )
	  exp <- paste0( 'WST_', VOC, '_em_global_final <- WST_', VOC, '_em_global' )
	  eval( parse( text = exp ) )
	  #####################################################
	  exp <- paste0( 'SHP_', VOC, '_em_global_final <- SHP_', VOC, '_em_global' )
	  eval( parse( text = exp ) )
	  exp <- paste0( 'SHP_', VOC, '_em_global_final <- matrix( 0, 360, 720 )' )
	  #####################################################
	  
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
  right_part <- paste0( 'ncvar_def( \'', left_part, '\', \'', data_unit, '\', dim_list, missval = missing_value, longname= \'' ,sector_list_longname, '\' , prec = \'float\', compression = 5 )' )
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
  ncatt_put( nc_new, 0, 'license', 'FOR TESTING AND REVIEW ONLY' )
  ncatt_put( nc_new, 0, 'history', 
             paste0( as.character( format( as.POSIXlt( Sys.time(), "UTC"), format = '%d-%m-%Y %H:%M:%S %p %Z' ) ),
                     '; College Park, MD, USA') )
  ncatt_put( nc_new, 0, 'comment', 'Test Dataset for CMIP6' )
  ncatt_put( nc_new, 0, 'data_usage_tips', 'Note that these are monthly average fluxes.' )
  ncatt_put( nc_new, 0, 'host', 'TBD' )
  ncatt_put( nc_new, 0, 'contact', 'ssmith@pnnl.gov' )
  ncatt_put( nc_new, 0, 'references', 'http://www.geosci-model-dev.net/special_issue590.html' )
  ncatt_put( nc_new, 0, 'molecular weight', VOC_names$molecular.weight[ which( VOC_names$VOC_id %in% VOC ) ], prec = 'float' )
  ncatt_put( nc_new, 0, 'molecular weight unit', 'g mole-1' )

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
# ------------------------------------------------------------------------------
# grid_one_year
# Brief: generate a fliped matrix by a given matrix
# Dependencies: 
# Author: Leyang Feng
# parameters: x - the matrix to be fliped  
# return: a fliped matrix 
# input files: 
# output: 
grid_one_year_subVOCs <- function( em_species, year, em_data, location_index, sector_list, grid_resolution, VOC_ratio_table, VOC_list, mass = F ) { 
  X_year <- paste0( 'X', year )
  emission_year <- em_data[ c( 'iso', 'CEDS_grd_sector', X_year ) ]

  invisible( lapply( sector_list, grid_one_sector_subVOCs, em_species, year, location_index, grid_resolution, emission_year, VOC_ratio_table, VOC_list, mass ) )
  
  
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
grid_one_sector_subVOCs <- function( sector, em_species, year, location_index, grid_resolution, em_data, VOC_ratio_table, VOC_list, mass ) {
  
  # extract VOC ratios for current sector
  ## VOC ratio sectors are not at itermediate level, so do some aggregation
  sector_final <- sector
  if ( sector == 'ELEC' | sector == 'FFFI' ) { sector_final = 'ENE' }
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
  }
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
    summary_dir <- filePath( "MED_OUT", "", extension="")
    
    
    # what if the backup proxy have all zero pattern 
    if ( sum(proxy_cropped * mask ) == 0 ) {
      message_line <- paste0( 'Backup proxy used but all zero pattern: ', country, ' ,', year, ' ,', sector, ' ,', em_species )
    } else {
      message_line <- paste0( 'Backup proxy used: ', country, ' ,', year, ' ,', sector, ' ,', em_species )  
      }
    cat( message_line, file = paste0( summary_dir, 'proxy_replacement_list .txt' ), append = TRUE, sep="\n" )
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
