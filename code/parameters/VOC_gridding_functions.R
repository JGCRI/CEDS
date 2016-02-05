# -------------------------------------------------
# monthly_nc_output
# Brief: generate a fliped matrix by a given matrix
# Dependencies: 
# Author: Leyang Feng
# parameters: x - the matrix to be fliped  
# return: a fliped matrix 
# input files: 
# output: 
final_monthly_nc_output_subVOCs <- function( output_dir, grid_resolution, year, em_species, sector_list, VOC_list, mass = F ) {
  
  # each VOC generates a netcdf file so loop on VOCs
  for ( VOC in VOC_list ) {
    # aggregate from intermediate grd level to final grd level 
    exp <- paste0( 'AGR_', VOC, '_em_global_final <- AGR_', VOC, '_em_global' )
	eval( parse( text = exp ) )
	exp <- paste0( 'ENE_', VOC, '_em_global_final <- ELEC_', VOC, '_em_global + FFFI_', VOC, '_em_global + ETRN_', VOC, '_em_global' )
	eval( parse( text = exp ) )
    exp <- paste0( 'IND_', VOC, '_em_global_final <- INDC_', VOC, '_em_global + INPU_', VOC, '_em_global' )
	eval( parse( text = exp ) )     
	exp <- paste0( 'TRA_', VOC, '_em_global_final <- NRTR_', VOC, '_em_global + ROAD_', VOC, '_em_global' )
	eval( parse( text = exp ) ) 
	exp <- paste0( 'RCO_', VOC, '_em_global_final <- RCO_', VOC, '_em_global' )
	eval( parse( text = exp ) )
	exp <- paste0( 'SLV_', VOC, '_em_global_final <- SLV_', VOC, '_em_global' )
	eval( parse( text = exp ) )
	exp <- paste0( 'WST_', VOC, '_em_global_final <- WST_', VOC, '_em_global' )
	eval( parse( text = exp ) )
	exp <- paste0( 'SHP_', VOC, '_em_global_final <- SHP_', VOC, '_em_global' )
	eval( parse( text = exp ) )
	
	data_list <- paste0( sector_list, '_', VOC, '_em_global_final')
	
	lons <- seq( -180 + grid_resolution / 2, 180 - grid_resolution / 2, grid_resolution )
    lats <- seq( -90 + grid_resolution / 2, 90 - grid_resolution / 2, grid_resolution )
    time <- c( 15.5, 45, 74.5, 105, 135.5, 166, 196.5, 227.5, 258, 288.5, 319, 349.5 )
    londim <- ncdim_def( "lon", "degrees_east", as.double( lons ), longname = 'longitude' )
    latdim <- ncdim_def( "lat", "degrees_north", as.double( lats ), longname = 'latitude' )
    timedim <- ncdim_def( "time", "days since 1750-01-01 0:0:0", as.double( time ), 
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
  right_part <- paste0( 'ncvar_def( \'', left_part, '\', \'', data_unit, '\', dim_list, missval = missing_value, longname= \'' ,final_sector_longname_list, '\' , prec = \'float\', compression = 5 )' )
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
  nc_file_name <- paste0( output_dir, 'CEDS_', VOC, '_anthro_', year, '_', grid_resolution, '_', ver_date, '.nc' ) 
  
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
  ncatt_put( nc_new, 0, 'title', paste0('Annual Emissions of ', VOC ) )
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
  # attributes for variables
  for ( each_var in sector_list ) {
    ncatt_put( nc_new, each_var, 'cell_methods', 'time:mean' )
    ncatt_put( nc_new, each_var, 'missing_value', 1e+20, prec = 'float' )
  }
  
  # close nc_new
  nc_close( nc_new)
  
  }

  
  # additional section: write a summary and check text
  # global_grid_area <- grid_area( grid_resolution, all_lon = T )
  # global_total <- c()
  # em_global_sectors <- c()
  # for ( em_global_data in data_list ) {
    # em_global_sector <- unlist( strsplit( em_global_data, split = '_' ) ) [ 1 ]
    # eval( parse( text = paste0( 'temp_data <- ', em_global_data ) ) )
    # temp_data <- temp_data * global_grid_area
    # total <- sum( temp_data )
    # global_total <- c( global_total, total )
    # em_global_sectors <- c( em_global_sectors, em_global_sector)
    # }
  # summary_table <- data.frame( year = year, species = em_species, 
                               # sector = em_global_sectors, 
                               # global_total = global_total,
                               # unit = 'kg s-1' )
  # summary_name <- paste0( output_dir, 'CEDS_', em_species, '_anthro_', year, '_', grid_resolution, '_', ver_date, '.csv' )
  # write.csv( summary_table, file = summary_name, row.names = F )
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
  X_year <<- paste0( 'X', year )
  emission_year <<- em_data[ c( 'iso', 'CEDS_grd_sector', X_year ) ]
  
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
  if ( sector == 'ELEC' | sector == 'FFFI' ) { 
  sector_final = 'ENE' 
  } else if ( sector == 'INDC' | sector == 'INPU' ) { 
  sector_final = 'IND' 
  } else if ( sector == 'ROAD' | sector == 'NRTR' ) { 
  sector_final = 'TRA' 
  } else { sector_final = sector }
  
  VOC_ratio_country <- subset( VOC_ratio_table, VOC_ratio_table$CEDS_grd_sector == sector_final, c( 'iso', VOC_list ) )
  # retrieve proxy
  proxy <- get_proxy( em_species, year, sector )
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
  invisible( lapply( country_list, grid_one_country_subVOCs, location_index, emission_year_sector, proxy, year, sector, em_species, VOC_ratio_country, VOC_list ) )

  # aggregating
  invisible( lapply( VOC_list, aggregate_all_countries_subVOCs, sector, country_list, location_index, grid_resolution, mass ) )
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

grid_one_country_subVOCs <- function( country, location_index, em_data, proxy, year, sector, em_species, VOC_ratio_country, VOC_list ) {
  
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
