#------------------------------------------------------------------------------
# Program Name: F.emissions_scaling_functions.R
# Author's Name: Tyler Pitkanen, Rachel Hoesly
# Date Last Modified: July 15, 2019
# Program Purpose: Header file containing generalized functions designed to
#   scale CEDS emissions data and emissions factors based on inventory data.
#   This file is made to be sourced at the beginning of each module F script to
#   load the functions for Module F's data read-in, inventory data arranging,
#   scaled data arranging, scaling factor calculation and estimation, and data
#   write-out.
# Note: Only designed for use within Module F scaling scripts
# TODO: change F.apply to use addToEmDb_overwrite for use with meta data tracking
#
#
# ------------------------------------------------------------------------------

# Special Packages

loadPackage('zoo')
source('../code/parameters/interpolation_extension_functions.R')
source('../code/parameters/data_functions.R')
# ------------------------------------------------------------------------------
# F.initializeMeta
# Brief: creates default meta data for scaled emissions and ef
# Dependencies: IO_functions.R
# Author: Rachel Hoesly
# parameters: input - default emissions of ef. uses the iso-sector-fuel combinations
# return: null
# input files: default emissions or ef
# output : null
# return: list of variables to be used in subsequent scaling functions

F.initializeMeta <- function(input) {

  # Create default meta data for scaling
  meta <- input[,c('iso','sector','fuel',X_emissions_years)]

  # Melt meta data
  meta_notes <- melt(meta, id.vars = c('iso','sector','fuel'))

  # Set all non-zero values to default (retaining zeros)
  meta_notes$value[which(meta_notes$value != 0)] <- "default"

  # Cast meta_notes back to wide form
  meta_out <- cast(meta_notes, iso+sector+fuel~variable, value = 'value')

  # writeData( meta, 'MED_OUT', paste0( 'F.', em, '_scaled_emissions-value_metadata' ) )
  writeData( meta_out, 'MED_OUT', paste0( "F.", em, "_", "scaled_EF-value_metadata" ), meta = F )

}

# F.readScalingData
# Brief: reads in data files and defines variables for scaling functions
# Details: reads in inventory data, the most recent versions of scaled emissions
#   and EFs, the mapping file data. Performs checks
#   for the ceds column of the mapping file as well as the input ceds data.
# Dependencies: CEDS_header.R, makefile specifications, modules B, C, and E
# Author: Tyler Pitkanen, Rachel Hoesly
# parameters:
#   inventory: file name of the inventory used in the script [default: inventory]
#   mapping:   file name of the mapping file [default: map]
#   method:    mapping method used to relate the inventory and ceds data
#              [default: mapping_method]
# return:
# input files: inventory = inventory_data_file, inv_data_folder,
#              mapping = sector_fuel_mapping, method = mapping_method,
#              region, inv_name, inv_years

# output : null
# return: list of variables to be used in subsequent scaling functions

F.readScalingData <- function( inventory = inventory_data_file, inv_data_folder,
                               mapping = sector_fuel_mapping,
                               method = mapping_method,
                               region, inv_name, inv_years) {
  ###add iso column for single country inventories

  # Determine scaling method and set params
  if( method == 'sector' ) {
    scaling_name  <- "scaling_sector"
    inv_matchcol_name  <- 'inv_sector'
    ceds_matchcol_name <- 'ceds_sector'
    method_col <- c('sector')
  } else if( method == 'fuel' ) {
    scaling_name  <- "scaling_fuel"
    inv_matchcol_name <- 'inv_fuel'
    ceds_matchcol_name <- 'ceds_fuel'
    method_col <- c('fuel')
  } else if( method == 'both' )  {
    scaling_name  <- c( 'scaling_sector' , 'scaling_fuel' )
    inv_matchcol_name <- c( 'inv_sector' , 'inv_fuel' )
    ceds_matchcol_name <- c( 'ceds_sector' , 'ceds_fuel' )
    method_col <- c('sector','fuel')
  }

  # Read in data
  inv_data_full <- readData( inv_data_folder , inventory)
  scaling_map <- readData( "SCALE_MAPPINGS", mapping , ".xlsx", sheet_selection = 'map' )
  scaling_map_names <- names(scaling_map)
  non_data_columns <- c("", NA, 'NA', paste0('X__', 1:length(scaling_map_names)))
  scaling_map_names <- scaling_map_names[scaling_map_names %!in% non_data_columns]
  scaling_map <- scaling_map[,scaling_map_names] %>% unique()

  # Check that ceds sectors are valid
  sectorCheck( scaling_map, colname = "ceds_sector" )

  # import scaling instruction sheets from scaling map
  ext_method <- readData( "SCALE_MAPPINGS", mapping , ".xlsx", sheet_selection = 'method' )
  ext_year <- readData( "SCALE_MAPPINGS", mapping , ".xlsx", sheet_selection = "year" )

  # other imports
  ef_file <- paste0( "F.", em, "_scaled_EF" )
  em_file <- paste0( "F.", em, "_scaled_emissions" )
  input_ef_read <- readData( "MED_OUT", ef_file )
  input_em_read <- readData( "MED_OUT", em_file )
  input_ef <- input_ef_read
  input_em <- input_em_read

  #Inventory Specific Data/Variables
  X_inv_years<-paste("X",inv_years,sep="")

  std_form_inv<-inv_data_full

  out <- list(  method, scaling_name ,inv_matchcol_name , ceds_matchcol_name,
             method_col, inv_data_full, scaling_map, ext_method, ext_year,
             ef_file, em_file, input_ef_read, input_em_read, input_ef,
             input_em, X_inv_years, std_form_inv)
  names(out) <- c( 'method', 'scaling_name' ,'inv_matchcol_name' , 'ceds_matchcol_name',
                  'method_col', 'inv_data_full', 'scaling_map', 'ext_method', 'ext_year',
                  'ef_file', 'em_file', 'input_ef_read', 'input_em_read', 'input_ef',
                  'input_em', 'X_inv_years', 'std_form_inv')

  return (  out )
}

# ---------------------------------------------------------------------------------
# F.invAggregate
# Brief: aggregates inventory data to the scaling sectors
# Details: reads in the standard form inv data, removes redundant rows, zeroes
#   out specified terms (eg NA), and aggregates the data to the scaling sectors
# Dependencies: CEDS_header.R, F.readScalingData(), module F scripts
# Author: Tyler Pitkanen, Rachel Hoesly
# parameters:
#   std_form_inv: inv data put into standard ceds format, with years along the
#                 y axis and sectors along x [default: std_form_inv]
#   zeroed_terms: inv data terms to make zero, such as blank cells (NA), '-',
#                 etc; this depends on the inventory's notation for zeroes.
#                 [default: c(NA, 'NA', 'NA ', '-')]
#   region: list of regions in inventory. Default - region
#   mapping_method: mapping method. Default - mapping_method
#   return: inv_data
#   input files: std_form_inv, region , mapping_method . All defined by early code

F.invAggregate <- function( std_form_inv, region , mapping_method,
                            zeroed_terms = c(NA, 'NA', 'NA ', '-')) {

  printLog( "Aggregating inventory data" )

  inv_data <- std_form_inv

  # Make data numeric

  inv_data[,X_inv_years] <- suppressWarnings( apply( inv_data[,X_inv_years] , 2, as.numeric ) )

  # Select regions
  inv_data <- inv_data[ inv_data$iso %in% region,]

  # Add scaling names,
  inv_data <- merge(inv_data, scaling_map[,c(inv_matchcol_name,scaling_name)],
                    by.x = method_col,
                    by.y = inv_matchcol_name)
  inv_data <- inv_data[ complete.cases (inv_data[,c(scaling_name)]),]

  # create empty data frame is no data from scaling mapping
  if( nrow( inv_data ) == 0 ) {
    agg_inv_data <- data.frame( matrix ( ncol = length ( c ( 'iso' , scaling_name , X_inv_years ) ) , nrow=0 ) )
    names( agg_inv_data ) <- c ( 'iso' , scaling_name , X_inv_years )
  } else {

  # Perform aggregation
  inv_data_long <- melt(inv_data[,c('iso',scaling_name,X_inv_years)], id = c('iso', scaling_name) )
  inv_data_long <- inv_data_long[complete.cases(inv_data_long[,c(scaling_name,'value')]),]

  if ( method %in% c("sector","fuel") ) cast.formula <- paste('iso +', scaling_name , '~ variable')
  if ( method == "both" ) cast.formula <- 'iso + scaling_sector + scaling_fuel ~ variable'
  agg_inv_data <- cast(inv_data_long, cast.formula , sum)
  }
  return( agg_inv_data )
}

# ---------------------------------------------------------------------------------
# F.cedsAggregate
# Brief: aggregates ceds data over fuel or sectors
# Details: reads in the ceds data, selects the region that corresponds to the
#   inventory, aggregates according to scaling map
# Dependencies: CEDS_header.R, F.readScalingData() ,module F scripts
# Author: Tyler Pitkanen, Rachel Hoesly
# parameters:
#   input_em: the most recent version of the scaled emissions [default: input_em]
#   region:   the iso code(s) of the region(s) corresponding to the regions
#             covered by the inventory [required]
#   method: the method used to aggregate and scale inv and ceds data
#             [default: mapping_method]
# return: ceds_data
# input files: null
# output files: null

F.cedsAggregate <- function( input_em, region, method = mapping_method ) {

  printLog('Aggregating CEDS data')
  #Select data in inventory region and inventory years
  region_input_em <- input_em[ input_em$iso %in% region , c('iso','sector','fuel','units',X_inv_years) ]

  # Make data numeric
  if( length(X_inv_years) >1)
  region_input_em[,X_inv_years] <- suppressWarnings( apply( region_input_em[,X_inv_years] , 2, as.numeric ) )
  if (length(X_inv_years) == 1) region_input_em[,X_inv_years] <- as.numeric(region_input_em[,X_inv_years])

  # Add scaling names,
  ceds_data <- merge(region_input_em, unique( scaling_map[,c(ceds_matchcol_name,scaling_name)] ),
                    by.x = method_col,
                    by.y = ceds_matchcol_name)
  ceds_data <- ceds_data[ complete.cases (ceds_data[,c(scaling_name)]),]

  # Perform aggregation
  ceds_data_long <- melt(ceds_data[,c('iso',scaling_name,X_inv_years)], id = c('iso', scaling_name) )
  ceds_data_long <- ceds_data_long[complete.cases(ceds_data_long[,c(scaling_name,'value')]),]

  if ( method %in% c("sector","fuel") ) cast.formula <- paste('iso +', scaling_name , '~ variable')
  if ( method == "both" ) cast.formula <- 'iso + scaling_sector + scaling_fuel ~ variable'

  ceds_data <- cast(ceds_data_long, cast.formula , sum)
  return(ceds_data)
}

# ---------------------------------------------------------------------------------
# F.scalingToCeds
# Brief: dissaggregates data frame from scaling aggregation to ceds sectors/fuel.
# Details: input and output in long form
# Dependencies:
# Author: Rachel Hoesly

# return: data frame in ceds aggregation, long or wide form
# input files: data frame in scaling aggregation
# output files:

F.scalingToCeds <- function( scalingData , dataFormat ,valueCol , valueLab = valueCol){
  # Disaggregate from scaling sectors to CEDS sectors and create CEDS scaling that
  #   is matched to the CEDS rows from the input
  #Format - 'long' or 'wide'. Long format has 'year' column and a value column,

  if (dataFormat == 'wide'){
    wide.names <- names(scalingData)
    wide.names <- wide.names[-which(wide.names == 'iso')]
    X_years <- names(scalingData)[grep('X',names(scalingData))]
    wide.names <- wide.names[-which(wide.names %in% X_years)]
    by_ceds <- merge(scalingData,
                     unique(scaling_map[complete.cases(scaling_map),
                                         c(scaling_name,ceds_matchcol_name)]),
                      all = TRUE,
                      by.x = c(wide.names),
                      by.y = scaling_name)
     by_ceds <- by_ceds[ , c('iso',wide.names,X_years)]
     names(by_ceds) <- c('iso',wide.names,X_years)
   }else if (dataFormat == 'long'){
     scaling_map_sectors <- unique(scaling_map[, c(scaling_name,ceds_matchcol_name)])
     scaling_map_sectors <- scaling_map_sectors[which(complete.cases(scaling_map_sectors)), ]
     names(scalingData)[which(names(scalingData)==method_col)] <- scaling_name
  by_ceds <- by_ceds <- merge(scalingData, scaling_map_sectors,
                              all = TRUE)
  by_ceds <- by_ceds[ , c('iso',ceds_matchcol_name,'year',valueCol)]
  names(by_ceds) <- c('iso',method_col,'year',valueLab) }

  return(by_ceds)
}


# ---------------------------------------------------------------------------------
# F.scaling
# Brief: produces scaling factors from aggregated ceds and inventory data
# Details: takes in the ceds data and the inventory data that has been put in ceds
#   standard form. Scaling factors are calculated by dividing inv data by ceds
#   data. The scaling factors are then interpolated and extrapolated to match
#   ceds years in a specified manner.
# Dependencies: CEDS_header.R, F.invAggregate(), F.cedsAggregate
# Author: Tyler Pitkanen, Rachel Hoesly, Linh Vu
# parameters:
#   ceds_data:      ceds data for the inventory's region [default: ceds_data]
#   inv_data:       inv data in ceds standard form [default: inv_data]
#   region:   the iso code(s) of the region(s) corresponding to the regions
#             covered by the inventory [required]
#   ext_start_year: year to extend scaling factors back to.
#             Default to global environment variable ‘start_year’ - 1960
#   ext_end_year: year to extend scaling factors forward to.
#             Default to global environment variable ‘end_year’ - 2013
#   interp_default: default interpolation method for scaling factors within the
#             inventory years. Either ‘interpolation’ or ‘constant’.
#             Defaults to linear interpolation.
#   pre_ext_default = default extrapolation method for pre inventory years.
#             Either ‘interpolation’, 'linear_1', or ‘constant’. Defaults to ‘constant’.
#   post_ext_default = default extrapolation method for post inventory years.
#             Either ‘interpolation’, 'linear_1', or ‘constant’. Defaults to ‘constant’.
#   replacement_method = Either 'none' or ‘replace’. If ‘replace’ then function checks scaling
#             factors and replaces values above and below the threshold defined by max scaling factor.
#   max_scaling_factor = if  replacement method = ‘replace’.  Scaling factors greater
#             than max_scaling_factors and less than 1/maximum_scaling_factor are replaced by
#   replacement_scaling factor or 1/replacement_scaling_factor.
#   replacement_scaling_factor = value to replace too small/large scaling factors with.
#             Defaults to max_scaling_factor.
#   return: scaling_ext
#   input files: null
#   output files: null

F.scaling <- function( ceds_data, inv_data, region,
                       ext_start_year = start_year, ext_end_year = end_year,
                       ext = TRUE, interp_default = 'linear',
                       pre_ext_default = 'constant', post_ext_default = 'constant',
                       replacement_method = 'none', max_scaling_factor = 100,
                       replacement_scaling_factor = max_scaling_factor,
                       meta = TRUE) {

# by pass entire function if there is no inventory data
  if ( nrow (inv_data) == 0 ){
    printLog ( paste('There is no inventory for', em, 'emission species') )

    out <- data.frame( matrix ( ncol = length ( c('iso',scaling_name, 'year','scaling_factor')  ) , nrow=0 ) )
    names(out) <- c('iso',scaling_name, 'year','scaling_factor')

    scaling_ext <- data.frame( matrix ( ncol = length ( c('iso',scaling_name, X_inv_years )  ) , nrow=0 ) )
    names(scaling_ext) <- c('iso',scaling_name, X_inv_years )

    meta_notes <- data.frame( matrix ( ncol = length ( c('iso',scaling_name,'year','comment')  ) , nrow=0 ) )
    meta_notes <- c('iso',scaling_name,'year','comment')

    list.out <- list(out, scaling_ext ,meta_notes)
    names(list.out) <- c( 'scaling_factors', 'scaling_factors_wide' ,'meta_notes')

  } else{

#   ext_start_year = start_year
#   ext_end_year = end_year
#   ext = TRUE
#   interp_default = 'linear'
#   pre_ext_default = 'constant'
#   post_ext_default = 'constant'
#   replacement_method = 'replace'
#   max_scaling_factor = 100
#   replacement_scaling_factor = max_scaling_factor
#   meta = FALSE

  valid_interp_methods <- c('linear','constant')
  valid_pre_ext_methods  <- c('constant','linear_1')
  valid_post_ext_methods  <- c('linear','constant','linear_1')

  # ------------------------------------
  # Default Methods and Scaling years by country and sector/fuel

  # Check Default options
  if( interp_default %!in% valid_interp_methods) {
    warning( paste0('"',interp_default,
                    '" is not a valid interpolation method. Replacing interpolation default with "linear".'))
    interp_default <- 'linear'}
  if( pre_ext_default %!in% valid_pre_ext_methods) {
    warning( paste0('"',pre_ext_default,
                    '" is not a valid extrapolation method. Replacing pre-extrapolation default with "constant".'))
    pre_ext_default <- 'constant'}
  if( post_ext_default %!in% valid_post_ext_methods) {
    warning( paste0('"',post_ext_default,
                    '" is not a valid extrapolation method. Replacing post-extrapolation default with "constant".'))
    post_ext_default <- 'constant'}
  if( pre_ext_default == 'linear' && length(inv_years) == 1 ) {
    warning( 'Not enough inventory years to extend backward linearly. Replacing pre-extrapolation default with "constant".')
    pre_ext_default <- 'constant'
  }
  if( pre_ext_default == 'linear' && length(inv_years) == 1 ) {
    warning( 'Not enough inventory years to extend backward linearly. Replacing pre-extrapolation default with "constant".')
    pre_ext_default <- 'constant'
  }
  if( post_ext_default == 'linear' && length(inv_years) == 1 ) {
    warning( 'Not enough inventory years to extend backward linearly. Replacing post-extrapolation default with "constant".')
    post_ext_default <- 'constant'
  }

  # Check Years
  if( !(ext_start_year <= min(inv_years) & ext_start_year >= min(emissions_years) ) ){
    ext_start_year <- min(inv_data)
    warning( 'Invalid scaling start year ("ext_start_year"). Using the first year of scaling inventory data.')
  }
  if( !(ext_end_year >= max(inv_years) & ext_end_year <= max(emissions_years) ) ){
    ext_start_year <- max(inv_data)
    warning( 'Invalid scaling end year ("ext_end_year"). Using the last year of scaling inventory data.')
  }

  # Check Scaling Factor Replacement
  if (replacement_method %!in% c('none','replace')){
    warning( 'Invalid scaling factor replacement method. Must be "none" or "replace". Using default: "none"')
    replacement_method <- 'none'
  }
  if (replacement_method == 'replace'){
    if (!is.numeric(max_scaling_factor) ) {
      warning( 'Invalid maximum scaling factor ("max_scaling_factor").
               Must be numeric. Using default max scaling factor = 100')
      max_scaling_factor <- 100
    }
    if (!is.numeric(replacement_scaling_factor) ) {
      warning( 'Invalid replacement scaling factor ("replacement_scaling_factor").
               Must be numeric. Using default, max_scaling_factor')
      replacement_scaling_factor <- max_scaling_factor
    }
  }

  # Check Methods and replace with default if invalid
  if ( ! all( ext_method$interp_method %in% c(valid_interp_methods,'NA') )) {
    index <- which( ext_method$interp_method %in% valid_interp_methods == FALSE )
    warning( paste0(  ext_method$interp_method[index] , ': invalid interpolation method. Using default option: ',
                      "'" ,interp_default),"'" )
    ext_method$interp_method[index] <- interp_default }

  if ( ! all( ext_method$pre_ext_method %in% c(valid_pre_ext_methods,'NA') )) {
    index <- which( ext_method$pre_ext_method %in% valid_pre_ext_methods == FALSE )
    warning( paste0(  ext_method$pre_ext_method[index] , ': invalid pre-extrapolation method. Using default option: ',
                      "'" ,pre_ext_default),"'" )
    ext_method$pre_ext_method[index] <- pre_ext_default }

  if ( ! all( ext_method$post_ext_method %in% c(valid_post_ext_methods,'NA') )) {
    index <- which( ext_method$post_ext_method %in% valid_post_ext_methods == FALSE )
    warning( paste0(  ext_method$post_ext_method[index] , ': invalid pre-extrapolation method. Using default option: ',
                      "'" ,post_ext_default,"'") )
    ext_method$post_ext_method[index] <- post_ext_default }

  # ------------------------------------
  # Create Methods and Year Data frame

  # Define Default Methods Data frame, update with mapping file
  # replace "all" in iso and scaling sector variable
  replacement_variable <- c('iso','scaling_sector')
  if ( any( ext_method[, replacement_variable]=='all', na.rm = T ) ) {
    # Define helper columns to decide selection priority
    ext_method$all_count <- 0     # how many 'all' columns does current row have?
    ext_method$all_count[ ext_method$iso == 'all' | ext_method$scaling_sector == 'all' ] <- 1
    ext_method$all_count[ ext_method$iso == 'all' & ext_method$scaling_sector == 'all' ] <- 2
    ext_method$row_num <- seq( 1, nrow( ext_method ) )  # original row number

    # Expand 'all'
    for (i in seq_along(replacement_variable)){
      all <- ext_method[ext_method[,replacement_variable[i]] == 'all', ]
      while(nrow(all) > 0 ){
        line <- all[1,]
        if ( replacement_variable[i] == 'scaling_sector'){
          replace_col <- unique(scaling_map[,replacement_variable[i]])
          replace_col <- replace_col[!is.na(replace_col)]}
        if ( replacement_variable[i] == 'iso') replace_col <- region
        replace_rows <- do.call(rbind, replicate(length(replace_col), line, simplify=FALSE))
        replace_rows[,replacement_variable[i]] <- replace_col
        ext_method <- rbind(replace_rows, ext_method)
        all <- all[-1,]
      }
      ext_method <- ext_method[which(ext_method[,replacement_variable[i]] != 'all'),]
    }

    # For any iso+scaling_sector combination, select instruction from most specific to less specific,
    # then last row in original instruction file
    ext_method <- group_by( ext_method, iso, scaling_sector ) %>%
      filter( all_count == min( all_count ) ) %>%   # most specific
      group_by( iso, scaling_sector ) %>%
      filter( row_num == max( row_num ) )           # last row

    # Remove duplicates and extra columns
    ext_method <- select( ext_method, -all_count, -row_num ) %>% unique() %>% data.frame()
  }

  # defaults
  ext_method_default <- inv_data[,c('iso',scaling_name)]
  ext_method_default[,'interp_method'] <- interp_default
  ext_method_default[,'pre_ext_method'] <- pre_ext_default
  ext_method_default[,'post_ext_method'] <- post_ext_default

  if( 'other' %in% names(ext_method)) ext_method_default[,'other'] <- NA

  # update with mapping file
  methods <- c('interp_method','pre_ext_method','post_ext_method')
  if( 'other' %in% names(ext_method)) methods <- c(methods, 'other')
  for( n in seq_along(methods)){
    ext_method_default <- replaceValueColMatch(ext_method_default, ext_method,
                                               x.ColName = methods[n],
                                               match.x = c('iso',scaling_name),
                                               addEntries = F)

  }

  # Define Default Years, update with mapping file
  # expand all option for iso and scaling_sector
  replacement_variable <- c('iso','scaling_sector')
  if ( any( ext_year[, replacement_variable]=='all', na.rm = T ) ) {
    # Define helper columns to decide selection priority
    ext_year$all_count <- 0     # how many 'all' columns does current row have?
    ext_year$all_count[ ext_year$iso == 'all' | ext_year$scaling_sector == 'all' ] <- 1
    ext_year$all_count[ ext_year$iso == 'all' & ext_year$scaling_sector == 'all' ] <- 2
    ext_year$row_num <- seq( 1, nrow( ext_year ) )  # original row number
    ext_year$is_range <- ext_year$start_scaling_year != 'NA'  # scaled by single year or year range?

    # Expand 'all'
    for (i in seq_along(replacement_variable)){
      all <- ext_year[ext_year[,replacement_variable[i]] == 'all', ]
      while(nrow(all) > 0 ){
        line <- all[1,]
        if ( replacement_variable[i] == 'scaling_sector'){
          replace_col <- unique(scaling_map[,replacement_variable[i]])
          replace_col <- replace_col[!is.na(replace_col)]}
        if ( replacement_variable[i] == 'iso') replace_col <- region
        replace_rows <- do.call(rbind, replicate(length(replace_col), line, simplify=FALSE))
        replace_rows[,replacement_variable[i]] <- replace_col
        ext_year <- rbind(replace_rows, ext_year)
        all <- all[-1,]
      }
      ext_year <- ext_year[which(ext_year[,replacement_variable[i]] != 'all'),]
    }

    # For any iso+scaling_sector combination, select instruction from most specific to less specific,
    # then last row in original instruction file. Do this for range scaling only.
    ext_year_range <- filter( ext_year, is_range ) %>%
      group_by( iso, scaling_sector ) %>%
      filter( all_count == min( all_count ) ) %>%     # most specific
      group_by( iso, scaling_sector ) %>%
      filter( row_num == max( row_num ) ) %>%         # last row
      data.frame()

    # Combine with single-year scale
    ext_year <- filter( ext_year, !is_range ) %>% rbind( ext_year_range )

    # Remove duplicates and extra columns
    ext_year <- select( ext_year, -all_count, -row_num, -is_range ) %>% unique() %>% data.frame()
  }

  # defaults
  ext_year_default <- inv_data[,c('iso',scaling_name)]
  ext_year_default[,'pre_ext_year'] <- ext_start_year
  ext_year_default[,'post_ext_year'] <- ext_end_year

  # update with mapping
  years <- c('pre_ext_year','post_ext_year')
  ext_year <- replace(ext_year, ext_year=='NA', NA)
  ext_year$pre_ext_year[is.na(ext_year$pre_ext_year)] <- ext_start_year
  ext_year$post_ext_year[is.na(ext_year$post_ext_year)] <- ext_end_year
  for( n in seq_along(years)){

    ext_year_default <- replaceValueColMatch(ext_year_default, ext_year,
                                               x.ColName = years[n],
                                               match.x = c('iso',scaling_name),
                                               addEntries = F)
  }

  # ------------------------------------
  # Create Meta data notes

  if (meta == TRUE) {
    names <- c('iso',scaling_name,'year','meta_comment')
    meta_notes <- data.frame(matrix(ncol = length(names), nrow = 0))
    names(meta_notes) <- names
  }

  # ------------------------------------
  # Select Inventory Years

  inv_data_original <- inv_data
  scaling_instructions_all <- ext_year

  if ( length(scaling_instructions_all$start_scaling_year) > 0 |
       length(scaling_instructions_all$end_scaling_year) > 0 |
       length(scaling_instructions_all$select_scaling_year) > 0 ) {

    scaling_iso_sectors <- unique(scaling_instructions_all[,c('iso',scaling_name)])

    for ( i in seq_along(scaling_iso_sectors[,1])){
  ###### need to change for method = sector and fuel
    instructions <- scaling_instructions_all[which( scaling_instructions_all$iso == scaling_iso_sectors$iso[i] &
                                    scaling_instructions_all$scaling_sector == scaling_iso_sectors$scaling_sector[i]  ), ]
    # find scaling years to keep for iso-sector combination
    scaling_years <- c()
    for (n in seq_along(instructions[,1])){
      #range years
      if( !anyNA(instructions[n, c('start_scaling_year','end_scaling_year')]) )
        scaling_years <- c(scaling_years, instructions[n, c('start_scaling_year')]:instructions[n, c('end_scaling_year')] )
      # individual years
      if( !is.na(instructions[n, c('select_scaling_year')]) )
        scaling_years <- c(scaling_years, instructions[n, c('select_scaling_year')] )
      }
     # years in inventory but not scaling
      not_scaling_years <- inv_years[inv_years %!in% scaling_years]
     # make not scaled years, NA
      inv_data[which( inv_data$iso == scaling_iso_sectors$iso[i] &
                      inv_data$scaling_sector == scaling_iso_sectors$scaling_sector[i]  ),
               paste0('X',not_scaling_years)] <- NA
  }

  }

  writeData(inv_data_original, 'DIAG_OUT', paste0('F.',em,'_',inv_name,'_orginal_inventory_data'), meta = FALSE)
  writeData(inv_data, 'DIAG_OUT', paste0('F.',em,'_',inv_name,'_scaling_years_inventory_data'),meta = FALSE)

  # ------------------------------------
  # Calculate the scaling factor

  printLog('Calculating Scaling Factors')
  #   by taking the ratio of CECS data over inventory
  #   data. This only gives a partial set of the scaling factors because they are
  #   only calculated where inventory data is available. The remaining scaling
  #   factors are calculated through interpolation and extrapolation.

  inv_long <- melt(inv_data, id= c('iso', scaling_name))
  names(inv_long)[which(names(inv_long)=='value')] <- 'inv_value'
  ceds_long <- melt(ceds_data, id= c('iso', scaling_name))
  names(ceds_long)[which(names(ceds_long)=='value')] <- 'ceds_value'

  scaling <- merge(inv_long, ceds_long , all.x = TRUE)
  scaling$scaling_factor <- scaling$inv_value/scaling$ceds_value


  # Make adjustments to the preliminary scaling factor data set:
  # Inf values - divide by zero - ceds value = 0
  scaling[which(scaling$scaling_factor==Inf),'scaling_factor'] <- NA
  scaling[which(scaling$scaling_factor==0),'scaling_factor'] <- NA

  scaling <- scaling[which( !is.na(scaling$scaling_factor)),]

  # Add Meta notes
  if (meta == TRUE) {
    meta_add <- scaling[,c('iso',scaling_name,'variable')]
    names(meta_add) <-  c('iso',scaling_name,'year')
    meta_add$comment <- paste('Scaled to Inventory -', inv_name )
    meta_notes <- rbind(meta_notes, meta_add) }


  # Cast melted data
  if ( method %in% c("sector","fuel") ) cast.formula <- paste('iso +', scaling_name , '~ variable')
  if ( method == "both" ) cast.formula <- 'iso + scaling_sector + scaling_fuel ~ variable'
  scaling <- as.data.frame(cast(scaling, cast.formula, value='scaling_factor'))

  # ------------------------------------
  # Check validity of and replace Scaling Factors

  if ( replacement_method == 'replace'){
    printLog( "Checking scaling factors... " )

    if (is.na(replacement_scaling_factor)) replacement_scaling_factor <- max_scaling_factor

    max <- max_scaling_factor
    min <- 1/max_scaling_factor

    temp_X_years <- names( scaling )[ grepl( "X", names( scaling ) ) ]
    index <- rbind( which( scaling[,temp_X_years] >= max , arr.ind=T ) ,
                    which( scaling[,temp_X_years] <= min , arr.ind=T) )

    if(nrow(index)>0){
      printLog( "Replacing very large/small scaling factors." )
      problem_scaling_factors <- melt(scaling, id.vars = c('iso', scaling_name))
      problem_scaling_factors <- problem_scaling_factors[!is.na(problem_scaling_factors$value),]
      problem_scaling_factors <- problem_scaling_factors[  problem_scaling_factors$value >= max |
                                                             problem_scaling_factors$value <= min , ]
      names(problem_scaling_factors) <- c('iso',scaling_name,'year', 'calculated_scaling_factor')
      problem_scaling_factors <- merge(problem_scaling_factors,inv_long,
                                        all.x=TRUE,all.y=FALSE,
                                        by.x = c('iso',scaling_name,'year'),
                                        by.y = c('iso',scaling_name,'variable'))
      problem_scaling_factors <- merge(problem_scaling_factors,ceds_long,
                                        all.x=TRUE,all.y=FALSE,
                                        by.x = c('iso',scaling_name,'year'),
                                        by.y = c('iso',scaling_name,'variable'))

      if(meta == TRUE) {
        add.max <- problem_scaling_factors[  problem_scaling_factors$calculated_scaling_factor >= max,
                                             c('iso',scaling_name,'year')]
        if(nrow(add.max)>0)  add.max[,c('comment')] <- paste0('Scaled to inventory ',inv_name,' - truncated at max scaling factor (','=', max_scaling_factor,')')

        add.min <- problem_scaling_factors[  problem_scaling_factors$calculated_scaling_factor <= min,
                                             c('iso',scaling_name,'year')]
        if(nrow(add.min)>0)  add.min[,c('comment')] <- paste0('Scaled to inventory ',inv_name,' - truncated at min scaling factor ( ','= 1/', max_scaling_factor,')')

        add <- rbind(add.min,add.max)

        meta_notes <- replaceValueColMatch(meta_notes, add,
                                     x.ColName="comment",
                                     y.ColName = "comment",
                                     match.x=c('iso',scaling_name,'year'),
                                     match.y=c('iso',scaling_name,'year'),
                                     addEntries = TRUE)
      }

      writeData( problem_scaling_factors , domain = "DIAG_OUT" , paste0('F.',em,'_Problem_Scaling_Factors_',inv_name), meta = FALSE )

      scaling[,temp_X_years] <- replace(scaling[,temp_X_years],
                                                 scaling[,temp_X_years] > max , max )
      scaling[,temp_X_years] <- replace(scaling[,temp_X_years],
                                                 scaling[,temp_X_years] < min , min )
    }  }

  # ------------------------------------
  # Check Scaling Factors against default method/year

  if( nrow(ext_method_default)  != nrow(scaling) |
      !identical(scaling$iso,ext_method_default$iso) |
      !identical(scaling$iso,ext_year_default$iso)){

    old_method <- ext_method_default
    old_yr <- ext_year_default
    new_method <- scaling[,c('iso',scaling_name)]
    new_yr <- scaling[,c('iso',scaling_name)]
    new_method[,c('interp_method','pre_ext_method','post_ext_method')] <- NA
    new_yr[,c("pre_ext_year","post_ext_year")] <- NA

    new_method <- replaceValueColMatch(new_method, old_method,
                        x.ColName = c('interp_method','pre_ext_method','post_ext_method'),
                        match.x = c('iso',scaling_name),
                        addEntries = FALSE)
  if( 'other' %in% names(old_method)) {
    new_method$other <- NA
    new_method <- replaceValueColMatch(new_method, old_method,
                         x.ColName = c('other'),
                         match.x = c('iso',scaling_name),
                         addEntries = FALSE)
  }
    new_year <- replaceValueColMatch(new_yr, old_yr,
                         x.ColName = c("pre_ext_year","post_ext_year"),
                         match.x = c('iso',scaling_name),
                         addEntries = FALSE)

    ext_method_default <- new_method
    ext_year_default <- new_year

  if( nrow(ext_method_default)  != nrow(scaling) |
     !identical(scaling$iso,ext_method_default$iso) |
     !identical(scaling$iso,ext_year_default$iso) ){stop('In F.scaling, scaling factor
           data frame and default extension methods do not match.')}

  }

  # ------------------------------------
  # Extend Scaling Factors
  # with sector specific methods for X_scaling_years to all X_emissions_years,
  #   i.e. fill in the gaps where inv data isn't present. needed_years represents
  #   the years where an estimation (inter/extrapolation) is needed

  printLog('Extending scaling factors over scaling years')
  # ------------------------------------
  # Interpolation
  printLog('Scaling Factors - Interpolating...')
  # Fill missing years with NAs
  X_inv_years_full <- paste0( 'X', min(inv_years):max(inv_years)  )
  scaling_interp <- as.data.frame(matrix(data=NA, nrow = nrow(scaling), ncol = length(X_inv_years_full)))
  scaling_interp <- cbind( scaling[,c('iso', scaling_name)],scaling_interp)
  names(scaling_interp) <- c('iso', scaling_name , X_inv_years_full )
  scaling_interp[, temp_X_years ] <- scaling[ , temp_X_years]

  # identify any non-trailing/leading na's
  interpolation_rows<-c()
  for( i in seq_along(scaling_interp$iso)){
    row <- as.numeric(scaling_interp[i,X_inv_years_full])
    if( length(rle(is.na(c(NA,row,NA)))$values)>3 & sum(!is.na(row)) != 1  ) {
      interpolation_rows<- rbind(interpolation_rows,i)}
  }
  interpolation_rows <- as.vector(interpolation_rows)

  #############
  # Interpolate

  if( length(interpolation_rows)>0){
    linear_rows <- which(ext_method_default$interp_method == 'linear')
    linear <- scaling_interp[which(ext_method_default$interp_method == 'linear'),]
    #remove rows with all NA data
    linear <-  linear[!apply(linear[,X_inv_years_full], 1, all.na),]
    cant_interp <- linear[  rowSums(!is.na(linear[,X_inv_years_full])) == 1 ,]
    linear <- linear[  rowSums(!is.na(linear[,X_inv_years_full])) != 1 ,]
    if ( nrow(linear)>0){
      # Add Meta notes
      if (meta == TRUE) {
        meta_pre_add <- scaling_interp[interpolation_rows,]
        meta_pre_add <- merge(meta_pre_add, ext_method_default[,c('iso', scaling_name,'interp_method')],
                              all.x=TRUE, all.y=FALSE)
        meta_pre_add <- meta_pre_add[which(meta_pre_add$interp_method == 'linear'),c('iso',scaling_name,X_inv_years_full)]

        meta_add <- c()
        for ( i in seq_along(meta_pre_add$iso)){
          row <- meta_pre_add[i,c('iso',scaling_name,X_inv_years_full)]
          min <- min( match( as.numeric(row[1,X_inv_years_full]), row[1,]), na.rm=TRUE)
          max <- max( match( as.numeric(row[1,X_inv_years_full]), row[1,]), na.rm=TRUE)
          non.trailing <- cbind( row[,c('iso',scaling_name)]  , row[, c( min:max)] )
          add <- melt(non.trailing, id.vars = c('iso',scaling_name))
          add <- add[which(is.na(add$value)),c('iso',scaling_name,'variable')]
          meta_add <- rbind(meta_add, add)
        }
        if(nrow(meta_add)>0){
          names(meta_add) <- c('iso',scaling_name,'year')
          meta_add$comment <- paste('Scaled by linearly interpolated scaling factor from inventory -', inv_name)
          meta_notes <- replaceValueColMatch(meta_notes, meta_add,
                                             x.ColName="comment",
                                             y.ColName = "comment",
                                             match.x=c('iso',scaling_name,'year'),
                                             match.y=c('iso',scaling_name,'year'),
                                             addEntries = TRUE)
        }}

      linear_int <- interpolate_NAs2( linear[,X_inv_years_full] )
      linear <- cbind( linear[,c('iso', scaling_name)] , linear_int)
      names(linear) <- c('iso', scaling_name , X_inv_years_full ) }
    if (nrow(cant_interp)>0) linear <- rbind(linear,cant_interp)
    constant <- scaling_interp[which(ext_method_default$interp_method == 'constant'),]
    constant <-  constant[apply(constant[,X_inv_years_full], 1, all.na),]
    if( nrow(constant)>0){
      # Add Meta notes
      if (meta == TRUE) {
        meta_pre_add <- scaling_interp[interpolation_rows,]
        meta_pre_add <- merge(meta_pre_add, ext_method_default[,c('iso', scaling_name,'interp_method')],
                              all.x=TRUE, all.y=FALSE)
        meta_pre_add <- meta_pre_add[which(meta_pre_add$interp_method == 'constant'),c('iso',scaling_name,X_inv_years_full)]

        meta_add <- c()
        for ( i in seq_along(meta_pre_add$iso)){
          row <- meta_pre_add[i,c('iso',scaling_name,X_inv_years_full)]
          min <- min( match( as.numeric(row[1,X_inv_years_full]), row[1,]), na.rm=TRUE)
          max <- max( match( as.numeric(row[1,X_inv_years_full]), row[1,]), na.rm=TRUE)
          non.trailing <- cbind( row[,c('iso',scaling_name)]  , row[, c( min:max)] )
          add <- melt(non.trailing, id.vars = c('iso',scaling_name))
          add <- add[which(is.na(add$value)),c('iso',scaling_name,'variable')]
          meta_add <- rbind(meta_add, add)
        }

        if(nrow(meta_add)>0){
          names(meta_add) <- c('iso',scaling_name,'year')
          meta_add$comment <- paste('Scaled by constantly interpolated scaling factor from inventory -', inv_name)
          meta_notes <- replaceValueColMatch(meta_notes, meta_add,
                                             x.ColName="comment",
                                             y.ColName = "comment",
                                             match.x=c('iso',scaling_name,'year'),
                                             match.y=c('iso',scaling_name,'year'),
                                             addEntries = TRUE)
          if( nrow(meta_notes) != nrow(unique(meta_notes[,c('iso',scaling_name,'year')])) ) stop('Error in value-meta_data.
                                                                        Duplicate entries in Interpolation Section')
        }}


      constant_int <- t( na.locf( t(constant[,X_inv_years_full]) , na.rm = FALSE ) )
      constant <- cbind( constant[,c('iso', scaling_name)] , constant_int)
      names(constant) <- c('iso', scaling_name , X_inv_years_full ) }

    scaling_interp <- rbind(linear, constant)

    if(method %in% c('sector','fuel'))
      scaling_interp <- scaling_interp[ order(scaling_interp[,'iso'], scaling_interp[,scaling_name]),]
    if(method == 'both')
      scaling_interp <- scaling_interp[ order(scaling_interp$iso, scaling_interp$scaling_sector, scaling_interp$scaling_fuel),]

  } else {scaling_interp <- scaling
  X_inv_years_full <- X_inv_years}

  # ------------------------------------
  # Extend Scaling Factors through Scaling Years and fill remaing (with scaling factor = 1)
  printLog('Scaling Factors - Extending...')

  scaling_ext <- as.data.frame(matrix(data=NA,nrow = nrow(scaling_interp), ncol = length(X_emissions_years)))
  names(scaling_ext) <- X_emissions_years
  scaling_ext <- cbind(scaling_interp[,c('iso', scaling_name)], scaling_ext)
  #
  for (i in seq_along(scaling_ext$iso)){
    # Interpolated inventory data
    scaling_ext[i,X_inv_years_full] <- scaling_interp[i,X_inv_years_full]

    if( !all.na( scaling_interp[i, X_inv_years_full] ) ) {

      # Pre-Extrapolation
      min_inv_year <- emissions_years[ min( which(!is.na(scaling_ext[i,X_emissions_years]))) ]
      if( min_inv_year > ext_year_default[i,'pre_ext_year']){
        # Define Pre-Extrapolation Years
        pre_scaling_ext_years <- c( ext_year_default[i,'pre_ext_year']:(min_inv_year-1) )
        X_pre_scaling_ext_years <- paste0( 'X', pre_scaling_ext_years )
        # Fill Years with scaling factor = 1, NA, or interpolated Scaling factor
        pre_scaling_ext_line <- as.data.frame(matrix(data=NA, nrow = 1,
                                                     ncol = length(X_pre_scaling_ext_years)+length(X_inv_years_full)   ))
        names(pre_scaling_ext_line)<- c( X_pre_scaling_ext_years , X_inv_years_full )
        pre_scaling_ext_line[1,X_inv_years_full] <- scaling_ext[i,X_inv_years_full]

        # Linear Extrapolation
        if( ext_method_default[i,'pre_ext_method'] == 'linear' ){
          x = min_inv_year:max(inv_years)
          y = t(scaling_interp[i,X_inv_years_full])
          xout = pre_scaling_ext_years

          if ( !length(x[complete.cases(x)]) > 1){
            printLog('Not enough data points to Linearly interpolate. Constantly extending.')
            ext_method_default[i,'pre_ext_method'] <- 'constant'
          }
          if (length(x[complete.cases(x)]) > 1){

            pre.lm <- lm(y ~ x)
            fitted <- predict(pre.lm, data.frame(x=xout),interval='none')

            pre_scaling_ext_line[1,X_pre_scaling_ext_years] <- fitted
            # Add meta notes
            if(meta==TRUE){
              year <- X_pre_scaling_ext_years
              add <- data.frame(year)
              add$iso <- scaling_ext[i,c('iso')]
              if(method %in% c('sector','fuel')) add[,scaling_name] <- scaling_ext[i,scaling_name]
              if(method %in% c('both')) add[,scaling_name] <- t(replicate(scaling_ext[i,scaling_name],n=length(year)))

              add$comment <- paste('Scaled by linearly pre-extended scaling factor from inventory -', inv_name)
              meta_notes <- replaceValueColMatch(meta_notes, add,
                                                 x.ColName="comment",
                                                 y.ColName = "comment",
                                                 match.x=c('iso',scaling_name,'year'),
                                                 match.y=c('iso',scaling_name,'year'),
                                                 addEntries = TRUE)

              if( nrow(meta_notes) != nrow(unique(meta_notes[,c('iso',scaling_name,'year')])) ) stop('Error in value-meta_data.
                                                                        Duplicate entries in Pre extrapolation Section')

#               add$comment <-  paste('Scaled to Inventory - Linearly extrapolated backward -', inv_name)
#               meta_notes <- rbind(meta_notes, add)
            } }
          # if linear interpolation, but only 1 inventory year
        } else if( ext_method_default[i,'pre_ext_method'] == 'linear' && length(X_inv_years_full)>1 ){
          ext_method_default[i,'pre_ext_method'] <- 'constant'
          printLog('Not enough data to linearly extend. Constant extending backward.')
          # Constant Extrapolation
        } else if( ext_method_default[i,'pre_ext_method'] == 'constant'){
          # Add meta notes
          if(meta==TRUE){
            year <- X_pre_scaling_ext_years
            add <- data.frame(year)
            add$iso <- scaling_ext[i,c('iso')]
            if(method %in% c('sector','fuel')) add[,scaling_name] <- scaling_ext[i,scaling_name]
            if(method %in% c('both')) add[,scaling_name] <- t(replicate(scaling_ext[i,scaling_name],n=length(year)))

            add$comment <- paste('Scaled by constantly pre-extended scaling factor from inventory -', inv_name)
            meta_notes <- replaceValueColMatch(meta_notes, add,
                                               x.ColName="comment",
                                               y.ColName = "comment",
                                               match.x=c('iso',scaling_name,'year'),
                                               match.y=c('iso',scaling_name,'year'),
                                               addEntries = TRUE)
            if( nrow(meta_notes) != nrow(unique(meta_notes[,c('iso',scaling_name,'year')])) ) stop('Error in value-meta_data.
                                                                        Duplicate entries in pre-extrapolation Section')

#             add$comment <-  paste('Scaled to Inventory - constant extrapolated backward -', inv_name)
#             meta_notes <- rbind(meta_notes, add)
          }
          pre_scaling_ext_line[1,] <-t(na.locf(t(pre_scaling_ext_line[1,]), fromLast = TRUE, na.rm = FALSE))
          # Linear Extrapolation to Scaling Factor = 1, from most recent value
        } else if( ext_method_default[i,'pre_ext_method'] == 'linear_1'){
          if( ext_method_default[i,'other'] ==  ext_year_default[i,'pre_ext_year'] ){
          pre_scaling_ext_line[1,1]<-1
          pre_scaling_ext_line[1,] <- interpolate_NAs(pre_scaling_ext_line[1,])
          } else if(ext_method_default[i,'other'] >  ext_year_default[i,'pre_ext_year']){
          pre_scaling_ext_line[1,paste0('X',ext_method_default[i,'other'])] <- 1
          pre_scaling_ext_line[1,] <- interpolate_NAs(pre_scaling_ext_line[1,])
          pre_scaling_ext_line[1,] <- na.locf(  t(pre_scaling_ext_line[1,]) , fromLast=TRUE ,na.rm=FALSE ,maxgap = Inf)
          }

          # Add meta notes
          if(meta==TRUE){
            year <- X_pre_scaling_ext_years
            add <- data.frame(year)
            add$iso <- scaling_ext[i,c('iso')]
            if(method %in% c('sector','fuel')) add[,scaling_name] <- scaling_ext[i,scaling_name]
            if(method %in% c('both')) add[,scaling_name] <- t(replicate(scaling_ext[i,scaling_name],n=length(year)))

            add$comment <- paste('Scaled by linear pre-extended to 1 scaling factor from inventory -', inv_name)
            meta_notes <- replaceValueColMatch(meta_notes, add,
                                               x.ColName="comment",
                                               y.ColName = "comment",
                                               match.x=c('iso',scaling_name,'year'),
                                               match.y=c('iso',scaling_name,'year'),
                                               addEntries = TRUE)
            if( nrow(meta_notes) != nrow(unique(meta_notes[,c('iso',scaling_name,'year')])) ) stop('Error in value-meta_data.
                                                                        Duplicate entries in pre extrapolation Section')

#             add$comment <-  paste('Scaled to Inventory - Linearly extrapolated backward to 1 -', inv_name)
#             meta_notes <- rbind(meta_notes, add)
          }
        }
        scaling_ext[i,X_pre_scaling_ext_years] <- pre_scaling_ext_line[1,X_pre_scaling_ext_years]
      } # end Pre-Extrapolation

      # Post-Extrapolation
      max_inv_year <- emissions_years[ max( which(!is.na(scaling_ext[i,X_emissions_years]))) ]
      if( max_inv_year < ext_year_default[i,'post_ext_year']){
        # Define post-Extrapolation Years
        post_scaling_ext_years <- c( max_inv_year:ext_year_default[i,'post_ext_year'] )
        post_scaling_ext_years <- post_scaling_ext_years[-1]
        X_post_scaling_ext_years <- paste0( 'X', post_scaling_ext_years )
        # Fill Years with scaling factor = 1, NA, or interpolated Scaling factor
        post_scaling_ext_line <- as.data.frame(matrix(data=NA, nrow = 1,
                                                      ncol = length(X_post_scaling_ext_years)+length(X_inv_years_full)   ))
        names(post_scaling_ext_line)<- c(  X_inv_years_full, X_post_scaling_ext_years  )
        post_scaling_ext_line[1,X_inv_years_full] <- scaling_ext[i,X_inv_years_full]

        # Linear Extrapolation
        if( ext_method_default[i,'post_ext_method'] == 'linear'){
          x = min(inv_years):max_inv_year
          y = t(scaling_ext[i,X_inv_years_full])
          xout = post_scaling_ext_years

          if ( !length(x[complete.cases(x)]) > 1){
            printLog('Not enough data points to Linearly interpolate. Constantly extending.')
            ext_method_default[i,'pre_ext_method'] <- 'constant'
          }
          if (length(x[complete.cases(x)]) > 1){

            post.lm <- lm(y ~ x)
            fitted <- predict(post.lm, data.frame(x=xout),interval='none')

            post_scaling_ext_line[1,X_post_scaling_ext_years] <- fitted
            # Add meta notes
            if(meta==TRUE){
              year <- X_post_scaling_ext_years
              add <- data.frame(year)
              add$iso <- scaling_ext[i,c('iso')]
              if(method %in% c('sector','fuel')) add[,scaling_name] <- scaling_ext[i,scaling_name]
              if(method %in% c('both')) add[,scaling_name] <- t(replicate(scaling_ext[i,scaling_name],n=length(year)))

              add$comment <- paste('Scaled by linearly post-extended scaling factor from inventory -', inv_name)
              meta_notes <- replaceValueColMatch(meta_notes, add,
                                                 x.ColName="comment",
                                                 y.ColName = "comment",
                                                 match.x=c('iso',scaling_name,'year'),
                                                 match.y=c('iso',scaling_name,'year'),
                                                 addEntries = TRUE)
              if( nrow(meta_notes) != nrow(unique(meta_notes[,c('iso',scaling_name,'year')])) ) stop('Error in value-meta_data.
                                                                        Duplicate entries in post-extrapolation Section')

#               add$comment <-  paste('Scaled to Inventory - Linearly extrapolated forward -', inv_name)
#               meta_notes <- rbind(meta_notes, add)
            }
          }}
        # Constant Extrapolation
        else if( ext_method_default[i,'post_ext_method'] == 'constant'){
          post_scaling_ext_line[1,] <-t(na.locf(t(post_scaling_ext_line[1,])))
          # Add meta notes
          if(meta==TRUE){
            year <- X_post_scaling_ext_years
            add <- data.frame(year)
            add$iso <- scaling_ext[i,c('iso')]
            if(method %in% c('sector','fuel')) add[,scaling_name] <- scaling_ext[i,scaling_name]
            if(method %in% c('both')) add[,scaling_name] <- t(replicate(scaling_ext[i,scaling_name],n=length(year)))

            add$comment <- paste('Scaled by constantly post-extended scaling factor from inventory -', inv_name)
            meta_notes <- replaceValueColMatch(meta_notes, add,
                                               x.ColName="comment",
                                               y.ColName = "comment",
                                               match.x=c('iso',scaling_name,'year'),
                                               match.y=c('iso',scaling_name,'year'),
                                               addEntries = TRUE)
            if( nrow(meta_notes) != nrow(unique(meta_notes[,c('iso',scaling_name,'year')])) ) stop('Error in value-meta_data.
                                                                        Duplicate entries in post extrapolation Section')

#             add$comment <-  paste('Scaled to Inventory - Linearly extrapolated forward -', inv_name)
#             meta_notes <- rbind(meta_notes, add)
          }
        }
        # Linear Extrapolation to Scaling Factor = 1, from most recent value
        else if( ext_method_default[i,'post_ext_method'] == 'linear_1'){
          post_scaling_ext_line[1,ncol(post_scaling_ext_line)]<-1
          post_scaling_ext_line[1,] <- interpolate_NAs(post_scaling_ext_line[1,])
          # Add meta notes
          if(meta==TRUE){
            year <- X_post_scaling_ext_years
            add <- data.frame(year)
            add$iso <- scaling_ext[i,c('iso')]
            if(method %in% c('sector','fuel')) add[,scaling_name] <- scaling_ext[i,scaling_name]
            if(method %in% c('both')) add[,scaling_name] <- t(replicate(scaling_ext[i,scaling_name],n=length(year)))

            add$comment <- paste('Scaled by linearly post-extended to 1 scaling factor from inventory -', inv_name)
            meta_notes <- replaceValueColMatch(meta_notes, add,
                                               x.ColName="comment",
                                               y.ColName = "comment",
                                               match.x=c('iso',scaling_name,'year'),
                                               match.y=c('iso',scaling_name,'year'),
                                               addEntries = TRUE)
            if( nrow(meta_notes) != nrow(unique(meta_notes[,c('iso',scaling_name,'year')])) ) stop('Error in value-meta_data.
                                                                        Duplicate entries in post-extrapolation Section')

#             add$comment <-  paste('Scaled to Inventory - Linearly extrapolated forward -', inv_name)
#             meta_notes <- rbind(meta_notes, add)
          }
        }
        scaling_ext[i,X_post_scaling_ext_years] <- post_scaling_ext_line[1,X_post_scaling_ext_years]
      } # End Post-Extrapolation

      scaling_ext[,X_emissions_years] <- replace(scaling_ext[,X_emissions_years],
                                                 scaling_ext[,X_emissions_years] < 0 , 1/max_scaling_factor )

    }
    }# End for loop over all scaling countries and sector/fuels


  # ------------------------------------
  # Final processing

  printLog('Scaling Factors - Final processing...')
  out <- melt(scaling_ext, id.vars= c('iso', scaling_name))
  out <- out[complete.cases(out),]
  names(out) <- c('iso',scaling_name, 'year','scaling_factor')

  scaling_ext_byCEDS <- F.scalingToCeds(scalingData=scaling_ext, dataFormat = 'wide')
  writeData( scaling_ext_byCEDS , domain = "DIAG_OUT", paste0('F.',em,'_Scaling_Factors_ceds_sectors_',inv_name), meta = FALSE )

  writeData( scaling_ext , domain = "DIAG_OUT", paste0('F.',em,'_Scaling_Factors_scaling_sectors_',inv_name), meta = FALSE )

  list.out <- list(out, scaling_ext ,meta_notes)
  names(list.out) <- c( 'scaling_factors', 'scaling_factors_wide' ,'meta_notes')
  }

  return(list.out)
    }


# ---------------------------------------------------------------------------------
# F.applyScale
# Brief: produces scaling factors from aggregated ceds and inventory data
# Details:
# Dependencies: CEDS_header.R, F.invAggregate(), F.cedsAggregate
# Author: Rachel Hoesly
# parameters:
#
# return: dataframes of scaled emissions and scaled EFs
# input files: null
# output files: list of data frames containing scaled_em and scaled_ef

F.applyScale <- function(scaling_factors){

  if ( nrow (scaling_factors) == 0 ){
  printLog ( paste('There is no inventory or scaling factors for', em, 'emission species.') )

  ef_scaled <- data.frame( matrix ( ncol = length ( c('iso','sector','fuel','year','scaled')  ) , nrow=0 ) )
  names(ef_scaled) <- c('iso','sector','fuel','year','scaled')

  em_scaled <- data.frame( matrix ( ncol = length ( c('iso','sector','fuel','year','scaled')  ) , nrow=0 ) )
  names(em_scaled) <- c('iso','sector','fuel','year','scaled')

  out <-  list(ef_scaled,em_scaled)
  } else{

  printLog( "Applying scaling factors to CEDS EFs and emissions" )

  # Disaggregate from scaling sectors to CEDS sectors and create CEDS scaling that
  #   is matched to the CEDS rows from the input

  scaling_ceds_map_unique <- unique(scaling_map[complete.cases(scaling_map[,c(scaling_name,ceds_matchcol_name)]),
                                                c(scaling_name,ceds_matchcol_name)])

  # scaling factors by ceds sectors in long format
  scaling_factors_by_ceds <- merge(scaling_factors,
                                   scaling_ceds_map_unique,
                                   all = TRUE)
  scaling_factors_by_ceds <- scaling_factors_by_ceds[ , c('iso',ceds_matchcol_name,'year','scaling_factor')]
  names(scaling_factors_by_ceds)[which(names(scaling_factors_by_ceds)==ceds_matchcol_name)] <- method_col

  # current emissions/ef
  em_long <- input_em[ input_em$iso %in% region,]
  em_long <- melt(em_long, id.vars = c('iso','sector','fuel','units'))
  names(em_long) <- c('iso','sector','fuel','units', 'year', 'emissions')
  ef_long <- input_ef[ input_ef$iso %in% region,]
  ef_long <- melt(ef_long, id.vars = c('iso','sector','fuel','units'))
  names(ef_long) <- c('iso','sector','fuel','units', 'year', 'ef')

  # merge scaling factors and current emissions/ef
  em_long$scaling_factor <- NA
  em_scaled <- replaceValueColMatch(em_long, scaling_factors_by_ceds,
                                    x.ColName = 'scaling_factor',
                                    match.x = c('iso',method_col, 'year'),
                                    addEntries = FALSE)
  em_scaled <- em_scaled[complete.cases(em_scaled),]
  ef_long$scaling_factor <- NA
  ef_scaled <- replaceValueColMatch(ef_long, scaling_factors_by_ceds,
                                    x.ColName = 'scaling_factor',
                                    match.x = c('iso',method_col, 'year'),
                                    addEntries = FALSE)
  ef_scaled <- ef_scaled[complete.cases(ef_scaled),]

  # calculate new emissions/ef
  em_scaled$scaled <- em_scaled$emissions * em_scaled$scaling_factor
  ef_scaled$scaled <- ef_scaled$ef * ef_scaled$scaling_factor

  # drop emissions that are zero, remained unchanged, unneeded columns
  em_scaled <- em_scaled[which(em_scaled$scaled > 0) , c('iso','sector','fuel','year','scaled') ]
  ef_scaled <- ef_scaled[which(ef_scaled$scaled > 0) , c('iso','sector','fuel','year','scaled') ]

  #check duplicate entries
  em_scaled_unique <- unique(em_scaled[,c('iso','sector','fuel','year')])
  ef_scaled_unique <- unique(ef_scaled[,c('iso','sector','fuel','year')])

  if(nrow(em_scaled_unique) != nrow(em_scaled)) {
    writeData(em_scaled_unique, domain = "DIAG_OUT", fn = paste0( "F.", em, "_em_scaled_unique" ), meta = FALSE )
    writeData(em_scaled, domain = "DIAG_OUT", fn = paste0( "F.", em, "em_scaled" ), meta = FALSE )

    stop('duplicates in scaling factors, please check scaling map')
  }

  out <-  list(ef_scaled,em_scaled)
  }
  return(out)
}

# ---------------------------------------------------------------------------------
# F.create_EF_value_meta_heatmap
# Brief: Takes the value metadata from a scaling process and generates a heatmap display.
# Dependencies: CEDS_header.R, F.update_value_metadata
# Author: Ben Goldstein
# Params:
#     meta_notes: the metavalue notes. By default this is null and read in from "MED_OUT"
#     type: the scaling type (EF by default)
#     iso: the country code to develop the heatmap for
# return: null
# input files: "F.", em, "_", "scaled_",type,"-value_metadata"
# output files: "F.", em, "_", iso, "_value_metadata_heatmap"

    F.create_EF_value_meta_heatmap <- function (type = "EF", meta_notes = NULL, iso = NULL, sectors = 'all', country_map = NULL) {

    # Vectorize iso if it isn't a vector already (in the case of multiple isos
    #   being passed)
        if ( !is.vector(iso) ) {
            iso <- c( iso )
        }

    # Read in the country-to-sector maps and extract the name of the country we
    #   want, for a) plot title and b) to ensure the iso is valid
        if ( is.null( country_map ) ) {
            country_map <- readData("MAPPINGS", "Master_Country_List")
        }

        # If the iso is not valid, break
        if ( is.null(iso) || iso %!in% country_map$iso ) {
          stop( paste0( "Invalid iso '", iso, "' specified in F.create_EF_value_meta_heatmap." ) )
        }

        countryName <- paste( country_map$Country_Name[country_map$iso %in% iso], collapse = ', ' )

    # Print to log which country we're dealing with
        printLog( paste0("Creating the value metadata heatmap for ", countryName) )

    # If meta_notes were not specified, they will be read in from the default
    # output file for writing value metadata, then melted to long form for
    # individual cell processing.
        if ( is.null( meta_notes ) ) {
          meta_notes <- readData( "MED_OUT", paste0( "F.", em, "_", "scaled_",type,"-value_metadata" ), meta = FALSE, to_numeric=FALSE)
          meta_notes <- melt(meta_notes, id.vars = c('iso','sector','fuel'))
          names( meta_notes ) <- c( "iso", "sector", "fuel", "year", "comment" )
          meta_notes$comment <- as.character(meta_notes$comment)
        }

    # Forcats library is needed for reordering sectors ### Is this necessary?/Should it be somewhere else?
        library("forcats")

        printLog("Clipping to final scaling comments")

    # create "meta_split" which will hold only the final scaling factor
        meta_split <- meta_notes

    # remove the semicolon from the end of all non-default entries
        indices <- grepl( ";", meta_split$comment )
        meta_split$comment <- as.character(meta_split$comment)
        meta_split$comment[ indices ] <- substr(meta_split$comment[indices], 0, nchar(meta_split$comment[indices]) - 2)

    # Identify the number of sectors that are being plotted; if not all,
    #   remove whichever aren't specified
        fig_ratio <- 1
        if (sectors != 'all') {
          meta_split <- meta_split[ which( meta_split$sector %in% sectors), ]
          fig_ratio <- 2
        }

    # If they weren't removed already, we can discard the three sectors that
    #   aren't actually in CEDS
        sectors_to_remove <- c("11A_Volcanoes", "11B_Forest-fires", "11C_Other-natural")
        meta_split <- meta_split[ which( meta_split$sector %!in% sectors_to_remove), ]

    # Discard all value metadata notes that occur before the final semicolon
        indices <- grepl( ";", meta_split$comment )
        meta_split$comment[indices]  <- sub( ".*; ", "", meta_split$comment[indices] )
        meta_split$comment <- as.character(meta_split$comment)

    # Reclassify the notes for display purposes
        meta_classified <- F.reclass_metavalue(meta_split)

    # Remove the X from year values so it can be used as a continuous variable in plot
        meta_classified$year <- as.numeric( substr(as.character(meta_classified$year), 2, 5) )

        printLog( paste0( "Creating and writing scaling comments" ) )
        plot_title <- paste0("Sectoral factors for emission ",em,": ",countryName)

    # A list containing the color associated with each inventory value
        inventory_colors <- c( "Default" = "#cccccc",
                  "Zero emissions" = "#ffffff",
                  "EDGAR 4.3-PEGASOS" = "#026fff",
                  "EMEP_NFR09" = "#00BE67",
                  "REAS 2.1" = "#d966ff",
                  "EMEP_NFR14" = "#73e600",
                  "UNFCCC, 2015" = "#f75555",
                  "Environment Canada, 2013" = "#ff8c1a",
                  "Environment and Climate Change Canada, 2016" = "#ffe11a",
                  "US EPA, 2016" = "#990033",
                  "US" = "#1d3d84",
                  "Li et al., 2017" = "#fcde1e",
                  "TEPA, 2016" = "#1de0cc",
                  "Argentina UNFCCC submission, 2016" = "#ff8484",
                  "Kurokawa et. al, 2013" = "#990606",
                  "South Korea National Institute of Environmental Research, 2016" = "#875c1d",
                  "Australian Department of the Environment, 2016" = "#1c661b",
                  "EDGAR 4.2" = "#80d4ff" )

        meta_classified$isosector <- paste0( meta_classified$iso, meta_classified$sector )

        options( warn = -1 )
    # Create a formatted ggplot and save to output
        p <- ggplot( meta_classified, aes(year, y=fct_rev(reorder(isosector,isosector)))) +
             geom_raster(aes(fill = meta_classified$value, alpha = meta_classified$prepost)) +
             coord_fixed(ratio = fig_ratio) +
             theme(panel.background=element_blank(),
                  panel.border = element_rect(colour = "grey80", fill=NA, size=.8))+
             scale_alpha_discrete(range = c(1, 0.4)) +
             ylab("CEDS Sector") + xlab("") + ggtitle(plot_title) +
             labs(fill="Inventory", alpha="Extension") +
             theme(text = element_text(size=8),
                   axis.text.y = element_text(size = 6,angle=20, hjust=1)) +
             scale_x_continuous(breaks = round(seq(min(meta_classified$year), max(meta_classified$year), by = 10),1)) +
             scale_fill_manual(values = inventory_colors)

        ggsave( plot=p, paste0("../diagnostic-output/value-meta-heatmaps/",em,"_",paste0(iso, collapse = ''),"_value_metadata_heatmap.pdf"),
                device = "pdf", width=8.0, height=5.0)
        options( warn = 0 )
    }

# ---------------------------------------------------------------------------------
# F.reclass_metavalue
# Brief: Identifies inventory names and extension types from value metadata
# Details: This is an auxilliary function for F.create_EF_value_meta_heatmap
#          used to unify and classify metavalue notes for display by ggplot.
# Dependencies: CEDS_header.R, F.update_value_metadata
# Author: Ben Goldstein
# parameters:
#     meta: the value metadata notes
# return:
#     meta: the reclassified value metadata notes
# input files: none
# output files: none

    F.reclass_metavalue <- function (meta) {
    # Initialize value and prepost columns to defaults. These store the inventory
    # and the extension type for each scaling note.
        meta$value <- 'Default'
        meta$prepost <- 'Matched to inventory'

    # determine if it's pre- or post- scaled
        meta$prepost[grep("pre-extended", meta$comment)] <- "Pre- or post-extended"
        meta$prepost[grep("post-extended", meta$comment)] <- "Pre- or post-extended"

    # determine the scaling inventory
        meta$value[grep("_PG", meta$comment)] <- ("EDGAR 4.3-PEGASOS")
        meta$value[grep("EMEP_NFR09", meta$comment)] <- ("EMEP_NFR09")
        meta$value[grep("REAS", meta$comment)] <- ("REAS 2.1")
        meta$value[grep("EMEP_NFR14", meta$comment)] <- ("EMEP_NFR14")
        meta$value[grep("UNFCCC", meta$comment)] <- ("UNFCCC, 2015")
        meta$value[grep("CAN_to2011", meta$comment)] <- ("Environment Canada, 2013")
        meta$value[grep("CAN", meta$comment)] <- ("Environment and Climate Change Canada, 2016")
        meta$value[grep("US-EPA", meta$comment)] <- ("US EPA, 2016")
        meta$value[grep("US", meta$comment)] <- ("US")
        meta$value[grep("CHN", meta$comment)] <- ("Li et al., 2017")
        meta$value[grep("TWN", meta$comment)] <- ("TEPA, 2016")
        meta$value[grep("ARG", meta$comment)] <- ("Argentina UNFCCC submission, 2016")
        meta$value[grep("Japan", meta$comment)] <- ("Kurokawa et. al, 2013")
        meta$value[grep("SKorea", meta$comment)] <- ("South Korea National Institute of Environmental Research, 2016")
        meta$value[grep("Australia", meta$comment)] <- ("Australian Department of the Environment, 2016")
        meta$value[grep("EDGAR", meta$comment)] <- ("EDGAR 4.2")
        meta$value[ which( meta$comment == '0') ] <- ("Zero emissions")
        meta$prepost[ which( meta$comment == '0') ] <- ("Matched to inventory")

        return(meta)
    }
# ---------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------
# F.update_value_metadata
# Brief: update meta comments and rewrites meta data files
# Details: input, meta comments in scaling aggregation
# Dependencies: CEDS_header.R, F.invAggregate(), F.cedsAggregate
# Author: Rachel Hoesly
# parameters:
#
# return: null
# input files: meta_notes - new meta notes in scaling aggregation
# output files: NULL

    F.update_value_metadata <- function(type, meta_notes = meta_notes ){

        if( type %!in% c('EF','emissions')) stop('Invalid emission type. Cannot update scaled value metadata')

    # read in previous value-metadata and melt to long form
        meta <- readData( "MED_OUT", paste0( "F.", em, "_", "scaled_",type,"-value_metadata" ), meta = FALSE, to_numeric=FALSE)
        meta <- melt(meta, id.vars = c('iso','sector','fuel'))
        names(meta) <- c("iso","sector","fuel","year","comment" )
        meta$comment <- as.character(meta$comment)

    # aggregate scaling factor metadata to ceds sectors
        printLog ("Aggregating meta notes")
        meta_new <- F.scalingToCeds(meta_notes, dataFormat = 'long','comment','new_comment')
        meta_new <- meta_new[complete.cases(meta_new),]

    # separate cells that were updated by this inventory vs cells that were not
        meta_old_unchanged <- meta[ meta$iso %!in% unique(meta_notes$iso),]
        meta_old_changed <- meta[ meta$iso %in% unique(meta_notes$iso),]

        printLog ("Merging meta notes")

    # empty cells which were default and now are not
        meta_old_changed$comment[which(meta_old_changed$comment == 'default')] <- ""

    # Paste old notes onto new ones, except those cells with 0 emissions
        meta_new <- left_join (meta_new, meta_old_changed, by = c("iso", "year", "sector"))

        meta_new$new_comment <- paste0(meta_new$new_comment, "; ")

    # identify all indices which arent zeros, blank, or duplicates
        valid_indices <- which( meta_new$comment != "0" &
                                trimws( meta_new$new_comment ) != "" &
                                !grepl( meta_new$new_comment, meta_new$comment ) )

        meta_new$new_comment[valid_indices] <- paste0( meta_new$comment[ valid_indices ],
                                                      meta_new$new_comment[ valid_indices ] )

    # "comment" gets the value of "new comment", and unused columns from left_join are tossed
        meta_combined <- replaceValueColMatch( x = meta_old_changed,
                             y = meta_new,
                             x.ColName = 'comment', y.ColName = 'new_comment',
                             match.x = c('iso', 'sector','year'),
                             addEntries=FALSE)
        names(meta_combined) <- c('iso','sector','fuel','year','comment')

    # cells that were blanked are returned to default
        meta_combined$comment[which(meta_combined$comment == '')] <- 'default'

    # unchanged data and changed data are combined into one dataset
        new_meta_out <- rbind(meta_combined, meta_old_unchanged)

    # unchanged data and changed data are combined into one dataset
        new_meta_out <- new_meta_out[order(new_meta_out$iso,new_meta_out$year,new_meta_out$sector),]
        meta <- meta[order(meta$iso,meta$year,meta$sector),]

    # Any comment beginning with a 0 can be replaced by a 0.
        new_meta_out$comment[which( substr( new_meta_out$comment, 1, 1 ) == '0' )] <- 0

    # Cast data to wide format and write to output
        printLog('Casting meta to wide format')
        new_meta_out <- cast(new_meta_out, iso+sector+fuel~year, value = 'comment')

        writeData( new_meta_out, domain = 'MED_OUT',
      			 fn =paste0( "F.", em, "_", "scaled_",type,"-value_metadata"), meta = F )
        printLog( "Finished with value-metadata" )
    }

# ---------------------------------------------------------------------------------
# F.write
# Brief: writes out newly scaled EF and emissions data
# Details: automatically forms comments from region and inventory specifications,
#   writes out emissions factor data as a csv, and updates the emissions object.
#   The file name is generated from the emissions species: F.[em]_scaled_EF
#   Also writes out the emissions data, but this may be removed in the future.
# Dependencies: CEDS_header.R, F.scale
# Author: Tyler Pitkanen
# parameters:
#   ef: scaled emissions factors data [default: null]
#   em: scaled emissions data [default: null]
#   domain: save location domain [default: "MED_OUT"]
#   comments: comments for the EF output csv file. The "default" option
#             automatically generates one comment noting the inventory
#             used and the regions affected
# return: null
# input files: null
# output files: F.[em]_scaled_EF

F.write <- function( scaled_ef = scaled_ef, scaled_em = scaled_em, domain = "MED_OUT",
                     comments = "default" ) {

  # Write the comments as default versions unless they are overridden
  if ( comments == "default" ) {
    ef_comment_text <- paste( "Global emissions factors, where", region,
                              "EFs have been scaled by data from", inventory_data_file )
    em_comment_text <- paste( "Global emissions, where", region,
                              "emissions have been scaled by data from", inventory_data_file )
    comments.F.scaled_EFs <- c( ef_comment_text )
    comments.F.scaled_emissions <- c( em_comment_text )
  } else {
    comments.F.scaled_EFs <- comments
    comments.F.scaled_emissions <- comments
  }

  # Create fn from em specification
  # The fn is selected such that it overwrites the previous F.[em]_scaled_EF
  #   version, so all module F scripts update it serially
  ef_fn <- paste0( 'F.', em, '_scaled_EF' )
  em_fn <- paste0( 'F.', em, '_scaled_emissions' )

  # Write EF table as a csv file
  writeData( scaled_ef, domain = domain, fn = ef_fn,
             comments = comments.F.scaled_emissions )

  # May be removed later; data file may not be necessary
  writeData( scaled_em, domain = domain, fn = em_fn,
             comments = comments.F.scaled_emissions )

  # This creates or updates an R object in the global environment. It has a name
  #   equivalent to the string in em_fn, and it has the value of em.
  # We use this format so the assignment works for a general emissions species;
  #   this method allows us to evaluate em and include it in the object name
  assign( em_fn, em, envir = .GlobalEnv )
}

# ---------------------------------------------------------------------------------
# F.addScaledToDb
# Brief: overwrites emissions with scaled emissions and updates meta comments
# Details:
# Dependencies:
# Author:
# parameters:
# return: null
# input files: null
# output files:

F.addScaledToDb <- function( ef_scaled, em_scaled,
                             meta_notes,
                             EM_old = input_em_read,
                             EF_old = input_ef_read){

  if ( nrow( ef_scaled ) == 0 ){

    printLog ( paste('There are no inventory data, scaling factors, or scaled emissions, for', em, 'emission species') )

  } else {

  printLog( "Updating database with scaled emissions/emission factors." )

  # pre-scaled emissions and new emissions
  old_emissions <- EM_old
  old_efs <- EF_old

  #---------

  # melt old db and merge with new scaled values
  # emissions
  old_em <- tidyr::gather(old_emissions, "variable", "value", -iso, -sector, -fuel, -units)
  names(old_em) <- c("iso","sector","fuel","units",'year','old_em')
  old_em$year <- as.factor(old_em$year)

  #split old emissions into two dfs. [only perform merge where you have to]
  old_em_changed <- old_em[ old_em$iso %in% region , ]
  old_em_unchanged <- old_em[ old_em$iso %!in% region , ]
  old_em_unchanged$scaled_em <- NA

  new_em <- merge(old_em_changed, em_scaled, all = TRUE)
  new_em <- new_em[,c("iso","sector","fuel",'units','year','old_em','scaled')]
  names(new_em) <- c("iso","sector","fuel",'units','year','old_em','scaled_em')
  new_em <- rbind(new_em, old_em_unchanged)
  new_em[which(is.na(new_em$scaled)),'scaled_em'] <- new_em[which(is.na(new_em$scaled)),'old_em']
  new_em <- new_em[,c("iso","sector","fuel",'units','year','scaled_em')]

  # efs
  old_ef <- melt(old_efs,id=c("iso","sector","fuel","units"))
  names(old_ef) <- c("iso","sector","fuel","units",'year','old_ef')

  old_ef_changed <- old_ef[ old_ef$iso %in% region , ]
  old_ef_unchanged <- old_ef[ old_ef$iso %!in% region , ]
  old_ef_unchanged$scaled_ef <- NA

  new_ef <- merge(old_ef_changed, ef_scaled, all = TRUE)
  new_ef <- new_ef[,c("iso","sector","fuel",'units','year','old_ef','scaled')]
  names(new_ef) <- c("iso","sector","fuel",'units','year','old_ef','scaled_ef')
  new_ef <- rbind(new_ef, old_ef_unchanged)
  new_ef[which(is.na(new_ef$scaled)),'scaled_ef'] <- new_ef[which(is.na(new_ef$scaled)),'old_ef']
  new_ef <- new_ef[,c("iso","sector","fuel",'units','year','scaled_ef')]

  # update value_metadata
  if ( Write_value_metadata ) {
	  F.update_value_metadata('EF', meta_notes)
	  # F.update_value_metadata('emissions', meta_notes)
  }

  # cast to wide format
  scaled_em_out <- tidyr::spread(new_em, year, scaled_em)
  scaled_ef_out <- tidyr::spread(new_ef, year, scaled_ef)

  # Sort
  scaled_em_out <- scaled_em_out[ with( scaled_em_out, order( iso, sector, fuel ) ), ]
  scaled_ef_out <- scaled_ef_out[ with( scaled_ef_out, order( iso, sector, fuel ) ), ]

  # Check for NAs
  if( na_error == 1) {
    if( anyNA( scaled_em_out[ , X_emissions_years] ) ) {
      stop("Checking NAs... NA's in EF_db. Check Code.")
    } else {
      printLog("Checking NAs... No NA's in EF_db")
    }
    if( anyNA( scaled_ef_out[ , X_emissions_years] ) ) {
      stop("Checking NAs... NA's in EF_db. Check Code.")
    } else {
      printLog("Checking NAs... No NA's in EF_db")
    }
  }

  # write
  F.write( scaled_ef = scaled_ef_out, scaled_em = scaled_em_out, domain = "MED_OUT")

}}
