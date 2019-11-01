#------------------------------------------------------------------------------
# Program Name: interpolation_extension_functions.R
# Author's Name: Rachel Hoesly
# Date Last Modified: Nov 20, 2015
# Program Purpose: Header file containing generalized functions designed to
#   expand time series data: interpolate, extend forward or backward.
#
# Note:
# TODO:
# ------------------------------------------------------------------------------

loadPackage('zoo')

# ------------------------------------------------------------------------------
# expandAll
# Brief: take a mapping file and expand the "all" option to all unique iso, sectors, fuels, or years
# Dependencies:
# Author: Rachel Hoesly
# parameters:
# return:
# input files:
#
# TODO: meta functionality does not work right now. must be false

expandAll <- function(input,
                      start = start_year,
                      end = end_year,
                      iso=NA , sector=NA, fuel=NA,
                      toWide = FALSE
){

  # Define parameters from data
  inp <- input
  year <- start:end
  names <- names( input )
  if( length(grep( "X", names)) >0 ){
    all.id.names <- names[-grep( "X", names)]
    toWide <- FALSE
  }else{ all.id.names <- names }
  id.names <- all.id.names[ all.id.names %in% c('iso','sector','fuel','year')  ]

  # replace "all" variables
  if (any(input =='all', na.rm = TRUE)){

    if(all.na( iso ) &
       'iso' %in% id.names &
       'all' %in% input$iso){
      MCL <- readData( "MAPPINGS", "Master_Country_List" )
      iso <- unique(MCL$iso)}
    if(all.na( sector ) &
       'sector' %in% id.names &
       'all' %in% input$sector){
      MSL <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Sectors" )
      sector <- unique(MSL$sector) }
    if(all.na( fuel ) &
       'fuel' %in% id.names &
       'all' %in% input$fuel){
      MFL <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Fuels" )
      fuel <- unique(MFL$fuel)}

    all <- list()
    if ( 'iso' %in% id.names)   all[[length(all)+1]] <- iso
    if ( 'sector' %in% id.names) all[[length(all)+1]] <- sector
    if ( 'fuel' %in% id.names) all[[length(all)+1]] <- fuel
    if ( 'year' %in% id.names) all[[length(all)+1]] <- year

    for (n in seq_along(id.names) ){
      name <- id.names[n]
      unique <- all[[n]]
      replace <- input[which(input[,name]=='all'),]
      while (nrow(replace)>0){
        replace.index <- min(which(input[,name]=='all'))
        replace.row <- input[replace.index,]
        input <- input[-replace.index,]
        add <- data.frame(replace = unique)
        names(add) <- name
        other.name <- names(input)[ names(input) %!in% name ]
        add[, other.name ] <- replace.row[, other.name ]
        input <- rbind(input, add)
        replace <- input[which(input[,name]=='all'),]
      }}
  }

  # fix year notation and long/wide format

  if ('year' %in% id.names){
      input$year <- sub("X","",input$year)
      input$year <- paste0('X',input$year)
      if( toWide == TRUE) {
        val <- all.id.names[which(all.id.names %!in% c('iso','sector','fuel','year', 'units'))]
        val <- val[val %!in% c("pre_ext_method", "pre_ext_year")]
        val <- val[sapply(inp[1,val], is.numeric)]
        input <- cast( input,
                      formula = paste(paste(id.names[id.names %in% c('iso','sector','fuel')], collapse=' + '), '~ year'),
                      value = val)
        if ( 'units' %in% names ){
          input$units <- inp$units[1]
          col <- names(input)
          col.order <- c('iso','sector','fuel','units', col[col %!in% c('iso','sector','fuel','units' )] )
          input <- input[,col.order]
          }
        other <- names[names %!in% c('iso','sector','fuel','year', 'units', val)]
        if ( length(other>0) ) input[,other] <-  inp[1,other]
        }
  }

  return(input)
}

# ------------------------------------------------------------------------------
# interpolateValues
# Brief: Interpolates missing "in between" years by linear or constant extension
# Dependencies:
# Author: Rachel Hoesly
# parameters:
# return:  interpolated value
# input files: ext_data - the inventory to be interpolated
#       ext_default: default interp method, linear or constant (carry forward)
#       ext_method: input method file that contains interpolation methods for non default
#           iso - sector (or whichever) variables. if NA uses default methods
#       meta:TRUE or FALSE. If true, updates value-meta-data
# TODO: meta functionality does not work right now. must be false

interpolateValues <- function(interp_data,interp_default = 'linear',
                              meta = FALSE,
                              interp_method = NA){



  # Define parameters from data
  unit.label <- FALSE
  names <- names( interp_data)
  if ('units' %in% names){unit.label <- TRUE
                          UNITS <- interp_data[1,'units'] }
  if ( 'year' %in% names){
    interp_data$year <- as.numeric(sub("X", "", interp_data$year))
    interp_data$year <- paste0('X', interp_data$year)
    id.names <- names[ names %!in% c('year')]
    val <- id.names[ id.names %!in% c('iso','sector','fuel','units','pre_ext_method','pre_ext_year')]
    id.names <- id.names[id.names %!in% val]
    formula <- paste0(paste(id.names, collapse = '+'), '~year')
    interp_data <- cast( interp_data, formula, value = val)
      }

  names <- names( interp_data)
  id.names <- names[-grep( "X", names)]
  id.names <- id.names[id.names %!in% 'units']
  X_years <- names[grep( "X", names)]
  years <- as.numeric(sub("X", "", X_years))
  years_full <- min(years):max(years)
  X_years_full <- paste0('X', years_full)

  # Check input
  valid_interp_methods <- c('linear','constant')
  # Default interp method
  if( interp_default %!in% valid_interp_methods) {
    warning( paste0('"',interp_default,
                    '" is not a valid interpolation method. Replacing interpolation default with "linear".'))
    interp_default <- 'linear'}

  # Interp Method by id variable
  if(any(!is.na(interp_method), na.rm = TRUE)) {
    if ( ! all( interp_method$interp_method %in% c(valid_interp_methods,'NA') )) {
      index <- which( interp_method$interp_method %in% valid_interp_methods == FALSE )
      warning( paste0(  interp_method$interp_method[index] , ': invalid interpolation method. Using default option: ',
                        "'" ,interp_default),"'" )
      interp_method$interp_method[index] <- interp_default } }

  # interpolation method defaults

  # Define Default Methods Data frame, update with mapping file
  # defaults
  interp_method_full <- interp_data[,id.names]
  interp_method_full[,'interp_method'] <- interp_default
  # update with method file

  # Replace "all" notation in interp_methods with unique iso/sector/fuel names
  if( any(!is.na(interp_method), na.rm = TRUE) ){
    for (n in seq_along(id.names) ){
      name <- id.names[n]
      other.name <- id.names[id.names %!in% name]
      unique <- unique(interp_method_full[,name])
      replace <- interp_method[which(interp_method[,name]=='all'),]
      while (nrow(replace)>0){
        replace.index <- min(which(interp_method[,name]=='all'))
        replace.row <- interp_method[replace.index,]
        interp_method <- interp_method[-replace.index,]
        add <- data.frame(replace = unique)
        names(add) <- name
        add[, c(other.name,'interp_method') ] <- replace.row[, c(other.name,'interp_method') ]
        interp_method <- rbind(interp_method, add)
        replace <- interp_method[which(interp_method[,name]=='all'),]
      }     }
    if( any(interp_method$interp_method %!in% interp_default, na.rm = TRUE) ){
      interp_method <- interp_method[ which(interp_method$interp_method != interp_default) , ] }

    # replace default interp_methods with interp_method
    names(interp_method)[which(names(interp_method)=='interp_method')] <- 'interp_method_new'
    new_method <- merge(interp_method_full,interp_method, all.x=TRUE)
    interp_method_full[which(!is.na(new_method$interp_method_new)) ,'interp_method'] <-new_method[which(!is.na(new_method$interp_method_new)) ,'interp_method_new']

  }

  # interpolate values

  # Create empty full df
  interp_df <- as.data.frame(matrix(data=NA, nrow = nrow( interp_data),
                                    ncol = length(X_years_full)))
  names( interp_df ) <- X_years_full
  interp_df <- cbind(  interp_data[, id.names ] , interp_df[, X_years_full ] )
  interp_df[,X_years] <-  interp_data[, X_years]

  # identify any non-trailing/leading na's
  interpolation_rows<-c()
  for( i in seq_along(interp_df[,1])){
    row <- as.numeric(interp_df[i,X_years_full])
    if( length(rle(is.na(c(NA,row,NA)))$values)>3 ) interpolation_rows<- rbind(interpolation_rows,i)
  }
  interpolation_rows <- as.vector(interpolation_rows)

  # Interpolate
  if( length(interpolation_rows)>0){
    linear <- interp_df[which(interp_method_full$interp_method == 'linear'),]
    cant_interpolate <- linear[which(rowSums(!is.na(linear[,X_years_full]))<=1),]
    if(nrow(cant_interpolate)>0) linear <- linear[which(rowSums(!is.na(linear[,X_years_full]))>1),]
    if ( nrow(linear)>0){
      # Add Meta notes
      if (meta == TRUE) {
        meta_pre_add <- interp_df[interpolation_rows,]
        meta_pre_add <- merge(meta_pre_add, interp_method_full,
                              all.x=TRUE, all.y=FALSE)
        meta_pre_add <- meta_pre_add[which(meta_pre_add$interp_method == 'linear'),c(id.names,X_years_full)]

        meta_add <- c()
        for ( i in seq_along(meta_pre_add[,1])){
          row <- meta_pre_add[i,c(id.names,X_years_full)]
          min <- min( match( as.numeric(row[1,X_years_full]), row[1,]), na.rm=TRUE)
          max <- max( match( as.numeric(row[1,X_years_full]), row[1,]), na.rm=TRUE)
          non.trailing <- cbind( row[,id.names]  , row[, c( min:max)] )
          add <- melt(non.trailing, id.vars = id.names)
          add <- add[which(is.na(add$value)),c(id.names,'variable')]
          meta_add <- rbind(meta_add, add)
        }
        if(nrow(meta_add)>0){
          names(meta_add) <- c(id.names,'year')
          meta_add$comment <- paste('Linearly interpolated')
          meta_notes <- rbind(meta_notes, meta_add)
        }}

      linear_int <- t( na.approx( t(linear[,X_years_full])  , na.rm = FALSE) )
      linear <- cbind( linear[,id.names] , linear_int)
      if(nrow(cant_interpolate)>0) {
        names(linear) <- names(cant_interpolate)
        linear <- rbind(cant_interpolate, linear) }
      names(linear) <- c(id.names , X_years_full ) }

    constant <- interp_df[which(interp_method_full$interp_method == 'constant'),]
    if( nrow(constant)>0){
      # Add Meta notes
      if (meta == TRUE) {
        meta_pre_add <- interp_df[interpolation_rows,]
        meta_pre_add <- merge(meta_pre_add, interp_method_full[,c(id.names,'interp_method')],
                              all.x=TRUE, all.y=FALSE)
        meta_pre_add <- meta_pre_add[which(meta_pre_add$interp_method == 'constant'),c(id.names,X_years_full)]

        meta_add <- c()
        for ( i in seq_along(meta_pre_add[,1])){
          row <- meta_pre_add[i,c('iso',scaling_name,X_years_full)]
          min <- min( match( as.numeric(row[1,X_years_full]), row[1,]), na.rm=TRUE)
          max <- max( match( as.numeric(row[1,X_years_full]), row[1,]), na.rm=TRUE)
          non.trailing <- cbind( row[,id.names]  , row[, c( min:max)] )
          add <- melt(non.trailing, id.vars = id.names)
          add <- add[which(is.na(add$value)),c(id.names,'variable')]
          meta_add <- rbind(meta_add, add)
        }
        if(nrow(meta_add)>0){
          names(meta_add) <- c('iso',scaling_name,'year')
          meta_add$comment <- paste('Constant interpolated from inventory -', inv_name)
          meta_notes <- rbind(meta_notes, meta_add)
        }}
      constant_int <- t( na.locf( t(constant[,X_years_full])  , na.rm = FALSE) )
      constant <- cbind( constant[,id.names] , constant_int)
      names(constant) <- c(id.names , X_years_full ) }

    if( nrow(linear)>0 && nrow(constant)>0 ) interp_df <- rbind(linear, constant)
    if( nrow(linear)>0 && !nrow(constant)>0 ) interp_df <- linear
    if( !nrow(linear)>0 && nrow(constant)>0 ) interp_df <-constant

  } else interp_df <- interp_data

  if (unit.label == TRUE){
    other <- names(interp_df)[names(interp_df) %!in% c( 'iso','sector','fuel','units', X_years_full )]
    interp_df$units <- UNITS
    interp_df <- interp_df[,c( id.names,'units', X_years_full,other)]
  }

  return( interp_df )
}

# ------------------------------------------------------------------------------
# extendValues
# Brief: extend forward or backward by iso/sector/fuel
# Dependencies:
# Author: Rachel Hoesly
# parameters:
# return:
# input files:
#     ext_method format : column names - id variables (iso, sector, fuels) - pre_ext_method - post_ext_method
#     ext_year format : column names - id variables (iso, sector, fuels) - pre_ext_year - post_ext_year
# TODO: meta functionality does not work right now. must be false

extendValues <- function(ext_data,
                         pre_ext_default = 'constant',
                         post_ext_default = 'constant',
                         pre_ext_year = start_year,
                         post_ext_year = end_year,
                         meta = FALSE,
                         ext_method = NA,
                         ext_year = NA,
                         defaultData = NA) {
  # ext_method format
  # column names - id variables (iso, sector, fuels) - pre_ext_method - post_ext_method
  # ext_year format
  # column names - id variables (iso, sector, fuels) - pre_ext_year - post_ext_year

#   ext_data = aviation_EF_wide
#   pre_ext_default = 'constant'
#   post_ext_default = 'constant'
#   pre_ext_year = start_year
#   post_ext_year = end_year
#   meta = FALSE
#   ext_method = NA
#   ext_year = NA
#   defaultData = NA

  # run through interpolation first
  ext_data <- interpolateValues( ext_data )
  # expand extension data
  ext_data <- expandAll( ext_data )


  # Define inventory variables
  names <- names( ext_data)
  id.names <- names[-grep( "X", names)]
  id.names <- id.names[ id.names %in% c('iso','sector','fuel','units')]
  id.names.NoUnits <- id.names[id.names %!in% 'units']
  X_years <- names[grep( "X", names)]
  years <- as.numeric(sub("X", "", X_years))
  years_full <- min(years):max(years)
  X_years_full <- paste0('X', years_full)

  # keep only important columns
  ext_data <- ext_data[,c(id.names,X_years)]

  # Check input
  valid_pre_ext_methods  <- c('constant','linear_1', 'linear_0', 'linear_default')
  valid_post_ext_methods  <- c('linear','constant','linear_1')

  if( pre_ext_default %!in% valid_pre_ext_methods) {
    warning( paste0('"',pre_ext_default,
                    '" is not a valid extrapolation method. Replacing pre-extrapolation default with "constant".'))
    pre_ext_default <- 'constant'}
  if( post_ext_default %!in% valid_post_ext_methods) {
    warning( paste0('"',post_ext_default,
                    '" is not a valid extrapolation method. Replacing post-extrapolation default with "constant".'))
    post_ext_default <- 'constant'}
  if( pre_ext_default == 'linear' && length(years) == 1 ) {
    warning( 'Not enough inventory years to extend backward linearly. Replacing pre-extrapolation default with "constant".')
    pre_ext_default <- 'constant'
  }
  if( pre_ext_default == 'linear' && length(years) == 1 ) {
    warning( 'Not enough inventory years to extend backward linearly. Replacing pre-extrapolation default with "constant".')
    pre_ext_default <- 'constant'
  }
  if( post_ext_default == 'linear' && length(years) == 1 ) {
    warning( 'Not enough inventory years to extend backward linearly. Replacing post-extrapolation default with "constant".')
    post_ext_default <- 'constant'
  }

  # Check Years
    if( !(pre_ext_year <= min(years) & pre_ext_year >= start_year ) ){
      pre_ext_year <- min(years)
      warning( 'Invalid start year ("pre_ext_year"). Using the first year of data.')
    }
    if( !(post_ext_year >= max(years) & post_ext_year <= end_year ) ){
      post_ext_year <- max(years)
      warning( 'Invalid end year ("post_ext_year"). Using the last year of data.')
    }

    # Check Methods map and replace with default if invalid
    if ( !anyNA( ext_method ) ) {
    if ( ! all( ext_method$pre_ext_method %in% c(valid_pre_ext_methods,'NA') )) {
      index <- which( ext_method$pre_ext_method %in% valid_pre_ext_methods == FALSE )
      warning( paste0(  ext_method$pre_ext_method[index] , ': invalid pre-extrapolation method. Using default option: ',
                        "'" ,pre_ext_default),"'" )
      ext_method$pre_ext_method[index] <- pre_ext_default }

    if ( ! all( ext_method$post_ext_method %in% c(valid_post_ext_methods,'NA') )) {
      index <- which( ext_method$post_ext_method %in% valid_post_ext_methods == FALSE )
      warning( paste0(  ext_method$post_ext_method[index] , ': invalid post-extrapolation method. Using default option: ',
                        "'" ,post_ext_default,"'") )
      ext_method$post_ext_method[index] <- post_ext_default }

    #check linear default option
    if( any(c( pre_ext_default,  post_ext_default ,
                 ext_method$post_ext_method , ext_method$pre_ext_method) %in% 'linear_default', na.rm = TRUE) ){
        if(is.na(defaultData)) stop("Linear extrapolation to default chosen, but no default data specified. Please specify.")
        defaults <- readData('MED_OUT', defaultData)
      }

      }

    # Check linear_default option

    if( any(c( pre_ext_default,  post_ext_default) %in% 'linear_default', na.rm = TRUE) ){
        if(is.na(defaultData)) stop("Linear extrapolation to default chosen, but no default data specified. Please specify.")
        defaults <- readData('MED_OUT', defaultData)
        }

    # Define Default Methods Data frame, update with mapping file
    #########
    # defaults
    ext_method_full <- ext_data[,id.names]
    ext_method_full[,'pre_ext_method'] <- pre_ext_default
    ext_method_full[,'post_ext_method'] <- post_ext_default
    # update with method file

    # Replace "all" notation in ext_methods with unique iso/sector/fuel names.
    # Replace ext_methods with ext_methods full

    ext_method <- expandAll(ext_method)

    # replace default ext_methods with ext_method
    if( !anyNA( ext_method ) ){
    ext_method_full <- replaceValueColMatch(x = ext_method_full,
                                            y = ext_method,
                                            x.ColName = 'pre_ext_method',
                                            match.x = id.names[ id.names %in% c('iso','sector','fuel') ],
                                            addEntries = FALSE
                                            )
    ext_method_full <- replaceValueColMatch(x = ext_method_full,
                                            y = ext_method,
                                            x.ColName = 'post_ext_method',
                                            match.x = id.names[ id.names %in% c('iso','sector','fuel') ],
                                            addEntries = FALSE
                                            )    }

    # Define Default Years Data frame, update with mapping file
    #########
    # defaults
    ext_year_full <- ext_data[,id.names.NoUnits]
    ext_year_full[,'pre_ext_year'] <- pre_ext_year
    ext_year_full[,'post_ext_year'] <- post_ext_year
    # update with year file

    # Replace "all" notation in ext_years with unique iso/sector/fuel names.
    # Replace ext_years with ext_years full

    ext_year <- expandAll(ext_year)

    # replace default ext_years with ext_year
    if( !anyNA( ext_year ) ){
    ext_year_full <- replaceValueColMatch(x = ext_year_full,
                                            y = ext_year,
                                            x.ColName = 'pre_ext_year',
                                            match.x = id.names[ id.names %in% c('iso','sector','fuel') ],
                                            addEntries = FALSE  )
    ext_year_full <- replaceValueColMatch(x = ext_year_full,
                                            y = ext_year,
                                            x.ColName = 'post_ext_year',
                                            match.x = id.names[ id.names %in% c('iso','sector','fuel') ],
                                            addEntries = FALSE  )  }
    # Create Meta data notes

    if (meta == TRUE) {
      names <- c('iso',scaling_name,'year','meta_comment')
      meta_notes <- data.frame(matrix(ncol = length(names), nrow = 0))
      names(meta_notes) <- names
    }

    # Extend values through ext Years and fill remaing NA
    min_year <- as.numeric(min(years, pre_ext_year, ext_year_full$pre_ext_year))
    max_year <- as.numeric(max(years, post_ext_year, ext_year_full$post_ext_year))

    years_all <- min_year:max_year
    X_years_all <- paste0("X",years_all)

    ext_data_extended <- as.data.frame(matrix(data=NA,nrow = nrow(ext_data), ncol = length(years_all)))
    names(ext_data_extended) <- X_years_all
    ext_data_extended <- cbind(ext_data[,c(id.names.NoUnits)], ext_data_extended)

    #extension loop

    for (i in seq_along(ext_data_extended[,1])){
      # Interpolated inventory data
      ext_data_extended[i,X_years_full] <- ext_data[i,X_years_full]
      if( !all.na( ext_data_extended[i, X_years_full] ) ) {
        # Pre-Extrapolation
        min_data_year <- years_all[ min( which(!is.na(ext_data_extended[i,X_years_all]))) ]
        if( min_data_year > ext_year_full[i,'pre_ext_year']){
          # Define Pre-Extrapolation Years
          pre_ext_data_extended_years <- c( ext_year_full[i,'pre_ext_year']:(min_data_year-1) )
          X_pre_ext_data_extended_years <- paste0( 'X', pre_ext_data_extended_years )
          # Fill Years with scaling factor = 1, NA, or interpolated Scaling factor
          pre_ext_data_extended_line <- as.data.frame(matrix(data=NA, nrow = 1,
                                                       ncol = length(X_pre_ext_data_extended_years)+length(X_years_full)   ))
          names(pre_ext_data_extended_line)<- c( X_pre_ext_data_extended_years , X_years_full )
          pre_ext_data_extended_line[1,X_years_full] <- ext_data_extended[i,X_years_full]

        # Constant Extrapolation
        if( ext_method_full[i,'pre_ext_method'] == 'constant'){
            # Add meta notes
            if(meta==TRUE){
              year <- X_pre_ext_data_extended_years
              add <- data.frame(year)
              add$iso <- ext_data_extended[i,c('iso')]
              if(method %in% c('sector','fuel')) add[,data_name] <- ext_data_extended[i,data_name]
              if(method %in% c('both')) add[,data_name] <- t(replicate(ext_data_extended[i,data_name],n=length(year)))

              add$comment <-  paste('Scaled to Inventory - constant extrapolated backward -', inv_name)
              meta_notes <- rbind(meta_notes, add)
            }
            pre_ext_data_extended_line[1,] <-t(na.locf(t(pre_ext_data_extended_line[1,]), fromLast = TRUE, na.rm = FALSE))
        }
          # Linear Extrapolation to 1, from most recent value
          else if( ext_method_full[i,'pre_ext_method'] == 'linear_1'){
            pre_ext_data_extended_line[1,1]<-1
            pre_ext_data_extended_line[1,] <- na.approx(  t(pre_ext_data_extended_line[1,]) , maxgap = Inf, na.rm = FALSE)
            # Add meta notes
            if(meta==TRUE){
              year <- X_pre_ext_data_extended_years
              add <- data.frame(year)
              add$iso <- ext_data_extended[i,c('iso')]
              if(method %in% c('sector','fuel')) add[,data_name] <- ext_data_extended[i,data_name]
              if(method %in% c('both')) add[,data_name] <- t(replicate(ext_data_extended[i,data_name],n=length(year)))
              add$comment <-  paste('Scaled to Inventory - Linearly extrapolated backward to 1 -', inv_name)
              meta_notes <- rbind(meta_notes, add)
            }
          }
          # Linear Extrapolation to Default, from most recent value
          else if( ext_method_full[i,'pre_ext_method'] == 'linear_default'){

            defaultEF <- ext_year_full[i, id.names[id.names %in% c('iso','sector','fuel')]  ]
            defaultEF$default_value <- NA

            defaultEF <- replaceValueColMatch(defaultEF, defaults,
                                              x.ColName = 'default_value',
                                              y.ColName = paste0('X',ext_year_full[i,'pre_ext_year']),
                                              match.x = id.names[id.names %in% c('iso','sector','fuel')],
                                              addEntries = FALSE)

            pre_ext_data_extended_line[1,1]<-defaultEF[1,'default_value']
            pre_ext_data_extended_line[1,] <- na.approx(  t(pre_ext_data_extended_line[1,]) , maxgap = Inf, na.rm = FALSE)
            # Add meta notes
            if(meta==TRUE){
              year <- X_pre_ext_data_extended_years
              add <- data.frame(year)
              add$iso <- ext_data_extended[i,c('iso')]
              if(method %in% c('sector','fuel')) add[,data_name] <- ext_data_extended[i,data_name]
              if(method %in% c('both')) add[,data_name] <- t(replicate(ext_data_extended[i,data_name],n=length(year)))
              add$comment <-  paste('Scaled to Inventory - Linearly extrapolated backward to default -', inv_name)
              meta_notes <- rbind(meta_notes, add)
            }
          }
          # Linear Extrapolation to 0, from most recent value
          else if( ext_method_full[i,'pre_ext_method'] == 'linear_0'){
            pre_ext_data_extended_line[1,1]<-0
            pre_ext_data_extended_line[1,] <- na.approx(  t(pre_ext_data_extended_line[1,]) , maxgap = Inf, na.rm = FALSE)
            # Add meta notes
            if(meta==TRUE){
              year <- X_pre_ext_data_extended_years
              add <- data.frame(year)
              add$iso <- ext_data_extended[i,c('iso')]
              if(method %in% c('sector','fuel')) add[,data_name] <- ext_data_extended[i,data_name]
              if(method %in% c('both')) add[,data_name] <- t(replicate(ext_data_extended[i,data_name],n=length(year)))
              add$comment <-  paste('Scaled to Inventory - Linearly extrapolated backward to 0 -', inv_name)
              meta_notes <- rbind(meta_notes, add)
            }
          }
          ext_data_extended[i,X_pre_ext_data_extended_years] <- pre_ext_data_extended_line[1,X_pre_ext_data_extended_years]
        } # end Pre-Extrapolation

        # Post-Extrapolation
        max_data_year <- years_all[ max( which(!is.na(ext_data_extended[i,X_years_all]))) ]
        if( max_data_year < ext_year_full[i,'post_ext_year']){
          # Define Post-Extrapolation Years
          post_ext_data_extended_years <- c( (max_data_year-1):ext_year_full[i,'post_ext_year'] )
          X_post_ext_data_extended_years <- paste0( 'X', post_ext_data_extended_years )
          # Fill Years with scaling factor = 1, NA, or interpolated Scaling factor
          post_ext_data_extended_line <- as.data.frame(matrix(data=NA, nrow = 1,
                                                             ncol = length(X_post_ext_data_extended_years)+length(X_years_full)   ))
          names(post_ext_data_extended_line)<- c( X_years_full , X_post_ext_data_extended_years)
          post_ext_data_extended_line[1,X_years_full] <- ext_data_extended[i,X_years_full]
          # Linear Extrapolation
          if( ext_method_full[i,'post_ext_method'] == 'linear'){
            x = min(years):max(years)
            y = t(ext_data[i,X_years_full])
            xout = post_ext_data_extended_years
            if ( !length(x[complete.cases(x)]) > 1){
              printLog('Not enough data points to Linearly interpolate. Constantly extending.')
              ext_method_full[i,'post_ext_method'] <- 'constant'
            } else if (length(x[complete.cases(x)]) > 1){
              post.lm <- lm(y ~ x)
              fitted <- predict(post.lm, data.frame(x=xout),interval='none')
              post_ext_data_extended_line[1,X_post_ext_data_extended_years] <- fitted
              # Add meta notes
              if(meta==TRUE){
                year <- X_post_ext_data_years
                add <- data.frame(year)
                add$iso <- ext_data[i,c('iso')]
                if(method %in% c('sector','fuel')) add[,scaling_name] <- ext_data[i,scaling_name]
                if(method %in% c('both')) add[,scaling_name] <- t(replicate(ext_data[i,scaling_name],n=length(year)))
                add$comment <-  paste('Scaled to Inventory - Linearly extrapolated forward -', inv_name)
                meta_notes <- rbind(meta_notes, add)
              }
            }
            # Check for negative values
            if( any( post_ext_data_extended_line[1,] < 0 , rm.na=TRUE ))
              post_ext_data_extended_line[post_ext_data_extended_line < 0] <- 0
            }
          # Constant Extrapolation
          else if( ext_method_full[i,'post_ext_method'] == 'constant'){
            post_ext_data_extended_line[1,] <-t(na.locf(t(post_ext_data_extended_line[1,]), na.rm= FALSE))
            # Add meta notes
            if(meta==TRUE){
              year <- X_post_ext_data_years
              add <- data.frame(year)
              add$iso <- ext_data[i,c('iso')]
              if(method %in% c('sector','fuel')) add[,scaling_name] <- ext_data[i,scaling_name]
              if(method %in% c('both')) add[,scaling_name] <- t(replicate(ext_data[i,scaling_name],n=length(year)))
              add$comment <-  paste('Scaled to Inventory - Linearly extrapolated forward -', inv_name)
              meta_notes <- rbind(meta_notes, add)
            }
          }
          # Linear Extrapolation to Scaling Factor = 1, from most recent value
          else if( ext_method_full[i,'post_ext_method'] == 'linear_1'){
            post_ext_data_extended_line[1,ncol(post_ext_data_extended_line)]<-1
            post_ext_data_extended_line[1,] <- na.approx(  t(post_ext_data_extended_line[1,]) , maxgap = Inf, na.rm = FALSE)
            # Add meta notes
            if(meta==TRUE){
              year <- X_post_ext_data_years
              add <- data.frame(year)
              add$iso <- ext_data[i,c('iso')]
              if(method %in% c('sector','fuel')) add[,scaling_name] <- ext_data[i,scaling_name]
              if(method %in% c('both')) add[,scaling_name] <- t(replicate(ext_data[i,scaling_name],n=length(year)))
              add$comment <-  paste('Scaled to Inventory - Linearly extrapolated forward -', inv_name)
              meta_notes <- rbind(meta_notes, add)
            }
          }
          ext_data_extended[i,X_post_ext_data_extended_years] <- post_ext_data_extended_line[1,X_post_ext_data_extended_years]
        } # End Post-Extrapolation
        }}# end extension loop

    names <- names( ext_data_extended )
    id.years <- names[grep( "X", names)]

    if( 'units' %in% id.names){
      ext_data_extended$units <- ext_data$units
      ext_data_extended <- ext_data_extended[,c(id.names,id.years)]}else{
        ext_data_extended <- ext_data_extended[,c(id.names[id.names %!in% 'units'],id.years)]
      }
    return(ext_data_extended)

     }# end function

# ------------------------------------------------------------------------------
# extendDefaultEF
# Brief: extend forward or backward default emissions factors
# Dependencies:
# Author: Rachel Hoesly
# parameters:
# return:
# input files:
#
# TODO: meta functionality does not work right now. must be false


extendDefaultEF <- function(exten_df,
                            pre_ext_method_default){

  # process
  exten_df <- expandAll(exten_df, toWide = TRUE)

  #set up data options for extension

  #     ext_method format : column names - id variables (iso, sector, fuels) - pre_ext_method - post_ext_method
  #     ext_year format : column names - id variables (iso, sector, fuels) - pre_ext_year - post_ext_year

  if( pre_ext_method_default == 'none') pre_ext_method_default <- 'constant'

  e_method <- exten_df[,c('iso','sector','fuel')]
  if( 'pre_ext_method' %in% names(exten_df) ){e_method$pre_ext_method <- exten_df$pre_ext_method}
  if( 'pre_ext_method' %!in% names(exten_df) ){e_method$pre_ext_method <- pre_ext_method_default}
  e_method$post_ext_method <- 'constant'
  e_year <- exten_df[,c('iso','sector','fuel')]
  if( 'pre_ext_year' %in% names(exten_df) ){e_year$pre_ext_year <- exten_df$pre_ext_year}
  if( 'pre_ext_year' %!in% names(exten_df) ){e_year$pre_ext_year <- start_year}
  e_year$post_ext_year <- end_year

  names <- names( exten_df)
  id.X_years <- names[grep( "X", names)]


  #extend
  out <- extendValues(ext_data = exten_df[,c('iso','sector','fuel','units',id.X_years)],
                      pre_ext_default = pre_ext_method_default,
                      post_ext_default = 'constant',
                      pre_ext_year = start_year,
                      post_ext_year = end_year,
                      meta = FALSE,
                      ext_method = e_method,
                      ext_year = e_year)


  # get names and reorder for output
  names <- names( out)
  id.names <- names[-grep( "X", names)]
  id.X_years <- names[grep( "X", names)]

  out <- out[,c('iso','sector','fuel','units',id.X_years)]

  return( out )

}
