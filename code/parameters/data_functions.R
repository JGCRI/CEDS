# -----------------------------------------------------------------------------
# CEDS R header file: data molding functions
# Authors: Ben Bond-Lamberty, Jon Seibert, Tyler Pitkanen, Caleb Braun
# Last Updated: 10 August 2018
#
# This file should be sourced by any R script doing heavy-duty reformatting of
# CEDS data. It contains helper functions for general data manipulation and some
# CEDS-specific data transformations.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# %!in%"
# Brief:        Is not an element of" (opposite of %in%).
# Details:      Determines whether the pattern is NOT found within y.
# Dependencies: None
# Author(s):    Ben Bond-Lamberty
# Params:
#   x:          Pattern to search for within y [required]
#   y:          List to search within for x [required]
# Return:       Boolean indicating whether x is NOT found within y.
# Input Files:  None
# Output Files: None
'%!in%' <- function( x, y ) !( '%in%'( x, y ) )


# -----------------------------------------------------------------------------
# replaceValueColMatch
# Brief:  replace values in one column of database x with values of column in database y
#         based on 2 or more matching files matching columns
# Details:
# Dependencies: None
# Author(s):
# Params:   x - data frame with data to be replaced (old data)
#           y - data frame with replacement data (new data)
#           x.colName - column name of data in x to be replaced
#           y.colName - column name of data in y to replace data in x$x.colName
#           match.x - cols in x to match data by (like by.x in merge())
#           match.y - cols in x to match data by (like by.x in merge())
#           addEntries - boolean (T/F) if True, add rows to x, when data in y does
#                         not match with rows in x
# Return: data frame similar to x with replaced values from y where applicable
# Input Files:  None
# Output Files: None

replaceValueColMatch <- function( x,y,x.ColName,y.ColName = x.ColName,
                                  match.x,match.y=match.x,
                                  addEntries,
                                  replace_NAs = F){

  out <- x
  n<-length(match.x)
  if ( n == 1) {
  x.match.cols <- x[,match.x[1]]
  y.match.cols <- y[,match.y[1]]
}
  if ( n > 1) {
    x.match.cols <- apply(X = x[match.x], MARGIN = 1, FUN = paste, collapse = '-')
    y.match.cols <- apply(X = y[match.y], MARGIN = 1, FUN = paste, collapse = '-')
      }


  out[,x.ColName] <- y[match(x.match.cols,y.match.cols),
                       y.ColName]

  index <- which(!complete.cases(out[,x.ColName]))
  out[index,x.ColName] <- x[index,x.ColName]

  if (addEntries){
    x.names <- names(x)

    hybrid.names <- x.names
    hybrid.names[which( hybrid.names %in% match.x)] <- match.y
    hybrid.names[which( hybrid.names %in% x.ColName)] <- y.ColName

    if( ! identical(x.names, hybrid.names) & replace_NAs == F ) {
      stop(paste0('Cannot add entries to original dataframe ',
                  'in replaceValueColMatch. Check column names.'))
      }

    out <- rbind.fill(out, y[which(is.na(match(y.match.cols,x.match.cols))),])
    if( replace_NAs) out[is.na(out)] <- 0
  }

  return(out)
}


# -----------------------------------------------------------------------------
# is.invalid
# Brief: Combination check for if a value is null, NA, or NaN
# Params:
#    x: some non-list object
# Return:       Input vector of strings with pattern replaced by its replacement.
# is.invalid
is.invalid <- function(x) return( is.null(x) || is.na(x) || is.nan(x) )

# -----------------------------------------------------------------------------
# is.nan/finite/infinite.df
# Brief: Makes up for the fact that R has no built-in vectorized check for
#    is.nan, is.infinite, or is.finite.
# Params:
#    x: a dataframe that may or may not contain NaN values
is.nan.df      <- function(x) do.call( cbind, lapply( x, is.nan ) )
is.finite.df   <- function(x) do.call( cbind, lapply( x, is.finite ) )
is.infinite.df <- function(x) do.call( cbind, lapply( x, is.infinite ) )


# -----------------------------------------------------------------------------
# all.na, some.na
# Brief: Are all values NA?
#        Are some, but not all values NA?
# Params:
#   x: an R atomic vector or list (including data frames)
all.na  <- function(x) return( all( is.na(x) ) )
some.na <- function(x) anyNA(x) & !all.na(x)


# -----------------------------------------------------------------------------
# repeatAndAddVector
# Brief:            Function for repeating a dataframe in order to add a new vector.
# Details:          Create a dataframe with a length greater than or equal to the length of
#                       vector_values by repeating the rows of the input dataframe, and append
#                       the new column (repeated as necessary).
# Dependencies:     sort, rep
# Author(s):        Tyler Pitkanen
# Params:
#  data:            Dataframe to be repeated and added to [required]
#  vector:          Location in the data frame to add the new vector [required]
#  vector_values:   Vector to be added to the dataframe[required]
# Return:           Modified data frame.
# Input Files:      None
# Output Files:     None
repeatAndAddVector <- function( data, vector, vector_values ) {
     data_new <- data[ rep( 1:nrow( data ), times = length( vector_values ) ), ]
     data_new[[vector]] <- sort( rep( vector_values, length.out = nrow( data_new ) ) )
     return( data_new )
	 }


# ------------------------------------------------------------------------------
# addCols
# Brief: Transfer a column to another table, rearranged based on a column
#   shared between the two tables
# Dependencies: null
# Author: Tyler Pitkanen
# parameters:
#   table1: data frame containing the column to be transferred [default: none]
#   table2: data frame to have the column transferred to it [default: none]
#   col: name of the column to be rearranged and transferred [default: none]
#   matchcol: name of a column that appears in both in table1 and table2. col
#       is already matched to matchcol in table1, and this function will
#       match col to matchcol in table2 [default: none]
# return: table1 with a rearranged col appended to it
# input files: null
# output files: null

addCols <- function( table1, table2, col, matchcol ) {

# Set locations of matching columsn and the column to be transferred
    table1matchcol <- table1[[matchcol]]
    table2matchcol <- table2[[matchcol]]
    colnum <- grep( col, names( table2 ) )

# Re-arrange the column to be transferred and bind it to table1
    x <- table2[ match( table1matchcol, table2matchcol ), colnum ]
    extendedtable <- cbind( table1, x )
    names( extendedtable ) <- c( names(table1), col )

    return( extendedtable )
}

# -----------------------------------------------------------------------------
# intersectNames
# Brief:        Return common names between two named objects
# Details:      Useful for explicitly specifying columns to perform join
#               operations on
# Author:       Caleb Braun
# Params:
#   x:          An object
#   y:          An object
# Return:       Vector of common names
intersectNames <- function(x, y) {
    intersect( names( x ), names( y ) )
}

# ----------------------------------------------------------------------------------
# isYear: determines whether a string or number is a 4-digit year or Xyear

# isNumYear: returns a boolean vector indicating whether the parameter's components are numerical years.
isNumYear <- function( yrs ){
    grepl( "^[0-9]{4}$", yrs )
}

# isXYear: returns a boolean vector indicating whether the parameter's components are xYears.
isXYear <- function( yrs ){
    grepl( "^[xX][0-9]{4}$", yrs )
}

isYear <- function( yrs ){ return( isNumYear( yrs ) | isXYear ( yrs ) ) }

# ----------------------------------------------------------------------------------
# removeBlanks
# Brief:        Eliminates non-data rows from an imported excel file and fixes column names.
# Details:      Puts imported excel sheet into a form that is easier to manipulate in R. If
#                   the column names for the sheet's data do not become the default column
#                   names for the data frame (because of empty rows or rows with descriptive
#                   information at the top of the sheet), removes said non-data rows and
#                   sets the column names to the intended vector of names, if found with the
#                   specified method (see "method" parameter). Intended for use with reading
#                   .xlsx files, but can be applied to any data frame. Also removes rows
#                   consisting solely of NAs within the body of the data frame. If the chosen
#                   method fails, the function will return an error. If the data frame already
#                   has a vector of names satisfying the method, the function will do nothing,
#                   and return the data frame passed to it with no changes.
# Dependencies: any, isYear, sapply, grep, rowSums
# Authors:      Jon Seibert
# Parameters:
# 	df: 		Data frame of the imported excel sheet to be fixed [required]
#   method:     String indicating which method of locating the column names and
#                   removing the non-data rows is to be used. Options are:
#                       year (first row with years in it is the names),
#                       string (first non-blank row is the names)
#                       name (first row with the parameter first_name
#                             as the first entry is the names)
#                   [default: "year"]
#	first_name: String of the intended name for column 1. Required if using the "string"
#                   method. [default: ""]
# Return:       modified data frame with correct column names and no empty rows
# Input files:  none
# Output files: none
# Notes: only use the "string" option if all rows preceding the actual names for the columns
#        are blank, including the default names.

# Usage examples: df <- removeBlanks( df,"Country" )
#			      data_list <- lapply( data_list, removeBlanks, first_name = "Country" )

removeBlanks <- function( df, method = "year", first_name = "iso" ){

    data_row_start <- 1

    # First row with years in it is the names
    if( method == "year" ){
        if( any( isYear( names( df ) ) ) ) return( df )
        # Finds the rows that are years in each column, and assumes the earliest such row contains
        # the column names. Row where data begins is the row after the true column names.
        data_row_start <- min( unlist( lapply( df, FUN = function( x ) which( isYear( x ) ) ) ) ) + 1
    }else{

    # First non-blank row is names
    if( method == "string" ){
        if( !any( names( df ) == ""  ) && !anyNA( names( df ) ) ) return( df )
        data_row_start <- min( unlist( lapply( df,
                          FUN = function( x ) which( ( x != "" ) && !is.na( x ) ) ) ) ) + 1
    }else{

    # First row with the parameter first_name as the first entry is the names
    if( method == "name" ){
        if ( first_name %in% names( df ) ) return( df )
        data_row_start <- grep(first_name, df[ ,1] ) + 1
        if( length( data_row_start ) == 0 ) stop( "Specified name not found." )
    }else{
        stop( "Invalid method. Available options are: year, string, name" )
    }}}

    result <- df[ data_row_start:nrow( df ), ]

    # Remove rows of all NAs
    result <- result[ rowSums( is.na( result ) ) != ncol( result ), ]
    if( data_row_start != 1 ) {
        names( result ) <- df[ data_row_start - 1, ]
    }else{ stop( "Column names not found." ) }

    return( result )
}

# ----------------------------------------------------------------------------------
# findDataStart
# Brief: Finds the start of the data section of CEDS data files
# Details: Iterates through the column names of the input data frame to
#          find the column index of the beginning of the data section-
#          the first year column (ex. X1960). This assumes that all year
#          columns in the data begin with X, as is the standard for within CEDS.
#          Intended as an aid for data frame re-organization.
# Dependencies: printLog
# Author: Jon Seibert
# Parameters:
# 	df: 		data frame to be examined[required]
# Return: Index of the first data column (integer). Returns -1 if no data found.
# Input files: none
# Output files: none
#
# Usage examples: data_start <- findDataStart( df )
findDataStart <- function( df ){
    result <- 1
    found <- FALSE
    for( i in 1:length( df ) ){
        name <- names( df )[[ i ]]
        initial <- substr( name,1,1 )
        year1 <- as.numeric( substr( name,2,5 ) )
        year2 <- as.numeric( substr( name,1,4 ) )
        if( !( found ) && ( ( initial == "X" ) || ( initial == "x" ) || !( is.na( year2 ) ) ) && !( is.na( year1 ) ) ){
            found <- TRUE
            result <- i
        }
    }
    if( found ){ return( result ) } else {
        printLog( "Error: No data columns found in data frame." )
        return( -1 )
    }
}
# TODO: I think we can replace this with: match(T, isYear(names(interp_group)))

# ----------------------------------------------------------------------------------------
# buildCEDSTemplate
# Brief:        Builds a blank template of all 0s for each possible CEDS entry.
# Details:      Takes tailored lists of iso codes, sectors, fuels, or activities,
#               and uses them to generate a standard-form data frame with entries for
#               each combination thereof, with correct units and year columns.
#               Can operate on iso-sector-fuel data or iso-activity data, using
#               boolean option parameters.
# Dependencies: findDataStart, expand.grid, common_data.R
# Author:       Jon Seibert
# Parameters:
#   iso_list:           List of iso codes to be used in template [default: NULL]
#   sector_list:        List of sectors to be used in template [default: NULL]
#   fuel_list:          List of fuels to be used in template [default: NULL]
#   activity_list:      List of activities to be used in template [default: NULL]
#   iso_sector_fuel:    Boolean indicating sector-fuel format
#   iso_activity:       Boolean indicating activity format
# Return:       Data frame of all possible combinations of given id variables
#               and 0 for all years
# Input Files:  none
# Output Files: none
#
# Usage example:
buildCEDSTemplate <- function( iso_list = NULL, sector_list = NULL, fuel_list = NULL,
                               activity_list = NULL, iso_sector_fuel = TRUE, iso_activity = FALSE ){

    # Required to get full list of names
    source( paste( PARAM_DIR, "common_data.R", sep = "" ) )

    # If any of the required id lists were not given in the function call, stop.
    if( is.null( iso_list ) || ( ( iso_sector_fuel && is.null( sector_list ) && is.null( fuel_list ) )
                                 || ( iso_activity && is.null( activity_list ) ) ) ){
        cat( "Error in buildCEDSTemplate: Must specify all identifying lists \n" )
        return( data.frame() )
    }

    # If template is to be in sector-fuel format
    if( iso_sector_fuel && !iso_activity ){

        # Data frame of all possible combinations
        template <- expand.grid( iso = iso_list, sector = sector_list, fuel = fuel_list )

        # Create list of final names, including all Xyears from common_data.R
        col_names <- c( "iso", "sector", "fuel", "units", X_emissions_years)

        # Add units for each sector
        for( sector_name in sector_list ){
            unit <- MSL[ MSL$sector == sector_name, "units" ]
            template[ template$sector == sector_name, "units" ] <- unit
        }
    }

    # If template is to be in activity format
    if( iso_activity && !iso_sector_fuel ){

        # Data frame of all possible combinations
        template <- expand.grid( iso = iso_list, activity = activity_list )

        # Create list of final names, including all Xyears from common_data.R
        col_names <- c( "iso", "activity", "units", X_emissions_years )

        # Add units for each activity
        for( activity_name in activity_list ){
            unit <- MSL[ MSL$sector == activity_name, "units" ]
            template[ template$activity == activity_name, "units" ] <- unit
        }
    }

    # Add a column for each emissions year
    for( year in X_emissions_years ){
        template <- cbind( template, template[ length( template ) ] )
    }

    # Rename final template columns
    names( template ) <- col_names

    # Set all data to 0
    data_start <- findDataStart( template )
    template[ data_start:length( template ) ] <- 0

    # Sort by iso, sector, and fuel
    template <- template[ with( template, order( iso, sector, fuel ) ), ]

    return( template )
}

# ----------------------------------------------------------------------------------
# interpolate_NAs
# Brief: Linearly interpolate over NA values
# Author: Rachel hoesly
# Parameters:
# 	df: data frame of numeric values (no id columns)
# Return: data frame of same size as df, with interpolated values
# Input files: none
# Output files: none

interpolate_NAs <- function(df) {
    if( 'data.frame' %!in% class(df) ) {
        warning("interpolate_NAs expects a data frame; attempting to convert")
        df <- as.data.frame(df)
    }

    value.cols <- sapply(df, is.numeric)
    interpolate_rows <- logical(nrow(df))
    for ( i in seq_along(1:nrow(df))) {
        row <- df[i, ]
        # Check if 1) there are NAs surrounded on either side by values and
        #          2) there are at least 2 non-NA elements (redundant check?)
        if( length(rle(is.na(c(NA, row, NA)))$values) > 3 &&
            length(row) - sum(is.na(row)) > 1) {
            interpolate_rows[i] <- TRUE
        }
    }

    if( any(interpolate_rows) ) {
        df[interpolate_rows, value.cols] <- t( na.approx(t(df[ interpolate_rows , value.cols]), na.rm = F))
    }
    return (df)
}

# A faster implementation of interpolate_NAs. Needs testing to ensure identical
# behavior.
interpolate_NAs2 <- function(df) {
    if(!is.data.frame(df)) {
        warning("interpolate_NAs expects a data frame; attempting to convert")
        df <- as.data.frame(df)
    }

    # Convert columns that are all NA to numeric
    df <- dplyr::mutate_if(df, function(x) all.na( x ), as.numeric)

    if(length(rle(sapply(df, is.numeric))$lengths) > 2) {
        warning("interpolate_NAs found mixed numeric and non-numeric columns;
                make sure all value columns are numeric")
    }

    value.cols <- sapply(df, is.numeric)
    df_flipped <- t(df[value.cols])
    df[ , value.cols] <- t(na.approx(df_flipped, na.rm = F))

    return(df)
}


# -----------------------------------------------------------------------------
# verify_calculate_share_params
# Brief:     Verify that the parameters to calculate_(correct_)shares are ok.
# Details:   Should not be called outside those 2 functions.
# Author(s): Caleb Braun
verify_calculate_share_params <- function(input_data, id_columns, target_column,
                                          replace_with_zeros) {

  if( length(target_column) != 1 )
      stop('calulating shares: must select only one target_column' )
  if( any(id_columns %in% target_column) && length(id_columns) > 1 )
      stop('in calculate_shares: specified id_columns must be different than target_column, if more than one id column')
  # if( all( id_columns %!in% names( input_data ) ) )
  #     stop('in calculate_shares: specified id_columns are not all in original_data')
}



# -----------------------------------------------------------------------------
# calculate_shares
# Brief:     calculate shares from data
# Details:
# Dependencies:
# Author(s):
# Params:
          # input_data - data of which to compute shares. In id cols , X year format
          # id_cols - vector of character strings, nanes of identifier columns for
          #           which to compute shares, ex - c('sector','fuel)
          # target_column - vector of character strings, nanes of identifier column for
          #           which to compute shares over, ex - 'iso'
          # replace_with_zeros - T/F boolean, defaults to T. If T, shares that are
          #         calculated as NA (have zero data) are replaced with zeros
#
# Return:  dataframe in same format as input, of calculated shares
# Input Files:
# Output Files:
# Notes: often used to renormalize shares to 1
# TODO:

calculate_shares <- function(input_data,
                             id_columns,
                             target_column,
                             replace_with_zeros = T

){

  same_id_target = F

  # input parameter checks
  if( length(target_column) != 1 ) stop('in calculate_shares: must select only one target_column' )
  if( any(id_columns %in% target_column) & length(id_columns) > 1 ) stop('in calculate_shares: specified id_columns must be different than target_column, if more than one id column')
  if( length(id_columns) == 0) {
    same_id_target <- T
    id_columns <- target_column    }
  if( length(id_columns) == 1 & all(id_columns == target_column) ) same_id_target <- T
  if( all( id_columns %!in% names(input_data )) ) stop('in calculate_shares: specified id_columns are not all in original_data')
  if( same_id_target) printLog( 'in calculate_shares: id_column and target columns are the same.')

  # useful variables
  X_years <- names(input_data)[grep('X',names(input_data))]

  # aggregate data
  if( ! same_id_target ) denominator <- aggregate( input_data[ X_years ],
                            by = input_data[ c( id_columns ) ],
                            sum )
  if( same_id_target ) {
    temp_input <- input_data
    temp_input$temp <- 'temp'
    denominator <- aggregate( temp_input[ X_years ],
                               by = list(temp_input$temp),
                               sum )
  }
  #To start, the shares will just be the unnormalized shares
  shares <- input_data

  # apply function below only works for mutliple columns
  if( length(id_columns) == 1){
    if( !same_id_target ) shares[ X_years ] <- shares[ X_years ] / denominator[ match( shares[[id_columns]], denominator[[id_columns]] ), X_years ]
    if( same_id_target ) shares[ X_years ] <- shares[ X_years ] / do.call("rbind", replicate(nrow(shares), denominator[  X_years ], simplify = FALSE))
  }else if( length(id_columns) > 1){
  shares[ X_years ] <- shares[ X_years ] / denominator[
    match( apply(FUN = paste, MARGIN = 1, X = shares[ ,id_columns], collapse = '-'),
           apply(FUN = paste, MARGIN = 1, X = denominator[ ,id_columns], collapse = '-')),
    X_years ]
  }

  shares[ is.na(shares) ]<- NA

  if( replace_with_zeros ) shares[is.na(shares)] <- 0

  shares <- arrange_(shares, c(target_column, id_columns))

  return(shares)

}

# -----------------------------------------------------------------------------
# calculate_correct_shares
# Brief:     corrects shares given a dataframe default breakdowns
# Details:
# Dependencies:
# Author(s): Rachel Hoesly
# Params:
#   a.input_data: Data frame of shares to correct in id cols, X year format
#   a.id_cols: vector of character strings, names of identifier columns for
#              which to compute shares, ex - c('sector','fuel)
#   a.target_column: Vector of character strings, names of identifier column for
#                    which to compute shares over, ex - 'iso'
#   a.corrections: Default corrections for when shares are from zero data. Must
#                  be in data frame format with 2 columns, target_id and
#                  breakdown. breakdown must sum to 1. For example:
#                  data.frame( ext_sector = c('1A4c_Agriculture-forestry-fishing',
#                                             'Industry','Power','RCO','Shipping',
#                                             'Transportation'),
#                              breakdown = c(0,0.5,0,0.5,0,0))
#   replace_with_zeros: Boolean, defaults to T. If T, shares that are calculated
#                       as NA (have zero data) are replaced with zeros.
#
# Return:  dataframe in same format as input, of calculated shares
# Input Files:
# Output Files: a dataframe of corrected shares in the same format as a.input_data
# TODO:

calculate_correct_shares <- function(a.input_data,
                                     a.id_columns,
                                     a.target_column,
                                     a.corrections,
                                     a.match_columns = a.target_column,
                                     replace_with_zeros = T ){

    # a.input_data = bond_sector_percentages_full
    # a.id_columns = c("iso","fuel")
    # a.target_column = c('ext_sector')
    # replace_with_zeros = T
    # a.corrections = ext_sector_percents_start_assumptions
    # a.match_columns = c('fuel', 'ext_sector')

    # ---------------------------
    #CR: Re-use the parameter verification in calculate_shares
    # 1. Input parameter checks
    # id and target columns
    same_id_target <- F
    if( length(a.target_column) != 1 )
        stop('in calculate_shares: must select only one a.target_column' )
    if( any(a.id_columns %in% a.target_column) && length(a.id_columns) > 1 )
        stop('in calculate_shares: specified a.id_columns must be different than a.target_column, if more than one id column')
    if( length(a.id_columns) == 0) {
        same_id_target <- T
        a.id_columns <- a.target_column    }
    if( length(a.id_columns) == 1 && all(a.id_columns == a.target_column) )
        same_id_target <- T
    if( all( a.id_columns %!in% names(a.input_data )) )
        stop('in calculate_shares: specified a.id_columns are not all in original_data')
    if( same_id_target)
        printLog( 'in calculate_shares: id_column and target columns are the same.')

    # correction file
    correction_id <- names(a.corrections)[ names(a.corrections) %!in% c("breakdown", a.target_column) ]

    if( 'breakdown' %!in% names(a.corrections) )
        stop('correction is in the wrong format, please check. Corrections must be in for of data frame with column names a.target_column and "breakdown" ')
    if( a.target_column %!in% names(a.corrections) )
        stop('correction is in the wrong format, please check. Corrections must be in for of data frame with column names a.target_column and "breakdown" ')
    if( all( names(a.corrections)[ names(a.corrections) %!in% c("breakdown") ] %!in% c(a.id_columns, a.target_column ) ) )
        stop('correction is in the wrong format, please check. Corrections must be in for of data frame with column names a.target_column and "breakdown" (optionally an id column) ')

    if( length(correction_id) == 0 && sum(a.corrections['breakdown']) != 1 )
            stop( 'correction breakdowns do not sum to 1' )
    if( length(correction_id > 0)) {
        sum <- a.corrections %>%
            select(-one_of(a.target_column)) %>%
            group_by_(correction_id) %>%
            summarise_all(funs(sum))
        if( !all(sum$breakdown == 1) )
            stop('breakdown must sum to 1 over target variable, check corrections')
    }

    # Check to see if there are corrections for all combinations in input data.
    # Do not stop, but throw warning if there are not defaults for all
    # combinations.
    no_defaults <- setdiff(
        unique( apply(a.input_data[a.match_columns],  1, paste, collapse = "-" ) ),
        unique( apply(a.corrections[a.match_columns], 1, paste, collapse = "-" ) )
    )

    if ( length(no_defaults) > 0 )
        stop( paste( 'In calculate_correct_shares: defaults are not provided for all breakdowns.',
                     'The following do not have defautls: ',
                     paste(no_defaults, collapse = ", ") ) )

    # ---------------------------
    # 2. Define useful variables
    X_years <- names(a.input_data)[grep('X',names(a.input_data))]
    a.input_data_long <- a.input_data %>%
        tidyr::gather(year, breakdown, -one_of(a.id_columns, a.target_column))

    # ---------------------------
    # 3. Separate input data into dataframes - those with zero sum breakdowns (to correct)
    # and those with non zero sum breakdowns (already correct), then
    # Replace zero sums (already correct df) with NAs

    # Start by replacing NAs with zero (makes sorting easier)
    a.input_data[is.na(a.input_data)] <- 0

    # Separate input data into 2 data frames
    # already_correct_years - non zero sums (set aside and add back to output later)
    # to_correct_years - zero sum data (will replace zeros with NAs then replace NAs with corrections)

    already_correct_years <- a.input_data_long %>%
        group_by_at(vars(a.id_columns, 'year')) %>%
        filter(sum(breakdown, na.rm = T) != 0)
    to_correct_years <- setdiff(a.input_data_long, already_correct_years)

    # check to make sure to_correct_years are all zero
    check_sum <- to_correct_years %>%
        dplyr::summarise(sum = sum(breakdown, na.rm = T))
    if( check_sum != 0)
        stop('In calculate_correct_shares(), correction years selected include non zero breakdowns. Please check.')

    # Check for all entries
    if( nrow(a.input_data_long) != (nrow(already_correct_years) + nrow(to_correct_years)) )
        stop("In calculate_correct_shares(), some entries are dropped. Please check.")

    # ---------------------------
    # 4. Correct to_correct_years
    #
    # 4.1 Correct with interpolation
    # interpolate between non NA values, and carry last observation forward
    # 1. Replace zeros with NA (in to_correct_correct years)
    # 2. add back in the non zero shares (alerady_correct_years) so there are values to interpolate over
    # 3. interpolate over NAs
    # 4. carry last observation forward over NAs
    if( nrow(to_correct_years) > 0 ) {
        correcting_years <- to_correct_years %>%
            replace(to_correct_years == 0, NA ) %>%
            rbind(data.frame(already_correct_years)) %>%
            spread(year, breakdown) %>%
            interpolate_NAs
        # last observation carried forward
        correcting_years[X_years] <- t(na.locf(t(correcting_years[X_years])))

        correcting_years_long <- correcting_years %>%
            tidyr::gather(year, breakdown, -one_of(a.id_columns, a.target_column)) %>%
            dplyr::distinct()

        # Again, seperate out non zero sum breakdowns into
        # already_corrected2 and to_correct2

        already_correct_years2 <- correcting_years_long %>%
            group_by_at(vars(a.id_columns, 'year')) %>%
            filter(sum(breakdown) != 0)
        to_correct_years2 <- setdiff(correcting_years_long, already_correct_years2)

    } else {
        # If there are no lines to correct with interpolation, then pass to new Dfs to renomalized below
        already_correct_years2 <- already_correct_years
        to_correct_years2 <- to_correct_years
    }

    # Check for all entries
    if( nrow(a.input_data_long) != (nrow(already_correct_years2) + nrow(to_correct_years2)) ){
        stop("In calculate_correct_shares(), some entries are dropped in while correcting shares through interpolation. Please check.")
    }
    # Check for NAs in already correct
    if( anyNA( already_correct_years2$breakdown ) ) stop( 'In calculate_correct_shares(), NA is default correction breakdowns, please check.')

    # Check that to_correct_years 2 are all NAs
    if( !all.na( to_correct_years2$breakdown ) ) stop( 'In calculate_correct_shares(), overwritting non NA percent breakdown, please check.')

    # ---------------------------
    # 3. Replace remaining NAs with default

    to_correct_years2$breakdown <- a.corrections[match( apply(to_correct_years2[a.match_columns], 1, paste, collapse='-') ,
                                                            apply(a.corrections[a.match_columns], 1, paste, collapse='-') ) , 'breakdown']

    if( anyNA( to_correct_years2$breakdown ) ) stop( 'In calculate_correct_shares(), NA is default correction breakdowns, please check.')

    all_corrected_years <- bind_rows(to_correct_years2, already_correct_years2) %>%
        spread(year, breakdown) %>%
        arrange_at(vars(a.id_columns, a.target_column))

    # Check for all entries again
    if( nrow(a.input_data_long) != (nrow(already_correct_years2) + nrow(to_correct_years2)) ){
        stop("In calculate_correct_shares(), some entries are dropped in while correcting shares through interpolation. Please check.")
    }


    # 4. Renormalize to 1
    out.df.renormalized <- calculate_shares( all_corrected_years,
                                             a.id_columns,
                                             a.target_column)
    # 5. Check ouput
    check_values <- out.df.renormalized %>%
        select(-one_of(a.target_column)) %>%
        group_by(.dots = a.id_columns) %>%
        summarise_all(funs(sum)) %>%
        ungroup() %>%
        select(-one_of(a.id_columns))

    if( ! nrow(check_values)*ncol(check_values) == sum(check_values) ) stop( "In calculate_correct_shares: all shares do not sum to 1 over id columns.")


    # 6. return value
    return( out.df.renormalized )
}


# -----------------------------------------------------------------------------
# extend_data_on_trend
# Brief:     extends data based on trend of other data
# Details:   for general use in modH, not cdiac extension.
# Dependencies:
# Author(s):
# Params:
        # driver_trend: data to extend with
        # input_data: data to be extended
        # start : start year of extension (must be in driver_trend)
        # end: end extension (year before ratio year)
#
# Return:
# Input Files:
# Output Files:
# TODO: merge, switch to extend_data_on_trend_cdiac

extend_data_on_trend <- function(driver_trend, input_data, start, end, diagnostics = F,
                                 IEA_mode = F,
                                 iea_start,
                                 iea_start_years_df){


  # Expand fuels - all-comb
  expand <- driver_trend[which(driver_trend$fuel == 'all' ) ,]
  driver_trend <- driver_trend[which(driver_trend$fuel != 'all' ) ,]
  comb_fuels <- c('biomass', 'hard_coal','brown_coal','coal_coke','natural_gas','heavy_oil','diesel_oil','light_oil')
  for (i in seq_along(comb_fuels)){
    expand$fuel <- rep(comb_fuels[i], times= nrow(expand) )
    driver_trend <- rbind( driver_trend, expand )
  }#for-ends

  #Check if the function is operating on IEA mode
  if(IEA_mode == T){
    #Extract OECD ountries (when iea_start = 1960) or Non-OECD ountries (when iea_start_year = 1970 )
    countries <- iea_start_years_df[which(iea_start_years_df$start_year == iea_start),'iso']

    #extract the drive data for each country
    driver_trend <- filter(driver_trend, iso %in% countries )

    #extract the input data for each country
    input_data <- filter(input_data, iso %in% countries )

    #initialize end variable to IEA start year
    end <- iea_start
  }#if Ends

  ratio_years <- paste0('X',c(end + 1:5))
  ext_start_year <- start
  ext_end_year <- end
  extension_years <- paste0('X',ext_start_year:ext_end_year)

  # select extension data for current method
  driver_lines <- driver_trend[, c('iso','sector','fuel') ]
  driver_lines <- unique(paste(driver_lines$iso,driver_lines$sector,driver_lines$fuel,sep='-'))

  # select ceds data to extend
  ceds_extension_ratios <- input_data[ which( paste(input_data$iso,input_data$sector, input_data$fuel, sep="-") %in% driver_lines  ) , ]

  #extended data template
  ceds_extension_ratios <- ceds_extension_ratios[,c('iso','sector','fuel',ratio_years)]

  # add Driver identifyer ratio year
  ceds_extension_ratios <- merge(ceds_extension_ratios, driver_trend[,c("iso", 'sector','fuel', ratio_years)],
                                 by.x = c('iso', 'sector','fuel'),
                                 by.y = c("iso", 'sector','fuel'),
                                 all.x = TRUE, all.y = FALSE)

  ceds_extension_ratios[ ratio_years ] <- ceds_extension_ratios[ paste0(ratio_years,'.x')]/ceds_extension_ratios[ paste0(ratio_years,'.y')]
  ceds_extension_ratios <- replace(ceds_extension_ratios, ceds_extension_ratios == 'NaN', 0)
  ceds_extension_ratios <- replace(ceds_extension_ratios, is.na(ceds_extension_ratios), 0)

  ceds_extension_ratios$ratio <-  rowMeans(ceds_extension_ratios[ ratio_years ])

  # Ratio Diagnostics
  if(diagnostics == T) writeData(ceds_extension_ratios , "DIAG_OUT",
                                 paste0('ceds_extension_ratios_',unique(driver_trend$sector)[1],'_',unique(driver_trend$fuel)[1],'_',start,'-',end),
                                 meta=F)

  # add driver data and use ratio to calculate extended value
  ceds_extended <- ceds_extension_ratios[,c('iso','fuel','sector','ratio')]
  ceds_extended [ extension_years ] <- NA
  ceds_extended <- replaceValueColMatch(ceds_extended, driver_trend,
                                        x.ColName = extension_years,
                                        match.x = c('iso','sector','fuel'),
                                        addEntries = FALSE)

  ceds_extended[is.na(ceds_extended)] <- 0

  # calculate extended data
  ceds_extended[ extension_years ] <- ceds_extended$ratio * ceds_extended[ extension_years ]

  # add to final extension template
  output_data <- replaceValueColMatch(input_data, ceds_extended,
                                     x.ColName = extension_years,
                                     match.x = c('iso','sector','fuel'),
                                     addEntries = FALSE) %>%
      select( one_of(names(input_data)))

  return(output_data)
}
# -----------------------------------------------------------------------------
# extend_data_on_trend_range
# Brief:     extends data based on trend of other data
# Details:   Calculates an average ratio of input:trend data in specified ratio
#            years.
#            Extended Data (year x) = input data(average ratio years) /
#                                     trend data(ratio years) * trend data (year x)
# Dependencies:
# Author(s):   Rachel Hoesly
#
# Params:
# iea_start_year - IEA start year is specified if the function is in IEA mode
# driver_trend - trend by which to extend input data
# input_data - data to be extended
# start - start of extension range (earliest year to be extended)
# end - end of extension range (latest year to be extended)
# expand - if input data has "all" or "all-combustion" for fuel, then this
#          expands the data; defaults to TRUE
# range - the length of the range of ratio years (calculates the average ratio);
#         defaults to 5
# ratio_start_year - earliest year of ratio years, defaults to the the year
#                    following extension end year
# id_match.driver - identifiers that match between driver and input (ex. for
#                   extension with population, iso and temp variable. Must be at
#                   least 2), defaults to c('iso','sector','fuel')
# id_match.input - id columns for the original data, if different than id driver
#                  (ex. cdiac, iso and fuel - but extended with iso and temp
#                  (population)) - used to match and replace variables in final
#                  part of function; defaults to value of id_match.driver
# extend_fwd_by_BP_years - Boolean specifying whether or not forward extension
#                          should be carried out; defaults to FALSE
# IEA_mode - Boolean indicating whether or not the function should treat input
#            and driver data as IEA data
# iea_start_years_df - Dataframe containing IEA start year from all countries

# Return:
# Input Files:
# Output Files:
# TODO:
      # must have at least 2 id variables
      # switch/merge with extend_data_on_trend

extend_data_on_trend_range <- function(iea_start_year, driver_trend, input_data,
                                       start, end,
                                       extend_fwd_by_BP_years = F,
                                       ratio_start_year = (end + 1),
                                 expand = T,
                                 range = 5,
                                 id_match.driver = c('iso','sector','fuel'),
                                 id_match.input = id_match.driver,
                                 IEA_mode = F, iea_start_years_df) {

  # define extension columns, extension data years, and extra id columns
  extension_years <- paste0('X', start:end)
  input_years <- names(input_data)[isXYear(names(input_data))]
  extra_id <- names(input_data)[names(input_data) %!in% c(input_years, id_match.driver, id_match.input, extension_years)]

  if( expand ){

    if( any('fuel' %in% names(driver_trend))){
      # Expand fuels - all-comb
      expand <- driver_trend[which(driver_trend$fuel == 'all' ) ,]
      driver_trend <- driver_trend[which(driver_trend$fuel != 'all' ) ,]
      comb_fuels <- c('biomass', 'hard_coal','brown_coal','coal_coke','natural_gas','heavy_oil','diesel_oil','light_oil')
      for (i in seq_along(comb_fuels)){
        expand$fuel <- rep(comb_fuels[i], times= nrow(expand) )
        driver_trend <- rbind( driver_trend, expand )
      }
    }
  }#if Ends

  #Check if the function is operating on IEA mode
  if(IEA_mode == T){
    #Extract OECD ountries (when iea_start_year =1960) or Non-OECD ountries (when iea_start_year = 1970 )
    countries <- iea_start_years_df[which(iea_start_years_df$start_year == iea_start_year),'iso']

    #extract the drive data for each country
    driver_trend <- filter(driver_trend, iso %in% countries )

    #extract the input data for each country
    input_data <- filter(input_data, iso %in% countries )

    #initialize end variable to IEA start year
    end <- iea_start_year

    #initialize ratio year
    ratio_start_year = (end + 1)
  }#if Ends

  #define ratio columns
  ratio_years <- paste0('X',c(ratio_start_year + 0:(range-1)))

  #define extension columns
  ext_start_year <- start
  ext_end_year <- end
  extension_years <- paste0('X',ext_start_year:ext_end_year)

  #extract orinal input_data columns
  input_data_cols_original <- names(input_data)

  #initialize id_match.input_original / id_match.driver_original
  id_match.input_original <- id_match.input
  id_match.driver_original <- id_match.driver

  # create vectors of input and driver data we want to use with match() to extend data
  driver_lines <- unique( driver_trend[ , id_match.driver ] )
  if ( length(id_match.driver) > 1 ) driver_lines <- unique( apply( driver_trend[ , id_match.driver ], 1, paste, collapse = "-" ) )
  input_lines <- input_data[ , id_match.driver ]
  if ( length(id_match.driver) > 1 ) input_lines <- apply( input_data[ , id_match.driver ], 1, paste, collapse = "-" )

  # select ceds data to extend
  extension_ratios <- input_data[ which( input_lines %in% driver_lines ) , ]

  #extended data template
  extension_ratios <- extension_ratios[,unique(c(id_match.driver,id_match.input,extra_id,ratio_years))]

  # add Driver identifyer ratio year
  extension_ratios <- merge(extension_ratios, driver_trend[,c(id_match.driver,ratio_years)],
                                 by.x = id_match.driver,
                                 by.y = id_match.driver,
                                 all.x = TRUE, all.y = FALSE)

  #compute the ratio for each ratio year
  extension_ratios[ ratio_years ] <- extension_ratios[ paste0(ratio_years,'.x')]/extension_ratios[ paste0(ratio_years,'.y')]

  #initialize non-numeric values to zero
  extension_ratios <- replace(extension_ratios, extension_ratios == 'NaN', 0)
  extension_ratios <- replace(extension_ratios, extension_ratios == 'Inf', 0)
  extension_ratios <- replace(extension_ratios, is.na(extension_ratios), 0)

  #compute the ratio mean
  extension_ratios$ratio <- rowMeans(extension_ratios[ ratio_years ])

  # add driver data and use ratio to calculate extended value
  ceds_extended <- extension_ratios[,unique(c(id_match.driver,id_match.input_original,extra_id,'ratio'))]
  ceds_extended [ extension_years ] <- NA
  ceds_extended <- replaceValueColMatch(ceds_extended, driver_trend,
                                        x.ColName = extension_years,
                                        match.x = id_match.driver,
                                        addEntries = FALSE)
  #initialize NAs to zero
  ceds_extended[is.na(ceds_extended)] <- 0

  # calculate extended data
  ceds_extended[ extension_years ] <- ceds_extended$ratio * ceds_extended[ extension_years ]

  # add to final extension template
  input_data[extension_years] <- NA
  input_data <- replaceValueColMatch(input_data[c(id_match.input,extra_id,extension_years,input_years)], ceds_extended,
                                     x.ColName = extension_years,
                                     match.x = id_match.input_original,
                                     addEntries = FALSE)

  #Remove duplicate rows produced by replaceValueColMatch function
  input_data <- Filter(function(x)!all.na( x ), input_data)

  return(input_data)
}#extend_data_on_trend_range() Ends





# -----------------------------------------------------------------------------
# disaggregate_country
# Brief:        Disaggregate data of specified aggregate country to specified split countries based on trend data
#               (e.g. FSU, etc.)
# Details:      Disaggregates data by user choice of 2 methods.
#               1 : Disaggregates using proportions initially from n earliest years from original split data, with proportion changing
#               over time according to trend data, proportions renormalized to 1.
#               2: Disaggregate using proportions from trend data overtime. Note: May result in discontinuities if trend data and original
# data have different proportions
# returns data frame of same format without the aggregate country data
#
# Dependencies:	replaceValueCol(), extend_data_on_trend_range
# Author(s):     Rachel Hoesly
#
# original_data - data frame with rows to be split. Contains lines with aggregate country data in disaggregation
#           years( dis_start_year:dis_end_year) and disaggregate data in non-disaggregation years. May contain data lines of other countries, which is not disturbed
# id_cols - columns for matching. defaults to using non year X- cols are selected (id_col = T). Used for normalizing ratios to 1.
#           ex: c(‘iso’,’sector’,’fuel’) - ratios normalized to 1 over all sector-fuel combinations (all iso’s add to 1 for each combination)
#           ex: c(‘iso’,’fuel’) - ratios normalized to 1 over all fuels (all iso’s add to 1 for each fuel)
# trend_data - data by which to trend split or ratios
# trend_match_cols - columns in trend_data to match with original_data
# combined_iso - character string, the iso name of the country to be split ex: 'ussr’
# disaggregate_iso - vector of character strings, the names of the countries to replace combined_iso ex: c('aze','rus','ukr',...)
# dis_end_year - numeric, the latest year of non zero data for the aggregate iso, ex 1991
# dis_start_year - earliest year of aggregate data, defaults to 1750
# ratio_range_length = 2
# ratio_start_year = (dis_end_year+1)
# method - numeric (1 or 2)
#           1: Disaggregates using proportions initially from n earliest years from original split data, with proportion changing
#           over time according to trend data, proportions renormalized to 1.
#           2: Disaggregate using proportions from trend data overtime. Note: May result in discontinuities if trend data and original
#           data have different proportions
# remove_aggregate - T/F boolean. If T, then aggregate county data is removed from the returned data frame. If F, lines of aggregate data remain
# write_over_values - T/F boolean. If T, then any nonzero values for disaggregate_iso countries in disaggregate years will be overwritten.
#           If F, then function will error and print message showing non zero values in disaggregate years.
# Return:   data frame with split values, in same format as all_data_in. Does not contain rows of aggregate country data, only
#           disaggregate countries
# Input Files:   none
# Output Files:  none
# Notes: If ratio = T:
#           - if aggregate data is zero in ratio years, defaults to ratio = F
#           - if disaggregate_iso are not all in all_data, defaults to ratio = F
#           - if disaggregate_iso are not all in trend_data, load population data to use as trend_data
# TODO: check extend_data_on_trend_range () - does it trend data forward also , or only back?

disaggregate_country <- function(original_data,
                                 trend_data,
                                 trend_match_cols,
                                 combined_iso,
                                 disaggregate_iso,
                                 dis_end_year,
                                 dis_start_year = historical_pre_extension_year,
                                 method = 1,
                                 ratio_range_length = 2,
                                 ratio_start_year = (dis_end_year+1),
                                 id_cols = T,
                                 remove_aggregate = T,
                                 write_over_values = F,
                                 allow_dropped_data = F
) {

  # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # 1. Prepare data, check inputs

  # replace NAs with zero in original_data
  original_data <- replace(original_data,is.na(original_data), 0)

  # Define id_cols if not user Defines
  if ( all( id_cols == T) ) id_cols <- names(original_data)[-grep('X', names(original_data))]

  # Input Parameter Checks
  if( !all (trend_match_cols %in% names(trend_data) ) ) stop('in disaggregate_country(): all "trend_match columns" are not in "trend_data"')
  if( !all( paste0('X',dis_start_year:dis_end_year) %in% names(original_data) )) stop('in disaggregate_country(): original data does not extend over specified disaggregation years')
  if( !all( paste0('X',dis_start_year:dis_end_year) %in% names(trend_data) )) stop('in disaggregate_country(): trend_data does not extend over all disaggregation years')

  if( !all( paste0('X',ratio_start_year:(ratio_start_year+ratio_range_length)) %in% names(trend_data) ) & method == 1 ) stop('in disaggregate_country(): trend_data does not extend over all ratio years')

  if( !combined_iso %in% original_data$iso ) stop('in disaggregate_country(): "combined_iso" is not in "original_data" ')
  if( !all(disaggregate_iso %in% trend_data$iso) ) stop('in disaggregate_country(): "disaggregate_iso" not all included in "trend_data" ')
  if( !all(trend_match_cols %in%  names(trend_data)) ) stop('in disaggregate_country(): "trend_match_cols" not valid columnnames in trend_data" ')
  if( !all(id_cols %in% names(original_data)) ) stop('in disaggregate_country(): "id_cols" not valid columnnames in original_data"  ')
  if( !((identical(method,1) | identical(method,2)) & length(method)==1) ) stop(paste( 'in disaggregate_country(): "', method,'" is not a valid method. Defaulting to method 1, disaggregation
          using fractions from original data in ratio years, extended with trend data, and renormalized'))

  # Remind the user of unused id cols if neccessary
  unused_ids <- names(original_data)[-grep('X', names(original_data))][ which( names(original_data)[-grep('X', names(original_data))] %!in% id_cols ) ]
  if( length(unused_ids) > 0) printLog(paste('in disaggregate_country(): There are (non year) id columns in original data that are not specified as id_cols: ',unused_ids ) )

  # Define match_cols as id_cols that are not 'iso'
  match_cols <- id_cols[id_cols != 'iso']

  # Define useful year ranges - variables from input
  X_all_years <- names(original_data)[grep('X', names(original_data))]
  X_all_years <- X_all_years[order(X_all_years)]
  all_years <- as.numeric(gsub( 'X','', X_all_years))
  all_years <- all_years[order(all_years)]
  disaggregate_years <- paste0('X', dis_start_year : dis_end_year)
  ratio_years <- paste0('X',c(ratio_start_year + 0:(ratio_range_length-1)))

  # Check if ratio years are in original data
  if( method == 1 & !all(ratio_years %in% X_all_years )){
      printLog('in disaggregate_country(): select ratio years are not in original data. Default to disaggregating with trend data only (not extended ratios from original_data)')
      method <- 2 }

  # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # 2. Subset Disaggregate and Aggregate Data, and process if neccessary
  disaggregate_data <- original_data[ which( original_data$iso %in% disaggregate_iso), ]
  aggregate_data <- original_data[ which( original_data$iso == combined_iso), c(match_cols,X_all_years) ]

  # Check  Data
    # often aggregate data becomes zero in ratio years (already disaggregated, like USSR). replace aggregate data with the sum
    # of disaggregate data
    if( method == 1 ) {
      if( sum(aggregate_data[ratio_years]) == 0 ) {
      aggregate_data_post <- aggregate(disaggregate_data[ ratio_years ],
                                       by = lapply( X = match_cols, FUN = function(x) { disaggregate_data[[x]] } ),
                                       sum)
      names(aggregate_data_post) <- c(match_cols,ratio_years)

      aggregate_data <- replaceValueColMatch(aggregate_data, aggregate_data_post,
                                             x.ColName = ratio_years,
                                             match.x = match_cols,
                                             addEntries = F)
      }}

    # if disaggregate data rows are not in data at all - add zero rows
  add_all_disag_iso <- F
  if( nrow(disaggregate_data) == 0){
      add_all_disag_iso <- T
      disaggregate_data <- data.frame(iso = disaggregate_iso)
      for (i in seq_along(match_cols)){
        nrow <- nrow(disaggregate_data)
        ids <- unique( aggregate_data[[ match_cols[i] ]] )
        number_ids <- length(ids)
        disaggregate_data <- do.call(rbind, replicate(n = number_ids, expr = disaggregate_data,simplify = FALSE) )
        disaggregate_data[ match_cols[i] ] <- rep(x = ids, each = nrow)
      }
      disaggregate_data[paste0('X',all_years)] <- 0
    }
  aggregate_data$iso <- combined_iso
  aggregate_data <- aggregate_data[, c(id_cols,X_all_years)]

  # check for non zero values in disaggregate data, in disaggregate years
  non_zero_disaggregate_iso <- unique(disaggregate_data[which( rowSums(disaggregate_data[disaggregate_years]) != 0 ),'iso'])

  if( length(non_zero_disaggregate_iso) > 1) non_zero_disaggregate_iso <- paste(non_zero_disaggregate_iso, collapse = ' , ')
  if( length(non_zero_disaggregate_iso) > 0 ) {
    # diagnostics on nonzero disaggregate values
    ag_long <- suppressMessages (  melt( aggregate_data[ c(id_cols, disaggregate_years ) ]) )
    names(ag_long) <- c(id_cols,'year','ag_value')
    non_zero_disaggregate <- disaggregate_data[which( rowSums(disaggregate_data[disaggregate_years]) != 0 ),]
    non_zero_disaggregate <- suppressMessages ( melt( non_zero_disaggregate[ c(id_cols, disaggregate_years ) ] , variable_name = 'year'))
    non_zero_disaggregate <- merge(non_zero_disaggregate[which(non_zero_disaggregate$value > 0),],
                                   ag_long[ c(match_cols, 'year', 'ag_value')],
                                   all.x = T, all.y = F)
    non_zero_disaggregate$percent <- non_zero_disaggregate$value/non_zero_disaggregate$ag_value * 100
    percent_range <- paste0( round(min(non_zero_disaggregate$percent),2 ),'% - ', round(max(non_zero_disaggregate$percent),2 ),'%')

    if( !write_over_values ){ stop( paste0( 'in disaggregate_country(): there is non zero data in disaggregate countries over disaggregation years, which make up, ',percent_range,
                                            ' of aggregate values. Disaggregate countries: ', non_zero_disaggregate_iso))}
    else if( write_over_values ){ printLog( paste0( 'in disaggregate_country(): there is non zero data in disaggregate countries over disaggregation years, which make up, ',percent_range,
                                                    ' of aggregate values. Disaggregate countries: ', non_zero_disaggregate_iso)) } }

  # sum aggregate data for checking at end
   sum_check <- data.frame(ag_value = colSums(aggregate_data[disaggregate_years]))
   aggregate_data_0 <- aggregate_data[which( rowSums( aggregate_data[disaggregate_years] ) == 0 ),]
  # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # 3. Check Methods:

  # If disaggregate countries are not in original data, then cannot calculate ratio to extend - if so, defatult to disaaggregate on trend rather than ratio extended on trend
  if( method == 1  & !all(disaggregate_iso %in% original_data$iso) ) {
    method <- 2
    printLog('in disaggregate_country(): Some disaggregate countries are not in origingal data. Default to disaggregating with trend data only (not extended ratios from original_data)')
  }
  # If all aggregate data is zero in ratio years (still, after steps above to aggregate disaggregate isos in ratio years), cannot calculate ratio
  if( method == 1 ){
    if( sum(aggregate_data[ratio_years]) == 0 ){
    printLog( 'in disaggregate_country(): aggregate data sums to zero. No ratios can be calculated. Default to disaggregating with trend data only (not extended ratios from original_data)')
    method <- 2
    }}

  # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # 4. Disaggregate with Method 1
  if(  method == 1 ){
    printLog( "Disaggregating using proportions initially from n earliest years from original split data, with proportion changing
              over time according to trend data, proportions renormalized to 1.")

    # Calculate Ratios - disaggregate country percent of aggregate by fuel
    ceds_extension_ratios <- merge(disaggregate_data[,c('iso',match_cols,ratio_years)], aggregate_data[,c(match_cols, ratio_years)],
                                   by = match_cols,
                                   all.x = TRUE, all.y = FALSE)

    ceds_extension_ratios[ ratio_years ] <- ceds_extension_ratios[ paste0(ratio_years,'.x')]/ceds_extension_ratios[ paste0(ratio_years,'.y')]

    ceds_extension_ratios <- replace(ceds_extension_ratios, ceds_extension_ratios == 'NaN', 0)
    ceds_extension_ratios <- replace(ceds_extension_ratios, is.na(ceds_extension_ratios), 0)
    ceds_extension_ratios <- ceds_extension_ratios[,c('iso',match_cols,ratio_years)]

    # Extend Ratios based on trend
    driver_trend_for_ratios <- trend_data

    extended_ceds_extension_ratios <- extend_data_on_trend_range(driver_trend = driver_trend_for_ratios, input_data = ceds_extension_ratios,
                                                                 start = dis_start_year, end = dis_end_year,
                                                                 ratio_start_year = ratio_start_year,
                                                                 expand = F,
                                                                 range = ratio_range_length,
                                                                 id_match.driver = trend_match_cols,
                                                                 id_match.input = c('iso',match_cols))

    extended_ceds_extension_ratios <- extended_ceds_extension_ratios[,c('iso',match_cols,disaggregate_years,ratio_years)]

    # Renomalize ratios so that all disaggregate countries sum to 1 by fuel

    extended_ceds_extension_ratios_renormalize <- calculate_shares(input_data = extended_ceds_extension_ratios,
                                                                   id_columns = match_cols,
                                                                   target_col = 'iso')

    # Calculate Split Data - [split data] = [aggregate]*[disaggregate ratio]
    # ratio
    ratio_matrix <- disaggregate_data[ , c('iso',match_cols) ]
    ratio_matrix <- disaggregate_data[ , c('iso',match_cols) ]
    ratio_matrix[disaggregate_years] <- NA
    ratio_matrix <- replaceValueColMatch(ratio_matrix, extended_ceds_extension_ratios_renormalize,
                                             x.ColName = disaggregate_years,
                                             match.x = c('iso',match_cols),
                                             addEntries = F)
    ratio_matrix <- arrange_( ratio_matrix, c('iso',match_cols))

    # check that all ratios add to 1. If they add to zero, tell user. May be missing trend data
    ratio_check <- aggregate(ratio_matrix[disaggregate_years], by = ratio_matrix[match_cols], sum)
    ratio_check_0 <- ratio_check[which( rowSums( ratio_check[disaggregate_years] ) == 0 ),]

    # aggregate data template
    aggregate_matrix <- disaggregate_data[ , c('iso',match_cols) ]
    aggregate_matrix[disaggregate_years] <- NA
    aggregate_matrix <- replaceValueColMatch(aggregate_matrix, aggregate_data,
                                             x.ColName = disaggregate_years,
                                             match.x = match_cols,
                                             addEntries = F)
    aggregate_matrix <- arrange_( aggregate_matrix, c('iso',match_cols))

    # check order of ratio and aggregate data matrix
    if(!all( ratio_matrix$iso == aggregate_matrix$iso )) stop('in disaggregate_country(): in method 1, dataframes in different order when calculating split data.')

    # calculate new data
    new_data <- ratio_matrix[ , c('iso',match_cols) ]
    new_data[disaggregate_years] <- aggregate_matrix[disaggregate_years] * ratio_matrix[ disaggregate_years ]

    # check total sums
    sum_check2 <- data.frame(ag_value = colSums(new_data[disaggregate_years]))
    compare_sum <-  cbind(sum_check, sum_check2)
    names(compare_sum) <- c('old','new')
    compare_sum$percent_difference <- abs(compare_sum$old - compare_sum$new)/abs(compare_sum$old)*100
    not_equal <- F
    if( max( compare_sum$percent_difference , na.rm = T) > .01 ) {
      not_equal <- T
      max_difference <- round(max( compare_sum$percent_difference , na.rm = T),2)
      max_year <- row.names(compare_sum)[which(compare_sum$percent_difference == max_difference)] }
    # Errors and Log Messages for dropped data
    if( nrow(ratio_check_0) > 0 ){
      if( length(match_cols) == 1 ) {zero_data <-  unique( ratio_check_0[[match_cols]] )
      zero_ag_data <-  unique( aggregate_data_0[[match_cols]]) }
      if( length(match_cols) > 1 ) {zero_data <-  unique(apply(MARGIN = 1, X= ratio_check_0[match_cols],FUN=paste0, collapse='-'))
      zero_ag_data <-  unique(apply(MARGIN = 1, X= aggregate_data_0[match_cols],FUN=paste0, collapse='-'))}

      zero_data <- zero_data[zero_data %!in% zero_ag_data]

      if( allow_dropped_data & not_equal) printLog( paste('in disaggregate_country(): There is non zero aggregate data, but no disaggregate trend data for: ',
                                               paste( zero_data, collapse = ' , '),'
                                               Some aggregate data dropped. Dropped data equals', max_difference, '% of the sum of all aggregate data
                                               (over all countries, fuels, etc) in year', max_year))
      if( !allow_dropped_data & not_equal )  stop( paste('in disaggregate_country(): There is non zero aggregate data, but no disaggregate trend data for: ',
                                                         paste( zero_data, collapse = ' , '),'
                                               Some aggregate data dropped. Dropped data equals', max_difference, '% of the sum of all aggregate data
                                               (over all countries, fuels, etc) in year', max_year, 'Inspect data, use method 2 for disaggregation,
                                               or if appropriate, consider "choosing the allow_dropped_data = T" option.') )
    }

    if( !allow_dropped_data ){
      if(not_equal) stop('in disaggregate_country(): sum of disaggregate data does not equal sum of aggregate data') }

    if( allow_dropped_data ){
      if(not_equal) printLog('in disaggregate_country(): sum of disaggregate data does not equal sum of aggregate data. Some data is dropped.') }

    # add back to full data
    if( !all( apply(FUN= paste, MARGIN = 1, X= new_data[ id_cols ], collapse = '-') %in% apply(FUN= paste, MARGIN = 1, X= original_data[ id_cols ] , collapse = '-') )) {

      split_data <- replaceValueColMatch(original_data,new_data,
                                         x.ColName = disaggregate_years,
                                         match.x = c('iso',match_cols),
                                         addEntries = TRUE,
                                         replace_NAs = T)
      split_data[is.na(split_data)] <- 0

    } else{

      split_data <- replaceValueColMatch(original_data,new_data,
                                         x.ColName = disaggregate_years,
                                         match.x = c('iso',match_cols),
                                         addEntries = FALSE)

    }
  }

  # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # 5. Disaggregate with Method 2
  if( method == 2 ) {
    printLog( "Disaggregating using proportions from trend data overtime.")

    non_iso_trend_match <- trend_match_cols[trend_match_cols %!in% 'iso']

    # If not all dissaggregate countries have data, then use population data as trend data. There should be
    # population data for all isos
    if( !all(disaggregate_iso %in% trend_data$iso) ){
      printLog( "Disaggregate iso not in original data, using population data as trend data ")

      # load population data
      un_pop <- readData( "MED_OUT" , 'A.UN_pop_master' )
      un_pop$X_year <- paste0( 'X', un_pop$year)
      un_pop$pop <- as.numeric(un_pop$pop)
      population <- cast( un_pop[which ( un_pop$year %in% historical_pre_extension_year:end_year ) , ] ,
                          iso ~ X_year, value = 'pop')

      trend_data <- population
      trend_match_cols <- 'iso'
      # If population data does not have all iso, stop function
      if( !all(disaggregate_iso %in% trend_data$iso) ){ stop('in disaggregate_country(): population data does not have data for all disaggregate_iso ')}
    }


    # Subset trend_data
    aggregate_trend_data <- trend_data[ which( trend_data$iso == combined_iso), c(trend_match_cols,disaggregate_years) ]
    disaggregate_trend_data <- trend_data[ which( trend_data$iso %in% disaggregate_iso), c(trend_match_cols,disaggregate_years)]

    # if the aggregate country is not in trend data, aggregate the disaggregate entries
    if( nrow(aggregate_trend_data ) == 0 ) {
      if( length(trend_match_cols[trend_match_cols %!in% 'iso']) == 0 ){
        # todo: get rid of temp column - round about way of aggregating
        temp_disaggregate_trend <- disaggregate_trend_data
        temp_disaggregate_trend$temp <- 'temp'

        aggregate_trend_data <- aggregate( temp_disaggregate_trend[disaggregate_years],
                                           by = list(iso = temp_disaggregate_trend$temp),
                                           sum)
        names(aggregate_trend_data) <- c(trend_match_cols,disaggregate_years)
      }
      if( length(trend_match_cols[trend_match_cols %!in% 'iso']) > 0 ){
        aggregate_trend_data <- aggregate(disaggregate_trend_data[ disaggregate_years ],
                                          by = lapply( X = trend_match_cols[trend_match_cols %!in% 'iso'], FUN = function(x) { disaggregate_trend_data[[x]] } ),
                                          sum)
        names(aggregate_trend_data) <- c(trend_match_cols[trend_match_cols %!in% 'iso'],disaggregate_years)}
    }

    # check that there is trend data for all aggregate data
    # # list values in Non zero aggregate original data (over disaggregate years)
    # non_zero_aggregate_data <- aggregate_data[which( rowSums(aggregate_data[disaggregate_years]) != 0 ), ]
    # if( length(non_iso_trend_match) > 1) agg_orig_entries <- unique(apply(MARGIN = 1, X= non_zero_aggregate_data[non_iso_trend_match],FUN=paste0, collapse='-'))
    # if( length(non_iso_trend_match) == 1) agg_orig_entries <- unique( non_zero_aggregate_data[[non_iso_trend_match]] )
    # # list values in disaggregate trend data
    # if( length(non_iso_trend_match) > 1) dis_trend_entries <- unique(apply(MARGIN = 1, X= disaggregate_trend_data[non_iso_trend_match],FUN=paste0, collapse='-'))
    # if( length(non_iso_trend_match) == 1) dis_trend_entries <- unique( disaggregate_trend_data[[non_iso_trend_match]] )
    #
    # agg_orig_entries[which(agg_orig_entries %!in% dis_trend_entries)]
    #
    # calculate trend ratios for disaggregate countries. shares of each country for each year
    trend_ratio <- calculate_shares(disaggregate_trend_data,
                                 id_columns = trend_match_cols[trend_match_cols %!in% 'iso'],
                                 target_column = 'iso')

    # Calculate Split Data - [split data] = [aggregate]*[disaggregate ratio]
    # ratio
        ratio_matrix <- disaggregate_data[ , c('iso',match_cols) ]
        ratio_matrix[disaggregate_years] <- 0
        ratio_matrix <- replaceValueColMatch(ratio_matrix, trend_ratio,
                                             x.ColName = disaggregate_years,
                                             match.x = c(trend_match_cols),
                                             addEntries = F)
        ratio_matrix <- arrange_( ratio_matrix, c(trend_match_cols))

        # check that all ratios add to 1. If they add to zero, tell user. May be missing trend data
        ratio_check <- aggregate(ratio_matrix[disaggregate_years], by = ratio_matrix[match_cols], sum)
        ratio_check_0 <- ratio_check[which( rowSums( ratio_check[disaggregate_years] ) == 0 ),]

        if( nrow(ratio_check_0) > 0 ){
          if( length(non_iso_trend_match) == 1 ) zero_data <-  unique( ratio_check_0[[non_iso_trend_match]] )
          if( length(non_iso_trend_match) > 1 ) zero_data <-  unique(apply(MARGIN = 1, X= ratio_check_0[non_iso_trend_match],FUN=paste0, collapse='-'))

         if( allow_dropped_data ) printLog( paste('in disaggregate_country(): There is non zero aggregate data, but no disaggregate trend data for: ',
                                                   paste( zero_data, collapse = ' , '),'
                                                   Some aggregate data may be dropped.'))
         if( !allow_dropped_data )  stop( paste('in disaggregate_country(): There is non zero aggregate data, but no disaggregate trend data for: ',
                                                 paste( zero_data, collapse = ' , ') ) )
        }

    # aggregate data template
        aggregate_matrix <- disaggregate_data[ , c('iso',match_cols) ]
        aggregate_matrix[disaggregate_years] <- 0
        aggregate_matrix <- replaceValueColMatch(aggregate_matrix, aggregate_data,
                                             x.ColName = disaggregate_years,
                                             match.x = match_cols,
                                             addEntries = F)
        aggregate_matrix <- arrange_( aggregate_matrix, trend_match_cols )


    # check order of ratio and aggregate data matrix
        for (i in seq_along((trend_match_cols))){
            rat <- ratio_matrix[ trend_match_cols[i] ]
            ag <- aggregate_matrix[ trend_match_cols[i] ]
            if(!all( rat == ag )) stop('in disaggregate_country(): in method 2, dataframes in different order when calculating split data.')
        }

    # calculate new data
    new_data <- ratio_matrix[ , c('iso',match_cols) ]
    new_data[disaggregate_years] <- aggregate_matrix[disaggregate_years] * ratio_matrix[ disaggregate_years ]

    # check total sums
    sum_check2 <- data.frame(ag_value = colSums(new_data[disaggregate_years]))
    compare_sum <-  cbind(sum_check, sum_check2)
    names(compare_sum) <- c('old','new')
    compare_sum$percent_difference <- abs(compare_sum$old - compare_sum$new)/abs(compare_sum$old)

    not_equal <- F
    if( max( compare_sum$percent_difference , na.rm = T) > .01 ) not_equal <- T
    if( !allow_dropped_data ){
      if(not_equal) stop('in disaggregate_country(): sum of disaggregate data does not equal sum of aggregate data') }

    if( allow_dropped_data ){
      if(not_equal) printLog('in disaggregate_country(): sum of disaggregate data does not equal sum of aggregate data. Some data is dropped.') }

    # add back to full data
    # if not all new rows are in the old code
    if( !all( apply(FUN= paste, MARGIN = 1, X= new_data[ id_cols ], collapse = '-') %in%
                apply(FUN= paste, MARGIN = 1, X= original_data[ id_cols ] , collapse = '-') ) ) {

    split_data <- replaceValueColMatch(original_data,new_data,
                                       x.ColName = disaggregate_years,
                                       match.x = c('iso',match_cols),
                                       addEntries = TRUE,
                                       replace_NAs = T)
    split_data[is.na(split_data)] <- 0

    } else{

      split_data <- replaceValueColMatch(original_data,new_data,
                                         x.ColName = disaggregate_years,
                                         match.x = c('iso',match_cols),
                                         addEntries = FALSE)

    }

  }

  # ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # 6. Final processing

  if( remove_aggregate == T ){
  #remove aggregate from data
  split_data <- split_data[which(split_data$iso %!in% combined_iso), ]
  split_data <- split_data[ ,c(id_cols , X_all_years) ] }

  return(split_data)
}
