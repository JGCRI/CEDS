# ----------------------------------------------------------------------------------
# CEDS R header file: data moulding functions
# Authors: Ben Bond-Lamberty, Jon Seibert, Tyler Pitkanen
# Last Updated: 22 June 2015

# This file should be sourced by any R script doing heavy-duty reformatting of CEDS data.
# Functions contained:
#   %!in%, replaceValueColMatch ,gsub2, repeatAndAddVector, addCols, findDataStart, naReplace, addCols,
#   buildCEDSTemplate, removeBlanks

# Notes:
# -----------------------------------------------------------------------------
# Brief:        
# Details:      
# Dependencies: 
# Author(s):    
# Params:       
#  
# Return:       
# Input Files:  
# Output Files: 
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
                                  addEntries){
  out <- x
  n<-length(match.x)
  x.match.cols <- x[,match.x[1]]
  y.match.cols <- y[,match.y[1]]
  for (i in 2:n){
    x.match.cols <- paste(x.match.cols, x[,match.x[i]]  )
    y.match.cols <- paste(y.match.cols, y[,match.y[i]]  )
  }
  
  out[,x.ColName] <- y[match(x.match.cols,y.match.cols),
                       y.ColName]
  
  index <- which(!complete.cases(out[,x.ColName]))
  out[index,x.ColName] <- x[index,x.ColName]
  
  if (addEntries){
    x.names <- names(x)
    
    hybrid.names <- x.names
    hybrid.names[which( hybrid.names %in% match.x)] <- match.y
    hybrid.names[which( hybrid.names== x.ColName)] <- y.ColName
    if( ! identical(x.names, hybrid.names)) stop(paste0('Cannot add entries to original dataframe ',
                                                        'in replaceValueColMatch. Check column names.'))
    out <- rbind.fill(out, y[which(is.na(match(y.match.cols,x.match.cols))),])
  }
  return(out)  
}


# -----------------------------------------------------------------------------
# gsub2
# Brief:        Pattern replacement in a vector.
# Details:      Performs gsub on each element of a vector.
# Dependencies: gsub
# Author(s):    Page Kyle
# Params:       
#  pattern:     Specific string or number to search for within x [required]
#  replacement: Replacement for the pattern [required]
#  x:           Vector of strings to be searched [required]
# Return:       Input vector of strings with pattern replaced by its replacement.
# Input Files:  None
# Output Files: None

gsub2 <- function( pattern, replacement, x, ... ) {
      for(i in 1:length(pattern))
      x <- gsub(pattern[i], replacement[i], x, ...)
      x
}

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
# naReplace
# Brief:        Replace NA or NaN values with something (generally a number 1 or 0).
# Details:      Useful for making small assumptions about data to make data more easily
#                   workable.
# Dependencies: apply
# Author(s):    Tyler Pitkanen
# Params:       
#  data:        Data frame or vector to perform replacement in [required]
#  target:      Data instances to be replaced [default: NA]
#  sub:         Value to be subsistuted for the target instances [default: 0]
# Return:       Modified data frame or vector.
# Input Files:  None
# Output Files: None
    naReplace <- function( data, target = NA, sub = 0 ) {
        if( is.na( target ) ) { list_targets <- apply( data, 2, is.na )
        } else { list_targets <- apply( data, 2, is.nan ) }
        mod <- data
        mod[ list_targets ] <- sub
        return( mod )
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
        if( !any( names( df ) == ""  ) && !any( is.na( names( df ) ) ) ) return( df )
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

# ----------------------------------------------------------------------------------------
