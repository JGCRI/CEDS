# ----------------------------------------------------------------------------------
# CEDS R header file: data moulding functions
# Authors: Ben Bond-Lamberty, Jon Seibert, Tyler Pitkanen
# Last Updated: 22 June 2015

# This file should be sourced by any R script doing heavy-duty reformatting of CEDS data.
# Functions contained:
#   %!in%, gsub2, repeatAndAddVector, addCols, findDataStart, naReplace, addCols,
#   buildCEDSTemplate

# Notes:

# -----------------------------------------------------------------------------
#Function "is not an element of" (opposite of %in%)
'%!in%' <- function( x, y ) !( '%in%'( x, y ) )

# -----------------------------------------------------------------------------
# gsub2: pattern replacement in a vector
gsub2 <- function( pattern, replacement, x, ... ) {
      for(i in 1:length(pattern))
      x <- gsub(pattern[i], replacement[i], x, ...)
      x
}

# -----------------------------------------------------------------------------
# repeatAndAddVector: function for repeating a dataframe in order to add a new vector
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
# naReplace: Replace NA or NaN values with something (generally a number 1 or 0). 
#   Useful for making small assumptions about data to make data more easily
#   workable.
    naReplace <- function( data, target = NA, sub = 0 ) {
        if( is.na( target ) ) { list_targets <- apply( data, 2, is.na )
        } else { list_targets <- apply( data, 2, is.nan ) }
        mod <- data
        mod[ list_targets ] <- sub
        return( mod )
    }
    
# ----------------------------------------------------------------------------------
# removeBlanks
# Brief: Eliminates non-data rows from an imported excel file and fixes column names.
# Details: Puts imported excel sheet into a form that is easier to manipulate in R. Fixes 
# 	     column names and eliminates white space, rows of NA values, sheet titles, etc.
#	     Intended for use with reading xlsx files.
# Dependencies: none
# Authors: Jon Seibert, Tyler Pitkanen
# Parameters: 
# 	df: 		data frame of the imported excel sheet to be fixed [required]
#	first_name: string of the intended name for column 1 [required]
# Return: modified data frame with correct column names and no empty rows
# Input files: none
# Output files: none

# Usage examples: df <- removeBlanks( df,"Country" )
#			data_list <- lapply( data_list, removeBlanks, first_name = "Country" )

removeBlanks <- function( df, first_name ){
    if ( first_name %in% names( df ) ){ 
        return( df ) 
    }
    data_row_start <- grep(first_name, df[ ,1] ) + 1
    result <- df[ data_row_start:nrow( df ), ]
    result <- result[ rowSums( is.na( result ) ) != ncol( result ), ]
    names( result ) <- df[ data_row_start - 1, ]
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
# Dependencies: findDataStart, expand.grid, common_data.r
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
    source( paste( PARAM_DIR, "common_data.r", sep = "" ) )
    
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
        
        # Create list of final names, including all Xyears from common_data.r
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
        
        # Create list of final names, including all Xyears from common_data.r
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
