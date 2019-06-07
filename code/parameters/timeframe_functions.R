# ----------------------------------------------------------------------------------
# CEDS R header file: timeframe functions
# Authors: Jon Seibert
# Last Updated: 22 June 2015

# This file should be sourced by any R script performing temporal reformatting, truncation, 
# extension, or interpolation of CEDS data.
# Functions contained:
#   isXYear, getEndYear, xYearToNum. getName. getIndex, getForwardExtensionRange,
#   getBackwardExtensionRange, getInterpolationRange, truncateAtYear, extendForward,
#   extendBackward, interpolate

# Notes: Required by activity_db_functions.R

# ----------------------------------------------------------------------------------
# Helper functions for addToActivityDb: can be used as standalone

# isXYear: returns a boolean indicating whether the parameter is an xYear.
isXYear <- function( yr ){
    if( is.numeric( yr ) ){ return( FALSE ) }
    first <- substr( yr, 1, 1 )
    rest <- substr( yr, 2, 5 )
    if( ( first == "X" || first == "x" ) && !is.na( as.numeric( rest ) ) ){ 
        return( TRUE ) 
    } else{ return( FALSE ) }
}

# getEndYear: Retrieve ending year for a data frame
getEndYear <- function( df ){ return( tail( names( df ) , 1 ) ) }

# xYearToNum: Convert a string Xyear (ex. X1990) to a numeric year
xYearToNum <- function( x ){ as.numeric( substr( x, 2, 5 ) ) }

# getName: Retrieve the column name of a column number
getName <- function( df, i ){ return( names( df )[[ i ]] ) }

# getIndex: Retrieve the column number of a column name
getIndex <- function( df, name ){
    for( i in 1:length( df ) ){
        if( names( df )[ i ] == name ){ return ( i ) }
    }
}

# The following functions list out the years to which the data must be extended.
# They accept numerical years, strings of years, and strings of xYears.
# They return the list in the same format as the inputs are recieved.
# Note: they are kept as separate functions for ease of use and understanding.
getForwardExtensionRange <- function( st_yr, end_yr) { 
    xYear <- FALSE
    char <- FALSE
    if( is.character( st_yr ) ){ 
        if( isXYear( st_yr ) ){ 
            st_yr <- xYearToNum( st_yr ) 
            xYear <- TRUE
        }
        else{ 
            st_yr <- as.numeric( st_yr )  
            char <- TRUE
        }
    }
    if( is.character( end_yr ) ){ 
        if( isXYear( end_yr ) ){ 
            end_yr <- xYearToNum( end_yr ) 
            xYear <- TRUE
        }
        else{ 
            end_yr <- as.numeric( end_yr )
            char <- TRUE
        }
    }
    results <- seq( st_yr + 1,end_yr )
    
    if( char ){ results <- as.character( results ) }
    if( xYear ){ results <- paste0( "X", as.character( results ) ) }
    
    return( results ) 
}

getBackwardExtensionRange <- function( end_yr, st_yr ){
    xYear <- FALSE
    char <- FALSE
    if( is.character( st_yr ) ){ 
        if( isXYear( st_yr ) ){ 
            st_yr <- xYearToNum( st_yr ) 
            xYear <- TRUE
        }
        else{ 
            st_yr <- as.numeric( st_yr )  
            char <- TRUE
        }
    }
    if( is.character( end_yr ) ){ 
        if( isXYear( end_yr ) ){ 
            end_yr <- xYearToNum( end_yr ) 
            xYear <- TRUE
        }
        else{ 
            end_yr <- as.numeric( end_yr )
            char <- TRUE
        }
    }
	results <- seq( st_yr,end_yr - 1 )
    
    if( char ){ results <- as.character( results ) }
    if( xYear ){ results <- paste0( "X", as.character( results ) ) }
    
    return( results ) 
}

getInterpolationRange <- function( early_edge, late_edge ){
    xYear <- FALSE
    char <- FALSE
    if( is.character( early_edge ) ){ 
        if( isXYear( early_edge ) ){ 
            early_edge <- xYearToNum( early_edge ) 
            xYear <- TRUE
        }
        else{ 
            early_edge <- as.numeric( early_edge )  
            char <- TRUE
        }
    }
    if( is.character( late_edge ) ){ 
        if( isXYear( late_edge ) ){ 
            late_edge <- xYearToNum( late_edge ) 
            xYear <- TRUE
        }
        else{ 
            late_edge <- as.numeric( late_edge )
            char <- TRUE
        }
    }
    results <- seq( early_edge + 1, late_edge - 1 )
    
    if( char ){ results <- as.character( results ) }
    if( xYear ){ results <- paste0( "X", as.character( results ) ) }
    
    return( results ) 
}

# truncateAtYear: Truncate data to use common starting year as earliest and common end year as latest.
# Cutoffs dependent on input parameters. Returns a truncateAtYeard data frame.
# Relies upon common_data.r values as defaults.
truncateAtYear <- function( df,st_yr = X_start_year,end_yr = X_end_year ){
    if( st_yr %in% names( df ) ){
        st_index <- match( st_yr,names( df ) )
    }else{
        i <- 1
        done <- FALSE
        while( !done ){
            name <- names( df )[[ i ]]
            if( !( is.na( as.numeric( substr( name, 2, 5 ) ) ) ) ){
                st_index <- i
                done <- TRUE
            }
            i <- i + 1
        }
    }
    if( end_yr %in% names( df ) ){
        end_index <- match( end_yr,names( df ) )
    }else{
        end_index <- length( df )
    }
    data_start <- findDataStart( df )
    range <- c( names( df )[ 1:( data_start - 1 ) ], names( df )[ st_index:end_index ] )
    return( df[ range ] )
}

# extendForward: Extension function (constant)
extendForward <- function( df, end, range ){
    for( j in 1:length( range ) ){
        df <- cbind( df,df[ end ] )
        names( df )[[ length( df ) ]] <- range[[ j ]]
    }
    return( df )
}

# extendBackward: Backwards extension function (constant)
extendBackward <- function( df, st_year, range ){
    data_start <- findDataStart( df )
    for( j in 1:length( range ) ){
        df <- cbind( cbind( df[ 1:( data_start + j - 2 ) ], df[ st_year ] ), 
            df[ ( data_start + j - 1 ):length( df ) ] )
        names( df )[[ data_start + j - 1 ]] <- range[[ j ]]
    }
    return(df)
}

# interpolate: Fills in missing years between two given years. 
# Default fill-in values are the average of the values of the two edge years.

# CURRENTLY BROKEN. WILL FIX TOMORROW.
interpolate <- function( df, early_edge, late_edge, method = "linear" ){ 
    range <- getInterpolationRange( early_edge, late_edge )
    #if( isXYear( early_edge ) ){ range <- paste0( "X", range ) }
    
    early <- getIndex( df, early_edge )
    late <- getIndex( df, late_edge )
    
    if( method == "average" ){  
        value <- ( ( df[ early ] + df[ late ] ) / 2 )
        
        for( column in range ){
            df <- cbind( cbind( df[ 1:early ], value ), df[ late:length( df ) ] )
            names( df )[ early + 1 ] <- column
            early <- early + 1
            late <- late + 1
        }
    }
    if( method == "linear" ){
        increment <- ( df[ late ] - df[ early ] ) / ( length( range ) + 1 )
        value <- df[ early ] + increment
    
        for( column in range ){
            df <- cbind( cbind( df[ 1:early ], value ), df[ late:length( df ) ] )
            names( df )[ early + 1 ] <- column
            early <- early + 1
            late <- late + 1
            value <- value + increment
        }
    }
    
    return( df )
}
