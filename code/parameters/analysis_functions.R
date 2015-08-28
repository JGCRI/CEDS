# ----------------------------------------------------------------------------------
# CEDS R header file: data analysis functions
# Authors: Ben Bond-Lamberty, Jon Seibert, Tyler Pitkanen
# Last Updated: 22 June 2015

# This file should be sourced by any R script running diagnostics on CEDS data.
# Functions contained:
#   activityCheck, sectorCheck, fuelCheck, CountryCheck

# ----------------------------------------------------------------------------------
# activityCheck 
# Brief:         Checks whether all the activities in the given dataset are in the 
# 		         master list, and whether all activities in the master list are present 
#		         in the data. 
# Details:       Uses printlog functions to print out detailed results: must be called 
#                while the log is running.
# Dependencies:  IO_functions.R
# Author(s):     Tyler Pitkanen, Jon Seibert  
# Params: 
#   data:        data frame to check, containing activity column [required]
#   colname:     name of the column containing activity designations [default: "activity"]
#   check_valid: boolean to control whether the function checks that all present activities
#                are valid [default: TRUE]
#   check_all:   boolean to control whether the function checks if all activities are
#                present [default: TRUE]
# Return:        boolean indicating pass or failure on all checks run
# Input Files:   none
# Output Files:  none

activityCheck <- function( x, colname = "activity", check_valid = TRUE, check_all = TRUE ){

	activity_list <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", 
                                sheet_selection = "Sectors", mute = TRUE )

    valid <- TRUE
    
    sec_col <- x[ , names( x ) == colname ]
    
    # Check that all activities in set x exist in the master activity list. Ignore blank
    #   spaces / NA values
    if( check_valid ){
        
        printLog( "Checking activity validity ", cr=F )
    
        invalid_activities <- list()
        n <- 1
        for( i in 1:length( sec_col ) ) {
            if( sec_col[[ i ]] %!in% activity_list$activity & !is.na( sec_col[[ i ]] ) ) {
                invalid_activities[[ n ]] <- sec_col[[ i ]]
                n <- n + 1
            }
        }
        invalid_activities <- unique( invalid_activities )
        if( length( invalid_activities ) > 0 ) {
            printLog( "...Invalid activities found:", paste( invalid_activities, collapse=', ' ), ts=F )
            valid <- FALSE
        } else {
            printLog( "...OK. All activities valid.", ts=F )
        }
    }

    # Check that all activities in the master list are found in data set x.
    if( check_all ){
        printLog( "Checking for missing activities ", cr=F )

        missing_activities <- list()
        n <- 1
        for( i in 1:length( activity_list$activity ) ) {
            target <- activity_list$activity[[ i ]]
            found <- grep( target, sec_col, fixed=T, value=T )
            if( length( found ) == 0 ) {
                missing_activities[[ n ]] <- target
                n <- n + 1
            }
        }
        missing_activities <- unique( missing_activities )
        missing_activities <- missing_activities[ !is.na( missing_activities ) ]
        if( length( missing_activities ) > 0 ) {
            printLog( "...activities missing:", paste( missing_activities, collapse=', ' ), ts=F )
            valid <- FALSE
        } else {
            printLog( "...OK. All activities present.", ts=F )
        }
    }
    
    return( valid )
}

# ----------------------------------------------------------------------------------
# sectorCheck 
# Brief:         Checks whether all the sectors in the given dataset are in the 
# 		         master list, and whether all sectors in the master list are present 
#		         in the data. 
# Details:       Uses printlog functions to print out detailed results: must be called 
#                while the log is running.
# Dependencies:  IO_functions.R
# Author(s):     Tyler Pitkanen, Jon Seibert  
# Params: 
#   data:        data frame to check, containing sector column [required]
#   colname:     name of the column containing sector designations [default: "sector"]
#   check_valid: boolean to control whether the function checks that all present sectors
#                are valid [default: TRUE]
#   check_all:   boolean to control whether the function checks if all sectors are
#                present [default: TRUE]
# Return:        boolean indicating pass or failure on all checks run
# Input Files:   none
# Output Files:  none

sectorCheck <- function( x, colname = "sector", check_valid = TRUE, check_all = TRUE ){

	sector_list <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", 
                              sheet_selection = "Sectors", mute = TRUE )

    valid <- TRUE
    
    sec_col <- x[ , names( x ) == colname ]
    
    # Check that all sectors in set x exist in the master sector list. Ignore blank
    #   spaces / NA values
    if( check_valid ){
        
        printLog( "Checking sector validity ", cr=F )
    
        invalid_sectors <- list()
        n <- 1
        for( i in 1:length( sec_col ) ) {
            if( sec_col[[ i ]] %!in% sector_list$sector & !is.na( sec_col[[ i ]] ) ) {
                invalid_sectors[[ n ]] <- sec_col[[ i ]]
                n <- n + 1
            }
        }
        invalid_sectors <- unique( invalid_sectors )
        if( length( invalid_sectors ) > 0 ) {
            printLog( "...Invalid sectors found:", paste( invalid_sectors, collapse=', ' ), ts=F )
            valid <- FALSE
        } else {
            printLog( "...OK. All sectors valid.", ts=F )
        }
    }

    # Check that all sectors in the master list are found in data set x.
    if( check_all ){
        printLog( "Checking for missing sectors ", cr=F )

        missing_sectors <- list()
        n <- 1
        for( i in 1:length( sector_list$sector ) ) {
            target <- sector_list$sector[[ i ]]
            found <- grep( target, sec_col, fixed=T, value=T )
            if( length( found ) == 0 ) {
                missing_sectors[[ n ]] <- target
                n <- n + 1
            }
        }
        missing_sectors <- unique( missing_sectors )
        missing_sectors <- missing_sectors[ !is.na( missing_sectors ) ]
        if( length( missing_sectors ) > 0 ) {
            printLog( "...Sectors missing:", paste( missing_sectors, collapse=', ' ), ts=F )
            valid <- FALSE
        } else {
            printLog( "...OK. All sectors present.", ts=F )
        }
    }
    
    return( valid )
}

# ---------------------------------------------------------------------------------
# fuelCheck: Checks whether all the fuels in the given dataset are in the 
# 		  master list, and whether all fuels in the master list are present in 
#	  	  the data. Must be called while the log is running.
# Params: data (dataset to check, containing fuel column)

fuelCheck <- function( x ){

	fuel_list <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", 
                            sheet_selection = "Fuels", mute = TRUE )

# Check that all fuels in set x exist in the master fuel list. Ignore blank
#   spaces / NA values
	printLog( "Checking fuel validity ", cr=F )
    
    invalid_fuels <- list()
    n <- 1
	for( i in 1:length( x$fuel ) ) {
		if( x$fuel[[i]] %!in% fuel_list$fuel & !is.na( x$fuel[[i]] ) ) {
			invalid_fuels[[n]] <- x$fuel[[i]]
            n <- n + 1
		}
	}
    invalid_fuels <- unique( invalid_fuels )
    if( length( invalid_fuels ) > 0 ) {
        printLog( "...Invalid fuels found:", paste( invalid_fuels, 
            collapse=', ' ), ts=F )
    } else {
        printLog( "...OK. All fuels valid.", ts=F )
    }

# Check that all fuels in the master list are found in data set x.
	printLog( "Checking for missing fuels ", cr=F )

	missing <- 0
    missing_fuels <- list()
    n <- 1
	for( i in 1:length( fuel_list$fuel ) ) {
		target <- fuel_list$fuel[[i]]
        found <- grep( target, x$fuel, fixed=T, value=T )
		if( length( found ) == 0 ) {
			missing_fuels[[n]] <- target
			n <- n + 1
		}
	}
    missing_fuels <- unique( missing_fuels )
    missing_fuels <- missing_fuels[ !is.na( missing_fuels ) ]
    if( length( missing_fuels ) > 0 ) {
		printLog( "...Fuels missing:", paste( missing_fuels, 
        collapse=', ' ), ts=F )
	} else {
        printLog( "...OK. All fuels present.", ts=F )
    }
}


# ---------------------------------------------------------------------------------
# countryCheck: Checks whether all the countries in the given dataset are in the 
# 		  master list, and whether all countries in the master list are present in 
#	  	  the data. Must be called while the log is running.
# Params: data (dataset to check, containing fuel column)

countryCheck <- function( data, cols = 1, convention = "ISO" ) {
# Generally, there will be one region column per data set and it will be the first
#   column in that data set, so cols = 1 by default. 
# Naming convention can be specified with the convention argument set as "ISO", 
#   "IEA", "IEA_Fert", or "BP" (not case-sensitive).

	country_list <- readData( "MAPPINGS", "Master_Country_List", meta=F, mute=T )

    conv_list    <- c( "ISO", "IEA", "IEA_Fert", "BP" )
    conv_col_num <- match( convention, conv_list )
    
# Choose sections of reference country list to search based on user input
    if ( is.na( conv_col_num ) == T ) {  # if convention is invalid or unspecified
        ref_names <- unlist( country_list )
        printLog( "Country check failed. Specify naming convention." )
    } else {  # if a valid convention is specified
        ref_names <- country_list[ , conv_col_num ]  
    }
    
    printLog( "Checking col(s)", cols, "for", convention, "names", cr=F )
    
# Check for name matching between data and reference section
    unmatched_names <- list()
    n = 1
    for ( i in 1:length( data[ , cols ] ) ) {
        check <- grep( data[ i, cols ], ref_names, value = T, fixed = T )
        if ( length( check ) == 0 ) {
            unmatched_names[n] <- data[ i, cols ]
            n = n + 1
        }
    }
    
# Print output to log
    if ( length( unmatched_names ) > 0 ) {
        unmatched_string <- paste( unmatched_names, collapse = ", " ) 
        printLog( "...Invalid names found for ", 
            convention, " naming: ", paste( unmatched_string, collapse = ', ' ), ts=F )
    } else {
        printLog( "...OK. All countries valid.", ts=F )
    }   
}
