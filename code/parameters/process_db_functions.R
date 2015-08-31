# ----------------------------------------------------------------------------------
# CEDS R header file: database functions
# Authors: Jon Seibert
# Last Updated: 22 June 2015

# This file should be sourced by any R script altering the CEDS databases
# on activity, emissions, or emissions factors data.
# Functions contained:
#   cleanData, addToActivityDb, addToEmissionsDb, addToEFDb

# Note: These functions require the functions in IO_functions.R, timeframe_functions.R,
#       and data_functions.R
# -------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------
# cleanData
# Brief:              Removes unneccessary columns, adds "X" to year names, and adds iso column.
# Details:            Uses removeBlanks function to delete blank or non-data rows, sets columns
#                       with numerical year names to Xyears, and replaces the "Coutnry" column
#                       with an iso code column if necessary. removeBlanks and Xyear conversion 
#                       have options to disable if not needed- iso conversion has its own checks.
# Dependencies:       common_data.R, findDataStart, readData, writeData, removeBlanks
# Author:             Jon Seibert
# Parameters:    
#     df:             Data frame to be cleaned [required]
#     remove_blanks:  Boolean option to call removeBlanks on df [default: TRUE]
#     first:          Name of the first column, used with removeBlanks [default: "Country"]
#     user_cols:      List of additional column names the user wishes to keep in the data:
#                       Country, iso, sector, fuel, activity, and units are excused by default.
#                       [default: NULL]
#     x_years:        Boolean option to add "X" to all [default: TRUE]
# Return:             Cleaned data frame
# Input files:        2011_NC_SO2_ctry.csv
# Output files:       none

cleanData <- function( df, remove_blanks = TRUE, first = "Country", user_cols = NULL, x_years = TRUE ){
    
    # Requires iso-Country mapping file
    iso_mapping <- readData( "MAPPINGS","2011_NC_SO2_ctry", mute = TRUE )

    # Apply header-reformatting function from CEDS_header to remove non-data rows:
    # We know for this particular input file that the row with the column headings 
    # starts with "Country".
    if( remove_blanks ){
        df <- removeBlanks( df, first_name = first )
    }

    id_cols <- c("Country", "iso", "activity", "sector", "fuel", "units" )
    keep_cols <- c( id_cols, user_cols )
    
    # Remove unnecessary columns (not emissions data or country label)
    for( col in seq( length( names( df ) ) , 1 , -1 ) ){
        name <- names( df )[[ col ]]
        numeric_name <- as.numeric( name )
    
        if( is.na( name ) | ( is.na( numeric_name ) && name %!in% keep_cols ) ){
            df[[ col ]] <- NULL
        }    
    }
    
    len <- length( df )
    data_start <- findDataStart( df ) # IS ERRORING BECAUSE NO X'S- DIDN'T I FIX THAT?!?!?

    # Add "X" to all year names
    if( x_years ){
        data_years <- names( df )[ data_start:len ]
        non_years <- names( df )[ 1:( data_start - 1 ) ]
        x_data_years <- paste0( "X",data_years )
        names( df ) <- c( non_years,x_data_years )
    }
    
    # Add column of iso codes mapped to each country, if necessary
    if( !( "iso" %in% names( df ) ) ){
        df$iso <- as.character( iso_mapping$iso[ 
            match( df$Country, iso_mapping$NC_ctry ) ] )
    }
    # Re-order columns such that iso comes first, and remove Country,
    # along with any rows that do not have an iso code (ex. OTHERAFRIC)
    len <- length( df )
    data_start <- findDataStart( df )
    df <- cbind( iso = df$iso, df[ data_start:( len - 1 ) ] )
    df <- subset( df, !( is.na( iso ) ) )
    
    # Cast iso column to characters instead of factors, for later sorting
    df$iso <- as.character( df$iso )
    
    return( df )
}

# ----------------------------------------------------------------------------------
# addToActivityDb
# Brief:            Adds data to the process activity database
# Details:          Takes a data frame as input and overwrites the existing entries in the
#                       activity database where necessary, only adding on if the iso-activity
#                       combination being examined is not already present in the database.
# Dependencies:     common_data.R, IO_functions.R, data_functions.R, timeframe_functions.R
# Author:           Jon Seibert
# Parameters: 
# 	df: 		    data frame containing data to be added to the activity database
#                       ( df must have form iso-activity-units-years ) [required]
#	ext_forward:    boolean indicating whether to extendForward the new data forward to the
#                       common end year, if missing more recent years [default: TRUE]
#   ext_backward:   boolean indicating whether to extendForward the new data backward to the
#                       common start year, if missing starting years [default: FALSE]
# Return:           none
# Input files:      A.NC_activity_db.csv, common_data.r
# Output files:     A.NC_activity_db.csv
# 
# Usage examples: addToActivityDb( df, ext_forward = FALSE, ext_backward = FALSE )
addToActivityDb <- function( df, ext_forward = TRUE, ext_backward = FALSE ){

    # Read in necessary files and data: common_data.R required 
    # to avoid variable overwrite carryover
    source( paste( PARAM_DIR, "common_data.R", sep = "" ) )
    activity_db <- readData( "MED_OUT", "A.NC_activity_db", meta = FALSE )
    
    # Rebind values from common_data.R
    X_start <- X_start_year
    X_end <- X_end_year
    ext_start_year <- start_year
    ext_end_year <- end_year
    
    w <- getOption( "warn" )
    options( warn=-1 )  # suppress the warnings about NAs introduced by coercion
    db_start <- findDataStart( activity_db )
    data_start <- findDataStart( df )
    
    if( data_start != db_start ){ # If in incorrect format
        printLog( paste0 ( "Error in addToActivityDb: ",
                  "Data to be added must have the form iso-activity-units-years." ) )
        return( 0 )
    }
    
    # Check whether the new data has the same set of years as the database.
    # If not, make it so via truncation and/or extension as necessary.
    if( ( FALSE %in% ( names( activity_db ) == names( df ) ) ) 
        || ( length( activity_db ) != length( df ) ) ){
        
        df <- truncateAtYear( df, X_start, X_end )
        
        options( warn=w )
        
        # Retrieve new starting and ending years
        start_year <- getName( df, data_start )
        cur_start_year <- xYearToNum( start_year )
        end_year <- getEndYear( df )
        cur_end_year <- xYearToNum( end_year )
        
        # If the data now has the correct years after truncation,
        # or if the missing years are within the normal range and
        # not at either end, do not attempt to extendForward the data.
        # Doing so results in incorrect years.
        if( start_year == X_start ){ ext_backward <- FALSE }
        if( end_year == X_end ){ ext_forward <- FALSE }
        
        # Extend the data forward if necessary and the parameter is TRUE
        if( ext_forward ){
            ext_range <- getForwardExtensionRange( cur_end_year, ext_end_year )
            X_range <- paste0( "X",ext_range )
            df <- extendForward( df, end_year, X_range )
        }
        # Extend the data backward if necessary and the parameter is TRUE
        if( ext_backward ){
            past_ext_range <- getBackwardExtensionRange( cur_start_year, ext_start_year )
            past_X_range <- paste0( "X",past_ext_range )
            df <- extendForward( df, start_year, past_X_range )
        }
    }
    
    options( warn=w )

    # Insert the new data in the proper locations- 
    # overwrite any existing data of the same combination and years,
    # and rbind on any data without a spot.
    results <- activity_db
    iso_list <- unique( df$iso )
    data_cols <- names( df )[ data_start:length( df ) ]
    
    for( country in iso_list ){
        df_set <- subset( df, df$iso == country )
        res_set <- subset( results, results$iso == country )
        act_list <- unique( df_set$activity )
        
        for( activity_name in act_list ){
            data <- subset( df_set, df_set$activity == activity_name )
            
            # If the iso-activity combo exists in the database already, overwrite it.
            # If it does not, add it.
            if( nrow( res_set[ res_set$activity == activity_name, data_cols ] ) > 0 ){
                results[ results$iso == country & results$activity == activity_name, data_cols ] <- data[ data_cols ]
            }else{
                # Use blank row proxy to avoid column mismatch errors:
                # May not have been extendForwarded to match column names exactly.
                blank <- results[ 1, ]
                blank [ data_start:length( blank ) ] <- 0
                blank [ 1,"iso" ] <- data[ 1,"iso" ]
                blank [ 1,"activity" ] <- data[ 1,"activity" ]
                blank [ 1,"units" ] <- data[ 1,"units" ]
                
                blank [ data_cols ] <- data[ data_cols ]
                
                results <- rbind( results, blank )  
            }
        }
    }
    
    # Sort
    results <- results[ with( results, order( iso, activity ) ), ]
    
    # Output
    writeData( results, domain = "MED_OUT", fn = "A.NC_activity_db", meta = FALSE )
}

# ----------------------------------------------------------------------------------
# addToEmissionsDb
# Brief:            Adds data to the emissions database
# Details:          Takes a data frame as input and overwrites the existing entries in the
#                       emissions database where necessary, only adding on if the iso-sector-fuel
#                       combination being examined is not already present in the database.
# Dependencies:     common_data.r, IO_functions.R, data_functions.R, timeframe_functions.R
# Author:           Jon Seibert
# Parameters: 
# 	df: 		    data frame containing data to be added to the emissions database
#                       ( df must have form iso-sector-fuel-units-years ) [required]
#   em:             emissions species (ex. SO2, CO2, BC) [required]
#   type:           either "NC" or "comb" for process (non-combustion) or combustion emissions
#                   [required]
#	ext_forward:    boolean indicating whether to extendForward the new data forward to the
#                       common end year, if missing more recent years [default: TRUE]
#   ext_backward:   boolean indicating whether to extendForward the new data backward to the
#                       common start year, if missing starting years [default: FALSE]
# Return:           none
# Input files:      C.[em]_[type]_emissions_db.csv, common_data.r
# Output files:     C.[em]_[type]_emissions_db.csv
# 
# Usage examples: addToEmissionsDb( df, ext_forward = FALSE, ext_backward = FALSE )
addToEmissionsDb <- function( df, em, type, ext_forward = TRUE, ext_backward = FALSE ){

    # # DEBUG
    # df <- data_list[[2]]
    # em <- "SO2"
    # type <- "NC"
    # ext_forward <- TRUE
    # ext_backward <- FALSE

    # Read in necessary files and data: common_data.r required 
    # to avoid variable overwrite carryover
    source( paste( PARAM_DIR, "common_data.r", sep = "" ) )
    emissions_db <- readData( "MED_OUT", paste0( "C.", em, "_", type, "_emissions_db" ), meta = FALSE )
    
    # Rebind values from common_data.r
    X_start <- X_start_year
    X_end <- X_end_year
    ext_start_year <- start_year
    ext_end_year <- end_year
    
    w <- getOption( "warn" )
    options( warn=-1 )  # suppress the warnings about NAs introduced by coercion
    db_start <- findDataStart( emissions_db )
    data_start <- findDataStart( df )
    
    if( data_start != db_start ){ # If in incorrect format
        printLog( paste0 ( "Error in addToEmissionsDb: ",
                  "Data to be added must have the form iso-sector-fuel-units-years." ) )
        return( 0 )
    }
    
    # Check whether the new data has the same set of years as the database.
    # If not, make it so via truncation and/or extension as necessary.
    if( ( FALSE %in% ( names( emissions_db ) == names( df ) ) ) 
        || ( length( emissions_db ) != length( df ) ) ){
        
        df <- truncateAtYear( df, X_start, X_end )
        
        options( warn=w )
        
        # Retrieve new starting and ending years
        start_year <- getName( df, data_start )
        cur_start_year <- xYearToNum( start_year )
        end_year <- getEndYear( df )
        cur_end_year <- xYearToNum( end_year )
        
        # If the data now has the correct years after truncation,
        # or if the missing years are within the normal range and
        # not at either end, do not attempt to extendForward the data.
        # Doing so results in incorrect years.
        if( start_year == X_start ){ ext_backward <- FALSE }
        if( end_year == X_end ){ ext_forward <- FALSE }
        
        # Extend the data forward if necessary and the parameter is TRUE
        if( ext_forward ){
            ext_range <- getForwardExtensionRange( cur_end_year, ext_end_year )
            X_range <- paste0( "X",ext_range )
            df <- extendForward( df, end_year, X_range )
        }
        # Extend the data backward if necessary and the parameter is TRUE
        if( ext_backward ){
            past_ext_range <- getBackwardExtensionRange( cur_start_year, ext_start_year )
            past_X_range <- paste0( "X",past_ext_range )
            df <- extendForward( df, start_year, past_X_range )
        }
    }
    
    options( warn=w )

    # Insert the new data in the proper locations- 
    # overwrite any existing data of the same combination and years,
    # and rbind on any data without a spot.
    results <- emissions_db
    iso_list <- unique( df$iso )
    data_cols <- names( df )[ data_start:length( df ) ]
    
    for( country in iso_list ){
        df_iso <- subset( df, df$iso == country )
        res_set <- subset( results, results$iso == country )
        sect_list <- unique( df_iso$sector )
        
        for( sector_name in sect_list ){
            df_sect <- subset( df_iso, df_iso$sector == sector_name )
            res_set <- subset( res_set, res_set$sector == sector_name )
            fuel_list <- unique( df_sect$fuel )
            
            for( f in fuel_list ){
                data <- subset( df_sect, df_sect$fuel == f )
            
                # If the iso-sector-fuel combo exists in the database already, overwrite it.
                # If it does not, add it.
                if( nrow( res_set[ res_set$fuel == f, data_cols ] ) > 0 ){
                    results[ results$iso == country & results$sector == sector_name & results$fuel == f, data_cols ] <- data[ data_cols ]
                }else{
                    # Use blank row proxy to avoid column mismatch errors:
                    # May not have been extendForwarded to match column names exactly.
                    blank <- results[ 1, ]
                    blank [ data_start:length( blank ) ] <- 0
                    blank [ 1,"iso" ] <- data[ 1,"iso" ]
                    blank [ 1,"sector" ] <- data[ 1,"sector" ]
                    blank [ 1,"fuel" ] <- data[ 1,"fuel" ]
                    blank [ 1,"units" ] <- data[ 1,"units" ]
                    
                    blank [ data_cols ] <- data[ data_cols ]
                    
                    results <- rbind( results, blank )  
                }
            }
        }
    }
    
    # Sort
    results <- results[ with( results, order( iso, sector, fuel ) ), ]
    
    # Output
    writeData( results, domain = "MED_OUT", fn = paste0( "C.", em, "_", type, "_emissions_db" ), meta = FALSE )
}

# ----------------------------------------------------------------------------------
# addToEFDb
# Brief:            Adds data to the emissions factors database
# Details:          Takes a data frame as input and overwrites the existing entries in the
#                       emissions factors database where necessary, only adding on if the 
#                       iso-sector-fuel combination being examined is not already present 
#                       in the database.
# Dependencies:     common_data.r, IO_functions.R, data_functions.R, timeframe_functions.R
# Author:           Jon Seibert
# Parameters: 
# 	df: 		    data frame containing data to be added to the emissions database
#                       ( df must have form iso-sector-fuel-units-years ) [required]
#   em:             emissions species (ex. SO2, CO2, BC) [required]
#   type:           either "NC" or "comb" for process (non-combustion) or combustion emissions
#                   [required]
#	ext_forward:    boolean indicating whether to extendForward the new data forward to the
#                       common end year, if missing more recent years [default: TRUE]
#   ext_backward:   boolean indicating whether to extendForward the new data backward to the
#                       common start year, if missing starting years [default: FALSE]
# Return:           none
# Input files:      C.[em]_[type]_EF_db.csv, common_data.r
# Output files:     C.[em]_[type]_EF_db.csv
# 
# Usage examples: addToEFDb( data, "SO2", "NC", ext_forward = FALSE, ext_backward = FALSE )
addToEFDb <- function( df, em, type, ext_forward = TRUE, ext_backward = FALSE ){

    # Read in necessary files and data: common_data.r required 
    # to avoid variable overwrite carryover
    source( paste( PARAM_DIR, "common_data.r", sep = "" ) )
    EF_db <- readData( "MED_OUT", paste0( "C.", em, "_", type, "_EF_db" ), meta = FALSE )
    
    # Rebind values from common_data.r
    X_start <- X_start_year
    X_end <- X_end_year
    ext_start_year <- start_year
    ext_end_year <- end_year
    
    w <- getOption( "warn" )
    options( warn=-1 )  # suppress the warnings about NAs introduced by coercion
    db_start <- findDataStart( EF_db )
    data_start <- findDataStart( df )
    
    if( data_start != db_start ){ # If in incorrect format
        printLog( paste0 ( "Error in addToEFDb: ",
                  "Data to be added must have the form iso-sector-fuel-units-years." ) )
        return( 0 )
    }
    
    # Check whether the new data has the same set of years as the database.
    # If not, make it so via truncation and/or extension as necessary.
    if( ( FALSE %in% ( names( EF_db ) == names( df ) ) ) 
        || ( length( EF_db ) != length( df ) ) ){
        
        df <- truncateAtYear( df, X_start, X_end )
        
        options( warn=w )
        
        # Retrieve new starting and ending years
        start_year <- getName( df, data_start )
        cur_start_year <- xYearToNum( start_year )
        end_year <- getEndYear( df )
        cur_end_year <- xYearToNum( end_year )
        
        # If the data now has the correct years after truncation,
        # or if the missing years are within the normal range and
        # not at either end, do not attempt to extendForward the data.
        # Doing so results in incorrect years.
        if( start_year == X_start ){ ext_backward <- FALSE }
        if( end_year == X_end ){ ext_forward <- FALSE }
        
        # Extend the data forward if necessary and the parameter is TRUE
        if( ext_forward ){
            ext_range <- getForwardExtensionRange( cur_end_year, ext_end_year )
            X_range <- paste0( "X",ext_range )
            df <- extendForward( df, end_year, X_range )
        }
        # Extend the data backward if necessary and the parameter is TRUE
        if( ext_backward ){
            past_ext_range <- getBackwardExtensionRange( cur_start_year, ext_start_year )
            past_X_range <- paste0( "X",past_ext_range )
            df <- extendForward( df, start_year, past_X_range )
        }
    }
    
    options( warn=w )

    # Insert the new data in the proper locations- 
    # overwrite any existing data of the same combination and years,
    # and rbind on any data without a spot.
    results <- EF_db
    iso_list <- unique( df$iso )
    data_cols <- names( df )[ data_start:length( df ) ]
    
    for( country in iso_list ){
        df_iso <- subset( df, df$iso == country )
        res_set <- subset( results, results$iso == country )
        sect_list <- unique( df_iso$sector )
        
        for( sector_name in sect_list ){
            df_sect <- subset( df_iso, df_iso$sector == sector_name )
            res_set <- subset( res_set, res_set$sector == sector_name )
            fuel_list <- unique( df_sect$fuel )
            
            for( f in fuel_list ){
                data <- subset( df_sect, df_sect$fuel == f )
            
                # If the iso-sector-fuel combo exists in the database already, overwrite it.
                # If it does not, add it.
                if( nrow( res_set[ res_set$fuel == f, data_cols ] ) > 0 ){
                    results[ results$iso == country & results$sector == sector_name & results$fuel == f, data_cols ] <- data[ data_cols ]
                }else{
                    # Use blank row proxy to avoid column mismatch errors:
                    # May not have been extendForwarded to match column names exactly.
                    blank <- results[ 1, ]
                    blank [ data_start:length( blank ) ] <- 0
                    blank [ 1,"iso" ] <- data[ 1,"iso" ]
                    blank [ 1,"sector" ] <- data[ 1,"sector" ]
                    blank [ 1,"fuel" ] <- data[ 1,"fuel" ]
                    blank [ 1,"units" ] <- data[ 1,"units" ]
                    
                    blank [ data_cols ] <- data[ data_cols ]
                    
                    results <- rbind( results, blank )  
                }
            }
        }
    }
    
    # Sort
    results <- results[ with( results, order( iso, sector, fuel ) ), ]
    
    # Output
    writeData( results, domain = "MED_OUT", fn = paste0( "C.", em, "_", type, "_EF_db" ), meta = FALSE )
}
# ----------------------------------------------------------------------------------
