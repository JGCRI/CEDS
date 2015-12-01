# ----------------------------------------------------------------------------------
# CEDS R header file: database functions
# Authors: Jon Seibert, Rachel Hoesly
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
#  Add to Database functions
# ------------------------------------------------------------------------------

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
addToEmissionsDb<- function( df, em, type, ext_forward = TRUE, ext_backward = FALSE ){

    # # DEBUG
    # df <- data_list[[2]]
    # em <- "SO2"
    # type <- "NC"
    # ext_forward <- TRUE
    # ext_backward <- FALSE

    # Read in necessary files and data: common_data.r required 
    # to avoid variable overwrite carryover
    source( paste( PARAM_DIR, "common_data.R", sep = "" ) )
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

#(Temporary?) Rewrite of addToEmissionsDb function. Similar functionality. Overwrite old data with new data for 
# duplicate entries. Order of data to add: worst then best
addToEmissionsDb_overwrite <- function( df, em, type ){
  
  # Read in necessary files and data: common_data.r required 
  # to avoid variable overwrite carryover
  source( paste( PARAM_DIR, "common_data.R", sep = "" ) )
  source( paste( PARAM_DIR, "IO_functions.R", sep = "" ) )
  source( paste( PARAM_DIR, "timeframe_functions.R", sep = "" ) )
  source( paste( PARAM_DIR, "data_functions.R", sep = "" ) )
  
  
  emissions_db <- readData( "MED_OUT", paste0( "C.", em, "_", type, "_emissions_db" ), meta = FALSE )
  
  # Rebind values from common_data.R
  X_start <- X_start_year
  X_end <- X_end_year
  ext_start_year <- start_year
  ext_end_year <- end_year
  
  w <- getOption( "warn" )
  options( warn=-1 )  # suppress the warnings about NAs introduced by coercion
  db_start <- findDataStart( emissions_db )
  data_start <- findDataStart( df )
  
  if( data_start != db_start ){ # If in incorrect format
    printLog( paste0 ( "Error in addToEmissionsDb_overwrite: ",
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
  
  #---------
  
  # melt old and new dbs
  df_add<-melt(df,id=c("iso","sector","fuel","units"))
  names(df_add)[which(names(df_add)=='variable')]<-'year'
  names(df_add)[which(names(df_add)=='value')]<-'em_new'
  
  df_old<-melt(emissions_db,id=c("iso","sector","fuel","units"))
  names(df_old)[which(names(df_old)=='variable')]<-'year'
  names(df_old)[which(names(df_old)=='value')]<-'em'
  
  #merge
  df_new<-merge(df_old,df_add,
                by=c("iso","sector","fuel","units","year"),
                all.x=TRUE,all.y=TRUE)
  
  df_new[which(is.na(df_new$em_new)),'em_new']<-df_new[which(is.na(df_new$em_new)),'em']
  
  df_new<-df_new[,c("iso","sector","fuel","units","year",'em_new')]
  
  df_new_wide<-cast(df_new, iso+sector+fuel+units~year, value = "em_new")
  
  # Sort
  results<-df_new_wide
  results <- results[ with( results, order( iso, sector, fuel ) ), ]
  
  # Check for NAs
  if( na_error == 1){
    
    if( all(is.na(results[,X_emissions_years]) %in% FALSE) ){
      printLog("Checking NAs... No NA's in EF_db")} else  Stop("Checking NAs... NA's in EF_db. Check Code.")
    
  }
  
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
# Input files:      B.[em]_[type]_EF_db.csv, common_data.R
# Output files:     B.[em]_[type]_EF_db.csv
# 
# Usage examples: addToEFDb( data, "SO2", "NC", ext_forward = FALSE, ext_backward = FALSE )
addToEFDb <- function( df, em, type, ext_forward = TRUE, ext_backward = FALSE ){

    # Read in necessary files and data: common_data.r required 
    # to avoid variable overwrite carryover
    source( paste( PARAM_DIR, "common_data.R", sep = "" ) )
    source( paste( PARAM_DIR, "IO_functions.R", sep = "" ) )
    source( paste( PARAM_DIR, "timeframe_functions.R", sep = "" ) )
    source( paste( PARAM_DIR, "data_functions.R", sep = "" ) )
      
    EF_db <- readData( "MED_OUT", paste0( "B.", em, "_", type, "_EF_db" ), meta = FALSE )
    
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
    writeData( results, domain = "MED_OUT", fn = paste0( "B.", em, "_", type, "_EF_db" ), meta = FALSE )
}


#(Temporary?) Rewrite of addToEFDb function. Similar functionality. Overwrite old data with new data for 
# duplicate entries. Order of data to add: worst then best
# file_extention: file name of database. Text after B.em_ for example 'comb_EF_db' for combustion EFs

addToDb_overwrite <- function( new_data, em, type, file_extention ){
  printLog ("Adding new data to database")
  #   # for original_db
  #   file_extention = paste0(type, "_original_db" )
  #   #for SO2 parameters
  #   file_extention = paste0(type, "_original_db" )
  
  new_data = control_percent 
  em = 'SO2' 
  type = 'comb' 
  file_extention = 'ControlFrac_db'
  
  #   Read in necessary files and data: common_data.r required 
  #   to avoid variable overwrite carryover
  source( paste( PARAM_DIR, "common_data.R", sep = "" ) )
  source( paste( PARAM_DIR, "IO_functions.R", sep = "" ) )
  source( paste( PARAM_DIR, "timeframe_functions.R", sep = "" ) )
  source( paste( PARAM_DIR, "data_functions.R", sep = "" ) )
  
  original_db <- readData( "MED_OUT", paste0( "B.", em, "_", file_extention ) )
  n.observations <- nrow(original_db)
  
  # Check units
  original_units <- unique(original_db$units)
  new_units <- unique(new_data$units)
  if( any(original_units %!in% new_units) | any(new_units %!in% original_units) )
    stop('Units do not match in addToDb_overwrite. Please check units')
  
  # Check id variables
  names <- names( new_data )
  id.names.new <- names[-grep( "X", names)]
  names <- names( original_db )
  id.names.old <- names[-grep( "X", names)]
  
  if( !identical(id.names.new,id.names.old)){ stop("In addToDb_overwrite, id variables (iso, sector, fuel) 
  of database and new data to be added to database are not the same.")}
  if( !identical(unique(original_db$units), unique(original_db$units))){ stop("In addToDb_overwrite, units of database and 
                                                                              new data to be added to database are not the same.")}
  
  # Insert the new data in the proper locations- 
  # overwrite any existing data of the same combination and years,
  # and rbind on any data without a spot.
  # results <- original_db
  #---------
  
  # melt old and new dbs
  df_add<-melt(new_data,id=c("iso","sector","fuel","units"))
  names(df_add)[which(names(df_add)=='variable')]<-'year'
  names(df_add)[which(names(df_add)=='value')]<-'new_value'
  
  df_old<-melt(original_db,id=c("iso","sector","fuel","units"))
  names(df_old)[which(names(df_old)=='variable')]<-'year'
  names(df_old)[which(names(df_old)=='value')]<-'old_value'
  
  #merge
  printLog('Merging new and old Data')
  df_new<-merge(df_old,df_add,
                by=c("iso","sector","fuel","units","year"),
                all.x=TRUE,all.y=TRUE)
  df_new[ which(is.na(df_new$new_value)),'new_value'] <- df_new[which(is.na(df_new$new_value)),'old_value']
  df_new<-df_new[,c("iso","sector","fuel","units","year",'new_value')]
  df_new_wide<-as.data.frame(cast(df_new, iso+sector+fuel+units~year, value = "new_value"))
  
  # Sort
  results<-df_new_wide
  results <- results[ with( results, order( iso, sector, fuel ) ), ]
  
  # Check for NAs
  if( na_error == 1){
    if( all(is.na(results[,X_emissions_years]) %in% FALSE) ){
      printLog("Checking NAs... No NA's in original_db")} else  stop("Checking NAs... NA's in original_db. Check Code.")
  }
  
  # Check length
  if ( n.observations != nrow(results)) stop("Outbut Database is not the same length as original DB. Duplicates in addToDb_overwrite.")
  
  # Output
  writeData( results, domain = "MED_OUT", fn = paste0( "B.", em, "_", file_extention))
  }



# ----------------------------------------------------------------------------------
#  Interpolation, Extrapolation, Expansion (all iso, sectors, etc)
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
  if(any(!is.na(interp_method))) {
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
  if( any(!is.na(interp_method)) ){
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
    if( any(interp_method$interp_method %!in% interp_default) ){
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
  for( i in seq_along(interp_df$iso)){
    row <- as.numeric(interp_df[i,X_years_full])
    if( length(rle(is.na(c(NA,row,NA)))$values)>3 ) interpolation_rows<- rbind(interpolation_rows,i)
  }
  interpolation_rows <- as.vector(interpolation_rows)
  
  # Interpolate
  if( length(interpolation_rows)>0){
    linear <- interp_df[which(interp_method_full$interp_method == 'linear'),]
    if ( nrow(linear)>0){
      # Add Meta notes
      if (meta == TRUE) {
        meta_pre_add <- interp_df[interpolation_rows,]
        meta_pre_add <- merge(meta_pre_add, interp_method_full,
                              all.x=TRUE, all.y=FALSE)
        meta_pre_add <- meta_pre_add[which(meta_pre_add$interp_method == 'linear'),c(id.names,X_years_full)]
        
        meta_add <- c()
        for ( i in seq_along(meta_pre_add$iso)){
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
      
      linear_int <- t( na.approx( t(linear[,X_years_full])  ) )
      linear <- cbind( linear[,id.names] , linear_int)
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
        for ( i in seq_along(meta_pre_add$iso)){
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
      constant_int <- t( na.locf( t(constant[,X_years_full])  ) )
      constant <- cbind( constant[,id.names] , constant_int)
      names(constant) <- c(id.names , X_years_full ) }
    
    if( nrow(linear)>0 && nrow(constant)>0 ) interp_df <- rbind(linear, constant)  
    if( nrow(linear)>0 && !nrow(constant)>0 ) interp_df <- linear 
    if( !nrow(linear)>0 && nrow(constant)>0 ) interp_df <-constant
    
  } else interp_df <- interp_data
  
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
                         ext_year = NA) {
  # ext_method format
  # column names - id variables (iso, sector, fuels) - pre_ext_method - post_ext_method
  # ext_year format
  # column names - id variables (iso, sector, fuels) - pre_ext_year - post_ext_year
  
  # Define inventory variables
  names <- names( ext_data)
  id.names <- names[-grep( "X", names)]
  id.names <- id.names[id.names %!in% 'units']
  X_years <- names[grep( "X", names)]
  years <- as.numeric(sub("X", "", X_years))
  years_full <- min(years):max(years)
  X_years_full <- paste0('X', years_full)
  
  # Check input
  valid_pre_ext_methods  <- c('constant','linear_1', 'linear_0')
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
  if ( !any(is.na(ext_method)) ){
    if ( ! all( ext_method$pre_ext_method %in% c(valid_pre_ext_methods,'NA') )) {
      index <- which( ext_method$pre_ext_method %in% valid_pre_ext_methods == FALSE )
      warning( paste0(  ext_method$pre_ext_method[index] , ': invalid pre-extrapolation method. Using default option: ',
                        "'" ,pre_ext_default),"'" )
      ext_method$pre_ext_method[index] <- pre_ext_default } 
    
    if ( ! all( ext_method$post_ext_method %in% c(valid_post_ext_methods,'NA') )) {
      index <- which( ext_method$post_ext_method %in% valid_post_ext_methods == FALSE )
      warning( paste0(  ext_method$post_ext_method[index] , ': invalid post-extrapolation method. Using default option: ',
                        "'" ,post_ext_default,"'") )
      ext_method$post_ext_method[index] <- post_ext_default } }
  
  # Check Years map and replace with default if invalid 
  ###########
  ###########
  ###########
  
  # Define Default Methods Data frame, update with mapping file
  # defaults
  ext_method_full <- ext_data[,id.names]
  ext_method_full[,'pre_ext_method'] <- pre_ext_default
  ext_method_full[,'post_ext_method'] <- post_ext_default
  # update with method file
  
  # Replace "all" notation in ext_methods with unique iso/sector/fuel names
  if( any(!is.na(ext_method)) && 
      (any(ext_method$pre_ext_method %!in% pre_ext_default )  || any(ext_method$post_ext_method %!in% post_ext_default )) ){
    for (n in seq_along(id.names) ){ 
      name <- id.names[n]
      other.name <- id.names[id.names %!in% name]
      unique <- unique(ext_method_full[,name]) 
      replace <- ext_method[which(ext_method[,name]=='all'),]
      while (nrow(replace)>0){
        replace.index <- min(which(ext_method[,name]=='all'))
        replace.row <- ext_method[replace.index,]  
        ext_method <- ext_method[-replace.index,]  
        add <- data.frame(replace = unique)
        names(add) <- name
        add[, c(other.name,'pre_ext_method','post_ext_method') ] <- replace.row[, c(other.name,'pre_ext_method','post_ext_method') ]
        ext_method <- rbind(ext_method, add)
        replace <- ext_method[which(ext_method[,name]=='all'),]  
      }     }
    
    # replace default ext_methods with ext_method
    names(ext_method)[names(ext_method) %in% c('pre_ext_method','post_ext_method')] <- c('pre_ext_method_new','post_ext_method_new')
    new_method <- merge(ext_method_full,ext_method, all.x=TRUE, all.y = FALSE)
    ext_method_full[which(!is.na(new_method$pre_ext_method_new)) ,'pre_ext_method'] <-new_method[which(!is.na(new_method$pre_ext_method_new)) ,'pre_ext_method_new']
    ext_method_full[which(!is.na(new_method$post_ext_method_new)) ,'post_ext_method'] <-new_method[which(!is.na(new_method$post_ext_method_new)) ,'post_ext_method_new']
  }
  
  # Define Default Years Data frame, update with mapping file
  # defaults
  ext_year_full <- ext_data[,id.names]
  ext_year_full[,'pre_ext_year'] <- pre_ext_year
  ext_year_full[,'post_ext_year'] <- post_ext_year
  # update with year file
  
  # Replace "all" notation in ext_years with unique iso/sector/fuel names
  if( any(!is.na(ext_year)) && 
      (any(ext_year$pre_ext_year %!in% pre_ext_year )  || any(ext_year$post_ext_year %!in% post_ext_year ) )){
    for (n in seq_along(id.names) ){ 
      name <- id.names[n]
      other.name <- id.names[id.names %!in% name]
      unique <- unique(ext_year_full[,name]) 
      replace <- ext_year[which(ext_year[,name]=='all'),]
      while (nrow(replace)>0){
        replace.index <- min(which(ext_year[,name]=='all'))
        replace.row <- ext_year[replace.index,]  
        ext_year <- ext_year[-replace.index,]  
        add <- data.frame(replace = unique)
        names(add) <- name
        add[, c(other.name,'pre_ext_year','post_ext_year') ] <- replace.row[, c(other.name,'pre_ext_year','post_ext_year') ]
        ext_year <- rbind(ext_year, add)
        replace <- ext_year[which(ext_year[,name]=='all'),]  
      }     }
    
    # replace default ext_years with ext_year
    names(ext_year)[names(ext_year) %in% c('pre_ext_year','post_ext_year')] <- c('pre_ext_year_new','post_ext_year_new')
    new_year <- merge(ext_year_full,ext_year, all.x=TRUE, all.y = FALSE)
    ext_year_full[which(!is.na(new_year$pre_ext_year_new)) ,'pre_ext_year'] <-new_year[which(!is.na(new_year$pre_ext_year_new)) ,'pre_ext_year_new']
    ext_year_full[which(!is.na(new_year$post_ext_year_new)) ,'post_ext_year'] <-new_year[which(!is.na(new_year$post_ext_year_new)) ,'post_ext_year_new']
  }
  
  # Create Meta data notes
  
  if (meta == TRUE) {
    names <- c('iso',scaling_name,'year','meta_comment')
    meta_notes <- data.frame(matrix(ncol = length(names), nrow = 0))
    names(meta_notes) <- names
  }
  
  # Extend Scaling Factors through Scaling Years and fill remaing (with scaling factor = 1)
  
  min_year <- as.numeric(min(years, pre_ext_year, ext_year_full$pre_ext_year))
  max_year <- as.numeric(max(years, post_ext_year, ext_year_full$post_ext_year))
  
  years_all <- min_year:max_year
  X_years_all <- paste0("X",years_all)
  
  ext_data_extended <- as.data.frame(matrix(data=NA,nrow = nrow(ext_data), ncol = length(years_all)))
  names(ext_data_extended) <- X_years_all
  ext_data_extended <- cbind(ext_data[,c(id.names)], ext_data_extended)
  
  
  for (i in seq_along(ext_data_extended$iso)){
    # Interpolated inventory data
    ext_data_extended[i,X_years_full] <- ext_data[i,X_years_full]
    if( !all(is.na( ext_data_extended[i,X_years_full] )) ){
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
          pre_ext_data_extended_line[1,] <-t(na.locf(t(pre_ext_data_extended_line[1,]), fromLast = TRUE))
        } 
        # Linear Extrapolation to 1, from most recent value
        else if( ext_method_full[i,'pre_ext_method'] == 'linear_1'){
          pre_ext_data_extended_line[1,1]<-1
          pre_ext_data_extended_line[1,] <- na.approx(  t(pre_ext_data_extended_line[1,]) , maxgap = Inf)
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
        # Linear Extrapolation to 0, from most recent value
        else if( ext_method_full[i,'pre_ext_method'] == 'linear_0'){
          pre_ext_data_extended_line[1,1]<-0
          pre_ext_data_extended_line[1,] <- na.approx(  t(pre_ext_data_extended_line[1,]) , maxgap = Inf)
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
          post_ext_data_extended_line[1,] <- na.approx(  t(post_ext_data_extended_line[1,]) , maxgap = Inf)
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
  return(ext_data_extended)
}# end function

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
  source( paste( PARAM_DIR, "common_data.R", sep = "" ) )
  source( paste( PARAM_DIR, "data_functions.R", sep = "" ) )

    #debug
#     input <- data.frame(iso= c("aus" , "aut" , "bel", "all"),
#                       sector= c('all','transp_road','industry_comb','unc_gas_production'),
#                       fuel=c('hard_coal','coal_coke','all','natural_gas'),
#                       year= c('X1990','all','1980','all'),
#                       Ash = c(.2,.1,.23,.25),
#                       stringsAsFactors = FALSE)
#   input<-ash_ret_list[[2]]
#     start = start_year
#     end = end_year
#     iso=NA 
#     sector=NA 
#     fuel=NA
#     toWide = TRUE
  
  if (any(input =='all')){
  
  year <- start:end 

  # Define parameters from data
  names <- names( input )
  if( length(grep( "X", names)) >0 ){ 
      all.id.names <- names[-grep( "X", names)]
      toWide <- FALSE 
      }else{ all.id.names <- names }
  id.names <- all.id.names[ all.id.names %in% c('iso','sector','fuel','year')  ]
  
  if(all(is.na(iso)) & 
     'iso' %in% id.names &
      'all' %in% input$iso){
    MCL <- readData( "MAPPINGS", "Master_Country_List" )
    iso <- unique(MCL$iso)}
  if(all(is.na(sector)) & 
     'sector' %in% id.names &
     'all' %in% input$sector){
    MSL <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Sectors" )
    sector <- unique(MSL$sector) }
  if(all(is.na(fuel)) & 
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
  
  if ('year' %in% id.names){
    input$year <- sub("X","",input$year)
    input$year <- paste0('X',input$year)
    if( toWide == TRUE) {
      input<- cast( input, 
              formula = paste(paste(id.names[id.names %in% c('iso','sector','fuel')], collapse=' + '), '~ year'), 
              value = all.id.names[all.id.names %!in% c('iso','sector','fuel','year')])}
    }
}
  return(input)
}
# ------------------------------------------------------------------------------
# toStandardWide
# Brief: convert dataframe to standard CEDS wide format dataframe
# Dependencies: 
# Author: Rachel Hoesly
# parameters: 
# return:  
# input files: 
#     

# toStandardWide <- toStandardWide(input){
#   
#   names <- names( input )
#   if( length(grep( "X", names)) >0 ){ 
#     all.id.names <- names[-grep( "X", names)]
#     toWide <- FALSE 
#   }else{ all.id.names <- names }
#   id.names <- all.id.names[ all.id.names %in% c('iso','sector','fuel','year')  ]
#   
#   
#   if ('year' %in% id.names){
#     input$year <- sub("X","",input$year)
#     input$year <- paste0('X',input$year)
#     if( toWide == TRUE) {
#       input<- cast( input, 
#                     formula = paste(paste(id.names[id.names %in% c('iso','sector','fuel')], collapse=' + '), '~ year'), 
#                     value = all.id.names[all.id.names %!in% c('iso','sector','fuel','year')])}
#   }
#   return(input)
# }




