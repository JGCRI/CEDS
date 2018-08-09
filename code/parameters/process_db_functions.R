# ----------------------------------------------------------------------------------
# CEDS R header file: database functions
# Authors: Jon Seibert, Rachel Hoesly
# Last Updated: 22 June 2015

# This file should be sourced by any R script altering the CEDS databases
# on activity, emissions, or emissions factors data.
# Functions contained:
#   cleanData, addToActivityDb, addToEmissionsDb, addToEFDb, addToDb_overwrite

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

    activity_list <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx",
                             sheet_selection = "Sectors", mute = TRUE )
    activity_list_unique <- unique(activity_list$activity)

    # Read in necessary files and data: common_data.R required
    # to avoid variable overwrite carryover
    source( paste( PARAM_DIR, "common_data.R", sep = "" ) )
    activity_db <- readData( "MED_OUT", "A.NC_activity_db", meta = FALSE )

    if ( !all(unique(df$activity) %in% activity_list_unique)){
      printLog("Not all activities are in the master list. Those are not added to A.NC_activity_db.")
      df <- df[df$activity %in% activity_list_unique,]
    }

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
    ext_forward <- T
    ext_backward <- T

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
    if( anyNA( results[ , X_emissions_years] ) ) {
      stop("Checking NAs... NA's in EF_db. Check Code.")
    } else {
      printLog("Checking NAs... No NA's in EF_db")
    }
  }

  # Output
  writeData( results, domain = "MED_OUT", fn = paste0( "C.", em, "_", type, "_emissions_db" ), meta = FALSE )
}

# ----------------------------------------------------------------------------------
# addToDb_overwrite
# Brief:            Adds data to a database by overwriting existing values for iso/sector/fuel/year
#                   combinations in supplied data frame.
# Details:          Reads in specified database (noted by input called file_extension) and
#                   replaces/overwrites values for iso-sector-fuel-combinations with values in
#                   input dataframe. Replaces values using match function (finds first match).
#                   If addEntries == FALSE, then the database is the same size and sort as
#                   the old database.
#                   If addEntries == TRUE then new database contains
#                   iso/sector/fuel/year combinations in the supplied dataframe
#                   that are not in the original dataabse (new data base has more rows)
#                   If duplicates exist in the new_data, the last entry of the dataframe is give
#                   priority
# Dependencies:     common_data.r, IO_functions.R, data_functions.R,
# Author:           Rachel Hoesly
# Parameters:
# 	new_data: 		    data frame containing data to be added to the database
#                       ( df must have form iso-sector-fuel-units-years ) [required]
#   em:             emissions species (ex. SO2, CO2, BC) [required]
#   file_extension: partial name of the old database to which the function adds new data. Name
#                   after "X.[em]_" excluding the file extension
#                   ex. "ControlFrac_db" for the file B.[em]_ControlFrac_db.csv
# Return:           none
# Input files:      B.[em]_xxxxx_db.csv, common_data.R
# Output files:     B.[em]_xxxxx_db.csv
# example:          addToDb_overwrite(new_data = s_content, em = 'SO2', file_extension = 'S_Content_db')

addToDb_overwrite <- function( new_data, em, file_extension, module = 'B',
                               addEntries = FALSE ){
  printLog ("Adding new data to database")

  #   Read in necessary files and data: common_data.r required
  #   to avoid variable overwrite carryover
  source( paste( PARAM_DIR, "common_data.R", sep = "" ) )
  source( paste( PARAM_DIR, "IO_functions.R", sep = "" ) )
  source( paste( PARAM_DIR, "timeframe_functions.R", sep = "" ) )
  source( paste( PARAM_DIR, "data_functions.R", sep = "" ) )

  original_db <- readData( "MED_OUT", paste0( module,".", em, "_", file_extension ) )
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

  # check New data for duplicates and remove - priority given to last data
  nrow_duplicated <- nrow(new_data)
  new_data <- new_data[  !duplicated( new_data[,id.names.new] , fromLast = TRUE)  , ]
  nrow_unique <- nrow( new_data)

  if( nrow_duplicated != nrow_unique) printLog('New Data has duplicate entries. Last given priority')

  # Insert the new data in the proper locations-
  # overwrite any existing data of the same combination and years,
  # and rbind on any data without a spot.
  # results <- original_db
  #---------

  # melt old and new dbs
  df_add<-melt(new_data,id=c("iso","sector","fuel","units"))
  names(df_add)[which(names(df_add)=='variable')]<-'year'
  df_add <- df_add[!is.na(df_add$value),] #don't add the NAs from the new df

  df_old<-melt(original_db,id=c("iso","sector","fuel","units"))
  names(df_old)[which(names(df_old)=='variable')]<-'year'

  #merge
  printLog('Merging new and old Data')
  df_new <- replaceValueColMatch(x=df_old, y=df_add,
                                 x.ColName = 'value',
                                 match.x = c('iso','sector','fuel','units','year'),
                                 addEntries = FALSE)

  df_new_wide<-as.data.frame(cast(df_new, iso+sector+fuel+units~year, value = "new_value"))

  # Sort
  results<-df_new_wide
  results <- results[ with( results, order( iso, sector, fuel ) ), ]

  # Check for NAs
  if( na_error == 1){
    if( anyNA( results[,X_emissions_years] ) ) {
      stop("Checking NAs... NA's in new data. Check Code.")
    } else {
      printLog("Checking NAs... No NA's in new data")
    }
  }

  # Check length
  if ( n.observations != nrow(results)) stop("Outbut Database is not the same length as original DB. Duplicates in addToDb_overwrite.")

  # Output
  writeData( results, domain = "MED_OUT", fn = paste0( module,".", em, "_", file_extension))
  }
