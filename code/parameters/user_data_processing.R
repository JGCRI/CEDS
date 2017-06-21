#------------------------------------------------------------------------------
# Program Name: user_data_proc_pseudocode.R
# Author: Ben Goldstein
# Date Last Updated: 21 June 2017
# Program Purpose: To process user-defined datasets for use in the historical
#                  energy extension. 
# Input Files: U.*, U.*-instructions, U.*-mapping
# Output Files: None
# Functions Defined: mapToCEDS, interpolateData, interpolateByTrend
# Notes: 

# ------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# processUserDefinedData()
    
    processUserDefinedData <- function( filename, interpolation_instructions,
                                        MSL = NULL, MCL = NULL, MFL = NULL, comb_or_NC = NULL ) {
        
    # Read in the data frame
        dataframe <- readData( paste0( "user-defined-energy/", filename ), domain = "EXT_IN" )
        
    # initialize null dataframe
        mapping_iso <- NULL
        mapping_agg_sector <- NULL
        mapping_ceds_sector <- NULL
        mapping_agg_fuel <- NULL
        mapping_ceds_fuel <- NULL
        
    # read in each potential mapping sheet. Uses try() since some sheets may not
    # be provided, which is allowed.
        try( mapping_iso <- readData( paste0( "user-defined-energy/", filename, "-mapping" ), 
                                      domain = "EXT_IN", 
                                      extension = ".xlsx",
                                      sheet_selection = "iso" ) )
        try( mapping_agg_sector <- readData( paste0( "user-defined-energy/", filename, "-mapping" ), 
                                             domain = "EXT_IN", 
                                             extension = ".xlsx",
                                             sheet_selection = "agg_sector" ) )
        try( mapping_ceds_sector <- readData( paste0( "user-defined-energy/", filename, "-mapping" ), 
                                              domain = "EXT_IN", 
                                              extension = ".xlsx",
                                              sheet_selection = "CEDS_sector" ) )
        try( mapping_agg_fuel <- readData( paste0( "user-defined-energy/", filename, "-mapping" ), 
                                           domain = "EXT_IN", 
                                           extension = ".xlsx",
                                           sheet_selection = "agg_fuel" ) )
        try( mapping_ceds_fuel <- readData( paste0( "user-defined-energy/", filename, "-mapping" ), 
                                            domain = "EXT_IN", 
                                            extension = ".xlsx",
                                            sheet_selection = "CEDS_fuel" ) )
    
    # If master mapping files were not provided, they need to be read in.
        if ( is.null( MSL ) ) {
            MSL <- readData( "Master_Sector_Level_map", domain = "MAPPINGS" )
            colnames( MSL )[ which( colnames( MSL ) == 'working_sectors_v1' ) ] <- 'CEDS_sector' 
        }
        if ( is.null( MCL ) ) MCL <- readData("Master_Country_List", domain = "MAPPINGS")
        if ( is.null( MFL ) ) MFL <- readData("Master_Fuel_Sector_List", domain = "MAPPINGS", extension = ".xlsx",
                                          sheet_selection = "Fuels")
        
    # Execute sub-functions; first map the dataframe, then interpolate this mapped dataframe.
        mapped_dataframe <- mapToCEDS(dataframe, MSL, MCL, MFL, 
                                      iso_map = mapping_iso, 
                                      agg_sector_map = mapping_agg_sector, 
                                      CEDS_sector_map = mapping_ceds_sector, 
                                      agg_fuel_map = mapping_agg_fuel, 
                                      CEDS_fuel_map = mapping_ceds_fuel)
        interp_dataframe <- interpolateData( mapped_dataframe, interpolation_instructions, MSL, MCL, MFL )
        
        return( interp_dataframe )
    }



    
#------------------------------------------------------------------------------
# mapToCEDS()
    mapToCEDS <- function( dataframe, MSL, MCL, MFL, iso_map = NULL, 
                           agg_sector_map = NULL, CEDS_sector_map = NULL, 
                           agg_fuel_map = NULL, CEDS_fuel_map = NULL, aggregate = TRUE ) {
      
        print( "Mapping to CEDS" )
        dataframe_categories <- colnames( dataframe )[ !grepl( "X", colnames( dataframe ) ) ]
        Xyears <- colnames( dataframe )[ which( colnames( dataframe ) %!in% 
                                                  dataframe_categories ) ]
      
    # If the iso is not already in the data, map to it
        if ( 'iso' %!in% dataframe_categories ) {
            if ( is.null( iso_map ) ) {
                stop( "Error in mapToCEDS: iso not in data, but no iso map provided" )
            }
          
        # Merge the datasets together. This assumes that the name of the non-iso country column
        #   in each dataset is the same. Can we make this assumption? It seems like yes, as that's
        #   the point of a mapping file.
            dataframe_w_iso <- left_join( dataframe, iso_map )  ### Do anti-join first, print a warning with the unmatchable country-level data
            if ( nrow( dataframe_w_iso ) != nrow( dataframe ) ) {
                stop( "Double-mapped isos are not allowed." )
            }
            dataframe <- dataframe_w_iso
          
        # Throw out data that has NAs for iso
            dataframe <- dataframe[ which( !is.na( dataframe$iso ) ), ]
        }
      
        printLog( "mapToCEDS: iso mapping complete" )
        
    # If a CEDS sector map was provided:
        if ( !is.null( CEDS_sector_map ) |
                       'CEDS_sector' %in% dataframe_categories ) {
            if ( 'CEDS_sector' %!in% dataframe_categories ) {
            # same join, same assumptions
                dataframe_w_sector <- left_join( dataframe, CEDS_sector_map )
                if ( nrow( dataframe_w_sector ) != nrow( dataframe ) ) {
                    stop( "Double-mapped sectors are not allowed." )
                }
                dataframe <- dataframe_w_sector
              
            # Throw out data that has NAs for CEDS_sector
                dataframe <- dataframe[ which( !is.na( dataframe$CEDS_sector ) ), ]
            }
          
        # if a CEDS sector map was provided but an agg sector map was not,
        #   we can automatically map to agg sector based on CEDS sectors and MSL
            if ( is.null( agg_sector_map ) ) {
                if ( 'agg_sector' %!in% dataframe_categories ) {
                    MSL_to_map <- unique( MSL[ , c( "CEDS_sector", "aggregate_sectors" ) ] )
                    colnames( MSL_to_map ) <- c( "CEDS_sector", "agg_sector" )
                    dataframe <- left_join( dataframe, MSL_to_map ) 
                }
            }
        }
      
    # There are three possibilities. If neither CEDS nor agg sector maps are provided,
    #   agg_sector_map never gets assigned. If only CEDS_sector_map is provided,
    #   agg_sector_map happens above. If agg_sector_map is provided, it will be 
    #   mapped below.
        if ( !is.null( agg_sector_map ) ) {
            if ( 'agg_sector' %!in% dataframe_categories ) {
            
            # same join, same assumptions
                dataframe_w_aggsec <- left_join( dataframe, agg_sector_map )
                if ( nrow( dataframe_w_aggsec ) != nrow( dataframe ) ) {
                    stop( "Double-mapped sectors are not allowed." )
                }
                dataframe <- dataframe_w_aggsec
                
            # Throw out data that has NAs for CEDS_sector
                dataframe <- dataframe[ which( !is.na( dataframe$agg_sector ) ), ]
                
            }
        }
        
        # If a CEDS fuel map was provided:
        if ( !is.null( CEDS_fuel_map ) |
             'CEDS_fuel' %in% dataframe_categories ) {
          if ( 'CEDS_fuel' %!in% dataframe_categories ) {
            # same join, same assumptions
            dataframe_w_fuel <- left_join( dataframe, CEDS_fuel_map )
            if ( nrow( dataframe_w_fuel ) != nrow( dataframe ) ) {
              stop( "Double-mapped sectors are not allowed." )
            }
            dataframe <- dataframe_w_fuel
            
            # Throw out data that has NAs for CEDS_fuel
            dataframe <- dataframe[ which( !is.na( dataframe$CEDS_fuel ) ), ]
          }
          
          # if a CEDS fuel map was provided but an agg fuel map was not,
          #   we can automatically map to agg fuel based on CEDS fuel and MSL
          if ( is.null( agg_fuel_map ) ) {
            if ( 'agg_fuel' %!in% dataframe_categories ) {
              MFL_to_map <- MFL[ , c( "fuel", "aggregated_fuel" ) ]
              colnames( MFL_to_map ) <- c( "CEDS_fuel", "agg_fuel" )
              dataframe <- left_join( dataframe, MFL_to_map ) 
            }
          }
        }
        
        # There are three possibilities. If neither CEDS nor agg sector maps are provided,
        #   agg_sector_map never gets assigned. If only CEDS_sector_map is provided,
        #   agg_sector_map happens above. If agg_sector_map is provided, it will be 
        #   mapped below.
        if ( !is.null( agg_fuel_map ) ) {
          if ( 'agg_fuel' %!in% dataframe_categories ) {
            
            # same join, same assumptions
            dataframe_w_aggfuel <- left_join( dataframe, agg_fuel_map )
            if ( nrow( dataframe_w_aggfuel ) != nrow( dataframe ) ) {
              stop( "Double-mapped fuels are not allowed." )
            }
            dataframe <- dataframe_w_aggfuel
            
            # Throw out data that has NAs for CEDS_sector
            dataframe <- dataframe[ which( !is.na( dataframe$agg_fuel ) ), ]
            
          }
        }
        
      
    # The pseudocode for agg_fuel and CEDS_fuel mapping should be identical to the
    #   sectoral mapping, so I won't reproduce it. Imagine that what follows occurs
    #   after fuel mapping is complete.
      
        present_columns <- colnames( dataframe ) [ which ( colnames( dataframe ) %in% 
                                                        c( "iso", "agg_sector", "CEDS_sector", 
                                                           "agg_fuel", "CEDS_fuel" ) ) ]
      
        dataframe <- dataframe[ , c( present_columns, Xyears ) ]
        
        printLog( "Done with mapping file to CEDS categories" )
      
    # Aggregate data frames by categories
        if ( aggregate ) {
            dataframe <- ddply( dataframe, present_columns, function(x) colSums( x[ Xyears ] ) )
        }
  
        return( dataframe )
      
    }


#------------------------------------------------------------------------------
# interpolateData()
# A function defining standard interpolation methodology. This assumes that 
#     0 values are not holes; only missing values or NAs are holes.
    interpolateData <- function( dataframe, interpolation_instructions, MSL, MCL, MFL ) {
      
    # Check if the data has any holes
      
    # We can assume that all columns are in this form as the data
    #   has already been processed by mapping
        non_year_cols <- colnames( dataframe ) [ which( colnames( dataframe ) %in% 
                                                        c( "iso", "agg_sector", "CEDS_sector", 
                                                           "agg_fuel", "CEDS_fuel" ) ) ]
      
    # Determine desired year range, and which years need to be added or removed;
    #  Then add and remove columns to the dataframe so it represents the desired range
        X_data_years <- paste0( "X", interpolation_instructions$start_year:interpolation_instructions$end_year )
        
        needed_Xyears <- X_data_years[ which( X_data_years %!in% colnames( dataframe ) ) ]
        extra_Xyears <- colnames( dataframe )[ which( colnames( dataframe ) %!in% X_data_years &
                                                      colnames( dataframe ) %!in% non_year_cols ) ]
        
        dataframe[ , needed_Xyears ] <- NA
      
        for ( year in extra_Xyears ) {
            dataframe[[ year ]] <- NA
        }
        dataframe <- dataframe[ , c( non_year_cols, X_data_years ) ]
      
    # Now we can finally check if the data has a hole
        if ( !any( apply ( dataframe, 1, function(r) any( is.na(r) ) ) ) ) {
        
        # In the event that there are no NAs we can return the data matched to the intended years.
            return( dataframe )
        # Otherwise, we have to continue on and fill the gaps.
        }
      
    # Confirm that a valid method was specified. I'm making a list to store valid methods
    #   so we can easily change them as they arise.
        valid_methods <- c( "match_to_trend", "linear" )
      
        if ( interpolation_instructions$method %!in% valid_methods ) {
        # Throw an error if the method is invalid
            stop( paste0( "Specified interpolation method '", 
                       interpolation_instructions$method, "' is invalid" ) )
        } else if ( interpolation_instructions$method == "match_to_trend" ) {
        # Execute trend-matching function
            final_dataframe <- interpolateByTrend( dataframe, 
                                                 interpolation_instructions$matching_file_name,
                                                 interpolation_instructions$domain,
                                                 MSL, MCL, MFL )
        } else if ( interpolation_instructions$method == "linear" ) {
        # CEDS already has a function for linear interpolation. 
            final_dataframe <- dataframe
            final_dataframe[ , X_data_years ] <- interpolate_NAs( final_dataframe[ , X_data_years ] )
        }
      
        return( final_dataframe )
      
    }

#------------------------------------------------------------------------------
# interpolateByTrend()
# This function takes a dataframe and the info to read in a matching file,
#    then
    interpolateByTrend <- function( dataframe, match_filename, match_domain, MSL, MCL, MFL ) {
    

    # Read in the trend that we'll match our values to next to each gap
        dataToMatch <- readData( file_name = paste0("user-defined-energy/", match_filename ), domain = match_domain )
        
        non_year_cols <- colnames( dataframe ) [ which (colnames( dataframe ) %in% 
                                                        c( "iso", "agg_sector", "CEDS_sector", 
                                                           "agg_fuel", "CEDS_fuel" ) ) ]
        dataToMatch <- mapToCEDS( dataToMatch, MSL, MCL, MFL, iso_map = MCL, CEDS_sector_map = MSL )
        
    # Trim the trend data to only those rows that correspond to a row in the dataframe
        for ( col in non_year_cols ) {
            dataToMatch <- dataToMatch[ which( dataToMatch[[ col ]] %in% dataframe[[ col ]] ), ]
        }
        
        
    # Check that the specified trend data has data for all the years desired
        all_Xyears <- colnames( dataframe )[ which( colnames( dataframe ) %in% paste0( "X", 1750:2016 ) ) ]
        if ( any( all_Xyears %!in% colnames( dataToMatch ) ) ) {
            stop( "Error: Trend data did not have years available for specified date range." )
        }
    
    # Assume the data is uniformly in need of interpolation (all rows have holes
    #     in the same years). Identify the years for which there is data.
        row1_year_data <- dataframe[ 1, all_Xyears ]
        years_with_data <- colnames( row1_year_data )[ which( !is.na( row1_year_data ) ) ]
    
    # Trim trend data to the years of the dataframe
        dataToMatch <- dataToMatch[ , c( non_year_cols, all_Xyears ) ]
    
    # Initialize a multiplier dataframe
        multiplier <- dataframe
    
    # All yearly data set to NA
        multiplier[ , all_Xyears ] <- NA
      
    # Multiplier for years with original data set as given data / trend data
        multiplier[ years_with_data ] <- dataframe[ years_with_data ] / dataToMatch [ years_with_data ]
        multiplier[ all_Xyears ] <- replace( multiplier[ all_Xyears ], multiplier[ all_Xyears ] == Inf, 1 )  
    # Linearly interpolate the multiplier
        multiplier[ , all_Xyears ] <- interpolate_NAs( multiplier[ , all_Xyears ] )  ### Thing to discuss: right now if a trend has a 0 value, that value will end up at 0 even if the data says otherwise.
                                                                                     ###   Should we go in and change this, maybe replace all 0s (or even all values) that we already had data for 
                                                                                     ###   with the original data just to be safe? probably yes
      
    # Apply the multiplier to the data; result is interpolated data
        interpolated_data <- dataToMatch
        interpolated_data[ , all_Xyears ] <- dataToMatch[ , all_Xyears ] * multiplier[ , all_Xyears ]
        return( interpolated_data )
    }

