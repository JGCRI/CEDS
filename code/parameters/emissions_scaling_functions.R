#------------------------------------------------------------------------------
# Program Name: E.emissions_scaling_functions.R
# Author's Name: Tyler Pitkanen
# Date Last Modified: June 16, 2015
# Program Purpose: Header file containing generalized functions designed to
#   scale CEDS emissions data and emissions factors based on inventory data. 
#   This file is made to be sourced at the beginning of each module E script to
#   load the functions for Module E's data read-in, inventory data arranging, 
#   scaled data arranging, scaling factor calculation and estimation, and data 
#   write-out. 
# Note: Only designed for use within Module E scaling scripts
# TODO: Possibly add function to organize inv data
#   Use R objects for readin/writeout of scaled EF and emissions data
#   Add a master debug option that would also write out the .csv files
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# E.readScalingData
# Brief: reads in data and makefile info for module E scripts
# Details: reads in inventory data, the most recent versions of scaled emissions
#   and EFs, the mapping file data, and the emissions species. Performs checks 
#   for the ceds column of the mapping file as well as the input ceds data.
# Dependencies: CEDS_header.R, makefile specifications, modules B, C, and E
# Author: Tyler Pitkanen
# parameters:
#   inventory: file name of the inventory used in the script [default: inventory]
#   mapping:   file name of the mapping file [default: map]
#   method:    mapping method used to relate the inventory and ceds data 
#              [default: mapping_method]
# return: input_ef, input_em, inv_data_full, mapping_data
# input files: makefile args, common_data.R, B.[em]_scaled_EF, 
#   C.[em]_scaled_emissions, specified inventory, specified mapping file
# output files: null

E.readScalingData <- function( inventory = inventory_data_file, 
    mapping = sector_fuel_mapping, method = mapping_method ) {
# Select the data sheet corresponding the emissions species of interest. Pass in
#   em_species via makefile, or use a default if running in R directly
    args_from_makefile <- commandArgs( TRUE )
    em_species <<- args_from_makefile[1]
    if ( is.na( em_species ) ) em_species <<- "SO2"

# Read in data    
    sourceData( "PARAM", "common_data", ".R" )
    ef_file <- paste0( "E.", em_species, "_scaled_EF" )
    em_file <- paste0( "E.", em_species, "_scaled_emissions" )
    input_ef <<- readData( "MED_OUT", ef_file )
    input_em <<- readData( "MED_OUT", em_file )
    inv_data_full <<- readData( "EM_INV", substr( inventory, 1, nchar( inventory ) - 5 ), ".xlsx" )
    mapping_data <<- readData( "MAPPINGS", mapping )

# Determine scaling method and set params
    if( method == 'sector' ) {
        scaling_name  <<- "scaling_sector"
        inv_matchcol_name  <<- 'inv_sector'
        ceds_matchcol_name <<- 'ceds_sector'
    } else if( method == 'fuel'   ) {
        scaling_name  <<- "scaling_fuel"
        inv_matchcol_name <<- 'inv_fuel'
        ceds_matchcol_name <<- 'ceds_fuel'
    } else if( method == 'both'   )  {
        scaling_name  <<- "scaling_activity"
        inv_matchcol_name <<- 'inv_activity'
        ceds_matchcol_name <<- 'ceds_activity'
    }
    
}

# ---------------------------------------------------------------------------------
# E.invAggregate
# Brief: aggregates inventory data to the scaling sectors
# Details: reads in the standard form inv data, removes redundant rows, zeroes
#   out specified terms (eg NA), and aggregates the data to the scaling sectors
# Dependencies: CEDS_header.R, E.read(), module E scripts
# Author: Tyler Pitkanen
# parameters:
#   std_form_inv: inv data put into standard ceds format, with years along the 
#                 y axis and sectors along x [default: std_form_inv]
#   zeroed_terms: inv data terms to make zero, such as blank cells (NA), '-',
#                 etc; this depends on the inventory's notation for zeroes.
#                 [default: c(NA, 'NA', 'NA ', '-')]
# return: inv_data
# input files: null
# output files: null

E.invAggregate <- function( std_form_inv, zeroed_terms = c( NA, 'NA', 'NA ', '-' ) ) {

    printLog( "Aggregating inventory data" )
    
# Discard redundant rows
    inv_data <- subset( std_form_inv, std_form_inv$inv_sector %in% 
        mapping_data$inv_sector )
    inv_data_app <- addCols( inv_data, mapping_data, scaling_name,
        matchcol = inv_matchcol_name )

# Replace NA values with zero and make the data numeric
    suppressWarnings( inv_data_app[ sapply( 
        inv_data_app, function(x) x %in% zeroed_terms ) ] <- 0 )
    inv_data_num <- data.frame( apply( inv_data_app[ , X_scaling_years ], 
        2, as.numeric ) ) 

# Perform aggregation    
    inv_data_list <- cbind( inv_data_app[ , scaling_name ], inv_data_num )
    names( inv_data_list )[[1]] <- scaling_name
    inv_data <- aggregate( inv_data_list[ , X_scaling_years ], by =
        list( scaling_col = inv_data_list[ , scaling_name ] ), sum ) 
        
    return( inv_data )
}

# ---------------------------------------------------------------------------------
# E.cedsAggregate
# Brief: aggregates ceds data over fuel or sectors
# Details: reads in the ceds data, selects the region that corresponds to the
#   inventory, aggregates by fuel and then aggregates to the scaling sectors
# Dependencies: CEDS_header.R, module E scripts
# Author: Tyler Pitkanen
# parameters:
#   input_ef: the most recent version of the scaled EFs [default: input_ef]
#   input_em: the most recent version of the scaled emissions [default: input_em]
#   region:   the iso code(s) of the region(s) corresponding to the regions
#             covered by the inventory [required]
#   mapping_method: the method used to aggregate and scale inv and ceds data
#             [default: mapping_method]
# return: ceds_data
# input files: null
# output files: null

E.cedsAggregate <- function( input_ef, input_em, region, method = mapping_method ) {
    
# Clean the data
    clean_input_ef <- removeBlanks( input_ef, 'name', 'iso' )
    clean_input_em <- removeBlanks( input_em, 'name', 'iso' )
    
# Skip the aggregation if mapping by both sector and fuel
    if( method == 'both' ) return( invisible( ) )
    
# Select the CEDS data that corresponds to the inventory region(s)
    sub_input_ef <- subset( clean_input_ef, clean_input_ef$iso %in% region )
    sub_input_em <- subset( clean_input_em, clean_input_em$iso %in% region )

# Make data numeric
    region_input_ef <- data.frame( apply( sub_input_ef[ , X_emissions_years ], 
                        2, as.numeric ) ) 
    region_input_ef <<- cbind( sub_input_ef[ , c( 'iso', 'sector', 'fuel', 'units' ) ], region_input_ef )
    region_input_em <<- data.frame( apply( sub_input_em[ , X_emissions_years ], 
                        2, as.numeric ) ) 

# If scaling by sector, aggregate over all fuels. If scaling by fuel, aggregate over all sectors.
    if( method == 'sector' ) {
        region_input_em <- cbind( region_input_em, sub_input_em[ , c( 'sector', 'iso' ) ] )
        region_em_sec <- aggregate( region_input_em[ , X_emissions_years], by = list( 
            iso = region_input_em$iso, ceds_sector = region_input_em$sector ), sum )
    } else if( method == 'fuel' ) {
        region_input_em <- cbind( region_input_em, sub_input_em[ , c( 'fuel', 'iso' ) ] )
        region_em_sec <- aggregate( region_input_em[ , X_emissions_years], by = list( 
            iso = region_input_em$iso, ceds_fuel = region_input_em$fuel ), sum )
    } else if( method == 'both' ) region_em_sec <- region_input_em

# Map the data from CEDS sectors/fuels to the scaling sectors/fuels
    if( method %in% c( 'sector', 'both' ) ){
    # Aggregate CEDS sectors to scaling sectors
        region_em_sec_appended <- addCols( region_em_sec, mapping_data, scaling_name,
            matchcol = ceds_matchcol_name )
        ceds_em_data <<- aggregate( region_em_sec_appended[ , X_emissions_years], by =
            list( scaling_col = region_em_sec_appended[ , scaling_name ] ), sum )  
    } else if( method %in% c( 'fuel', 'both' ) ) {
    # Aggregate CEDS fuels to scaling fuels
        if( method == 'both' ) region_em_sec <- ceds_em_data
        region_em_sec_appended <- addCols( region_em_sec, mapping_data, scaling_name,
            matchcol = ceds_matchcol_name )
        ceds_em_data <<- aggregate( region_em_sec_appended[ , X_emissions_years], by =
            list( scaling_col = region_em_sec_appended[ , scaling_name ] ), sum )             
    }
}

# ---------------------------------------------------------------------------------
# E.scale
# Brief: produces scaling factors from aggregated ceds and inventory data
# Details: takes in the ceds data and the inventory data that has been put in ceds
#   standard form. Scaling factors are calculated by dividing inv data by ceds 
#   data. The scaling factors are then interpolated and extrapolated to match
#   ceds years in a specified manner. The ceds data is then disaggregated
#   back into its original form
# Dependencies: CEDS_header.R, E.invAggregate(), E.cedsAggregate
# Author: Tyler Pitkanen
# parameters:
#   ceds_data:      ceds data for the inventory's region [default: ceds_data]
#   inv_data:       inv data in ceds standard form [default: inv_data]
#   scaling_years:  the years covered by the inventory data [default: scaling_years]
#   int_method:     the interpolation method used to estimate scaling factors for 
#                   years not covered by inventory data, linear or none [default: linear]
#   pre_ext_method: the extrapolation method used to estimate scaling factors
#                   for years before inventory data begins [default: constant]
#   post_ext_method: the extrapolation method used to estimate scaling factors
#                    for years after inventory data ends [default: constant]
# return: null
# input files: null
# output files: null

E.scale <- function( ceds_data = ceds_em_data, inv_data, scaling_years, 
    int_method = interpolation, pre_ext_method = extrapolation_before, 
    post_ext_method = extrapolation_after, region ) {

# If interpolation and extrapolation methods aren't specified, use defaults
    if( !exists( int_method ) )      int_method <- "linear"
    if( !exists( pre_ext_method  ) ) pre_ext_method  <- "constant"
    if( !exists( post_ext_method ) ) post_ext_method <- "constant"

# Calculate the scaling factors by taking the ratio of inventory data to CEDS 
#   data. This only gives a partial set of the scaling factors because they are
#   only calculated where inventory data is available. The remaining scaling
#   factors are calculated through interpolation and extrapolation.
    region_data_scaling_years <- ceds_data[ , c( "scaling_col", X_scaling_years ) ]
    row_arrangement <- match( region_data_scaling_years[ ,1], inv_data[ ,1] )
    scaling <- mapply( '/', region_data_scaling_years[ , 2:ncol( region_data_scaling_years ) ], 
        inv_data[ row_arrangement, 2:ncol( inv_data ) ] )
    scaling <- data.frame( scaling )
    scaling[ , scaling_name ] <- paste( inv_data[ row_arrangement, 1 ] )

# Make some adjustments to the preliminary scaling factor data set:
    # Inf results where a CEDS emissions is almost zero and its corresponding inv
    #   emission is simply listed as zero. This difference in rounding is solved
    #   by changing all instances of Inf to 1
    scaling[ sapply( scaling, is.infinite ) ] <- 1
    
    # NaN results where both a CEDS and inv value are zero. Again we change the
    #   ratio to 1 because there is a 1:1 correspondence between CEDS data and inv
    #   data.
    scaling[ sapply( scaling, is.na ) ] <- 1

# Now expand the inventory data for X_scaling_years to all X_emissions_years,
#   i.e. fill in the gaps where inv data isn't present. needed_years represents 
#   the years where an estimation (inter/extrapolation) is needed
    needed_years <- emissions_years[ emissions_years %!in% scaling_years ]
    needed_years <- needed_years[ needed_years >= start_year & needed_years <= end_year ]
    X_needed_years <- paste0( 'X', needed_years )
    
# Make a column of ones. This is used if 'none' is selected for pre/post 
#   extrapolation or interpolation; scaling factors for needed years will just be
#   set as a column of ones so the emissions/EFs are unaffected for those years.
    col_of_ones <- scaling[ , 1]
    col_of_ones[] <- 1 

# Begin extrapolation/interpolation calculations 
    added_cols <- list()
    n <- 1
    for( i in needed_years ) {
        # Extrapolate for years before first inventory data point. 
        if( i < min( scaling_years ) ) { 
            if( pre_ext_method == 'constant' ) {
            # Use the first year's scaling factors for all previous years
                added_cols[[n]] <- scaling[ , X_scaling_years[1] ] 
                n <- n + 1
            } else if( pre_ext_method == 'linear' ) {
            # Use the slope of the last two year's scaling factors to extendForward
            #   to previous years
                factor_change <- scaling[ , X_scaling_years[2]] - 
                    scaling[ , X_scaling_years[1]]
                year_change <- X_scaling_years[2] - X_scaling_years[1]
                slope <- factor_change / year_change
                years <- needed_years[i] - X_scaling_years[1]
                added_cols[[n]] <- years * slope
                n <- n + 1
            } else if( pre_ext_method == 'none' ) {
            # Use a column of ones so emissions factors are unaffected by scaling
                added_cols[[n]] <- col_of_ones
                n <- n + 1
            }
        # Extrapolate for years after last inventory data point. 
        } else if( i > max( scaling_years ) ) { 
            if( post_ext_method == 'constant' ) {
            # Use the last year's scaling factors for all following years
                added_cols[[n]] <- scaling[ , X_scaling_years[ length( 
                    X_scaling_years ) ] ]
                    n <- n + 1
            } else if( post_ext_method == 'linear' ) {
            # Use the slope of the last two year's scaling factors to extendForward
            #   to following years
                last <- length( X_scaling_years )
                factor_change <- scaling[ , X_scaling_years[last]] - 
                    scaling[ , X_scaling_years[last - 1]]
                year_change <- X_scaling_years[last] - X_scaling_years[last - 1]
                slope <- factor_change / year_change
                years <- needed_years[i] - X_scaling_years[last]
                added_cols[[n]] <- years * slope
                n <- n + 1
            } else if( post_ext_method == 'none' ) {
            # Use a column of ones so emissions factors are unaffected by scaling
                added_cols[[n]] <- col_of_ones
                n <- n + 1
            }
        # Interpolate for years between inventory data points. 
        } else { 
            if( int_method == 'linear' ) {
            # Use the slope of the nearest surrounding years to determine scaling
            #   for intermittent years
                dif  <- scaling_years - i
                ub   <- min( scaling_years[ dif > 0 ] )
                lb   <- max( scaling_years[ dif < 0 ] )
                X_ub <- paste0( 'X', ub )
                X_lb <- paste0( 'X', lb )
                
                year_range  <- ub - lb
                data_range  <- scaling[ , X_ub ] - scaling[ , X_lb ]
                year_change <- i - lb
                data_change <- data_range / year_range * year_change
                
                added_cols[[n]] <- scaling[ , X_lb ] + data_change
                n <- n + 1
            } else if( int_method == 'none' ) {
            # Use a column of ones so emissions factors are unaffected by scaling
                added_cols[[n]] <- col_of_ones
                n <- n + 1
            }
        }
    }
    names( added_cols ) <- X_needed_years
    
# Combine the directly calculated scaling factors with inter/extrapolated ones
    scaling_ext <- cbind( scaling, added_cols )
    scaling_ext <- scaling_ext[ , order( names( scaling_ext ) ) ]  

    printLog( "Applying scaling factors to CEDS EFs and emissions" )

# Disaggregate from scaling sectors to CEDS sectors and create CEDS scaling that
#   is matched to the CEDS rows from the input
    disagg_rows <- match( mapping_data[ , scaling_name ], scaling_ext[ , scaling_name ] )
    ceds_scaling <- scaling_ext[ disagg_rows, ]
    ceds_scaling[ , ceds_matchcol_name ] <- mapping_data[ , ceds_matchcol_name ]
    ceds_scaling_col <- ceds_scaling[ , c( length( ceds_scaling ), 2:( length( ceds_scaling ) - 1 ) ) ]
    ceds_scaling_col[ sapply( ceds_scaling_col, is.na ) ] <- 1
 
    ceds_rows <- match( region_input_ef$sector, ceds_scaling_col[ , ceds_matchcol_name ] )
    ceds_scaling <- ceds_scaling_col[ ceds_rows, ] 
    ceds_scaling[ , ceds_matchcol_name ] <- region_input_ef$sector
    ceds_scaling[ sapply( ceds_scaling, is.na ) ] <- 1

# Apply ceds scaling to the most recent scaled emissions factors  
    scaled_region_ef <- mapply( '*', region_input_ef[ , 5:ncol( region_input_ef ) ], 
                                ceds_scaling[ , 2:ncol( ceds_scaling ) ] )
    scaled_ef <- removeBlanks( input_ef,'name', 'iso' )
    scaled_ef[ scaled_ef$iso %in% region, 5:ncol( scaled_ef ) ] <- scaled_region_ef
    ef_output <<- scaled_ef
    
# Apply ceds scaling to the most recent scaled emissions
    scaled_region_em <- mapply( '*', region_input_em,
        ceds_scaling[ , 2:ncol( ceds_scaling ) ] )
    scaled_em <- removeBlanks( input_em, 'name','iso' )
    scaled_em[ scaled_em$iso %in% region, 5:ncol( scaled_em ) ] <- scaled_region_em
    em_output <<- scaled_em
}

# ---------------------------------------------------------------------------------
# E.write
# Brief: writes out newly scaled EF and emissions data
# Details: automatically forms comments from region and inventory specifications,
#   writes out emissions factor data as a csv, and updates the emissions object.
#   The file name is generated from the emissions species: E.[em]_scaled_EF
#   Also writes out the emissions data, but this may be removed in the future.
# Dependencies: CEDS_header.R, E.scale
# Author: Tyler Pitkanen
# parameters:
#   ef: scaled emissions factors data [default: null]
#   em: scaled emissions data [default: null]
#   domain: save location domain [default: "MED_OUT"]
#   comments: comments for the EF output csv file. The "default" option 
#             automatically generates one comment noting the inventory 
#             used and the regions affected
# return: null
# input files: null
# output files: E.[em]_scaled_EF

E.write <- function( ef = ef_output, em = em_output, domain = "MED_OUT", 
                     comments = "default" ) {
    
# Write the comments as default versions unless they are overridden     
    if ( comments == "default" ) {
        ef_comment_text <- paste( "Global emissions factors, where", region,
            "EFs have been scaled by data from", inventory_data_file )
        em_comment_text <- paste( "Global emissions, where", region,
            "emissions have been scaled by data from", inventory_data_file )
        comments.E.scaled_EFs <- c( ef_comment_text )
        comments.E.scaled_emissions <- c( em_comment_text )
    } else {
        comments.E.scaled_EFs <- comments
        comments.E.scaled_emissions <- comments
    }
 
# Create fn from em_species specification
# The fn is selected such that it overwrites the previous E.[em]_scaled_EF
#   version, so all module E scripts update it serially
    ef_fn <- paste0( 'E.', em_species, '_scaled_EF' )
    em_fn <- paste0( 'E.', em_species, '_scaled_emissions' )
 
# Write EF table as a csv file 
    writeData( ef, domain = domain, fn = ef_fn, 
        comments = comments.E.scaled_EFs )

# May be removed later; data file may not be necessary 
    writeData( em, domain = domain, fn = em_fn, 
        comments = comments.E.scaled_emissions )

# This creates or updates an R object in the global environment. It has a name
#   equivalent to the string in em_fn, and it has the value of em.
# We use this format so the assignment works for a general emissions species;
#   this method allows us to evaluate em_species and include it in the object name 
    assign( em_fn, em, envir = .GlobalEnv )
}
