# ------------------------------------------------------------------------------
# Program Name: G0.3.flaring_proxy_generation.R
# Authors: Noah Prime, Hamza Ahsan
# Date Last Updated: 3/5/25
# Program Purpose: Generate the proxies for the FLR sector
# Input Files:  Intermediate FLR proxy files and ECLIPSE data
#
# Output Files: FLR proxy files
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# 0.) Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Read in universal header files, support scripts, and start logging
headers <- c( 'utility_functions.R', 'common_data.R', 'gridding_functions.R' )
log_msg <- "Generating FLR proxy files"
source( paste0( PARAM_DIR, "header.R" ) )
initialize( "G0.3.flaring_proxy_generation.R", log_msg, headers )


# ------------------------------------------------------------------------------
# 0.1) Set-up details for script

# Define emissions species and gridding resolution
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
res <- as.numeric( args_from_makefile[ 2 ] )
if ( is.na( em ) ) em <- "SO2"
if ( is.na( res ) ) res <- 0.5 # default gridding resolution of 0.5

# check to make sure passed in emissions species is available in ECLIPSE
em_list <- c( 'SO2', 'NOx', 'NH3', 'NMVOC', 'BC', 'OC', 'CO', 'CH4' )
if( em %in% em_list ){
    # check to make res is same as ECLIPSE data
    if( res == 0.5 ){
        # ------------------------------------------------------------------------------
        # 1. Set paths
        input_path          <- filePath( "GRIDDING", "ECLIPSE/",                 extension = "" )
        input_filename      <- 'ECLIPSE_V5a_CLE_base_flaring.nc'

        # Read in file which defines paths
        pathmapping <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'proxy_generation_flow', meta = F) %>%
          dplyr::rename( 'emission' = em)

        # extracting path to final proxy files from pathmapping
        final_path <- pathmapping %>%
          dplyr::filter( emission == em, type == 'ceds_sector' ) %>%
          dplyr::select( path ) %>%
          pull()

        # Path to extended / completed point source yaml files
        source_path <- filePath( 'MED_OUT', 'full_point_source_scaled_yml/', extension = "" )

        # Default proxy generation parameters
        default_params <- readData( file_name = 'default_proxy_generation_params',
                                    domain = 'GRIDDING', domain_extension = 'gridding_mappings/',
                                    meta = FALSE ) %>%
            dplyr::filter( sector == 'FLR' )

        # ------------------------------------------------------------------------------
        # 2. read netCDF and extract 1990 to 2030 data
        nc_file <- nc_open( paste0( input_path, input_filename ) )

        # read the variable list
        var_list <- unlist( lapply( seq( nc_file$var ), function( i ){
          var_name <- nc_file$var[[ i ]]$name
          return( var_name )
        } ) )

        # ------------------------------------------------------------------------------
        # 3. Read in YAML point source files

        source_data <- load_point_source_attributes( source_path, em )
        source_data <- source_data %>%
            dplyr::filter( EDGAR_sector == 'FLR' )


        #------------------------------------------------------------------------------
        # 4. Extract flaring data and interpolate for intermediate years

        # naming convention for variables in data
        variable_extension <- em
        if( em == 'NMVOC' ){ variable_extension <- 'VOC' }

        # extract data for species for years 1990 to 2030
        em_data <- ncvar_get( nc_file, paste0( 'emis_', variable_extension, '_flr' ) )
        em_data <- em_data[ , , 1:9 ]

        # set NA's to 0
        em_data[ is.nan( em_data ) ] <- 0

        # flip each layer in the array then write out to a list
        em_data <- lapply( seq( 1, dim( em_data )[ 3 ] ), function( i ){
          em_data[ , , i ] <- flip_a_matrix( t( em_data[ , , i ] ) )
        } )

        # name data in list
        eclipse_years <- seq(1990, 2030, 5)
        names( em_data ) <- eclipse_years

        # Define the target years (all years between 1990 and 2030)
        target_years <- 1990:2030

        # Create a named list to store all interpolated matrices
        interpolated_matrices <- list()

        # Store original matrices first
        for (i in seq_along(eclipse_years)) {
            interpolated_matrices[[as.character(eclipse_years[i])]] <- em_data[[i]]
        }

        # Function for linear interpolation
        interpolate_matrix <- function(mat1, mat2, weight1, weight2) {
            return(mat1 * weight1 + mat2 * weight2)
        }

        # Generate interpolated matrices for missing years between 1990 and 2025
        for (interp_year in target_years) {
            if (!as.character(interp_year) %in% names(interpolated_matrices)) {
                # Find the two surrounding years
                lower_idx <- max(which(eclipse_years < interp_year))
                upper_idx <- min(which(eclipse_years > interp_year))

                # Get the matrices for those years
                mat_lower <- em_data[[lower_idx]]
                mat_upper <- em_data[[upper_idx]]

                # Compute weights based on distance from lower/upper bound
                weight_lower <- (eclipse_years[upper_idx] - interp_year) / (eclipse_years[upper_idx] - eclipse_years[lower_idx])
                weight_upper <- (interp_year - eclipse_years[lower_idx]) / (eclipse_years[upper_idx] - eclipse_years[lower_idx])

                # Perform interpolation
                interpolated_matrices[[as.character(interp_year)]] <- interpolate_matrix(mat_lower, mat_upper, weight_lower, weight_upper)
            }
        }

        # --------------------------------------------------------------
        # 5. construct FLR proxy using mixed EDGAR ETRN grids and ECLIPSE grids

        eclipse_fraction <- 0.25
        year_list <- EDGAR_start_year : EDGAR_end_year
        for ( year in year_list ){

          # retrieve EDGAR ETRN proxy
          edgar_proxyname <- paste0( em, '_', year, '_ETRN' )
          load( paste0( final_path, '/', edgar_proxyname, '.Rd' ) )
          edgar_proxy <- get( edgar_proxyname )
          rm( list = edgar_proxyname )

          # retrieve ECLIPSE proxy (for any year before 1990, assign the 1990 flaring data)
          if (year < min(target_years)) {
              eclipse_proxy <- interpolated_matrices[["1990"]]
          } else {
              eclipse_proxy <- interpolated_matrices[[as.character(year)]]
          }

          # construct FLR proxy
          flr_proxy <- eclipse_fraction * eclipse_proxy + ( 1 - eclipse_fraction ) * edgar_proxy

          # remove point sources
          flr_proxy <- remove_point_sources( flr_proxy, source_data, default_params, 'FLR', 0.5 )

          # save flr_proxy to disk
          proxyname <- paste0( em, '_', year, '_FLR' )
          assign( proxyname, flr_proxy )
          save( list = proxyname, file = paste0( final_path, '/', proxyname, '.Rd' ) )
          rm( list = proxyname )
        }

    }
    else{
        print('Only 0.5 degree is currently supported for FLR proxy generation.')
    }
}else{
    print('Emissions species not in ECLIPSE data')
}

#------------------------------------------------------------------------------
# Write out log
log_data <- data.frame( species = em, Generated_On = Sys.time() )
writeData( log_data, domain = 'LOGS', meta = FALSE,
           fn = paste0( em, '_FLR_proxy_log' ) )

logStop()
