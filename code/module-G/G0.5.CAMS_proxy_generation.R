# ------------------------------------------------------------------------------
# Program Name: G0.5.CAMS_proxy_generation.R
# Authors: Hamza Ahsan
# Date Last Updated: 3/6/2025
# Program Purpose: Generate the proxies for SHP sector using CAMS shipping grids
# Input Files:  CAMS data
#
# Output Files: SHP proxy files
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Read in universal header files, support scripts, and start logging
headers <- 'common_data.R'
log_msg <- "Generating SHP proxy files from CAMS data"
source( paste0( PARAM_DIR, "header.R" ) )
initialize( "G0.5.CAMS_proxy_generation.R", log_msg, headers )

# 1. Set-up details for script ------------------------------------------------

# Define emissions species and gridding resolution
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
res <- as.numeric( args_from_makefile[ 2 ] )
if ( is.na( em ) ) em <- "N2O"
if ( is.na( res ) ) res <- 0.5 # default gridding resolution of 0.5

# check to make sure passed in emissions species is available in ECLIPSE
em_list <- c( 'SO2', 'NOx', 'NMVOC', 'OC', 'CO', 'CO2', 'CH4' )
if( em %in% em_list ){

    # 2. Setting paths ---------------------------------------------------------------

    # Read in file which defines paths
    pathmapping <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'proxy_generation_flow', meta = F) %>%
        dplyr::rename( 'emission' = em)

    # extracting path to input zip file from pathmapping
    input_folder <- pathmapping %>%
        dplyr::filter( emission == em, type == 'cams' ) %>%
        dplyr::select( path ) %>%
        pull()

    # extracting path to temporary directory from pathmapping
    temp_path <- pathmapping %>%
        dplyr::filter( type == 'temp' ) %>%
        dplyr::select( path ) %>%
        dplyr::mutate( path = gsub('<em>', em, path ) ) %>%
        pull()

    # extracting the path to proxy folder
    proxy_path <- pathmapping %>%
        dplyr::filter( emission == em, type == 'ceds_sector' ) %>%
        dplyr::select( path ) %>%
        pull()

    # Path to folder to save 0.1 degree resolution proxy files
    if( res == 0.1 ){
        proxy_path <- filePath( 'MED_OUT', 'final_generated_proxy_0.1', extension = '' )
        dir.create( proxy_path, showWarnings = FALSE )
    }


    # 3. Read in CAMS grids ---------------------------------------------------

    # unzip the downloaded file into temp dir
    zip_file <- list.files( input_folder )
    unzip(paste0( input_folder, '/', zip_file ), exdir = temp_path)


    cams_nc_process <- function( nc_file, name_parts, parts_length ){

        # finding the file year
        if(em == "NMVOC"){
            file_year <- name_parts[9]
        } else{
            file_year <- name_parts[8]
        }

        # Name the output file
        file_name <- paste0( em, '_', file_year, '_SHP' )

        # Read in grid
        raw_grid <- raster::raster( paste0( temp_path, '/', tools::file_path_sans_ext(zip_file), '/', nc_file ) )

        return( list(
            raw_grid = raw_grid,
            file_name = file_name
        ) )
    }

    # Reading, changing latitude bounds, aggregate, saving
    ncProcess <- function( nc_file ) {

        # extract info from file from its name by splitting into parts on '_'
        name_parts <- unlist( strsplit( tools::file_path_sans_ext(nc_file), split ='_' ) )
        parts_length <- length( name_parts )

        cams_data_list <- cams_nc_process( nc_file, name_parts, parts_length )
        file_name <- cams_data_list$file_name

        # Turn into matrix object
        grid <- raster::as.matrix( cams_data_list$raw_grid )

        # Not allowing downscale, but here starts aggregation process
        stopifnot( res >= 0.1)

        # Find the factor of aggregation
        fact <- res/0.1

        # Aggregate grid
        agg_grid <- raster::aggregate( raster::raster( grid ), fact = fact, fun = sum )

        # Save formatted/aggregated grid as .Rd file to pre-specified output file
        assign( file_name, raster::as.matrix( agg_grid ) )

        save( list = file_name, file = paste0( proxy_path, '/', file_name, '.Rd' ) )
        rm( list = file_name )

    }

    # Get list of NetCDF files to be processed
    nc_file_list <- list.files( path = paste0( temp_path, '/', tools::file_path_sans_ext(basename(zip_file))), pattern = '*.nc' )

    # Apply NetCDF processing function to each file
    invisible( lapply( nc_file_list, ncProcess ) )


    # 4. Empty the temp folder -----------------------------------------------------

    unlink( temp_path, recursive = T )

}else{
    print('Emissions species not in CAMS data')
}

logStop()
