# Header ----------------------------------------------------------------------
# Program Name: G0.2.EDGAR_proxy_generation.R
# Authors: Noah Prime
# Date Last Updated: 7/23/21
# Program Purpose: Generate any proxy files which are dependent on EDGAR grids,
#                  though the FLR and RCO sectors will have proxy processed further,
#                  or extended in other scripts
# Input Files:  EDGAR global grids
#
# Output Files: EDGAR dependent proxy files
# ------------------------------------------------------------------------------


# 0.) Read in global settings and headers ----------------------------------
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Read in universal header files, support scripts, and start logging
headers <- c( 'utility_functions.R', 'needed_libraries.R', 'common_data.R', 'gridding_functions.R' )
log_msg <- "Extracting and processing EDGAR_v5 NetCDF files and generating proxy files..."
source( paste0( PARAM_DIR, "header.R" ) )
initialize( "G0.2.EDGAR_proxy_generation.R", log_msg, headers )

# 1.) Set-up details for script ------------------------------------------------

# Define emissions species and gridding resolution
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
res <- as.numeric( args_from_makefile[ 2 ] )
if ( is.na( em ) ) em <- "N2O"
if ( is.na( res ) ) res <- 0.5 # default gridding resolution of 0.5

if( em == 'CO2' ){
    EDGAR_end_year <- EDGAR_end_year_CO2
}

year_list <- EDGAR_start_year:EDGAR_end_year


# 2. Setting paths ---------------------------------------------------------------

# Read in file which defines paths
pathmapping <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'proxy_generation_flow', meta = F) %>%
    dplyr::rename( 'emission' = em)


# extracting path to input zip file from pathmapping
input_folder <- pathmapping %>%
    dplyr::filter( emission == em, type == 'input' ) %>%
    dplyr::select( path ) %>%
    pull()


# extracting path to temporary directory from pathmapping
temp_path <- pathmapping %>%
    dplyr::filter( type == 'temp' ) %>%
    dplyr::select( path ) %>%
    dplyr::mutate( path = gsub('<em>', em, path ) ) %>%
    pull()


# extracting path to output file from pathmapping
out_path <- pathmapping %>%
    dplyr::filter( emission == em, type == 'edgar_sector' ) %>%
    dplyr::select( path ) %>%
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


# Path to extended / completed point source yaml files
source_path <- filePath( 'MED_OUT', 'full_point_source_scaled_yml/', extension = "" )

# 3. Needed files -----------------------------------------------------------------

# Read in file name to EDGAR gridding sector mapping file
nc_namemapping <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/',
                            paste0( em, '_nc_filename_to_edgar_grd_sector.csv' ), meta = FALSE )

# Default proxy generation parameters
default_params <- readData( file_name = 'default_proxy_generation_params',
                            domain = 'GRIDDING', domain_extension = 'gridding_mappings/',
                            meta = FALSE )

# Delete old log proxy gen log file if exists
unlink( paste0( '../logs/', em, '_proxy_generation.csv' ) )


# 4. Read in EDGAR 5 grids ---------------------------------------------------

# unzip the downloaded file into temp dir
zip_list <- list.files( input_folder )
lapply( paste0( input_folder, '/', zip_list ), unzip, exdir = temp_path )



# 5. Read in YAML point source files -----------------------------------------------

# uses function from gridding_functions.R to read in point sources without the time series
source_data <- load_point_source_attributes( source_path, em )
source_data <- source_data %>%
    dplyr::mutate( latitude = as.numeric(latitude),
                   longitude = as.numeric(longitude) )



# 6. Process nc files -------------------------------------------------------

# Reading, removing point sources, changing longitude bounds, aggregate, saving
ncProcess <- function( nc_file ) {

    # extract info from file from its name by splitting into parts on '_'
    name_parts <- unlist( strsplit( nc_file, split ='_' ) )
    parts_length <- length( name_parts )

    # finding the file year
    file_year <- subset( name_parts, name_parts %in% year_list )

    # find the file sector (follows the year in the file name but could be separated by '_')
    file_year_string_position <- which(name_parts == file_year)
    sector_string_begins_position <- file_year_string_position + 1

    # This grabs the last part of the file name, beginning
    # with the sector. For example: "_IRO.0.1x0.1.nc"
    last_parts <- paste0( '_', paste( name_parts[ sector_string_begins_position : parts_length ], collapse = '_' ) )

    # If file corresponds to EDGAR gridding sector, we process the grid:
    if ( last_parts %in% nc_namemapping$EDGAR_nc_last_part ) {

        # Get EDGAR gridding sector
        sector <- nc_namemapping %>%
            dplyr::filter( EDGAR_nc_last_part == last_parts ) %>%
            dplyr::select( EDGAR_gridding_shortname ) %>%
            pull()

        # Name the output file
        file_name <- paste0( em, '_', file_year, '_', sector )

        # Read in grid
        raw_grid <- raster( paste0( temp_path, '/', nc_file ) )

        # Turn into matrix object, and order grid so that we use
        # longitude bounds of -180 to 180 rather than 0 to 360
        grid <- as.matrix( raw_grid )
        grid <- cbind( grid[ , ((1800) + 1) : (3600) ], grid[ , 1 : (1800) ] )

        # Sector generally written in upper
        sector <- toupper(sector)

        # Get the parameters for given sector
        params <- default_params %>%
            dplyr::filter( sector == !!sector )

        # Zero out cells from proxy grid
        grid <- remove_point_sources(grid, source_data, params, sector, res = 0.1)

        # Not allowing downscale, but here starts aggregation process
        stopifnot( res >= 0.1)

        # Find the factor of aggregation
        fact <- res/0.1

        # Aggregate grid
        agg_grid <- aggregate( raster( grid ), fact = fact, fun = sum )

        # Save formatted/aggregated grid as .Rd file to pre-specified output file
        assign( file_name, as.matrix( agg_grid ) )

        save( list = file_name, file = paste0( out_path, '/', file_name, '.Rd' ) )
        rm( list = file_name )

    }else{

        stop( "Files ending in ", last_parts, " for ", em, " do not match the corresponding ",
              "entry in ", em,"_nc_filename_to_edgar_grd_sector.csv. Please check spelling... ")

    }
}

# Get list of NetCDF files to be processed
nc_file_list <- list.files( temp_path, '*.nc' )

# Apply NetCDF processing function to each file
dir.create( out_path )
invisible( lapply( nc_file_list, ncProcess ) )


# 7. Empty the temp folder -----------------------------------------------------

unlink( temp_path, recursive = T )


# 8. Read in mapping files for proxy generation --------------------------------
sec_mapping <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'EDGAR_sector_replace_mapping', meta = F)
sector_list <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'CEDS_EDGAR_processing_sectors', meta = F)
sector_list <- sector_list$CEDS_EDGAR_processing_sector


# 9. Generate CEDS proxies -----------------------------------------------------

# Read in formatted proxy files and add them according to EDGAR sector to CEDS
# sector mapping
proxyGenerate <- function( grid_resolution = 0.5,
                           input_path,
                           out_path,
                           sec_mapping,
                           year_list,
                           sector_list ) {

    invisible( lapply( sector_list, function( sector ) {

        # get CEDS level 1 shortname based on sector and species
        sec_mapping_info <- sec_mapping %>%
            dplyr::filter( CEDS_level1_shortname == sector ) %>%
            dplyr::select( matches( em ) ) %>%
            na.omit() %>%
            pull()

        # Check that the sector exists for the given emission species
        if( length( sec_mapping_info ) == 0 ){

            printLog( "EDGAR v5", "information for", sector, "doesn't exist for", paste0( em, "..." ) )

            # Continue annual processing if sector exists
        } else{ invisible( lapply( year_list, function( year ) {

            # name of the proxy file
            output_name <- paste0( em, '_', year, '_', sector )

            # list of processed EDGAR grids to look for
            edgar_list <- c( )
            for ( sec in sec_mapping_info ) {
                edgar_name <- paste0( em, '_', year, '_', sec )
                edgar_list <- c( edgar_list, edgar_name )
            }


            # temporary matrix of zeros of correct dimension, and second copy
            # that won't change, to check if we actually created proxy or did nothing
            temp_proxy <- matrix( 0, 180 / grid_resolution, 360 / grid_resolution )
            do_nothing_check <- temp_proxy

            # create the final proxy files for each sector
            for ( edgar_grid in edgar_list ) {

                # Check if file exists to load grid and create proxy
                if( file.exists( paste0( input_path, '/', edgar_grid, '.Rd' ) ) ){
                    load( paste0( input_path, '/', edgar_grid, '.Rd' ) )
                    temp_edgar_grid <- get( edgar_grid )
                    temp_proxy <- temp_proxy + temp_edgar_grid
                }else{

                    # Warn user if file doesn't exist
                    printLog( edgar_grid, "does not exist. It is likely that information for this",
                              "em + sector combination does not exist for this year. Please check",
                              "the raw EDGAR grid inputs to confirm if this is expected, if you",
                              "haven't already..." )
                }

            }

            # Write output if any of the EDGAR grids which are aggregated to 1 CEDS proxy grid
            # existed for the given year
            # Note: Currently all cases where multiple EDGAR gridding sectors
            #       map to 1 CEDS sector have each EDGAR gridding sector provided for each year.
            #       If this changes in the future, one might want to change this to write the output
            #       only if all EDGAR gridding sectors are available for the year, to ensure consistency
            #       between years in CEDS gridding sectors. If this happens, then years missing one of the
            #       EDGAR gridding sectors can use the last available year where all EDGAR gridding
            #       sectors were provided (in CEDS-dev, defined in proxy_mapping.csv)
            if( !identical( temp_proxy, do_nothing_check ) ){
                ceds_proxy <- temp_proxy
                assign( output_name, ceds_proxy )
                save( list = output_name, file = paste0( out_path, '/', output_name, '.Rd' ) )

            }

        } ) ) }
    } ) )
}

# call the function to generate proxies
proxyGenerate( grid_resolution = res, out_path, proxy_path, sec_mapping, year_list, sector_list )

# Remove temp proxy files
unlink( out_path, recursive = T )


# 10.) Diagnostics ---------------------------------------------------------

# Write out point sources used in proxy generation
writeData( source_data, domain = 'LOGS', meta = FALSE,
           fn = paste0( em, '_proxy_generation' ) )

logStop()

