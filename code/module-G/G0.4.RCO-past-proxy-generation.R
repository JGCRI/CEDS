# ------------------------------------------------------------------------------
# Program Name: G0.4.RCO-past-proxy-generation.R
# Authors: Noah Prime
# Date Last Updated: 7/23/21
# Program Purpose: Generate the RCO proxy files for years 1900-1969
# Input Files:  RCO proxy files for years 1970-2015 and population proxy files
#
# Output Files: RCO procy files for years 1900-1969
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# 0.) Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Read in universal header files, support scripts, and start logging
headers <- c( 'utility_functions.R', 'needed_libraries.R', 'common_data.R' )
log_msg <- "Generating past RCO proxy files"
source( paste0( PARAM_DIR, "header.R" ) )
initialize( "G0.4.RCO-past-proxy-generation.R", log_msg, headers )

# ------------------------------------------------------------------------------
# 1.) Set-up details for script

# Define emissions species and gridding resolution
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
res <- as.numeric( args_from_makefile[ 2 ] )
if ( is.na( em ) ) em <- "SO2"
if ( is.na( res ) ) res <- 0.5 # default gridding resolution of 0.5

# em supported by EDGAR v5
em_list <- c( 'CH4', 'CO', 'CO2', 'NH3', 'NMVOC', 'NOx', 'SO2', 'OC', 'BC', 'N2O' )
if( em %in% em_list ){
    # Resolution of 0.5 supported for extension
    if( res == 0.5 ){
        # ------------------------------------------------------------------------------
        # 2. set dirs

        # Read in file which defines paths
        pathmapping <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'proxy_generation_flow', meta = F) %>%
          dplyr::rename( 'emission' = em)

        # extracting path to final proxy files from pathmapping
        final_path <- pathmapping %>%
          dplyr::filter( emission == em, type == 'ceds_sector' ) %>%
          dplyr::select( path ) %>%
          pull()

        # Path to folder to save 0.1 degree resolution proxy files
        if( res == 0.1 ){
            stop('Only 0.5 degree is currently supported for RCO extension.')
            #proxy_path <- filePath( 'MED_OUT', 'final_generated_proxy_0.1', extension = '' )
        }

        # population proxy files in this directory
        proxy_backup_dir    <- filePath( "GRIDDING", "proxy-backup/",          extension = "" )


        # ------------------------------------------------------------------------------
        # 3. start

        past_year_list <- 1900:1969
        pop_year <- past_year_list
        edgar_proxy_year <- '1970'

        # load EDGAR proxy
        edgar_rco_pattern <- '_1970_RCO'
        edgar_path <- final_path
        edgar_proxy_name <- paste0( em, edgar_rco_pattern )
        load( paste0( edgar_path, '/', edgar_proxy_name, '.Rd' ) )
        edgar_proxy <- get( edgar_proxy_name )
        rm( list = edgar_proxy_name )

        # proxy normalized
        edgar_proxy_norm <- edgar_proxy / sum( edgar_proxy )

        for ( year in past_year_list ) {
          pop_name <- paste0( 'population_', year )
          load( paste0( proxy_backup_dir, '/', pop_name, '.Rd' ) )
          population <- get( pop_name )
          rm( list = pop_name )

          population_sum <- sum( population )
          population_norm <- population / population_sum

          # blending
          proxy_weight <- which( past_year_list %in% year ) / length( past_year_list )
          pop_weight <- 1 - proxy_weight
          new_proxy_norm <- edgar_proxy_norm * proxy_weight + population_norm * pop_weight
          new_proxy <- new_proxy_norm * population_sum

          new_name <- paste0( em, '_', year, '_RCO.Rd' )
          assign( new_name, new_proxy )
          save( list = new_name, file = paste0( final_path, '/', new_name ) )
          rm( list = new_name )

        }


        # ------------------------------------------------------------------------------
        # Write out log
        log_data <- data.frame( species = em, Generated_On = Sys.time() )
        writeData( log_data, domain = 'LOGS', meta = FALSE,
                   fn = paste0( em, '_RCO_proxy_extension_log' ) )
    }else{
        print('Only 0.5 degree is currently supported for FLR proxy generation.')
    }
}else{
    print('Emissions species not in ECLIPSE data')
}

logStop()


