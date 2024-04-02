# ------------------------------------------------------------------------------
# Program Name: G0.1.generate_proxy.R
# Authors: Noah Prime
# Date Last Updated: July 7, 2021
# Program Purpose: Generate any proxy files which are not specific to any emission species.
#                  (For now just capped_rural_population_<year>.Rd for WST sector)
# Input Files:  GRIDDING:   proxy/[relevant proxy files]
#                           proxy-backup/[relevant proxy-backup files]
#                           gridding_mappings/proxy_mapping.csv
#
# Output Files: GRIDDING:   new proxy files in proxy and proxy-backup folders
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# 0.) Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Read in universal header files, support scripts, and start logging
headers <- c( 'gridding_functions.R' )
log_msg <- "Generating species independent proxy files for gridding routine"
source( paste0( PARAM_DIR, "header.R" ) )
initialize( "G0.1.generate_proxy.R", log_msg, headers )

# ------------------------------------------------------------------------------
# 0.5.) Set-up details for script

# Define emissions species and gridding resolution
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
res <- as.numeric( args_from_makefile[ 2 ] )
if ( is.na( em ) ) em <- "BC"
if ( is.na( res ) ) res <- 0.5 # default gridding resolution of 0.5


# Set up directories
proxy_dir           <- filePath( "MED_OUT",  "final_generated_proxy/", extension = "" )
backup_proxy_dir    <- filePath( "GRIDDING", "proxy-backup/",          extension = "" )
if(res == 0.1){
    proxy_dir           <- filePath( "MED_OUT",  "final_generated_proxy_0.1/", extension = "" )
    backup_proxy_dir    <- filePath( "GRIDDING", "proxy-backup-01/",          extension = "" )
}


# load proxy_mapping
proxy_mapping <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'proxy_mapping', meta = FALSE )

# grid area matrix
global_grid_area <- grid_area( res, all_lon = T )

# meta data initialization
meta_df <- data.frame( proxy_name = character(),
                       backup = character(),
                       emission_specific = character(),
                       emission_species = character(),
                       date_time = character())

# function for adding proxy meta data
add_proxy_to_meta_data <- function( proxy_name,
                                    backup = "F",
                                    emission_specific = "F",
                                    emission_species = "NA",
                                    date_time){
    meta_df <<- meta_df %>%
        add_row( proxy_name, backup, emission_specific, emission_species, date_time)
}

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# 1.)
# GENERATING RURAL POPULATION PROXY FOR WST SECTOR
#     - uses population proxy and applies a cut-off

printLog("Generating capped rural population proxy files for use in WST sector")


# dates for which we have population proxy
population_proxy_years <- proxy_mapping %>%
    dplyr::filter( sector == 'WST' ) %>%
    dplyr::select( year ) %>%
    dplyr::distinct() %>%
    pull()



# For each year we have population proxy, generate rural population and add it to
# proxy files, if file does not already exist
for(year in population_proxy_years){

    # full path and name of output file
    rural_pop_file <- paste0(proxy_dir, 'capped_rural_population_', year, '.Rd')

    # check to see if file exists
    if( file.exists( rural_pop_file ) ){

        # get date/time file was created
        file_date_time <- file.info( rural_pop_file )$ctime

        # add info for existing proxy file to meta data
        add_proxy_to_meta_data( proxy_name = paste0( 'capped_rural_population_', year ),
                                date_time = as.character( file_date_time ) )


    }else{ # else generate proxy

        generate_rural_population_proxy( year, global_grid_area, proxy_dir, backup_proxy_dir )

        # add info for new proxy file to meta data
        add_proxy_to_meta_data( proxy_name = paste0( 'capped_rural_population_', year ),
                                date_time = as.character( Sys.time() ) )

    }
}

printLog("Finished generating capped rural population proxy files for use in WST sector")


#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# 2.) Saving meta data and closing log

# Add note to meta data to specify what the extra file is used for (targeting in makefile)
meta_names <- c( "Data.Type","Source.Comment" )

meta_note <- c( "Proxy file info",
                paste0( "Used to be a reliable file that is always created during proxy generation ",
                        "to be used for targeting in the makefile." ) )

source_info <- 'G1.0.generate_proxy.R'

addMetaData( meta_note, meta_names, source_info )

# save proxy info to intermediate output directory
writeData(meta_df, 'MED_OUT', 'general_proxy_file_info')

# stop log
logStop()
