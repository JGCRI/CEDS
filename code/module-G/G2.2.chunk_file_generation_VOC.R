# ------------------------------------------------------------------------------
# Program Name: G2.2.chunk_file_generation_VOC.R
# Author(s): Leyang Feng
# Date Last Updated: 10 June 2016
# Program Purpose:      
# Input Files: 
# Output Files: 
# Notes: 
# TODO: 
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Set working directory to the CEDS "input" directory and define PARAM_DIR as the
# location of the CEDS "parameters" directory, relative to the new working directory.
dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
for ( i in 1:length( dirs ) ) {
  setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
  wd <- grep( 'CEDS/input', list.dirs(), value = T )
  if ( length( wd ) > 0 ) {
    setwd( wd[ 1 ] )
    break
  }
}
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files - 
# provides logging, file support, and system functions - and start the script log.
headers <- c( 'gridding_functions.R', 'data_functions.R', 'nc_generation_functions.R' ) # Any additional function files required
log_msg <- "Generates chunk NetCDF files for speciated VOCs." # First message to be printed to the log
script_name <- "G2.2.chunk_file_generation_VOC.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 0.5 Initialize gridding setups

grid_resolution <- 0.5
start_year <- 1900
end_year <- 2014
chunk_years <- 50
VOC_chunk <- T
CEDS_version_value <- '2016-07-26'

# basic start year/end year check 
# if ( start_year %% chunk_years != 0 ) { stop( 'Start year must be a multiple of the chunk_years. ' ) }
if ( end_year < start_year ) { stop( ' End year must not be earlier than start year. ') }

# calculate chunk start years
total_years <- end_year - start_year + 1
chunk_count <- ceiling( total_years / chunk_years  )  
# calculate chunk end years
chunk_start_years <- unlist( lapply( 1 : chunk_count, function( i ) { chunk_start_years <- start_year + ( i - 1 ) * chunk_years } ) )
chunk_end_years <- chunk_start_years + ( chunk_years - 1 ) 
if ( chunk_end_years[ length( chunk_end_years ) ] > end_year ) { chunk_end_years[ length( chunk_end_years ) ] <- end_year }

# define dirs
input_dir <- filePath( 'MED_OUT', 'gridded-emissions/', extension = "" )
output_dir <- filePath( 'FIN_OUT', 'gridded-emissions/', extension = "" )

# ------------------------------------------------------------------------------
# 1. read in VOC mapping 
VOC_mapping <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', file_name = 'VOC_id_name_mapping' )

# ------------------------------------------------------------------------------
# 2. For each chunk count generate nc read in list 

# Define emissions species variable
em <- 'NMVOC'
# define gridtype -- VOC
args_from_makefile <- commandArgs( TRUE )
VOC_type <- args_from_makefile[ 1 ]
if ( is.na( VOC_type ) )  VOC_type <- "VOC01"

MODULE_G <- "../code/module-G/"

# generate gridtype for VOC
VOC_short_name <- VOC_mapping[ VOC_mapping$VOC_id == VOC_type, 'VOC_name' ]
VOC_short_name <- substring( VOC_short_name, 1, 10 )
VOC_gridtype <- paste0( VOC_type, '-', VOC_short_name )


for ( chunk_count_index in 1 : chunk_count ) {
  
  annual2chunk( em, grid_resolution, gridtype = VOC_gridtype, 
                chunk_start_years, chunk_end_years, chunk_count_index, 
                input_dir, output_dir, 
                VOC_chunk, VOC_info = VOC_mapping, CEDS_version = CEDS_version_value,
                output_format = 'singleVar' )  
}

# -----------------------------------------------------------------------------
# 3. Stop 

# Every script should finish with this line:
logStop()  