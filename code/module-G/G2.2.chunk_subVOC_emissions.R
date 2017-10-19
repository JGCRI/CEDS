# ------------------------------------------------------------------------------
# Program Name: G2.2.chunk_subVOC_emissions.R
# Author(s): Leyang Feng
# Date Last Updated: May 4 2017
# Program Purpose: Generate multi-year emissions chunks for subVOC emissions.        
# Input Files: CEDS_[VOCID]_anthro_[year]_0.5_[CEDS_version].nc
# Output Files: FIN_OUT: [VOCID]-acids-em-speciated-VOC-anthro_input4MIPs_emissions_CMIP_CEDS-[CEDS_grid_version]_supplement-data_gn_[time_range].nc
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
  if ( length(wd) > 0 ) {
    setwd( wd[1] )
    break
  }
}
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files - 
# provides logging, file support, and system functions - and start the script log.
headers <- c( 'gridding_functions.R', 'data_functions.R', 'nc_generation_functions.R' ) # Any additional function files required
log_msg <- "Generates chunk NetCDF files for subVOC emissions" # First message to be printed to the log
script_name <- "G2.2.chunk_subVOC_emissions.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 0.5 Initialize gridding setups

grid_resolution <- 0.5
start_year <- 1750 
end_year <- 1850
chunk_years <- 50
CEDS_gridding_version <- '2017-05-18'

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

# VOC name mapping 
VOC_names <- readData( domain = 'GRIDDING', domain_extension = "gridding_mappings/", 'VOC_id_name_mapping' )

# ------------------------------------------------------------------------------
# 1. For each chunk count generate nc read in list 

# Define emissions species variable
args_from_makefile <- commandArgs( TRUE )
VOC_em <- args_from_makefile[ 1 ]
if ( is.na( VOC_em ) ) VOC_em <- "VOC01"

MODULE_G <- "../code/module-G/"

printLog( paste0( 'Start ', VOC_em, ' grids chunking from ', start_year, ' to ', end_year ) )

for ( chunk_count_index in 1 : chunk_count ) {
  
  singleVarChunking_subVOCemissions( VOC_em, 
                                     grid_resolution, 
                                     chunk_start_years, 
                                     chunk_end_years, 
                                     chunk_count_index, 
                                     input_dir, 
                                     output_dir, 
                                     gridding_version = CEDS_gridding_version,
                                     VOC_names )
  }


# -----------------------------------------------------------------------------
# 2. Stop 

# Every script should finish with this line:
logStop()  



