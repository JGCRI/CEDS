# ------------------------------------------------------------------------------
# Program Name: G3.4.chunk_CH4_extended_aircraft_emissions.R
# Author(s): Leyang Feng
# Date Last Updated: May 4 2017
# Program Purpose: Generate multi-year emissions chunks for aircraft emissions.
# Input Files: CEDS_[em]_AIR_anthro_[year]_0.5_[CEDS_version].nc
# Output Files: FIN_OUT: [em]-em-AIR-anthro_input4MIPs_emissions_CMIP_CEDS-[CEDS_grid_version]_gn_[time_range].nc
# Notes:
# TODO:
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
  PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
  headers <- c( 'gridding_functions.R', 'data_functions.R', 'nc_generation_functions.R' )
  log_msg <- "Generates chunk NetCDF files for aircraft emissions" 
  script_name <- "G2.4.chunk_aircraft_emissions.R"

  source( paste0( PARAM_DIR, "header.R" ) )
  initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 0.5 Initialize gridding setups

  grid_resolution <- 0.5
  start_year <- 1850
  end_year <- 1960
  chunk_density <- 10 
  chunk_years <- 12
  CEDS_gridding_version <- '2017-05-18'

# basic start year/end year check 
  if ( end_year < start_year ) { stop( ' End year must not be earlier than start year. ') }

# calculate chunk start years
  total_years <- ( end_year - start_year ) / chunk_density + 1
  chunk_count <- ceiling( total_years / chunk_years  )  
# calculate chunk end years
  chunk_start_years <- unlist( lapply( 1 : chunk_count, function( i ) { chunk_start_years <- start_year + ( i - 1 ) * chunk_years * chunk_density } ) )
  chunk_end_years <- chunk_start_years + ( chunk_years * chunk_density - 1 * chunk_density ) 
  if ( chunk_end_years[ length( chunk_end_years ) ] > end_year ) { chunk_end_years[ length( chunk_end_years ) ] <- end_year }

# setup dirs
  input_dir <- filePath( 'MED_OUT', 'gridded-emissions/', extension = "" )
  output_dir <- filePath( 'FIN_OUT', 'gridded-emissions/', extension = "" )

# ------------------------------------------------------------------------------
# 1. Chunking 

# Define emissions species variable
  args_from_makefile <- commandArgs( TRUE )
  em <- args_from_makefile[ 1 ]
  if ( is.na( em ) ) em <- "CH4"

  MODULE_G <- "../code/module-G/"

# Start chunking    
  
  printLog( paste0( 'Start ', em, ' grids chunking from ', start_year, ' to ', end_year ) )

  for ( chunk_count_index in 1 : chunk_count ) {
  
    singleVarChunking_extendedCH4air( em, 
                                      grid_resolution, 
                                      chunk_start_years, 
                                      chunk_end_years, 
                                      chunk_density,
                                      chunk_count_index, 
                                      input_dir, 
                                      output_dir, 
                                      gridding_version = CEDS_gridding_version )
  } # END of for loop

# -----------------------------------------------------------------------------
# 2. Stop

# Every script should finish with this line:
  logStop()  



