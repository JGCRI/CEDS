# ------------------------------------------------------------------------------
# Program Name: G1.3.Aircraft_emission_gridding.R
# Author(s): Leyang Feng
# Date Last Updated: March 29 2016 
# Program Purpose:  Produce gridded emissions for Aircraft sector using CEDS emissions final output.         
# Input Files: S.[em]_Extended_CEDS_Emissions.csv 
# Output Files: CEDS_[em]_AIR_anthro_year_grid-resolution_version_mm_dd_yyyy.csv
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
    log_msg <- "CEDS emissions gridding" # First message to be printed to the log
    script_name <- "G1.3.Aircraft_emission_gridding.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 0.5 Initialize gridding setups
    output_dir <- filePath( 'MED_OUT', 'gridded-emissions/', extension="" )
	
    gridding_initialize( grid_resolution = 0.5,
                         start_year = 1851,
                         end_year = 2014, load_masks = T, load_seasonality_profile = T )
    
# ------------------------------------------------------------------------------
# 1. Define emission species and read in files
  
# Define emissions species variable
    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "SO2"
    em_lc <- tolower( em ) 
  
    MODULE_G <- "../code/module-G/"
    
# read in the emission data
    target_filename <- list.files( filePath( "FIN_OUT", "", extension = "", domain_extension = "current-versions/" ),
                                   pattern = paste0( ".*_", em, '_emissions_by_country_CEDS_secto.*' ) )
    target_filename <- substr( target_filename, 1, ( nchar( target_filename ) - 4 ) )
    emissions <- readData( "FIN_OUT", domain_extension = "current-versions/", target_filename )
# read in the CEDS gridding sector mapping
    ceds_gridding_mapping<- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', file_name = 'CEDS_sector_to_gridding_sector_mapping' )
# read in the proxy mapping 
    proxy_mapping <- readData( domain = "GRIDDING", domain_extension = "gridding_mappings/", 'proxy_mapping' )
# read in the seasonality mapping 
    seasonality_mapping <- readData( domain = "GRIDDING", domain_extension = "gridding_mappings/", 'seasonality_mapping' )
    
# ------------------------------------------------------------------------------
# 2. Pre-processing
    # extract aircraft related CEDS working sectors for ceds gridding sector mapping
    air_mapping <- ceds_gridding_mapping[ ceds_gridding_mapping$CEDS_level_1_grids_abr == 'AIR', ] 
    aircraft_sectors <- unique( air_mapping$CEDS_working_sector )
    aircraft_sectors <- aircraft_sectors[ ! is.na( aircraft_sectors ) ]
    # extract aircraft emissions only
    air_emissions <- subset( emissions, 
                             sector == '1A3ai_International-aviation' | sector == '1A3aii_Domestic-aviation', 
                             c( 'iso', 'sector', paste0( 'X', year_list ) ) )    
    # sum up all aircraft emissions 
    air_emissions <- cbind( CEDS_gridding_sector = 'AIR', air_emissions )
    air_emissions <- aggregate( air_emissions[ , paste0( 'X', year_list ) ],
                                by = list( air_emissions$CEDS_gridding_sector ),
                                FUN = sum ) 
    colnames( air_emissions) <- c( 'sector', paste0( 'X', year_list ) ) 
    
# ------------------------------------------------------------------------------
# 3. Scalling and writing output data 
    # For now, the scaling routine uses nested for loops to go through every years
    # gases and sectors. May consider to take away for loop for sectors and keep year loops 
    # for future parallelization 
    for ( year in year_list ) {
      
      AIR_em_global <- grid_one_year_air( em, year, air_emissions, grid_resolution, sector = 'AIR', mass = F )
      
      # write netCDF to disk, each netCDF contains one year's all sectors' data for one gas 

      final_monthly_nc_output_air( output_dir, grid_resolution, year, em, sector = 'AIR', sector_long = 'Aircraft', mass = F )

    }

# -----------------------------------------------------------------------------
# 4. Stop 
    
# Every script should finish with this line:
logStop()  

    
    