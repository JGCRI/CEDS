# ------------------------------------------------------------------------------
# Program Name: G1.3.Aircraft_emission_gridding.R
# Author(s): Leyang Feng
# Date Last Updated: 
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
    headers <- c( 'gridding_functions.R', 'data_functions.R' ) # Any additional function files required
    log_msg <- "CEDS emissions gridding" # First message to be printed to the log
    script_name <- "G1.3.Aircraft_emission_gridding.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 0.5 Initialize gridding setups

    gridding_initialize( grid_resolution = 0.5,
                         start_year = 1970,
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
    emissions <- readData( "MED_OUT", paste0( "F.", em, "_scaled_emissions" ) )

# ------------------------------------------------------------------------------
# 2. Pre-processing
    aircraft_sectors <- c( '1A3ai_International-aviation', '1A3aii_Domestic-aviation' )
    
    air_emissions <- subset( emissions, 
                             sector == '1A3ai_International-aviation' | sector == '1A3aii_Domestic-aviation', 
                             c( 'iso', 'sector', 'fuel', paste0( 'X', emissions_years ) ) )    
    air_emissions <- cbind( CEDS_gridding_sector = 'AIR', air_emissions )
    air_emissions <- aggregate( air_emissions[ , paste0( 'X', emissions_years ) ],
                                by = list( air_emissions$CEDS_gridding_sector ),
                                FUN = sum ) 
    colnames( air_emissions) [ 1 ] <- 'sector' 
# ------------------------------------------------------------------------------
# 3. Scalling and writing output data 
    # For now, the scaling routine uses nested for loops to go through every years
    # gases and sectors. May consider to take away for loop for sectors and keep year loops 
    # for future parallelization 
    #for ( year in year_list ) {
    #for ( year in year_list[ ( length( year_list ) - 5 ): length( year_list) ] ) {  
    for ( year in '2000' ) {  
      
      grid_one_year_air( em, year, air_emissions, grid_resolution, sector = 'AIR', mass = F )
      
      # write netCDF to disk, each netCDF contains one year's all sectors' data for one gas 
      output_dir <- filePath( 'MED_OUT', '', extension="")
      final_monthly_nc_output_air( output_dir, grid_resolution, year, em, sector = 'AIR', sector_long = 'Aircraft', mass = F )

    }

# -----------------------------------------------------------------------------
# 4. Stop 
    
# Every script should finish with this line:
logStop()  

    
    