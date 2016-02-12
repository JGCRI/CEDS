# ------------------------------------------------------------------------------
# Program Name: G1.1.Grid_emissions.R
# Author(s): Leyang Feng
# Date Last Updated: 18 January 2016
# Program Purpose: Produce gridded emissions using CEDS SO2 output.      
# Input Files: F.SO2_scaled_emissions.csv, country_location_index_05.csv, 
#              EDGAR_CEDS_SO2_mapping.csv, EDGAR_sectors.csv, [iso]_mask
# Output Files: CEDS_SO2_anthro_[year]_0.5_v1_01_18_2016.nc
# Notes: 1. Hard coded for SO2 but relatively easy to expand to other gases.
#        2. Only produce gridded emission for 1970 ~ 2014 
# TODO: 1. Modify the nest for loops for efficiency 
#       2. Plug in better emission-pattern check function  
#       3. Add summary print out function
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
    headers <- c( 'gridding_functions.R', 'data_functions.R', 'VOC_gridding_functions.R' ) # Any additional function files required
    log_msg <- "CEDS emissions gridding" # First message to be printed to the log
    script_name <- "G1.1.Grid_emissions.R"

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
    if ( is.na( em ) ) em <- "NMVOC"
    em_lc <- tolower( em ) 
  
    MODULE_G <- "../code/module-G/"
    
# read in the emission data
    emissions <- readData( "MED_OUT", paste0( "F.", em, "_scaled_emissions" ) )
# read in the country_location_index, which indicates the location of each country mask in the 'world' matrix 
    country_location_index <- 
      readData( "GRIDDING", domain_extension = "gridding_mappings/", file_name =  paste0( "country_location_index_", as.character( grid_resolution ) ) ) 
# read in the CEDS gridding sector mapping
    ceds_gridding_mapping<- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', file_name = 'CEDS_sector_to_gridding_sector_mapping' )
# read in the CEDS griding sector list 
    ceds_gridding_sector_list<- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', file_name = 'CEDS_gridding_sectors' )
# read in VOC ratios
    VOC_ratios <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', file_name = 'VOC_ratio_AllSectors' )
# read in VOC name weight list
    VOC_names <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', file_name = 'VOC_id_name_mapping' )
# ------------------------------------------------------------------------------
# 2. Pre-processing
# 2.1. Extract CEDS intermediate gridding sector list
    ceds_gridding_sector_list <- ceds_gridding_sector_list[ order( ceds_gridding_sector_list$CEDS_level_1_grids_abr ) , ]
    level1_sector_list <- ceds_gridding_sector_list$CEDS_level_1_grids_abr
# 2.2. Extract CEDS final gridding sector list 
    ceds_gridding_sector_list <- ceds_gridding_sector_list[ order( ceds_gridding_sector_list$CEDS_level_3_grids_abr ) , ]
    level3_sector_list <- unique( ceds_gridding_sector_list$CEDS_level_3_grids_abr )
    level3_sector_longname_list <- unique( ceds_gridding_sector_list$CEDS_level_3_grids )
# 2.3.  Convert the emission data from CEDS working sectors to CEDS intermediate gridding sector 
    ceds_gridding_mapping_level1 <- ceds_gridding_mapping[ , c( 'CEDS_working_sector', 'CEDS_level_1_grids_abr' ) ]
    emissions_level1_sector <- merge( emissions, ceds_gridding_mapping_level1, 
                                           by.x = 'sector', by.y = 'CEDS_working_sector' )
    # drop non-matched sectors
    emissions_level1_sector <- emissions_level1_sector[ !is.na( emissions_level1_sector$CEDS_level_1_grids_abr ), ]
    # aggregate the emissions by CEDS level1 gridding sectors 
    emissions_level1_sector <- aggregate( emissions_level1_sector[ , paste0( 'X', year_list ) ], 
                                               by = list( emissions_level1_sector$CEDS_level_1_grids_abr, 
                                                          emissions_level1_sector$iso ), 
                                               FUN = sum )
    # change column names
    colnames( emissions_level1_sector )[ 1 : 2 ] <- c( 'CEDS_grd_sector', 'iso' ) 
# 2.4. Extract VOC list
    VOC_list <- VOC_names$VOC_id
# ------------------------------------------------------------------------------
# 3. Scalling and writing output data 
    # For now, the scaling routine uses nested for loops to go through every years
    # gases and sectors. May consider to take away for loop for sectors and keep year loops 
    # for future parallelization 
    #for ( year in year_list ) {
    #for ( year in year_list[ ( length( year_list ) - 5 ): length( year_list) ] ) {  
    for ( year in '2000' ) { 
      grid_one_year_subVOCs( em, year, emissions_level1_sector, country_location_index, level1_sector_list, grid_resolution, VOC_ratios, VOC_list, mass = F )
    
      # write netCDF to disk, each netCDF contains one year's all sectors' data for one gas 
      output_dir <- filePath( 'MED_OUT', '', extension="")
      final_monthly_nc_output_subVOCs( output_dir, grid_resolution, year, em, level3_sector_list, level3_sector_longname_list, VOC_list, VOC_names, mass = F )

    }

# -----------------------------------------------------------------------------
# 4. Stop 
    
# Every script should finish with this line:
logStop()  

    
    