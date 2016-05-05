# ------------------------------------------------------------------------------
# Program Name: G1.4.Biomass_emissions_gridding.R
# Author(s): Leyang Feng
# Date Last Updated: 19 April 2016
# Program Purpose: Produce gridded emissions using CEDS emissions final output for biomass only.      
# Input Files: [em]_total_CEDS_emissions.csv
# Output Files: CEDS_[em]_BIO_anthro_[year]_grid-resolution_version_mm_dd_yyyy.nc
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
    log_msg <- "CEDS emissions gridding for biomass" # First message to be printed to the log
    script_name <- "G1.4.Biomass_emissions_gridding.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 0.5 Initialize gridding setups

    gridding_initialize( grid_resolution = 0.5,
                         start_year = 1750,
                         end_year = 1850, load_masks = T, load_seasonality_profile = T )
    output_dir <- filePath( 'FIN_OUT', 'gridded-emissions/', extension = "" )
    proxy_dir <- filePath( "GRIDDING", "", extension="", domain_extension = "proxy/")
    
# ------------------------------------------------------------------------------
# 1. Define emission species and read in files
  
# Define emissions species variable
    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "BC"
    em_lc <- tolower( em ) 
  
    MODULE_G <- "../code/module-G/"
    
# read in the emission data
    #emissions <- readData( "MED_OUT", paste0( "F.", em, "_scaled_emissions" ) )
    emissions <- readData( "MED_OUT", paste0( em, '_total_CEDS_emissions'  ) )
# read in the country_location_index, which indicates the location of each country mask in the 'world' matrix 
    country_location_index <- 
      readData( "GRIDDING", domain_extension = "gridding_mappings/", file_name =  paste0( "country_location_index_", as.character( grid_resolution ) ) ) 
# read in the CEDS gridding sector mapping
    ceds_gridding_mapping<- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', file_name = 'CEDS_sector_to_gridding_sector_mapping' )
# read in the CEDS griding sector list 
    ceds_gridding_sector_list<- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', file_name = 'CEDS_gridding_sectors' )
# read in the country emission combine mapping ( could be empty if no combination is needed )
    country_combine_list <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', file_name = 'country_emission_combine_mapping' )
# ------------------------------------------------------------------------------
# 2. Pre-processing
# 2.1. Extract CEDS level 1 gridding sector list
    ceds_gridding_sector_list <- ceds_gridding_sector_list[ order( ceds_gridding_sector_list$CEDS_level_1_grids_abr ) , ]
    level1_sector_list <- ceds_gridding_sector_list$CEDS_level_1_grids_abr
# 2.2. Extract CEDS level 3 gridding sector list 
    ceds_gridding_sector_list <- ceds_gridding_sector_list[ order( ceds_gridding_sector_list$CEDS_level_3_grids_abr ) , ]
    level3_sector_list <- unique( ceds_gridding_sector_list$CEDS_level_3_grids_abr )
    level3_sector_longname_list <- unique( ceds_gridding_sector_list$CEDS_level_3_grids )
# 2.3. Extract only biomass emissions 
    emissions_bio <- emissions[ emissions$fuel == 'biomass', ]
# 2.4. Convert the emission data from CEDS working sectors to CEDS level 1 gridding sector 
    # special treatment for NMVOC 2L_other_process_emissions emissions
    X2L_sector_flag <- em == 'NMVOC' & any( '2L_Other-process-emissions' %in% emissions_bio$sector )
    if ( X2L_sector_flag == T ) {
      # Extract NMVOC 2L emission under global up in front 
      X2L_emissions <- subset( emissions_bio, 
                               iso == 'global' & sector == '2L_Other-process-emissions', 
                               colnames( emissions ) )
      # drop the global-2L line 
      emissions <- subset( emissions, 
                           !( iso == 'global' & sector == '2L_Other-process-emissions' ), 
                           colnames( emissions ) )
      }
    
    ceds_gridding_mapping_level1 <- ceds_gridding_mapping[ , c( 'CEDS_working_sector', 'CEDS_level_1_grids_abr' ) ]
    emissions_level1_sector <- merge( emissions_bio, ceds_gridding_mapping_level1, 
                                           by.x = 'sector', by.y = 'CEDS_working_sector' )
    # drop non-matched sectors
    emissions_level1_sector <- emissions_level1_sector[ !is.na( emissions_level1_sector$CEDS_level_1_grids_abr ), ]
    # aggregate the emissions by CEDS level1 gridding sectors 
    emissions_level1_sector <- aggregate( emissions_level1_sector[ , paste0( 'X', year_list ) ], 
                                               by = list( emissions_level1_sector$CEDS_level_1_grids_abr, 
                                                          emissions_level1_sector$iso ), 
                                               FUN = sum )
    # change column names
    colnames( emissions_level1_sector ) <- c( 'CEDS_grd_sector', 'iso', paste0( 'X', year_list ) ) 
    # remove AIR sector in data
    emissions_level1_sector <- emissions_level1_sector[ !emissions_level1_sector$CEDS_grd_sector == 'AIR', ]
# 2.5. Combine two or more country's emission into one country ( if necessary )
    emissions_level1_sector <- region_emCombine( emissions_level1_sector, country_combine_list )
# 2.6. remove non-exists sectors 
    level1_sector_list <- level1_sector_list[ which( level1_sector_list %in% unique( emissions_level1_sector$CEDS_grd_sector ) ) ]
# ------------------------------------------------------------------------------
# 3. Scalling and writing output data 
    # For now, the scaling routine uses nested for loops to go through every years
    # gases and sectors. May consider to take away for loop for sectors and keep year loops 
    # for future parallelization 
    for ( year in year_list ) {
      
      sectorl1_em_global_list <- grid_one_year( em, year, emissions_level1_sector, country_location_index, level1_sector_list, grid_resolution, mass = F )
      
      # when dealing with NMVOC, treat sector 2L under global differently then add back to shipping grids
      if ( X2L_sector_flag == T ) {
        X2L_em_golbal <- grid_2L( X2L_emissions, year )
        sectorl1_em_global_list$SHP_em_global <- sectorl1_em_global_list$SHP_em_global + X2L_em_golbal
        }

      
      # write netCDF to disk, each netCDF contains one year's all sectors' data for one gas 
      final_monthly_nc_output_biomass( output_dir, grid_resolution, year, em, level3_sector_list, level3_sector_longname_list, mass = F )

    }

# -----------------------------------------------------------------------------
# 4. Stop 
    
# Every script should finish with this line:
logStop()  

    
    