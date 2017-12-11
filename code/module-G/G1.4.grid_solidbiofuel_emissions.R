# ------------------------------------------------------------------------------
# Program Name: G1.4.grid_solidbiofuel_emissions.R
# Author(s): Leyang Feng
# Date Last Updated: May 1, 2017
# Program Purpose: Grid aggregated biomass emissions into NetCDF grids for bulk emissions (excluding AIR)
# Input Files: [em]_total_CEDS_emissions.csv
# Output Files: MED_OUT: CEDS_[em]_solidbiofuel_anthro_[year]_0.5_[CEDS_version].nc; CEDS_[em]_solidbiofuel_anthro_[year]_0.5_[CEDS_version].csv
#               DIAG_OUT: G.[em]_solidfuel_emissions_checksum_comparison_diff.csv; G.[em]_solidfuel_emissions_checksum_comparison_per.csv
# Notes: 
# TODO: see code review ###TODO
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
  PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
  headers <- c( 'gridding_functions.R', 'data_functions.R', 'nc_generation_functions.R' ) 
  log_msg <- "Gridding anthropogenic biomass emissions (excluding AIR) " 
  script_name <- "G1.4.grid_solidbiofuel_emissions.R"

  source( paste0( PARAM_DIR, "header.R" ) )
  initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 0.5 Initialize gridding setups

# Define emissions species variable
  args_from_makefile <- commandArgs( TRUE )
  em <- args_from_makefile[ 1 ]
  if ( is.na( em ) ) em <- "SO2"

  MODULE_G <- "../code/module-G/"

# Set up directories  
  	
  output_dir <- filePath( 'MED_OUT', 'gridded-emissions/', extension = "" )
  total_grid_dir <- filePath( 'DIAG_OUT', 'total-emissions-grids/', extension = "" )
  proxy_dir <- filePath( "GRIDDING", "", extension = "", domain_extension = "proxy/" )
  proxy_backup_dir <- filePath( "GRIDDING", "", extension = "", domain_extension = "proxy_backup/")
  mask_dir <- filePath( "GRIDDING", "", extension = "", domain_extension = "mask/")
  seasonality_dir <- filePath( "GRIDDING", "", extension = "", domain_extension = "seasonality/" )
  final_emissions_dir <- filePath( "FIN_OUT", "", extension = "", domain_extension = "current-versions/" )

# Initialize the gridding parameters  
  	  
  gridding_initialize( grid_resolution = 0.5,
                       start_year = 1750,
                       end_year = 2014, 
                       load_masks = T, 
                       load_seasonality_profile = T )
	
# ------------------------------------------------------------------------------
# 1. Read in files

# read in the emission data
  emissions <- readData( "MED_OUT", paste0( em, '_total_CEDS_emissions'  ) )

# read in some mapping files 
# read in the region location index, which indicates the location of each region mask in the 'world' matrix 
  location_index <- readData( "GRIDDING", domain_extension = "gridding_mappings/", file_name =  "country_location_index_05" ) 
# read in the CEDS gridding sector mapping
  ceds_gridding_mapping <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', file_name = 'CEDS_sector_to_gridding_sector_mapping' )
# read in the proxy mapping 
  proxy_mapping <- readData( domain = "GRIDDING", domain_extension = "gridding_mappings/", 'proxy_mapping' )
# read in the seasonality mapping 
  seasonality_mapping <- readData( domain = "GRIDDING", domain_extension = "gridding_mappings/", 'seasonality_mapping' )
# read in the proxy substitution mapping 
  proxy_substitution_mapping <- readData( domain = 'GRIDDING', domain_extension = "gridding_mappings/", 'proxy_subsititution_mapping' )
# read in CEDS final gridding sector name list 
  sector_name_mapping <- readData( domain = 'GRIDDING', domain_extension = "gridding_mappings/", 'CEDS_gridding_sectors' )
  sector_name_mapping <- unique( sector_name_mapping[ , c( "CEDS_fin_sector", "CEDS_fin_sector_short" ) ] )
    
# ------------------------------------------------------------------------------
# 2. Pre-processing
  
###TODO: for the below data processing part, merging into tidyverse  
  
# Convert the emission data from CEDS working sectors to CEDS level 1 gridding sector 
  ceds_gridding_mapping_int <- ceds_gridding_mapping[ , c( 'CEDS_working_sector', 'CEDS_int_gridding_sector_short' ) ]
    
  gridding_emissions <- emissions[ emissions$fuel == 'biomass', ]
  gridding_emissions$fuel <- NULL
  gridding_emissions <- merge( gridding_emissions, ceds_gridding_mapping_int, by.x = 'sector', by.y = 'CEDS_working_sector' )
  
# drop non-matched sectors
  gridding_emissions <- gridding_emissions[ !is.na( gridding_emissions$CEDS_int_gridding_sector_short ), ]
# aggregate the emissions at gridding sectors 
  gridding_emissions <- aggregate( gridding_emissions[ , paste0( 'X', year_list ) ], 
                                   by = list( gridding_emissions$iso, gridding_emissions$CEDS_int_gridding_sector_short ),
                                   FUN = sum )
# change column names
  colnames( gridding_emissions ) <- c( 'iso', 'sector', paste0( 'X', year_list ) ) 
# remove AIR sector in data
  gridding_emissions <- gridding_emissions[ !gridding_emissions$sector == 'AIR', ]
    
# ------------------------------------------------------------------------------
# 3. Gridding and writing output data 
  
# For now, the gridding routine uses nested for loops to go through every years
# gases and sectors. May consider to take away for loop for sectors and keep year loops 
# for future parallelization 
  
  printLog( paste0( 'Start ', em, ' gridding for each year ' ) )
    
  for ( year in year_list ) {
   
  # grid one years emissions    
       
    int_grids_list <- grid_one_year( em, 
                                     year, 
                                     grid_resolution,
                                     gridding_emissions, 
                                     location_index, 
                                     proxy_mapping, 
                                     proxy_substitution_mapping )
  
  # generate nc file for gridded one years emissions,
  # a checksum file is also generated along with the nc file 
  # which summarize the emissions in mass by sector by month.    
    
    generate_final_grids_nc_solidbiofuel( int_grids_list,
                                          output_dir, 
                                          grid_resolution, 
                                          year, 
                                          em, 
                                          sector_name_mapping,
                                          seasonality_mapping )
    
    } # END of for loop
  
# -----------------------------------------------------------------------------
# 4. Checksum
    printLog( 'Start checksum check' )

    ceds_gridding_mapping_fin <- ceds_gridding_mapping[ , c( 'CEDS_int_gridding_sector_short', 'CEDS_final_gridding_sector_short' ) ]
    ceds_gridding_mapping_fin <- unique( ceds_gridding_mapping_fin )
    gridding_emissions_fin <- merge( gridding_emissions, ceds_gridding_mapping_fin,
                                     by.x = 'sector', by.y = 'CEDS_int_gridding_sector_short', all.x = T )
    gridding_emissions_fin <- aggregate( gridding_emissions_fin[ paste0( 'X', year_list ) ],
                                         by = list( gridding_emissions_fin$CEDS_final_gridding_sector_short ),
                                         FUN = sum )
    colnames( gridding_emissions_fin ) <- c( 'sector', paste0( 'X', year_list ) )
    # manually add in all 0 lines foe AGR, SLV, and WST because they do have emissions from fuel biomass
    temp_matrix <- matrix( 0, 3, length( year_list ) )
    temp_df <- data.frame( temp_matrix )
    temp_df$sector <- c( 'AGR', 'SLV', 'WST' )
    colnames( temp_df ) <- c( paste0( 'X', year_list ), 'sector' )
    temp_df <- temp_df[ ,c( 'sector', paste0( 'X', year_list ) ) ]
    gridding_emissions_fin <- rbind( gridding_emissions_fin, temp_df)
    gridding_emissions_fin <- gridding_emissions_fin[ order( gridding_emissions_fin$sector ), ]

    checksum_file_list <- list.files( path = output_dir, pattern = paste0( '_', em, '_solidbiofuel_anthro_' ) )
    checksum_file_list <- grep( '.csv', checksum_file_list, fixed = T, value = T )
    checksum_res_list <- lapply( checksum_file_list, function( file_name ) {
      temp_csv <- read.csv( paste0( output_dir, file_name  ), stringsAsFactors = F )
      } )
    checksum_df <- do.call( 'rbind', checksum_res_list )
    checksum_df <- aggregate( checksum_df$value, by = list( checksum_df$sector, checksum_df$year ), FUN = sum  )
    colnames( checksum_df ) <- c( 'sector', 'year', 'value' )
    checksum_df <- cast( checksum_df, sector ~ year )
    colnames( checksum_df ) <- c( 'sector', paste0( 'X', year_list ) )
    checksum_df <- checksum_df[ order( checksum_df$sector ), ]

    diag_diff_df <- cbind( checksum_df$sector, abs( gridding_emissions_fin[ paste0( 'X', year_list ) ] - checksum_df[ paste0( 'X', year_list ) ] ) )
    diag_per_df <- cbind( checksum_df$sector, ( diag_diff_df[ paste0( 'X', year_list ) ] / gridding_emissions_fin[ paste0( 'X', year_list ) ] ) * 100 )
    diag_per_df[ is.na( diag_per_df ) ] <- NA

# -----------------------------------------------------------------------------
# 5. Write-out and Stop
    out_name <- paste0( 'G.', em, '_solidfuel_emissions_checksum_comparison_diff' )
    writeData( diag_diff_df, "DIAG_OUT", out_name )
    out_name <- paste0( 'G.', em, '_solidfuel_emissions_checksum_comparison_per' )
    writeData( diag_per_df, "DIAG_OUT", out_name )

# Every script should finish with this line:
logStop()
