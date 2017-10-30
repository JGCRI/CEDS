# ------------------------------------------------------------------------------
# Program Name: G1.2.grid_subVOC_emissions.R
# Author(s): Leyang Feng
# Date Last Updated: May 4, 2017
# Program Purpose: Grid aggregated emissions into NetCDF grids for subVOCs
# Input Files: CEDS_NMVOC_emissions_by_country_CEDS_sector_[CEDS_version].csv
# Output Files: MED_OUT: CEDS_[VOCID]_anthro_[year]_0.5_[CEDS_version].nc; CEDS_[VOCID]_anthro_[year]_0.5_[CEDS_version].csv
#               DIAG_OUT: G.[VOCID]_bulk_emissions_checksum_comparison_per.csv; G.[VOCID]_bulk_emissions_checksum_comparison_diff.csv
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
  log_msg <- "VOC speciation gridding " 
  script_name <- "G1.2.grid_subVOC_emissions.R"

  source( paste0( PARAM_DIR, "header.R" ) )
  initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 0.5 Initialize gridding setups

# Define emissions species variable
  args_from_makefile <- commandArgs( TRUE )
  VOC_em <- args_from_makefile[ 1 ]
  if ( is.na( VOC_em ) ) VOC_em <- "VOC01"
    
  em <- 'NMVOC'
    
  MODULE_G <- "../code/module-G/"

# Set up directories
  
  output_dir <- filePath( 'MED_OUT', 'gridded-emissions/', extension = "" )
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
  target_filename <- list.files( final_emissions_dir,
                                 pattern = paste0( ".*_", em, '_emissions_by_country_CEDS_sector.*' ) )
  target_filename <- substr( target_filename, 1, ( nchar( target_filename ) - 4 ) )
  emissions <- readData( "FIN_OUT", domain_extension = "current-versions/", target_filename )

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
# VOC ratios
  VOC_ratios <- readData( domain = 'GRIDDING', domain_extension = "gridding_mappings/", 'VOC_ratio_AllSectors' )
# VOC name mapping 
  VOC_names <- readData( domain = 'GRIDDING', domain_extension = "gridding_mappings/", 'VOC_id_name_mapping' )

# ------------------------------------------------------------------------------
# 2. Pre-processing
  
###TODO: for the below data processing part, merging into tidyverse
  
# Convert the emission data from CEDS working sectors to CEDS level 1 gridding sector 
    ceds_gridding_mapping_int <- ceds_gridding_mapping[ , c( 'CEDS_working_sector', 'CEDS_int_gridding_sector_short' ) ]
    gridding_emissions <- merge( emissions, ceds_gridding_mapping_int, by.x = 'sector', by.y = 'CEDS_working_sector' )
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
# apply VOC ratio
    gridding_emissions <- merge( gridding_emissions, VOC_ratios[ , c( 'iso', 'sector', VOC_em ) ], by = c( 'iso', 'sector' ) )
    ratio_mat <- matrix( rep( unlist( gridding_emissions[ VOC_em ] ), length( year_list ) ), ncol = length( year_list ) )
    gridding_emissions[ paste0( 'X', year_list ) ] <- gridding_emissions[ paste0( 'X', year_list ) ] * ratio_mat
    gridding_emissions[ em ] <- NULL

# ------------------------------------------------------------------------------
# 3. Gridding and writing output data 
    
# For now, the gridding routine uses nested for loops to go through every years
# gases and sectors. May consider to take away for loop for sectors and keep year loops 
# for future parallelization 
    
  printLog( paste0( 'Start ', VOC_em, ' gridding for each year ' ) )
    
  for ( year in year_list ) {
    
  # grid one years emissions for subVOCs
    
    int_grids_list <- grid_one_year( em, 
                                     year, 
                                     grid_resolution,
                                     gridding_emissions, 
                                     location_index, 
                                     proxy_mapping, 
                                     proxy_substitution_mapping )
  
  # generate nc file for gridded one years subVOC emissions,
  # a checksum file is also generated along with the nc file 
  # which summarize the emissions in mass by sector by month.   
    
    generate_final_grids_nc_subVOC( int_grids_list,
                                    output_dir, 
                                    grid_resolution, 
                                    year, 
                                    em = 'NMVOC',
                                    VOC_em = VOC_em, 
                                    VOC_names,
                                    sector_name_mapping,
                                    seasonality_mapping )
  } # END of for loop
    
# -----------------------------------------------------------------------------
# 4. Diagnostic: checksum 
# The checksum process uses the checksum files generated along the nc file 
# for all gridding years then compare with the input emissions at 
# final gridding sector level for each year.  
# The comparisons are done in two ways: absolute difference and percentage difference
  
  printLog( 'Start checksum check' )

# calculate global total emissions by sector by year  
    
  ceds_gridding_mapping_fin <- ceds_gridding_mapping[ , c( 'CEDS_int_gridding_sector_short', 'CEDS_final_gridding_sector_short' ) ]
  ceds_gridding_mapping_fin <- unique( ceds_gridding_mapping_fin )
  gridding_emissions_fin <- merge( gridding_emissions, ceds_gridding_mapping_fin, 
                                   by.x = 'sector', by.y = 'CEDS_int_gridding_sector_short', all.x = T )
  gridding_emissions_fin <- aggregate( gridding_emissions_fin[ paste0( 'X', year_list ) ], 
                                       by = list( gridding_emissions_fin$CEDS_final_gridding_sector_short ),
                                       FUN = sum )
  colnames( gridding_emissions_fin ) <- c( 'sector', paste0( 'X', year_list ) )
  gridding_emissions_fin <- gridding_emissions_fin[ order( gridding_emissions_fin$sector ), ]

# consolidate different checksum files to have total emissions by sector by year     
    
  checksum_file_list <- list.files( path = output_dir, pattern = paste0( '_', VOC_em, '_' ) )
  checksum_file_list <- grep( '.csv', checksum_file_list, fixed = T, value = T )
  checksum_res_list <- lapply( checksum_file_list, function( file_name ) { 
    temp_csv <- read.csv( paste0( output_dir, file_name  ) )
    } )
  checksum_df <- do.call( 'rbind', checksum_res_list )
  checksum_df <- aggregate( checksum_df$value, by = list( checksum_df$sector, checksum_df$year ), FUN = sum  )
  colnames( checksum_df ) <- c( 'sector', 'year', 'value' )
  checksum_df <- cast( checksum_df, sector ~ year )
  colnames( checksum_df ) <- c( 'sector', paste0( 'X', year_list ) )
  checksum_df <- checksum_df[ order( checksum_df$sector ), ]

  # comparison  
      
  diag_diff_df <- cbind( checksum_df$sector, abs( gridding_emissions_fin[ paste0( 'X', year_list ) ] - checksum_df[ paste0( 'X', year_list ) ] ) )
  diag_per_df <- cbind( checksum_df$sector, ( diag_diff_df[ paste0( 'X', year_list ) ] / gridding_emissions_fin[ paste0( 'X', year_list ) ] ) * 100 )
  diag_per_df[ is.na( diag_per_df ) ] <- NA
    
# -----------------------------------------------------------------------------
# 5. Write-out and Stop 
  
  out_name <- paste0( 'G.', VOC_em, '_bulk_emissions_checksum_comparison_diff' )
  writeData( diag_diff_df, "DIAG_OUT", out_name )
  out_name <- paste0( 'G.', VOC_em, '_bulk_emissions_checksum_comparison_per' )
  writeData( diag_per_df, "DIAG_OUT", out_name )
    
# Every script should finish with this line:
  logStop()  
