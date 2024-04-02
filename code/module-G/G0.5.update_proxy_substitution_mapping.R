# ------------------------------------------------------------------------------
# Program Name: G0.5.update_proxy_substitution_mapping.R
# Authors: Noah Prime
# Date Last Updated: June 22, 2023
# Program Purpose: Grid aggregated emissions into NetCDF grids for bulk emissions (excluding AIR)
# Input Files:  MED_OUT:    proxy files
# Output Files: MED_OUT:   
# TODO: Add this as a step in the Makefile 
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Read in universal header files, support scripts, and start logging
headers <- c( 'data_functions.R', 'gridding_functions.R', 'nc_generation_functions.R',
              'point_source_util_functions.R' )
log_msg <- "Gridding anthropogenic emissions (excluding AIR) "
source( paste0( PARAM_DIR, "header.R" ) )
initialize( "G0.5.update_proxy_substitution.R", log_msg, headers )
if ( grid_remove_iso != "" ) printLog( paste("Gridding will exclude",grid_remove_iso) )


# Define emissions species variable
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
res <- as.numeric( args_from_makefile[ 2 ] ) # introducing second command line argument as gridding resolution
if ( is.na( em ) ) em <- "SO2"
if ( is.na( res ) ) res <- 0.5 # default gridding resolution of 0.5

# SHP, AIR, and TANK sectors proxy pre-installed in different location
SAT_proxy_dir       <- filePath( "GRIDDING", "proxy/",                 extension = "" )
proxy_dir           <- filePath( "MED_OUT",  "final_generated_proxy/", extension = "" )
proxy_backup_dir    <- filePath( "GRIDDING", "proxy-backup/",          extension = "" )
mask_dir            <- filePath( "GRIDDING", "mask/",                  extension = "" )
seasonality_dir     <- filePath( "GRIDDING", "seasonality/",           extension = "" )


# Initialize the gridding parameters
gridding_initialize( grid_resolution = res,
                     start_year = EDGAR_start_year, # 1750
                     end_year = end_year, # end_year,
                     load_masks = T,
                     load_seasonality_profile = T )


# 1. Read in files --------------------------------------------------------------
location_index               <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'country_location_index_05', meta = FALSE )
ceds_gridding_mapping        <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'CEDS_sector_to_gridding_sector_mapping', meta = FALSE )
proxy_mapping                <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'proxy_mapping', meta = FALSE )

# Remove AIR sector
ceds_gridding_mapping <- ceds_gridding_mapping %>%
    dplyr::filter(CEDS_final_gridding_sector_short != 'AIR')

# Move pre-installed proxy to final_proxy folder (no overwriting)
pre_installed <- list.files(SAT_proxy_dir)
file.copy( from = paste0(SAT_proxy_dir, pre_installed),
           to = paste0(proxy_dir, pre_installed), 
           overwrite = FALSE )

# List of proxy files
proxy_files <- list( primary = list.files( proxy_dir ), backup = list.files( proxy_backup_dir ) )

# Extend to last year
proxy_mapping <- extendProxyMapping( proxy_mapping )

# List of Intermediate gridding sectors
sector_list <- sort( unique( ceds_gridding_mapping$CEDS_int_gridding_sector_short ) )
iso_list <- sort( unique( location_index$iso ) )

# 2. Define Functions --------------------------------------------------------------

# Returns the sum of emissions in the given proxy in the given iso
check_one_iso <- function(iso, location_index, proxy ) {

    # retrieve matrix indexes for iso for later proxy cropping
    index_line <- location_index[ location_index$iso == iso, ]
    start_row <- index_line$start_row
    end_row <- index_line$end_row
    start_col <- index_line$start_col
    end_col <- index_line$end_col

    # retrieve the iso_mask from memory
    mask_name <- paste0( iso, '_mask')
    mask <- get( mask_name )

    # Crop and weight proxy using iso mask
    proxy_cropped <- proxy[ start_row : end_row, start_col : end_col ]
    weighted_proxy <- proxy_cropped * mask
    weighted_proxy[ is.nan( weighted_proxy ) ] <- 0

    # Sum of emissions in proxy
    total_proxy_emissions <- sum(weighted_proxy)

    # Return whether or not this iso has no emissions
    return( total_proxy_emissions )
}

check_one_sector <- function(sector, em, year, location_index, proxy_files) {

    # Load in proxy files
    proxy <- get_proxy( em, year, sector, proxy_mapping, proxy_files, proxy_type = 'primary' )
    proxy_backup <- get_proxy( em, year, sector, proxy_mapping, proxy_files, proxy_type = 'backup' )

    # Currently, won't do anything for SHP and TANK sectors, but want to sperate those cases from the rest.
    if ( (sector == 'SHP') || (sector == 'TANK') ) {

        # Get and normalize the global proxy using for SHP and TANK
        mask <- global_mask
        proxy_weighted <- proxy * mask
        proxy_weighted[ is.na( proxy_weighted ) ] <- 0

        # Whether global proxy is non-zero
        total_global_proxy <- sum(proxy_weighted)

        # Common return form
        iso_em_spatial_list <- list(global = total_global_proxy)

    } else {
        # Continue if sector is another of the recognized CEDS intermediate gridding sectors
        stopifnot(sector %in% c( 'AGR', 'ELEC', 'ETRN', 'FFFI', 'FLR', 'INDC', 'INPU', 'NRTR', 'RCOO', 'RCORC', 'ROAD', 'SLV', 'WST' ) )

        # Iterate over all isos
        iso_em_spatial_list <- lapply( iso_list, check_one_iso, location_index, proxy )

        # Rename list elements as each respective iso
        names( iso_em_spatial_list ) <- iso_list
    }

    iso_df <- data.frame( iso_em_spatial_list )
    iso_df$sector <- sector

    # Returns a list of the total proxy emissions in each iso in the given sector
    return( iso_df )
}

check_one_year <- function(year, em, location_index, proxy_files){

    print(year)
    # Apply check one sector to each CEDS intermediate gridding sector
    res_list <- lapply( sector_list, check_one_sector, em, year, location_index, proxy_files )
    # Rename output list elements with respective sectors
    names( res_list ) <- paste0( sector_list )

    sec_df <- bind_rows(res_list)
    sec_df$year <- year

    return( sec_df )
}

# 3. Find zero iso/sectors for each year ------------------------------------------------
printLog( paste( 'Checking ', em, 'emissions proxy for each year / iso / sector...' ) )

# Get list of results for each year
results_list <- lapply(year_list, check_one_year, em, location_index, proxy_files)

# Turn into data frame, and add emission species column
res_df <- bind_rows(results_list)
res_df$em <- em

# Get columns that represent isos
last_iso_col <- dim(res_df)[2] - 3
iso_cols <- colnames(res_df)[1:last_iso_col]

# Turn into long format. Now each row is a unique year, iso, sector, combination
res_df_long <- res_df %>%
    gather(iso, proxy_emissions, iso_cols)

# Find all iso / sector combinations with at least one year of zero total emissions in proxy
res_df_long <- res_df_long %>%
    dplyr::filter(proxy_emissions <= 0) %>%
    dplyr::select(em, sector, iso) %>%
    dplyr::distinct() %>%
    dplyr::mutate(sub_flag = 1)

# Write Out
writeData(res_df_long, 
          domain = 'MED_OUT', 
          fn = paste0(em, '_proxy_substitution_mapping'), meta=FALSE)

# END
logStop()