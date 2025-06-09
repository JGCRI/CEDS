# ------------------------------------------------------------------------------
# Program Name: diag_negative_grid_check_aviation.R
# Author(s): Hamza Ahsan
# Date Last Updated: November 21, 2024
# Program Purpose: Checks for negative values in annual gridded data.
# Input Files:
# Output Files:
# Notes:
# ------------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Read in universal header files, support scripts, and start logging
headers <- c( 'data_functions.R', 'gridding_functions.R', 'nc_generation_functions.R')
log_msg <- "Diagnostic check for negative values in gridded files"
source( paste0( PARAM_DIR, "header.R" ) )
initialize( "diag_negative_grid_check_aviation.R", log_msg, headers )

# specify em species
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"

# load utility functions
source( '../code/diagnostic/diag_utility_functions.R' )

# load intermediate gridded emission files
nc_file_path <- '../intermediate-output/gridded-emissions'
nc_file_list <- list.files( path = nc_file_path, pattern = paste0( 'CEDS_', em, '_AIR_anthro' ) )
nc_file_list <- grep( '.nc', nc_file_list, fixed = T, value = T )

# select years
start_year <- 1750
extract_years <- sapply(strsplit(nc_file_list, "[_]"),function(x) x[5])
nc_file_list <- nc_file_list[which(extract_years >= start_year)]

gridcell_area <- grid_area( 0.5, all_lon = T )
days_in_month <- c( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 )

print(paste0("Processing ", length(nc_file_list), " files..."))

for (nc_file in nc_file_list) {

    year <- unlist( strsplit( nc_file, split = '_' ) )[ 5 ]
    temp_nc <- nc_open( paste0( nc_file_path, '/', nc_file ) )
    AIR_var <- ncvar_get( temp_nc, 'AIR' )
    nc_close( temp_nc )

    AIR_temp_array <- array( dim = c( 360, 720, 25, 12 ) )
    DIM_rep <- rep( days_in_month, dim(AIR_temp_array)[3] / 12 )

    AIR_array <- array( dim = c( 12 ) )

    # Convert to kt and get global totals
    for ( i in 1: dim( AIR_temp_array )[ 4 ] ) {
        for ( j in 1: dim( AIR_temp_array )[ 3 ] ) {
            # Rotate and transpose grid
            AIR_temp_array[ , , j, i ] <- flip_a_matrix( t( AIR_var[ , , j, i ] ) )

            # Convert from flux to mass (kg m-2 s-1 to kt)
            AIR_temp_array[ , , j, i ] <- AIR_temp_array[ , , j, i ] * gridcell_area * ( DIM_rep[ i ] * 24 * 60 * 60 ) * 0.000001
        }
    }


    # Check for negative values and percent relative to each sector
    if (min(AIR_temp_array) < 0) {
        print(paste0("Negative value(s) found in the AIR sector in ", nc_file))
        print(paste0("The percent of negative values relative to total AIR emissions is ", abs((sum(AIR_temp_array[which(AIR_temp_array < 0)])/sum(AIR_temp_array))*100),"%"))
    }

    print(paste0("Finished checking ", nc_file))
}

print("Diagnostic complete")

