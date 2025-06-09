# ------------------------------------------------------------------------------
# Program Name: diag_negative_grid_check.R
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
initialize( "diag_negative_grid_check.R", log_msg, headers )

# specify em species
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"

# load utility functions
source( '../code/diagnostic/diag_utility_functions.R' )

# load intermediate gridded emission files
nc_file_path <- '../intermediate-output/gridded-emissions'
nc_file_list <- list.files( path = nc_file_path, pattern = paste0( 'CEDS_', em, '_anthro' ) )
nc_file_list <- grep( '.nc', nc_file_list, fixed = T, value = T )

# select years
start_year <- 1750
extract_years <- sapply(strsplit(nc_file_list, "[_]"),function(x) x[4])
nc_file_list <- nc_file_list[which(extract_years >= start_year)]

gridcell_area <- grid_area( 0.5, all_lon = T )
days_in_month <- c( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 )

print(paste0("Processing ", length(nc_file_list), " files..."))

for (nc_file in nc_file_list) {

    year <- unlist( strsplit( nc_file, split = '_' ) )[ 4 ]
    temp_nc <- nc_open( paste0( nc_file_path, '/', nc_file ) )
    AGR_var <- ncvar_get( temp_nc, 'AGR' )
    ENE_var <- ncvar_get( temp_nc, 'ENE' )
    IND_var <- ncvar_get( temp_nc, 'IND' )
    TRA_var <- ncvar_get( temp_nc, 'TRA' )
    SLV_var <- ncvar_get( temp_nc, 'SLV' )
    WST_var <- ncvar_get( temp_nc, 'WST' )
    RCO_var <- ncvar_get( temp_nc, 'RCO' )
    SHP_var <- ncvar_get( temp_nc, 'SHP' )
    nc_close( temp_nc )


    AGR_temp_array <- array( dim = c( 360, 720, 12 ) )
    ENE_temp_array <- array( dim = c( 360, 720, 12 ) )
    IND_temp_array <- array( dim = c( 360, 720, 12 ) )
    TRA_temp_array <- array( dim = c( 360, 720, 12 ) )
    SLV_temp_array <- array( dim = c( 360, 720, 12 ) )
    WST_temp_array <- array( dim = c( 360, 720, 12 ) )
    RCO_temp_array <- array( dim = c( 360, 720, 12 ) )
    SHP_temp_array <- array( dim = c( 360, 720, 12 ) )

    AGR_array <- array( dim = c( 12 ) )
    ENE_array <- array( dim = c( 12 ) )
    IND_array <- array( dim = c( 12 ) )
    TRA_array <- array( dim = c( 12 ) )
    SLV_array <- array( dim = c( 12 ) )
    WST_array <- array( dim = c( 12 ) )
    RCO_array <- array( dim = c( 12 ) )
    SHP_array <- array( dim = c( 12 ) )

    # Convert to kt and get global totals by sector
    for ( i in 1 : 12 ) {
        AGR_temp_array[ , , i ] <- flip_a_matrix( t( AGR_var[ , , i ] ) )
        ENE_temp_array[ , , i ] <- flip_a_matrix( t( ENE_var[ , , i ] ) )
        IND_temp_array[ , , i ] <- flip_a_matrix( t( IND_var[ , , i ] ) )
        TRA_temp_array[ , , i ] <- flip_a_matrix( t( TRA_var[ , , i ] ) )
        SLV_temp_array[ , , i ] <- flip_a_matrix( t( SLV_var[ , , i ] ) )
        WST_temp_array[ , , i ] <- flip_a_matrix( t( WST_var[ , , i ] ) )
        RCO_temp_array[ , , i ] <- flip_a_matrix( t( RCO_var[ , , i ] ) )
        SHP_temp_array[ , , i ] <- flip_a_matrix( t( SHP_var[ , , i ] ) )

        AGR_temp_array[ , , i ] <- AGR_temp_array[ , , i ] * gridcell_area * ( days_in_month[ i ] * 24 * 60 * 60 ) * 0.000001
        ENE_temp_array[ , , i ] <- ENE_temp_array[ , , i ] * gridcell_area * ( days_in_month[ i ] * 24 * 60 * 60 ) * 0.000001
        IND_temp_array[ , , i ] <- IND_temp_array[ , , i ] * gridcell_area * ( days_in_month[ i ] * 24 * 60 * 60 ) * 0.000001
        TRA_temp_array[ , , i ] <- TRA_temp_array[ , , i ] * gridcell_area * ( days_in_month[ i ] * 24 * 60 * 60 ) * 0.000001
        SLV_temp_array[ , , i ] <- SLV_temp_array[ , , i ] * gridcell_area * ( days_in_month[ i ] * 24 * 60 * 60 ) * 0.000001
        WST_temp_array[ , , i ] <- WST_temp_array[ , , i ] * gridcell_area * ( days_in_month[ i ] * 24 * 60 * 60 ) * 0.000001
        RCO_temp_array[ , , i ] <- RCO_temp_array[ , , i ] * gridcell_area * ( days_in_month[ i ] * 24 * 60 * 60 ) * 0.000001
        SHP_temp_array[ , , i ] <- SHP_temp_array[ , , i ] * gridcell_area * ( days_in_month[ i ] * 24 * 60 * 60 ) * 0.000001

    }

    # Check for negative values and percent relative to each sector
    if (min(AGR_temp_array) < 0) {
        print(paste0("Negative value(s) found in the AGR sector in ", nc_file))
        print(paste0("The percent of negative values relative to total AGR is ", abs((sum(AGR_temp_array[which(AGR_temp_array < 0)])/sum(AGR_temp_array))*100),"%"))
    } else if (min(ENE_temp_array) < 0) {
        print(paste0("Negative value(s) found in the ENE sector in ", nc_file))
        print(paste0("The percent of negative values relative to total ENE is ", abs((sum(ENE_temp_array[which(ENE_temp_array < 0)])/sum(ENE_temp_array))*100),"%"))
    } else if (min(IND_temp_array) < 0) {
        print(paste0("Negative value(s) found in the IND sector in ", nc_file))
        print(paste0("The percent of negative values relative to total IND is ", abs((sum(IND_temp_array[which(IND_temp_array < 0)])/sum(IND_temp_array))*100),"%"))
    } else if (min(TRA_temp_array) < 0) {
        print(paste0("Negative value(s) found in the TRA sector in ", nc_file))
        print(paste0("The percent of negative values relative to total TRA is ", abs((sum(TRA_temp_array[which(TRA_temp_array < 0)])/sum(TRA_temp_array))*100),"%"))
    } else if (min(SLV_temp_array) < 0) {
        print(paste0("Negative value(s) found in the SLV sector in ", nc_file))
        print(paste0("The percent of negative values relative to total SLV is ", abs((sum(SLV_temp_array[which(SLV_temp_array < 0)])/sum(SLV_temp_array))*100),"%"))
    } else if (min(WST_temp_array) < 0) {
        print(paste0("Negative value(s) found in the WST sector in ", nc_file))
        print(paste0("The percent of negative values relative to total WST is ", abs((sum(WST_temp_array[which(WST_temp_array < 0)])/sum(WST_temp_array))*100),"%"))
    } else if (min(RCO_temp_array) < 0) {
        print(paste0("Negative value(s) found in the RCO sector in ", nc_file))
        print(paste0("The percent of negative values relative to total RCO is ", abs((sum(RCO_temp_array[which(RCO_temp_array < 0)])/sum(RCO_temp_array))*100),"%"))
    } else if (min(SHP_temp_array) < 0) {
        print(paste0("Negative value(s) found in the SHP sector in ", nc_file))
        print(paste0("The percent of negative values relative to total SHP is ", abs((sum(SHP_temp_array[which(SHP_temp_array < 0)])/sum(SHP_temp_array))*100),"%"))
    }

    print(paste0("Finished checking ", nc_file))
}

print("Diagnostic complete")

