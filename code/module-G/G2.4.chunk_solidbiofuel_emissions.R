# ------------------------------------------------------------------------------
# Program Name: G2.4.chunk_solidbiofuel_emissions.R
# Authors: Leyang Feng, Caleb Braun
# Date Last Updated: March 28, 2019
# Program Purpose: Generate multi-year emissions chunks for solid biofuel emissions.
# Input Files: CEDS_[em]_solidbiofuel_anthro_[year]_0.5_[CEDS_version].nc
# Output Files: FIN_OUT: [em]-em-SOLID-BIOFUEL-anthro_input4MIPs_emissions_CMIP_CEDS-[CEDS_grid_version]_supplement-data_gn_[time_range].nc
# ------------------------------------------------------------------------------


# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Read in universal header files, support scripts, and start logging
headers <- c( 'gridding_functions.R', 'nc_generation_functions.R' )
log_msg <- "Generates chunk NetCDF files for solid biofuel emissions"
source( paste0( PARAM_DIR, "header.R" ) )
initialize( "G2.4.chunk_solidbiofuel_emissions.R", log_msg, headers )

# Define emissions species variable
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"

# Chunk bulk emissions
chunk_emissions( singleVarChunking_solidbiofuelemissions, em )

logStop()