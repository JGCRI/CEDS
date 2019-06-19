# ------------------------------------------------------------------------------
# Program Name: G2.2.chunk_subVOC_emissions.R
# Authors: Leyang Feng, Caleb Braun
# Date Last Updated: March 28, 2019
# Program Purpose: Generate multi-year emissions chunks for subVOC emissions.
# Input Files: CEDS_[VOCID]_anthro_[year]_0.5_[CEDS_version].nc
# Output Files: FIN_OUT: [VOCID]-acids-em-speciated-VOC-anthro_input4MIPs_emissions_CMIP_CEDS-[CEDS_grid_version]_supplement-data_gn_[time_range].nc
# ------------------------------------------------------------------------------


# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Read in universal header files, support scripts, and start logging
headers <- c( 'gridding_functions.R', 'nc_generation_functions.R' )
log_msg <- "Generates chunk NetCDF files for subVOC emissions"
source( paste0( PARAM_DIR, "header.R" ) )
initialize( "G2.2.chunk_subVOC_emissions.R", log_msg, headers )

# Define emissions species variable
args_from_makefile <- commandArgs( TRUE )
VOC_em <- args_from_makefile[ 1 ]
if ( is.na( VOC_em ) ) VOC_em <- "VOC01"

VOC_names <- readData( domain = 'GRIDDING', domain_extension = "gridding_mappings/", 'VOC_id_name_mapping' )

# Chunk bulk emissions
chunk_emissions( singleVarChunking_subVOCemissions, em, VOC_names = VOC_names )

logStop()