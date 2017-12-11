# ------------------------------------------------------------------------------
# Program Name: speciate_NMVOC.R
# Author: Leyang feng
# Date Last Updated: Nov 11 2016
# Program Purpose: Produces speciated VOCs by country and by CEDS sector
# Input Files: CEDS_[em]_emissions_by_country_CEDS_sector_[ceds_version_stamp].csv
# Output Files: CEDS_VOC[id]_emissions_by_country_CEDS_sector_[ceds_version_stamp].csv
# Note: (1) CEDS emissions for sector '11A_Volcanoes', '11B_Forest-fires',
#           '11C_Other-natural', '6B_Other-not-in-total' are dropped through voc_ratio_sector_mapping
#       (2) CEDS emissions for sector '1A3aii_Domestic-aviation' and '1A3ai_International-aviation'
#           are dropped through voc ratios file
# TODO:
# ---------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
headers <- c( 'data_functions.R' ) # Any additional function files required
log_msg <- "sub-VOCs speciation" # First message to be printed to the log
script_name <- "speciate_NMVOC.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define emission species

# Define emissions species variable
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "NMVOC"

if ( em != 'NMVOC' ) { stop( 'non-NMVOC emissions are not supported by this script' ) }

# ------------------------------------------------------------------------------
# 2. Read in files
# read in the emission data
target_filename <- list.files( filePath( "FIN_OUT", "", extension = "", domain_extension = "current-versions/" ),
                               pattern = paste0( ".*_", em, '_emissions_by_country_CEDS_secto.*' ) )
target_filename <- substr( target_filename, 1, ( nchar( target_filename ) - 4 ) )
ceds_version <- substr( target_filename, ( nchar( target_filename ) - 11 ), nchar( target_filename ) )
nmvoc_emissions <- readData( "FIN_OUT", domain_extension = "current-versions/", target_filename )

# read in voc sector mapping
voc_sector_mapping<- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', file_name = 'VOC_ratio_sector_mapping' )

# read in voc ratio mapping
voc_ratios <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', file_name = 'voc_ratio_allsector' )

# read in voc id mapping
voc_names <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', file_name = 'VOC_id_name_mapping' )

# ------------------------------------------------------------------------------
# 3. Basic set-ups
nmvoc_years <- grep( 'X', colnames( nmvoc_emissions ), fixed = T, value = T )

# ------------------------------------------------------------------------------
# 4. Match voc ratios tonmvoc_emissions
em_sector <- merge( nmvoc_emissions, voc_sector_mapping, by.x = 'sector', by.y = 'CEDS_working_sector' )
em_sector <- em_sector[ !is.na( em_sector$VOC_ratio_sectors ), ]
# manually change voc_sector for global-2L combination
em_sector[ ( em_sector$sector == '2L_Other-process-emissions' &
               em_sector$iso == 'global' ), "VOC_ratio_sectors" ] <- 'X2L'
em_ratio <- merge( em_sector, voc_ratios, by.x = c( 'iso', 'VOC_ratio_sectors' ),
                   by.y = c( 'iso', 'CEDS_grd_sector' ) ) # emissions for global will be dropped except 'SHP' and 'X2L'


# ------------------------------------------------------------------------------
# 5. perforem sub-VOC speciation
# 5.1 define a VOC speciation function
spceiate_one_VOC <- function( VOC_id, em_ratio ) {

  temp_voc_df <- em_ratio
  temp_voc_df$em <- VOC_id

  # construct the ratio matrix for all years
  ratio_mat <- matrix( rep( temp_voc_df[ , VOC_id ], length( nmvoc_years ) ),
                            nrow = length( temp_voc_df[ , VOC_id ] ),
                            ncol = length( nmvoc_years ) )
  temp_voc_df[ , nmvoc_years ] <- temp_voc_df[ , nmvoc_years ] * ratio_mat

  voc_df <- temp_voc_df[ , c( "iso", "sector", "em", "units", nmvoc_years ) ]

  out_filename <- gsub( "_NMVOC_", paste0( '_', VOC_id, '_' ), target_filename )
  writeData( voc_df, domain = 'DIAG_OUT', domain_extension = 'sub-VOCs/', fn = out_filename )

  }

# 5.2 apply the function for all VOC_ids
invisible( lapply( unique( voc_names$VOC_id ) , spceiate_one_VOC, em_ratio ) )

# ---------------
# 6. end

logStop( )
