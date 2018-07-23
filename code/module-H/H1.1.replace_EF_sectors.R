# ------------------------------------------------------------------------------
# Program Name: H1.1.replace_EF_sectors.R
# Author: Linh Vu
# Date Last Modified: 30 Mar 2016
# Program Purpose: Replace EF of one sector with another sector
# Input Files:  H.[em]_total_EFs_extended_adjusted-pathway.csv
# Output Files:  H.[em]_total_EFs_extended_adjusted-sector.csv,
# TODO:
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R" ) # Additional function files may be required.
log_msg <- "Replace sector EFs" # First message to be printed to the log
script_name <- "H1.1.replace_EF_sectors.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "BC"


# ---------------------------------------------------------------------------
# 1. Load input
# Load library
  library( "tools" )

# Load data
  ef_full <- readData( 'MED_OUT', paste0( 'F.',em,'_scaled_EF' ) )
  sector_map_list <- list.files( path = "extension/sector-change/", pattern = "*.csv" )
  sector_map_list <- tools::file_path_sans_ext( sector_map_list )
  sector_map_list <- sector_map_list[ grepl( em, sector_map_list )  &
                                        !grepl( "metadata", sector_map_list ) ]
  if ( em == "OC" )
    sector_map_list <- sector_map_list[ !grepl( "NMVOC", sector_map_list ) ]
  sector_map_list <- lapply( sector_map_list, FUN = readData, domain = "EXT_IN", domain_extension = "sector-change/" )


# ---------------------------------------------------------------------------
# 2. Replace sector EFs

if ( length( sector_map_list ) > 0 ){
  printLog( "Replace sector EFs by instructions in sector-change folder" )

  # Read all sector change into one df
  sector_map <- do.call( rbind, sector_map_list )

  # Replace EFs
  ef_changed <- filter( ef_full, paste( iso, sector, fuel ) %in%
                      paste( sector_map$iso, sector_map$changed_sector, sector_map$fuel ) )
  ef_source <- filter( ef_full, paste( iso, sector, fuel ) %in%
                      paste( sector_map$iso, sector_map$source_sector, sector_map$fuel ) )
  ef_changed[, X_emissions_years ] <- ef_source[, X_emissions_years ]
  ef_full_changed <- filter( ef_full, paste( iso, sector, fuel ) %!in%
                               paste( sector_map$iso, sector_map$changed_sector, sector_map$fuel ) ) %>%
    rbind( ef_changed ) %>% dplyr::arrange( iso, sector, fuel )

# Do nothing if no instruction files exist
} else {
  printLog( paste( "No sector-change instructions exist for", em, ". No modification made." ) )
  ef_full_changed <- ef_full
}

# ---------------------------------------------------------------------------
# 3. Output
  writeData( ef_full_changed, "MED_OUT", paste0( "H.", em, "_total_EFs_adjusted-sector" ) )


logStop()
