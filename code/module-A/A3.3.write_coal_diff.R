#------------------------------------------------------------------------------
# Program Name: A3.3.write_coal_diff.R
# Authors Names: Linh Vu
# Date Last Modified: 1 April 2016
# Program Purpose:    Write out difference between IEA DOMSUP coal and CEDS
#                     coal consumption
# Input Files: A.IEA_en_stat_ctry_hist.csv, A.IEA_BP_energy_ext.csv
# Output Files: A.IEA_CEDS_coal_difference.csv
# Notes:
# TODO:
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Before we can load headers we need some paths defined. They may be provided by
#   a system environment variable or may have already been set in the workspace.
# Set variable PARAM_DIR to be the data system directory
dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
for ( i in 1:length( dirs ) ) {
  setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
  wd <- grep( 'CEDS/input', list.dirs(), value = T )
  if ( length(wd) > 0 ) {
    setwd( wd[1] )
    break
  }
}
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "common_data.R" ) # Additional function files required.
log_msg <- "Write out difference between IEA coal and CEDS coal" # First message to be printed to the log
script_name <- "A3.3.write_coal_diff.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Read in files
  IEA_en_stat_ctry_hist <- readData( "MED_OUT", "A.IEA_en_stat_ctry_hist" )
  IEA_BP_energy_ext <- readData( "MED_OUT", "A.IEA_BP_energy_ext" )
  
# ------------------------------------------------------------------------------
# 2. Write out difference between IEA DOMSUP coal and CEDS coal
  primary_coals <- c( "Brown coal (if no detail) (kt)", "Coking coal (kt)", 
                      "Hard coal (if no detail) (kt)", "Other bituminous coal (kt)", 
                      "Sub-bituminous coal (kt)", "Lignite (kt)", "Anthracite (kt)", 
                      "Patent fuel (kt)" )
  IEA_coal <- filter( IEA_en_stat_ctry_hist, FLOW == "DOMSUP", PRODUCT %in% primary_coals ) %>%
    select( -PRODUCT ) %>% group_by( iso, FLOW ) %>%
    summarise_each( "sum" )
  
  CEDS_coal <- filter( IEA_BP_energy_ext, fuel %in% c( "hard_coal", "brown_coal", "coal_coke" ) ) %>%
    select( -fuel, -sector, -units ) %>% group_by( iso ) %>%
    summarise_each( "sum" )
  
# Compute difference
  IEA_coal <- filter( IEA_coal, iso %in% CEDS_coal$iso ) %>% arrange( iso )
  IEA_coal <- IEA_coal[ names( IEA_coal ) %in% names( CEDS_coal ) ]
  CEDS_coal <- filter( CEDS_coal, iso %in% IEA_coal$iso ) %>% arrange( iso )
  CEDS_coal <- CEDS_coal[ names( CEDS_coal ) %in% names( IEA_coal ) ]
  diff_coal <- IEA_coal
  Xyears <- names( diff_coal )[ grepl( "X", names( diff_coal ) ) ]
  diff_coal[ Xyears ] <- diff_coal[ Xyears ] - CEDS_coal[ Xyears ]
  diff_coal <- as.data.frame( diff_coal )
  
# If difference < 0, bring up to 0 and write out diagnostics
  subzero <- melt( diff_coal, id = "iso" ) %>%
    filter( value < 0 ) %>%
    cast()
  diff_coal[ diff_coal < 0 ] <- 0
  
# Clean up
  diff_coal$sector <- "1A1bc_Other-transformation"
  diff_coal$fuel <- "coal"
  diff_coal$units <- "kt"
  diff_coal <- diff_coal[ c( "iso", "sector", "fuel", "units", Xyears ) ]

# ------------------------------------------------------------------------------
# 3. Output
  writeData( diff_coal, "MED_OUT", "A.IEA_CEDS_coal_difference" )
  writeData( subzero, "DIAG_OUT", "A.IEA_CEDS_coal_difference_subzero")

  logStop()

