# ------------------------------------------------------------------------------
# Program Name: Paper_Figures_all.R
# Author: Rachel Hoesly
# Date Last Updated: 
# Program Purpose: Run all scripts that create paper and supplement figures
#                  
# Input Files: 
# Output Files: 
# Note: 
# TODO: 
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers

# Set working directory
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
headers <- c( "data_functions.R", "analysis_functions.R",'process_db_functions.R',
              'common_data.R', 'IO_functions.R', 'data_functions.R', 'timeframe_functions.R') # Additional function files may be required.
log_msg <- "Processing Paper and Supplement Figures for all emission species" # First message to be printed to the log
script_name <- "Paper_Figures_all.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )


# ---------------------------------------------------------------------------
# Call Scripts

source('../code/diagnostic/Paper_Figures_compare_RCP_GAINS_EDGAR.R')
source('../code/diagnostic/Paper_Figures_global_summaries.R')
# source('../code/diagnostic/Paper_Figures_Regional_EF_graphs.R')
source('../code/diagnostic/Paper_Figures_regional_summaries.R')
source('../code/diagnostic/Paper_Figures_sector_summaries.R')
source('../code/diagnostic/Paper_Figures_BC_residential_biomass.R')
source('../code/diagnostic/Paper_Figures_emission_ratios.R')

# ---------------------------------------------------------------------------
logStop()






