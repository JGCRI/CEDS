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
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"
    
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
source('../code/diagnostic/Paper_Figures_BC_residential_biomass.R')

source('../code/diagnostic/Paper_Figures_global_summaries.R')
source('../code/diagnostic/Paper_Figures_regional_summaries.R')
source('../code/diagnostic/Paper_Figures_sector_summaries.R')

source('../code/diagnostic/Paper_Figures_global_summaries_recent.R')
source('../code/diagnostic/Paper_Figures_regional_recent.R')
source('../code/diagnostic/Paper_Figures_sector_recent.R')

# source('../code/diagnostic/Paper_Figures_Regional_EF_graphs.R')


# ---------------------------------------------------------------------------
logStop()
