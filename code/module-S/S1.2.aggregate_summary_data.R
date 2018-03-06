# ------------------------------------------------------------------------------
# Program Name: S1.2.aggregate_summary_data.R
# Author: Caleb Braun
# Date Last Updated: Mar 06, 2018
# Program Purpose: Generate cross-species summary files
#
# Output Files: data in final-emissions folder
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers

# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

headers <- c( "data_functions.R", "summary_functions.R" )
log_msg <- "Aggregate emissions into summary files" # First message to be printed to the log
script_name <- "S1.2.aggregate_summary_data.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )


# ---------------------------------------------------------------------------
# 1. Combine diagnostic files

fpath <- "../final-emissions/"
fregex <- "diagnostics/global_.+_emissions_by_CEDS_sector\\.csv"
fnames <- grep( fregex, dir( fpath, recursive = T ), value = T )

sub( '.csv', '', fnames ) %>%
lapply( readData, domain = "FIN_OUT" ) %>%
lapply( write_global_emissions_by_sector )

# Remove the .csv files
file.remove( paste0( fpath, fnames ) )


logStop()