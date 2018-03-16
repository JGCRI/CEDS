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
log_msg <- "Aggregate emissions into summary files"
script_name <- "S1.2.aggregate_summary_data.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )


# ---------------------------------------------------------------------------
# 1. Combine all global_EM_emissions_by_CEDS_sector files into one excel file

fpath <- "../final-emissions/diagnostics"

# Create a vector of the desired file names. The regex symbols '.+' match one
# or more character, so file names for all emissions should be captured.
file_regex <- "global_.+_emissions_by_CEDS_sector\\.csv"
file_names <- list.files( fpath, file_regex, recursive = T )

# Write the contents of each .csv file into a tabbed excel file
if ( length( file_names ) ) {
    sub( '.csv', '', file_names ) %>%
    lapply( readData, domain = "FIN_OUT", domain_extension = "diagnostics/" ) %>%
    lapply( write_global_emissions_by_sector )

    # Remove the .csv files
    invisible( file.remove( paste0( fpath, "/", file_names ) ) )
}

# ---------------------------------------------------------------------------
# 2. Combine all global_total_emissions_for_EM files into one excel file

file_regex <- "global_total_emissions_for_.+\\.Rd"
file_names <- list.files( fpath, file_regex, full.names = T, recursive = T )

# Read final emissions output from saved R data objects into single dataframe,
# then write it out to an excel file
global_total_ems <- do.call( rbind, lapply( file_names, readRDS ) )
write_global_emissions_by_species( global_total_ems )

# Remove the saved R data objects
invisible( file.remove( file_names ) )


logStop()
