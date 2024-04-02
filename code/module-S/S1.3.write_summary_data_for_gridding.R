# ------------------------------------------------------------------------------
# Program Name: S1.3.write_summary_data_for_gridding.R
# Authors: Rachel Hoesly, Steve Smith, Linh Vu, Presley Muwan, Leyang Feng,
#          Caleb Braun, Patrick O'Rourke, Noah Prime
# Date Last Updated: 9/12/2022
# Program Purpose: Produces summary output
# Input Files: Master_Country_List.csv
#              [em]_total_CEDS_emissions.csv
#              Master_Sector_Level_map.csv
# Output Files: Data in final-emissions folder
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers

# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "summary_functions.R", 'common_data.R')
log_msg <- "Writes Final summary data" # First message to be printed to the log
script_name <- "S1.3.write_summary_data_for_gridding.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"


# Setting params, paths, and reading in files --------------------------------

# years to write data
write_years <- 1750:end_year

# Name of CEDS inventory file with point sources removed
totals_file <- paste0(em,'_total_CEDS_emissions_no_point_sources')

# Master sector list/description file
Master_Sector_Level_map <- readData(domain = 'MAPPINGS', file_name = 'Master_Sector_Level_map')

# output file name
out_name <- paste( "CEDS", em , "emissions_by_country_CEDS_sector", version_stamp, sep = "_" )

# Save data ----------------------------------------------------------------
write_emissions_by_country_CEDS_sector( totals_file, em, write_years, Master_Sector_Level_map, out_name, 'MED_OUT', return_em = FALSE )

# End ----------------------------------------------------------------
logStop()









