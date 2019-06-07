#------------------------------------------------------------------------------
# Program Name: C1.2.add_SO2_NC_emissions_all.R
# Author: Jon Seibert
# Date Last Modified: June 30, 2015
# Program Purpose: To process and reformat default emissions data for non-combustion (process)
#     emissions, and to add it to the correct emissions database.
# Input Files: Process_SO2_Emissions_to_2005.xlsx, Master_Fuel_Sector_List.xlsx,
#              sector_input_mapping.xlsx
# Output Files: C.[em]_NC_emissions_db.csv
# Notes:
# TODO:
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "timeframe_functions.R", "process_db_functions.R",
                  "analysis_functions.R" ) # Additional function files required.
    log_msg <- "Addition of multiple sets of emissions data to the emissions database" # First message to be printed to the log
    script_name <- "C1.2.add_SO2_NC_emissions_all.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# -----------------------------------------------------------------------------
# 0.5. Settings

# Input file
input <- c( "Process_SO2_Emissions_to_2005", ".xlsx" )

# Location of input files relative to wd
input_domain <- "ACTIVITY_IN"

fuel <- "process"

# ------------------------------------------------------------------------------
# 1. Read in files

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"

master_sector_list <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Sectors" )
sector_input <- readData( "MAPPINGS", "sector_input_mapping", ".xlsx" )

data_list <- readData( input_domain, input[ 1 ], input[ 2 ] )

# ------------------------------------------------------------------------------
# 1.5 Derive required information from inputs and mappings

data_length <- length( data_list )

sector_list <- c()
# unit_list <- c()
for( input in names( data_list ) ){
    sect <- sector_input[ sector_input$file == input, "sector" ]
    sector_list <- c( sector_list, sect )
#     unit <- master_sector_list[ master_sector_list$sector == sect, "units" ]
#     unit_list <- c( unit_list, unit )
}

# ------------------------------------------------------------------------------
# 2. Reformat emissions data

# Apply reformatting header function
data_list <- lapply( data_list, cleanData )

# Applies sector, fuel, and unit assignments and reorders columns to standard form
for( i in 1:data_length ){
    data <- data_list[[ i ]]
    data$sector <- sector_list[[ i ]]
    data$fuel <- fuel
    data$units <- "kt"
    data_list[[ i ]] <- cbind( data[ c( "iso","sector","fuel","units" ) ] ,
        data[ 2:( length( data ) - 3 ) ] )
}

x<-data_list[[2]]

# ------------------------------------------------------------------------------
# 3. Output

# Add reformatted data to the emissions database, extending or truncating it as necessary.
# By default, it will be extended forward to the common end year, but not backwards.
# Only do this if the sectorCheck header function determines that the sectors in
# the reformatted data are all present in the Master List.
for( i in 1:data_length ){
    if( sectorCheck( data_list[[ i ]], check_all = FALSE ) ){
        addToEmissionsDb( data_list[[ i ]], em, "NC" )
    }
}

logStop()
# END
