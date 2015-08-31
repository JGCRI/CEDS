#------------------------------------------------------------------------------
# Program Name: C2.1.base_NC_EF.R
# Author: Jon Seibert
# Date Last Modified: July 7, 2015
# Program Purpose: To calculate default emissions factors from the process emissions
#                  and activity databases, and use them to generate the base process
#                  emissions factors database.
# Input Files: A.NC_activity.csv, C.[em]_NC_emissions.csv
# Output Files: C.[em]_NC_EF_db.csv
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
        if ( length( wd ) > 0 ) {
            setwd( wd[ 1 ] )
            break
        }
    }
    PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R" ) # Additional function files required.
    log_msg <- "Generation of process emissions factors" # First message to be printed to the log
    script_name <- "C2.1.base_NC_EF.R"
    
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )
    
# ------------------------------------------------------------------------------
# 1. Read in files

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"
em_lc <- tolower( em )

activity_data <- readData( "MED_OUT", "A.NC_activity" )

emissions_data <- readData( "MED_OUT", paste0( "C.", em, "_NC_emissions" ) )

# ------------------------------------------------------------------------------
# 2. Generate emissions factors and extendForward (constant) as necessary

# Set up data frame for new emissions factors
new_efs <- emissions_data[ 1:3 ]
new_efs <- cbind( new_efs, units = NA )
for( i in 5:length( emissions_data ) ){
    new_efs <- cbind( new_efs, x = NA )
}
names( new_efs ) <- names( emissions_data )

# Ef * Activity = Emissions
# Divide emissions data by activity data to generate emissions factors:

# 0/0 must be set to 0.

# # Unused function, may still have utility later
# find_row_averages <- function(df){
    # data <- as.data.frame( lapply( df[ range ], as.numeric ) )
    # avg <- rowSums( avg, na.rm = FALSE, dims = 1) / num_cols
    # return( avg )
# }

data_start <- findDataStart( emissions_data )
num_cols <- length( names( emissions_data ) )
range <- data_start:num_cols

em_nums <- emissions_data[ range ]
act_nums <- activity_data[ range ]

for( i in 1:length( em_nums ) ){
    ef_col <- i + data_start - 1
    em_nums[[ i ]] <- as.numeric( em_nums[[ i ]] )
    act_nums[[ i ]] <- as.numeric( act_nums[[ i ]] )
    new_efs[[ ef_col ]] <- em_nums[[ i ]] / act_nums[[ i ]]
    new_efs[[ ef_col ]][ !is.finite( new_efs[[ ef_col ]] ) ] <- 0
}

new_efs$units <- paste0( emissions_data$units, "/", activity_data$units )

# --------------------------------------------------------------------------------------------
# 3. Output

writeData( new_efs, domain = "MED_OUT", fn = paste0( "C.", em, "_", "NC", "_EF_db" ) )

logStop()
# END
