# ------------------------------------------------------------------------------
# Program Name: H3.3.add_emissions_SO2_other_transformation.R
# Author: Rachel Hoesly
# Program Purpose: Extend emissions back for 1A1bc Other transformation for SO2
# Input Files: A.comb_activity_extended_coal.csv
# Output Files:  H.SO2_other_transformation_emissions.csv
# TODO:
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R",'ModH_extension_functions.R') # Additional function files may be required.
log_msg <- "Extending SO2 emissions for 1A1bc-Other-transformation" # First message to be printed to the log
script_name <- "H3.3.add_emissions_SO2_other_transformation.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers, clear_metadata = FALSE )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"

if( em != 'SO2') stop('This script should not run for emission species other than
                      SO2, please check H4.1.proc_Extended_Emissions')

# ---------------------------------------------------------------------------
# 1. Load Data

other_transformation_coal <- readData( 'MED_OUT', 'A.comb_activity_extended_coal' )

# ---------------------------------------------------------------------------
# 2. Calculate SO2 EF

calculated_other_emissions <- data.frame(iso = other_transformation_coal[,c('iso')])

# pre 1900
calculated_other_emissions[paste0('X',1750:1900)] <- other_transformation_coal[paste0('X',1750:1900)]* .01 * 2 * (1 - .1)
# 1900 - 1970
calculated_other_emissions[paste0('X',1900:1970)] <- other_transformation_coal[paste0('X',1900:1970)]* .01 * 2 * (1 - .1) *
                                                         matrix( rep( x= (1970 - 1900:1970)/(1970 - 1900), times = nrow(other_transformation_coal) ),
                                                                 nrow = nrow(other_transformation_coal), byrow = T)
calculated_other_emissions[which(calculated_other_emissions$iso == 'usa' ),paste0('X',1900:1970)] <-
                            other_transformation_coal[ which(other_transformation_coal$iso == 'usa' ),paste0('X',1900:1970)]* .01 * 2 * (1 - .1) *
                                                            (1970 - 1900:1970)/(1970 - 1900)

# post 1970
calculated_other_emissions[paste0('X',1971:end_year)] <- 0

calculated_other_emissions$sector <- "1A1bc_Other-transformation"
calculated_other_emissions$fuel <- 'process'
calculated_other_emissions <- calculated_other_emissions[,c('iso','sector','fuel',paste0('X',1750:end_year))]

# ---------------------------------------------------------------------------
# 3. Write Data and End Script

writeData(calculated_other_emissions, 'MED_OUT', 'H.SO2_calculated_other_transformation_emissions')

logStop()
