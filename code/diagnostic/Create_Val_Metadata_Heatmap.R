# ------------------------------------------------------------------------------
# Program Name: Create_Val_Metadata_Heatmap.R
# Author: Ben Goldstein
# Date Last Updated: 9 June 2017
# Program Purpose: Uses the F.create_EF_value_meta_heatmap to create a heatmap
#                  diagnostic of the value metadata for a single country.
# Input Files: F.[em]_scaled_EF-value_metadata.csv
#
# Output Files: A figure in the diagnostic-output
# TODO:
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R",'common_data.R',
                  'IO_functions.R', 'emissions_scaling_functions.R') # Additional function files may be required.
    log_msg <- "Create value metadata heatmap" # First message to be printed to the log
    script_name <- "Create_Val_Metadata_Heatmap.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "NH3"

# ---------------------------------------------------------------------------
# 1. Set parameters
#    Choose parameters for this run. Define the country and the desired sectors
#    (use "all" to plot all sectors)

    run_isos <- c( 'rus' )
    # run_isos <- c('aus')
    run_sectors <- 'all'

    value_metadata <- readData( "MED_OUT", paste0( "F.", em, "_", "scaled_EF-value_metadata" ), meta = FALSE, to_numeric=FALSE)
    value_metadata <- melt(value_metadata, id.vars = c('iso','sector','fuel'))
    names( value_metadata ) <- c( "iso", "sector", "fuel", "year", "comment" )
    value_metadata$comment <- as.character(value_metadata$comment)
# Example setting specific sectors:
    # run_sectors = c( "2C_Metal-production",
    #              "2D_Degreasing-Cleaning",
    #              "2D_Paint-application",
    #              "2D3_Chemical-products-manufacture-processing",
    #              "2D3_Other-product-use",
    #              "2H_Pulp-and-paper-food-beverage-wood",
    #              "2L_Other-process-emissions" )

# ---------------------------------------------------------------------------
# 2. Exectue function

    for (run_iso in run_isos) {
        F.create_EF_value_meta_heatmap( meta_notes = value_metadata, iso = run_isos, sectors = run_sectors )
    }
