#------------------------------------------------------------------------------
# Program Name: F1.1.US_scaling.R
# Authors' Names: Tyler Pitkanen, Jon Seibert
# Date Last Modified: July 8, 2015
# Program Purpose: To create scaling factors and update emissions estimate for
# the US region from latest emissions working copy by using aggregate 
# US emission trends inventory data.
# Input Files: emissions_scaling_functions.R, F.[em]_scaled_EF.csv, 
#              F.[em]_scaled_emissions.csv, US_sector_mapping.csv, 
#              national_tier1_caps.xlsx
# Output Files: F.[em]_total_scaled_EF.csv, F.[em]_total_scaled_emissions.csv
# Notes: 
# TODO:
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Set working directory to the CEDS “input” directory and define PARAM_DIR as the
# location of the CEDS “parameters” directory relative to the new working directory.
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
    headers <- c( "data_functions.R" ,"emissions_scaling_functions.R" ) # Additional function files required.
    log_msg <- "Modifying emissions factors working copy from US inventory data" # First message to be printed to the log
    script_name <- "F1.1.US_scaling.R"
    
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )
    
# ------------------------------------------------------------------------------
# 1. Define parameters and read in files
    
# For each Module E script, define the following parameters:
    # Inventory parameters. Provide the inventory and mapping file names, the
    #   mapping method (by sector, fuel, or both), and the regions covered by
    #   the inventory (as a vector of iso codes)
        inventory_data_file <- 'national_tier1_caps.xlsx'
        sector_fuel_mapping <- 'US_sector_mapping'
        mapping_method <- 'sector'
        region <- c( 'usa' )
    # Interpolation/Extrapolation settings. Interpolation can be "linear" or
    #   "none". Extrapolation can be "constant", "linear", or "none", and
    #   can be defined differently before and after the inventory time range.
        interpolation <- "linear"
        extrapolation_before <- "constant"
        extrapolation_after  <- "constant"
    
# Read in the inventory data, mapping file, the specified emissions species, and
#   the latest versions of the scaled EFs and 
    F.readScalingData( inventory_data_file, sector_fuel_mapping )
    
# ------------------------------------------------------------------------------
# 2. Select the inventory data and arrange it into the standard CEDS format

# Select the excel sheet of interest
    sheets <- names( inv_data_full )
    sheet_num <- grep( em_species, sheets )
    inv_data_sheet <- inv_data_full[[sheet_num]]
    
# Put inventory excel sheet into a form that is easier to manipulate in R; fix 
#   col names and eliminate white space, rows of NA values, sheet titles, etc.
# Select rows.
    data_row_start <- grep( "Source Category", inv_data_sheet[ ,1] ) + 1 
    inv_data <- inv_data_sheet[ data_row_start:nrow( inv_data_sheet ), ]
    inv_data <- inv_data[ rowSums( is.na( inv_data ) ) != ncol( inv_data ), ]
    names( inv_data ) <- inv_data_sheet[ data_row_start - 1, ]
 
# Select columns.
    names( inv_data ) <- paste0( "X", names( inv_data ) )
    names( inv_data )[[1]] <- "inv_sector"
    X_scaling_years <- X_emissions_years[ X_emissions_years %in% names( inv_data ) ]
    scaling_years <- as.numeric( gsub2( 'X', '', X_scaling_years ) )
    std_form_inv <- inv_data[ , 1:( length( X_scaling_years ) + 1 ) ]
 
# Remove redundant and blank rows, and aggregate by the mapping file 
    inv_data <- F.invAggregate( std_form_inv )
    
# ------------------------------------------------------------------------------
# 3. Arrange the CEDS emissions data to match the inventory data
 
# Take the ceds data for the regions of interest and aggregate with the
#   specified mapping method
    F.cedsAggregate( input_ef, input_em, region, mapping_method )

# ------------------------------------------------------------------------------
# 4. Create scaling factors and scaled emissions factors

   writeData( inv_data, fn = "USADebug_inventory_data", domain = "DIAG_OUT" ) 
   writeData( ceds_em_data, fn = "USADebug_scaled_em_data", domain = "DIAG_OUT" ) 

# Get ratio of inventory data to CEDS data
    F.scale( ceds_em_data, inv_data, scaling_years, int_method = interpolation,
        pre_ext_method = extrapolation_before, 
        post_ext_method = extrapolation_after, region )

# ------------------------------------------------------------------------------
# 5. Output
# Write out the complete set of emissions and emission factors.
# Only those sectors/fuels/country that have been modified by this routine are changed

    
    F.write( ef_output, em_output, domain = "MED_OUT" ) 

    logStop()
