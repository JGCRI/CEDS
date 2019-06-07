#------------------------------------------------------------------------------
# Program Name: E.CAN_emissions_newerData.R
# Authors' Names: Tyler Pitkanen, Jon Seibert, Rachel Hoesly, Steve Smith, Huong Nguyen
# Date Last Modified: Sept 07, 2016
# Program Purpose: To read in & reformat Canada emissions inventory data. 
#                  This file uses the newer format used since 2012. This data
#                  only extends back to 1990, so older data is still used back
#                  to 1985 in a separate scaling operation. This newer data
#                  should be used last so that any discrepancies are resolved in
#                  favor of the newer data.
# Input Files: ape_results_e_[em]_2014.xlsx
# Output Files: E.[em]_CAN_inventory.csv
# Notes:
# TODO: Re-write read-in so that order of years is taken from input data instead of assumed.
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Get emission species first so can name log appropriately
    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "CO"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'common_data.R', "data_functions.R", 
                  "emissions_scaling_functions.R", "analysis_functions.R",
                  "interpolation_extension_functions.R" ) # Additional function files required.
    log_msg <- "Initial reformatting of Canada emissions (newer data)" # First message to be printed to the log
    script_name <- "E.CAN_emissions_newerData2014.R"
    
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define parameters for inventory specific script
    inventory_data_file <- paste0( 'ape_results_e_', em, '_2014' )
    subfolder_name <- 'Canada/'
    inv_data_folder <- "EM_INV"
    inv_name <- 'CAN' # For naming diagnostic files
    inv_years <- c( 1990:2014 )
# Because this data comes read in as reversed.
    inv_years_reversed <- c( 2014:1990 )

# ------------------------------------------------------------------------------
# 2. Inventory in Standard Form (iso-sector-fuel-years, iso-sector-years, etc)
    file_path <- filePath( inv_data_folder, inventory_data_file, 
                           extension = ".xlsx", 
                           domain_extension = subfolder_name )

# Process given emission if inventory data exists
    if ( file.exists( file_path ) ) {
    # Import Sheet
        sheet_name <- "Sheet1"
        inv_data_sheet <- readData( inv_data_folder,
                                    domain_extension = subfolder_name,
                                    inventory_data_file , ".xlsx",
                                    sheet_selection = sheet_name )
        
    # Clean rows and columns to standard format
        inv_data_sheet <- inv_data_sheet[ -1:-7, ]
    # Rename cols; add iso cols
        names( inv_data_sheet ) <- c( 'sector', paste0( 'X', inv_years_reversed ) )
        inv_data_sheet$iso <- 'can'
        inv_data_sheet <- inv_data_sheet[ , c( 'iso', 'sector', 
                                               paste0( 'X', inv_years ) ) ]
        
    # Remove rows with all NAs
        remove.na <- which( apply( inv_data_sheet[ , paste0( 'X', inv_years ) ],
                                   1, function( x ) all.na( x ) ) )
        inv_data_sheet <- inv_data_sheet[ -remove.na, ]
        
    # Make numeric
        inv_data_sheet[ , paste0( 'X', inv_years ) ] <- 
                 sapply( inv_data_sheet[ , paste0( 'X', inv_years ) ], 
                         as.numeric )
    # Convert from tonnes to kt
        inv_data_sheet[ , paste0( 'X', inv_years ) ] <- 
               as.matrix( inv_data_sheet[ , paste0( 'X', inv_years ) ] ) / 1000
        
    # Write out blank df if no inventory data exists for given emission
    } else {
        inv_data_sheet <- data.frame()
    }


# ------------------------------------------------------------------------------
# 3. Write standard form inventory
    
    writeData( inv_data_sheet, domain = "MED_OUT", 
               paste0( 'E.', em, '_', inv_name, '_inventory' ) )

# Every script should finish with this line
    logStop()
# END
