#------------------------------------------------------------------------------
# Program Name: E.Japan_emissions.R
# Authors' Names: Tyler Pitkanen, Jon Seibert, Rachel Hoesly, Steve Smith, Ryan Bolt
# Date Last Modified: Jan 15, 2016
# Program Purpose: To read in and reformat Japan emissions inventory data
# Input Files: CEDS_REAS_JAPAN.xlsx
# Output Files: E.[em]_Japan_inventory.csv
# Notes: 
# TODO: Re-write read-in so that order of years is taken from input data instead of assumed.
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Get emission species first so can name log appropriately
    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[1]
    if ( is.na( em ) ) em <- "NOx"
  
# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'common_data.R', "data_functions.R", 
                  "emissions_scaling_functions.R", "analysis_functions.R" ) # Additional function files required.
    log_msg <- "Initial reformatting of Japan emissions" # First message to be printed to the log
    script_name <- "E.Japan_emissions.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define parameters for inventory specific script
    inventory_data_file <- 'Japan/CEDS_REAS_JAPAN'
    inv_data_folder <- "EM_INV"
    inv_name <- 'Japan' #for naming diagnostic files
    inv_years<-c( 1950:2010 )

# ------------------------------------------------------------------------------
# 1.5. Inventory in Standard Form (iso-sector-fuel-years, iso-sector-years, etc)
# Import Sheet
    sheet_name <- em
    
    inv_data_sheet <- readData( inv_data_folder, inventory_data_file, ".xlsx" )

# Process given emission if inventory data exists
    if ( sheet_name %in% names( inv_data_sheet ) ) {
        inv_data_sheet <- data.frame( inv_data_sheet[ sheet_name ] )
      
    # Clean rows and columns to standard format
        names( inv_data_sheet ) <- c( 'sector', paste0( 'X', inv_years ) )
        inv_data_sheet$iso <- 'jpn'
        inv_data_sheet <- inv_data_sheet[ , c( 'iso', 'sector', 
                                               paste0( 'X', inv_years ) ) ]
      
    # Make numeric
        inv_data_sheet[ , paste0( 'X', inv_years ) ] <- 
                sapply( inv_data_sheet[ , paste0( 'X', inv_years ) ], 
                        as.numeric )
        
    # Convert from tonnes to kt
        inv_data_sheet[ , paste0( 'X', inv_years ) ] <- 
            as.matrix( inv_data_sheet[ , paste0( 'X', inv_years ) ] ) / 1000
      
    # Japan's data goes to 1950 but the EF and emissions data sheets only go
    # back to 1960. Therefore, we need to remove data from 1950 - 1959 for the
    # time being.
        keep <- c( "iso", "sector", paste0( 'X', 1960:2010 ) )
        inv_data_sheet <- inv_data_sheet[ , keep ]
    
    # Write out blank df if no inventory data exists for given emission
    } else  {
        inv_data_sheet <- data.frame()
    }

# ------------------------------------------------------------------------------
# 2. Write standard form inventory
    writeData( inv_data_sheet, domain = "MED_OUT", 
               paste0( 'E.', em, '_', inv_name, '_inventory' ) )
  
# Every script should finish with this line
    
    logStop()
# END
