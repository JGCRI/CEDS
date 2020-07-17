#------------------------------------------------------------------------------
# Program Name: E.US-EPA_emissions.R
# Authors' Names: Linh Vu
# Date Last Modified: 8 Sept 2016
# Program Purpose: To read in & reformat US EPA emissions inventory data
# Input Files: EPA_Table3-25.csv
# Output Files: E.[em]_USEPA_inventory.csv
# Notes: 
# TODO:
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Get emission species first so can name log appropriately
    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[1]
    if ( is.na( em ) ) em <- "CO2"
  
# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'common_data.R', "data_functions.R", 
                  "emissions_scaling_functions.R", "analysis_functions.R",
                  "interpolation_extension_functions.R" ) # Additional function files required.
    log_msg <- "Initial reformatting of US EPA emissions" # First message to be printed to the log
    script_name <- "E.US-EPA_emissions.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define parameters for inventory-specific script
    
    inventory_data_file <- paste0( 'USA/EPA_inventory_', em )
    inv_data_folder <- "EM_INV"
    inv_name <- 'US-EPA' #for naming diagnostic files
    inv_years <- 1990:2014

# ------------------------------------------------------------------------------
# 2. Inventory to Standard Form (iso-sector-fuel-years, iso-sector-years, etc)

# Get filepath from inventory parameters
    file_path <- filePath( inv_data_folder, inventory_data_file, 
                           extension = ".csv" )

# Process given emission if inventory data exists
    if ( file.exists( file_path ) ) {
    # Read inventory
        waste <- readData( inv_data_folder, inventory_data_file ) 
    # Retain only 4 waste products, drop the product name, and sum
        waste <- filter( waste, waste_product %in% 
                            c( "Plastics", 
                               "Synthetic Rubber in Tires", 
                               "Carbon Black in Tires", 
                               "Synthetic Fibers" ) ) %>%
                             select( -waste_product ) %>% 
                            group_by( sector, units ) %>% 
                        summarise_each( funs( sum ) )
        
    # Add iso tag
        waste$iso <- "usa"
    # Convert year headers to xyears
        names( waste )[ grepl( "X", names( waste ) ) ] <- 
                 substr( names( waste )[ grepl( "X", names( waste ) ) ], 1, 5 )
        
    # Get X waste years
        X_waste_years <- names( waste )[ grepl( "X", names( waste ) ) ]
        
        inv_data_sheet <- waste[ c( "iso", "sector", 
                                    paste0( "X", inv_years ) ) ]  ### Use X waste years
    
    # Write out blank df if no inventory data exists for given emission
    } else {
        inv_data_sheet <- data.frame()
    }


# ------------------------------------------------------------------------------
# 3. Write standard form inventory
    writeData( inv_data_sheet , domain = "MED_OUT",
               paste0( 'E.', em, '_', inv_name, '_inventory' ) )

# Every script should finish with this line
    logStop()
# END
