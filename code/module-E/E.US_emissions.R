# ------------------------------------------------------------------------------
# Program Name: E.US_emissions.R
# Authors' Names: Tyler Pitkanen, Jon Seibert, Rachel Hoesly
# Date Last Modified: Oct 29, 2015
# Program Purpose: To read in & reformat US emissions inventory data
# Input Files: national_tier1_caps.xlsx
# Output Files: E.[em]_US_inventory.csv
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
    if ( is.na( em ) ) em <- "NOx"
  
# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'common_data.R', "data_functions.R", 
                  "emissions_scaling_functions.R", "analysis_functions.R",
                  "interpolation_extension_functions.R" ) # Additional function files required.
    log_msg <- "Initial reformatting of US emissions" # First message to be printed to the log
    script_name <- "E.US_emissions.R"
    
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define parameters for inventory-specific script

    inventory_data_file <- 'USA/national_tier1_caps'
    inv_data_folder <- "EM_INV"
    inv_name <- 'US' #for naming diagnostic files
    inv_years<-c( 1970, 1975, 1980, 1985, 1990:2014 )

# ------------------------------------------------------------------------------
# 2. Inventory in Standard Form (iso-sector-fuel-years, iso-sector-years, etc)

# Import Sheet
    sheet_name <- em
    if ( em == 'NOx' ) sheet_name <- 'NOX'
    if ( em == 'NMVOC' ) sheet_name <- 'VOC'
    if ( em == 'PM25' ) sheet_name <- 'PM25Primary'
    if ( em == 'PM10' ) sheet_name <- 'PM10Primary'

    inv_data_sheet <- readData( inv_data_folder, inventory_data_file , ".xlsx" ) 

# Process given emission if inventory data exists
    if ( sheet_name %in% names( inv_data_sheet ) ) {
    # Extract the data sheet corresponding to the emissions species
        inv_data_sheet <- data.frame( inv_data_sheet[ sheet_name ] ) 
      
    # Clean rows and columns to standard format; different emissions species
    # require different columns and year ranges
        if ( em == 'NH3' ) {
            inv_years <- c( 1990:2014 )
            inv_data_sheet <- inv_data_sheet[ -1:-3, 1:26 ]
        } else if ( em == 'NMVOC' ) {
            inv_data_sheet <- inv_data_sheet[ -1:-3, 1:30 ]
        } else {
            inv_data_sheet <- inv_data_sheet[ -1:-4, 1:30 ]
        }
        
    # Name columns
        names( inv_data_sheet ) <- c( 'sector', paste0( 'X', inv_years ) )
        
    # Convert years to Xyears
        X_inv_years <- paste0( 'X', inv_years )
        
    # Set iso
        inv_data_sheet$iso <- 'usa'
        inv_data_sheet <- inv_data_sheet[ , c( 'iso', 'sector', 
                                               paste0( 'X', inv_years ) ) ] ### use X_inv_years
        
    # Remove rows with all NAs
        remove.na <- which( apply( inv_data_sheet[ , paste0( 'X', inv_years ) ],
                                   1, function( x ) all.na( x ) ) )
        inv_data_sheet <- inv_data_sheet[ -remove.na, ]
        
    # Make numeric, convert "NA" to NA
        inv_data_sheet[ , paste0( 'X', inv_years ) ] <- 
                suppressWarnings( sapply( inv_data_sheet[ , paste0( 'X',
                                                                    inv_years ) ],
                                          as.numeric ) )
        
    # Remove wildfire emissions; identify those rows with sector "Wildfires" and
    # subtract from "Miscellaneous"
        wildfire_emissions <-
               inv_data_sheet[ which( inv_data_sheet$sector == 'Wildfires' ),
                               X_inv_years ]
        wildfire_emissions[ is.na( wildfire_emissions ) ] <- 0
        
        inv_data_sheet[ which( inv_data_sheet$sector == 'MISCELLANEOUS' ), 
                                                            X_inv_years ] <-
           inv_data_sheet[ which( inv_data_sheet$sector == 'MISCELLANEOUS' ), 
                           X_inv_years ] - wildfire_emissions
        
    # Convert to metric tonnes
        inv_data_sheet[ , paste0( 'X', inv_years ) ] <- 
          as.matrix( inv_data_sheet[ , paste0( 'X', inv_years ) ] ) * 0.9072
        
    # Seems this introduces too many NAs for NH3, so only run for other species
        if ( em != 'NH3' ) {
        # Remove values that are the are constant carried forward
            check_years <- length( X_inv_years ):2
            check_against <- ( length( X_inv_years ) - 1 ):1
            for ( i in seq_along( check_years ) ) {
              
                for ( n in seq_along( inv_data_sheet[ , 1 ] ) ) {
                    if ( any( inv_data_sheet[ n, X_inv_years[ check_years[ i ] ] ] == 
                              inv_data_sheet[ n, X_inv_years[ check_against[ i ] ] ], 
                              na.rm = TRUE ) )
                      inv_data_sheet[ n, X_inv_years[ check_years[ i ] ] ] <- NA
                }
            }
        }
    } else {
    # Write out blank df if no inventory data exists for given emission
        inv_data_sheet <- data.frame()
    }

# ------------------------------------------------------------------------------
# 3. Write standard form inventory
    writeData( inv_data_sheet, domain = "MED_OUT", 
               paste0( 'E.', em, '_', inv_name, '_inventory' ) )

# Every script should finish with this line
    logStop()
# END
