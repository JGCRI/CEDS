#------------------------------------------------------------------------------
# Program Name: E.US-GHG_emissions.R
# Authors' Names: Rachel Hoesly
# Date Last Modified: Oct 29, 2015
# Program Purpose: To read in & reformat US emissions inventory data
# Input Files: US_GHG_inventory.csv
# Output Files: E.[em]_US_GHG_inventory.csv
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
    if ( is.na( em ) ) em <- "CH4"
  
# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'common_data.R',"data_functions.R" ) # Additional function files required.
    log_msg <- "Initial reformatting of US GHG emissions" # First message to be printed to the log
    script_name <- "E.US-GHG_emissions.R"
    
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define parameters for inventory specific script

    inventory_data_file <- 'USA/US_GHG_inventory'
    inv_data_folder <- "EM_INV"
    inv_name <- 'US_GHG' #for naming diagnostic files
    inv_years<-c( 1990:2014 )

# ------------------------------------------------------------------------------
# 2. Inventory in Standard Form (iso-sector-fuel-years, iso-sector-years, etc)

# Import Sheet
    inv_data_sheet <- readData( inv_data_folder, inventory_data_file ) 
    
    ghg_em <- c( 'CO2', 'N2O', 'CH4', 'NF3', 'SF6', 'PFCs', 'HFCs' )

# Process given emission if inventory data exists
    if ( em %in% ghg_em ) {
    
    # select rows and columns 
        inv_data <- inv_data_sheet[ 3:nrow( inv_data_sheet ),
                                    2:ncol( inv_data_sheet ) ]
        names( inv_data ) <- inv_data_sheet[ 2, 2:ncol( inv_data_sheet ) ]
        names( inv_data )[ 1 ] <- 'Source'
        inv_data <- inv_data[ -grep( "Does not exceed", 
                                     inv_data[ , 1 ] ):-nrow( inv_data_sheet ), ]
    
    # Define emission species
        inv_data$EM <- NA
        inv_data[ inv_data$Source %in% ghg_em, 'EM' ] <- 
                  inv_data[ inv_data$Source %in% ghg_em, 'Source' ]
        inv_data <- fill( inv_data, EM )
      
    # remove totals
        inv_data <- filter( inv_data, Source %!in% ghg_em, EM %in% em )
    # add X to year columns
        inv_data <- inv_data[ c( 'Source', paste( inv_years ) ) ]
        names( inv_data ) <- c( 'sector', paste0( 'X', inv_years ) )
        
    # convert to numeric
        inv_data[ paste0( 'X', inv_years ) ] <-
                  apply( X = inv_data[ paste0( 'X', inv_years ) ], 
                         2, FUN = gsub, pattern = ',', replacement = '' )
        inv_data[ paste0( 'X', inv_years ) ] <-
                  apply( X = inv_data[ paste0( 'X', inv_years ) ], 
                         2, as.numeric )
        
    # replace NAs with 0
        inv_data <- replace( inv_data, is.na( inv_data ), 0 )
       
    # Write out blank df if no inventory data exists for given emission
    } else {
        inv_data <- data.frame()
    }


# ------------------------------------------------------------------------------
# 3. Write standard form inventory
    writeData( inv_data, domain = "MED_OUT", 
               paste0( 'E.', em, '_', inv_name, '_inventory' ) )

# Every script should finish with this line
    logStop()
# END
