# ------------------------------------------------------------------------------
# Program Name: E.Taiwan_emission.R
# Author(s): Leyang Feng
# Date Last Updated: March 16, 2016
# Program Purpose: To read in and reformat Taiwan emissions data.
# Input Files: Taiwan_emissions.xlsx
# Output Files: E.[EM]_TWN_inventory.csv
# Notes: Taiwan emission data are only available for year 2003, 2006, 2010
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
    headers <- c( 'common_data.R', "data_functions.R", "analysis_functions.R" ) # Additional function files required.
    log_msg <- "Generating Taiwan emission inventory data" # First message to be printed to the log
    script_name <- paste0( em, "-E.Taiwan_emission.R" )
    
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define parameters for inventory specific script

# Stop script if running for unsupported species
#if ( em %!in% c( 'SO2', 'NOx', 'NMVOC', 'CO' ) ) {
#  stop (paste( ' TWN scaling is not supported for emission species', em, 'remove from script
#               list in F1.1.inventory_scaling.R') )
#}

# For each Module E script, define the following parameters:
# Inventory parameters. Provide the inventory and mapping file names, the
#   mapping method (by sector, fuel, or both), and the regions covered by
#   the inventory (as a vector of iso codes)
    inventory_data_file <- 'Taiwan_emissions'
    subfolder_name <- 'Taiwan/'
    inv_data_folder <- "EM_INV"
    sector_fuel_mapping <- 'TWN_scaling_mapping'
    mapping_method <- 'sector'
    inv_name <- 'TWN' #for naming diagnostic files
    region <- c( "twn" ) 
    inv_years<-c( 2003, 2006, 2010 )



# ------------------------------------------------------------------------------
# 2. Read in the inventory


    if ( em %!in% c( 'SO2', 'NOx', 'NMVOC', 'CO' ) ) {
    # write out a dummy file for unsupported species
        inv_data_species <- data.frame( )
    
    } else { 
    # Import Sheets containing 2003, 2006, 2010 data.
        sheet_name <- "2003"
        inv_data_sheet_two <- readData( inv_data_folder, inventory_data_file, ### Why two, three, four, not 1,2,3?
                                        ".xlsx", skip = 1, 
                                        sheet_selection = sheet_name, 
                                        domain_extension = subfolder_name )
        sheet_name <- "2006"
        inv_data_sheet_three <- readData( inv_data_folder, inventory_data_file, 
                                          ".xlsx", skip = 1, 
                                          sheet_selection = sheet_name, 
                                          domain_extension = subfolder_name )
        sheet_name <- "2010"
        inv_data_sheet_four <- readData( inv_data_folder, inventory_data_file, 
                                         ".xlsx", skip = 1, 
                                         sheet_selection = sheet_name, 
                                         domain_extension = subfolder_name )

# ------------------------------------------------------------------------------
# 3. Convert to standard format

    # Trim each dataframe to the desired rows and rename
        keep_columns <- c( 'sector', 'sub-sector1', 'sub-sector2', 'SOx', 'NOx', 'NMHC', 'CO' )
        col_names <- c( 'sector', 'subsector_l1', 'subsector_l2', 'SO2', 'NOx', 'NMVOC', 'CO' )
        df2003 <- subset( inv_data_sheet_two, select = keep_columns )
        colnames( df2003 ) <- col_names 
        df2003 <- df2003[ !is.na( df2003$sector ), ]
        df2006 <- subset( inv_data_sheet_three, select = keep_columns )
        colnames( df2006 ) <- col_names
        df2006 <- df2006[ !is.na( df2006$sector ), ]
        df2010 <- subset( inv_data_sheet_four, select = keep_columns )
        colnames( df2010 ) <- col_names 
        df2010 <- df2010[ !is.na( df2010$sector ), ]

    # construct unified sector names by combining l1 and l2 names
        df2003$sector <- paste( df2003$sector, df2003$subsector_l1, 
                                df2003$subsector_l2, sep = '_' )
        df2003$sector <- gsub( '_NA', '', df2003$sector )
        df2003 <- df2003[ , !( colnames( df2003 ) %in% c( 'subsector_l1', 
                                                          'subsector_l2' ) ) ]
        df2006$sector <- paste( df2006$sector, df2006$subsector_l1, 
                                df2006$subsector_l2, sep = '_' )
        
        df2006$sector <- gsub( '_NA', '', df2006$sector )
        df2006 <- df2006[ , !( colnames( df2006 ) %in% c( 'subsector_l1', 
                                                          'subsector_l2' ) ) ]
        df2010$sector <- paste( df2010$sector, df2010$subsector_l1, 
                                df2010$subsector_l2, sep = '_' )  
        df2010$sector <- gsub( '_NA', '', df2010$sector )
        df2010 <- df2010[ , !( colnames( df2010 ) %in% c( 'subsector_l1', 
                                                          'subsector_l2' ) ) ]

    # Make sure each df contains all sectors, even those with 0 emissions for
    # that year
        df2003 <- merge( df2003, df2006[ 'sector' ], by = 'sector', all = T )
        df2003 <- merge( df2003, df2010[ 'sector' ], by = 'sector', all = T )
        df2006 <- merge( df2006, df2003[ 'sector' ], by = 'sector', all = T )
        df2010 <- merge( df2010, df2003[ 'sector' ], by = 'sector', all = T )

    # Keep only data for the given emissions species
        sector <- df2003$sector
        X2003 <- df2003[ , em ]
        X2006 <- df2006[ , em ]
        X2010 <- df2010[ , em ]

        inv_data_species <- data.frame( sector, X2003, X2006, X2010 ) 

    # Convert from tonnes to kt
        inv_data_species[ , paste0( 'X', inv_years ) ] <- 
          as.matrix( inv_data_species[ , paste0( 'X', inv_years ) ] ) / 1000

    # Clean rows and columns to standard format
        inv_data_species$iso <- 'twn'
        inv_data_species$unit <- 'kt'
        inv_data_species <- inv_data_species[ , c( 'iso', 'sector', 'unit',
                                                   paste0( 'X', inv_years ) ) ]

    }

# ------------------------------------------------------------------------------
# 4. Output
# write standard form inventory
    writeData( inv_data_species , domain = "MED_OUT", 
               paste0( 'E.', em, '_', inv_name, '_inventory' ) )
    
# Every script should finish with this line
    
    logStop()
# END

