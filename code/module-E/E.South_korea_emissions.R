#------------------------------------------------------------------------------
# Program Name: E.South_korea_emissions.R
# Authors' Names: Steve Smith, Ryan Bolt
# Program Purpose: Read emissions estimate South Korea inventory file
# Note: This file is not in git due to size. This utility file should not be in makefile.
# This extracts data from 1999 - 2012
# Units are initially in Mg or Metric Tonnes
# Input Files: Air pollutants_South Korea_1999_2012_Korean font.xlsx
# Output Files: F.[em]Korea_inventory.csv
# Notes:
# TODO:
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'common_data.R', "data_functions.R",
                  "emissions_scaling_functions.R" ) # Additional function files required.
    log_msg <- "test inventory data" # First message to be printed to the log
    script_name <- "E.South_korea_emissions.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[1]
    if ( is.na( em ) ) em <- "SOx"

# ------------------------------------------------------------------------------
# 0.5. Define parameters for inventory specific script

# Stop script if running for unsupported species
    if ( em %!in% c( 'SO2', 'NOx', 'CO', 'NMVOC' ) ) {
      stop( paste( 'SKorea scaling is not supported for emission species', em,
                   'remove from script list in F1.1.inventory_scaling.R' ) )
    }
# Variable to use for interacting with emissions file
    em_temp = em
# South Korea only reports SOx which is similiar to SO2.
    if ( em_temp == "SO2" ) {
        em_temp <- "SOx"
    }

    if ( em_temp == "NMVOC" ) {
        em_temp <- "VOC"
    }

    inv_name <- 'SKorea' # For naming diagnostic files



# ------------------------------------------------------------------------------
# 1. Read in initial input

# Inventory-specific parameters
    inv_data_folder <- "EM_INV"
    inv_years <- c( 1999:2012 )
    inventory_filename <- 'Air pollutants_South Korea_1999_2012_Korean font'
    subfolder_name <- 'Korea/'
    sheet_name2 <- "1999"

# Read in inventory data
    Inventory_data <- readData( inv_data_folder, domain_extension =
                                                   subfolder_name,
                                inventory_filename , ".xlsx",
                                sheet_selection = sheet_name2, skip = 3 )

# Seat headers
    colnames( Inventory_data ) <- c( paste0( "X", 1:10 ),
                                     colnames( Inventory_data[
                                          11:ncol( Inventory_data ) ] ) )
# Get the
    inv_data_sheet <- Inventory_data[ , c( paste0( "X", 1:10 ), em_temp ) ]

# Set years
    specie_years <- c( paste0( "X", 1:10 ), "X1999" )
    colnames( inv_data_sheet ) <- specie_years

# ------------------------------------------------------------------------------
# 2. Read in Emissions

    for( year in 2000:2012 ) {
    	old_names <- colnames( inv_data_sheet )
    	sheet_name2 <- as.character( year )
    	Inventory_data <- readData( inv_data_folder, inventory_filename , ".xlsx",
    						                  domain_extension = subfolder_name,
    						                  sheet_selection = sheet_name2, skip = 3 )

    	colnames( Inventory_data ) <- c( paste0( "X", 1:10 ),
    	                                 colnames( Inventory_data[
    	                                           11:ncol( Inventory_data ) ] ) )
    	Inventory_data <- Inventory_data[ , c( paste0( "X", 1:10 ), em_temp ) ]

    	inv_data_sheet <- merge( inv_data_sheet, Inventory_data,
    	                         by = c( paste0( "X", 1:10 ) ),
    	                         all.x = T, all.y = F)

    	colnames( inv_data_sheet ) <- c( old_names, paste0( "X", year ) )
    }

# Changing NA's to 0
    inv_data_sheet[ is.na( inv_data_sheet ) ] <- 0

# Make numeric and convert from kg to kt
    inv_data_sheet[ , paste0( 'X', inv_years ) ] <-
             sapply( inv_data_sheet[ , paste0( 'X', inv_years ) ], as.numeric )
    inv_data_sheet[ , paste0( 'X', inv_years ) ] <-
        as.matrix( inv_data_sheet[ , paste0( 'X', inv_years ) ] ) / 1000 / 1000

# We need to get an iso column in and also decide which columns are most important for scaling.
# I believe column 6 is the only one we really need.All the categories will go to the scaling sector
# and then into the CEDS sector.

#names(df)[names(df) == 'old.var.name'] <- 'new.var.name'
    colnames( inv_data_sheet )[ colnames( inv_data_sheet ) == 'X6' ] <- "sector"
    inv_data_sheet <- inv_data_sheet[ , c( 'sector',
                                           paste0( 'X', inv_years ) ) ]
    inv_data_sheet$iso <- 'kor'
    inv_data_sheet <- inv_data_sheet[ , c( 'iso', 'sector', paste0( 'X', inv_years ) ) ]


# write standard form inventory
    writeData( inv_data_sheet, domain = "MED_OUT",
               paste0( 'E.', em, '_', inv_name, '_inventory' ) )
    inventory_data_file <- paste0( 'E.', em, '_', inv_name, '_inventory' )
    inv_data_folder <- 'MED_OUT'


# Every script should finish with this line
    logStop()
# END
