#------------------------------------------------------------------------------
# Program Name: E.Argentina_emissions.R
# Authors' Names: Tyler Pitkanen, Jon Seibert, Rachel Hoesly, Steve Smith, Ryan Bolt
# Date Last Modified: January 20, 2016
# Program Purpose: To read in & reformat Argentina emissions inventory data
#                  This data only contains data from 1990 - 2011, missing data
#                  from 2000 and 2010. Units are initially in metric tonnes
# Input Files: Argentina_Translation.xlsx, Argentina Inventario 1990-2012-ipcc1996.xlsx,
# Output Files: E.[em]_ARG_inventory.csv
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
    log_msg <- "Initial reformatting of Argentina emissions" # First message to be printed to the log
    script_name <- "E.Argentina_emissions.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )


# ------------------------------------------------------------------------------
# 0.5. Define parameters for inventory specific script
    em_temp = em  # Variable to use for interacting with emissions file
    if ( em_temp == "NMVOC" ) {
        em_temp <- "COVNM"
    }

    inv_data_folder <- "EM_INV"
    subfolder_name <- 'Argentina/'
    inv_name <- 'ARG' #for naming diagnostic files
    inv_years<-c( 1990:1999, 2001:2009, 2011 )

# -------------------------------------------
# 1. Reading in translation file and the first sheet of Argentina Inventory

# Read in Argentina translation mapping
    translation_file <- 'Argentina_Translation'
    sheet_name <- "Sheet1"
    translation <- readData( inv_data_folder, translation_file, ".xlsx",
                             domain_extension = subfolder_name,
                             sheet_selection = sheet_name, meta = F )

# Read in Argentina inventory
    attempt_file <- 'Argentina Inventario 1990-2012-ipcc1996'
    sheet_name2 <- "Inventario 1990"
    attempt <- readData( inv_data_folder, attempt_file, ".xlsx",
                         domain_extension = subfolder_name,
                         sheet_selection = sheet_name2, skip = 2 )

# Renaming sector column, removing NA sectors and translating sector names from Spanish to English.
    colnames( attempt )[ 2 ] <- "sector"
    attempt <- attempt[ complete.cases( attempt$sector ), ]
    attempt[ , 2 ] <- translation$English[ match( attempt[ , 2 ],
                                                  translation$Spanish ) ]


# -------------------------------------------
# Process given emission if inventory data exists
# We need the sectors (column 2) and the emission (column name = emission).

# Check to make sure the desired emissions species has a data column in the
# inventory
    if ( em_temp %in% names( attempt ) ) {
    # Trim to the emissions species' column
        inv_data_sheet <- attempt[ , c( 'sector', em_temp ) ]
        inv_data_sheet[ , 2 ] <- as.numeric( inv_data_sheet[ , 2 ] )

    # Renaming specie column to year. Making NA's into 0's.
        colnames( inv_data_sheet )[ colnames( inv_data_sheet ) == em_temp ] <- "X1990"
        inv_data_sheet[ is.na( inv_data_sheet ) ] <- 0

    # Adding years to dataframe in a loop. Each year has its own sheet in the
    # inventory .xlsx so we have to loop through all of them
        for ( year in inv_years ) {
        # Reading data sheet in.
            attempt <- 0
            old_names <- colnames( inv_data_sheet )
        # Get the name of the year-specific xlsx sheet
            attempt_file <- 'Argentina Inventario 1990-2012-ipcc1996'
            sheet_name2 <- paste( "Inventario", as.character( year ) )
        # Read in the sheet for each year
            attempt <- readData( inv_data_folder, attempt_file , ".xlsx",
                                 domain_extension = subfolder_name,
                                 sheet_selection = sheet_name2, skip = 2 )

        # Translating sectors from Spanish to English. Removing NA rows from sector list.
            colnames( attempt )[ 2 ] <- "sector"
            attempt <- attempt[ complete.cases( attempt$sector ), ]
            attempt[ , 2 ] <- translation$English[ match( attempt[ , 2 ],
                                                          translation$Spanish ) ]

        #
            attempt <- attempt[ , c( 'sector', em_temp ) ]
            attempt[ , 2 ] <- as.numeric( attempt[ , 2 ] )
            attempt[ is.na( attempt ) ] <- 0

            inv_data_sheet <- cbind( inv_data_sheet, attempt[ , em_temp ] )
            colnames( inv_data_sheet ) <- c( old_names, paste0( "X", year ) )
        }

    # Make numeric and convert from tonnes to kt
        inv_data_sheet[ , paste0( 'X', inv_years ) ] <-
             as.matrix( inv_data_sheet[ , paste0( 'X', inv_years ) ] ) / 1000

    # Adding ISO column
        inv_data_sheet$iso <- 'arg'
        inv_data_sheet <- inv_data_sheet[ , c( 'iso', 'sector',
                                               paste0( 'X', inv_years ) ) ]


    # -------------------------------------------
    # Write out blank df if no inventory data exists for given emissions
    } else {
        inv_data_sheet <- data.frame()
    }

# ------------------------------------------------------------------------------
# 2. Write standard form inventory

    writeData( inv_data_sheet , domain = "MED_OUT",
               paste0( 'E.', em, '_', inv_name, '_inventory' ) )

# Every script should finish with this line
    logStop()
# END
