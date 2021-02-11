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
if ( is.na( em ) ) em <- "CO2"

em.read <- em
if (em %in% c ('BC','OC')) em.read <- "PM2.5"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'common_data.R', "data_functions.R", "analysis_functions.R","emissions_scaling_functions.R" ) # Additional function files required.
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
    inv_years<-c( 2003, 2006, 2010, 2013, 2016 )



# ------------------------------------------------------------------------------
# 2. Read in the inventory


    if ( em.read %!in% c( 'SO2', 'NOx', 'NMVOC', 'CO','PM2.5' ) ) {
    # write out a dummy file for unsupported species
        inv_data_species <- data.frame( )

    } else {
    # Import Sheets containing 2003, 2006, 2010, 2013, 2016 data.

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

        sheet_name <- "2013"
        inv_data_sheet_five <- readData( inv_data_folder, inventory_data_file,
                                         ".xlsx", skip = 4,
                                         sheet_selection = sheet_name,
                                         domain_extension = subfolder_name )

        sheet_name <- "2016"
        inv_data_sheet_six <- readData( inv_data_folder, inventory_data_file,
                                         ".xlsx", skip = 4,
                                         sheet_selection = sheet_name,
                                         domain_extension = subfolder_name )

    # Combine rows with different emission types
        id_cols <- c( "source type", "sector", "sub-sector1", "sub-sector2" )
        inv_data_sheet_five <- inv_data_sheet_five %>%
            dplyr::select( -"emission-type" ) %>% # remove id column not using, as it won't be able to aggregate over this, as it won't be included in the group_by()
            dplyr::group_by_at( id_cols )  %>% # group by columns in the id _col object
            dplyr::summarise_all( funs( sum(., na.rm = T ) ) ) %>% # aggregate
            dplyr::ungroup( ) # ungroup

        inv_data_sheet_six <- inv_data_sheet_six %>%
            dplyr::select( -"emission-type" ) %>% # remove id column not using, as it won't be able to aggregate over this, as it won't be included in the group_by()
            dplyr::group_by_at( id_cols )  %>% # group by columns in the id _col object
            dplyr::summarise_all( funs( sum(., na.rm = T ) ) ) %>% # aggregate
            dplyr::ungroup( ) # ungroup



# ------------------------------------------------------------------------------
# 3. Convert to standard format

    # Trim each dataframe to the desired rows and rename
        keep_columns <- c( 'sector', 'sub-sector1', 'sub-sector2','SOx', 'NOx', 'NMHC', 'CO','PM2.5' )
        col_names <- c( 'sector', 'subsector_l1', 'subsector_l2', 'SO2', 'NOx', 'NMVOC', 'CO','PM2.5' )
        df2003 <- subset( inv_data_sheet_two, select = keep_columns )
        colnames( df2003 ) <- col_names
        df2003 <- df2003[ !is.na( df2003$sector ), ]
        df2006 <- subset( inv_data_sheet_three, select = keep_columns )
        colnames( df2006 ) <- col_names
        df2006 <- df2006[ !is.na( df2006$sector ), ]
        df2010 <- subset( inv_data_sheet_four, select = keep_columns )
        colnames( df2010 ) <- col_names
        df2010 <- df2010[ !is.na( df2010$sector ), ]
        df2013 <- subset( inv_data_sheet_five, select = keep_columns )
        colnames( df2013 ) <- col_names
        df2013 <- df2013[ !is.na( df2013$sector ), ]
        df2016 <- subset( inv_data_sheet_six, select = keep_columns )
        colnames( df2016 ) <- col_names
        df2016 <- df2016[ !is.na( df2016$sector ), ]

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


        df2013$sector <- paste( df2013$sector, df2013$subsector_l1,
                                df2013$subsector_l2, sep = '_' )
        df2013$sector <- gsub( '_NA', '', df2013$sector )
        df2013 <- df2013[ , !( colnames( df2013 ) %in% c( 'subsector_l1',
                                                          'subsector_l2' ) ) ]

        df2016$sector <- paste( df2016$sector, df2016$subsector_l1,
                                df2016$subsector_l2, sep = '_' )
        df2016$sector <- gsub( '_NA', '', df2016$sector )
        df2016 <- df2016[ , !( colnames( df2016 ) %in% c( 'subsector_l1',
                                                          'subsector_l2' ) ) ]

    # Make sure each df contains all sectors, even those with 0 emissions for
    # that year. 2010 is the outlier which is "by sector" instead of "by source"
        df2003 <- merge( df2003, df2006[ 'sector' ], by = 'sector', all = T )
        df2003 <- merge( df2003, df2010[ 'sector' ], by = 'sector', all = T )
        df2003 <- merge( df2003, df2016[ 'sector' ], by = 'sector', all = T )
        df2006 <- merge( df2006, df2003[ 'sector' ], by = 'sector', all = T )
        df2010 <- merge( df2010, df2003[ 'sector' ], by = 'sector', all = T )
        df2013 <- merge( df2013, df2003[ 'sector' ], by = 'sector', all = T )
        df2016 <- merge( df2016, df2003[ 'sector' ], by = 'sector', all = T )

    # Keep only data for the given emissions species
        sector <- df2003$sector
        X2003 <- df2003[ , em.read ]
        X2006 <- df2006[ , em.read ]
        X2010 <- df2010[ , em.read ]
        X2013 <- df2013[ , em.read ]
        X2016 <- df2016[ , em.read ]

        inv_data_species <- data.frame( sector, X2003, X2006, X2010, X2013, X2016 )

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
    # Find BC and OC emissions

    if (em %in% c ('BC','OC') ) {

    # Define parameters for BC and OC specific script

    ceds_sector <- "1A3b_Road"
    inv_iso <- "twn"
    PM <- "PM25"

    # Read in scaling mapping file and filter transportation sectors
    mapping_file <- readData("SCALE_MAPPINGS", "Taiwan_scaling_mapping.csv")
    mapping_file <- mapping_file %>%
        filter(str_detect(scaling_sector,"on-road transportation"))
    inv_sector_name <- mapping_file$inv_sector

    # Match formatting from PM2.5 inventory to BC/OC script
    X_inv_years <- paste0("X",inv_years)
    inv_data_sheet <- inv_data_species %>% select(-unit)
    inv_data_sheet[is.na(inv_data_sheet)] = 0


    # Calculate BC and OC emissions

        inv_data_sheet <- F.Estimate_BC_OC_emissions(em,PM,inv_iso,ceds_sector,inv_sector_name,X_inv_years)
        inv_data_species <- inv_data_sheet
    }


# ------------------------------------------------------------------------------
# 4. Output
# write standard form inventory
    writeData( inv_data_species , domain = "MED_OUT",
               paste0( 'E.', em, '_', inv_name, '_inventory' ) )

# Every script should finish with this line

    logStop()
# END

