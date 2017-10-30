#------------------------------------------------------------------------------
# Program Name: E.China_emissions.R
# Authors' Names: Tyler Pitkanen, Jon Seibert, Rachel Hoesly, Steve Smith, Ryan Bolt
# Date Last Modified: January 12, 2016
# Program Purpose: To read in and reformat China emissions inventory data
# This data only contains data from 2008,2010,2012.
# Units are initially in Mg or Metric Tonnes
# Input Files: CEDS_MEIC_Emissions_2rdLevel_20160226.xlsx
# Output Files: E.[em]_CHN_inventory.csv
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
    if ( is.na( em ) ) em <- "NOx"
  
# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'common_data.R', "data_functions.R", 
                  "emissions_scaling_functions.R", "analysis_functions.R" ) # Additional function files required.
    log_msg <- "Initial reformatting of China emissions" # First message to be printed to the log
    script_name <- "E.China_emissions.R"
    
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define parameters for inventory-specific script
    inventory_data_file <- 'CEDS_MEIC_Emissions_2rdLevel_20160226_plus'
    inv_data_folder <- "EM_INV"
    subfolder_name <- 'China/'
    inv_name <- 'CHN' #for naming diagnostic files
    inv_years<-c( 2008, 2010, 2012 )


# ------------------------------------------------------------------------------
# 2. Inventory in Standard Form (iso-sector-fuel-years, iso-sector-years, etc)

# Import Sheets containing 2008,2010,2012 data.
    sheet_name <- "2008"
    inv_data_sheet_eight <- readData( inv_data_folder, inventory_data_file,
                                      ".xlsx", sheet_selection = sheet_name,
                                      domain_extension = subfolder_name )
    sheet_name <- "2010"
    inv_data_sheet_ten <- readData( inv_data_folder, inventory_data_file, 
                                    ".xlsx", sheet_selection = sheet_name, 
                                    domain_extension = subfolder_name )
    sheet_name <- "2012"
    inv_data_sheet_twelve <- readData( inv_data_folder, inventory_data_file, 
                                       ".xlsx", sheet_selection = sheet_name, 
                                       domain_extension = subfolder_name )
  
# Process given emission if inventory data exists
# Here assuming data for specific emission species either do not exist or exist
# for all 3 years: 2008, 2010, 2012
    if ( em %in% names( inv_data_sheet_eight ) ) {
    # Putting data for specific emission into a single dataframe.
        sector <- inv_data_sheet_eight[ , 'CEDS-Working-Sector-Name, Unit: Mg' ]
        X2008 <- inv_data_sheet_eight[ , em ]
        X2010 <- inv_data_sheet_ten[ , em ]
        X2012 <- inv_data_sheet_twelve[ , em ]
        
        inv_data_species <- data.frame( sector, X2008, X2010, X2012 )
      
    # Clean rows and columns to standard format
        inv_data_species$iso <- 'chn'
        inv_data_species <- inv_data_species[ , c( 'iso', 'sector', 
                                                   paste0( 'X', inv_years ) ) ]
      
      
    # Make numeric
        inv_data_species[ , paste0( 'X', inv_years ) ] <- 
               sapply( inv_data_species[ , paste0( 'X', inv_years ) ],
                       as.numeric )
    # Convert from tonnes to kt
        inv_data_species[ , paste0( 'X', inv_years ) ] <- 
          as.matrix( inv_data_species[ , paste0( 'X', inv_years ) ] ) / 1000
    
    } else {
    # Write out blank df if no inventory data exists for given emission
        inv_data_species <- data.frame()
    }


# ------------------------------------------------------------------------------
# 3. Write out standard form inventory
    writeData( inv_data_species, domain = "MED_OUT", 
               paste0( 'E.', em, '_', inv_name, '_inventory' ) )
    
    logStop()
# END
