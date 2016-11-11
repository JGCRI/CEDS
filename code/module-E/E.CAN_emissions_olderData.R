#------------------------------------------------------------------------------
# Program Name: E.CAN_emissions_olderData.R
# Authors' Names: Tyler Pitkanen, Jon Seibert, Rachel Hoesly
# Date Last Modified: Oct 29, 2015
# Program Purpose: To read in & reformat Canada emissions inventory data. 
# This file uses the older Canadian inventory
# data used until 2011. This data extends back to 1985.
#
# Input Files: 1985-2011_CAC_Trends_Feb2013_ENG.xlsx
# Output Files: E.[em]_CAN_to2011_inventory.csv
# Notes: 
# TODO:
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Set working directory to the CEDS “input” directory and define PARAM_DIR as the
# location of the CEDS “parameters” directory relative to the new working directory.
dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
for ( i in 1:length( dirs ) ) {
  setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
  wd <- grep( 'CEDS/input', list.dirs(), value = T )
  if ( length(wd) > 0 ) {
    setwd( wd[1] )
    break
  }
} 
PARAM_DIR <- "../code/parameters/"

# Get emission species first so can name log appropriately
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[1]
if ( is.na( em ) ) em <- "NOx"
  
# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
headers <- c( 'common_data.R',"data_functions.R" ,"emissions_scaling_functions.R", "analysis_functions.R",
              "interpolation_extention_functions.R" ) # Additional function files required.
log_msg <- "Initial reformatting of Canada emissions (older data)" # First message to be printed to the log
script_name <- "E.CAN_emissions_olderData.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define parameters for inventory specific script
  inventory_data_file <- '1985-2011_CAC_Trends_Feb2013_ENG'
  inv_data_folder <- "EM_INV"
  subfolder_name <- 'Canada/'
  inv_name <- 'CAN_to2011' #for naming diagnostic files
  inv_years<-c(1985:2011)

# ------------------------------------------------------------------------------
# 1.5 Inventory in Standard Form (iso-sector-fuel-years, iso-sector-years, etc)

# Import Sheet
  sheet_name <- em
  if( sheet_name == 'SO2') sheet_name <- 'SOx'
  if( sheet_name == 'NMVOC') sheet_name <- 'VOC'
  
  inv_data_sheet <- readData( inv_data_folder, domain_extension = "Canada/", 
                              domain_extension = subfolder_name, inventory_data_file , ".xlsx" ) 
  
# Process given emission if inventory data exists
if ( sheet_name %in% names( inv_data_sheet ) ){
  inv_data_sheet <- data.frame( inv_data_sheet[ sheet_name ] )   
  
  # Clean rows and columns to standard format
  inv_data_sheet <- inv_data_sheet[-1:-3,]
  names(inv_data_sheet) <- c('sector', paste0('X',inv_years))
  inv_data_sheet$iso <- 'can'
  inv_data_sheet <- inv_data_sheet[,c('iso','sector', paste0('X',inv_years))]
  
  # Remove rows with all NAs  
  remove.na <- which(apply(inv_data_sheet[,paste0('X',inv_years)], 1, function(x) all(is.na(x))))
  inv_data_sheet <- inv_data_sheet[-remove.na,]
  
  # Make numeric and convert from tonnes to kt
  inv_data_sheet[,paste0('X',inv_years)] <- sapply(inv_data_sheet[,paste0('X',inv_years)],as.numeric)
  inv_data_sheet[,paste0('X',inv_years)] <- 
    as.matrix(inv_data_sheet[,paste0('X',inv_years)])/1000
  
# Write out blank df if no inventory data exists for given emissions  
} else {
  inv_data_sheet <- data.frame()
}


# ------------------------------------------------------------------------------
# 2. Write standard form inventory
  writeData( inv_data_sheet , domain = "MED_OUT", paste0('E.',em,'_',inv_name,'_inventory'))
  
  # Every script should finish with this line
  logStop()
  # END
  