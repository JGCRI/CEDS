#------------------------------------------------------------------------------
# Program Name: F1.1.CAN_scaling.R
# Authors' Names: Tyler Pitkanen, Jon Seibert, Rachel Hoesly
# Date Last Modified: Oct 29, 2015
# Program Purpose: To create scaling factors and update emissions estimate for
# the CAN region from latest emissions working copy by using aggregate 
# CAN trends inventory data. This file uses the older Canadian inventory
# data used until 2011. This data extends back to 1985.
#
# Input Files: emissions_scaling_functions.R, F.[em]_scaled_EF.csv, 
#              F.[em]_scaled_emissions.csv, CAN_sector_mapping.csv, 
#              national_tier1_caps.xlsx
# Output Files: F.[em]_total_scaled_EF.csv, F.[em]_total_scaled_emissions.csv
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

# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
headers <- c( 'common_data.R',"data_functions.R" ,"emissions_scaling_functions.R" ) # Additional function files required.
log_msg <- "test inventory data" # First message to be printed to the log
script_name <- "F1.1.CAN_scaling.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define parameters for inventory specific script

  args_from_makefile <- commandArgs( TRUE )
  em <- args_from_makefile[1]
  if ( is.na( em ) ) em <- "SO2"
  
  # Stop script if running for unsupported species
  if ( em %!in% c('SO2','NO2','VOC','CO','NH3') ) {
    stop (paste( ' CAN scaling is not supported for emission species', em, 'remove from script
                 list in F1.1.inventory_scaling.R'))
  }
  
    

# For each Module E script, define the following parameters:
# Inventory parameters. Provide the inventory and mapping file names, the
#   mapping method (by sector, fuel, or both), and the regions covered by
#   the inventory (as a vector of iso codes)
  inventory_data_file <- 'Canada/1985-2011_CAC_Trends_Feb2013_ENG'
  inv_data_folder <- "EM_INV"
  sector_fuel_mapping <- 'CAN_scaling_mapping'
  mapping_method <- 'sector'
  inv_name <- 'CAN_to2011' #for naming diagnostic files
  region <- c( "can" ) 
  inv_years<-c(1985:2011)

# ------------------------------------------------------------------------------
# 1.5 Inventory in Standard Form (iso-sector-fuel-years, iso-sector-years, etc)
  
  # Import Sheet
  sheet_name <- em
  if( sheet_name == 'SO2') sheet_name <- 'SOx'
  if( sheet_name == 'NO2') sheet_name <- 'NOx'
  inv_data_sheet <- readData( inv_data_folder, inventory_data_file , ".xlsx", 
                              sheet_selection = sheet_name ) 
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
  
  # write standard form inventory
  writeData( inv_data_sheet , domain = "MED_OUT", paste0('E.',em,'_',inv_name,'_inventory'))
  inventory_data_file <- paste0('E.',em,'_',inv_name,'_inventory')
  inv_data_folder <- 'MED_OUT'
  
  # ------------------------------------------------------------------------------
  # 2. Read In Data with scaling functions
  
  # Read in the inventory data, mapping file, the specified emissions species, and
  # the latest versions of the scaled EFs  
  
  scaling_data <- F.readScalingData( inventory = inventory_data_file, inv_data_folder,
                                     mapping = sector_fuel_mapping, method = mapping_method,
                                     region, inv_name, inv_years)
  list2env(scaling_data , envir = .GlobalEnv) 
  
  
  # ------------------------------------------------------------------------------
  # 3. Arrange the CEDS emissions data to match the inventory data
  
  # Aggregate inventory data to scaling sectors/fuels 
  inv_data <- F.invAggregate( std_form_inv , region )
  
  # Aggregate ceds data to scaling sectors/fuels
  ceds_data <- F.cedsAggregate( input_em, region, mapping_method )
  
  # ------------------------------------------------------------------------------
  # 4. Calculate Scaling Factors, reaggregate to CEDS sectors  
  
  # Calculate and extend scaling factors
  scaling_factors_list <- F.scaling( ceds_data, inv_data, region, 
                                     replacement_method = 'replace', 
                                     max_scaling_factor = 100,
                                     replacement_scaling_factor = 100)
  list2env(scaling_factors_list , envir = .GlobalEnv)
  
  # Apply Scaling Factors to Ceds data
  scaled <- F.applyScale(scaling_factors)
  scaled_ef <- scaled[[1]]
  scaled_em <- scaled[[2]]
  
  # ------------------------------------------------------------------------------
  # 5. Encorporate scaled em and EF and
  # Write Scaled emissions and emission factors
  
  F.addScaledToDb( scaled_ef, scaled_em, meta_notes )

  
  # Every script should finish with this line
  logStop()
  # END
  