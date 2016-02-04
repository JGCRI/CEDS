#------------------------------------------------------------------------------
# Program Name: F1.1.US_scaling.R
# Authors' Names: Tyler Pitkanen, Jon Seibert, Rachel Hoesly
# Date Last Modified: Oct 29, 2015
# Program Purpose: To create scaling factors and update emissions estimate for
# the USA region from latest emissions working copy by using aggregate 
# USA trends inventory data.
# Input Files: emissions_scaling_functions.R, F.[em]_scaled_EF.csv, 
#              F.[em]_scaled_emissions.csv, USA_sector_mapping.csv, 
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

# Get emission species first so can name log appropriately
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[1]
if ( is.na( em ) ) em <- "NOx"
  
# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
headers <- c( 'common_data.R',"data_functions.R" ,"emissions_scaling_functions.R", "analysis_functions.R",
              "interpolation_extention_functions.R" ) # Additional function files required.
log_msg <- "test inventory data" # First message to be printed to the log
script_name <- paste0(em,"-F1.1.US_scaling.R")

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define parameters for inventory specific script

# Stop script if running for unsupported species
if ( em %!in% c('SO2','NOx','NMVOC','CO','NH3','PM10','PM2.5') ) {
  stop (paste( ' US scaling is not supported for emission species', em, 'remove from script
                 list in F1.1.inventory_scaling.R'))
}

# For each Module E script, define the following parameters:
# Inventory parameters. Provide the inventory and mapping file names, the
#   mapping method (by sector, fuel, or both), and the regions covered by
#   the inventory (as a vector of iso codes)
inventory_data_file <- 'USA/national_tier1_caps'
inv_data_folder <- "EM_INV"
sector_fuel_mapping <- 'US_scaling_mapping'
mapping_method <- 'sector'
inv_name <- 'US' #for naming diagnostic files
region <- c( "usa" ) 
inv_years<-c(1970,1975,1980,1985,1990:2014)

# ------------------------------------------------------------------------------
# 1.5 Inventory in Standard Form (iso-sector-fuel-years, iso-sector-years, etc)

# Import Sheet
sheet_name <- em
if (em == 'NOx') sheet_name <- 'NOX'
if (em == 'NMVOC') sheet_name <- 'VOC'
if (em == 'PM25') sheet_name <- 'PM25Primary'
if (em == 'PM10') sheet_name <- 'PM10Primary'

inv_data_sheet <- readData( inv_data_folder, inventory_data_file , ".xlsx", 
                            sheet_selection = sheet_name ) 
# Clean rows and columns to standard format
if ( em == 'NH3' ) {
  inv_years<-c( 1990:2014 )
  inv_data_sheet <- inv_data_sheet[-1:-3, 1:26]
} else if ( em == 'NMVOC' ) {
  inv_data_sheet <- inv_data_sheet[-1:-3, 1:30]
} else {
  inv_data_sheet <- inv_data_sheet[-1:-4, 1:30]
}

names(inv_data_sheet) <- c('sector', paste0('X',inv_years))

X_inv_years <- paste0('X',inv_years)

inv_data_sheet$iso <- 'usa'
inv_data_sheet <- inv_data_sheet[,c('iso','sector', paste0('X',inv_years))]

# Remove rows with all NAs  
remove.na <- which(apply(inv_data_sheet[,paste0('X',inv_years)], 1, function(x) all(is.na(x))))
inv_data_sheet <- inv_data_sheet[-remove.na,]

# Make numeric, convert "NA" to NA
inv_data_sheet[,paste0('X',inv_years)] <- suppressWarnings(
                      sapply( inv_data_sheet[,paste0('X',inv_years)] , as.numeric) )

# Remove wildfire emissions
wildfire_emissions = inv_data_sheet[ which( inv_data_sheet$sector == 'Wildfires' ), X_inv_years ]
wildfire_emissions[ is.na( wildfire_emissions ) ] <- 0
inv_data_sheet[ which( inv_data_sheet$sector == 'MISCELLANEOUS' ), X_inv_years ] <-
  inv_data_sheet[ which( inv_data_sheet$sector == 'MISCELLANEOUS' ), X_inv_years ] -
  wildfire_emissions

# Convert to metric tonnes
inv_data_sheet[ , paste0( 'X' , inv_years ) ] <- 
  as.matrix( inv_data_sheet[ , paste0( 'X' , inv_years ) ] ) * 0.9072

# Seems this introduces too many NAs for NH3
if ( em != 'NH3' ) {
	#remove values that are the are constant carried forward
	
	check_years <- length(X_inv_years):2
	check_against <- (length(X_inv_years)-1):1
	for (i in seq_along( check_years )) {

	  for (n in seq_along (inv_data_sheet[,1])){
		  if(  any(inv_data_sheet[n,X_inv_years[check_years[i]]] == inv_data_sheet[n,X_inv_years[check_against[i]]] , na.rm=TRUE )) 
			inv_data_sheet[n,X_inv_years[check_years[i]]] <- NA
	  }
	}
}

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
