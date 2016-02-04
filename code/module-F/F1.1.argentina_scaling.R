#------------------------------------------------------------------------------
# Program Name: F1.1.argentina_scaling.R
# Authors' Names: Tyler Pitkanen, Jon Seibert, Rachel Hoesly, Steve Smith, Ryan Bolt
# Date Last Modified: January 20, 2016
# Program Purpose: To create scaling factors and update emissions estimate for
# the Argentinian region from latest emissions working copy by using aggregate 
# inventory data.
# This data only contains data from 1990 - 2011, missing data from 2000 and 2010.
# Units are initially in metric tonnes
# Input Files: emissions_scaling_functions.R, F.[em]_scaled_EF.csv, 
#              F.[em]_scaled_emissions.csv, Argentina_scaling_mapping.csv, 
#              national_tier1_caps.xlsx
# Output Files: F.[em]_total_scaled_EF.csv, F.[em]_total_scaled_emissions.csv
# Notes: 
# TODO: Re-write read-in so that order of years is taken from input data instead of assumed.
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
headers <- c( 'common_data.R',"data_functions.R" ,"emissions_scaling_functions.R",  "analysis_functions.R" ) # Additional function files required.
log_msg <- "test inventory data" # First message to be printed to the log
script_name <- paste0(em,"-F1.1.argentina_scaling.R")

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )


# ------------------------------------------------------------------------------
# 1. Define parameters for inventory specific script

# Stop script if running for unsupported species
if ( em %!in% c('SO2', 'NOx', 'CO', 'NMVOC' ) ) {
  stop (paste( ' ARG scaling is not supported for emission species', em, 'remove from script
               list in F1.1.inventory_scaling.R'))
}

em_temp = em  # Variable to use for interacting with emissions file
if (em_temp == "NMVOC"){
  em_temp <- "COVNM"
}

# For each Module E script, define the following parameters:
# Inventory parameters. Provide the inventory and mapping file names, the
#   mapping method (by sector, fuel, or both), and the regions covered by
#   the inventory (as a vector of iso codes)

inv_data_folder <- "EM_INV"
sector_fuel_mapping <- 'Argentina_scaling_mapping'
mapping_method <- 'sector'
inv_name <- 'ARG' #for naming diagnostic files
region <- c( "arg" ) 
inv_years<-c(1990:1999, 2001:2009, 2011)

# -------------------------------------------
# Reading in translation file and the first sheet of Argentina Inventory

translation_file <- 'Argentina/Argentina_Translation'
sheet_name <- "Sheet1"
translation <- readData( inv_data_folder, translation_file , ".xlsx", 
                            sheet_selection = sheet_name )

attempt_file <- 'Argentina/Argentina Inventario 1990-2012-ipcc1996'
sheet_name2 <- "Inventario 1990"
attempt <- readData( inv_data_folder, attempt_file , ".xlsx", 
                     sheet_selection = sheet_name2,  skip_rows = 2)

# Renaming sector column, removing NA sectors and translating sector names from Spanish to English.
colnames(attempt)[2] <- "sector"
attempt <- attempt[complete.cases(attempt$sector),]
attempt[,2] <- translation$English[ match( attempt[,2], translation$Spanish ) ]

# We need the sectors (column 2) and the emission (column name = emission). 
inv_data_sheet <- attempt[,c('sector', em_temp)]
inv_data_sheet[,2] <- as.numeric(inv_data_sheet[,2])

# Renaming specie column to year. Making NA's into 0's. 
colnames(inv_data_sheet)[colnames(inv_data_sheet) == em_temp] <- "X1990"
inv_data_sheet[is.na(inv_data_sheet)] <- 0

# ----------------- Adding years to dataframe in a loop ------------------

for(year in inv_years){
# Reading data sheet in. 
  attempt <- 0
  old_names <- colnames(inv_data_sheet)
  attempt_file <- 'Argentina/Argentina Inventario 1990-2012-ipcc1996'
  sheet_name2 <- paste("Inventario", as.character(year))
  attempt <- readData( inv_data_folder, attempt_file , ".xlsx", 
                       sheet_selection = sheet_name2, skip_rows = 2 )
  
  # Translating sectors from Spanish to English. Removing NA rows from sector list.
  colnames(attempt)[2] <- "sector"
  attempt <- attempt[complete.cases(attempt$sector),]
  attempt[,2] <- translation$English[ match( attempt[,2], translation$Spanish ) ]
  

  attempt <- attempt[,c('sector', em_temp)]
  attempt[,2] <- as.numeric(attempt[,2])
  attempt[is.na(attempt)] <- 0
  
  inv_data_sheet <- cbind(inv_data_sheet, attempt[,em_temp])
  colnames(inv_data_sheet) <- c(old_names, paste0("X",year))
}

# Make numeric and convert from tonnes to kt
inv_data_sheet[,paste0('X',inv_years)] <- 
  as.matrix(inv_data_sheet[,paste0('X',inv_years)])/1000

# Adding ISO column
inv_data_sheet$iso <- 'arg'
inv_data_sheet <- inv_data_sheet[,c('iso','sector', paste0('X',inv_years))]

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




