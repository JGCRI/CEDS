#------------------------------------------------------------------------------
# Program Name: B1.1.base_SO2_comb_EF_parameters.R.R
# Authors: Leyang Feng, Jon Seibert, Tyler Pitkanen
# Date Last Updated: Nov 4, 2015
# Program Purpose: Use the default sulfur content, ash retention, control percentage for a 
# specified combustion emission species to create default sulfur content, ash retention, control 
# percentage databases accurately corresponding to the fuels and sectors in A.comb_activity.csv
# Input Files: A.comb_activity.csv, [em]_base_EF.csv, Master_Fuel_Sector_List.xlsx    
# Output Files: B.[em]_S_Content_db.csv, B.[em]_S_AshRet_db.csv, B.[em]_S_ControlFrac_db.csv
# Notes: 
# TODO: For ash retention, use join instead of for loop
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Before we can load headers we need some paths defined. They may be provided by
#   a system environment variable or may have already been set in the workspace.
dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
for ( i in 1:length( dirs ) ) {
  setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
  wd <- grep( 'CEDS/input', list.dirs(), value = T )
  if ( length( wd ) > 0 ) {
    setwd( wd[ 1 ] )
    break
  }
}
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R" ) # Additional function files required.
log_msg <- "Extrapolating default emissions factors to full dataset" # First message to be printed to the log
script_name <- "B1.1.base_SO2_comb_EF_parameters.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )
# ------------------------------------------------------------------------------
# 1. Read in files
# for running directly from R - defines emission species (normally from command line)
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"

# "em" is defined from parent script
em_lc <- tolower( em )

activity_data <- readData( "MED_OUT", "A.comb_activity" )
fuel_list <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Fuels" )
sector_list <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Sectors" )
fuel_S_Content <- readData( "DEFAULT_EF_IN", paste0 ( em, "_base_EF" ) ) 
fuel_AshRet <- readData("MAPPINGS","fuel_sector_ash_retention_mapping")
# ------------------------------------------------------------------------------
# 2. Create default sulfur content, ash retention, control percentage databases

# List out all fuels and sectors
all_fuels <- fuel_list[[ "fuel" ]]
all_sectors <- sector_list[['sector']]

# Remove "process" from the list, if present, to avoid errors,
# as it is not in the fuel_efs list.
all_fuels <- all_fuels[ all_fuels != "process"]

# Create the default sulfur content database (use to fill in iso/fuel/sector/year
# combinations at end).

# Set up the layout from activity_data
default_S_Content <- activity_data

# Fill in the default values
for ( i in seq_along( all_fuels ) ) {
  default_S_Content[ default_S_Content$fuel == all_fuels[[ i ]], X_emissions_years ] <-
    fuel_S_Content[ fuel_S_Content$fuel == all_fuels[[ i ]], 
              paste0( em_lc, "_percent" ) ]
}

#No unit for sulfur content persentage
default_S_Content$units <- NA

# Create default ash retention database
default_AshRet <- activity_data

for ( fuel in all_fuels  ) {
  for (sector in all_sectors) {
    default_AshRet[ default_AshRet$fuel == fuel & default_AshRet$sector == sector, X_emissions_years ] <-
      fuel_AshRet[ fuel_AshRet$fuel == fuel & fuel_AshRet$sector == sector, 
                    "AshRet"]
  }
}

#unit for sulfur content percentage
default_AshRet$units <- 'NA'

# Create default control fraction database
default_ControlFrac <- activity_data
# The default for control percentage is 0
default_ControlFrac[,5:ncol(default_ControlFrac)] <- 0

#No unit for sulfur content percentage
default_ControlFrac$units <- 'NA'

# ------------------------------------------------------------------------------
# 4. Output    

# Write out all three default databases
writeData( default_S_Content, "MED_OUT", paste0( "B.", em ,"_", "S_Content_db") )
writeData( default_AshRet, "MED_OUT", paste0( "B.", em ,"_", "AshRet_db") )
writeData( default_ControlFrac, "MED_OUT", paste0( "B.", em ,"_", "ControlFrac_db") )
# NOTE: Users wishing to add more sulfur content data should create a new script
# entitled B.add_?_S_Content.R in which to do so.

# Every script should finish with this line
logStop()
# END