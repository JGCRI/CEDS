# ------------------------------------------------------------------------------
# Program Name: E.Australia_emission.R
# Author(s): Leyang Feng
# Date Last Updated: March 28, 2016
# Program Purpose: To read in and reformat Australia NEI data.
# Input Files: [em]_Australia_UNFCCC_and_NEI.xlsx
# Output Files: E.[EM]_Australia_inventory.csv
# Notes: Only process Australia NEI data for year 2000, 2006, 2012
# TODO: 
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Set working directory to the CEDS "input" directory and define PARAM_DIR as the
# location of the CEDS "parameters" directory, relative to the new working directory.
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
headers <- c( 'common_data.R',"data_functions.R" , "analysis_functions.R") # Additional function files required.
log_msg <- "Generating Australia emission inventory data" # First message to be printed to the log
script_name <- paste0(em,"-E.Australia_emission.R")

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define parameters for inventory specific script

# Stop script if running for unsupported species
if ( em %!in% c( 'SO2', 'NOx', 'NMVOC', 'CO' ) ) {
  stop (paste( ' Australia scaling is not supported for emission species', em, 'remove from script
               list in F1.1.inventory_scaling.R') )
}

# For each Module E script, define the following parameters:
# Inventory parameters. Provide the inventory and mapping file names, the
#   mapping method (by sector, fuel, or both), and the regions covered by
#   the inventory (as a vector of iso codes)
inventory_data_file <- paste0( 'Australia/', em, '_Australia_UNFCCC_and_NEI' )
inv_data_folder <- "EM_INV"
sector_fuel_mapping <- 'Australia_scaling_mapping'
mapping_method <- 'sector'
inv_name <- 'AUS' #for naming diagnostic files
region <- c( "aus" ) 
inv_years<-c( 2000, 2006, 2012 )



# ------------------------------------------------------------------------------
# 2 Inventory in Standard Form (iso-sector-fuel-years, iso-sector-years, etc)

# Import Sheets containing 2008,2010,2012 data.
sheet_name <- "1999-2000"
inv_data_2000 <- readData( inv_data_folder, inventory_data_file , ".xlsx", sheet_selection = sheet_name )
sheet_name <- "2005-2006"
inv_data_2006 <- readData( inv_data_folder, inventory_data_file , ".xlsx", sheet_selection = sheet_name )
sheet_name <- "2011-2012"
inv_data_2012 <- readData( inv_data_folder, inventory_data_file , ".xlsx", sheet_selection = sheet_name )

# kepp desired columns
keep_columns <- c( "NPI Sector", "NPI - Total (kt)" )
drop_rows <- c( 'TOTAL', 'Total', 'total' )

df2000 <- subset( inv_data_2000, select = keep_columns )
col_names <- c( 'sector', 'X2000')
colnames( df2000 ) <- col_names
df2000 <- df2000[ ! df2000$sector %in% drop_rows , ]

df2006 <- subset( inv_data_2006, select = keep_columns )
col_names <- c( 'sector', 'X2006')
colnames( df2006 ) <- col_names
df2006 <- df2006[ ! df2006$sector %in% drop_rows , ]

df2012 <- subset( inv_data_2012, select = keep_columns )
col_names <- c( 'sector', 'X2012')
colnames( df2012 ) <- col_names
df2012 <- df2012[ ! df2012$sector %in% drop_rows , ]


# construct unified sectors in each df
df2000 <- merge( df2000, df2006['sector'], by = 'sector', all = T )
df2000 <- merge( df2000, df2012['sector'], by = 'sector', all = T )
df2006 <- merge( df2006, df2000['sector'], by = 'sector', all = T )
df2012 <- merge( df2012, df2000['sector'], by = 'sector', all = T )

# Putting data for specific emission into a single dataframe.
sector <- as.character( df2000$sector )
X2000 <- df2000[ , 2 ]
X2006 <- df2006[ , 2 ]
X2012 <- df2012[ , 2 ]

inv_data <- data.frame(sector, X2000, X2006, X2012, stringsAsFactors = F ) 

# Clean rows and columns to standard format
inv_data$iso <- 'aus'
inv_data$unit <- 'kt'
inv_data <- inv_data[ , c( 'iso', 'sector', 'unit', paste0( 'X', inv_years ) ) ]
inv_data[ is.na( inv_data ) ] <- 0

# write out a dummy file unsupported species
if ( em %!in% c( 'SO2', 'NOx', 'NMVOC', 'CO' ) ) {
  inv_data <- data.frame( )
}

# ------------------------------------------------------------------------------
# 5. Output
# write standard form inventory
writeData( inv_data , domain = "MED_OUT", paste0('E.',em,'_',inv_name,'_inventory') )
# Every script should finish with this line
logStop()
# END





