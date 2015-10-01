#------------------------------------------------------------------------------
# Program Name: F1.1.inventory_scaling.R
# Author: Rachel Hoesly adapted from Jon Seibert
# Date Last Updated: Sept 24, 2015
# Program Purpose: To select and run the correct script(s) to fill out to the combustion
#                   emissions database for the given emissions type.
# Input Files: None
# Output Files: None
# Notes: 
# TODO: Add conditionals and script specifications for other emissions types
#       as they are added to the system.
# ------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------
# Before we can run other scripts we need some paths defined. They may be provided by
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
headers <- c( "data_functions.R" ,"emissions_scaling_functions.R" ) # Additional function files required.
log_msg <- paste0( "Calling species-specific child script to scale emissions to",
                   " inventories" ) # First message to be printed to the log
script_name <- "F1.1.inventory_scaling.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------------

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"
em_lc <- tolower( em )    

MODULE_F <- "../code/module-F/"

# Create a base dataframe of emissions for input into scaling modules - Just the default df
# Load then write the data
EF <- MCL <- readData( "MED_OUT", paste0( "D.", em, "_default_total_EF" ) )
emissions <- MCL <- readData( "MED_OUT", paste0( "D.", em, "_default_total_emissions" ) )

writeData(EF, domain = "MED_OUT", fn = paste0( "F.", em, "_scaled_EF" ), meta = TRUE )
writeData(emissions, domain = "MED_OUT", fn = paste0( "F.", em, "_scaled_emissions" ), meta = TRUE )


# Create a function that can be applied to source all child scripts for the given
# emissions type.
source_child <- function( file_name ){ source( paste( MODULE_F, file_name, sep = "" ) ) }

scripts <- c()

# Set scripts to add emissions data for SO2
if( em == "SO2" ){
  scripts <- c( "F1.1.US_scaling.R", 'F1.1.CAN_scaling.R' , 'F1.1.UNFCCC_scaling.R' )
}

# Set scripts to add emissions data for BC or OC
if( em == "BC" || em == "OC" ){
  scripts <- c(  )
}

# Run all child scripts for the given emissions type. The call to
# invisible() prevents extraneous output from appearing in the console.
# lapply calls source_child on each script in the list.
invisible( lapply( scripts, source_child ) )

logStop()
# END
