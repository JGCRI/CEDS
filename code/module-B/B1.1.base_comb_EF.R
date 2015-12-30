#------------------------------------------------------------------------------
# Program Name: B1.1.base_comb_EF.R
# Author: Jon Seibert
# Date Last Updated: July 16, 2015
# Program Purpose: To select and run the correct script(s) to generate the base combustion
#                  emissions factors database for the given emissions type.
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
    headers <- c() # Additional function files required.
    log_msg <- paste0( "Calling species-specific child script to generate",
              " default combustion emissions factors" ) # First message to be printed to the log
    script_name <- "B1.1.base_comb_EF.R"
    
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "SO2"
    
# ------------------------------------------------------------------------------------

MODULE_B <- "../code/module-B/"

# Create a function that can be applied to source all child scripts for the given
# emissions type.
source_child <- function( file_name ){ source( paste( MODULE_B, file_name, sep = "" ) ) }

# ------------------------------------------------------------------------------------

scripts <- c('B1.1.base_comb_EF_control_percent.R')

# Set scripts to generate species-specific base emission factors

if( em == "SO2" ){
    scripts <- c( scripts, "B1.1.base_SO2_comb_EF_parameters.R" )
} else if( em == "BC" || em == "OC" ){
    scripts <- c( scripts, "B1.1.base_BC_comb_EF.R" )
} else {
  # Else create a blank default database
  activity_data <- readData( "MED_OUT", "A.comb_activity" )
  default_EM_db <- activity_data
  default_EM_db[,X_emissions_years] <- 0
  default_EM_db$units <- 'kt/kt'
  writeData( default_EM_db, "MED_OUT", paste0( "B.", em ,"_", "comb_EF_db") )
}

# Run all child scripts for the given emissions type. The call to
# invisible() prevents extraneous output from appearing in the console.
# lapply calls source_child on each script in the list.
invisible( lapply( scripts, source_child ) )

logStop()
# END
