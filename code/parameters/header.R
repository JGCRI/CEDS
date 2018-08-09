# ----------------------------------------------------------------------------------
# CEDS R header file: Script initialization
# Author(s): Jon Seibert
# Last Updated: 7 August 2015

# This file must be sourced by all CEDS R scripts to perform log initialization,
#   read in other required functions, and note initial dependencies.
# Functions contained:
#   sourceFunctions, addDep, initialize

# Notes: Requires functions in IO_functions.R (automatically loaded)

# -----------------------------------------------------------------------------

# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- "../code/parameters/"

# Set working directory to the CEDS "input" directory
if( "input" %in% dir() ){
  setwd( "input/" )
} else if ( "input" != tail( strsplit( getwd(), '/' )[[ 1 ]], n = 1 ) ){
  stop("Cannot find 'input' directory")
}

sourceFunctions <- function( file_name ) {
    file_path <- paste0( PARAM_DIR, file_name)
    if ( file.exists( file_path ) ) {
        source( file_path )
    } else {
        tryCatch({
            file_path <- filePath( "DIAG_IN", file_name, "" )
            source( file_path )
        }, error = function(e) {
            stop( "Header file ", file_name, " not found" )
        })
    }
}

addDep <- function( file_name ) addDependency( paste0( PARAM_DIR, file_name ) )

initialize <- function( script_name, log_msg, headers, common_data = TRUE, clear_metadata = TRUE){

    # Include common_data.R by default
    if( common_data && ( !"common_data.R" %in% headers ) ) {
        headers <- c( headers, "common_data.R" )
    }

    # Ensure the critical headers are read in first, in the correct order
    if( !"IO_functions.R" %in% headers ) {
        headers <- c( "IO_functions.R", headers )
    }
    if( !"global_settings.R" %in% headers ) {
        headers <- c( "global_settings.R", headers )
    }

    invisible( lapply( headers, sourceFunctions ) )
    logStart( script_name )
    if ( clear_metadata ) clearMeta()
    invisible( lapply( headers, addDep ) )
    printLog( log_msg )
}
