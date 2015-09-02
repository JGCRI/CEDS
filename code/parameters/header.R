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

# PARAM_DIR defined by each script
sourceFunctions <- function( file_name ){ source( paste0( PARAM_DIR, file_name) ) }
addDep <- function( file_name ){ addDependency( paste0 ( PARAM_DIR, file_name ) ) }

initialize <- function( script_name, log_msg, headers, common_data = TRUE ){
    
    # Include common_data.R by default
    if( common_data && ( ! "common_data.R" %in% headers ) ){ headers <- c( headers, "common_data.R" ) }

    # Ensure the critical headers are read in first, in the correct order
    if( ! "IO_functions.R" %in% headers ){ headers <- c( "IO_functions.R", headers ) }
    if( ! "global_settings.R" %in% headers ){ headers <- c( "global_settings.R", headers ) }

    invisible( lapply( headers, sourceFunctions ) )
	logStart( script_name )
    clearMeta()
    invisible( lapply( headers, addDep ) )
    printLog( log_msg )

}