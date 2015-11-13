# ----------------------------------------------------------------------------------
# CEDS R header file: global settings
# Authors: Ben Bond-Lamberty, Jon Seibert, Tyler Pitkanen, Rachel Hoesly
# Last Updated: 24 August 2015

# This file must be sourced by all CEDS R scripts, before any other sourced files.
# Provides global variables and necessary system settings.

# -----------------------------------------------------------------------------


# Load required libraries. If library isn't installed, outputs warning message
loadPackage<-function(package){
  if( suppressMessages(!require( package, lib.loc=.libPaths()[ 1 ], character.only=T ) )){
    cat( "Couldn't load '", package, "'. Please Install.\n" ,sep="")
    stop(paste( "Couldn't load '", package, "'. Please Install.\n" ,sep=""))
  }
}
libs <- c( "ggplot2", "magrittr", "pbapply", "plyr", "dplyr", "reshape", "stringr", "XML", "readxl")
for( i in seq_along(libs)){
    package <- libs[[ i ]]
    loadPackage(package)
    }

# # Excel output requires the xlsx package, which is dependent on rJava. rJava
# #   requires an updated version of Java compatible with the version of R used
# # If rJava can't be loaded, stop trying to load it at the start of each script.
# # This block is currently commented out due to lack of use of the functions requiring rJava.

# if( exists( 'RJAVA_PACKAGE_LOADED' ) == FALSE ) {
    # # The first time the package is loaded, try to install 
    # require( "xlsx", lib.loc=libPath )
    # if( !require( "xlsx", lib.loc=libPath ) ) {
        # cat( "Couldn't load xlsx; trying to download it...\n" )
        # try( install.packages( "xlsx", repos="http://cran.rstudio.com/" ) )
    # }
   # # Check if the install worked and mark the flag accordingly
    # check <- library( "rJava" )
    # if( exists( 'check' ) ) { 
        # RJAVA_PACKAGE_LOADED <<- TRUE
    # } else { 
        # RJAVA_PACKAGE_LOADED <<- FALSE
        # cat( "rJava can't be loaded. Excel output set to OFF" )
    # }
# } else if( RJAVA_PACKAGE_LOADED == TRUE ) require( "xlsx", lib.loc=libPath )

# -----------------------------------------------------------------------------
# Global settings (in CAPITALS)
# TODO: check build target. If it's "clean", or something like that, reset everything
# This first group of settings is protected--we don't want it re-set every time
# this header is read.
if( !exists( "GCAM_SOURCE_FN" ) ) {		# i.e. #ifndef
	GCAM_SOURCE_FN 		<- c( "?" ) 	# name of currently executing source file (stack structure)
	GCAM_LOG_SAVE	 	<- c( FALSE )	# whether current log is also being saved to file (stack structure)
	GCAM_SOURCE_RD 		<- 0			# recursion depth, an index into above structures
	DEPENDENCIES 		<- list()		# dependencies (i.e. what files scripts read)
    OUTPUTS             <- list()       # outputs (i.e. what files scripts write)
}

MODULE_PROC_ROOT		<- ""   #Module processing code root folder should be set in module-specific header file
GCAM_DATA_COMMENT 		<- "#"							# Comment character for files
XML_TEMPLATE_FILENAME 	<- "batch_xml_template.xml"		                               # XML template file name
GCAM_HEADERS_MI 		<- "ModelInterface_headers.txt"				               # csv to xml header file name
PATH_FROM_MI 			<- ""		                                       # Path from Model Interface
DOMAINPATHMAP 			<- paste( MODULE_PROC_ROOT, "../input/mappings/domainmapping.csv", sep="" )    # List of domain (groups of files) mappings

# Specify the location of the module from the data system root directory
MODULE_PROC_ROOT		<- PARAM_DIR


# -----------------------------------------------------------------------------
# Logical Check - Options


#na_error : Check for NA's. 
# 1: If NA's exists in in EF database or dataframes, then error and stop script. 
# There should be no NA's in these files. NA's  are the result of faulty code
#
na_error <- 1
