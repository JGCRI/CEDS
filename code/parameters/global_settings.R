# ----------------------------------------------------------------------------------
# CEDS R header file: global settings
# Authors: Ben Bond-Lamberty, Jon Seibert, Tyler Pitkanen, Rachel Hoesly, Huong Nguyen
# Last Updated: 24 June 2019
#
# Provides global variables and necessary system settings. This file must be
# sourced by all CEDS R scripts, before any other sourced files.
#
# -----------------------------------------------------------------------------

# Load required libraries. If library isn't installed, outputs warning message
loadPackage <- function( pName, versions = NULL ) {
  minVersion <- if( !is.null(versions) ) versions[[pName]] else 0

  if( suppressMessages( !require( pName, character.only=T ) ) ) {
    cat( "Couldn't load '", pName, "'. Please Install.\n", sep="")
    stop( paste0( "Couldn't load '", pName, "'. Please Install.\n" ) )
  }

  if( packageVersion( pName ) < minVersion ) {
    stop( paste0( "Package '", pName, "' version ", minVersion,
                  " or greater is required.") )
  }
}

libs <- c( dplyr = "0.7.6", ggplot2 = "3.0.0", gridExtra = "2.2.1",
           magrittr = "1.5", plyr = "1.8.4", readxl = "1.0.0",
           reshape = "0.8.6", stringr = "1.1.0", tidyr = "0.8.1",
           openxlsx = "4.0.0", XML = "3.98-1.5", zoo = "1.7-14" )

lapply(names(libs), loadPackage, libs)


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

GCAM_DATA_COMMENT 		<- "#"	# Comment character for files
MODULE_PROC_ROOT		<- ""   # Module processing code root folder should be set in module-specific header file
DOMAINPATHMAP 			<- paste0( MODULE_PROC_ROOT, "../input/mappings/domainmapping.csv" )    # List of domain (groups of files) mappings

# Specify the location of the module from the data system root directory
MODULE_PROC_ROOT		<- PARAM_DIR

# Should gridding be done with subregional data products or not?
GRID_SUBREGIONS         <- FALSE
GRIDDING_VERSION        <- as.character( Sys.Date() )

SUPPORTED_SPECIES       <- c( 'BC', 'CH4', 'CO', 'CO2', 'NH3', 'NMVOC', 'NOx', 'OC', 'SO2', 'NONE' ) # Note that 'NONE' is a supported em,
                                                                                                     # as this allows the user to make activity data.
PROPRIETARY_FILES       <- c( 'OECD_E_Stat.csv', 'NonOECD_E_Stat.csv',
                              'OECD_Conversion_Factors_Full.csv', 'NonOECD_Conversion_Factors_Full.csv',
                              'OECD_Conversion_Factors.csv', 'NonOECD_Conversion_Factors.csv' )


# -----------------------------------------------------------------------------
# Logical Check - Options


#na_error : Check for NA's.
# 1: If NA's exists in in EF database or dataframes, then error and stop script.
# There should be no NA's in these files. NA's  are the result of faulty code
#
na_error <- 1

# If true write value meta data in scaling module (FALSE to save time while test running)
Write_value_metadata <- FALSE

# Verbosity of output logging
VERBOSE <- TRUE


#-----------------------------------------------------------------------------------------
#Generate system-wise version_stamp
#The version_stamp indicates the current CEDS version, and it will be marked on
#diagnostic plots. If user has an specific version_stamp he/she wants to use,
#follow the instruction in the next comment

# User should uncomment the following line if he/she has a specific cedsUserVersionNumber to use
 options(cedsUserVersionNumber = "v_2019_06_07")

getcedsVersionNumber <- function( ) {

  # get the current date
  version_date <- Sys.Date( )

  # retrieve the cedsUserVersionNumber from options()
  ceds_user_version_number <- getOption( "cedsUserVersionNumber" )

  # generate the version_stamp
  if( is.null( ceds_user_version_number ) ) {
    version_stamp <- paste0( "v_", format( version_date, format="%Y_%m_%d" ) )
  } else {
    version_stamp <- ceds_user_version_number
  }

  return ( version_stamp )
}

# create system version stamp
version_stamp <- getcedsVersionNumber( )


# Check that required inputs are available
checkSystemInputs <- function(em) {
    # Make sure a valid emission species was given
    if (!em %in% SUPPORTED_SPECIES) {
        if (grepl('0', em)) {
            em <- paste0(em, ', did you mean ', gsub('0', 'O', em), '?')
        }
        stop("CEDS does not support ", em)
    }

    # Make sure the files that do not come with CEDS exist
    required_files <- file.exists(sapply(PROPRIETARY_FILES, filePath, domain = 'ENERGY_IN'))
    if (!all(required_files)) {
        stop("The following required input files are missing:\n\t",
             paste(PROPRIETARY_FILES[!required_files], collapse = '\n\t'))
    }
}
