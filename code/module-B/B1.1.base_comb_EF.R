#------------------------------------------------------------------------------
# Program Name: B1.1.base_comb_EF.R
# Author: Jon Seibert, Rachel Hoesly
# Date Last Updated: Jan 7, 2016
# Program Purpose: To select and run the correct script(s) to generate the base combustion
#                  emissions factors database for the given emissions type.
# Input Files: None
# Output Files: None
# Notes:
# TODO: Add conditionals and script specifications for other emissions types
#       as they are added to the system.
# ------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

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
    if ( is.na( em ) ) em <- "OC"

# ------------------------------------------------------------------------------------
# 1. Initialize parameters and functions

    MODULE_B <- "../code/module-B/"

# Create a function that can be applied to source all child scripts for the given
# emissions type.
    source_child <- function( file_name ){ source( paste( MODULE_B, file_name, sep = "" ) ) }

# ------------------------------------------------------------------------------------
# Select which scripts will be sourced based on the emissions species

    scripts <- c()

# For all GAINS emissions species:
    if ( em %in% c( 'SO2', 'NOx', 'NMVOC', 'BC', 'OC', 'CO', 'CH4', 'CO2' ) )
      scripts <- c( scripts, 'B1.1.base_comb_GAINS_EMF-30.R' )

# All emissions species execute this file, which initializes the control pct
# dataframe
    scripts <- c( scripts, 'B1.1.base_comb_EF_control_percent.R' )

# Set scripts to generate species-specific base emission factors
    if ( em %in% c( 'SO2', 'NOx', 'NMVOC', 'BC', 'OC',
                    'CO', 'CH4', 'CO2', 'NH3', 'CO2' ) ) {
        if( em == "SO2" ) {
            scripts <- c( scripts, "B1.1.base_SO2_comb_EF_parameters.R" )
        } else if ( em == "BC" ) {
            scripts <- c( scripts, "B1.1.base_BCOC_comb_EF.R" )
        } else if ( em == "OC" ) {
            scripts <- c( scripts, "B1.1.base_BCOC_comb_EF.R" )
        } else if ( em == "NH3" ) {
            scripts <- c( scripts, "B1.1.base_NH3_comb_EF.R" )
        } else if ( em == "CO2" ) {
            scripts <- c( scripts, "B1.1.base_CO2_comb_EF.R",
                                   "B1.1.2.CO2_biofuels_EF.R" )
        } else {
        # B1.1.base_OTHER_comb_EF.R processes GAINS EMF-30 efs for base emissions
        # other than SO2 and BC/OC, if emission species is SO2, BC, or OC, CO2, CH4
        # it only writes diagnostic output
            scripts <- c( scripts, "B1.1.base_OTHER_comb_EF.R")
        }
    } else {
        stop('No instructions for selecting base emission factors for
             selected emission species.')
    }

# Run all child scripts for the given emissions type. The call to
# invisible() prevents extraneous output from appearing in the console.
# lapply calls source_child on each script in the list.
    invisible( lapply( scripts, source_child ) )

    logStop()
# END
