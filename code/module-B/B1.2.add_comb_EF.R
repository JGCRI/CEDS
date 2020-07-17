#------------------------------------------------------------------------------
# Program Name: B1.2.add_comb_EF.R
# Author: Jon Seibert, Rachel Hoesly
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
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c() # Additional function files required.
    log_msg <- paste0( "Calling species-specific child script to add combustion",
                       " emissions data to the database" ) # First message to be printed to the log
    script_name <- "B1.2.add_comb_EF.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )


    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "SO2"

    MODULE_B <- "../code/module-B/"
# ---------------------------------------------------------------------------
# 0.5. Load Packages and Define functions

# Create a function that can be applied to source all child scripts for the given
# emissions type.
    source_child <- function( file_name ) {

        source( paste( MODULE_B, file_name, sep = "" ) )

    }

#-------------------------------------------------------------------------------------
# 1. List scripts to call
    scripts <- c()

# Set scripts to add emissions data for SO2
    if ( em == "SO2" ) {
      #scripts that fill out parameters for SO2 EF (ash retention, control fraction)
        scripts <- c( 'B1.2.add_SO2_comb_GAINS_ash_ret.R',
                      'B1.2.add_SO2_comb_GAINS_s_content.R',
                      'B1.2.add_SO2_comb_GAINS_control_percent.R',
                      'B1.2.add_SO2_comb_diesel_sulfur_content.R' )
    # Remove this for now. Script now overwrites previous control%, which is not what we want.
    # Script needs to read other control %s, and extend from there.
    #                'B1.2.add_SO2_recent_control_percent.R')
    # adds script that calculates emission factors from SO2 parameters
        scripts <- c( scripts, 'B1.2.add_SO2_comb_S_content_ash.R',
                      'B1.3.proc_SO2_comb_EF_S_content_ash.R' )
    }

# Set scripts to add emissions data for BC or OC
    if ( em == "BC" || em == "OC" ) {
        scripts <- c( 'B1.2.add_BCOC_recent_control_percent.R' )
    }

# Add scripts For all emissions species - default EFs and control percent
# Default EFs
    scripts <- c( scripts, 'B1.2.add_comb_default_EF.R' )

# Add control percent and processing script for all emission types except CO2
    if ( em != 'CO2' ) scripts <- c( scripts, 'B1.2.add_comb_control_percent.R' )

# Add recent control percentage for SO2
    if ( em == 'SO2' ) {
        scripts <- c( scripts, 'B1.2.add_SO2_recent_control_percent.R' )
    }

    if ( em != 'CO2' ) {
        scripts <- c( scripts, 'B1.3.proc_comb_EF_control_percent.R' )
    }
#-------------------------------------------------------------------------------------
# 1. Call Scripts

# Run all child scripts for the given emissions type. The call to
# invisible() prevents extraneous output from appearing in the console.
# lapply calls source_child on each script in the list.
    invisible( lapply( scripts, source_child ) )

    logStop()
# END
