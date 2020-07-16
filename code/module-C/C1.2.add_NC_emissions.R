#------------------------------------------------------------------------------
# Program Name: C1.2.add_NC_emissions.R
# Author: Jon Seibert
# Date Last Updated: January 22, 2020
# Program Purpose: To select and run the correct script(s) to fill out to the non-combustion
#                  (process) emissions database for the given emissions type.
# Input Files: None
# Output Files: None
# Notes:
# TODO: Add conditionals and script specifications for other emissions types
#       as they are added to the system.
# ------------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if( "input" %in% dir( ) ) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R" ) # Additional function files required.
    log_msg <- paste0( "Calling species-specific child script to add non-combustion",
              " emissions data to the database" ) # First message to be printed to the log
    script_name <- "C1.2.add_NC_emissions.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------------

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]

if ( is.na( em ) ) em <- "CH4"

MODULE_C <- "../code/module-C/"

# Create a function that can be applied to source all child scripts for the given
# emissions type.
source_child <- function( file_name ){ source( paste( MODULE_C, file_name, sep = "" ) ) }

scripts <- c()

# Set scripts to add emissions data for SO2
if( em == "SO2" ){
# Remove outdated scripts -- revise when move to working sectors v2
#   scripts <- c( "C1.2.add_SO2_NC_emissions_all.R" , "C1.2.add_SO2_NC_emissions_FAO.R" )
}

# Set scripts to add emissions data for BC or OC (currently no data to add)
if( em == "BC" || em == "OC" ){
    scripts <- c(  )
}

# Create GAINS fugitive oil and gas subsector emission splits
if ( em %!in% c( "NH3" ) ){

    scripts <- c( scripts, 'C1.2.GAINS_fugitive_petr_gas_emissions.R' )

}

# Add EDGAR data script for all relevant emissions species
if( em == "SO2" || em == "CO" || em == "NOx" || em == "NMVOC"  || em == "NH3" ){
    scripts <- c( scripts, "C1.2.add_NC_emissions_EDGAR_PEGASOS.R" )
}

# Note if using EDGAR 4.2 then also need to edit correction for end-year in C2.1.base_NC_EF.R
# TODO: check if the note above is still relevant
if( em == "CH4" || em == "CO2" || em == "N2O" ){
    scripts <- c( scripts, "C1.2.add_NC_emissions_EDGAR.R" )
}

# Add EPA adipic and nitric acid emissions data (extended by EDGAR)
#   Processed EPA data
    if( em == "N2O" ){
        scripts <- c( scripts, "C1.2.EPA_adipic_and_nitric_acid.R" )
    }

#   The below script currently makes emissions with all 0 values for emission species other than N2O.
#   For N2O, emissions processed in C1.2.EPA_adipic_and_nitric_acid.R are extended here
    if( em %in% c( "BC", "CH4", "CO", "CO2", "N2O", "NH3", "NMVOC", "NOx", "OC", "SO2" ) ){
        scripts <- c( scripts, "C1.2.Adipic_nitric_acid_default_process_emissions.R" )
    }

# Add FAO Agriculture methane data
if( em == "CH4" ){
  scripts <- c( scripts, "C1.2.add_CH4_NC_emissions_FAO.R" )
}

# Add FAO Agriculture N2O data
# TODO: could make this a paste0 with em object so that it's just one line with CH4
if( em == "N2O" ){
    scripts <- c( scripts, "C1.2.add_N2O_NC_emissions_FAO.R" )
}

# Add CDIAC for CO2
if( em == "CO2" ){
  scripts <- c( scripts, "C1.2.add_CO2_NC_emissions_CDIAC.R" )
}


# Add Fugitive defaults for non-CO2 emission species
if ( !(em %in% c( "CO2" ) ) ) {
  scripts <- c( scripts, 'C1.2.ECLIPSE_flaring_emissions_extension.R' )
  scripts <- c( scripts, 'C1.2.Fugitive-petr-and-gas_default_process_emissions.R' )
}

# Run all child scripts for the given emissions type. The call to
# invisible() prevents extraneous output from appearing in the console.
# lapply calls source_child on each script in the list.
invisible( lapply( scripts, source_child ) )

logStop()
# END
