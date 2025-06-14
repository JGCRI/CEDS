#------------------------------------------------------------------------------
# Program Name: F1.1.inventory_scaling.R
# Author: Rachel Hoesly adapted from Jon Seibert
# Date Last Updated: September 17, 2020
# Program Purpose: To select and run script(s) to scale default emissions and
#                  emission factors to inventories.
# Input Files: D.[em]_default_total_EF.sv, D.[em]_default_total_emissions.csv
# Output Files: F.[em]_scaled_EF.csv, F.[em]_scaled_emissions.csv
# Functions Defined: source_child
# Notes:
# TODO: Add conditionals and script specifications for other emissions types
#       as they are added to the system.
# ------------------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Get emission species first so can name log appropriately
    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[1]
    if ( is.na( em ) ) em <- "NMVOC"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "emissions_scaling_functions.R",
                  "analysis_functions.R",
                  "interpolation_extension_functions.R" ) # Additional function files required.
    log_msg <- paste0( "Calling inventory emission scaling stripts..." ) # First message to be printed to the log
    script_name <- paste0( em, "-F1.1.inventory_scaling.R" )

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------------
# 0.5 Define Functions
#     Create a function that can be applied to source all child scripts for the
#     given emissions type.
    source_child <- function( file_name ) {
        source( paste( MODULE_F, file_name, sep = "" ) )
    }

# ------------------------------------------------------------------------------------
# 1. Define emission species and read in files

    MODULE_F <- "../code/module-F/"

    EF  <- readData( "MED_OUT", paste0( "D.", em, "_default_total_EF" ) )
    emissions <- readData( "MED_OUT", paste0( "D.", em, "_default_total_emissions" ) )

# ------------------------------------------------------------------------------------
# 2. Initialize base files

# Create a base dataframe of scaled emissions and emissions factors for input into scaling
# modules - This is the working set of emissions and ef's that the scaling scripts alter.
# Before scripts are run, these are identical to the default emissions.

    writeData( EF, domain = "MED_OUT",
               fn = paste0( "F.", em, "_scaled_EF" ), meta = TRUE )
    writeData( emissions, domain = "MED_OUT",
               fn = paste0( "F.", em, "_scaled_emissions" ), meta = TRUE )

# Initialize blank value_metadata file
    F.initializeMeta( EF )

# ------------------------------------------------------------------------------------
# 3. Call scaling scripts for various species.
#    Depending on the emissions species, this section of code accumulates in a
#    list the names of all scripts that correspond to that em.
    scripts <- c()

# EDGAR
    if ( em %in% c( 'CH4', 'CO', 'N2O', 'NH3', 'NMVOC', 'NOx' ) ){ scripts <-
              c( scripts, 'F1.1.Edgar_scaling.R' ) }

# EMEP NFR09 (older data - use because has more sectors and goes back further)
    if ( em %in% c( 'CO', 'NH3', 'NMVOC', 'NOx', 'SO2' ) ){ scripts <-
        c( scripts,'F1.1.EMEP_NFR09_scaling.R' ) }

# EMEP NFR14
# Note 1: While EMEP has BC, there is no OC, so retain consistent BC & OC estimates by not
#         scaling to EMEP for these ems
# Note 2: While CEDS does not include EMEP BC scaling, CEDS-GBD does include BC EMEP scaling
# Note 3: Below, added BC and OC using PM2.5 data and BC/PM2.5 and OC/PM2.5 default ratios

    if ( em %in% c( 'CO', 'NH3', 'NMVOC', 'NOx', 'SO2','BC','OC' ) ){ scripts <-
        c( scripts, 'F1.1.EMEP_NFR14_scaling.R' ) }

# UNFCCC
    if ( em %in% c( 'SO2', 'CO', 'NMVOC', 'NOx', 'CO2', 'CH4', 'N2O' ) ){ scripts <-
        c( scripts, 'F1.1.UNFCCC_scaling.R' ) }

# REAS 3.2
    if ( em %in% c('SO2','CO','NMVOC','NOx', 'NH3') ){ scripts <-
        c( scripts, 'F1.1.REAS32_scaling.R' ) }

# CAN - old to 2011
    if ( em %in% c('SO2','NOx','NMVOC','CO','PM10','PM25' ) ){ scripts <-
        c( scripts, 'F1.1.CAN_scaling_old_to2011.R' ) }

# CAN - Newer data must run last
    if ( em %in% c( 'SO2', 'NOx', 'NMVOC', 'CO','NH3','BC','OC' ) ){ scripts <-
        c( scripts, 'F1.1.CAN_scaling.R' ) }

# USA
    # removed scaling of BC and OC due to discrepancy in US PM2.5 data
    if ( em %in% c( 'SO2', 'NOx', 'NMVOC', 'CO','NH3' ) ){ scripts <-
        c( scripts, 'F1.1.US_scaling.R' ) }

# US EPA (remove - use updated UNFCCC to scale non-combustion emissions instead)
    if ( em %in% c( 'CO2xx' ) ){ scripts <-
        c( scripts, 'F1.1.US-EPA_scaling.R' ) }

# US GHG
    if ( em %in% c( 'CH4', 'N2O' ) ){ scripts <-
        c( scripts, 'F1.1.US-GHG_scaling.R' ) }

# China
    # OLD MEIC:
    # GBD settings: if ( em %in% c( 'SO2', 'NOx', 'NH3', 'NMVOC', 'CO','BC','OC' ) ) scripts <-
    if ( em %in% c( 'SO2', 'NOx', 'NH3', 'NMVOC', 'CO' ) ){ scripts <-
        c( scripts, 'F1.1.China_scaling.R' ) }

    # New MEIC:
    # GBD settings: if ( em %in% c( 'SO2', 'NOx', 'NH3', 'NMVOC', 'CO','BC','OC' ) ) scripts <-
    if ( em %in% c( 'SO2', 'NOx', 'NH3', 'NMVOC', 'CO','BC','OC' ) ){ scripts <-
        c( scripts, 'F1.1.China_MEIC_2018_scaling.R' ) }

# Japan
    if ( em %in% c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3','BC','OC' ) ){ scripts <-
        c(scripts, 'F1.1.Japan_scaling.R' ) }

# Taiwan
    if ( em %in% c( 'SO2', 'NOx', 'CO', 'NMVOC', 'BC', 'OC' ) ){ scripts <-
        c( scripts, 'F1.1.Taiwan_scaling.R' ) }

# Australia
    if ( em %in% c( 'SO2', 'NOx', 'CO', 'NMVOC', 'NH3' ) ){ scripts <-
        c( scripts, 'F1.1.Australia_scaling.R' ) }

# Argentina
    if ( em %in% c( 'SO2', 'NOx', 'CO', 'NMVOC' ) ){ scripts <-
        c( scripts, 'F1.1.Argentina_scaling.R' ) }

# South Korea
    if ( em %in% c( 'SO2', 'NOx', 'CO', 'NMVOC','NH3', 'BC','OC' ) ) { scripts <-
        c( scripts, 'F1.1.South_Korea_scaling.R' ) }

# South Korea EDGAR-HTAPv3.1
    if ( em %in% c('CO','NH3','NMVOC','NOx','SO2' ) ) { scripts <-
        c( scripts, 'F1.1.South_Korea_EDGAR-HTAPv3.1_scaling.R' ) }

# Japan EDGAR-HTAPv3.1
    if ( em %in% c('CO','NH3','NMVOC','NOx','SO2' ) ) { scripts <-
        c( scripts, 'F1.1.Japan_EDGAR-HTAPv3.1_scaling.R' ) }

# EDGAR-HTAPv3.1 BC-OC
    if ( em %in% c('BC','OC' ) ) { scripts <-
        c( scripts, 'F1.1.BCOC_EDGAR-HTAPv3.1_scaling.R' ) }

# ------------------------------------------------------------------------------------
# 4. Run all scripts for the given emissions type

    invisible( lapply( scripts, source_child ) )

    logStop()

# END
