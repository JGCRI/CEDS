#------------------------------------------------------------------------------
# Program Name: F1.1.EMEP_NFR09_scaling.R
# Authors' Names: Rachel Hoesly, Patrick O'Rourke
# Date Last Modified: December 26th, 2015
# Program Purpose: To create scaling factors & update emissions estimate for
#                  the EMEP regions from latest emissions working CEDS copy.
# Input Files: emissions_scaling_functions.R, F.[em]_scaled_EF.csv,
#              F.[em]_scaled_emissions.csv, E.[em]_EMEP_inventory.csv,
#              EMEP_NFR09_scaling_mapping.csv, EMEP_NFR09_scaling_year.csv,
#              EMEP_NFR09_SO2_scaling_mapping.csv, EMEP_NFR09_SO2_scaling_year.csv,
#              EMEP_NFR09_SO2_scaling_method.csv
# Output Files: F.[em]_total_scaled_EF.csv, F.[em]_total_scaled_emissions.csv
# Notes: EMEP inventory has both level 1 and level 2 sectors. Choose data in
#        E.EMEP_emissions.R. Ensure that the correct scaling map is chosen
# TODO: 1.) Include a function to specify which sectors should not be scaled for
#       certain years for certain countries
#       2.) Include a function to specify which sectors should not be scaled for
#       certain countries at all

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Get emission species first so can name log appropriately
    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[1]
    if ( is.na( em ) ) em <- "NOx"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'common_data.R', "data_functions.R",
                  "emissions_scaling_functions.R", "analysis_functions.R",
                  "interpolation_extension_functions.R" ) # Additional function files required.
    log_msg <- "Modifying emissions factors from EMEP NFR09 inventory data..." # First message to be printed to the log
    script_name <- paste0( em, "-F1.1.EMEP_NFR09_scaling.R" )

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define parameters read in files

# Stop script if running for unsupported emissions species
# Note, while EMEP has BC, there is no OC, so retain consistent BC, OC estimates
    if ( em %!in% c( 'CO', 'NH3', 'NMVOC', 'NOx', 'SO2' ) ) {
        stop( paste( 'EMEP scaling is not supported for emission species ',
                     em, '. Remove from script in F1.1.inventory_scaling.R' ) )
    }

# For each Module E script, define the following parameters:
# Inventory parameters. Provide the inventory & mapping file names, the
#   mapping method (by sector, fuel, or both), & the regions covered by
#   the inventory (as a vector of iso codes)
    inv_name <- 'EMEP_NFR09'

    sector_fuel_mapping <- inv_name

# Use different mapping function if SO2 - scale back to default EFs
    if ( em == 'SO2' ) {

        sector_fuel_mapping <- paste0( sector_fuel_mapping, "_", em )

    }

    mapping_method <- 'sector'

# Do not include regions with problematic inventories ("aze", "blr", "mda", "srb", "tur", "ukr" )
# Do not include "can" since have higher resolution data to use
    region <- c( "aut", "bel", "bgr", "che", "cyp", "deu",
                 "dnk", "esp", "est", "fin", "fra", "gbr", "grc", "hrv", "hun",
                 "irl", "isl", "ita", "ltu", "lux", "lva", "mkd", "mlt",
                 "nld", "nor", "pol", "prt", "svk", "svn", "swe" )

# Limited number of years or no data for these isos, so are not included:  "
# arm", "cze", "geo", "kgz", "mne". "rou",
#       "alb" has unrealistic SO2 industrial emissions reported (looks like process and combustion were combined)
    inv_years <- c( 1980 : 2012 )

# EMEP level 1 inventory is reformatted by the E2.EMEP_em_emissions_lvl1.R script
    inventory_data_file <- paste0( "E.", em, "_", inv_name, "_inventory" )
    inv_data_folder <- "MED_OUT"

# ------------------------------------------------------------------------------
# 2. Read In Data with scaling functions

# Read in the inventory data, mapping file, the specified emissions species, and
# the latest versions of the scaled EFs
    scaling_data <- F.readScalingData( inventory = inventory_data_file,
                                       inv_data_folder,
                                       mapping = sector_fuel_mapping,
                                       method = mapping_method,
                                       region, inv_name, inv_years )

    list2env( scaling_data , envir = .GlobalEnv )

# ------------------------------------------------------------------------------
# 3. Arrange the CEDS emissions data to match the inventory data

# Aggregate inventory data to scaling sectors/fuels
    inv_data <- F.invAggregate( std_form_inv, region )

# Aggregate ceds data to scaling sectors/fuels
    ceds_data <- F.cedsAggregate( input_em, region, mapping_method )

# ------------------------------------------------------------------------------
# 4. Calculate Scaling Factors, reaggregate to CEDS sectors

# Calculate and extend scaling factors
    scaling_factors_list <- F.scaling( ceds_data, inv_data, region,
                                       replacement_method = 'replace',
                                       max_scaling_factor = 100,
                                       replacement_scaling_factor = 100 )

    list2env( scaling_factors_list, envir = .GlobalEnv )

# Apply Scaling Factors to Ceds data
    scaled <- F.applyScale( scaling_factors )
    scaled_ef <- scaled[[ 1 ]]
    scaled_em <- scaled[[ 2 ]]

# ------------------------------------------------------------------------------
# 5. Encorporate scaled em and EF

# Write Scaled emissions and emission factors
    F.addScaledToDb( scaled_ef, scaled_em, meta_notes )

# Every script should finish with this line
    logStop()

# END

