#------------------------------------------------------------------------------
# Program Name: F1.1.REAS_scaling.R
# Authors' Names: Patrick O'Rourke, Rachel Hoesly, Steve Smith
# Program Purpose: To create scaling factors & update emissions estimate for
#                  the REAS regions from latest emissions working CEDS copy.
# Input Files: emissions_scaling_functions.R, F.[em]_scaled_EF.csv,
#              F.[em]_scaled_emissions.csv, REAS_scaling_mapping.csv,
#              E.[em]_REAS_inventory.csv
# Output Files: F.[em]_total_scaled_EF.csv, F.[em]_total_scaled_emissions.csv
# Notes: REAS inventory has both level 1 and level 2 sectors. Choose data in
#       E.REAS_emissions.R. Ensure that the correct scaling map is chosen
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
    if ( is.na( em ) ) em <- "SO2"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'common_data.R', "data_functions.R",
                  "emissions_scaling_functions.R", "analysis_functions.R",
                  "interpolation_extension_functions.R" ) # Additional function files required.
    log_msg <- "Scaling emissions factors to REAS 3.2 inventory data" # First message to be printed to the log
    script_name <- paste0( em, "-F1.1.REAS32_scaling.R" )

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define parameters read in files

# Stop script if running for unsupported emissions species
# Note, while REAS has BC, there is no OC, so retain consistent BC, OC estimates
    if ( em %!in% c( 'CO', 'NH3', 'NMVOC', 'NOx', 'SO2' ) ) {
        stop( paste( 'REAS script is not supported for emission species ',
                     em, '. Remove from script list in F1.1.inventory_scaling.R...' ) )
    }

# For each Module E script, define the following parameters:
# Inventory parameters. Provide the inventory & mapping file names, the
#   mapping method (by sector, fuel, or both), & the regions covered by
#   the inventory (as a vector of iso codes)
    inv_name <- 'REAS32'
    sector_fuel_mapping <- inv_name
    mapping_method <- 'sector'

    # REAS 3.2 has a different mapping for NMVOC and NH3
    if ( em %in% c("NMVOC", "NH3") ) {
      sector_fuel_mapping <- paste0(inv_name,"_",em)
    }

    # Include all REAS countries that we don't have country-specific data for 
    # For SO2 only include "jpn","kor", "twn" so we capture longer-term trends
    if (em == "SO2" ) { 
       region <- c( "jpn","kor", "twn" )
       inv_years <- c( 1980 : 2014 )
    } else {
       # Otherwise, user REAS32 for countries we dont otherwise have inventories for
       region <- c( "afg", "bgd", "brn", "btn", "idn", 
                  "khm", "lao", "lka", "mdv", "mmr", "mng", "mys", "npl",
                  "pak", "phl", "prk", "sgp", "tha", "vnm" )
       # Use narrower range of years where activity data is more likely to be consistent
       # between IEA data versions, etc.
       inv_years <- c( 1990 : 2013 )
    }

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

