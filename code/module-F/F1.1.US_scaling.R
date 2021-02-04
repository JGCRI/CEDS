#------------------------------------------------------------------------------
# Program Name: F1.1.US_scaling.R
# Authors' Names: Tyler Pitkanen, Jon Seibert, Rachel Hoesly
# Date Last Modified: April 30, 2020
# Program Purpose: To create scaling factors and update emissions estimate for
#                  the USA region from latest emissions working copy by using
#                  aggregate USA trends inventory data.
# Input Files: emissions_scaling_functions.R, F.[em]_scaled_EF.csv,
#              F.[em]_scaled_emissions.csv, USA_scaling_mapping.csv,
#              E.[em]_US_inventory.csv
# Output Files: F.[em]_total_scaled_EF.csv, F.[em]_total_scaled_emissions.csv
# Notes:
# TODO:
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Get emission species first so can name log appropriately
    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[1]
    if ( is.na( em ) ) em <- "BC"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'common_data.R', "data_functions.R",
                  "emissions_scaling_functions.R", "analysis_functions.R",
                  "interpolation_extension_functions.R" ) # Additional function files required.
    log_msg <- "US inventory scaling..." # First message to be printed to the log
    script_name <- paste0( em, "-F1.1.US_scaling.R" )

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define parameters for inventory specific script

# Stop script if running for unsupported species
    if ( em %!in% c( 'SO2', 'NOx', 'NMVOC', 'CO', 'NH3', 'PM10', 'PM2.5','BC','OC' ) ) {
        stop( paste( 'US scaling is not supported for emission species ',
                     em, '. Remove from script list in F1.1.inventory_scaling.R...' ) )
    }

# For each Module E script, define the following parameters:
# Inventory parameters. Provide the inventory and mapping file names, the
#   mapping method (by sector, fuel, or both), and the regions covered by
#   the inventory (as a vector of iso codes)
    inv_name <- 'US' #for naming diagnostic files
    region <- c( "usa" )
    mapping_method <- 'sector'
    last_inv_year <- min( 2018, BP_last_year )
    inv_years <- c( 1970, 1975, 1980, 1985, 1990 : last_inv_year )
    inventory_data_file <- paste0( 'E.', em, '_', inv_name, '_inventory' )
    inv_data_folder <- 'MED_OUT'

    if ( em %in% c('NH3', 'PM10','PM25','BC','OC' )) {
        inv_years <- c( 1990 : last_inv_year )
    }

    # For species without signifiant emissions in this category leave at default. This allows
    # NOx from agricultural soils to be retained (which isn't otherwise in US inventory)
    sector_fuel_mapping <- paste0(inv_name,'_nomisc')

    # These species have signifaint tier1 Miscellaneous emissions
    # This will result in a small overestimate of emissions, particularly NMVOC,
    # from this category since AWB emissions are included
    if ( em %in% c("NMVOC", "PM2.5", "NH3") ) {
      sector_fuel_mapping <- inv_name
    }
# ------------------------------------------------------------------------------
# 2. Read In Data with scaling functions

# Remove commertial sector diesel emissions before scaling since most of this combustion is in the
# inventory's mobile sector
# Because we have not changed the EFs (and these also are re-set back to default values) these will
# return in the final emissions data
default_emissions <- readData( "MED_OUT", paste0( "F.", em, "_scaled_emissions" ) )

default_emissions[which(default_emissions$iso == "usa" &
                        default_emissions$sector == "1A4a_Commercial-institutional" &
                        default_emissions$fuel == "diesel_oil"), X_emissions_years] <- 0

writeData( default_emissions, domain = "MED_OUT",
           fn = paste0( "F.", em, "_scaled_emissions" ), meta = TRUE )

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

# Aggregate CEDS data to scaling sectors/fuels
    ceds_data <- F.cedsAggregate( input_em, region, mapping_method )

# ------------------------------------------------------------------------------
# 4. Calculate Scaling Factors, reaggregate to CEDS sectors

# Calculate and extend scaling factors
    scaling_factors_list <- F.scaling( ceds_data, inv_data, region,
                                       replacement_method = 'replace',
                                       max_scaling_factor = 100,
                                       replacement_scaling_factor = 100 )

    list2env( scaling_factors_list , envir = .GlobalEnv )

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
