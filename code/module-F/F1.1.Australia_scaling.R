#------------------------------------------------------------------------------
# Program Name: F1.1.Australia_scaling.R
# Authors' Names: Leyang Feng, Erin McDuffie
# Date Last Modified: September 17, 2020
# Program Purpose: To create scaling factors and update emissions estimate for
# Australia from latest emissions working copy by using Australia NEI data.
# Input Files: emissions_scaling_functions.R, F.[em]_scaled_EF.csv,
#              F.[em]_scaled_emissions.csv, Australia_scaling_mapping.xlsx,
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
    if ( is.na( em ) ) em <- "NOx"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'common_data.R', "data_functions.R",
                  "emissions_scaling_functions.R", "analysis_functions.R" ) # Additional function files required.
    log_msg <- "Australia inventory scaling" # First message to be printed to the log
    script_name <- paste0( em, "-F1.1.Australia_scaling.R" )

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define parameters for inventory specific script

# Stop script if running for unsupported species
    if ( em %!in% c( 'SO2', 'NOx', 'NMVOC', 'CO','NH3' ) ) {
        stop( paste( 'Australia scaling is not supported for emission species',
                     em, 'remove from script list in F1.1.inventory_scaling.R' ) )
    }

# Inventory parameters. Provide the inventory and mapping file names, the
#   mapping method (by sector, fuel, or both), and the regions covered by
#   the inventory (as a vector of iso codes)
    inv_data_folder <- 'MED_OUT'
    sector_fuel_mapping <- 'Australia'
    mapping_method <- 'sector'
    inv_name <- 'AUS_NPI' #for naming diagnostic files
    region <- c( "aus" )
    inv_years<-c( 2000:2022 )
    inventory_data_file <- paste0( 'E.', em, '_', inv_name, '_inventory' )

 # Scaling for SO2

    if ( em %in% c("SO2") ) {
        sector_fuel_mapping <- paste0(sector_fuel_mapping,'_SO2')
    }

# ------------------------------------------------------------------------------
# 2. Read In Data with scaling functions
#    Read in the inventory data, mapping file, the specified emissions species,
#    and the latest versions of the scaled EFs

    scaling_data <- F.readScalingData( inventory = inventory_data_file,
                                       inv_data_folder,
                                       mapping = sector_fuel_mapping,
                                       method = mapping_method,
                                       region, inv_name, inv_years )

    list2env( scaling_data, envir = .GlobalEnv )

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
# 5. Encorporate scaled em and EF and
#    Write Scaled emissions and emission factors

    F.addScaledToDb( scaled_ef, scaled_em, meta_notes )

# Every script should finish with this line
    logStop()
# END

