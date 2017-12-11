#------------------------------------------------------------------------------
# Program Name: F1.1.UNFCCC_scaling.R
# Authors' Names: Rachel Hoesly
# Date Last Modified: Oct 26, 2015
# Program Purpose: To create scaling factors and update emissions estimate for
# the UNFCCC regions from latest emissions working copy
# Input Files: emissions_scaling_functions.R, F.[em]_scaled_EF.csv,
#              F.[em]_scaled_emissions.csv, UNFCCC_sector_mapping.xls,
#              E.SO2_UNFCCC_inventory.csv
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
                  "emissions_scaling_functions.R", "analysis_functions.R",
                  "interpolation_extension_functions.R" ) # Additional function files required.
    log_msg <- "Modifying emissions factors from UNFCCC inventory data" # First message to be printed to the log
    script_name <- paste0( em, "-F1.1.UNFCCC_scaling.R" )
  
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define parameters read in files

# Stop script if running for unsupported species
    if ( em %!in% c( 'SO2', 'CO', 'NMVOC', 'NOx', 'CO2' ) ) {
        stop( paste( 'UNFCCC script is not supported for emission species ', em,
                     '. Remove from script list in F1.inventory_scaling.R' ) )
    }

# For each Module E script, define the following parameters:
# Inventory parameters. Provide the inventory and mapping file names, the
#   mapping method (by sector, fuel, or both), and the regions covered by
#   the inventory (as a vector of iso codes)
    
    sector_fuel_mapping <- 'UNFCCC_scaling_mapping'
    if ( em == 'CH4')  sector_fuel_mapping <- 'UNFCCC_scaling_mapping_CH4'
    mapping_method <- 'sector'
    inv_name <- 'UNFCCC'
    region <- c( "aus", "aut", "bel", "bgr", "blr", "che", "cyp", "cze", "deu",
                 "dnk", "esp", "est", "fin", "fra", "gbr", "grc", "hrv", "hun",
                 "irl", "isl", "ita", "jpn", "ltu", "lva", "mlt", "nld", "nor",
                 "nzl", "prt", "rou", "svk", "svn", "swe", "tur", "ukr" )
    
# include only regions that aren't scaled elsewhere for non-CO2 emissions
# TODO: blr and ukr should only be used for specific years where is close to expert estimates, or to calibrate parameters off-line
# TODO: why is grc not in EMEP?
# Ukraine (ukr) energy reporting is inconsistent, instead have calibrated coal S% by hand. Check other emissions
    if ( em != "CO2" ) region <- c( "blr" , "grc" , "nzl" )
    inv_years <- c( 1990:2012 )
  
# UNFCCC inventory is processed in E.UNFCCC_[em]_emissions.R script
    inventory_data_file <- paste0( 'E.', em, '_UNFCCC_inventory' )
    inv_data_folder <- "MED_OUT"
  
# ------------------------------------------------------------------------------
# 2. Read In Data with scaling functions
# Read in the inventory data, mapping file, the specified emissions species, and
# the latest versions of the scaled EFs  
  
    scaling_data <- F.readScalingData( inventory = inventory_data_file, 
                                       inv_data_folder,
                                       mapping = sector_fuel_mapping,
                                       method = mapping_method,
                                       region, inv_name, inv_years)
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
  
