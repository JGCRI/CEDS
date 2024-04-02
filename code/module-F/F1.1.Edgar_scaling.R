#------------------------------------------------------------------------------
# Program Name: F1.1.Edgar_scaling.R
# Authors' Names: Tyler Pitkanen, Jon Seibert, Rachel Hoesly, Patrick O'Rourke, Noah Prime
# Date Last Modified: June 30, 2021
# Program Purpose: To create scaling factors and update emissions estimate for
#                  Edgar emissions
# Input Files: emissions_scaling_functions.R, F.[em]_scaled_EF.csv,
#              F.[em]_scaled_emissions.csv, relevant EDGAR emissions
#              data( EDGAR v5 = v50_[em]_1970_[edgar_end_year].xls ),
#              Edgar_scaling_mapping.csv
# Output Files: F.[em]_total_scaled_EF.csv, F.[em]_total_scaled_emissions.csv,
#               E.[em]_EDGAR_inventory.csv
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
    if ( is.na( em ) ) em <- "CO"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "common_data.R", "data_functions.R",
                  "emissions_scaling_functions.R", "analysis_functions.R",
                  "interpolation_extension_functions.R" ) # Additional function files required.
    log_msg <- "Edgar inventory scaling..." # First message to be printed to the log
    script_name <- paste0( em, "-F1.1.Edgar_scaling.R" )

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define parameters for inventory specific script

# Stop script if running for unsupported species
  if ( em %!in% c( 'CH4', 'CO', 'CO2', 'N2O', 'NH3', 'NMVOC', 'NOx', 'SO2' ) ) {

      stop( paste( 'Edgar scaling is not supported for emission species ',
                   em, '. Remove from script list in F1.1.inventory_scaling.R...' ) )

  }

# For each Module E script, define the following inventory parameters:
#   Provide the inventory and mapping file names, the mapping
#   method (by sector, fuel, or both), and the regions covered by
#   the inventory (as a vector of iso codes)

    inv_data_folder <- "EM_INV"
    inv_name <- 'EDGAR' #for naming diagnostic files

    sector_fuel_mapping <- "Edgar"
    mapping_method <- 'sector'

# Identify all isos with EDGAR data
# For N2O, scale to more regions since are only scaling N2O with Annex I countries
if ( em == "N2O") {
    region <-  c( "abw", "afg", "ago", "aia", "air", "alb", "ant", "are",
                  "arg", "arm", "atg", "aze", "bdi",
                  "ben", "bfa", "bgd", "bhs", "bih", "blr", "blz",
                  "bmu", "bol", "bra", "brb", "brn", "btn", "bwa", "caf",
                  "chn", "chl", "civ", "cmr", "cod", "cog",
                  "cok", "col", "com", "cpv", "cri", "cub", "cym",
                  "dji", "dma", "dom", "dza", "ecu",
                  "egy", "eri", "esh", "eth", "fji",
                  "flk", "fro", "gab", "geo", "gha", "gib",
                  "gin", "glp", "gmb", "gnb", "gnq", "grd", "grl",
                  "gtm", "guf", "guy", "hkg", "hnd", "hti",
                  "idn", "ind", "irn", "irq", "isr", "jam",
                  "jor", "kaz", "kgz", "khm", "ken", "kir", "kna",
                  "kor", "kwt", "lbn", "lbr", "lby", "lca",
                  "lao", "lka", "lux", "lso", "mac", "mar", "mda", "mdg",
                  "mex", "mli", "moz", "mrt", "mdv", "mkd", "mmr", "mng", "mys",
                  "msr", "mtq", "mus", "mwi", "nam", "ncl", "ner",
                  "nga", "nic", "npl", "nzl", "omn",
                  "pak", "phl", "prk", "pan", "per", "plw", "png", "pri",
                  "pry", "pyf", "qat", "reu", "rus", "rwa",
                  "sau", "scg", "sdn", "sea", "sen", "shn", "slb", "sgp",
                  "sle", "slv", "som", "spm", "stp", "sur",
                  "swz", "syc", "syr", "tca", "tcd", "tgo", "tha", "tjk",
                  "tkm", "tls", "ton", "tto", "tun", "twn", "tza",
                  "uga", "ukr", "ury", "uzb", "vct", "ven", "vgb",
                  "vnm", "vut", "wsm", "yem", "zaf", "zmb", "zwe" )

# Otherwise, remove additional iso's that are scaled using some other source
} else {
    region <-  c( "abw", "ago", "aia", "air", "alb", "ant", "are",
                  "arm", "atg", "aze", "bdi",
                  "ben", "bfa", "bhs", "bih", "blr", "blz",
                  "bmu", "bol", "bra", "brb", "bwa", "caf",
                  "chl", "civ", "cmr", "cod", "cog",
                  "cok", "col", "com", "cpv", "cri", "cub", "cym",
                  "dji", "dma", "dom", "dza", "ecu",
                  "egy", "eri", "esh", "eth", "fji",
                  "flk", "fro", "gab", "geo", "gha", "gib",
                  "gin", "glp", "gmb", "gnb", "gnq", "grd", "grl",
                  "gtm", "guf", "guy", "hkg", "hnd", "hti",
                  "irn", "irq", "isr", "jam",
                  "jor", "ken", "kir", "kna",
                  "kwt", "lbn", "lbr", "lby", "lca",
                  "lso", "mac", "mar", "mda", "mdg",
                  "mex", "mli", "moz", "mrt",
                  "msr", "mtq", "mus", "mwi", "nam", "ncl", "ner",
                  "nga", "nic", "nzl", "omn",
                  "pan", "per", "plw", "png", "pri",
                  "pry", "pyf", "qat", "reu", "rus", "rwa",
                  "sau", "scg", "sdn", "sea", "sen", "shn", "slb",
                  "sle", "slv", "som", "spm", "stp", "sur",
                  "swz", "syc", "syr", "tca", "tcd", "tgo",
                  "tkm", "tls", "ton", "tto", "tun", "tza",
                  "uga", "ukr", "ury", "uzb", "vct", "ven", "vgb",
                  "vnm", "vut", "wsm", "yem", "zaf", "zmb", "zwe" )
}

# If em is CO2 and EDGAR v5 is being used, use EDGAR end year 2018
# TODO: (Future): If CO2 is ever scaled to EDGAR v5 in the future, then confirm
#        that this would work properly (as there are NAs for
#        certain values from 2016-2018).

if( em %in% c('CH4','N2O','CO2') ) {
     EDGAR_end_year = EDGAR_end_year_GHG # GHG Emissions are provided for more years than air pollutants
}

  inv_years <- c( EDGAR_start_year : EDGAR_end_year )


# ------------------------------------------------------------------------------
# 2. Read In Data with scaling functions

# Read in the inventory data, mapping file, the specified emissions species, and
# the latest versions of the scaled EFs
   inventory_data_file <- paste0( "E.", em, "_EDGAR" )
   scaling_data <- F.readScalingData( inventory = inventory_data_file,
                                      'MED_OUT',
                                      mapping = sector_fuel_mapping,
                                      method = mapping_method,
                                      region, inv_name, inv_years)

    list2env( scaling_data, envir = .GlobalEnv )

# ------------------------------------------------------------------------------
# 3. Arrange the CEDS emissions data to match the inventory data

# Create a boolean switch to determine if diagnostic data should be written
    DEBUG = TRUE

# Aggregate inventory data to scaling sectors/fuels
    inv_data <- F.invAggregate( std_form_inv, region )

    if ( DEBUG ) {
      writeData( inv_data, domain = "DIAG_OUT",
                 paste0( 'F.', em, '_Aggregated_inventory_data_', inv_name ),
                 meta = FALSE )
    }

# Aggregate ceds data to scaling sectors/fuels
    ceds_data <- F.cedsAggregate( input_em, region, mapping_method )

    if ( DEBUG ) {
      writeData( ceds_data, domain = "DIAG_OUT",
                 paste0( 'F.', em, '_Aggregated_CEDS_data_', inv_name ),
                 meta = FALSE )
    }

# ------------------------------------------------------------------------------
# 4. Calculate Scaling Factors, reaggregate to CEDS sectors

# Calculate and extend scaling factors
    scaling_factors_list <- F.scaling( ceds_data, inv_data, region,
                                       replacement_method = 'replace',
                                       max_scaling_factor = 100,
                                       replacement_scaling_factor = 100 )
    list2env( scaling_factors_list, envir = .GlobalEnv )

# Apply Scaling Factors to CEDS data
    scaled <- F.applyScale(scaling_factors)
    scaled_ef <- scaled[[ 1 ]]
    scaled_em <- scaled[[ 2 ]]

# ------------------------------------------------------------------------------
# 5. Write outputs

# Encorporate scaled em and EF
  F.addScaledToDb( scaled_ef, scaled_em, meta_notes )

# Every script should finish with this line
    logStop()

# END
