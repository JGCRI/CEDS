#------------------------------------------------------------------------------
# Program Name: F1.1.Edgar_PEGASOS_scaling.R
# Authors' Names: Tyler Pitkanen, Jon Seibert, Rachel Hoesly
# Date Last Modified: Dec 27, 2015
# Program Purpose: To create scaling factors and update emissions estimate for
#                  Edgar PEGASOS
# Input Files: emissions_scaling_functions.R, F.[em]_scaled_EF.csv,
#              F.[em]_scaled_emissions.csv, JRC_PEGASOS_[em]_TS_REF.xlsx
# Output Files: F.[em]_total_scaled_EF.csv, F.[em]_total_scaled_emissions.csv,
#               E.[em]_EDGAR_PG_inventory.csv
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
    log_msg <- "Edgar inventory scaling" # First message to be printed to the log
    script_name <- paste0( em, "-F1.1.Edgar_PEGASOS_scaling.R" )

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define parameters for inventory specific script

# Stop script if running for unsupported species
  if ( em %!in% c( 'SO2', 'NOx', 'NMVOC', 'CO', 'CH4', 'NH3' ) ) {
      stop( paste( 'Edgar scaling is not supported for emission species ',
                    em, '. Remove from script
                    list in F1.1.inventory_scaling.R' ) )
  }


# For each Module E script, define the following parameters:
# Inventory parameters. Provide the inventory and mapping file names, the
#   mapping method (by sector, fuel, or both), and the regions covered by
#   the inventory (as a vector of iso codes)

    inventory_data_file <- paste0( 'JRC_PEGASOS_', em, '_TS_REF' )
    inv_data_folder <- "EM_INV"
    sector_fuel_mapping <- 'Edgar_scaling_mapping'
    mapping_method <- 'sector'

# Identify all countries in this inventory. Countries with poor or inconsistent
# data not included ('srb', "mlt", "tur", bhr')
    inv_name <- 'EDGAR_PG' # for naming diagnostic files
    region <-  c( "can", "spm", "usa", "mex", "abw", "aia", "ant", "atg", "bhs",
                  "blz", "bmu", "brb", "cri", "cub", "cym", "dma", "dom", "glp",
                  "grd", "gtm", "hnd", "hti", "jam", "kna", "lca", "msr", "mtq",
                  "nic", "pan", "pri", "slv", "tca", "tto", "vct", "vgb", "bra",
                  "arg", "bol", "chl", "col", "ecu", "flk", "guf", "guy", "per",
                  "pry", "sur", "ury", "ven", "dza", "egy", "esh", "lby", "mar",
                  "tun", "ben", "bfa", "caf", "civ", "cmr", "cod", "cog", "cpv",
                  "gab", "gha", "gin", "gmb", "gnb", "gnq", "lbr", "mli", "mrt",
                  "ner", "nga", "sen", "shn", "sle", "stp", "tcd", "tgo", "bdi",
                  "com", "dji", "eri", "eth", "ken", "mdg", "mus", "reu", "rwa",
                  "sdn", "som", "syc", "uga", "ago", "bwa", "lso", "moz", "mwi",
                  "nam", "swz", "tza", "zaf", "zmb", "zwe", "aut", "bel", "che",
                  "deu", "dnk", "esp", "fin", "fra", "fro", "gbr", "gib", "grc",
                  "grl", "irl", "isl", "ita", "lux", "nld", "nor", "prt", "swe",
                  "alb", "bgr", "bih", "cyp", "cze", "est", "hrv", "hun", "ltu",
                  "lva", "mkd", "pol", "rou", "scg", "svk", "svn", "blr", "mda",
                  "ukr", "kaz", "kgz", "tjk", "tkm", "uzb", "arm", "aze", "geo",
                  "rus", "are", "irn", "irq", "isr", "jor", "kwt", "lbn", "omn",
                  "qat", "sau", "syr", "yem", "afg", "bgd", "btn", "ind", "lka",
                  "mdv", "npl", "pak", "kor", "prk", "chn", "hkg", "mac", "mng",
                  "twn", "brn", "khm", "lao", "mmr", "mys", "phl", "sgp", "tha",
                  "tls", "vnm", "idn", "png", "jpn", "aus", "cok", "fji", "kir",
                  "ncl", "nzl", "plw", "pyf", "slb", "ton", "vut", "wsm", "sea",
                  "air" )

    inv_years <- c( EDGAR_start_year:EDGAR_end_year )

# ------------------------------------------------------------------------------
# 2. Inventory in Standard Form (iso-sector-fuel-years, iso-sector-years, etc)

    sheet_name = paste0( 'NEW_v4.3_EM_', em, '_ref' )

# Import file
    inv_data_sheet <- readData( inv_data_folder, domain_extension = "EDGAR/",
                                inventory_data_file, ".xlsx",
                                sheet_selection = sheet_name, skip = 8 )

# Clean rows and columns to standard format
    inv_data_sheet$units <- 'kt'
    inv_data_sheet <- inv_data_sheet[ , c( 'ISO_A3', 'IPCC',
                                           'units', inv_years ) ]
    names( inv_data_sheet ) <- c( 'iso', 'sector', 'units',
                                  paste0( 'X', inv_years ) )
    inv_data_sheet$iso <- tolower( inv_data_sheet$iso )

# Remove rows with all NAs
    inv_data_sheet <-
         inv_data_sheet[ apply( X = inv_data_sheet[ , paste0( "X", inv_years ) ],
                                MARGIN = 1,
                                function( x ) ( !all( is.na( x ) ) ) ), ]

# Write standard form inventory
    writeData( inv_data_sheet, domain = "MED_OUT",
               paste0( 'E.', em, '_', inv_name, '_inventory' ) )

    inventory_data_file <- paste0( 'E.', em, '_', inv_name, '_inventory' )
    inv_data_folder <- 'MED_OUT'

# ------------------------------------------------------------------------------
# 2. Read In Data with scaling functions
#    Read in the inventory data, mapping file, the specified emissions species, and
#    the latest versions of the scaled EFs

    scaling_data <- F.readScalingData( inventory = inventory_data_file,
                                       inv_data_folder,
                                       mapping = sector_fuel_mapping,
                                       method = mapping_method,
                                       region, inv_name, inv_years )
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

# Apply Scaling Factors to Ceds data
    scaled <- F.applyScale( scaling_factors )
    scaled_ef <- scaled[[ 1 ]]
    scaled_em <- scaled[[ 2 ]]

# ------------------------------------------------------------------------------
# 5. Incorporate scaled em and EF and write scaled emissions and emission factors

    F.addScaledToDb( scaled_ef, scaled_em, meta_notes )

# Every script should finish with this line

    logStop()

# END
