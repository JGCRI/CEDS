#------------------------------------------------------------------------------
# Program Name: F1.1.Edgar_scaling.R
# Authors' Names: Tyler Pitkanen, Jon Seibert, Rachel Hoesly, Patrick O'Rourke
# Date Last Modified: August 12, 2020
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
    if ( is.na( em ) ) em <- "CH4"

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
    vn <- "5.0"  # EDGAR data version number
    inv_data_folder <- "EM_INV"

    if( as.numeric( vn ) == 4.3 ){

      inv_name <- 'EDGAR_PG' # for naming diagnostic files

    } else{

      inv_name <- 'EDGAR' #for naming diagnostic files

    }

    sector_fuel_mapping <- "Edgar"
    mapping_method <- 'sector'

# Identify all isos with EDGAR data
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

# If em is CO2 and EDGAR v5 is being used, use EDGAR end year 2018
# TODO: (Future): If CO2 is ever scaled to EDGAR v5 in the future, then confirm
#        that this would work properly (as there are NAs for
#        certain values from 2016-2018).
  # if( vn == "5.0" & em == "CO2" ){
  #
  #   EDGAR_end_year <- 2018
  #
  # }

  inv_years <- c( EDGAR_start_year : EDGAR_end_year )

# ------------------------------------------------------------------------------
# 1.5 Inventory in Standard Form (iso-sector-fuel-years, iso-sector-years, etc)

# Import file
#   Define settings for EDGAR v4.2
  if( as.numeric( vn ) == 4.2 ){

    inventory_data_file <- paste0( 'EDGAR42_', em )
    inv_data_sheet <- readData( inv_data_folder, domain_extension = "EDGAR/",
                                    inventory_data_file )

#   Define settings for EDGAR v4.3
  } else if( as.numeric( vn ) == 4.3 ){

    inventory_data_file <- paste0( 'JRC_PEGASOS_', em, '_TS_REF' )
    sheet_name = paste0( 'NEW_v4.3_EM_', em, '_ref' )
    rows_to_skip <- 8
    extension_use <- ".xls"

    inv_data_sheet <- readData( inv_data_folder, domain_extension = "EDGAR/",
                                inventory_data_file, extension_use,
                                sheet_selection = sheet_name, skip = rows_to_skip )

#   Define settings for EDGAR v5
  } else if( as.numeric( vn ) == 5 ){

    inventory_data_file <- paste0( "v",  gsub( "[.]", "", vn ), "_", em, "_",
                                   EDGAR_start_year, "_", EDGAR_end_year )
    sheet_name <- paste0( "v", vn, "_EM_", em, "_IPCC1996" )
    rows_to_skip <- 9
    extension_use <- ".xls"

    inv_data_sheet <-  readData( inv_data_folder, domain_extension = "EDGAR/",
                        inventory_data_file,  extension_use,
                        sheet_selection = sheet_name, skip = rows_to_skip,
                        missing_value = c( "", "NULL" ) )

#   Define settings for other EDGAR versions
  } else {

    stop( script_name, " has not been formatted to process Edgar v", vn, ". Please ",
          "reformat the script and rerun." )
  }

# Clean rows and columns to standard format
    inv_data_sheet$units <- 'kt'

    if( as.numeric( vn ) %in% c( 4.3, 5 ) ){

      inv_data_sheet <- inv_data_sheet[ , c( 'ISO_A3', 'IPCC', 'units',
                                           inv_years ) ]

    } else if( as.numeric( vn ) == 4.2 ){

      inv_data_sheet <- inv_data_sheet[ , c( 'ISO_A3', 'IPCC', 'units',
                                             paste0( 'X', inv_years ) ) ]

    }

    names( inv_data_sheet ) <- c( 'iso', 'sector', 'units',
                                  paste0( 'X', inv_years ) )
    inv_data_sheet$iso <- tolower( inv_data_sheet$iso )

# Remove rows with all NA's
    inv_data_sheet <-
        inv_data_sheet[ apply( X = inv_data_sheet[ , paste0( "X", inv_years ) ],
                               MARGIN = 1, function( x )
                                             ( !all( is.na( x ) ) ) ) ,]

# Make negative emissions zero
    inv_data_sheet_neg_fixed <- inv_data_sheet %>%
      mutate_at( .vars = paste0( "X", inv_years ), .funs = funs( if_else( . < 0 , 0, . ) ) )

    rows_that_had_negatives <- dplyr::setdiff( inv_data_sheet, inv_data_sheet_neg_fixed )

    if( nrow( rows_that_had_negatives ) > 0 ){

      printLog( "Some EDGAR values were negative and reset to 0 in ", script_name, "..." )

      inv_data_sheet <- inv_data_sheet_neg_fixed

    }

# Write standard form inventory
    writeData( inv_data_sheet, domain = "MED_OUT",
               paste0( 'E.', em, '_', inv_name, '_inventory' ) )

    inventory_data_file <- paste0( 'E.', em, '_', inv_name, '_inventory' )
    inv_data_folder <- 'MED_OUT'

# ------------------------------------------------------------------------------
# 2. Read In Data with scaling functions

# Read in the inventory data, mapping file, the specified emissions species, and
# the latest versions of the scaled EFs
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
