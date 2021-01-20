#------------------------------------------------------------------------------
# Program Name: F1.1.South_korea_scaling.R
# Authors' Names: Tyler Pitkanen, Jon Seibert, Rachel Hoesly, Steve Smith, Ryan Bolt, Andrea Mott
# Date Last Modified: May 4, 2020
# Program Purpose: To create scaling factors and update emissions estimate for
#                  the South Korea from latest emissions working copy by using
#                  aggregate inventory data.
# Input Files: emissions_scaling_functions.R, F.[em]_scaled_EF.csv,
#              F.[em]_scaled_emissions.csv, S_Korea_scaling_mapping.csv,
#              E.[em]_Korea_inventory.csv
# Output Files: F.[em]_total_scaled_EF.csv, F.[em]_total_scaled_emissions.csv
# Notes: (1) This uses data from 1999 - 2012
#        (2( Units are initially in Mg or Metric Tonnes
# TODO: Re-write read-in so that order of years is taken from input data instead of assumed.
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
                  "emissions_scaling_functions.R", "analysis_functions.R" ) # Additional function files required.
    log_msg <- "South Korea inventory scaling..." # First message to be printed to the log
    script_name <- paste0( em, "-F1.1.South_korea_scaling.R" )

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define parameters for inventory specific script

# Stop script if running for unsupported species
    if ( em %!in% c( 'SO2', 'NOx', 'CO', 'NMVOC','BC','OC' ) ) {
      stop( paste( 'KOR scaling is not supported for emission species ',
                    em, '. Remove from script list in F1.1.inventory_scaling.R...' ) )
    }

# For each Module E script, define the following parameters:
# Inventory parameters. Provide the inventory and mapping file names, the
#   mapping method (by sector, fuel, or both), and the regions covered by
#   the inventory (as a vector of iso codes)
    sector_fuel_mapping <- 'S_Korea'
    mapping_method <- 'sector'
    inv_name <- 'SKorea' #for naming diagnostic files
    region <- c( "kor" )
    inv_years <- c( 1999 : 2012 )

    em.read <- em
    if (em %in% c('BC','OC')) em.read = "PM10"

    inventory_data_file <- paste0( 'Korea/E.', em.read, '_', inv_name, '_inventory' )
    inv_data_folder <- 'EM_INV'

# ------------------------------------------------------------------------------
# 1.5 Calculate BC/OC emissions by PM10
    #Korea's mod E script isn't used since inv files were so large.
    # TODO: move conversion of PM10 to PM2.5 outside of BC/OC function and place here instead?
    if (em %in% c ('BC','OC') ) {

      inv_data_sheet <- readData( inv_data_folder, inventory_data_file , ".csv" )
      inv_data_sheet <- inv_data_sheet %>%
          select(-fuel) %>%
          group_by(iso, sector) %>%
          summarise_each(funs(sum)) %>%
          ungroup()

      # The Korean inv data from PM2.5 is off by 1e6
      inv_iso_sector <- inv_data_sheet[,1:2]
      inv_data_sheet <- inv_data_sheet %>%
          select(-iso,-sector)
      inv_data_sheet <- inv_data_sheet * 1000000
      inv_data_sheet <- cbind(inv_iso_sector,inv_data_sheet)

    # Define parameters for BC and OC specific script
      ceds_sector <- "1A3b_Road"
      inv_iso <- "kor"
      mapping_file <- readData("SCALE_MAPPINGS", "S_Korea_scaling_mapping.csv")
      mapping_file <- mapping_file %>%
        filter(str_detect(scaling_sector,"1A3b_Road"))
    # Remove RV since no values in data frame
      mapping_file <- subset(mapping_file, inv_sector!="RV")

      inv_sector_name <- mapping_file$inv_sector

      X_inv_years <- paste0("X",inv_years)
      PM <- "PM10"

    # Calculate BC and OC emissions

      inv_data_sheet <- F.Estimate_BC_OC_emissions(em,PM,inv_iso,ceds_sector,inv_sector_name,X_inv_years)
      inv_data_file <- inv_data_sheet

    # Print out new inv_data_file
      writeData( inv_data_file, domain = "MED_OUT",
                 paste0( 'E.', em, '_', inv_name, '_inventory' ) )

    # Change whereabout for input file for F.readScaling script
      inventory_data_file <- paste0( 'E.', em, '_', inv_name, '_inventory' )
      inv_data_folder <- 'MED_OUT'

    }


  # Scaling for specific species
    if ( em %in% c("BC", "OC") ) {
      sector_fuel_mapping <- paste0(sector_fuel_mapping,'_BCOC')
    }


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
