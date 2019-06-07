# ---------------------------------------------------------------------------
# Program Name: B1.1.base_NH3_comb_EF.R
# Author: Rachel Hoesly
# Date Last Updated: 19 Jan 2016
# Program Purpose: Generate base emission factors for NH3 from NEI data
# Input Files: NH3_Default_EF_US-NEI2011.xlsx - base efs estimated from US NEI
#              A.total_activity.csv
# Output Files: Default B.[em]_comb_EF_db.csv
# Notes:
# ---------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'data_functions.R' )
# Additional function files may be required.
    log_msg <- "Processing GAINS EMF-30 data. Using as base comb EF where appropriate"
# First message to be printed to the log
    script_name <- 'B1.1.base_NH3_comb_EF.R'

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "NH3"

# Stop script if running for unsupported species
    if ( em %!in% c( 'NH3' ) ) {
      stop ( paste( 'not supported for emission species', em,
                    'remove from script list in B1.2.add_comb_EF.R
                    and/or makefile' ) )
    }
# ---------------------------------------------------------------------------
# 1. Load Data

# Read activity data calculated in Module A
    activity_data <- readData( "MED_OUT", "A.comb_activity" )
# Read in NEI data
    nei_ef <- readData ( "DEFAULT_EF_IN", 'NH3_Default_EF_US-NEI2011',
                         '.xlsx', sheet = 'EF' )
# Read in mapping file
    sector_map <- readData( "MAPPINGS", "NH3_EF_USNEI_mapping" )

# ---------------------------------------------------------------------------
# 2. Process NEI data
#    Begin with raw NEI emissions factor inputs; end with NEI emissions factors
#    per fuel extended back to 1960

# Reformat NEI data with column names and units
    units <- 't/kt'
    names( nei_ef ) <- nei_ef[ 14, ]
    fuels <- unlist( nei_ef[ 14, 5:11 ] )
    nei_ef$units <- units
    nei_ef <- nei_ef[ 1:6, c( 'CEDS_Sector', 'units' , fuels ) ]

# Make NEI fuels values numeric
    nei_ef[ fuels ] <- apply( nei_ef[ fuels ] , 2, as.numeric )

# Convert to long form and rename columns
    nei_ef_long <- melt( nei_ef, id.vars = c( 'CEDS_Sector', 'units' ) )
    names( nei_ef_long )[ which( names( nei_ef_long ) == 'value' ) ] <- 'ef'
    names( nei_ef_long )[ which( names( nei_ef_long ) == 'variable' ) ] <- 'fuel'
    names( nei_ef_long )[ which( names( nei_ef_long ) == 'CEDS_Sector' ) ] <-
          'scaling_sector'

# Convert units to kt/kt
    nei_ef_long$ef <- nei_ef_long$ef / 1000
    nei_ef_long$units <- 'kt/kt'

# Extend NEI EFs continuously back through CEDS years
    nei_ef_extended <- nei_ef_long[ , c( 'scaling_sector', 'units', 'fuel' ) ]
    nei_ef_extended[ , X_emissions_years ] <-
              replicate ( nei_ef_long$ef, n = length( X_emissions_years ) )

# Map to CEDS sectors
    sector_version <- 'v1'
    nei_ef_extended <-
        merge( unique( sector_map[ , c( paste0( 'working_sectors_',
                                                sector_version ),
                                        'EF_summary_sector' ) ] ),
                             nei_ef_extended,
                             by.x = 'EF_summary_sector',
                             by.y = 'scaling_sector',
                             all.x = TRUE )

# Rename working sector column
    names( nei_ef_extended )[ which( names( nei_ef_extended ) ==
                                     paste0( 'working_sectors_',
                                             sector_version ) ) ] <- 'sector'
# Trim unnecessary columns
    nei_ef_extended <- nei_ef_extended[ , c( 'sector', 'fuel',
                                             'units', X_emissions_years ) ]

# ---------------------------------------------------------------------------
# 3. Create default efs
#    Begin with extended efs from NEI; end with NEI

# Create a template for all combinations of CEDS isos x fuels x sectors
    default_efs <- activity_data
    default_efs[ , X_emissions_years ] <- 0

# Replace NEI values into the default efs template
    default_efs <- replaceValueColMatch( default_efs, nei_ef_extended,
                                         x.ColName = X_emissions_years,
                                         match.x = c( 'sector', 'fuel' ),
                                         addEntries = FALSE )
# Sort values
    printLog( 'Sorting' )
    default_efs <-  default_efs[ with( default_efs,
                                       order( iso, sector, fuel ) ), ]

# ---------------------------------------------------------------------------
# 6. Output
    writeData( default_efs,
               domain = "MED_OUT",
               fn = paste0( 'B.', em, '_comb_EF_db' ) )

    logStop()
# END
