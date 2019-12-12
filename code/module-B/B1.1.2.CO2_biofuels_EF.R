# ----------------------------------------------------------------------------
# Program Name: B1.1.2.CO2_biofuels_EF.R
# Author's Name: Rachel Hoesly
# Date Last Modified: 14 December 2016
# Program Purpose: This file processes CO2 biofuel combustion emissions factors
# Input Files: A.IEA_en_stat_ctry_hist.csv, B.[em]_comb_EF_db.csv, IEA_product_fuel.csv
# Output Files: B.[em]_comb_EF_db.csv
# Notes:
# TODO: We should rename this script B1.1.CO2_biofuels_EF.R
# ------------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "analysis_functions.R" ) # Additional function files required.
    log_msg <- paste0( "Historical energy balances from IEA, aggregated to CEDS",
                       " sectors, and fuels" ) # First message to be printed to the log
    script_name <- "B1.1.2.CO2_biofuels_EF.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "CO2"

# Stop script if running for unsupported species
    if ( em %!in% c( 'CO2' ) ) {
        stop (paste( 'not supported for emission species', em, 'remove from script
                     list in B1.2.add_comb_EF.R and/or makefile' ) )
    }

# ------------------------------------------------------------------------------
# 1. Read in files

# IEA processed historical flows df
    IEA_energy <- readData( 'MED_OUT', 'A.IEA_en_stat_ctry_hist' )
# Initialized combustion emissions factors df
    CO2_ef <- readData( 'MED_OUT', paste0( "B.", em, "_comb_EF_db" ) )

# IEA mapping file
    IEA_product_fuel <- readData( "EN_MAPPINGS", "IEA_product_fuel" )


# ------------------------------------------------------------------------------
# 2. Calculate biofuels fraction

# map fuels, biofuels notation to IEA energy
    IEA_energy$fuel <-  IEA_product_fuel[ match( IEA_energy$PRODUCT,
                                                 IEA_product_fuel$product ),
                                          'fuel' ]
    IEA_energy$bio_flag <-  IEA_product_fuel[ match( IEA_energy$PRODUCT,
                                                     IEA_product_fuel$product ),
                                              'biofuel_flag']

# Aggregate IEA flows by iso, fuel for biofuels...
    IEA_energy_bio <- IEA_energy[ which( IEA_energy$bio_flag == 1 ), ]
    IEA_aggregate_biofuels <- aggregate( IEA_energy_bio[ X_IEA_years ],
                                         by = list( iso = IEA_energy_bio$iso,
                                                    fuel = IEA_energy_bio$fuel ),
                                         sum )
# ...and for all fuels
    IEA_aggregate <- aggregate( IEA_energy[ X_IEA_years ],
                                by = list( iso = IEA_energy$iso,
                                           fuel = IEA_energy$fuel ),
                                sum )

# Calculate fraction of biofuels for each year & country
    IEA_biofuels_fraction <- IEA_aggregate[ , c( 'iso', 'fuel' ) ]
    IEA_biofuels_fraction[ X_IEA_years ] <-
          IEA_aggregate_biofuels[ match( paste( IEA_biofuels_fraction$iso,
                                                IEA_biofuels_fraction$fuel ),
                                         paste( IEA_aggregate_biofuels$iso,
                                                IEA_aggregate_biofuels$fuel ) ),
                                         X_IEA_years] /
                   IEA_aggregate[ X_IEA_years ]
# Clean up the resulting dataframe
    IEA_biofuels_fraction <-
        IEA_biofuels_fraction[ which( !is.na( IEA_biofuels_fraction[ X_IEA_end_year ] ) ), ]
    IEA_biofuels_fraction <-
        replace( IEA_biofuels_fraction, is.na( IEA_biofuels_fraction ), 0 )
    IEA_biofuels_fraction <-
        IEA_biofuels_fraction[ -which( IEA_biofuels_fraction$fuel == 'biomass' ), ]

    X_extension_years <- X_emissions_years[ X_emissions_years %!in% X_IEA_years ]
    IEA_biofuels_fraction[ X_extension_years ] <- IEA_biofuels_fraction[ X_IEA_end_year ]

# ------------------------------------------------------------------------------
# 3. Remove biofuels fraction

# Calculate final CO2 emissions factors
    final_ef <- CO2_ef[ , c( "iso", "sector", "fuel", "units" ) ]

    multiplier <- CO2_ef[ , c( "iso", "sector", "fuel", "units" ) ]
    multiplier[ X_emissions_years ] <-
        IEA_biofuels_fraction[ match( paste( final_ef$iso, final_ef$fuel ),
                                      paste( IEA_biofuels_fraction$iso,
                                             IEA_biofuels_fraction$fuel ) ),
                               X_emissions_years ]
    multiplier <- replace( multiplier, is.na( multiplier ), 0 ) ### At the moment, this creates a df of all 0s

    final_ef[ X_emissions_years ] <-
        CO2_ef[ X_emissions_years ] * ( 1 - multiplier[ X_emissions_years ] )


# -----------------------------------------------------------------------------
# 5. Output
# Add comments for each table

    comment.final_ef <- paste0( 'Base CO2 combustion EFs taking into account',
                               ' the fraction of liquid and gas fuels that',
                               ' are from biofuels' )

    writeData( final_ef, "MED_OUT", paste0( "B.", em, "_comb_EF_db" ),
               comments = comment.final_ef)

# Every script should finish with this line:
    logStop()
