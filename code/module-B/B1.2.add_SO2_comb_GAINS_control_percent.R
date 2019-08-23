# ---------------------------------------------------------------------------
# Program Name: B1.2.add_SO2_comb_GAINS_control_percent.R
# Author: Ryan Bolt, Leyang, Linh Vu, Patrick O'Rourke
# Date Last Updated: April 1, 2019
# Program Purpose: Process 2005 GAINS emissions and fuel to calculate GAINS EF, then calculate
# 2005 GAINS control percentage. Combine 2005 GAINS control percentage to SO2 control percentage
# database in the end.
# Input Files: Master_Country_List.csv,
#              GAINS_emiss_act_sect-EU28-2005-SO2_nohead.csv,
#              GAINS_aen_act_sect-nohead-EU28.csv,
#              GAINS_country_mapping.csv, GAINS_fuel_mapping.csv,
#              GAINS_sector_mapping.csv, B.SO2_GAINS_s_content,
#              B.SO2_GAINS_s_ash_ret.csv, B1.1.Europe_heat_content_IEA,
#              GAINS_pre_ext_year.csv
# Output Files: B.SO2_GAINS_control_percent.csv
# Notes:
# TODO:
#
# ---------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "analysis_functions.R",'process_db_functions.R',
                  'common_data.R', 'IO_functions.R', 'data_functions.R', 'timeframe_functions.R',
                  'interpolation_extension_functions.R') # Additional function files may be required.
    log_msg <- "Aggregating EU GAINS Data" # First message to be printed to the log
    script_name <- "B1.2.add_SO2_GAINS_control_percent.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 0.5 Load Packages, define functions

    loadPackage( 'zoo' )

# ---------------------------------------------------------------------------
# 1. Reading data and mapppings into script

# Read in CEDS mapping file
    MCL <- readData( "MAPPINGS",  "Master_Country_List" )

# Read in GAINS input
    EUemiss.import  <- readData( "EM_INV",
                                 "GAINS_emiss_act_sect-EU28-2005-SO2_nohead" )
    EUfuel.import  <- readData( "EM_INV",  "GAINS_aen_act_sect-nohead-EU28" )

# GAINS mapping files
    gainstoiso <- readData( "GAINS_MAPPINGS",  "GAINS_country_mapping" )
    fuelmap <- readData(  "GAINS_MAPPINGS", "GAINS_fuel_mapping" )
    sectormap <- readData( "GAINS_MAPPINGS",  "GAINS_sector_mapping" )

# Previously processed GAINS S content
    s_content <- readData( "DEFAULT_EF_PARAM", "B.SO2_GAINS_s_content")

# Previously processed GAINS S ash retention - retain only EU
    gains_ashret <- readData('DEFAULT_EF_PARAM', 'B.SO2_GAINS_s_ash_ret')

# Previously processed IEA heat content data
    GAINS_heat_content <- readData( "MED_OUT", "B1.1.Europe_heat_content_IEA" )

# If there is GAINS pre ext year default data, read that in
    if ( file.exists( filePath( "GAINS_MAPPINGS", "GAINS_pre_ext_year" ) ) )
        pre_ext_year_map <- readData( "GAINS_MAPPINGS",  "GAINS_pre_ext_year" )

# ---------------------------------------------------------------------------

# 2. Subset s content and ash retention data only for European countries in GAINS
#    data
    unique_GAINS_european_isos_list <- c("aut", "bel", "bgr", "cyp", "cze", "deu",
                                        "dnk", "esp", "est", "fin", "fra", "gbr",
                                        "grc", "hrv", "hun", "irl", "ita",
                                        "ltu", "lux", "lva", "mlt", "nld", "pol",
                                        "prt", "rou", "svk", "svn", "swe" )

    s_content_EU <- s_content %>%
        dplyr::filter( iso %in% unique_GAINS_european_isos_list ) %>%
        dplyr::select_if( ~sum( ! is.na( . ) ) > 0 )

    gains_ashret_EU <- gains_ashret %>%
        dplyr::filter( iso %in% unique_GAINS_european_isos_list ) %>%
        dplyr::select_if( ~sum( ! is.na( . ) ) > 0 )

# ---------------------------------------------------------------------------
# 3. Pre-process EUfuel and EUemiss and map to CEDS

# Initialize processing dfs
    EUfuel <- EUfuel.import
    EUemiss <- EUemiss.import

# Prepare the data: change n.a's into 0s and make numeric
    EUfuel[ EUfuel == "n.a" ] <- 0
    EUfuel[ , 4:ncol( EUfuel ) ] <- lapply( EUfuel[ , 4:ncol( EUfuel ) ], as.numeric )
    EUemiss[ EUemiss == "n.a" ] <- 0
    EUemiss[ , 4:ncol( EUemiss ) ] <- lapply( EUemiss[ , 4:ncol( EUemiss ) ], as.numeric )

# Melt the data to long form
    EUfuel <- melt( EUfuel, id.vars = c( "cou_abb", "reg_abb", "sector.activity" ) ) ### use gather
    EUemiss <- melt( EUemiss, id.vars = c( "cou_abb", "reg_abb", "Sector.Activity" ) )

# Change column names along with puting sectors and fuels into CEDS names
# Note 0_Temp-Aggregated is a temporary/aggregated sector, here used to aggregate DOM
# and IN_*. So need to copy the values of 0_Temp-Aggregated back to those sectors
    disagg_sectors <- c( "1A2a_Ind-Comb-Iron-steel",
                         "1A2b_Ind-Comb-Non-ferrous-metals",
                         "1A2c_Ind-Comb-Chemicals",
                         "1A2d_Ind-Comb-Pulp-paper",
                         "1A2e_Ind-Comb-Food-tobacco",
                         "1A2f_Ind-Comb-Non-metalic-minerals",
                         "1A2g_Ind-Comb-Construction",
                         "1A2g_Ind-Comb-machinery",
                         "1A2g_Ind-Comb-mining-quarying",
                         "1A2g_Ind-Comb-other",
                         "1A2g_Ind-Comb-textile-leather",
                         "1A2g_Ind-Comb-transpequip",
                         "1A2g_Ind-Comb-wood-products",
                         "1A4a_Commercial-institutional",
                         "1A4b_Residential",
                         "1A4c_Agriculture-forestry-fishing",
                         "1A5_Other-unspecified" )

# EUfuel processing:
# Rename columns
    colnames( EUfuel ) <- c( "iso", "reg_abb", "sector", "fuel", "Energy" )

# Change from GAINS fuels to CEDS fuels
    EUfuel$fuel <- fuelmap[ match( EUfuel$fuel, fuelmap$GAINS.fuel ), 1 ]

# Change from GAINS sectors to CEDS sectors
    EUfuel <- mapCEDS_sector_fuel( mapping_data = EUfuel,
                                   mapping_file = sectormap,
                                   data_match_col = 'sector',
                                   map_match_col = 'GAINS.sectors',
                                   map_merge_col = c( 'detailed_sectors' ),
                                   new_col_names = c( 'sector' ),
                                   level_map_in = 'detailed_sectors',
                                   level_out = 'working_sectors_v1',
                                   aggregate = TRUE,
                                   aggregate_col = c( 'Energy' ),
                                   oneToOne = FALSE,
                                   agg.fun = sum )

# Disaggregate 0_Temp-Aggregated, a placeholder for reading in GAINS
    EUfuel_disagg <- filter( EUfuel, sector == "0_Temp-Aggregated" ) %>%
                      repeatAndAddVector( "sector", disagg_sectors )

# Add disaggregated rows back into the main dataframe and remove 0_Temp-Aggregated
    EUfuel <- filter( EUfuel, sector != "0_Temp-Aggregated" ) %>%
                                   bind_rows( EUfuel_disagg ) %>%
                                 dplyr::arrange( iso, sector, fuel )

# EUemiss processing:
# Rename columns again
    colnames( EUemiss ) <- c( "iso", "reg_abb", "sector",
                              "fuel", "Sulfur_emiss" )

# Convert from GAINS fuels to CEDS fuels
    EUemiss$fuel <- fuelmap[ match( EUemiss$fuel, fuelmap$GAINS.fuel ), 1 ]

# Convert from GAINS sectors to CEDS sectors
    EUemiss <- mapCEDS_sector_fuel( mapping_data = EUemiss,
                                    mapping_file = sectormap,
                                    data_match_col = 'sector',
                                    map_match_col = 'GAINS.sectors',
                                    map_merge_col = c( 'detailed_sectors' ),
                                    new_col_names = c( 'sector' ),
                                    level_map_in = 'detailed_sectors',
                                    level_out = 'working_sectors_v1',
                                    aggregate = TRUE,
                                    aggregate_col = c( 'Sulfur_emiss' ),
                                    oneToOne = FALSE,
                                    agg.fun = sum )
# Disaggregate 0_Temp-Aggregated, a placeholder for reading in GAINS
    EUemiss_disagg <- filter( EUemiss, sector == "0_Temp-Aggregated" ) %>%
                        repeatAndAddVector( "sector", disagg_sectors )

# Add disaggregated rows back into the main dataframe and remove 0_Temp-Aggregated
    EUemiss <- filter( EUemiss, sector != "0_Temp-Aggregated" ) %>%
                                    bind_rows( EUemiss_disagg ) %>%
                                   dplyr::arrange( iso, sector, fuel )

# Convert to isocode
    EUemiss$iso <- tolower( gainstoiso$ISO.code[ match( EUemiss$iso,
                                                        gainstoiso$country ) ] )
    EUfuel$iso <- tolower( gainstoiso$ISO.code[ match( EUfuel$iso,
                                                       gainstoiso$country ) ] )

# ---------------------------------------------------------------------------
# 4. Calculate emissions factors
#    First, convert fuels to mass values from energy values given heat content.
#    Then calculate emissions factors by dividing emissions by fuel burned


# Aggregate by iso x sector x fuel
    EUfuel <- aggregate( EUfuel[ c( "Energy" ) ],
                         by = EUfuel[ c( "iso", "sector", "fuel" ) ],
                         FUN = sum )
    EUemiss <- aggregate( EUemiss[ c( "Sulfur_emiss" ) ],
                          by = EUemiss[ c( "iso", "sector", "fuel" ) ],
                          FUN = sum )

# Combine fuel and emissions into a single df
    EmissFactors <- merge( EUfuel, EUemiss, all.x = T, all.y = F )

# Convert PetaJoules into kiloJoules    ### Add a units column earlier than this to track units
    EmissFactors[, "Energy" ] <- EmissFactors[, "Energy" ] * 10^12

# Convert GAINS energy to kg by dividing by GAINS_heat_content
# then multiply by 10^-6 to convert to kt   ### Could do this as a function, or better yet just merge and divide;
                                            ### either way it would improve the aesthetic of the code greatly. If you're
                                            ### doing it this way you may as well have a for loop.
    EmissFactors[ EmissFactors$fuel %in% "brown_coal", "Energy" ] <-
      ( EmissFactors[ EmissFactors$fuel %in% "brown_coal", "Energy" ] /
         GAINS_heat_content[ which( GAINS_heat_content$fuel == 'brown_coal' ),
                             'heat_content' ] ) * 10^-6
    EmissFactors[ EmissFactors$fuel %in% "hard_coal", "Energy" ] <-
      ( EmissFactors[ EmissFactors$fuel %in% "hard_coal", "Energy" ] /
         GAINS_heat_content[ which( GAINS_heat_content$fuel == 'hard_coal' ),
                             'heat_content' ] ) * 10^-6
    EmissFactors[ EmissFactors$fuel %in% "biomass", "Energy" ] <-
      ( EmissFactors[ EmissFactors$fuel %in% "biomass", "Energy" ] /
         GAINS_heat_content[ which( GAINS_heat_content$fuel == 'biomass' ),
                             'heat_content' ] ) * 10^-6
    EmissFactors[ EmissFactors$fuel %in% "light_oil", "Energy" ] <-
      ( EmissFactors[ EmissFactors$fuel %in% "light_oil", "Energy" ] /
         GAINS_heat_content[ which( GAINS_heat_content$fuel == 'light_oil' ),
                             'heat_content' ] ) * 10^-6
    EmissFactors[ EmissFactors$fuel %in% "natural_gas", "Energy" ] <-
      ( EmissFactors[ EmissFactors$fuel %in% "natural_gas", "Energy" ] /
         GAINS_heat_content[ which( GAINS_heat_content$fuel == 'natural_gas' ),
                             'heat_content' ] ) * 10^-6
    EmissFactors[ EmissFactors$fuel %in% "natural_gas", "Energy" ] <-
      ( EmissFactors[ EmissFactors$fuel %in% "natural_gas", "Energy" ] /
         GAINS_heat_content[ which( GAINS_heat_content$fuel == 'natural_gas' ),
                             'heat_content' ] ) * 10^-6
    EmissFactors[ EmissFactors$fuel %in% "diesel_oil", "Energy" ] <-
      ( EmissFactors[ EmissFactors$fuel %in% "diesel_oil", "Energy" ] /
         GAINS_heat_content[ which( GAINS_heat_content$fuel == 'diesel_oil' ),
                             'heat_content' ] ) * 10^-6
    EmissFactors[ EmissFactors$fuel %in% "heavy_oil", "Energy" ] <-
      ( EmissFactors[ EmissFactors$fuel %in% "heavy_oil", "Energy" ] /
         GAINS_heat_content[ which( GAINS_heat_content$fuel == 'heavy_oil' ),
                             'heat_content' ] ) * 10^-6


# The following sectors are not important to this information set, therefore remove
    out <- c( "Heat", "Electricity", "Hydro",
              "Hydrogen", "Nuclear", "Renewables", "" )
    EmissFactors <- EmissFactors[ !EmissFactors$fuel %in% out, ] ### use %!in%

# Add units--kt emissions per kt fuel
    EmissFactors$units <- c( "kt/kt" )

# Calculate emissions factors by dividing emissions by fuel burned
# (now in mass units)
    EmissFactors$EF_2005 <- EmissFactors$Sulfur_emiss / EmissFactors$Energy

# Drop unneeded columns
    EmissFactors <- EmissFactors[ , c( "iso", "sector", "fuel", "units", "EF_2005" ) ]

# Retain only complete cases
    EmissFactors <- EmissFactors[ complete.cases( EmissFactors ), ]

# Drop values of infinity
    EmissFactors <- EmissFactors[ -which( EmissFactors$EF_2005 == 'Inf' ), ]

# Rename columns ### this seems very unnecessary given that we just used these column names to order the rows
    names( EmissFactors ) <- c( "iso", "sector", "fuel", "units", "EF_2005" )

# Drop EFs of 0
    EmissFactors <- EmissFactors[ which( EmissFactors$EF_2005 > 0 ), ]

# Drop very large EFs
    EmissFactors <- EmissFactors[ which( EmissFactors$EF_2005 < 10^4 ), ]

# -------------------------------------------------------------------------------
# 5.Calculate GAINS control percentage using GAINS emission factors

    Gains_EF  <- EmissFactors

    names( Gains_EF ) <- c( 'iso', 'sector', 'fuel', 'units', 'X2005' )
    countries <- unique( Gains_EF$iso )

# Calculate Default emissions factors (pre control) with base EF and ash retention
    base <- s_content_EU[ , c( 'iso', 'sector', 'fuel', 'X2005' ) ]

### Why are we just multiplying by 2?
    base$X2005 <- base$X2005 * 2
    names( base ) <- c( 'iso', 'sector', 'fuel', 'X2005base' )

# merge ash retention with S content
    default <- merge( gains_ashret_EU, base, all.x = TRUE, all.y = FALSE )

# Calculate default EFs from S content and ash retention
    default$default_EF <- default$X2005base * ( 1 - default$X2005 )

# Drop columns that are NA (had NA for either calculation variable)
    default <- default[ !is.na( default$default_EF ), ]
    default$units <- 'kt/kt'

# Drop calculation columns
    default <- default[ , c( 'iso', "sector", 'fuel', 'units', 'default_EF' ) ]

# Rename to Xyear form
    colnames( default )[ which( names( default ) == "default_EF" ) ] <- 'X2005'

# Melt default to long form
    default.long <- melt( default,
                          c( 'iso', 'sector', 'fuel', 'units' ),
                          val = 'X2005' )
    names( default.long ) <- c( 'iso', 'sector', 'fuel',
                                'units', 'years', 'default' )

# melt GAINS EFs (from block 3) to long form
    gains.long <- melt( Gains_EF, c( 'iso', 'sector', 'fuel', 'units' ) )
    names( gains.long ) <- c( 'iso', 'sector', 'fuel',
                              'units', 'years', 'gains' )

# Combine EFs and default calculated values into a single df ### These comments could be improved by someone who understands the control pct calculation
    combined.all <- merge( gains.long, default.long,
                           by = c( 'iso', 'sector', 'fuel', 'units', 'years' ),
                           all.x = TRUE, all.y=TRUE )

# Calculate Control Percent
    combined.all$control_percent <- 1 - combined.all$gains / combined.all$default

# Drop 0 control pct rows
    combined <- combined.all[ which( combined.all$control_percent > 0 ), ]

# Drop control pct values over 1
    combined <- combined[ which( combined$control_percent <= 1 ), ]
    combined$units <- 'percent'

# Cast to wide
    control_percent <- cast( combined[ , c( 'iso', 'sector', 'fuel', 'units',
                                            'years', 'control_percent' ) ],
                            iso + sector + fuel + units ~ years,
                            value = 'control_percent' )

# -------------------------------------------------------------------------------
# 6. Prepare for automated addition to ContFrac_db in other mod B scripts.
#    Define extension options.

# Split eastern and western europe
    west <- MCL[ which( MCL$IEA_Fert_reg == 'Western Europe' ), 'iso' ]

# control_percent
    control_percent$pre_ext_method <- 'linear_0'
    control_percent$pre_ext_year <- '1990'
    control_percent[ control_percent$iso %in% west, 'pre_ext_year' ] <- '1980'

# Override above default with values in pre_ext_year_map
    if ( exists( "pre_ext_year_map" ) & nrow( pre_ext_year_map ) > 0 ) {

        names( pre_ext_year_map )[ names( pre_ext_year_map ) == "pre_ext_year" ] <-
                    "pre_ext_year_override"

    # Combine control_percent with pre_ext_year
        control_percent <- merge( control_percent, pre_ext_year_map, all.x = T )

    # Isolate rows with values for pre_ext-year
        override <- !is.na( control_percent$pre_ext_year_override )

    # Incorporate and drop old column
        control_percent$pre_ext_year[ override ] <- control_percent$pre_ext_year_override[ override ]
        control_percent$pre_ext_year_override <- NULL
    }

# -------------------------------------------------------------------------------
# 7. Write output

    writeData( control_percent, domain = "DEFAULT_EF_PARAM",
               fn = "B.SO2_GAINS_control_percent")

    logStop()

# END
