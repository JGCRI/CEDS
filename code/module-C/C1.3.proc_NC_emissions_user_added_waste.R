#------------------------------------------------------------------------------
# Program Name: C1.3.proc_NC_emissions_user_added_waste.R
# Author: Linh Vu, Steve Smith, Patrick O'Rourke
# Date Last Updated: 19 August, 2019
# Program Purpose:  Write out waste incineration emissions as default NC emissions. Outputs
#                   of this program will be read in by C1.3.proc_NC_emissions_user_added.R
#                   and overwrite existing Edgar emissions
# Input Files: Global_Emissions_of_Pollutants_from_Open_Burning_of_Domestic_Waste.xlsx,
#              Master_Country_List.csv, PM25_Inventory.xlsx
# Output Files: C.[em]_NC_default_waste_emissions.csv, C.[em]_NC_waste_emissions_rescaled.csv,
#               C.[em]_NC_waste_emissions_inventory_trend_user_added.csv
# Notes:
# TODO: Small iso issue fix
#       Make flexible to work with years other than 2010?
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R", "process_db_functions.R" ) # Any additional function files required
log_msg <- "Write out waste incineration emissions"
script_name <- "C1.3.proc_NC_emissions_user_added_waste.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "NMVOC"

# ------------------------------------------------------------------------------
# 0.5 Initialize constants and prepare equations for to replicate Wiedinmyer proceedure

# The following are the general equations (taken from Wiedinmyer et al.)
# that describe the calculations performed in this document
# Emissions[i] = (Mass of waste burned) * EF[i]

# (Waste burned) = Population * (Pct of pop that burns waste) *
#                  (Mass of annual per-capita waste) * (fraction of waste to burn that is combusted)

# Residential waste burning:
#     For developed (WB says high-income) countries:
#         Wb = (Mass of annual per-capita waste) * (Rural pop.) * (% waste uncollected) *
#              (fraction of waste to burn that is combusted)
#     For developing countries:
#         Wb = [(Mass of annual per-capita waste) * (Rural pop.) +
#               (Mass of annual per-capita waste) * (Urban pop.) * (% waste uncollected)]
#             * (fraction of waste to burn that is combusted)

# Collected waste burned in open dumps:
#    Wb_dump = (Mass of annual per-capita waste) * (Urban pop.) * (% waste collected) *
#              (fraction of waste to burn that is combusted)


# ------------------------------------------------------------------------------
# 1. Read in & label Wiedinmyer et al. population, waste, and EF data

    scenario <- "central"

    MCL <- readData( "MAPPINGS", "Master_Country_List" )

    Europe_PM25_Compare <- readData( "EM_INV", "PM25_Inventory",
                                     ".xlsx" , sheet_selection = "PM25_compare")

    waste_burning_params <- readData( "ACTIVITY_IN",
                                      "Waste_burning_pct_parameters")

# Unit conversion factors
    OC_conv <- 1/1.3  # weight to units of carbon
    NOx_conv <- ( 14 + 16*2 )/( 14 + 16 )  # NO to NO2

    all_waste_data <- readData( domain = "EM_INV",
                                file_name = "Global_Emissions_of_Pollutants_from_Open_Burning_of_Domestic_Waste",
                                extension = ".xlsx",
                                sheet_selection = "Table S1",
                                skip = 2 )[ , c( 1:3, 5:6, 8, 10 ) ]

    waste_column_names <- colnames(readData( domain = "EM_INV",
                                file_name = "Global_Emissions_of_Pollutants_from_Open_Burning_of_Domestic_Waste",
                                extension = ".xlsx",
                                sheet_selection = "Table S3",
                                skip = 1
                                ) )

# Emissions factors, from Wiedinmyer et al. Table 1
    emissions_factors <- readData( domain = "EM_INV", file_name = "Wiedinmyer_domestic_waste_EF" )
    colnames( emissions_factors ) [ 2 ] <- "EF"

    colnames( all_waste_data ) <- c( "Country", "income_level", "pop2010",
                                     "Urban_pop", "Waste_gen_rate", "Collection-efficiency",
                                     "Frac_not_collected" )
# Inventory years
  X_waste_inventory_years <- paste0( 'X', 1980 : 2010 )

# ------------------------------------------------------------------------------
# 2. Calculate the amount of waste burned
#    Using the equations outlined in 0.5, this section of code processes
#    developing & developed data to determine the total volume of waste burned
#    based on population, urban/rural proportion, and per capita waste gen.

# Waste produced = 2010 pop * waste generation rate
    all_waste_data$total_waste <- all_waste_data$pop2010 * all_waste_data$Waste_gen_rate

# Calculate rural populations
    all_waste_data$Urban_pop[ is.na( all_waste_data$Urban_pop ) ] <- 0
    all_waste_data$Rural_pop <- all_waste_data$pop2010 - all_waste_data$Urban_pop

# Create variables holding indices for developing and developed countries
    developing <- which( all_waste_data$income_level != 'HIC' )
    developed <- which( all_waste_data$income_level == 'HIC' )

# Initialize residential mass burned
    all_waste_data$WB_res <- NA

# This uses a simple method to extend waste emisisons over time but instead of the flat 0.6
# burned fraction uses value input from the file Waste_burning_pct_parameters.csv.

# Residential mass burned in developing countries: whole rural pop, plus
# whatever fraction of urban pop is not collected (assumes 100% of rural is uncollected)
    all_waste_data$WB_res_rural[ developing ] <-
              ( ( all_waste_data$Waste_gen_rate *
                  all_waste_data$Rural_pop *
                  waste_burning_params[ which(waste_burning_params$collected_or_not == "uncollected" &
                                              waste_burning_params$income_level == "developing" &
                                              waste_burning_params$rural_or_urban == "rural" &
                                              waste_burning_params$scenario == scenario), ]$fraction_burnt
                      ) )[ developing ]

    all_waste_data$WB_res_urban[ developing ] <-
              ( ( all_waste_data$Waste_gen_rate *
                  all_waste_data$Urban_pop *
                  all_waste_data$Frac_not_collected *
                  waste_burning_params[ which(waste_burning_params$collected_or_not == "uncollected" &
                                              waste_burning_params$income_level == "developing" &
                                              waste_burning_params$rural_or_urban == "urban" &
                                              waste_burning_params$scenario == scenario), ]$fraction_burnt
                      ) )[ developing ]


# Residential waste burning in developed countries
    all_waste_data$WB_res_rural[ developed ] <-
          ( ( all_waste_data$Waste_gen_rate *
              all_waste_data$Rural_pop *
              all_waste_data$Frac_not_collected *
              waste_burning_params[ which(waste_burning_params$collected_or_not == "uncollected" &
                                          waste_burning_params$income_level == "developed" &
                                          waste_burning_params$rural_or_urban == "rural" &
                                          waste_burning_params$scenario == scenario), ]$fraction_burnt
              ) )[ developed ]


    all_waste_data$WB_res_urban[ developed ] <-
          ( ( all_waste_data$Waste_gen_rate *
              all_waste_data$Rural_pop *
              (1 - all_waste_data$Frac_not_collected) *
              waste_burning_params[ which(waste_burning_params$collected_or_not == "uncollected" &
                                          waste_burning_params$income_level == "developed" &
                                          waste_burning_params$rural_or_urban == "urban" &
                                          waste_burning_params$scenario == scenario), ]$fraction_burnt
          ) )[ developed ]

# Sum rural and urban uncollected waste production
    all_waste_data$WB_res <-
              all_waste_data$WB_res_rural +
              all_waste_data$WB_res_urban


# Initialize open dump burning at 0; high-income countries are assumed to have
# no waste burning of this type
    all_waste_data$WB_dump <- 0

# Developed waste burning: waste from urban centers that do get pick-up
# These calculations are producing NAs?
# TODO: confirm that changes made here are correct
    all_waste_data$WB_dump_rural[ developing ] <- ( all_waste_data$Waste_gen_rate * all_waste_data$Rural_pop *
                                                  ( all_waste_data$`Collection-efficiency` ) *
                                                    waste_burning_params[ which(waste_burning_params$collected_or_not == "collected" &
                                                        waste_burning_params$income_level == "developing" &
                                                        waste_burning_params$rural_or_urban == "rural" &
                                                        waste_burning_params$scenario == scenario), ]$fraction_burnt )[ developing ]

    all_waste_data$WB_dump_urban[ developing ] <- ( all_waste_data$Waste_gen_rate *
                                                    all_waste_data$Urban_pop *
                                                    ( all_waste_data$`Collection-efficiency` ) *
                                                    waste_burning_params[ which(waste_burning_params$collected_or_not == "collected" &
                                                        waste_burning_params$income_level == "developing" &
                                                        waste_burning_params$rural_or_urban == "urban" &
                                                        waste_burning_params$scenario == scenario), ]$fraction_burnt )[ developing ]

    all_waste_data$WB_dump_rural[ developed ] <- ( all_waste_data$Waste_gen_rate *
                                                  all_waste_data$Rural_pop *
                                                  ( all_waste_data$`Collection-efficiency` ) *
                                                  waste_burning_params[ which(waste_burning_params$collected_or_not == "collected" &
                                                     waste_burning_params$income_level == "developed" &
                                                     waste_burning_params$rural_or_urban == "rural" &
                                                     waste_burning_params$scenario == scenario), ]$fraction_burnt )[ developed ]

    all_waste_data$WB_dump_urban[ developed ] <- ( all_waste_data$Waste_gen_rate *
                                                  all_waste_data$Urban_pop *
                                                  ( all_waste_data$`Collection-efficiency` ) *
                                                  waste_burning_params[ which(waste_burning_params$collected_or_not == "collected" &
                                                    waste_burning_params$income_level == "developed" &
                                                    waste_burning_params$rural_or_urban == "urban" &
                                                    waste_burning_params$scenario == scenario), ]$fraction_burnt )[ developed ]

    all_waste_data$WB_dump <- all_waste_data$WB_dump_rural +
                              all_waste_data$WB_dump_urban

    all_waste_data$total_WB <- all_waste_data$WB_dump + all_waste_data$WB_res
    all_waste_data$pct_burned <- all_waste_data$total_WB / all_waste_data$total_waste

# ------------------------------------------------------------------------------
# 3. Calculate emissions from emissions factors

    emissions_factors$EF <- as.numeric( emissions_factors$EF ) / 10^6 # Units are in kg/tonne; for comparison,
                                                                      # we want Gg/tonne

    total_burning_em <- data.frame( all_waste_data$total_WB %o% emissions_factors$EF )

    colnames( total_burning_em ) <- emissions_factors$compound
    total_burning_em$Country <- all_waste_data$Country

    total_burning_em <- subset( total_burning_em, select = c( Country, 1:25 ) )

    waste_input <- total_burning_em
    colnames(waste_input) <- waste_column_names

# ------------------------------------------------------------------------------
# 4. Process and convert to standard CEDS format
# Add ISO code
    waste_input$iso <- MCL$iso[ match( waste_input$`Country Name`, MCL$Country_Name ) ]
    waste_input$iso[ waste_input$`Country Name` == "Cote d'Ivoire" ] <- "civ"
    waste_input$iso[ waste_input$`Country Name` == "Democratic People's Republic of Korea (North Korea)" ] <- "prk"
    waste_input$iso[ waste_input$`Country Name` == "Democratic Republic of the Congo" ] <- "cod"
    waste_input$iso[ waste_input$`Country Name` == "Falkland Islands (Malvinas)" ] <- "flk"
    waste_input$iso[ waste_input$`Country Name` == "Faroe Islands" ] <- "fro"
    waste_input$iso[ waste_input$`Country Name` == "Iran (Islamic Republic of)" ] <- "irn"
    waste_input$iso[ waste_input$`Country Name` == "Korea, South" ] <- "kor"
    waste_input$iso[ waste_input$`Country Name` == "Lao People's Democratic Republic" ] <- "lao"
    waste_input$iso[ waste_input$`Country Name` == "Libyan Arab Jamahiriya" ] <- "lby"
    waste_input$iso[ waste_input$`Country Name` == "Marshall Islands" ] <- "mhl"
    waste_input$iso[ waste_input$`Country Name` == "Micronesia (Federated States of)" ] <- "fsm"
    waste_input$iso[ waste_input$`Country Name` == "Myanmar" ] <- "mmr"
    waste_input$iso[ waste_input$`Country Name` == "Russian Federation" ] <- "rus"
    waste_input$iso[ waste_input$`Country Name` == "Saint Vincent and the Grenadines" ] <- "vct"
    waste_input$iso[ waste_input$`Country Name` == "Syrian Arab Republic" ] <- "syr"
    waste_input$iso[ waste_input$`Country Name` == "The former Yugoslav Republic of Macedonia" ] <- "mkd"
    waste_input$iso[ waste_input$`Country Name` == "United Kingdom of Great Britain and Northern Ireland" ] <- "gbr"
    waste_input$iso[ waste_input$`Country Name` == "United Republic of Tanzania" ] <- "tza"
    waste_input$iso[ waste_input$`Country Name` == "United States of America" ] <- "usa"
    waste_input$iso[ waste_input$`Country Name` == "Venezuela, Bolivarian Republic of" ] <- "ven"
    waste_input$iso[ waste_input$`Country Name` == "Wallis and Futuna" ] <- "wlf"
    waste_input <- filter( waste_input, !is.na( iso ) ) %>%  # drop countries not in CEDS
      dplyr::arrange( iso )

# TODO: add these countries in. We have population values in CEDS, and could just use per-capita values from a comparable country.
    drop_countries <- c( "Cyprus", "Luxembourg", "Malta", "Singapore", "Slovakia" )
    waste_input <- filter( waste_input, `Country Name` %!in% drop_countries ) %>%  # drop 5 countries with no vals (to match pre-Wiedinmyer)
      dplyr::arrange( iso )

# Keep relevant emissions (TODO)
    waste_input <- select( waste_input, iso, `Sulfur Dioxide (SO2)`, `Nitrogen Oxides (NOx as NO)`,
                           `Carbon Monoxide (CO)`, `NMOC (identified)`, BC, OC,
                           `Ammonia (NH3)`, `Methane (CH4)`, `Carbon Dioxide (CO2)` )
    em_names <- c( "SO2", "NOx", "CO", "NMVOC", "BC", "OC", "NH3", "CH4", "CO2" )
    names( waste_input ) <- c( "iso", em_names )

# Unit conversion
# Note OC reported here is total OC and NOx is NO, so need to convert OC to units
# of carbon and NO to NO2 for CEDS
    waste_input$OC <- waste_input$OC * OC_conv
    waste_input$NOx <- waste_input$NOx * NOx_conv

# Convert to standard CEDS input format
    waste_input <- melt( waste_input, id = "iso" )
    names( waste_input )[ names( waste_input ) %in% c( "variable", "value" ) ] <- c( "emission", "X2010" )
    waste_input$sector <- "5C_Waste-incineration"
    waste_input$fuel <- "process"
    waste_input$units <- "kt"

# Filter out emission being processed
    waste_inventory <- filter( waste_input, emission == em ) %>% select( iso, sector, fuel, units, X2010 )

# ------------------------------------------------------------------------------
# 5. Compare selected inventory PM2.5 values to default calcuation and either match with
#    inventory or replace with a European regional average if the inventory value is several
#    orders of magnitude lower that default calcuation

#   Calculate the ratio between the selected em and PM2.5 from emissions_factors data frame
    em_filter_for <- em
    if( em == "NMVOC" ){ em_filter_for <- "NMOC (identified + unidentified)" }
    if( em == "NOx" ){ em_filter_for <- "Nox" }

    emissions_factor_em <- emissions_factors %>%
        dplyr::filter( compound == em_filter_for )

    emissions_factor_PM2.5 <-  emissions_factors %>%
        dplyr::filter( compound == "PM2.5" )

    emissions_factor_ratio <- emissions_factor_PM2.5[[2]] / emissions_factor_em[[2]]

#   Apply ratio between selected em and PM2.5 emissions factors to the waste inventory, in order to convert waste emissions to PM2.5
    waste_inventory_converted_to_PM25 <- waste_inventory %>%
        dplyr::mutate( X2010_waste_inventory = X2010 * emissions_factor_ratio ) %>%
        dplyr::select( -X2010 )

#   Calculate ratio between inventory and wiedinmyer
    Europe_PM25_Compare_with_ratio <- Europe_PM25_Compare %>%
        dplyr::left_join( waste_inventory_converted_to_PM25, by = c( "iso", "units" ) ) %>%
        dplyr::mutate( X2010_inv_to_waste_inventory = X2010_inventory / X2010_waste_inventory ) %>%
        dplyr::mutate( ratio_summary = if_else( X2010_inv_to_waste_inventory > 1, "> 1",
                                       if_else( X2010_inv_to_waste_inventory <= 1 & X2010_inv_to_waste_inventory >= 0.1, "[ 0.1, 1 ]",
                                       if_else( X2010_inv_to_waste_inventory < 0.1 & X2010_inv_to_waste_inventory >= 0.01, "[ 0.01, 0.1 )",
                                       if_else( X2010_inv_to_waste_inventory < 0.01 & X2010_inv_to_waste_inventory >= 0.001, "[ 0.001, 0.01 )",
                                       if_else( X2010_inv_to_waste_inventory < 0.001 & X2010_inv_to_waste_inventory > 0,  "< 0.001", NA_character_ ) ) ) ) ) )

#   Make mean ratio calculations
#       All inventires with a value
        mean_ratio_all_inv_with_value <- Europe_PM25_Compare_with_ratio %>%
            dplyr::filter( X2010_inventory != 0 & !( is.na( X2010_inventory ) ) ) %>%
            dplyr::select( X2010_inv_to_waste_inventory ) %>%
            dplyr::summarise_all( funs( mean( . ) )  )

#       All inventires with values within 0.1% ( ratio > or = 0.001 )
        mean_ratio_inv_0.1percent <- Europe_PM25_Compare_with_ratio %>%
            dplyr::filter( X2010_inv_to_waste_inventory >= 0.001 ) %>%
            dplyr::select( X2010_inv_to_waste_inventory ) %>%
            dplyr::summarise_all( funs( mean( . ) )  )

#       All inventires judged "reasonable"
        mean_ratio_reasonable <- Europe_PM25_Compare_with_ratio %>%
            dplyr::filter( Use_for_median == 1 ) %>%
            dplyr::select( X2010_inv_to_waste_inventory ) %>%
            dplyr::summarise_all( funs( mean( . ) )  )

#       All inventires judged "reasonable" and don't have a ratio to wideinmyer that is >1
        mean_ratio_reasonable_no_greater_than_1 <- Europe_PM25_Compare_with_ratio %>%
            dplyr::filter( Use_for_median == 1 &  ratio_summary != "> 1"   ) %>%
            dplyr::select( X2010_inv_to_waste_inventory ) %>%
            dplyr::summarise_all( funs( mean( . ) )  )

#       Assign default oecd ratio to use
        oecd_default_ratio <- mean_ratio_reasonable_no_greater_than_1[[1]]

#   Set X2010 values to the value in X2010_inventory ( X2010_inventory = closest year available for the iso, if X2010 was NA)
    Europe_PM25_replaced_NAs <- Europe_PM25_Compare_with_ratio %>%
        dplyr::mutate( X2010 = if_else( is.na( X2010 ), X2010_inventory, X2010 ) )

#   Join the waste inventory data with waste_inventory data
    waste_inventory_match <- waste_inventory %>%
        dplyr::left_join( dplyr::select( Europe_PM25_replaced_NAs, iso, X2010_inv_to_waste_inventory, Use_for_median, Use_for_trend ), by = "iso" ) %>%
        dplyr::mutate_at( .vars = c( "Use_for_median", "Use_for_trend" ), funs( if_else( is.na( . ), 0, . ) ) )

#   For European regions that don't appear to have reasonable data, change 2010 value to default average value
#       Define european isos
        europe_isos <- MCL[grep('Euro',MCL$Region),'iso'] %>%
                    unique( )

        waste_inventory_match <- waste_inventory_match %>%
            dplyr::mutate( X2010_inv_to_waste_inventory = if_else( iso %in% europe_isos & Use_for_median == 0, oecd_default_ratio, X2010_inv_to_waste_inventory ) )

# Split waste_inventory_match into 3 categories for different treatment

#   1) For most values use the default inventory estimate and default extension over time
    default_waste_only <- waste_inventory_match %>%
        dplyr::filter( Use_for_median == 0 & iso %!in% europe_isos  ) %>%
        dplyr::select( iso, sector, fuel, units, X2010 )

#   2) For isos that had "reasonable" inventory emission values but no or limited trend data:
#   Re-scale by inv_to_waste_inventory ratio so that waste emissions match inventory value in 2010,
#   while also using the default trend over time ( reports out only 2010 data, CEDS will extend this later automatically with default trend)
    waste_inventory_no_trend <-  waste_inventory_match %>%
        dplyr::filter( ( Use_for_median == 1 | iso %in% europe_isos ) & Use_for_trend == 0)  %>%
        dplyr::select( iso, sector, fuel, units, X2010, X2010_inv_to_waste_inventory ) %>%
        dplyr::mutate( X2010 = X2010 * X2010_inv_to_waste_inventory ) %>%
        dplyr::select( iso, sector, fuel, units, X2010 )

#   3) For isos that had "reasonable" inventory emission values and trend data, re-scale by
#   to match inventory and use inventory trends as much as available. Reports only to 2009 as this is the last year most inventories have data
    waste_inventory_trend <- waste_inventory_match %>%
        dplyr::filter( ( Use_for_median == 1 | iso %in% europe_isos ) & Use_for_trend == 1 ) %>%
        dplyr::mutate( X2010 = X2010 * X2010_inv_to_waste_inventory ) %>%
        dplyr::select( iso, sector, fuel, units, X2010 )

    waste_inventory_trend <- extend_data_on_trend_range( input_data = waste_inventory_trend,
                                                         driver_trend = Europe_PM25_Compare[c('iso', X_waste_inventory_years )],
                                                         start = 1980,
                                                         end = 2009,
                                                         id_match.driver = c('iso'),
                                                         range = 1 )

    X_waste_inventory_years_no_2010 <- subset( X_waste_inventory_years, X_waste_inventory_years != "X2010" )

    waste_inventory_trend <- waste_inventory_trend %>%
        dplyr::mutate_at( .vars = X_waste_inventory_years_no_2010, funs( if_else( . == 0, NA_real_, . ) )  )

# ------------------------------------------------------------------------------
# 6. Output
    writeData( default_waste_only, "DEFAULT_EF_IN", domain_extension = "non-combustion-emissions/",
               fn = paste0( "C.", em, "_NC_default_waste_emissions" ) )

    writeData( waste_inventory_no_trend, "DEFAULT_EF_IN", domain_extension = "non-combustion-emissions/",
               fn = paste0( "C.", em, "_NC_waste_emissions_rescaled" ) )

    writeData( waste_inventory_trend, "DEFAULT_EF_IN", domain_extension = "non-combustion-emissions/",
               fn = paste0( "C.", em, "_NC_waste_emissions_inventory_trend_user_added" ) )

logStop()
