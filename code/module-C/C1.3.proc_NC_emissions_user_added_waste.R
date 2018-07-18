#------------------------------------------------------------------------------
# Program Name: C1.3.proc_NC_emissions_user_added_waste.R
# Author: Linh Vu
# Date Last Updated: 6 June 2016
# Program Purpose:  Write out waste incineration emissions as default NC emissions. Outputs
#                   of this program will be read in by C1.3.proc_NC_emissions_user_added.R
#                   and overwrite existing Edgar emissions
# Input Files: Global_Emissions_of_Pollutants_from_Open_Burning_of_Domestic_Waste.xlsx,
#              Master_Country_List.csv
# Output Files:    C.[em]_NC_emissions_waste.csv
# Notes:
# TODO:
#
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
if ( is.na( em ) ) em <- "NH3"

# ------------------------------------------------------------------------------
# 0.5 Initialize constants and prepare equations for Wiedinmyer replication

    burned_fraction = 0.6 # Paper's estimate--we might adjust


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

    MCL <- readData( "MAPPINGS", "Master_Country_List" )
    wiedinmyer <- readData( "EM_INV", "PM25_Inventory_vs_Wiedinmyer",
                            ".xlsx", sheet_selection = "PM25_compare")

# Unit conversion factors
    OC_conv <- 1/1.3  # weight to units of carbon
    NOx_conv <- (14 + 16*2)/(14 + 16)  # NO to NO2

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

# ------------------------------------------------------------------------------
# 2. Wiedinmyer replication - calculate the amount of waste burned
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

# Residential mass burned in developing countries: whole rural pop, plus
# whatever fraction of urban pop is not collected
    all_waste_data$WB_res[ developing ] <- ( ( ( all_waste_data$Waste_gen_rate *
                                                all_waste_data$Rural_pop ) +
                                              ( all_waste_data$Waste_gen_rate *
                                                all_waste_data$Urban_pop *
                                                all_waste_data$Frac_not_collected
                                               ) ) * burned_fraction )[ developing ]

# Residential waste burning in developed countries: non-collected rural pop only
    all_waste_data$WB_res[ developed ] <- ( all_waste_data$Waste_gen_rate *
                                            all_waste_data$Rural_pop *
                                            all_waste_data$Frac_not_collected *
                                            burned_fraction )[ developed ]


# Initialize open dump burning at 0; high-income countries are assumed to have
# no waste burning of this type
    all_waste_data$WB_dump <- 0

# Developed waste burning: waste from urban centers that do get pick-up
    all_waste_data$WB_dump[ developing ] <- ( all_waste_data$Waste_gen_rate *
                                              all_waste_data$Urban_pop *
                                              ( all_waste_data$`Collection-efficiency` ) *
                                              burned_fraction )[ developing ]

    all_waste_data$total_WB <- all_waste_data$WB_dump + all_waste_data$WB_res
    all_waste_data$pct_burned <- all_waste_data$total_WB / all_waste_data$total_waste


# ------------------------------------------------------------------------------
# 3. Wiedinmyer replication - calculate emissions from emissions factors

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

### TODO: add these countries in. We have population values in CEDS, and could just use per-capita values from a comparable country.
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
# 5. Match to wiedinmyer PM2.5 inventory
    other_europe <- wiedinmyer[which( wiedinmyer$iso == 'other_europe_default'),'X2010_inv_to_wiedinmyer']
    europe_isos <- MCL[grep('Euro',MCL$Region),'iso']

    wiedinmyer[which(is.na(wiedinmyer$X2010)),'X2010'] <- wiedinmyer[which(is.na(wiedinmyer$X2010)),'X2010_inventory']

    # match waste inventory with wiedinmyer inventory data
    waste_inventory_wiedinmyer_match <- merge(waste_inventory, wiedinmyer[ c('iso','X2010_inv_to_wiedinmyer',"Use_for_median","Use_for_trend") ],
                                         all.x = T)
    waste_inventory_wiedinmyer_match[c("Use_for_median","Use_for_trend")] <- replace(waste_inventory_wiedinmyer_match[c("Use_for_median","Use_for_trend")],
                                                                                is.na(waste_inventory_wiedinmyer_match[c("Use_for_median","Use_for_trend")]),0)

    waste_inventory_wiedinmyer_match[which( waste_inventory_wiedinmyer_match$iso %in% europe_isos &
                                            waste_inventory_wiedinmyer_match$Use_for_median == 0 ), 'X2010_inv_to_wiedinmyer'] <- other_europe

    # seperate data frame into no wiedinmyer, wiedinmyer trend and wiedinmyer no trend
    waste_inventory_no_wiedinmyer <- filter(waste_inventory_wiedinmyer_match,
                                            Use_for_median == 0 & iso %!in% europe_isos ) %>% select( iso, sector, fuel, units, X2010 )

    waste_inventory_wiedinmyer_no_trend <- filter(waste_inventory_wiedinmyer_match,
                                             (Use_for_median == 1 | iso %in% europe_isos ) & Use_for_trend == 0) %>% select( iso, sector, fuel, units, X2010, X2010_inv_to_wiedinmyer )

    waste_inventory_wiedinmyer_trend <- filter(waste_inventory_wiedinmyer_match,
                                               (Use_for_median == 1 | iso %in% europe_isos ) & Use_for_trend == 1)

    # match to inventory  - wiedinmyer but no trend
    waste_inventory_wiedinmyer_no_trend$X2010 <-  waste_inventory_wiedinmyer_no_trend$X2010 * waste_inventory_wiedinmyer_no_trend$X2010_inv_to_wiedinmyer
    waste_inventory_wiedinmyer_no_trend <- waste_inventory_wiedinmyer_no_trend %>% select( iso, sector, fuel, units, X2010)

    # match to inventory  - wiedinmyer but no trend
    waste_inventory_wiedinmyer_trend$X2010 <-  waste_inventory_wiedinmyer_trend$X2010 * waste_inventory_wiedinmyer_trend$X2010_inv_to_wiedinmyer
    waste_inventory_wiedinmyer_trend <- waste_inventory_wiedinmyer_trend %>% select( iso, sector, fuel, units, X2010)

    waste_inventory_wiedinmyer_trend <- extend_data_on_trend_range( input_data = waste_inventory_wiedinmyer_trend,
                                                                     driver_trend= wiedinmyer[c('iso', paste0('X', 1980:2010))],
                                                                     start = 1980,
                                                                     end = 2009,
                                                                     id_match.driver = c('iso'),
                                                                     range = 1)
    waste_inventory_wiedinmyer_trend <- replace(waste_inventory_wiedinmyer_trend, waste_inventory_wiedinmyer_trend == 0, NA)
# ------------------------------------------------------------------------------
# 6. Output
    writeData( waste_inventory_no_wiedinmyer, "DEFAULT_EF_IN", domain_extension = "non-combustion-emissions/",
               fn = paste0( "C.", em, "_NC_waste_emissions_no_wiedinmyer_user_added" ) )

    writeData( waste_inventory_wiedinmyer_no_trend, "DEFAULT_EF_IN", domain_extension = "non-combustion-emissions/",
               fn = paste0( "C.", em, "_NC_waste_emissions_wiedinmyer_no_trend_user_added" ) )

    writeData( waste_inventory_wiedinmyer_trend, "DEFAULT_EF_IN", domain_extension = "non-combustion-emissions/",
               fn = paste0( "C.", em, "_NC_waste_emissions_wiedinmyer_trend_user_added" ) )

logStop()
