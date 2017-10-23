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
# 1. Read input
    waste_input <- readData( "EM_INV", "Global_Emissions_of_Pollutants_from_Open_Burning_of_Domestic_Waste",
                             ".xlsx", sheet_selection = "Table S3", skip_rows = 1 )[ 1: 226, ]
    MCL <- readData( "MAPPINGS", "Master_Country_List" )
    wiedinmyer <- readData( "EM_INV", "PM25_Inventory_vs_Wiedinmyer",
                            ".xlsx", sheet_selection = "PM25_compare")

# Unit conversion factors
    OC_conv <- 1/1.3  # weight to units of carbon
    NOx_conv <- (14 + 16*2)/(14 + 16)  # NO to NO2

# ------------------------------------------------------------------------------
# 2. Process and convert to standard CEDS format
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
      arrange( iso )

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
# 3. Match to wiedinmyer PM2.5 inventory
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
# 4. Output
    writeData( waste_inventory_no_wiedinmyer, "DEFAULT_EF_IN", domain_extension = "non-combustion-emissions/",
               fn = paste0( "C.", em, "_NC_waste_emissions_no_wiedinmyer_user_added" ) )

    writeData( waste_inventory_wiedinmyer_no_trend, "DEFAULT_EF_IN", domain_extension = "non-combustion-emissions/",
               fn = paste0( "C.", em, "_NC_waste_emissions_wiedinmyer_no_trend_user_added" ) )

    writeData( waste_inventory_wiedinmyer_trend, "DEFAULT_EF_IN", domain_extension = "non-combustion-emissions/",
               fn = paste0( "C.", em, "_NC_waste_emissions_wiedinmyer_trend_user_added" ) )

logStop()
