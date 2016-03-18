#------------------------------------------------------------------------------
# Program Name: C1.3.proc_NC_emissions_waste.R
# Author: Linh Vu
# Date Last Updated: 15 Mar 2016
# Program Purpose:  Write out waste emissions as default NC emissions. Outputs
#                   of this program will be read in by C1.3.proc_NC_emissions_user_added.R
#                   and overwrite existing Edgar emissions
# Input Files: Global_Emissions_of_Pollutants_from_Open_Burning_of_Domestic_Waste-suppl_(Wiedinmyer_etal_2015).xlsx,
#              Master_Country_List.csv 
# Output Files:    C.[em]_NC_emissions_waste.csv
# Notes:
# TODO: 
#   
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Set working directory to the CEDS "input" directory and define PARAM_DIR as the
# location of the CEDS "parameters" directory, relative to the new working directory.
dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
for ( i in 1:length( dirs ) ) {
  setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
  wd <- grep( 'CEDS/input', list.dirs(), value = T )
  if ( length( wd ) > 0 ) {
    setwd( wd[ 1 ] )
    break
  }
}
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files - 
# provides logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R", "process_db_functions.R" ) # Any additional function files required
log_msg <- "Write out waste emissions as default NC emissions"
script_name <- "C1.3.proc_NC_emissions_waste.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )


# ------------------------------------------------------------------------------
# 1. Read input
    waste_input <- readData( "EM_INV", "Global_Emissions_of_Pollutants_from_Open_Burning_of_Domestic_Waste-suppl_(Wiedinmyer_etal_2015)", 
                             ".xlsx", sheet_selection = "Table S3", skip_rows = 1 )[ 1: 226, ]
    MCL <- readData( "MAPPINGS", "Master_Country_List" )
    
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
                           `Carbon Monoxide (CO)`, `NMOC (identified + unidentified)`, BC, OC, 
                           `Ammonia (NH3)`, `Methane (CH4)` )
    em_names <- c( "SO2", "NOx", "CO", "NMVOC", "BC", "OC", "NH3", "CH4" )
    names( waste_input ) <- c( "iso", em_names )
    
# Convert to standard CEDS input format
    waste_input <- melt( waste_input, id = "iso" )
    names( waste_input )[ names( waste_input ) %in% c( "variable", "value" ) ] <- c( "emission", "X2000" )
    waste_input$sector <- "5C_Waste-incineration"
    waste_input$fuel <- "process"
    waste_input$units <- "kt"

# ------------------------------------------------------------------------------
# 3. Output
    for ( em in em_names ){
      df <- filter( waste_input, emission == em ) %>%
        select( iso, sector, fuel, units, X2000 )
      writeData( df, "DEFAULT_EF_IN", domain_extension = "non-combustion-emissions/", 
                 fn = paste0( "C.", em, "_NC_emissions_waste" ) )
    }
    
logStop()
