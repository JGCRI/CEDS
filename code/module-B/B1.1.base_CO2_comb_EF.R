# Program Name: B1.1.base_CO2_comb_EF.R
# Author: Linh Vu
# Date Last Updated: 25 Oct 2016 
# Program Purpose: Generate base emission factors for CO2
# Input Files: A.coal_heat_content.csv, CO2_base_EF_CDIAC.csv, A.comb_activity.csv
#     Master_Country_List.csv, IEA_product_fuel.csv
# Output Files:  B.[em]_comb_EF_db.csv
# Notes: 
# TODO: 
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers

# Before we can load headers we need some paths defined. They may be provided by
#   a system environment variable or may have already been set in the workspace.
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
# provide logging, file support, and system functions - and start the script log.
headers <- c( 'data_functions.R' ) 
#                 Additional function files may be required.
log_msg <- "Producing base CO2 combustion EF" 
script_name <- 'B1.1.base_CO2_comb_EF.R'

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "CO2"
em_lc <- tolower( em )   

# Stop script if running for unsupported species
if ( em %!in% c('CO2') ) {
  stop (paste( 'not supported for emission species', em, 'remove from script
               list in B1.2.add_comb_EF.R and/or makefile'))
}


# ---------------------------------------------------------------------------
# 1. Load Data
  coal_heat_content <- readData( "MED_OUT", "A.coal_heat_content" )
  default_ef <- readData( "DEFAULT_EF_IN", "CO2_base_EF", ".xlsx", sheet_selection = "main" )
  activity_data <- readData( "MED_OUT", "A.comb_activity" )
  
  MCL <- readData( "MAPPINGS", "Master_Country_List" )
  IEA_product_fuel <- readData( "MAPPINGS", "IEA_product_fuel", domain_extension = "energy/" )
  
  bunker_sectors <- c( "1A3aii_Domestic-aviation", "1A3ai_International-aviation", 
                       "1A3di_International-shipping", "1A3dii_Domestic-navigation" )
  
# ---------------------------------------------------------------------------
# 2. Unit conversions
# Convert coal heat content from kJ/kg to kJ/kt
  coal_heat_content[ coal_heat_content$units == "kJ/kg", X_emissions_years ] <- 
    coal_heat_content[ coal_heat_content$units == "kJ/kg", X_emissions_years ] * 10^6
  coal_heat_content$units[ coal_heat_content$units == "kJ/kg" ] <- "kJ/kt"
  
# Convert gas_fuels EF from kt/TJ to kt/kt
  default_ef$EF[ default_ef$cdiac_fuel %in% "gas_fuels" ] <-
    default_ef$EF[ default_ef$cdiac_fuel %in% "gas_fuels" ] * 
    conversionFactor_naturalgas_TJ_per_kt  # kt/TJ * TJ/kt = kt/kt
  default_ef$units[ default_ef$cdiac_fuel %in% "gas_fuels" ] <- "kt CO2/kt"
  
# Convert solid_fuels EF from kt/kJ to kt/kt, using country/type/year-specific coal heat content
  if ( any( coal_heat_content$units != "kJ/kt" ) | any( default_ef$units[ default_ef$cdiac_fuel %in% "solid_fuels" ] != "kt CO2/kJ" ) )
    stop( "Units mismatched. Check that coal heat content is kJ/kt and CDIAC solid fuels EF is kt/kJ." )
  solid_fuels_ef <- coal_heat_content
  solid_fuels_ef[, X_emissions_years ] <- solid_fuels_ef[, X_emissions_years ] * default_ef$EF[ default_ef$cdiac_fuel %in% "solid_fuels" ]
  solid_fuels_ef$units <- "kt CO2/kt"

# ---------------------------------------------------------------------------
# 3. Make EF df for all iso+sector+fuel
# Blank EF template
  ef_data <- activity_data
  ef_data[, X_emissions_years ] <- NA
  ef_data$cdiac_fuel <- IEA_product_fuel$cdiac_fuel[ match( ef_data$fuel, IEA_product_fuel$fuel ) ]
  
# Add solid_fuels EF
  ef_data[, X_emissions_years ] <- solid_fuels_ef[ match( 
    paste( ef_data$iso, ef_data$fuel ), paste( solid_fuels_ef$iso, solid_fuels_ef$fuel ) ),
    X_emissions_years ]
  
# Add liquid_fuels EF
  ef_data[ ef_data$cdiac_fuel == "liquid_fuels", X_emissions_years ] <- default_ef$EF[ default_ef$cdiac_fuel %in% "liquid_fuels" ]
  
# Add gas_fuels EF
  ef_data[ ef_data$cdiac_fuel == "gas_fuels", X_emissions_years ] <- default_ef$EF[ default_ef$cdiac_fuel %in% "gas_fuels" ]
  
# Add EF for specific fuel types: coal coke and petroleum fuels
  ef_data[ ef_data$fuel == "coal_coke", X_emissions_years ] <- default_ef$EF[ default_ef$ceds_fuel %in% "coal_coke" ]
  ef_data[ ef_data$fuel == "heavy_oil", X_emissions_years ] <- default_ef$EF[ default_ef$ceds_fuel %in% "heavy_oil" ]
  ef_data[ ef_data$fuel == "diesel_oil", X_emissions_years ] <- default_ef$EF[ default_ef$ceds_fuel %in% "diesel_oil" ]
  ef_data[ ef_data$fuel == "light_oil", X_emissions_years ] <- default_ef$EF[ default_ef$ceds_fuel %in% "light_oil" ]
  ef_data[ ef_data$fuel == "diesel_oil" & ef_data$sector %in% bunker_sectors, X_emissions_years ] <- 
    default_ef$EF[ default_ef$ceds_fuel %in% "diesel_oil (aviation)" ]
  
# Final processing
  ef_data[ is.na( ef_data ) ] <- 0
  ef_data$units <- "kt/kt"  # kt CO2/kt
  ef_data <- ef_data[, c( "iso", "sector", "fuel", "units", X_emissions_years ) ]
  ef_data <- arrange( ef_data, iso, sector, fuel )
  
# Diagnostic output: country-specific EF for non-bunker sectors  
  ef_non_bunker <- filter( ef_data, sector %!in% bunker_sectors ) %>% select( -sector ) %>% unique()
  if( any( duplicated( ef_non_bunker[ c( "iso", "fuel", "units" ) ] ) ) )
    warning( "There are duplicates in non-bunker sector CO2 EFs.")
  

# ---------------------------------------------------------------------------
# 4. Output
  writeData( ef_data, "MED_OUT", paste0( "B.", em, "_comb_EF_db" ) )
  writeData( ef_non_bunker, "DIAG_OUT", paste0( "B.", em, "_comb_EF_non-bunker" ) )
  
  logStop()
  
  
