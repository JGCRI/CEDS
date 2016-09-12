# Program Name: B1.1.base_CO2_comb_EF.R
# Author: Linh Vu
# Date Last Updated: 27 Jul 2016 
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
  cdiac_EF <- readData( "DEFAULT_EF_IN", "CO2_base_EF_CDIAC" )
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
  cdiac_EF$EF[ cdiac_EF$cdiac_fuel == "gas_fuels" & cdiac_EF$units == "kt/TJ" ] <-
    cdiac_EF$EF[ cdiac_EF$cdiac_fuel == "gas_fuels" & cdiac_EF$units == "kt/TJ" ] * 
    conversionFactor_naturalgas_TJ_per_kt  # kt/TJ * TJ/kt = kt/kt
  cdiac_EF$units[ cdiac_EF$cdiac_fuel == "gas_fuels" & cdiac_EF$units == "kt/TJ" ] <- "kt/kt"
  
# Convert solid_fuels EF from kt/kJ to kt/kt, using country/type/year-specific coal heat content
  if ( any( coal_heat_content$units != "kJ/kt" ) | any( cdiac_EF$units[ cdiac_EF$cdiac_fuel == "solid_fuels" ] != "kt/kJ" ) )
    stop( "Units mismatched. Check that coal heat content is kJ/kt and CDIAC solid fuels EF is kt/kJ." )
  solid_fuels_EF <- coal_heat_content
  solid_fuels_EF[, X_emissions_years ] <- solid_fuels_EF[, X_emissions_years ] * cdiac_EF$EF[ cdiac_EF$cdiac_fuel == "solid_fuels" ]
  solid_fuels_EF$units <- "kt/kt"

# ---------------------------------------------------------------------------
# 3. Make EF df for all iso+sector+fuel
# Blank EF template
  EF_data <- activity_data
  EF_data[, X_emissions_years ] <- NA
  EF_data$cdiac_fuel <- IEA_product_fuel$cdiac_fuel[ match( EF_data$fuel, IEA_product_fuel$fuel ) ]
  
# Add solid_fuels EF
  EF_data[, X_emissions_years ] <- solid_fuels_EF[ match( 
    paste( EF_data$iso, EF_data$fuel ), paste( solid_fuels_EF$iso, solid_fuels_EF$fuel ) ),
    X_emissions_years ]
  
# Add liquid_fuels EF
  EF_data[ EF_data$cdiac_fuel == "liquid_fuels", X_emissions_years ] <- cdiac_EF$EF[ cdiac_EF$cdiac_fuel == "liquid_fuels" ]
  
# Add gas_fuels EF
  EF_data[ EF_data$cdiac_fuel == "gas_fuels", X_emissions_years ] <- cdiac_EF$EF[ cdiac_EF$cdiac_fuel == "gas_fuels" ]
  
# # Add bunker_fuels EF 
# # Note CDIAC bunker_fuels EF is liquid bunker fuels
#   EF_data$cdiac_fuel[ EF_data$sector %in% bunker_sectors
#                       & EF_data$cdiac_fuel == "liquid_fuels" ] <- "bunker_fuels"
#   EF_data[ EF_data$cdiac_fuel == "bunker_fuels", X_emissions_years ] <- cdiac_EF$EF[ cdiac_EF$cdiac_fuel == "bunker_fuels" ]
  
  
# Final processing
  EF_data[ is.na( EF_data ) ] <- 0
  EF_data[, X_emissions_years ] <- EF_data[, X_emissions_years ] * conversionFactor_C_CO2  # C to CO2
  EF_data$units <- "kt/kt"
  EF_data <- EF_data[, c( "iso", "sector", "fuel", "units", X_emissions_years ) ]
  EF_data <- arrange( EF_data, iso, sector, fuel )

# ---------------------------------------------------------------------------
# 4. Output
  writeData( EF_data, "MED_OUT", paste0( "B.", em, "_comb_EF_db" ) )
  
  logStop()
  
  
