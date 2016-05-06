#------------------------------------------------------------------------------
# Program Name: A3.3.write_IEA_diff.R
# Authors Names: Linh Vu
# Date Last Modified: 6 May 2016
# Program Purpose:    Write out difference between IEA DOMSUP and CEDS
#                     consumption for coal, natural gas, petroleum
# Input Files: A.IEA_en_stat_ctry_hist.csv, A.IEA_BP_energy_ext.csv
# Output Files: A.IEA_CEDS_coal_difference.csv, A.IEA_CEDS_natural_gas_difference.csv, 
#               A.IEA_CEDS_petroleum_difference.csv
# Notes:  The purpose of this script is to quantify how much fuel is consumed in the 
#         transformation sector and, therefore, not accounted for in CEDS fuel consumption. 
#         This is then used for balancing fuel consumption and extending emissions back in time.
# TODO: 
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Before we can load headers we need some paths defined. They may be provided by
#   a system environment variable or may have already been set in the workspace.
# Set variable PARAM_DIR to be the data system directory
dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
for ( i in 1:length( dirs ) ) {
  setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
  wd <- grep( 'CEDS/input', list.dirs(), value = T )
  if ( length(wd) > 0 ) {
    setwd( wd[1] )
    break
  }
}
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "common_data.R" ) # Additional function files required.
log_msg <- "Write out difference between IEA and CEDS consumption" # First message to be printed to the log
script_name <- "A3.3.write_IEA_diff.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Read in files
  IEA_en_stat_ctry_hist <- readData( "MED_OUT", "A.IEA_en_stat_ctry_hist" )
  IEA_BP_energy_ext <- readData( "MED_OUT", "A.IEA_BP_energy_ext" )
  IEA_product_fuel <- readData( "MAPPINGS", "IEA_product_fuel", domain_extension = "energy/" )
  IEA_flow_sector <- readData( "MAPPINGS", "IEA_flow_sector", domain_extension = "energy/" )
  
# Define values
  IEA_coal_list <- c( "Brown coal (if no detail) (kt)", "Coking coal (kt)", 
                      "Hard coal (if no detail) (kt)", "Other bituminous coal (kt)", 
                      "Sub-bituminous coal (kt)", "Lignite (kt)", "Anthracite (kt)", 
                      "Patent fuel (kt)" )
  CEDS_coal_list <- c( "hard_coal", "brown_coal", "coal_coke" )
  
  IEA_natural_gas_list <- c( "Gas works gas (TJ-gross)", "Coke oven gas (TJ-gross)", 
                             "Blast furnace gas (TJ-gross)", "Other recovered gases (TJ-gross)", 
                             "Natural gas (TJ-gross)", "Natural Gas (TJ-gross)", 
                             "Refinery gas (kt)", "Biogases (TJ-net)" )
  CEDS_natural_gas_list <- c( "natural_gas" )
  
  IEA_oil_list_primary <- c( "Oil shale and oil sands (kt)", "Crude/NGL/feedstocks (if no detail) (kt)", "Crude oil (kt)", 
                                 "Natural gas liquids (kt)", "Refinery feedstocks (kt)", "Additives/blending components (kt)", 
                                 "Other hydrocarbons (kt)" )
  IEA_oil_list_secondary <- c( "Ethane (kt)", "Liquefied petroleum gases (LPG) (kt)", "Motor gasoline excl. biofuels (kt)", 
                                     "Aviation gasoline (kt)", "Gasoline type jet fuel (kt)", "Kerosene type jet fuel excl. biofuels (kt)", 
                                     "Other kerosene (kt)", "Other Kerosene (kt)", "Gas/diesel oil excl. biofuels (kt)", "Fuel oil (kt)", 
                                     "Naphtha (kt)", "White spirit & SBP (kt)", "Lubricants (kt)", "Bitumen (kt)", "Paraffin waxes (kt)", 
                                     "Petroleum coke (kt)", "Other oil products (kt)", "Biogasoline (kt)", "Biodiesels (kt)", 
                                     "Other liquid biofuels (kt)" ) 
  CEDS_oil_list <- c( "heavy_oil", "light_oil", "diesel_oil" )
  
# Define functions
# writeDiff(): Write out IEA and CEDS consumption difference
#   params:   IEA_fuel_list: vector of IEA PRODUCT names
#             CEDS_fuel_list: vector of CEDS fuel names
#             fuel_name: fuel name in function's output
#   Returns:  List of two data frames, where df1 contains IEA and CEDS difference and
#             df2 contains diagnostics of where IEA < CEDS 
# Note that units is kt, so may need to process IEA input for certain fuels
  writeDiff <- function( IEA_fuel_list, CEDS_fuel_list, fuel_name ) {
    IEA_subset <- filter( IEA_en_stat_ctry_hist, FLOW == "DOMSUP", PRODUCT %in% IEA_fuel_list ) %>%
      select( -PRODUCT ) %>% group_by( iso, FLOW ) %>%
      summarise_each( "sum" )
    
    CEDS_subset <- filter( IEA_BP_energy_ext, fuel %in% CEDS_fuel_list ) %>%
      select( -fuel, -sector, -units ) %>% group_by( iso ) %>%
      summarise_each( "sum" )
    
    return ( computeDiff( IEA_subset, CEDS_subset, fuel_name ) )
  }
  
# computeDiff(): Helper function to compute IEA and CEDS difference
#   params:   IEA_subset: df of IEA consumption, aggregated by country
#             CEDS_subset: df of CEDS consumption, aggregated by country
#             fuel_name: fuel name in function's output
#   Returns:  List of two data frames, where df1 contains IEA and CEDS difference and
#             df2 contains diagnostics of where IEA < CEDS 
  computeDiff <- function( IEA_subset, CEDS_subset, fuel_name ) {
    # Compute difference
    IEA_subset <- filter( IEA_subset, iso %in% CEDS_subset$iso ) %>% arrange( iso )
    IEA_subset <- IEA_subset[ names( IEA_subset ) %in% names( CEDS_subset ) ]
    CEDS_subset <- filter( CEDS_subset, iso %in% IEA_subset$iso ) %>% arrange( iso )
    CEDS_subset <- CEDS_subset[ names( CEDS_subset ) %in% names( IEA_subset ) ]
    diff <- IEA_subset
    Xyears <- names( diff )[ grepl( "X", names( diff ) ) ]
    diff[ Xyears ] <- diff[ Xyears ] - CEDS_subset[ Xyears ]
    diff <- as.data.frame( diff )
    
    # If difference < 0, bring up to 0 and write out diagnostics
    subzero <- melt( diff, id = "iso" ) %>%
      filter( value < 0 ) %>%
      cast()
    diff[ diff < 0 ] <- 0
    
    # Clean up
    diff$sector <- "1A1bc_Other-transformation"
    diff$fuel <- fuel_name
    diff$units <- "kt"
    diff <- diff[ c( "iso", "sector", "fuel", "units", Xyears ) ]
    
    # Output
    out <- list( diff, subzero )
    names( out ) <- c( "diff", "subzero" )
    return( out )
  }
  
# ------------------------------------------------------------------------------
# 2. Write out difference between IEA DOMSUP and CEDS consumption
# Prelim processing: Some IEA natural gas PRODUCTS are in TJ, so convert to kt
  IEA_TJ_gas <- unique( IEA_en_stat_ctry_hist$PRODUCT[ IEA_en_stat_ctry_hist$PRODUCT %in% IEA_natural_gas_list & 
                                                        grepl( "TJ", IEA_en_stat_ctry_hist$PRODUCT ) ] )
  IEA_en_stat_ctry_hist[ IEA_en_stat_ctry_hist$PRODUCT %in% IEA_TJ_gas, X_IEA_years ] <- 
    IEA_en_stat_ctry_hist[ IEA_en_stat_ctry_hist$PRODUCT %in% IEA_TJ_gas, X_IEA_years ] / 
    conversionFactor_naturalgas_TJ_per_kt

# Coal
  out <- writeDiff( IEA_coal_list, CEDS_coal_list, "coal" )
  diff_coal <- out[[ "diff" ]]
  subzero_coal <- out[[ "subzero" ]]
  
# Natural gas
  out <- writeDiff( IEA_natural_gas_list, CEDS_natural_gas_list, "natural_gas" )
  diff_natural_gas <- out[[ "diff" ]]
  subzero_natural_gas <- out[[ "subzero" ]]
  
# Oil: Take IEA supply = primary oil DOMSUP + secondary oil products imports/exports
  IEA_oil_primary <- filter( IEA_en_stat_ctry_hist, FLOW == "DOMSUP", PRODUCT %in% IEA_oil_list_primary )
  IEA_oil_secondary <- filter( IEA_en_stat_ctry_hist, FLOW %in% c( "IMPORTS", "EXPORTS" ), 
                               PRODUCT %in% IEA_oil_list_secondary )
  IEA_oil <- bind_rows( IEA_oil_primary, IEA_oil_secondary ) %>% 
    select( -PRODUCT, -FLOW ) %>% group_by( iso ) %>%
    summarise_each( "sum" )
  CEDS_oil <- filter( IEA_BP_energy_ext, fuel %in% CEDS_oil_list ) %>%
    select( -fuel, -sector, -units ) %>% group_by( iso ) %>%
    summarise_each( "sum" )
  
  out <- computeDiff( IEA_oil, CEDS_oil, "petroleum" )
  diff_oil <- out[[ "diff" ]]
  subzero_oil <- out[[ "subzero" ]]
  
# Diagnostics: Compute diff/total CEDS
  diag_ratio <- filter( diff_oil, iso %in% CEDS_oil$iso ) %>% arrange( iso ) 
  CEDS_oil_temp <- filter( CEDS_oil, iso %in% diag_ratio$iso ) %>% arrange( iso )
  diag_ratio[, X_IEA_years ] <- diag_ratio[, X_IEA_years ] / CEDS_oil_temp[, X_IEA_years ]
  
# Diagnostics: Write out IEA Non-Energy consumption for natural gas and oil
  IEA_NE_sectors <- c( "NECHEM", "NEINTREN", "NEOTHER", "NETRANS", "NONENUSE", "NONFERR", "NONMET" )
  CEDS_fuels <- c( "light_oil", "heavy_oil", "diesel_oil", "natural_gas" )
  IEA_nonenergy <- filter( IEA_en_stat_ctry_hist, FLOW %in% IEA_NE_sectors )
  IEA_nonenergy$sector <- IEA_flow_sector$sector[ match( IEA_nonenergy$FLOW, IEA_flow_sector$flow_code ) ]
  IEA_nonenergy$fuel <- IEA_product_fuel$fuel[ match( IEA_nonenergy$PRODUCT, IEA_product_fuel$product ) ]
  IEA_nonenergy$units <- "kt"
  diag_nonenergy <- filter( IEA_nonenergy, fuel %in% CEDS_fuels ) %>% arrange( iso, FLOW, PRODUCT )
  diag_nonenergy <- diag_nonenergy[ c( "iso", "FLOW", "PRODUCT", "sector", "fuel", "units", X_IEA_years ) ]

# ------------------------------------------------------------------------------
# 3. Output
  writeData( diff_coal, "MED_OUT", "A.IEA_CEDS_coal_difference" )
  writeData( subzero_coal, "DIAG_OUT", "A.IEA_CEDS_coal_difference_subzero" )
  
  writeData( diff_natural_gas, "MED_OUT", "A.IEA_CEDS_natural_gas_difference" )
  writeData( subzero_natural_gas, "DIAG_OUT", "A.IEA_CEDS_natural_gas_difference_subzero" )
  
  writeData( diff_oil, "MED_OUT", "A.IEA_CEDS_petroleum_difference" )
  writeData( subzero_oil, "DIAG_OUT", "A.IEA_CEDS_petroleum_difference_subzero" )

# Diagnostics
  writeData( diag_ratio, "DIAG_OUT", "A.IEA_CEDS_petroleum_difference_ratio" )
  diag_nonenergy[ is.na( diag_nonenergy ) ] <- ""
  writeData( diag_nonenergy, "DIAG_OUT", "A.IEA_en_stat_ctry_hist-NonEnergy-Oil&Gas" )
  
logStop()

