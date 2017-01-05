# Program Name: B1.1.base_CO2_comb_EF.R
# Author: Linh Vu, Rachel Hoesly
# Date Last Updated: 14 December 2016 
# Program Purpose: Remove fraction of liquid biofuels from EFs
# Input Files: B.[em]_comb_EF_db.csv
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
log_msg <- "Removing liquid biofuels from CO2 combustion EFs" 
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
  emission_coefficient <- readData( "DEFAULT_EF_IN", "CO2_base_EF", ".xlsx", 
                                    sheet_selection = "Emission_Coefficient" )
  fraction_oxidized <- readData( "DEFAULT_EF_IN", "CO2_base_EF", ".xlsx", 
                                 sheet_selection = "Fraction_Oxidized" )
  activity_data <- readData( "MED_OUT", "A.comb_activity" )
  
  MCL <- readData( "MAPPINGS", "Master_Country_List" )
  IEA_product_fuel <- readData( "MAPPINGS", "IEA_product_fuel", domain_extension = "energy/" )

  
  conversion_fuels <- c('hard_coal','brown_coal')
  no_conversion_fuels <- c("light_oil","heavy_oil","diesel_oil","natural_gas", 'coal_coke')
  bunker_sectors <- c('1A3aii_Domestic-aviation',
                      '1A3ai_International-aviation',
                      '1A3di_International-shipping',
                      '1A3dii_Domestic-navigation')
# ---------------------------------------------------------------------------
# 2. Unit conversions
# Convert coal heat content from kJ/kg to kJ/kt
  coal_heat_content[ coal_heat_content$units == "kJ/kg", X_emissions_years ] <- 
    coal_heat_content[ coal_heat_content$units == "kJ/kg", X_emissions_years ] * 10^6
  coal_heat_content$units[ coal_heat_content$units == "kJ/kg" ] <- "kJ/kt"

  if ( any( coal_heat_content$units != "kJ/kt" ) | any( emission_coefficient$units[ emission_coefficient$cdiac_fuel %in% conversion_fuels ] != "kt CO2/kJ" ) )
    stop( "Units mismatched. Check that coal heat content is kJ/kt and CDIAC solid fuels EF is kt/kJ." )

  
# Convert gas_fuels EF from kt/TJ to kt/kt
  emission_coefficient$Emission_Coefficient[ emission_coefficient$fuel %in% "natural_gas" ] <-
    emission_coefficient$Emission_Coefficient[ emission_coefficient$fuel %in% "natural_gas" ] * 
    conversionFactor_naturalgas_TJ_per_kt  # kt/TJ * TJ/kt = kt/kt
  emission_coefficient$units[ emission_coefficient$fuel %in% "natural_gas" ] <- "kt CO2/kt"
  
# ---------------------------------------------------------------------------
# 3. Calculate Emission Factor according to:
# For Solid Fuels
# Oxidized_Emission_Coefficient = Emission_Coefficient (kg CO2/kJ fuel) * Fraction_Oxidized * 
#                   Energy_Content (kJ fuel/kt fuel) 
# For Liquid Fuels
# Oxidized_Emission_Coefficient = Emission_Coefficient (kg CO2/kt fuel) * Fraction_Oxidized

# template
  ef_data <- activity_data[c('iso','sector','fuel')]
  
# Fill in db with Emission Coefficient
  #first match by fuel defaults
  default <- emission_coefficient[ which( emission_coefficient$iso == 'default' & 
                                            emission_coefficient$sector == 'default'),]

  ef_data$Emission_Coefficient <-  default[ match(ef_data$fuel,default$fuel),'Emission_Coefficient']
  # match by iso and fuel
  iso_default <- emission_coefficient[ which( emission_coefficient$iso == 'default' &
                                                   emission_coefficient$iso != 'default'),]
  ef_data<- replaceValueColMatch(ef_data, iso_default,
                                           x.ColName = 'Emission_Coefficient',
                                           match.x = c('iso','fuel'),
                                           addEntries = F)
  # match by sector and fuel
  sector_default <- emission_coefficient[ which( emission_coefficient$iso == 'default' &
                                                    emission_coefficient$sector != 'default'),]
  ef_data<- replaceValueColMatch(ef_data, sector_default,
                                           x.ColName = 'Emission_Coefficient',
                                           match.x = c('sector','fuel'),
                                           addEntries = F)
  # match by iso, sector & fuel
  iso_sector_fuel <- emission_coefficient[ which( emission_coefficient$iso != 'default' &
                                                   emission_coefficient$sector != 'default'),]
  ef_data<- replaceValueColMatch(ef_data, iso_sector_fuel,
                                           x.ColName = 'Emission_Coefficient',
                                           match.x = c('iso','sector','fuel'),
                                           addEntries = F)

# Fill in db with Fraction Oxidized
  #first match by fuel defaults
  default <- fraction_oxidized[ which( fraction_oxidized$iso == 'default' &
                                            fraction_oxidized$sector == 'default'),]

  ef_data$fraction_oxidized <-  fraction_oxidized[ match(ef_data$fuel, default$fuel),'Fraction_Oxidized']
  # match by iso and fuel
  iso_default <- fraction_oxidized[ which( fraction_oxidized$sector == 'default' &
                                                fraction_oxidized$iso != 'default'),]
  ef_data<- replaceValueColMatch(ef_data, iso_default,
                                           x.ColName = 'fraction_oxidized',
                                           y.ColName = 'Fraction_Oxidized',
                                           match.x = c('iso','fuel'),
                                           addEntries = F)
  # match by sector and fuel
  sector_default <- fraction_oxidized[ which( fraction_oxidized$iso == 'default' &
                                                   fraction_oxidized$sector != 'default'),]
  ef_data<- replaceValueColMatch(ef_data, sector_default,
                                           x.ColName = 'fraction_oxidized',
                                           y.ColName = 'Fraction_Oxidized',
                                           match.x = c('sector','fuel'),
                                           addEntries = F)
  # match by iso, sector & fuel
  iso_sector_fuel <- fraction_oxidized[ which( fraction_oxidized$iso != 'default' &
                                                    fraction_oxidized$sector != 'default'),]
  ef_data <- replaceValueColMatch(ef_data, sector_default,
                                           x.ColName = 'fraction_oxidized',
                                           y.ColName = 'Fraction_Oxidized',
                                           match.x = c('iso','sector','fuel'),
                                           addEntries = F)

# Multiply emission coefficient by oxidation fraction
  ef_data$EF <- ef_data$fraction_oxidized*ef_data$Emission_Coefficient
  ef_data[, X_emissions_years ] <- NA
# fill in trend with constant EF for liquid fuels
  ef_data[which(ef_data$fuel %in% no_conversion_fuels), X_emissions_years ] <-  matrix( rep( ef_data$EF[which(ef_data$fuel %in% no_conversion_fuels)], 
                                                                                      each = length(X_emissions_years)),
                                                                            ncol=length(X_emissions_years) , byrow=T) 
# fill in zero for biomass 
  ef_data[which(ef_data$fuel == 'biomass'),c("Emission_Coefficient","fraction_oxidized",'EF', X_emissions_years)] <- 0
  
# ---------------------------------------------------------------------------
# 3. Conversion for solid fuel using coal heat content

#   # Convert solid_fuels EF from kt/kJ to kt/kt, using country/type/year-specific coal heat content
  ef_data[which(ef_data$fuel %in% conversion_fuels), X_emissions_years ] <- ef_data[which(ef_data$fuel %in% conversion_fuels), 'EF' ]*
                                                      coal_heat_content[match(paste(ef_data[which(ef_data$fuel %in% conversion_fuels), 'iso' ], 
                                                                                    ef_data[which(ef_data$fuel %in% conversion_fuels), 'fuel' ]),
                                                                              paste(coal_heat_content$iso,coal_heat_content$fuel)),
                                                                        X_emissions_years]

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
  
  
  
  

  
