# Program Name: B1.1.base_comb_GAINS_EMF-30.R
# Author: Rachel Hoesly, Linh Vu
# Date Last Updated: 25 Mar 2016
# Program Purpose: Generate base emission factors from global GAINS EMF-30 data
#                  SO2, BC, OC, CO2, and CH4 are just used as diagnostics
#
# Input Files:    files in the EF_parameters folder contailing control_percent and em
#               
# Output Files:  
# Notes: transportation_rail only hase a 2020 values, so interpolated values are constant
#           extended back from 2020 to 2011
# TODO:  For SO2, create post 2010 S control trends instead of EFs (only writes to diagnostic now)
#        
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
headers <- c( 'process_db_functions.R','data_functions.R',
              'interpolation_extension_functions.R','common_data.R', 'analysis_functions.R') 
#                 Additional function files may be required.
log_msg <- "Processing GAINS EMF-30 data" 
    # First message to be printed to the log
script_name <- 'B1.1.base_comb_GAINS_EMF-30.R'

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"
em_lc <- tolower( em )   

# Stop script if running for unsupported species
if ( em %!in% c('SO2','NOx','NMVOC','BC','OC','CH4','CO','CO2') ) {
  stop (paste( 'GAINS EMF-30 is not supported for emission species', em, 'remove from script
               list in B1.2.add_comb_EF.R and/or makefile' ))
}

# ---------------------------------------------------------------------------
# 0.5. Load Packages and Define functions

# Define heat_contents function used in section 2
calc_heat_content <- function( conversion, IEAfuel ) {
  conversion <- conversion[ , IEAfuel ]
  unit_multiplier <- sum( colSums(conversion, na.rm = T ) ) / 
    length( na.omit( unlist( conversion ) ) )
  return( unit_multiplier )
}

# ---------------------------------------------------------------------------
# 1. Load Data
#activities sheet
a.sheet <- 'Air pollutants'
if(em == 'CH4') a.sheet <- 'ch4'
if(em == 'CO2') a.sheet <- 'co2'

#emissions sheet
e.sheet <- em
if(em == 'NMVOC') e.sheet <- 'VOC'
emissions <- readData( domain = 'EM_INV', domain_extension = 'GAINS/', file_name = 'GAINS_EMF30_EMISSIONS_extended_Ev5a_CLE_Nov2015',
                       ".xlsx", sheet_selection = e.sheet)
activities <-  readData( domain = 'EM_INV' , domain_extension = 'GAINS/', file_name ='GAINS_EMF30_ACTIVITIES_extended_Ev5a_Nov2015',
                        ".xlsx", sheet_selection = a.sheet)
ctry_map <- readData(domain = 'MAPPINGS',  domain_extension = 'GAINS/', file_name ='emf-30_ctry_map')
fuel_sector_map <- readData(domain = 'MAPPINGS', domain_extension = 'GAINS/'  , file_name ='emf-30_fuel_sector_map')

OECDconversion <- readData( "ENERGY_IN",  "OECD_Conversion_Factors" )
NONconversion <- readData( "ENERGY_IN",  "NonOECD_Conversion_Factors" )

# ---------------------------------------------------------------------------
# 2. Calculate GAINS heat content

# The IEA data provides conversions (heat content) for a variety of flows and fuels
# and for most countries. Take only EU countries and develop a conversion factor 
# by taking a weighted average of fuels brought together into CEDS fuels.
# units of heat_content - kJ/kg

printLog('Calculating Gains Heat Content')

# just European Countries in Gains data.
EU <- c("Czech Republic", "Denmark", "Estonia", "Finland", "France", 
        "Germany", "Greece", "Hungary", "Ireland", "Italy", "Luxembourg",
        "Netherlands", "Poland", "Portugal", "Slovak Republic", "Slovenia",
        "Spain", "Sweden", "United Kingdom", "Belgium", "Latvia",
        "Lithuania", "Austria", "Romania", "Croatia", "Bulgaria",
        "Malta", "Cyprus")
Flows <- c("NCV of imports", "Average net calorific value")

colnames(OECDconversion) <- colnames(NONconversion)
OECD <- OECDconversion[OECDconversion$COUNTRY %in% EU & 
                         OECDconversion$FLOW %in% Flows,]
NONOECD <- NONconversion[NONconversion$COUNTRY %in% EU & 
                           NONconversion$FLOW %in% Flows,]
conversion <- rbind(NONOECD, OECD) 

conversion[,3:ncol(conversion)] <- 
  suppressWarnings(lapply(conversion[,3:ncol(conversion)], as.numeric))

# diagnostic-output
writeData( conversion, domain = "DIAG_OUT", 
           fn = "B1.2.energy_conversion_factors") 

# Using the function and IEA fuels to create a unit conversion multiplier for
# the CEDS categories in the GAINS data. 

browncoal <- c("Sub.bituminous.coal", "Lignite")
mult_browncoal <- calc_heat_content(conversion, browncoal)

hardcoal <- c("Anthracite", "Other.bituminous.coal")
mult_hardcoal <- calc_heat_content(conversion, hardcoal)

# biomass <- c("Biogasoline", "Biodiesels", "Other.liquid.biofuels", "Charcoal")
# IEA does not have covnersion factors for biomass, so assume value for wood
mult_biomass <- 13799.60448 # Value for wood

lightoil <- c("Motor.gasoline", "Aviation.gasoline","Gasoline.type.jet.fuel", 
              "Kerosene.type.jet.fuel", "Other.kerosene")
mult_lightoil <- calc_heat_content(conversion, lightoil)

heavy_oil <- c("Crude.oil", "Fuel.oil")
mult_heavy_oil <- calc_heat_content(conversion, heavy_oil)

# These have a single column of data and therefore the function is not necessary
NaturalGas <- c("Natural.gas.liquids")
mult_NaturalGas <- sum(conversion[,NaturalGas] ,na.rm = T)/
  length(na.omit(conversion[,NaturalGas]))

Diesel <- c("Gas.diesel.oil")
mult_Diesel <- sum(conversion[,Diesel] ,na.rm = T)/
  length(na.omit(conversion[,Diesel]))

# make list of calculated heat contents
GAINS_heat_content <- data.frame(fuel=c('brown_coal', 'hard_coal', 'biomass', 'light_oil', 
                                        'natural_gas', 'diesel_oil', 'heavy_oil'),
                                 heat_content=c(mult_browncoal, mult_hardcoal, mult_biomass, mult_lightoil, 
                                                mult_NaturalGas, mult_Diesel, mult_heavy_oil))
GAINS_heat_content$units <- "kJ/kg"

writeData( GAINS_heat_content, domain = "MED_OUT", fn = "B1.1.Europe_heat_content_IEA") 

# ---------------------------------------------------------------------------
# 2. Change to CEDS sectors and fuels

printLog("Mapping and aggregating to ceds fuels and sectors")

X_years <- c('X2000','X2005','X2010','X2020')
years <- c('2000','2005','2010','2020')

#select years
emissions <- emissions[,c('Region','Sector',years)]
activities <- activities[,c('Region','Sector','Unit',years)]

#convert to fuel and sector
emissions_ceds <- merge(emissions, ctry_map,
                        by.x = 'Region',
                        by.y = 'emf_name', all=TRUE)  

emissions_ceds <- mapCEDS_sector_fuel(mapping_data = emissions_ceds,
                              mapping_file = fuel_sector_map,
                              data_match_col = 'Sector',
                              map_match_col = 'emf_sector',
                              map_merge_col = c('ceds_sector', 'ceds_fuel'),
                              new_col_names = c('sector', 'fuel'),
                              level_map_in = 'working_sectors_v1',
                              level_out = 'working_sectors_v1',
                              aggregate = TRUE,
                              aggregate_col = years,
                              oneToOne = FALSE,
                              agg.fun = sum )


emissions_ceds <- emissions_ceds[complete.cases(emissions_ceds),]
emissions_ceds$units <- 'kt'
# only CO2 is reported in Tg rather than Gg, so multiply by 10^3 to convert Tg to Gg
if(em == 'CO2') emissions_ceds[,years] <- emissions_ceds[,years]*10^3

emissions_ceds <- emissions_ceds[,c('iso','sector','fuel','units',years)]
names(emissions_ceds) <- c('iso','sector','fuel','units',X_years)

activities_ceds <- merge(activities, ctry_map,
                        by.x = 'Region',
                        by.y = 'emf_name', all=TRUE)  
activities_ceds <- mapCEDS_sector_fuel(mapping_data = activities_ceds,
                                        mapping_file = fuel_sector_map,
                                        data_match_col = 'Sector',
                                        map_match_col = 'emf_sector',
                                        map_merge_col = c('ceds_sector', 'ceds_fuel'),
                                        new_col_names = c('sector', 'fuel'),
                                        level_map_in = 'working_sectors_v1',
                                        level_out = 'working_sectors_v1',
                                        aggregate = TRUE,
                                        aggregate_col = years,
                                        oneToOne = FALSE,
                                        agg.fun = sum )

activities_ceds <- activities_ceds[complete.cases(activities_ceds),]
activities_ceds <- activities_ceds[,c('iso','sector','fuel','Unit',years)]

names(activities_ceds) <- c('iso','sector','fuel','units',X_years)

# ---------------------------------------------------------------------------
# 3. Convert energy units to mass units (Activity)

printLog("Converting energy units to mass units")

# Convert PetaJoules into kiloJoules
activities_ceds[which(activities_ceds$units=='PJ'), 
                X_years ] <- activities_ceds[which(activities_ceds$units=='PJ'), X_years ] * 10^12
activities_ceds[which(activities_ceds$units=='PJ'), 'units' ] <- 'kJ'

# Use heat content to 
# Convert energy to kg by dividing by heat_content (KJ/Kg)
# then multiply by 10^-6 to convert to kt
activities_ceds[activities_ceds$fuel %in% "brown_coal", X_years] <- 
  (activities_ceds[activities_ceds$fuel %in% "brown_coal", X_years] / 
     GAINS_heat_content[which(GAINS_heat_content$fuel=='brown_coal'),'heat_content']) * 10^-6
activities_ceds[activities_ceds$fuel %in% "hard_coal", X_years] <- 
  (activities_ceds[activities_ceds$fuel %in% "hard_coal", X_years] / 
     GAINS_heat_content[which(GAINS_heat_content$fuel=='hard_coal'),'heat_content']) * 10^-6
activities_ceds[activities_ceds$fuel %in% "biomass", X_years] <- 
  (activities_ceds[activities_ceds$fuel %in% "biomass", X_years] / 
     GAINS_heat_content[which(GAINS_heat_content$fuel=='biomass'),'heat_content']) * 10^-6
activities_ceds[activities_ceds$fuel %in% "light_oil", X_years] <- 
  (activities_ceds[activities_ceds$fuel %in% "light_oil", X_years] / 
     GAINS_heat_content[which(GAINS_heat_content$fuel=='light_oil'),'heat_content']) * 10^-6
activities_ceds[activities_ceds$fuel %in% "natural_gas", X_years] <- 
  (activities_ceds[activities_ceds$fuel %in% "natural_gas", X_years] / 
     GAINS_heat_content[which(GAINS_heat_content$fuel=='natural_gas'),'heat_content']) * 10^-6
activities_ceds[activities_ceds$fuel %in% "diesel_oil", X_years] <- 
  (activities_ceds[activities_ceds$fuel %in% "diesel_oil", X_years] / 
     GAINS_heat_content[which(GAINS_heat_content$fuel=='diesel_oil'),'heat_content']) * 10^-6
activities_ceds[activities_ceds$fuel %in% "heavy_oil", X_years] <- 
  (activities_ceds[activities_ceds$fuel %in% "heavy_oil", X_years] / 
     GAINS_heat_content[which(GAINS_heat_content$fuel=='heavy_oil'),'heat_content']) * 10^-6

activities_ceds[which(activities_ceds$units=='kJ'), 'units' ] <- 'kt'

# Convert mass units to kt
activities_ceds[which(activities_ceds$units=='Mt'), 
                X_years ] <- activities_ceds[which(activities_ceds$units=='Mt'), X_years ]*10^3
activities_ceds[which(activities_ceds$units=='Mt'), 'units' ] <- 'kt'

# Aggregate to ceds sectors and fuels

activities_ceds <- aggregate( activities_ceds[,X_years],
                              by = list ( iso = activities_ceds$iso ,
                                          sector = activities_ceds$sector ,
                                          fuel = activities_ceds$fuel ,
                                          units = activities_ceds$units ),
                              FUN = sum)

emissions_ceds <- aggregate( emissions_ceds[,X_years],
                             by = list ( iso = emissions_ceds$iso ,
                                         sector = emissions_ceds$sector ,
                                         fuel = emissions_ceds$fuel ,
                                         units = emissions_ceds$units ),
                             FUN = sum)


# ---------------------------------------------------------------------------
# 4. Interpolate Data through current year

printLog("Interpolating and Extending Data")

activities_ceds_interp <- interpolateValues(activities_ceds)
emissions_ceds_interp <- interpolateValues(emissions_ceds)

ceds_years <- 2000:end_year
X_ceds_years <- paste0('X',ceds_years)

emf_years <- 2000:2020
X_emf_years <- paste0('X',emf_years)

# ---------------------------------------------------------------------------
# 5. Seperate process data and Calculate Emission Factors

printLog('Calculating emission factors')

combined <- merge(activities_ceds_interp, 
                  emissions_ceds_interp, 
                  by = c('iso','sector','fuel'),
                  suffixes = c(".a",".e"), all = T )

combined[X_emf_years] <- combined[ paste0(X_emf_years, '.e') ]/combined[ paste0(X_emf_years, '.a') ]
combined$units <- paste0(combined$units.e,'/',combined$units.a)

combined <- combined[which(apply(MARGIN = 1, X= combined[X_emf_years], FUN = function(x) !all(is.na(x)) )),
                     c('iso','sector','fuel','units',X_emf_years)]

gainsEMF30_all <- combined[,c('iso','sector','fuel','units',X_ceds_years)]

# check for NAs and I
gainsEMF30_all <- replace(gainsEMF30_all, gainsEMF30_all==Inf, NA)
gainsEMF30_all <- replace(gainsEMF30_all, gainsEMF30_all==NaN, NA)

# Remove coal_coke if SO2 emissions. EMF note only coal, and coal coke has similar EF's
# as other coals for other emission species, except SO2

if (em == 'SO2'){
  gainsEMF30_all <- gainsEMF30_all[-which(gainsEMF30_all$fuel == 'coal_coke'),]
}

# Brown coal has lower heat content so ends up having a higher default EF when converting
# kg/PJ to kg/kg. To correct, give brown coal EF the same value as hard coal for most 
# emissions, and 70% hard coal value for NOx.
#
# This is because brown coal has a lower NOx EF per unit weight than hard coal. 
# The ratio ranges from 0.6 for pulverized coal units to 0.8 for Spreader stokers 
# (Source: US EPA, AP-42), so we use 0.7 average value
  Xyears <- names( gainsEMF30_all )[ grepl( "X", names( gainsEMF30_all ) ) ]
  gains_brown_coal <- filter( gainsEMF30_all, fuel == "brown_coal" ) %>% arrange( iso, sector )
  gains_hard_coal <- filter( gainsEMF30_all, fuel == "hard_coal" ) %>% arrange( iso, sector )
  gains_brown_coal[, Xyears ] <- gains_hard_coal[, Xyears ]
  if ( em == "NOx" )
    gains_brown_coal[, Xyears ] <- .7 * gains_brown_coal[, Xyears ]
  gainsEMF30_all <- filter( gainsEMF30_all, fuel != "brown_coal" ) %>%
    rbind( gains_brown_coal ) %>% arrange( iso, sector, fuel )
  
# seperate process and combustion
process_gainsEMF30 <- gainsEMF30_all[which(gainsEMF30_all$fuel=='process'),]
gainsEMF30_comb <- gainsEMF30_all[gainsEMF30_all$fuel %!in% 'process',]

# ---------------------------------------------------------------------------
# 6. Diagnostics

gains_diagnostics <- gainsEMF30_all
# ratio  of the last year to the last inventory year (2008)
gains_diagnostics$ratio <- gains_diagnostics$X2014/gains_diagnostics$X2008
gains_diagnostics <- gains_diagnostics[which(gains_diagnostics$ratio > 1.5 | gains_diagnostics$ratio < .5),]


# ---------------------------------------------------------------------------
# 7. Output

writeData(process_gainsEMF30, domain = "DIAG_OUT", fn = paste0('B.',em,'_NC_EF_GAINS_EMF30'))

# For SO2, BC, OC, CH4, CO2 don't use global GAINS data, but write to diagnostic
# for other species, write to intermediate-output to be used for base emission factors
if ( em %in% c('SO2','BC','OC','CO2') ) {
  writeData(gainsEMF30_comb, domain = "DIAG_OUT", fn = paste0('B.',em,'_comb_EF_GAINS_EMF30'))
} else{
  writeData(gainsEMF30_comb, domain = "MED_OUT", fn = paste0('B.',em,'_comb_EF_GAINS_EMF30'))
}

logStop()
# END
