# Program Name: B1.2.add_GAINS_EMF-30.R
# Author: Rachel Hoesly
# Date Last Updated: 16 Dec 2015 
# Program Purpose: 
# Input Files:    files in the EF_parameters folder contailing control_percent and em
#               
# Output Files:  
# Notes: transportation_rail only hase a 2020 values, so interpolated values are constant
#           extended back from 2020 to 2011
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
headers <- c( 'process_db_functions.R','data_functions.R',
              'interpolation_extention_functions.R','common_data.R') 
#                 Additional function files may be required.
log_msg <- "Adding control percent data to data base" # First message to be printed to the log
script_name <- "B1.2.add_comb_control_percent.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"
em_lc <- tolower( em )    

# ---------------------------------------------------------------------------
# 1. Load Data

sheet <- 'Air pollutants'
if(em == 'CH4') sheet <- 'ch4'
if(em == 'CO2') sheet <- 'co2'

emissions <- readData( 'EM_INV', 'GAINS_EMF30_EMISSIONS_extended_Ev5a_CLE_Nov2015',
                       ".xlsx", sheet_selection = em)
activities <-  readData( 'EM_INV' , 'GAINS_EMF30_ACTIVITIES_extended_Ev5a_Nov2015',
                        ".xlsx", sheet_selection = sheet)
ctry_map <- readData('MAPPINGS', 'emf-30_ctry_map')
fuel_sector_map <- readData('MAPPINGS', 'emf-30_fuel_sector_map')

heat_content <- readData('MED_OUT', 'B1.2.heat_content')

# ---------------------------------------------------------------------------
# 2. Change to CEDS sectors and fuels

printLog("Mapping and aggregating to ceds fuels and sectors")

X_years <- c('X2000','X2005','X2010','X2020')
years <- c('2000','2005','2010','2020')

emissions_ceds <- merge(emissions, ctry_map,
                        by.x = 'Region',
                        by.y = 'emf_name', all=TRUE)  
emissions_ceds <- merge(emissions_ceds, fuel_sector_map,
                        by.x = 'Sector',
                        by.y = 'emf_sector', all=TRUE) 
emissions_ceds <- emissions_ceds[complete.cases(emissions_ceds),]
emissions_ceds$units <- 'kt'
# only CO2 is reported in Tg rather than Gg, so multiply by 10^3 to convert Tg to Gg
if(em == 'CO2') emissions_ceds[,years] <- emissions_ceds[,years]*10^3

emissions_ceds <- emissions_ceds[,c('iso','ceds_sector','ceds_fuel','units',years)]
names(emissions_ceds) <- c('iso','sector','fuel','units',X_years)

activities_ceds <- merge(activities, ctry_map,
                        by.x = 'Region',
                        by.y = 'emf_name', all=TRUE)  
activities_ceds <- merge(activities_ceds, fuel_sector_map,
                        by.x = 'Sector',
                        by.y = 'emf_sector', all=TRUE) 
activities_ceds <- activities_ceds[complete.cases(activities_ceds),]
activities_ceds <- activities_ceds[,c('iso','ceds_sector','ceds_fuel','Unit',years)]
names(activities_ceds) <- c('iso','sector','fuel','units',X_years)

# Aggregate

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
     heat_content[which(heat_content$fuel=='brown_coal'),'heat_content']) * 10^-6
activities_ceds[activities_ceds$fuel %in% "hard_coal", X_years] <- 
  (activities_ceds[activities_ceds$fuel %in% "hard_coal", X_years] / 
     heat_content[which(heat_content$fuel=='hard_coal'),'heat_content']) * 10^-6
activities_ceds[activities_ceds$fuel %in% "biomass", X_years] <- 
  (activities_ceds[activities_ceds$fuel %in% "biomass", X_years] / 
     heat_content[which(heat_content$fuel=='biomass'),'heat_content']) * 10^-6
activities_ceds[activities_ceds$fuel %in% "light_oil", X_years] <- 
  (activities_ceds[activities_ceds$fuel %in% "light_oil", X_years] / 
     heat_content[which(heat_content$fuel=='light_oil'),'heat_content']) * 10^-6
activities_ceds[activities_ceds$fuel %in% "natural_gas", X_years] <- 
  (activities_ceds[activities_ceds$fuel %in% "natural_gas", X_years] / 
     heat_content[which(heat_content$fuel=='natural_gas'),'heat_content']) * 10^-6
activities_ceds[activities_ceds$fuel %in% "diesel_oil", X_years] <- 
  (activities_ceds[activities_ceds$fuel %in% "diesel_oil", X_years] / 
     heat_content[which(heat_content$fuel=='diesel_oil'),'heat_content']) * 10^-6
activities_ceds[activities_ceds$fuel %in% "heavy_oil", X_years] <- 
  (activities_ceds[activities_ceds$fuel %in% "heavy_oil", X_years] / 
     heat_content[which(heat_content$fuel=='heavy_oil'),'heat_content']) * 10^-6

activities_ceds[which(activities_ceds$units=='kJ'), 'units' ] <- 'kt'

# Convert mass units to kt
activities_ceds[which(activities_ceds$units=='Mt'), 
                X_years ] <- activities_ceds[which(activities_ceds$units=='Mt'), X_years ]*10^3
activities_ceds[which(activities_ceds$units=='Mt'), 'units' ] <- 'kt'

# ---------------------------------------------------------------------------
# 4. Interpolate Data through current year

activities_ceds_interp <- interpolateValues(activities_ceds)
emissions_ceds_interp <- interpolateValues(emissions_ceds)

years <- 2000:end_year
X_years <- paste0('X',years)

# ---------------------------------------------------------------------------
# 5. Seperate process data and Calculate Emission Factors

printLog('Calculating emission factors')

process_gainsEMF30 <- activities_ceds_interp[which(activities_ceds_interp$fuel=='process'),]
activities_ceds_comb <- activities_ceds_interp[activities_ceds_interp$fuel %!in% 'process',]

#combine activity and emissions
a <- melt(activities_ceds_comb, id.vars = c('iso','sector','fuel','units'))
e <- melt(emissions_ceds_interp, id.vars = c('iso','sector','fuel','units'))

combined <- merge(a, e, 
                  by = c('iso','sector','fuel','variable'),
                  suffixes = c(".a",".e") )
combined$EF <- combined$value.e/combined$value.a
combined$units <- paste0(combined$units.e,'/',combined$units.a)

combined <- combined[,c('iso','sector','fuel','units','variable','EF')]

# select years
combined <- combined[which(combined$variable %in% X_years),]

# remove Inf values
combined <- combined[which(is.finite(combined$EF)) , ]
# remove zero values
combined <- combined[which(combined$EF>0) , ]

combined <- cast(combined, iso + sector + fuel + units ~ variable,
                 value = 'EF')

gainsEMF30 <- combined[,c('iso','sector','fuel','units',X_years)]

# ---------------------------------------------------------------------------
# 6. Output

writeData(gainsEMF30, domain = "DEFAULT_EF_PARAM", fn = paste0('B.',em,'_GAINS_EMF30_EF'))
writeData(process_gainsEMF30, domain = "MED_OUT", fn = paste0('B.',em,'_GAINS_EMF30_process'))

logStop()
# END
