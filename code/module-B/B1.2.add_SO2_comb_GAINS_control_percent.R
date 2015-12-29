# Program Name: B1.2.add_SO2_GAINS_control_percent.R
# Author: Ryan Bolt, Leyang
# Date Last Updated: 9 Nov 2015 
# Program Purpose: Process 2005 GAINS emissions and fuel to calculate GAINS EF, then calculate 
# 2005 GAINS control percentage. Combine 2005 GAINS control percentage to SO2 control percentage
# database in the end. 
# 
# Input Files: aen_act_sect-nohead-EU28.csv , emiss_act_sect-EU28-2005-SO2_nohead.csv,
#               
# Output Files: B.SO2_S_ControlFrac_db.csv
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
headers <- c( "data_functions.R", "analysis_functions.R",'process_db_functions.R',
              'common_data.r', 'IO_functions.R', 'data_functions.R', 'timeframe_functions.R',
              'interpolation_extention_functions.R') # Additional function files may be required.
log_msg <- "Aggregating EU GAINS Data" # First message to be printed to the log
script_name <- "B1.2.add_SO2_GAINS_ControlFrac.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 0.5 Load Packages, define functions

loadPackage('zoo')

# Define heat_contents function
calc_heat_content <- function(conversion, IEAfuels) {
  conversion <- conversion[,IEAfuels]
  unit_multiplier <- sum(colSums(conversion, na.rm = T)) / 
    length(na.omit(unlist(conversion)))
  return(unit_multiplier)
}

# ---------------------------------------------------------------------------
# 1. Reading data and mapppings into script

MCL <- readData( "MAPPINGS",  "Master_Country_List" )

EUemiss.import  <- readData( "EM_INV", "GAINS_emiss_act_sect-EU28-2005-SO2_nohead")
EUfuel.import  <- readData( "EM_INV",  "GAINS_aen_act_sect-nohead-EU28" )

gainstoiso <- readData( "GAINS_MAPPINGS",  "GAINS_country_mapping" )
fuelmap <- readData(  "GAINS_MAPPINGS", "GAINS_fuel_mapping" )
sectormap <- readData( "GAINS_MAPPINGS",  "GAINS_sector_mapping" )

OECDconversion <- readData( "ENERGY_IN",  "IEA_energyconversion" )
NONconversion <- readData( "ENERGY_IN",  "IEA_NonOECDconversion" )

s_content <- readData( "DEFAULT_EF_PARAM", "B.SO2_GAINS_s_content")

gains_ashret <- readData('DEFAULT_EF_PARAM', 'B.SO2_GAINS_s_ash_ret')

# ---------------------------------------------------------------------------
# 2. GAINS multipliers (unit conversions from energy to mass)

# The IEA data provides conversions (heat content) for a variety of flows and fuels
# and for most countries. Take only EU countries and develop a conversion factor 
# by taking a weighted average of fuels brought together into CEDS fuels.
# units of heat_content - kJ/kg


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

biomass <- c("Biogasoline", "Biodiesels", "Other.liquid.biofuels", "Charcoal")
mult_biomass <- calc_heat_content(conversion, biomass)

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

# diagnostic-output
writeData( GAINS_heat_content, domain = "MED_OUT", 
           fn = "B1.2.heat_content") 

# ---------------------------------------------------------------------------
# 2.0 Melting and combing same sectors and fuels in fuel consumption
# 2.0.1 If the data has a header (how it is downloaded from GAINS)

EUfuel <- EUfuel.import
EUemiss <- EUemiss.import

# 2.1 Preparing the data, changing n.a's into 0's and making numbers numeric
EUfuel[EUfuel == "n.a"] <- 0
EUfuel[,4:ncol(EUfuel)] <- lapply(EUfuel[,4:ncol(EUfuel)], as.numeric) 
EUemiss[EUemiss == "n.a"] <- 0
EUemiss[,4:ncol(EUemiss)] <- lapply(EUemiss[,4:ncol(EUemiss)], as.numeric)

# 2.2 Melting the data so that it is in long format instead of wide.
EUfuel <- melt(EUfuel, id.vars = c( "cou_abb", "reg_abb","sector.activity"))
EUemiss <- melt(EUemiss, id.vars = c( "cou_abb", "reg_abb", "Sector.Activity"))

# 2.3 Changing column names along with puting sectors and fuels into CEDS names
colnames(EUfuel) <- c("iso", "reg_abb", "sector", "fuel", "Energy")
EUfuel$fuel <- fuelmap[match(EUfuel$fuel,fuelmap$GAINS.fuel),1]
EUfuel <- mapCEDS_sector_fuel( mapping_data = EUfuel,
                                     mapping_file = sectormap,
                                     data_match_col = 'sector',
                                     map_match_col = 'GAINS.sectors',
                                     map_merge_col = c('detailed_sectors'),
                                     new_col_names = c('sector'),
                                     level_map_in = 'detailed_sectors',
                                     level_out = 'working_sectors_v1',
                                     aggregate = TRUE,
                                     aggregate_col = c('Energy'),
                                     oneToOne = FALSE,
                                     agg.fun = sum)


colnames(EUemiss) <- c("iso", "reg_abb", "sector", "fuel", "Sulfur_emiss")
EUemiss$fuel <- fuelmap[match(EUemiss$fuel,fuelmap$GAINS.fuel),1]
EUemiss <- mapCEDS_sector_fuel( mapping_data = EUemiss,
                               mapping_file = sectormap,
                               data_match_col = 'sector',
                               map_match_col = 'GAINS.sectors',
                               map_merge_col = c('detailed_sectors'),
                               new_col_names = c('sector'),
                               level_map_in = 'detailed_sectors',
                               level_out = 'working_sectors_v1',
                               aggregate = TRUE,
                               aggregate_col = c('Sulfur_emiss'),
                               oneToOne = FALSE,
                               agg.fun = sum)

#Convert to isocode
EUemiss$iso <- tolower(gainstoiso$ISO.code[match(EUemiss$iso,gainstoiso$country)])
EUfuel$iso <- tolower(gainstoiso$ISO.code[match(EUfuel$iso,gainstoiso$country)])  

# 2.4 Aggregating and combine fuel and emissions data Emissions Factors
EUfuel <- aggregate(EUfuel[c("Energy")], 
                    by = EUfuel[c("iso","sector", "fuel")], FUN=sum)
EUemiss <- aggregate(EUemiss[c("Sulfur_emiss")], 
                     by = EUemiss[c("iso","sector", "fuel")], FUN=sum)
EmissFactors <- merge(EUfuel,EUemiss, all.x = T, all.y = F)

# 2.5 Converting units of energy from PJ into kt
# Convert PetaJoules into kiloJoules
EmissFactors[, "Energy" ] <- EmissFactors[, "Energy" ] * 10^12

# Convert GAINS energy to kg by dividing by GAINS_heat_content
# then multiply by 10^-6 to convert to kt
EmissFactors[EmissFactors$fuel %in% "brown_coal", "Energy"] <- 
  (EmissFactors[EmissFactors$fuel %in% "brown_coal", "Energy"] / 
     GAINS_heat_content[which(GAINS_heat_content$fuel=='brown_coal'),'heat_content']) * 10^-6
EmissFactors[EmissFactors$fuel %in% "hard_coal", "Energy"] <- 
  (EmissFactors[EmissFactors$fuel %in% "hard_coal", "Energy"] / 
     GAINS_heat_content[which(GAINS_heat_content$fuel=='hard_coal'),'heat_content']) * 10^-6
EmissFactors[EmissFactors$fuel %in% "biomass", "Energy"] <- 
  (EmissFactors[EmissFactors$fuel %in% "biomass", "Energy"] / 
     GAINS_heat_content[which(GAINS_heat_content$fuel=='biomass'),'heat_content']) * 10^-6
EmissFactors[EmissFactors$fuel %in% "light_oil", "Energy"] <- 
  (EmissFactors[EmissFactors$fuel %in% "light_oil", "Energy"] / 
     GAINS_heat_content[which(GAINS_heat_content$fuel=='light_oil'),'heat_content']) * 10^-6
EmissFactors[EmissFactors$fuel %in% "natural_gas", "Energy"] <- 
  (EmissFactors[EmissFactors$fuel %in% "natural_gas", "Energy"] / 
     GAINS_heat_content[which(GAINS_heat_content$fuel=='natural_gas'),'heat_content']) * 10^-6
EmissFactors[EmissFactors$fuel %in% "diesel_oil", "Energy"] <- 
  (EmissFactors[EmissFactors$fuel %in% "diesel_oil", "Energy"] / 
     GAINS_heat_content[which(GAINS_heat_content$fuel=='diesel_oil'),'heat_content']) * 10^-6
EmissFactors[EmissFactors$fuel %in% "heavy_oil", "Energy"] <- 
  (EmissFactors[EmissFactors$fuel %in% "heavy_oil", "Energy"] / 
     GAINS_heat_content[which(GAINS_heat_content$fuel=='heavy_oil'),'heat_content']) * 10^-6

# 2.6 Creating Emission Factor for 2005
# The following sectors are not important to this information set, therefore remove
out <- c("Heat", "Electricity", "Hydro", "Hydrogen", "Nuclear", "Renewables", "")
EmissFactors <- EmissFactors[!EmissFactors$fuel %in% out,]
EmissFactors$units <- c("kt/kt")
EmissFactors$EF_2005 <- EmissFactors$Sulfur_emiss/EmissFactors$Energy
EmissFactors <- EmissFactors[,c("iso","sector", "fuel", "units", "EF_2005")]
EmissFactors <- EmissFactors[complete.cases(EmissFactors),]
EmissFactors <- EmissFactors[-which(EmissFactors$EF_2005=='Inf'),]
names(EmissFactors) <- c("iso","sector", "fuel", "units", "EF_2005")

# keep non zero EF
EmissFactors <- EmissFactors[which(EmissFactors$EF_2005 > 0),]

#get rid of very large EF
EmissFactors <- EmissFactors[which(EmissFactors$EF_2005 < 10^4),]

# -------------------------------------------------------------------------------
# 3.Calculate GAINS control percentage using GAINS emission factors

Gains_EF  <- EmissFactors

names(Gains_EF) <- c('iso','sector','fuel','units','X2005')
countries <- unique(Gains_EF$iso)

# Calculate Default emissions factors (pre control) with base EF and ash retention
base <- s_content[, c('iso','sector','fuel','X2005')]
base$X2005 <- base$X2005*2
names(base) <- c('iso','sector','fuel','X2005base')

default <- merge(gains_ashret, base, all.x = TRUE, all.y = FALSE)
default$default_EF <- default$X2005base*(1-default$X2005)
default <- default[!is.na(default$default_EF),]
default$units <- 'kt/kt'
default <- default[,c('iso',"sector",'fuel', 'units', 'default_EF')]
colnames(default)[which(names(default) == "default_EF")] <- 'X2005'

default.long <- melt(default, c('iso','sector','fuel', 'units'), val='X2005')
names(default.long)<-c('iso','sector','fuel','units','years','default')

gains.long <- melt(Gains_EF, c('iso','sector','fuel','units'))
names(gains.long)<-c('iso','sector','fuel','units','years','gains')

combined.all<-merge(gains.long, default.long, by=c('iso','sector','fuel','units','years'),
                all.x = TRUE, all.y=TRUE)

# Calculate Control Percent
combined.all$control_percent <- 1-combined.all$gains/combined.all$default
combined <- combined.all[which(combined.all$control_percent > 0),]
combined <- combined[which(combined$control_percent <= 1),]
combined$units <- 'percent' 

# Cast and reformat
control_percent <- cast(combined[,c('iso','sector','fuel','units','years','control_percent')],
                        iso+sector+fuel+units~years,
                        value = 'control_percent')

# -------------------------------------------------------------------------------
# 4. Prepare for automated addition to ContFrac_db in other mod B scripts.
#  Define extention options.

# Split eastern and western europe

west <- MCL[which(MCL$IEA_Fert_reg == 'Western Europe'),'iso']

control_percent$pre_ext_method <- 'linear_0'
control_percent$pre_ext_year <- '1990'
control_percent[control_percent$iso %in% west, 'pre_ext_year' ] <- '1980'

# -------------------------------------------------------------------------------
# 5. Output

writeData( control_percent, domain = "DEFAULT_EF_PARAM", fn = "B.SO2_GAINS_control_percent")

logStop()
# END
