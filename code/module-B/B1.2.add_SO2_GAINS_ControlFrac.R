# Program Name: B1.2.add_SO2_GAINS_ControlFrac.R
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
# 0.5 Load Packages

loadPackage('zoo')

# ---------------------------------------------------------------------------
# 1. Reading data and mapppings into script

EUemiss  <- readData( "EM_INV", "GAINS_emiss_act_sect-EU28-2005-SO2_nohead")
EUfuel  <- readData( "EM_INV",  "GAINS_aen_act_sect-nohead-EU28" )

gainstoiso <- readData( "GAINS_MAPPINGS",  "GAINS_country_mapping" )
fuelmap <- readData(  "GAINS_MAPPINGS", "GAINS_fuel_mapping" )
sectormap <- readData( "GAINS_MAPPINGS",  "GAINS_sector_mapping" )

OECDconversion <- readData( "ENERGY_IN",  "IEA_energyconversion" )
NONconversion <- readData( "ENERGY_IN",  "IEA_NonOECDconversion" )

s_content <- readData( "MED_OUT", 'B.SO2_S_Content_db')

gains_ashret <- readData('MED_OUT', 'B.GAINS_SO2_AshRet_db')

# ---------------------------------------------------------------------------
# 2. GAINS multipliers (unit conversions from energy to mass)

# The IEA data provides conversions (heat content) for a variety of flows and fuels
# and for most countries. Take only EU countries and develop a conversion factor 
# by taking a weighted average of fuels brought together into CEDS fuels.
# Multiply the energy data from GAINS into kJ, then divide by the
# conversion factor to retrieve data in kg which is then multiplied by 1*10^-6 to put
# into kilotons.

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
  lapply(conversion[,3:ncol(conversion)], as.numeric)

# Define multipliers function
multipliers <- function(conversion, IEAfuels) {
  conversion <- conversion[,IEAfuels]
  unit_multiplier <- sum(colSums(conversion, na.rm = T)) / 
    length(na.omit(unlist(conversion)))
  return(unit_multiplier)
}

# diagnostic-output
writeData( conversion, domain = "DIAG_OUT", 
           fn = "B1.2.energy_conversion_factors") 

# Using the function and IEA fuels to create a unit conversion multiplier for
# the CEDS categories in the GAINS data. 

browncoal <- c("Sub.bituminous.coal", "Lignite")
mult_browncoal <- multipliers(conversion, browncoal)

hardcoal <- c("Anthracite", "Other.bituminous.coal")
mult_hardcoal <- multipliers(conversion, hardcoal)

biomass <- c("Biogasoline", "Biodiesels", "Other.liquid.biofuels", "Charcoal")
mult_biomass <- multipliers(conversion, biomass)

lightoil <- c("Motor.gasoline", "Aviation.gasoline","Gasoline.type.jet.fuel", 
              "Kerosene.type.jet.fuel", "Other.kerosene")
mult_lightoil <- multipliers(conversion, lightoil)

heavy_oil <- c("Crude.oil", "Fuel.oil")
mult_heavy_oil <- multipliers(conversion, heavy_oil)

# These have a single column of data and therefore the function is not necessary
NaturalGas <- c("Natural.gas.liquids")
mult_NaturalGas <- sum(conversion[,NaturalGas] ,na.rm = T)/
  length(na.omit(conversion[,NaturalGas]))

Diesel <- c("Gas.diesel.oil")
mult_Diesel <- sum(conversion[,Diesel] ,na.rm = T)/
  length(na.omit(conversion[,Diesel]))

# make list of calculated multipliers
GAINS_multipliers <- data.frame(fuel=c('brown_coal', 'hard_coal', 'biomass', 'light_oil', 
                                       'natural_gas', 'diesel_oil', 'heavy_oil'),
                                multiplier=c(mult_browncoal, mult_hardcoal, mult_biomass, mult_lightoil, 
                                             mult_NaturalGas, mult_Diesel, mult_heavy_oil))

# ---------------------------------------------------------------------------
# 2.0 Melting and combing same sectors and fuels in fuel consumption
# 2.0.1 If the data has a header (how it is downloaded from GAINS)
#colnames( EUfuel ) <- c( EUfuel[7,] )
#EUfuel <- EUfuel[-c(1:7), ]
#colnames( EUemiss ) <- c( EUemiss[7,] )
#EUemiss <- EUemiss[-c(1:7), ]

# 2.1 Preparing the data, changing n.a's into 0's and making numbers numeric
EUfuel[EUfuel == "n.a"] <- 0
EUfuel[,4:ncol(EUfuel)] <- lapply(EUfuel[,4:ncol(EUfuel)], as.numeric) 
EUemiss[EUemiss == "n.a"] <- 0
EUemiss[,4:ncol(EUemiss)] <- lapply(EUemiss[,4:ncol(EUemiss)], as.numeric)

# 2.2 Melting the data so that it is in long format instead of wide.
EUfuel <- melt(EUfuel, id.vars = c( "cou_abb", "reg_abb","sector.activity"))
EUemiss <- melt(EUemiss, id.vars = c( "cou_abb", "reg_abb", "Sector.Activity"))

# 2.3 Changing column names along with puting sectors and fuels into CEDS names
colnames(EUfuel) <- c("iso", "reg_abb", "Sector", "Fuel", "Energy")
EUfuel$Sector <- sectormap[match(EUfuel$Sector,sectormap$GAINS.Sectors),1]
EUfuel$Fuel <- fuelmap[match(EUfuel$Fuel,fuelmap$GAINS.fuel),1]
colnames(EUemiss) <- c("iso", "reg_abb", "Sector", "Fuel", "Sulfur_emiss")
EUemiss$Sector <- sectormap[match(EUemiss$Sector,sectormap$GAINS.Sectors),1]
EUemiss$Fuel <- fuelmap[match(EUemiss$Fuel,fuelmap$GAINS.fuel),1]

#Convert to isocode
EUemiss$iso <- tolower(gainstoiso$ISO.code[match(EUemiss$iso,gainstoiso$country)])
EUfuel$iso <- tolower(gainstoiso$ISO.code[match(EUfuel$iso,gainstoiso$country)])  

# 2.4 Aggregating and Creating Emissions Factors
EUfuel <- aggregate(EUfuel[c("Energy")], 
                    by = EUfuel[c("iso","Sector", "Fuel")], FUN=sum)
EUemiss <- aggregate(EUemiss[c("Sulfur_emiss")], 
                     by = EUemiss[c("iso","Sector", "Fuel")], FUN=sum)
EmissFactors <- merge(EUfuel,EUemiss, all.x = T, all.y = F)

# 2.5 Converting units of energy from PJ into kt
# Changing PetaJoules into kiloJoules
EmissFactors[, "Energy" ] <- EmissFactors[, "Energy" ] * 1*10^12

# Now we need to divide by our factor to get into kg. Then multiply into kt.
# Note, the 1*10^-6 is simply the conversion factor between kt and kg. We derived
# the previous conversion factor (from kJ into kg) from data from the IEA. 
EmissFactors[EmissFactors$Fuel %in% "brown_coal", "Energy"] <- 
  (EmissFactors[EmissFactors$Fuel %in% "brown_coal", "Energy"] / 
     GAINS_multipliers[which(GAINS_multipliers$fuel=='brown_coal'),'multiplier']) * 1*10^-6
EmissFactors[EmissFactors$Fuel %in% "hard_coal", "Energy"] <- 
  (EmissFactors[EmissFactors$Fuel %in% "hard_coal", "Energy"] / 
     GAINS_multipliers[which(GAINS_multipliers$fuel=='hard_coal'),'multiplier']) * 1*10^-6
EmissFactors[EmissFactors$Fuel %in% "biomass", "Energy"] <- 
  (EmissFactors[EmissFactors$Fuel %in% "biomass", "Energy"] / 
     GAINS_multipliers[which(GAINS_multipliers$fuel=='biomass'),'multiplier']) * 1*10^-6
EmissFactors[EmissFactors$Fuel %in% "light_oil", "Energy"] <- 
  (EmissFactors[EmissFactors$Fuel %in% "light_oil", "Energy"] / 
     GAINS_multipliers[which(GAINS_multipliers$fuel=='light_oil'),'multiplier']) * 1*10^-6
EmissFactors[EmissFactors$Fuel %in% "natural_gas", "Energy"] <- 
  (EmissFactors[EmissFactors$Fuel %in% "natural_gas", "Energy"] / 
     GAINS_multipliers[which(GAINS_multipliers$fuel=='natural_gas'),'multiplier']) * 1*10^-6
EmissFactors[EmissFactors$Fuel %in% "diesel_oil", "Energy"] <- 
  (EmissFactors[EmissFactors$Fuel %in% "diesel_oil", "Energy"] / 
     GAINS_multipliers[which(GAINS_multipliers$fuel=='diesel_oil'),'multiplier']) * 1*10^-6
EmissFactors[EmissFactors$Fuel %in% "heavy_oil", "Energy"] <- 
  (EmissFactors[EmissFactors$Fuel %in% "heavy_oil", "Energy"] / 
     GAINS_multipliers[which(GAINS_multipliers$fuel=='heavy_oil'),'multiplier']) * 1*10^-6

# 2.6 Creating Emission Factor for 2005
# The following sectors are not important to this information set, therefore remove
out <- c("Heat", "Electricity", "Hydro", "Hydrogen", "Nuclear", "Renewables", "")
EmissFactors <- EmissFactors[!EmissFactors$Fuel %in% out,]
EmissFactors$units <- c("NA")
EmissFactors$EF_2005 <- EmissFactors$Sulfur_emiss/EmissFactors$Energy
EmissFactors <- EmissFactors[,c("iso","Sector", "Fuel", "units", "EF_2005")]
EmissFactors <- EmissFactors[complete.cases(EmissFactors),]
EmissFactors <- EmissFactors[-which(EmissFactors$EF_2005=='Inf'),]
names(EmissFactors) <- c("iso","sector", "fuel", "units", "EF_2005")

#get rid of 
EmissFactors <- EmissFactors[-which(EmissFactors$EF_2005>10^4),]

# -------------------------------------------------------------------------------
# 3.Calculate GAINS control percentage using GAINS emission factors

Gains_EF  <- EmissFactors

names(Gains_EF) <- c('iso','sector','fuel','units','X2005')
countries <- unique(Gains_EF$iso)

# melt and combine default and gains data
base <- s_content[which(s_content$iso %in% countries), c('iso','sector','fuel','X2005')]
base$X2005 <- base$X2005*2

default <- join(base, gains_ashret, by=c('iso','sector', 'fuel'))
default$default_EF <- default$X2005*(1-default[,ncol(default)])
default <- default[!is.na(default$default_EF),]
default <- default[,c('iso',"sector",'fuel', 'units', 'default_EF')]
colnames(default)[which(names(default) == "default_EF")] <- 'X2005'

default.wide <- melt(default, c('iso','sector','fuel', 'units'), val='X2005')
names(default.wide)<-c('iso','sector','fuel','units','years','default')

gains.wide <- melt(Gains_EF, c('iso','sector','fuel','units'))
names(gains.wide)<-c('iso','sector','fuel','units','years','gains')

combined<-merge(gains.wide, default.wide, by=c('iso','sector','fuel','units','years'),
                all.x = TRUE, all.y=FALSE)


# Calculate Control Percent
combined$control_percent <- 1-combined$gains/combined$default
combined$control_percent[which(combined$control_percent<0)]<-0
combined <- combined[which(is.finite(combined$control_percent)),]

# Cast and reformat
control_percent <- cast(combined[,c('iso','sector','fuel','units','years','control_percent')],
                        iso+sector+fuel+units~years,
                        value = 'control_percent')
# Gard code formatting
control_percent$pre_ext_method <- 'linear_0'
control_percent$pre_ext_year <- 1960
control_percent$interp_method <- 'linear'
control_percent$post_ext_method <- 'constant'
control_percent$post_ext_year <- 2014
control_percent<- control_percent[c('iso','sector','fuel','units','pre_ext_method','pre_ext_year',
                                    'interp_method','post_ext_method','post_ext_year','X2005')]


# -------------------------------------------------------------------------------
# 3. Extrapolation/Interpolation

control_percent_values <- control_percent[,c('iso','sector','fuel','units','X2005')]
control_percent_interp_method <- control_percent[c('iso','sector','fuel','interp_method')]
control_percent_ext_method <- control_percent[c('iso','sector','fuel','pre_ext_method','post_ext_method')]
control_percent_ext_year <- control_percent[c('iso','sector','fuel','pre_ext_year','post_ext_year')]

control_percent_extended <- interpolateValues(control_percent_values, interp_method = control_percent_interp_method)

control_percent_extended <- extendValues(control_percent_extended, pre_ext_default = 'linear_0', 
                                      ext_method = control_percent_ext_method, ext_year = control_percent_ext_year)
if( identical(control_percent_extended$iso, control_percent$iso) &&
    identical(control_percent_extended$sector, control_percent$sector)){
  control_percent_extended <- cbind(control_percent[,c('iso','sector','fuel','units')],
                                 control_percent_extended[, names(control_percent_extended)[names(control_percent_extended) %!in% c('iso','sector','fuel','units')] ]) }

# -------------------------------------------------------------------------------
# 4. Add to parameter Db

addToDb_overwrite( new_data = control_percent_extended , em = 'SO2' , type = 'comb' , file_extention = "ControlFrac_db" )

# -------------------------------------------------------------------------------
# 5. Output

writeData( control_percent_extended, domain = "MED_OUT", fn = "B.GAINS_SO2_ControlFrac_db")


logStop()
# END
