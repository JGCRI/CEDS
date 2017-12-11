#------------------------------------------------------------------------------
# Program Name: D1.US_EPA_total_emissions_Totals_check.R
# Author: Cecilia Moura
# Date Last Updated: September 10, 2015
# Program Purpose:
#   Calculates air pollutant emission totals for CO, NOx, PM10, M2.5, SO2, VOC, NH3, CO2 and HAP-VOC.
#   Emissions are calculated for each of the 4 EPA data categories, and consistency checks are done.
#   HAP-VOCs emissions are extracted from the list of all pollutant emissions, based on EPA definition of HAP-VOC pollutants.
#   An emissions table is created.
#
# Input Files:
#   onroad_emissions_state,nonroad_emissions_state,point_emissions_state, nonpoint_emissions_state
#   onroad_emissions_US,nonroad_emissions_US,point_emissions_US, nonpoint_emissions_US
#   point_emissions_tribe, nonpoint_emissions_tribe
#   all_emissions_state,all_emissions_US,all_emissions_tribe
#   List_HAP_VOC.csv  (created from EPA site http://www.epa.gov/ttn/chief/net/2011inventory.html,
#   from section entitled "Sector Summaries - Criteria and Hazardous Air Pollutants by 60 EIS emission sector")
# Output Files: Emission_totals.csv
# Notes:
#
# TODO:
# 1 - Unit checking is incomplete.
# 2 - The Emissions_total file was used for further calculations, but the original file was not changed. The additional work,
# which was in progress, based on the Emissions_total results, can be found in the file Summary_emission_totals_CM.csv.
# 3 - Checking and diagnostics need to be improved.
# 4- For an unknown reason, row names were not written out to the Emissions_totals.csv file. Note that the data frame from
# which it is written (totals) does have row names.
#
# -----------------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Set working directory to the CEDS input�� directory and define PARAM_DIR as the
# location of the CEDS parameters � directory, relative to the new working directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R" ) # Any additional function files required
log_msg <- "short description of code" # First message to be printed to the log
script_name <- "D1.US_EPA_total_emissions_Totals_check.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# Read in emissions files

onroad_emissions_state <- readData("US_EPA_MED_OUT","onroad_emissions_state")
nonroad_emissions_state <- readData("US_EPA_MED_OUT","nonroad_emissions_state")
point_emissions_state <- readData("US_EPA_MED_OUT","point_emissions_state")
nonpoint_emissions_state <- readData("US_EPA_MED_OUT","nonpoint_emissions_state")

onroad_emissions_US <- readData("US_EPA_MED_OUT","onroad_emissions_US")
nonroad_emissions_US <- readData("US_EPA_MED_OUT","nonroad_emissions_US")
point_emissions_US <- readData("US_EPA_MED_OUT","point_emissions_US")
nonpoint_emissions_US <- readData("US_EPA_MED_OUT","nonpoint_emissions_US")

point_emissions_tribe <- readData("US_EPA_MED_OUT","point_emissions_tribe")
nonpoint_emissions_tribe <- readData("US_EPA_MED_OUT","nonpoint_emissions_tribe")

all_emissions_state <- readData("US_EPA_MED_OUT","all_emissions_state")
all_emissions_US <- readData("US_EPA_MED_OUT","all_emissions_US")
all_emissions_tribe <- readData("US_EPA_MED_OUT","all_emissions_tribe")


# Get total US CO emissions

CO<-subset(all_emissions_US,all_emissions_US$pollutant=="Carbon Monoxide")
sum(CO$emissions)

# Get unit for this pollutant
CO_unit<-unique(CO$unit)
if(length(CO_unit)!=1) print("Error: More than one unit for CO")

CO_onroad<-subset(onroad_emissions_US,onroad_emissions_US$pollutant=="Carbon Monoxide")
CO_onroad_total <-sum(CO_onroad$emissions)

CO_nonroad<-subset(nonroad_emissions_US,nonroad_emissions_US$pollutant=="Carbon Monoxide")
CO_nonroad_total <-sum(CO_nonroad$emissions)

CO_point<-subset(point_emissions_US,point_emissions_US$pollutant=="Carbon Monoxide")
CO_point_total<-sum(CO_point$emissions)

CO_nonpoint<-subset(nonpoint_emissions_US,nonpoint_emissions_US$pollutant=="Carbon Monoxide")
CO_nonpoint_total<-sum(CO_nonpoint$emissions)

CO_total <-CO_onroad_total+CO_nonroad_total+CO_point_total+CO_nonpoint_total

diff<-sum(CO$emissions)-CO_total
if(diff>0.001) print("Error: Sum of CO emissions by NEI area onroad nonroad etc  is not equal to national total")

# -----------------------------------------------------------------------------------------------------------
#Get total CO point emissions by state and tribe totals

CO_point_tribe<-subset(point_emissions_tribe,point_emissions_tribe$pollutant=="Carbon Monoxide")
CO_point_tribe_total<-sum(CO_point_tribe$emissions)

CO_point_state<-subset(point_emissions_state,point_emissions_state$pollutant=="Carbon Monoxide")
CO_point_state_total<-sum(CO_point_state$emissions)

diff<-CO_point_total-CO_point_tribe_total-CO_point_state_total
if(diff>0.001) print("Error: Sum of state and tribal CO emissions (point emissions) is not equal to national total)

# -----------------------------------------------------------------------------------------------------------
#Get total CO emissions for specific sector - Highway Vehicles and electricity generation

CO_HighwayVehicles<-subset(all_emissions_US,all_emissions_US$pollutant=="Carbon Monoxide" & all_emissions_US$Tier_1_Description=="Highway Vehicles")
sum(CO_HighwayVehicles$emissions)

CO_NFR_1A1a<-subset(all_emissions_US,all_emissions_US$pollutant=="Carbon Monoxide" & all_emissions_US$NFR=="1A1a")
sum(CO_NFR_1A1a$emissions)

# -----------------------------------------------------------------------------------------------------------
# Get total NOx emissions

NOx<-subset(all_emissions_US,all_emissions_US$pollutant=="Nitrogen Oxides")
sum(NOx$emissions)

NOx_unit<-unique(NOx$unit)  # Get unit for this pollutant
if(length(NOx_unit)!=1) print("Error: More than one unit for NOx")

NOx_onroad<-subset(onroad_emissions_US,onroad_emissions_US$pollutant=="Nitrogen Oxides")
NOx_onroad_total <-sum(NOx_onroad$emissions)

NOx_nonroad<-subset(nonroad_emissions_US,nonroad_emissions_US$pollutant=="Nitrogen Oxides")
NOx_nonroad_total <-sum(NOx_nonroad$emissions)

NOx_point<-subset(point_emissions_US,point_emissions_US$pollutant=="Nitrogen Oxides")
NOx_point_total<-sum(NOx_point$emissions)

NOx_nonpoint<-subset(nonpoint_emissions_US,nonpoint_emissions_US$pollutant=="Nitrogen Oxides")
NOx_nonpoint_total<-sum(NOx_nonpoint$emissions)

NOx_total <-NOx_onroad_total+NOx_nonroad_total+NOx_point_total+NOx_nonpoint_total

diff<-sum(NOx$emissions)-NOx_total
if(diff>0.001) print("Error: Sum of NOx emissions by NEI area onroad nonroad etc  is not equal to national total")

# -----------------------------------------------------------------------------------------------------------
# Get total PM10 Primary emissions

PM10_Primary<-subset(all_emissions_US,all_emissions_US$pollutant=="PM10 Primary (Filt + Cond)")
sum(PM10_Primary$emissions)

PM10_Primary_unit<-unique(PM10_Primary$unit) # Get unit for this pollutant
if(length(PM10_Primary_unit)!=1) print("Error: More than one unit for PM10_Primary")

PM10_Primary_onroad<-subset(onroad_emissions_US,onroad_emissions_US$pollutant=="PM10 Primary (Filt + Cond)")
PM10_Primary_onroad_total <-sum(PM10_Primary_onroad$emissions)

PM10_Primary_nonroad<-subset(nonroad_emissions_US,nonroad_emissions_US$pollutant=="PM10 Primary (Filt + Cond)")
PM10_Primary_nonroad_total <-sum(PM10_Primary_nonroad$emissions)

PM10_Primary_point<-subset(point_emissions_US,point_emissions_US$pollutant=="PM10 Primary (Filt + Cond)")
PM10_Primary_point_total<-sum(PM10_Primary_point$emissions)

PM10_Primary_nonpoint<-subset(nonpoint_emissions_US,nonpoint_emissions_US$pollutant=="PM10 Primary (Filt + Cond)")
PM10_Primary_nonpoint_total<-sum(PM10_Primary_nonpoint$emissions)

PM10_Primary_total <-PM10_Primary_onroad_total+PM10_Primary_nonroad_total+PM10_Primary_point_total+PM10_Primary_nonpoint_total

# -----------------------------------------------------------------------------------------------------------
# Get total PM2.5 Primary emissions

PM2.5_Primary<-subset(all_emissions_US,all_emissions_US$pollutant=="PM2.5 Primary (Filt + Cond)")
sum(PM2.5_Primary$emissions)

PM2.5_Primary_unit<-unique(PM2.5_Primary$unit) # Get unit for this pollutant
if(length(PM2.5_Primary_unit)!=1) print("Error: More than one unit for PM2.5_Primary")

PM2.5_Primary_onroad<-subset(onroad_emissions_US,onroad_emissions_US$pollutant=="PM2.5 Primary (Filt + Cond)")
PM2.5_Primary_onroad_total <-sum(PM2.5_Primary_onroad$emissions)

PM2.5_Primary_nonroad<-subset(nonroad_emissions_US,nonroad_emissions_US$pollutant=="PM2.5 Primary (Filt + Cond)")
PM2.5_Primary_nonroad_total <-sum(PM2.5_Primary_nonroad$emissions)

PM2.5_Primary_point<-subset(point_emissions_US,point_emissions_US$pollutant=="PM2.5 Primary (Filt + Cond)")
PM2.5_Primary_point_total<-sum(PM2.5_Primary_point$emissions)

PM2.5_Primary_nonpoint<-subset(nonpoint_emissions_US,nonpoint_emissions_US$pollutant=="PM2.5 Primary (Filt + Cond)")
PM2.5_Primary_nonpoint_total<-sum(PM2.5_Primary_nonpoint$emissions)

PM2.5_Primary_total <-PM2.5_Primary_onroad_total+PM2.5_Primary_nonroad_total+PM2.5_Primary_point_total+PM2.5_Primary_nonpoint_total



# -----------------------------------------------------------------------------------------------------------
# Get total Sulfur Dioxide emissions

SO2<-subset(all_emissions_US,all_emissions_US$pollutant=="Sulfur Dioxide")
sum(SO2$emissions)

SO2_unit<-unique(SO2$unit) # Get unit for this pollutant
if(length(SO2_unit)!=1) print("Error: More than one unit for SO2")

SO2_onroad<-subset(onroad_emissions_US,onroad_emissions_US$pollutant=="Sulfur Dioxide")
SO2_onroad_total <-sum(SO2_onroad$emissions)

SO2_nonroad<-subset(nonroad_emissions_US,nonroad_emissions_US$pollutant=="Sulfur Dioxide")
SO2_nonroad_total <-sum(SO2_nonroad$emissions)

SO2_point<-subset(point_emissions_US,point_emissions_US$pollutant=="Sulfur Dioxide")
SO2_point_total<-sum(SO2_point$emissions)

SO2_nonpoint<-subset(nonpoint_emissions_US,nonpoint_emissions_US$pollutant=="Sulfur Dioxide")
SO2_nonpoint_total<-sum(SO2_nonpoint$emissions)

SO2_total <-SO2_onroad_total+SO2_nonroad_total+SO2_point_total+SO2_nonpoint_total


# -----------------------------------------------------------------------------------------------------------
# Get total VOC emissions

VOC<-subset(all_emissions_US,all_emissions_US$pollutant=="Volatile Organic Compounds")
sum(VOC$emissions)

VOC_unit<-unique(VOC$unit) # Get unit for this pollutant
if(length(VOC_unit)!=1) print("Error: More than one unit for VOC")

VOC_onroad<-subset(onroad_emissions_US,onroad_emissions_US$pollutant=="Volatile Organic Compounds")
VOC_onroad_total <-sum(VOC_onroad$emissions)

VOC_nonroad<-subset(nonroad_emissions_US,nonroad_emissions_US$pollutant=="Volatile Organic Compounds")
VOC_nonroad_total <-sum(VOC_nonroad$emissions)

VOC_point<-subset(point_emissions_US,point_emissions_US$pollutant=="Volatile Organic Compounds")
VOC_point_total<-sum(VOC_point$emissions)

VOC_nonpoint<-subset(nonpoint_emissions_US,nonpoint_emissions_US$pollutant=="Volatile Organic Compounds")
VOC_nonpoint_total<-sum(VOC_nonpoint$emissions)

VOC_total <-VOC_onroad_total+VOC_nonroad_total+VOC_point_total+VOC_nonpoint_total

# -----------------------------------------------------------------------------------------------------------
# Get total NH3 emissions

NH3<-subset(all_emissions_US,all_emissions_US$pollutant=="Ammonia")
sum(NH3$emissions)

NH3_unit<-unique(NH3$unit) # Get unit for this pollutant
if(length(NH3_unit)!=1) print("Error: More than one unit for NH3")

NH3_onroad<-subset(onroad_emissions_US,onroad_emissions_US$pollutant=="Ammonia")
NH3_onroad_total <-sum(NH3_onroad$emissions)

NH3_nonroad<-subset(nonroad_emissions_US,nonroad_emissions_US$pollutant=="Ammonia")
NH3_nonroad_total <-sum(NH3_nonroad$emissions)

NH3_point<-subset(point_emissions_US,point_emissions_US$pollutant=="Ammonia")
NH3_point_total<-sum(NH3_point$emissions)

NH3_nonpoint<-subset(nonpoint_emissions_US,nonpoint_emissions_US$pollutant=="Ammonia")
NH3_nonpoint_total<-sum(NH3_nonpoint$emissions)

NH3_total <-NH3_onroad_total+NH3_nonroad_total+NH3_point_total+NH3_nonpoint_total

# Get total CO2 emissions

CO2<-subset(all_emissions_US,all_emissions_US$pollutant=="Carbon Dioxide")
sum(CO2$emissions)

CO2_unit<-unique(CO2$unit) # Get unit for this pollutant
if(length(CO2_unit)!=1) print("Error: More than one unit for CO2")

CO2_onroad<-subset(onroad_emissions_US,onroad_emissions_US$pollutant=="Carbon Dioxide")
CO2_onroad_total <-sum(CO2_onroad$emissions)

CO2_nonroad<-subset(nonroad_emissions_US,nonroad_emissions_US$pollutant=="Carbon Dioxide")
CO2_nonroad_total <-sum(CO2_nonroad$emissions)

CO2_point<-subset(point_emissions_US,point_emissions_US$pollutant=="Carbon Dioxide")
CO2_point_total<-sum(CO2_point$emissions)

CO2_nonpoint<-subset(nonpoint_emissions_US,nonpoint_emissions_US$pollutant=="Carbon Dioxide")
CO2_nonpoint_total<-sum(CO2_nonpoint$emissions)

CO2_total <-CO2_onroad_total+CO2_nonroad_total+CO2_point_total+CO2_nonpoint_total



# Extract HAP-VOCs from all pollutant list

HAP_VOC <- readData("US_EPA_IN_EM_INV","List_HAP_VOC")
all_pollutants<-data.frame(unique(all_emissions_US$pollutant))
colnames(all_pollutants)[which(names(all_pollutants) == "unique.all_emissions_US.pollutant.")] <- "pollutant"

Num_HAP_VOC <- nrow(list_HAP_VOC) # 157
Num_all_pollutants <- nrow(list_all_pollutants) # 288

# subdata<-subset(list_all_pollutants,list_all_pollutants[,1]!=list_HAP_VOC[,1]) #why doesn't this work?

# Pollutants which are not HAP-VOCs (288-153=135)
all_but_HAP_VOC <- data.frame(all_pollutants[!(all_pollutants$pollutants %in% HAP_VOC$HAP.VOC),])

# HAP-VOcs listed in all_pollutants (153).
HAP_VOC_short <- data.frame(intersect(all_pollutants[,1],HAP_VOC[,1]))  #153
colnames(HAP_VOC_short)[which(names(HAP_VOC_short) == "intersect.all_pollutants...1...HAP_VOC...1..")] <- "HAP.VOC"

# Note that there are 4 HAP-VOCs which are not listed in all-pollutants (157-153)
HAP_VOC_extra <- data.frame(HAP_VOC[!(HAP_VOC$HAP.VOC %in% HAP_VOC_short),])

# Sum HAP_VOC emissions
v <- match(HAP_VOC_short$HAP.VOC,all_emissions_US$pollutant,nomatch = NA_integer_)
HAP_VOC_short$emissions<-all_emissions_US$emissions[v]
HAP_VOC_total <- sum(HAP_VOC_short$emissions)


# Create emissions table

CO <- c(CO_unit,CO_onroad_total,CO_nonroad_total,CO_point_total,CO_nonpoint_total)
NOx <- c(NOx_unit,NOx_onroad_total,NOx_nonroad_total,NOx_point_total,NOx_nonpoint_total)
PM10_Primary <- c(PM10_Primary_unit,PM10_Primary_onroad_total,PM10_Primary_nonroad_total,PM10_Primary_point_total,PM10_Primary_nonpoint_total)
PM2.5_Primary <- c(PM2.5_Primary_unit,PM2.5_Primary_onroad_total,PM2.5_Primary_nonroad_total,PM2.5_Primary_point_total,PM2.5_Primary_nonpoint_total)
SO2 <- c(SO2_unit,SO2_onroad_total,SO2_nonroad_total,SO2_point_total,SO2_nonpoint_total)
VOC <- c(VOC_unit,VOC_onroad_total,VOC_nonroad_total,VOC_point_total,VOC_nonpoint_total)
NH3 <- c(NH3_unit,NH3_onroad_total,NH3_nonroad_total,NH3_point_total,NH3_nonpoint_total)
CO2 <- c(CO2_unit,CO2_onroad_total,CO2_nonroad_total,CO2_point_total,CO2_nonpoint_total)
# HAP_VOC <- c()
totals <- data.frame(CO,NOx,PM10_Primary,PM2.5_Primary,SO2,VOC,NH3,CO2)
row.names(totals)<-c("unit","onroad", "nonroad", "point","nonpoint")
writeData(totals,"US_EPA_MED_OUT","Emission_totals" )  #Why are row names not being written out?
