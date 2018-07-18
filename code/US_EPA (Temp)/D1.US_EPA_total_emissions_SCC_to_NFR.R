#------------------------------------------------------------------------------
# Program Name: D1.US_EPA_total_emissions_SCC_to_NFR.R
# Author: Cecilia Moura
# Date Last Updated: August 10, 2015
# Program Purpose:  This program creates two aggregated emission files, one at the state level
#                   and one at the national level, for air pollutants listed in the version 2
#                   of the EPA 2011 National Emission Inventory (NEI). It is the first script in a series
#                   of scripts that processes NEI emissions data for the CEDS project.
#                   The code aggregates emissions by unique EPA Source Classification Codes (SCC).
#                   The European NFR categories, obtained from an EPA SCC-NFR mapping,
#                   is mapped for each SCC.
#                   The 2 emission files produced here are further processed by the next script,
#                   where the NFR categories are mapped to the CEDS categories.
# Input Files:      EPA emission files are grouped into 4 categories:
#                   1. onroad: onroad_4.csv, onroad_5.csv, onroad_67.csv,onroad_123.csv,
#                     onroad_8910.csv
#                   2. nonroad: nonroad_4.csv, nonroad_5.csv, nonroad_67.csv,nonroad_123.csv,
#                     nonroad_8910.csv
#                   3. point: process_12345.csv, process_678910.csv
#                   4. nonpoint: 2011v2_nonpoint.csv
#                   The above files can be accessed in:
#                   http://www.epa.gov/ttn/chief/net/2011inventory.html
#                   The mapping file was provided by EPA in personal communication:
#                   5.SCCactiveJul2015_NFRmap_original.xlsx
#                   A list of additional SCCs was provided by EPA in personal communication:
#                   6.NEIv2_new_sccs_2011.xlsx
# Output Files: Xwalk.csv, all_emissions_state.csv, all_emissions_US.csv
# Notes:  1.All input and output files relating to this script are classified in several folders caled US_EPA:
#           ..\input\emissions-inventories\US_EPA (raw emissions data)
#           ..\input\mappings\US_EPA (raw mapping data and processed mapping files, called Xwalks)
#           ..\input\intermediate-output\US_EPA
#           ..\input\final-emissions\US_EPA
#           ..\code\US_EPA
#         2.Files are first bound for each category, resulting in one file for each of the
#           EPA emission categories. To produce state-level emissions, the code aggregates
#           the 4 files by state, SCC and pollutant. To produce US-level emissions,
#           it aggregates by SCC and pollutant.
#         3.Each row of the resulting aggregated files, which corresponds to a sector described
#           by an SCC, is then matched with its corresponding NFR code, listed in the SCC-NFR
#           mapping file. The aggregated files are then bound together,
#           producing one state-level and one US-level file.
#         4.Since EPA emission  files are very large, special care must be taken to clear
#           the working memory.
#         5.To check emissions, go to EPA trends data:
#           http://www.epa.gov/ttn/chief/trends/index.html
#
# TODO:
#
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Set working directory to the CEDS input directory and define PARAM_DIR as the
# location of the CEDS parameters directory, relative to the new working directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R" ) # Any additional function files required
log_msg <- "short description of code" # First message to be printed to the log
script_name <- "D1.US_EPA_total_emissions_SCC_to_NFR.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )



# ------------------------------------------------------------------------------
#1. Read mappings file; create dataframe from SCC-NFR crosswalk file
# (latest - sent by Lee Tooly July 1, 2015)

# The file read here maps SCC to NFR09. Two changes were done manually to prepare file: Blank spaces were removed from column
# titles and SCCs originally in string format were reformatted as numbers.
Xwalk_onetab <- readData("US_EPA_IN_MAPPINGS", "SCCactiveJul2015_NFRmap", ".xlsx", sheet_selection = "SCCsActiveJul2015_NFRmap", meta=F)

# Create dataframe with columns of interest (B-SCC code number; E-Fuel according to SCC;
# M-Fuel according to NFR; N-NFR subfuel; V-NFR code number)
Xwalk <-select(Xwalk_onetab,Code,SCC_Level_One,SCC_Level_Two, SCC_Level_Three, Tier_1_Description,Tier_2_Description,Tier_3_Description,NFR_Code)

# The file read here maps NFR09 to NFR14 (as well as to CEDS sectors, used only in next script).
Xwalk_NFR09_NFR14<- readData("US_EPA_IN_MAPPINGS","Xwalk_NFR09_NFR14",".xlsx",meta=F )

# Write to file and clean up
writeData(Xwalk, "US_EPA_MED_OUT", "Xwalk")
rm(Xwalk_onetab)

# This file being read in has additional SCCs, but no associated NFRs:
New_sccs<-readData("US_EPA_IN_MAPPINGS", "NEIv2_new_sccs_2011", ".xlsx", meta = F )

# ------------------------------------------------------------------------------
# 2.Read emission files from EPA v2; create small dataframes with 5 columns of interest
# (state,SCC,description of pollutant, emissions, unit); bind when there is more than one region

# 2a. Read and bind Onroad emissions - for 5 regions

onroad4_emissions<- readData("US_EPA_IN_EM_INV","onroad_4",meta=F ) %>% select(tribal_name,st_usps_cd,scc,description,total_emissions,uom)
onroad5_emissions<- readData("US_EPA_IN_EM_INV","onroad_5",meta=F ) %>% select(tribal_name,st_usps_cd,scc,description,total_emissions,uom)
onroad67_emissions<- readData("US_EPA_IN_EM_INV","onroad_67",meta=F ) %>% select(tribal_name,st_usps_cd,scc,description,total_emissions,uom)
onroad123_emissions<- readData("US_EPA_IN_EM_INV","onroad_123",meta=F ) %>% select(tribal_name,st_usps_cd,scc,description,total_emissions,uom)
onroad8910_emissions<- readData("US_EPA_IN_EM_INV","onroad_8910",meta=F ) %>% select(tribal_name,st_usps_cd,scc,description,total_emissions,uom)

onroad_emissions<- bind_rows(onroad4_emissions,onroad5_emissions,onroad67_emissions,onroad123_emissions,onroad8910_emissions)
rm(onroad4_emissions,onroad5_emissions,onroad67_emissions,onroad123_emissions,onroad8910_emissions)

# 2b. Read and bind Nonroad emissions- for 5 regions

nonroad4_emissions<- readData("US_EPA_IN_EM_INV","nonroad_4",meta=F ) %>% select(tribal_name,st_usps_cd,scc,description,total_emissions,uom)
nonroad5_emissions<- readData("US_EPA_IN_EM_INV","nonroad_5",meta=F ) %>% select(tribal_name,st_usps_cd,scc,description,total_emissions,uom)
nonroad67_emissions<- readData("US_EPA_IN_EM_INV","nonroad_67",meta=F ) %>% select(tribal_name,st_usps_cd,scc,description,total_emissions,uom)
nonroad123_emissions<- readData("US_EPA_IN_EM_INV","nonroad_123",meta=F ) %>% select(tribal_name,st_usps_cd,scc,description,total_emissions,uom)
nonroad8910_emissions<- readData("US_EPA_IN_EM_INV","nonroad_8910",meta=F ) %>% select(tribal_name,st_usps_cd,scc,description,total_emissions,uom)

nonroad_emissions<- bind_rows(nonroad4_emissions,nonroad5_emissions,nonroad67_emissions,nonroad123_emissions,nonroad8910_emissions)
rm(nonroad4_emissions,nonroad5_emissions,nonroad67_emissions,nonroad123_emissions,nonroad8910_emissions)

# Check to see if pollutants of interest are in emission files
# nrow(nonroad_emissions_small[nonroad_emissions_small$description=="Sulfur Dioxide",])

# 2c. Read and bind Point emissions

point12345_emissions<- readData("US_EPA_IN_EM_INV","process_12345",meta=F ) %>% select(tribal_name,st_usps_cd,scc,description,total_emissions,uom)
point678910_emissions<- readData("US_EPA_IN_EM_INV","process_678910",meta=F ) %>% select(tribal_name,st_usps_cd,scc,description,total_emissions,uom)

point_emissions <- bind_rows(point12345_emissions,point678910_emissions)
rm(point12345_emissions,point678910_emissions)

# 2d. Read Nonpoint emissions (just one file, so no need to bind)

nonpoint_emissions<-readData("US_EPA_IN_EM_INV","2011v2_nonpoint",meta=F) %>% select(tribal_name,st_usps_cd,scc,description,total_emissions,uom)

# ------------------------------------------------------------------------------
# 3. Aggregate to sum emissions for each (SCC, pollutant) pair over counties.
# "description" is the column title of pollutant description from dataframe
# Obs: There are no emissions for tribes in onroad and nonroad.

# Check to see which categories have tribes.
onroad_tribes<-unique(onroad_emissions$tribal_name)
nonroad_tribes<-unique(nonroad_emissions$tribal_name)
point_tribes<-unique(point_emissions$tribal_name)
nonpoint_tribes<-unique(nonpoint_emissions$tribal_name)

# Attempt to create df, but columns are different lengths - look into this:
# tribe_names<-data.frame(onroad_tribes,nonroad_tribes,point_tribes,nonpoint_tribes)

# 3a. Onroad (change the order of the elements of list to get different orders of display)

# State or tribe aggregation by SCC and pollutant
onroad_emissions_state <- aggregate(onroad_emissions$total_emissions, by = list("state"=onroad_emissions$st_usps_cd,"scc"=onroad_emissions$scc,"pollutant"=onroad_emissions$description, "unit"=onroad_emissions$uom), FUN=sum)
onroad_emissions_state <- onroad_emissions_state[,c("state","scc","pollutant","x","unit")] #Change order of columns
colnames(onroad_emissions_state)[which(names(onroad_emissions_state) == "x")] <- "emissions"
writeData(onroad_emissions_state, "US_EPA_MED_OUT","onroad_emissions_state")



# Check - compare this with sum from csv above. Ex. AK SO2 = 51.30989
# sum<-onroad_emissions_aggr[onroad_emissions_aggr$state=="AK",]
# aggregate(sum$x,by=list(sum$pollutant), FUN=sum)

# National aggregation by SCC and pollutant
onroad_emissions_US <- aggregate(onroad_emissions$total_emissions, by = list("scc"=onroad_emissions$scc,"pollutant"=onroad_emissions$description,"unit"=onroad_emissions$uom),FUN=sum)
onroad_emissions_US <- onroad_emissions_US[,c("scc","pollutant","x","unit")] #Change order of columns
colnames(onroad_emissions_US)[which(names(onroad_emissions_US) == "x")] <- "emissions"
writeData(onroad_emissions_US, "US_EPA_MED_OUT","onroad_emissions_US")

diff<-sum(onroad_emissions_state$emissions)-sum(onroad_emissions_US$emissions)
if(diff>0.001) print("Error: Sum of state onroad emissions is not equal to national total. Check existence of tribes")

# rm(onroad_emissions)

# 3b. Nonroad
# State aggregation by SCC and pollutant

nonroad_emissions_state <- aggregate(nonroad_emissions$total_emissions, by = list("state"=nonroad_emissions$st_usps_cd,"scc"=nonroad_emissions$scc,"pollutant"=nonroad_emissions$description, "unit"=nonroad_emissions$uom), FUN=sum)
nonroad_emissions_state <- nonroad_emissions_state[,c("state","scc","pollutant","x","unit")] #Change order of columns
colnames(nonroad_emissions_state)[which(names(nonroad_emissions_state) == "x")] <- "emissions"
writeData(nonroad_emissions_state,"US_EPA_MED_OUT","nonroad_emissions_state")

# National aggregation by SCC and pollutant

nonroad_emissions_US <- aggregate(nonroad_emissions$total_emissions, by = list("scc"=nonroad_emissions$scc,"pollutant"=nonroad_emissions$description,"unit"=nonroad_emissions$uom),FUN=sum)
nonroad_emissions_US <- nonroad_emissions_US[,c("scc","pollutant","x","unit")] #Change order of columns
colnames(nonroad_emissions_US)[which(names(nonroad_emissions_US) == "x")] <- "emissions"
writeData(nonroad_emissions_US,"US_EPA_MED_OUT","nonroad_emissions_US.csv")

# Check
diff<-sum(nonroad_emissions_state$emissions)-sum(nonroad_emissions_US$emissions)
if(diff>0.001) print("Error: Sum of state nonroad emissions is not equal to national total. Check existence of tribes")

# rm(nonroad_emissions)

# 3c. Point
# State or tribe aggregation by SCC and pollutant
point_emissions_state<- aggregate(point_emissions$total_emissions, by = list("state"=point_emissions$st_usps_cd,"scc"=point_emissions$scc,"pollutant"=point_emissions$description, "unit"=point_emissions$uom), FUN=sum)
point_emissions_state<- point_emissions_state[,c("state","scc","pollutant","x","unit")] #Change order of columns
colnames(point_emissions_state)[which(names(point_emissions_state) == "x")] <- "emissions"
writeData(point_emissions_state,"US_EPA_MED_OUT","point_emissions_state")

point_emissions_tribe<- aggregate(point_emissions$total_emissions, by = list("tribe"=point_emissions$tribal_name,"scc"=point_emissions$scc,"pollutant"=point_emissions$description, "unit"=point_emissions$uom), FUN=sum)
point_emissions_tribe<- point_emissions_tribe[,c("tribe","scc","pollutant","x","unit")] #Change order of columns
colnames(point_emissions_tribe)[which(names(point_emissions_tribe) == "x")] <- "emissions"
writeData(point_emissions_tribe,"US_EPA_MED_OUT","point_emissions_tribe")

# National aggregation by SCC and pollutant
point_emissions_US <- aggregate(point_emissions$total_emissions, by = list("scc"=point_emissions$scc,"pollutant"=point_emissions$description,"unit"=point_emissions$uom),FUN=sum)
point_emissions_US <- point_emissions_US[,c("scc","pollutant","x","unit")] #Change order of columns
colnames(point_emissions_US)[which(names(point_emissions_US) == "x")] <- "emissions"
writeData(point_emissions_US,"US_EPA_MED_OUT","point_emissions_US")

# rm(point_emissions)

# 3d. Nonpoint
# State aggregation by SCC and pollutant
nonpoint_emissions_state<- aggregate(nonpoint_emissions$total_emissions, by = list("state"=nonpoint_emissions$st_usps_cd,"scc"=nonpoint_emissions$scc,"pollutant"=nonpoint_emissions$description, "unit"=nonpoint_emissions$uom), FUN=sum)
nonpoint_emissions_state<- nonpoint_emissions_state[,c("state","scc","pollutant","x","unit")] #Change order of columns
colnames(nonpoint_emissions_state)[which(names(nonpoint_emissions_state) == "x")] <- "emissions"
writeData(nonpoint_emissions_state, "US_EPA_MED_OUT","nonpoint_emissions_state")

nonpoint_emissions_tribe<- aggregate(nonpoint_emissions$total_emissions, by = list("tribe"= nonpoint_emissions$tribal_name,"scc"=nonpoint_emissions$scc,"pollutant"=nonpoint_emissions$description, "unit"=nonpoint_emissions$uom), FUN=sum)
nonpoint_emissions_tribe<- nonpoint_emissions_tribe[,c("tribe","scc","pollutant","x","unit")] #Change order of columns
colnames(nonpoint_emissions_tribe)[which(names(nonpoint_emissions_tribe) == "x")] <- "emissions"
writeData(nonpoint_emissions_tribe, "US_EPA_MED_OUT","nonpoint_emissions_tribe")

#National aggregation by SCC and pollutant
nonpoint_emissions_US <- aggregate(nonpoint_emissions$total_emissions, by = list("scc"=nonpoint_emissions$scc,"pollutant"=nonpoint_emissions$description,"unit"=nonpoint_emissions$uom),FUN=sum)
nonpoint_emissions_US <- nonpoint_emissions_US[,c("scc","pollutant","x","unit")] #Change order of columns
colnames(nonpoint_emissions_US)[which(names(nonpoint_emissions_US) == "x")] <- "emissions"
writeData(nonpoint_emissions_US, "US_EPA_MED_OUT","nonpoint_emissions_US")

# rm(nonpoint_emissions)

# Create table with emission totals for all pollutants (for checking purposes)
state <-c(sum(onroad_emissions_state$emissions),sum(nonroad_emissions_state$emissions),sum(point_emissions_state$emissions),sum(nonpoint_emissions_state$emissions))
tribe<-c(NA,NA,sum(point_emissions_tribe$emissions), sum(nonpoint_emissions_tribe$emissions))
US <-c(sum(onroad_emissions_US$emissions),sum(nonroad_emissions_US$emissions),sum(point_emissions_US$emissions),sum(nonpoint_emissions_US$emissions))
totals <- data.frame(state,tribe,US)
row.names(totals)<-c("onroad", "nonroad", "point","nonpoint")
writeData(totals,"US_EPA_MED_OUT","Emission_totals" )  #Why are row names not being written out?

# ------------------------------------------------------------------------------
# 4. Create vector with positions in emissions file corresponding to SCCs in Xwalk and to NFR09 in NFR09-NFR14 mapping.
# (in the latter, scc column is named "Code")

# Check - If an SCC listed in the emissions df is not found in the Xwalk, R will include an NA in the emissions df row.
# That is, are there any SCCs in the emission files which have not be found in Xwalk? To check this, the following code can be executed:
# any(is.na(position_of_scc_in_Xwalk)==TRUE)
# or this code gives us a new df with rows where there are non-NA values.
# na_check<-subset(position_of_scc_in_Xwalk,!is.na(position_of_scc_in_Xwalk))

# 4a. Onroad
# For state-level
position_of_scc_in_Xwalk<-match(onroad_emissions_state$scc,Xwalk$Code,nomatch = NA_integer_)

# any(is.na(position_of_scc_in_Xwalk)==TRUE)

# Create columns with NFR09 code and information corresponding to SCC
onroad_emissions_state$NFR09<-Xwalk$NFR_Code[position_of_scc_in_Xwalk]
position_of_NFR09_in_mapping<-match(onroad_emissions_state$NFR09, Xwalk_NFR09_NFR14$NFR09,nomatch = NA_integer_)
onroad_emissions_state$NFR14<-Xwalk_NFR09_NFR14$NFR14[position_of_NFR09_in_mapping]
onroad_emissions_state$NFR14_description<-Xwalk_NFR09_NFR14$NFR14_description[position_of_NFR09_in_mapping]
onroad_emissions_state$SCC_Level_One<-Xwalk$SCC_Level_One[position_of_scc_in_Xwalk]
onroad_emissions_state$SCC_Level_Two<-Xwalk$SCC_Level_Two[position_of_scc_in_Xwalk]
onroad_emissions_state$SCC_Level_Three<-Xwalk$SCC_Level_Three[position_of_scc_in_Xwalk]
onroad_emissions_state$Tier_1_Description<-Xwalk$Tier_1_Description[position_of_scc_in_Xwalk]
onroad_emissions_state$Tier_2_Description<-Xwalk$Tier_2_Description[position_of_scc_in_Xwalk]
onroad_emissions_state$Tier_3_Description<-Xwalk$Tier_3_Description[position_of_scc_in_Xwalk]

writeData(onroad_emissions_state, "US_EPA_MED_OUT","onroad_emissions_state")

# For national-level
position_of_scc_in_Xwalk<-match(onroad_emissions_US$scc,Xwalk$Code,nomatch = NA_integer_)

# Create columns with NFR09 code and information corresponding to SCC
onroad_emissions_US$NFR09<-Xwalk$NFR_Code[position_of_scc_in_Xwalk]
position_of_NFR09_in_mapping<-match(onroad_emissions_US$NFR09, Xwalk_NFR09_NFR14$NFR09,nomatch = NA_integer_)
onroad_emissions_US$NFR14<-Xwalk_NFR09_NFR14$NFR14[position_of_NFR09_in_mapping]
onroad_emissions_US$NFR14_description<-Xwalk_NFR09_NFR14$NFR14_description[position_of_NFR09_in_mapping]
onroad_emissions_US$SCC_Level_One<-Xwalk$SCC_Level_One[position_of_scc_in_Xwalk]
onroad_emissions_US$SCC_Level_Two<-Xwalk$SCC_Level_Two[position_of_scc_in_Xwalk]
onroad_emissions_US$SCC_Level_Three<-Xwalk$SCC_Level_Three[position_of_scc_in_Xwalk]
onroad_emissions_US$Tier_1_Description<-Xwalk$Tier_1_Description[position_of_scc_in_Xwalk]
onroad_emissions_US$Tier_2_Description<-Xwalk$Tier_2_Description[position_of_scc_in_Xwalk]
onroad_emissions_US$Tier_3_Description<-Xwalk$Tier_3_Description[position_of_scc_in_Xwalk]

writeData(onroad_emissions_US, "US_EPA_MED_OUT","onroad_emissions_US")

# 4b. Nonroad
# For state-level
position_of_scc_in_Xwalk<-match(nonroad_emissions_state$scc,Xwalk$Code,nomatch = NA_integer_)
# any(is.na(position_of_scc_in_Xwalk)==TRUE)

# Create columns with NFR09 code and information corresponding to SCC
nonroad_emissions_state$NFR09<-Xwalk$NFR_Code[position_of_scc_in_Xwalk]
position_of_NFR09_in_mapping<-match(nonroad_emissions_state$NFR09, Xwalk_NFR09_NFR14$NFR09,nomatch = NA_integer_)
nonroad_emissions_state$NFR14<-Xwalk_NFR09_NFR14$NFR14[position_of_NFR09_in_mapping]
nonroad_emissions_state$NFR14_description<-Xwalk_NFR09_NFR14$NFR14_description[position_of_NFR09_in_mapping]
nonroad_emissions_state$SCC_Level_One<-Xwalk$SCC_Level_One[position_of_scc_in_Xwalk]
nonroad_emissions_state$SCC_Level_Two<-Xwalk$SCC_Level_Two[position_of_scc_in_Xwalk]
nonroad_emissions_state$SCC_Level_Three<-Xwalk$SCC_Level_Three[position_of_scc_in_Xwalk]
nonroad_emissions_state$Tier_1_Description<-Xwalk$Tier_1_Description[position_of_scc_in_Xwalk]
nonroad_emissions_state$Tier_2_Description<-Xwalk$Tier_2_Description[position_of_scc_in_Xwalk]
nonroad_emissions_state$Tier_3_Description<-Xwalk$Tier_3_Description[position_of_scc_in_Xwalk]

writeData(nonroad_emissions_state,"US_EPA_MED_OUT","nonroad_emissions_state")

# For national-level
position_of_scc_in_Xwalk<-match(nonroad_emissions_US$scc,Xwalk$Code,nomatch = NA_integer_)

# Create columns with NFR09 code and information corresponding to SCC
nonroad_emissions_US$NFR09<-Xwalk$NFR_Code[position_of_scc_in_Xwalk]
position_of_NFR09_in_mapping<-match(nonroad_emissions_US$NFR09, Xwalk_NFR09_NFR14$NFR09,nomatch = NA_integer_)
nonroad_emissions_US$NFR14<-Xwalk_NFR09_NFR14$NFR14[position_of_NFR09_in_mapping]
nonroad_emissions_US$NFR14_description<-Xwalk_NFR09_NFR14$NFR14_description[position_of_NFR09_in_mapping]
nonroad_emissions_US$SCC_Level_One<-Xwalk$SCC_Level_One[position_of_scc_in_Xwalk]
nonroad_emissions_US$SCC_Level_Two<-Xwalk$SCC_Level_Two[position_of_scc_in_Xwalk]
nonroad_emissions_US$SCC_Level_Three<-Xwalk$SCC_Level_Three[position_of_scc_in_Xwalk]
nonroad_emissions_US$Tier_1_Description<-Xwalk$Tier_1_Description[position_of_scc_in_Xwalk]
nonroad_emissions_US$Tier_2_Description<-Xwalk$Tier_2_Description[position_of_scc_in_Xwalk]
nonroad_emissions_US$Tier_3_Description<-Xwalk$Tier_3_Description[position_of_scc_in_Xwalk]

writeData(nonroad_emissions_US, "US_EPA_MED_OUT", "nonroad_emissions_US")

# 4c. Point
# For state-level
position_of_scc_in_Xwalk<-match(point_emissions_state$scc,Xwalk$Code,nomatch = NA_integer_)
# any(is.na(position_of_scc_in_Xwalk)==TRUE)

# Create columns with NFR09 code and information corresponding to SCC
point_emissions_state$NFR09<-Xwalk$NFR_Code[position_of_scc_in_Xwalk]
position_of_NFR09_in_mapping<-match(point_emissions_state$NFR09, Xwalk_NFR09_NFR14$NFR09,nomatch = NA_integer_)
point_emissions_state$NFR14<-Xwalk_NFR09_NFR14$NFR14[position_of_NFR09_in_mapping]
point_emissions_state$NFR14_description<-Xwalk_NFR09_NFR14$NFR14_description[position_of_NFR09_in_mapping]
point_emissions_state$SCC_Level_One<-Xwalk$SCC_Level_One[position_of_scc_in_Xwalk]
point_emissions_state$SCC_Level_Two<-Xwalk$SCC_Level_Two[position_of_scc_in_Xwalk]
point_emissions_state$SCC_Level_Three<-Xwalk$SCC_Level_Three[position_of_scc_in_Xwalk]
point_emissions_state$Tier_1_Description<-Xwalk$Tier_1_Description[position_of_scc_in_Xwalk]
point_emissions_state$Tier_2_Description<-Xwalk$Tier_2_Description[position_of_scc_in_Xwalk]
point_emissions_state$Tier_3_Description<-Xwalk$Tier_3_Description[position_of_scc_in_Xwalk]

writeData(point_emissions_state, "US_EPA_MED_OUT","point_emissions_state")


# For tribe-level
position_of_scc_in_Xwalk<-match(point_emissions_tribe$scc,Xwalk$Code,nomatch = NA_integer_)
# any(is.na(position_of_scc_in_Xwalk)==TRUE)

# Create columns with NFR09 code and information corresponding to SCC
point_emissions_tribe$NFR09<-Xwalk$NFR_Code[position_of_scc_in_Xwalk]
position_of_NFR09_in_mapping<-match(point_emissions_tribe$NFR09, Xwalk_NFR09_NFR14$NFR09,nomatch = NA_integer_)
point_emissions_tribe$NFR14<-Xwalk_NFR09_NFR14$NFR14[position_of_NFR09_in_mapping]
point_emissions_tribe$NFR14_description<-Xwalk_NFR09_NFR14$NFR14_description[position_of_NFR09_in_mapping]
point_emissions_tribe$SCC_Level_One<-Xwalk$SCC_Level_One[position_of_scc_in_Xwalk]
point_emissions_tribe$SCC_Level_Two<-Xwalk$SCC_Level_Two[position_of_scc_in_Xwalk]
point_emissions_tribe$SCC_Level_Three<-Xwalk$SCC_Level_Three[position_of_scc_in_Xwalk]
point_emissions_tribe$Tier_1_Description<-Xwalk$Tier_1_Description[position_of_scc_in_Xwalk]
point_emissions_tribe$Tier_2_Description<-Xwalk$Tier_2_Description[position_of_scc_in_Xwalk]
point_emissions_tribe$Tier_3_Description<-Xwalk$Tier_3_Description[position_of_scc_in_Xwalk]

writeData(point_emissions_tribe, "US_EPA_MED_OUT","point_emissions_tribe")

# For national-level
position_of_scc_in_Xwalk<-match(point_emissions_US$scc,Xwalk$Code,nomatch = NA_integer_)

# Create columns with NFR09 code and information corresponding to SCC
point_emissions_US$NFR09<-Xwalk$NFR_Code[position_of_scc_in_Xwalk]
position_of_NFR09_in_mapping<-match(point_emissions_US$NFR09, Xwalk_NFR09_NFR14$NFR09,nomatch = NA_integer_)
point_emissions_US$NFR14<-Xwalk_NFR09_NFR14$NFR14[position_of_NFR09_in_mapping]
point_emissions_US$NFR14_description<-Xwalk_NFR09_NFR14$NFR14_description[position_of_NFR09_in_mapping]
point_emissions_US$SCC_Level_One<-Xwalk$SCC_Level_One[position_of_scc_in_Xwalk]
point_emissions_US$SCC_Level_Two<-Xwalk$SCC_Level_Two[position_of_scc_in_Xwalk]
point_emissions_US$SCC_Level_Three<-Xwalk$SCC_Level_Three[position_of_scc_in_Xwalk]
point_emissions_US$Tier_1_Description<-Xwalk$Tier_1_Description[position_of_scc_in_Xwalk]
point_emissions_US$Tier_2_Description<-Xwalk$Tier_2_Description[position_of_scc_in_Xwalk]
point_emissions_US$Tier_3_Description<-Xwalk$Tier_3_Description[position_of_scc_in_Xwalk]

writeData(point_emissions_US, "US_EPA_MED_OUT","point_emissions_US")

# 4d. Nonpoint
# For state-level
position_of_scc_in_Xwalk<-match(nonpoint_emissions_state$scc,Xwalk$Code,nomatch = NA_integer_)
# any(is.na(position_of_scc_in_Xwalk)==TRUE)

# Create columns with NFR09 code and information corresponding to SCC
nonpoint_emissions_state$NFR09<-Xwalk$NFR_Code[position_of_scc_in_Xwalk]
position_of_NFR09_in_mapping<-match(nonpoint_emissions_state$NFR09, Xwalk_NFR09_NFR14$NFR09,nomatch = NA_integer_)
nonpoint_emissions_state$NFR14<-Xwalk_NFR09_NFR14$NFR14[position_of_NFR09_in_mapping]
nonpoint_emissions_state$NFR14_description<-Xwalk_NFR09_NFR14$NFR14_description[position_of_NFR09_in_mapping]
nonpoint_emissions_state$SCC_Level_One<-Xwalk$SCC_Level_One[position_of_scc_in_Xwalk]
nonpoint_emissions_state$SCC_Level_Two<-Xwalk$SCC_Level_Two[position_of_scc_in_Xwalk]
nonpoint_emissions_state$SCC_Level_Three<-Xwalk$SCC_Level_Three[position_of_scc_in_Xwalk]
nonpoint_emissions_state$Tier_1_Description<-Xwalk$Tier_1_Description[position_of_scc_in_Xwalk]
nonpoint_emissions_state$Tier_2_Description<-Xwalk$Tier_2_Description[position_of_scc_in_Xwalk]
nonpoint_emissions_state$Tier_3_Description<-Xwalk$Tier_3_Description[position_of_scc_in_Xwalk]

writeData(nonpoint_emissions_state, "US_EPA_MED_OUT","nonpoint_emissions_state")

# For tribe-level
position_of_scc_in_Xwalk<-match(nonpoint_emissions_tribe$scc,Xwalk$Code,nomatch = NA_integer_)
# any(is.na(position_of_scc_in_Xwalk)==TRUE)

# Create columns with NFR09 code and information corresponding to SCC
nonpoint_emissions_tribe$NFR09<-Xwalk$NFR_Code[position_of_scc_in_Xwalk]
position_of_NFR09_in_mapping<-match(nonpoint_emissions_tribe$NFR09, Xwalk_NFR09_NFR14$NFR09,nomatch = NA_integer_)
nonpoint_emissions_tribe$NFR14<-Xwalk_NFR09_NFR14$NFR14[position_of_NFR09_in_mapping]
nonpoint_emissions_tribe$NFR14_description<-Xwalk_NFR09_NFR14$NFR14_description[position_of_NFR09_in_mapping]
nonpoint_emissions_tribe$SCC_Level_One<-Xwalk$SCC_Level_One[position_of_scc_in_Xwalk]
nonpoint_emissions_tribe$SCC_Level_Two<-Xwalk$SCC_Level_Two[position_of_scc_in_Xwalk]
nonpoint_emissions_tribe$SCC_Level_Three<-Xwalk$SCC_Level_Three[position_of_scc_in_Xwalk]
nonpoint_emissions_tribe$Tier_1_Description<-Xwalk$Tier_1_Description[position_of_scc_in_Xwalk]
nonpoint_emissions_tribe$Tier_2_Description<-Xwalk$Tier_2_Description[position_of_scc_in_Xwalk]
nonpoint_emissions_tribe$Tier_3_Description<-Xwalk$Tier_3_Description[position_of_scc_in_Xwalk]

writeData(nonpoint_emissions_tribe, "US_EPA_MED_OUT","nonpoint_emissions_tribe")

# For national-level
position_of_scc_in_Xwalk<-match(nonpoint_emissions_US$scc,Xwalk$Code,nomatch = NA_integer_)

# Create columns with NFR09 code and information corresponding to SCC
nonpoint_emissions_US$NFR09<-Xwalk$NFR_Code[position_of_scc_in_Xwalk]
position_of_NFR09_in_mapping<-match(nonpoint_emissions_US$NFR09, Xwalk_NFR09_NFR14$NFR09,nomatch = NA_integer_)
nonpoint_emissions_US$NFR14<-Xwalk_NFR09_NFR14$NFR14[position_of_NFR09_in_mapping]
nonpoint_emissions_US$NFR14_description<-Xwalk_NFR09_NFR14$NFR14_description[position_of_NFR09_in_mapping]
nonpoint_emissions_US$SCC_Level_One<-Xwalk$SCC_Level_One[position_of_scc_in_Xwalk]
nonpoint_emissions_US$SCC_Level_Two<-Xwalk$SCC_Level_Two[position_of_scc_in_Xwalk]
nonpoint_emissions_US$SCC_Level_Three<-Xwalk$SCC_Level_Three[position_of_scc_in_Xwalk]
nonpoint_emissions_US$Tier_1_Description<-Xwalk$Tier_1_Description[position_of_scc_in_Xwalk]
nonpoint_emissions_US$Tier_2_Description<-Xwalk$Tier_2_Description[position_of_scc_in_Xwalk]
nonpoint_emissions_US$Tier_3_Description<-Xwalk$Tier_3_Description[position_of_scc_in_Xwalk]

writeData(nonpoint_emissions_US, "US_EPA_MED_OUT", "nonpoint_emissions_US")

# Check if there are SCCs repeated in the 4 groups. There are not.
# Number of unique sccs:onroad = 30;nonroad=212;point=4879; nonpoint=571;total unique sccs = 5692
# Check number of unique occurences of SCCs, pollutants,states.
# onroad_unique_sccs <-unique(onroad_emissions_US$scc)
# onroad_unique_states <-unique(onroad_emissions_state$state)
# onroad_unique_pollutants <-unique(onroad_emissions_US$pollutant)
# onroad_unique_units <-unique(onroad_emissions_US$unit)
#
# nonroad_unique_sccs <-unique(nonroad_emissions_US$scc)
# nonroad_unique_states <-unique(nonroad_emissions_state$state)
# nonroad_unique_pollutants <-unique(nonroad_emissions_US$pollutant)
# nonroad_unique_units <-unique(nonroad_emissions_US$unit)
#
# point_unique_sccs <-unique(point_emissions_US$scc)
# point_unique_states <-unique(point_emissions_state$state)
# point_unique_pollutants <-unique(point_emissions_US$pollutant)
# point_unique_units <-unique(point_emissions_US$unit)
#
# nonpoint_unique_sccs <-unique(nonpoint_emissions_US$scc)
# nonpoint_unique_states <-unique(nonpoint_emissions_state$state)
# nonpoint_unique_pollutants <-unique(nonpoint_emissions_US$pollutant)
# nonpoint_unique_units <-unique(nonpoint_emissions_US$unit)
#
# Summed number of unique SCCs per group is same as number of unique SCCs in bound groups (expected)
# all_unique_sccs_1 <-length(unique(all_emissions_US$scc))
# all_unique_sccs_2<-sum(length(onroad_unique_sccs),length(nonroad_unique_sccs),length(point_unique_sccs),length(nonpoint_unique_sccs))
#
# Summed number of unique pollutant per group - 565-  is larger than number of unique SCCs in bound groups - 288 (expected, as there are repeated pollutants in groups)
# all_unique_pollutant_1 <-length(unique(all_emissions_US$pollutant))
# all_unique_pollutant_2<-sum(length(onroad_unique_pollutants),length(nonroad_unique_pollutants),length(point_unique_pollutants),length(nonpoint_unique_pollutants))

# ------------------------------------------------------------------------------
# 5. Bind 4 groups into one df with all emissions at state level, and into one df at US level.

all_emissions_state <- bind_rows(onroad_emissions_state,nonroad_emissions_state,point_emissions_state,nonpoint_emissions_state)
writeData(all_emissions_state, "US_EPA_MED_OUT","all_emissions_state")

all_emissions_US <- bind_rows(onroad_emissions_US,nonroad_emissions_US,point_emissions_US,nonpoint_emissions_US)
writeData(all_emissions_US, "US_EPA_MED_OUT","all_emissions_US")

all_emissions_tribe <- bind_rows(point_emissions_tribe,nonpoint_emissions_tribe)
writeData(all_emissions_tribe, "US_EPA_MED_OUT","all_emissions_tribe")

# Create SCC to NFR09 to NFR14 mapping file and add NFR14 description
mapping_SCC_NFR <- unique(all_emissions_US[,c("scc","NFR09","NFR14","NFR14_description", "SCC_Level_One","SCC_Level_Two","SCC_Level_Three","Tier_1_Description","Tier_2_Description","Tier_3_Description")])
writeData(mapping_SCC_NFR, "US_EPA_MED_OUT","mapping_SCC_NFR")
