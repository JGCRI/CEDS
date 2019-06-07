# ------------------------------------------------------------------------------
# Program Name: C.1.2.add_SO2_NC_emissions_FAO_pulp_paper.R
# Authors: Ryan Bolt, Jon Seibert, Linh Vu
# Date Last Modified: 10 May 2019
# Program Purpose: Use the package FAOSTAT to retrieve data on pulp production
#                  and with an emission factor to produce driver data.
# Input Files: Master_Country_List.csv, FAO_SO2_emissions.csv
# Output Files: C.SO2_NC_emissions_PulpPaper.csv
# To Do:
# Notes:
# -----------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Universal header file - provides logging, file support, etc.
    headers <- c( "common_data.R","data_functions.R", "analysis_functions.R",
                  "process_db_functions.R") # Additional function files required.
    log_msg <- paste0( "Sulfite and Sulfate data processing from FAO pulp and paper",
                       "processing data" ) # First message to be printed to the log
    script_name <- "C.1.2.add_SO2_NC_emissions_FAO_pulp_paper.R"
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# -----------------------------------------------------------------------------
# 1. Define constants and load Master_Country_List

 iso.codes <- readData( "MAPPINGS", "Master_Country_List", meta = F )

 sulfateEmisFactor <- 4
 SulfiteEmisFactor <- 8
 lbs_to_kt <- 4.536*10^-7

 unit <- "kt"
 act <- "Pulp_Paper_Production"

# -----------------------------------------------------------------------------
# 2. Now we can begin bringing in the information. In order to retrieve information,
# you need several identifiers. The first is the domain code. This is the
# broadest category, in this case corresponding to forestry. The second is
# the element code corresponding to the type of data desired.
# Finally, the item code describes the exact data set. VarName is the internal program
# name for each data set.

# This is the set up for retrieving the information - a dataframe of lists
FAOquery.df = data.frame(varName = c("sulfateB", "sulfateUn",
      "SulfiteB", "SulfiteUn"),
    domainCode = c("FO", "FO","FO", "FO"),
    itemCode = c(1663, 1662, 1661,1660),
    elementCode = c(5510,5510,5510,5510),
    stringsAsFactors = FALSE)

# Either retrieve the data with the function getFAOtoSYB or read from local
# directory (currently selected)

# sulfurdioxide <- with(FAOquery.df,
#     getFAOtoSYB(name = varName, domainCode = domainCode,
#     itemCode = itemCode, elementCode = elementCode,
#     useCHMT = TRUE, outputFormat = "wide"))
#
# # The data is given as a list of dataframes. The piece we care about is the
# # dataframe entitled entity. We can simplify the code by creating a new object for
# # that dataframe, total_emiss.
# total_emiss <- sulfurdioxide$entity

total_emiss <- readData( "EM_INV", "FAO_SO2_emissions", meta = F )

#----------------------------------------------------------------------------
# 3. Manipulate emission data.

# Replace NA's with 0 because thats what they are.
total_emiss[is.na(total_emiss)] <- 0

# Now we want to add the two sulfate procedure columns together and the
# two sulfite procedure columns together. Following this we mulitply by the
# emission factor for each process.
total_emiss$sulfateEmission <-
    (total_emiss$sulfateB + total_emiss$sulfateUn)*sulfateEmisFactor * lbs_to_kt
total_emiss$SulfiteEmission <-
    (total_emiss$SulfiteB + total_emiss$SulfiteUn)*SulfiteEmisFactor * lbs_to_kt

# We want all the emissions combined, therefore we add column of both emission types
total_emiss$emission <- total_emiss$SulfiteEmission + total_emiss$sulfateEmission

# Adding a column containing the iso codes then removing the columns with the STAT
# code and the four columns containing the nonaggregated data.
total_emiss$iso <- iso.codes$iso[ match( total_emiss$FAOST_CODE, iso.codes$FAO_Country_Code ) ]
keeps <- c("iso", "Year", "emission")
total_emiss <- total_emiss[!total_emiss$FAOST_CODE >= 351,keeps]

#----------------------------------------------------------------------------------
# 4. Split into sulphite and sulfate, reformat to CEDS Standard

# Change names to allow casting
names( total_emiss ) <- c( "iso","variable","value" )

# Reshape the data so that the years are columns
total_emiss <- cast( total_emiss, iso~variable, mean )

# Rename the columns for consistency with CEDS Standard
names( total_emiss )[ 2:length( total_emiss ) ] <-
      paste0( "X", names( total_emiss )[ 2:length( total_emiss ) ] )

# Set previously nonexistent data spots to 0
# **Consider leaving NAs and handling later to avoid overwriting actual data?**
total_emiss[ is.na( total_emiss ) ] <- 0

# Add activity and units columns
total_emiss$activity <- act
total_emiss$units <- unit

# Reorder columns
order <- c("iso","activity", "units", paste0("X",1961:2014))
total_emiss <- total_emiss[,order]

#--------------------------------------------------------------------------------
# 5 Splitting of aggregate countries
country_splitting <- function(emission, info, sector = c("nosectors"), sec_loc = 2){
    if(match(info[3],emission$iso,nomatch = 0) > 0){

# A few defintions taken from the info read into function such as the parent
# country, successor countries, and years the parent country existed.
      successor_start <- as.numeric(info[2])
      years <- paste0("X",info[1]:(successor_start - 1))
      parent_country <- info[3]
      info <- info[-3]
      info <- as.character(na.omit(info[match(emission$iso,info)]))
      successor_cols <- paste0("X",successor_start)
      sector <- unique(sector)

# Determine if there are multiple sectors or only one sector based on the read in.
      if(length(sector) == 1) {
# Defining data frames for successor and parent countries. Secondly, the sum
# of total_emiss in the successor countries first year.
        successor <- subset(emission, emission$iso %in% info)
        combined <- sum(successor[,successor_cols])
        parent <- emission[(emission$iso %in% parent_country),]

# Developing the multiplying factor and then using the multiplying factor on the
# parent country, finallying pasting the adjusted information into the successors.
        multiplying.factor <- successor[1:length(info), successor_cols]/combined
        successor[1:nrow(successor),years] <- parent[,years]
        successor <- successor[,years] * multiplying.factor[1:nrow(successor)]
        emission[emission$iso %in% info,years] <- successor[,years]
      } else {

# The for loop: In the case of multiple sectors/products/flows sequences along each
# The if statement: If the split in a specific sector will be zero for each, keep
# 0's in place and move on to the next sector. Speeds up function when data is 0.
        for(product in seq_along(sector)){
          if(0 != sum(subset(emission, emission[,sec_loc] ==
              sector[product] & iso %in% info)[,successor_cols])){

# Defining a data frames for the sucessor countries and parent country, and the sum of
# total_emiss of sucessor countries in first year.
          activity.df <- subset(emission, emission[,sec_loc] == sector[product])
          successor <- subset(activity.df, activity.df$iso %in% info)
          combined <- sum(successor[,successor_cols])
          parent <- activity.df[(activity.df$iso %in% parent_country),]

# Developing the multiplying factor and then using the multiplying factor on the
# parent country, finallying pasting the adjusted information into the successors.
          multiplying.factor <- successor[1:length(info), successor_cols]/combined
          successor[1:nrow(successor),years] <- parent[,years]
          successor <- successor[,years] * multiplying.factor[1:nrow(successor)]
          emission[emission$iso %in% info & emission[,sec_loc] %in%
                sector[product],years] <- successor[,years]
          } # End of sector data if statement
        } # End of Sector Loop
	} # End of if determining number of sectors
# Removing parent country from data
    emission <- emission[!(emission$iso %in% parent_country),]
    } # End of "does parent exist if"
    return(emission)
} # End of Function

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# To perform a country split, data must be read in along with a LIST of information.
# To add a country, the year when the successor countries appear must be first
# (For example, the Soviet Union disolved in 1991, therefore the first year of the
# successor countries is 1992), secondly the parent country, finally, the
# successor countries follow.
# If you have a list of lists to store country info, be sure to use DOUBLE BRACKETS.
# The function is also capable of multiple sectors. A list of sectors/activities
# is to be added to function. Also note, sectors/activities column in expected to be
# in column 2.

cou_info <- list(czech.slovkia = c(1961, 1993, "csk","cze","svk"),
                 bel.lux = c(1961, 2000, "blx","bel","lux"),
                 yugo = c(1961, 1992, "yug", "scg", "svn", "mkd","hrv","bih"),
                 ser.mont = c(1961, 2006, "scg","srb","mne"),
                 soviet = c(1961, 1992, "USSR","arm","aze","blr","est","geo","kaz",
                            "kgz","ltu","lva","mda","rus","tjk","tkm","ukr","uzb"))

for (m in seq_along(cou_info)){
    total_emiss <- country_splitting(total_emiss, cou_info[[m]])
}

# --------------------------------------------------------------------------------
# 6. Output
  addToEmissionsDb_overwrite(total_emiss,em='SO2',type='NC')

  writeData( total_emiss, domain = "MED_OUT", fn = "C.SO2_NC_emissions_PulpPaper")

  logStop()
# END
