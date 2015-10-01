# ------------------------------------------------------------------------------
# Program Name: C.1.2.add_SO2_NC_emissions_FAO_pulp_paper.R
# Authors: Ryan Bolt, Jon Seibert
# Date Last Modified: June 18, 2015
# Program Purpose: Use the package FAOSTAT to retrieve data on pulp production
#                  and with an emission factor to produce driver data.
# Input Files: 
# Output Files: 
# To Do: 
# Notes: 
# -----------------------------------------------------------------------------
# 0. Read in global settings and headers

# Before we can load headers we need some paths defined. They may be provided by
#   a system environment variable or may have already been set in the workspace.
# Set variable PARAM_DIR to be the data system directory
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
# Universal header file - provides logging, file support, etc.

    headers <- c( "common_data.R","data_functions.R", "analysis_functions.R", 
                  "process_db_functions.R") # Additional function files required.
    log_msg <- paste0( "Sulfite and Sulfate data processing from FAO pulp and paper",
                       "processing data" ) # First message to be printed to the log
    script_name <- "C.1.2.add_SO2_NC_emissions_FAO_pulp_paper.R" 
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )    
  
# -----------------------------------------------------------------------------
# 1. We need to start up the library and define a few things
 loadPackage('FAOSTAT')

 # iso.codes <- readData( "DEFAULT_EF_IN", "FAOMaster_Country_List", meta = F )
 iso.codes <- readData( "MAPPINGS", "FAOMaster_Country_List", meta = F )
 
 sulfateEmisFactor <- 4
 SulfiteEmisFactor <- 8

 unit <- "tons"
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

# Actually retriving the data with the function getFAOtoSYB
sulfurdioxide <- with(FAOquery.df, 
    getFAOtoSYB(name = varName, domainCode = domainCode,
    itemCode = itemCode, elementCode = elementCode,
    useCHMT = TRUE, outputFormat = "wide"))

#----------------------------------------------------------------------------
# 3. The Data has been retrieved as a list of dataframes, now we can start 
# manipulating it into giving us emission data.

# The data is given as a list of dataframes. The piece we care about is the 
# dataframe entitled entity. We can simplify the code by creating a new object for
# that dataframe, entity.
entity <- sulfurdioxide$entity

# Replace NA's with 0 because thats what they are.
entity[is.na(entity)] <- 0

# Now we want to add the two sulfate procedure columns together and the 
# two sulfite procedure columns together. Following this we mulitply by the
# emission factor for each process.
entity$sulfateEmission <-
    (entity$sulfateB + entity$sulfateUn)*sulfateEmisFactor
entity$SulfiteEmission <-
    (entity$SulfiteB + entity$SulfiteUn)*SulfiteEmisFactor

# Adding a column containing the iso codes then removing the columns with the STAT
# code and the four columns containing the nonaggregated data.
entity$iso <- iso.codes$iso[ match( entity$FAOST_CODE, iso.codes$FAO ) ]
keeps <- c("iso", "Year", "sulfateEmission", "SulfiteEmission")
entity <- entity[!entity$FAOST_CODE >= 351,keeps]

#-----------------------------------------------------------------------------
# 4. Reformating Slovania data
# Slovania seemed to have had an error where there was a jump from 0 
# prodcution to around 600,000 with a corresponding drop in sulfite prodcution 
# for 2003 and 2004. This doesn't make sense as it takes a lot to change 
# production and we believe this to be a reporting error. Therefore, we added 
# these two data points sulfate Pulp to the Sulfite Pulp.

years <- c(2003,2004)
entity[(entity$iso %in% "svn" & entity$Year %in% years),"SulfiteEmission"] <-
    entity[(entity$iso %in% "svn" & entity$Year %in% years),"SulfiteEmission"] + 
    entity[(entity$iso %in% "svn" & entity$Year %in% years),"sulfateEmission"]
entity[(entity$iso %in% "svn" & entity$Year %in% years),"sulfateEmission"] <- 0

#----------------------------------------------------------------------------------
# 5. Split into sulphite and sulfate, reformat to CEDS Standard

# Remove sulfate.procedure
sulfite.procedure <- entity[ -3 ]

# Change names to allow casting
names( sulfite.procedure ) <- c( "iso","variable","value" )

# Reshape the data so that the years are columns
sulfite.procedure <- cast( sulfite.procedure, iso~variable, mean )

# Rename the columns for consistency with CEDS Standard
names( sulfite.procedure )[ 2:length( sulfite.procedure ) ] <- 
      paste0( "X", names( sulfite.procedure )[ 2:length( sulfite.procedure ) ] )

# Set previously nonexistent data spots to 0
# **Consider leaving NAs and handling later to avoid overwriting actual data?**
sulfite.procedure[ is.na( sulfite.procedure ) ] <- 0

# Add activity and units columns
sulfite.procedure$activity <- act
sulfite.procedure$units <- unit

L <- length( sulfite.procedure )

# Reorder columns
sulfite.procedure <- cbind( sulfite.procedure[ 1 ],
      cbind( sulfite.procedure[ ( L - 1 ):L ], sulfite.procedure[ 2:( L - 2 ) ]  ) )

# Set up sulfate.procedure in same way
sulfate.procedure <- entity[ -4 ]
names( sulfate.procedure ) <- c("iso","variable","value")
sulfate.procedure <- cast( sulfate.procedure, iso~variable, mean )
names( sulfate.procedure )[ 2:length( sulfate.procedure ) ] <- 
      paste0( "X", names( sulfate.procedure )[ 2:length( sulfate.procedure ) ] )
sulfate.procedure[ is.na( sulfate.procedure ) ] <- 0
sulfate.procedure$activity <- act
sulfate.procedure$units <- unit
L <- length( sulfate.procedure )
sulfate.procedure <- cbind( sulfate.procedure[ 1 ], 
      cbind( sulfate.procedure[ ( L - 1 ):L ], sulfate.procedure[ 2:( L - 2 ) ]  ) )

#--------------------------------------------------------------------------------
# 6. Country Splitting

country_splitting <- function(emission, info){
# A few definitions - the sectors/products/flows in the country data and removing
# pieces of the info data to make the information easier to use. 

    sector <- unique(emission[,2])
    info.noyear <- info[-1]
    info.noparent <- info.noyear[-1]
    end <- (as.numeric(info[1]) - 1)
    successor_cols <- paste0("X",info[1])
    years <- paste0("X",1961:end)

# In the case of multiple sectors/products/flows the for loop goes through each sector.
    for(product in seq_along(sector)){

# This if statement says that if the sum of emissions in their first year is not 0,
# do the following. Otherwise, move onto the next sector/end function.
      if(0 != sum(subset(emission, emission[,2] == 
          sector[product] & iso %in% info.noparent)[,successor_cols])){

# Defining a data frames for the sucessor countries and parent country, and the sum of
# emissions of sucessor countries in first year.
      activity.df <- subset(emission, emission[,2] == sector[product])
      successor <- subset(activity.df, activity.df$iso %in% info.noparent)
      combined <- sum(successor[successor_cols]) 
      parent <- activity.df[(activity.df$iso %in% info.noyear[1]),]

# Developing the multiplying factor and then using the multiplying factor on the
# parent country, finallying pasting the adjusted information into the successors.
      multiplying.factor <- successor[1:length(info.noparent), successor_cols]/combined
      successor[1:length(successor$iso),years] <- parent[,years]
      successor <- successor[,years] * 
            multiplying.factor[1:length(successor$iso)]
      emission[emission$iso %in% info.noparent & emission[,2] %in% 
              sector[product],years] <- successor[,years]
      } # End of if statement
    } # End of Sector Loop

# Removing parent country from data
    emission <- emission[!(emission$iso %in% info.noyear[1]),]
    return(emission)
} # End of Function 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# To perform a country split, data must be read in along with a LIST of information.
# To add a country, the year when the successor countries appear must be first 
# (For example, the Soviet Union disolved in 1991, therefore the first year of the 
# successor countries is 1992), secondly the parent country, finally, the 
# successor countries follow.
# If you have a list of lists to store country info, be sure to use DOUBLE BRACKETS.

country_splits <- list(czech.slovkia = c(1993, "csk","cze","svk"), 
                 bel.lux = c(2000, "BLX","bel","lux"),
                 yugo = c(1992, "YUG", "scg", "svn", "mkd","hrv","bih"),
                 ser.mont = c(2006, "scg","srb","mne"),
                 soviet = c(1992, "USSR","arm","aze","blr","est","geo","kaz","kgz",
                            "ltu","lva","mda","rus","tjk","tkm","ukr","uzb"))

new.sulfate.procedure <- sulfate.procedure
new.sulfite.procedure <- sulfite.procedure

for (m in seq_along(list)){
    new.sulfate.procedure <- country_splitting(new.sulfate.procedure, country_splits[[m]])
    new.sulfite.procedure <- country_splitting(new.sulfite.procedure,country_splits[[m]])
}


#--------------------------------------------------------------------------------
# 6. combine Sulfit and Sulfate - convert to kt
sulfite<-melt(new.sulfite.procedure, id=c("iso",'activity','units'))
names(sulfite)<-c('iso','sector','units','year','sulfite')
sulfate<-melt(new.sulfate.procedure, id=c("iso",'activity','units'))
names(sulfate)<-c('iso','sector','units','year','sulfate')
total_emissions<-merge(sulfite,sulfate,by=c("iso",'sector','units','year'))
total_emissions$units<-'kt'
total_emissions$sulfite<-total_emissions$sulfite/1000
total_emissions$sulfate<-total_emissions$sulfate/1000
total_emissions$total<-total_emissions$sulfite+total_emissions$sulfate
total_emissions$sector<-'process_PulpPaper'
total_emissions$fuel<-'process'
total<-cast(total_emissions[,c("iso","sector","fuel",'units',"year","total")],
     iso+sector+fuel+units~year, value = "total")

sulfite<-cast(total_emissions[,c("iso","sector","fuel",'units',"year","sulfite")],
            iso+sector+fuel+units~year, value = "sulfite")

sulfate<-cast(total_emissions[,c("iso","sector","fuel",'units',"year","sulfate")],
            iso+sector+fuel+units~year, value = "sulfate")

total<-as.data.frame(total)
# --------------------------------------------------------------------------------
# 7. Output
  addToEmissionsDb_overwrite(total,em='SO2',type='NC')
  
  writeData( total, domain = "MED_OUT", fn = "C.SO2_NC_emissions_PulpPaper")
  writeData( sulfite, domain = "MED_OUT", fn = "C.SO2_NC_emissions_PulpPaperSulfite")
  writeData( sulfate, domain = "MED_OUT", fn = "C.SO2_NC_emissions_PulpPaperSulfate")
  
  logStop()
# END
