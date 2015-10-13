# Program Name: B1.2.add_SO2_comb_EF_ash_retention.R
# Author: Ryan Bolt, Rachel Hoesly
# Date Last Updated: 20 September 2015 
# Program Purpose: Aggregate GAINS ash retention data into Emissions Factors for SO2 and SO2 retention
# values from 2005 back to 1975
# 
# Input Files: GAINS_country_mapping.csv,GAINS_fuel_mapping.csv,
#             GAINS_sector_mapping.csv, sinash@SO2-EU28_nohead.csv
# Output Files: GAINS_Sulfur_Retention.csv
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
headers <- c( "data_functions.R", "analysis_functions.R","process_db_functions.R" ) # Additional function files may be required.
log_msg <- "Processing GAINS ash_retention data" # First message to be printed to the log
script_name <- "B1.2.add_comb_SO2_EF_ash_retention.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 1. Reading data and mapppings into script


ash_ret  <- readData( "EM_INV",  "GAINS_sinash@SO2-EU28_nohead" )
gainstoiso <- readData( "GAINS_MAPPINGS",  "GAINS_country_mapping" )
fuelmap <- readData(  "GAINS_MAPPINGS", "GAINS_fuel_mapping" )
sectormap <- readData( "GAINS_MAPPINGS",  "GAINS_sector_mapping" )

EF_db<-readData( "MED_OUT",  "B.SO2_comb_EF_db" )

# ---------------------------------------------------------------------------
# 2. Define Function

extendback <- function(df, ret, percent_total = 0, extention = 1975, 
                       vars = c("iso", "Sector","Fuel","units") ) {
  # Multiply the data by the percent total to get final year. Then take the difference
  # of those new and old. Get the number of years since the earliest year. 
  df <- df[,vars]
  old <- ret * percent_total
  dif <- (ret - old)
  years <- (extention:2005) - extention
  
  # "percent_change" is how much of a change there is year over year. 
  percent_change <- dif / (2005-extention)
  
  # We repeat each number by the number of rows in the dataframe. 
  years <- rep(years, each = nrow(df))
  
  # This is the extended data in a single vector.
  changed <- (percent_change * years) + old
  
  # Setting up the dataframe to stick the data into.
  start <- ncol(df) + 1
  last <- as.numeric(ncol(df) + (2005-extention) + 1)
  
  # Putting the data into the df and changing column names. We then return the df.
  df[ ,start:last] <- changed
  colnames(df) <- c("iso", "sector", "fuel", "units", paste0("X",extention:2005))
  return(df)
}

# ---------------------------------------------------------------------------
# 3. Aggregating Sulfur Retention values from 2005 back to 1975
# 3.1 Melting data into CEDS format

ash_ret[ash_ret == "n.a"] <- 0
ash_ret[,4:ncol(ash_ret)] <- lapply(ash_ret[,4:ncol(ash_ret)], as.numeric)
ash_ret <- melt(ash_ret, id.vars = c("cou_abb", "reg_abb", "fuel"))
colnames(ash_ret) <- c("iso", "reg_abb", "Fuel", "Sector", "Sulfur")
ash_ret$Fuel <- fuelmap[match(ash_ret$Fuel,fuelmap$GAINS.fuel),'fuel']
ash_ret$Sector <- sectormap[match(ash_ret$Sector,sectormap$GAINS.Sectors),'Sector']
ash_ret$iso <- tolower(gainstoiso[match(ash_ret$iso,gainstoiso$country),'ISO.code'])

# 3.2 Aggregating by taking the mean of the Sulfur Retention Values.
ash_ret <- aggregate(ash_ret[c("Sulfur")], 
                     by = ash_ret[c("iso","Sector", "Fuel")], FUN=mean)
ash_ret$units <- c("fraction")

# We then use the extendback function that we previously used to extend the data back
# to 1975. 
ash_ret <- extendback(ash_ret, ash_ret[,"Sulfur"], .5)

# -------------------------------------------------------------------------------
# 4. Apply to ash_ret to Emission Factors
# melt and merge data
printLog("Calculating ash retention adjusted emission factors")
ash_long<-melt(ash_ret,id=c("iso","sector","fuel","units"))
names(ash_long)[which(names(ash_long)=='variable')]<-'year'
names(ash_long)[which(names(ash_long)=='value')]<-'retention'
ash_long<-ash_long[,c("iso","sector","fuel","year","retention" )]

ef<-melt(EF_db,id=c("iso","sector","fuel","units"))
names(ef)[which(names(ef)=='variable')]<-'year'
names(ef)[which(names(ef)=='value')]<-'ef'

ef_ash_adj<-merge(ef,ash_long,
                  by=c("iso","sector","fuel","year"),
                  all.x=TRUE,all.y=TRUE)
# replace retention fraction NA's with 0
ef_ash_adj[which(is.na(ef_ash_adj$retention)),'retention']<-0

# calculate adjusted EF
ef_ash_adj$ef_adj<-ef_ash_adj$ef*(1-ef_ash_adj$retention)

ef_ash_adj_wide<-cast(ef_ash_adj[,c("iso","sector","fuel",'units',"year","ef_adj")],
                      iso+sector+fuel+units~year, value = "ef_adj")



# -------------------------------------------------------------------------------
# 5. Output
printLog("Adding GAINS - ash retention adjusted emission factors to B.SO2_comb_EF_db")
addToEFDb_overwrite(ef_ash_adj_wide,em='SO2',type='comb')
writeData( ash_ret, domain = "MED_OUT", fn = "B.SO2_comb_EF_ash_retention")

logStop()
# END



