# Program Name: B1.2.add_SO2_GAINS_AshRet.R
# Author: Leyang Feng, Ryan Bolt, Rachel Hoesly
# Date Last Updated: 09 Nov 2015 
# Program Purpose: Add 2005 GAINS ash retention data into defualt ash retention database
# 
# Input Files: GAINS_country_mapping.csv,GAINS_fuel_mapping.csv,
#             GAINS_sector_mapping.csv, sinash@SO2-EU28_nohead.csv, B.SO2_S_AshRet_db.csv
# Output Files: B.SO2_S_AshRet_db.csv
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
headers <- c( 'timeframe_functions.R', "data_functions.R", "analysis_functions.R",
              "process_db_functions.R", 'interpolation_extention_functions.R' ) # Additional function files may be required.
log_msg <- "Processing GAINS ash_retention data" # First message to be printed to the log
script_name <- "B1.2.add_SO2_GAINS_AshRet.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 1. Reading data and mapppings into script

gainsash_ret  <- readData( "EM_INV",  "GAINS_sinash@SO2-EU28_nohead" )
gainstoiso <- readData( "GAINS_MAPPINGS",  "GAINS_country_mapping" )
fuelmap <- readData(  "GAINS_MAPPINGS", "GAINS_fuel_mapping" )
sectormap <- readData( "GAINS_MAPPINGS",  "GAINS_sector_mapping" )

# ---------------------------------------------------------------------------
# 2. GAINS ash retention data processing

gainsash_ret[gainsash_ret == "n.a"] <- 0
gainsash_ret[,4:ncol(gainsash_ret)] <- lapply(gainsash_ret[,4:ncol(gainsash_ret)], as.numeric)
gainsash_ret <- melt(gainsash_ret, id.vars = c("cou_abb", "reg_abb", "fuel"))
colnames(gainsash_ret) <- c("iso", "reg_abb", "fuel", "sector", "X2005")
gainsash_ret$fuel <- fuelmap[match(gainsash_ret$fuel,fuelmap$GAINS.fuel),'fuel']
gainsash_ret$sector <- sectormap[match(gainsash_ret$sector,sectormap$GAINS.sectors),'sector']
gainsash_ret$iso <- tolower(gainstoiso[match(gainsash_ret$iso,gainstoiso$country),'ISO.code'])

# 2.1 Aggregating by taking the mean of the Sulfur Retention Values.
gainsash_ret <- aggregate(gainsash_ret[c("X2005")], 
                     by = gainsash_ret[c("iso","sector", "fuel")], FUN=mean)
gainsash_ret <- gainsash_ret[which( gainsash_ret$X2005 > 0),]

# -------------------------------------------------------------------------------
# 3. Output
writeData(gainsash_ret, domain = "DEFAULT_EF_PARAM", fn = "B.SO2_GAINS_ash_ret")



logStop()
# END

