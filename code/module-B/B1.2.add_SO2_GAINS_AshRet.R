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
colnames(gainsash_ret) <- c("iso", "reg_abb", "Fuel", "Sector", "X2005")
gainsash_ret$Fuel <- fuelmap[match(gainsash_ret$Fuel,fuelmap$GAINS.fuel),'fuel']
gainsash_ret$Sector <- sectormap[match(gainsash_ret$Sector,sectormap$GAINS.Sectors),'Sector']
gainsash_ret$iso <- tolower(gainstoiso[match(gainsash_ret$iso,gainstoiso$country),'ISO.code'])

# 2.1 Aggregating by taking the mean of the Sulfur Retention Values.
gainsash_ret <- aggregate(gainsash_ret[c("X2005")], 
                     by = gainsash_ret[c("iso","Sector", "Fuel")], FUN=mean)

# 2.2 Formatting

colnames(gainsash_ret)[which(names(gainsash_ret) == 'Sector')] <- 'sector'
colnames(gainsash_ret)[which(names(gainsash_ret) == 'Fuel')] <- 'fuel'
gainsash_ret$units <- 'NA'
gainsash_ret$pre_ext_method<- 'constant'
gainsash_ret$pre_ext_year <- 1960
gainsash_ret$interp_method <- 'linear'
gainsash_ret$post_ext_method <- 'constant'
gainsash_ret$post_ext_year <- 2014
gainsash_ret<- gainsash_ret[c('iso','sector','fuel','units','pre_ext_method','pre_ext_year','interp_method','post_ext_method','post_ext_year','X2005')]

# -------------------------------------------------------------------------------
# 3. Extrapolation/Interpolation

gainsash_ret_values <- gainsash_ret[,c('iso','sector','fuel','units','X2005')]
gainsash_ret_interp_method <- gainsash_ret[c('iso','sector','fuel','interp_method')]
gainsash_ret_ext_method <- gainsash_ret[c('iso','sector','fuel','pre_ext_method','post_ext_method')]
gainsash_ret_ext_year <- gainsash_ret[c('iso','sector','fuel','pre_ext_year','post_ext_year')]

gainsash_ret_extended <- interpolateValues(gainsash_ret_values, interp_method = gainsash_ret_interp_method)

gainsash_ret_extended <- extendValues(gainsash_ret_extended, pre_ext_default = 'linear_0', 
                                      ext_method = gainsash_ret_ext_method, ext_year = gainsash_ret_ext_year)
if( identical(gainsash_ret_extended$iso, gainsash_ret$iso) &&
    identical(gainsash_ret_extended$sector, gainsash_ret$sector)){
    gainsash_ret_extended <- cbind(gainsash_ret[,c('iso','sector','fuel','units')],
                                   gainsash_ret_extended[, names(gainsash_ret_extended)[names(gainsash_ret_extended) %!in% c('iso','sector','fuel','units')] ]) }

# -------------------------------------------------------------------------------
# 4. Add to parameter Db

addToDb_overwrite( new_data = gainsash_ret_extended , em = 'SO2' , type = 'comb' , file_extention = 'AshRet_db' )

# -------------------------------------------------------------------------------
# 5. Output
writeData(gainsash_ret_extended, domain = "MED_OUT", fn = "B.GAINS_SO2_AshRet_db")



logStop()
# END

