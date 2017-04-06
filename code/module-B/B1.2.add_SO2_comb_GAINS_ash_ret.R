# Program Name: B1.2.add_SO2_GAINS_AshRet.R
# Author: Leyang Feng, Ryan Bolt, Rachel Hoesly, Linh Vu
# Date Last Updated: 19 April 2016
# Program Purpose: Add 2005 GAINS ash retention data into defualt ash retention database
# 
# Input Files: GAINS_country_mapping.csv,GAINS_fuel_mapping.csv,
#             GAINS_sector_mapping.csv, sinash@SO2-EU28_nohead.csv, B.SO2_S_AshRet_db.csv
# Output Files: B.SO2_GAINS_s_ash_ret.csv
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

gainsash_ret_input  <- readData( "EM_INV",  "GAINS_sinash@SO2-EU28_nohead" )
gainstoiso <- readData( "GAINS_MAPPINGS",  "GAINS_country_mapping" )
fuelmap <- readData(  "GAINS_MAPPINGS", "GAINS_fuel_mapping" )
sectormap <- readData( "GAINS_MAPPINGS",  "GAINS_sector_mapping" )

# ---------------------------------------------------------------------------
# 2. GAINS ash retention data processing

gainsash_ret <- gainsash_ret_input
gainsash_ret[gainsash_ret == "n.a"] <- 0
gainsash_ret[,4:ncol(gainsash_ret)] <- lapply(gainsash_ret[,4:ncol(gainsash_ret)], as.numeric)
gainsash_ret <- melt(gainsash_ret, id.vars = c("cou_abb", "reg_abb", "fuel"))
gainsash_ret <- gainsash_ret[-which(gainsash_ret$fuel == 'SUM'),]
colnames(gainsash_ret) <- c("iso", "reg_abb", "fuel", "sector", "X2005")

# add fuels and iso
gainsash_ret$fuel <- fuelmap[match(gainsash_ret$fuel,fuelmap$GAINS.fuel),'fuel']
gainsash_ret$iso <- tolower(gainstoiso[match(gainsash_ret$iso,gainstoiso$country),'ISO.code'])

# add sectors
gainsash_ret_mapped <- mapCEDS_sector_fuel( mapping_data = gainsash_ret,
                      mapping_file = sectormap,
                      data_match_col = 'sector',
                      map_match_col = 'GAINS.sectors',
                      map_merge_col = c('detailed_sectors'),
                      new_col_names = c('sector'),
                      level_map_in = 'detailed_sectors',
                      level_out = 'working_sectors_v1',
                      aggregate = TRUE,
                      aggregate_col = c('X2005'),
                      oneToOne = FALSE,
                      agg.fun = mean)

# 2.1 Aggregating by taking the mean of the Sulfur Retention Values.
gainsash_ret_mapped <- aggregate(gainsash_ret_mapped[c("X2005")], 
                     by = gainsash_ret_mapped[c("iso","sector", "fuel")], FUN=mean)

# add units
gainsash_ret_mapped$units <- 'fraction'
gainsash_ret_mapped <- gainsash_ret_mapped[,c("iso","sector", "fuel","units","X2005")]

# retain non zero values
gainsash_ret_mapped <- gainsash_ret_mapped[which( gainsash_ret_mapped$X2005 > 0),]

# Note 0_Temp-Aggregated is a temporary/aggregated sector, here used to aggregate DOM
# and IN_*. Copy the values of 0_Temp-Aggregated back to those sectors
disagg_sectors <- c( "1A2a_Ind-Comb-Iron-steel", "1A2b_Ind-Comb-Non-ferrous-metals",
                     "1A2c_Ind-Comb-Chemicals", "1A2d_Ind-Comb-Pulp-paper", 
                     "1A2e_Ind-Comb-Food-tobacco", "1A2f_Ind-Comb-Non-metalic-minerals",
                     "1A2g_Ind-Comb-Construction", "1A2g_Ind-Comb-machinery",
                     "1A2g_Ind-Comb-mining-quarying", "1A2g_Ind-Comb-other",
                     "1A2g_Ind-Comb-textile-leather", "1A2g_Ind-Comb-transpequip",
                     "1A2g_Ind-Comb-wood-products", "1A4a_Commercial-institutional",
                     "1A4b_Residential", "1A4c_Agriculture-forestry-fishing",
                     "1A5_Other-unspecified" )
gainsash_ret_mapped_disagg <- filter( gainsash_ret_mapped, sector == "0_Temp-Aggregated" ) %>%
  repeatAndAddVector( "sector", disagg_sectors )
gainsash_ret_mapped_final <- filter( gainsash_ret_mapped, sector != "0_Temp-Aggregated" ) %>%
  bind_rows( gainsash_ret_mapped_disagg ) %>%
  arrange( iso, sector, fuel )

# -------------------------------------------------------------------------------
# 3. Output
writeData(gainsash_ret_mapped_final, domain = "DEFAULT_EF_PARAM", fn = "B.SO2_GAINS_s_ash_ret")



logStop()
# END

