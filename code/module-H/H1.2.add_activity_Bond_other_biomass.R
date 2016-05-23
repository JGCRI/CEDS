# ------------------------------------------------------------------------------
# Program Name: H1.2.add_activity_Bond_other_biomass.R
# Author: Rachel Hoesly
# Program Purpose: Extend other Biomass linearly back. 
# In the future: extend back with Bond data, transition from ceds to bond values
#               
# Output Files:'H.',em,'_total_activity_extended_db'
# TODO: extend with Bond data
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers

# Set working directory
dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
for ( i in 1:length( dirs ) ) {
  setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
  wd <- grep( 'CEDS/input', list.dirs(), value = T )
  if ( length(wd) > 0 ) {
    setwd( wd[1] )
    break
    
  }
}
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R","process_db_functions.R", "ModH_extention_functions.R") # Additional function files may be required.
log_msg <- "Extending other biomass activity_data before 1960 with Bond data" # First message to be printed to the log
script_name <- "H1.2.add_activity_Bond_other_biomass.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "NH3"

# ---------------------------------------------------------------------------
# 1. Load files
activity_all <- readData( 'MED_OUT',paste0('H.',em,'_total_activity_extended_db') , meta=F)

bond_historical <- readData( "EM_INV" ,"160227_SPEW_BCOCemission", ".xlsx", meta = F )
iso_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_country_map", meta = F )
fuel_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_fuel_map", meta = F )
sector_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_sector_map", meta = F )
iea_start_year <- readData( 'ENERGY_IN' , 'IEA_iso_start_data', meta = F )

# ---------------------------------------------------------------------------
# 2. Load files

other_sectors <- c('1A1a_Electricity-autoproducer','1A1a_Electricity-public',
                   '1A1a_Heat-production', '1A3ai_International-aviation',
                   '1A3aii_Domestic-aviation', '1A3b_Road', '1A3c_Rail',
                   '1A3dii_Domestic-navigation', '1A3eii_Other-transp',
                   '1A4a_Commercial-institutional', '1A4c_Agriculture-forestry-fishing',
                   '1A5_Other-unspecified')

activity <- activity_all
# ---------------------------------------------------------------------------
# 3. 

# # Industrial Biomass
# bond <- merge( bond_historical, iso_map[,c('iso','Country')])
# bond <- merge( bond, fuel_map)
# bond <- bond[which( bond[,'Fuel (kt)'] > 0),]
# bond <- bond[which( bond$fuel == 'biomass'),]
# bond <- bond[which( bond[,'Year'] < 2005),]
# bond$Year <- paste0('X',bond$Year)
# 
# # bond <- aggregate(bond["Fuel (kt)"],
# #                   by = list(iso = bond$iso,
# #                             Year = bond$Year),
# #                   FUN = sum)
# bond_other_biomass <- cast(bond, iso + Tech + Sector + fuel ~ Year, value = 'Fuel (kt)',fun.aggregate = sum, na.rm=T)

# ---------------------------------------------------------------------------
# 4. Linearly extend For now, extend with bond data later

# replace zeros with nas
activity[which( activity$fuel == 'biomass' &
                  activity$sector %in% other_sectors), paste0('X',1750:1970)] <- 
  replace( activity[which( activity$fuel == 'biomass' &
                             activity$sector %in% other_sectors), paste0('X',1750:1970)],
           activity[which( activity$fuel == 'biomass' &
                                   activity$sector %in% other_sectors), paste0('X',1750:1970)] == 0, NA)

activity[which( activity$fuel == 'biomass' &
                activity$sector %in% other_sectors), paste0('X',1750)] <- 0
activity[which( activity$fuel == 'biomass' &
                  activity$sector %in% other_sectors), paste0('X',1900)] <- 0

activity[which( activity$fuel == 'biomass' & activity$sector %in% other_sectors), paste0('X',1750:1971)] <- 
  interpolate_NAs(activity[which( activity$fuel == 'biomass' & activity$sector %in% other_sectors), paste0('X',1750:1971)])


# ---------------------------------------------------------------------------
# 4. Write to database

if( !(nrow(activity_all) == nrow(activity) & ncol(activity_all) == ncol(activity) ) ){
  stop( "New and old activity do not match") } else{
    writeData( activity, "MED_OUT" , paste0('H.',em,'_total_activity_extended_db')) }

logStop()
