# ------------------------------------------------------------------------------
# Program Name: H1.2.add_activity_CDIAC-Bond.R
# Author: Rachel Hoesly
# Program Purpose: Extend coal and biomass back with CDIAC and Bond data
#               
# Output Files:
# TODO: 
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
headers <- c( "data_functions.R","process_db_functions.R") # Additional function files may be required.
log_msg <- "Extending coal and biomass activity_data before 1960 with CDIAC-Bond data" # First message to be printed to the log
script_name <- "H1.2.add_activity_CDIAC-Bond.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "BC"

# ---------------------------------------------------------------------------
# 1. Load files

activity_all <- readData( 'MED_OUT',paste0('H.',em,'_total_activity_extended_db') )
extension_drivers_all <- readData("EXT_IN", 'CEDS_historical_extension_drivers_activity')

bond_historical <- readData( "EM_INV" ,"160227_SPEW_BCOCemission", ".xlsx", meta = F )
sector_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_sector_ext_map", meta = F )
iso_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_country_map", meta = F )
fuel_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_fuel_map", meta = F )

cdiac_fuel_map <- readData('EM_INV', domain_extension = "CDIAC/",  'CDIAC_fuel_map'  ) 

cdiac <- readData( "MED_OUT" , 'E.CO2_CDIAC_inventory' )
# ---------------------------------------------------------------------------
# 2. Select data to extend based on extension drivers

extension_drivers <- extension_drivers_all[which( extension_drivers_all$driver_data_source == 'CDIAC-Bond' ), ]
activity <- activity_all

# ---------------------------------------------------------------------------
# 3. Data processing

# cdiac
cdiac_combustion <- cdiac[ which( cdiac$fuel %in% c("solid_fuels" , "liquid_fuels" , "gas_fuels",'liquid_and_gas_fuels') ) , ]
cdiac_cement <- cdiac[ which( cdiac$fuel %in% c("cement_production") ) , ]
cdiac_total <- cdiac[ which( cdiac$fuel %in% c("Total_CO2") ) , ]

# bond
# map to iso sector fuel
bond <- merge( bond_historical, unique(sector_map[,c( "Tech","Sector","ext_sector")]))
bond <- merge( bond, fuel_map)
bond <- merge( bond, iso_map[,c('iso','Country')])
bond <- bond[which( bond$fuel %in% c("brown_coal" ,"hard_coal" , "biomass" , "coal_coke" ) ),
                  c('iso','fuel',"ext_sector","Year","Fuel (kt)")]
bond$Year <- paste0('X',bond$Year)
bond <- aggregate( bond['Fuel (kt)' ],
                   by = list( iso = bond$iso,
                              fuel = bond$fuel,
                              ext_sector = bond$ext_sector,
                              Year = bond$Year),
                   FUN = sum)
bond_wide <- cast( bond ,iso + fuel + ext_sector ~ Year, value = 'Fuel (kt)')


# ---------------------------------------------------------------------------
# 4. Extend fuel totals back with CDIAC

year_intervals <- unique(paste(extension_drivers$ext_start_year,extension_drivers$ext_end_year,sep='-'))

if ( length(year_intervals) > 1 ) stop( 'CDIAC-Bond extention has many year intervals, script cannot handle')
 
 interval <- year_intervals
  
 drivers <- extension_drivers[ which( extension_drivers$extra_driver_info %in% c('fuels','solid_fuels','liquid_fuels','gas_fuels','liquid_and_gas_fuels') &
                                        paste(extension_drivers$ext_start_year,extension_drivers$ext_end_year,sep='-') == interval ), ]
 
  ratio_year <- unique(drivers[,'ext_end_year'])
  ext_start_year <- unique(drivers[,'ext_start_year'])
  extention_years <- paste0('X',ext_start_year:ratio_year)
  
  # select extension data for current method
  sectors <- drivers[, c('sector','fuel') ]
  sectors <- paste(sectors$sector,sectors$fuel,sep='-')
  
  # select ceds data to extend
  ceds_extention_ratios <- activity[ which( paste(activity$sector, activity$fuel, sep="-") 
                                            %in% sectors  ) , c('iso','sector','fuel',paste0('X',ext_start_year:ratio_year))]
  # add "extra_driver_info" - CDIA driver
  ceds_extention_ratios$extra_driver_info <- drivers[ match( paste(ceds_extention_ratios$sector, ceds_extention_ratios$fuel, sep="-") ,
                                                             paste(drivers$sector, drivers$fuel, sep="-")) , 'extra_driver_info']
  
  #extended data template
  ceds_extention_ratios <- ceds_extention_ratios[,c('iso','sector','fuel','extra_driver_info',paste0('X',ratio_year))]
  names(ceds_extention_ratios)[which(names(ceds_extention_ratios) == paste0('X',ratio_year))] <- 'CEDS_ratio_year'
  
  
  # add Driver data - map to CDIAC fuel and ratio year
  ceds_extention_ratios$CDIAC_fuel <- cdiac_fuel_map[ match( ceds_extention_ratios$fuel , 
                                                             cdiac_fuel_map$ceds_fuel ) , 'cdiac_fuel']
  # add driver ratio year
  ceds_extention_ratios <- merge(ceds_extention_ratios, cdiac_combustion[,c("iso","fuel", paste0('X',ratio_year))],
                                 by.x = c('iso', 'CDIAC_fuel'),
                                 by.y = c("iso","fuel"),
                                 all.x = TRUE, all.y = FALSE)
  names(ceds_extention_ratios)[which(names(ceds_extention_ratios) == paste0('X',ratio_year))] <- 'CDIAC_ratio_year'
  
  # calculate ratio
  ceds_extention_ratios$ratio <- ceds_extention_ratios$CEDS_ratio_year/ceds_extention_ratios$CDIAC_ratio_year
  # make all infinite ratios zero
  ceds_extention_ratios[!is.finite(ceds_extention_ratios$ratio) , 'ratio'] <- 0
  
  # add driver data and use ratio to calculate extended value
  cdiac_extended <- ceds_extention_ratios[,c('iso','fuel','sector',"CDIAC_fuel",'ratio')]
  cdiac_extended[, extention_years] <- NA
  cdiac_extended <- replaceValueColMatch(cdiac_extended , cdiac_combustion,
                                        x.ColName = extention_years ,
                                        match.x = c('iso', 'CDIAC_fuel'),
                                        match.y = c("iso","fuel"),
                                        addEntries = FALSE)
  cdiac_extended[ extention_years ] <- cdiac_extended$ratio * cdiac_extended[ extention_years ]
  cdiac_extended[is.na(cdiac_extended)] <- 0
  cdiac_extended <- cdiac_extended[ , c('iso','sector','fuel',  extention_years )]
  
  
  # add to final extention template
  activity <- replaceValueColMatch(activity, cdiac_extended,
                                   x.ColName = extention_years,
                                   match.x = c('iso','sector','fuel'),
                                   addEntries = FALSE)
  
# # ---------------------------------------------------------------------------
# # 5. Merge/Compare with Bond data
#   
#   cdiac_extended <- merge( cdiac_extended ,  unique(sector_map[ , c('sector','ext_sector')]), all.x = T)
#   cdiac_extended_agg <- aggregate( cdiac_extended[extention_years],
#                                by = list(iso= cdiac_extended$iso,
#                                          ext_sector = cdiac_extended$ext_sector,
#                                          fuel = cdiac_extended$fuel),
#                                FUN = sum)
#   x <- bond_wide[,c('iso','ext_sector','fuel','X1850','X1930')]
#   names(x) <- c('iso','ext_sector','fuel','X1850bond','X1930bond')
#   test <- merge( cdiac_extended_agg[,c('iso','ext_sector','fuel','X1850','X1930')],x )
#   test <- test[ , c('iso','ext_sector','fuel','X1850','X1850bond','X1930','X1930bond')]
# ---------------------------------------------------------------------------
# 6. Write to database

if( !(nrow(activity_all) == nrow(activity) & ncol(activity_all) == ncol(activity) ) ){ 
  stop( "New and old activity do not match") } else{
    writeData( activity, "MED_OUT" , paste0('H.',em,'_total_activity_extended_db')) }

logStop()


