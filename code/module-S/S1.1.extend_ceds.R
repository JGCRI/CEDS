# ------------------------------------------------------------------------------
# Program Name: S1.1.extend_ceds.R
# Author: Rachel Hoesly
# Program Purpose: Extend scaled ceds data backward 
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
headers <- c( "data_functions.R") # Additional function files may be required.
log_msg <- "Extending CEDS data before 1960" # First message to be printed to the log
script_name <- "S1.1.extend_ceds.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"

# ---------------------------------------------------------------------------
# 0.5 Load Packages


# ---------------------------------------------------------------------------
# 0.5. Script Options



# ---------------------------------------------------------------------------
# 1. Load files

ceds_emissions <- readData( 'MED_OUT', paste0( 'F.' , em , '_scaled_emissions' ) )
Master_Country_List <- readData( "MAPPINGS", "Master_Country_List" )
driver_lookup <- readData( "MAPPINGS", "CEDS_historical_extension_drivers" )
cdiac_fuel_map <- readData('EM_INV', domain_extension = "CDIAC/",  'CDIAC_fuel_map'  ) 

cdiac <- readData( "MED_OUT" , 'E.CO2_CDIAC_inventory' )
un_pop <- readData( "MED_OUT" , 'A.UN_pop_master' )
biomass_fernandez <- readData('EM_INV','Fernandes_residential_biomass_full')
  
# ---------------------------------------------------------------------------
# 2. Check driver lookups for valid driver options

valid_drivers <- c( 'CDIAC-fuels', 'CDIAC-Total_CO2','CDIAC-cement_production' , 'population-NA',
                    'Biomass-NA')

invalid_drivers <- driver_lookup[which( paste(driver_lookup$driver_data_source, driver_lookup$extra_driver_info,
                                              sep="-") %!in% valid_drivers ) , ]
invalid_drivers <- invalid_drivers[!is.na(invalid_drivers$driver_data_source),]

if( nrow(invalid_drivers) > 0 ) stop( 'Invalid Drivers in ceds historical extention')

# ---------------------------------------------------------------------------
# 3. Data processing

# cdiac
cdiac_combustion <- cdiac[ which( cdiac$fuel %in% c("solid_fuels" , "liquid_fuels" , "gas_fuels") ) , ]
cdiac_cement <- cdiac[ which( cdiac$fuel %in% c("cement_production") ) , ]
cdiac_total <- cdiac[ which( cdiac$fuel %in% c("Total_CO2") ) , ]

# un population
un_pop$X_year <- paste0( "X" , un_pop$year)
population <- cast( un_pop[which ( un_pop$year %in% historical_pre_extension_year:end_year ) , ] , 
                    iso ~ X_year, value = 'pop')

# biomass
biomass <- biomass_fernandez[,c('iso','year','consumption')]
biomass$X_year <- paste0('X',biomass$year)
biomass <- cast(biomass, iso ~ X_year, value = 'consumption',
                fun.aggregate = sum)

# ---------------------------------------------------------------------------
# 1. Make CEDS extention total template

CEDS_total_extended <- ceds_emissions
CEDS_total_extended[paste0('X',historical_pre_extension_year:(start_year-1))] <- NA

# ---------------------------------------------------------------------------
# 2. Extend biomass with fernandez biomass
# Driver = Biomass

drivers <- driver_lookup[which(driver_lookup$driver_data_source == 'Biomass' &
                                 is.na(driver_lookup$extra_driver_info) ),]
ratio_year <- drivers[,'ext_end_year']
ext_start_year <- drivers[,'ext_start_year']
extention_years <- paste0('X',ext_start_year:ratio_year)

# select extension data for current method
sectors <- drivers[, c('sector','fuel') ]
sectors <- paste(sectors$sector,sectors$fuel,sep='-')

# select ceds data to extend
ceds_extention_ratios <- ceds_emissions[ which( paste(ceds_emissions$sector, ceds_emissions$fuel, sep="-") %in% sectors  ) , ]

#extended data template
ceds_extention_ratios <- ceds_extention_ratios[,c('iso','sector','fuel',paste0('X',ratio_year))]
names(ceds_extention_ratios)[which(names(ceds_extention_ratios) == paste0('X',ratio_year))] <- 'CEDS_ratio_year'

# add Driver identifyer ratio year
ceds_extention_ratios <- merge(ceds_extention_ratios, biomass[,c("iso", paste0('X',ratio_year))],
                               by.x = c('iso'),
                               by.y = c("iso"),
                               all.x = TRUE, all.y = FALSE)
names(ceds_extention_ratios)[which(names(ceds_extention_ratios) == paste0('X',ratio_year))] <- 'CDIAC_ratio_year'


# calculate ratio
ceds_extention_ratios$ratio <- ceds_extention_ratios$CEDS_ratio_year/ceds_extention_ratios$CDIAC_ratio_year
# make all infinite ratios zero
ceds_extention_ratios[!is.finite(ceds_extention_ratios$ratio) , 'ratio'] <- 0

# add driver data and use ratio to calculate extended value
ceds_extended <- ceds_extention_ratios[,c('iso','fuel','sector','ratio')]
ceds_extended[extention_years] <- biomass[ match( ceds_extended$iso , biomass$iso )
                                              ,extention_years]
ceds_extended[is.na(ceds_extended)] <- 0

# calculate extended data
ceds_extended[ extention_years ] <- ceds_extended$ratio * ceds_extended[ extention_years ]

# add to final extention template
CEDS_total_extended <- replaceValueColMatch(CEDS_total_extended, ceds_extended,
                                            x.ColName = extention_years,
                                            match.x = c('iso','sector','fuel'),
                                            addEntries = FALSE)

# ---------------------------------------------------------------------------
# 3. Extend Combustion Sectors with cdiac
# Driver = CDIAC-fuels

drivers <- driver_lookup[which(driver_lookup$driver_data_source == 'CDIAC' &
                                 driver_lookup$extra_driver_info == 'fuels' ),]

year_intervals <- unique(paste(drivers$ext_start_year,drivers$ext_end_year,sep='-'))

for (i in seq_along(year_intervals)) {

interval <- year_intervals[i]
drivers <- driver_lookup[which(driver_lookup$driver_data_source == 'CDIAC' &
                                 driver_lookup$extra_driver_info == 'fuels' ),]
drivers <- drivers[which( paste(drivers$ext_start_year,drivers$ext_end_year,sep='-') == interval ),]
ratio_year <- unique(drivers[,'ext_end_year'])
ext_start_year <- unique(drivers[,'ext_start_year'])
extention_years <- paste0('X',ext_start_year:ratio_year)

# select extension data for current method
sectors <- drivers[, c('sector','fuel') ]
sectors <- paste(sectors$sector,sectors$fuel,sep='-')

# select ceds data to extend
ceds_extention_ratios <- CEDS_total_extended[ which( paste(CEDS_total_extended$sector, CEDS_total_extended$fuel, sep="-") 
                                                %in% sectors  ) , ]

#extended data template
ceds_extention_ratios <- ceds_extention_ratios[,c('iso','sector','fuel',paste0('X',ratio_year))]
names(ceds_extention_ratios)[which(names(ceds_extention_ratios) == paste0('X',ratio_year))] <- 'CEDS_ratio_year'

# add Driver identifyer ratio year
ceds_extention_ratios$CDIAC_fuel <- cdiac_fuel_map[ match( ceds_extention_ratios$fuel , 
                                                           cdiac_fuel_map$ceds_fuel ) , 'cdiac_fuel']
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
ceds_extended <- ceds_extention_ratios[,c('iso','fuel','sector',"CDIAC_fuel",'ratio')]
ceds_extended[, extention_years] <- NA
ceds_extended <- replaceValueColMatch(ceds_extended , cdiac_combustion,
                                      x.ColName = extention_years ,
                                      match.x = c('iso', 'CDIAC_fuel'),
                                      match.y = c("iso","fuel"),
                                      addEntries = FALSE)
ceds_extended[ extention_years ] <- ceds_extended$ratio * ceds_extended[ extention_years ]
ceds_extended[is.na(ceds_extended)] <- 0

# add to final extention template
CEDS_total_extended <- replaceValueColMatch(CEDS_total_extended, ceds_extended,
                                            x.ColName = extention_years,
                                            match.x = c('iso','sector','fuel'),
                                            addEntries = FALSE)

}

# ---------------------------------------------------------------------------
# 4. Extend cement with cediat
# Driver = CDIAC-cement_production

ratio_year <- 1965
extention_years <- paste0('X',historical_pre_extension_year:1965)

# select extension data for current method
sectors <- driver_lookup[which(driver_lookup$driver_data_source == 'CDIAC' &
                                 driver_lookup$extra_driver_info == 'cement_production' ), c('sector','fuel') ]
sectors <- paste(sectors$sector,sectors$fuel,sep='-')

# select ceds data to extend
ceds_extention_ratios <- CEDS_total_extended[ which( paste(CEDS_total_extended$sector, CEDS_total_extended$fuel, sep="-") %in% sectors  ) , ]

#extended data template
ceds_extention_ratios <- ceds_extention_ratios[,c('iso','sector','fuel',paste0('X',ratio_year))]
names(ceds_extention_ratios)[which(names(ceds_extention_ratios) == paste0('X',ratio_year))] <- 'CEDS_ratio_year'

# add Driver identifyer ratio year
ceds_extention_ratios <- merge(ceds_extention_ratios, cdiac_cement[,c("iso","fuel", paste0('X',ratio_year))],
                               by.x = c('iso'),
                               by.y = c("iso"),
                               all.x = TRUE, all.y = FALSE)
names(ceds_extention_ratios)[which(names(ceds_extention_ratios) == paste0('X',ratio_year))] <- 'CDIAC_ratio_year'
names(ceds_extention_ratios)[which(names(ceds_extention_ratios) == 'fuel.y')] <- 'CDIAC_fuel'
names(ceds_extention_ratios)[which(names(ceds_extention_ratios) == 'fuel.x')] <- 'fuel'

# calculate ratio
ceds_extention_ratios$ratio <- ceds_extention_ratios$CEDS_ratio_year/ceds_extention_ratios$CDIAC_ratio_year
# make all infinite ratios zero
ceds_extention_ratios[!is.finite(ceds_extention_ratios$ratio) , 'ratio'] <- 0

# add driver data and use ratio to calculate extended value
ceds_extended <- ceds_extention_ratios[,c('iso','fuel','sector',"CDIAC_fuel",'ratio')]
ceds_extended[extention_years] <- cdiac_cement[ match( ceds_extended$iso , cdiac_cement$iso )
                                                ,extention_years]

# calculate extended data
ceds_extended[ extention_years ] <- ceds_extended$ratio * ceds_extended[ extention_years ]
ceds_extended[is.na(ceds_extended)] <- 0

# add to final extention template
CEDS_total_extended <- replaceValueColMatch(CEDS_total_extended, ceds_extended,
                                            x.ColName = extention_years,
                                            match.x = c('iso','sector','fuel'),
                                            addEntries = FALSE)

# ---------------------------------------------------------------------------
# 5. Extend fossil driven process sectors with cdiac total CO2
# Driver = CDIAC-Total_CO2

ratio_year <- 1965
extention_years <- paste0('X',historical_pre_extension_year:1965)

# select extension data for current method
sectors <- driver_lookup[which(driver_lookup$driver_data_source == 'CDIAC' &
                                 driver_lookup$extra_driver_info == 'Total_CO2' ), c('sector','fuel') ]
sectors <- paste(sectors$sector,sectors$fuel,sep='-')

# select ceds data to extend
ceds_extention_ratios <- CEDS_total_extended[ which( paste(CEDS_total_extended$sector, CEDS_total_extended$fuel, sep="-") %in% sectors  ) , ]

#extended data template
ceds_extention_ratios <- ceds_extention_ratios[,c('iso','sector','fuel',paste0('X',ratio_year))]
names(ceds_extention_ratios)[which(names(ceds_extention_ratios) == paste0('X',ratio_year))] <- 'CEDS_ratio_year'

# add Driver identifyer ratio year
ceds_extention_ratios <- merge(ceds_extention_ratios, cdiac_total[,c("iso","fuel", paste0('X',ratio_year))],
                               by.x = c('iso'),
                               by.y = c("iso"),
                               all.x = TRUE, all.y = FALSE)
names(ceds_extention_ratios)[which(names(ceds_extention_ratios) == paste0('X',ratio_year))] <- 'CDIAC_ratio_year'
names(ceds_extention_ratios)[which(names(ceds_extention_ratios) == 'fuel.y')] <- 'CDIAC_fuel'
names(ceds_extention_ratios)[which(names(ceds_extention_ratios) == 'fuel.x')] <- 'fuel'

# calculate ratio
ceds_extention_ratios$ratio <- ceds_extention_ratios$CEDS_ratio_year/ceds_extention_ratios$CDIAC_ratio_year
# make all infinite ratios zero
ceds_extention_ratios[!is.finite(ceds_extention_ratios$ratio) , 'ratio'] <- 0

# add driver data and use ratio to calculate extended value
ceds_extended <- ceds_extention_ratios[,c('iso','fuel','sector',"CDIAC_fuel",'ratio')]
ceds_extended[extention_years] <- cdiac_total[ match( ceds_extended$iso , cdiac_total$iso )
                                                ,extention_years]

# calculate extended data
ceds_extended[ extention_years ] <- ceds_extended$ratio * ceds_extended[ extention_years ]
ceds_extended[is.na(ceds_extended)] <- 0

# add to final extention template
CEDS_total_extended <- replaceValueColMatch(CEDS_total_extended, ceds_extended,
                                            x.ColName = extention_years,
                                            match.x = c('iso','sector','fuel'),
                                            addEntries = FALSE)


# ---------------------------------------------------------------------------
# 6. Extend others with population
# Driver = population

ratio_year <- 1965
extention_years <- paste0('X',historical_pre_extension_year:1965)

# select extension data for current method
sectors <- driver_lookup[which(driver_lookup$driver_data_source == 'population' &
                                 is.na(driver_lookup$extra_driver_info) ), c('sector','fuel') ]
sectors <- paste(sectors$sector,sectors$fuel,sep='-')

# select ceds data to extend
ceds_extention_ratios <- CEDS_total_extended[ which( paste(CEDS_total_extended$sector, CEDS_total_extended$fuel, sep="-") %in% sectors  ) , ]

#extended data template
ceds_extention_ratios <- ceds_extention_ratios[,c('iso','sector','fuel',paste0('X',ratio_year))]
names(ceds_extention_ratios)[which(names(ceds_extention_ratios) == paste0('X',ratio_year))] <- 'CEDS_ratio_year'

# add Driver identifyer ratio year
ceds_extention_ratios <- merge(ceds_extention_ratios, population[,c("iso", paste0('X',ratio_year))],
                               by.x = c('iso'),
                               by.y = c("iso"),
                               all.x = TRUE, all.y = FALSE)
names(ceds_extention_ratios)[which(names(ceds_extention_ratios) == paste0('X',ratio_year))] <- 'CDIAC_ratio_year'


# calculate ratio
ceds_extention_ratios$ratio <- ceds_extention_ratios$CEDS_ratio_year/ceds_extention_ratios$CDIAC_ratio_year
# make all infinite ratios zero
ceds_extention_ratios[!is.finite(ceds_extention_ratios$ratio) , 'ratio'] <- 0

# add driver data and use ratio to calculate extended value
ceds_extended <- ceds_extention_ratios[,c('iso','fuel','sector','ratio')]
ceds_extended[extention_years] <- population[ match( ceds_extended$iso , population$iso )
                                               ,extention_years]

# calculate extended data
ceds_extended[ extention_years ] <- ceds_extended$ratio * ceds_extended[ extention_years ]
ceds_extended[is.na(ceds_extended)] <- 0

# add to final extention template
CEDS_total_extended <- replaceValueColMatch(CEDS_total_extended, ceds_extended,
                                            x.ColName = extention_years,
                                            match.x = c('iso','sector','fuel'),
                                            addEntries = FALSE)

# ---------------------------------------------------------------------------
# 7. Final Processing

CEDS_total_extended[is.na(CEDS_total_extended)] <- 0
final <- CEDS_total_extended[,c('iso','sector','fuel','units', paste0('X',historical_pre_extension_year:end_year))]
final <- final[ with( final, order( iso, sector, fuel ) ), ]

# ---------------------------------------------------------------------------
# 5. Write

writeData(CEDS_total_extended, domain = "FIN_OUT", fn = paste0( "S.",em,"_Extended_CEDS_Emissions" ), meta = TRUE )

logStop()

# END
