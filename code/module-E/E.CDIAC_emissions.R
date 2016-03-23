# ------------------------------------------------------------------------------
# Program Name: E.CDIAC_emissions.R
# Author(s): Rachel Hoesly
# Date Last Updated: Feb 3, 2016
# Program Purpose: To read in & reformat CDIAC emissions data.
# Input Files: 
# Output Files: 
# Notes: 
# TODO: 
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Set working directory to the CEDS “input” directory & define PARAM_DIR as the
# location of the CEDS “parameters” directory, relative to the new working directory.
dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
for ( i in 1:length( dirs ) ) {
  setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
  wd <- grep( 'CEDS/input', list.dirs(), value = T )
  if ( length( wd ) > 0 ) {
    setwd( wd[ 1 ] )
    break
  }
}
INPUT <- paste(getwd())
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R") # Any additional function files required
log_msg <- "Read and format CDIAC emissions" # First message to be printed to the log
script_name <- "E.CDIAC_emissions.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# -----------------------------------------------------------------------------------------------------------
# 0.5 Load Package

  loadPackage('zoo')

rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

# -----------------------------------------------------------------------------------------------------------
# 1. Read in files
  cdiac_read <- readData('EM_INV', domain_extension = "CDIAC/",  'CDIAC_national_1751_2011'  ) 
  MCL <- readData( "MAPPINGS", "Master_Country_List" )
  cdiac_country_map <- readData('EM_INV', domain_extension = "CDIAC/",  'CDIAC_country_map'  ) 
  un_pop <- readData( "MED_OUT" , 'A.UN_pop_master' )
  
# -----------------------------------------------------------------------------------------------------------
# 2. Formatting Data to ceds format

  cdiac_start_year <- 1751
  cdiac_end_year <- 2011
  
  
  # un population
  un_pop$X_year <- paste0( "X" , un_pop$year)
  un_pop$pop <- as.numeric(un_pop$pop)
  population <- cast( un_pop[which ( un_pop$year %in% historical_pre_extension_year:end_year ) , ] , 
                      iso ~ X_year, value = 'pop')
  
  # cdiac
  cdiac_fuel_wide <- cdiac_read
  cdiac_fuel_wide$units <- 'kt-C'
  cdiac_fuel_wide <- cdiac_fuel_wide[-1:-2,]
  cdiac_fuels <- c('Total_CO2' , 'solid_fuels' , 'liquid_fuels', 'gas_fuels','cement_production',
                   'gas_flaring','per_capital_CO2', 'bunker_fuels')
  names(cdiac_fuel_wide)[3:10] <- cdiac_fuels
  cdiac_fuel_wide$X_year <- paste0('X',cdiac_fuel_wide$Year) 
  
  # make numeric
  cdiac_fuel_wide[, c('Year',cdiac_fuels) ] <- sapply(cdiac_fuel_wide[ , c('Year',cdiac_fuels) ], FUN = as.numeric )
  
  # add iso and aggregate
  cdiac_fuel_wide$iso <- cdiac_country_map[ match( cdiac_fuel_wide$Nation ,
                                         cdiac_country_map$CDIAC )  ,'iso']

  cdiac_fuel_wide <- aggregate( cdiac_fuel_wide[ cdiac_fuels ] ,
                                by = list( iso = cdiac_fuel_wide$iso,
                                           year = cdiac_fuel_wide$Year,
                                           X_year = cdiac_fuel_wide$X_year,
                                           units = cdiac_fuel_wide$units),
                                FUN = sum)
  
  # reshape to standard ceds format, select post 1850 years
  cdiac_long <- melt(cdiac_fuel_wide, id = c('iso','year','X_year','units'))
  names(cdiac_long)[which(names(cdiac_long) == 'variable')] <- 'fuel'
  
  cdiac_year_wide <- cast(cdiac_long, iso + fuel + units ~ X_year)
  cdiac_year_wide[is.na(cdiac_year_wide)] <- 0

  # -----------------------------------------------------------------------------------------------------------
  # 2. Remove negative CDIAC values, extend to 1750
  
  id_cdiac <- cdiac_year_wide[, 1:3]
  years_cdiac <- cdiac_year_wide[, 4:ncol(cdiac_year_wide)]
  
  years_cdiac[years_cdiac < 0 ] <- NA
  years_cdiac <- as.data.frame(t(apply(years_cdiac,MARGIN=1, na.approx)  ) , sstringsAsFactors = FALSE )
  names(years_cdiac) <- names(cdiac_year_wide)[4:ncol(cdiac_year_wide)]
  
  cdiac_corrected <- cbind(id_cdiac,years_cdiac)
  cdiac_corrected$X1750 <- cdiac_corrected$X1751
  cdiac_corrected$fuel <- as.character(cdiac_corrected$fuel)
  cdaic_start_year <- 1750
  
  X_cdiac_years <- paste0('X',cdiac_start_year:cdiac_end_year)
  
  # -----------------------------------------------------------------------------------------------------------
  # 2. Extend FSU
  #        ratio: Pop(FSU)/Pop(USSR)
  #   CDIAC(FSU): CDIAC(FSU,fuel)*ratio
  
  FSU_countries <- c('aze','arm' , 'blr','est','geo','kaz','kgz','lva',
                     'ltu','mda','tjk','tkm','ukr','uzb')
  FSU_end_year <- 1991
  USSR_data <- cdiac_corrected[ which( cdiac_corrected$iso == 'USSR'),]
  FSU_years <- paste0('X', historical_pre_extension_year:FSU_end_year)
  
  # template for extended FSU - fill
  FSU_extended <- cdiac_corrected[ which( cdiac_corrected$iso %in% FSU_countries), c('iso','fuel')]
  
  # driver data - population FSU and USSR
  #part of the ratio - FSU population
  FSU_population <- FSU_extended
  FSU_population[FSU_years] <- population[ match(FSU_population$iso,population$iso) , FSU_years]
  #part of the ratio - USSR population
  USSR_pop <- population[ which( population$iso %in% FSU_countries), c('iso',FSU_years)]
  USSR_pop <- rbind(USSR_pop,c('USSR',colSums(USSR_pop[FSU_years])))
  USSR_pop[FSU_years] <- sapply(USSR_pop[FSU_years],FUN=as.numeric)
  USSR_population <- FSU_extended
  USSR_population[FSU_years] <- USSR_pop[ match(rep( x='USSR',times=nrow(USSR_population)),
                                                       USSR_pop$iso) , FSU_years]
  
  # multiplyer - USSR CDIAC data
  USSR_cdiac_multiplier <- FSU_extended 
  USSR_cdiac_multiplier[FSU_years] <- USSR_data[ match(USSR_cdiac_multiplier$fuel,USSR_data$fuel),
                                                    FSU_years]
  
  #Extend Data
  FSU_extended[FSU_years] <- as.matrix(USSR_cdiac_multiplier[FSU_years]) * 
                             as.matrix(FSU_population[FSU_years]) / as.matrix(USSR_population[FSU_years])
  
  # add back to full data
  cdiac_final <- replaceValueColMatch(cdiac_corrected,FSU_extended,
                                      x.ColName = FSU_years,
                                      match.x = c('iso','fuel'),
                                      addEntries = FALSE)
  #remove USSR from final cdiac data to prevent double counting
  cdiac_final <- cdiac_final[-which(cdiac_final$iso == 'USSR'),]
  
  # -----------------------------------------------------------------------------------------------------------
  # 2. Extend Korea
  #        ratio: Pop(FSU)/Pop(USSR)
  #   CDIAC(FSU): CDIAC(FSU,fuel)*ratio
  
# -----------------------------------------------------------------------------------------------------------
# 5. Add liquid and gas fuels 
  
  X_cdiac_years <- c('X1750',X_cdiac_years)
  cdiac_final <- cdiac_final[ ,c('iso','fuel', X_cdiac_years)]
  
  cdiac_liquid_and_gas <- cdiac_final[which(cdiac_final$fuel %in% c( 'liquid_fuels','gas_fuels')),]
  cdiac_liquid_and_gas <- aggregate( cdiac_liquid_and_gas[X_cdiac_years],
                                     by = list(iso = cdiac_liquid_and_gas$iso),
                                     FUN = sum)
  
  cdiac_liquid_and_gas$fuel <- 'liquid_and_gas_fuels'
  
  cdiac_liquid_and_gas <- cdiac_liquid_and_gas[ ,c('iso','fuel', X_cdiac_years)]
  
  cdiac_final <- rbind(cdiac_final,cdiac_liquid_and_gas)
 
   # sort and organize
  cdiac_final <- cdiac_final[ ,c('iso','fuel', X_cdiac_years)]
  cdiac_final <- cdiac_final[ with( cdiac_final, order( iso, fuel ) ), ]
  
  
# -----------------------------------------------------------------------------------------------------------
# 6. Summary  
  # non combustion 
  cdiac_cement <- cdiac_final[ which( cdiac_final$fuel %in% c("cement_production") ) , ]
  cdiac_total <- cdiac_final[ which( cdiac_final$fuel %in% c("Total_CO2") ) , ]
  
  # Figure region and cdiac fuel
  cdiac_region_fuel <- cdiac_final 
  cdiac_region_fuel$Figure_Region <- MCL[ match( cdiac_region_fuel$iso , MCL$iso ) , "Figure_Region"]
  cdiac_region_fuel <- aggregate( cdiac_region_fuel[ X_cdiac_years],
                                    by = list( Figure_Region = cdiac_region_fuel$Figure_Region,
                                               fuel = cdiac_region_fuel$fuel),
                                    FUN = sum)
  cdiac_region_fuel <- cdiac_region_fuel[ with( cdiac_region_fuel, order( Figure_Region, fuel ) ), ]
  
  # ceds iso and cdiac fuel
  cdiac_iso_fuel <- cdiac_final
  cdiac_iso_fuel <- aggregate( cdiac_iso_fuel[ X_cdiac_years],
                                  by = list( iso = cdiac_iso_fuel$iso,
                                             fuel = cdiac_iso_fuel$fuel),
                                  FUN = sum)
  cdiac_iso_fuel <- cdiac_iso_fuel[ with( cdiac_iso_fuel, order( iso, fuel ) ), ]
  
# -----------------------------------------------------------------------------------------------------------
# 5. Output
 
  writeData(cdiac_final, domain = "MED_OUT", fn = paste0( "E.CO2_CDIAC_inventory" ), meta = F )
  writeData(cdiac_cement, domain = "MED_OUT", fn = paste0( "E.CO2_CDIAC_Cement" ), meta = F )
  writeData(cdiac_total, domain = "MED_OUT", fn = paste0( "E.CO2_CDIAC_Total_CO2" ), meta = F )
  
  writeData(cdiac_region_fuel, domain = "DIAG_OUT", fn = paste0( "E.CO2_CDIAC_by_figure_region_CIDACfuel" ), meta = TRUE )
  writeData(cdiac_iso_fuel, domain = "DIAG_OUT", fn = paste0( "E.CO2_CDIAC_by_iso_CIDACfuel" ), meta = TRUE )
  
  logStop()
    
# END