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
# 0.5 Load Package, Define Functions

  loadPackage('zoo')

  rep.row <- function(x,n){ matrix(rep(x,each=n),nrow=n) }

  split_countries <- function(all_data_in,
                              combined_iso,
                              dissagregate_iso,
                              dis_end_year,
                              dis_start_year = historical_pre_extension_year,
                              population_data = population){
    
    aggregate_data <- all_data_in[ which( all_data_in$iso == combined_iso),]
    dissaggregate_years <- paste0('X', dis_start_year : dis_end_year)
    
    # template for extended disaggregate - fill
    disaggregate_extended <- all_data_in[ which( all_data_in$iso %in% dissagregate_iso), c('iso','fuel')]
    
    # driver data - population disaggregate and aggregate
    
    #part of the ratio - disaggregate population
    disaggregate_population_data <- disaggregate_extended
    disaggregate_population_data[dissaggregate_years] <- population_data[ match(disaggregate_population_data$iso,population_data$iso) , dissaggregate_years]
    #part of the ratio - aggregate population
    aggregate_pop <- population_data[ which( population_data$iso %in% dissagregate_iso), c('iso',dissaggregate_years)]
    aggregate_pop <- rbind(aggregate_pop,c(combined_iso,colSums(aggregate_pop[dissaggregate_years])))
    aggregate_pop[dissaggregate_years] <- sapply(aggregate_pop[dissaggregate_years],FUN=as.numeric)
    aggregate_population_data <- disaggregate_extended
    aggregate_population_data[dissaggregate_years] <- aggregate_pop[ match(rep( x=combined_iso,times=nrow(aggregate_population_data)),
                                                                      aggregate_pop$iso) , dissaggregate_years]
    
    # multiplyer - aggregate CDIAC data
    aggregate_cdiac_multiplier <- disaggregate_extended 
    aggregate_cdiac_multiplier[dissaggregate_years] <- aggregate_data[ match(aggregate_cdiac_multiplier$fuel,aggregate_data$fuel),
                                                                       dissaggregate_years]
    
    #Extend Data
    disaggregate_extended[dissaggregate_years] <- as.matrix(aggregate_cdiac_multiplier[dissaggregate_years]) * 
      as.matrix(disaggregate_population_data[dissaggregate_years]) / as.matrix(aggregate_population_data[dissaggregate_years])
    
    # add back to full data
    cdiac_aggregate_corrected <- replaceValueColMatch(all_data_in,disaggregate_extended,
                                                      x.ColName = dissaggregate_years,
                                                      match.x = c('iso','fuel'),
                                                      addEntries = FALSE)
    #remove aggregate from final cdiac data to prevent double counting
    cdiac_aggregate_corrected <- cdiac_aggregate_corrected[-which(cdiac_aggregate_corrected$iso == combined_iso),]
    
    return(cdiac_aggregate_corrected)
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
  # 3. Remove negative CDIAC values, extend to 1750
  
  id_cdiac <- cdiac_year_wide[, 1:3]
  years_cdiac <- cdiac_year_wide[, 4:ncol(cdiac_year_wide)]
  
  years_cdiac[years_cdiac < 0 ] <- NA
  years_cdiac <- as.data.frame(t(apply(years_cdiac,MARGIN=1, na.approx)  ) , stringsAsFactors = FALSE )
  names(years_cdiac) <- names(cdiac_year_wide)[4:ncol(cdiac_year_wide)]
  
  cdiac_corrected <- cbind(id_cdiac,years_cdiac)
  cdiac_corrected$X1750 <- cdiac_corrected$X1751
  cdiac_corrected$fuel <- as.character(cdiac_corrected$fuel)
  cdaic_start_year <- 1750
  
  X_cdiac_years <- paste0('X',cdiac_start_year:cdiac_end_year)
  
  # -----------------------------------------------------------------------------------------------------------
  # 4. Split Countries
 
  # FSU
    cdiac_ussr_corrected <- split_countries(all_data_in = cdiac_corrected,
                                          combined_iso = 'USSR',
                                          dis_end_year = 1991,
                                          dissagregate_iso = c('aze','arm' , 'blr','est','geo','kaz','kgz','lva',
                                                                   'ltu','mda','tjk','tkm','ukr','uzb', 'rus'))
   #  Yugoslavia
  cdiac_yug_corrected  <- split_countries(all_data_in = cdiac_ussr_corrected,
                                          combined_iso = 'yug',
                                          dis_end_year = 1991,
                                          dissagregate_iso = c('bih','hrv','mkd','svn', 'scg'))
  # Serbia and Montenegro
  cdiac_scg_corrected  <- split_countries(all_data_in = cdiac_yug_corrected,
                                          combined_iso = 'scg',
                                          dis_end_year = 2005,
                                          dissagregate_iso = c('srb','mne'))
  # Czechoslovakia
    cdiac_csk_corrected  <- split_countries(all_data_in = cdiac_scg_corrected,
                                           combined_iso = 'csk',
                                           dis_end_year = 1991,
                                           dissagregate_iso = c('cze','svk') ) 
  # Czechoslovakia
  cdiac_csk_corrected  <- split_countries(all_data_in = cdiac_scg_corrected,
                                          combined_iso = 'csk',
                                          dis_end_year = 1991,
                                          dissagregate_iso = c('cze','svk') ) 
  # East and West Pakistan
  cdiac_pak_corrected  <- split_countries(all_data_in = cdiac_csk_corrected,
                                          combined_iso = 'EAST_WEST_PAKISTAN',
                                          dis_end_year = 1971,
                                          dissagregate_iso = c('pak','bgd') ) 
  # United Korea
  cdiac_kor_corrected  <- split_countries(all_data_in = cdiac_pak_corrected,
                                          combined_iso = 'UNITED_KOREA',
                                          dis_end_year = 1944,
                                          dissagregate_iso = c('prk','kor') ) 
  
  cdiac_final <- cdiac_kor_corrected
  
  # -----------------------------------------------------------------------------------------------------------
  # 8. Add liquid and gas fuels 
 
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
  
  # cdiac_solid
  cdiac_solid_fuel <- cdiac_final[ which(cdiac_final$fuel == 'solid_fuels'), ]
  
# -----------------------------------------------------------------------------------------------------------
# 5. Output
 
  writeData(cdiac_final, domain = "MED_OUT", fn = paste0( "E.CO2_CDIAC_inventory" ), meta = F )
  writeData(cdiac_cement, domain = "MED_OUT", fn = paste0( "E.CO2_CDIAC_Cement" ), meta = F )
  writeData(cdiac_total, domain = "MED_OUT", fn = paste0( "E.CO2_CDIAC_Total_CO2" ), meta = F )
  writeData(cdiac_solid_fuel, domain = "MED_OUT", fn = paste0( "E.CO2_CDIAC_solid_fuel" ), meta = F )
  
  writeData(cdiac_region_fuel, domain = "DIAG_OUT", fn = paste0( "E.CO2_CDIAC_by_figure_region_CIDACfuel" ), meta = TRUE )
  writeData(cdiac_iso_fuel, domain = "DIAG_OUT", fn = paste0( "E.CO2_CDIAC_by_iso_CIDACfuel" ), meta = TRUE )
  
  logStop()
    
# END