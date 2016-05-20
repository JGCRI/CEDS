# ------------------------------------------------------------------------------
# Program Name: E.CDIAC_emissions.R
# Author(s): Rachel Hoesly
# Date Last Updated: Feb 3, 2016
# Program Purpose: To read in & reformat CDIAC emissions data.
# Input Files: A.UN_pop_master.csv,CDIAC_national_1751_2011.csv, CDIAC_country_map.csv
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

# split cdiac countries - get ratio of emission by fuel in ratio year, extend that ratio with cdiac emissions,
# and renomalize to sum to 1 by fuel
  
  split_cdiac <- function(all_data_in,
                        combined_iso,
                        dissagregate_iso,
                        dis_end_year,
                        range_cdiac = 2,
                        dis_start_year = historical_pre_extension_year,
                        ratio_start_year_cdiac = (dis_end_year+1),
                        population_data = population) {
  
  if( dis_end_year == cdiac_end_year) all_data_in[paste0('X',(cdiac_end_year+1):(cdiac_end_year+range_cdiac)) ] <- all_data_in[paste0('X',cdiac_end_year) ]
     
  # Define useful variables
  dissaggregate_years <- paste0('X', dis_start_year : dis_end_year)
  CDIAC_categories <- unique( all_data_in$fuel)
  ratio_years <- paste0('X',c(ratio_start_year_cdiac + 0:(range_cdiac-1)))
  
  # Subset CDIAC data

  if( all(dissagregate_iso %in% unique(all_data_in$iso)) == FALSE) {
    warning( "Disaggregate iso not in original data, splitting with population")
  dis_pop <- population[which( population$iso %in% dissagregate_iso),c('iso',dissaggregate_years)]   
  agg_pop <- population[which( population$iso %in% combined_iso),c('iso',dissaggregate_years)] 
  
  # population ratio for dissaggregate countries
  pop_ratio <- dis_pop[c('iso',dissaggregate_years)]
  pop_ratio[dissaggregate_years] <- agg_pop[dissaggregate_years]
  pop_ratio[dissaggregate_years] <- dis_pop[dissaggregate_years] / pop_ratio[dissaggregate_years]
  
  # Subset aggregate data
  aggregate_data <- all_data_in[ which( all_data_in$iso == combined_iso), ]
  
  # Disaggregate CDIAC aggregate data
  #set up template
  fuels <- unique(aggregate_data$fuel)
  new_data_agg_template <- data.frame(iso = rep(dissagregate_iso, each = length(fuels)),
                         fuel = rep(fuels, times = length(dissagregate_iso)))
  new_data_agg_template <- merge(new_data_agg_template, aggregate_data[c('fuel',dissaggregate_years)],
                                 all.x=T)
  new_data_agg_template <- new_data_agg_template[order(new_data_agg_template$iso,new_data_agg_template$fuel), c('iso','fuel',dissaggregate_years)]
  
  new_data_ratio_template <- data.frame(iso = rep(dissagregate_iso, each = length(fuels)),
                                      fuel = rep(fuels, times = length(dissagregate_iso)))
  new_data_ratio_template <- merge(new_data_ratio_template, pop_ratio[c('iso',dissaggregate_years)],
                                   all.x=T)
  new_data_ratio_template <- new_data_ratio_template[order(new_data_ratio_template$iso,new_data_ratio_template$fuel),]
  
  # calculate
  new_data <- new_data_agg_template
  new_data[dissaggregate_years] <- new_data_agg_template[dissaggregate_years] * new_data_ratio_template[dissaggregate_years]
  
  # add back to full data
  cdiac_aggregate_corrected <- rbind.fill(all_data_in,new_data)
  cdiac_aggregate_corrected[is.na(cdiac_aggregate_corrected)] <- 0
   
  } else {
  # Subset CDIAC data
  # disaggregate_data <- all_data_in[ which( all_data_in$iso %in% dissagregate_iso), ]
  # aggregate_data <- all_data_in[ which( all_data_in$iso == combined_iso), ]
  # 
  # aggregate_replace <- aggregate( disaggregate_data[ratio_years], 
  #                                 by = list(fuel = disaggregate_data$fuel),
  #                                 FUN = sum)
  # aggregate_data[ paste0('X',ratio_years) ] <- aggregate_replace[ match( aggregate_data$fuel , aggregate_replace$fuel ) ,  
  #                                                                                     ratio_years ] 
  # 
  disaggregate_data <- all_data_in[ which( all_data_in$iso %in% dissagregate_iso), ]
  aggregate_data <- all_data_in[ which( all_data_in$iso == combined_iso), ]
  aggregate_replace <- aggregate( disaggregate_data[paste0('X',(dis_end_year+1):cdiac_end_year)], by = list(fuel = disaggregate_data$fuel),FUN = sum)
  aggregate_data[ paste0('X',(dis_end_year+1):cdiac_end_year) ] <- aggregate_replace[ match( aggregate_data$fuel , aggregate_replace$fuel ) ,  
                                                                                      paste0('X',(dis_end_year+1):cdiac_end_year) ]
  
  # Calculate ratios - disaggregate country percent of aggregate by fuel
  # add Driver identifyer ratio year
  ceds_extension_ratios <- merge(disaggregate_data[,c('iso','fuel',ratio_years)], aggregate_data[,c('fuel', ratio_years)],
                                 by.x = c('fuel'),
                                 by.y = c('fuel'),
                                 all.x = TRUE, all.y = FALSE)
  
  ceds_extension_ratios[ ratio_years ] <- ceds_extension_ratios[ paste0(ratio_years,'.x')]/ceds_extension_ratios[ paste0(ratio_years,'.y')]
  ceds_extension_ratios <- replace(ceds_extension_ratios, ceds_extension_ratios == 'NaN', 0)
  ceds_extension_ratios <- replace(ceds_extension_ratios, is.na(ceds_extension_ratios), 0) 
  
  ceds_extension_ratios <- ceds_extension_ratios[,c('iso','fuel',ratio_years)]
  
  # Extend Ratios based on population trend
  # extend_data_on_trend works better with two variables for id match - change later
  driver_trend_for_ratios <- population_data
  driver_trend_for_ratios$temp <- 'place_holder'
  ceds_extension_ratios$temp <- 'place_holder'
  
  # input_data1 <- ceds_extension_ratios
  
  extended_ceds_extension_ratios <- extend_data_on_trend_cdiac(driver_trend_for_ratios,ceds_extension_ratios, 
                                                               start = dis_start_year, end = dis_end_year,
                                                               ratio_start_year = ratio_start_year_cdiac,
                                                               expand = F,
                                                               range = range_cdiac,
                                                               id_match.driver = c('iso','temp'),
                                                               id_match.input = c('iso','fuel'))
  
  extended_ceds_extension_ratios <- extended_ceds_extension_ratios[,c('iso','fuel',dissaggregate_years,ratio_years)]
  
  # Aggregate 
  extended_ceds_extension_ratios_agg <- aggregate(extended_ceds_extension_ratios[dissaggregate_years],
                                     by = list(iso = extended_ceds_extension_ratios$iso,
                                               fuel = extended_ceds_extension_ratios$fuel),
                                     FUN = sum)
  
  # Renomalize ratios so that all dissaggregate countries sum to 1 by fuel
  # TO DO: get rid of loops
  extended_ceds_extension_ratios_renormalize <- extended_ceds_extension_ratios_agg
  for (i in seq_along(cdiac_fuels)){
    for ( j in seq_along( dissaggregate_years )){
      
      breakdown <- extended_ceds_extension_ratios_renormalize[which( extended_ceds_extension_ratios_renormalize$fuel == cdiac_fuels[i]   ), 
                                                              dissaggregate_years[j] ]
      if ( all(is.na(breakdown)|is.nan(breakdown)|breakdown=='NaN') ){ stop(paste('No data to renormalize for ', fuel[i] ))
        
      }else if( sum(breakdown) != 1 ){  
        sum = sum(breakdown)
        if(sum == 0) sum = 1
        extended_ceds_extension_ratios_renormalize[ which( extended_ceds_extension_ratios_renormalize$fuel == cdiac_fuels[i] ), 
                                                    dissaggregate_years[j] ] <- breakdown/sum
      }
      
    }}
  
  # multiplyer - aggregate CDIAC data
  aggregate_cdiac_multiplier <- extended_ceds_extension_ratios_renormalize[ , c('iso','fuel') ] 
  aggregate_cdiac_multiplier[ dissaggregate_years ] <- aggregate_data[ match(aggregate_cdiac_multiplier$fuel,aggregate_data$fuel),
                                                                       dissaggregate_years]
  
  #Extend Data
  disaggregate_extended <- extended_ceds_extension_ratios_renormalize[ , c('iso','fuel' )]
  disaggregate_extended[dissaggregate_years] <- as.matrix(aggregate_cdiac_multiplier[dissaggregate_years]) * 
    extended_ceds_extension_ratios_renormalize[ dissaggregate_years ] 
  

  # add back to full data
  cdiac_aggregate_corrected <- replaceValueColMatch(all_data_in,disaggregate_extended,
                                                    x.ColName = dissaggregate_years,
                                                    match.x = c('iso','fuel'),
                                                    addEntries = FALSE)
  
  
  }
  
  #remove aggregate from final cdiac data to prevent double counting
  cdiac_aggregate_corrected <- cdiac_aggregate_corrected[which(cdiac_aggregate_corrected$iso %!in% combined_iso), ]
  cdiac_aggregate_corrected <- cdiac_aggregate_corrected[ ,c('iso','fuel',paste0('X', historical_pre_extension_year:cdiac_end_year)) ]
  
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
    cdiac_ussr_corrected <- split_cdiac(all_data_in = cdiac_corrected,
                                          combined_iso = 'USSR',
                                          dis_end_year = 1991,
                                          dissagregate_iso = c('aze','arm' , 'blr','est','geo','kaz','kgz','lva',
                                                                   'ltu','mda','tjk','tkm','ukr','uzb', 'rus'))
   #  Yugoslavia
  cdiac_yug_corrected  <- split_cdiac(all_data_in = cdiac_ussr_corrected,
                                          combined_iso = 'yug',
                                          dis_end_year = 1991,
                                          dissagregate_iso = c('bih','hrv','mkd','svn', 'scg'))
  # Serbia and Montenegro
  cdiac_scg_corrected  <- split_cdiac(all_data_in = cdiac_yug_corrected,
                                          combined_iso = 'scg',
                                          dis_end_year = 2005,
                                          dissagregate_iso = c('srb','mne'))
  # Czechoslovakia
    cdiac_csk_corrected  <- split_cdiac(all_data_in = cdiac_scg_corrected,
                                           combined_iso = 'csk',
                                           dis_end_year = 1991,
                                           dissagregate_iso = c('cze','svk') ) 
  # East and West Pakistan
  cdiac_pak_corrected  <- split_cdiac(all_data_in = cdiac_csk_corrected,
                                          combined_iso = 'EAST_WEST_PAKISTAN',
                                          dis_end_year = 1971,
                                          dissagregate_iso = c('pak','bgd') ) 
  # United Korea
  cdiac_kor_corrected  <- split_cdiac(all_data_in = cdiac_pak_corrected,
                                          combined_iso = 'UNITED_KOREA',
                                          dis_end_year = 1944,
                                          ratio_start_year_cdiac = 1948,
                                          dissagregate_iso = c('prk','kor') ) 
  # French Equatorial Africa
  cdiac_FrEqAf_corrected  <- split_cdiac(all_data_in = cdiac_kor_corrected,
                                      combined_iso = 'FRENCH_EQUATORIAL_AFRICA',
                                      dis_end_year = 1958,
                                      dis_start_year = 1950,
                                      dissagregate_iso = c('caf','cmr','cog','gab','tcd') ) 
  # French Indo-china
  cdiac_FrInCh_corrected  <- split_cdiac(all_data_in = cdiac_FrEqAf_corrected,
                                      combined_iso = 'FRENCH_INDO-CHINA',
                                      dis_end_year = 1954,
                                      dissagregate_iso = c('loa','vnm','khm') ) 
  # French West Africa
  cdiac_FrWeAf_corrected  <- split_cdiac(all_data_in = cdiac_FrInCh_corrected,
                                      combined_iso = 'FRENCH_WEST_AFRICA',
                                      dis_end_year = 1957,
                                      dissagregate_iso = c('mrt','sen','mli','gin','civ','bra','ben','ner') ) 
  #Rwanda-Urundi
  cdiac_RU_corrected  <- split_cdiac(all_data_in = cdiac_FrWeAf_corrected,
                                         combined_iso = 'RWANDA-URUNDI',
                                         dis_end_year = 1961,
                                         dissagregate_iso = c('rwa','bdi') ) 
  
  cdiac_NAR_corrected <- split_cdiac(all_data_in = cdiac_RU_corrected,
                                     combined_iso = 'NETHERLAND_ANTILLES_AND_ARUBA',
                                     dis_end_year = 1985,
                                     dis_start_year = 1926,
                                     dissagregate_iso = c('ant','abw'),
                                     range_cdiac = 2)
  
  cdiac_NA_corrected <- split_cdiac(all_data_in = cdiac_NAR_corrected,
                                    combined_iso = 'ant',
                                    dis_end_year = 2011,
                                    dissagregate_iso = c('cuw','sxm') )
 
  cdiac_RN_corrected <- split_cdiac(all_data_in = cdiac_NA_corrected,
                                    combined_iso = 'RHODESIA-NYASALAND',
                                    dis_end_year = 1963,
                                    dissagregate_iso = c('zmb','mwi') )
  cdiac_LI_corrected <- split_cdiac(all_data_in = cdiac_RN_corrected,
                                    combined_iso = 'LEEWARD ISLANDS',
                                    dis_end_year = 1956,
                                    dis_start_year = 1950,
                                    dissagregate_iso = c('kna','atg') )
  
  cdiac_final <- cdiac_LI_corrected
  
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