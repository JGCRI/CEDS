# ------------------------------------------------------------------------------
# Program Name: H1.2.add_activity_Bond_industrial_biomass.R
# Author: Rachel Hoesly
# Program Purpose: Extend Industrial Biomass with bond data. Transition from ceds to bond values
#               
# Output Files:'H.',em,'_total_activity_extended_db'
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
headers <- c( "data_functions.R","process_db_functions.R", "ModH_extention_functions.R") # Additional function files may be required.
log_msg <- "Extending coal and biomass activity_data before 1960 with CDIAC-Bond data" # First message to be printed to the log
script_name <- "H1.2.add_activity_Bond_industrial_biomass.R"

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

iea_start_year <- readData( 'ENERGY_IN' , 'IEA_iso_start_data', meta = F )

un_pop <- readData( "MED_OUT" , 'A.UN_pop_master' , meta = F )

# ---------------------------------------------------------------------------
# 1. Definte Fuction

# un population
un_pop$X_year <- paste0( "X" , un_pop$year)
un_pop$pop <- as.numeric(un_pop$pop)
population <- cast( un_pop[which ( un_pop$year %in% historical_pre_extension_year:end_year ) , ] , 
                    iso ~ X_year, value = 'pop')



disaggregate_countries <- function(original_data,aggregate_country,disaggregate_countries, aggregate_end_year,
                                   data_start_year = 1850, id_cols=c('iso','fuel'), population){

aggregate_country_data <- original_data[ which( original_data$iso == aggregate_country),]
disaggregate_years <- paste0('X', data_start_year:aggregate_end_year)

# template for extended disaggregate - fill
disaggregate_extended <- original_data[ which( original_data$iso %in% disaggregate_countries), id_cols]

# driver data - population disaggregate and aggregate_country
#part of the ratio - disaggregate population
disaggregate_population <- disaggregate_extended
disaggregate_population <- population[ match(disaggregate_population $iso,population$iso) , disaggregate_years]
#part of the ratio - aggregate_country population
aggregate_country_pop <- population[ which( population$iso %in% disaggregate_countries), c('iso',disaggregate_years)]
aggregate_country_pop <- rbind(aggregate_country_pop,c(aggregate_country,colSums(aggregate_country_pop[disaggregate_years])))
aggregate_country_pop[disaggregate_years] <- sapply(aggregate_country_pop[disaggregate_years],FUN=as.numeric)
aggregate_country_population <- disaggregate_extended
aggregate_country_population[disaggregate_years] <- aggregate_country_pop[ match(rep( x=aggregate_country,times=nrow(aggregate_country_population)),
                                              aggregate_country_pop$iso) , disaggregate_years]

#TODO: matches by fuel, not robust (will work for this script)
# multiplyer - aggregate_country CDIAC data
aggregate_country_cdiac_multiplier <- disaggregate_extended[id_cols]
aggregate_country_cdiac_multiplier[disaggregate_years] <- aggregate_country_data[ match(aggregate_country_cdiac_multiplier$fuel,aggregate_country_data$fuel),
                                               disaggregate_years]

#Extend Data
disaggregate_extended[disaggregate_years] <- as.matrix(aggregate_country_cdiac_multiplier[disaggregate_years]) * 
  as.matrix(disaggregate_population[disaggregate_years]) / as.matrix(aggregate_country_population[disaggregate_years])

# add back to full data
corrected <- replaceValueColMatch(original_data,disaggregate_extended,
                                             x.ColName = disaggregate_years,
                                             match.x = id_cols,
                                             addEntries = FALSE)
#remove aggregate_country from final cdiac data to prevent double counting
corrected <- corrected[-which(corrected$iso == aggregate_country),]
return(corrected)
}


# ---------------------------------------------------------------------------
# 2. Select data to extend based on extension drivers

activity <- activity_all

start_years <- c(1960,1971)

iea_start_year$start_year <- 1971
iea_start_year[which(iea_start_year$iso %in% c('usa','aus','swe','can','prt','pol')),'start_year'] <- 1960
# ---------------------------------------------------------------------------
# 3. Bond Data processing

# Industrial Biomass
bond <- merge( bond_historical, iso_map[,c('iso','Country')])
bond <- bond[which( bond$Tech %in% c( " Category       ") &
                      bond$Sector %in% c( " Industrial  ")  ) ,]
bond <- merge( bond, fuel_map)
bond <- bond[which( bond[,'Fuel (kt)'] > 0),]
bond <- bond[which( bond$fuel == 'biomass'),]
bond <- bond[which( bond[,'Year'] < 2005),]
bond$Year <- paste0('X',bond$Year)

bond <- aggregate(bond["Fuel (kt)"],
                  by = list(iso = bond$iso,
                            Year = bond$Year),
                  FUN = sum)
bond_industrial_biomass <- cast(bond, iso ~ Year, value = 'Fuel (kt)')
bond_industrial_biomass$fuel <- 'biomass'
# Intepolate years
bond_industrial_biomass[ paste0('X',1850:2000)[ paste0('X',1850:2000)  %!in% names(bond_industrial_biomass)]  ] <- NA
bond_industrial_biomass <- bond_industrial_biomass[ , c('iso','fuel',paste0('X',1850:2000))]

bond_industrial_biomass[ , c(paste0('X',1850:2000))] <- interpolate_NAs(bond_industrial_biomass[ , c(paste0('X',1850:2000))])


# ---------------------------------------------------------------------------
# 3. CEDS Data processing

industry_sectors <- c( '1A2a_Ind-Comb-Iron-steel',
                       '1A2b_Ind-Comb-Non-ferrous-metals',
                       '1A2c_Ind-Comb-Chemicals',
                       '1A2d_Ind-Comb-Pulp-paper',
                       '1A2e_Ind-Comb-Food-tobacco',
                       '1A2f_Ind-Comb-Non-metalic-minerals',
                       '1A2g_Ind-Comb-Construction',
                       '1A2g_Ind-Comb-machinery',
                       '1A2g_Ind-Comb-mining-quarying',
                       '1A2g_Ind-Comb-other',
                       '1A2g_Ind-Comb-textile-leather',
                       '1A2g_Ind-Comb-transpequip',
                       '1A2g_Ind-Comb-wood-products' )

ceds_industrial_biomass <- activity[ which( activity$fuel == 'biomass' &
                                            activity$sector %in% industry_sectors), ]

ceds_industrial_biomass_agg <- aggregate( ceds_industrial_biomass[ X_emissions_years ], 
                                      by = list( iso = ceds_industrial_biomass$iso,
                                                 fuel = ceds_industrial_biomass$fuel),
                                      FUN = sum)

# ---------------------------------------------------------------------------
# 3. Disaggregate Bond Data - FSU and USSR

bond_industrial_biomass_fsu<- disaggregate_countries(original_data = bond_industrial_biomass,
                                                     aggregate_country = 'ussr',
                                                     disaggregate_countries = c('aze','arm' , 'blr','est','geo','kaz','kgz','lva',
                                                                                'ltu','mda','tjk','tkm','ukr','uzb', 'rus'), 
                                                     aggregate_end_year = 1991,
                                                     data_start_year = 1850, id_cols=c('iso','fuel'), population)

bond_industrial_biomass_yug<- disaggregate_countries(original_data = bond_industrial_biomass_fsu,
                                                     aggregate_country = 'yug',
                                                     disaggregate_countries = c('bih','hrv','mkd','svn', 'srb','mne'), 
                                                     aggregate_end_year = 1991,
                                                     data_start_year = 1850, id_cols=c('iso','fuel'), population)


# ---------------------------------------------------------------------------
# 3. Total Industry Biomass

# Merge bond and ceds industrial biomass

#get bond and ceds data into same format, add zero lines
biomass_template <- merge(ceds_industrial_biomass_agg, bond_industrial_biomass_yug,
                  by = c('iso','fuel'), all = TRUE)[,c('iso','fuel')]
biomass_template$fuel <- 'biomass'

ceds_biomass_full<- biomass_template
ceds_biomass_full[paste0('X', 1960:end_year)] <- ceds_industrial_biomass_agg[match(ceds_biomass_full$iso, ceds_industrial_biomass_agg$iso), paste0('X', 1960:end_year)]
bond_biomass_full<- biomass_template
bond_biomass_full[paste0('X', 1850:2000)] <- bond_industrial_biomass_yug[match(bond_biomass_full$iso, bond_industrial_biomass_yug$iso), paste0('X', 1850:2000)]

bond_biomass_full[is.na(bond_biomass_full)] <- 0
ceds_biomass_full[is.na(ceds_biomass_full)] <- 0

#loop over different IEA data start years
blended_biomass <- bond_biomass_full[,c('iso','fuel')]
blended_biomass[paste0('X',1850:end_year)] <- NA

for ( i in seq_along(start_years ) ) {
  # select countries
  countries <- iea_start_year[which( iea_start_year$start_year == start_years[i]),'iso']
  
  data_start <- start_years[i]
  merge_years <- (data_start - 35):(data_start+5)
  
  #linearly extend ceds data for merging
  ceds_biomass_full[paste0('X',1920:(data_start-1))] <- ceds_biomass_full[paste0('X',data_start)]
  ceds_biomass_full <- ceds_biomass_full[,c('iso','fuel',paste0('X',1920:end_year))]
  
  ceds_fractions <- (1:length(merge_years)-1)*(1/(length(1:length(merge_years))-1)) 
  bond_fractions <- 1-ceds_fractions
  
  # pre blending bond
  blended_biomass [ which( blended_biomass$iso %in% countries), paste0('X',1850:(merge_years[1]-1)) ]  <- 
            bond_biomass_full[ which( bond_biomass_full$iso %in% countries) , paste0('X',1850:(merge_years[1]-1)) ]
  # blending  
  blended_biomass[which( blended_biomass$iso %in% countries), paste0('X',merge_years)]  <- 
            matrix( data = rep(x=ceds_fractions, times = length(countries)), nrow = length(countries), byrow=T) * ceds_biomass_full[which( ceds_biomass_full$iso %in% countries),paste0('X',merge_years)] + 
            matrix( data = rep(x=bond_fractions, times = length(countries)), nrow = length(countries), byrow=T) * bond_biomass_full[which( bond_biomass_full$iso %in% countries),paste0('X',merge_years)]
  # post blending ceds
  blended_biomass[which( blended_biomass$iso %in% countries), paste0('X',(merge_years[length(merge_years)]+1):end_year) ]  <- 
          ceds_biomass_full[ which( ceds_biomass_full$iso %in% countries) , paste0('X',(merge_years[length(merge_years)]+1):end_year) ]
  }

blended_biomass <- blended_biomass[ , c('iso','fuel',paste0('X',1850:end_year))]


# ---------------------------------------------------------------------------
# 6. Calculate sector breakdown

ceds_sector_breakdown_list <- list()
for ( i in seq_along((start_years))){
  countries <- iea_start_year[which( iea_start_year$start_year == start_years[i]),'iso']
  disaggregate <- ceds_industrial_biomass[which( ceds_industrial_biomass$iso %in% countries),]
  
  percentages <- disaggregate[,c('iso','fuel','sector',paste0('X',start_years[i]))]
  percentages$total <- ceds_industrial_biomass_agg[match( paste(percentages$iso, percentages$fuel),
                                    paste(ceds_industrial_biomass_agg$iso, ceds_industrial_biomass_agg$fuel)), paste0('X',start_years[i]) ]
  
  percentages['percent'] <- percentages[paste0('X',start_years[i])]/percentages$total
  percentages[which( percentages$total == 0),'percent'] <- 0
  
  #format
  percentages <- percentages[ ,c('iso','sector','fuel','percent') ]
  names(percentages) <- c('iso','sector','fuel', paste0('X',start_years[i]))
  
  ceds_sector_breakdown_list[[i]]<-percentages  
}
ceds_sector_breakdown <- do.call(rbind.fill, ceds_sector_breakdown_list)

ceds_sector_breakdown$X1850 <- 0
ceds_sector_breakdown[which(ceds_sector_breakdown$sector == '1A2g_Ind-Comb-other'),'X1850'] <- 1

# Intepolate years (keep 5 year intervals, fill in missing data)
ceds_sector_breakdown [ paste0('X',1850:1971)[ paste0('X',1850:1971) %!in% names(ceds_sector_breakdown) ] ] <- NA
ceds_sector_breakdown <- ceds_sector_breakdown[ , c('iso','sector','fuel',paste0('X',1850:1971))]
ceds_sector_breakdown[ , paste0('X',1850:1971)] <- interpolate_NAs(ceds_sector_breakdown[ , paste0('X',1850:1971)])

# ---------------------------------------------------------------------------
# 6. Disaggregate to CEDS sectors

for ( i in seq_along((start_years))){
  countries <- iea_start_year[which( iea_start_year$start_year == start_years[i]),'iso']
  
  percentages <- ceds_sector_breakdown[which(ceds_sector_breakdown$iso %in% countries),]
    
    # total fuel template - same order as percentages
    aggregate <- percentages[c('iso','sector','fuel')]
    aggregate[ paste0('X',1850:(start_years[i]-1) ) ] <- 0
    aggregate <- replaceValueColMatch(aggregate, blended_biomass,
                                      x.ColName = paste0('X',1850:(start_years[i]-1) ),
                                      match.x = c('iso','fuel'),
                                      addEntries = F)
    # multiply total by percentages breakdown
    aggregate[paste0('X',1850:(start_years[i]-1) )] <- aggregate[paste0('X',1850:(start_years[i]-1) )]*
      percentages[paste0('X',1850:(start_years[i]-1) )]
    
    # save data to list 
    if (i == 1) dissagregate_biomass_1960 <- aggregate
    if (i == 2) dissagregate_biomass_1971 <- aggregate
    
  }

# ---------------------------------------------------------------------------
# 6. Extend 1750 to 1850 with Population

# set up extention data (population)
  extension_data_1960 <- dissagregate_biomass_1960[,c('iso','sector','fuel')]
  extension_data_1960[ paste0('X',1750:1900) ] <- population[match(extension_data_1960$iso, population$iso), paste0('X',1750:1900)]
  
  extension_data_1971 <- dissagregate_biomass_1971[,c('iso','sector','fuel')]
  extension_data_1971[ paste0('X',1750:1900) ] <- population[match(extension_data_1971$iso, population$iso), paste0('X',1750:1900)]

# add 1750 - 1849 columns
  dissagregate_biomass_1960[ paste0('X', 1750:1849)] <- NA
  dissagregate_biomass_1971[ paste0('X', 1750:1849)] <- NA

# extend
  final_extended_1960 <- extend_data_on_trend(driver_trend = extension_data_1960, input_data = dissagregate_biomass_1960 , start = 1750, end = 1850)
  final_extended_1971 <- extend_data_on_trend(driver_trend = extension_data_1971, input_data = dissagregate_biomass_1971 , start = 1750, end = 1850)

# ---------------------------------------------------------------------------
# 6. Add to Activity Database

activity.replace <- activity[ which(activity$sector %in% industry_sectors &
                                   activity$fuel == 'biomass'),] 
activity.done <- activity[ which(activity$sector %!in% industry_sectors |
                                      activity$fuel != 'biomass'),] 

# loop over IEA data start years
for ( i in seq_along ( start_years ) ){
  countries <- iea_start_year[which( iea_start_year$start_year == start_years[i]),'iso']
  
  if( i == 1) biomass <- final_extended_1960
  if( i == 2) biomass <- final_extended_1971
  
  new_activity <- activity.replace[ which( activity.replace$iso %in% countries),]
  activity.replace <- activity.replace[ which( activity.replace$iso %!in% countries),]
  
  new_activity [ paste0('X',1750:(start_years[i]-1)) ] <- biomass[ match( paste(new_activity$iso, new_activity$fuel, new_activity$sector ),
                                                                       paste(biomass$iso, biomass$fuel, biomass$sector ) )  , paste0('X',1750:(start_years[i]-1)) ]
  activity.done <- rbind( activity.done, new_activity)
}

activity <- rbind( activity.done,activity.replace)


# ---------------------------------------------------------------------------
# 7. Write to database

writeData( rbind.fill(dissagregate_biomass_1960, dissagregate_biomass_1971),
           'DIAG_OUT', paste0('H.',em,'_extended_industrial_biomass'))
writeData(blended_biomass, 'DIAG_OUT', paste0('H.',em,'_extended_total_industrial_biomass'))

if( !(nrow(activity_all) == nrow(activity) & ncol(activity_all) == ncol(activity) ) ){
  stop( "New and old activity do not match") } else{
    writeData( activity, "MED_OUT" , paste0('H.',em,'_total_activity_extended_db')) }

logStop()
