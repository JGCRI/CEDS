# ------------------------------------------------------------------------------
# Program Name: H1.2.add_activity_total_coal.R
# Author: Rachel Hoesly
# Program Purpose: Extend Coal data back for all countries from IEA data start (1960, 1971) 
#                 using CDIAC, Bond data, IEA data
#               
# Output Files:  H.EM_total_activity_extended_db
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
log_msg <- "Extending Coal data with bond and IEA" # First message to be printed to the log
script_name <- "H1.2.add_activity_total_coal.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "NH3"

# ---------------------------------------------------------------------------
# 1. Load files

activity_all <- readData( 'MED_OUT',paste0('H.',em,'_total_activity_extended_db') , meta = F)

bond_historical <- readData( "EM_INV" ,"160227_SPEW_BCOCemission", ".xlsx", meta = F )
iso_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_country_map", meta = F )
fuel_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_fuel_map", meta = F )
sector_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_sector_ext_map", ".xlsx", sheet_selection = 'Bond_to_ext',meta = F )
ext_sector_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_sector_ext_map", ".xlsx", sheet_selection = 'CEDS_to_ext',meta = F )

bond_percent_1850 <- readData( 'EXT_IN', 'Bond_sector_extention_percents_1850')

iea_other_coal <- readData( 'MED_OUT','A.IEA_CEDS_coal_difference' )
iea_start_year <- readData( 'ENERGY_IN' , 'IEA_iso_start_data')

cdiac_solid_fuel <- readData( 'MED_OUT' , 'E.CO2_CDIAC_solid_fuel')
# ---------------------------------------------------------------------------
# 2. Select data to extend based on extension drivers

activity <- activity_all

all_countries <- unique(activity$iso)

# ---------------------------------------------------------------------------
# 3. Bond Data processing

# Coal
  bond <- merge( bond_historical, iso_map[,c('iso','Country')])
  bond <- bond[which( bond$Fuel %in% c( " Hard Coal      " , " Brown Coal     ", " Coking Coal    ")),]
  bond <- bond[which( bond[,'Fuel (kt)'] > 0),]
  bond$Year <- paste0('X',bond$Year)
  
  bond <- aggregate(bond["Fuel (kt)"],
                    by = list(iso = bond$iso,
                              Year = bond$Year),
                    FUN = sum)
  
  bond_total_coal <- cast(bond, iso ~ Year, value = 'Fuel (kt)')
  bond_years <- names(bond_total_coal)[names(bond_total_coal) %!in% 'iso']
# Intepolate years (keep 5 year intervals, fill in missing data)
  bond_total_coal <- bond_total_coal[ , c('iso',bond_years)]
  bond_total_coal[ , bond_years] <- interpolate_NAs(bond_total_coal[ , bond_years])

# ---------------------------------------------------------------------------
# 3. CEDS Data processing

# CEDS Reported coal
  ceds_total_coal <- activity_all[which( activity_all$fuel %in% c('hard_coal', 'brown_coal','coal_coke')),
                                c('iso', paste0('X',1960:2010))]
# Add other coal to ceds total coal
  ceds_total_coal <- rbind.fill( ceds_total_coal , iea_other_coal[c('iso',paste0('X',1960:2010))] )
  ceds_total_coal <- aggregate( ceds_total_coal[paste0('X',1960:2010)],
                                by = list( iso = ceds_total_coal$iso) ,
                                FUN = sum)
# Make NA values where IEA data exists  
  ceds_total_coal[ which( ceds_total_coal$iso %in% iea_start_year[which( iea_start_year$start_year == '1965') ,'iso']) ,
                      paste0("X",1960:1964) ] <- NA
  ceds_total_coal[ which( ceds_total_coal$iso %in% iea_start_year[which( iea_start_year$start_year == '1971') ,'iso']) ,
                   paste0("X",1960:1970) ] <- NA

# ---------------------------------------------------------------------------
# 4. Extend total coal with cdiac data
  printLog('Extending Total Coal Values with CDIAC')
  
  ceds_total_coal[ paste0("X",1750:1959) ] <- NA
  ceds_total_coal$fuel <- 'solid_fuels'
  ceds_total_coal$sector <- 'all'
  ceds_total_coal <- ceds_total_coal[  ,c('iso','sector','fuel',paste0("X",1750:2010)) ]
  cdiac_solid_fuel$sector <- 'all'
  cdiac_solid_fuel <- cdiac_solid_fuel[  ,c('iso','sector','fuel',paste0("X",1750:2010)) ]

# loop over different start dates
  ceds_total_coal_extended <- ceds_total_coal
  start_years <- c(1960,1971)
  for ( i in seq_along(start_years ) ){
    countries <- iea_start_year[which( iea_start_year$start_year == start_years[i]),'iso']
    drivers <- cdiac_solid_fuel[which(cdiac_solid_fuel$iso %in% countries ),]
    ceds_total_coal_extended <- extend_data_on_trend(driver_trend = drivers, input_data = ceds_total_coal_extended, 
                                                     start = 1750, end = start_years[i])
  }

#some small countries don't have cdiac or have zero values through extention. 
  ceds_total_coal_extended[ is.na( ceds_total_coal_extended ) ] <- 0

# ---------------------------------------------------------------------------
# 5. Calculate and apply multiplier to merge extended CEDS total coal with bond
#    total coal. 
#    object names: (ceds_total_coal_extended, bond_total_coal) -> final_ceds_total_coal

# calculate multiplier = bond value/cdiac extended value
bond_multiplier <- as.data.frame(bond_total_coal)
  #template
cdiac_extention_values <- bond_total_coal[,c('iso','X1850')]
cdiac_extention_values[bond_years] <- ceds_total_coal_extended[match(cdiac_extention_values$iso,ceds_total_coal_extended$iso),bond_years]
  # calculate
bond_multiplier[bond_years] <- bond_multiplier[bond_years] / cdiac_extention_values[bond_years]
  # correct unreal values
bond_multiplier[bond_years] <- replace( bond_multiplier[bond_years],bond_multiplier[bond_years] == Inf, 1)  
bond_multiplier[bond_years] <- replace( bond_multiplier[bond_years],bond_multiplier[bond_years] == NA, 1) 

bond_multiplier$X1845 <- 1

# Add columns for inbetween years
bond_multiplier[ paste0('X',1845:2000)[ paste0('X',1845:2000)  %!in% names(bond_multiplier)]  ] <- NA
bond_multiplier <- bond_multiplier[ , c('iso',paste0('X',1845:2000))]
# correct multiplier - isr
bond_multiplier[ which( bond_multiplier$iso == 'isr'), paste0('X',1976:1984)] <- NA
# Interpolate
bond_multiplier[ , c(paste0('X',1845:2000))] <- interpolate_NAs(bond_multiplier[ , c(paste0('X',1845:2000))])
bond_multiplier[ is.na(bond_multiplier) ] <- 1

# Apply multipler - total coal = cdaic extended value * multiplier
multipliers <- ceds_total_coal_extended[,c('iso','X1850')]
multipliers[ paste0('X',1845:2000) ] <- bond_multiplier[ match(multipliers$iso,bond_multiplier$iso) , paste0('X',1845:2000) ]
multipliers[ is.na(multipliers) ] <- 1

final_ceds_total_coal <- ceds_total_coal_extended
final_ceds_total_coal[ paste0('X',1845:2000) ] <- final_ceds_total_coal[ paste0('X',1845:2000) ] * multipliers[ paste0('X',1845:2000) ]

# ---------------------------------------------------------------------------
# 5. Dissaggregate total coal into fuel types using CEDS stary year split 

printLog('Disaggregating total coal into fuel types')

# Calculate Ceds fuel % (by iso) in start_year
ceds_coal_fuel <- activity_all[which( activity_all$fuel %in% c('hard_coal', 'brown_coal','coal_coke')),
                                c('iso', 'sector','fuel', paste0('X',1960:2010))]
ceds_coal_fuel <- rbind.fill( ceds_coal_fuel , iea_other_coal[c('iso','sector','fuel',paste0('X',1960:2010))] )
ceds_coal_fuel <- aggregate( ceds_coal_fuel[paste0('X',1960:2010)],
                            by = list(iso = ceds_coal_fuel$iso,
                                      fuel = ceds_coal_fuel$fuel) ,
                            FUN = sum )
ceds_coal <- aggregate( ceds_coal_fuel[paste0('X',1960:2010)],
                       by = list(iso = ceds_coal_fuel$iso) ,
                       FUN = sum )

ceds_coal_percentages <- ceds_coal_fuel[ ,c('iso','fuel')]
ceds_coal_percentages$start_year <- iea_start_year[match(ceds_coal_percentages$iso, iea_start_year$iso), 'start_year']

for( i in seq_along( ( ceds_coal_percentages$iso ))){
  year <- ceds_coal_percentages$start_year[i]
  total <- rowMeans(ceds_coal[match(ceds_coal_percentages$iso[i], ceds_coal$iso),paste0('X', c(year, year+1,year+2, year+3))])
  ceds_coal_percentages$percent[i] <- rowMeans(ceds_coal_fuel[i , paste0('X', c(year, year+1,year+2, year+3)) ]) / total
  if ( total == 0) ceds_coal_percentages$percent[i] <- 0
}

# Dissaggregate Coal into coal types

ceds_coal_dissaggregate <- ceds_coal_percentages
# loop over IEA data start years
dissaggregate_fuel_list<-list()
for( i in seq_along(start_years) ){
  # define countries with start year i
  countries <- iea_start_year[which( iea_start_year$start_year == start_years[i]),'iso']
  # total - same dataframe as percent breakdown
  disaggregated <- ceds_coal_dissaggregate[which(ceds_coal_dissaggregate$iso %in% countries),]
  disaggregated[ paste0('X',1750:(start_years[i]-1) ) ] <-  final_ceds_total_coal[ match(disaggregated$iso, final_ceds_total_coal$iso), paste0('X',1750:(start_years[i]-1) ) ]
                          
  disaggregated[paste0('X',1750:(start_years[i]-1) )] <- disaggregated[paste0('X',1750:(start_years[i]-1) )]*disaggregated$percent
  dissaggregate_fuel_list[[i]] <- disaggregated
}
coal_extended_dissagregate_by_fuel <- do.call('rbind.fill', dissaggregate_fuel_list)

# fill out dataframe with zeros for iso-fuel combinations
template <- data.frame( iso = rep( all_countries , each = 4),
                        fuel = rep( x=c('hard_coal','brown_coal','coal_coke','coal'), times = length(all_countries) ), stringsAsFactors = F )
template[ paste0('X', 1750:1970)] <- 0
coal_extended_dissagregate_by_fuel_full <- template
coal_extended_dissagregate_by_fuel_full[paste0('X', 1750:1970)] <- 
                    coal_extended_dissagregate_by_fuel[match(paste(template$iso,template$fuel),
                                                              paste(coal_extended_dissagregate_by_fuel$iso,coal_extended_dissagregate_by_fuel$fuel) ),
                                                                paste0('X', 1750:1970) ]

# seperate coal by fuel into other tranformation coal and combustion coal
other_transformation_coal <- coal_extended_dissagregate_by_fuel_full[which(coal_extended_dissagregate_by_fuel_full$fuel %in% 'coal'),c('iso','fuel', paste0('X',1750:1970))]
combustion_coal_by_fuel <- coal_extended_dissagregate_by_fuel_full[which(coal_extended_dissagregate_by_fuel_full$fuel %!in% 'coal'),]


# ---------------------------------------------------------------------------
# 6. Merge Bond Sector Splits and CEDS aggregate Sector Splits
#  Sector breakdowns from total fuel type to ext_sector
printLog('Calculating coal sector breakdowns')

# Calculate Bond Sector Percentages
bond <- merge( bond_historical, iso_map[,c('iso','Country')])
bond <- merge( bond, sector_map)
bond <- merge( bond, fuel_map)
bond <- bond[which( bond$Fuel %in% c( " Hard Coal      " , " Brown Coal     ", " Coking Coal    ")),]
bond <- bond[which( bond[,'Fuel (kt)'] > 0),]
bond$Year <- paste0('X',bond$Year)

bond <- aggregate(bond["Fuel (kt)"],
                  by = list(iso = bond$iso,
                            fuel = bond$fuel,
                            ext_sector = bond$Ext_Sector,
                            Year = bond$Year),
                  FUN = sum)
bond_sectors <- cast(bond, iso + fuel + ext_sector ~ Year, value = 'Fuel (kt)')
bond_sectors$X1850 <- bond_sectors$X1855
  
bond_sectors[ , bond_years] <- interpolate_NAs(bond_sectors[ , bond_years])
bond_sectors[is.na(bond_sectors)] <- 0

bond_sector <- aggregate( bond_sectors[bond_years],
                                 by = list(iso = bond_sectors$iso,
                                           ext_sector = bond_sectors$ext_sector,
                                           fuel = bond_sectors$fuel),
                                 FUN = sum)
bond_sector_total <- aggregate( bond_sectors[bond_years],
                          by = list(iso = bond_sectors$iso,
                                    fuel = bond_sectors$fuel),
                          FUN = sum)

bond_sector_percentages_total_template <- bond_sector[,c('iso','fuel','ext_sector')]
bond_sector_percentages_total_template[bond_years] <- 0
bond_sector_percentages_total_template <- replaceValueColMatch( bond_sector_percentages_total_template, bond_sector_total,
                                                                x.ColName = bond_years,
                                                                match.x = c('iso','fuel'),
                                                                addEntries = F)
bond_sector_percentages <- bond_sector
bond_sector_percentages[ bond_years ] <- bond_sector_percentages[ bond_years ]/bond_sector_percentages_total_template[ bond_years ]

bond_sector_percentages <- replace( bond_sector_percentages , bond_sector_percentages == 'NaN' , NA)
bond_sector_percentages <- replace( bond_sector_percentages , bond_sector_percentages == 'Inf' , NA)

bond_sector_percentages[ paste0('X',1850:2000)[ paste0('X',1850:2000)  %!in% names(bond_sector_percentages)]  ] <- NA
bond_sector_percentages <- bond_sector_percentages[ , c('iso','ext_sector','fuel',paste0('X',1850:2000))]
bond_sector_percentages[ , paste0('X',1850:2000) ] <- interpolate_NAs(bond_sector_percentages[ , paste0('X',1850:2000)])
bond_sector_percentages[paste0('X',1750:1849)] <- bond_sector_percentages[paste0('X',1850)]
bond_sector_percentages <- bond_sector_percentages[ , c('iso','ext_sector','fuel',paste0('X',1750:2000))]
bond_sector_percentages[ is.na(bond_sector_percentages) ] <- 0

# fill out dataframe with zero values
fuels = c('hard_coal','brown_coal','coal_coke')
ext_sectors = c('Power','Industry','RCO','Shipping','Transportation')
template <- data.frame( iso = rep(all_countries, each =  length(fuels)*length(ext_sectors)) ,
                        fuel =  rep(fuels, each=length(ext_sectors), times= length(all_countries))   ,
                        ext_sector = rep(x= ext_sectors, times = length(all_countries)*length(fuels)), stringsAsFactors=F )
bond_sector_percentages_full <- template
bond_sector_percentages_full[ paste0('X',1750:1970) ] <- bond_sector_percentages[match(paste(template$iso, template$fuel, template$e),
                                                                                    paste(bond_sector_percentages$iso, bond_sector_percentages$fuel, bond_sector_percentages$ext_sector))
                                                                                    , paste0('X',1750:1970)]
bond_sector_percentages_full[is.na(bond_sector_percentages_full)] <- 0

# for countries with no bond sectors, fill in arbitrary sector split (50% RCO, 50% industry)
#TODO get rid of for loops
bond_sector_percentages_corrected <- bond_sector_percentages_full
for (i in seq_along( all_countries)){
  for ( n in seq(fuels)){
    for (m in seq_along(1750:1970)){
    if( sum(bond_sector_percentages_corrected[which( bond_sector_percentages_corrected$iso == all_countries[i] &
                                           bond_sector_percentages_corrected$fuel == fuels[n] ) , paste0('X',(1750:1970)[m])]) == 0 ){
     bond_sector_percentages_corrected[which( bond_sector_percentages_corrected$iso == all_countries[i] &
                                           bond_sector_percentages_corrected$fuel == fuels[n] ) , paste0('X',(1750:1970)[m])] <- c(0,0.5,0.5,0,0)
   } }}}


# Calculate CEDS aggregate sector splits
ceds_aggregate_sectors <- activity_all[ which( activity_all$fuel %in% c('hard_coal','brown_coal','coal_coke')),]
ceds_aggregate_sectors$ext_sector <- ext_sector_map[match(ceds_aggregate_sectors$sector, ext_sector_map$sector),'ext_sector']
ceds_aggregate_sectors <- aggregate(ceds_aggregate_sectors[, c('X1960','X1965','X1971')],
                   by = list(iso = ceds_aggregate_sectors$iso,
                             fuel =  ceds_aggregate_sectors$fuel,
                             ext_sector = ceds_aggregate_sectors$ext_sector),
                   FUN = sum)
total <- aggregate(ceds_aggregate_sectors[, c('X1960','X1965','X1971')],
                                    by = list(iso = ceds_aggregate_sectors$iso,
                                              fuel =  ceds_aggregate_sectors$fuel),
                                    FUN = sum)

ceds_agg_percent_list <- list()
for ( i in seq_along((start_years))){
    countries <- iea_start_year[which( iea_start_year$start_year == start_years[i]),'iso']
    disaggregate <- ceds_aggregate_sectors[which( ceds_aggregate_sectors$iso %in% countries),]
    
    percentages <- disaggregate[,c('iso','fuel','ext_sector',paste0('X',start_years[i]))]
    percentages$total <- 0
    percentages$total <- total[match( paste(percentages$iso, percentages$fuel),
                                      paste(total$iso, total$fuel)), paste0('X',start_years[i]) ]
    

    percentages['percent'] <- percentages[paste0('X',start_years[i])]/percentages$total
    percentages[which( percentages$total == 0),'percent'] <- 0
    ceds_agg_percent_list[[i]]<-percentages  
}
ceds_agg_percent <- do.call(rbind.fill, ceds_agg_percent_list)

# Combine Bond and CEDS aggregate splits. Slowly transition from Bond to Ceds
#ceds_agg_percent  and bond_sector_percentages
combined_sector_percentages_list <- list()
for ( i in seq_along(start_years)) {
  year0 <- 1850
  yearEnd <- start_years[i]
  years <- year0:yearEnd
  countries <- iea_start_year[which( iea_start_year$start_year == start_years[i]),'iso' ]
  
  combined_percentages <- merge(bond_sector_percentages_corrected[,c('iso','ext_sector','fuel',paste0('X',1750:1850))],
                                ceds_agg_percent[,c('iso','ext_sector','fuel','percent')] )
  names(combined_percentages)[which(names(combined_percentages) == 'percent' )] <- paste0('X',yearEnd)
  
  combined_percentages <- combined_percentages[which( combined_percentages$iso %in% countries),]
  
  # percent ( year n ) = 
  for ( n in seq_along( years)){
    ceds_fraction <- (n-1)*(1/(length(years)-1))
    bond_fraction <- 1-ceds_fraction
    
    bond_split <- bond_sector_percentages[ match( paste(combined_percentages$iso, combined_percentages$fuel, combined_percentages$ext_sector)  , 
                                                  paste(bond_sector_percentages$iso, bond_sector_percentages$fuel, bond_sector_percentages$ext_sector) ), 
                                           c(paste0('X',years[n]) )]
    ceds_split <- ceds_agg_percent[ match( paste(combined_percentages$iso, combined_percentages$fuel, combined_percentages$ext_sector)  , 
                                           paste( ceds_agg_percent$iso,  ceds_agg_percent$fuel, ceds_agg_percent$ext_sector) )
                                    , c('percent') ]
    
    combined_percentages[,paste0('X',years[n])] <- bond_split*bond_fraction + ceds_split*ceds_fraction

  }
  combined_sector_percentages_list[[i]] <- combined_percentages
  
}
combined_sector_percentages <- do.call(rbind.fill,combined_sector_percentages_list)
combined_sector_percentages <- combined_sector_percentages[, c('iso','ext_sector','fuel',paste0('X',1750:1971))]
combined_sector_percentages[is.na(combined_sector_percentages)] <- 0

# Renormalize combined percentages 
combined_sector_percentages_corrected <- combined_sector_percentages
for( i in seq_along(all_countries)) {
  for ( n in seq_along( fuels )){
    for (m in seq_along( ext_sectors )){
     for (l in seq_along( 1750:1970 )) {
        breakdown <- combined_sector_percentages_corrected[which( combined_sector_percentages_corrected$iso == all_countries[i] &
                                                                    combined_sector_percentages_corrected$fuel == fuels[n]   ), 
                                                           paste0('X',(1750:1970)[l]) ]
        if ( all(is.na(breakdown)) ){
          combined_sector_percentages_corrected[which( combined_sector_percentages_corrected$iso == all_countries[i] &
                                                         combined_sector_percentages_corrected$fuel == fuels[n]   ), 
                                                paste0('X',(1750:1970)[l]) ] <- c(0.5,0,0.5,0,0)
        }else if( sum(breakdown) != 1 ){  
         combined_sector_percentages_corrected[which( combined_sector_percentages_corrected$iso == all_countries[i] &
                                                        combined_sector_percentages_corrected$fuel == fuels[n]   ), 
                                               paste0('X',(1750:1970)[l]) ] <- breakdown/sum(breakdown)
         } }}}}

# ---------------------------------------------------------------------------
# 6. CEDS disaggregate Sector Splits

# CEDS dissagregate sector splits in data start year (1960, 1971)
# Calculate Ceds sector fuel % in start_year as percent of ext sector-fuel
ceds_coal_sector <- activity_all[which( activity_all$fuel %in% c('hard_coal', 'brown_coal','coal_coke')),
                                 c('iso', 'sector','fuel', paste0('X',1960:2010))]
ceds_coal_sector$ext_sector <- bond_percent_1850[match( ceds_coal_sector$sector, bond_percent_1850$sector),'ext_sector']
ceds_coal_sector_total <- aggregate( ceds_coal_sector[paste0('X',1960:2010)],
                                     by = list(iso = ceds_coal_sector$iso,
                                               ext_sector = ceds_coal_sector$ext_sector,
                                               fuel = ceds_coal_sector$fuel) ,
                                     FUN = sum )

ceds_coal_extsector_percentages <- ceds_coal_sector[ ,c('iso','sector','fuel','ext_sector')]
ceds_coal_extsector_percentages$start_year <- iea_start_year[match(ceds_coal_extsector_percentages$iso, iea_start_year$iso), 'start_year']

for( i in seq_along( ( ceds_coal_extsector_percentages[,1] ))) {
  year <- ceds_coal_extsector_percentages$start_year[i]
  total <- rowMeans(ceds_coal_sector_total[match(paste(ceds_coal_extsector_percentages$iso[i], ceds_coal_extsector_percentages$ext_sector[i],ceds_coal_extsector_percentages$fuel[i]),
                                                 paste(ceds_coal_sector_total$iso, ceds_coal_sector_total$ext_sector,ceds_coal_sector_total$fuel)),
                                           paste0('X', c(year, year+1,year+2, year+3))])
  ceds_coal_extsector_percentages[i,'total'] <- total
  ceds_coal_extsector_percentages[i,'disaggregate'] <- rowMeans(ceds_coal_sector[i , paste0('X', c(year, year+1,year+2, year+3)) ])
  if ( total == 0 | is.na(total)) {ceds_coal_extsector_percentages$percent[i] <- 0} else{
  ceds_coal_extsector_percentages[i,'percent'] <- ceds_coal_extsector_percentages[i,'disaggregate'] / ceds_coal_extsector_percentages[i,'total']
  }

}

# correct ceds_coal_extsector_percentages - correct zero sums
ceds_coal_extsector_percentages_corrected <- ceds_coal_extsector_percentages
for( i in seq_along(all_countries)) {
  for ( n in seq_along( fuels )){
    for ( m in seq_along( ext_sectors)){
      selected_percents <- ceds_coal_extsector_percentages_corrected[which( 
                              ceds_coal_extsector_percentages_corrected$iso == all_countries[i] &
                                ceds_coal_extsector_percentages_corrected$fuel == fuels[n] &
                                ceds_coal_extsector_percentages_corrected$ext_sector == ext_sectors[m]), 'percent']
      if( sum ( selected_percents ) == 0 ){
        if( ext_sectors[m] == 'Power' ){  
          ceds_coal_extsector_percentages_corrected[which( 
            ceds_coal_extsector_percentages_corrected$iso == all_countries[i] &
              ceds_coal_extsector_percentages_corrected$fuel == fuels[n] &
              ceds_coal_extsector_percentages_corrected$ext_sector == ext_sectors[m]), 'percent'] <- c(0,0,1)  }
        if( ext_sectors[m] == 'Industry' ){
          ceds_coal_extsector_percentages_corrected[which( 
            ceds_coal_extsector_percentages_corrected$iso == all_countries[i] &
              ceds_coal_extsector_percentages_corrected$fuel == fuels[n] &
              ceds_coal_extsector_percentages_corrected$ext_sector == ext_sectors[m]), 'percent'] <- c(0,0,0,0,0,0,0,0,0,1,0,0,0) 
        }
        if( ext_sectors[m] == 'RCO' ){
          ceds_coal_extsector_percentages_corrected[which( 
            ceds_coal_extsector_percentages_corrected$iso == all_countries[i] &
              ceds_coal_extsector_percentages_corrected$fuel == fuels[n] &
              ceds_coal_extsector_percentages_corrected$ext_sector == ext_sectors[m]), 'percent'] <- c(0,0,1)
        }
        if( ext_sectors[m] == 'Shipping' ){
          ceds_coal_extsector_percentages_corrected[which( 
            ceds_coal_extsector_percentages_corrected$iso == all_countries[i] &
              ceds_coal_extsector_percentages_corrected$fuel == fuels[n] &
              ceds_coal_extsector_percentages_corrected$ext_sector == ext_sectors[m]), 'percent'] <- c(1)
        }
        if( ext_sectors[m] == 'Transportation' ){
          ceds_coal_extsector_percentages_corrected[which( 
            ceds_coal_extsector_percentages_corrected$iso == all_countries[i] &
              ceds_coal_extsector_percentages_corrected$fuel == fuels[n] &
              ceds_coal_extsector_percentages_corrected$ext_sector == ext_sectors[m]), 'percent'] <- c(0,0,0,1,0,0)
        }
      }
      
    }}}

#combine bond_sector_percentages and ceds breakdown

# CEDS sector splits,aggregate sectors to CEDS sectors in 1960. Defined Splits in 1850, linearly interpolate
ceds_breakdown <- ceds_coal_extsector_percentages_corrected
ceds_breakdown[which( ceds_breakdown$start_year == '1960'), 'X1960'] <- ceds_breakdown[which( ceds_breakdown$start_year == '1960'), 'percent']
ceds_breakdown[which( ceds_breakdown$start_year == '1971'), 'X1971'] <- ceds_breakdown[which( ceds_breakdown$start_year == '1971'), 'percent']

ceds_breakdown <- merge( ceds_breakdown, bond_percent_1850, all.x = T, all.y=F)
ceds_breakdown <- ceds_breakdown[ , c('iso','sector','ext_sector','fuel','X1849','X1960','X1971' )]


ceds_breakdown[ paste0('X',1849:1971)[ paste0('X',1849:1971)  %!in% names(ceds_breakdown)]  ] <- NA
ceds_breakdown <- ceds_breakdown[ , c('iso','sector','ext_sector','fuel',paste0('X',1849:1971))]
ceds_breakdown[ , paste0('X',1849:1971)] <- interpolate_NAs(ceds_breakdown[ ,paste0('X',1849:1971)])
ceds_breakdown[paste0('X',1750:1848)] <- ceds_breakdown[paste0('X',1849)]
ceds_breakdown <- ceds_breakdown[ , c('iso','sector','ext_sector','fuel',paste0('X',1750:1971))]

ceds_breakdown <- replace( ceds_breakdown, is.na(ceds_breakdown) , 0 )

# percentages as a total of iso-fuel
# disaggregate percent, ceds_breakdown
# aggregate category to dissagregate <- combined sector percentages 
final_percentages<-ceds_breakdown
combined_template <- ceds_breakdown[c('iso','ext_sector','sector','fuel')]
combined_template[paste0('X',1750:1971)] <- NA
combined_template[paste0('X',1750:1971)] <-combined_sector_percentages[
                                                    match(paste(combined_template$iso, combined_template$ext_sector, combined_template$fuel),
                                                          paste(combined_sector_percentages$iso, combined_sector_percentages$ext_sector, combined_sector_percentages$fuel)),
                                                    paste0('X',1750:1971)] 
combined_template <- replace( combined_template, is.na(combined_template), 0)

final_percentages[paste0('X',1750:1971)] <- ceds_breakdown[paste0('X',1750:1971)]* combined_template[paste0('X',1750:1971)]

# Renormalize combined percentages 
final_percentages_corrected <- final_percentages
for( i in seq_along(all_countries)) {
  for ( n in seq_along( fuels )){
      for (l in seq_along( 1750:1970 )) {
        breakdown <- final_percentages_corrected[which( final_percentages_corrected$iso == all_countries[i] &
                                                                    final_percentages_corrected$fuel == fuels[n]   ), 
                                                           paste0('X',(1750:1970)[l]) ]
        if ( sum(breakdown) == 0 ){
          final_percentages_corrected[which( final_percentages_corrected$iso == all_countries[i] &
                                                         final_percentages_corrected$fuel == fuels[n]   ), 
                                                paste0('X',(1750:1970)[l]) ] <- c(rep(times=12,x=0),0.5,rep(times=11,x=0),0.5,0,0)
        }else if( sum(breakdown) != 1 ){  
          final_percentages_corrected[which( final_percentages_corrected$iso == all_countries[i] &
                                                         final_percentages_corrected$fuel == fuels[n]   ), 
                                                paste0('X',(1750:1970)[l]) ] <- breakdown/sum(breakdown)
        } }}}


# ---------------------------------------------------------------------------
# 7. Dissaggregate total CEDS coal to CEDS sectors
# Dissagregate combustion_coal_by_fuel using final_percentages
printLog('Disaggregating coal to ceds sectors')
# Loop over IEA data start years
  dissaggregate_sector_list<-list()
  for( i in seq_along(start_years) ){
    # define countries with start year i
    countries <- iea_start_year[which( iea_start_year$start_year == start_years[i]),'iso']
    percentages <- final_percentages_corrected[which(final_percentages_corrected$iso %in% countries),]
    
    # total fuel template - same order as percentages
    totals <- percentages[c('iso','sector','ext_sector','fuel')]
    totals[ paste0('X',1750:(start_years[i]-1) ) ] <- 0
    totals <- replaceValueColMatch(totals, combustion_coal_by_fuel,
                                          x.ColName = paste0('X',1750:(start_years[i]-1) ),
                                          match.x = c('iso','fuel'),
                                          addEntries = F)
    aggregate <- totals
   # multiply total by percentages breakdown
    aggregate[paste0('X',1750:(start_years[i]-1) )] <- aggregate[paste0('X',1750:(start_years[i]-1) )]*
      percentages[paste0('X',1750:(start_years[i]-1) )]
   
  # save data to list   
    dissaggregate_sector_list[[i]] <- aggregate
  }
  
  coal_dissaggregate_final <- do.call('rbind.fill', dissaggregate_sector_list)

  
  # fill out data with zero country and sectors 
  coal_activity <- activity_all[which( activity_all$fuel %in% c('hard_coal', 'brown_coal','coal_coke')),]
  final_coal <- coal_activity
  final_coal[paste0('X',1750:1970)] <- NA
  # loop over start_years
  
  for ( i in seq_along ( start_years ) ){
    countries <- iea_start_year[which( iea_start_year$start_year == start_years[i]),'iso']
    
    coal_values_start_date <- coal_dissaggregate_final[ which(coal_dissaggregate_final$iso %in% countries), 
                                                        c('iso','sector','fuel', paste0('X',1750:(start_years[i]-1)) )]
    
    final_coal <- replaceValueColMatch(final_coal , coal_values_start_date,
                                     x.ColName = paste0('X',1750:(start_years[i]-1)),
                                     match.x = c('iso','sector','fuel'),
                                     addEntries = F)
    
  
    # coal_dissaggregate_final[match(paste( final_coal$iso, final_coal$fuel, final_coal$sector)  , 
    #                                              paste(coal_dissaggregate_final$iso, coal_dissaggregate_final$fuel, coal_dissaggregate_final$sector )), 
    #                                              paste0('X',1750:1970)]
  
  }  

  # do not add intl shipping ( use other data )  
  final_coal <- final_coal[which(final_coal$sector%!in% '1A3di_International-shipping'),]
  
  final_coal[is.na(final_coal)] <- 0
  
# ---------------------------------------------------------------------------
# 7. Add to database

#Split activity data into data to replace, and not replace
  rows <- which(!is.na(activity$X1750) | activity$fuel %!in% c('hard_coal','brown_coal','coal_coke') )
  activity.done <- activity[  rows , ]
  activity.replace <- activity[ which( 1:nrow(activity) %!in% rows ) , ]

# loop over IEA data start years ( 1960, 1965, 1971)
  for ( i in seq_along ( start_years ) ){
    countries <- iea_start_year[which( iea_start_year$start_year == start_years[i]),'iso']
    coal <- final_coal[which( final_coal$iso %in% countries),]
    
    new_activity <- activity.replace[ which( activity.replace$iso %in% countries),]
    activity.replace <- activity.replace[ which( activity.replace$iso %!in% countries),]
    
    new_activity [ paste0('X',1750:(start_years[i]-1)) ] <- coal[ match( paste(new_activity$iso, new_activity$fuel, new_activity$sector ),
                                                                 paste(coal$iso, coal$fuel, coal$sector ) )  , paste0('X',1750:(start_years[i]-1)) ]
        
    new_activity [ is.na(new_activity )] <- 0
  
    activity.done <- rbind( activity.done, new_activity)
  }

# combine final activity data
  activity <- rbind( activity.replace, activity.done )

# ---------------------------------------------------------------------------
# Diagnostic
# Sum Coal and add other trasformation
 summed_coal_total <- rbind.fill( final_coal , other_transformation_coal)
 summed_coal_total <- aggregate( summed_coal_total[ paste0('X',1750:1970)],
                                  by = list(fuel = summed_coal_total$fuel),
                                  FUN = sum, na.rm=T)
 all_other_tranformation_coal <-  rbind.fill(other_transformation_coal,iea_other_coal)
 all_other_tranformation_coal <- aggregate(all_other_tranformation_coal[paste0('X',1750:2013)],
                                           by = list(all_other_tranformation_coal$iso),
                                           sum)
   
# ---------------------------------------------------------------------------
# 7. Write to database

  writeData(final_ceds_total_coal, 'DIAG_OUT', 'H.Extended_total_coal')
  writeData( all_other_tranformation_coal , 'DIAG_OUT', 'H.Extended_other_tranformation_coal')
  writeData(combustion_coal_by_fuel, 'DIAG_OUT', 'H.Extended_coal_by_fuel')
  writeData(final_coal, 'DIAG_OUT', 'H.Extended_coal_by_sector_fuel')
  writeData(summed_coal_total, 'DIAG_OUT', 'H.Extended_coal_dissagregated_by_sector_fuel_aggregated') 
  
  if( !(nrow(activity_all) == nrow(activity) & ncol(activity_all) == ncol(activity) ) ){
    stop( "New and old activity do not match") } else{
      writeData( activity, "MED_OUT" , paste0('H.',em,'_total_activity_extended_db')) }
  
  writeData( aggregate( activity[,paste0('X',1750:2014)],
                        by = list(activity$fuel), sum, na.rm=T)
    ,"DIAG_OUT", 'H.extended_aggregate_CEDS_COAL')
  
  logStop()
