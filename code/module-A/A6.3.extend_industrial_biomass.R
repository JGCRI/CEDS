# Program Name:  A6.3.extend_industrial_biomass.R
# Authors:       Rachel Hoesly, Caleb Braun
# Last Modified: August 19, 2019
# Purpose:       Extend Industrial Biomass with Bond data.
#                Transition from CEDS to Bond values
#
# Input Files:   A.default_comb_activity_with_other, A.UN_pop_master, IEA_iso_start_data
#                CD.Bond_country_industrial_biomass
# Output Files:  A.industrial_biomass_extended
# Method Summary: 1. Disaggregate Bond data in CEDS Countries
#                 2. Blend Bond and CEDS biomass totals
#                 3. Calculate CEDS sector breakdowns
#                 4. Apply sector breakdowns to total Biomass
#                 5. Extend other (no bond data) with population
# ------------------------------------------------------------------------------


# 0. Read in global settings and headers ----------------------------------
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Read in additional header files and start the script log
headers <- c( "data_functions.R","process_db_functions.R")
log_msg <- "Extending biomass activity_data before 1960 with CDIAC-Bond data"
script_name <- "A6.3.extend_industrial_biomass.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )


# 1. Load files -----------------------------------------------------------

iea_start_year <- readData( 'ENERGY_IN', 'IEA_iso_start_data', meta = F )
bond_biomass   <- readData( 'EXT_IN', 'CD.Bond_country_industrial_biomass' )
activity <- readData( 'MED_OUT', "A.default_comb_activity_with_other" , meta = F)
un_pop         <- readData( 'MED_OUT', 'A.UN_pop_master', meta = F )


# 2. Define Functions -----------------------------------------------------

disaggregate_countries <- function(original_data, aggregate_country,
                                   disaggregate_countries, aggregate_end_year,
                                   data_start_year = bond_start,
                                   id_cols=c('iso','fuel'), population) {

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
    # multiplier - aggregate_country CDIAC data
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


# 3. Select data to extend based on extension drivers ---------------------

# UN population
population <- un_pop %>%
    dplyr::select( iso, year, pop ) %>%
    dplyr::filter( year %in% historical_pre_extension_year:end_year ) %>%
    tidyr::spread( year, pop ) %>%
    dplyr::rename_all( make.names )

activity[paste0('X',1750:1959)] <- NA

start_years <- c(1960,1971)

iea_start_year$start_year <- 1971
iea_start_year[iea_start_year$iso %in% c('usa','aus','swe','can','prt','pol'), 'start_year'] <- 1960


# 4. CEDS Data processing ---------------------------------------------------

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

ceds_industrial_biomass <- activity[ activity$fuel == 'biomass' &
                                     activity$sector %in% industry_sectors, ]

ceds_industrial_biomass_agg <- ceds_industrial_biomass %>%
    dplyr::group_by( iso, fuel ) %>%
    dplyr::summarise_if( is.numeric, sum )

# 5. Disaggregate Bond Data - FSU and USSR ----------------------------------

  if (!all_equal(interpolate_NAs(bond_biomass), interpolate_NAs2(  bond_biomass))) stop()
bond_biomass <- interpolate_NAs( bond_biomass )

bond_industrial_biomass_fsu <- disaggregate_countries(
    original_data          = bond_biomass,
    aggregate_country      = 'ussr',
    disaggregate_countries = c('aze','arm','blr','est','geo','kaz','kgz','lva',
                               'ltu','mda','tjk','tkm','ukr','uzb', 'rus'),
    aggregate_end_year     = 1991,
    data_start_year        = bond_start,
    id_cols                = c('iso','fuel'),
    population             = population
)
bond_industrial_biomass_yug <- disaggregate_countries(
    original_data          = bond_industrial_biomass_fsu,
    aggregate_country      = 'yug',
    disaggregate_countries = c('bih','hrv','mkd','svn', 'srb','mne'),
    aggregate_end_year     = 1991,
    data_start_year        = bond_start,
    id_cols                = c('iso','fuel'),
    population             = population
)


# 6. Total Industry Biomass -------------------------------------------------
# Merge bond and ceds industrial biomass

# get Bond and CEDS data into same format, add zero lines
biomass_template <- dplyr::union( ceds_industrial_biomass_agg[ , c('iso', 'fuel')],
                                  bond_industrial_biomass_yug[ , c('iso', 'fuel')] )

ceds_biomass_full<- biomass_template
ceds_biomass_full[paste0('X', 1960:end_year)] <- ceds_industrial_biomass_agg[match(ceds_biomass_full$iso, ceds_industrial_biomass_agg$iso), paste0('X', 1960:end_year)]
bond_biomass_full<- biomass_template
bond_biomass_full[paste0('X', bond_start:2000)] <- bond_industrial_biomass_yug[match(bond_biomass_full$iso, bond_industrial_biomass_yug$iso), paste0('X', bond_start:2000)]

bond_biomass_full[is.na(bond_biomass_full)] <- 0
ceds_biomass_full[is.na(ceds_biomass_full)] <- 0

#loop over different IEA data start years
blended_biomass <- bond_biomass_full[,c('iso','fuel')]
blended_biomass[paste0('X',bond_start:end_year)] <- NA

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
  blended_biomass [ which( blended_biomass$iso %in% countries), paste0('X',bond_start:(merge_years[1]-1)) ]  <-
            bond_biomass_full[ which( bond_biomass_full$iso %in% countries) , paste0('X',bond_start:(merge_years[1]-1)) ]
  # blending
  blended_biomass[which( blended_biomass$iso %in% countries), paste0('X',merge_years)]  <-
            matrix( data = rep(x=ceds_fractions, times = length(countries)), nrow = length(countries), byrow=T) * ceds_biomass_full[which( ceds_biomass_full$iso %in% countries),paste0('X',merge_years)] +
            matrix( data = rep(x=bond_fractions, times = length(countries)), nrow = length(countries), byrow=T) * bond_biomass_full[which( bond_biomass_full$iso %in% countries),paste0('X',merge_years)]
  # post blending ceds
  blended_biomass[which( blended_biomass$iso %in% countries), paste0('X',(merge_years[length(merge_years)]+1):end_year) ]  <-
          ceds_biomass_full[ which( ceds_biomass_full$iso %in% countries) , paste0('X',(merge_years[length(merge_years)]+1):end_year) ]
}

blended_biomass <- blended_biomass[ , c('iso','fuel',paste0('X',bond_start:end_year))]


# 7. Calculate sector breakdown ---------------------------------------------

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

breakdown_years <- paste0( 'X', bond_start:1971 )
ceds_sector_breakdown <- do.call(rbind.fill, ceds_sector_breakdown_list)

# Initialize breakdowns for start year (only 1A2g_Ind-Comb-other should be 1),
# then fill in all intermediate years and interpolate
ceds_sector_breakdown <- ceds_sector_breakdown %>%
    dplyr::mutate( X1850 = as.numeric( sector == '1A2g_Ind-Comb-other' ) ) %>%
    dplyr::mutate_at( setdiff( breakdown_years, names( . ) ), funs( +NA_real_ ) ) %>%
    dplyr::select( iso, sector, fuel, breakdown_years ) %>%
    interpolate_NAs()


# 8. Disaggregate to CEDS sectors -------------------------------------------

for ( i in seq_along( start_years ) ) {
    countries <- iea_start_year[which( iea_start_year$start_year == start_years[i]),'iso']

    percentages <- ceds_sector_breakdown[which(ceds_sector_breakdown$iso %in% countries),]

    # total fuel template - same order as percentages
    aggregate <- percentages[c('iso','sector','fuel')]
    aggregate[ paste0('X',bond_start:(start_years[i]-1) ) ] <- 0
    aggregate <- replaceValueColMatch(aggregate, blended_biomass,
                                      x.ColName = paste0('X',bond_start:(start_years[i]-1) ),
                                      match.x = c('iso','fuel'),
                                      addEntries = F)
    # multiply total by percentages breakdown
    aggregate[paste0('X',bond_start:(start_years[i]-1) )] <- aggregate[paste0('X',bond_start:(start_years[i]-1) )]*
      percentages[paste0('X',bond_start:(start_years[i]-1) )]

    # save data to list
    if (i == 1) dissagregate_biomass_1960 <- aggregate
    if (i == 2) dissagregate_biomass_1971 <- aggregate
}


# 9. Extend 1750 to 1750 with Population ------------------------------------

# set up extension data (population)
  extension_data_1960 <- dissagregate_biomass_1960[,c('iso','sector','fuel')]
  extension_data_1960[ paste0('X',1750:1900) ] <- population[match(extension_data_1960$iso, population$iso), paste0('X',1750:1900)]

  extension_data_1971 <- dissagregate_biomass_1971[,c('iso','sector','fuel')]
  extension_data_1971[ paste0('X',1750:1900) ] <- population[match(extension_data_1971$iso, population$iso), paste0('X',1750:1900)]

# add 1750 - 1849 columns
  pre_bond_years <- paste0('X', historical_pre_extension_year:(bond_start - 1))
  dissagregate_biomass_1960[pre_bond_years] <- NA
  dissagregate_biomass_1971[pre_bond_years] <- NA

# extend
  final_extended_1960 <- extend_data_on_trend(driver_trend = extension_data_1960, input_data = dissagregate_biomass_1960 , start = 1750, end = 1849)
  final_extended_1971 <- extend_data_on_trend(driver_trend = extension_data_1971, input_data = dissagregate_biomass_1971 , start = 1750, end = 1849)

  # TODO: change extend_data_on_trend function - it currently takes average of previous years.
  # If start year is zero then data should be extended at zero - or option
  final_extended_1960[ final_extended_1960$X1850 == 0, pre_bond_years ] <- 0
  final_extended_1971[ final_extended_1971$X1850 == 0, pre_bond_years ] <- 0

  final_extended_biomass <- rbind.fill(final_extended_1960, final_extended_1971)
  final_extended_biomass <- final_extended_biomass %>%
      dplyr::select(iso, sector, fuel, paste0('X', 1750:1970)) %>%
      dplyr::arrange(iso, sector, fuel)


# 10. Check final values ----------------------------------------------------

  if( anyNA(final_extended_biomass[paste0('X',1750:1959)]) )
      stop('NAs in final industrial biomass data. Please Check.')


# 11. Write to database -----------------------------------------------------

writeData( final_extended_biomass, "MED_OUT", 'A.industrial_biomass_extended' )

logStop()
