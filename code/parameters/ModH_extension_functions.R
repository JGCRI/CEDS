# ----------------------------------------------------------------------------------
# CEDS R header file:  ModH_extension_functions.R
# Authors: Rachel Hoesly
# Last Updated: April 1, 2016

# This file should be sourced by R scripts in the H - EF  module
# Functions contained:
#
# Notes:

# -----------------------------------------------------------------------------
# select_EF_drivers
# Brief:     select the correct lines from the EF driver file to extend EFs
# Details:
# Dependencies:
# Author(s):
# Params:
#
# Return:
# Input Files:  trend_name = the string for method of extension in the EF driver file, example - 'CDIAC' or 'default'
# Output Files: dataframe with correct lines from the EF driver file

# -----------------------------------------------------------------------------
 select_EF_drivers <- function(trend_name){

   # Expand fuels - all-comb
   expand <- extension_drivers_EF[which(extension_drivers_EF$fuel == 'all-comb' ) ,]
   extension_drivers_EF <- extension_drivers_EF[which(extension_drivers_EF$fuel != 'all-comb' ) ,]
   comb_fuels <- c('biomass', 'hard_coal','brown_coal','coal_coke','natural_gas','heavy_oil','diesel_oil','light_oil')
   for (i in seq_along(comb_fuels)){
     expand$fuel <- rep(comb_fuels[i], times= nrow(expand) )
     extension_drivers_EF <- rbind( extension_drivers_EF, expand )
   }
   extension_drivers_EF <- extension_drivers_EF[ which( extension_drivers_EF$em %in% c(em , 'all' )), ]

   # delete, all row for a sector-fuel if there is a sector-fuel entry for the specific emission species
   driver_em <- extension_drivers_EF[which( extension_drivers_EF$em == em), ]
   if( nrow(driver_em) > 0 ){
     em_instruction <- unique( paste( driver_em$sector,driver_em$fuel ,sep = '-'))
     extension_drivers_EF <- extension_drivers_EF[ which(
       paste( extension_drivers_EF$sector, extension_drivers_EF$fuel , extension_drivers_EF$em, sep = '-') %!in%
         paste( em_instruction ,'all' ,sep = '-') ), ]
 }

   # select em
   extension_drivers_EF$em <- em

   # select method
   extension_drivers_EF <- extension_drivers_EF[ which( extension_drivers_EF$method == trend_name ) ,]
   extension_drivers_EF <-  extension_drivers_EF[order( - extension_drivers_EF$start_year),]
   return( extension_drivers_EF)
 }

 #########################################################################
 # The following funcitons are for use in Fossil Fuel activity extension

 # -----------------------------------------------------------------------------
 # H.process_bond_data
 # Brief:     process historical bond data for use in Module H fossil fuel extension
 # Details:   imports, process, filters, and aggregates, historical bond data for use
 #            in Module H fossil fuel extension
 # Dependencies: data_functions.R
 # Author(s): Rachel Hoesly
 # Params:    a.ceds_ext_fuels: ceds fuels that are being extended in the script
 # Return:    list of data frames, bond data aggregated at different levels
 # Input Files:
        # "EM_INV" ,"160227_SPEW_BCOCemission"
        # "MAPPINGS", domain_extension = "Bond/" , "Bond_country_map"
        # "MAPPINGS", domain_extension = "Bond/" , "Bond_fuel_map"
        # "MAPPINGS", domain_extension = "Bond/" , "Bond_sector_ext_map", ".xlsx", sheet_selection = 'Bond_to_ext'
 # Output Files: none

 H.process_bond_data <- function( a.ceds_extension_fuels = ceds_extension_fuels,
                                  a.extension_fuel_category = extension_fuel_category,
                                  a.bond_start = bond_start,
                                  a.bond_end = bond_end,
                                  a.X_bond_years = X_bond_years){
   # a.ceds_extension_fuels = ceds_extension_fuels
   # a.extension_fuel_category = extension_fuel_category
   # a.bond_start = bond_start
   # a.bond_end = bond_end
   # a.X_bond_years = X_bond_years
   #
   # import data and mapping files
   bond_ctypes = c(rep("text", 5), rep("numeric", 4), "skip") # last column contains value in last cell: set option to skip
   bond_import <- readData( "EM_INV", domain_extension = "Bond-BCOC/" ,"160227_SPEW_BCOCemission", ".xlsx", col_types = bond_ctypes, meta = T )

   fuel_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_fuel_map", meta = F )
   bond_sector_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_sector_ext_map", ".xlsx", sheet_selection = 'Bond_to_ext',meta = F )

   ext_sectors <- unique(bond_sector_map$Ext_Sector)[!is.na(unique(bond_sector_map$Ext_Sector))]
   ext_sectors <- ext_sectors[order(ext_sectors)]

   if(a.extension_fuel_category == 'coal' ){ iso_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_country_map", meta = F )
   } else{  warning('Bond mapping double counts energy use. Totals are no longer real values.')
            iso_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_historical_country_map", meta = F )}

   # map to ceds iso, sector, fuels
   if(a.extension_fuel_category == 'coal' ){
            bond <- merge( bond_import, iso_map[,c('iso','Country')], all.x=T, all.y=F)} else{

            bond <- merge( bond_import, iso_map[,c('iso','Country')], all.x=T, all.y=T)
            }


   bond <- merge( bond, fuel_map, all.x=T, all.y=F)
   bond <- merge( bond, bond_sector_map, all.x=T, all.y=F)

   # filter data and aggregate/cast
   bond <- bond[which(bond$fuel %in% a.ceds_extension_fuels) , ]
   bond <- bond[which( bond[,'Fuel (kt)'] > 0),]
   bond$Year <- paste0('X',bond$Year)

   bond_long <- aggregate(bond["Fuel (kt)"],
                                by = list(iso = bond$iso,
                                          ext_sector = bond$Ext_Sector,
                                          fuel = bond$fuel,
                                          Year = bond$Year),
                                FUN = sum)


   # convert natural gas from TJ to kt. (It's labeled as kt in the original data, but actually in TJ)
   if ( a.extension_fuel_category == 'natural_gas') {
   bond_long['Fuel (kt)'] <- bond_long['Fuel (kt)'] / conversionFactor_naturalgas_TJ_per_kt
   # Manually correct bond values
   bond_long[which( bond_long$iso == 'usa' & bond_long$Year %in% c(paste0('X',a.bond_start:1955))),'Fuel (kt)'] <- NA
   }
   # Manually correct bond values
   if( a.extension_fuel_category == 'petroleum'){
    bond_long[which( bond_long$Year %in% c(paste0('X',a.bond_start:1915))),'Fuel (kt)'] <- NA
   }


   # cast
   bond_wide <- cast(bond_long, iso + ext_sector + fuel ~ Year, value = 'Fuel (kt)')
   bond_wide[ names(bond_wide)[ grep('X',names(bond_wide)) ] ] <- interpolate_NAs(bond_wide[names(bond_wide)[ grep('X',names(bond_wide)) ]])

   # Intepolate years (keep 5 year intervals, fill in missing data)
   X_all_bond_years <- paste0('X',a.bond_start:a.bond_end)
   bond_wide [ X_all_bond_years[ X_all_bond_years %!in% names(bond_wide)[names(bond_wide) %!in% c('iso','ext_sector','fuel')] ]  ] <- NA
   bond_wide <- bond_wide [ , c('iso', 'ext_sector', 'fuel' ,X_all_bond_years) ]
   bond_wide[ X_all_bond_years] <- interpolate_NAs(bond_wide[ X_all_bond_years])
   # replace NAs with 0
   bond_wide[ is.na( bond_wide )] <- 0

   bond_iso <- aggregate(bond_wide[X_all_bond_years],
                         by = list(iso = bond_wide$iso),
                         FUN = sum)
   bond_iso_fuel <- aggregate(bond_wide[X_all_bond_years],
                         by = list(iso = bond_wide$iso,
                                   fuel = bond_wide$fuel),
                         FUN = sum)
   bond_iso_sector_fuel <- aggregate(bond_wide[X_all_bond_years],
                         by = list(iso = bond_wide$iso,
                                   fuel = bond_wide$fuel,
                                   ext_sector = bond_wide$ext_sector),
                         FUN = sum)

   # write summary files
   writeData(bond_iso, 'DIAG_OUT', paste0('H.Bond_inventory_iso_',a.extension_fuel_category))
   writeData(bond_iso_fuel, 'DIAG_OUT', paste0('H.Bond_inventory_iso_fuel_',a.extension_fuel_category))
   writeData(bond_iso_sector_fuel, 'DIAG_OUT', paste0('H.Bond_inventory_iso_sector_fuel',a.extension_fuel_category))

   # output
   out <- list(bond_iso,bond_iso_fuel, bond_iso_sector_fuel, ext_sectors)
   names(out) <- c('bond_iso','bond_iso_fuel', 'bond_iso_sector_fuel','ext_sectors')
   return(out)
 }

 # -----------------------------------------------------------------------------
 # H.process_ceds_data
 # Brief:     process IEA ceds data for use in Module H fossil fuel extension
 # Details:   imports, process, filters, and aggregates, ceds for use
 #            in Module H fossil fuel extension
 # Dependencies: data_functions.R
 # Author(s): Rachel Hoesly
 # Params:    a.ceds_ext_fuels: ceds fuels that are being extended in the script
           # a.ceds_extension_fuels = ceds_extension_fuels,
           # a.bond_end = bond_end,
           # a.iea_other = iea_other,
           # a.all_countries = all_countries
 # Return:    list of data frames:
 #            ceds_total_iso: total fuel (including IEA other) by iso,
 #            iea_start_year: list of start year of IEA data by country, 1960 or 1971
 # Input Files:
 # Output Files: none

 H.process_ceds_data <-  function(a.activity = activity,
                                  a.ceds_extension_fuels = ceds_extension_fuels,
                                  a.bond_end = bond_end,
                                  a.iea_other = iea_other,
                                  a.all_countries = all_countries,
                                  a.extension_end_year = extension_end_year) {

   # a.activity = activity
   # a.ceds_extension_fuels = ceds_extension_fuels
   # a.bond_end = bond_end
   # a.iea_other = iea_other
   # a.all_countries = all_countries
   # a.extension_end_year = extension_end_year

   ceds_sector_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_sector_ext_map", ".xlsx", sheet_selection = 'CEDS_to_ext',meta = F )

   # CEDS
   ceds_original_all <- a.activity[which( a.activity$fuel %in% c(a.ceds_extension_fuels)),
                                     c('iso','sector','fuel', paste0('X',1960:a.bond_end))]

   # Add IEA other to ceds
   ceds_total <- rbind.fill( ceds_original_all, a.iea_other[c('iso','sector','fuel', paste0('X',1960:a.bond_end))] )
   ceds_total$ext_sector <- ceds_sector_map[match(ceds_total$sector,ceds_sector_map$sector),'ext_sector']

   # Remove international-shipping bunkers
   ceds_total <- ceds_total[which(ceds_total$sector %!in% '1A3di_International-shipping'),]

   # Aggregate
   ceds_total_iso  <- aggregate( ceds_total [paste0('X',1960:a.bond_end)],
                                 by = list( iso = ceds_total$iso) ,
                                 FUN = sum)
   ceds_total_iso_fuel  <- aggregate( ceds_total [paste0('X',1960:a.bond_end)],
                                 by = list( iso = ceds_total$iso,
                                            fuel = ceds_total$fuel) ,
                                 FUN = sum)
   ceds_total_iso_fuel_sector  <- aggregate( ceds_total [paste0('X',1960:a.bond_end)],
                                      by = list( iso = ceds_total$iso,
                                                 fuel = ceds_total$fuel,
                                                 sector= ceds_total$sector) ,
                                      FUN = sum)
   ceds_total_iso_fuel_ext_sector  <- aggregate( ceds_total [paste0('X',1960:a.bond_end)],
                                             by = list( iso = ceds_total$iso,
                                                        fuel = ceds_total$fuel,
                                                        ext_sector= ceds_total$ext_sector) ,
                                             FUN = sum)

   # Make NA values where IEA data exists
   ceds_total_iso[ which( ceds_total_iso$iso %in% iea_start_year[which( iea_start_year$start_year == 1971) ,'iso']) ,
                   paste0("X",1960:a.extension_end_year) ] <- NA

   # output
   out <- list(ceds_total_iso,ceds_total_iso_fuel,ceds_total_iso_fuel_sector,ceds_total_iso_fuel_ext_sector)
   names(out) <- c('ceds_total_iso','ceds_total_iso_fuel','ceds_total_iso_fuel_sector','ceds_total_iso_fuel_ext_sector')

   return(out)

 }

 # -----------------------------------------------------------------------------
 # H.extend_merge_ceds_total
 # Brief:   Extends ceds total and merges with bond totals
 # Details: Extends ceds total (including IEA other) with CDIAC values, then merges with bond totals
 # Dependencies: data_functions.R
 # Author(s): Rachel Hoesly
 # Params:
         # a.ceds_total_iso = ceds_total_iso,
         # a.extension_start_year = extension_start_year,
         # a.bond_start = bond_start,
         # a.bond_end = bond_end,
         # a.cdiac_fuel = cdiac_fuel,
         # a.ceds_extension_fuels = ceds_extension_fuels,
         # a.cdiac = cdiac,
         # a.iea_start_year = iea_start_year,
         # a.X_bond_years = X_bond_years,
         # a.bond_total_iso = bond_iso
 # Return:    data frame: extended final total fuel by iso
 # Input Files: none
 # Output Files: none
 #TODO: Right now, manual section for bond multiplier corrections based on fuel

 H.extend_merge_ceds_total <- function( a.ceds_total_iso = ceds_total_iso,
                                        a.extension_start_year = extension_start_year,
                                        a.bond_start = bond_start,
                                        a.bond_end = bond_end,
                                        a.cdiac_fuel = cdiac_fuel,
                                        a.extension_fuel_category = extension_fuel_category,
                                        a.ceds_extension_fuels = ceds_extension_fuels,
                                        a.cdiac = cdiac,
                                        a.iea_start_year = iea_start_year,
                                        a.X_bond_years = X_bond_years,
                                        a.bond_total_iso = bond_iso) {

   # a.ceds_total_iso = ceds_total_iso
   # a.extension_start_year = extension_start_year
   # a.bond_start = bond_start
   # a.bond_end = bond_end
   # a.cdiac_fuel = cdiac_fuel
   # a.extension_fuel_category = extension_fuel_category
   # a.ceds_extension_fuels = ceds_extension_fuels
   # a.cdiac = cdiac
   # a.iea_start_year = iea_start_year
   # a.X_bond_years = X_bond_years
   # a.bond_total_iso = bond_iso

   # 1. Extend CEDS total fuel (including IEA other) with CDIAC

   a.ceds_total_iso[ paste0("X",a.extension_start_year:1959) ] <- NA
   a.ceds_total_iso$fuel <- a.cdiac_fuel
   a.ceds_total_iso$sector <- 'all'
   a.ceds_total_iso <- a.ceds_total_iso[  ,c('iso','sector','fuel',paste0("X",a.extension_start_year:a.bond_end)) ]

   a.cdiac$sector <- 'all'
   a.cdiac<- a.cdiac[  ,c('iso','sector','fuel',paste0("X",a.extension_start_year:a.bond_end)) ]

   # loop over different start dates
   ceds_total_iso_extended <- a.ceds_total_iso
   start_years <- unique(a.iea_start_year$start_year)
   for ( i in seq_along(start_years ) ){
     countries <- a.iea_start_year[which( a.iea_start_year$start_year == start_years[i]),'iso']
     drivers <- a.cdiac[which(a.cdiac$iso %in% countries ),]
     ceds_total_iso_extended <- extend_data_on_trend(driver_trend = drivers, input_data = ceds_total_iso_extended,
                                                 start = a.extension_start_year, end = start_years[i],
                                                 diagnostics = F)
   }

   # some small countries don't have cdiac or have zero values through extension.
   ceds_total_iso_extended[ is.na( ceds_total_iso_extended ) ] <- 0

 #2. Calculate  bond multiplier to merge extended CEDS with bond total fuel.
  #  ONLY MATCH TO BOND TOTALS FOR COAL. Bond database drops sectors so that natural gas and petroleum totals are
   # less reliable (especially historical) than CDIAC data.

   if( a.extension_fuel_category == 'coal'){

   # calculate multiplier = bond value/cdiac extended value
   bond_multiplier <- as.data.frame(a.bond_total_iso[,c('iso',a.X_bond_years)])
   #template
   cdiac_extension_values <- a.bond_total_iso[,c('iso', paste0('X',a.bond_start))]
   cdiac_extension_values[a.X_bond_years] <- ceds_total_iso_extended[match(cdiac_extension_values$iso,ceds_total_iso_extended$iso),a.X_bond_years]
   # calculate
   bond_multiplier[a.X_bond_years] <- bond_multiplier[a.X_bond_years] / cdiac_extension_values[a.X_bond_years]

   # correct start, end, and unreal values
   bond_multiplier[a.X_bond_years] <- replace( bond_multiplier[a.X_bond_years],bond_multiplier[a.X_bond_years] == Inf, NA)
   bond_multiplier[a.X_bond_years] <- replace( bond_multiplier[a.X_bond_years],bond_multiplier[a.X_bond_years] == 0, NA)
   bond_multiplier[paste0('X',a.bond_start-5)]<- 1
   bond_multiplier[which( is.na( bond_multiplier [ paste0('X', a.bond_end ) ] )) , paste0( 'X', a.bond_end ) ] <- 1
   bond_multiplier[a.X_bond_years] <- replace( bond_multiplier[a.X_bond_years],bond_multiplier[a.X_bond_years] == 'NaN', NA)
   # remove bond multipliers greater than 500
   bond_multiplier[a.X_bond_years] <- replace( bond_multiplier[a.X_bond_years],bond_multiplier[a.X_bond_years] > 800, NA)

   # manual corrections -
   if( a.extension_fuel_category == 'natural_gas') {bond_multiplier[which( bond_multiplier$iso == 'lux'),c('X1960','X1965')] <- NA
   } else if (a.extension_fuel_category == 'coal'){

   } else if (a.extension_fuel_category == 'petroleum'){}


   # Add columns for inbetween years
   bond_multiplier[ paste0('X',(a.bond_start-5):a.bond_end)[ paste0('X',(a.bond_start-5):a.bond_end)  %!in% names(bond_multiplier)]  ] <- NA
   bond_multiplier <- bond_multiplier[ , c('iso',paste0('X',(a.bond_start-5):a.bond_end))]

   # Interpolate
   bond_multiplier[ , c(paste0('X',(bond_start-5):bond_end))] <- interpolate_NAs(bond_multiplier[ , c(paste0('X',(a.bond_start-5):a.bond_end))])
   bond_multiplier[ is.na(bond_multiplier) ] <- 1

   # 3. Apply Multiplier
   # Apply multipler - total = cdaic extended value * multiplier
   multipliers <- ceds_total_iso_extended[,c('iso',paste0('X', a.bond_start))]
   multipliers[ paste0('X',(a.bond_start):a.bond_end) ] <- bond_multiplier[ match(multipliers$iso,bond_multiplier$iso) , paste0('X',(a.bond_start):a.bond_end) ]
   multipliers[ is.na(multipliers) ] <- 1

   final_ceds_total_iso <- ceds_total_iso_extended
   final_ceds_total_iso[ paste0('X',(a.bond_start):a.bond_end) ] <- final_ceds_total_iso[ paste0('X',(a.bond_start):a.bond_end) ] * multipliers[ paste0('X',(a.bond_start):a.bond_end) ]

   final_ceds_total_iso[is.na(final_ceds_total_iso)] <- 0

   } else{final_ceds_total_iso  <- ceds_total_iso_extended }

 return(final_ceds_total_iso)
 }


 # -----------------------------------------------------------------------------
 # H.disaggregate_total
 # Brief:     disaggregate exteded-final-total into fuel types
 # Details:
 # Dependencies: data_functions.R
 # Author(s): Rachel Hoesly
 # Params:
               # a.extension_start_year = extension_start_year,
               # a.extension_end_year = extension_end_year,
               # a.iea_start_year = iea_start_year,
               # a.ceds_extension_fuels = ceds_extension_fuels,
               # a.ceds_total_iso_fuel = ceds_total_iso_fuel,
               # a.final_total_iso = final_total_iso,
               # a.all_countries = all_countries
 # Return:    list of data frames:
         # other_transformation_fuel: extended other transformation fuel
         # final_iso_fuel: final fuels by fuel, iso
 # Input Files:
 # Output Files: none
  H.disaggregate_total <- function( a.extension_start_year = extension_start_year,
                                    a.extension_end_year = extension_end_year,
                                    a.iea_start_year = iea_start_year,
                                    a.ceds_extension_fuels = ceds_extension_fuels,
                                    a.extension_fuel_category = extension_fuel_category,
                                    a.ceds_total_iso_fuel = ceds_total_iso_fuel,
                                    a.final_total_iso = final_total_iso,
                                    a.all_countries = all_countries,
                                    a.bond_end = bond_end,
                                    a.bond_iso = bond_iso,
                                    a.bond_iso_fuel = bond_iso_fuel,
                                    a.bond_merge_start = bond_merge_start,
                                    a.bond_start = bond_start
                                  ){

    # a.extension_start_year = extension_start_year
    # a.extension_end_year = extension_end_year
    # a.iea_start_year = iea_start_year
    # a.ceds_extension_fuels = ceds_extension_fuels
    # a.ceds_total_iso_fuel = ceds_total_iso_fuel
    # a.final_total_iso = final_total_iso
    # a.all_countries = all_countries
    # a.extension_fuel_category = extension_fuel_category
    # a.bond_iso = bond_iso
    # a.bond_iso_fuel= bond_iso_fuel
    # a.bond_merge_start = bond_merge_start
    # a.bond_start = bond_start
    # a.bond_end = bond_end

    start_years <- unique(a.iea_start_year$start_year)

# 1. Calculate ceds fuel % of total (by iso) in start_year (4 year average)
   # ceds_percentages - percent of
    ceds_percentages <- data.frame(iso = rep(x=a.all_countries, each= length(c(a.ceds_extension_fuels,'other'))),
                                   fuel = rep(x=c(a.ceds_extension_fuels,'other'), length(a.all_countries)))
    ceds_percentages$start_year <- a.iea_start_year[match(ceds_percentages$iso, a.iea_start_year$iso), 'start_year']
    countries <- unique(ceds_percentages$iso)
   for( i in seq_along( ceds_percentages$iso )){
     year <- ceds_percentages[i,'start_year']
     part <- rowMeans(a.ceds_total_iso_fuel[which( a.ceds_total_iso_fuel$iso ==  ceds_percentages[i,'iso'] &
                                                   a.ceds_total_iso_fuel$fuel ==  ceds_percentages[i,'fuel']  ),paste0('X', c(year, year+1,year+2, year+3))])
     if( length( part) == 0 ) part <-  0
     total <- sum( rowMeans(a.ceds_total_iso_fuel[which( a.ceds_total_iso_fuel$iso ==  ceds_percentages[i,'iso']  ),paste0('X', c(year, year+1,year+2, year+3))]) )
     if( length( total) == 0 ) total <-  0
     ceds_percentages[i,'total'] <- total
     ceds_percentages[i,'part'] <- part

     if (a.extension_fuel_category == 'natural_gas' ){
     if( total == 0 & ceds_percentages[i,'fuel'] == 'natural_gas') {ceds_percentages[i,'percent'] <- 1}
     else if( total == 0 & ceds_percentages[i,'fuel'] == 'other') {ceds_percentages[i,'percent'] <- 0}
     else { ceds_percentages[i,'percent'] <- part / total }
     }

     if (a.extension_fuel_category == 'petroleum' ){
       if( total == 0 & ceds_percentages[i,'fuel'] == 'light_oil') {ceds_percentages[i,'percent'] <- 1}
       else if( total == 0 & ceds_percentages[i,'fuel'] == 'diesel_oil') {ceds_percentages[i,'percent'] <- 0}
       else if( total == 0 & ceds_percentages[i,'fuel'] == 'heavy_oil') {ceds_percentages[i,'percent'] <- 0}
       else if( total == 0 & ceds_percentages[i,'fuel'] == 'other') {ceds_percentages[i,'percent'] <- 0}
       else { ceds_percentages[i,'percent'] <- part / total }
     }

   }
    # reformat
    ceds_percentages_other <- ceds_percentages[c('iso','fuel','start_year','percent')]
    ceds_percentages_other$start_year <- paste0('X',ceds_percentages_other$start_year)
    ceds_percentages_other <- cast( ceds_percentages_other, iso + fuel ~ start_year, value = 'percent')

# 2. Calculate Bond Fuel Breakdown overtime (does not include iea other in fuels)
    #set up templates
    #part
    bond_percentages_part <- data.frame(iso = rep(a.all_countries , each = length(a.ceds_extension_fuels)),
                                        fuel =  rep(a.ceds_extension_fuels, times = length(a.all_countries)) )
    bond_percentages_part[,paste0('X',a.bond_start:a.bond_end)] <- a.bond_iso_fuel[match(paste(bond_percentages_part$iso,bond_percentages_part$fuel),
                                                                                         paste(a.bond_iso_fuel$iso,a.bond_iso_fuel$fuel)),
                                                                                   paste0('X',a.bond_start:a.bond_end)]
    bond_percentages_part[is.na(bond_percentages_part)]<- 0
    #total
    bond_percentages_total <- bond_percentages_part[,c('iso',paste0('X',a.bond_start:a.bond_end))]
    bond_percentages_total[,paste0('X',a.bond_start:a.bond_end)] <- a.bond_iso[match(bond_percentages_total$iso, a.bond_iso$iso),
                                                                               paste0('X',a.bond_start:a.bond_end) ]
    # calculate
    bond_percentages <- bond_percentages_part[c('iso','fuel')]
    bond_percentages[,paste0('X',a.bond_start:a.bond_end)] <-
      bond_percentages_part[,paste0('X',a.bond_start:a.bond_end)] / bond_percentages_total[,paste0('X',a.bond_start:a.bond_end)]
    #fill in missing percents and extend
    bond_percentages[,paste0('X',a.bond_start:a.bond_end)] <- replace( bond_percentages[,paste0('X',a.bond_start:a.bond_end)], is.na(bond_percentages[,paste0('X',a.bond_start:a.bond_end)]), NA )
    bond_percentages[,paste0('X',a.bond_start:a.bond_end)] <- t(na.locf( t(bond_percentages[,paste0('X',a.bond_start:a.bond_end)]), fromLast = T))

    for ( i in seq_along(a.all_countries)){
    for (j in seq_along( paste0('X',a.bond_start:a.bond_end) )){
      line <- bond_percentages[which( bond_percentages$iso == a.all_countries[i] ),
                               paste0('X',a.bond_start:a.bond_end)[j] ]
      if ( sum (line) == 0 | is.na(sum(line)) ) {
        if(a.extension_fuel_category == 'petroleum'){
            bond_percentages[which( bond_percentages$iso == a.all_countries[i] ),
                             paste0('X',a.bond_start:a.bond_end)[j] ] <- c(1,0,0)}
        if(a.extension_fuel_category == 'coal'){
          bond_percentages[which( bond_percentages$iso == a.all_countries[i] ),
                           paste0('X',a.bond_start:a.bond_end)[j] ] <- c(1,0,0)}
        if(a.extension_fuel_category == 'natural_gas'){
          bond_percentages[which( bond_percentages$iso == a.all_countries[i] ),
                           paste0('X',a.bond_start:a.bond_end)[j] ] <- c(1)}
      }

    }}

# 3. Combine bond fuel breakdown with ceds(fuel vs other) breakdown
    # combine bond_percentages and ceds_percentages_other -> bond_percentages_final
    # add other line to bond_percentages
    bond_percentages_final <- rbind.fill(bond_percentages, ceds_percentages_other[which(ceds_percentages_other$fuel == 'other'),])
    bond_percentages_final[,paste0('X',a.bond_start:a.bond_end)] <- t(na.locf( t( bond_percentages_final[,paste0('X',a.bond_start:a.bond_end)]), fromLast = T))
    bond_percentages_final <-  bond_percentages_final[order(bond_percentages_final$iso, bond_percentages_final$fuel),]
    # renormalize
    for ( j in seq_along(start_years))
      countries <- a.iea_start_year[which( a.iea_start_year$start_year == start_years[j]),'iso']
    for ( i in seq_along(countries)){
      for (n in seq_along( paste0('X',a.bond_start:a.bond_end) )){
        column <- paste0('X',a.bond_start:a.bond_end)[n]
        selection <- bond_percentages_final[which( bond_percentages_final$iso == countries[i] ),
                                 c('iso','fuel',column) ]
        other_value <- selection[which(selection$fuel =='other'), column]
        selection['new_values'] <- selection[column]
        selection[which(selection$fuel != 'other'), 'new_values'] <- selection[which(selection$fuel != 'other'), 'new_values']*(1-other_value)

        bond_percentages_final[which( bond_percentages_final$iso == countries[i] ),
                               column ] <- selection['new_values']
      }}

 # 4. Combine bond percentages and ceds percentages (in start Year) to slowly transition from bond to ceds
    combined_fuel_percentages_list <- list()
    for ( i in seq_along(start_years)) {
      year0 <- a.bond_merge_start
      years <- year0:start_years[i]
      countries <- a.iea_start_year[which( a.iea_start_year$start_year == start_years[i]),'iso' ]

      combined_percentages <- merge(bond_percentages_final[,c('iso','fuel',paste0('X',a.extension_start_year: (year0 - 1) ))],
                                    ceds_percentages_other[,c('iso','fuel',paste0('X',start_years[i]))] )
      combined_percentages <- combined_percentages[which( combined_percentages$iso %in% countries),]

      # percent ( year n ) =
      for ( n in seq_along( years)){
        ceds_fraction <- (n-1)*(1/(length(years)-1))
        bond_fraction <- 1-ceds_fraction

        bond_split <- bond_percentages_final[ match( paste(combined_percentages$iso, combined_percentages$fuel, combined_percentages$ext_sector)  ,
                                                                paste(bond_percentages_final$iso, bond_percentages_final$fuel, bond_percentages_final$ext_sector) ),
                                                         c(paste0('X',years[n]) )]
        ceds_split <- ceds_percentages[ match( paste(combined_percentages$iso, combined_percentages$fuel, combined_percentages$ext_sector)  ,
                                               paste( ceds_percentages$iso,  ceds_percentages$fuel, ceds_percentages$ext_sector) )
                                        , c('percent') ]
        combined_percentages[,paste0('X',years[n])] <- bond_split*bond_fraction + ceds_split*ceds_fraction

      }

      combined_fuel_percentages_list[[i]] <- combined_percentages
    }
    combined_fuel_percentages <- do.call(rbind.fill,combined_fuel_percentages_list)
    combined_fuel_percentages <- combined_fuel_percentages[, c('iso','fuel',paste0('X',a.extension_start_year:max(start_years)))]

    # renormalize
    combined_fuel_percentages_corrected <- combined_fuel_percentages
    for ( i in seq_along(a.all_countries)){
      for (j in seq_along( paste0('X',a.bond_start:max(start_years)) )){
        line <- combined_fuel_percentages_corrected[which( combined_fuel_percentages_corrected$iso == a.all_countries[i] ),
                                 paste0('X',a.bond_start:a.bond_end)[j] ]
        combined_fuel_percentages_corrected[which( combined_fuel_percentages_corrected$iso == a.all_countries[i] ),
                                            paste0('X',a.bond_start:a.bond_end)[j] ] <- line/sum(line)

      }}



#5. Dissaggregate fuel into fuel types, based on combined_fuel_percentages_corrected

    start_years <- unique(a.iea_start_year$start_year)

    # Loop over IEA data start years
    dissaggregate_fuel_list<-list()
    for( i in seq_along(start_years) ){
      # define countries with start year i
      countries <- a.iea_start_year[which( a.iea_start_year$start_year == start_years[i]),'iso']
      percentages <- combined_fuel_percentages_corrected[which(combined_fuel_percentages_corrected$iso %in% countries),]

      # total fuel template - same order as percentages
      totals <- percentages[c('iso','fuel')]
      totals[ paste0('X',a.extension_start_year:(start_years[i]-1) ) ] <- a.final_total_iso[match(totals$iso, a.final_total_iso$iso),
                                                                                            paste0('X',a.extension_start_year:(start_years[i]-1) ) ]

      aggregate <- totals
      # multiply total by percentages breakdown
      aggregate[paste0('X',a.extension_start_year:(start_years[i]-1) )] <- aggregate[paste0('X',a.extension_start_year:(start_years[i]-1) )]*
                                                                                            percentages[paste0('X',a.extension_start_year:(start_years[i]-1) )]

      # save data to list
      dissaggregate_fuel_list[[i]] <- aggregate
    }

    dissaggregate_final <- do.call('rbind.fill', dissaggregate_fuel_list)
    dissaggregate_final <- dissaggregate_final[order(dissaggregate_final$iso, dissaggregate_final$fuel),]

    # fill out dataframe with zeros for all iso-fuel combinations
    dissaggregate_final_all <- data.frame( iso = rep( a.all_countries , each = length(c(a.ceds_extension_fuels,'other')) ),
                            fuel = rep( x=c(a.ceds_extension_fuels,'other'), times = length(a.all_countries) ), stringsAsFactors = F )
    dissaggregate_final_all[ paste0('X', a.extension_start_year:(max(start_years)-1)) ] <- dissaggregate_final[match(paste0(dissaggregate_final_all$iso, dissaggregate_final_all$fuel),
                                                                                                  paste0(dissaggregate_final$iso, dissaggregate_final$fuel)),
                                                                                                paste0('X', a.extension_start_year:(max(start_years)-1))]

    # add IEA ceds years (post start_years)
    extended_dissagregate_list <- list()
    for( i in seq_along(start_years) ){
      # define countries with start year i
      countries <- a.iea_start_year[which( a.iea_start_year$start_year == start_years[i]),'iso']

      dissaggregate <- dissaggregate_final_all[which( dissaggregate_final_all$iso %in% countries), ]
      dissaggregate[paste0('X',start_years[i]:a.bond_end)] <- a.ceds_total_iso_fuel[ match(paste0(dissaggregate$iso, dissaggregate$fuel),
                                                                                         paste0(a.ceds_total_iso_fuel$iso, a.ceds_total_iso_fuel$fuel)), paste0('X',start_years[i]:a.bond_end)]

      extended_dissagregate_list[[i]] <- dissaggregate
    }
    extended_dissagregate <- do.call('rbind.fill', extended_dissagregate_list)
    extended_dissagregate <- extended_dissagregate[order(extended_dissagregate$iso, extended_dissagregate$fuel),]

   # seperate fuel by fuel into other tranformation fuel and combustion fuel
   other_transformation_fuel <- extended_dissagregate[which(extended_dissagregate$fuel %!in% a.ceds_extension_fuels),c('iso','fuel', paste0('X',a.extension_start_year:a.extension_end_year))]
   final_iso_fuel <- extended_dissagregate[which(extended_dissagregate$fuel %in% a.ceds_extension_fuels),]

   # out
   out <- list(other_transformation_fuel,final_iso_fuel,extended_dissagregate, combined_fuel_percentages_corrected)
   names(out) <- c('other_transformation_fuel','final_iso_fuel','all_iso_fuel', 'iso_fuel_breakdown')
   return(out)
 }


  # -----------------------------------------------------------------------------
  # H.ext_sector_breakdown
  # Brief:    Calculate and merge bond and CEDS sector breakdown (percent by fuel of total
  #           fuel for each extension sector)
  # Details:
  # Dependencies: data_functions.R
  # Author(s): Rachel Hoesly
  # Params:
                # a.extension_start_year = extension_start_year,
                # a.extension_end_year = extension_end_year,
                # a.bond_iso_fuel = bond_iso_fuel,
                # a.bond_iso_sector_fuel = bond_iso_sector_fuel,
                # a.ext_sectors = ext_sectors,
                # a.bond_start = bond_start,
                # a.bond_end = bond_end,
                # a.all_countries = all_countries,
                # a.X_extended_bond_years = X_extended_bond_years,
                # a.X_bond_years = X_bond_years,
                # a.activity = activity,
                # a.ceds_extension_fuels = ceds_extension_fuels,
                # a.ceds_total_iso_fuel = ceds_total_iso_fuel,
                # a.ceds_total_iso_fuel_sector = ceds_total_iso_fuel_sector,
                # a.ceds_total_iso_fuel_ext_sector = ceds_total_iso_fuel_ext_sector,
                # a.final_total_iso = final_total_iso,
                # a.bond_merge_start = bond_merge_start,
                # a.iea_start_year = iea_start_year

  # Return:    data frames: combined ext_sector breakdown

  # Input Files:
  # Output Files: none

  H.ext_sector_breakdown <- function(a.extension_start_year = extension_start_year,
                                     a.extension_end_year = extension_end_year,
                                     a.bond_iso_fuel = bond_iso_fuel,
                                     a.bond_iso_sector_fuel = bond_iso_sector_fuel,
                                     a.ext_sectors = ext_sectors,
                                     a.bond_start = bond_start,
                                     a.bond_end = bond_end,
                                     a.all_countries = all_countries,
                                     a.X_extended_bond_years = X_extended_bond_years,
                                     a.X_bond_years = X_bond_years,
                                     a.activity = activity,
                                     a.ceds_extension_fuels = ceds_extension_fuels,
                                     a.ceds_total_iso_fuel = ceds_total_iso_fuel,
                                     a.ceds_total_iso_fuel_sector = ceds_total_iso_fuel_sector,
                                     a.ceds_total_iso_fuel_ext_sector = ceds_total_iso_fuel_ext_sector,
                                     a.final_total_iso = final_total_iso,
                                     a.bond_merge_start = bond_merge_start,
                                     a.iea_start_year = iea_start_year ,
                                     a.extension_fuel_category = extension_fuel_category,
                                     a.ext_sector_percent_start = ext_sector_percent_start
                                     ){
    # a.extension_start_year = extension_start_year
    # a.extension_end_year = extension_end_year
    # a.bond_iso_fuel = bond_iso_fuel
    # a.bond_iso_sector_fuel = bond_iso_sector_fuel
    # a.ext_sectors = ext_sectors
    # a.bond_start = bond_start
    # a.bond_end = bond_end
    # a.all_countries = all_countries
    # a.X_extended_bond_years = X_extended_bond_years
    # a.X_bond_years = X_bond_years
    # a.activity = activity
    # a.ceds_extension_fuels = ceds_extension_fuels
    # a.ceds_total_iso_fuel = ceds_total_iso_fuel
    # a.ceds_total_iso_fuel_sector = ceds_total_iso_fuel_sector
    # a.ceds_total_iso_fuel_ext_sector = ceds_total_iso_fuel_ext_sector
    # a.final_total_iso = final_total_iso
    # a.bond_merge_start = bond_merge_start
    # a.iea_start_year = iea_start_year
    # a.extension_fuel_category = extension_fuel_category
    # a.ext_sector_percent_start = ext_sector_percent_start

    start_years <- unique(a.iea_start_year$start_year)
    bond_percent_start_year <- names(a.ext_sector_percent_start)[3]

    #######
    # Calculate Bond sector Breakdown - percent of total fuel use by each aggregate sector for iso,year for all bond years
    # - calculate percentages
    # - adds start percent, inbetween years and interpolate
    # - manually correct wierd values
    # - renormalize so that each iso-fuel-year adds to 1

    bond_sector_percentages_total_template <- a.bond_iso_sector_fuel[,c('iso','fuel','ext_sector')]
    bond_sector_percentages_total_template[a.X_bond_years] <- 0
    bond_sector_percentages_total_template <- replaceValueColMatch( bond_sector_percentages_total_template, a.bond_iso_fuel,
                                                                    x.ColName = a.X_bond_years,
                                                                    match.x = c('iso','fuel'),
                                                                    addEntries = F)
    bond_sector_percentages <- a.bond_iso_sector_fuel[,c('iso','fuel','ext_sector',a.X_bond_years)]
    bond_sector_percentages[ a.X_bond_years ] <- bond_sector_percentages[ a.X_bond_years ]/bond_sector_percentages_total_template[ a.X_bond_years ]

    bond_sector_percentages <- replace( bond_sector_percentages , bond_sector_percentages == 'NaN' , NA)
    bond_sector_percentages <- replace( bond_sector_percentages , bond_sector_percentages == 'Inf' , NA)

    # Manually Correct Bond Percentages
    if ( a.extension_fuel_category == 'petroleum'){
     # RCO stops in 1950 - 1970 so remove zeros, and extend/renormalize percentages (so it doesn't drop out completely)
      bond_sector_percentages[which( bond_sector_percentages$ext_sector == 'RCO'), paste0('X',seq(a.bond_start,1970,5))] <- replace(
                            bond_sector_percentages[which( bond_sector_percentages$ext_sector == 'RCO'), paste0('X',seq(a.bond_start,1970,5))] ,
                            bond_sector_percentages[which( bond_sector_percentages$ext_sector == 'RCO'), paste0('X',seq(a.bond_start,1970,5))] == 0 ,
                              NA)
    }


    # add 1850 values, add inbetween columns, and interpolate
    bond_sector_percentages[ , paste0('X',bond_percent_start_year)] <- a.ext_sector_percent_start[ match( paste0(bond_sector_percentages$fuel, bond_sector_percentages$ext_sector),
                                                                                                          paste0(a.ext_sector_percent_start$fuel, a.ext_sector_percent_start$ext_sector)), bond_percent_start_year]
    bond_sector_percentages[ paste0('X',a.bond_start:a.bond_end)[ paste0('X',a.bond_start:a.bond_end)  %!in% names(bond_sector_percentages)]  ] <- NA
    bond_sector_percentages <- bond_sector_percentages[ , c('iso','ext_sector','fuel',paste0('X',a.bond_start:a.bond_end))]
    bond_sector_percentages[ , paste0('X',a.bond_start:a.bond_end) ] <- interpolate_NAs(bond_sector_percentages[ , paste0('X',a.bond_start:a.bond_end)])

    bond_sector_percentages[paste0('X',a.extension_start_year:a.bond_start)] <- bond_sector_percentages[paste0('X',a.bond_start)]
    bond_sector_percentages <- bond_sector_percentages[ , c('iso','ext_sector','fuel',a.X_extended_bond_years)]

    bond_sector_percentages[ is.na(bond_sector_percentages) ] <- 0

    # fill out dataframe with zero values
    template <- data.frame( iso = rep(a.all_countries, each =  length(a.ceds_extension_fuels)*length(a.ext_sectors)) ,
                            fuel =  rep(a.ceds_extension_fuels, each=length(a.ext_sectors), times= length(a.all_countries))   ,
                            ext_sector = rep(x= a.ext_sectors, times = length(a.all_countries)*length(a.ceds_extension_fuels)), stringsAsFactors=F )

    bond_sector_percentages_full <- template
    bond_sector_percentages_full[ X_extended_bond_years ] <- bond_sector_percentages[match(paste(template$iso, template$fuel, template$ext_sector),
                                                                                           paste(bond_sector_percentages$iso, bond_sector_percentages$fuel, bond_sector_percentages$ext_sector))
                                                                                     , X_extended_bond_years]

    # For natural gas - manually change US percentages
    if(a.extension_fuel_category == 'natural_gas'){

      bond_sector_percentages_full[which(bond_sector_percentages_full$iso == 'usa'), paste0('X', 1900:1959)]<- NA
      bond_sector_percentages_full[which(bond_sector_percentages_full$iso == 'usa'), paste0('X', c(1850,1910))]<- c(0,1,0,0,0,0)
      bond_sector_percentages_full[which(bond_sector_percentages_full$iso == 'usa'), paste0('X', 1850:1960)] <-
        interpolate_NAs(bond_sector_percentages_full[which(bond_sector_percentages_full$iso == 'usa'), paste0('X', 1850:1960)])
    }

    bond_sector_percentages_full[is.na(bond_sector_percentages_full)] <- 0

    # for countries with no bond sectors, fill in arbitrary sector split (different for fuel)
    #TODO get rid of for loops
    bond_sector_percentages_corrected <- bond_sector_percentages_full
    for (i in seq_along( a.all_countries)){
      for ( n in seq(a.ceds_extension_fuels)){
        for (m in seq_along(a.bond_start:a.extension_end_year)){
          Xyear <- paste0('X',(a.bond_start:a.extension_end_year)[m] )
          lines <- bond_sector_percentages_corrected[which( bond_sector_percentages_corrected$iso == a.all_countries[i] &
                                                              bond_sector_percentages_corrected$fuel == a.ceds_extension_fuels[n] ) , c('iso','fuel','ext_sector',Xyear) ]
          if( sum(lines[,Xyear]) == 0 ){

            bond_sector_percentages_corrected[which( bond_sector_percentages_corrected$iso == a.all_countries[i] &
                                                       bond_sector_percentages_corrected$fuel == a.ceds_extension_fuels[n] ) , Xyear] <-
              a.ext_sector_percent_start[ match(paste0(lines$fuel, lines$ext_sector),paste0(a.ext_sector_percent_start$fuel, a.ext_sector_percent_start$ext_sector)), 3]

            } else if( sum(lines[,Xyear]) != 1 ){

              bond_sector_percentages_corrected[which( bond_sector_percentages_corrected$iso == a.all_countries[i] &
                                                         bond_sector_percentages_corrected$fuel == a.ceds_extension_fuels[n] ) ,
                                                Xyear] <- lines[,Xyear]/sum(lines[,Xyear])

            }} }}



    #######
    # Calculate CEDS aggregate sector splits in start year
    # - calucalte ceds breakdown in start year for each iso

    ceds_agg_percent_list <- list()
    for ( i in seq_along((start_years))){
      countries <- a.iea_start_year[which( a.iea_start_year$start_year == start_years[i]),'iso']
      disaggregate <-a.ceds_total_iso_fuel_ext_sector[which(a.ceds_total_iso_fuel_ext_sector$iso %in% countries),]

      percentages <- disaggregate[,c('iso','fuel','ext_sector',paste0('X',start_years[i]))]
      percentages$a.ceds_total_iso_fuel <- 0
      percentages$a.ceds_total_iso_fuel <- a.ceds_total_iso_fuel[match( paste(percentages$iso, percentages$fuel),
                                        paste(a.ceds_total_iso_fuel$iso, a.ceds_total_iso_fuel$fuel)), paste0('X',start_years[i]) ]


      percentages['percent'] <- percentages[paste0('X',start_years[i])]/percentages$a.ceds_total_iso_fuel
      percentages[which( percentages$a.ceds_total_iso_fuel == 0),'percent'] <- 0
      ceds_agg_percent_list[[i]]<-percentages
    }
    ceds_agg_percent <- do.call(rbind.fill, ceds_agg_percent_list)
    ######


    # Combine Bond and CEDS aggregate splits. Slowly transition from Bond breakdowns to to CEDS breakdowns starting in bond_merge_start
    #ceds_agg_percent  and bond_sector_percentages

    combined_sector_percentages_list <- list()
    for ( i in seq_along(start_years)) {
      year0 <- a.bond_merge_start
      years <- year0:start_years[i]
      countries <- a.iea_start_year[which( a.iea_start_year$start_year == start_years[i]),'iso' ]

      combined_percentages <- merge(bond_sector_percentages_corrected[,c('iso','ext_sector','fuel',paste0('X',a.extension_start_year: (year0 - 1) ))],
                                    ceds_agg_percent[,c('iso','ext_sector','fuel','percent')] )
      names(combined_percentages)[which(names(combined_percentages) == 'percent' )] <- paste0('X',start_years[i])

      combined_percentages <- combined_percentages[which( combined_percentages$iso %in% countries),]

      # percent ( year n ) =
      for ( n in seq_along( years)){
        ceds_fraction <- (n-1)*(1/(length(years)-1))
        bond_fraction <- 1-ceds_fraction

        bond_split <- bond_sector_percentages_corrected[ match( paste(combined_percentages$iso, combined_percentages$fuel, combined_percentages$ext_sector)  ,
                                                      paste(bond_sector_percentages_corrected$iso, bond_sector_percentages_corrected$fuel, bond_sector_percentages_corrected$ext_sector) ),
                                               c(paste0('X',years[n]) )]
        ceds_split <- ceds_agg_percent[ match( paste(combined_percentages$iso, combined_percentages$fuel, combined_percentages$ext_sector)  ,
                                               paste( ceds_agg_percent$iso,  ceds_agg_percent$fuel, ceds_agg_percent$ext_sector) )
                                        , c('percent') ]

        combined_percentages[,paste0('X',years[n])] <- bond_split*bond_fraction + ceds_split*ceds_fraction

      }


      combined_sector_percentages_list[[i]] <- combined_percentages
    }
    combined_sector_percentages <- do.call(rbind.fill,combined_sector_percentages_list)
    combined_sector_percentages <- combined_sector_percentages[, c('iso','ext_sector','fuel',paste0('X',a.extension_start_year:a.extension_end_year))]
    combined_sector_percentages[is.na(combined_sector_percentages)] <- 0

    # fill out dataframe with zero values
    template <- data.frame( iso = rep(a.all_countries, each =  length(a.ceds_extension_fuels)*length(a.ext_sectors)) ,
                            fuel =  rep(a.ceds_extension_fuels, each=length(a.ext_sectors), times= length(a.all_countries))   ,
                            ext_sector = rep(x= a.ext_sectors, times = length(a.all_countries)*length(a.ceds_extension_fuels)), stringsAsFactors=F )

    combined_sector_percentages_full <- template
    combined_sector_percentages_full[ paste0('X',a.bond_start:extension_end_year) ] <- combined_sector_percentages[match(paste(combined_sector_percentages_full$iso, combined_sector_percentages_full$fuel, combined_sector_percentages_full$ext_sector),
                                                                                                   paste(combined_sector_percentages$iso, combined_sector_percentages$fuel, combined_sector_percentages$ext_sector))
                                                                                     , paste0('X',a.bond_start:extension_end_year) ]
    combined_sector_percentages_full[is.na(combined_sector_percentages_full)] <- 0

    # Renormalize combined percentages
    combined_sector_percentages_corrected <- combined_sector_percentages_full
    for( i in seq_along(a.all_countries)) {
      for( n in seq_along(a.ceds_extension_fuels)){
        for (l in seq_along( a.extension_start_year:a.extension_end_year )) {
          breakdown <- combined_sector_percentages_corrected[which( combined_sector_percentages_corrected$iso == a.all_countries[i] &
                                                                    combined_sector_percentages_corrected$fuel == a.ceds_extension_fuels[n] ),
                                                             paste0('X',(a.extension_start_year:a.extension_end_year)[l]) ]
          if ( all.na( breakdown ) ) {

            combined_sector_percentages_corrected[which( combined_sector_percentages_corrected$iso == a.all_countries[i] &
                                                           combined_sector_percentages_corrected$fuel == a.ceds_extension_fuels[n] ) , paste0('X',(a.extension_start_year:a.extension_end_year)[l])] <-
              a.ext_sector_percent_start[ match(paste0(lines$fuel, lines$ext_sector),paste0(a.ext_sector_percent_start$fuel, a.ext_sector_percent_start$ext_sector)), 3]

          }else if( sum(breakdown) != 1 ){
            combined_sector_percentages_corrected[which( combined_sector_percentages_corrected$iso == a.all_countries[i] &
                                                           combined_sector_percentages_corrected$fuel == a.ceds_extension_fuels[n]  ),
                                                  paste0('X',(a.extension_start_year:a.extension_end_year)[l]) ] <- breakdown/sum(breakdown)
          } }}}

    return(combined_sector_percentages_corrected)
  }

  # -----------------------------------------------------------------------------
  # H.sector_breakdown
  # Brief:    Calculate final percentage sector breakdown (percent by fuel-sector of total
  #           fuel for each sector)
  # Details:
  # Dependencies:
  # Author(s): Rachel Hoesly
  # Params:
 # a.extension_start_year = extension_start_year
 # a.extension_end_year = extension_end_year
 # a.ceds_extension_fuels = ceds_extension_fuels
 # a.activity = activity
 # a.iea_start_year = iea_start_year
 # a.sector_percent_start = sector_percent_start
 # a.all_countries = all_countries
 # a.ext_sectors = ext_sectors
 # a.ceds_total_iso_fuel_sector = ceds_total_iso_fuel_sector
 # a.ceds_total_iso_fuel_ext_sector = ceds_total_iso_fuel_ext_sector
 # a.ext_sector_breakdown = ext_sector_breakdown

  # Return:    data frames: final sector breakdown

  # Input Files:
  # Output Files: none
  H.sector_breakdown <- function(a.extension_start_year = extension_start_year,
                                 a.extension_end_year = extension_end_year,
                                 a.ceds_extension_fuels = ceds_extension_fuels,
                                 a.activity = activity,
                                 a.iea_start_year = iea_start_year,
                                 a.bond_percent = bond_percent,
                                 a.all_countries = all_countries,
                                 a.ext_sectors = ext_sectors,
                                 a.ceds_total_iso_fuel_sector = ceds_total_iso_fuel_sector,
                                 a.ceds_total_iso_fuel_ext_sector = ceds_total_iso_fuel_ext_sector,
                                 a.ext_sector_breakdown = ext_sector_breakdown,
                                 a.bond_start = bond_start,
                                 a.bond_end = bond_end,
                                 a.sector_percent_start = sector_percent_start
                                 ){
    # a.extension_start_year = extension_start_year
    # a.extension_end_year = extension_end_year
    # a.ceds_extension_fuels = ceds_extension_fuels
    # a.activity = activity
    # a.iea_start_year = iea_start_year
    # a.sector_percent_start = sector_percent_start
    # a.all_countries = all_countries
    # a.ext_sectors = ext_sectors
    # a.ceds_total_iso_fuel_sector = ceds_total_iso_fuel_sector
    # a.ceds_total_iso_fuel_ext_sector = ceds_total_iso_fuel_ext_sector
    # a.ext_sector_breakdown = ext_sector_breakdown
    # a.bond_start = bond_start
    # a.bond_end = bond_end

    # Load Relevant Data
    ceds_sector_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_sector_ext_map", ".xlsx", sheet_selection = 'CEDS_to_ext',meta = F )

    # CEDS dissagregate sector splits in data start year (1960, 1971)
    # Calculate Ceds sector fuel % in start_year as percent of ext sector-fuel
    a.ceds_total_iso_fuel_sector$ext_sector <- ceds_sector_map[match(a.ceds_total_iso_fuel_sector$sector,ceds_sector_map$sector),'ext_sector']

    ceds_ext_sector_percentages <- a.ceds_total_iso_fuel_sector[ ,c('iso','sector','fuel','ext_sector')]
    ceds_ext_sector_percentages$start_year <- a.iea_start_year[match(ceds_ext_sector_percentages$iso, a.iea_start_year$iso), 'start_year']

    for( i in seq_along( ( ceds_ext_sector_percentages[,1] ))) {
      year <- ceds_ext_sector_percentages$start_year[i]
      total <- rowMeans(a.ceds_total_iso_fuel_ext_sector[match(paste(ceds_ext_sector_percentages$iso[i], ceds_ext_sector_percentages$ext_sector[i],ceds_ext_sector_percentages$fuel[i]),
                                                paste(a.ceds_total_iso_fuel_ext_sector$iso, a.ceds_total_iso_fuel_ext_sector$ext_sector,a.ceds_total_iso_fuel_ext_sector$fuel)),
                                          paste0('X', c(year, year+1,year+2, year+3))])
      ceds_ext_sector_percentages[i,'total'] <- total
      ceds_ext_sector_percentages[i,'disaggregate'] <- rowMeans(a.ceds_total_iso_fuel_sector[i , paste0('X', c(year, year+1,year+2, year+3)) ])
      if ( total == 0 | is.na(total)) {ceds_ext_sector_percentages$percent[i] <- 0} else{
        ceds_ext_sector_percentages[i,'percent'] <- ceds_ext_sector_percentages[i,'disaggregate'] / ceds_ext_sector_percentages[i,'total']
      }

    }

    ceds_ext_sector_percentages <- ceds_ext_sector_percentages[which( ceds_ext_sector_percentages$sector %!in% c('1A1bc_Other-transformation')),]

    # correct ceds_ext_sector_percentages - correct zero sums
    ceds_ext_sector_percentages_corrected <- ceds_ext_sector_percentages
    for( i in seq_along(a.all_countries)) {
      for ( n in seq_along( a.ceds_extension_fuels )){
        for ( m in seq_along( a.ext_sectors)){
          selected_percents <- ceds_ext_sector_percentages_corrected[which(
              ceds_ext_sector_percentages_corrected$iso == a.all_countries[i] &
              ceds_ext_sector_percentages_corrected$fuel == a.ceds_extension_fuels[n] &
              ceds_ext_sector_percentages_corrected$ext_sector == a.ext_sectors[m]), ]
          if( sum ( selected_percents$percent ) == 0 ){
            if( a.ext_sectors[m] == 'Power' ){
              ceds_ext_sector_percentages_corrected[which(
                  ceds_ext_sector_percentages_corrected$iso == a.all_countries[i] &
                  ceds_ext_sector_percentages_corrected$fuel == a.ceds_extension_fuels[n] &
                  ceds_ext_sector_percentages_corrected$ext_sector == a.ext_sectors[m]), 'percent'] <- a.sector_percent_start[
                    match(paste0(selected_percents$sector),
                          paste0(a.sector_percent_start$sector)), 3]
            }else if( a.ext_sectors[m] == 'Industry' ){
              ceds_ext_sector_percentages_corrected[which(
                  ceds_ext_sector_percentages_corrected$iso == a.all_countries[i] &
                  ceds_ext_sector_percentages_corrected$fuel == a.ceds_extension_fuels[n] &
                  ceds_ext_sector_percentages_corrected$ext_sector == a.ext_sectors[m]), 'percent'] <- a.sector_percent_start[
                    match(paste0(selected_percents$sector),
                          paste0(a.sector_percent_start$sector)), 3]
            }else if( a.ext_sectors[m] == 'RCO' ){
              ceds_ext_sector_percentages_corrected[which(
                  ceds_ext_sector_percentages_corrected$iso == a.all_countries[i] &
                  ceds_ext_sector_percentages_corrected$fuel == a.ceds_extension_fuels[n] &
                  ceds_ext_sector_percentages_corrected$ext_sector == a.ext_sectors[m]), 'percent'] <- a.sector_percent_start[
                    match(paste0(selected_percents$sector),
                          paste0(a.sector_percent_start$sector)), 3]
            }else if( a.ext_sectors[m] == 'Shipping' ){
              ceds_ext_sector_percentages_corrected[which(
                  ceds_ext_sector_percentages_corrected$iso == a.all_countries[i] &
                  ceds_ext_sector_percentages_corrected$fuel == a.ceds_extension_fuels[n] &
                  ceds_ext_sector_percentages_corrected$ext_sector == a.ext_sectors[m]), 'percent'] <- a.sector_percent_start[
                    match(paste0(selected_percents$sector),
                          paste0(a.sector_percent_start$sector)), 3]
            }else if( a.ext_sectors[m] == 'Transportation' ){
                ceds_ext_sector_percentages_corrected[which(
                ceds_ext_sector_percentages_corrected$iso == a.all_countries[i] &
                  ceds_ext_sector_percentages_corrected$fuel == a.ceds_extension_fuels[n] &
                  ceds_ext_sector_percentages_corrected$ext_sector == a.ext_sectors[m]), 'percent'] <- a.sector_percent_start[
                                                                  match(paste0(selected_percents$sector),
                                                                        paste0(a.sector_percent_start$sector)), 3]

                  }
          }else if( a.ext_sectors[m] == '1A4c_Agriculture-forestry-fishing' ){
            ceds_ext_sector_percentages_corrected[which(
              ceds_ext_sector_percentages_corrected$iso == a.all_countries[i] &
                ceds_ext_sector_percentages_corrected$fuel == a.ceds_extension_fuels[n] &
                ceds_ext_sector_percentages_corrected$ext_sector == a.ext_sectors[m]), 'percent'] <- c(1) }
        }

        }}

    # Create historical breakdown from aggregate sectors to ceds sectors
    # Take ceds_ext_sector_percentages_corrected from above (ceds breakdown from aggreagate to ceds sectors in IEA start year, per country)
    # And the self defined splits sector_percent_start_year (year defined in the column header of that csv file), then linear interplate between, and renormalize
    # CEDS sector splits,aggregate sectors to CEDS sectors in 1960. Defined Splits in a.extension_start_year, linearly interpolate
    #combine bond_sector_percentages and ceds breakdown
    # a.sector_percent_start = sector_percent_start
    # New dataframe for all ceds breakdowns
    ceds_breakdown <- ceds_ext_sector_percentages_corrected
    # Add ceds breakdowns in start years
    ceds_breakdown[which( ceds_breakdown$start_year == '1960'), 'X1960'] <- ceds_breakdown[which( ceds_breakdown$start_year == '1960'), 'percent']
    ceds_breakdown[which( ceds_breakdown$start_year == '1971'), 'X1971'] <- ceds_breakdown[which( ceds_breakdown$start_year == '1971'), 'percent']
    # New Dataframe for bond breakdown in bond start year (defined in column name in a.sector_percent_start)
    sector_percent_start_year <- names(a.sector_percent_start)[which( names(a.sector_percent_start) %!in% c("ext_sector","sector"))]
    sector_percent_start_year <- as.numeric(sector_percent_start_year)
    if(sector_percent_start_year > a.bond_start ){
      a.sector_percent_start[,paste0('X',a.extension_start_year:(sector_percent_start_year-1))] <- a.sector_percent_start[ , paste(sector_percent_start_year) ]

    }
    names( a.sector_percent_start )[which( names( a.sector_percent_start ) == sector_percent_start_year ) ] <- paste0('X',sector_percent_start_year)
    a.sector_percent_start <- a.sector_percent_start[  c( "ext_sector","sector" , paste0('X',a.extension_start_year:sector_percent_start_year)) ]
    # merge ceds and bond breakdown
    ceds_breakdown <- merge( ceds_breakdown[c('iso','sector','ext_sector','fuel','X1960','X1971')], a.sector_percent_start, all.x = T, all.y=F)
    ceds_breakdown <- ceds_breakdown[ , c('iso','sector','ext_sector','fuel',paste0('X',a.extension_start_year:sector_percent_start_year),'X1960','X1971' )]

    # Add interpolation years and interpolate
    ceds_breakdown_years <- names( ceds_breakdown)[grep( 'X',names( ceds_breakdown))]
    ceds_breakdown[ paste0('X',a.extension_start_year:a.extension_end_year)[ paste0('X',a.extension_start_year:a.extension_end_year)  %!in% ceds_breakdown_years]  ] <- NA
    ceds_breakdown <- ceds_breakdown[ , c('iso','sector','ext_sector','fuel',paste0('X',a.extension_start_year:(a.extension_end_year+1))) ]
    ceds_breakdown[ , paste0('X',a.extension_start_year:(a.extension_end_year+1))] <- interpolate_NAs(ceds_breakdown[ ,paste0('X',a.extension_start_year:(a.extension_end_year+1))])

    # if(a.extension_start_year < 1850) ceds_breakdown[paste0('X',a.extension_start_year:(a.bond_start))] <- ceds_breakdown[paste0('X',a.extension_start_year)]
    ceds_breakdown <- ceds_breakdown[ , c('iso','sector','ext_sector','fuel',paste0('X',a.extension_start_year:a.extension_end_year))]

    ceds_breakdown <- replace( ceds_breakdown, is.na(ceds_breakdown) , 0 )

    # percentages as a total of iso-fuel
    # disaggregate percent, ceds_breakdown
    # aggregate category to dissagregate <- combined sector percentages
    final_percentages<-ceds_breakdown
    combined_template <- ceds_breakdown[c('iso','ext_sector','sector','fuel')]
    combined_template[paste0('X',a.extension_start_year:a.extension_end_year)] <- NA
    combined_template[paste0('X',a.extension_start_year:a.extension_end_year)] <- a.ext_sector_breakdown[
      match(paste(combined_template$iso, combined_template$ext_sector, combined_template$fuel),
            paste(a.ext_sector_breakdown$iso, a.ext_sector_breakdown$ext_sector, a.ext_sector_breakdown$fuel)),
      paste0('X',a.extension_start_year:a.extension_end_year)]
    combined_template <- replace( combined_template, is.na(combined_template), 0)

    final_percentages[paste0('X',a.extension_start_year:a.extension_end_year)] <- ceds_breakdown[paste0('X',a.extension_start_year:a.extension_end_year)]* combined_template[paste0('X',a.extension_start_year:a.extension_end_year)]

    # Renormalize combined percentages
    final_sector_breakdown <- final_percentages
    for( i in seq_along(a.all_countries)) {
      for ( n in seq_along( a.ceds_extension_fuels )){
        for (l in seq_along( a.extension_end_year:a.extension_start_year )) {
          breakdown <- final_sector_breakdown[which( final_sector_breakdown$iso == a.all_countries[i] &
                                                            final_sector_breakdown$fuel == a.ceds_extension_fuels[n]   ),
                                                   paste0('X',(a.extension_start_year:a.extension_end_year)[l]) ]
          if ( sum(breakdown) == 0 ){
            # final_sector_breakdown[which( final_sector_breakdown$iso == a.all_countries[i] &
            #                                      final_sector_breakdown$fuel == a.ceds_extension_fuels[n]   ),
            #                             paste0('X',(a.extension_start_year:a.extension_end_year)[l]) ] <- c(rep(times=12,x=0),0.5,rep(times=11,x=0),0.5,0,0)
            # breakdown <- c(rep(times=12,x=0),0.5,rep(times=11,x=0),0.5,0,0)
            final_sector_breakdown[which( final_sector_breakdown$iso == a.all_countries[i] &
                                                 final_sector_breakdown$fuel == a.ceds_extension_fuels[n]   ),
                                              paste0('X',(a.extension_start_year:a.extension_end_year)[l]) ] <- final_sector_breakdown[ which( final_sector_breakdown$iso == a.all_countries[i] &
                                                                                                                                         final_sector_breakdown$fuel == a.ceds_extension_fuels[n]   ),
                                                                                                                                paste0('X',(a.extension_start_year:a.extension_end_year)[(l-1)]) ]

            breakdown <- final_sector_breakdown[which( final_sector_breakdown$iso == a.all_countries[i] &
                                                           final_sector_breakdown$fuel == a.ceds_extension_fuels[n]   ),
                                                  paste0('X',(a.extension_start_year:a.extension_end_year)[l]) ]

          }else if( sum(breakdown) != 1 ){
            final_sector_breakdown[which( final_sector_breakdown$iso == a.all_countries[i] &
                                                 final_sector_breakdown$fuel == a.ceds_extension_fuels[n]   ),
                                        paste0('X',(a.extension_start_year:a.extension_end_year)[l]) ] <- breakdown/sum(breakdown)
          } }}}

    return( final_sector_breakdown )
   }
  # -----------------------------------------------------------------------------
  # H.disaggregate_to_final
  # Brief:
  # Details:
  # Dependencies:
  # Author(s): Rachel Hoesly
  # Params:

  # Return:    dataframe:

  # Input Files:
  # Output Files: none

  H.disaggregate_to_final <- function(a.iea_start_year = iea_start_year,
                                      a.activity = activity,
                                       a.final_iso_fuel = final_iso_fuel,
                                       a.ceds_extension_fuels = ceds_extension_fuels ,
                                       a.extension_start_year = extension_start_year,
                                       a.extension_end_year = extension_end_year,
                                       a.sector_breakdown = sector_breakdown
                                     ){
    # a.iea_start_year = iea_start_year
    # a.activity = activity
    # a.final_iso_fuel = final_iso_fuel
    # a.ceds_extension_fuels = ceds_extension_fuels
    # a.extension_start_year = extension_start_year
    # a.extension_end_year = extension_end_year
    # a.sector_breakdown = sector_breakdown


    start_years <- unique(a.iea_start_year$start_year)

    # Loop over IEA data start years
    dissaggregate_sector_list<-list()
    for( i in seq_along(start_years) ){
      # define countries with start year i
      countries <- a.iea_start_year[which( a.iea_start_year$start_year == start_years[i]),'iso']
      percentages <- a.sector_breakdown[which(a.sector_breakdown$iso %in% countries),]

      # total fuel template - same order as percentages
      totals <- percentages[c('iso','sector','ext_sector','fuel')]
      totals[ paste0('X',a.extension_start_year:(start_years[i]-1) ) ] <- 0
      totals <- replaceValueColMatch(totals, a.final_iso_fuel,
                                     x.ColName = paste0('X',a.extension_start_year:(start_years[i]-1) ),
                                     match.x = c('iso','fuel'),
                                     addEntries = F)
      aggregate <- totals
      # multiply total by percentages breakdown
      aggregate[paste0('X',a.extension_start_year:(start_years[i]-1) )] <- aggregate[paste0('X',a.extension_start_year:(start_years[i]-1) )]*
        percentages[paste0('X',a.extension_start_year:(start_years[i]-1) )]

      # save data to list
      dissaggregate_sector_list[[i]] <- aggregate
    }

    dissaggregate_final <- do.call('rbind.fill', dissaggregate_sector_list)


    # fill out data with zero country and sectors
    fuel_activity <- a.activity[ which ( a.activity$fuel %in% a.ceds_extension_fuels),
                                 c('iso','sector','fuel','units',paste0('X',a.extension_start_year:end_year))]

    final_values <- fuel_activity
    final_values[paste0('X',a.extension_start_year:1959)] <- NA

    for ( i in seq_along( start_years )){
      countries <- iea_start_year[which( iea_start_year$start_year == start_years[i]),'iso']

      final_values[which(final_values$iso %in% countries), paste0('X',a.extension_start_year:(start_years[i]-1))] <- NA

      values_start_date <- dissaggregate_final[ which(dissaggregate_final$iso %in% countries),
                                                c('iso','sector','fuel', paste0('X',a.extension_start_year:(start_years[i]-1)) )]

      final_values <- replaceValueColMatch(final_values , values_start_date,
                                           x.ColName = paste0('X',a.extension_start_year:(start_years[i]-1)),
                                           match.x = c('iso','sector','fuel'),
                                           addEntries = F)
    }

    # do not add intl shipping ( use other data )
    final_values <- final_values[which(final_values$sector%!in% c('1A3di_International-shipping')), ]

    final_values[is.na(final_values)] <- 0

    out <- list(dissaggregate_final,final_values)
    names(out) <- c('final_iso_fuel_sector_calculated','final_iso_fuel_sector_full_extension')

    return( out )
  }

  # -----------------------------------------------------------------------------
  # H.add_to_database
  # Brief:
  # Details:
  # Dependencies:
  # Author(s): Rachel Hoesly
  # Params:

  # Return:    dataframe:

  # Input Files:
  # Output Files: none

  H.add_to_database <- function(a.extension_start_year = extension_start_year,
                                a.extension_end_year = extension_end_year,
                                a.final_iso_fuel_sector = final_iso_fuel_sector_full_extension,
                                a.iea_start_year = iea_start_year,
                                a.activity = activity,
                                a.ceds_extension_fuels = ceds_extension_fuels){
    # define variables
    replace_sectors <- unique(a.final_iso_fuel_sector$sector)
    start_years <- unique(a.iea_start_year$start_year)

    #Split activity data into data to replace, and not replace
    activity.replace <- a.activity[ which( a.activity$sector %in% replace_sectors &
                                           a.activity$fuel %in% a.ceds_extension_fuels ) , ]

    activity.done <- a.activity[ which( !(a.activity$sector %in% replace_sectors &
                                          a.activity$fuel %in% a.ceds_extension_fuels ) ) , ]

    # loop over IEA data start years ( 1960, 1971)
    for ( i in seq_along ( start_years ) ){
      countries <- a.iea_start_year[which( a.iea_start_year$start_year == start_years[i]),'iso']
      current_replace <- a.final_iso_fuel_sector[which( a.final_iso_fuel_sector$iso %in% countries),]

      new_activity <- activity.replace[ which( activity.replace$iso %in% countries),]
      activity.replace <- activity.replace[ which( activity.replace$iso %!in% countries),]

      new_activity [ paste0('X',a.extension_start_year:(start_years[i]-1)) ] <- current_replace[ match( paste(new_activity$iso, new_activity$fuel, new_activity$sector ),
                                                                                      paste(current_replace$iso, current_replace$fuel, current_replace$sector ) )  ,
                                                                                      paste0('X',a.extension_start_year:(start_years[i]-1)) ]

      new_activity [ is.na(new_activity )] <- 0

      activity.done <- rbind( activity.done, new_activity)
    }

    # combine final activity data
    final_activity <- rbind( activity.replace, activity.done )

    return(final_activity)
  }
