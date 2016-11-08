# ------------------------------------------------------------------------------
# Program Name: E.CDIAC_emissions.R
# Author(s): Rachel Hoesly, Linh Vu
# Date Last Updated: 15 Sept 2016
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
  
  # Function to process individual USGS cement production tab
  procUSGS <- function( df ) {
    df$Country <- gsub( "\\s+$", "", df$Country )  # trim trailing whitespace
    df$Country <- gsub("[^a-zA-Z ]", "", df$Country )  # remove non-characters
    names( df ) <- make.names( names( df ) )
    e_years <- grepl( "^X.*e$", names( df ) )
    names( df )[ e_years ] <- gsub( "e", "", names( df )[ e_years ] )
    df <- filter( df, !is.na( Country ) )
    df$iso <- usgs_ctry_map$iso[ match( df$Country, usgs_ctry_map$Country ) ]
    X_years <- names( df )[ grepl( "X", names( df ) ) ]
    df[, X_years ] <- lapply( df[, X_years], function(x) return( as.numeric( as.character( x ) ) ) )
    return( df )
  }

# -----------------------------------------------------------------------------------------------------------
# 1. Read in files
  cdiac_read <- readData('EM_INV', domain_extension = "CDIAC/",  'CDIAC_national_1751_2011'  ) 
  MCL <- readData( "MAPPINGS", "Master_Country_List" )
  cdiac_country_map <- readData('EM_INV', domain_extension = "CDIAC/",  'CDIAC_country_map'  ) 
  un_pop <- readData( "MED_OUT" , 'A.UN_pop_master' )

  usgs_sheets <- c( "2002", "2005", "2007", "2010", "2011", "2013", "2016" )
  usgs_data_in <- readData( "ACTIVITY_IN", "USGS_Commodity_Summaries_Cement_Production", ".xlsx", sheet_selection = usgs_sheets )
  usgs_ctry_map <- readData( "ACTIVITY_IN", "USGS_Commodity_Summaries_Cement_Production", ".xlsx", sheet_selection = "mapping" )
  
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
                                FUN = sum, na.rm = T)
  
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
  cdiac_start_year <- 1750
  
  X_cdiac_years <- paste0('X',cdiac_start_year:cdiac_end_year)
  
# -----------------------------------------------------------------------------------------------------------
# 4. Split Countries
  # FSU
    cdiac_ussr_corrected <- disaggregate_country( original_data = cdiac_corrected,
                                                      id_cols = c('iso','fuel'),
                                                      trend_data = population,
                                                      trend_match_cols = 'iso',
                                                      combined_iso = 'USSR',
                                                      dis_end_year = 1991,
                                                      disaggregate_iso = c('aze','arm' , 'blr','est','geo','kaz','kgz','lva',
                                                                            'ltu','mda','tjk','tkm','ukr','uzb', 'rus'),
                                                      write_over_values = T)

   #  Yugoslavia
  cdiac_yug_corrected  <- disaggregate_country(original_data = cdiac_ussr_corrected,
                                               id_cols = c('iso','fuel'),
                                               trend_data = population,
                                               trend_match_cols = 'iso',
                                              combined_iso = 'yug',
                                              dis_end_year = 1991,
                                              disaggregate_iso = c('bih','hrv','mkd','svn', 'scg'),
                                              allow_dropped_data = T)
  # Serbia and Montenegro
  cdiac_scg_corrected  <- disaggregate_country(original_data = cdiac_yug_corrected,
                                               id_cols = c('iso','fuel'),
                                               trend_data = population,
                                               trend_match_cols = 'iso',
                                          combined_iso = 'scg',
                                          dis_end_year = 2005,
                                          disaggregate_iso = c('srb','mne'))
  # Czechoslovakia
    cdiac_csk_corrected  <- disaggregate_country(original_data = cdiac_scg_corrected,
                                                 id_cols = c('iso','fuel'),
                                                 trend_data = population,
                                                 trend_match_cols = 'iso',
                                           combined_iso = 'csk',
                                           dis_end_year = 1991,
                                           disaggregate_iso = c('cze','svk') ) 
  # East and West Pakistan
  cdiac_pak_corrected  <- disaggregate_country(original_data = cdiac_csk_corrected,
                                               id_cols = c('iso','fuel'),
                                               trend_data = population,
                                               trend_match_cols = 'iso',
                                                combined_iso = 'EAST_WEST_PAKISTAN',
                                                dis_end_year = 1971,
                                                disaggregate_iso = c('pak','bgd') ) 
  # United Korea
  cdiac_kor_corrected  <- disaggregate_country(original_data = cdiac_pak_corrected,
                                               id_cols = c('iso','fuel'),
                                               trend_data = population,
                                               trend_match_cols = 'iso',
                                                combined_iso = 'UNITED_KOREA',
                                                dis_end_year = 1944,
                                                ratio_start_year = 1948,
                                                disaggregate_iso = c('prk','kor') ) 
  # French Equatorial Africa
  cdiac_FrEqAf_corrected  <- disaggregate_country(original_data = cdiac_kor_corrected,
                                                  id_cols = c('iso','fuel'),
                                                  trend_data = population,
                                                  trend_match_cols = 'iso',
                                                combined_iso = 'FRENCH_EQUATORIAL_AFRICA',
                                                dis_end_year = 1958,
                                                disaggregate_iso = c('caf','cog','gab','tcd')  ,
                                                allow_dropped_data = T) 

  # French West Africa
  cdiac_FrWeAf_corrected  <- disaggregate_country(original_data = cdiac_FrEqAf_corrected,
                                                  id_cols = c('iso','fuel'),
                                                  trend_data = population,
                                                  trend_match_cols = 'iso',
                                                  combined_iso = 'FRENCH_WEST_AFRICA',
                                                  method = 2,
                                                  dis_end_year = 1957,
                                                  disaggregate_iso = c('mrt','sen','mli','gin','civ','bfa','ben','ner') ) 
  #Rwanda-Urundi
  cdiac_RU_corrected  <- disaggregate_country(original_data = cdiac_FrWeAf_corrected,
                                              id_cols = c('iso','fuel'),
                                              trend_data = population,
                                              trend_match_cols = 'iso',
                                           combined_iso = 'RWANDA-URUNDI',
                                           dis_end_year = 1961,
                                           disaggregate_iso = c('rwa','bdi') ,
                                           allow_dropped_data = T) 
 # Netherland Antiliies and Aruba 
  cdiac_NAR_corrected <- disaggregate_country(original_data = cdiac_RU_corrected,
                                              id_cols = c('iso','fuel'),
                                              trend_data = population,
                                              trend_match_cols = 'iso',
                                             combined_iso = 'NETHERLAND_ANTILLES_AND_ARUBA',
                                             dis_end_year = 1985,
                                             dis_start_year = 1926,
                                             disaggregate_iso = c('ant','abw'))
 # Netherland Antillies 
  cdiac_NA_corrected <- disaggregate_country(original_data = cdiac_NAR_corrected,
                                             id_cols = c('iso','fuel'),
                                             trend_data = population,
                                             trend_match_cols = 'iso',
                                            combined_iso = 'ant',
                                            dis_end_year = 2011,
                                            ratio_range_length = 2,
                                            disaggregate_iso = c('cuw','sxm') )
 # Rhodesia Nyasaland
  cdiac_RN_corrected <- disaggregate_country(original_data = cdiac_NA_corrected,
                                             id_cols = c('iso','fuel'),
                                             trend_data = population,
                                             trend_match_cols = 'iso',
                                            combined_iso = 'RHODESIA-NYASALAND',
                                            dis_end_year = 1963,
                                            disaggregate_iso = c('zmb','mwi'),
                                            allow_dropped_data = T)
 # Leeward Islands 
  cdiac_LI_corrected <- disaggregate_country(original_data = cdiac_RN_corrected,
                                             id_cols = c('iso','fuel'),
                                             trend_data = population,
                                             trend_match_cols = 'iso',
                                            combined_iso = 'LEEWARD ISLANDS',
                                            dis_end_year = 1956,
                                            dis_start_year = 1950,
                                            disaggregate_iso = c('kna','atg'),
                                            allow_dropped_data = T)
  
  cdiac_split_final <- cdiac_LI_corrected
  
# -----------------------------------------------------------------------------------------------------------
# 5. Add zero values for nations too small for cdiac
  # define countries to add to cdiac data
  non_cdaic_countries <- MCL$iso[MCL$iso %!in% unique(cdiac_split_final$iso)]
  non_cdaic_countries <-   non_cdaic_countries[  non_cdaic_countries %in% MCL[which(MCL$final_data_flag == 1),'iso']]
  non_cdaic_countries <-   non_cdaic_countries[   non_cdaic_countries %!in% 'global']
  
  add_zeros <- data.frame( iso = rep(non_cdaic_countries, each = length(cdiac_fuels)),
                           fuel = rep(cdiac_fuels, times = length(non_cdaic_countries)),
                           units = 'kt-C'  )
  add_zeros[X_cdiac_years] <- 0
  
  
  cdiac_disaggregated <- rbind.fill(add_zeros,cdiac_split_final)
  cdiac_disaggregated$units <- 'kt-C'
  cdiac_disaggregated <- arrange_(cdiac_disaggregated, c('iso','fuel','units',X_cdiac_years))
  
# -----------------------------------------------------------------------------------------------------------
# 6. Corrections
  cdiac_smooth <- cdiac_disaggregated 
  
  # CORRECTIONS-1950 discontinuity, linear interpolate between 1952 and last zero value
  # make non zeros NA
  cdiac_smooth[which( cdiac_smooth$iso %in% c('abw','arg','bhr','cuw','tto','irn','ven','brn','kwt') &
                       cdiac_smooth$fuel == 'liquid_fuels') , paste0('X',historical_pre_extension_year:1951)] <- 
    replace ( cdiac_smooth[which( cdiac_smooth$iso %in% c('abw','arg','bhr','cuw','tto','irn','ven','brn','kwt') &
                     cdiac_smooth$fuel == 'liquid_fuels') , paste0('X',historical_pre_extension_year:1951)],
              (cdiac_smooth[which( cdiac_smooth$iso %in% c('abw','arg','bhr','cuw','tto','irn','ven','brn','kwt') &
                                        cdiac_smooth$fuel == 'liquid_fuels') , paste0('X',historical_pre_extension_year:1951)] ) != 0 , 
               NA)
  cdiac_smooth[which(is.na(cdiac_smooth$X1750)),'X1750'] <- 0

   # Various corrections
  cdiac_smooth[which( cdiac_smooth$iso == 'kwt'), paste0('X',1960:1969)] <- NA
  cdiac_smooth[which( cdiac_smooth$iso == 'sau'), paste0('X',1947)] <- NA
  cdiac_smooth[which( cdiac_smooth$iso == 'irn'), paste0('X',1953:1954)] <- NA  
  cdiac_smooth[which( cdiac_smooth$iso == 'irq'), paste0('X',1949:1955)] <- NA 
  cdiac_smooth[which( cdiac_smooth$iso == 'mex'), paste0('X',1912:1938)] <- NA
  
  cdiac_smooth[X_cdiac_years] <- interpolate_NAs(cdiac_smooth[, X_cdiac_years])
  
  cdiac_smooth[which( cdiac_smooth$iso == 'abw'),] <- replace(cdiac_smooth[which( cdiac_smooth$iso == 'abw'),], 
                          is.na(cdiac_smooth[which( cdiac_smooth$iso == 'abw'),]),
                          0)
  # -----------------------------------------------------------------------------------------------------------
  # 7. Recalcuate total CO2 after corrections
  cdiac_final <- cdiac_smooth
  
  cdiac_final <- cdiac_final[ which( cdiac_final$fuel %!in% 'Total_CO2'),]
  cdiac_sum <- cdiac_final[ which( cdiac_final$fuel %in% c("solid_fuels","liquid_fuels","gas_fuels","cement_production",  
                                                           "gas_flaring", "bunker_fuels")),]
  total_CO2 <- aggregate( cdiac_final[X_cdiac_years],
                          by = list(iso = cdiac_final$iso),
                          FUN = sum)
  total_CO2$fuel <- 'Total_CO2'
  
  cdiac_final <- rbind.fill(cdiac_final, total_CO2)
  
# -----------------------------------------------------------------------------------------------------------
# 8. Add entry for "liquid and gas fuels" 
  cdiac_liquid_and_gas <- cdiac_final[which(cdiac_final$fuel %in% c( 'liquid_fuels','gas_fuels')),]
  cdiac_liquid_and_gas <- aggregate( cdiac_liquid_and_gas[X_cdiac_years],
                                     by = list(iso = cdiac_liquid_and_gas$iso),
                                     FUN = sum)
  
  cdiac_liquid_and_gas$fuel <- 'liquid_and_gas_fuels'
  cdiac_liquid_and_gas$units <- 'kt-C'
  
  cdiac_liquid_and_gas <- cdiac_liquid_and_gas[ ,c('iso','fuel','units', X_cdiac_years)]
  
  cdiac_final <- rbind(cdiac_final,cdiac_liquid_and_gas)
  
  # sort and organize
  cdiac_final <- cdiac_final[ ,c('iso','fuel',  X_cdiac_years)]
  cdiac_final <- cdiac_final[ with( cdiac_final, order( iso, fuel ) ), ]

# -----------------------------------------------------------------------------------------------------------
# 9. Extend CDIAC cement emissions using USGS cement production data 
# # Routine to make USGS country mapping
# MCL <- readData( "MAPPINGS", "Master_Country_List" )
# df <- do.call( "rbind.fill", usgs_data_in )
# df$Country <- gsub( "\\s+$", "", df$Country )  # trim trailing whitespace
# df$Country <- gsub("[^a-zA-Z ]", "", df$Country )  # remove non-characters
# df <- df["Country"] %>% unique() %>% arrange( Country )
# df$iso <- MCL$iso[ match( df$Country, MCL$Country_Name ) ]  # try to match iso
# df$iso2 <- MCL$iso[ match( df$Country, paste0( MCL$Country_Name, "e" ) ) ] 
# df$iso[ is.na( df$iso ) ] <- df$iso2[ is.na( df$iso ) ]
# df$iso2 <- NULL
# for ( i in 2:nrow( df ) ) {
#   if ( is.na( df$iso[[ i ]] ) & grepl( df$Country[[ i-1 ]], df$Country[[ i ]] ) )
#     df$iso[[ i ]] <- df$iso[[ i-1 ]]
# }
# df$iso[ is.na( df$iso ) ] <- ""
# writeData( df, "DIAG_OUT", "E.USGS_country_mapping", meta=F )
  
# Combine all tabs into one df
# Make list of all iso and unmatched countries
  usgs <- lapply( usgs_data_in, procUSGS )
  all_countries <- lapply( usgs, function( df ) return( df[ c( "iso", "Country" ) ] ) ) 
  all_countries <- do.call( "rbind", all_countries ) %>% unique()
  all_iso <- filter( all_countries, !is.na( iso ) ) %>% select( iso ) %>% unique()
  all_iso <- sort( all_iso$iso )
  unmatched_countries <- filter( all_countries, is.na( iso ) ) %>% select( Country ) %>% unique()
  unmatched_countries <- sort( unmatched_countries$Country )
  if ( length( unmatched_countries ) > 0 ) {
    unmatched_countries <- paste0("\"", unmatched_countries, "\"" )
    warning( paste0( "The following countries were unmatched. Please update mapping:\n",
                     paste( unmatched_countries, collapse=", ") ) )
  }
  
# Make template df with all iso and years
  all_years <- lapply( usgs, function( df ) return( names( df )[ grepl( "X", names( df ) ) ] ) )
  all_years <- Reduce( c, all_years ) %>% unique() %>% sort()
  cement <- merge( data.frame( iso=all_iso ), data.frame( year=all_years ), all=T )
  cement$value <- NA
  
# Add data to template
  for (i in seq_along( usgs ) ) {
    df <- usgs[[ i ]]
    df$Country <- NULL
    df <- melt( df, id="iso" ) %>% filter( !is.na( value ) )
    names( df )[ names( df ) == "variable" ] <- "year"
    names( df )[ names( df ) == "value" ] <- "value_new"
    cement <- merge( cement, df, all.x = T )
    cement$value[ is.na( cement$value ) ] <- 
      cement$value_new[ is.na( cement$value ) ]
    cement$value_new <- NULL
  }
  cement$units <- "kt"
  cement <- cast( cement, iso+units ~ year)
  Xyears <- names( cement )[ grepl( "X", names( cement ) ) ]
  cement <- cement[ rowSums( is.na( cement[, ] ) ) < length( Xyears ), ]  # drop rows of all NA
  
# Give all scg cement before 2005 to srb
  cement[ cement$iso == "srb", paste0( "X", 1998:2005 ) ] <- 
    cement[ cement$iso == "scg", paste0( "X", 1998:2005 ) ]
  cement <- filter( cement, iso != "scg" )
  
# Make all NA 2013 production zero (sgp only)
  cement$X2013[ is.na( cement$X2013 ) ] <- 0
  
# Disaggregate 2014/2015 production based on 2013 shares
  shares <- filter( cement, is.na( X2014 ) ) %>%
    select( iso, X2013 )
  shares$ratio <- shares$X2013 / sum( shares$X2013 )
  
  cement_other_X2014_X2015 <- filter( cement, iso == "OTHER" ) %>%
    select( X2014, X2015 ) %>%
    merge( select( shares, iso, ratio ), all = T ) %>%
    mutate( X2014 = X2014*ratio, X2015 = X2015*ratio )
  
  cement_other <- filter( cement, is.na( X2014 ) ) %>%
    select( -X2014, -X2015 ) %>%
    merge( select( cement_other_X2014_X2015, -ratio ) )
  
  cement_all <- filter( cement, !is.na( X2014 ), iso != "OTHER" ) %>%
    rbind( cement_other ) %>% arrange( iso )
  
# Interpolate. Make NA zero
  cement_all[, Xyears] <- interpolate_NAs( cement_all[, Xyears ] )
  cement_all[ is.na( cement_all ) ] <- 0  

# Calculate EFs over time using USGS cement production  
  X_cement_years <- paste0( "X", 1998:2011 )
  X_cement_ext_years <- paste0( "X", 2012:2015 )
  X_cdiac_years_ext <- paste0('X',cdiac_start_year:cdiac_end_year_cement)
  
  cdiac_ext <- filter( cdiac_final, fuel == "cement_production", iso %in% cement_all$iso ) %>%
    arrange( iso )
  cement_prod <- filter( cement_all, iso %in% cdiac_ext$iso ) %>% arrange( iso )
  cement_ef <- cdiac_ext
  cement_ef[, X_cement_years ] <- cdiac_ext[, X_cement_years ] / cement_prod[, X_cement_years ]
  
# Extend cement emissions to 2015 using production and 2009-2011 average EFs
  cement_ef$avg <- rowMeans( cement_ef[ paste0( "X", 2009:2011 ) ], na.rm = T )
  cdiac_ext[, X_cement_ext_years ] <- cement_prod[, X_cement_ext_years ] * cement_ef$avg
  cement_ef_diag <- cement_ef[ c( "iso", "fuel", X_cement_years ) ]
  
# Combine extended data back
  cdiac_final <- filter( cdiac_final, paste( iso, fuel ) %!in%
                           paste( cdiac_ext$iso, cdiac_ext$fuel ) ) %>%
    bind_rows( cdiac_ext ) %>% arrange( iso, fuel ) %>%
    data.frame()

# NA to zero
  cdiac_final[ is.na( cdiac_final ) ] <- 0
  
# -----------------------------------------------------------------------------------------------------------
# 10. Summary  
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
  
  # cdiac_solid cumulative
  cdiac_solid_fuel_cumulative <- melt( cdiac_solid_fuel, id = c( "iso", "fuel" ) )
  cdiac_solid_fuel_cumulative <- arrange( cdiac_solid_fuel_cumulative, iso, fuel, variable ) %>%
    ddply( .(iso, fuel), function( df ){
      df$value <- cumsum( df$value )
      return( df )
    })
  cdiac_solid_fuel_cumulative <- cast( cdiac_solid_fuel_cumulative )
  cdiac_solid_fuel_cumulative$fuel <- "solid_fuels_cumulative"
  
  # cdiac global total for each categories 
  cdiac_fuel_cats <- unique( cdiac_final$fuel ) 
  cdiac_cats_total <- sapply( cdiac_fuel_cats, function( cdiac_fuel_cat ) {
    temp_cat_data <- subset( cdiac_final, cdiac_final$fuel == cdiac_fuel_cat )
    temp_cat_agg <- aggregate( temp_cat_data[ , X_cdiac_years_ext ], by = list( temp_cat_data$fuel ), FUN = sum )
    } )
  cdiac_cats_total <- t( cdiac_cats_total )
  cdiac_cats_total <- as.data.frame( cdiac_cats_total, row.names = F ) 
  colnames( cdiac_cats_total )[ 1 ] <- 'fuel'
  cdiac_cats_total$iso <- 'global'
  cdiac_cats_total <- cdiac_cats_total[ , c( 'iso', 'fuel', X_cdiac_years_ext ) ]  
  # add the totals back to cdiac_final
  cdiac_final <- rbind( cdiac_final, cdiac_cats_total ) 
  cdiac_final <- cdiac_final[ order( cdiac_final$iso ), ]

  
    
# -----------------------------------------------------------------------------------------------------------
# 11. Output
 
  writeData(cdiac_final, domain = "MED_OUT", fn = paste0( "E.CO2_CDIAC_inventory" ), meta = T )
  writeData(cdiac_cement, domain = "MED_OUT", fn = paste0( "E.CO2_CDIAC_Cement" ), meta = T )
  writeData(cdiac_total, domain = "MED_OUT", fn = paste0( "E.CO2_CDIAC_Total_CO2" ), meta = T )
  writeData(cdiac_liquid_and_gas, domain = "MED_OUT", fn = paste0( "E.CO2_CDIAC_liquid_and_gas" ), meta = T )
  writeData(cdiac_solid_fuel, domain = "MED_OUT", fn = paste0( "E.CO2_CDIAC_solid_fuel" ), meta = T )
  writeData(cdiac_solid_fuel_cumulative, domain = "MED_OUT", fn = paste0( "E.CO2_CDIAC_solid_fuel_cumulative" ), meta = T )

  writeData(cdiac_region_fuel, domain = "DIAG_OUT", fn = "E.CO2_CDIAC_by_figure_region_CDIAC_fuel", meta = TRUE )
  writeData(cdiac_iso_fuel, domain = "DIAG_OUT", fn = "E.CO2_CDIAC_by_iso_CDIAC_fuel", meta = TRUE )
  writeData(cement_ef_diag, "DIAG_OUT", "E.CDIAC_cement_EF" )
  writeData(cement_all, "DIAG_OUT", "E.USGS_cement_production" )
 
  logStop()
    
# END