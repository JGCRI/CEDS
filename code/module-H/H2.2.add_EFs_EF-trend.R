# ------------------------------------------------------------------------------
# Program Name: H2.2.add_EFs_EF-trend.R
# Author: Rachel Hoesly
# Program Purpose: Extend EFs back by EF trend
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
log_msg <- "Extending emissions factors 1960 using EF trend" # First message to be printed to the log
script_name <- "H2.2.add_EFs_EF-trend.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "OC"

# ---------------------------------------------------------------------------
# 1. Load Data

ceds_EFs <- readData( 'MED_OUT', paste0('H.',em,'_total_EFs_extended_db') )  
extension_drivers_EF<- readData("EXT_IN", 'CEDS_historical_extension_methods_EF')

# ---------------------------------------------------------------------------
# 2. Select relavent driver-methods

# Expand fuels - all-comb
expand <- extension_drivers_EF[which(extension_drivers_EF$fuel == 'all-comb' ) ,]
extension_drivers_EF <- extension_drivers_EF[which(extension_drivers_EF$fuel != 'all-comb' ) ,]
comb_fuels <- c('biomass', 'hard_coal','brown_coal','coal_coke','natural_gas','heavy_oil','diesel_oil','light_oil')
for (i in seq_along(comb_fuels)){
  expand$fuel <- rep(comb_fuels[i], times= nrow(expand) )
  extension_drivers_EF <- rbind( extension_drivers_EF, expand )
}

# select em
extension_drivers_EF <- extension_drivers_EF[ which( extension_drivers_EF$em %in% c(em , 'all' )), ]
extension_drivers_EF$em <- em

drivers <- extension_drivers_EF[ which( extension_drivers_EF$method == 'EF-trend' ) ,]

# ---------------------------------------------------------------------------
# 3. Import data files from driver-method-file

if ( nrow(drivers ) > 0){
drivers_method_files <- unique(drivers[,c("file_name","domain","domain_extention" )])
drivers_method_data_list <- list()
for(i in seq_along(drivers_method_files$file_name) ){
  if( is.na(drivers_method_files[i,"domain_extention"]) ) {
    x <- readData( domain = drivers_method_files[i,'domain'], file_name = drivers_method_files[i,"file_name"]) } else{
      x <- readData( domain = drivers_method_files[i,'domain'], file_name = drivers_method_files[i,"file_name"], domain_extension = drivers_method_files[i,"domain_extention"])}
  drivers_method_data_list[[i]] <- x
}
names(drivers_method_data_list) <- drivers_method_files$file_name
drivers_method_data_extended_list <- list()
for ( i in seq_along(drivers_method_files$file_name)) {
  drivers_method_data_sector_fuel <- drivers[which( drivers$file_name == drivers_method_files$file_name[i] ), c('sector','fuel')]
  drivers_method_data <- drivers_method_data_list[[i]]
  if( length( unique(drivers_method_data$sector) ) > 1 | length( unique(drivers_method_data$fuel) ) > 1 ) stop( 'Driver-Method data has more than one sector or fuel')
  drivers_method_data_extended <- data.frame()
  years <- names( drivers_method_data )[grep( "X",names( drivers_method_data )  )]
  for ( n in seq_along(drivers_method_data_sector_fuel$sector)){
    drivers_method_data$sector <- rep(drivers_method_data_sector_fuel$sector[n], times= nrow(drivers_method_data) )
    drivers_method_data$fuel <- rep(drivers_method_data_sector_fuel$fuel[n], times= nrow(drivers_method_data) )
    drivers_method_data <- drivers_method_data[ , c( 'iso','sector','fuel',years)]
    drivers_method_data_extended <- rbind(drivers_method_data_extended, drivers_method_data)
  }
  drivers_method_data_extended_list[[i]] <- drivers_method_data_extended
}
drivers_method_data_extended <- do.call(rbind.fill, drivers_method_data_extended_list)

#Mdaybe change later - replace NAs with zero
drivers_method_data_extended <- replace( drivers_method_data_extended, is.na(drivers_method_data_extended), 0)

} else { drivers_method_data_extended <- data.frame() }

# ---------------------------------------------------------------------------
# 4. Import files from user drop folder

user_files_list <- list.files(path =  './extention/extention-data', 
                              pattern = '*.csv')
user_files_list <- file_path_sans_ext( user_files_list )

#de select meta-data
if (length(grep(pattern = "metadata", user_files_list )) > 0)
  user_files_list <- files_list[-grep(pattern = "metadata", user_files_list )]

# select emission
user_files_list <- user_files_list[c(grep(pattern = paste0( '\\.', em ), user_files_list ),
                                     grep(pattern = paste0( '\\.', 'ALL' ), user_files_list ))]
# select 'Emission-trend'
user_files_list <- user_files_list[c(grep(pattern = 'EF-trend', user_files_list )) ]


user_data_list <- lapply ( X = user_files_list, FUN = readData, 
                           domain = "EXT_IN" , 
                           domain_extension = "extention-data/")
names(user_data_list ) <- user_files_list 

user_data <- do.call(rbind.fill, user_data_list)

# expand fuel
# Expand fuels - all-comb
expand <- user_data[which(user_data$fuel == 'all' ) ,]
user_data <- user_data[which(user_data$fuel != 'all' ) ,]
comb_fuels <- c('biomass', 'hard_coal','brown_coal','coal_coke','natural_gas','heavy_oil','diesel_oil','light_oil')
for (i in seq_along(comb_fuels)){
  expand$fuel <- rep(comb_fuels[i], times= nrow(expand) )
  user_data <- rbind( user_data, expand )
}


# ---------------------------------------------------------------------------
# 5. Transform Use EF trend to extend

EF_trends <- rbind.fill ( drivers_method_data_extended , user_data )
years <- names( EF_trends )[grep( "X",names( EF_trends )  )]
EF_trends[years] <- replace ( EF_trends[ years] , EF_trends[ years]=='NaN' , NA )

year_intervals <- unique(EF_trends[,c('start_year','end_year')])

for (i in seq_along(year_intervals$start_year)) {

  ratio_year <- as.numeric(year_intervals[i,'end_year'])+1
  if( is.na(ratio_year) ) ratio_year <- 1965
  end_year <- as.numeric(year_intervals[i,'end_year'])
  start_year <- as.numeric(year_intervals[i,'start_year'])
  
  
  
  if( !any(is.na(c(end_year,start_year))) ) { 
    
  extention_years <- paste0('X',start_year:ratio_year)  
    
  # select extension data for current method
  sectors <- EF_trends[ which( EF_trends$end_year == end_year &
                               EF_trends$start_year == start_year)  , ]
  sectors <- paste(sectors$iso,sectors$sector,sectors$fuel,sep='-')
  
  # select ceds data to extend
  ceds_extention_ratios <- ceds_EFs[ which( paste(ceds_EFs$iso,ceds_EFs$sector, ceds_EFs$fuel, sep="-") %in% sectors  ) , ]
  
  #extended data template
  ceds_extention_ratios <- ceds_extention_ratios[,c('iso','sector','fuel',paste0('X',ratio_year))]
  names(ceds_extention_ratios)[which(names(ceds_extention_ratios) == paste0('X',ratio_year))] <- 'CEDS_ratio_year'
  
  # add Driver identifyer ratio year
  ceds_extention_ratios <- merge(ceds_extention_ratios, EF_trends[,c('iso','sector','fuel', paste0('X',ratio_year))],
                                 by.x = c('iso','sector','fuel'),
                                 all.x = TRUE, all.y = FALSE)
  names(ceds_extention_ratios)[which(names(ceds_extention_ratios) == paste0('X',ratio_year))] <- 'trend_ratio_year'
  
  # calculate ratio
  ceds_extention_ratios$ratio <- ceds_extention_ratios$CEDS_ratio_year/ceds_extention_ratios$trend_ratio_year
  # make all infinite ratios zero
  ceds_extention_ratios[!is.finite(ceds_extention_ratios$ratio) , 'ratio'] <- 0
  
  # add driver data and use ratio to calculate extended value
  ceds_extended <- ceds_extention_ratios[,c('iso','fuel','sector','ratio')]
  ceds_extended[extention_years] <- NA
  ceds_extended <- replaceValueColMatch( ceds_extended, EF_trends,
                                         x.ColName = extention_years ,
                                         match.x = c('iso','fuel','sector'),
                                         addEntries = F)
  
  # calculate extended data
  ceds_extended[ extention_years ] <- ceds_extended$ratio * ceds_extended[ extention_years ]
  ceds_extended[is.na(ceds_extended)] <- 0
  
  # add to final extention template
  ceds_EFs <- replaceValueColMatch(ceds_EFs, ceds_extended,
                                   x.ColName = extention_years,
                                   match.x = c('iso','sector','fuel'),
                                   addEntries = FALSE)
  
  }
  
  if( any(is.na(c(end_year,start_year))) ) {
    extention_years <- paste0('X',1750:1964)
    
    # select extension data for current method
    sectors <- EF_trends[ which( is.na(EF_trends$start_year) )  , ]
    sectors <- paste(sectors$iso,sectors$sector,sectors$fuel,sep='-')
    
    # select ceds data to extend
    ceds_extention_ratios <- ceds_EFs[ which( paste(ceds_EFs$iso,ceds_EFs$sector, ceds_EFs$fuel, sep="-") %in% sectors  ) , ]
    
    #extended data template
    ceds_extention_ratios <- ceds_extention_ratios[,c('iso','sector','fuel',paste0('X',ratio_year))]
    names(ceds_extention_ratios)[which(names(ceds_extention_ratios) == paste0('X',ratio_year))] <- 'CEDS_ratio_year'
    
    # add Driver identifyer ratio year
    ceds_extention_ratios <- merge(ceds_extention_ratios, EF_trends[,c('iso','sector','fuel', paste0('X',ratio_year))],
                                   by.x = c('iso','sector','fuel'),
                                   all.x = TRUE, all.y = FALSE)
    names(ceds_extention_ratios)[which(names(ceds_extention_ratios) == paste0('X',ratio_year))] <- 'trend_ratio_year'
    
    # calculate ratio
    ceds_extention_ratios$ratio <- ceds_extention_ratios$CEDS_ratio_year/ceds_extention_ratios$trend_ratio_year
    # make all infinite ratios zero
    ceds_extention_ratios[!is.finite(ceds_extention_ratios$ratio) , 'ratio'] <- 0
    
    # add driver data and use ratio to calculate extended value
    ceds_extended <- ceds_extention_ratios[,c('iso','fuel','sector','ratio')]
    ceds_extended[extention_years] <- NA
    ceds_extended <- replaceValueColMatch( ceds_extended, EF_trends,
                                           x.ColName = extention_years ,
                                           match.x = c('iso','fuel','sector'),
                                           addEntries = F)
    
    # calculate extended data
    ceds_extended[ extention_years ] <- ceds_extended$ratio * ceds_extended[ extention_years ]
    ceds_extended[is.na(ceds_extended)] <- 0
    
    # add to final extention template
    ceds_EFs <- replaceValueColMatch(ceds_EFs, ceds_extended,
                                     x.ColName = extention_years,
                                     match.x = c('iso','sector','fuel'),
                                     addEntries = FALSE)
    
    
  }
}

# ---------------------------------------------------------------------------
# 4. Output

writeData( ceds_EFs, "MED_OUT" , paste0('H.',em,'_total_EFs_extended_db'))

logStop()