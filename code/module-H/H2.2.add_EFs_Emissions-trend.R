# ------------------------------------------------------------------------------
# Program Name: H2.2.add_EFs_Emissions-trend.R
# Author: Rachel Hoesly
# Program Purpose: Create base database to extend EFs backward
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
log_msg <- "Creating database for CEDS EFs extention before 1960" # First message to be printed to the log
script_name <- "H2.2.add_EFs_Emissions-trend.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "OC"

# ---------------------------------------------------------------------------
# 1. Load Data

activity <- readData("MED_OUT", paste0('H.',em,'_total_activity_extended') )
extension_drivers_EF<- readData("EXT_IN", 'CEDS_historical_extension_methods_EF')
MCL <- readData("MAPPINGS",'Master_Country_List')

final_iso <- unique(MCL[which(MCL$final_data_flag == 1),'iso'])

# ---------------------------------------------------------------------------
# 2. Select relavent driver-methods

trend <- 'Emissions-trend'

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
  em_instruction <- unique( paste( driver_em$sector,driver_em$fuel,driver_em$start_year,driver_em$end_year  ,sep = '-'))
  extension_drivers_EF <- extension_drivers_EF[ which( 
    paste( extension_drivers_EF$sector, extension_drivers_EF$fuel , extension_drivers_EF$start_year, extension_drivers_EF$end_year, extension_drivers_EF$em, sep = '-') %!in%  
      paste( em_instruction ,'all' ,sep = '-') ), ]
}

# select em
extension_drivers_EF$em <- em

# select method
extension_drivers_EF <- extension_drivers_EF[ which( extension_drivers_EF$method == trend ) ,]

drivers <- extension_drivers_EF

# ---------------------------------------------------------------------------
# 3. Import data files from driver-method-file


drivers_method_files <- unique(drivers[,c("file_name",'start_year','end_year',"domain","domain_extention" )])
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
  drivers_method_data_sector_fuel <- drivers[which( drivers$file_name == drivers_method_files$file_name[i] ), c('sector','fuel','start_year','end_year')]
  drivers_method_data <- drivers_method_data_list[[i]]
  if( length( unique(drivers_method_data$sector) ) > 1 | length( unique(drivers_method_data$fuel) ) > 1 ) stop( 'Driver-Method data has more than one sector or fuel')
  drivers_method_data_extended <- data.frame()
  years <- names( drivers_method_data )[grep( "X",names( drivers_method_data )  )]
  for ( n in seq_along(drivers_method_data_sector_fuel$sector)){
    drivers_method_data$sector <- rep(drivers_method_data_sector_fuel$sector[n], times= nrow(drivers_method_data) )
    drivers_method_data$fuel <- rep(drivers_method_data_sector_fuel$fuel[n], times= nrow(drivers_method_data) )
    drivers_method_data$start_year <- rep(drivers_method_data_sector_fuel$start_year[n], times= nrow(drivers_method_data) )
    drivers_method_data$end_year <- rep(drivers_method_data_sector_fuel$end_year[n], times= nrow(drivers_method_data) )

    drivers_method_data <- drivers_method_data[ , c( 'iso','sector','fuel','start_year','end_year',years)]
    drivers_method_data_extended <- rbind(drivers_method_data_extended, drivers_method_data)
  }
  drivers_method_data_extended_list[[i]] <- drivers_method_data_extended
  }
drivers_method_data_extended <- do.call(rbind.fill, drivers_method_data_extended_list)

#Maybe change later - replace NAs with zero
drivers_method_data_extended <- replace( drivers_method_data_extended, is.na(drivers_method_data_extended), 0)

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
user_files_list <- user_files_list[c(grep(pattern = 'Emissions-trend', user_files_list )) ]


user_data_list <- lapply ( X = user_files_list, FUN = readData, 
                           domain = "EXT_IN" , 
                           domain_extension = "extention-data/")
names(user_data_list ) <- user_files_list 

user_data <- do.call(rbind.fill, user_data_list)
if( length(user_data_list) == 0) user_data <- data.frame( iso = character(0), 
                                                  sector = character(0))

# expand fuel
# Expand fuels - all-comb
if( nrow(user_data) > 0 ){
expand <- user_data[which(user_data$fuel == 'all' ) ,]
user_data <- user_data[which(user_data$fuel != 'all' ) ,]
comb_fuels <- c('biomass', 'hard_coal','brown_coal','coal_coke','natural_gas','heavy_oil','diesel_oil','light_oil')
for (i in seq_along(comb_fuels)){
  expand$fuel <- rep(x = comb_fuels[i], times= nrow(expand) )
  user_data <- rbind( user_data, expand )
}
}

# ---------------------------------------------------------------------------
# 5. Transform Emissions Trend into EF trend

emissions_trends <- rbind.fill(drivers_method_data_extended, user_data)
emissions_trends <- emissions_trends[which(emissions_trends$iso %in% final_iso),]
years <- names( emissions_trends )[grep( "X",names( emissions_trends )  )]

activity_trends <- emissions_trends[,c('iso','sector','fuel','start_year','end_year')]
activity_trends[ c(years,'start_year','end_year') ] <- NA

activity_trends <- replaceValueColMatch(activity_trends, activity,
                                        x.ColName = years,
                                        match.x = c('iso','sector','fuel'),
                                        addEntries = FALSE)

if( !identical(emissions_trends[,c('iso','sector','fuel')], activity_trends[,c('iso','sector','fuel')]))
  stop('Extendted emissions trends and activity trends are not identical, cannot calculate extended EF trend.')

EF_trends <- emissions_trends[,c('iso','sector','fuel','start_year','end_year')]
EF_trends[years] <- emissions_trends[years]/activity_trends[ years ]
EF_trends[years] <- replace ( EF_trends[ years] , EF_trends[ years]=='NaN' , NA ) 

# ---------------------------------------------------------------------------
# 4. Output

writeData( EF_trends, domain = "EXT_IN" , domain_extension = "extention-data/", 
           fn = paste0('H.',em,'_extended_Emissions_trends_EFs_EF-trend'), meta = F)

logStop()
