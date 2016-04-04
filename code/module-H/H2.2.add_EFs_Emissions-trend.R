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
headers <- c( "data_functions.R",'ModH_extention_functions.R') # Additional function files may be required.
log_msg <- "Creating database for CEDS EFs extention before 1960" # First message to be printed to the log
script_name <- "H2.2.add_EFs_Emissions-trend.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"

# ---------------------------------------------------------------------------
# 1. Load Data

activity <- readData("MED_OUT", paste0('H.',em,'_total_activity_extended') )
extension_drivers_EF<- readData("EXT_IN", 'CEDS_historical_extension_methods_EF')
MCL <- readData("MAPPINGS",'Master_Country_List')

final_iso <- unique(MCL[which(MCL$final_data_flag == 1),'iso'])

# ---------------------------------------------------------------------------
# 2. Select relavent driver-methods

trend <- 'Emissions-trend'

drivers <-  select_EF_drivers(trend)

# ---------------------------------------------------------------------------
# 3. Import data files from driver-method-file

# import files
drivers_method_files <- unique(drivers[,c("file_name","domain","domain_extention" )])
drivers_method_data_list <- list()
for(i in seq_along(drivers_method_files$file_name) ){
  if( is.na(drivers_method_files[i,"domain_extention"]) ) {
      x <- readData( domain = drivers_method_files[i,'domain'], file_name = drivers_method_files[i,"file_name"]) } else{
      x <- readData( domain = drivers_method_files[i,'domain'], file_name = drivers_method_files[i,"file_name"], domain_extension = drivers_method_files[i,"domain_extention"])}
      drivers_method_data_list[[i]] <- x
    }
names(drivers_method_data_list) <- drivers_method_files$file_name

# All Driver Data
drivers_method_data_extended_list <- list()
for ( i in seq_along(drivers$file_name)) {

  driver_data <- drivers_method_data_list[[ which(names(drivers_method_data_list) == drivers[i, 'file_name',]) ]]
  driver_data$sector <- drivers[i,'sector']
  driver_data$fuel <-drivers[i,'fuel']
  driver_data$start_year <-drivers[i,'start_year']
  driver_data$end_year <-drivers[i,'end_year']
  driver_data <- driver_data[,c('iso','sector','fuel','start_year','end_year',paste0('X',drivers[i,'start_year']:(drivers[i,'end_year'] + 5)))]
  drivers_method_data_extended_list[[i]] <- driver_data
  }
names( drivers_method_data_extended_list ) <- drivers$file_name
# # ---------------------------------------------------------------------------
# # 4. Import files from user drop folder
# 
# user_files_list <- list.files(path =  './extention/extention-data', 
#                          pattern = '*.csv')
# user_files_list <- file_path_sans_ext( user_files_list )
# 
# #de select meta-data
# if (length(grep(pattern = "metadata", user_files_list )) > 0)
#   user_files_list <- files_list[-grep(pattern = "metadata", user_files_list )]
# 
# # select emission
# user_files_list <- user_files_list[c(grep(pattern = paste0( '\\.', em ), user_files_list ),
#                                      grep(pattern = paste0( '\\.', 'ALL' ), user_files_list ))]
# # select 'Emission-trend'
# user_files_list <- user_files_list[c(grep(pattern = 'Emissions-trend', user_files_list )) ]
# 
# 
# user_data_list <- lapply ( X = user_files_list, FUN = readData, 
#                            domain = "EXT_IN" , 
#                            domain_extension = "extention-data/")
# names(user_data_list ) <- user_files_list 
# 
# user_data <- do.call(rbind.fill, user_data_list)
# if( length(user_data_list) == 0) user_data <- data.frame( iso = character(0), 
#                                                   sector = character(0))
# 
# # expand fuel
# # Expand fuels - all-comb
# if( nrow(user_data) > 0 ){
# expand <- user_data[which(user_data$fuel == 'all' ) ,]
# user_data <- user_data[which(user_data$fuel != 'all' ) ,]
# comb_fuels <- c('biomass', 'hard_coal','brown_coal','coal_coke','natural_gas','heavy_oil','diesel_oil','light_oil')
# for (i in seq_along(comb_fuels)){
#   expand$fuel <- rep(x = comb_fuels[i], times= nrow(expand) )
#   user_data <- rbind( user_data, expand )
# }
# }
# 
# if ( length(user_data > 0)) emissions_trend_list <- c( emissions_trend_list, user_data) 

# ---------------------------------------------------------------------------
# 5. Transform Emissions Trend into EF trend and write 

emissions_trend_list <- drivers_method_data_extended_list

for ( i in seq_along(emissions_trend_list) ) {

emissions_trends <- emissions_trend_list[[i]]
emissions_trends <- emissions_trends[which(emissions_trends$iso %in% final_iso),]
years <- names( emissions_trends )[grep( "X",names( emissions_trends )  )]
emissions_trends <- emissions_trends[ , c('iso','sector','fuel','start_year','end_year',years[order(years)])]

activity_trends <- emissions_trends
activity_trends[ years ] <- NA

activity_trends <- replaceValueColMatch(activity_trends, activity,
                                        x.ColName = years,
                                        match.x = c('iso','sector','fuel'),
                                        addEntries = FALSE)

if( !identical(emissions_trends[,c('iso','sector','fuel')], activity_trends[,c('iso','sector','fuel')]))
  stop('Extendted emissions trends and activity trends are not identical, cannot calculate extended EF trend.')

EF_trends <- emissions_trends
EF_trends[years] <- emissions_trends[years]/activity_trends[ years ]
EF_trends[years] <- replace ( EF_trends[ years] , EF_trends[ years]=='NaN' , NA ) 

writeData( EF_trends, domain = "EXT_IN" , domain_extension = "extention-data/", 
           fn = paste0('H.',em,'_',
                       drivers[i,'sector'],'-',drivers[i,'fuel'],'-',drivers[i,'file_name'],
                       '_EF-trend'), meta = F)

}
# ---------------------------------------------------------------------------
# 4. End Script



logStop()
