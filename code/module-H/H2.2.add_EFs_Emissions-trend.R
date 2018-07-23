# ------------------------------------------------------------------------------
# Program Name: H2.2.add_EFs_Emissions-trend.R
# Author: Rachel Hoesly
# Program Purpose: Create base database to extend EFs backward
#
# Output Files:
# TODO:
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R",'ModH_extension_functions.R') # Additional function files may be required.
log_msg <- "Creating database for CEDS EFs extension before 1960" # First message to be printed to the log
script_name <- "H2.2.add_EFs_Emissions-trend.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"

# ---------------------------------------------------------------------------
# 1. Load Data

activity <- readData("MED_OUT", 'A.total_activity_extended', meta = T)
extension_drivers_EF <- readData("EXT_IN", 'CEDS_historical_extension_methods_EF', meta = T )
MCL <- readData("MAPPINGS",'Master_Country_List', meta = T)
# read in population data used for later extension of drivers_method_data_list
pop <- readData("MED_OUT", "A.UN_pop_master", meta = T)

final_iso <- unique(MCL[which(MCL$final_data_flag == 1),'iso'])

# ---------------------------------------------------------------------------
# 2. Select relavent driver-methods

trend <- 'Emissions-trend'

drivers <-  select_EF_drivers(trend)

# ---------------------------------------------------------------------------
# 3. Import data files from driver-method-file

# import files
drivers_method_files <- unique(drivers[,c("file_name","domain","domain_extension" )])
drivers_method_data_list <- list()
for(i in seq_along(drivers_method_files$file_name) ){
  if( is.na(drivers_method_files[i,"domain_extension"]) ) {
      x <- readData( domain = drivers_method_files[i,'domain'], file_name = drivers_method_files[i,"file_name"]) } else{
      x <- readData( domain = drivers_method_files[i,'domain'], file_name = drivers_method_files[i,"file_name"], domain_extension = drivers_method_files[i,"domain_extension"])}
      drivers_method_data_list[[i]] <- x
    }
names(drivers_method_data_list) <- drivers_method_files$file_name

# # The U.N_manure data only contains data from 1860 to 2004
# # codes below extends the drivers_method_data_list$U.N_manure data back to 1750 using population data

# # for non_N emissions there would not be U.N_manure data
# if ( 'U.N_manure' %in% names(drivers_method_data_list ) ) {

# # first, preprocess the population data
# pop$year <- paste0( 'pop_', pop$year )
# pop_wide <- cast( pop, iso ~ year, value = 'pop', fun.aggregate = sum )
# pop_wide <- pop_wide[ , c( 'iso', paste0( 'pop_', as.character( 1750 : 2014 ) ) ) ]

# # extract drivers_method_data_list$U.N_manure as manure
# manure <- drivers_method_data_list$U.N_manure
# # merge manure and pop_wide to get the comman isos
# pop_manure <- merge( manure, pop_wide, by = c( 'iso' ), all.x = T )

# # make unmatched country's population into 0
# pop_manure[ is.na( pop_manure ) ] <- 0

# # 1750 - 1859 pop data
# ext_years <- as.character( 1750 : 1859 )
# pop_ext_years <- pop_manure[ , c( paste0( 'pop_', ext_years ) ) ]
# # 1860 manure data
# manure_1860 <- matrix( rep( pop_manure$X1860, ncol( pop_ext_years ) ), ncol = ncol( pop_ext_years ) )
# # 1860 pop_data
# pop_1860 <- matrix( rep( pop_manure$pop_1860, ncol( pop_ext_years ) ), ncol = ncol( pop_ext_years ) )

# # compute ext_year manure values
# manure_ext_year <- manure_1860 * pop_ext_years / pop_1860
# # replace NAs with ) generated during calculation
# manure_ext_year[ is.na( manure_ext_year ) ] <- 0

# # make manure_ext_year into proper layout and adding data after 1860
# last_year_in_manure <- as.numeric( substr( tail( names( manure ), n = 1 ), 2, nchar( tail( names( manure ), n = 1 ) ) ) )
# manure_extended <- cbind( pop_manure[ , c( 'iso', 'units' ) ],
                          # manure_ext_year,
                          # pop_manure[ , paste0( 'X', as.character( 1860 : last_year_in_manure ) ) ] )

# colnames( manure_extended ) <- c( 'iso', 'units', paste0( 'X', as.character( 1750 : last_year_in_manure ) ) )
# drivers_method_data_list$U.N_manure <- manure_extended
# # end of U.N_manure fix
# }

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
# user_files_list <- list.files(path =  './extension/extension-data',
#                          pattern = '*.csv')
# user_files_list <- tools::file_path_sans_ext( user_files_list )
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
#                            domain_extension = "extension-data/")
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
EF_trends[years] <- replace ( EF_trends[ years] , EF_trends[ years]=='NaN' , 0 )

writeData( EF_trends, domain = "EXT_IN" , domain_extension = "extension-data/",
           fn = paste0('H.',em,'_',
                       drivers[i,'sector'],'-',drivers[i,'fuel'],'-',drivers[i,'file_name'],
                       '_EF-trend'), meta = F)

}
# ---------------------------------------------------------------------------
# 4. End Script



logStop()
