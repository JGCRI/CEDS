# ------------------------------------------------------------------------------
# Program Name: H2.2.add_EFs_EF-trend.R
# Author: Rachel Hoesly
# Program Purpose: Extend EFs back by EF trend
# Input Files: H.[em]_total_EFs_extended_db.csv, CEDS_historical_extension_methods_EF.csv
# Output Files: H.[em]_total_EFs_extended_db.csv
# TODO:
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R",'ModH_extension_functions.R') # Additional function files may be required.
log_msg <- "Extending emissions factors 1960 using EF trend" # First message to be printed to the log
script_name <- "H2.2.add_EFs_EF-trend.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "OC"

# ---------------------------------------------------------------------------
# 1. Load Data

ceds_EFs <- readData( 'MED_OUT', paste0('H.',em,'_total_EFs_extended_db') , meta = T )
extension_drivers_EF<- readData("EXT_IN", 'CEDS_historical_extension_methods_EF', meta = T )

# ---------------------------------------------------------------------------
# 2. Select relavent driver-methods

trend <- 'EF-trend'

drivers <-  select_EF_drivers(trend)


# ---------------------------------------------------------------------------
# 3. Import data files from driver-method-file

if ( nrow(drivers ) > 0){

# Import Driver Data from driver file
drivers_method_files <- unique(drivers[,c('start_year','end_year',"file_name","domain","domain_extension" )])
drivers_method_data_list <- list()
for(i in seq_along(drivers_method_files$file_name) ){
  if( is.na(drivers_method_files[i,"domain_extension"]) ) {
    x <- readData( domain = drivers_method_files[i,'domain'], file_name = drivers_method_files[i,"file_name"]) } else{
      x <- readData( domain = drivers_method_files[i,'domain'], file_name = drivers_method_files[i,"file_name"], domain_extension = drivers_method_files[i,"domain_extension"])}
  drivers_method_data_list[[i]] <- x
}
names(drivers_method_data_list) <- drivers_method_files$file_name


# Process driver file
drivers_method_data_extended_list <- list()
for ( i in seq_along(drivers_method_files$file_name)) {
  drivers_method_data_sector_fuel <- drivers[which( drivers$file_name == drivers_method_files$file_name[i] ), c('start_year','end_year','sector','fuel')]
  drivers_method_data <- drivers_method_data_list[[i]]
  drivers_method_data_extended <- data.frame()
  years <- names( drivers_method_data )[grep( "X",names( drivers_method_data )  )]

  # If the driver data does not have sector, fuel notation
  if( any(c('sector','fuel') %!in% names(drivers_method_data))){
  for ( n in seq_along(drivers_method_data_sector_fuel$sector)){
    drivers_method_data$sector <- rep(drivers_method_data_sector_fuel$sector[n], times= nrow(drivers_method_data) )
    drivers_method_data$fuel <- rep(drivers_method_data_sector_fuel$fuel[n], times= nrow(drivers_method_data) )
    drivers_method_data$start_year <- rep(drivers_method_data_sector_fuel$start_year[n], times= nrow(drivers_method_data) )
    drivers_method_data$end_year <- rep(drivers_method_data_sector_fuel$end_year[n], times= nrow(drivers_method_data) )
    drivers_method_data <- drivers_method_data[ , c( 'iso','sector','fuel','start_year','end_year',years)]
    drivers_method_data_extended <- rbind(drivers_method_data_extended, drivers_method_data)
  }
    } else {  # If the driver data does have sector, fuel notation
    drivers_method_data <- merge(drivers_method_data, drivers_method_data_sector_fuel)
    drivers_method_data <- drivers_method_data[ , c( 'iso','sector','fuel','start_year','end_year',years)]
    drivers_method_data_extended <- rbind(drivers_method_data_extended, drivers_method_data)
  }
  drivers_method_data_extended_list[[i]] <- drivers_method_data_extended
}

drivers_method_data_extended <- do.call(rbind.fill, drivers_method_data_extended_list)

sector_list <- unique( drivers_method_data_extended$sector )

for ( sector in sector_list ){
  temp_data <- drivers_method_data_extended[ drivers_method_data_extended$sector == sector, ]
  writeData( temp_data, 'EXT_IN', domain_extension = 'extension-data/', paste0('H.',em,'_user_defined_data_EF-trend_', sector ) )
}
}

# ---------------------------------------------------------------------------
# 4. Import files from user drop folder

user_files_list <- list.files(path =  './extension/extension-data',
                              pattern = '*.csv')
user_files_list <- tools::file_path_sans_ext( user_files_list )

#de select meta-data
if (length(grep(pattern = "metadata", user_files_list )) > 0)
  user_files_list <- user_files_list[-grep(pattern = "metadata", user_files_list )]

# select emission
user_files_list <- user_files_list[c(grep(pattern = paste0( '\\.', em, '_' ), user_files_list ),
                                     grep(pattern = paste0( '\\.', 'ALL' ), user_files_list ))]
# select 'Emission-trend'
user_files_list <- user_files_list[c(grep(pattern = 'EF-trend', user_files_list )) ]


user_data_list <- lapply ( X = user_files_list, FUN = readData,
                           domain = "EXT_IN" ,
                           domain_extension = "extension-data/")
names(user_data_list ) <- user_files_list

user_data <- do.call(rbind.fill, user_data_list)

# ---------------------------------------------------------------------------
# 5. Use trends to extend EFs

order <- data_frame(order= numeric(0),
                    start= numeric(0))
for (i in seq_along(user_data_list) ){
  order[i,] <- c(i,user_data_list[[i]]$start_year[1])
}
order <- order[order(-order$start),]
order_user_data_list <- list()
for ( i in seq_along(order$order) ){
  order_user_data_list[[i]] <- user_data_list[[order$order[i]]]
}

new_EFs <- ceds_EFs
for (i in seq_along(order_user_data_list) ){
  driver_trend <- order_user_data_list[[i]]
  start <- unique(driver_trend$start_year)
  end <-unique(driver_trend$end_year)

  new_EFs <- extend_data_on_trend (driver_trend, new_EFs, start, end)

  }


# ---------------------------------------------------------------------------
# 4. Output

writeData( new_EFs, "MED_OUT" , paste0('H.',em,'_total_EFs_extended_db'), meta = T)

logStop()
