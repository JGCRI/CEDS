# ------------------------------------------------------------------------------
# Program Name: H2.2.add_EFs_default.R.R
# Author: Rachel Hoesly
# Program Purpose: Extend Efs - or replace extended EFs - with default EFs
# Input Files: H.[em]_total_EFs_extended_db.csv, D.[em]_default_total_EF.csv
#               CEDS_historical_extension_methods_EF.csv
# Output Files: H.[em]_total_EFs_extended_db'
# TODO:
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R",'ModH_extension_functions.R') # Additional function files may be required.
log_msg <- "Replace selected EFs with defaults" # First message to be printed to the log
script_name <- "H2.2.add_EFs_default.R.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "OC"

# ---------------------------------------------------------------------------
# 1. Load Data

ceds_EFs <- readData( 'MED_OUT', paste0('H.',em,'_total_EFs_extended_db') )
default_EFs <- readData( 'MED_OUT' , paste0('D.',em,'_default_total_EF') )
extension_drivers_EF<- readData("EXT_IN", 'CEDS_historical_extension_methods_EF' )

# expand default_EFs
default_EFs [ paste0('X',1750:1959)] <- default_EFs[ 'X1960']

# ---------------------------------------------------------------------------
# 2. Expand and select drivers

trend <- 'default'

extension_drivers_EF <- select_EF_drivers(trend)

  if ( nrow( extension_drivers_EF) > 0 ) {

  drivers <- extension_drivers_EF

  drivers <- drivers[ , c('sector','fuel','em','start_year','end_year')]
# ---------------------------------------------------------------------------
# 3. Revert to Default EFs

year_intervals <- unique(drivers[,c('start_year','end_year')])

if (length(year_intervals) > 0 ){
for (i in seq_along(year_intervals$start_year)) {
 interval_driver <- drivers[which( drivers$start_year == year_intervals[i,'start_year'] &
                                     drivers$end_year == year_intervals[i,'end_year'] ),]

  defaults <- default_EFs[ which( paste( default_EFs$sector, default_EFs$fuel ,sep = '-') %in%
                                    paste( interval_driver $sector, interval_driver $fuel ,sep = '-')) ,
                           c('iso','sector','fuel',paste0('X',year_intervals[i,"start_year"]:year_intervals[i,"end_year"]) )]

  # add to final extension template
  ceds_EFs <- replaceValueColMatch(ceds_EFs, defaults,
                                   x.ColName = paste0('X',year_intervals[i,"start_year"]:year_intervals[i,"end_year"]),
                                   match.x = c('iso','sector','fuel'),
                                   addEntries = FALSE)

}

# ---------------------------------------------------------------------------
# 4. Output

writeData( ceds_EFs, "MED_OUT" , paste0('H.',em,'_total_EFs_extended_db'))
}
}

logStop()
