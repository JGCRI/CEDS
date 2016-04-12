#------------------------------------------------------------------------------
# Program Name: H4.1.proc_Extended_Emissions.R
# Author: Rachel Hoesly
# Date Last Updated: March 22, 2016
# Program Purpose: Process extendtion EFs database to finalize and sort CEDS EFs database.
# Input Files: None
# Output Files: None
# Notes: 
# TODO: 
# ------------------------------------------------------------------------------------
# Before we can run other scripts we need some paths defined. They may be provided by
#   a system environment variable or may have already been set in the workspace.
dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
for ( i in 1:length( dirs ) ) {
  setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
  wd <- grep( 'CEDS/input', list.dirs(), value = T )
  if ( length( wd ) > 0 ) {
    setwd( wd[ 1 ] )
    break
    
  }
}

PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
headers <- c('data_functions.R') # Additional function files required.
log_msg <- paste0( "Processing CEDS extention EFs database" ) # First message to be printed to the log
script_name <- "H4.1.proc_Extended_Emissions.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "BC"

# ------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# 1. Load files

EFs <- readData( 'MED_OUT',paste0('H.',em,'_total_EFs_extended_adjusted-sector') )
activity <- readData( 'MED_OUT',paste0('H.',em,'_total_activity_extended') )

# ---------------------------------------------------------------------------
# 2. Sort

EFs <- EFs[ with( EFs, order( iso, sector, fuel ) ), ]
activity <- activity[ with( activity, order( iso, sector, fuel ) ), ]

check_iso <- identical( activity$iso, EFs$iso)
check_sector <- identical( activity$sector, EFs$sector)
check_fuel <- identical( activity$fuel, EFs$fuel)

if( !all( c(check_iso, check_sector, check_fuel) )) stop("Activity and EFs databases do not match.")

emissions <- EFs[,c('iso','sector','fuel')]
emissions$units <- 'kt'
emissions[X_extended_years] <- EFs[X_extended_years] * activity[X_extended_years]

# ---------------------------------------------------------------------------
# 5. Write to file

writeData( emissions, "MED_OUT" , paste0(em,'_total_CEDS_emissions') )


