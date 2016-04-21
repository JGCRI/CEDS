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
if ( is.na( em ) ) em <- "SO2"

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
# 5. Replace SO2 other transformation emission

if( em == 'SO2'){
  
  MODULE_H <- "../code/module-H/"
  source_child <- function( file_name ){ source( paste( MODULE_H, file_name, sep = "" ) ) }
  source_child ('H4.2.add_emissions_SO2_other_transformation.R' )
  
  other_tranformation_emissions_calculated <- readData('MED_OUT', 'H.SO2_calculated_other_transformation_emissions')
  
  other_transformation_emissions_extended <- emissions[which(emissions$sector == "1A1bc_Other-transformation"),]
  
  # Take Max of calculated and extended Other transformation emissions
  both_estimates <- rbind.fill(other_tranformation_emissions_calculated,other_transformation_emissions_extended)[
                                          , c('iso','sector','fuel',paste0('X',1750:end_year)) ] 
  
  final_other_tranformation <- aggregate(both_estimates[paste0('X',1750:end_year)],
                                         by = list(iso = both_estimates$iso,
                                                   sector = both_estimates$sector,
                                                   fuel = both_estimates$fuel),
                                         FUN = max)
  
  emissions <- replaceValueColMatch(emissions, final_other_tranformation,
                                    x.ColName = paste0('X',1750:end_year),
                                    match.x = c('iso','sector','fuel'),
                                    addEntries = F)
  
 writeData(final_other_tranformation , 'DIAG_OUT', 'H.SO2_final_other_tranformation_emissions')
 
  }

# ---------------------------------------------------------------------------
# 6. Write to file

writeData( emissions, "MED_OUT" , paste0(em,'_total_CEDS_emissions') )


