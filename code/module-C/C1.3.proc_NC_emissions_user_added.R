#------------------------------------------------------------------------------
# Program Name: C1.3.proc_NC_emissions_user_added.R
# Author(s): Rachel Hoesly
# Date Last Modified: August 19, 2015
# Program Purpose: To fill out missing sections in the process emissions database
# Input Files: C.[em]_NC_emissions.csv
# Output Files:  C.[em]_NC_emissions.csv
# Notes:
# TODO:
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "process_db_functions.R",
              'interpolation_extension_functions.R' ) # Additional function files required.
log_msg <- "Integration of process emissions data" # First message to be printed to the log
script_name <- "C1.3.proc_NC_emissions_user_added.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "NOx"

# ---------------------------------------------------------------------------
# 1. Reading data and mapppings into script

MSL <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Sectors" )

# Read in parameter files
files <- list.files(path = './default-emissions-data/non-combustion-emissions',
                    pattern = '*.csv')
files <- tools::file_path_sans_ext( files )

#de select meta-data
if (length(grep(pattern = "metadata", files )) > 0)
      files <- files[-grep(pattern = "metadata", files )]

# select emission
files <- files[grep(pattern = paste0( '\\.', em, '_' ), files )]

emissions_list <- lapply ( X = files, FUN = readData,
                    domain = "DEFAULT_EF_IN" ,
                    domain_extension = "non-combustion-emissions/")

# ---------------------------------------------------------------------------
# 2. Interpolate, select process-fuel, convert list to one df
process_sectors <- MSL[which(MSL$type == 'NC'),'sector']

process_emissions <- function( e_data ){
  combustion_sectors <- c()
  # sort the years
  names <- names( e_data )
  id <- names[-grep('X',names)]
  years <- names[grep('X',names)]
  years <- years[order(years)]
  e_data <- e_data[,c( id, years )]

  e_data <- interpolateValues(e_data)

  combustion_sectors <- e_data[ which( e_data$sector %!in% process_sectors ) , ]
  if (nrow(combustion_sectors) > 0 ) {
    printLog(paste('Combustion sectors added inventory data to process emissions.',
                   'Combustion data printed to diagnostic-output'))

    writeData(combustion_sectors, domain = 'DIAG_OUT',
              paste0('C.',em,'combustion_data_added_to_process_emissions')) }

  e_data <- e_data[ which( e_data$sector %in% process_sectors ) , ]
  return(e_data)
}

# Expand all, interpolate and Extend forward and back
if ( length(emissions_list) > 0 & any( sapply( FUN=nrow, X=emissions_list) > 0 ) ){

  emissions_extended <- lapply( X= emissions_list, FUN = process_emissions)
  emissions <- do.call(rbind.fill, emissions_extended)
}

if ( !exists( "emissions" ) ) emissions <- data.frame( iso = character(0),
                                                     sector = character(0),
                                                     fuel = character(0),
                                                     units = character(0),
                                                     X1960 = numeric(0))
# write out to diagnostic
 writeData(emissions, 'DEFAULT_EF_IN', domain_extension = 'non-combustion-emissions/',
            paste0('C.',em,'_NC_emissions_user_added'),
            meta= F)

# ---------------------------------------------------------------------------
# 2. Add to existing parameter Dbs

if( nrow(emissions)>0 ){
  printLog(paste('Adding new data to process emissions for', em))
  addToDb_overwrite(new_data = emissions, em = em, module = 'C',file_extension = 'NC_emissions')
}else{
  printLog(paste('No data to be added to existing emissions database for ', em))}

logStop()
# END
