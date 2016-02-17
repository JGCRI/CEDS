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

# Before we can load headers we need some paths defined. They may be provided by
#   a system environment variable or may have already been set in the workspace.
# Set variable PARAM_DIR to be the data system directory
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
headers <- c( "data_functions.R", "process_db_functions.R", 
              'interpolation_extention_functions.R' ) # Additional function files required.
log_msg <- "Integration of process emissions data" # First message to be printed to the log
script_name <- "C1.3.proc_NC_emissions_user_added.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"
em_lc <- tolower( em )

# ------------------------------------------------------------------------------
# 0.5 Load Packages

loadPackage('tools')

# ---------------------------------------------------------------------------
# 1. Reading data and mapppings into script

# Read in parameter files

files_list <- list.files(path =  './default_emissions_data/Non_Combustion_emissions', pattern = '*.csv')
files_list <- file_path_sans_ext( files_list )

#de select meta-data
if (length(grep(pattern = "metadata", files_list )) > 0)
      files_list <- files_list[!grep(pattern = "metadata", files_list )]

# select emission
files_list <- files_list[grep(pattern = em, files_list )]

emissions_list <- lapply ( X = files_list, FUN = readData, 
                    domain = "DEFAULT_EF_IN" , 
                    domain_extension = "Non_Combustion_emissions/")
# ---------------------------------------------------------------------------
# 2. Interpolate, convert list to one df

# Expand all, interpolate and Extend forward and back
emissions_extended <- lapply( X= emissions_list, FUN = interpolateValues)

emissions <- do.call("rbind.fill", emissions_extended)

if ( is.null( emissions ) ) emissions <- data.frame( iso = character(0),
                                                     sector = character(0),
                                                     fuel = character(0),
                                                     units = character(0),
                                                     X1960 = numeric(0))


# write out to diagnostic
writeData(emissions, 'MED_OUT', paste0('C.',em,'_NC_emissions_user_added'))
# ---------------------------------------------------------------------------
# 2. Add to existing parameter Dbs

if( nrow(emissions)>0 ){
  printLog(paste('Adding new data to process emissions for', em))
  addToDb_overwrite(new_data = emissions, em = em, module = 'C',file_extention = 'NC_emissions') 
}else{
  printLog(paste('No data to be added to existing emissions database for ', em))}

logStop()
# END
