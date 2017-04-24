# Program Name: B1.2.add_comb_default_EF.R
# Author: Rachel Hoesly
# Date Last Updated: 23 Nov 2015 
# Program Purpose: Adds default_EF data in the EF_parameters folder to the 
#                  ControlFrac_db
# Input Files:    files in the EF_parameters folder contailing default_EF and em
#               
# Output Files:   
# Notes: 
# TODO: 
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers

# Before we can load headers we need some paths defined. They may be provided by
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
headers <- c( 'process_db_functions.R','data_functions.R',
              'interpolation_extension_functions.R','common_data.R') 
#                 Additional function files may be required.
log_msg <- "Adding additional emission factors" # First message to be printed to the log
script_name <- "B1.2.add_comb_default_EF.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "CO2"
em_lc <- tolower( em )    

# ---------------------------------------------------------------------------
# 0.5 Load Packages


loadPackage('tools')

# ---------------------------------------------------------------------------
# 1. Reading data and mapppings into script

# Read in parameter files

files_list <- list.files(path =  './default-emissions-data/EF_parameters', pattern = '*.csv')
files_list <- file_path_sans_ext( files_list )

#select "default_EF"
EF_file_list <- files_list[grep(pattern = "_EF", files_list )]

#de select meta-data
EF_file_list <- EF_file_list[-grep(pattern = "metadata", EF_file_list )]

# select emission
EF_file_list <- EF_file_list[grep(pattern = paste0( '\\.', em ), EF_file_list )]

EF_list <- lapply ( X = EF_file_list, FUN = readData, 
                                 domain = "DEFAULT_EF_PARAM")
# ---------------------------------------------------------------------------
# 2. Expand "all" variable and extend over time, convert list to one df

# Expand all, interpolate and Extend forward and back
EF_extended <- lapply( X= EF_list, FUN = extendDefaultEF, 
                                    pre_ext_method_default = 'none')

EF <- do.call("rbind.fill", EF_extended)

EF$units <- 'kt/kt'
# ---------------------------------------------------------------------------
# 2. Add to existing parameter Dbs

if( length(EF_list)>0 ){
  printLog(paste('Adding new data to existing emission factor data base for', em))
  writeData( EF, 'DIAG_OUT', paste0('B.',em,'_comb_User_Added_EF'))
  addToDb_overwrite(new_data = EF, em = em, file_extension = 'comb_EF_db') 
  }else{
  printLog(paste('No data to be added to existing EF database for ', em))}

logStop()
# END