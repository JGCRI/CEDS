# ------------------------------------------------------------------------------
# Program Name: fix_EDGAR_HTAP_files.R
# Author: Harrison Suchyta
# Date Last Updated: April 26, 2021
# Program Purpose: to fix up the EDGAR_HTAP files so that they can be compared
#                   to CEDS
# Input Files: edgar_HTAPv3_BETA_[year]
# Output Files: EDGAR_HTAP_[em]
#-------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if( "input" %in% dir( ) ) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", 'common_data.R', 'IO_functions.R' ) # Additional function files may be required.
log_msg <- "Comparing invenotry emissions to CEDS, GAINS, EDGAR, and REAS..." # First message to be printed to the log
script_name <- "fix_EDGAR_HTAP_files.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "NOx"
#-------------------------------------------------------------------------------
library(dplyr)

# Load in original HTAP files
modE_dir <- paste0('C:/users/such559/Documents/CEDS-Dev/input/emissions-inventories/EDGAR_HTAP')
files <-list.files(modE_dir)

#set the directory to where all the files are located
setwd("../input/emissions-inventories/EDGAR_HTAP")

# Read in data and merge into a dataframe
EDGAR_HTAP_data <-lapply(files,read.csv)
EDGAR_HTAP_cumulative = Reduce(function(...) merge(..., all=T), EDGAR_HTAP_data)

EDGAR_split <- sapply(EDGAR_HTAP_cumulative, gsub, pattern=";", replacement=",")
EDGAR_HTAP <- read.table(text=paste(EDGAR_split, collapse='\n'), header = FALSE, stringsAsFactors = FALSE, sep=',')
colnames(EDGAR_HTAP) <- c("Data_provider","Substance","Year","Country","Sector","January","February","March","April","May","June","July","August","September","October","November","December")

EDGAR_yearly %>% mutate(Cumulative_emissions = select(., January:December) %>% rowSums(na.rm = TRUE))



