# ------------------------------------------------------------------------------
# Program Name: fix_EDGAR_HTAP_files.R
# Author: Harrison Suchyta
# Date Last Updated: April 26, 2021
# Program Purpose: to fix up the EDGAR_HTAP files so that they can be compared
#                   to CEDS
# Input Files: edgar_HTAPv3_BETA_[year]
# Output Files: EDGAR_HTAP_[em]
#-------------------------------------------------------------------------------
setwd('C:/users/such559/Documents/CEDS-Dev')
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
setwd(modE_dir)

# Read in data and merge into a dataframe
EDGAR_HTAP_data <-lapply(files,read.csv)
EDGAR_HTAP_cumulative = Reduce(function(...) merge(..., all=T), EDGAR_HTAP_data)

#Replace all the semicolons in the files to commas in order to use csv commands
EDGAR_split <- sapply(EDGAR_HTAP_cumulative, gsub, pattern=";", replacement=",")

#read the csv values into EDGAR_HTAP_read and provide the column names
EDGAR_HTAP_read <- read.table(text=paste(EDGAR_split, collapse='\n'), header = FALSE, stringsAsFactors = FALSE, sep=',')
colnames(EDGAR_HTAP_read) <- c("Data_provider","Substance","Year","Country","Sector","January","February","March","April","May","June","July","August","September","October","November","December")

#sum the monthly values into one cumulative yearly value and then delete the monthly values
EDGAR_HTAP_read %>% mutate(Cumulative_emissions = select(., January:December) %>% rowSums(na.rm = TRUE)) -> EDGAR_HTAP
EDGAR_HTAP <- subset(EDGAR_HTAP,select = -c(January,February,March,April,May,June,July,August,September,October,November,December))

#Split the matrix into separate dataframes for each emission
Split_based_on_emission <- split(EDGAR_HTAP, f=EDGAR_HTAP$Substance)

#move the data frame information into separate csvs for each pollutant
write.csv(Split_based_on_emission[[1]], "C:/users/such559/Documents/CEDS-Dev/input/emissions-inventories/EDGAR_HTAP/EDGAR_HTAP_BC.csv",row.names=FALSE)
write.csv(Split_based_on_emission[[2]], "C:/users/such559/Documents/CEDS-Dev/input/emissions-inventories/EDGAR_HTAP/EDGAR_HTAP_CO.csv",row.names=FALSE)
write.csv(Split_based_on_emission[[3]], "C:/users/such559/Documents/CEDS-Dev/input/emissions-inventories/EDGAR_HTAP/EDGAR_HTAP_NH3.csv",row.names=FALSE)
write.csv(Split_based_on_emission[[4]], "C:/users/such559/Documents/CEDS-Dev/input/emissions-inventories/EDGAR_HTAP/EDGAR_HTAP_NMVOC.csv",row.names=FALSE)
write.csv(Split_based_on_emission[[5]], "C:/users/such559/Documents/CEDS-Dev/input/emissions-inventories/EDGAR_HTAP/EDGAR_HTAP_NOx.csv",row.names=FALSE)
write.csv(Split_based_on_emission[[6]], "C:/users/such559/Documents/CEDS-Dev/input/emissions-inventories/EDGAR_HTAP/EDGAR_HTAP_OC.csv",row.names=FALSE)
write.csv(Split_based_on_emission[[7]], "C:/users/such559/Documents/CEDS-Dev/input/emissions-inventories/EDGAR_HTAP/EDGAR_HTAP_PM10.csv",row.names=FALSE)
write.csv(Split_based_on_emission[[8]], "C:/users/such559/Documents/CEDS-Dev/input/emissions-inventories/EDGAR_HTAP/EDGAR_HTAP_PM2.5.csv",row.names=FALSE)
write.csv(Split_based_on_emission[[9]], "C:/users/such559/Documents/CEDS-Dev/input/emissions-inventories/EDGAR_HTAP/EDGAR_HTAP_SO2.csv",row.names=FALSE)
