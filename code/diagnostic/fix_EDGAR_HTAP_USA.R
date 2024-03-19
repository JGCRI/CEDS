# ------------------------------------------------------------------------------
# Program Name: fix_EDGAR_HTAP_USA.R
# Author: Harrison Suchyta
# Date Last Updated: April 26, 2021
# Program Purpose: to fix up the EDGAR_HTAP files so that they can be compared
#                   to CEDS
# Input Files: edgar_HTAPv3_BETA_[year]
# Output Files: EDGAR_HTAP_USA

#load in EDGAR_HTAP_USA files
modE_dir <- paste0('../emissions-inventories/EDGAR_HTAP_USA')
#files <-list.files(modE_dir)

#set the directory to where all the files are located
setwd(modE_dir)

# Read in data and merge into a dataframe
EDGAR_HTAP_data <- read.csv(file='HTAP_emissions_summary_2002-2017.csv')

colnames(EDGAR_HTAP_data) <- c("Year","Month","Species","Sector","Cumulative_emissions","Country")

EDGAR_new <- gsub(",", "", EDGAR_HTAP_data$Cumulative_emissions)

EDGAR_HTAP_data$Cumulative_emissions <- as.numeric(as.character( EDGAR_new ))

#EDGAR_USA <- summarise_at(group_by(EDGAR_HTAP_data,Year),vars(Cumulative_emissions),funs(sum(.,na.rm=TRUE)))
EDGAR_HTAP_data %>% group_by(Year,Species) %>% summarize_if(is.numeric,sum,na.rm = TRUE) -> EDGAR_HTAP_long

#Edgar_HTAP_USA_data %>% group_by(Year,Species,Country) %>% summarize_if(is.numeric,sum,na.rm = TRUE) %>% filter(Species == em) -> EDGAR_HTAP_USA_long





