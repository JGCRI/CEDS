# Program Name: gains EMF summary
# Author: Rachel Hoesly
# Date Last Updated: 16 Dec 2015
# Program Purpose:
# Input Files:    files in the EF_parameters folder contailing control_percent and em
#
# Output Files:
# Notes:
# TODO:
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( 'process_db_functions.R','data_functions.R',
              'interpolation_extension_functions.R','common_data.R')
#                 Additional function files may be required.
log_msg <- "Adding control percent data to data base" # First message to be printed to the log
script_name <- "gains EMF summary.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"

# ---------------------------------------------------------------------------
# 1. Load Data
if ( em %in% c('SO2','BC','OC','CH4','CO2') ) {
  EMF30_ef <- readData( "DIAG_OUT" , paste0('B.',em,'_comb_EF_GAINS_EMF30'))
  }else if(em %in% c('NOx','NMVOC','CO')){
  EMF30_ef <- readData( "MED_OUT" , paste0('B.',em,'_comb_EF_GAINS_EMF30'))
}


MCL <- readData(("MAPPINGS"),'Master_Country_List')
OECD_E_Stat <- readData( "ENERGY_IN", "OECD_E_Stat", ".csv" )


# ---------------------------------------------------------------------------
#
all_ef <- EMF30_ef[,c('iso','sector','fuel','X2000','X2005','X2010')]
years <- c('X2000','X2005','X2010')
all_ef[,years] <- sapply(all_ef[,years],as.numeric)

# Denote OECD and Non OECD
OECDlist <- unique(OECD_E_Stat$COUNTRY)
all_ef <- merge (all_ef, MCL[,c('IEAName','iso')],
                 all.x = TRUE, all.y = FALSE)
all_ef <- all_ef[complete.cases(all_ef),]

OECD <- all_ef[which(all_ef$IEAName %in% OECDlist),]
nonOECD <- all_ef[which(all_ef$IEAName %!in% OECDlist),]

fuels <- unique(all_ef$fuel)

#loop analysis
for(i in seq_along(fuels)){
	out <- list()

	dfo <-OECD[which(OECD$fuel == fuels[i]),]
	summary_OECD_X2000 <- ddply( dfo,
						   .(sector), summarize,
						   OECDn = length(X2000),
						   OECDmin = min(X2000),
						   OECDmean = mean(X2000),
						   OECDmax = max(X2000),
						   year = 'X2000',
						   fuel = fuels[i])
	summary_OECD_X2005 <- ddply( dfo,
								 .(sector), summarize,
								 OECDn = length(X2005),
								 OECDmin = min(X2005),
								 OECDmean = mean(X2005),
								 OECDmax = max(X2005),
								 year = 'X2005',
								 fuel = fuels[i])
	summary_OECD_X2010 <- ddply( dfo,
								 .(sector), summarize,
								 OECDn = length(X2010),
								 OECDmin = min(X2010),
								 OECDmean = mean(X2010),
								 OECDmax = max(X2010),
								 year = 'X2010',
								 fuel = fuels[i])
	summary_OECD <- rbind (summary_OECD_X2000,summary_OECD_X2005,summary_OECD_X2010)
	#nonOECD
	dfn <-nonOECD[which(nonOECD$fuel == fuels[i]),]

	summary_nonOECD_X2000 <- ddply( dfn,
								 .(sector), summarize,
								 nonOECDn = length(X2000),
								 nonOECDmin = min(X2000),
								 nonOECDmean = mean(X2000),
								 nonOECDmax = max(X2000),
								 year = 'X2000',
								 fuel = fuels[i])
	summary_nonOECD_X2005 <- ddply( dfn,
								 .(sector), summarize,
								 nonOECDn = length(X2005),
								 nonOECDmin = min(X2005),
								 nonOECDmean = mean(X2005),
								 nonOECDmax = max(X2005),
								 year = 'X2005',
								 fuel = fuels[i])
	summary_nonOECD_X2010 <- ddply( dfn,
								 .(sector), summarize,
								 nonOECDn = length(X2010),
								 nonOECDmin = min(X2010),
								 nonOECDmean = mean(X2010),
								 nonOECDmax = max(X2010),
								 year = 'X2010',
								 fuel = fuels[i])
	summary_nonOECD <- rbind (summary_nonOECD_X2000,summary_nonOECD_X2005,summary_nonOECD_X2010)

	out[[i]] <- merge(summary_OECD,summary_nonOECD)

	out <- do.call(rbind, out)
	writeData( out, "DIAG_OUT", paste0('GAINS-EF-summary/',em ,'_summary_',fuels[i]) )
}
