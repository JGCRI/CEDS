# Program Name: B1.2.add_comb_SO2_EF_control_percent.R
# Author: Ryan Bolt
# Date Last Updated: 10 oct 2015 
# Program Purpose: Use default EF and Gains EF to calculate control %.
#     Linearly interpolate control% back to 1970.
# 
# Input Files: 
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
headers <- c(  "data_functions.R","analysis_functions.R",'process_db_functions.R',
              'common_data.R', 'IO_functions.R') # Additional function files may be required.
log_msg <- "Aggregating EU GAINS Data" # First message to be printed to the log
script_name <- "B1.2.add_comb_SO2_EF_gains.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 0.5 Load Packages

loadPackage('zoo')

# ---------------------------------------------------------------------------
# 1. Reading data and mapppings into script

Gains_EF  <- readData( "MED_OUT", "B.SO2_comb_EF_gains")
Default_EF <- readData( "MED_OUT", "B.SO2_comb_EF_db")

# ---------------------------------------------------------------------------
# 2. Calculate 2005 control %

names(Gains_EF) <- c('iso','sector','fuel','units','X2005')
countries <- unique(Gains_EF$iso)

# melt and combine default and gains data
default <- Default_EF[which(Default_EF$iso %in% countries), 
                      c('iso','sector','fuel','units','X2005')]
default.wide <- melt(default, c('iso','sector','fuel','units'), val='X2005')
names(default.wide)<-c('iso','sector','fuel','units','years','default')

gains.wide <- melt(Gains_EF, c('iso','sector','fuel','units'))
names(gains.wide)<-c('iso','sector','fuel','units','years','gains')
combined<-merge(gains.wide, default.wide, by=c('iso','sector','fuel','units','years'),
                all.x = TRUE, all.y=FALSE)


# Calculate Control Percent
combined$control_percent <- 1-combined$gains/combined$default
combined$control_percent[which(combined$control_percent<0)]<-0
combined <- combined[which(is.finite(combined$control_percent)),]

# Cast and reformat
control_percent <- cast(combined[,c('iso','sector','fuel','units','years','control_percent')],
                        iso+sector+fuel~years,
                        value = 'control_percent')

# ---------------------------------------------------------------------------
# 3. Interpolate Control % back to 1970

#create empty matrix to extend values back
control_percent_extended <- matrix(data=NA,nrow(control_percent),ncol=2005-1970-1)
#add 1970 and 2005 control %
control_percent_extended <- cbind(rep(x=0,times=nrow(control_percent)),
                                  control_percent_extended,
                                  control_percent$X2005)
#interpolate values
control_percent_extended <- t(control_percent_extended)
control_percent_extended <- t(na.approx(control_percent_extended))
# reformatt
control_percent_extended <- as.data.frame(control_percent_extended)
names(control_percent_extended) <- paste('X',1970:2005,sep="")

control_percent_extended <- cbind(combined[,c('iso','sector','fuel')],control_percent_extended)


# ---------------------------------------------------------------------------
# 3. Encorporate control % into default EFs
printLog("Encorporating control% into default EFs")

controlpercent_long <- melt(control_percent_extended,id=c("iso","sector","fuel"))
names(controlpercent_long) <- c('iso','sector','fuel','year','control_percent')

ef<-melt(Default_EF,id=c("iso","sector","fuel","units"))
names(ef)[which(names(ef)=='variable')]<-'year'
names(ef)[which(names(ef)=='value')]<-'ef'

ef_control_adj<-merge(ef,controlpercent_long,
                  by=c("iso","sector","fuel","year"),
                  all.x=TRUE,all.y=TRUE)

ef_control_adj$control_percent[which(is.na(ef_control_adj$control_percent))]<-0

ef_control_adj$ef_adjusted <- ef_control_adj$ef*(1-ef_control_adj$control_percent)

ef_controlled <- cast(ef_control_adj, iso + fuel + sector + units ~ year, 
                     value = 'ef_adjusted')


# -------------------------------------------------------------------------------
# 5. Output


writeData( ef_controlled, domain = "MED_OUT", fn = "B.SO2_comb_EF_db")


logStop()
# END







