# ------------------------------------------------------------------------------
# Program Name: B1.2.add_SO2_comb_EF_sulfur_standards.R
# Author: Rachel Hoesly
# Date Last Updated: Oct 1 2015 
# Program Purpose: Add Sulfur Standards to default sulfur EF
# values from 2005 back to 1975
# 
# Input Files: 
#               
# Output Files: 
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
headers <- c( "data_functions.R", "analysis_functions.R",'process_db_functions.R',
              'common_data.r', 'IO_functions.R', 'data_functions.R', 'timeframe_functions.R') # Additional function files may be required.
log_msg <- "Adding sulfur standards to diesel SO2 EF" # First message to be printed to the log
script_name <- "B1.2.add_SO2_comb_EF_sulfur_standards.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 0.5 Load Packages

loadPackage('zoo')

# ---------------------------------------------------------------------------
# 1. Reading data

  diesel_standards <- readData( "DEFAULT_EF_IN",  "Diesel_transport_S_trend" , '.xlsx')
  MCL <- readData( "MAPPINGS", "Master_Country_List" )

# -------------------------------------------------------------------------------
# 2. Format to CEDS format

#grab sheet with ppm data
  diesel_standards_ppm <- diesel_standards[['ppm']]
  regions<-diesel_standards_ppm[,1:2]

#select and rename columns
  col.names<-names(diesel_standards_ppm)
  years<-col.names[which(is.na(as.numeric(col.names))==FALSE)]
  start_year<-years[1]
  X_years<-paste('X',years,sep="")
  diesel_standards_ppm<-diesel_standards_ppm[,c('iso',years)] 
  names(diesel_standards_ppm)<-c('iso',X_years)

#only seclect through 2015
  X_standard_years<-paste("X", start_year:2015 ,sep="")
  
  diesel_standards_ppm <- diesel_standards_ppm[,c('iso', 
                                                  X_standard_years)]
  
  diesel_standards_ppm <- diesel_standards_ppm[-which(is.na(diesel_standards_ppm$iso)),]
# -------------------------------------------------------------------------------
# 3. Fill in default and interpolate between data estimates
  
  # default starting standard = 8000ppm
  diesel_standards_ppm[which(is.na(diesel_standards_ppm$X1970)),'X1970']<-8000
  
  # extend most recent standard through 2015
  diesel_standard_extend <-  t(diesel_standards_ppm[,-1])
  for (i in seq_along(diesel_standard_extend[1,]) ){
  x<-diesel_standard_extend[,i]
  lx<-length(x)
  xtrim<-na.trim(x)
  lxtrim<-length(xtrim)
  if(lx > lxtrim) diesel_standard_extend[,i]<-c(xtrim,rep(x[lxtrim],time=lx-lxtrim))
  }
  
  # fill in NA. constant 
  diesel_standards_fill<-na.approx(diesel_standard_extend,method='constant')
  diesel_standards_ppm_filled <-diesel_standards_ppm
  diesel_standards_ppm_filled[,-1]<-t(diesel_standards_fill)

# -------------------------------------------------------------------------------
# 3. Fill in regional average for countries with no data

  iso.region.average<-setdiff(unique(MCL$iso),diesel_standards_ppm_filled$iso)
  
  diesel_standards_ppm_all<-merge(diesel_standards_ppm_filled,unique(MCL[,c('iso','Region')]),
                                   all=TRUE)
  diesel_standards_region_tofill<-diesel_standards_ppm_all[which(diesel_standards_ppm_all$iso %in% iso.region.average),]
  
  diesel_standards_region_wide<-melt(diesel_standards_ppm_filled, id.vars='iso')
  
  region_average_long<-ddply(diesel_standards_region_wide,.(iso,variable),summarize,
        average=mean(value))
  
  region_average_wide<-cast(region_average_long,iso~variable,value='average')
  region_average_wide<-merge(MCL[,c('iso','Region')],region_average_wide)
  
  diesel_standards_region_filled<-diesel_standards_region_tofill
  diesel_standards_region_filled[,X_standard_years]<-  region_average_wide[match(diesel_standards_region_filled$Region,region_average_wide$Region),X_standard_years]
  
  diesel_standards_ppm_complete<-rbind(diesel_standards_region_filled[,c('iso',X_standard_years)],diesel_standards_ppm_filled)
  
# -------------------------------------------------------------------------------
# 3. Calculate EF, fill in sectors and fuel
  
  diesel_EF<-diesel_standards_ppm_complete
  diesel_EF[,X_standard_years]<-diesel_EF[,X_standard_years]/10^6/2
  
  diesel_EF$fuel<-'diesel_oil'
  diesel_EF$sector<-'transp_road'
  diesel_EF$units<-'kt/kt'


# -------------------------------------------------------------------------------
# 5. Output

diesel_EF<-diesel_EF[ with( diesel_EF, order( iso, sector, fuel,units ) ), c('iso','fuel','sector','units',X_standard_years)]
  
printLog("Adding diesel sulfer standard SO2 EFs to B.SO2_comb_EF_db")
addToEFDb_overwrite(diesel_EF,em='SO2',type='comb')



logStop()
# END



