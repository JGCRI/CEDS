# ------------------------------------------------------------------------------
# Program Name: Compare_to_RCP.R
# Author: Rachel Hoesly
# Date Last Updated: Jan 26 2016 
# Program Purpose: Produces diagnostic summary figures of default and 
#                  scaled emissions
# Input Files: F.em_scaled_emissions
#               
# Output Files: figures in the diagnostic-output
# TODO: 
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers

# Set working directory
dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
for ( i in 1:length( dirs ) ) {
  setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
  wd <- grep( 'CEDS/input', list.dirs(), value = T )
  if ( length(wd) > 0 ) {
    setwd( wd[1] )
    break
    
  }
}
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R",'process_db_functions.R',
              'common_data.r', 'IO_functions.R', 'data_functions.R', 'timeframe_functions.R') # Additional function files may be required.
log_msg <- "Compare_to_RCP" # First message to be printed to the log
script_name <- "Compare_to_RCP.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"

# ---------------------------------------------------------------------------
# 0.5 Load Packages

library('ggplot2')
library('plyr')
library('scales')

# ---------------------------------------------------------------------------
# 0.5. Script Options

rcp_start_year <- 1970
rcp_end_year <- 2000
rcp_years <- seq(from=rcp_start_year,to=rcp_end_year,by=10)
x_rcp_years <- paste0('X',rcp_years)

# ---------------------------------------------------------------------------
# 1. Load files

Region_map <- readData( "EM_INV", domain_extension = 'RCP/',"RCP Region Mapping", ".xlsx", sheet_selection = 'EDGAR32 & IEA',
                        meta=FALSE)
Scaled_Emissions <- readData('MED_OUT', paste0('F.',em,'_scaled_emissions'))

#wrangle files in the zip
#read in RCP files
# RCP_zip_path <- paste0('./emissions-inventories/RCP/RCP_all_files.zip')
RCP_list <- paste0('regional_average_',em,'_',rcp_years,'.dat')
RCP_list_filepaths <- paste0('./emissions-inventories/RCP/RCP_all_files/',RCP_list)

# RCP <- lapply(RCP_list, FUN = unz, paste0('./emissions-inventories/RCP/RCP_all_files.zip'))
# RCP <- read.table(unz('./emissions-inventories/RCP/RCP_all_files.zip', 'regional_average_SO2_1960.dat'))

RCP_df_list <- lapply(X=RCP_list_filepaths,FUN=read.table,strip.white = TRUE,header=TRUE,skip = 4,fill=TRUE)
for (i in seq_along(rcp_years)){
  RCP_df_list[[i]]$year <- rcp_years[i]
}
RCP_df <- do.call("rbind", RCP_df_list)

setwd('../diagnostic-output')

# ---------------------------------------------------------------------------
# Process Emissions Data

# RCP data
RCP <- RCP_df
names(RCP)[which(names(RCP)== 'Tot.')] <- "Tot_Ant"
names(RCP)[which(names(RCP)== 'Ant.')] <- "Region_Name_1"
names(RCP)[which(names(RCP)== 'Region.1')] <- "Region_Name_2"

RCP$Region_Name_2 <- gsub("(Rest","",RCP$Region_Name_2,fixed=TRUE)
RCP$Region_Name <- paste(RCP$Region_Name_1,RCP$Region_Name_2)

RCP <- RCP[,c("Region_Name","ENE","IND","TRA","DOM","SLV","AGR","AWB","WST","Tot_Ant",'year')]
RCP <- RCP[which(complete.cases(RCP)),]

RCP$ENE <- as.numeric(RCP$ENE)
RCP$year <- paste0('X',RCP$year)

RCP_long <- melt(RCP, id.vars = c('Region_Name','year') )

RCP <- cast( RCP_long , Region_Name + variable ~ year)
RCP$em <- em

# 1. Prepare Data 
x_years<-paste('X',1970:2014,sep="")

ceds <- Scaled_Emissions
ceds$Region <- Region_map[match(ceds$iso,tolower(Region_map$Code)),'IMAGE24 Reg']
ceds$em <- em

ceds <- ceds[,c('Region','sector','em',x_years)]
ceds <- ceds[-which(ceds$sector == "1A3di_International-shipping" ),]

# ---------------------------------------------------------------------------
# 1.  Gloabal Comparisons

#Prime Data
global_ceds <- aggregate(ceds[x_years], 
                         by = list(total= ceds$em ),FUN=sum )
global_ceds$inv <- 'CEDS'
global_ceds_long <- melt(global_ceds, id.vars = c('total','inv'))

rcp <- RCP[which(RCP$variable == 'Tot_Ant'),]
global_rcp <- aggregate(rcp[,x_rcp_years], 
                         by = list(total= rcp$em ),FUN=sum )
global_rcp$inv <- 'RCP'
global_rcp_long <- melt(global_rcp, id.vars = c('total','inv'))

global_long <- rbind(global_ceds_long,global_rcp_long)
names(global_long) <- c('total','inv','year','total_emissions')
global_long$year <- gsub('X',"",global_long$year)
global_long$year <- as.numeric(global_long$year)

global <- rbind( global_ceds[,c('inv',x_rcp_years)],global_rcp[,c('inv',x_rcp_years)])

#writeout
writeData(global,'DIAG_OUT', paste0(em,'_Global_RCP_Comparison'),domain_extension = 'ceds-comparisons/',meta=F)

#Plot

df <- global_long[,c('inv','year','total_emissions')]
df$inv <- as.factor(df$inv)
max <- 1.2*(max(df$total_emissions))
plot <- ggplot(df, aes(x=year,y=total_emissions, color = inv)) + 
  geom_line(size=1) +
  geom_point(shape=1) +
  scale_x_continuous(breaks=c(1970,1980,1990,2000,2010))+
  scale_y_continuous(limits = c(0,max ),labels = comma)+
  ggtitle( paste('Global',em,'Emissions') )+
  labs(x='Year',y= paste(em,'Emissions [kt]') )
plot              
ggsave( paste0('ceds-comparisons/',em,'_Global_RCP_Comparison.pdf') , width = 11, height = 6)

# # ---------------------------------------------------------------------------
# # 2.  Region Comparisons
# 
# #Prime Data
# region_ceds <- aggregate(ceds[x_years], 
#                          by = list(region = ceds$Region ),FUN=sum )
# region_ceds$inv <- 'CEDS'
# region_ceds_long <- melt(region_ceds, id.vars = c('region','inv'))
# 
# rcp <- RCP[which(RCP$variable == 'Tot_Ant'),]
# region_rcp <- aggregate(rcp[,x_rcp_years], 
#                         by = list(region = rcp$Region ),FUN=sum )
# region_rcp$inv <- 'RCP'
# region_rcp_long <- melt(region_rcp, id.vars = c('region','inv'))
# 
# region_long <- rbind(region_ceds_long,region_rcp_long)
# names(region_long) <- c('region','inv','year','total_emissions')
# region_long$year <- gsub('X',"",region_long$year)
# region_long$year <- as.numeric(region_long$year)
# 
# region <- rbind( region_ceds[,c('inv','region',x_rcp_years)],region_rcp[,c('inv','region',x_rcp_years)])
# region <- region [ with( region , order( region , inv ) ), ]
# 
# #writeout
# writeData(region,'DIAG_OUT', paste0(em,'_region_RCP_Comparison'),domain_extension = 'ceds-comparisons/',meta=F)
# 
# #Plot
# 
# df <- region_long[,c('inv','year','total_emissions')]
# df$inv <- as.factor(df$inv)
# max <- 1.2*(max(df$total_emissions))
# plot <- ggplot(df, aes(x=year,y=total_emissions, color = inv)) + 
#   geom_line(size=1) +
#   geom_point(shape=1) +
#   scale_x_continuous(breaks=c(1970,1980,1990,2000,2010))+
#   scale_y_continuous(limits = c(0,max ),labels = comma)+
#   ggtitle( paste('Global',em,'Emissions') )+
#   labs(x='Year',y= paste(em,'Emissions [kt]') )
# plot              
# ggsave( paste0('ceds-comparisons/',em,'_Global_RCP_Comparison.pdf') , width = 11, height = 6)
