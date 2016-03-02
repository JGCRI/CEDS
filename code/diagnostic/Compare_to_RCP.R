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
library('gridExtra')

# ---------------------------------------------------------------------------
# 0.5. Script Options

rcp_start_year <- 1970
rcp_end_year <- 2000
rcp_years <- seq(from=rcp_start_year,to=rcp_end_year,by=10)
x_rcp_years <- paste0('X',rcp_years)

# ---------------------------------------------------------------------------
# 1. Load files

Map_region_codes <- readData( "EM_INV", domain_extension = 'RCP/',"RCP Region Mapping", ".xlsx", sheet_selection = 'Reg Codes',
                        meta=FALSE)
Map_iso_codes <- readData( "EM_INV", domain_extension = 'RCP/',"RCP Region Mapping", ".xlsx", sheet_selection = 'EDGAR32 & IEA',
                        meta=FALSE)
Map_sector <- readData( "EM_INV", domain_extension = 'RCP/',"RCP_CEDS_sector_map",
                        meta=FALSE)


Master_Country_List <- readData('MAPPINGS', 'Master_Country_List')
Scaled_Emissions <- readData('MED_OUT', paste0('F.',em,'_scaled_emissions'))

# ---------------------------------------------------------------------------
# 1. Load and process RCP files

# set wd to REAS folder  
setwd( './emissions-inventories/RCP')

# create temporary folder to extract zipped files
zipfile_path <- paste0('./',em,'.zip')
dir.name <- paste0('./',em,'_RCP_temp_folder')
dir.create(dir.name)
# unzip files to temp folder  
unzip(zipfile_path, exdir = dir.name)

# list files in the folder
files <- list.files(paste0(dir.name,'/',em)  ,pattern = '.dat')
files <- paste0(dir.name,'/',em,'/',files)

rcp_files <- list()
for (i in seq_along(rcp_years)){
  rcp_files[i] <- files[grep(rcp_years[i], files)] 
}
rcp_files <- unlist(rcp_files)

RCP_df_list <- lapply(X=rcp_files,FUN=read.table,strip.white = TRUE,header=TRUE,skip = 4,fill=TRUE, stringsAsFactors = FALSE)

for (i in seq_along(rcp_years)){
  RCP_df_list[[i]]$year <- rcp_years[i]
}
RCP_df <- do.call("rbind", RCP_df_list)

# delete temp folder
unlink(dir.name,recursive = TRUE)

setwd('../')
setwd('../')
setwd('../diagnostic-output')
# ---------------------------------------------------------------------------
# 2. Process RCP Emissions Data

RCP <- RCP_df
names(RCP)[which(names(RCP)== 'Tot.')] <- "Tot_Ant"
names(RCP)[which(names(RCP)== 'Ant.')] <- "Region_Name_1"
names(RCP)[which(names(RCP)== 'Region.1')] <- "Region_Name_2"

RCP$Region_Name_2 <- gsub("(Rest","",RCP$Region_Name_2,fixed=TRUE)
RCP$Region_Name <- paste(RCP$Region_Name_1,RCP$Region_Name_2)

RCP <- RCP[,c('Region','Subregion',"Region_Name","ENE","IND","TRA","DOM","SLV","AGR","AWB","WST","Tot_Ant",'year')]

RCP <- RCP[which(complete.cases(RCP)),]

RCP$ENE <- as.numeric(RCP$ENE)
RCP$year <- paste0('X',RCP$year)

RCP_long <- melt(RCP, id.vars = c('Region','Subregion','Region_Name','year'))

RCP <- cast( RCP_long , Region + Subregion + Region_Name + variable ~ year)
RCP$em <- em
names(RCP)[which(names(RCP) == 'Region')] <- 'Region_code'
names(RCP)[which(names(RCP) == 'Subregion')] <- 'Subregion_code'
names(RCP)[which(names(RCP) == 'Region_Name')] <- 'Region'
names(RCP)[which(names(RCP) == 'variable')] <- 'Sector'

RCP[grep('Stan',RCP$Region),'Region'] <- "Asia-Stan"

RCP$Region <- gsub(" $","", RCP$Region, perl=T)
# ---------------------------------------------------------------------------
# 1. Process CEDS Emissions Data 
x_years<-paste('X',1970:2014,sep="")

ceds <- Scaled_Emissions
ceds$em <- em

# remove internation shipping, and aviation for comparison with RCP
ceds <- ceds[-which(ceds$sector %in% 
                      c("1A3ai_International-aviation","1A3di_International-shipping",'1A3aii_Domestic-aviation' )),]


# Create complete region map for ceds to RCP
complete_region_map <- merge(Map_iso_codes, Map_region_codes,
                             by.x= "RCP Template Reg #",
                             by.y=, 'RCP Template Reg Code')
complete_region_map$Region <- gsub(" [(]Rest of[)]","",complete_region_map$Region)
complete_region_map$Region <- gsub(" [(]Estonia, Latvia, Lithuania[)]","",complete_region_map$Region)
complete_region_map$Region <- gsub(" [(]Republic of Korea[)]","",complete_region_map$Region)
complete_region_map$Region <- gsub(" [(]Democratic People's Republic of Korea[)]","",complete_region_map$Region)
complete_region_map[which(complete_region_map$Code == 'GRL'),'Region'] <- 'Greenland'
complete_region_map$Region <- gsub(" $","", complete_region_map$Region, perl=T)

# Create complete sector map
sector_map <- Map_sector[complete.cases(Map_sector[,c('CEDS','RCP')]),c('CEDS','RCP')]

# add region to ceds data
ceds$Region <- complete_region_map[match(ceds$iso,tolower(complete_region_map$Code)),'Region']
ceds[which(is.na(ceds$Region)),'Region']<- 'Not Mapped'
ceds <- ceds[,c('Region','sector','em',x_years)]

# add sector to ceds data
ceds$Sector <- sector_map[match(ceds$sector,sector_map$CEDS),'RCP']
ceds <- ceds[,c('Region','Sector','em',x_years)]
ceds <- ceds[which(!is.na(ceds$Sector)),]

# ---------------------------------------------------------------------------
# 3.  Gloabal Comparisons

#Prime Data
global_ceds <- aggregate(ceds[x_years], 
                         by = list(total= ceds$em ),FUN=sum )
global_ceds$inv <- 'CEDS'
global_ceds_long <- melt(global_ceds, id.vars = c('total','inv'))

rcp <- RCP[which(RCP$Sector == 'Tot_Ant'),]
if(em == 'OC'){
rcp_awb <- RCP[which(RCP$Sector == 'AWB'),]
rcp[,x_rcp_years] <- rcp[,x_rcp_years] - rcp_awb[,x_rcp_years]
rcp[,x_rcp_years] <- rcp[,x_rcp_years]/1.4
}

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
  geom_point(shape=19) +
  geom_line(data = subset(df, inv=='CEDS'),size=1,aes(x=year,y=total_emissions, color = inv)) +
  scale_x_continuous(breaks=c(1970,1980,1990,2000,2010))+
  scale_y_continuous(limits = c(0,max ),labels = comma)+
  ggtitle( paste('Global',em,'Emissions') )+
  labs(x='Year',y= paste(em,'Emissions [kt]') )
plot              
ggsave( paste0('ceds-comparisons/',em,'_Global_RCP_Comparison.pdf') , width = 7, height = 4)

# ---------------------------------------------------------------------------
# 2.  Region Comparisons

#Prime Data
region_ceds <- aggregate(ceds[x_years], 
                         by = list(region = ceds$Region ),FUN=sum )
region_ceds$inv <- 'CEDS'
region_ceds_long <- melt(region_ceds, id.vars = c('region','inv'))

rcp <- RCP[which(RCP$Sector == 'Tot_Ant'),]
region_rcp <- aggregate(rcp[,x_rcp_years], 
                        by = list(region = rcp$Region ),FUN=sum )
region_rcp$inv <- 'RCP'
region_rcp_long <- melt(region_rcp, id.vars = c('region','inv'))

region_long <- rbind(region_ceds_long,region_rcp_long)
names(region_long) <- c('region','inv','year','total_emissions')
region_long$year <- gsub('X',"",region_long$year)
region_long$year <- as.numeric(region_long$year)

region <- rbind( region_ceds[,c('inv','region',x_rcp_years)],region_rcp[,c('inv','region',x_rcp_years)])
region <- region [ with( region , order( region , inv ) ), ]

#writeout
writeData(region,'DIAG_OUT', paste0(em,'_region_RCP_Comparison'),domain_extension = 'ceds-comparisons/',meta=F)

#Plot

regions_list <- region_long[,c('region','total_emissions')]
regions_list <- regions_list[order(-regions_list$total_emissions),]
regions_list_order <- unique(regions_list$region)
regions_df_order <- data.frame(region=regions_list_order,
                               group= unlist(lapply(X=1:6,FUN=rep, times=7))[1:41])

#5 seperate graphs, saved individually
for(i in 1:6){
  
plot_regions <- regions_list_order[(i*6-5):(i*6)]
  
plot_df <- region_long[which(region_long$region %in% plot_regions),c('inv','year','region','total_emissions')]
plot_df$inv <- as.factor(plot_df$inv)
plot_df$region <- as.factor(plot_df$region)
max <- 1.2*(max(plot_df$total_emissions))

plot <- ggplot(plot_df, aes(x=year,y=total_emissions, color = region, shape=inv)) + 
  geom_point(data = subset(plot_df, inv =='RCP'),size=2,aes(x=year,y=total_emissions, color = region)) +
  geom_line(data = subset(plot_df, inv =='CEDS'),size=1,aes(x=year,y=total_emissions, color = region)) +
  scale_x_continuous(breaks=c(1970,1980,1990,2000,2010))+
  # guides(color=guide_legend(ncol=3))+
  ggtitle( paste('Total',em,'Emissions by Region') )+
  labs(x='Year',y= paste(em,'Emissions [kt]'))+
  scale_y_continuous(limits = c(0,max ),labels = comma)
plot              
ggsave( paste0('ceds-comparisons/',em,'_Regional_RCP_Comparison_', 
               paste(plot_regions,collapse ='-' ),
               '.pdf') , width = 7, height = 4)

}

#5 seperate graphs, saved individually
plot_list <- list()
for(i in 1:6){
  
  plot_regions <- regions_list_order[(i*6-5):(i*6)]
  
  plot_df <- region_long[which(region_long$region %in% plot_regions),c('inv','year','region','total_emissions')]
  plot_df$inv <- as.factor(plot_df$inv)
  plot_df$region <- as.factor(plot_df$region)
  max <- 1.2*(max(plot_df$total_emissions))
  
  if(em == 'SO2'){
  if ( i %in% c(1)) max = 200000
  if ( i %in% c(2,3,4)) max = 20000
  if ( i %in% c(5,6)) max = 10000 }
  
  plot <- ggplot(plot_df, aes(x=year,y=total_emissions, color = region, shape=inv)) + 
    geom_point(data = subset(plot_df, inv =='RCP'),size=2,aes(x=year,y=total_emissions, color = region)) +
    geom_line(data = subset(plot_df, inv =='CEDS'),size=1,aes(x=year,y=total_emissions, color = region)) +
    scale_x_continuous(breaks=c(1970,1980,1990,2000,2010))+
    scale_y_continuous(limits = c(0,max ),labels = comma)+
    scale_shape_discrete(guide=FALSE)+
    labs(x='Year',y= paste(em,'Emissions [kt]'))+
    theme(legend.title=element_blank())
  plot
  plot_list[[i]]<-plot              
}

pdf(paste0('ceds-comparisons/',em,'_Regional_RCP_Comparison_All.pdf'),width=12,height=10,paper='special')
grid.arrange(plot_list[[1]],plot_list[[2]],
             plot_list[[3]],plot_list[[4]],
             plot_list[[5]],plot_list[[6]], ncol=2,
             top = paste('RCP vs CEDS - Regional',em,'Emissions'))
dev.off()

# ---------------------------------------------------------------------------
# 3.  Sector Comparisons

#Prime Data
sector_ceds <- aggregate(ceds[x_years], 
                         by = list(sector = ceds$Sector ),FUN=sum )
sector_ceds$inv <- 'CEDS'
sector_ceds_long <- melt(sector_ceds, id.vars = c('sector','inv'))

rcp <- RCP[-which(RCP$Sector == 'Tot_Ant'),]
sector_rcp <- aggregate(rcp[,x_rcp_years], 
                        by = list(sector = rcp$Sector ),FUN=sum )
sector_rcp$inv <- 'RCP'
sector_rcp_long <- melt(sector_rcp, id.vars = c('sector','inv'))

sector_long <- rbind(sector_ceds_long,sector_rcp_long)
names(sector_long) <- c('sector','inv','year','total_emissions')
sector_long$year <- gsub('X',"",sector_long$year)
sector_long$year <- as.numeric(sector_long$year)

sector <- rbind( sector_ceds[,c('inv','sector',x_rcp_years)],sector_rcp[,c('inv','sector',x_rcp_years)])
sector <- sector [ with( sector , order( sector , inv ) ), ]

#writeout
writeData(sector,'DIAG_OUT', paste0(em,'_sector_RCP_Comparison'),domain_extension = 'ceds-comparisons/',meta=F)

#Plot

  plot_df <- sector_long
  plot_df$inv <- as.factor(plot_df$inv)
  plot_df$sector <- as.factor(plot_df$sector)
  max <- 1.2*(max(plot_df$total_emissions))
  
  plot <- ggplot(plot_df, aes(x=year,y=total_emissions, color = sector, shape=inv)) + 
    geom_point(data = subset(plot_df, inv =='RCP'),size=2,aes(x=year,y=total_emissions, color = sector)) +
    geom_line(data = subset(plot_df, inv =='CEDS'),size=1,aes(x=year,y=total_emissions, color = sector)) +
    scale_x_continuous(breaks=c(1970,1980,1990,2000,2010))+
    ggtitle( paste('Global',em,'Emissions by Sector') )+
    labs(x='Year',y= paste(em,'Emissions [kt]'))+
    scale_shape_discrete(guide=FALSE)+
    scale_y_continuous(limits = c(0,max ),labels = comma)
  plot              
  ggsave( paste0('ceds-comparisons/',em,'_sector_RCP_Comparison_',
                 '.pdf') , width = 7, height = 4)


logStop()
