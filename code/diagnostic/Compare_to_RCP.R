# ------------------------------------------------------------------------------
# Program Name: Compare_to_RCP.R
# Author: Rachel Hoesly, Linh Vu, Leyang feng
# Date Last Updated: 4 April 2016 
# Program Purpose: Produces comparison - diagnostic files and plots between CEDS and
#                  RCP. Comparison by global totals, regions, sectors
#                  Like with like comparison does not include 
#                      open burning (grassland and forest fires),
#                      fossil-fuel fires,
#                      agricultural waste burning on fields,
#                      international shipping,
#                      aviation
#                  
# Input Files: [em]_total_CEDS_emissions.csv
# Output Files: figures in the diagnostic-output
# Note: (1) the script uses 'cowplot' package to add footnotes for each pdf plot 
#           except the pdf that contains multiple grobs. The package 'cowplot' is
#           imported in section 0.5 than set the plot theme back to ggplot2 default.
#           All codes related to the use of 'cowplot' are surrounded by ### comments 
# TODO: (1) Remove the use of 'cowplot' when new version of ggplot2 is available ( > 2.1).
#           The new ggplot2 will provide default method of adding captions. 
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
              'common_data.R', 'IO_functions.R', 'data_functions.R', 'timeframe_functions.R') # Additional function files may be required.
log_msg <- "Compare_to_RCP" # First message to be printed to the log
script_name <- "Compare_to_RCP.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "NMVOC"

# ---------------------------------------------------------------------------
# 0.5 Load Packages

library('ggplot2')
library('plyr')
library('scales')
library('gridExtra')

### see note (1) for triple # comments 
library( 'cowplot' )
theme_set( theme_gray( ) ) # switch back to default ggplot2 theme
### end 

# ---------------------------------------------------------------------------
# 0.5. Script Options

# years
rcp_start_year <- 1850
rcp_end_year <- 2000
CEDS_start_year <- 1850
CEDS_end_year <- end_year

rcp_years <- seq(from=rcp_start_year,to=rcp_end_year,by=10)
x_rcp_years <- paste0('X',rcp_years)

# footnotes
footnote_v1 <- 'This figure shows a "like with like" comparison between CEDS and RCP emissions. \nThese totals, therefore, do not include open burning (grassland and forest fires), fossil-fuel fires, \nagricultural waste burning on fields, international shipping, or aviation.'
if ( em == 'OC' ) {
  footnote_v1 <- 'This figure shows a "like with like" comparison between CEDS and RCP emissions. \nThese totals, therefore, do not include open burning (grassland and forest fires), fossil-fuel fires, \nagricultural waste burning on fields, international shipping, or aviation.\n(Note, OC emissions are in units of carbon, NOT total mass.)'
}  

# Non Comparable Sectors
rcp_remove_sectors <- c('AWB','Tot_Ant')
ceds_remove_sectors <- c("1A3ai_International-aviation",
                         "1A3di_International-shipping",
                         '1A3aii_Domestic-aviation',
                         '7A_Fossil-fuel-fires',
                         '3F_Agricultural-residue-burning-on-fields',
                         '11A_Volcanoes', 
                         '11B_Forest-fires', 
                         '11C_Other-natural', 
                         '6B_Other-not-in-total')

# ---------------------------------------------------------------------------
# 1. Load files

Map_region_codes <- readData( "EM_INV", domain_extension = 'RCP/',"RCP Region Mapping", ".xlsx", sheet_selection = 'Reg Codes',
                              meta=FALSE)
Map_iso_codes <- readData( "EM_INV", domain_extension = 'RCP/',"RCP Region Mapping", ".xlsx", sheet_selection = 'EDGAR32 & IEA',
                           meta=FALSE)
Map_sector <- readData( "EM_INV", domain_extension = 'RCP/',"RCP_CEDS_sector_map",
                        meta=FALSE)


Master_Country_List <- readData('MAPPINGS', 'Master_Country_List')
Total_Emissions <- readData('MED_OUT', paste0(em,'_total_CEDS_emissions'))

# ---------------------------------------------------------------------------
# 2. Load and process RCP files

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
# 3. Process RCP Emissions Data

# Process and clean RCP data. No removing sectors yet
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

# Remove sectors not comparable with CEDS. Recalculated totals
RCP <- RCP[,c('em','Region','Sector',x_rcp_years)]

# ---------------------------------------------------------------------------
# 4. Process CEDS Emissions Data 
x_years<-paste('X',CEDS_start_year:CEDS_end_year,sep="")

CEDS <- Total_Emissions
CEDS$em <- em

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

# add sector to ceds data
CEDS$RCP_Sector <- sector_map[match(CEDS$sector,sector_map$CEDS),'RCP']

# add region to ceds data
CEDS$Region <- complete_region_map[match(CEDS$iso,tolower(complete_region_map$Code)),'Region']
CEDS[which(is.na(CEDS$Region)),'Region']<- 'Not Mapped'
CEDS <- CEDS[,c('em','iso','Region','sector','RCP_Sector',x_years)]


# ---------------------------------------------------------------------------
# 5. Remove sectors to make like with like comparison
rcp_comparable <- RCP[which(RCP$Sector %!in% rcp_remove_sectors),]
ceds_comparable <- CEDS[-which(CEDS$sector %in% ceds_remove_sectors),]

# ---------------------------------------------------------------------------
# 6.  Gloabal Comparisons

#Aggregate CEDS
global_ceds <- aggregate(ceds_comparable[x_years], 
                         by = list(em = ceds_comparable$em ),
                         FUN=sum )

global_ceds$Inventory <- 'CEDS'
global_ceds_long <- melt(global_ceds, id.vars = c('em','Inventory'))

#Aggregate RCP
global_rcp <- aggregate(rcp_comparable[,x_rcp_years], 
                        by = list(em = rcp_comparable$em ),
                        FUN=sum )
global_rcp$Inventory <- 'RCP'
global_rcp_long <- melt(global_rcp, id.vars = c('em','Inventory'))

# Combine
global <- rbind( global_ceds[,c('em','Inventory',x_rcp_years)],global_rcp[,c('em','Inventory',x_rcp_years)])

global_long <- rbind(global_ceds_long,global_rcp_long)
names(global_long) <- c('em','Inventory','year','total_emissions')
global_long <- global_long[,c('Inventory','year','total_emissions')]
global_long$year <- gsub('X',"",global_long$year)
global_long$year <- as.numeric(global_long$year)
global_long$Inventory <- as.factor(global_long$Inventory)

#writeout
writeData(global,'DIAG_OUT', paste0('RCP_',em,'_Global_Comparison'),domain_extension = 'ceds-comparisons/',meta=F)

# Global Plot
# Prime Data
df <- global_long
df$total_emissions <- global_long$total_emissions/1000 #convert from Gg to Tg
max <- 1.2*(max(df$total_emissions))

plot <- ggplot(df, aes(x=year,y=total_emissions,group=Inventory,shape=Inventory,linetype=Inventory)) + 
  geom_line(data = subset(df, Inventory=='CEDS'),size=1, color = 'black') +
  geom_point(data = subset(df, Inventory=='RCP'),color='dodgerblue1') +
  scale_x_continuous(limits = c(CEDS_start_year,2015 ),
                     breaks= seq(from=CEDS_start_year, to=rcp_end_year, by=50),
                     minor_breaks = seq(from=CEDS_start_year, to=rcp_end_year, by=25)) +
  scale_y_continuous(limits = c(0,max ),labels = comma)+
  ggtitle( em )+
  labs(x= "" , y= 'Emissions [Tg/yr]' )+
  theme(panel.background=element_blank(),
        panel.grid.minor = element_line(colour="gray95"),
        panel.grid.major = element_line(colour="gray88"))+
  scale_linetype_manual(name= 'Inventory',
                        breaks = c('CEDS','RCP'),
                        values = c('solid','blank'))+
  scale_shape_manual(name= 'Inventory',
                     breaks = c('CEDS','RCP'),
                     values = c(NA,19))

### adding footnote -- see note (1) for triple # comments 
footnote_added <- add_sub( plot, footnote_v1, size = 6 ) # add footnote 
ggdraw( footnote_added )
### end 

ggsave( paste0('ceds-comparisons/RCP_',em,'_Global_Comparison.pdf') , width = 7, height = 4)

# ---------------------------------------------------------------------------
# 7.  Region Comparisons

#Prime CEDS Data
region_ceds <- aggregate(ceds_comparable[x_years], 
                         by = list(region = ceds_comparable$Region ),FUN=sum )
region_ceds$Inventory <- 'CEDS'
region_ceds_long <- melt(region_ceds, id.vars = c('region','Inventory'))

#Prime RCP Data
region_rcp <- aggregate(rcp_comparable[x_rcp_years], 
                         by = list(region = rcp_comparable$Region ),FUN=sum )
region_rcp$Inventory <- 'RCP'
region_rcp_long <- melt(region_rcp, id.vars = c('region','Inventory'))

# Combine
region_long <- rbind(region_ceds_long,region_rcp_long)
names(region_long) <- c('region','Inventory','year','total_emissions')
region_long$year <- gsub('X',"",region_long$year)
region_long$year <- as.numeric(region_long$year)

region <- rbind( region_ceds[,c('Inventory','region',x_rcp_years)],region_rcp[,c('Inventory','region',x_rcp_years)])
region <- region [ with( region , order( region , Inventory ) ), ]

#writeout
writeData(region,'DIAG_OUT', paste0('RCP_',em,'_region_Comparison'),domain_extension = 'ceds-comparisons/',meta=F)

#Plot Regions
# Create order of plots
regions_list <- region_long[,c('region','total_emissions')]
regions_list <- regions_list[order(-regions_list$total_emissions),]
regions_list_order <- unique(regions_list$region)
regions_df_order <- data.frame(region=regions_list_order,
                               group= unlist(lapply(X=1:6,FUN=rep, times=7))[1:41])

#5 seperate graphs, Save individually and together
plot_list <- list()
for(i in 1:6){
  
  plot_regions <- regions_list_order[(i*6-5):(i*6)]
  
  plot_df <- region_long[which(region_long$region %in% plot_regions),c('Inventory','year','region','total_emissions')]
  plot_df$Inventory <- as.factor(plot_df$Inventory)
  plot_df$region <- as.factor(plot_df$region)
  max <- 1.2*(max(plot_df$total_emissions))
  
  plot <- ggplot(plot_df, aes(x=year,y=total_emissions, color = region, 
                              shape=Inventory,linetype = Inventory)) +
    geom_line(data = subset(plot_df, Inventory =='CEDS'),size=1,aes(x=year,y=total_emissions, color = region)) +
    geom_point(data = subset(plot_df, Inventory =='RCP'),size=2,aes(x=year,y=total_emissions, color = region)) +
    scale_x_continuous(breaks=seq(from=rcp_start_year,to=rcp_end_year,by=30))+
    ggtitle( em )+
    labs(x= "" , y= 'Emissions [Tg/yr]' )+
    theme(panel.background=element_blank(),
          panel.grid.minor = element_line(colour="gray95"),
          panel.grid.major = element_line(colour="gray88"))+
    scale_y_continuous(limits = c(0,max ),labels = comma)+
    scale_shape_manual(name= 'Inventory',
                       breaks = c('CEDS','RCP'),
                       values = c(46,19))+
    scale_linetype_manual(name= 'Inventory',
                       breaks = c('CEDS','RCP'),
                       values = c('solid','blank'))
  plot_list[[i]]<-plot
  ### adding footnote -- see note (1) for triple # comments 
  footnote_added <- add_sub( plot, footnote_v1, size = 6 ) # add footnote 
  ggdraw( footnote_added )
  ### end 
  
  ggsave( paste0('ceds-comparisons/RCP_',em,'_Regional_Comparison_', 
                 paste(plot_regions,collapse ='-' ),
                 '.pdf') , width = 7, height = 4)
  
}

pdf(paste0('ceds-comparisons/RCP_',em,'_Regional_Comparison_All.pdf'),width=12,height=10,paper='special')
grid.arrange(plot_list[[1]],plot_list[[2]],
             plot_list[[3]],plot_list[[4]],
             plot_list[[5]],plot_list[[6]], ncol=2,
             top = paste('RCP vs CEDS - Regional',em,'Emissions'))
dev.off()

# ---------------------------------------------------------------------------
# 8.  Sector Comparisons

#Prime CEDS Data
sector_ceds <- aggregate(ceds_comparable[x_years], 
                         by = list(sector = ceds_comparable$RCP_Sector ),FUN=sum )
sector_ceds$Inventory <- 'CEDS'
sector_ceds_long <- melt(sector_ceds, id.vars = c('sector','Inventory'))

#Prime RCP Data
sector_rcp <- aggregate(rcp_comparable[x_rcp_years], 
                        by = list(sector = rcp_comparable$Sector ),FUN=sum )
sector_rcp$Inventory <- 'RCP'
sector_rcp_long <- melt(sector_rcp, id.vars = c('sector','Inventory'))

# Combine
sector_long <- rbind(sector_ceds_long,sector_rcp_long)
names(sector_long) <- c('sector','Inventory','year','total_emissions')
sector_long$year <- gsub('X',"",sector_long$year)
sector_long$year <- as.numeric(sector_long$year)

sector <- rbind( sector_ceds[,c('Inventory','sector',x_rcp_years)],sector_rcp[,c('Inventory','sector',x_rcp_years)])
sector <- sector [ with( sector , order( sector , Inventory ) ), ]

#writeout
writeData(sector,'DIAG_OUT', paste0('RCP_',em,'_sector_Comparison'),domain_extension = 'ceds-comparisons/',meta=F)

#Plot

plot_df <- sector_long
plot_df$Inventory <- as.factor(plot_df$Inventory)
plot_df$sector <- as.factor(plot_df$sector)
max <- 1.2*(max(plot_df$total_emissions))

plot <- ggplot(plot_df, aes(x=year,y=total_emissions, color = sector, 
                            shape=Inventory,linetype = Inventory)) +
  geom_line(data = subset(plot_df, Inventory =='CEDS'),size=1,aes(x=year,y=total_emissions, color = sector)) +
  geom_point(data = subset(plot_df, Inventory =='RCP'),size=2,aes(x=year,y=total_emissions, color = sector)) +
  scale_x_continuous(breaks=seq(from=rcp_start_year,to=rcp_end_year,by=30))+
  ggtitle( em )+
  labs(x= "" , y= 'Emissions [Tg/yr]' )+
  theme(panel.background=element_blank(),
        panel.grid.minor = element_line(colour="gray95"),
        panel.grid.major = element_line(colour="gray88"))+
  scale_y_continuous(limits = c(0,max ),labels = comma)+
  scale_shape_manual(name= 'Inventory',
                     breaks = c('CEDS','RCP'),
                     values = c(46,19))+
  scale_linetype_manual(name= 'Inventory',
                        breaks = c('CEDS','RCP'),
                        values = c('solid','blank'))

### adding footnote -- see note (1) for triple # comments 
footnote_added <- add_sub( plot, footnote_v1, size = 6 ) 
ggdraw( footnote_added )
### end 

ggsave( paste0('ceds-comparisons/RCP_',em,'_sector_Comparison',
               '.pdf') , width = 7, height = 4)

# ---------------------------------------------------------------------------
# 9.  Region and Sector Comparisons (tables only)

#Prime Data
region_sector_ceds <- aggregate(ceds_comparable[x_years], 
                                by = list(region = ceds_comparable$Region, 
                                          sector = ceds_comparable$RCP_Sector ),FUN=sum )
region_sector_ceds$Inventory <- 'CEDS'

region_sector_rcp <- aggregate(rcp_comparable[,x_rcp_years], 
                               by = list(region = rcp_comparable$Region, 
                                         sector = rcp_comparable$Sector ),FUN=sum )
region_sector_rcp$Inventory <- 'RCP'

region_sector_both <- rbind( region_sector_ceds[,c( 'Inventory', 'region', 'sector', x_rcp_years )],
                             region_sector_rcp[,c( 'Inventory', 'region', 'sector', x_rcp_years )])

region_sector_both <- region_sector_both [ with( region_sector_both , order( region , sector, Inventory ) ), ]

#writeout
writeData( region_sector_both,'DIAG_OUT', paste0('RCP_',em,'_region_sector_Comparison'),domain_extension = 'ceds-comparisons/',meta=F)


# ---------------------------------------------------------------------------
# 10. End

logStop()
