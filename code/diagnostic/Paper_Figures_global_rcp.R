# ------------------------------------------------------------------------------
# Program Name: R
# Author: 
# Date Last Updated: 
# Program Purpose: plots for CEDS compared to RCP and edgar (like with like - does not
#                  include forest fires/savannah burning, aviation, and international 
#                  shipping)
# Input Files: [em]_total_CEDS_emissions.csv
#               
# Output Files: 
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
              'common_data.R', 'IO_functions.R', 'data_functions.R', 'timeframe_functions.R') # Additional function files may be required.
log_msg <- "Paper_Figures_global_rcp" # First message to be printed to the log
script_name <- "Paper_Figures_global_rcp.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 0.5 Load Packages

library('ggplot2')
library('plyr')
library('scales')
library('gridExtra')
library('grid')

# function from stack exchange
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# ---------------------------------------------------------------------------
# 0.5. Script Options

rcp_start_year <- 1850
rcp_end_year <- 2000
CEDS_start_year <- 1750
CEDS_end_year <- end_year

rcp_years <- seq(from=rcp_start_year,to=rcp_end_year,by=10)
x_rcp_years <- paste0('X',rcp_years)


# 1. Load files

Map_region_codes <- readData( "EM_INV", domain_extension = 'RCP/',"RCP Region Mapping", ".xlsx", sheet_selection = 'Reg Codes',
                              meta=FALSE)
Map_iso_codes <- readData( "EM_INV", domain_extension = 'RCP/',"RCP Region Mapping", ".xlsx", sheet_selection = 'EDGAR32 & IEA',
                           meta=FALSE)
Map_sector <- readData( "EM_INV", domain_extension = 'RCP/',"RCP_CEDS_sector_map",
                        meta=FALSE)


Master_Country_List <- readData('MAPPINGS', 'Master_Country_List')
MSLevel <- readData('MAPPINGS', 'Master_Sector_Level_map')

#-----------
# start RCP emissions loop

em_list <- c('SO2','NOx','BC','OC','NH3','CO','NMVOC')
total_comparison_list <- list()
total_stacked_sector_list <- list()

for( h in seq_along(em_list)){

em <- em_list[h]
  
Total_Emissions <- readData('MED_OUT', paste0(em,'_total_CEDS_emissions'))

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
x_years<-paste('X',CEDS_start_year:CEDS_end_year,sep="")

ceds <- Total_Emissions
ceds$em <- em

# remove internation shipping, and aviation for comparison with RCP
ceds <- ceds[-which(ceds$sector %in% 
                      c("1A3ai_International-aviation",
                        "1A3di_International-shipping",
                        '1A3aii_Domestic-aviation',
                        '7A_Fossil-fuel-fires',
                        '3F_Agricultural-residue-burning-on-fields',
                        '1A_Volcanoes', 
                        '11B_Forest-fires', 
                        '11C_Other-natural', 
                        '6B_Other-not-in-total'
                         )),]

ceds$agg_Sector <- MSLevel[match(ceds$sector,MSLevel$working_sectors_v1),'Figure_sector']
ceds$agg_Sector <- as.factor(ceds$agg_Sector)

# ---------------------------------------------------------------------------
# 3.  Gloabal Comparisons

#Prime Data
#global
global_ceds <- aggregate(ceds[x_years], 
                         by = list(em= ceds$em ),FUN=sum )
global_ceds$inv <- 'CEDS'
global_ceds_long <- melt(global_ceds, id.vars = c('em','inv'))
#sector
sector_ceds <- aggregate(ceds[x_years], 
                         by = list(sector = ceds$agg_Sector ),FUN=sum )
sector_ceds_long <- melt(sector_ceds, id.vars = c('sector'))
names(sector_ceds_long) <- c('sector','year','total_emissions')
sector_ceds_long$year <- gsub('X',"",sector_ceds_long$year)
sector_ceds_long$year <- as.numeric(sector_ceds_long$year)

# Remove AWB from rcp totals
rcp <- RCP[which(RCP$Sector == 'Tot_Ant'),]
rcp_awb <- RCP[which(RCP$Sector == 'AWB'),]
rcp[,x_rcp_years] <- rcp[,x_rcp_years] - rcp_awb[,x_rcp_years]

#long format
global_rcp <- aggregate(rcp[,x_rcp_years], 
                        by = list(em= rcp$em ),FUN=sum )
global_rcp$inv <- 'CMIP5'
global_rcp_long <- melt(global_rcp, id.vars = c('em','inv'))


# combine
global_long <- rbind(global_ceds_long,global_rcp_long)
names(global_long) <- c('em','inv','year','total_emissions')
global_long$year <- gsub('X',"",global_long$year)
global_long$year <- as.numeric(global_long$year)

global <- rbind( global_ceds[,c('inv',x_rcp_years)],global_rcp[,c('inv',x_rcp_years)])

######
#Plot - only totals - no stacked sectors
df <- global_long[,c('inv','year','total_emissions')]
df$inv <- as.factor(df$inv)
df$total_emissions <- df$total_emissions/1000
max <- 1.1*(max(df$total_emissions))

plot <- ggplot(df, aes(x=year,y=total_emissions,group=inv,shape=inv,linetype=inv)) + 
  geom_line(data = subset(df, inv=='CEDS'),size=1, color = 'black') +
  geom_point(data = subset(df, inv=='CMIP5'),color='dodgerblue1') +
  scale_x_continuous(limits = c(CEDS_start_year,2015 ),
                     breaks= seq(from=CEDS_start_year, to=rcp_end_year, by=50),
                     minor_breaks = seq(from=CEDS_start_year, to=rcp_end_year, by=25)) +
  scale_y_continuous(limits = c(0,max ),labels = comma)+
  ggtitle( em )+
  labs(x= "" , y= paste(em ,'Emissions [Tg/yr]' )) +
  theme(panel.background=element_blank(),
        panel.grid.minor = element_line(colour="gray95"),
        panel.grid.major = element_line(colour="gray88"))+
  scale_linetype_manual(name= 'Inventory',
                        breaks = c('CEDS','CMIP5'),
                        values = c('solid','blank'))+
  scale_shape_manual(name= 'Inventory',
                     breaks = c('CEDS','CMIP5'),
                     values = c(NA,19))
plot

total_comparison_list[[h]] <- plot

#########
# Plot - totals with sector stack

df <- global_long[,c('inv','year','total_emissions')]
df$inv <- as.factor(df$inv)
df$total_emissions <- df$total_emissions/1000
max <- 1.1*(max(df$total_emissions))

df2 <- sector_ceds_long
df2$total_emissions <- df2$total_emissions/1000

plot <- ggplot(data= df2, aes(x=year,y=total_emissions)) + 
  geom_area( data= df2, aes(x=year,y=total_emissions, fill = sector),  alpha = .68)+
  geom_point(data = subset(df, inv=='CMIP5'), aes(x=year,y=total_emissions, shape = inv)) + 
  scale_x_continuous(limits = c(CEDS_start_year,2015 ),
                     breaks= seq(from=CEDS_start_year, to=rcp_end_year, by=50),
                     minor_breaks = seq(from=CEDS_start_year, to=rcp_end_year, by=25)) +
  scale_y_continuous(limits = c(0,max ),labels = comma)+
  ggtitle( em )+
  labs(x= "" , y= paste(em ,'Emissions [Tg/yr]' ) )+
  theme(#legend.position="none",
        panel.background=element_blank(),
        panel.grid.minor = element_line(colour="gray96", size = .4),
        panel.grid.major = element_line(colour="gray90", size = .3),
        panel.border = element_rect(colour="gray75", fill = NA, size = .8) )+
  scale_fill_discrete(name = 'CEDS Sector')+
  scale_shape_manual(name= 'Inventory',
                     breaks = c('CMIP5'),
                     values = c(19))
plot  

total_stacked_sector_list[[h]] <- plot

# ---------------------------------------------------------------------------
} # end emissions loop

total_stacked_sector_nolegend_list <- lapply(total_stacked_sector_list, function(x) x + theme(legend.position="none"))

total_comparison_nolegend_list <- lapply(total_comparison_list, function(x) x + theme(legend.position="none"))

# # Totals - 3x3 legend
# leg <-g_legend(total_comparison_list[[1]])
# pdf(paste0('../diagnostic-output/paper-figures/Paper_Figures_RCP_Comparison_totals_3x3.pdf'),width=9.5,height=9.5,paper='special')
# grid.arrange(total_comparison_nolegend_list[[1]],total_comparison_nolegend_list[[2]],total_comparison_nolegend_list[[3]],
#              total_comparison_nolegend_list[[4]],total_comparison_nolegend_list[[5]],total_comparison_nolegend_list[[6]],
#              total_comparison_nolegend_list[[7]],leg,  
#              ncol=3)
# dev.off()
# Totals - 4x2 legend
# leg <-g_legend(total_comparison_list[[1]])
# pdf(paste0('../diagnostic-output/paper-figures/Paper_Figures_RCP_Comparison_4x2_legend.pdf'),width=11,height=6,paper='special')
# grid.arrange(total_comparison_nolegend_list[[1]],total_comparison_nolegend_list[[2]],total_comparison_nolegend_list[[3]],
#              total_comparison_nolegend_list[[4]],total_comparison_nolegend_list[[5]],total_comparison_nolegend_list[[6]],
#              total_comparison_nolegend_list[[7]],leg, 
#              ncol=4)
# dev.off()

# Stacked Sector
leg <-g_legend(total_stacked_sector_list[[1]])
# Stacked Sector 
# 4x2 legend
pdf(paste0('../diagnostic-output/paper-figures/Paper_Figures_RCP_Comparison_stacked_sector 4x2_legend.pdf'),
    width=11,height=5.5,paper='special')
grid.arrange(total_stacked_sector_nolegend_list[[1]],total_stacked_sector_nolegend_list[[2]],total_stacked_sector_nolegend_list[[3]],
             total_stacked_sector_nolegend_list[[4]],total_stacked_sector_nolegend_list[[5]],total_stacked_sector_nolegend_list[[6]],
             total_stacked_sector_nolegend_list[[7]],leg, 
             ncol=4)
dev.off()
# 3x3
pdf(paste0('../diagnostic-output/paper-figures/Paper_Figures_RCP_Comparison_stacked_sector 3x3_legend.pdf'),width=9.5,height=9.5,paper='special')
grid.arrange(total_stacked_sector_nolegend_list[[1]],total_stacked_sector_nolegend_list[[2]],total_stacked_sector_nolegend_list[[3]],
             total_stacked_sector_nolegend_list[[4]],total_stacked_sector_nolegend_list[[5]],total_stacked_sector_nolegend_list[[6]],
             total_stacked_sector_nolegend_list[[7]],leg,
             ncol=3)
dev.off()



# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------



# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
  logStop()
