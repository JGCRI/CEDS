# ------------------------------------------------------------------------------
# Program Name: Paper_Figres_Compare_to_RCP.R
# Author: Rachel Hoesly, Linh Vu, Leyang Feng, Huong Nguyen
# Date Last Updated: 17 January
# Program Purpose: Produces comparison - diagnostic files and plots between CEDS and
#                  RCP. Comparison by global totals, regions, sectors
#                  Like with like comparison does not include
#                       open burning (grassland and forest fires),
#                       fossil-fuel fires,
#                       international shipping (See Note (2) and (3))
#                       aviation
#
# Input Files: [em]_total_CEDS_emissions.csv
# Output Files: figures in the diagnostic-output
# Note: (1) Shipping emissions is included in global comparison.
#       (2) RCP shipping emissions does not have data for NH3
# TODO: (1) In RCP shipping data NMVOC and CH4 data doesn't include emissions for tanker loading.
#           Change the sector drop mapping when tanker loading sector is added for CEDS.
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R",'process_db_functions.R',
              'common_data.R', 'IO_functions.R', 'data_functions.R', 'timeframe_functions.R') # Additional function files may be required.
log_msg <- "Compare_to_RCP" # First message to be printed to the log
script_name <- "Compare_to_RCP.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# Load Packages

library('ggplot2')
library('plyr')
library('scales')
library('gridExtra')

# function from stack exchange
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# ---------------------------------------------------------------------------
# 0.5. Script Options

# years
rcp_start_year <- 1850
rcp_end_year <- 2000
CEDS_start_year <- 1850
CEDS_end_year <- end_year

rcp_years <- seq(from=rcp_start_year,to=rcp_end_year,by=10)
x_rcp_years <- paste0('X',rcp_years)


# ---------------------------------------------------------------------------
# Start Emissions Loop
em_list <- c('SO2','NOx','BC','OC','NH3','CO','NMVOC')

# Create Plot Lists
global_sector_plot_list <- list()
top_region_plot_list <- list()

for( h in seq_along(em_list)){
  em <- em_list[h]

# footnotes
footnote_v1 <- 'This figure shows a "like with like" comparison between CEDS and RCP emissions. \nThese totals, therefore, do not include open burning (grassland and forest fires), fossil-fuel fires, \nagricultural waste burning on fields, or aviation.'
if ( em == 'OC' ) {
  footnote_v1 <- 'This figure shows a "like with like" comparison between CEDS and RCP emissions. \nThese totals, therefore, do not include open burning (grassland and forest fires), fossil-fuel fires, \nagricultural waste burning on fields, or aviation.\n(Note, OC emissions are in units of carbon, NOT total mass.)'
}
if ( em == "NH3" ) {
  footnote_v1 <- 'This figure shows a "like with like" comparison between CEDS and RCP emissions. \nThese totals, therefore, do not include open burning (grassland and forest fires), fossil-fuel fires, \nagricultural waste burning on fields, international shipping, or aviation.'
}

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

rcp_ship_emissions <- readData( domain = 'EM_INV', domain_extension = 'RCP/',
                                file_name = 'Historicalshipemissions_IPCC_FINAL_Jan09_updated_1850',
                                extension = '.xlsx',  sheet_selection = 'CO2Emis_TgC', skip = 8 )[ 1:140, 1:12 ]

# ---------------------------------------------------------------------------
# 1.5. Other script Options

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


# if current em does not have ship emissions
# for the RCP shipping emissions data Historicalshipemissions_IPCC_FINAL_Jan09_updated_1850.xlsx
# it doesn't contain data for NH3
has_ship <- em != "NH3"

if ( has_ship ) {
  ceds_remove_sectors_global <- c("1A3ai_International-aviation",
                                  '1A3aii_Domestic-aviation',
                                  '7A_Fossil-fuel-fires',
                                  '3F_Agricultural-residue-burning-on-fields',
                                  '11A_Volcanoes',
                                  '11B_Forest-fires',
                                  '11C_Other-natural',
                                  '6B_Other-not-in-total')

} else {
  ceds_remove_sectors_global <- ceds_remove_sectors

}

# ---------------------------------------------------------------------------
# 2. Load and process RCP files

rcp_dir <- './emissions-inventories/RCP/'

# create temporary folder to extract zipped files
zipfile_path <- paste0(rcp_dir, em, '.zip')
dir.name <- paste0(rcp_dir, em, '_RCP_temp_folder')
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

# ---------------------------------------------------------------------------
# 3. Process RCP Emissions Data
# ---------------------------------------------------------------------------
# 3.1 process RCP for sectors except international shipping ( shipping is in a seperate file and prcessed in 2.2 )
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
# 3.2 Process RCP shipping emissions
names( rcp_ship_emissions ) <- c( "year", "CO2", "fleet", "NOx", "SO2", "PM", "NMVOC", "CH4", "BC", "OC", "Refrigerants", "CO" )
rcp_shipping_em_list <- c( "CO2", "NOx", "SO2", "NMVOC", "BC", "OC", "CO", "CH4" )
rcp_ship_emissions <- rcp_ship_emissions[ , c( "year", rcp_shipping_em_list ) ]
# convert unit from TG to kt
rcp_ship_emissions [ , rcp_shipping_em_list ] <- rcp_ship_emissions [ , rcp_shipping_em_list ] * 1000
rcp_ship_emissions$units <- "kt"
rcp_ship_emissions$SO2 <- rcp_ship_emissions$SO2 * 2  #Convert from S to SO2 for SO2
rcp_ship_emissions$NOx <- rcp_ship_emissions$NOx * 3.285  # Convert from N to NO2 for NOx

# ---------------------------------------------------------------------------
# 4. Process CEDS Emissions Data
x_years<-paste('X',CEDS_start_year:CEDS_end_year,sep="")

CEDS <- Total_Emissions
CEDS$em <- em

# Create complete region map for CEDS to RCP
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

ceds_comparable_global <- CEDS [-which(CEDS$sector %in% ceds_remove_sectors_global),]

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

#Plot Regions
# Create order of plots
regions_list <- region_long[,c('region','total_emissions')]
regions_list <- regions_list[order(-regions_list$total_emissions),]
regions_list_order <- unique(regions_list$region)
regions_df_order <- data.frame(region=regions_list_order,
                               group= unlist(lapply(X=1:6,FUN=rep, times=7))[1:41])
# only the top emitters

  plot_regions <- c('China+','USA','Russia+','India','Western Africa')
  plot_df <- region_long[which(region_long$region %in% plot_regions),c('Inventory','year','region','total_emissions')]
  plot_df$Inventory <- as.factor(plot_df$Inventory)
  plot_df$region <- as.factor(plot_df$region)
  max <- 1.2*(max(plot_df$total_emissions))

  plot <- ggplot(plot_df, aes(x=year,y=total_emissions, color = region,
                              shape=Inventory,linetype = Inventory)) +
    geom_line(data = subset(plot_df, Inventory =='CEDS'),size=1,aes(x=year,y=total_emissions, color = region), alpha= .5) +
    geom_point(data = subset(plot_df, Inventory =='RCP'),size=1,aes(x=year,y=total_emissions, color = region), alpha= .5) +
    scale_x_continuous(breaks=seq(from=rcp_start_year,to=rcp_end_year,by=30))+
    ggtitle( em )+
    labs(x= "" , y= 'Emissions [Gg/yr]' )+
    theme(panel.background=element_blank(),
          panel.grid.minor = element_line(colour="gray95"),
          panel.grid.major = element_line(colour="gray88"))+
    scale_y_continuous(limits = c(0,max ),labels = comma)+
    scale_color_discrete(name = 'Region')+
    scale_shape_manual(name= 'Inventory',
                       breaks = c('CEDS','RCP'),
                       values = c(46,19))+
    scale_linetype_manual(name= 'Inventory',
                          breaks = c('CEDS','RCP'),
                          values = c('solid','blank'))
  top_region_plot_list[[h]] <- plot


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

#Plot

plot_df <- sector_long
plot_df$Inventory <- as.factor(plot_df$Inventory)
plot_df$sector <- as.factor(plot_df$sector)
max <- 1.2*(max(plot_df$total_emissions))

plot <- ggplot(plot_df, aes(x=year,y=total_emissions, color = sector,
                            shape=Inventory,linetype = Inventory)) +
  geom_line(data = subset(plot_df, Inventory =='CEDS'),size=1,aes(x=year,y=total_emissions, color = sector), alpha= .5) +
  geom_point(data = subset(plot_df, Inventory =='RCP'),size=1,aes(x=year,y=total_emissions, color = sector), alpha= .5) +
  scale_x_continuous(breaks=seq(from=rcp_start_year,to=rcp_end_year,by=30))+
  ggtitle( em )+
  labs(x= "" , y= 'Emissions [Gg/yr]' )+
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
global_sector_plot_list[[h]] <- plot

}
# End emissions loop

# ---------------------------------------------------------------------------
# 9.  Region and Sector Comparisons (tables only)

legend <- g_legend(top_region_plot_list[[1]])
top_region_plot_list <- lapply(top_region_plot_list, function(x) x + theme(legend.position="none"))
pdf(paste0('../diagnostic-output/paper-figures/Supplement/CMIP5_Region_Comparison.pdf'),width=12,height=10,paper='special')
grid.arrange(top_region_plot_list[[1]],top_region_plot_list[[2]],top_region_plot_list[[3]],
             top_region_plot_list[[4]],top_region_plot_list[[5]],top_region_plot_list[[6]],
             top_region_plot_list[[7]],legend,
             ncol=3,top = 'CMIP5 vs CEDS - Top Emitting Regions')
dev.off()

legend <- g_legend(global_sector_plot_list[[1]])
global_sector_plot_list <- lapply(global_sector_plot_list, function(x) x + theme(legend.position="none"))
pdf(paste0('../diagnostic-output/paper-figures/Supplement/CMIP5_Sector_Comparison.pdf'),width=12,height=10,paper='special')
grid.arrange(global_sector_plot_list[[1]],global_sector_plot_list[[2]],global_sector_plot_list[[3]],
             global_sector_plot_list[[4]],global_sector_plot_list[[5]],global_sector_plot_list[[6]],
             global_sector_plot_list[[7]],legend,
             ncol=3,top = 'CMIP5 vs CEDS - Global Emissions by Sector')
dev.off()

# ---------------------------------------------------------------------------
# 10. End

logStop()
