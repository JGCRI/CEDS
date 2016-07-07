# ------------------------------------------------------------------------------
# Program Name: Compare_to_GAINS.R
# Author: Rachel Hoesly
# Date Last Updated: 28 June 2016 
# Program Purpose: 
# Input Files: [em]_total_CEDS_emissions.csv
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
headers <- c( "data_functions.R",'common_data.R', 'IO_functions.R') # Additional function files may be required.
log_msg <- "Compare_to_GAINS" # First message to be printed to the log
script_name <- "Compare_to_GAINS.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "CO"

# Stop script if running for unsupported species
if ( em %!in% c('SO2','NOx','NMVOC','BC','OC','CH4','CO','CO2') ) {
  stop (paste( 'GAINS EMF-30 is not supported for emission species', em))
}

# ---------------------------------------------------------------------------
# 0.5 Load Packages

library('ggplot2')
library('plyr')
library('scales')
library('gridExtra')

# ---------------------------------------------------------------------------
# 0.5. Script Options

gains_start_year <- 2000
gains_end_year <- 2010
CEDS_start_year <- 1960
CEDS_end_year <- end_year

gains_years <- seq(from=gains_start_year,to=gains_end_year,by=5)
x_gains_years <- paste0('X',gains_years)

ceds_start_year <- (gains_start_year - 5)
ceds_years <- ceds_start_year:end_year
x_ceds_years <- paste0('X',ceds_years)

# ---------------------------------------------------------------------------
# 1. Load files

# GAINS emissions
e.sheet <- em
if(em == 'NMVOC') e.sheet <- 'VOC'
gains_emissions <- readData( domain = 'EM_INV', domain_extension = 'GAINS/', file_name = 'GAINS_EMF30_EMISSIONS_extended_Ev5a_CLE_Nov2015',
                       ".xlsx", sheet_selection = e.sheet)

# CEDS emissions
Master_Country_List <- readData('MAPPINGS', 'Master_Country_List')
ceds_emissions <- readData('MED_OUT', paste0(em,'_total_CEDS_emissions'))

#mapping files
ctry_map <- readData(domain = 'MAPPINGS',  domain_extension = 'GAINS/', file_name ='emf-30_ctry_map')

# ---------------------------------------------------------------------------
# 2. Process GAINS

gains <- gains_emissions[ which(gains_emissions$Sector %in% c('Sum') ), 
                          c("Region" ,"Sector", "2000" ,  "2005" ,  "2010")]

names(gains) <- c("Region" ,"Sector", "X2000" ,  "X2005" ,  "X2010")

gains <- aggregate( gains[x_gains_years], by = list( Region = gains$Region) , sum )

# only CO2 is reported in Tg rather than Gg, so multiply by 10^3 to convert Tg to Gg
if(em == 'CO2') gains[,years] <- gains[,years]*10^3

gains_long <- melt(gains)
gains_long$inv <- 'GAINS'

# ---------------------------------------------------------------------------
# 3. Process CEDS

not_gains_sectors <- c('1A3ai_International-aviation',
                        '1A3aii_Domestic-aviation',
                        '1A3di_International-shipping',
                        '1A3dii_Domestic-navigation',
                        '1B2d_Fugitive-other-energy',
                        '6A_Other-in-total',
                        '6B_Other-not-in-total',
                        '7A_Fossil-fuel-fires',
                        '11A_Volcanoes',
                        '11B_Forest-fires',
                        '11C_Other-natural',
                       '5C_Waste-incineration')

ceds <- ceds_emissions[ which( ceds_emissions$sector %!in% not_gains_sectors )
                        ,c('iso',x_ceds_years)]
ceds$Region <- ctry_map[match(ceds$iso, ctry_map$iso),'emf_name']

ceds <- aggregate( ceds[x_ceds_years], by = list( Region = ceds$Region) , sum )

ceds_long <- melt(ceds)
ceds_long$inv <- 'CEDS'

# ---------------------------------------------------------------------------
# 4. Graph Global Comparison

global_long <- rbind(ceds_long,gains_long)
names(global_long) <- c("Region", "year" ,"total","inv" )
global_long$year <- gsub('X',"",global_long$year)
global_long$year <- as.numeric(global_long$year)

global <- cast(global_long, inv ~ year, value = 'total', fun.aggregate = sum)[c('inv',gains_years)]

#writeout
writeData(global,'DIAG_OUT', paste0('GAINS_',em,'_Global_Comparison'),domain_extension = 'ceds-comparisons/',meta=F)

#Plot
df <- global_long[,c('inv','year','total')]
df <- aggregate(df['total'], by = list(inv = df$inv, year = df$year), sum)
df$inv <- as.factor(df$inv)
max <- 1.2*(max(df$total))
plot <- ggplot(df, aes(x=year,y=total, color = inv)) + 
  geom_point(shape=19) +
  geom_line(data = subset(df, inv=='CEDS'),size=1,aes(x=year,y=total, color = inv)) +
  scale_x_continuous(breaks= seq(from=ceds_start_year,to=end_year,by=5) )+
  scale_y_continuous(limits = c(0,max ),labels = comma)+
  ggtitle( paste('Global',em,'Emissions') )+
  labs(x='Year',y= paste(em,'Emissions [kt]') )
plot
ggsave( paste0('../diagnostic-output/ceds-comparisons/GAINS_',em,'_Global_Comparison.pdf') , width = 7, height = 4)




# ---------------------------------------------------------------------------
# 4. Graph Regional Comparison

region_long <- global_long
region_wide <- cast(region_long, Region + inv ~ year, value = 'total')
region_wide <- region_wide[c( 'Region', 'inv', gains_years)]

#write out table
writeData(region_wide,'DIAG_OUT', paste0('GAINS_',em,'_Region_Comparison'),domain_extension = 'ceds-comparisons/',meta=F)

regions_list <- region_long[which(region_long$year == '2000'),c('Region','total')]
regions_list <- regions_list[order(-regions_list$total),]
regions_list_order <- unique(regions_list$Region)
regions_df_order <- data.frame(Region=regions_list_order,
                               plot = c(unlist(lapply(X=1:4,FUN=rep, times=4)),
                                        unlist(lapply(X=5:6,FUN=rep, times=5))))

plot_list <- list()
for(i in 1:6){
  
  plot_regions <- regions_df_order [which(regions_df_order$plot == i),'Region']
  
  plot_df <- region_long[which(region_long$Region %in% plot_regions),c('inv','year','Region','total')]
  plot_df$inv <- as.factor(plot_df$inv)
  plot_df$region <- as.factor(plot_df$Region)
  max <- 1.2*(max(plot_df$total))
  
  plot <- ggplot(plot_df, aes(x=year,y=total, color = region, shape=inv)) + 
    geom_point(data = subset(plot_df, inv =='GAINS'),size=2,aes(x=year,y=total, color = Region)) +
    geom_line(data = subset(plot_df, inv =='CEDS'),size=1,aes(x=year,y=total, color = Region)) +
    scale_x_continuous(breaks=seq(from=ceds_start_year,to=end_year,by=5))+
    scale_y_continuous(limits = c(0,max ),labels = comma)+
    scale_shape_discrete(guide=FALSE)+
    labs(x='Year',y= paste(em,'Emissions [kt]'))+
    theme(legend.title=element_blank())
  plot
  plot_list[[i]]<-plot              
}

pdf(paste0('../diagnostic-output/ceds-comparisons/GAINS_',em,'_Regional_Comparison_All.pdf'),width=12,height=10,paper='special')
grid.arrange(plot_list[[1]],plot_list[[2]],
             plot_list[[3]],plot_list[[4]],
             plot_list[[5]],plot_list[[6]], ncol=2,
             top = paste('GAINS vs CEDS - Regional',em,'Emissions'))
dev.off()

# ---------------------------------------------------------------------------
# 5. End

logStop()

