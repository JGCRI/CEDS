# ------------------------------------------------------------------------------
# Program Name: Figures.R
# Author: Rachel Hoesly
# Date Last Updated: Oct 1 2015 
# Program Purpose: Produces diagnostic summary figures of default and 
#                  scaled emissions
# Input Files: F.em_scaled_emissions, D.em_default_emissions
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
log_msg <- "Figures" # First message to be printed to the log
script_name <- "Figures.R"

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
# 1. Load files

MCL <- readData( "MAPPINGS", "Master_Country_List")
TotalEmissions <- readData('MED_OUT', paste0('F.',em,'_scaled_emissions'))
DefaultEmissions <- readData('MED_OUT', paste0('D.',em,'_default_total_emissions'))

setwd('../diagnostic-output')

# ---------------------------------------------------------------------------
# Data Problems

# 1. Prepare Data for GGplot
x_years<-paste('X',1960:2013,sep="")

TotalEmissions.long <- melt(TotalEmissions,id.vars = c('iso','sector','fuel','units'))
names(TotalEmissions.long) <- c('iso','sector','fuel','units','year','SO2_Emissions')

TotalEmissions.long$iso<-tolower(TotalEmissions.long$iso)
TotalEmissions.long$year<-substr(TotalEmissions.long$year,2,6)

TotalEmissions.long$Region <- MCL[match(TotalEmissions.long$iso,MCL$iso),'Figure_Region']

TotalEmissions.long$Region <- as.factor(TotalEmissions.long$Region)
TotalEmissions.long$year <- as.integer(TotalEmissions.long$year)

#-----

DefaultEmissions.long <- melt(DefaultEmissions,id.vars = c('iso','sector','fuel','units'))
names(DefaultEmissions.long) <- c('iso','sector','fuel','units','year','SO2_Emissions')

DefaultEmissions.long$iso<-tolower(DefaultEmissions.long$iso)
DefaultEmissions.long$year<-substr(DefaultEmissions.long$year,2,6)

DefaultEmissions.long$Region <- MCL[match(DefaultEmissions.long$iso,MCL$iso),'Figure_Region']

DefaultEmissions.long$Region <- as.factor(DefaultEmissions.long$Region)
DefaultEmissions.long$year <- as.integer(DefaultEmissions.long$year)

# ---------------------------------------------------------------------------
# 1. Plots - #Stacked Area Plot By Region

#Scaled Emissions
Regions<-ddply(TotalEmissions.long, .(Region,year),summarize,
               SO2_Emissions=sum(SO2_Emissions, na.rm=TRUE))

df <- Regions
df$Region <- factor(df$Region,levels=c('North America','Western Europe',
                                        'Eastern Europe and FSU','Middle East',
                                       'Africa','Asia','Central and South America',
                                       'South East Asia and Aust/NZ'))
df <- df[order(-df$Region),]
plot <- ggplot(df, aes(x=year,y=SO2_Emissions,
                       fill=Region)) + 
                  geom_area(size=1) +
                  scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010))+
                  scale_y_continuous(labels = comma)+
                  ggtitle( paste('Global Scaled',em,' Emissions') )+
                  labs(x='Year',y= paste(em,'Emissions [kt]') )
plot              
ggsave( paste0('summary-plots/',em,'_regions.scaled.pdf') )
#Convert to wide format for easier viewing
data.long <- cast(df, Region ~ year, mean, value="SO2_Emissions")
writeData( data.long, "DIAG_OUT", paste0('summary-plots/',em ,'_emissions_scaled_by_region') )

#----
# Default Emissions
Regions<-ddply(DefaultEmissions.long, .(Region,year),summarize,
               SO2_Emissions=sum(SO2_Emissions, na.rm=TRUE))

df <- Regions
df <- df[-which(is.na(df$Region)),]
plot <- ggplot(df, aes(x=year,y=SO2_Emissions,
                        fill=Region)) + 
  geom_area(size=1) +
  scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010))+
  scale_y_continuous(labels = comma)+
  ggtitle( paste('Global Default',em,' Emissions') )+
  labs(x='Year',y= paste(em,'Emissions [kt]') )
plot              
ggsave( paste0('summary-plots/',em,'_regions.default.pdf') )

##line graphs by region
#Scaled Emissions
Regions<-ddply(TotalEmissions.long, .(Region,year),summarize,
               SO2_Emissions=sum(SO2_Emissions, na.rm=TRUE))

df <- Regions
df <- df[-which(is.na(df$Region)),]
plot <- ggplot(df, aes(x=year,y=SO2_Emissions,
                       color=Region)) + 
  geom_line(size=1) +
  scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010))+
  scale_y_continuous(labels = comma)+
  ggtitle( paste('Global Scaled',em,' Emissions') )+
  labs(x='Year',y= paste(em,'Emissions [kt]') )
plot              
ggsave( paste0('summary-plots/',em,'_regions.scaled.line.pdf') )
#----
# Default Emissions
Regions<-ddply(DefaultEmissions.long, .(Region,year),summarize,
               SO2_Emissions=sum(SO2_Emissions, na.rm=TRUE))

df <- Regions
df <- df[-which(is.na(df$Region)),]
plot <- ggplot(df, aes(x=year,y=SO2_Emissions,
                        color=Region)) + 
  geom_line(size=1) +
  scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010))+
  scale_y_continuous(labels = comma)+
  ggtitle( paste('Global Default',em,' Emissions') ) +
  labs(x='Year',y= paste(em,'Emissions [kt]') )
plot              
ggsave( paste0('summary-plots/',em,'_regions.default.line.pdf') )


# ---------------------------------------------------------------------------
# 1. Plots - #Stacked Area Plot By Sector

#Scaled
Sectors<-ddply(TotalEmissions.long, .(sector,year),summarize,
               SO2_Emissions=sum(SO2_Emissions, na.rm=TRUE))

df <- Sectors
plot <- ggplot(df, aes(x=year,y=SO2_Emissions, fill=sector)) + 
  geom_area(size=1) +
  scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010))+
  scale_y_continuous(labels = comma)+
  ggtitle('Global Scaled SO2 Emissions')+
  labs(x='Year',y= paste(em,'Emissions [kt]') )+
  guides(fill=guide_legend(ncol=2))
plot              
ggsave( paste0('summary-plots/',em,'_sectors.scaled.pdf') )
#Convert to wide format for easier viewing
data.long <- cast(df, sector ~ year, mean, value="SO2_Emissions")
writeData( data.long, "DIAG_OUT", paste0('summary-plots/',em ,'_emissions_scaled_by_sector') )


#Default
Sectors<-ddply(DefaultEmissions.long, .(sector,year),summarize,
               SO2_Emissions=sum(SO2_Emissions, na.rm=TRUE))

df <- Sectors
plot <- ggplot(df, aes(x=year,y=SO2_Emissions,
                        fill=sector)) + 
  geom_area(size=1) +
  scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010))+
  scale_y_continuous(labels = comma)+
  ggtitle('Global Default SO2 Emissions')+
  labs(x='Year',y= paste(em,'Emissions [kt]') )+
  guides(fill=guide_legend(ncol=2))
  
plot              
ggsave( paste0('summary-plots/',em,'_sectors.default.pdf') )



# ---------------------------------------------------------------------------
# 1. Plots - #Stacked Area Plot By Fuel

# Scaled
Fuels<-ddply(TotalEmissions.long, .(fuel,year),summarize,
             SO2_Emissions=sum(SO2_Emissions, na.rm=TRUE))

df <- Fuels
plot <- ggplot(df, aes(x=year,y=SO2_Emissions,
                        fill=fuel)) + 
  geom_area(size=1) +
  scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010))+
  scale_y_continuous(labels = comma)+
  ggtitle('Global Scaled SO2 Emissions')+
  labs(x='Year',y= paste(em,'Emissions [kt]') )
plot              
ggsave( paste0('summary-plots/',em,'_fuel.scaled.pdf') )
#Convert to wide format for easier viewing
data.long <- cast(df, fuel ~ year, mean, value="SO2_Emissions")
writeData( data.long, "DIAG_OUT", paste0('summary-plots/',em ,'_emissions_scaled_by_fuel') )

# Default
Fuels<-ddply(DefaultEmissions.long, .(fuel,year),summarize,
             SO2_Emissions=sum(SO2_Emissions, na.rm=TRUE))

df <- Fuels
plot <- ggplot(df, aes(x=year,y=SO2_Emissions,
                      fill=fuel)) + 
  geom_area(size=1) +
  scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010))+
  scale_y_continuous(labels = comma)+
  ggtitle('Global Default SO2 Emissions')+
  labs(x='Year',y= paste(em,'Emissions [kt]') )
plot              
ggsave( paste0('summary-plots/',em,'_fuel.default.pdf') )











