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
# 0.5. Script Options

PRINT_DEFAULTS <- TRUE
PRINT_BIG_TABLES <- FALSE

# ---------------------------------------------------------------------------
# 1. Load files

Master_Country_List <- readData( "MAPPINGS", "Master_Country_List")
TotalEmissions <- readData('MED_OUT', paste0('F.',em,'_scaled_emissions'))

if ( PRINT_DEFAULTS ) DefaultEmissions <- readData('MED_OUT', paste0('D.',em,'_default_total_emissions'))

setwd('../diagnostic-output')

# ---------------------------------------------------------------------------
# Data Problems

# 1. Prepare Data 
x_years<-paste('X',1960:2013,sep="")

TotalEmissions.long <- melt(TotalEmissions,id.vars = c('iso','sector','fuel','units'))
names(TotalEmissions.long) <- c('iso','sector','fuel','units','year','Emissions')

TotalEmissions.long$iso<-tolower(TotalEmissions.long$iso)
TotalEmissions.long$year<-substr(TotalEmissions.long$year,2,6)

TotalEmissions.long$Region <- Master_Country_List[match(TotalEmissions.long$iso,Master_Country_List$iso),'Figure_Region']
TotalEmissions.long$Country <- Master_Country_List[match(TotalEmissions.long$iso,Master_Country_List$iso),'Country_Name']

TotalEmissions.long$Region <- as.factor(TotalEmissions.long$Region)
TotalEmissions.long$year <- as.integer(TotalEmissions.long$year)

# Also add to the long format to use aggregate
TotalEmissions$Region <- Master_Country_List[match(TotalEmissions$iso,Master_Country_List$iso),'Figure_Region']
TotalEmissions$Country <- Master_Country_List[match(TotalEmissions$iso,Master_Country_List$iso),'Country_Name']

#-----
if ( PRINT_DEFAULTS ){
	DefaultEmissions.long <- melt(DefaultEmissions,id.vars = c('iso','sector','fuel','units'))
	names(DefaultEmissions.long) <- c('iso','sector','fuel','units','year','Emissions')

	DefaultEmissions.long$iso<-tolower(DefaultEmissions.long$iso)
	DefaultEmissions.long$year<-substr(DefaultEmissions.long$year,2,6)

	DefaultEmissions.long$Region <- Master_Country_List[match(DefaultEmissions.long$iso,Master_Country_List$iso),'Figure_Region']
	DefaultEmissions.long$Country <- Master_Country_List[match(DefaultEmissions.long$iso,Master_Country_List$iso),'Country_Name']

	DefaultEmissions.long$Region <- as.factor(DefaultEmissions.long$Region)
	DefaultEmissions.long$year <- as.integer(DefaultEmissions.long$year)
	
	# Also add to the long format to use aggregate
	DefaultEmissions$Region <- Master_Country_List[match(DefaultEmissions$iso,Master_Country_List$iso),'Figure_Region']
	DefaultEmissions$Country <- Master_Country_List[match(DefaultEmissions$iso,Master_Country_List$iso),'Country_Name']
	
}
# ---------------------------------------------------------------------------
# 0. Tables - Total scaled emissions by country

Em_by_Country<-aggregate( TotalEmissions[ X_emissions_years ],
                         by=list( Region =  TotalEmissions$Region,
                                  Country =  TotalEmissions$Country,
                                  iso = TotalEmissions$iso),sum )

writeData( Em_by_Country, "DIAG_OUT", paste0('summary-plots/',em ,'_emissions_scaled_by_country'), meta = FALSE )

if ( PRINT_DEFAULTS ){
  Em_by_Country<-aggregate( DefaultEmissions[ X_emissions_years ],
                            by=list( Region =  DefaultEmissions$Region,
                                     Country =  DefaultEmissions$Country,
                                     iso = DefaultEmissions$iso ),sum )
  
	writeData( Em_by_Country, "DIAG_OUT", paste0('summary-plots/',em ,'_emissions_default_by_country'), meta = FALSE )
}

# Total emissions by country and fuel

Em_by_Country<-aggregate( TotalEmissions[ X_emissions_years ],
                           by=list( Region =  DefaultEmissions$Region,
                                    Country =  DefaultEmissions$Country,
                                    iso = DefaultEmissions$iso,
                                    fuel =  TotalEmissions$fuel),sum )

writeData( Em_by_Country, "DIAG_OUT", paste0('summary-plots/',em ,'_emissions_scaled_by_country_fuel'), meta = FALSE )

if ( PRINT_DEFAULTS ){
  Em_by_Country<-aggregate( DefaultEmissions[ X_emissions_years ],
                            by=list( Region =  DefaultEmissions$Region,
                                     Country =  DefaultEmissions$Country,
                                     iso = DefaultEmissions$iso,
                                     fuel =  DefaultEmissions$fuel),sum )
  
	writeData( Em_by_Country, "DIAG_OUT", paste0('summary-plots/',em ,'_emissions_default_by_country_fuel'), meta = FALSE )

	Em_by_Country<-aggregate( DefaultEmissions[ X_emissions_years ],
							by=list( Region =  DefaultEmissions$Region,
									 Country =  DefaultEmissions$Country,
									 iso = DefaultEmissions$iso,
	                                 sector =  DefaultEmissions$sector),sum )
	
	writeData( Em_by_Country, "DIAG_OUT", paste0('summary-plots/',em ,'_emissions_default_by_country_sector'), meta = FALSE )

	Em_by_Sector<-aggregate( DefaultEmissions[ X_emissions_years ],
	                          by=list( sector =  DefaultEmissions$sector),sum )
	
	writeData( Em_by_Sector, "DIAG_OUT", paste0('summary-plots/',em ,'_emissions_default_global_sector'), meta = FALSE )
}

# Total emissions by region and fuel

Em_by_Region<-aggregate( TotalEmissions[ X_emissions_years ],
                          by=list( Region =  TotalEmissions$Region,
                                   fuel =  TotalEmissions$fuel),sum )

writeData( Em_by_Region, "DIAG_OUT", paste0('summary-plots/',em ,'_emissions_scaled_by_region_fuel'), meta = FALSE )

if ( PRINT_BIG_TABLES ) {
	# Total emissions by region and CEDS sector

  Em_by_Region<-aggregate( TotalEmissions[ X_emissions_years ],
                            by=list( Region =  TotalEmissions$Region,
                                     sector =  TotalEmissions$sector),sum )
  
	writeData( Em_by_Region, "DIAG_OUT", paste0('summary-plots/',em ,'_emissions_scaled_by_region_sector'), meta = FALSE )

	# Total emissions by region, CEDS sector, and fuel

	Em_by_Region<-aggregate( TotalEmissions[ X_emissions_years ],
	                          by=list( Region =  TotalEmissions$Region,
	                                   sector =  TotalEmissions$sector, 
  	                                   fuel =  TotalEmissions$fuel),sum )

	writeData( Em_by_Region, "DIAG_OUT", paste0('summary-plots/',em ,'_emissions_scaled_by_region_sector_fuel'), meta = FALSE )

	# Total emissions by country, CEDS sector
	Em_by_Country<-aggregate( TotalEmissions[ X_emissions_years ],
	                         by=list( Region =  TotalEmissions$Region,
                                  	  Country =  TotalEmissions$Country,
                                  	  iso = TotalEmissions$iso,
	                                  sector =  TotalEmissions$sector),sum )

		writeData( Em_by_Country, "DIAG_OUT", paste0('summary-plots/',em ,'_emissions_scaled_by_country_sector'), meta = FALSE )
}

# ---------------------------------------------------------------------------
# 1. Plots - #Stacked Area Plot By Region

#Scaled Emissions
Regions<-ddply(TotalEmissions.long, .(Region,year),summarize,
               Emissions=sum(Emissions, na.rm=TRUE))

df <- Regions
	df$Region <- factor(df$Region,levels=c('North America','Western Europe',
											'Eastern Europe', 'FSU','Middle East',
										   'Africa','Other Asia','China','Central and South America',
										   'South East Asia and Aust/NZ'))
df <- df[order(-df$Region),]
plot <- ggplot(df, aes(x=year,y=Emissions,
                       fill=Region)) + 
                  geom_area(size=1) +
                  scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010))+
                  scale_y_continuous(labels = comma)+
                  ggtitle( paste('Global Scaled',em,' Emissions') )+
                  labs(x='Year',y= paste(em,'Emissions [kt]') )
plot              
ggsave( paste0('summary-plots/',em,'_regions.scaled.pdf') , width = 11, height = 6 )
#Convert to wide format for easier viewing
data.wide <- cast(df, Region ~ year, mean, value="Emissions")
writeData( data.wide, "DIAG_OUT", paste0('summary-plots/',em ,'_emissions_scaled_by_region'), meta = FALSE )

#----
# Default Emissions
if ( PRINT_DEFAULTS ){
	Regions<-ddply(DefaultEmissions.long, .(Region,year),summarize,
				   Emissions=sum(Emissions, na.rm=TRUE))

	df <- Regions
	df$Region <- factor(df$Region,levels=c('North America','Western Europe',
											'Eastern Europe', 'FSU','Middle East',
										   'Africa','Other Asia','China','Central and South America',
										   'South East Asia and Aust/NZ'))
	df <- df[-which(is.na(df$Region)),]
	plot <- ggplot(df, aes(x=year,y=Emissions,
							fill=Region)) + 
	  geom_area(size=1) +
	  scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010))+
	  scale_y_continuous(labels = comma)+
	  ggtitle( paste('Global Default',em,' Emissions') )+
	  labs(x='Year',y= paste(em,'Emissions [kt]') )
	plot              
	ggsave( paste0('summary-plots/',em,'_regions.default.pdf') , width = 11, height = 6)
}
##line graphs by region
#Scaled Emissions
Regions<-ddply(TotalEmissions.long, .(Region,year),summarize,
               Emissions=sum(Emissions, na.rm=TRUE))

df <- Regions
	df$Region <- factor(df$Region,levels=c('North America','Western Europe',
											'Eastern Europe', 'FSU','Middle East',
										   'Africa','Other Asia','China','Central and South America',
										   'South East Asia and Aust/NZ'))
df <- df[-which(is.na(df$Region)),]
plot <- ggplot(df, aes(x=year,y=Emissions,
                       color=Region)) + 
  geom_line(size=1) +
  scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010))+
  scale_y_continuous(labels = comma)+
  ggtitle( paste('Total Scaled',em,' Emissions') )+
  labs(x='Year',y= paste(em,'Emissions [kt]') )
plot              
ggsave( paste0('summary-plots/',em,'_regions.scaled.line.pdf') , width = 11, height = 6)
#----
# Default Emissions
if ( PRINT_DEFAULTS ){
	Regions<-ddply(DefaultEmissions.long, .(Region,year),summarize,
				   Emissions=sum(Emissions, na.rm=TRUE))

	df <- Regions
	df$Region <- factor(df$Region,levels=c('North America','Western Europe',
											'Eastern Europe', 'FSU','Middle East',
										   'Africa','Other Asia','China','Central and South America',
										   'South East Asia and Aust/NZ'))
	df <- df[-which(is.na(df$Region)),]
	plot <- ggplot(df, aes(x=year,y=Emissions,
							color=Region)) + 
	  geom_line(size=1) +
	  scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010))+
	  scale_y_continuous(labels = comma)+
	  ggtitle( paste('Total Default',em,' Emissions') ) +
	  labs(x='Year',y= paste(em,'Emissions [kt]') )
	plot              
	ggsave( paste0('summary-plots/',em,'_regions.default.line.pdf') , width = 11, height = 6)
}

# ---------------------------------------------------------------------------
# 1. Plots - #Stacked Area Plot By Sector

#Scaled
Sectors<-ddply(TotalEmissions.long, .(sector,year),summarize,
               Emissions=sum(Emissions, na.rm=TRUE))

df <- Sectors
plot <- ggplot(df, aes(x=year,y=Emissions, fill=sector)) + 
  geom_area(size=1) +
  scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010))+
  scale_y_continuous(labels = comma)+
  ggtitle(paste('Global Scaled', em ,'Emissions'))+
  labs(x='Year',y= paste(em,'Emissions [kt]') )+
  guides(fill=guide_legend(ncol=2))
plot              
ggsave( paste0('summary-plots/',em,'_sectors.scaled.pdf'), width = 11, height = 6 )
#Convert to wide format for easier viewing
data.wide <- cast(df, sector ~ year, mean, value="Emissions")
writeData( data.wide, "DIAG_OUT", paste0('summary-plots/',em ,'_emissions_scaled_by_sector') )

#Default
if ( PRINT_DEFAULTS ){
  Sectors<-ddply(DefaultEmissions.long, .(sector,year),summarize,
               Emissions=sum(Emissions, na.rm=TRUE))

df <- Sectors
plot <- ggplot(df, aes(x=year,y=Emissions,
                        fill=sector)) + 
  geom_area(size=1) +
  scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010))+
  scale_y_continuous(labels = comma)+
  ggtitle(paste('Global Default', em ,'Emissions'))+
  labs(x='Year',y= paste(em,'Emissions [kt]') )+
  guides(fill=guide_legend(ncol=2))
  
plot              
ggsave( paste0('summary-plots/',em,'_sectors.default.pdf'), width = 11, height = 6 )
}


# ---------------------------------------------------------------------------
# 1. Plots - #Stacked Area Plot By Fuel

# Scaled
Fuels<-ddply(TotalEmissions.long, .(fuel,year),summarize,
             Emissions=sum(Emissions, na.rm=TRUE))

df <- Fuels
plot <- ggplot(df, aes(x=year,y=Emissions,
                        fill=fuel)) + 
  geom_area(size=1) +
  scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010))+
  scale_y_continuous(labels = comma)+
  ggtitle(paste('Global Scaled', em ,'Emissions'))+
  labs(x='Year',y= paste(em,'Emissions [kt]') )
plot              
ggsave( paste0('summary-plots/',em,'_fuel.scaled.pdf'), width = 11, height = 6 )
#Convert to wide format for easier viewing
data.wide <- cast(df, fuel ~ year, mean, value="Emissions")
writeData( data.wide, "DIAG_OUT", paste0('summary-plots/',em ,'_emissions_scaled_by_fuel'), meta = FALSE )

# Default
if ( PRINT_DEFAULTS ){
Fuels<-ddply(DefaultEmissions.long, .(fuel,year),summarize,
             Emissions=sum(Emissions, na.rm=TRUE))

df <- Fuels
plot <- ggplot(df, aes(x=year,y=Emissions,
                      fill=fuel)) + 
  geom_area(size=1) +
  scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010))+
  scale_y_continuous(labels = comma)+
  ggtitle(paste('Global Default', em ,'Emissions'))+
  labs(x='Year',y= paste(em,'Emissions [kt]') )
plot              
ggsave( paste0('summary-plots/',em,'_fuel.default.pdf'), width = 11, height = 6 )
}










