# ------------------------------------------------------------------------------
# Program Name: Figures.R
# Author: Rachel Hoesly, Huong Nguyen
# Date Last Updated: October 07 2016
# Program Purpose: Produces diagnostic summary figures of default and
#                  scaled emissions
# Input Files: F.em_scaled_emissions, D.em_default_emissions
#
# Output Files: figures in the diagnostic-output
# TODO:
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R",'process_db_functions.R',
              'common_data.R', 'IO_functions.R', 'data_functions.R', 'timeframe_functions.R') # Additional function files may be required.
log_msg <- "Figures" # First message to be printed to the log
script_name <- "Figures.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "BC"

# ---------------------------------------------------------------------------
# 0.5 Load Packages

library('ggplot2')
library('plyr')
library('scales')

# ---------------------------------------------------------------------------
# 0.5. Script Options

PRINT_DEFAULTS <- FALSE
PRINT_BIG_TABLES <- TRUE


# ---------------------------------------------------------------------------
# 1. Load files

Master_Country_List <- readData( "MAPPINGS", "Master_Country_List")
MSLevel <- readData( "MAPPINGS", "Master_Sector_Level_map" )
TotalEmissions <- readData('MED_OUT', paste0(em,'_total_CEDS_emissions'))

ActivityData <- readData('MED_OUT', paste0('H.',em,'_total_activity_extended'))
MasterSectorList <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Sectors" , meta = F)

if ( PRINT_DEFAULTS ) DefaultEmissions <- readData('MED_OUT', paste0('D.',em,'_default_total_emissions'))


# ---------------------------------------------------------------------------
# Data Processing

# 1. Prepare Data
start <- 1750
end <- end_year
x_years<-paste('X',start:end,sep="")

TotalEmissions <- TotalEmissions[,c('iso','sector','fuel','units',x_years)]

TotalEmissions.long <- melt(TotalEmissions,id.vars = c('iso','sector','fuel','units'))
names(TotalEmissions.long) <- c('iso','sector','fuel','units','year','Emissions')

TotalEmissions.long$iso<-tolower(TotalEmissions.long$iso)
TotalEmissions.long$year<-substr(TotalEmissions.long$year,2,6)

TotalEmissions.long$Region <- Master_Country_List[match(TotalEmissions.long$iso,Master_Country_List$iso),'Figure_Region']
TotalEmissions.long$Country <- Master_Country_List[match(TotalEmissions.long$iso,Master_Country_List$iso),'Country_Name']

TotalEmissions.long$Region <- as.factor(TotalEmissions.long$Region)
TotalEmissions.long$year <- as.integer(TotalEmissions.long$year)

TotalEmissions.long$agg_Sector <- MSLevel[match(TotalEmissions$sector,MSLevel$working_sectors_v1),'FSU_extension']
TotalEmissions.long[which(is.na(TotalEmissions.long$agg_Sector)),'agg_Sector'] <- "Process"
TotalEmissions.long$agg_Sector <- as.factor(TotalEmissions.long$agg_Sector)

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

  DefaultEmissions.long$agg_Sector <- MSLevel[match(DefaultEmissions$sector,Master_Country_List$sector),'Figure_Region']


	# Also add to the long format to use aggregate
	DefaultEmissions$Region <- Master_Country_List[match(DefaultEmissions$iso,Master_Country_List$iso),'Figure_Region']
	DefaultEmissions$Country <- Master_Country_List[match(DefaultEmissions$iso,Master_Country_List$iso),'Country_Name']
}

combustion_sectors <- MasterSectorList[ which( MasterSectorList$type == 'comb' ), 'sector' ]
Energy_Data <- ActivityData[ which( ActivityData$sector %in% combustion_sectors ), ]
Energy_Data$Country <- Master_Country_List[ match( Energy_Data$iso, Master_Country_List$iso ), 'Country_Name' ]

Energy_country<-aggregate( Energy_Data[ x_years ],
                           by=list( Country = Energy_Data$Country,
                                    iso = Energy_Data$iso,
                                    fuel = Energy_Data$fuel ),sum )

writeData( Energy_country, "DIAG_OUT", paste0( 'summary-plots/', 'Energy_data_contry_fuel' ), meta = FALSE )

Energy_sector<-aggregate( Energy_Data[ x_years ],
                           by=list( Country = Energy_Data$Country,
                                    iso = Energy_Data$iso,
                                    sector = Energy_Data$sector ),sum )

writeData( Energy_sector, "DIAG_OUT", paste0( 'summary-plots/', 'Energy_data_contry_sector' ), meta = FALSE )

Energy_global_fuel<-aggregate( Energy_country[ x_years ],
                           by=list( fuel = Energy_country$fuel ),sum )

writeData( Energy_global_fuel, "DIAG_OUT", paste0( 'summary-plots/', 'Energy_data_global_fuel' ), meta = FALSE )

Energy_global_fuel_sector<-aggregate( Energy_Data[ x_years ],
                           by=list( Country = Energy_Data$Country,
                                    iso = Energy_Data$iso,
                                    sector = Energy_Data$sector,
                                    fuel = Energy_Data$fuel ),sum )

writeData( Energy_global_fuel_sector, "DIAG_OUT", paste0( 'summary-plots/', 'Energy_data_global_fuel_sector' ), meta = FALSE )



# ---------------------------------------------------------------------------
# 0. Tables -
# Total scaled emissions by country

Em_by_Country<-aggregate( TotalEmissions[ x_years ],
                         by=list( Region =  TotalEmissions$Region,
                                  Country =  TotalEmissions$Country,
                                  iso = TotalEmissions$iso),sum )

writeData( Em_by_Country, "DIAG_OUT", paste0('summary-plots/',em ,'_emissions_scaled_by_country'), meta = FALSE )

if ( PRINT_DEFAULTS ){
  Em_by_Country<-aggregate( DefaultEmissions[ x_years ],
                            by=list( Region =  DefaultEmissions$Region,
                                     Country =  DefaultEmissions$Country,
                                     iso = DefaultEmissions$iso ),sum )

	writeData( Em_by_Country, "DIAG_OUT", paste0('summary-plots/',em ,'_emissions_default_by_country'), meta = FALSE )
}

# Total emissions by country and fuel

Em_by_Country<-aggregate( TotalEmissions[ x_years ],
                           by=list( Region =  TotalEmissions$Region,
                                    Country =  TotalEmissions$Country,
                                    iso = TotalEmissions$iso,
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

Em_by_Region<-aggregate( TotalEmissions[ x_years ],
                          by=list( Region =  TotalEmissions$Region,
                                   fuel =  TotalEmissions$fuel),sum )

writeData( Em_by_Region, "DIAG_OUT", paste0('summary-plots/',em ,'_emissions_scaled_by_region_fuel'), meta = FALSE )

# Global emissions by CEDS sector, and fuel

Em_by_CEDS_Sector_Fuel<-aggregate( TotalEmissions[ x_years ],
								   by=list( sector =  TotalEmissions$sector,
							 			   fuel =  TotalEmissions$fuel),sum )

writeData( Em_by_CEDS_Sector_Fuel, "DIAG_OUT", paste0('summary-plots/',em ,'_gbl_emissions_scaled_by_CEDS-sector_fuel'), meta = FALSE )


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

plot <- ggplot(df, aes(x=year,y=Emissions, fill=Region)) +
               geom_area(size=1) +
        annotate("text", x = 2000, y = -80, label = version_stamp, size = 3) +
        theme( plot.title = element_text(vjust=0.8),
               axis.title.x = element_text(vjust=0.3), axis.title.y = element_text(vjust=0.3),
               legend.position=c( 0.12, 0.69 ) ) +
        theme( legend.background =element_rect(color="black"), legend.key = element_blank() ) +
                scale_x_continuous(breaks=seq(start,end, 20))+
                scale_y_continuous(labels = comma)+
                ggtitle( paste('Global ',em,' Emissions') )+
                labs(x='Year',y= paste(em,'Emissions [kt]') )

savePlot("DIAG_OUT", "summary-plots/", paste0(em, '_regions.scaled'),
         width = 11, height = 6, plot = plot)

#Convert to wide format for easier viewing
data.wide <- cast(df, Region ~ year, mean, value="Emissions")
writeData( data.wide, "DIAG_OUT", paste0('summary-plots/',em ,'_emissions_scaled_by_region'), meta = FALSE )

#----
# Default Emissions
if ( PRINT_DEFAULTS ){
	Regions<-ddply(DefaultEmissions.long, .(Region,year),summarize,
				   Emissions=sum(Emissions, na.rm=TRUE))

	df <- Regions
	df <- df[-which(is.na(df$Region)),]
	plot <- ggplot(df, aes(x=year,y=Emissions, fill=Region)) +
	  geom_area(size=1) +
	  annotate("text", x = 2000 , y = -80, label = version_stamp, size = 3 ) +
	  scale_x_continuous(breaks=seq(start,end, 20))+
	  scale_y_continuous(labels = comma)+
	  ggtitle( paste('Global Default',em,' Emissions ') ) +
	  labs(x='Year',y= paste(em,'Emissions [kt]') )
  	savePlot("DIAG_OUT", "summary-plots/", paste0(em, '_regions.default'),
  	         width = 11, height = 6, plot = plot)
}

##line graphs by region
#Scaled Emissions
Regions<-ddply(TotalEmissions.long, .(Region,year),summarize,
               Emissions=sum(Emissions, na.rm=TRUE))

df <- Regions
plot <- ggplot(df, aes(x=year,y=Emissions,
                       color=Region)) +
  geom_line(size=1) +
  annotate("text", x = 2000, y = -80, label = version_stamp, size = 3) + scale_x_continuous(breaks=seq(start,end, 20))+
  scale_y_continuous(labels = comma)+
  ggtitle( paste('Total Scaled',em,' Emissions') )+
  labs(x='Year',y= paste(em,'Emissions [kt]') )
savePlot("DIAG_OUT", "summary-plots/", paste0(em, '_regions.scaled.line'),
         width = 11, height = 6, plot = plot)

#----
# Default Emissions
if ( PRINT_DEFAULTS ){
	Regions<-ddply(DefaultEmissions.long, .(Region,year),summarize,
				   Emissions=sum(Emissions, na.rm=TRUE))

	df <- Regions
	df <- df[-which(is.na(df$Region)),]
	plot <- ggplot(df, aes(x=year,y=Emissions,
							color=Region)) +
	  annotate("text", x = 2000, y = -70, label = version_stamp, size = 3) +
	  geom_line(size=1) +
	  scale_x_continuous(breaks=seq(start,end, 20))+
	  scale_y_continuous(labels = comma)+
	  ggtitle( paste('Total Default',em,' Emissions') ) +
	  labs(x='Year',y= paste(em,'Emissions [kt]') )
	savePlot("DIAG_OUT", "summary-plots/", paste0(em, '_regions.default.line'),
         width = 11, height = 6, plot = plot)

}

# ---------------------------------------------------------------------------
# 1. Plots - #Stacked Area Plot By aggregate Sector

#Scaled
Sectors<-ddply(TotalEmissions.long, .(agg_Sector,year),summarize,
               Emissions=sum(Emissions, na.rm=TRUE))

df <- Sectors
plot <- ggplot(df, aes(x=year,y=Emissions, fill=agg_Sector)) +
  geom_area(size=1) +
  annotate("text", x = 2000, y = -80, label = version_stamp, size=3) +
  theme( plot.title = element_text(vjust=0.8),
         axis.title.x = element_text(vjust=0.3), axis.title.y = element_text(vjust=0.3),
         legend.position=c( 0.08, 0.78 ) ) +
  theme( legend.background =element_rect(color="black"), legend.key = element_blank() ) +
  scale_x_continuous(breaks=seq(start,end, 20))+
  scale_y_continuous(labels = comma)+
  ggtitle(paste('Global ', em ,'Emissions'))+
  labs(x='Year',y= paste(em,'Emissions [kt]') )+
  guides(fill=guide_legend(ncol=1) ) + labs(fill="Sector")
savePlot("DIAG_OUT", "summary-plots/", paste0(em, '_agg_sectors.scaled'),
         width = 11, height = 6, plot = plot)


#Convert to wide format for easier viewing
data.wide <- cast(df, agg_Sector ~ year, mean, value="Emissions")
writeData( data.wide, "DIAG_OUT", paste0('summary-plots/',em ,'_emissions_scaled_by_agg_sector') )

plot <- ggplot(df, aes(x=year,y=Emissions, color=agg_Sector)) +
  geom_line(size=1) +
  annotate("text", x = 2000, y = -80, label = version_stamp, size=3) +
  scale_x_continuous(breaks=seq(start,end, 20))+
  scale_y_continuous(labels = comma)+
  ggtitle(paste('Global Scaled', em ,'Emissions'))+
  labs(x='Year',y= paste(em,'Emissions [kt]') )+
  guides(fill=guide_legend(ncol=1))
savePlot("DIAG_OUT", "summary-plots/", paste0(em, '_agg_sectors_line.scaled'),
         width = 11, height = 6, plot = plot)

#Default
if ( PRINT_DEFAULTS ){
  Sectors<-ddply(DefaultEmissions.long, .(agg_Sector,year),summarize,
                 Emissions=sum(Emissions, na.rm=TRUE))

  df <- Sectors
  plot <- ggplot(df, aes(x=year,y=Emissions,
                         fill=agg_Sector)) +
    geom_area(size=1) +
    annotate("text", x = 2000, y = -80, label = version_stamp, size = 3) +
    scale_x_continuous(breaks=seq(start,end, 20))+
    scale_y_continuous(labels = comma)+
    ggtitle(paste('Global Default', em ,'Emissions'))+
    labs(x='Year',y= paste(em,'Emissions [kt]') )+
    guides(fill=guide_legend(ncol=2))

  savePlot("DIAG_OUT", "summary-plots/", paste0(em, '_agg_sectors.default'),
         width = 11, height = 6, plot = plot)
}

# ---------------------------------------------------------------------------
# 1. Plots - #Stacked Area Plot By Fuel

# Scaled
Fuels<-ddply(TotalEmissions.long, .(fuel,year),summarize,
             Emissions=sum(Emissions, na.rm=TRUE))

df <- Fuels
plot <- ggplot(df, aes(x=year,y=Emissions, fill=fuel)) +
  geom_area(size=1) +
  annotate("text", x = 2000, y = -80, label = version_stamp, size = 3) +
  scale_x_continuous(breaks=seq(start,end, 20))+
  scale_y_continuous(labels = comma)+
  ggtitle(paste('Global Scaled', em ,'Emissions'))+
  labs(x='Year',y= paste(em,'Emissions [kt]') )
savePlot("DIAG_OUT", "summary-plots/", paste0(em, '_fuel.scaled_', version_stamp),
         width = 11, height = 6, plot = plot)

#Convert to wide format for easier viewing
data.wide <- cast(df, fuel ~ year, mean, value="Emissions")
writeData( data.wide, "DIAG_OUT", paste0('summary-plots/',em ,'_emissions_scaled_by_fuel_', version_stamp), meta = FALSE )

# Default
if ( PRINT_DEFAULTS ){
Fuels<-ddply(DefaultEmissions.long, .(fuel,year),summarize,
             Emissions=sum(Emissions, na.rm=TRUE))

df <- Fuels
plot <- ggplot(df, aes(x=year,y=Emissions, fill=fuel)) +
  geom_area(size=1) +
  annotate("text", x = 2000, y = -80, label = version_stamp, size = 3) +
  scale_x_continuous(breaks=seq(start,end, 20))+
  scale_y_continuous(labels = comma)+
  ggtitle(paste('Global Default', em ,'Emissions'))+
  labs(x='Year',y= paste(em,'Emissions [kt]') )
savePlot("DIAG_OUT", "summary-plots/", paste0(em, '_fuel.default'),
         width = 11, height = 6, plot = plot)
}

# ---------------------------------------------------------------------------

logStop()
