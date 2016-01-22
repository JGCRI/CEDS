# ------------------------------------------------------------------------------
# Program Name: country-plots.R
# Author: Rachel Hoesly, Ryan Bolt
# Date Last Updated: Jan 22, 2016
# Program Purpose: Produces diagnostic summary figures of default and 
#                  scaled emissions for specific country and specific emission type
# Input Files: F.em_scaled_emissions, D.em_default_emissions
#               
# Output Files: figures in the diagnostic-output/country-plots
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

# ----------------------------------------------------------------------------
# Local Settings
# 0.5 Load Packages

library('ggplot2')
library('plyr')
library('scales')

# --------------- 0.5. Script Options  ---------------------------------------
print_defaults <- "True"


# --------------- 1. Load Country List --------------------------------------
Master_Country_List <- readData( "MAPPINGS", "Master_Country_List")
setwd('../diagnostic-output')

# ---------------------------------------------------------------------------
# Emissions
emissions <- c("SO2", "NOx")
for(emiss in seq_along(emissions)){
  em <- emissions[emiss]
# Reading in the Emissions. This is different for all species
  TotalEmissions <- readData('MED_OUT', paste0('F.',em,'_scaled_emissions'))
  if (print_defaults) DefaultEmissions <- readData('MED_OUT', paste0('D.',em,'_default_total_emissions'))
# ----------------------------------------------------------------------------
# Finding Scalled Emissions for specific country
  isos <- c("arg","kor","jpn","chn")

  for(iso in seq_along(isos)){
    CountryEmissions <- TotalEmissions[TotalEmissions$iso == isos[iso],]

    country <- Master_Country_List[(Master_Country_List$iso == isos[iso]),]
    Full_Name <- country$IEAName

# ---------------------------------------------------------------------------
# Data Problems

# 2. Prepare Data for GGplot
    x_years<-paste('X',1960:2013,sep="")

    CountryEmissions.long <- melt(CountryEmissions,id.vars = c('iso','sector','fuel','units'))
    names(CountryEmissions.long) <- c('iso','sector','fuel','units','year','Emissions')

    CountryEmissions.long$iso<-tolower(CountryEmissions.long$iso)
    CountryEmissions.long$year<-substr(CountryEmissions.long$year,2,6)

    CountryEmissions.long$Region <- Master_Country_List[match(CountryEmissions.long$iso,Master_Country_List$iso),'Figure_Region']
    CountryEmissions.long$Country <- Master_Country_List[match(CountryEmissions.long$iso,Master_Country_List$iso),'Country_Name']

    CountryEmissions.long$Region <- as.factor(CountryEmissions.long$Region)
    CountryEmissions.long$year <- as.integer(CountryEmissions.long$year)
# -----------------------------------------------------------
# Removing Sectors that are zeros
    sectors <- unique(CountryEmissions.long$sector)

    for(secs in seq_along(sectors)) {
      is.emis <- 0
      CE.LL <- CountryEmissions.long[CountryEmissions.long$sector == sectors[secs],]
      is.emis <- sum(CE.LL$Emissions)
      if(is.emis == 0){
        CountryEmissions.long <- CountryEmissions.long[!(CountryEmissions.long$sector == sectors[secs]),]
      }
    }

#-----
    if (print_defaults){
      DefaultEmissions.long <- melt(DefaultEmissions,id.vars = c('iso','sector','fuel','units'))
      names(DefaultEmissions.long) <- c('iso','sector','fuel','units','year','Emissions')
  
      DefaultEmissions.long$iso<-tolower(DefaultEmissions.long$iso)
      DefaultEmissions.long$year<-substr(DefaultEmissions.long$year,2,6)
  
      DefaultEmissions.long$Region <- Master_Country_List[match(DefaultEmissions.long$iso,Master_Country_List$iso),'Figure_Region']
  
      DefaultEmissions.long$Region <- as.factor(DefaultEmissions.long$Region)
      DefaultEmissions.long$year <- as.integer(DefaultEmissions.long$year)
    }
# ---------------------------------------------------------------------------
# 3. Tables - Total emissions by country

#Scaled Emissions
    Em_by_Country<-ddply(CountryEmissions.long, .(Country,year,iso),summarize,
                     Emissions=sum(Emissions, na.rm=TRUE))

#Convert to wide format for writeout
    data.long <- cast(Em_by_Country, Country+iso ~ year , mean, value="Emissions")
    writeData( data.long, "DIAG_OUT", paste0('country-plots/',em ,isos[iso],'_emissions_scaled_by_country') )

# 3. Tables - Total emissions by country and fuel

#Scaled Emissions
    Em_by_Country<-ddply(CountryEmissions.long, .(Country, year, iso, fuel),summarize,
                     Emissions=sum(Emissions, na.rm=FALSE))

#Convert to wide format for writeout
    data.long <- cast(Em_by_Country, Country+iso+fuel ~ year , mean, value="Emissions")
    writeData( data.long, "DIAG_OUT", paste0('country-plots/',em ,isos[iso],'_emissions_scaled_by_country_fuel') )

# ---------------------------------------------------------------------------
# 4. Plots - #Line Plot By Sector

#Scaled
    Sectors<-ddply(CountryEmissions.long, .(sector,year),summarize,
               Emissions=sum(Emissions, na.rm=TRUE))

    df <- Sectors
    plot <- ggplot(df, aes(x=year,y=Emissions, color=sector)) + 
      geom_line(size=1) +
      scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010))+
      scale_y_continuous(labels = comma)+
      ggtitle(paste(Full_Name,'_', em ,'Emissions'))+
      labs(x='Year',y= paste(em,'Emissions [kt]') )+
      guides(fill=guide_legend(ncol=2))
    plot              
    ggsave( paste0('country-plots/',em,'_',isos[iso],'_sectors.scaled.pdf'), width = 11, height = 6 )
#Convert to wide format for easier viewing
    data.long <- cast(df, sector ~ year, mean, value="Emissions")
    writeData( data.long, "DIAG_OUT", paste0('country-plots/',em,'_',isos[iso],'_emissions_scaled_by_sector') )


#Default
    if (print_defaults){
      Sectors<-ddply(DefaultEmissions.long, .(sector,year),summarize,
                 Emissions=sum(Emissions, na.rm=TRUE))
  
      df <- Sectors
      plot <- ggplot(df, aes(x=year,y=Emissions,
                         color=sector)) + 
        geom_line(size=1) +
        scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010))+
        scale_y_continuous(labels = comma)+
        ggtitle(paste(Full_Name,'_', em ,'Emissions'))+
        labs(x='Year',y= paste(em,'Emissions [kt]') )+
        guides(fill=guide_legend(ncol=2))
  
      plot              
      ggsave( paste0('country-plots/',em,'_',isos[iso],'_sectors.default.pdf'), width = 11, height = 6 )
    }


# ---------------------------------------------------------------------------
# 5. Plots - #Line Plot By Fuel

# Scaled
    Fuels<-ddply(CountryEmissions.long, .(fuel,year),summarize,
                Emissions=sum(Emissions, na.rm=TRUE))

    df <- Fuels
    plot <- ggplot(df, aes(x=year,y=Emissions,
                       color=fuel)) + 
      geom_line(size=1) +
      scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010))+
      scale_y_continuous(labels = comma)+
      ggtitle(paste(Full_Name,'_', em ,'Emissions'))+
      labs(x='Year',y= paste(em,'Emissions [kt]') )+
     guides(fill=guide_legend(ncol=2))
    plot              
    ggsave( paste0('country-plots/',em,'_',isos[iso],'_fuel.scaled.pdf'), width = 11, height = 6 )
#Convert to wide format for easier viewing
    data.long <- cast(df, fuel ~ year, mean, value="Emissions")
    writeData( data.long, "DIAG_OUT", paste0('country-plots/',em ,'_',isos[iso],'_emissions_scaled_by_fuel') )

# Default
    if (print_defaults){
      Fuels<-ddply(DefaultEmissions.long, .(fuel,year),summarize,
                   Emissions=sum(Emissions, na.rm=TRUE))
  
      df <- Fuels
     plot <- ggplot(df, aes(x=year,y=Emissions,
                         color=fuel)) + 
        geom_line(size=1) +
        scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010))+
        scale_y_continuous(labels = comma)+
        ggtitle(paste(Full_Name,'_', em ,'Emissions'))+
        labs(x='Year',y= paste(em,'Emissions [kt]') )
        plot              
        ggsave( paste0('country-plots/',em,'_',isos[iso],'_fuel.default.pdf'), width = 11, height = 6 )
    }

  } # Country Loop
} # Emission Loop