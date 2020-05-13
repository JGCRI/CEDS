# ------------------------------------------------------------------------------
# Program Name: country-plots.R
# Author: Rachel Hoesly, Ryan Bolt
# Date Last Updated: March 20, 2020
# Program Purpose: Produces diagnostic summary figures of default and
#                  scaled emissions for specific country and specific emission type
# Input Files: Master_Country_List.csv, Master_Sector_Level_map.csv,
#              F.[em]_scaled_emissions.csv, D.[em]_default_total_emissions.csv,
#              F.[em]_scaled_EF.csv
# Output Files: [em]_[iso]_emissions_scaled_by_country.csv, [em]_[iso]_emissions_scaled_by_fuel.csv,
#               [em]_[iso]__emissions_scaled_by_fuel_sector.csv, [em]_[iso]__emission-factors_scaled.csv,
#               [em]_[iso]__emissions_scaled_full_detail.csv, [em]_[iso]__emissions_default_full_detail.csv,
#               [em]_[iso]__emissions_scaled_by_sector.csv, [em]_[iso]__emissions_default_by_sector.csv,
#               [em]_[iso]__emissions_scaled_by_fuel.csv, [em]_[iso]__sectors.scaled.pdf,
#               [em]_[iso]__sectors.default.pdf, [em]_[iso]__fuel.scaled.pdf, [em]_[iso]_fuel.default.pdf
# TODO: (Future) tidyverse (cast, match, melt)
# TODO: (Future) Move output to the end of the loop (will require dfs to be renamed,
#                as many are named data.wide)
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R",'process_db_functions.R',
              'common_data.R', 'IO_functions.R', 'data_functions.R', 'timeframe_functions.R') # Additional function files may be required.
log_msg <- paste0( "Producing diagnostic summary figures of default and scaled emissions for ",
                   "specific country and emissions species combinations..." ) # First message to be printed to the log
script_name <- "country-plots.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"

# ----------------------------------------------------------------------------
# Local Settings
# 0.5 Load Packages
library('scales') # TODO: (Future) Include within CEDS global_settings.R packages?

# --------------- 0.5. Script Options  ---------------------------------------
print_defaults <- "True"

# --------------- 1. Load Country List --------------------------------------
Master_Country_List <- readData( "MAPPINGS", "Master_Country_List")
Master_Sector_Level_map <- readData(domain = 'MAPPINGS', file_name = 'Master_Sector_Level_map')

# ---------------------------------------------------------------------------
# Emissions and countries for which to produce plots
emissions <- c("NMVOC", "SO2", "NOx", "CO")
country_list <- c("arg","kor","jpn","chn")

# Years for plots
plot_years <- seq( start_year, 2010, 10 )

# Emissions Loop
for(emiss in seq_along(emissions)){
  em <- emissions[emiss]

# Read in the Emissions.
  TotalEmissions <- readData('MED_OUT', paste0('F.',em,'_scaled_emissions'))
  if (print_defaults) {
    DefaultEmissions <- readData('MED_OUT', paste0('D.',em,'_default_total_emissions'))
  }
  Scaled_Emission_Factors <- readData('MED_OUT', paste0('F.',em,'_scaled_EF'))

# ----------------------------------------------------------------------------
# Extract Scaled Emissions for specific country

  # Country Loop
  for(iso in seq_along(country_list)){

    # Extract emissions for the specified country
    CountryEmissions <- TotalEmissions[TotalEmissions$iso == country_list[iso],]
    if (print_defaults) {
      DefaultCountryEmissions <- DefaultEmissions[DefaultEmissions$iso == country_list[iso],]
    }

    # Also emission factors (for diagnosis)
    Country_Emission_Factors <- Scaled_Emission_Factors[Scaled_Emission_Factors$iso == country_list[iso],]

    country <- Master_Country_List[(Master_Country_List$iso == country_list[iso]),]
    Full_Name <- country$IEAName

# ---------------------------------------------------------------------------
# Data Processing

# 2. Prepare Data for GGplot
    CountryEmissions.long <- melt(CountryEmissions,id.vars = c('iso','sector','fuel','units')) # TODO: (Future) Replace melt with gather
    names(CountryEmissions.long) <- c('iso','sector','fuel','units','year','Emissions')

    CountryEmissions.long$iso<-tolower(CountryEmissions.long$iso)
    CountryEmissions.long$year<-substr(CountryEmissions.long$year,2,6)

    CountryEmissions.long$Region <- Master_Country_List[match(CountryEmissions.long$iso,Master_Country_List$iso),'Figure_Region'] # TODO: (Future) Replace match with left_join
    CountryEmissions.long$Country <- Master_Country_List[match(CountryEmissions.long$iso,Master_Country_List$iso),'Country_Name']  # TODO: (Future) Replace match with left_join
    CountryEmissions.long$Summary_Sector <- Master_Sector_Level_map[match(CountryEmissions.long$sector,Master_Sector_Level_map$working_sectors_v1),'aggregate_sectors']  # TODO: (Future) Replace match with left_join

    CountryEmissions.long$Region <- as.factor(CountryEmissions.long$Region)
    CountryEmissions.long$year <- as.integer(CountryEmissions.long$year)

# Remove Sectors that are zeros
    sectors <- unique(CountryEmissions.long$sector)

    for(secs in seq_along(sectors)) {
      is.emis <- 0
      CE.LL <- CountryEmissions.long[CountryEmissions.long$sector == sectors[secs],]
      is.emis <- sum(CE.LL$Emissions)
      if(is.emis == 0){
        CountryEmissions.long <- CountryEmissions.long[!(CountryEmissions.long$sector == sectors[secs]),]
      }
    }

# -----------------------------------------------------------
    if (print_defaults){
      DefaultCountryEmissions.long <- melt(DefaultCountryEmissions,id.vars = c('iso','sector','fuel','units')) # TODO: (Future) Replace melt with gather
      names(DefaultCountryEmissions.long) <- c('iso','sector','fuel','units','year','Emissions')

      DefaultCountryEmissions.long$iso<-tolower(DefaultCountryEmissions.long$iso)
      DefaultCountryEmissions.long$year<-substr(DefaultCountryEmissions.long$year,2,6)

      DefaultCountryEmissions.long$Region <- Master_Country_List[match(DefaultCountryEmissions.long$iso,Master_Country_List$iso),'Figure_Region'] # TODO: (Future) Replace match with left_join
      DefaultCountryEmissions.long$Country <- Master_Country_List[match(DefaultCountryEmissions.long$iso,Master_Country_List$iso),'Country_Name'] # TODO: (Future) Replace match with left_join
      DefaultCountryEmissions.long$Summary_Sector <- Master_Sector_Level_map[match(DefaultCountryEmissions.long$sector,Master_Sector_Level_map$working_sectors_v1),'aggregate_sectors'] # TODO: (Future) Replace match with left_join

      DefaultCountryEmissions.long$Region <- as.factor(DefaultCountryEmissions.long$Region)
      DefaultCountryEmissions.long$year <- as.integer(DefaultCountryEmissions.long$year)

	# Remove Sectors that are zeros
		sectors <- unique(DefaultCountryEmissions.long$sector)

		for(secs in seq_along(sectors)) {
		  is.emis <- 0
		  CE.LL <- DefaultCountryEmissions.long[DefaultCountryEmissions.long$sector == sectors[secs],]
		  is.emis <- sum(CE.LL$Emissions)
		  if(is.emis == 0){
			DefaultCountryEmissions.long <- DefaultCountryEmissions.long[!(DefaultCountryEmissions.long$sector == sectors[secs]),]
		  }
		}

    }
# ---------------------------------------------------------------------------
# 3. Tables - Total emissions by country

#Scaled Emissions
    Em_by_Country<-ddply(CountryEmissions.long, .(Country,year,iso),summarize,
                     Emissions=sum(Emissions, na.rm=TRUE))
#Convert to wide format for writeout
    data.wide <- cast(Em_by_Country, Country+iso ~ year , mean, value="Emissions") # TODO: (Future) Replace cast with spread
    writeData( data.wide, "DIAG_OUT", paste0('country-sector/',em,"_",country_list[iso],'_emissions_scaled_by_country') )


#Total emissions by country and fuel

#Scaled Emissions
    Em_by_Country<-ddply(CountryEmissions.long, .(Country, year, iso, fuel),summarize,
                     Emissions=sum(Emissions, na.rm=FALSE))
#Convert to wide format for writeout
    data.wide <- cast(Em_by_Country, Country+iso+fuel ~ year , mean, value="Emissions") # TODO: (Future) Replace cast with spread
    writeData( data.wide, "DIAG_OUT", paste0('country-sector/',em,"_",country_list[iso],'_emissions_scaled_by_fuel') )

#Scaled Emissions by fuel and sector
    Em_by_Country<-ddply(CountryEmissions.long, .(Country, year, iso, fuel, Summary_Sector),summarize,
                     Emissions=sum(Emissions, na.rm=FALSE))
#Convert to wide format for writeout
    data.wide <- cast(Em_by_Country, Country+iso+fuel+Summary_Sector ~ year , mean, value="Emissions") # TODO: (Future) Replace cast with spread
    writeData( data.wide, "DIAG_OUT", paste0('country-sector/',em,"_",country_list[iso],'_emissions_scaled_by_fuel_sector') )

#Scaled Emission Factors
    writeData( Country_Emission_Factors, "DIAG_OUT", paste0('country-sector/',em,"_",country_list[iso],'_emission-factors_scaled') )

#Detailed Emissions
    data.wide <- cast(CountryEmissions.long, Country+iso+fuel+sector+Summary_Sector ~ year , mean, value="Emissions") # TODO: (Future) Replace cast with spread
    writeData( data.wide, "DIAG_OUT", paste0('country-sector/',em,"_",country_list[iso],'_emissions_scaled_full_detail') )
    if (print_defaults){
		data.wide <- cast(DefaultCountryEmissions.long, Country+iso+fuel+sector+Summary_Sector ~ year , mean, value="Emissions") # TODO: (Future) Replace cast with spread
		writeData( data.wide, "DIAG_OUT", paste0('country-sector/',em,"_",country_list[iso],'_emissions_default_full_detail') )
	}

# ---------------------------------------------------------------------------
# 4. Plots - #Line Plot By Sector

#Scaled
    Sectors<-ddply(CountryEmissions.long, .(Summary_Sector,year),summarize,
               Emissions=sum(Emissions, na.rm=TRUE))

    df <- Sectors
    plot <- ggplot(df, aes(x=year,y=Emissions, color=Summary_Sector)) +
      geom_line(size=1) +
      scale_x_continuous(breaks=plot_years)+
      scale_y_continuous(labels = comma)+
      ggtitle(paste(Full_Name,'_', em ,'Emissions'))+
      labs(x='Year',y= paste(em,'Emissions [kt]') )+
      guides(fill=guide_legend(ncol=2))
    savePlot('DIAG_OUT', 'country-sector/', paste0(em,'_', country_list[iso], '_sectors.scaled.pdf'),
             width = 11, height = 6, plot = plot)

    #Convert to wide format for easier viewing
    data.wide <- cast(df, Summary_Sector ~ year, mean, value="Emissions") # TODO: (Future) Replace cast with spread
    writeData( data.wide, "DIAG_OUT", paste0('country-sector/',em,'_',country_list[iso],'_emissions_scaled_by_sector') )

#Default
    if (print_defaults){
      Sectors<-ddply(DefaultCountryEmissions.long, .(Summary_Sector,year),summarize,
                 Emissions=sum(Emissions, na.rm=TRUE))

      df <- Sectors
      plot <- ggplot(df, aes(x=year,y=Emissions,
                         color=Summary_Sector)) +
        geom_line(size=1) +
        scale_x_continuous(breaks=plot_years)+
        scale_y_continuous(labels = comma)+
        ggtitle(paste(Full_Name,'_', em ,'Emissions'))+
        labs(x='Year',y= paste(em,'Emissions [kt]') )+
        guides(fill=guide_legend(ncol=2))

      savePlot('DIAG_OUT', 'country-sector/', paste0(em,'_', country_list[iso], '_sectors.default.pdf'),
               width = 11, height = 6, plot = plot)

    #Convert to wide format for easier viewing
    data.wide <- cast(df, Summary_Sector ~ year, mean, value="Emissions") # TODO: (Future) Replace cast with spread
    writeData( data.wide, "DIAG_OUT", paste0('country-sector/',em,'_',country_list[iso],'_emissions_default_by_sector') )

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
      scale_x_continuous(breaks=plot_years)+
      scale_y_continuous(labels = comma)+
      ggtitle(paste(Full_Name,'_', em ,'Emissions'))+
      labs(x='Year',y= paste(em,'Emissions [kt]') )+
     guides(fill=guide_legend(ncol=2))

    savePlot('DIAG_OUT', 'country-sector/', paste0(em,'_', country_list[iso], '_fuel.scaled.pdf'),
             width = 11, height = 6, plot = plot)
#Convert to wide format for easier viewing
    data.wide <- cast(df, fuel ~ year, mean, value="Emissions") # TODO: (Future) Replace cast with spread
    writeData( data.wide, "DIAG_OUT", paste0('country-sector/',em ,'_',country_list[iso],'_emissions_scaled_by_fuel') )

# Default
    if (print_defaults){
      Fuels<-ddply(DefaultCountryEmissions.long, .(fuel,year),summarize,
                   Emissions=sum(Emissions, na.rm=TRUE))

      df <- Fuels
     plot <- ggplot(df, aes(x=year,y=Emissions,
                         color=fuel)) +
        geom_line(size=1) +
        scale_x_continuous(breaks=plot_years)+
        scale_y_continuous(labels = comma)+
        ggtitle(paste(Full_Name,'_', em ,'Emissions'))+
        labs(x='Year',y= paste(em,'Emissions [kt]') )

        savePlot('DIAG_OUT', 'country-sector/', paste0(em,'_', country_list[iso], '_fuel.default.pdf'),
                 width = 11, height = 6, plot = plot)
    }

  } # Country Loop
} # Emission Loop
