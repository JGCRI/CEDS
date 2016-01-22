# ------------------------------------------------------------------------------
# Program Name: Figures.R
# Author: Rachel Hoesly, Steve Smith
# Program Purpose: Produces summary output
#               
# Output Files: data in final-emissions folder
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



# ---------------------------------------------------------------------------
# 1. Load files

Master_Country_List <- readData( "MAPPINGS", "Master_Country_List")
TotalEmissions <- readData('MED_OUT', paste0('F.',em,'_scaled_emissions'))
Master_Sector_Level_map <- readData(domain = 'MAPPINGS', file_name = 'Master_Sector_Level_map')

# ---------------------------------------------------------------------------
# Data processing

#TODO: only write out data for these years
x_years<-paste('X',1970:2014,sep="")

TotalEmissions.long <- melt(TotalEmissions,id.vars = c('iso','sector','fuel','units'))
names(TotalEmissions.long) <- c('iso','sector','fuel','units','year','Emissions')

TotalEmissions.long$iso<-tolower(TotalEmissions.long$iso)
TotalEmissions.long$year<-substr(TotalEmissions.long$year,2,6)

TotalEmissions.long$Country <- Master_Country_List[match(TotalEmissions.long$iso,Master_Country_List$iso),'Country_Name']

TotalEmissions.long$Summary_Sector <- Master_Sector_Level_map[match(TotalEmissions.long$sector,Master_Sector_Level_map$working_sectors_v1),'aggregate_sectors']

TotalEmissions.long$year <- as.integer(TotalEmissions.long$year)

TotalEmissions.long$em <- em

# ---------------------------------------------------------------------------
# 1. Write Tables

#TODO: Remove 1960-1969 emissions
#TODO: Remove NA sector
#TODO: Remove international shipping and all aircraft emissions from totals (If remove NA summary sector this will do that)

#Total emissions by Country
Em_by_Country<-ddply(TotalEmissions.long, .(iso, Country,year,em),summarize,
               Emissions=sum(Emissions, na.rm=TRUE))

#Convert to wide format for writeout
data.wide <- cast(Em_by_Country, Country+iso+em ~ year , mean, value="Emissions")
writeData( data.wide, "FIN_OUT", paste0(em ,'_emissions_by_country') )

#Total emissions by fuel
Summary_Emissions<-ddply(TotalEmissions.long, .(year,em, iso, fuel),summarize,
                     Emissions=sum(Emissions, na.rm=FALSE))

#Convert to wide format for writeout
data.wide <- cast(Summary_Emissions, fuel+em ~ year , mean, value="Emissions")
writeData( data.wide, "FIN_OUT", paste0(em ,'_global_emissions_by_fuel') )

# Total Emissions by Sector and Country
Em_by_Country_Sector<-ddply(TotalEmissions.long, .(Country, iso, year,em, Summary_Sector),summarize,
                     Emissions=sum(Emissions, na.rm=FALSE))

#Convert to wide format for writeout
data.wide <- cast(Em_by_Country_Sector, Country+iso+Summary_Sector+em ~ year , mean, value="Emissions")
writeData( data.wide, "FIN_OUT", paste0(em ,'_emissions_by_country_sector') )


logStop()

# END










