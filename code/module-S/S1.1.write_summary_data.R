# ------------------------------------------------------------------------------
# Program Name: S1.1.write_summary_data.R
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
log_msg <- "Writes Final summary data" # First message to be printed to the log
script_name <- "S1.1.write_summary_data.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "NH3"

# ---------------------------------------------------------------------------
# 0.5 Load Packages


# ---------------------------------------------------------------------------
# 0.5. Script Options

write_years <- 1970:end_year

# ---------------------------------------------------------------------------
# 1. Load files

Master_Country_List <- readData( "MAPPINGS", "Master_Country_List")
final_emissions_read <- readData('MED_OUT', paste0('F.',em,'_scaled_emissions'))
Master_Sector_Level_map <- readData(domain = 'MAPPINGS', file_name = 'Master_Sector_Level_map')

# ---------------------------------------------------------------------------
# Data processing

X_write_years <- paste0('X',write_years)
final_emissions <- final_emissions_read[,c('iso','sector','fuel','units',X_write_years)]
final_emissions$em <- em

# remove international shipping and aviation emissions
final_emissions <- final_emissions[-which(final_emissions$sector == "1A3di_International-shipping" ),]
final_emissions <- final_emissions[-which(final_emissions$sector == "1A3ai_International-aviation" ),]
final_emissions <- final_emissions[-which(final_emissions$sector == "1A3aii_Domestic-aviation" ),]

# add summary sectors
final_emissions$summary_sector <- Master_Sector_Level_map[match(final_emissions$sector,
                                  Master_Sector_Level_map$working_sectors_v1),'aggregate_sectors']
#simplify units
# This assumes units are correct. Need to add a more comprehensive unit conversion function (since drivers will come in various units)
final_emissions$units <- 'kt'

# reorder columns
final_emissions <- final_emissions[,c("iso","summary_sector","fuel","em","units",X_write_years)]


# ---------------------------------------------------------------------------
# 1. Write Tables

FILENAME_POSTSCRIPT = "_FOR-REVIEW-ONLY"

#Total emissions by Country
Em_by_Country<-aggregate(final_emissions[X_write_years],
                         by=list(iso=final_emissions$iso,
                                 em= final_emissions$em,
                                 units=final_emissions$units),sum )

#Sort and writeout
Em_by_Country <- Em_by_Country[ with( Em_by_Country, order( iso ) ), ]
writeData( Em_by_Country, "FIN_OUT", paste0(em ,'_emissions_by_country',FILENAME_POSTSCRIPT), meta=FALSE )

#Total emissions by fuel
Summary_Emissions <- aggregate(final_emissions[X_write_years],
                               by=list(fuel=final_emissions$fuel,
                                       em= final_emissions$em,
                                       units=final_emissions$units),sum )

#Sort and writeout
Summary_Emissions <- Summary_Emissions[ with( Summary_Emissions, order( fuel ) ), ]
writeData( Summary_Emissions, "FIN_OUT", paste0(em ,'_global_emissions_by_fuel',FILENAME_POSTSCRIPT), meta=FALSE )

# Total Emissions by Sector and Country
Em_by_Country_Sector <- aggregate(final_emissions[X_write_years],
                               by=list(iso=final_emissions$iso,
                                       sector=final_emissions$summary_sector,
                                       em= final_emissions$em,
                                       units=final_emissions$units),sum )
#Sort and writeout
Em_by_Country_Sector <- Em_by_Country_Sector[ with( Em_by_Country_Sector, order( iso , sector ) ), ]
writeData( Em_by_Country_Sector, "FIN_OUT", paste0(em ,'_emissions_by_country_sector',FILENAME_POSTSCRIPT), meta=FALSE )

logStop()

# END










