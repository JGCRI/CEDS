# ------------------------------------------------------------------------------
# Program Name: country_sector_check.R
# Author(s): Leyang Feng
# Date Last Updated: 19 April 2016
# Program Purpose:       
# Input Files:  
# Output Files: 
# Notes: 
# TODO: 
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Set working directory to the CEDS "input" directory and define PARAM_DIR as the
# location of the CEDS "parameters" directory, relative to the new working directory.
dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
for ( i in 1:length( dirs ) ) {
  setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
  wd <- grep( 'CEDS/input', list.dirs(), value = T )
  if ( length( wd ) > 0 ) {
    setwd( wd[ 1 ] )
    break
  }
}
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files - 
# provides logging, file support, and system functions - and start the script log.
headers <- c( 'data_functions.R' ) # Any additional function files required
log_msg <- " -- " # First message to be printed to the log
script_name <- " -- "

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# Define emissions species variable
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"
em_lc <- tolower( em ) 

dir.create(  "../diagnostic-output/country-sector", showWarnings = F )
out_dir <- filePath( 'DIAG_OUT', 'country-sector/', extension = "" )

# ------------------------------------------------------------------------------
# 0.5. Load libraries
library('ggplot2')
library('plyr')
library('scales')

# ------------------------------------------------------------------------------
# 1. Read in final emissions 

# read in the emission data
emissions <- readData( "MED_OUT", paste0( em, '_total_CEDS_emissions') )
MCL <- readData( "MAPPINGS", 'Master_Country_List' )
# ------------------------------------------------------------------------------
# 2. data aggregation 
year_list <- as.character( 1900 : 2000 )
emissions_iso_sector <- aggregate( emissions[ , paste0( 'X', year_list ) ], by = list( emissions$iso, emissions$sector ), FUN = sum )
colnames( emissions_iso_sector )[ 1 : 2 ] <- c( 'iso', 'sector' )
emissions_iso_sector <- emissions_iso_sector[ order( emissions_iso_sector$iso ), ]

# ------------------------------------------------------------------------------
# 3. Extract desired sectors
emissions_sector <- aggregate( emissions[ , paste0( 'X', year_list ) ], by = list( emissions$sector ), FUN = sum )
colnames( emissions_sector )[ 1 ] <- 'sector' 
total_emissions_per_sector <- apply( emissions_sector[ , paste0( 'X', year_list ) ], 1, sum )

total_emissions_global <- sum( emissions[ , paste0( 'X', year_list ) ] )

logic_list <- total_emissions_per_sector > ( total_emissions_global * 0.01 ) 

logic_sectors <- emissions_sector$sector[ which( logic_list ) ]

# -----------------------------------------------------------------------------
# 4. Extract country-sector data 
emissions_iso_sector_selected <- emissions_iso_sector[ emissions_iso_sector$sector %in% logic_sectors , ]
# adding continent info to emissions
emissions_iso_sector_selected <- merge( emissions_iso_sector_selected, MCL, by = 'iso' )
emissions_iso_sector_selected <- emissions_iso_sector_selected[ , c( 'iso', 'Figure_Region', 'sector', paste0( 'X', year_list ) ) ]
cont_list <- sort( unique( emissions_iso_sector_selected$Figure_Region ) ) 
cont_list[10] <- 'South East Asia and Austrlia'
# -----------------------------------------------------------------------------
# 5. Make graphs

for ( sector in logic_sectors ) {
  for ( cont in cont_list ) {
      temp_data <- emissions_iso_sector_selected[ emissions_iso_sector_selected$sector == sector, c( 'iso', 'Figure_Region', paste0( 'X', year_list ) )  ]
      temp_data <- temp_data[ temp_data$Figure_Region == cont, c( 'iso', paste0( 'X', year_list ) ) ]
      
      if ( dim( temp_data )[ 1 ] == 0 ){ next } else{ 
      temp_data <- melt( temp_data )
      colnames( temp_data ) <- c( 'iso', 'year', 'emissions' )
      temp_data$year <- as.numeric( substr( temp_data$year, 2, 5 ) )
  
      plot <- ggplot( temp_data, aes( x = year, y = emissions, group = iso, color = iso ) ) + 
                geom_line( size=1, aes( x = year, y = emissions, color = iso ) ) + 
                scale_x_continuous( breaks = c( 1900, 1920, 1940, 1960, 1980, 2000 ) ) + 
                scale_y_continuous( labels = comma ) +
                labs( x = 'Year', y = paste( em, 'Emissions [kt]' ) ) + 
                guides( fill = guide_legend( ncol = 2 ) )
      
      sector_short <- unlist( strsplit( sector, split = '_' ) )[ 1 ] 
      file_name <- paste0( out_dir, em, "_", sector_short, "_", cont, "-sector_greater_than_1_percent.pdf" )
		  ggsave( file_name, width = 20, height = 15 )
      }
    }
  }

# -----------------------------------------------------------------------------
# 5. Stop 

# Every script should finish with this line:
logStop()  