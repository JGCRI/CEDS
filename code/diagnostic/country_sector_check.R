# ------------------------------------------------------------------------------
# Program Name: country_sector_check.R
# Author(s): Leyang Feng
# Date Last Updated: 28 April 2016
# Program Purpose:
# Input Files:
# Output Files:
# Notes:
# TODO:
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

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
# Setting the threshold
em_threshold <- 0.01

emissions_sector <- aggregate( emissions[ , paste0( 'X', year_list ) ], by = list( emissions$sector ), FUN = sum )
colnames( emissions_sector )[ 1 ] <- 'sector'
total_emissions_per_sector <- apply( emissions_sector[ , paste0( 'X', year_list ) ], 1, sum )

total_emissions_global <- sum( emissions[ , paste0( 'X', year_list ) ] )

logic_list <- total_emissions_per_sector > ( total_emissions_global * em_threshold  )

target_sectors <- emissions_sector$sector[ which( logic_list ) ]

# -----------------------------------------------------------------------------
# 4. Extract country-sector data

emissions_iso_sector_selected <- emissions_iso_sector[ emissions_iso_sector$sector %in% target_sectors , ]
# adding continent info to emissions
emissions_iso_sector_selected <- merge( emissions_iso_sector_selected, MCL, by = 'iso' )
emissions_iso_sector_selected <- emissions_iso_sector_selected[ , c( 'iso', 'Figure_Region', 'sector', paste0( 'X', year_list ) ) ]
colnames( emissions_iso_sector_selected ) <- c( 'iso', 'region', 'sector', paste0( 'X', year_list ) )
region_list <- sort( unique( emissions_iso_sector_selected$region ) )
region_list[10] <- 'South East Asia and Austrlia'

# -----------------------------------------------------------------------------
# 5. Make graphs
# 5.1. all isos on one graph
for ( sector in target_sectors ){
  # create 10 letters short sector name for later use
  sector_short <- substr( sector, 1, 10 )
  # melt the data
  plot_data <- emissions_iso_sector_selected[ emissions_iso_sector_selected$sector == sector, c( 'iso', 'region', paste0( 'X', year_list ) )  ]
  plot_data <- melt( plot_data )
  colnames( plot_data ) <- c( 'iso', 'region', 'year', 'emissions' )
  plot_data$year <- as.numeric( substr( plot_data$year, 2, 5 ) )
  plot <- ggplot( plot_data, aes( x = year, y = emissions, group = iso, color = iso ) ) +
                  geom_line( size = 0.5, aes( x = year, y = emissions, color = iso ) ) +
                  scale_x_continuous( breaks = c( 1900, 1920, 1940, 1960, 1980, 2000 ) ) +
                  scale_y_continuous( labels = comma ) +
                  labs( x = 'Year', y = paste( em, 'Emissions [kt]' ) ) +
                  guides( col = guide_legend( ncol = 25 ) ) +
                  theme( legend.position = "bottom" )
  file_name <- paste0( out_dir, em, "_", sector_short, "_all-country_sector-greater-than-", as.character( em_threshold * 100 ), "-percent.pdf" )
	ggsave( file_name, width = 20, height = 15 )
}


# 5.2. region separated
for ( sector in target_sectors ) {
  sector_short <- substr( sector, 1, 10 )
  for ( region in region_list ) {
      plot_data <- emissions_iso_sector_selected[ emissions_iso_sector_selected$sector == sector, c( 'iso', 'region', paste0( 'X', year_list ) )  ]
      plot_data <- plot_data[ plot_data$region == region, c( 'iso', paste0( 'X', year_list ) ) ]

      if ( dim( plot_data )[ 1 ] == 0 ){ next } else{
        if ( region == 'Global' ) {
          if ( sum( plot_data[ , 3 : ncol( plot_data ) ] ) == 0 ) {
            next } else {
              plot_data <- melt( plot_data )
            colnames( plot_data ) <- c( 'iso', 'year', 'emissions' )
            plot_data$year <- as.numeric( substr( plot_data$year, 2, 5 ) )

            plot <- ggplot( plot_data, aes( x = year, y = emissions, group = iso, color = iso ) ) +
                      geom_line( size=1, aes( x = year, y = emissions, color = iso ) ) +
                      scale_x_continuous( breaks = c( 1900, 1920, 1940, 1960, 1980, 2000 ) ) +
                      scale_y_continuous( labels = comma ) +
                      labs( x = 'Year', y = paste( em, 'Emissions [kt]' ) ) +
                      guides( col = guide_legend( ncol = 10 ) ) +
                      theme( legend.position = "bottom" )
            file_name <- paste0( out_dir, em, "_", sector_short, "_", region, "_sector-greater-than-", as.character( em_threshold * 100 ), "-percent.pdf" )
		        ggsave( file_name, width = 20, height = 15 )
              }
          } else {
            plot_data <- melt( plot_data )
            colnames( plot_data ) <- c( 'iso', 'year', 'emissions' )
            plot_data$year <- as.numeric( substr( plot_data$year, 2, 5 ) )
            plot <- ggplot( plot_data, aes( x = year, y = emissions, group = iso, color = iso ) ) +
                      geom_line( size=1, aes( x = year, y = emissions, color = iso ) ) +
                      scale_x_continuous( breaks = c( 1900, 1920, 1940, 1960, 1980, 2000 ) ) +
                      scale_y_continuous( labels = comma ) +
                      labs( x = 'Year', y = paste( em, 'Emissions [kt]' ) ) +
                      guides( col = guide_legend( ncol = 10 ) ) +
                      theme( legend.position = "bottom" )
            file_name <- paste0( out_dir, em, "_", sector_short, "_", region, "_sector-greater-than-", as.character( em_threshold * 100 ), "-percent.pdf" )
		        ggsave( file_name, width = 20, height = 15 )
      }
    }
  }
}

# -----------------------------------------------------------------------------
# 6. write csvs for each sector

for ( sector in target_sectors ) {
  temp_data <- emissions_iso_sector_selected[ emissions_iso_sector_selected$sector == sector, c( 'iso', 'sector', 'region', paste0( 'X', year_list ) )  ]
  sector_short <- substr( sector, 1, 10 )
  file_name <- paste0( em, '_', sector_short, '_great_than_',as.character( em_threshold *100 ), '_percent' )
  writeData( temp_data, domain = "DIAG_OUT", domain_extension = "country-sector/", file_name )
  }

# -----------------------------------------------------------------------------
# 7. Stop

# Every script should finish with this line:
logStop()
