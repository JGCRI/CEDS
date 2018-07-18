
edgar_em_list <- c( 'BC', 'CO', 'NH3', 'NMVOC', 'NOx', 'OC', 'SO2' )
for (k in seq_along(edgar_em_list)){
  args_from_makefile <- commandArgs( TRUE )
  em <- args_from_makefile[ 1 ]
  if ( is.na( em ) ) em <- edgar_em_list[k]

# ------------------------------------------------------------------------------
# Program Name: Compare_to_EDGAR.R
# Author: Rachel Hoesly
# Date Last Updated: 18 November 2016
# Program Purpose:
# Input Files: [em]_total_CEDS_emissions.csv
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
headers <- c( "data_functions.R",'common_data.R', 'IO_functions.R') # Additional function files may be required.
log_msg <- "Compare_to_GAINS" # First message to be printed to the log
script_name <- "Compare_to_GAINS.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )


# ---------------------------------------------------------------------------
# 0.5 Load Packages

library('ggplot2')
library('plyr')
library('scales')
library('gridExtra')

# ------------------------------------------------------------------------------
# 0.5 Load Packages

library('ggplot2')
library('plyr')
library('scales')
library('gridExtra')

# 0.5. Script Options

edgar_start_year <- 1970
edgar_end_year <- 2010
ceds_start_year <- 1850
ceds_end_year <- 2014

edgar_years<- edgar_start_year : edgar_end_year
x_edgar_years<- paste0( 'X', edgar_years )
ceds_years <- ceds_start_year : ceds_end_year
x_ceds_years <- paste0( 'X', ceds_years )

plot_start_year <- 1970
plot_end_year <- 2020
plot_years <- plot_start_year : plot_end_year
x_plot_years <- paste0( 'X', plot_years )

# --------------------------------------------
# 0.8 default emission setup and emission availability check
# Get emission species first so can name log appropriately


edgar_em_list <- c( 'BC', 'CO', 'NH3', 'NMVOC', 'NOx', 'OC', 'SO2' )

# Stop script if running for unsupported species
if ( em %!in% c('SO2','NOx','NH3','NMVOC','BC','OC','CH4','CO','CO2') ) {
  stop (paste( 'EDGAR is not supported for emission species', em))
}

# ------------------------------------------------------------------------------
# 1. Read in and load files for CEDS, EDGAR and GAINS ( RCP will be loaded in section 2 )
# if the em is not supportted by any of the emissions invertories, a dummy data frame will be created
# ------------------------------------------------------------------------------
# 1.1 read in CEDS total emissions

  ceds_emissions <- readData( 'MED_OUT', paste0( em, '_total_CEDS_emissions' ) )

# 1.2 Read in and load files for EDGAR

  # construct sheet name, for BC and OC the sheet name is slightly different
  edgar_sheet_name <- paste0( "NEW_v4.3_EM_" ,em, "_ref" )
  if ( em %in% c( 'BC', 'OC' ) ) { edgar_sheet_name <- paste0( "NEW_v4.3_EM_" ,em, "_hindc") }
  # read in the edgar data
  edgar_emissions <- readData( domain = "EM_INV",
                               domain_extension = "EDGAR/",
                               file_name = paste0( "JRC_PEGASOS_" ,em, "_TS_REF" ),
                               extension = ".xlsx",
                               sheet_selection = edgar_sheet_name,
                               skip = 8 )

# 1.3 Read in mapping files

  map <- readData('MAPPINGS', 'EDGAR_sector_comparison_map')
  Master_Country_List <- readData('MAPPINGS', 'Master_Country_List')

# ---------------------------------------------------------------------------
# 2. Process EDGAR

  # Clean rows and columns to standard format
  edgar_emissions$units <- 'kt'
  edgar_emissions <- edgar_emissions[ ,c( 'ISO_A3', 'IPCC', 'units', edgar_years ) ]
  names( edgar_emissions ) <- c ('iso','edgar_sector','units', x_edgar_years )
  edgar_emissions$iso <- tolower( edgar_emissions$iso )

  # map to ceds
  edgar_emissions$sector <- map[match(edgar_emissions$edgar_sector,map$edgar_sector),
                                         'scaling_sector']
  # map to region
  edgar_emissions$region <- Master_Country_List[match(edgar_emissions$iso,Master_Country_List$iso),
                                'Figure_Region']


  # aggregate by sector
  edgar_sector <- aggregate(edgar_emissions[ x_edgar_years ],
                                             by = list(sector = edgar_emissions$sector),
                                             sum, na.rm=T)
  edgar_sector_long <- melt(edgar_sector)
  names(edgar_sector_long) <- c("sector","year","emissions")
  edgar_sector_long$inventory <- 'EDGAR'
  edgar_sector_long$emissions <- edgar_sector_long$emissions/1000

  # aggregate by sector
  edgar_sector_region <- aggregate(edgar_emissions[ x_edgar_years ],
                            by = list(sector = edgar_emissions$sector,
                                      region = edgar_emissions$region),
                            sum, na.rm=T)
  edgar_sector_region_long <- melt(edgar_sector_region)
  names(edgar_sector_region_long) <- c("sector","region","year","emissions")
  edgar_sector_region_long$inventory <- 'EDGAR'
  edgar_sector_region_long$emissions <- edgar_sector_region_long$emissions/1000

  # ---------------------------------------------------------------------------
  # 3. Process CEDS

  ceds_emissions$scaling_sector <- map[match(ceds_emissions$sector,map$ceds_sector),
                                       'scaling_sector']
  # map to region
  ceds_emissions$region <- Master_Country_List[match(ceds_emissions$iso,Master_Country_List$iso),
                                                'Figure_Region']


  # aggregate by sector
  ceds_sector <- aggregate(ceds_emissions[ x_ceds_years ],
                            by = list(sector = ceds_emissions$scaling_sector),
                            sum, na.rm=T)
  ceds_sector_long <- melt(ceds_sector)
  names(ceds_sector_long) <- c("sector","year","emissions")
  ceds_sector_long$inventory <- 'CEDS'
  ceds_sector_long$emissions <- ceds_sector_long$emissions/1000

  # aggregate by sector,region
  ceds_sector_region <- aggregate(ceds_emissions[ x_ceds_years ],
                           by = list(sector = ceds_emissions$scaling_sector,
                                     region = ceds_emissions$region),
                           sum, na.rm=T)
  ceds_sector_region_long <- melt(ceds_sector_region)
  names(ceds_sector_region_long) <- c("sector","region","year","emissions")
  ceds_sector_region_long$inventory <- 'CEDS'
  ceds_sector_region_long$emissions <- ceds_sector_region_long$emissions/1000

  # ---------------------------------------------------------------------------
  # 4. Global Sector Figures

  combined_long <- rbind.fill(ceds_sector_long, edgar_sector_long)
  combined_long$year <- as.numeric(gsub('X','',combined_long$year))


  # sort sectors by size
  find_max <- function(x){(max(combined_long[which(combined_long$sector == x),'emissions']))}
  sectors <- data.frame(sector = unique(combined_long$sector))
  sectors$max <- sapply( X = unique(combined_long$sector),
                        FUN = find_max  )
  sectors <- sectors[order(-sectors$max),]

  for(i in 1:5){
  sec <- sectors[(1+5*(i-1)):((1+5*(i-1))+4), 'sector' ]
  df <- combined_long[which(combined_long$sector %in% sec),]

  plot <- ggplot( df, aes(x = year, y = emissions, color = sector, linetype = inventory))+
    geom_line(size = 1)+
    scale_x_continuous(limits = c(edgar_start_year,edgar_end_year ),
                     breaks= seq(from=1970, to=2014, by=10),
                     minor_breaks = seq(from=1970, to=2014, by=5)) +
    scale_y_continuous(labels = comma)+
    ggtitle( paste('Global',em,'Inventories by Sector') )+
    labs(x= "" , y= paste(em ,'Emissions [Tg/yr]') )+
    theme(panel.background=element_blank(),
          panel.grid.minor = element_line(colour="gray95"),
          panel.grid.major = element_line(colour="gray88"),
          panel.border = element_rect(colour = "gray80", fill=NA, size=1))

  ggsave(paste0('../diagnostic-output/ceds-comparisons/sector-level/CEDS_EDGAR_sector_',em,'_',i,'.pdf') , width = 5, height = 4)

  }

  # ---------------------------------------------------------------------------
  # 5. Region Sector Figures

  combined_long <- rbind.fill(ceds_sector_region_long, edgar_sector_region_long)
  combined_long$year <- as.numeric(gsub('X','',combined_long$year))

  number_of_sectors <- 6

  # sort sectors by size
  find_max <- function(x){(max(combined_long[which(combined_long$sector == x),'emissions']))}
  sectors <- data.frame(sector = unique(combined_long$sector))
  sectors$max <- sapply( X = unique(combined_long$sector),
                         FUN = find_max  )
  sectors <- sectors[order(-sectors$max),]
  plot_sectors <- sectors[(1:number_of_sectors), 'sector' ]

  for(i in 1:number_of_sectors){

    df <- combined_long[which(combined_long$sector %in% plot_sectors[i]),]

    plot <- ggplot( df, aes(x = year, y = emissions, color = region, linetype = inventory))+
      geom_line(size = 1)+
      scale_x_continuous(limits = c(edgar_start_year,edgar_end_year ),
                         breaks= seq(from=1970, to=2014, by=10),
                         minor_breaks = seq(from=1970, to=2014, by=5)) +
      scale_y_continuous(labels = comma)+
      ggtitle( paste(em,'Inventories by Region:',plot_sectors[i]) )+
      labs(x= "" , y= paste(em ,'Emissions [Tg/yr]') )+
      theme(panel.background=element_blank(),
            panel.grid.minor = element_line(colour="gray95"),
            panel.grid.major = element_line(colour="gray88"),
            panel.border = element_rect(colour = "gray80", fill=NA, size=1))
    plot
    ggsave(paste0('../diagnostic-output/ceds-comparisons/sector-level/CEDS_EDGAR_region_',em,'_',plot_sectors[i],'.pdf') , width = 5, height = 4)

  }




  # ---------------------------------------------------------------------------
  # 5. End

  logStop()
}
