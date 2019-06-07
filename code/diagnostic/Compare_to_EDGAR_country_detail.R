
edgar_em_list <- c( 'BC',  'SO2',  'NOx')
#'CO', 'NH3', 'NMVOC',, 'OC',

for (k in seq_along(edgar_em_list)){
  args_from_makefile <- commandArgs( TRUE )
em <- edgar_em_list[k]

  # ------------------------------------------------------------------------------
  # Program Name: Compare_to_EDGAR_country_details.R
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
  log_msg <- "Compare_to_EDGAR_" # First message to be printed to the log
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

  edgar_start_year <- 1995
  edgar_end_year <- 2010
  ceds_start_year <- 1995
  ceds_end_year <- 2014

  edgar_years<- edgar_start_year : edgar_end_year
  x_edgar_years<- paste0( 'X', edgar_years )
  ceds_years <- ceds_start_year : ceds_end_year
  x_ceds_years <- paste0( 'X', ceds_years )

  plot_start_year <- 1995
  plot_end_year <- 2015
  plot_years <- plot_start_year : plot_end_year
  x_plot_years <- paste0( 'X', plot_years )

  # --------------------------------------------
  # 0.8 default emission setup and emission availability check
  # Get emission species first so can name log appropriately


  # edgar_em_list <- c( 'BC', 'CO', 'NH3', 'NMVOC', 'NOx', 'OC', 'SO2' )

  # Stop script if running for unsupported species
  if ( em %!in% c('SO2','NOx','NH3','NMVOC','BC','OC','CH4','CO','CO2') ) {
    stop (paste( 'EDGAR is not supported for emission species', em))
  }



  # ------------------------------------------------------------------------------
  # 1. Read in and load files for CEDS, EDGAR and GAINS ( RCP will be loaded in section 2 )
  # if the em is not supportted by any of the emissions inventories, a dummy data frame will be created
  # ------------------------------------------------------------------------------
  # 1.1 read in CEDS total emissions

  ceds_emissions_read <- readData( 'MED_OUT', paste0( em, '_total_CEDS_emissions' ) )

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

  select_countries <- Master_Country_List[which( Master_Country_List$Country_Name %in% c('Argentina', 'Colombia','Brazil','Peru','Chile')),'iso']

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
  # remove unmapped sectors (sectors that are not included in ceds) - ag waste and forrest fires
  edgar_emissions <- edgar_emissions[which(!is.na(edgar_emissions$sector)),]

  # aggregate by sector
  edgar_sector <- aggregate(edgar_emissions[ x_edgar_years ],
                            by = list(sector = edgar_emissions$sector),
                            sum, na.rm=T)
  edgar_sector_long <- melt(edgar_sector)
  names(edgar_sector_long) <- c("sector","year","emissions")
  edgar_sector_long$inventory <- 'EDGAR'
  edgar_sector_long$emissions <- edgar_sector_long$emissions/1000


  # aggregate by sector
  edgar_sector_iso <- aggregate(edgar_emissions[ x_edgar_years ],
                                   by = list(sector = edgar_emissions$sector,
                                             iso = edgar_emissions$iso),
                                   sum, na.rm=T)
  edgar_sector_iso_long <- melt(edgar_sector_iso)
  names(edgar_sector_iso_long) <- c("sector","iso","year","emissions")
  edgar_sector_iso_long$inventory <- 'EDGAR'
  edgar_sector_iso_long$emissions <- edgar_sector_iso_long$emissions/1000

  # aggregate by sector
  edgar_iso <- aggregate(edgar_emissions[ x_edgar_years ],
                                by = list(iso = edgar_emissions$iso),
                                sum, na.rm=T)
  edgar_iso_long <- melt(edgar_iso)
  names(edgar_iso_long) <- c("iso","year","emissions")
  edgar_iso_long$inventory <- 'EDGAR'
  edgar_iso_long$emissions <- edgar_iso_long$emissions/1000

  # ---------------------------------------------------------------------------
  # 3. Process CEDS
  ceds_emissions <- ceds_emissions_read
  ceds_emissions$scaling_sector <- map[match(ceds_emissions$sector,map$ceds_sector),
                                       'scaling_sector']

   # remove unmapped sectors (sectors that are not included in ceds) - ag waste and forrest fires
  remove_sectors <- c("6A_Other-in-total", "6B_Other-not-in-total", "11A_Volcanoes", "11C_Other-natural")
  ceds_emissions <- ceds_emissions[which(ceds_emissions$sector %!in% remove_sectors),]
  ceds_emissions <- ceds_emissions[which(!is.na(ceds_emissions$scaling_sector)),]



  # aggregate by sector
  ceds_sector <- aggregate(ceds_emissions[ x_ceds_years ],
                           by = list(sector = ceds_emissions$scaling_sector),
                           sum, na.rm=T)
  ceds_sector_long <- melt(ceds_sector)
  names(ceds_sector_long) <- c("sector","year","emissions")
  ceds_sector_long$inventory <- 'CEDS'
  ceds_sector_long$emissions <- ceds_sector_long$emissions/1000


  # aggregate by sector,country
  ceds_sector_iso <- aggregate(ceds_emissions[ x_ceds_years ],
                                  by = list(sector = ceds_emissions$scaling_sector,
                                            iso = ceds_emissions$iso),
                                  sum, na.rm=T)
  ceds_sector_iso_long <- melt(ceds_sector_iso)
  names(ceds_sector_iso_long) <- c("sector","iso","year","emissions")
  ceds_sector_iso_long$inventory <- 'CEDS'
  ceds_sector_iso_long$emissions <- ceds_sector_iso_long$emissions/1000

  # aggregate by country
  ceds_iso <- aggregate(ceds_emissions[ x_ceds_years ],
                               by = list(iso = ceds_emissions$iso),
                               sum, na.rm=T)
  ceds_iso_long <- melt(ceds_iso)
  names(ceds_iso_long) <- c("iso","year","emissions")
  ceds_iso_long$inventory <- 'CEDS'
  ceds_iso_long$emissions <- ceds_iso_long$emissions/1000


   # ---------------------------------------------------------------------------
  # 4. Country Comparison Figures



# country, sector
  combined_long <- rbind(ceds_sector_iso_long,edgar_sector_iso_long) %>%
    dplyr::mutate( year = as.numeric(gsub('X','',year)) )

  for ( g in seq_along(select_countries)){
    country_name <- Master_Country_List[which(Master_Country_List$iso == select_countries[g]),"Country_Name"]

    # country total
    country_combined_long <- rbind(ceds_iso_long,edgar_iso_long) %>%
      dplyr::mutate( year = as.numeric(gsub('X','',year)) ) %>%
      filter (iso == select_countries[g])

    plot <- ggplot( country_combined_long, aes(x = year, y = emissions, linetype = inventory, alpha = inventory))+
      geom_line(size = 1)+
      scale_x_continuous(limits = c(edgar_start_year,edgar_end_year ),
                         breaks= seq(from=1995, to=2014, by=5),
                         minor_breaks = seq(from=1995, to=2014, by=1)) +
      scale_y_continuous(labels = comma)+
      scale_alpha_manual(values = c(.6,1))+
      ggtitle( paste(country_name,': ',em,'Global') )+
      labs(x= "" , y= paste(em ,'Emissions [Tg/yr]') )+
      theme(panel.background=element_blank(),
            panel.grid.minor = element_line(colour="gray95"),
            panel.grid.major = element_line(colour="gray88"),
            panel.border = element_rect(colour = "gray80", fill=NA, size=1))


    ggsave(paste0('../diagnostic-output/ceds-comparisons/sector-level/country specific comparison/EDGAR_comparison_',country_name,'_',em,'_global.pdf') , width = 6, height = 4)

  # Sector Comparison
    df <- combined_long %>%
      filter(iso == select_countries[g]) %>%
      filter(!is.na(sector)) %>%
      filter(emissions>0) %>%
      filter(year>1969)

    # sort sectors by size
    sectors <- unique(df$sector)
    number_of_sectors <- length(sectors)
    find_max <- function(x){ max( df[which(df$sector == x),'emissions']) }
    sectors <- data.frame(sector = sectors)
    sectors$max <- sapply( X = sectors$sector,
                           FUN = find_max  )
    sectors <- sectors[order(-sectors$max),]
    sectors <- sectors %>% filter(max>0)
    sectors_per_plot <- 4
    plots <- ceiling(number_of_sectors/sectors_per_plot)

  for(h in seq_along(1:plots)){
    index_plot <- ((h-1)*sectors_per_plot+1):(h*sectors_per_plot)
    index_plot <- index_plot[which(index_plot <= number_of_sectors)]
    plot_sectors <- sectors$sector[ index_plot  ]
    df_plot <- df %>% filter(sector %in% plot_sectors )

    plot <- ggplot( df_plot, aes(x = year, y = emissions, color = sector, linetype = inventory, alpha = inventory))+
      geom_line(size = 1)+
      scale_x_continuous(limits = c(edgar_start_year,edgar_end_year ),
                         breaks= seq(from=1995, to=2014, by=5),
                         minor_breaks = seq(from=1995, to=2014, by=1)) +
      scale_y_continuous(labels = comma)+
      scale_alpha_manual(values = c(.6,1))+
      ggtitle( paste(country_name,': ',em,'Inventories by Sector') )+
      labs(x= "" , y= paste(em ,'Emissions [Tg/yr]') )+
      theme(panel.background=element_blank(),
            panel.grid.minor = element_line(colour="gray95"),
            panel.grid.major = element_line(colour="gray88"),
            panel.border = element_rect(colour = "gray80", fill=NA, size=1))

    ggsave(paste0('../diagnostic-output/ceds-comparisons/sector-level/country specific comparison/EDGAR_comparison_',country_name,'_',em,'_',h,'_sectors.pdf') ,
           width = 6, height = 4)

  }

    # Sector Difference
    df <- combined_long %>%
      filter(iso == select_countries[g]) %>%
      filter(!is.na(sector)) %>%
      filter(year>1969) %>%
      spread(inventory, emissions) %>%
      replace(is.na(.), 0) %>%
      dplyr::mutate(difference = CEDS-EDGAR) %>%
      filter(difference > 0 )

    # sort sectors by size
    sectors <- unique(df$sector)
    number_of_sectors <- length(sectors)
    find_max <- function(x){ max( df[which(df$sector == x),'difference']) }
    sectors <- data.frame(sector = sectors)
    sectors$max <- sapply( X = sectors$sector,
                           FUN = find_max  )
    sectors <- sectors[order(-sectors$max),]
    sectors <- sectors %>% filter(max>0)
    sectors_per_plot <- 7
    plots <- ceiling(number_of_sectors/sectors_per_plot)

    for(h in seq_along(1:plots)){
      index_plot <- ((h-1)*sectors_per_plot+1):(h*sectors_per_plot)
      index_plot <- index_plot[which(index_plot <= number_of_sectors)]
      plot_sectors <- sectors$sector[ index_plot  ]
      df_plot <- df %>% filter(sector %in% plot_sectors )

      plot <- ggplot( df_plot, aes(x = year, y = difference, fill = sector))+
        geom_area(alpha = .8)+
        scale_x_continuous(limits = c(edgar_start_year,edgar_end_year ),
                           breaks= seq(from=1995, to=2014, by=5),
                           minor_breaks = seq(from=1995, to=2014, by=1)) +
        scale_y_continuous(labels = comma)+
        ggtitle( paste(country_name,': ',em,'Difference by Sector (CEDS-EDGAR)') )+
        labs(x= "" , y= paste(em ,'Emissions [Tg/yr]') )+
        theme(panel.background=element_blank(),
              panel.grid.minor = element_line(colour="gray95"),
              panel.grid.major = element_line(colour="gray88"),
              panel.border = element_rect(colour = "gray80", fill=NA, size=1))
      plot
      ggsave(paste0('../diagnostic-output/ceds-comparisons/sector-level/country specific comparison/EDGAR_difference_',country_name,'_',em,'_',h,'_sectors.pdf') ,
             width = 6, height = 4)

    }




    }


  logStop()
}