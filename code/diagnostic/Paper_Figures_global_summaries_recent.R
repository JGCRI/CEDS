# ------------------------------------------------------------------------------
# Program Name: Recent_Paper_Figures_global_summaries_CH4.R
# Author: Rachel Hoesly, Linh Vu, Leyang Feng, Huong Nguyen
# Date Last Updated: 28 October, 2016
# Program Purpose: Produces comparison - diagnostic files and plots between CEDS and
#                  RCP. Comparison by global totals, regions, sectors
#                  Like with like comparison does not include
#                       open burning (grassland and forest fires),
#                       fossil-fuel fires,
#                       international shipping (See Note (2) and (3))
#                       aviation
#
# Input Files: [em]_total_CEDS_emissions.csv
# Output Files: figures in the diagnostic-output
# Note: (1) Shipping emissions is included in global comparison, but removed in regional comparison
#       (2) RCP shipping emissions does not have data for NH3
# TODO: (1) In RCP shipping data NMVOC and CH4 data doesn't include emissions for tanker loading.
#           Change the sector drop mapping when tanker loading sector is added for CEDS.
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R",'process_db_functions.R',
              'common_data.R', 'IO_functions.R', 'data_functions.R', 'timeframe_functions.R') # Additional function files may be required.
log_msg <- "Processing Paper and Supplement Figures for all emission species" # First message to be printed to the log
script_name <- "Paper_Figures_global_summaries.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# Load Packages

library('ggplot2')
library('plyr')
library('scales')
library('gridExtra')

# function from stack exchange
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# ---------------------------------------------------------------------------
# 0.5. Script Options

# years
rcp_start_year <- 1990
rcp_end_year <- 2000
CEDS_start_year <- 1990
CEDS_end_year <- 2014

X_plot_years <- paste0('X',min(rcp_start_year,CEDS_start_year):max(rcp_end_year,CEDS_end_year))

major <- 10
minor <- 5

rcp_years <- seq(from=rcp_start_year,to=rcp_end_year,by=10)
x_rcp_years <- paste0('X',rcp_years)

ceds_years <- seq(from=CEDS_start_year,to=CEDS_end_year,by=1)
x_ceds_years <- paste0('X',ceds_years)


cdiac_years <- seq(from=cdiac_start_year,to=cdiac_end_year,by=1)
x_cdiac_years <- paste0('X',cdiac_years)

# ---------------------------------------------------------------------------
# 1. Load species independent files

Map_region_codes <- readData( "EM_INV", domain_extension = 'RCP/',"RCP Region Mapping", ".xlsx", sheet_selection = 'Reg Codes',
                              meta=FALSE)
Map_iso_codes <- readData( "EM_INV", domain_extension = 'RCP/',"RCP Region Mapping", ".xlsx", sheet_selection = 'EDGAR32 & IEA',
                           meta=FALSE)
Map_sector <- readData( "EM_INV", domain_extension = 'RCP/',"RCP_CEDS_sector_map",
                        meta=FALSE)
MSLevel <- readData('MAPPINGS', 'Master_Sector_Level_map')
Master_Country_List <- readData('MAPPINGS', 'Master_Country_List')

rcp_ship_emissions <- readData( domain = 'EM_INV', domain_extension = 'RCP/',
                                file_name = 'Historicalshipemissions_IPCC_FINAL_Jan09_updated_1850',
                                extension = '.xlsx',  sheet_selection = 'CO2Emis_TgC', skip = 8 )[ 1:140, 1:12 ]

cdiac <- readData('MED_OUT', 'E.CO2_CDIAC_Total_CO2')

source('../code/diagnostic/Paper_Figures_plot_colors.R')

# ---------------------------------------------------------------------------
# 1.5 Process RCP shipping emissions
names( rcp_ship_emissions ) <- c( "year", "CO2", "fleet", "NOx", "SO2", "PM", "NMVOC", "CH4", "BC", "OC", "Refrigerants", "CO" )
rcp_shipping_em_list <- c( "CO2", "NOx", "SO2", "NMVOC", "BC", "OC", "CO", "CH4" )
rcp_ship_emissions <- rcp_ship_emissions[ , c( "year", rcp_shipping_em_list ) ]
# convert unit from TG to kt
rcp_ship_emissions [ , rcp_shipping_em_list ] <- rcp_ship_emissions [ , rcp_shipping_em_list ] * 1000
rcp_ship_emissions$units <- "kt"
rcp_ship_emissions$SO2 <- rcp_ship_emissions$SO2 * 2  #Convert from S to SO2 for SO2
rcp_ship_emissions$NOx <- rcp_ship_emissions$NOx * 3.285  # Convert from N to NO2 for NOx

# ---------------------------------------------------------------------------
# 1.5 Process CDIAC CO2 emissions

#Aggregate CDIAC to Global
cdiac$em <- 'CO2'
cdiac_agg <- aggregate(cdiac[,x_cdiac_years],
                       by = list(em = cdiac$em ),
                       FUN=sum )
cdiac_agg$Inventory <- 'CDIAC'
global_cdiac_long <- melt(cdiac_agg, id.vars = c('em','Inventory'))
names(global_cdiac_long) <- c("em","Inventory","year","total_emissions")
global_cdiac_long$year <- as.numeric(gsub('X', '', global_cdiac_long$year) )
#cdiac emissions are in units of thousand metric tons carbon (not co2) - convert to kt CO2
# convert to units of CO2
global_cdiac_long$total_emissions <- 44/12*global_cdiac_long$total_emissions

# ---------------------------------------------------------------------------
# Start Emissions Loop
em_list <- c('SO2','NOx','CO','OC','BC','NH3','NMVOC','CO2','CH4')

# Create Plot Lists
total_comparison_list <- list()
total_stacked_sector_list <- list()
total_stacked_region_list <- list()
total_stacked_fuel_list <- list()
total_line_sector_list <- list()
total_line_region_list <- list()
total_line_fuel_list <- list()

for( h in seq_along(em_list)){
  wd <- getwd()
  if( wd != "/Users/hoes919/Documents/CEDS/input") stop('working directiory')
  em <- em_list[h]

  # ---------------------------------------------------------------------------
  # 1.5. Other script Options

  unit <- '[Tg/year]'

  if (em == 'SO2') unit <- '[Tg SO2/year]'
  if (em == 'NOx') unit <- '[Tg NO2/year]'
  if (em %in% c('OC','BC')) unit <- '[Tg C/year]'
  if (em == 'NH3') unit <- '[Tg NH3/year]'
  if (em == 'CO') unit <- '[Tg CO/year]'
  if (em == 'NMVOC') unit <- '[Tg NMVOC/year]'
  if (em == 'CO2') unit <- '[1000 Tg CO2/year]'
  if (em == 'CH4') unit <- '[Tg CH4/year]'

  # Non Comparable Sectors
  rcp_remove_sectors <- c('AWB','Tot_Ant')

  # if current em does not have ship emissions
  # for the RCP shipping emissions data Historicalshipemissions_IPCC_FINAL_Jan09_updated_1850.xlsx
  # it doesn't contain data for NH3
  # CDIAC contains both shipping and aviation fuel use

  has_ship <- em %!in% c("NH3", 'CO2')

  if ( em %!in% c("NH3", 'CO2') ) { # RCP has shipping, but no aviation fuel us
    ceds_remove_sectors <- c("1A3ai_International-aviation",
                             '1A3aii_Domestic-aviation',
                             '7A_Fossil-fuel-fires',
                             '3F_Agricultural-residue-burning-on-fields',
                             '11A_Volcanoes',
                             '11B_Forest-fires',
                             '11C_Other-natural',
                             '6B_Other-not-in-total')
  }else if (em == 'NH3'){# RCP has no shipping for NH3, no aviation fuel us
    ceds_remove_sectors <- c("1A3ai_International-aviation",
                             "1A3di_International-shipping",
                             '1A3aii_Domestic-aviation',
                             '7A_Fossil-fuel-fires',
                             '3F_Agricultural-residue-burning-on-fields',
                             '11A_Volcanoes',
                             '11B_Forest-fires',
                             '11C_Other-natural',
                             '6B_Other-not-in-total')

  } else if (em == 'CO2'){ # CDIAC contains both aviation and fuel use
    ceds_remove_sectors <- c('7A_Fossil-fuel-fires',
                             '3F_Agricultural-residue-burning-on-fields',
                             '11A_Volcanoes',
                             '11B_Forest-fires',
                             '11C_Other-natural',
                             '6B_Other-not-in-total')
  }

  ceds_remove_sectors_global <- ceds_remove_sectors

  # ---------------------------------------------------------------------------
  # 2. Load and process RCP files
  # set wd to RCP folder
  if (em != 'CO2'){
    rcp_dir <- './emissions-inventories/RCP'

    # create temporary folder to extract zipped files
    zipfile_path <- paste0(rcp_dir, em, '.zip')
    dir.name <- paste0(rcp_dir, em, '_RCP_temp_folder')
    dir.create(dir.name)
    # unzip files to temp folder
    unzip(zipfile_path, exdir = dir.name)

    # list files in the folder
    files <- list.files(paste0(dir.name,'/',em)  ,pattern = '.dat')
    files <- paste0(dir.name,'/',em,'/',files)

    rcp_files <- list()
    for (i in seq_along(rcp_years)){
      rcp_files[i] <- files[grep(rcp_years[i], files)]
    }
    rcp_files <- unlist(rcp_files)

    RCP_df_list <- lapply(X=rcp_files,FUN=read.table,strip.white = TRUE,header=TRUE,skip = 4,fill=TRUE, stringsAsFactors = FALSE)

    for (i in seq_along(rcp_years)){
      RCP_df_list[[i]]$year <- rcp_years[i]
    }
    RCP_df <- do.call("rbind", RCP_df_list)

    # delete temp folder
    unlink(dir.name,recursive = TRUE)

    # ---------------------------------------------------------------------------
    # 3. Process RCP Emissions Data
    # ---------------------------------------------------------------------------
    # 3.1 process RCP for sectors except international shipping ( shipping is in a seperate file and prcessed in 2.2 )
    # Process and clean RCP data. No removing sectors yet
    RCP <- RCP_df
    names(RCP)[which(names(RCP)== 'Tot.')] <- "Tot_Ant"
    names(RCP)[which(names(RCP)== 'Ant.')] <- "Region_Name_1"
    names(RCP)[which(names(RCP)== 'Region.1')] <- "Region_Name_2"

    RCP$Region_Name_2 <- gsub("(Rest","",RCP$Region_Name_2,fixed=TRUE)
    RCP$Region_Name <- paste(RCP$Region_Name_1,RCP$Region_Name_2)

    RCP <- RCP[,c('Region','Subregion',"Region_Name","ENE","IND","TRA","DOM","SLV","AGR","AWB","WST","Tot_Ant",'year')]

    RCP <- RCP[which(complete.cases(RCP)),]

    RCP$ENE <- as.numeric(RCP$ENE)
    RCP$year <- paste0('X',RCP$year)

    RCP_long <- melt(RCP, id.vars = c('Region','Subregion','Region_Name','year'))
    RCP <- cast( RCP_long , Region + Subregion + Region_Name + variable ~ year)
    RCP$em <- em

    names(RCP)[which(names(RCP) == 'Region')] <- 'Region_code'
    names(RCP)[which(names(RCP) == 'Subregion')] <- 'Subregion_code'
    names(RCP)[which(names(RCP) == 'Region_Name')] <- 'Region'
    names(RCP)[which(names(RCP) == 'variable')] <- 'Sector'

    RCP[grep('Stan',RCP$Region),'Region'] <- "Asia-Stan"
    RCP$Region <- gsub(" $","", RCP$Region, perl=T)

    # Remove sectors not comparable with CEDS. Recalculated totals
    RCP <- RCP[,c('em','Region','Sector',x_rcp_years)]

    writeData(RCP, 'MED_OUT', 'RCP_no_shipping')
  }
  # ---------------------------------------------------------------------------
  # 4. Load and Process CEDS Emissions Data

  Total_Emissions <- readData('MED_OUT', paste0(em,'_total_CEDS_emissions'))

  x_years<-paste('X',CEDS_start_year:CEDS_end_year,sep="")

  CEDS <- Total_Emissions
  CEDS$em <- em

  # rename other process emissions to tanker loading
  CEDS[which(CEDS$sector == '2L_Other-process-emissions'),'sector'] <- '1A3di_International-shipping'

  # rename international shipping, international air, and domestic air to global
  CEDS[which(CEDS$sector %in% c('1A3ai_International-aviation','1A3di_International-shipping','1A3aii_Domestic-aviation')),'iso'] <- 'global'

  # add sector
  CEDS$agg_Sector <- MSLevel[match(CEDS$sector,MSLevel$working_sectors_v1),'Figure_sector']
  CEDS$agg_Sector <- as.factor(CEDS$agg_Sector)

  # add region
  CEDS$Region <- Master_Country_List[match(CEDS$iso,Master_Country_List$iso),'Paper_Figure_Region']


  # remove other total
  other <- CEDS[ which(CEDS$sector %in% c('6A_Other-in-total', '11C_Other-natural', '11B_Forest-fires','11A_Volcanoes','6B_Other-not-in-total')), ]
  other_sum <- sum(other[X_plot_years])
  if(other_sum != 0) stop('There are non zero emissions in "other in total". Please check.')
  CEDS <- CEDS[-which(CEDS$sector %in% c('6A_Other-in-total', '11C_Other-natural', '11B_Forest-fires','11A_Volcanoes','6B_Other-not-in-total') ), ]

  # select only post 1970 for methane
  if ( em == 'CH4'){
    CEDS <- CEDS[,c( "iso","sector","fuel","units","Region","agg_Sector",'em',paste0('X', 1970:end_year))]
  }else{
    # select years
    CEDS <- CEDS[,c( "iso","sector","fuel","units","Region","agg_Sector",'em',x_ceds_years)]
  }
  # ---------------------------------------------------------------------------
  # 5. Remove sectors to make like with like comparison
  if (em != 'CO2') rcp_comparable <- RCP[which(RCP$Sector %!in% rcp_remove_sectors),]

  ceds_comparable <- CEDS[-which(CEDS$sector %in% ceds_remove_sectors),]

  ceds_comparable_global <- CEDS [-which(CEDS$sector %in% ceds_remove_sectors_global),]

  # ---------------------------------------------------------------------------
  # 6.  Aggregate and Combine Data
  # 6.1 Global CEDS and RCP

  #Aggregate CEDS to Global
  if ( em == 'CH4') x_years <- paste0('X',1970:2014)
  global_ceds <- aggregate(ceds_comparable_global[x_years],
                           by = list(em = ceds_comparable_global$em ),
                           FUN=sum )

  global_ceds$Inventory <- 'CEDS'
  global_ceds_long <- melt(global_ceds, id.vars = c('em','Inventory'))
  names(global_ceds_long) <- c("em","Inventory","year","total_emissions")
  global_ceds_long$year <- as.numeric(gsub('X', '', global_ceds_long$year) )

  if (em != 'CO2'){
    #Aggregate RCP to Global
    rcp_agg <- aggregate(rcp_comparable[,x_rcp_years],
                         by = list(em = rcp_comparable$em ),
                         FUN=sum )
    rcp_agg$Inventory <- 'CMIP5'
    global_rcp_long <- melt(rcp_agg, id.vars = c('em','Inventory'))
    names(global_rcp_long) <- c("em","Inventory","year","total_emissions")

    # Add ship emissions to global RCP
    if ( has_ship ) {
      # some cleaning up for selected rcp shipping emissions
      rcp_ship_emissions_long <- rcp_ship_emissions[ , c( "year", em ) ]
      rcp_ship_emissions_long$year <- paste0( 'X', rcp_ship_emissions_long$year )
      rcp_ship_emissions_long$Inventory <- "CMIP5"
      rcp_ship_emissions_long$em <- em
      rcp_ship_emissions_long <- rcp_ship_emissions_long[, c( "em", "Inventory", "year", em ) ]
      names(rcp_ship_emissions_long) <- c("em","Inventory","year","total_emissions")
      rcp_ship_emissions_long <- rcp_ship_emissions_long[which( rcp_ship_emissions_long$year %in% x_rcp_years),]
      # add rcp shipping emissions back to rcp emissions
      global_rcp_long <- merge( global_rcp_long, rcp_ship_emissions_long, by = c( "em", "Inventory", "year" )  )
      global_rcp_long$total_emissions <- global_rcp_long$total_emissions.x + global_rcp_long$total_emissions.y
      global_rcp_long <- global_rcp_long[ , c( "em", "Inventory", "year","total_emissions" ) ]
    }

    global_rcp <- cast( global_rcp_long , em + Inventory ~ year, value = 'total_emissions' )

    global_rcp_long$year <- as.numeric(gsub('X', '', global_rcp_long$year) )

    # Combine Global
    if(em != 'CO2') {
      global_long <- rbind( global_ceds_long, global_rcp_long )
      global_long$Inventory <- as.factor( global_long$Inventory )
    }
  }

  if(em == 'CO2') {
    global_long <- rbind( global_cdiac_long, global_ceds_long )
    global_long$Inventory <- as.factor( global_long$Inventory )
  }


  #--------------------------------------------------------------------------------------
  # 6.2 Sector Comparisons
  sector_ceds <- aggregate(ceds_comparable_global[x_years],
                           by = list(sector = ceds_comparable_global$agg_Sector ),FUN=sum )
  sector_ceds_long <- melt(sector_ceds, id.vars = c('sector'))
  names(sector_ceds_long) <- c('sector','year','total_emissions')
  sector_ceds_long$year <- as.numeric(gsub('X',"",sector_ceds_long$year))
  # add a data frame with 2 zero values for each sector, so legend will have all regions for sector figures
  sector_ceds_long <- rbind.fill(sector_ceds_long,
                                 data.frame('sector' = rep(sector_colors$sector, each = 2),
                                            'year' = rep(c(2010,2011),times = n_sectors),
                                            'total_emissions' = rep(0,times = n_sectors*2)))
  sector_ceds_long <- sector_ceds_long[!duplicated(sector_ceds_long[c('sector','year')]),]

  # 6.3 Region Comparisons
  region_ceds <- aggregate(ceds_comparable_global[x_years],
                           by = list(region = ceds_comparable_global$Region ),FUN=sum )
  region_ceds_long <- melt(region_ceds, id.vars = c('region'))
  names(region_ceds_long) <- c('region','year','total_emissions')
  region_ceds_long$year <- as.numeric(gsub('X',"",region_ceds_long$year))
  # add a data frame with 2 zero values for each region, so legend will have all regions
  region_ceds_long <- rbind.fill(region_ceds_long,
                                 data.frame('region' = rep(region_colors$region, each = 2),
                                            'year' = rep(c(2010,2011),times = n_regions),
                                            'total_emissions' = rep(0,times = n_regions*2)))
  region_ceds_long <- region_ceds_long[!duplicated(region_ceds_long[c('region','year')]),]


  # 6.3 Fuel Comparisons
  fuel_ceds <- aggregate(ceds_comparable_global[x_years],
                         by = list(fuel = ceds_comparable_global$fuel ),FUN=sum )
  fuel_ceds_long <- melt(fuel_ceds, id.vars = c('fuel'))
  names(fuel_ceds_long) <- c('fuel','year','total_emissions')
  fuel_ceds_long$year <- as.numeric(gsub('X',"",fuel_ceds_long$year))


  # ---------------------------------------------------------------------------
  # 7. Global Plots - only totals

  df <- global_long[,c('Inventory','year','total_emissions')]
  df$Inventory <- as.factor(df$Inventory)
  df$total_emissions <- df$total_emissions/1000
  if (em == 'CO2') df$total_emissions <- df$total_emissions/1000
  max <- 1.1*(max(df$total_emissions))
  if( em != 'CO2'){
    plot <- ggplot(df, aes(x=year,y=total_emissions,group=Inventory,shape=Inventory,linetype=Inventory)) +
      geom_line(data = subset(df, Inventory=='CEDS'),size=1, color = 'black') +
      geom_point(data = subset(df, Inventory=='CMIP5'),color='dodgerblue1') +
      scale_x_continuous(limits = c(CEDS_start_year,2015 ),
                         breaks= seq(from=CEDS_start_year, to=rcp_end_year, by=major),
                         minor_breaks = seq(from=CEDS_start_year, to=rcp_end_year, by=minor)) +
      scale_y_continuous(limits = c(0,max ),labels = comma)+
      ggtitle( em ) +
      labs(x= "" , y= paste(em, 'Emissions', unit ))+
      theme(panel.background=element_blank(),
            panel.grid.minor = element_line(colour="gray95"),
            panel.grid.major = element_line(colour="gray88"),
            panel.border = element_rect(colour = "grey85", fill=NA, size=.7))+
      scale_linetype_manual(name= 'Inventory',
                            breaks = c('CEDS','CMIP5'),
                            values = c('solid','blank'))+
      scale_shape_manual(name= 'Inventory',
                         breaks = c('CEDS','CMIP5'),
                         values = c(NA,19))
  }
  if( em == 'CO2'){
    plot <- ggplot(data = df, aes(x=year,y=total_emissions,linetype=Inventory, color =Inventory) ) +
      geom_line(size=1) +
      scale_x_continuous(limits = c(CEDS_start_year,2015 ),
                         breaks= seq(from=CEDS_start_year, to=rcp_end_year, by=major),
                         minor_breaks = seq(from=CEDS_start_year, to=rcp_end_year, by=25)) +
      scale_y_continuous(limits = c(0,max ),labels = comma)+
      ggtitle( em ) +
      labs(x= "" , y= paste(em, 'Emissions', unit ))+
      theme(panel.background=element_blank(),
            panel.grid.minor = element_line(colour="gray95"),
            panel.grid.major = element_line(colour="gray88"),
            panel.border = element_rect(colour = "grey80", fill=NA, size=.8))+
      scale_linetype_manual(name= 'Inventory',
                            breaks = c('CEDS','CDIAC'),
                            values = c('solid','dashed'))+
      scale_colour_manual(breaks = c('CEDS','CDIAC'),
                          values = c('black','dodgerblue'))

  }

  total_comparison_list[[h]] <- plot

  # ---------------------------------------------------------------------------
  # 8. Sector Stack Plots

  # Plot Data

  df <- rbind(global_cdiac_long[,c('Inventory','year','total_emissions')],
              global_rcp_long[,c('Inventory','year','total_emissions')])
  df$Inventory <- as.factor(df$Inventory)
  df$total_emissions <- df$total_emissions/1000
  if (em == 'CO2') df$total_emissions <- df$total_emissions/1000
  max <- 1.1*(max(df$total_emissions))
  df$sector <- 'none'

  df2 <- sector_ceds_long
  df2$total_emissions <- df2$total_emissions/1000
  if (em == 'CO2') df2$total_emissions <- df2$total_emissions/1000
  df2$Inventory <- NA
  df2$sector <- factor(df2$sector , levels = sectors[which(sectors != 'Air')] )
  df2 <- df2 %>% dplyr::arrange(sector)

  # Legend Plot
  no_air_sectors <- sector_colors[which(sector_colors$sector != 'Air'),'sector']
  no_air_colors <- sector_colors[which(sector_colors$sector != 'Air'),'color']
  Sector_plot <- ggplot(data= df2, aes(x=year,y=total_emissions)) +
    geom_area( data= df2, aes(x=year,y=total_emissions, fill = sector),  alpha = .7)+
    theme(legend.position="bottom")+
    guides(fill=guide_legend(nrow=1))+
    scale_fill_manual(name = 'Sector',
                      breaks = no_air_sectors,
                      values = no_air_colors)
  # Stacked Plots
  if( em != 'CO2'){
    plot <- ggplot(data= df2, aes(x=year,y=total_emissions)) +
      geom_area( data= df2, aes(x=year,y=total_emissions, fill = sector), alpha = .7,
                 position = 'stack', linetype = 1, size = .05 ,colour="grey40")+
      geom_point(data = subset(df,Inventory=='CMIP5'), aes(x=year,y=total_emissions, shape = Inventory)) +
      scale_x_continuous(limits = c(CEDS_start_year,CEDS_end_year ),
                         breaks= seq(from=CEDS_start_year, to=CEDS_end_year, by=major),
                         minor_breaks = seq(from=CEDS_start_year, to=CEDS_end_year, by=minor)) +
      scale_y_continuous(labels = comma)+
      ggtitle( em )+
      labs(x= "" , y= paste('Emissions', unit ))+
      theme(#legend.position="none",
        panel.background=element_blank(),
        panel.grid.minor = element_line(colour="gray95"),
        panel.grid.major = element_line(colour="gray88"),
        panel.border = element_rect(colour = "grey85", fill=NA, size=.7))+
      scale_shape_manual(name= 'Inventory',
                         values = c(19,NA))+
      scale_fill_manual(name = 'Sector',
                        breaks = sector_colors$sector,
                        values = sector_colors$color)

  }
  if( em == 'CO2'){
    Inventory_plot <- ggplot(data= df, aes(x=year,y=total_emissions, linetype = Inventory, shape = Inventory)) +
      geom_line(data = subset(df, Inventory == 'CDIAC'), aes(x=year,y=total_emissions)) +
      geom_point(data = subset(df, Inventory == 'CMIP5'), aes(x=year,y=total_emissions)) +
      theme(legend.position="bottom")+
      # guides(fill=guide_legend(nrow=1))+
      scale_fill_manual(name = 'Sector',
                        breaks = sector_colors$sector,
                        values = sector_colors$color)+
      scale_shape_manual("Inventory",
                         breaks=c('CDIAC','CMIP5'),
                         values = c(NA,19))+
      scale_linetype_manual("Inventory",
                            breaks=c('CDIAC','CMIP5'),
                            values = c('solid','blank'))

    plot <- ggplot(data= df, aes(x=year,y=total_emissions, linetype = Inventory, shape = Inventory)) +
      geom_area( data= df2, aes(x=year,y=total_emissions, fill = sector),  alpha = .7,
                 position = 'stack', linetype = 1, size = .05 ,colour="grey40")+
      geom_line(data = subset(df, Inventory == 'CDIAC'), aes(x=year,y=total_emissions, linetype = Inventory)) +
      geom_point(data = subset(df, Inventory == 'CMIP5'), aes(x=year,y=total_emissions), shape = 32) +
      scale_x_continuous(limits = c(CEDS_start_year,CEDS_end_year ),
                         breaks= seq(from=CEDS_start_year, to=CEDS_end_year, by=major),
                         minor_breaks = seq(from=CEDS_start_year, to=CEDS_end_year, by=minor)) +
      scale_y_continuous(labels = comma)+
      ggtitle( em )+
      labs(x= "" , y= paste('Emissions', unit ))+
      theme(
        panel.background=element_blank(),
        panel.grid.minor = element_line(colour="gray95"),
        panel.grid.major = element_line(colour="gray88"),
        panel.border = element_rect(colour = "grey85", fill=NA, size=.7))+
      scale_fill_manual(name = 'Sector',
                        breaks = sector_colors$sector,
                        values = sector_colors$color)
  }

  total_stacked_sector_list[[h]] <- plot

  # Line Graph - no comparison to CMIP5 or CDIAC
  total_line_sector_list[[h]] <- ggplot(data= df2, aes(x=year,y=total_emissions, color = sector)) +
    geom_line( size = 1, alpha = .7)+
    scale_x_continuous(limits = c(CEDS_start_year,CEDS_end_year ),
                       breaks= seq(from=CEDS_start_year, to=CEDS_end_year, by=major),
                       minor_breaks = seq(from=CEDS_start_year, to=CEDS_end_year, by=minor)) +
    scale_y_continuous(labels = comma)+
    ggtitle( em )+
    labs(x= "" , y= paste('Emissions', unit ))+
    theme(#legend.position="none",
      panel.background=element_blank(),
      panel.grid.minor = element_line(colour="gray95"),
      panel.grid.major = element_line(colour="gray88"),
      panel.border = element_rect(colour = "grey85", fill=NA, size=.7))+
    scale_color_manual(name = 'Sector',
                       breaks = sector_colors$sector,
                       values = sector_colors$color)

  # ---------------------------------------------------------------------------
  # 8. Region Stacked Plots

  df <- global_rcp_long[,c('Inventory','year','total_emissions')]
  if( em == 'CO2') df <- global_cdiac_long
  df$Inventory <- as.factor(df$Inventory)
  df$total_emissions <- df$total_emissions/1000
  if (em == 'CO2') df$total_emissions <- df$total_emissions/1000
  max <- 1.1*(max(df$total_emissions))

  df2 <- region_ceds_long
  df2$total_emissions <- df2$total_emissions/1000
  if (em == 'CO2') df2$total_emissions <- df2$total_emissions/1000
  df2$Inventory <- NA
  df2$region <- factor(df2$region , levels = regions )
  df2 <- df2 %>% dplyr::arrange(region)

  if( em != 'CO2'){
    plot <- ggplot(data= df2, aes(x=year,y=total_emissions)) +
      geom_area( data= df2, aes(x=year,y=total_emissions, fill = region), alpha = .7,
                 position = 'stack', linetype = 1, size = .05 ,colour="grey40")+
      geom_point(data = df, aes(x=year,y=total_emissions, shape = Inventory)) +
      scale_x_continuous(limits = c(CEDS_start_year,CEDS_end_year ),
                         breaks= seq(from=CEDS_start_year, to=CEDS_end_year, by=major),
                         minor_breaks = seq(from=CEDS_start_year, to=CEDS_end_year, by=minor)) +
      scale_y_continuous(labels = comma)+
      ggtitle( em )+
      labs(x= "" , y= paste('Emissions', unit ))+
      theme(#legend.position="none",
        panel.background=element_blank(),
        panel.grid.minor = element_line(colour="gray95"),
        panel.grid.major = element_line(colour="gray88"),
        panel.border = element_rect(colour = "grey85", fill=NA, size=.7))+
      scale_fill_manual(name = 'Region',
                        breaks = region_colors$region,
                        values = region_colors$color)+
      scale_shape_manual(name= 'Inventory',
                         values = c(19))
  }
  if( em == 'CO2'){

    Region_plot <- ggplot(data= df2, aes(x=year,y=total_emissions)) +
      geom_area( data= df2, aes(x=year,y=total_emissions, fill = region),  alpha = .7)+
      theme(legend.position="bottom")+
      # guides(fill=guide_legend(nrow=1))+
      scale_fill_manual(name = 'Region',
                        breaks = region_colors$region,
                        values = region_colors$color)

    plot <- ggplot(data= df2, aes(x=year,y=total_emissions)) +
      geom_area( data= df2, aes(x=year,y=total_emissions, fill = region), alpha = .7,
                 position = 'stack', linetype = 1, size = .05 ,colour="grey40")+
      geom_line(data = df, aes(x=year,y=total_emissions, linetype = Inventory)) +
      scale_x_continuous(limits = c(CEDS_start_year,CEDS_end_year ),
                         breaks= seq(from=CEDS_start_year, to=CEDS_end_year, by=major),
                         minor_breaks = seq(from=CEDS_start_year, to=CEDS_end_year, by=minor)) +
      scale_y_continuous(labels = comma)+
      ggtitle( em )+
      labs(x= "" , y= paste('Emissions', unit ))+
      theme(#legend.position="none",
        panel.background=element_blank(),
        panel.grid.minor = element_line(colour="gray95"),
        panel.grid.major = element_line(colour="gray88"),
        panel.border = element_rect(colour = "grey85", fill=NA, size=.7))+
      scale_fill_manual(name = 'Region',
                        breaks = region_colors$region,
                        values = region_colors$color)+
      scale_linetype_manual(name= 'Inventory',
                            values = c('solid'))
  }

  total_stacked_region_list[[h]] <- plot

  # Line Graph - no comparison to CMIP5 or CDIAC
  total_line_region_list[[h]] <- ggplot(data= df2, aes(x=year,y=total_emissions, color = region)) +
    geom_line( size = 1, alpha = 0.7)+
    scale_x_continuous(limits = c(CEDS_start_year,CEDS_end_year ),
                       breaks= seq(from=CEDS_start_year, to=CEDS_end_year, by=major),
                       minor_breaks = seq(from=CEDS_start_year, to=CEDS_end_year, by=minor)) +
    scale_y_continuous(labels = comma)+
    ggtitle( em )+
    labs(x= "" , y= paste('Emissions', unit ))+
    theme(#legend.position="none",
      panel.background=element_blank(),
      panel.grid.minor = element_line(colour="gray95"),
      panel.grid.major = element_line(colour="gray88"),
      panel.border = element_rect(colour = "grey85", fill=NA, size=.7))+
    scale_color_manual(name = 'Region',
                       breaks = region_colors$region,
                       values = region_colors$color)

  # ---------------------------------------------------------------------------
  # 8. Fuel Stacked Plots

  df <- global_rcp_long[,c('Inventory','year','total_emissions')]
  if( em == 'CO2') df <- global_cdiac_long
  df$Inventory <- as.factor(df$Inventory)
  df$total_emissions <- df$total_emissions/1000
  if (em == 'CO2') df$total_emissions <- df$total_emissions/1000
  max <- 1.1*(max(df$total_emissions))

  df2 <- fuel_ceds_long
  df2$total_emissions <- df2$total_emissions/1000
  if (em == 'CO2') df2$total_emissions <- df2$total_emissions/1000
  df2$Inventory <- NA
  df2$fuel <- factor(df2$fuel , levels = fuels )
  df2 <- df2 %>% dplyr::arrange(fuel)

  if( em != 'CO2'){
    plot <- ggplot(data= df2, aes(x=year,y=total_emissions)) +
      geom_area( data= df2, aes(x=year,y=total_emissions, fill = fuel), alpha = .7,
                 position = 'stack', linetype = 1, size = .05 ,colour="grey40")+
      geom_point(data = df, aes(x=year,y=total_emissions, shape = Inventory)) +
      scale_x_continuous(limits = c(CEDS_start_year,CEDS_end_year ),
                         breaks= seq(from=CEDS_start_year, to=CEDS_end_year, by=major),
                         minor_breaks = seq(from=CEDS_start_year, to=CEDS_end_year, by=minor)) +
      scale_y_continuous(labels = comma)+
      ggtitle( em )+
      labs(x= "" , y= paste('Emissions', unit ))+
      theme(#legend.position="none",
        panel.background=element_blank(),
        panel.grid.minor = element_line(colour="gray95"),
        panel.grid.major = element_line(colour="gray88"),
        panel.border = element_rect(colour = "grey85", fill=NA, size=.7))+
      scale_fill_manual(name = 'Fuel',
                        breaks = fuel_colors$fuel,
                        values = fuel_colors$color)+
      scale_shape_manual(name= 'Inventory',
                         values = c(19))
  }
  if( em == 'CO2'){

    Fuel_plot <- ggplot(data= df2, aes(x=year,y=total_emissions)) +
      geom_area( data= df2, aes(x=year,y=total_emissions, fill = fuel),  alpha = .7)+
      theme(legend.position="bottom")+
      guides(fill=guide_legend(nrow=1))+
      scale_fill_manual(name = 'Fuel',
                        breaks = fuel_colors$fuel,
                        values = fuel_colors$color)

    plot <- ggplot(data= df2, aes(x=year,y=total_emissions)) +
      geom_area( data= df2, aes(x=year,y=total_emissions, fill = fuel), alpha = .7,
                 position = 'stack', linetype = 1, size = .05 ,colour="grey40")+
      geom_line(data = df, aes(x=year,y=total_emissions, linetype = Inventory)) +
      scale_x_continuous(limits = c(CEDS_start_year,CEDS_end_year ),
                         breaks= seq(from=CEDS_start_year, to=CEDS_end_year, by=major),
                         minor_breaks = seq(from=CEDS_start_year, to=CEDS_end_year, by=minor)) +
      scale_y_continuous(labels = comma)+
      ggtitle( em )+
      labs(x= "" , y= paste('Emissions', unit ))+
      theme(#legend.position="none",
        panel.background=element_blank(),
        panel.grid.minor = element_line(colour="gray95"),
        panel.grid.major = element_line(colour="gray88"),
        panel.border = element_rect(colour = "grey85", fill=NA, size=.7))+
      scale_fill_manual(name = 'Fuel',
                        breaks = fuel_colors$fuel,
                        values = fuel_colors$color)+
      scale_linetype_manual(name= 'Inventory',
                            values = c('solid'))
  }

  total_stacked_fuel_list[[h]] <- plot

  # Line Graph - no comparison to CMIP5 or CDIAC
  total_line_fuel_list[[h]] <- ggplot(data= df2, aes(x=year,y=total_emissions, color = fuel)) +
    geom_line( size = 1, alpha = .7)+
    scale_x_continuous(limits = c(CEDS_start_year,CEDS_end_year ),
                       breaks= seq(from=CEDS_start_year, to=CEDS_end_year, by=major),
                       minor_breaks = seq(from=CEDS_start_year, to=CEDS_end_year, by=minor)) +
    scale_y_continuous(labels = comma)+
    ggtitle( em )+
    labs(x= "" , y= paste('Emissions', unit ))+
    theme(#legend.position="none",
      panel.background=element_blank(),
      panel.grid.minor = element_line(colour="gray95"),
      panel.grid.major = element_line(colour="gray88"),
      panel.border = element_rect(colour = "grey85", fill=NA, size=.7))+
    scale_color_manual(name = 'Fuel',
                       breaks = fuel_colors$fuel,
                       values = fuel_colors$color)

  # ---------------------------------------------------------------------------
} # end emissions loop

# Legend Processing
total_comparison_nolegend_list <- lapply(total_comparison_list, function(x) x + theme(legend.position="none"))
total_stacked_sector_nolegend_list <- lapply(total_stacked_sector_list, function(x) x + theme(legend.position="none"))
total_stacked_region_nolegend_list <- lapply(total_stacked_region_list, function(x) x + theme(legend.position="none"))
total_stacked_fuel_nolegend_list <- lapply(total_stacked_fuel_list, function(x) x + theme(legend.position="none"))
total_line_sector_nolegend_list <- lapply(total_line_sector_list, function(x) x + theme(legend.position="none"))
total_line_region_nolegend_list <- lapply(total_line_region_list, function(x) x + theme(legend.position="none"))
total_line_fuel_nolegend_list <- lapply(total_line_fuel_list, function(x) x + theme(legend.position="none"))

Inventory_plot_legend <- g_legend(Inventory_plot)
Sector_plot_legend <- g_legend(Sector_plot)
Region_plot_legend <- g_legend(Region_plot)
Fuel_plot_legend <- g_legend(Fuel_plot)

# Stacked Sector
pdf(paste0('../diagnostic-output/paper-figures/Supplement/Recent_Paper_Figures_sector_stacked.pdf'),width=10,height=10,paper='special', onefile=F)
grid.arrange(arrangeGrob(total_stacked_sector_nolegend_list[[1]],total_stacked_sector_nolegend_list[[2]],total_stacked_sector_nolegend_list[[3]],
                         total_stacked_sector_nolegend_list[[4]],total_stacked_sector_nolegend_list[[5]],total_stacked_sector_nolegend_list[[6]],
                         total_stacked_sector_nolegend_list[[7]],total_stacked_sector_nolegend_list[[8]],total_stacked_sector_nolegend_list[[9]],ncol=3),
             arrangeGrob(Sector_plot_legend,Inventory_plot_legend, ncol=1),
             ncol=1, heights = c(10,1))
dev.off()

# Line Sector
pdf(paste0('../diagnostic-output/paper-figures/Supplement/Recent_Paper_Figures_sector_line.pdf'),width=10,height=10,paper='special', onefile=F)
grid.arrange(arrangeGrob(total_line_sector_nolegend_list[[1]],total_line_sector_nolegend_list[[2]],total_line_sector_nolegend_list[[3]],
                         total_line_sector_nolegend_list[[4]],total_line_sector_nolegend_list[[5]],total_line_sector_nolegend_list[[6]],
                         total_line_sector_nolegend_list[[7]],total_line_sector_nolegend_list[[8]],total_line_sector_nolegend_list[[9]],ncol=3),
             Sector_plot_legend,
             ncol=1, heights = c(10,1))
dev.off()

# Stacked Region
pdf(paste0('../diagnostic-output/paper-figures/Paper/Recent_Paper_Figures_region_stacked.pdf'),width=10,height=10,paper='special', onefile=F)
grid.arrange(arrangeGrob(total_stacked_region_nolegend_list[[1]],total_stacked_region_nolegend_list[[2]],total_stacked_region_nolegend_list[[3]],
                         total_stacked_region_nolegend_list[[4]],total_stacked_region_nolegend_list[[5]],total_stacked_region_nolegend_list[[6]],
                         total_stacked_region_nolegend_list[[7]],total_stacked_region_nolegend_list[[8]],total_stacked_region_nolegend_list[[9]],ncol=3),
             arrangeGrob(Region_plot_legend,Inventory_plot_legend, ncol=1),
             ncol=1, heights = c(10,1))
dev.off()

# Line Region
pdf(paste0('../diagnostic-output/paper-figures/Supplement/Recent_Paper_Figures_region_line.pdf'),width=10,height=10,paper='special', onefile=F)
grid.arrange(arrangeGrob(total_line_region_nolegend_list[[1]],total_line_region_nolegend_list[[2]],total_line_region_nolegend_list[[3]],
                         total_line_region_nolegend_list[[4]],total_line_region_nolegend_list[[5]],total_line_region_nolegend_list[[6]],
                         total_line_region_nolegend_list[[7]],total_line_region_nolegend_list[[8]],total_line_region_nolegend_list[[9]],ncol=3),
             Region_plot_legend,
             ncol=1, heights = c(10,1))
dev.off()

# Stacked fuel
pdf(paste0('../diagnostic-output/paper-figures/Supplement/Recent_Paper_Figures_fuel_stacked.pdf'),width=10,height=10,paper='special', onefile=F)
grid.arrange(arrangeGrob(total_stacked_fuel_nolegend_list[[1]],total_stacked_fuel_nolegend_list[[2]],total_stacked_fuel_nolegend_list[[3]],
                         total_stacked_fuel_nolegend_list[[4]],total_stacked_fuel_nolegend_list[[5]],total_stacked_fuel_nolegend_list[[6]],
                         total_stacked_fuel_nolegend_list[[7]],total_stacked_fuel_nolegend_list[[8]],total_stacked_fuel_nolegend_list[[9]],ncol=3),
             arrangeGrob(Fuel_plot_legend,Inventory_plot_legend, ncol=1),
             ncol=1, heights = c(10,1))
dev.off()


# Line Fuel
pdf(paste0('../diagnostic-output/paper-figures/Supplement/Recent_Paper_Figures_fuel_line.pdf'),width=10,height=10,paper='special', onefile=F)
grid.arrange(arrangeGrob(total_line_fuel_nolegend_list[[1]],total_line_fuel_nolegend_list[[2]],total_line_fuel_nolegend_list[[3]],
                         total_line_fuel_nolegend_list[[4]],total_line_fuel_nolegend_list[[5]],total_line_fuel_nolegend_list[[6]],
                         total_line_fuel_nolegend_list[[7]],total_line_fuel_nolegend_list[[8]],total_line_fuel_nolegend_list[[9]],ncol=3),
             Fuel_plot_legend,
             ncol=1, heights = c(10,1))
dev.off()

# ---------------------------------------------------------------------------
logStop()
