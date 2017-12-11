# ------------------------------------------------------------------------------
# Program Name: Paper_Figures_sector_summaries.R
# Author: Rachel Hoesly
# Date Last Updated:
# Program Purpose:
#
# Input Files: [em]_total_CEDS_emissions.csv
# Output Files: figures in the diagnostic-output
# Note:
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
log_msg <- "Processing Paper and Supplement Figures for all emission species" # First message to be printed to the log
script_name <- "Paper_Figures_sector_summaries.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# Load Packages

library('ggplot2')
library('plyr')
library('scales')
library('gridExtra')
require(grid)

# function from stack exchange
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# ---------------------------------------------------------------------------
# 1. Load species independent files

Master_Country_List <- readData('MAPPINGS', 'Master_Country_List')
MSLevel <- readData('MAPPINGS', 'Master_Sector_Level_map')

source('../code/diagnostic/Paper_Figures_plot_colors.R')

# ---------------------------------------------------------------------------
# Start Emissions Loop
em_list <- c('SO2','NOx','CO','OC','BC','NH3','NMVOC','CO2','CH4')

# Create Plot Lists

em_plot_list <- list()
em_plot_list_nolegend <- list()

for( h in seq_along(em_list)){

  em <- em_list[h]

  # ---------------------------------------------------------------------------
  # 2. Other script Options

  start <- 1750
  end <- 2014
  plot_years <- start:end
  X_plot_years <- paste0('X',start:end)
  if( em == 'CH4') X_plot_years <- paste0('X',1970:end)
  major_break <- 50
  minor_break <- 25
  unit <- '[Tg/year]'
  unit <- '[Tg/year]'

  if (em == 'SO2') unit <- '[Tg SO2/year]'
  if (em == 'NOx') unit <- '[Tg NO2/year]'
  if (em %in% c('OC','BC')) unit <- '[Tg C/year]'
  if (em == 'NH3') unit <- '[Tg NH3/year]'
  if (em == 'CO') unit <- '[Tg CO/year]'
  if (em == 'NMVOC') unit <- '[Tg NMVOC/year]'
  if (em == 'CO2') unit <- '[Tg CO2/year]'

  # ---------------------------------------------------------------------------
  # 3. Load and Process CEDS Emissions Data

  CEDS <- readData('MED_OUT', paste0(em,'_total_CEDS_emissions'))

  # rename other process emissions to tanker loading
  CEDS[which(CEDS$sector == '2L_Other-process-emissions'),'sector'] <- '1A3di_International-shipping'

  # rename international shipping to global
  CEDS[which(CEDS$sector %in% c('1A3ai_International-aviation','1A3di_International-shipping','1A3aii_Domestic-aviation')),'iso'] <- 'global'

  # add sector
  CEDS$agg_Sector <- MSLevel[match(CEDS$sector,MSLevel$working_sectors_v1),'Figure_sector']
  CEDS$agg_Sector <- as.factor(CEDS$agg_Sector)

  # add region
  CEDS$Region <- Master_Country_List[match(CEDS$iso,Master_Country_List$iso),'Paper_Figure_Region']

  # remove other total
  other <- CEDS[ which(CEDS$sector %in% c('6A_Other-in-total', '11C_Other-natural', '11B_Forest-fires','11A_Volcanoes','6B_Other-not-in-total')), ]
  other_sum <- sum(other[X_extended_years])
  if(other_sum != 0) stop('There are non zero emissions in "other in total". Please check.')
  CEDS <- CEDS[-which(CEDS$sector %in% c('6A_Other-in-total', '11C_Other-natural', '11B_Forest-fires','11A_Volcanoes','6B_Other-not-in-total') ), ]

  #--------------------------------------------------------------------------------------
  # 4. Sector Comparisons
  sector_region_ceds <- aggregate(CEDS[X_plot_years],
                                  by = list(region = CEDS$Region,
                                            sector = CEDS$agg_Sector ),
                                  FUN=sum )
  sector_region_ceds_long <- melt(sector_region_ceds, id.vars = c('region','sector'))
  names(sector_region_ceds_long) <- c('region','sector','year','emissions')
  sector_region_ceds_long$year <- as.numeric(gsub('X',"",sector_region_ceds_long$year))

  # ---------------------------------------------------------------------------
  # 5. Global Plots - only totals

  # unit conversion for emissions results (Gg to Tg)
  df <- sector_region_ceds_long
  df$emissions <- df$emissions/1000
  df$region <- factor(df$region , levels = regions )
  df <- df %>% dplyr::arrange(region)

  # Produce legend with all data
  df_color <- region_colors
  df_color$region <- factor(df_color$region , levels = regions )
  df_color$value <- 1
  region_legend_plot <- ggplot(df_color, aes(region, value, color = region))+
    geom_point(size = 7, shape = 15)+
    theme(legend.position="bottom")+
    scale_color_manual(name = 'Region',
                       breaks = region_colors$region,
                       values = region_colors$color)
  # sector loop
  sector_plot_list <- list()
  sector_plot_list_nolegend <- list()

  # create a data frame with 2 zero values for each region, so legend will have all regions for sector figures
  zero_df <- data.frame('region' = rep(region_colors$region, each = 2),
                        'year' = rep(c(2010,2011),times = n_regions),
                        'emissions' = rep(0,times = n_regions*2))

  for (k in seq_along(sectors)){

    df_plot <- df[which(df$sector == sectors[k]),]
    df_plot <- df_plot[which(df_plot$emissions != 0),]
# add zero values, then remove duplicates, so that df_plot has a data entry for all regions
    df_plot <- rbind.fill(df_plot,zero_df)
    df_plot <- df_plot[!duplicated(df_plot[c('region','year')]),]
# check for all regions
    if(length(unique(df_plot$region)) != n_regions) stop("figure doesn't have all regions. Legend won't work")
    if(nrow(df_plot) == 0) df_plot <- zero_df

    max <- 1.1*(max(df_plot$emissions))
    if(max < 1) max <- 1
    # if(max < 1 & em %!in% c('BC','OC')) max <- 1
    # if(max == 0) max <- 1

    plot <- ggplot(data = df_plot, aes(x=year,y=emissions, color = region) ) +
      geom_line(size=1, alpha=.7) +
      scale_x_continuous(limits = c(start,end ),
                         breaks= seq(from=start, to=end, by=major_break),
                         minor_breaks = seq(from=start, to=end, by=minor_break)) +
      scale_y_continuous(limits = c(0,max ),labels = comma)+
      ggtitle( em ) +
      # ggtitle( sectors[k] )+
      labs(x= "" , y= paste(em, 'Emissions', unit ))+
      theme(panel.background=element_blank(),
            panel.grid.minor = element_line(colour="gray95"),
            panel.grid.major = element_line(colour="gray88", width = 5),
            panel.border = element_rect(colour = "grey80", fill=NA, size=.8))+
      geom_hline(yintercept = 0, size = .5, color = 'white') +
      geom_hline(yintercept = 0, size = .25, color = 'grey3') +
      scale_color_manual(name = 'Region',
                         breaks = region_colors$region,
                         values = region_colors$color)
    # if all emissions equal zero, clean up graph
    if( sum(df_plot$emissions) == 0 ){
      plot <- plot +
        geom_hline(yintercept = 0, size = 1.1, color = 'white') +
        geom_hline(yintercept = 0, size = .25, color = 'grey3')
    }

    sector_plot_list[[k]] <- plot
    sector_plot_list_nolegend[[k]] <- plot + theme(legend.position="none")
    # ggsave(paste0(em,sectors[k],'.pdf'))
  }

  em_plot_list[[h]] <-  sector_plot_list
  em_plot_list_nolegend[[h]] <-  sector_plot_list_nolegend

} # end emissions loop
# ---------------------------------------------------------------------------
# 6. Arrange plots

# loop for region
for(k in seq_along(sectors)){
  leg <- g_legend( region_legend_plot )

  sector_name <- sectors[k]
  sector_name <- gsub('/','_',sector_name)
  sector_name <- gsub(' ','_',sector_name)

  file_name <- paste0('../diagnostic-output/paper-figures/Supplement/Sector_summaries_',sector_name,'.pdf')

  pdf( file_name ,width=9.5,height=9.5,paper='special', onefile=F)
  grid.arrange(arrangeGrob(em_plot_list_nolegend[[1]][[k]],em_plot_list_nolegend[[2]][[k]],em_plot_list_nolegend[[3]][[k]],
               em_plot_list_nolegend[[4]][[k]],em_plot_list_nolegend[[5]][[k]],em_plot_list_nolegend[[6]][[k]],
               em_plot_list_nolegend[[7]][[k]],em_plot_list_nolegend[[8]][[k]],em_plot_list_nolegend[[9]][[k]],ncol = 3),
  leg,
  ncol=1, heights = c(10,1),top=textGrob(sectors[[k]], gp=gpar(fontsize=16,font=8)))
  dev.off()
}

# ---------------------------------------------------------------------------
logStop()
