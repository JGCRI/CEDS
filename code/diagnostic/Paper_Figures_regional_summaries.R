# ------------------------------------------------------------------------------
# Program Name: Paper_Figures_regional_summaries.R
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
              'common_data.R', 'IO_functions.R', 'data_functions.R', 'timeframe_functions.R') # Additional function files may be required.
log_msg <- "Processing Paper and Supplement Figures for all emission species" # First message to be printed to the log
script_name <- "Paper_Figures_regional_summaries.R"

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

# ---------------------------------------------------------------------------
# Start Emissions Loop
em_list <- c('SO2','NOx','BC','OC','NH3','CO','NMVOC', 'CO2')

# Create Plot Lists
start <- 1750
end <- 2014
plot_years <- start:end
X_plot_years <- paste0('X',start:end)

em_plot_list <- list()
em_plot_list_nolegend <- list()

for( h in seq_along(em_list)){ 
  
  em <- em_list[h]
  
# ---------------------------------------------------------------------------
# 2. Other script Options
  
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
 
  # add sector
  CEDS$agg_Sector <- MSLevel[match(CEDS$sector,MSLevel$working_sectors_v1),'Figure_sector']
  CEDS$agg_Sector <- as.factor(CEDS$agg_Sector)
  
  # add region
  CEDS$Region <- Master_Country_List[match(CEDS$iso,Master_Country_List$iso),'Paper_Figure_Region']
  
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
  
  df <- sector_region_ceds_long
  df$emissions <- df$emissions/1000
  
  # region loop
  regions <- unique(df$region)
  region_plot_list <- list()
  region_plot_list_nolegend <- list()

  for (k in seq_along(regions)){
  
  df_plot <- df[which(df$region == regions[k]),]
  max <- 1.1*(max(df_plot$emissions))
  
    plot <- ggplot(data = df_plot, aes(x=year,y=emissions, color = sector) ) + 
      geom_line(size=1) +
      scale_x_continuous(limits = c(start,end ),
                         breaks= seq(from=start, to=end, by=50),
                         minor_breaks = seq(from=start, to=end, by=25)) +
      scale_y_continuous(limits = c(0,max ),labels = comma)+
      ggtitle( em ) +
      labs(x= "" , y= paste(em, 'Emissions', unit ))+
      theme(panel.background=element_blank(),
            panel.grid.minor = element_line(colour="gray95"),
            panel.grid.major = element_line(colour="gray88"),
            panel.border = element_rect(colour = "grey80", fill=NA, size=.8))+
      scale_color_discrete(name = 'Sector')

  region_plot_list[[k]] <- plot
  region_plot_list_nolegend[[k]] <- plot + theme(legend.position="none")

}
  em_plot_list[[h]] <-  region_plot_list  
  em_plot_list_nolegend[[h]] <-  region_plot_list_nolegend

} # end emissions loop
# ---------------------------------------------------------------------------
# 6. Arrange plots

# loop for region
for(k in seq_along(regions)){
leg <- g_legend( em_plot_list[[1]][[k]] )
file_name <- paste0('../diagnostic-output/paper-figures/Supplement/Regional_summaries_',regions[k],'.pdf')
if(regions[[k]] == "Other Asia/Pacific") file_name <- paste0('../diagnostic-output/paper-figures/Supplement/Regional_summaries_OtherAsia.pdf')
if(regions[[k]] == "Shipping/Air") file_name <- paste0('../diagnostic-output/paper-figures/Supplement/Regional_summaries_Shipping.pdf')

pdf( file_name ,width=9.5,height=9.5,paper='special', onefile=F)
grid.arrange(em_plot_list_nolegend[[1]][[k]],em_plot_list_nolegend[[2]][[k]],em_plot_list_nolegend[[3]][[k]],
             em_plot_list_nolegend[[4]][[k]],em_plot_list_nolegend[[5]][[k]],em_plot_list_nolegend[[6]][[k]],
             em_plot_list_nolegend[[7]][[k]],em_plot_list_nolegend[[8]][[k]],leg,
             ncol=3, top=textGrob(regions[[k]], gp=gpar(fontsize=16,font=8)))
dev.off()
}

# ---------------------------------------------------------------------------
logStop()
