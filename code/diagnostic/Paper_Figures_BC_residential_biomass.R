# ------------------------------------------------------------------------------
# Program Name: Paper_Figures_BC_residential_biomass.R
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
log_msg <- "Processing Paper and Supplement Figures for residential biomass and population side by side figures" # First message to be printed to the log
script_name <- "Paper_Figures_BC_residential biomass"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# Load Packages

library('ggplot2')
library('plyr')
library('scales')
library('gridExtra')
require(grid)

# function from stack exchange to extract legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# ---------------------------------------------------------------------------
# 1. Load species independent files

Master_Country_List <- readData('MAPPINGS', 'Master_Country_List')
MSLevel <- readData('MAPPINGS', 'Master_Sector_Level_map')
pop_master <- readData('MED_OUT', 'A.UN_pop_master')
BC_em <- readData('MED_OUT', 'BC_total_CEDS_emissions')

# ---------------------------------------------------------------------------
# 1. Plot Settings

start <- 1960
end <- end_year
plot_years <- start:end
major_break <- 10
minor_break <- 5

residential_sectors <- c("1A4b_Residential","1A4a_Commercial-institutional")


# ---------------------------------------------------------------------------
# 1.

# population data
# calculation rural population
population <- pop_master[which(pop_master$year %in% plot_years),]
population$rural_popularion <- (1-population$urban_share)*population$pop

# add region map
population$region <- Master_Country_List[match(population$iso,Master_Country_List$iso),'Paper_Figure_Region']

#aggregate
population_region <- aggregate(population$pop, 
                               by = list(year = population$year,
                                         region = population$region), sum)
# emissions data
plot_em <- BC_em[which(BC_em$sector %in% residential_sectors),]
plot_em <- BC_em[which(BC_em$fuel %in% 'biomass'),]
plot_em$region <- Master_Country_List[match(plot_em$iso,Master_Country_List$iso),'Paper_Figure_Region']
plot_em <- plot_em[which(plot_em$region %!in% 'Shipping/Air'),]
em_region <- aggregate(plot_em[paste0('X',plot_years)], 
                       by = list(region = plot_em$region), sum)
em_region<- melt(em_region)
em_region$variable <- as.numeric(gsub(pattern = 'X',replacement = "", x = em_region$variable))
names(em_region) <- c('region','year','x')

# ---------------------------------------------------------------------------
# 2. Plot

# population

max <- max(em_region$x)
emissions_plot <- ggplot(data = em_region, aes(x=year,y=x, color = region) ) + 
  geom_line(size=1, alpha = 0.5) +
  scale_x_continuous(limits = c(start,end ),
                     breaks= seq(from=start, to=end, by=major_break),
                     minor_breaks = seq(from=start, to=end, by=minor_break)) +
  scale_y_continuous(limits = c(0,max ),labels = comma)+
  ggtitle( 'BC Residential Biomass Emissions' ) +
  labs(x= "" , y= paste('BC Emissions [Gg]' ))+
  theme(panel.background=element_blank(),
        panel.grid.minor = element_line(colour="gray95"),
        panel.grid.major = element_line(colour="gray88"),
        panel.border = element_rect(colour = "grey80", fill=NA, size=.8))+
  scale_color_discrete(name = 'Region')

population_region$x <- population_region$x/1000
max <- max(population_region$x)
population_plot <- ggplot(data = population_region, aes(x=year,y=x, color = region) ) + 
  geom_line(size=1, alpha = 0.5) +
  scale_x_continuous(limits = c(start,end ),
                     breaks= seq(from=start, to=end, by=major_break),
                     minor_breaks = seq(from=start, to=end, by=minor_break)) +
  scale_y_continuous(limits = c(0,max ),labels = comma)+
  ggtitle( 'Rural Population' ) +
  labs(x= "" , y= paste('Rural Population [Thousands]' ))+
  theme(panel.background=element_blank(),
        panel.grid.minor = element_line(colour="gray95"),
        panel.grid.major = element_line(colour="gray88"),
        panel.border = element_rect(colour = "grey80", fill=NA, size=.8))+
  scale_color_discrete(name = 'Region')
population_plot



file_name <- paste0('../diagnostic-output/paper-figures/Supplement/Residential_biomass.pdf')
pdf( file_name ,width=8,height=4,paper='special', onefile=T)
grid.arrange(emissions_plot+ theme(legend.position="none"),
             population_plot+ theme(legend.position="none"), 
             g_legend(population_plot),ncol = 3, widths= c(1,1,.5))
dev.off()



# ---------------------------------------------------------------------------
# 

logStop()