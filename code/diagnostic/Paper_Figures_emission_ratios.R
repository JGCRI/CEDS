# ------------------------------------------------------------------------------
# Program Name: Paper_Figures_emission_ratios.R
# Author(s): Rachel Hoesly
# Date Last Updated: 28 Dec 2016
# Program Purpose: Produces diagnostic figures of emissions ratios BC adn NOx over CO    
# Input Files: [em]_total_CEDS_emissions.csv
# Output Files: Figures in the diagnostic-output
# Notes: 
# TODO: 1. 
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Set working directory to the CEDS “input” directory and define PARAM_DIR as the
# location of the CEDS “parameters” directory, relative to the new working directory.
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
# provides logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R",
              'common_data.R', 'IO_functions.R')# Any additional function files required
log_msg <- "Paper figures for emission ratios" 
script_name <- "Paper_Figures_emission_ratios.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "BC"

if ( em %!in!)

# ------------------------------------------------------------------------------
# 0.5 Load Packages
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "NOx"

# ------------------------------------------------------------------------------
# 1. Load Data

em_emissions <- readData('MED_OUT', paste0(em, '_total_CEDS_emissions'))
CO_emissions <- readData('MED_OUT', 'CO_total_CEDS_emissions')

# ------------------------------------------------------------------------------
# 2. Define Values

# Unit conversions
# ? mole/mole = ktNOx/ktCO * (12+16)gCO/moleCO / (40.03gNOx/molNOx)
# https://www.quora.com/What-is-the-molar-mass-g-mol-of-NOx

if( em == 'NOx') molar_mass_em <- 40.03
molar_mass_CO <- 28

# Road Emissions
CO_road <- CO_emissions[which(CO_emissions$sector == "1A3b_Road"), ]
CO_road <- aggregate(CO_road[X_extended_years], by = list(iso = CO_road$iso), sum)

em_road <- em_emissions[which(em_emissions$sector == "1A3b_Road"), ]
em_road <- aggregate(em_road[X_extended_years], by = list(iso = em_road$iso), sum)

#Check order
if(!identical(CO_road$iso, em_road$iso)) stop('Data frames are not in identical order, please sort')

# Calculate 
em_over_CO_road <- em_road
em_over_CO_road[X_extended_years] <- em_road[X_extended_years]/CO_road[X_extended_years]
em_over_CO_road <- melt(em_over_CO_road, id.vars = c('iso'))
names(em_over_CO_road)[which(names(em_over_CO_road) == 'value')] <- 'ratio' 
names(em_over_CO_road)[which(names(em_over_CO_road) == 'variable')] <- 'year' 
em_over_CO_road <- em_over_CO_road[-which(is.na(em_over_CO_road$ratio)),]
em_over_CO_road$ratio <- em_over_CO_road$ratio*molar_mass_CO/molar_mass_em
  
# Total Emissions
CO_total <- aggregate(CO_emissions[X_extended_years], by = list(iso = CO_emissions$iso), sum)
em_total <- aggregate(em_emissions[X_extended_years], by = list(iso = em_emissions$iso), sum)

#Check order
if(!identical(CO_road$iso, em_road$iso)) stop('Data frames are not in identical order, please sort')

# Calculate 
em_over_CO_total <- em_total
em_over_CO_total[X_extended_years] <- em_total[X_extended_years]/CO_total[X_extended_years]
em_over_CO_total <- melt(em_over_CO_total, id.vars = c('iso'))
names(em_over_CO_total)[which(names(em_over_CO_total) == 'value')] <- 'ratio' 
names(em_over_CO_total)[which(names(em_over_CO_total) == 'variable')] <- 'year' 
em_over_CO_total <- em_over_CO_total[-which(is.na(em_over_CO_total$ratio)),]
em_over_CO_total$ratio <- em_over_CO_total$ratio*molar_mass_CO/molar_mass_em

# ------------------------------------------------------------------------------
# 4. Plot
plot_countries <- c('usa','fra','gbr')
x_axis_start <- 1950
x_axis_end <- 2014

# road
plot_df <- em_over_CO_road
plot_df <- plot_df[which(plot_df$iso %in% plot_countries),]  
plot_df$year <- as.numeric(gsub('X','',plot_df$year))

plot_road <- ggplot( plot_df, aes(x=year,y=ratio,colour=iso)) +
  geom_line(size=1) +
  scale_x_continuous(limits = c(x_axis_start,x_axis_end ),
                     breaks= seq(from=x_axis_start, to=x_axis_end, by=10),
                     minor_breaks = seq(from=x_axis_start, to=x_axis_end, by=5)) +
  labs(x= "" , y= paste0(em,'/CO [mole/mole]') )+
  theme(panel.background=element_blank(),
        panel.grid.minor = element_line(colour="gray95"),
        panel.grid.major = element_line(colour="gray88"),
        panel.border = element_rect(colour = "gray80", fill=NA, size=1))+
  ggtitle('Road Emissions')

# total
plot_df <- em_over_CO_total
plot_df <- plot_df[which(plot_df$iso %in% plot_countries), ]  
plot_df$year <- as.numeric(gsub('X','',plot_df$year))

plot_total <- ggplot( plot_df, aes(x=year,y=ratio,colour=iso)) +
  geom_line(size=1) +
  scale_x_continuous(limits = c(x_axis_start,x_axis_end ),
                     breaks= seq(from=x_axis_start, to=x_axis_end, by=10),
                     minor_breaks = seq(from=x_axis_start, to=x_axis_end, by=5)) +
  labs(x= "" , y= paste0(em,'/CO [mole/mole]') )+
  theme(panel.background=element_blank(),
        panel.grid.minor = element_line(colour="gray95"),
        panel.grid.major = element_line(colour="gray88"),
        panel.border = element_rect(colour = "gray80", fill=NA, size=1))+
  ggtitle('Total Emissions')

pdf(paste0('../diagnostic-output/paper-figures/Paper/Paper_Figure_',em,'_ratios.pdf'),width=8,height=3,paper='special', onefile=F)
grid.arrange(plot_road,plot_total,ncol=2)
dev.off()

# ------------------------------------------------------------------------------
# 5. Write Data

writeData(BC_over_CO_total, 'DIAG_OUT', 'BC_over_CO_ratio')
writeData(NOx_over_CO_total, 'DIAG_OUT', 'NOx_over_CO_ratio')


# ------------------------------------------------------------------------------
# 6. End Script

logStop()

