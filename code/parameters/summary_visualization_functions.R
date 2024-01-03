# ----------------------------------------------------------------------------------
# CEDS R header file: summary visualization functions
# Authors: Rachel Hoesly
# Last Updated: January 24, 2023

# This file should be sourced by any R script running diagnostics on CEDS data.
# Functions contained:
#   diagnostic_compare_plot

# ----------------------------------------------------------------------------------
# summary_comparison_plots
# Brief:         Produces plots to compare the current Version with the last Version
# Details:
# Dependencies:
# Author(s):
# Params:   country_select: vector or isos to be plotted individually
# Return:
# Input Files:
# Output Files:  list of summary plots (ggplot objects)

# debug # # later add country select option
# country_select <- c('usa')

summary_comparison_plots <- function(
                          MCL = Master_Country_List,
                          MSL = Master_Sector_Level_map,
                          CS = country_select,
                          EM = em){
printLog("Create Comparison plots for current vs previous data")
# Plot options
graph_start <- min(extended_years)
graph_end <- max(extended_years)
year_breaks <- 10

# Plot theme
theme_diagnostic <- list(theme_minimal(),
                         theme(panel.background = element_rect(fill = "#D8D9DA",colour = "#D8D9DA",size = 0.5, linetype = "solid")),
                         theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white")),
                         theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white")))

version_comparison_formatting <- list(
    scale_x_continuous( breaks = seq( from = graph_start, to = graph_end, by = year_breaks ) ,
                    limits = c(1960, graph_end+1)) ,
    #scale_y_continuous( labels = 'comma' ),
    guides( linetype = guide_legend( override.aes = list( size = c( 1.5, 0.5 ) ) ) ),
    labs( x = "" , y = 'Emissions [Gg/yr]' ),
    ggtitle(EM),
    scale_linetype_manual( name= 'Version',
                       breaks = c( 'Last' ,'Current'),
                       values = c( 'solid','solid' ) ) )

# Read in current data and previous data
# Some of this is already loaded into the the summary script, but just re-read
# here for ease of understanding and compartmentalizing the figures function

last_Em_by_Country_Sector  <-  readData( "FIN_OUT", paste0( EM, "_last-run/",
                                              list.files( paste0( "../final-emissions/", EM, "_last-run/" ),
                                              pattern = paste0('CEDS_',EM,'_emissions_by_country_sector') ) ),
                                              meta = F )

last_Em_by_Country_Fuel  <-  readData( "FIN_OUT", paste0( EM, "_last-run/",
                                              list.files( paste0( "../final-emissions/", EM, "_last-run/" ),
                                              pattern = paste0('CEDS_',EM,'_emissions_by_country_fuel') ) ),
                                              meta = F )

current_Em_by_Country_Sector  <-  readData( "FIN_OUT", paste0("current-Versions/",
                                              list.files( paste0( "../final-emissions/current-Versions" ),
                                              pattern = paste0('CEDS_',EM,'_emissions_by_country_sector') ) ),
                                              meta = F )

current_Em_by_Country_Fuel  <-  readData( "FIN_OUT", paste0( "current-Versions/",
                                              list.files( paste0( "../final-emissions/current-Versions" ),
                                              pattern = paste0('CEDS_',EM,'_emissions_by_country_fuel') ) ),
                                       meta = F )

#Process Data for figures
dfplot_global_sectors <- current_Em_by_Country_Sector %>%  mutate(Version = 'Current') %>%
    rbind( last_Em_by_Country_Sector %>%  mutate(Version = 'Last')  ) %>%
    left_join(MSL %>% select(aggregate_sectors,Figure_sector), by = c('sector' = "aggregate_sectors")) %>%
    mutate(Sector = Figure_sector) %>%
    filter(Sector != "Shipping") %>%
    group_by(Sector, Version) %>%
    summarize_if(is.numeric, sum) %>%
    gather(year, value, -Sector, -Version) %>%
    mutate(year = as.numeric(str_replace(year, 'X','')))

dfplot_global_fuel <- current_Em_by_Country_Fuel %>% mutate(Version = 'Current') %>%
    rbind( last_Em_by_Country_Fuel %>%  mutate(Version = 'Last')  ) %>%
    mutate(Fuel = fuel) %>%
    group_by(Fuel, Version) %>%
    summarize_if(is.numeric, sum) %>%
    gather(year, value, -Fuel, -Version) %>%
    mutate(year = as.numeric(str_replace(year, 'X','')))

dfplot_global_total <- current_Em_by_Country_Fuel %>% mutate(Version = 'Current') %>%
    rbind( last_Em_by_Country_Fuel %>%  mutate(Version = 'Last')  ) %>%
    group_by(Version) %>%
    summarize_if(is.numeric, sum) %>%
    gather(year, value, -Version) %>%
    mutate(year = as.numeric(str_replace(year, 'X','')))

dfplot_Regions_sectors <- current_Em_by_Country_Sector %>%  mutate(Version = 'Current') %>%
    rbind( last_Em_by_Country_Sector %>%  mutate(Version = 'Last')  ) %>%
    left_join(MCL %>% select(iso, Paper_Figure_Region)) %>%
    left_join(MSL %>% select(aggregate_sectors,Figure_sector), by = c('sector' = "aggregate_sectors")) %>%
    mutate(Sector = Figure_sector) %>%
    filter(Sector != "Shipping") %>%
    mutate(Region = Paper_Figure_Region) %>%
    group_by(Region, Sector, Version) %>%
    summarize_if(is.numeric, sum) %>%
    gather(year, value, -Sector, -Region, -Version) %>%
    mutate(year = as.numeric(str_replace(year, 'X','')))

dfplot_Africa_sectors <- dfplot_Regions_sectors %>%
    filter(Region == 'Africa')

dfplot_China_sectors <- dfplot_Regions_sectors %>%
    filter(Region == 'China')

dfplot_North_America_sectors <- dfplot_Regions_sectors %>%
    filter(Region == 'North America')

dfplot_Europe_sectors <- dfplot_Regions_sectors %>%
    filter(Region == 'Europe')

dfplot_FSU_sectors <- dfplot_Regions_sectors %>%
    filter(Region == 'Former Soviet Union')

dfplot_Latin_America_sectors <- dfplot_Regions_sectors %>%
    filter(Region == 'Latin America')

dfplot_Other_sectors <- dfplot_Regions_sectors %>%
    filter(Region == 'Other Asia/Pacific/ME')

# Plots
plot_global_sectors <- ggplot(dfplot_global_sectors, aes(x = year, y = value, color = Sector, linetype= Version))+
    geom_line( data = dplyr::filter( dfplot_global_sectors, Version == 'Last' ),
               size = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
    geom_line( data = dplyr::filter( dfplot_global_sectors, Version == 'Current'  ),
               size = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
    version_comparison_formatting+
    theme_diagnostic

plot_global_fuel <- ggplot(dfplot_global_fuel, aes(x = year, y = value, color = Fuel, linetype= Version))+
    geom_line( data = dplyr::filter( dfplot_global_fuel, Version == 'Last' ),
               size = 2 ,aes( x = year, y= value, color = Fuel ), alpha = .4 ) +
    geom_line( data = dplyr::filter( dfplot_global_fuel, Version == 'Current'  ),
               size = 0.5, aes( x = year, y = value, color = Fuel ), alpha = 1 ) +
    version_comparison_formatting+
    theme_diagnostic

plot_global_total <- ggplot(dfplot_global_total, aes(x = year, y = value,  linetype= Version))+
    geom_line( data = dplyr::filter( dfplot_global_total, Version == 'Last' ),
               size = 2 ,aes( x = year, y= value), alpha = .4 ) +
    geom_line( data = dplyr::filter( dfplot_global_total, Version == 'Current'  ),
               size = 0.5, aes( x = year, y = value ), alpha = 1 ) +
    version_comparison_formatting+
    theme_diagnostic

# Plot Regions
plot_region <- function(df_region){

    ggplot(df_region, aes(x = year, y = value, color = Sector, linetype= Version))+
        geom_line( data = dplyr::filter( df_region, Version == 'Last' ),
                   size = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
        geom_line( data = dplyr::filter( df_region, Version == 'Current'  ),
                   size = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
        version_comparison_formatting+
        theme_diagnostic
}

region_plots <- lapply(list(dfplot_Africa_sectors, dfplot_China_sectors, dfplot_North_America_sectors, dfplot_Europe_sectors, dfplot_FSU_sectors, dfplot_Latin_America_sectors, dfplot_Other_sectors),
       plot_region)

#output

summary_plot_output <- c(list(plot_global_sectors, plot_global_fuel, plot_global_total), region_plots)
names(summary_plot_output) <- c("Global Sectors",'Global Fuel','Globl Total',
                                "Africa", "China", "North America", "Europe", "FSU", "Latin America", "Other Asia/Pacific")

# If individual country(ies) selected:
#Plot one graph with all country totals
if(!is.na(country_select)){

    dfplot_all_countries <- current_Em_by_Country_Sector %>%  mutate(Version = 'Current') %>%
        rbind( last_Em_by_Country_Sector %>%  mutate(Version = 'Last')  ) %>%
        filter(sector != "1A3_International-shipping",
               iso %in% country_select) %>%
        mutate(Country = iso) %>%
        group_by(Country, Version) %>%
        summarize_if(is.numeric, sum) %>%
        gather(year, value, -Country, -Version) %>%
        mutate(year = as.numeric(str_replace(year, 'X','')))

    plot_all_countries <- ggplot(dfplot_all_countries, aes(x = year, y = value, color = Country, linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_all_countries, Version == 'Last' ),
                   size = 2 ,aes( x = year, y= value, color = Country ), alpha = .4 ) +
        geom_line( data = dplyr::filter( dfplot_all_countries, Version == 'Current'  ),
                   size = 0.5, aes( x = year, y = value, color = Country ), alpha = 1 ) +
        version_comparison_formatting+
        theme_diagnostic

# Redefine lists of plots for output
last_index <- length(summary_plot_output)
summary_plot_output[[(last_index+1)]] <- plot_all_countries
names(summary_plot_output)[(last_index+1)] <- "Selected Countries"

#Plot individual country graphs by sector
dfplot_countries_sectors <- current_Em_by_Country_Sector %>%  mutate(Version = 'Current') %>%
    rbind( last_Em_by_Country_Sector %>%  mutate(Version = 'Last')  ) %>%
    left_join(MSL %>% select(aggregate_sectors,Figure_sector), by = c('sector' = "aggregate_sectors")) %>%
    mutate(Sector = Figure_sector) %>%
    filter(Sector != "Shipping") %>%
    group_by(iso, Sector, Version) %>%
    summarize_if(is.numeric, sum) %>%
    gather(year, value, -Sector, -iso, -Version) %>%
    mutate(year = as.numeric(str_replace(year, 'X','')))

for (i in seq_along(country_select)){

dfplot_country <- dfplot_countries_sectors %>%
    filter(iso == country_select[i])

plot_country <- ggplot(dfplot_country, aes(x = year, y = value, color = Sector, linetype= Version))+
    geom_line( data = dplyr::filter( dfplot_country, Version == 'Last' ),
               size = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
    geom_line( data = dplyr::filter( dfplot_country, Version == 'Current'  ),
               size = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
    version_comparison_formatting+
    theme_diagnostic

last_index <- length(summary_plot_output)
summary_plot_output[[(last_index+1)]] <- plot_country
names(summary_plot_output)[(last_index+1)] <- country_select[i]

}}

# Save as R object, to print to pdf later
save(summary_plot_output, file = paste0('../final-emissions/diagnostics/Comparison_Plots_',EM,'.RData'))

return(summary_plot_output)

 }

# ----------------------------------------------------------------------------------
# print_single_em_comparison_plots
# Brief:         Print the comparison plots for the current emission species to
#       to diagnostic output
# Details:
# Dependencies:
# Author(s):
# Params:   country_select: vector or isos to be plotted individually
# Return:
# Input Files: comparison_plot: list of ggplots created with summary_comparison_plot()
# Output Files:  list of summary plots (ggplot objects)

print_single_em_comparison_plots <- function(plot_list = comparison_plots,
                                             CS = country_select,
                                             EM = em){


    pdf( paste0( "../final-emissions/diagnostics/Comparison_Plots_",EM,'.pdf' ),
         width = 14, height = 10, paper ='special' )

    grid.arrange( plot_list[["Global Sectors"]]+ggtitle('Global Sectors'),
                  plot_list[["Global Fuel"]]+ggtitle('Global Fuel'),
                  plot_list[["Globl Total"]]+ggtitle('Globl Total'),
                  ncol = 3, nrow = 3,padding = unit(3, "line"),
                  top = paste0( "Global CEDS comparison - current to previous version" ) )

    grid.arrange( plot_list[["Africa"]]+ggtitle('Africa'),
                  plot_list[["China"]]+ggtitle('China'),
                  plot_list[["North America"]]+ggtitle('North America'),
                  plot_list[["Europe"]]+ggtitle('Europe'),
                  plot_list[["FSU"]]+ggtitle('FSU'),
                  plot_list[["Latin America"]]+ggtitle('Latin America'),
                  plot_list[["Other Asia/Pacific"]]+ggtitle('Other Asia/Pacific'),
                  ncol = 3, nrow = 3,
                  top = paste0( "Regional CEDS comparison - current to previous version" ) )

    if(!is.na(CS)){
        index_last <- length(plot_list)
        countries_plot_list <- plot_list[11:index_last]
        list_last <- length(countries_plot_list)
        for(i in 2:list_last){
            countries_plot_list[[i]] <- countries_plot_list[[i]]+ ggtitle(names(countries_plot_list)[i])
        }
        do.call("grid.arrange", c(countries_plot_list, ncol=3,nrow=3,
                                  top = paste0( "Regional CEDS comparison - current to previous version" )))

}
    dev.off( )
}

# ----------------------------------------------------------------------------------
# g_legend
# Brief:
# Details:
# Dependencies:
# Author(s):
# Params:
# Return:
# Input Files:
# Output Files:

g_legend<-function( a.gplot ){

    tmp <- ggplot_gtable( ggplot_build( a.gplot ) )
    leg <- which( sapply( tmp$grobs, function( x ) x$name ) == "guide-box" )
    legend <- tmp$grobs[[leg]]
    return( legend )

}

