# ----------------------------------------------------------------------------------
# CEDS R header file: summary visualization functions
# Authors: Rachel Hoesly
# Last Updated: January 24, 2023

# This file should be sourced by any R script running diagnostics on CEDS data.
# Functions contained:
#   summary_comparison_plots
#   print_single_em_comparison_plots
#   print_summary_graphs
#
# ----------------------------------------------------------------------------------
# summary_comparison_plots
# Brief: Produces plots to compare the current Version with the last Version
# Details: Produces plots by aggregate sector, global, fuels etc. Can also pull
#       out individual country comparison plots
# Dependencies: none
# Author(s): Rachel
# Params: country_select: vector or isos to be plotted individually
# Return:
# Input Files:
# Output Files:  list of summary plots (ggplot objects)

# debug # # later add country select option
# country_select <- c('usa')

summary_comparison_plots <- function(
                          MCL = Master_Country_List,
                          MSL = Master_Sector_Level_map,
                          CS = country_select,
                          EM = em,
                          global_color = 'darkorchid'){

    # MCL = Master_Country_List
    # MSL = Master_Sector_Level_map
    # CS = country_select
    # EM = em
    # global_color = 'darkorchid'

printLog("Create Comparison plots for current vs previous data")
# Plot options
graph_start <- min(extended_years)
graph_end <- max(extended_years)
year_breaks <- 10

# Plot theme
# Gray background for diagnostic figures
theme_diagnostic <- list(theme(panel.background = element_rect(fill = "gray90",colour = "gray90",size = 0.5, linetype = "solid")),
                         theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "gray95")),
                         theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white")))
# Formatting for the 3 different time scales
version_comparison_formatting <- list(
    scale_x_continuous( breaks = seq( from = graph_start, to = graph_end, by = year_breaks )) ,
    #scale_y_continuous( labels = 'comma' ),
    guides( linetype = guide_legend( override.aes = list( size = c( 1.5, 0.5 ) ) ) ),
    labs( x = "" , y = 'Emissions [Tg/yr]' ),
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

current_Em_by_Country_Sector  <-  readData( "FIN_OUT", paste0("current-versions/",
                                              list.files( paste0( "../final-emissions/current-versions" ),
                                              pattern = paste0('CEDS_',EM,'_emissions_by_country_sector') ) ),
                                              meta = F )

current_Em_by_Country_Fuel  <-  readData( "FIN_OUT", paste0( "current-versions/",
                                              list.files( paste0( "../final-emissions/current-versions" ),
                                              pattern = paste0('CEDS_',EM,'_emissions_by_country_fuel') ) ),
                                       meta = F )

#Process Data for figures
# Convert from Gg to Tg: 1000Gg = 1 Tg
dfplot_global_sectors <- current_Em_by_Country_Sector %>%  mutate(Version = 'Current') %>%
    bind_rows( last_Em_by_Country_Sector %>%  mutate(Version = 'Last')  ) %>%
    left_join(MSL %>% select(aggregate_sectors,Figure_sector) %>% unique, by = c('sector' = "aggregate_sectors")) %>%
    mutate(Sector = Figure_sector) %>%
    group_by(Sector, Version) %>%
    summarize_if(is.numeric, sum) %>%
    gather(year, value, -Sector, -Version) %>%
    mutate(year = as.numeric(str_replace(year, 'X',''))) %>%
    mutate(value = value/1000) %>% #Convert from Gg to Tg
    filter(!is.na(value))

dfplot_global_fuel <- current_Em_by_Country_Fuel %>% mutate(Version = 'Current') %>%
    bind_rows( last_Em_by_Country_Fuel %>%  mutate(Version = 'Last')  ) %>%
    mutate(Fuel = fuel) %>%
    group_by(Fuel, Version) %>%
    summarize_if(is.numeric, sum) %>%
    gather(year, value, -Fuel, -Version) %>%
    mutate(year = as.numeric(str_replace(year, 'X',''))) %>%
    mutate(value = value/1000) %>% #Convert from Gg to Tg
    filter(!is.na(value))


dfplot_global_total <- current_Em_by_Country_Sector %>% mutate(Version = 'Current') %>%
    bind_rows( last_Em_by_Country_Sector %>%  mutate(Version = 'Last')  ) %>%
    group_by(Version) %>%
    summarize_if(is.numeric, sum) %>%
    gather(year, value, -Version) %>%
    mutate(year = as.numeric(str_replace(year, 'X',''))) %>%
    mutate(value = value/1000) %>% #Convert from Gg to Tg
    filter(!is.na(value))

dfplot_Regions_sectors <- current_Em_by_Country_Sector %>%  mutate(Version = 'Current') %>%
    bind_rows( last_Em_by_Country_Sector %>%  mutate(Version = 'Last')  ) %>%
    left_join(MCL %>% select(iso, Paper_Figure_Region)%>% unique) %>%
    left_join(MSL %>% select(aggregate_sectors,Figure_sector)%>% unique, by = c('sector' = "aggregate_sectors")) %>%
    mutate(Sector = Figure_sector) %>%
    # filter(Sector != "Shipping") %>%
    mutate(Region = Paper_Figure_Region) %>%
    group_by(Region, Sector, Version) %>%
    summarize_if(is.numeric, sum) %>%
    gather(year, value, -Sector, -Region, -Version) %>%
    mutate(year = as.numeric(str_replace(year, 'X',''))) %>%
    mutate(value = value/1000) %>% #Convert from Gg to Tg
    filter(!is.na(value))

dfplot_global_region <- dfplot_Regions_sectors %>%
    group_by(Region, Version, year) %>%
    summarize_if(is.numeric, sum)

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

# Plots -------------
# plot global sector
plot_global_sectors_modern <- ggplot(dfplot_global_sectors %>%
                                         filter(year>1959),
                                     aes(x = year, y = value, color = Sector, linetype= Version))+
    geom_line( data = dplyr::filter( dfplot_global_sectors, Version == 'Last' ),
               size = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
    geom_line( data = dplyr::filter( dfplot_global_sectors, Version == 'Current'  ),
               size = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
    theme_diagnostic+
    version_comparison_formatting
plot_global_sectors_mid <- ggplot(dfplot_global_sectors %>%
                                      filter(year < 1971,
                                             year > 1899),
                                  aes(x = year, y = value, color = Sector, linetype= Version))+
    geom_line( data = dplyr::filter( dfplot_global_sectors, Version == 'Last' ),
               size = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
    geom_line( data = dplyr::filter( dfplot_global_sectors, Version == 'Current'  ),
               size = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
    theme_diagnostic+
    version_comparison_formatting
plot_global_sectors_early <- ggplot(dfplot_global_sectors %>%
                                        filter(year < 1911),
                                    aes(x = year, y = value, color = Sector, linetype= Version))+
    geom_line( data = dplyr::filter( dfplot_global_sectors, Version == 'Last' ),
               size = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
    geom_line( data = dplyr::filter( dfplot_global_sectors, Version == 'Current'  ),
               size = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
    theme_diagnostic+
    version_comparison_formatting
# plot global fuel
plot_global_fuel_modern <- ggplot(dfplot_global_fuel %>%
                                      filter(year>1959),
                                  aes(x = year, y = value, color = Fuel, linetype= Version))+
    geom_line( data = dplyr::filter( dfplot_global_fuel, Version == 'Last' ),
               size = 2 ,aes( x = year, y= value, color = Fuel ), alpha = .4 ) +
    geom_line( data = dplyr::filter( dfplot_global_fuel, Version == 'Current'  ),
               size = 0.5, aes( x = year, y = value, color = Fuel ), alpha = 1 ) +
    theme_diagnostic+
    version_comparison_formatting
plot_global_fuel_mid <- ggplot(dfplot_global_fuel %>%
                                   filter(year < 1971,
                                          year > 1899),
                               aes(x = year, y = value, color = Fuel, linetype= Version))+
    geom_line( data = dplyr::filter( dfplot_global_fuel, Version == 'Last' ),
               size = 2 ,aes( x = year, y= value, color = Fuel ), alpha = .4 ) +
    geom_line( data = dplyr::filter( dfplot_global_fuel, Version == 'Current'  ),
               size = 0.5, aes( x = year, y = value, color = Fuel ), alpha = 1 ) +
    theme_diagnostic+
    version_comparison_formatting
plot_global_fuel <- ggplot(dfplot_global_fuel, aes(x = year, y = value, color = Fuel, linetype= Version))+
    geom_line( data = dplyr::filter( dfplot_global_fuel, Version == 'Last' ),
               size = 2 ,aes( x = year, y= value, color = Fuel ), alpha = .4 ) +
    geom_line( data = dplyr::filter( dfplot_global_fuel, Version == 'Current'  ),
               size = 0.5, aes( x = year, y = value, color = Fuel ), alpha = 1 ) +
    theme_diagnostic+
    version_comparison_formatting
# plot global region
plot_global_region_modern <- ggplot(dfplot_global_region %>%
                                        filter(year>1959),
                                    aes(x = year, y = value, color = Region, linetype= Version))+
    geom_line( data = dplyr::filter( dfplot_global_region, Version == 'Last' ),
               size = 2 ,aes( x = year, y= value, color = Region ), alpha = .4 ) +
    geom_line( data = dplyr::filter( dfplot_global_region, Version == 'Current'  ),
               size = 0.5, aes( x = year, y = value, color = Region ), alpha = 1 ) +
    theme_diagnostic+
    version_comparison_formatting
plot_global_region_mid <- ggplot(dfplot_global_region %>%
                                     filter(year < 1971,
                                            year > 1899),
                                 aes(x = year, y = value, color = Region, linetype= Version))+
    geom_line( data = dplyr::filter( dfplot_global_region, Version == 'Last' ),
               size = 2 ,aes( x = year, y= value, color = Region ), alpha = .4 ) +
    geom_line( data = dplyr::filter( dfplot_global_region, Version == 'Current'  ),
               size = 0.5, aes( x = year, y = value, color = Region ), alpha = 1 ) +
    theme_diagnostic+
    version_comparison_formatting
plot_global_region <- ggplot(dfplot_global_region, aes(x = year, y = value, color = Region, linetype= Version))+
    geom_line( data = dplyr::filter( dfplot_global_region, Version == 'Last' ),
               size = 2 ,aes( x = year, y= value, color = Region ), alpha = .4 ) +
    geom_line( data = dplyr::filter( dfplot_global_region, Version == 'Current'  ),
               size = 0.5, aes( x = year, y = value, color = Region ), alpha = 1 ) +
    theme_diagnostic+
    version_comparison_formatting
# plot global total
plot_global_total_modern <- ggplot(dfplot_global_total %>%
                                       filter(year>1959),
                                   aes(x = year, y = value,  linetype= Version))+
    geom_line( data = dplyr::filter( dfplot_global_total, Version == 'Last' ),
               size = 2 ,aes( x = year, y= value), alpha = .4 , color = global_color) +
    geom_line( data = dplyr::filter( dfplot_global_total, Version == 'Current'  ),
               size = 0.5, aes( x = year, y = value ), alpha = 1 , color = global_color) +
    theme_diagnostic+
    version_comparison_formatting
plot_global_total_mid <- ggplot(dfplot_global_total %>%
                                    filter(year < 1971,
                                           year > 1899),
                                aes(x = year, y = value,  linetype= Version))+
    geom_line( data = dplyr::filter( dfplot_global_total, Version == 'Last' ),
               size = 2 ,aes( x = year, y= value), alpha = .4 , color = global_color) +
    geom_line( data = dplyr::filter( dfplot_global_total, Version == 'Current'  ),
               size = 0.5, aes( x = year, y = value ), alpha = 1 , color = global_color) +
    theme_diagnostic+
    version_comparison_formatting
plot_global_total <- ggplot(dfplot_global_total, aes(x = year, y = value,  linetype= Version))+
    geom_line( data = dplyr::filter( dfplot_global_total, Version == 'Last' ),
               size = 2 ,aes( x = year, y= value), alpha = .4 , color = global_color) +
    geom_line( data = dplyr::filter( dfplot_global_total, Version == 'Current'  ),
               size = 0.5, aes( x = year, y = value ), alpha = 1 , color = global_color) +
    theme_diagnostic+
    version_comparison_formatting

# Plot Regions ---------------------
plot_region <- function(df_region){

   modern <-  ggplot(df_region %>%
                         filter(year>1959),
                     aes(x = year, y = value, color = Sector, linetype= Version))+
        geom_line( data = dplyr::filter( df_region, Version == 'Last' ),
                   size = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
        geom_line( data = dplyr::filter( df_region, Version == 'Current'  ),
                   size = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
        theme_diagnostic+
       version_comparison_formatting
   mid <-  ggplot(df_region %>%
                      filter(year < 1971,
                             year > 1899),
                  aes(x = year, y = value, color = Sector, linetype= Version))+
        geom_line( data = dplyr::filter( df_region, Version == 'Last' ),
                   size = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
        geom_line( data = dplyr::filter( df_region, Version == 'Current'  ),
                   size = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
        theme_diagnostic+
       version_comparison_formatting
  early <- ggplot(df_region %>%
                      filter(year < 1911),
                  aes(x = year, y = value, color = Sector, linetype= Version))+
        geom_line( data = dplyr::filter( df_region, Version == 'Last' ),
                   size = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
        geom_line( data = dplyr::filter( df_region, Version == 'Current'  ),
                   size = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
        theme_diagnostic+
      version_comparison_formatting

  out <- list(modern, mid, early)
  return(out)
}

region_plots <- lapply(list(dfplot_Africa_sectors, dfplot_China_sectors, dfplot_North_America_sectors, dfplot_Europe_sectors, dfplot_FSU_sectors, dfplot_Latin_America_sectors, dfplot_Other_sectors),
       plot_region)

#output

summary_plot_output <- c(list(plot_global_sectorsmodern,
                                plot_global_sectors_mid,
                                plot_global_sectors_early,
                              plot_global_fuel_modern,
                                plot_global_fuel_mid,
                                plot_global_fuel_early,
                              plot_global_region_modern,
                                plot_global_region_mid,
                                plot_global_region_early,
                              plot_global_total_modern,
                                plot_global_total_mid,
                                plot_global_total_early),
                         unlist(region_plots, recursive = FALSE))

names(summary_plot_output) <- c('Global Sectors - Modern','Global Sectors - Mid', 'Global Sectors - Early',
                                'Global Fuel - Modern', 'Global Fuel - Mid','Global Fuel - Early',
                                'Global Region - Modern', 'Global Region - Mid', 'Global Region - Early',
                                'Global Total - Modern','Global Total - Mid','Global Total - Early',
                                "Africa - Modern", "Africa - Mid","Africa - Early",
                                "China - Modern", "China - Mid", "China - Early",
                                "North America - Modern", "North America - Mid", "North America - Early",
                                "Europe - Modern", "Europe - Mid", "Europe - Early",
                                "FSU - Modern", "FSU - Mid","FSU - Early",
                                "Latin America - Modern", "Latin America - Mid","Latin America - Early",
                                "Other Asia/Pacific - Modern","Other Asia/Pacific - Mid","Other Asia/Pacific - Early")

# If individual country(ies) selected:
#Plot one graph with all country totals
if(!is.na(country_select)){

    dfplot_all_countries <- current_Em_by_Country_Sector %>%  mutate(Version = 'Current') %>%
        bind_rows( last_Em_by_Country_Sector %>%  mutate(Version = 'Last')  ) %>%
        filter(sector != "1A3_International-shipping",
               iso %in% country_select) %>%
        mutate(Country = iso) %>%
        group_by(Country, Version) %>%
        summarize_if(is.numeric, sum) %>%
        gather(year, value, -Country, -Version) %>%
        mutate(year = as.numeric(str_replace(year, 'X','')))%>%
        mutate(value = value/1000) #Convert from Gg to Tg

    plot_all_countries_modern <- ggplot(dfplot_all_countries %>%
                                            filter(year>1959),
                                        aes(x = year, y = value, color = Country, linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_all_countries, Version == 'Last' ),
                   size = 2 ,aes( x = year, y= value, color = Country ), alpha = .4 ) +
        geom_line( data = dplyr::filter( dfplot_all_countries, Version == 'Current'  ),
                   size = 0.5, aes( x = year, y = value, color = Country ), alpha = 1 ) +
        version_comparison_formatting+
        theme_diagnostic
    plot_all_countries_mid <- ggplot(dfplot_all_countries %>%
                                         filter(year < 1971,
                                                year > 1899),
                                     aes(x = year, y = value, color = Country, linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_all_countries, Version == 'Last' ),
                   size = 2 ,aes( x = year, y= value, color = Country ), alpha = .4 ) +
        geom_line( data = dplyr::filter( dfplot_all_countries, Version == 'Current'  ),
                   size = 0.5, aes( x = year, y = value, color = Country ), alpha = 1 ) +
        version_comparison_formatting+
        theme_diagnostic
    plot_all_countries_early <- ggplot(dfplot_all_countries %>%
                                           filter(year < 1911),
                                       aes(x = year, y = value, color = Country, linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_all_countries, Version == 'Last' ),
                   size = 2 ,aes( x = year, y= value, color = Country ), alpha = .4 ) +
        geom_line( data = dplyr::filter( dfplot_all_countries, Version == 'Current'  ),
                   size = 0.5, aes( x = year, y = value, color = Country ), alpha = 1 ) +
        version_comparison_formatting+
        theme_diagnostic

# Redefine lists of plots for output
last_index <- length(summary_plot_output)
summary_plot_output[[(last_index+1)]] <- plot_all_countries_modern
summary_plot_output[[(last_index+2)]] <- plot_all_countries_mid
summary_plot_output[[(last_index+3)]] <- plot_all_countries_early
names(summary_plot_output)[(last_index+1)] <- "Selected Countries - Modern"
names(summary_plot_output)[(last_index+2)] <- "Selected Countries - Mid"
names(summary_plot_output)[(last_index+3)] <- "Selected Countries - Early"

#Plot individual country graphs by sector
dfplot_countries_sectors <- current_Em_by_Country_Sector %>%  mutate(Version = 'Current') %>%
    bind_rows( last_Em_by_Country_Sector %>%  mutate(Version = 'Last')  ) %>%
    left_join(MSL %>% select(aggregate_sectors,Figure_sector) %>% unique, by = c('sector' = "aggregate_sectors")) %>%
    mutate(Sector = Figure_sector) %>%
    filter(Sector != "Shipping") %>%
    group_by(iso, Sector, Version) %>%
    summarize_if(is.numeric, sum) %>%
    gather(year, value, -Sector, -iso, -Version) %>%
    mutate(year = as.numeric(str_replace(year, 'X',''))) %>%
    mutate(value = value/1000) #Convert from Gg to Tg

for (i in seq_along(country_select)){

dfplot_country <- dfplot_countries_sectors %>%
    filter(iso == country_select[i])

plot_country_modern <- ggplot(dfplot_country %>%
                                  filter(year > 1959),
                              aes(x = year, y = value, color = Sector, linetype= Version))+
    geom_line( data = dplyr::filter( dfplot_country, Version == 'Last' ),
               size = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
    geom_line( data = dplyr::filter( dfplot_country, Version == 'Current'  ),
               size = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
    version_comparison_formatting+
    theme_diagnostic
plot_country_mid <- ggplot(dfplot_country %>%
                               filter(year < 1971,
                                      year > 1899),
                           aes(x = year, y = value, color = Sector, linetype= Version))+
    geom_line( data = dplyr::filter( dfplot_country, Version == 'Last' ),
               size = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
    geom_line( data = dplyr::filter( dfplot_country, Version == 'Current'  ),
               size = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
    version_comparison_formatting+
    theme_diagnostic
plot_country_early <- ggplot(dfplot_country %>%
                                 fikter(year <1911),
                             aes(x = year, y = value, color = Sector, linetype= Version))+
    geom_line( data = dplyr::filter( dfplot_country, Version == 'Last' ),
               size = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
    geom_line( data = dplyr::filter( dfplot_country, Version == 'Current'  ),
               size = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
    version_comparison_formatting+
    theme_diagnostic

last_index <- length(summary_plot_output)
summary_plot_output[[(last_index+1)]] <- plot_country_modern
summary_plot_output[[(last_index+2)]] <- plot_country_mid
summary_plot_output[[(last_index+3)]] <- plot_country_early
names(summary_plot_output)[(last_index+1)] <- paste0( country_select[i], ' - Modern')
names(summary_plot_output)[(last_index+2)] <- paste0( country_select[i], ' - Mid')
names(summary_plot_output)[(last_index+3)] <- paste0( country_select[i], ' - Early')

}}

# Save as R object, to print to pdf later
save(summary_plot_output, file = paste0('../final-emissions/diagnostics/Comparison_Plots_',EM,'.RData'))

return(summary_plot_output)

 }

# ----------------------------------------------------------------------------------
# print_single_em_comparison_plots
# Brief: Print the comparison plots for the current emission species to
#       to diagnostic output
# Details: Only prints the figures plotted in summary_comparison_plots
# Dependencies: uses output from summary_comparison_plots
# Author(s): Rachel Hoesly
# Params:   country_select: vector or isos to be plotted individually
#           comparison_plots: output from summary_comparison_plots
# Return: none, saves plots in pdf
# Input Files: comparison_plot: list of ggplots created with summary_comparison_plot()
# Output Files: pdf of plotted summary plots (not returned just saved)

print_single_em_comparison_plots <- function(plot_list = comparison_plots,
                                             CS = country_select,
                                             EM = em){


    pdf( paste0( "../final-emissions/diagnostics/Comparison_Plots_",EM,'.pdf' ),
         width = 14, height = 10, paper ='special' )

    grid.arrange( plot_list[["Global Sectors - Modern"]]+ggtitle('Global Sectors'),
                  plot_list[["Global Fuel - Modern"]]+ggtitle('Global Fuel'),
                  plot_list[["Global Region - Modern"]]+ggtitle('Global Region'),
                  plot_list[["Global Total - Modern"]]+ggtitle('Global Total'),
                  ncol = 3, nrow = 3,padding = unit(3, "line"),
                  top = paste0( "Global CEDS comparison - current to previous version" ) )

    grid.arrange( plot_list[["Global Sectors - Mid"]]+ggtitle('Global Sectors'),
                  plot_list[["Global Fuel - Mid"]]+ggtitle('Global Fuel'),
                  plot_list[["Global Region - Mid"]]+ggtitle('Global Region'),
                  plot_list[["Global Total - Mid"]]+ggtitle('Global Total'),
                  ncol = 3, nrow = 3,padding = unit(3, "line"),
                  top = paste0( "Global CEDS comparison - current to previous version" ) )

    grid.arrange( plot_list[["Global Sectors - Early"]]+ggtitle('Global Sectors'),
                  plot_list[["Global Fuel - Early"]]+ggtitle('Global Fuel'),
                  plot_list[["Global Region - Early"]]+ggtitle('Global Region'),
                  plot_list[["Global Total - Early"]]+ggtitle('Global Total'),
                  ncol = 3, nrow = 3,padding = unit(3, "line"),
                  top = paste0( "Global CEDS comparison - current to previous version" ) )

    grid.arrange( plot_list[["Africa - Modern"]]+ggtitle('Africa'),
                  plot_list[["China - Modern"]]+ggtitle('China'),
                  plot_list[["North America - Modern"]]+ggtitle('North America'),
                  plot_list[["Europe - Modern"]]+ggtitle('Europe'),
                  plot_list[["FSU - Modern"]]+ggtitle('FSU'),
                  plot_list[["Latin America - Modern"]]+ggtitle('Latin America'),
                  plot_list[["Other Asia/Pacific - Modern"]]+ggtitle('Other Asia/Pacific'),
                  ncol = 3, nrow = 3,
                  top = paste0( "Regional CEDS comparison - current to previous version" ) )

    grid.arrange( plot_list[["Africa - Mid"]]+ggtitle('Africa'),
                  plot_list[["China - Mid"]]+ggtitle('China'),
                  plot_list[["North America - Mid"]]+ggtitle('North America'),
                  plot_list[["Europe - Mid"]]+ggtitle('Europe'),
                  plot_list[["FSU - Mid"]]+ggtitle('FSU'),
                  plot_list[["Latin America - Mid"]]+ggtitle('Latin America'),
                  plot_list[["Other Asia/Pacific - Mid"]]+ggtitle('Other Asia/Pacific'),
                  ncol = 3, nrow = 3,
                  top = paste0( "Regional CEDS comparison - current to previous version" ) )

    grid.arrange( plot_list[["Africa - Early"]]+ggtitle('Africa'),
                  plot_list[["China - Early"]]+ggtitle('China'),
                  plot_list[["North America - Early"]]+ggtitle('North America'),
                  plot_list[["Europe - Early"]]+ggtitle('Europe'),
                  plot_list[["FSU - Early"]]+ggtitle('FSU'),
                  plot_list[["Latin America - Early"]]+ggtitle('Latin America'),
                  plot_list[["Other Asia/Pacific - Early"]]+ggtitle('Other Asia/Pacific'),
                  ncol = 3, nrow = 3,
                  top = paste0( "Regional CEDS comparison - current to previous version" ) )

    if(!is.na(CS)){
        if(length(CS)>6)stop("individual countries selected is more than 6 - summary visualization can't support more than 6")
        index_last <- length(plot_list)
        countries_plot_list <- plot_list[37:index_last]
        list_last <- length(countries_plot_list)
        for(i in 1:list_last){
            countries_plot_list[[i]] <- countries_plot_list[[i]]+ ggtitle(names(countries_plot_list)[i])
        }
    if( length(countries_plot_list) > 9){
        do.call("grid.arrange", c(countries_plot_list[1:9], ncol=3,nrow=3,
                                  top = paste0( "Regional CEDS comparison - current to previous version" )))
        do.call("grid.arrange", c(countries_plot_list[10: length(countries_plot_list) ], ncol=3,nrow=3,
                                  top = paste0( "Regional CEDS comparison - current to previous version" )))

    }else(
        do.call("grid.arrange", c(countries_plot_list, ncol=3,nrow=3,
                                  top = paste0( "Regional CEDS comparison - current to previous version" )))
    )
}
    dev.off( )
}

# ----------------------------------------------------------------------------------
# g_legend
# Brief: extracts the legend from a ggplot plot
# Dependencies: none
# Author(s): the internet
# Params: a.gplot: a ggplot plot
# Return: the ggplot legend object

g_legend<-function( a.gplot ){

    tmp <- ggplot_gtable( ggplot_build( a.gplot ) )
    leg <- which( sapply( tmp$grobs, function( x ) x$name ) == "guide-box" )
    legend <- tmp$grobs[[leg]]
    return( legend )

}

# ----------------------------------------------------------------------------------
# print_summary_graphs
# Brief: print the individual summary graphs as pdfs. Useful for presentations.
# Details: Only prints the figures plotted in summary_comparison_plots
# Dependencies: uses output from summary_comparison_plots
# Author(s): Rachel Hoesly
# Params:   country_select: vector or isos to be plotted individually
#           comparison_plots: output from summary_comparison_plots
# Return: none, saves plots in pdf
# Input Files: comparison_plot: list of ggplots created with summary_comparison_plot()
# Output Files: individual pdfs of plotted summary plots (not returned just saved)

print_summary_graphs <- function(plot_list = comparison_plots,
                                 print_regions = FALSE){
    ggsave(paste0("../final-emissions/diagnostics/",em,"_global_sector.pdf"),
           plot = plot_list[["Global Sectors"]], width =6 , height=4, units = "in")

    ggsave(paste0("../final-emissions/diagnostics/",em,"_global_fuel.pdf"),
           plot = plot_list[["Global Fuel"]], width =6 , height=4, units = "in")

    ggsave(paste0("../final-emissions/diagnostics/",em,"_global_region.pdf"),
           plot = plot_list[["Global Region"]], width =6 , height=4, units = "in")

    ggsave(paste0("../final-emissions/diagnostics/",em,"_global_total.pdf"),
           plot = plot_list[["Global Total"]], width =4 , height=2.25, units = "in")

    ggsave(paste0("../final-emissions/diagnostics/",em,"_global_total_no_legend.pdf"),
           plot = plot_list[["Global Total"]]+ theme(legend.position = "none"),
           width =3 , height=2.25, units = "in")

    if(print_regions == TRUE){

        ggsave(paste0("../final-emissions/diagnostics/",em,"_africa.pdf"),
               plot = plot_list[["Africa"]], width =4 , height=2.25, units = "in")
        ggsave(paste0("../final-emissions/diagnostics/",em,"_china.pdf"),
               plot = plot_list[["China"]], width =4 , height=2.25, units = "in")
        ggsave(paste0("../final-emissions/diagnostics/",em,"_northamerica.pdf"),
               plot = plot_list[["North America"]], width =4 , height=2.25, units = "in")
        ggsave(paste0("../final-emissions/diagnostics/",em,"_europe.pdf"),
               plot = plot_list[["Europe"]], width =4 , height=2.25, units = "in")
        ggsave(paste0("../final-emissions/diagnostics/",em,"_fsu.pdf"),
               plot = plot_list[["FSU"]], width =4 , height=2.25, units = "in")
        ggsave(paste0("../final-emissions/diagnostics/",em,"_latinamerica.pdf"),
               plot = plot_list[["Latin America"]], width =4 , height=2.25, units = "in")
        ggsave(paste0("../final-emissions/diagnostics/",em,"_otherasia.pdf"),
               plot = plot_list[["Other Asia/Pacific"]], width =4 , height=2.25, units = "in")

    }}
