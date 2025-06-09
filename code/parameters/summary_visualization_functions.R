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
        global_color = 'darkorchid',
        Last_name = "Last",
        Current_name = "Current"){

    # MCL = Master_Country_List
    # MSL = Master_Sector_Level_map
    # CS = country_select
    # EM = em
    # global_color = 'darkorchid'
    # Last_name = comparison_plot_last_name
    # Current_name = comparison_plot_current_name

    printLog("Create Comparison plots for current vs previous data in summary_comparison_plots()")
    # Plot options
    graph_start <- min(extended_years)
    graph_end <- max(extended_years)
    year_breaks <- 10
    year_breaks_early <- 50

    # Plot theme
    # Gray background for diagnostic figures
    theme_diagnostic <- list(theme(panel.background = element_rect(fill = "gray90",colour = "gray90",linewidth = 0.5, linetype = "solid")),
                             theme(panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',colour = "gray95")),
                             theme(panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',colour = "white")))

    theme_diagnostic <- list(theme(panel.background = element_rect(fill = "white",colour = "gray90",linewidth = 0.5, linetype = "solid")),
                          theme(panel.grid.major = element_line(linewidth = 0.4, linetype = 'solid',colour = "gray85")),
                          theme(panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',colour = "gray95")),
                          theme(panel.border = element_rect(colour = "gray10", fill=NA, size=.4)))


    # Formatting for the 3 different time scales
    version_comparison_formatting <- list(
        scale_x_continuous( breaks = seq( from = graph_start, to = graph_end, by = year_breaks )) ,
        #scale_y_continuous( labels = 'comma' ),
        guides( linetype = guide_legend( override.aes = list( size = c( 1.5, 0.5 ) ) ) ),
        labs( x = "" , y = 'Emissions [Tg/yr]' ),
        ggtitle(EM),
        scale_linetype_manual( name= 'Version',
                               breaks = c( Last_name , Current_name),
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
    dfplot_global_sectors <- current_Em_by_Country_Sector %>%  mutate(Version = Current_name) %>%
        bind_rows( last_Em_by_Country_Sector %>%  mutate(Version = Last_name)  ) %>%
        left_join(MSL %>% select(aggregate_sectors,Figure_sector) %>% unique, by = c('sector' = "aggregate_sectors")) %>%
        mutate(Sector = Figure_sector) %>%
        group_by(Sector, Version) %>%
        summarize_if(is.numeric, sum) %>%
        gather(year, value, -Sector, -Version) %>%
        mutate(year = as.numeric(str_replace(year, 'X',''))) %>%
        mutate(value = value/1000) %>% #Convert from Gg to Tg
        filter(!is.na(value))

    dfplot_global_fuel <- current_Em_by_Country_Fuel %>% mutate(Version = Current_name) %>%
        bind_rows( last_Em_by_Country_Fuel %>%  mutate(Version = Last_name)  ) %>%
        mutate(Fuel = fuel) %>%
        group_by(Fuel, Version) %>%
        summarize_if(is.numeric, sum) %>%
        gather(year, value, -Fuel, -Version) %>%
        mutate(year = as.numeric(str_replace(year, 'X',''))) %>%
        mutate(value = value/1000) %>% #Convert from Gg to Tg
        filter(!is.na(value))


    dfplot_global_total <- current_Em_by_Country_Sector %>% mutate(Version = Current_name) %>%
        bind_rows( last_Em_by_Country_Sector %>%  mutate(Version = Last_name)  ) %>%
        group_by(Version) %>%
        summarize_if(is.numeric, sum) %>%
        gather(year, value, -Version) %>%
        mutate(year = as.numeric(str_replace(year, 'X',''))) %>%
        mutate(value = value/1000) %>% #Convert from Gg to Tg
        filter(!is.na(value))

    dfplot_Regions_sectors <- current_Em_by_Country_Sector %>%  mutate(Version = Current_name) %>%
        bind_rows( last_Em_by_Country_Sector %>%  mutate(Version = Last_name)  ) %>%
        left_join(MCL %>% select(iso, Paper_Figure_Region) %>% unique, relationship = "many-to-many") %>%
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

    # Create list of Regions
    RegionList <- unique(dfplot_Regions_sectors$Region)
    NReg <- length(RegionList)

    dfplot_global_region <- dfplot_Regions_sectors %>%
        group_by(Region, Version, year) %>%
        summarize_if(is.numeric, sum)

    dfplot_Reg1_sectors <- dfplot_Regions_sectors %>%
        filter(Region == RegionList[1])

    dfplot_Reg2_sectors <- dfplot_Regions_sectors %>%
        filter(Region == RegionList[2])

    dfplot_Reg3_sectors <- dfplot_Regions_sectors %>%
        filter(Region == RegionList[3])

    dfplot_Reg4_sectors <- dfplot_Regions_sectors %>%
        filter(Region == RegionList[4])

    if (NReg > 4) dfplot_Reg5_sectors <- dfplot_Regions_sectors %>%
        filter(Region == RegionList[5])

    if (NReg > 5)  dfplot_Reg6_sectors <- dfplot_Regions_sectors %>%
        filter(Region == RegionList[6])

    if (NReg > 6) dfplot_Reg7_sectors <- dfplot_Regions_sectors %>%
        filter(Region == RegionList[7])

    if (NReg > 7) dfplot_Reg8_sectors <- dfplot_Regions_sectors %>%
        filter(Region == RegionList[8])

    if (NReg > 8) dfplot_Reg9_sectors <- dfplot_Regions_sectors %>%
        filter(Region == RegionList[9])

    if (NReg > 9) dfplot_Reg10_sectors <- dfplot_Regions_sectors %>%
        filter(Region == RegionList[10])

    if (NReg > 8) {
        printLog("Summary regions > 7, some regions not displayed")
    }
    # Individual Countries ---------------------
    # Country Totals
    dfplot_country_totals <- current_Em_by_Country_Sector %>% mutate(Version = Current_name) %>%
        bind_rows( last_Em_by_Country_Sector %>%  mutate(Version = Last_name)  ) %>%
        group_by(iso, Version) %>%
        summarize_if(is.numeric, sum) %>%
        gather(year, value, -iso, -Version) %>%
        mutate(year = as.numeric(str_replace(year, 'X',''))) %>%
        mutate(value = value/1000) %>% #Convert from Gg to Tg
        filter(!is.na(value))

    country_max <- dfplot_country_totals %>%
        group_by(iso) %>%
        summarize_at(vars(value), max, na.rm = TRUE) %>%
        arrange(-value) %>%
        mutate(iso = factor(iso, levels = iso, ordered = T)) %>%
        filter(iso != 'global')

    dfplot_country_sectors <- current_Em_by_Country_Sector %>%  mutate(Version = Current_name) %>%
        bind_rows( last_Em_by_Country_Sector %>%  mutate(Version = Last_name)  ) %>%
        left_join(MSL %>% select(aggregate_sectors,Figure_sector)%>% unique, by = c('sector' = "aggregate_sectors")) %>%
        mutate(Sector = Figure_sector) %>%
        # filter(Sector != "Shipping") %>%
        group_by(iso, Sector, Version) %>%
        summarize_if(is.numeric, sum) %>%
        gather(year, value, -Sector, -iso, -Version) %>%
        mutate(year = as.numeric(str_replace(year, 'X',''))) %>%
        mutate(value = value/1000) %>% #Convert from Gg to Tg
        filter(!is.na(value))

    # Plots -------------
    # plot global sector
    plot_global_sectors_modern <- ggplot(dfplot_global_sectors %>% filter(year>1959),
                                         aes(x = year, y = value, color = Sector, linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_sectors %>% filter(year>1959), Version == Last_name ),
                   linewidth = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
        geom_line( data = dplyr::filter( dfplot_global_sectors %>% filter(year>1959), Version == Current_name  ),
                   linewidth = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
        theme_diagnostic+
        version_comparison_formatting
    plot_global_sectors_mid <- ggplot(dfplot_global_sectors %>% filter(year < 1971, year > 1899),
                                      aes(x = year, y = value, color = Sector, linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_sectors %>% filter(year < 1971, year > 1899), Version == Last_name ),
                   linewidth = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
        geom_line( data = dplyr::filter( dfplot_global_sectors %>% filter(year < 1971, year > 1899), Version == Current_name  ),
                   linewidth = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
        theme_diagnostic+
        version_comparison_formatting
    plot_global_sectors_early <- ggplot(dfplot_global_sectors %>% filter(year < 1911),
                                        aes(x = year, y = value, color = Sector, linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_sectors %>% filter(year < 1911), Version == Last_name ),
                   linewidth = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
        geom_line( data = dplyr::filter( dfplot_global_sectors %>% filter(year < 1911), Version == Current_name  ),
                   linewidth = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
        theme_diagnostic+
        version_comparison_formatting+
        scale_x_continuous( breaks = seq( from = graph_start, to = graph_end, by = year_breaks_early ))
    # plot global fuel
    plot_global_fuel_modern <- ggplot(dfplot_global_fuel %>%filter(year>1959),
                                      aes(x = year, y = value, color = Fuel, linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_fuel %>%filter(year>1959), Version == Last_name ),
                   linewidth = 2 ,aes( x = year, y= value, color = Fuel ), alpha = .4 ) +
        geom_line( data = dplyr::filter( dfplot_global_fuel %>%filter(year>1959), Version == Current_name  ),
                   linewidth = 0.5, aes( x = year, y = value, color = Fuel ), alpha = 1 ) +
        theme_diagnostic+
        version_comparison_formatting
    plot_global_fuel_mid <- ggplot(dfplot_global_fuel %>% filter(year < 1971, year > 1899),
                                   aes(x = year, y = value, color = Fuel, linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_fuel %>% filter(year < 1971, year > 1899), Version == Last_name ),
                   linewidth = 2 ,aes( x = year, y= value, color = Fuel ), alpha = .4 ) +
        geom_line( data = dplyr::filter( dfplot_global_fuel %>% filter(year < 1971, year > 1899), Version == Current_name  ),
                   linewidth = 0.5, aes( x = year, y = value, color = Fuel ), alpha = 1 ) +
        theme_diagnostic+
        version_comparison_formatting
    plot_global_fuel_early <- ggplot(dfplot_global_fuel %>% filter(year < 1911),
                                     aes(x = year, y = value, color = Fuel, linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_fuel %>% filter(year < 1911), Version == Last_name ),
                   linewidth = 2 ,aes( x = year, y= value, color = Fuel ), alpha = .4 ) +
        geom_line( data = dplyr::filter( dfplot_global_fuel %>% filter(year < 1911), Version == Current_name  ),
                   linewidth = 0.5, aes( x = year, y = value, color = Fuel ), alpha = 1 ) +
        theme_diagnostic+
        version_comparison_formatting+
        scale_x_continuous( breaks = seq( from = graph_start, to = graph_end, by = year_breaks_early ))
    plot_global_fuel <- ggplot(dfplot_global_fuel, aes(x = year, y = value, color = Fuel, linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_fuel, Version == Last_name ),
                   linewidth = 2 ,aes( x = year, y= value, color = Fuel ), alpha = .4 ) +
        geom_line( data = dplyr::filter( dfplot_global_fuel, Version == Current_name  ),
                   linewidth = 0.5, aes( x = year, y = value, color = Fuel ), alpha = 1 ) +
        theme_diagnostic+
        version_comparison_formatting
    # plot global region
    plot_global_region_modern <- ggplot(dfplot_global_region %>% filter(year>1959),
                                        aes(x = year, y = value, color = Region, linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_region %>% filter(year>1959), Version == Last_name ),
                   linewidth = 2 ,aes( x = year, y= value, color = Region ), alpha = .4 ) +
        geom_line( data = dplyr::filter( dfplot_global_region %>% filter(year>1959), Version == Current_name  ),
                   linewidth = 0.5, aes( x = year, y = value, color = Region ), alpha = 1 ) +
        theme_diagnostic+
        version_comparison_formatting
    plot_global_region_early <- ggplot(dfplot_global_region %>% filter(year < 1911),
                                       aes(x = year, y = value, color = Region, linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_region %>% filter(year < 1911), Version == Last_name ),
                   linewidth = 2 ,aes( x = year, y= value, color = Region ), alpha = .4 ) +
        geom_line( data = dplyr::filter( dfplot_global_region %>% filter(year < 1911), Version == Current_name  ),
                   linewidth = 0.5, aes( x = year, y = value, color = Region ), alpha = 1 ) +
        theme_diagnostic+
        version_comparison_formatting+
        scale_x_continuous( breaks = seq( from = graph_start, to = graph_end, by = year_breaks_early ))
    plot_global_region_mid <- ggplot(dfplot_global_region %>% filter(year < 1971, year > 1899),
                                     aes(x = year, y = value, color = Region, linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_region %>% filter(year < 1971, year > 1899), Version == Last_name ),
                   linewidth = 2 ,aes( x = year, y= value, color = Region ), alpha = .4 ) +
        geom_line( data = dplyr::filter( dfplot_global_region %>% filter(year < 1971, year > 1899), Version == Current_name  ),
                   linewidth = 0.5, aes( x = year, y = value, color = Region ), alpha = 1 ) +
        theme_diagnostic+
        version_comparison_formatting
    plot_global_region <- ggplot(dfplot_global_region, aes(x = year, y = value, color = Region, linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_region, Version == Last_name ),
                   linewidth = 2 ,aes( x = year, y= value, color = Region ), alpha = .4 ) +
        geom_line( data = dplyr::filter( dfplot_global_region, Version == Current_name  ),
                   linewidth = 0.5, aes( x = year, y = value, color = Region ), alpha = 1 ) +
        theme_diagnostic+
        version_comparison_formatting
    # plot global total
    plot_global_total_modern <- ggplot(dfplot_global_total %>% filter(year>1959),
                                       aes(x = year, y = value,  linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_total %>% filter(year>1959), Version == Last_name ),
                   linewidth = 2 ,aes( x = year, y= value), alpha = .4 , color = global_color) +
        geom_line( data = dplyr::filter( dfplot_global_total %>% filter(year>1959), Version == Current_name  ),
                   linewidth = 0.5, aes( x = year, y = value ), alpha = 1 , color = global_color) +
        theme_diagnostic+
        version_comparison_formatting
    plot_global_total_mid <- ggplot(dfplot_global_total %>% filter(year < 1971, year > 1899),
                                    aes(x = year, y = value,  linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_total %>% filter(year < 1971, year > 1899), Version == Last_name ),
                   linewidth = 2 ,aes( x = year, y= value), alpha = .4 , color = global_color) +
        geom_line( data = dplyr::filter( dfplot_global_total %>% filter(year < 1971, year > 1899), Version == Current_name  ),
                   linewidth = 0.5, aes( x = year, y = value ), alpha = 1 , color = global_color) +
        theme_diagnostic+
        version_comparison_formatting
    plot_global_total_early <- ggplot(dfplot_global_total %>%  filter(year < 1911),
                                      aes(x = year, y = value,  linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_total %>%  filter(year < 1911), Version == Last_name ),
                   linewidth = 2 ,aes( x = year, y= value), alpha = .4 , color = global_color) +
        geom_line( data = dplyr::filter( dfplot_global_total %>%  filter(year < 1911), Version == Current_name  ),
                   linewidth = 0.5, aes( x = year, y = value ), alpha = 1 , color = global_color) +
        theme_diagnostic+
        version_comparison_formatting+
        scale_x_continuous( breaks = seq( from = graph_start, to = graph_end, by = year_breaks_early ))
    plot_global_total <- ggplot(dfplot_global_total, aes(x = year, y = value,  linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_total, Version == Last_name ),
                   linewidth = 2 ,aes( x = year, y= value), alpha = .4 , color = global_color) +
        geom_line( data = dplyr::filter( dfplot_global_total, Version == Current_name  ),
                   linewidth = 0.5, aes( x = year, y = value ), alpha = 1 , color = global_color) +
        theme_diagnostic+
        version_comparison_formatting

    # Plot Regions ---------------------
    plot_region <- function(df_region){

        modern <-  ggplot(df_region %>% filter(year>1959),
                          aes(x = year, y = value, color = Sector, linetype= Version))+
            geom_line( data = dplyr::filter( df_region %>% filter(year>1959), Version == Last_name ),
                       linewidth = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
            geom_line( data = dplyr::filter( df_region %>% filter(year>1959), Version == Current_name  ),
                       linewidth = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
            theme_diagnostic+
            version_comparison_formatting
        mid <-  ggplot(df_region %>% filter(year < 1971, year > 1899),
                       aes(x = year, y = value, color = Sector, linetype= Version))+
            geom_line( data = dplyr::filter( df_region %>% filter(year < 1971, year > 1899), Version == Last_name ),
                       linewidth = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
            geom_line( data = dplyr::filter( df_region %>% filter(year < 1971, year > 1899), Version == Current_name  ),
                       linewidth = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
            theme_diagnostic+
            version_comparison_formatting
        early <- ggplot(df_region %>% filter(year < 1911),
                        aes(x = year, y = value, color = Sector, linetype= Version))+
            geom_line( data = dplyr::filter( df_region %>% filter(year < 1911), Version == Last_name ),
                       linewidth = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
            geom_line( data = dplyr::filter( df_region %>% filter(year < 1911), Version == Current_name  ),
                       linewidth = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
            theme_diagnostic+
            version_comparison_formatting+
            scale_x_continuous( breaks = seq( from = graph_start, to = graph_end, by = year_breaks_early ))

        out <- list(modern, mid, early)
        return(out)
    }

    region_df_list <- list(dfplot_Reg1_sectors, dfplot_Reg2_sectors, dfplot_Reg3_sectors, dfplot_Reg4_sectors, dfplot_Reg5_sectors)
    if(NReg > 5){ region_df_list[[6]] <- dfplot_Reg6_sectors }
    if(NReg > 6){ region_df_list[[7]] <- dfplot_Reg7_sectors }
    if(NReg > 7){ region_df_list[[8]] <- dfplot_Reg8_sectors }
    if(NReg > 8){ region_df_list[[9]] <- dfplot_Reg9_sectors }
    if(NReg > 9){ region_df_list[[10]] <- dfplot_Reg10_sectors }


    region_plots <- lapply( region_df_list,
                           plot_region)

    # Plot Individual Countries ---------------------

    # Country Total
    plot_individual_country <- function(data,
                                        time_period,
                                        iso_break = 20,
                                        iso_break_early = 50){
        if(!(time_period %in% c('modern','mid','early'))) stop('Time period spelled wrong')
        if(time_period == 'modern'){
            plot_df <- data %>% filter(year>1959) }
        if(time_period == 'mid'){
            plot_df <- data %>% filter(year < 1971, year > 1899) }
        if(time_period == 'early'){
            plot_df <- data %>% filter(year< 1911) }

        plot <- ggplot(plot_df  ,
                       aes(x = year, y = value))+
            geom_line( data = plot_df %>% filter(Version == Last_name ),
                       linewidth = 2 , aes( x = year, y= value), alpha = .4 , color = global_color) +
            geom_line( data = plot_df %>% filter(Version == Current_name ),
                       linewidth = 0.5, aes( x = year, y = value ), alpha = 1 , color = global_color) +
            facet_wrap(~iso, ncol = 7, scales = "free")+
            theme_diagnostic+
            version_comparison_formatting+
            scale_x_continuous( breaks = seq( from = graph_start, to = graph_end, by = iso_break ))

        if(time_period == 'early'){
            plot <- plot +
                scale_x_continuous( breaks = seq( from = graph_start, to = graph_end, by = iso_break_early ))
        }
        return(plot)
    }

    dfplot_country1 <- dfplot_country_totals %>%
        filter(iso %in% country_max$iso[1:42])
    dfplot_country2 <- dfplot_country_totals %>%
        filter(iso %in% country_max$iso[43:84])
    dfplot_country3 <- dfplot_country_totals %>%
        filter(iso %in% country_max$iso[85:126])

    plot_country1_modern <- plot_individual_country(dfplot_country1, 'modern')
    plot_country1_mid <- plot_individual_country(dfplot_country1, 'mid')
    plot_country1_early <- plot_individual_country(dfplot_country1, 'early')

    plot_country2_modern <- plot_individual_country(dfplot_country2, 'modern')
    plot_country2_mid <- plot_individual_country(dfplot_country2, 'mid')
    plot_country2_early <- plot_individual_country(dfplot_country2, 'early')

    plot_country3_modern <- plot_individual_country(dfplot_country3, 'modern')
    plot_country3_mid <- plot_individual_country(dfplot_country3, 'mid')
    plot_country3_early <- plot_individual_country(dfplot_country3, 'early')

    # Country Sectors

    dfplot_countrysector1 <- dfplot_country_sectors %>%
        filter(iso %in% country_max$iso[1:28]) %>%
        filter(year >1950)

    plot_country_sector1 <- ggplot(dfplot_countrysector1, aes(x = year, y = value, color = Sector))+
            geom_line( data = dfplot_countrysector1 %>% filter(Version == Last_name ),
                       linewidth = 2 , aes( x = year, y= value, color = Sector), alpha = .4 ) +
            geom_line( data = dfplot_countrysector1 %>% filter(Version == Current_name ),
                       linewidth = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
            facet_wrap(~iso, ncol = 4, scales = "free")+
            theme_diagnostic+
            version_comparison_formatting+
            scale_x_continuous( breaks = seq( from = graph_start, to = graph_end, by = 10 ))

    dfplot_countrysector2 <- dfplot_country_sectors %>%
            filter(iso %in% country_max$iso[29:56]) %>%
            filter(year >1950)

    plot_country_sector2 <- ggplot(dfplot_countrysector2, aes(x = year, y = value, color = Sector))+
            geom_line( data = dfplot_countrysector2 %>% filter(Version == Last_name ),
                       linewidth = 2 , aes( x = year, y= value, color = Sector), alpha = .4 ) +
            geom_line( data = dfplot_countrysector2 %>% filter(Version == Current_name ),
                       linewidth = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
            facet_wrap(~iso, ncol = 4, scales = "free")+
            theme_diagnostic+
            version_comparison_formatting+
            scale_x_continuous( breaks = seq( from = graph_start, to = graph_end, by = 10 ))

    #output

    summary_plot_output <- c(list(plot_global_sectors_modern,
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
                                  plot_global_total_early,
                                  plot_country1_modern,
                                  plot_country1_mid,
                                  plot_country1_early,
                                  plot_country2_modern,
                                  plot_country2_mid,
                                  plot_country2_early,
                                  plot_country3_modern,
                                  plot_country3_mid,
                                  plot_country3_early,
                                  plot_country_sector1,
                                  plot_country_sector2), #23
                             unlist(region_plots, recursive = FALSE))

    names(summary_plot_output)[1:23] <- c('Global Sectors - Modern','Global Sectors - Mid', 'Global Sectors - Early',
                                    'Global Fuel - Modern', 'Global Fuel - Mid','Global Fuel - Early',
                                    'Global Region - Modern', 'Global Region - Mid', 'Global Region - Early',
                                    'Global Total - Modern','Global Total - Mid','Global Total - Early',
                                    'Individual Countries - Modern - 1 of 3', 'Individual Countries - Mid - 1 of 3','Individual Countries - Early - 1 of 3',
                                    'Individual Countries - Modern - 2 of 3', 'Individual Countries - Mid - 2 of 3','Individual Countries - Early - 2 of 3',
                                    'Individual Countries - Modern - 3 of 3','Individual Countries - Mid - 3 of 3','Individual Countries - Early - 3 of 3',
                                    'Country-Sector 1 of 2','Country-Sector 2 of 2' )
    names(summary_plot_output)[24:35] <- c( paste(RegionList[1],"- Modern"), paste(RegionList[1],"- Mid"),paste(RegionList[1],"- Early"),
                                    paste(RegionList[2],"- Modern"), paste(RegionList[2],"- Mid"),paste(RegionList[2],"- Early"),
                                    paste(RegionList[3],"- Modern"), paste(RegionList[3],"- Mid"),paste(RegionList[3],"- Early"),
                                    paste(RegionList[4],"- Modern"), paste(RegionList[4],"- Mid"),paste(RegionList[4],"- Early"))

    if(length(summary_plot_output) > 35){
        names(summary_plot_output)[36:38] <-  c( paste(RegionList[5],"- Modern"), paste(RegionList[5],"- Mid"),paste(RegionList[5],"- Early")) }
    if(length(summary_plot_output) > 38){
        names(summary_plot_output)[39:41] <-  c( paste(RegionList[6],"- Modern"), paste(RegionList[6],"- Mid"),paste(RegionList[6],"- Early")) }
    if(length(summary_plot_output) > 41){
        names(summary_plot_output)[42:44] <-  c( paste(RegionList[7],"- Modern"), paste(RegionList[7],"- Mid"),paste(RegionList[7],"- Early")) }
    if(length(summary_plot_output) > 44){
        names(summary_plot_output)[45:47] <-  c( paste(RegionList[8],"- Modern"), paste(RegionList[8],"- Mid"),paste(RegionList[8],"- Early")) }



    # If individual country(ies) selected:
    #Plot one graph with all country totals
    if(!any(is.na(country_select))){

        dfplot_all_countries <- current_Em_by_Country_Sector %>%  mutate(Version = Current_name) %>%
            bind_rows( last_Em_by_Country_Sector %>%  mutate(Version = Last_name)  ) %>%
            filter(sector != "1A3_International-shipping",
                   iso %in% country_select) %>%
            mutate(Country = iso) %>%
            group_by(Country, Version) %>%
            summarize_if(is.numeric, sum) %>%
            gather(year, value, -Country, -Version) %>%
            mutate(year = as.numeric(str_replace(year, 'X','')))%>%
            mutate(value = value/1000) #Convert from Gg to Tg

        plot_all_countries_modern <- ggplot(dfplot_all_countries %>% filter(year>1959),
                                            aes(x = year, y = value, color = Country, linetype= Version))+
            geom_line( data = dplyr::filter(dfplot_all_countries %>% filter(year>1959), Version == Last_name ),
                       linewidth = 2 ,aes( x = year, y= value, color = Country ), alpha = .4 ) +
            geom_line( data = dplyr::filter( dfplot_all_countries %>% filter(year>1959), Version == Current_name  ),
                       linewidth = 0.5, aes( x = year, y = value, color = Country ), alpha = 1 ) +
            version_comparison_formatting+
            theme_diagnostic
        plot_all_countries_mid <- ggplot(dfplot_all_countries %>% filter(year < 1971, year > 1899),
                                         aes(x = year, y = value, color = Country, linetype= Version))+
            geom_line( data = dplyr::filter( dfplot_all_countries %>% filter(year < 1971, year > 1899), Version == Last_name ),
                       linewidth = 2 ,aes( x = year, y= value, color = Country ), alpha = .4 ) +
            geom_line( data = dplyr::filter( dfplot_all_countries %>% filter(year < 1971, year > 1899), Version == Current_name  ),
                       linewidth = 0.5, aes( x = year, y = value, color = Country ), alpha = 1 ) +
            version_comparison_formatting+
            theme_diagnostic
        plot_all_countries_early <- ggplot(dfplot_all_countries %>% filter(year < 1911),
                                           aes(x = year, y = value, color = Country, linetype= Version))+
            geom_line( data = dplyr::filter( dfplot_all_countries %>% filter(year < 1911), Version == Last_name ),
                       linewidth = 2 ,aes( x = year, y= value, color = Country ), alpha = .4 ) +
            geom_line( data = dplyr::filter( dfplot_all_countries %>% filter(year < 1911), Version == Current_name  ),
                       linewidth = 0.5, aes( x = year, y = value, color = Country ), alpha = 1 ) +
            version_comparison_formatting+
            theme_diagnostic+
            scale_x_continuous( breaks = seq( from = graph_start, to = graph_end, by = year_breaks_early ))

        # Redefine lists of plots for output
        last_index <- length(summary_plot_output)
        summary_plot_output[[(last_index+1)]] <- plot_all_countries_modern
        summary_plot_output[[(last_index+2)]] <- plot_all_countries_mid
        summary_plot_output[[(last_index+3)]] <- plot_all_countries_early
        names(summary_plot_output)[(last_index+1)] <- "Selected Countries - Modern"
        names(summary_plot_output)[(last_index+2)] <- "Selected Countries - Mid"
        names(summary_plot_output)[(last_index+3)] <- "Selected Countries - Early"

        #Plot individual country graphs by sector
        dfplot_countries_sectors <- current_Em_by_Country_Sector %>%  mutate(Version = Current_name) %>%
            bind_rows( last_Em_by_Country_Sector %>%  mutate(Version = Last_name)  ) %>%
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
                geom_line( data = dplyr::filter( dfplot_country%>%
                                                     filter(year > 1959), Version == Last_name ),
                           linewidth = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
                geom_line( data = dplyr::filter( dfplot_country%>%
                                                     filter(year > 1959), Version == Current_name  ),
                           linewidth = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
                version_comparison_formatting+
                theme_diagnostic
            plot_country_mid <- ggplot(dfplot_country %>%
                                           filter(year < 1971,
                                                  year > 1899),
                                       aes(x = year, y = value, color = Sector, linetype= Version))+
                geom_line( data = dplyr::filter( dfplot_country%>%
                                                     filter(year < 1971,
                                                            year > 1899), Version == Last_name ),
                           linewidth = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
                geom_line( data = dplyr::filter( dfplot_country%>%
                                                     filter(year < 1971,
                                                            year > 1899), Version == Current_name  ),
                           linewidth = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
                version_comparison_formatting+
                theme_diagnostic
            plot_country_early <- ggplot(dfplot_country %>%
                                             filter(year <1911),
                                         aes(x = year, y = value, color = Sector, linetype= Version))+
                geom_line( data = dplyr::filter( dfplot_country%>%
                                                     filter(year <1911), Version == Last_name ),
                           linewidth = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
                geom_line( data = dplyr::filter( dfplot_country%>%
                                                     filter(year <1911), Version == Current_name  ),
                           linewidth = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
                version_comparison_formatting+
                theme_diagnostic+
                scale_x_continuous( breaks = seq( from = graph_start, to = graph_end, by = year_breaks_early ))

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
                                             EM = em,
                                             MCL = Master_Country_List_Compare_Reg){

    RegionList <- unique(MCL$Paper_Figure_Region)
    NReg <- length(RegionList)
    selected_countries_index_start <- 21+NReg*3+1

    pdf( paste0( "../final-emissions/diagnostics/Comparison_Plots_",EM,'.pdf' ),
         width = 14, height = 10, paper ='special' )

    grid.arrange( plot_list[["Global Sectors - Modern"]]+ggtitle('Global Sectors'),
                  plot_list[["Global Fuel - Modern"]]+ggtitle('Global Fuel'),
                  plot_list[["Global Region - Modern"]]+ggtitle('Global Region'),
                  plot_list[["Global Total - Modern"]]+ggtitle('Global Total'),
                  ncol = 3, nrow = 3,padding = unit(3, "line"),
                  top = paste0( "Global CEDS comparison - current to previous version" ) )

    # grid.arrange( plot_list[["Global Sectors - Mid"]]+ggtitle('Global Sectors'),
    #               plot_list[["Global Fuel - Mid"]]+ggtitle('Global Fuel'),
    #               plot_list[["Global Region - Mid"]]+ggtitle('Global Region'),
    #               plot_list[["Global Total - Mid"]]+ggtitle('Global Total'),
    #               ncol = 3, nrow = 3,padding = unit(3, "line"),
    #               top = paste0( "Global CEDS comparison - current to previous version" ) )

    # grid.arrange( plot_list[["Global Sectors - Early"]]+ggtitle('Global Sectors'),
    #               plot_list[["Global Fuel - Early"]]+ggtitle('Global Fuel'),
    #               plot_list[["Global Region - Early"]]+ggtitle('Global Region'),
    #               plot_list[["Global Total - Early"]]+ggtitle('Global Total'),
    #               ncol = 3, nrow = 3,padding = unit(3, "line"),
    #               top = paste0( "Global CEDS comparison - current to previous version" ) )

    grid.arrange( plot_list[[paste(RegionList[1],"- Modern")]]+ggtitle(RegionList[1]),
                  plot_list[[paste(RegionList[2],"- Modern")]]+ggtitle(RegionList[2]),
                  plot_list[[paste(RegionList[3],"- Modern")]]+ggtitle(RegionList[3]),
                  plot_list[[paste(RegionList[4],"- Modern")]]+ggtitle(RegionList[4]),
                  plot_list[[paste(RegionList[5],"- Modern")]]+ggtitle(RegionList[5]),
                  plot_list[[paste(RegionList[6],"- Modern")]]+ggtitle(RegionList[6]),
                  plot_list[[paste(RegionList[7],"- Modern")]]+ggtitle(RegionList[7]),
                  plot_list[[paste(RegionList[8],"- Modern")]]+ggtitle(RegionList[8]),
                  ncol = 3, nrow = 3,
                  top = paste0( "Regional CEDS comparison - current to previous version" ) )

    # grid.arrange( plot_list[[paste(RegionList[1],"- Mid")]]+ggtitle(RegionList[1]),
    #               plot_list[[paste(RegionList[2],"- Mid")]]+ggtitle(RegionList[2]),
    #               plot_list[[paste(RegionList[3],"- Mid")]]+ggtitle(RegionList[3]),
    #               plot_list[[paste(RegionList[4],"- Mid")]]+ggtitle(RegionList[4]),
    #               plot_list[[paste(RegionList[5],"- Mid")]]+ggtitle(RegionList[5]),
    #               plot_list[[paste(RegionList[6],"- Mid")]]+ggtitle(RegionList[6]),
    #               plot_list[[paste(RegionList[7],"- Mid")]]+ggtitle(RegionList[7]),
    #               plot_list[[paste(RegionList[8],"- Mid")]]+ggtitle(RegionList[8]),
    #               ncol = 3, nrow = 3,
    #               top = paste0( "Regional CEDS comparison - current to previous version" ) )

    # grid.arrange( plot_list[[paste(RegionList[1],"- Early")]]+ggtitle(RegionList[1]),
    #               plot_list[[paste(RegionList[2],"- Early")]]+ggtitle(RegionList[2]),
    #               plot_list[[paste(RegionList[3],"- Early")]]+ggtitle(RegionList[3]),
    #               plot_list[[paste(RegionList[4],"- Early")]]+ggtitle(RegionList[4]),
    #               plot_list[[paste(RegionList[5],"- Early")]]+ggtitle(RegionList[5]),
    #               plot_list[[paste(RegionList[6],"- Early")]]+ggtitle(RegionList[6]),
    #               plot_list[[paste(RegionList[7],"- Early")]]+ggtitle(RegionList[7]),
    #               plot_list[[paste(RegionList[8],"- Early")]]+ggtitle(RegionList[8]),
    #               ncol = 3, nrow = 3,
    #               top = paste0( "Regional CEDS comparison - current to previous version" ) )

    # if(!any(is.na(CS))){
    #     if(length(CS)>6)stop("individual countries selected is more than 6 - summary visualization can't support more than 6")
    #     index_last <- length(plot_list)
    #     countries_plot_list <- plot_list[selected_countries_index_start:index_last]
    #     list_last <- length(countries_plot_list)
    #     for(i in 1:list_last){
    #         countries_plot_list[[i]] <- countries_plot_list[[i]]+ ggtitle(names(countries_plot_list)[i])
    #     }
    #     if( length(countries_plot_list) > 9){
    #         do.call("grid.arrange", c(countries_plot_list[1:9], ncol=3,nrow=3,
    #                                   top = paste0( "Regional CEDS comparison - current to previous version" )))
    #         do.call("grid.arrange", c(countries_plot_list[10: length(countries_plot_list) ], ncol=3,nrow=3,
    #                                   top = paste0( "Regional CEDS comparison - current to previous version" )))
    #
    #     }else(
    #         do.call("grid.arrange", c(countries_plot_list, ncol=3,nrow=3,
    #                                   top = paste0( "Regional CEDS comparison - current to previous version" )))
    #     )
    # }
    #
    # grid.arrange( plot_list[["Individual Countries - Modern - 1 of 3"]]+ggtitle('Individual Countries - Modern - 1 of 3'),
    #               padding = unit(3, "line"),
    #               top = paste0( "Global CEDS comparison - current to previous version" ) )
    # grid.arrange( plot_list[["Individual Countries - Mid - 1 of 3"]]+ggtitle('Individual Countries - Mid - 1 of 3'),
    #               padding = unit(3, "line"),
    #               top = paste0( "Global CEDS comparison - current to previous version" ) )
    # grid.arrange( plot_list[["Individual Countries - Early - 1 of 3"]]+ggtitle('Individual Countries - Early - 1 of 3'),
    #               padding = unit(3, "line"),
    #               top = paste0( "Global CEDS comparison - current to previous version" ) )
    #
    # grid.arrange( plot_list[["Individual Countries - Modern - 2 of 3"]]+ggtitle('Individual Countries - Modern - 2 of 3'),
    #               padding = unit(3, "line"),
    #               top = paste0( "Global CEDS comparison - current to previous version" ) )
    # grid.arrange( plot_list[["Individual Countries - Mid - 2 of 3"]]+ggtitle('Individual Countries - Mid - 2 of 3'),
    #               padding = unit(3, "line"),
    #               top = paste0( "Global CEDS comparison - current to previous version" ) )
    # grid.arrange( plot_list[["Individual Countries - Early - 2 of 3"]]+ggtitle('Individual Countries - Early - 2 of 3'),
    #               padding = unit(3, "line"),
    #               top = paste0( "Global CEDS comparison - current to previous version" ) )
    #
    # grid.arrange( plot_list[["Individual Countries - Modern - 3 of 3"]]+ggtitle('Individual Countries - Modern - 3 of 3'),
    #               padding = unit(3, "line"),
    #               top = paste0( "Global CEDS comparison - current to previous version" ) )
    # grid.arrange( plot_list[["Individual Countries - Mid - 3 of 3"]]+ggtitle('Individual Countries - Mid - 3 of 3'),
    #               padding = unit(3, "line"),
    #               top = paste0( "Global CEDS comparison - current to previous version" ) )
    # grid.arrange( plot_list[["Individual Countries - Early - 3 of 3"]]+ggtitle('Individual Countries - Early - 3 of 3'),
    #               padding = unit(3, "line"),
    #               top = paste0( "Global CEDS comparison - current to previous version" ) )
    #
    # grid.arrange( plot_list[['Country-Sector 1 of 2']]+ggtitle('Country-Sector 1 of 2'),
    #               padding = unit(3, "line"),
    #               top = paste0( "Global CEDS comparison - current to previous version" ) )
    # grid.arrange( plot_list[['Country-Sector 2 of 2']]+ggtitle('Country-Sector 2 of 2'),
    #               padding = unit(3, "line"),
    #               top = paste0( "Global CEDS comparison - current to previous version" ) )


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


#----------------------------------------------
# compare_emissions_files_summary_visuals
# Brief: Plot emisssions comparisons from 2 emissions files anywhere in the system
# Details: Based on the summary visualization figures. Plots modern, mid, and early
#   plots. Will print blank plots if the file only has modern era.
# Dependencies: none
# Author(s): Rachel
# Params: country_select: vector or isos to be plotted individually
#   em1 = comparison emissions file 1
#   em1_name = lable for emissions file 1
#   em2 = comparison emissions file 2
#   em2_name = lable for emissions file 2
#   file_indicator = file tag for the printed pdf file Comparison_em_fileindicator.pdf
#   global_color = color of the global emissions plot
# Return: none
# Input Files:
# Output Files:  nonne

compare_emissions_files_summary_visuals <- function(
        em1,em1_name,
        em2,em2_name,
        file_indicator,
        country_select = c('usa','chn','idn','ind'),
        EM = em,
        global_color = 'darkorchid'){

    # em1 = unscaled_em
    # em1_name = 'unscaled'
    # em2 =  energy_scaled_em
    # em2_name = 'IEA_scaled'

    # file_indicator = 'EDGAR_scaling_diagnostics'
    # country_select = c('usa','chn','idn','ind')
    # EM = em
    # global_color = 'darkorchid'

    if(em1_name == em2_name){stop('em1_name and em2_name must be different in compare_emissions_files_summary_visuals' )}

    MCL <- readData( "MAPPINGS", "Master_Country_List")
    MSL <- readData(domain = 'MAPPINGS', file_name = 'Master_Sector_Level_map')

    printLog("Create comparison plots for comparison data in compare_emissions_files_summary_visuals()")
    # Plot options
    graph_start <- min(extended_years)
    graph_end <- max(extended_years)
    year_breaks <- 10

    # Plot theme
    # Gray background for diagnostic figures
    theme_diagnostic <- list(theme(panel.background = element_rect(fill = "gray90",colour = "gray90",linewidth = 0.5, linetype = "solid")),
                             theme(panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',colour = "gray95")),
                             theme(panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',colour = "white")))
    # Formatting for the 3 different time scales
    version_comparison_formatting <- list(
        scale_x_continuous( breaks = seq( from = graph_start, to = graph_end, by = year_breaks )) ,
        #scale_y_continuous( labels = 'comma' ),
        guides( linetype = guide_legend( override.aes = list( size = c( 1.5, 0.5 ) ) ) ),
        labs( x = "" , y = 'Emissions [Tg/yr]' ),
        ggtitle(EM),
        scale_linetype_manual( name= 'Version',
                               breaks = c( em1_name ,em2_name),
                               values = c( 'solid','solid' ) ) )

    #Process Data for figures
    # Convert from Gg to Tg: 1000Gg = 1 Tg

    printLog("Process data for comparison plots in compare_emissions_files_summary_visuals()")

    dfplot_global_sectors <- em1 %>%  mutate(Version = em1_name) %>%
        bind_rows( em2 %>%  mutate(Version = em2_name)  ) %>%
        left_join(MSL %>% select(working_sectors_v1, Figure_sector) %>% unique, by = c('sector' = "working_sectors_v1")) %>%
        mutate(Sector = Figure_sector) %>%
        group_by(Sector, Version) %>%
        summarize_if(is.numeric, sum) %>%
        gather(year, value, -Sector, -Version) %>%
        mutate(year = as.numeric(str_replace(year, 'X',''))) %>%
        mutate(value = value/1000) %>% #Convert from Gg to Tg
        filter(!is.na(value))

    dfplot_global_fuel <- em1 %>%  mutate(Version = em1_name) %>%
        bind_rows( em2 %>%  mutate(Version = em2_name)  ) %>%
        mutate(Fuel = fuel) %>%
        group_by(Fuel, Version) %>%
        summarize_if(is.numeric, sum) %>%
        gather(year, value, -Fuel, -Version) %>%
        mutate(year = as.numeric(str_replace(year, 'X',''))) %>%
        mutate(value = value/1000) %>% #Convert from Gg to Tg
        filter(!is.na(value))


    dfplot_global_total <- em1 %>%  mutate(Version = em1_name) %>%
        bind_rows( em2 %>%  mutate(Version = em2_name)  ) %>%
        group_by(Version) %>%
        summarize_if(is.numeric, sum) %>%
        gather(year, value, -Version) %>%
        mutate(year = as.numeric(str_replace(year, 'X',''))) %>%
        mutate(value = value/1000) %>% #Convert from Gg to Tg
        filter(!is.na(value))

    dfplot_Regions_sectors <- em1 %>%  mutate(Version = em1_name) %>%
        bind_rows( em2 %>%  mutate(Version = em2_name)  ) %>%
        left_join(MCL %>% select(iso, Paper_Figure_Region)%>% unique, relationship = "many-to-many") %>%
        left_join(MSL %>% select(working_sectors_v1, Figure_sector) %>% unique, by = c('sector' = "working_sectors_v1")) %>%
        mutate(Sector = Figure_sector) %>%
        # filter(Sector != "Shipping") %>%
        mutate(Region = Paper_Figure_Region) %>%
        group_by(Region, Sector, Version) %>%
        summarize_if(is.numeric, sum) %>%
        gather(year, value, -Sector, -Region, -Version) %>%
        mutate(year = as.numeric(str_replace(year, 'X',''))) %>%
        mutate(value = value/1000) %>% #Convert from Gg to Tg
        filter(!is.na(value))

    # Create list of Regions
    RegionList <- unique(dfplot_Regions_sectors$Region)
    NReg <- length(RegionList)

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
    printLog("Plot global comparison plots for current vs previous data in compare_emissions_files_summary_visuals()")
    # plot global sector
    plot_global_sectors_modern <- ggplot(dfplot_global_sectors %>% filter(year>1959),
                                         aes(x = year, y = value, color = Sector, linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_sectors %>% filter(year>1959), Version == em1_name ),
                   linewidth = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
        geom_line( data = dplyr::filter( dfplot_global_sectors %>% filter(year>1959), Version == em2_name  ),
                   linewidth = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
        theme_diagnostic+
        version_comparison_formatting

    plot_global_sectors_mid <- ggplot(dfplot_global_sectors %>% filter(year < 1971, year > 1899),
                                      aes(x = year, y = value, color = Sector, linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_sectors %>% filter(year < 1971, year > 1899), Version == em1_name ),
                   linewidth = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
        geom_line( data = dplyr::filter( dfplot_global_sectors %>% filter(year < 1971, year > 1899), Version == em2_name  ),
                   linewidth = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
        theme_diagnostic+
        version_comparison_formatting
    plot_global_sectors_early <- ggplot(dfplot_global_sectors %>% filter(year < 1911),
                                        aes(x = year, y = value, color = Sector, linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_sectors %>% filter(year < 1911), Version == em1_name ),
                   linewidth = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
        geom_line( data = dplyr::filter( dfplot_global_sectors %>% filter(year < 1911), Version == em2_name  ),
                   linewidth = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
        theme_diagnostic+
        version_comparison_formatting

    # plot global fuel
    plot_global_fuel_modern <- ggplot(dfplot_global_fuel %>%filter(year>1959),
                                      aes(x = year, y = value, color = Fuel, linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_fuel %>%filter(year>1959), Version == em1_name ),
                   linewidth = 2 ,aes( x = year, y= value, color = Fuel ), alpha = .4 ) +
        geom_line( data = dplyr::filter( dfplot_global_fuel %>%filter(year>1959), Version == em2_name  ),
                   linewidth = 0.5, aes( x = year, y = value, color = Fuel ), alpha = 1 ) +
        theme_diagnostic+
        version_comparison_formatting

    plot_global_fuel_mid <- ggplot(dfplot_global_fuel %>% filter(year < 1971, year > 1899),
                                   aes(x = year, y = value, color = Fuel, linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_fuel %>% filter(year < 1971, year > 1899), Version == em1_name ),
                   linewidth = 2 ,aes( x = year, y= value, color = Fuel ), alpha = .4 ) +
        geom_line( data = dplyr::filter( dfplot_global_fuel %>% filter(year < 1971, year > 1899), Version == em2_name  ),
                   linewidth = 0.5, aes( x = year, y = value, color = Fuel ), alpha = 1 ) +
        theme_diagnostic+
        version_comparison_formatting
    plot_global_fuel_early <- ggplot(dfplot_global_fuel %>% filter(year < 1911),
                                     aes(x = year, y = value, color = Fuel, linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_fuel %>% filter(year < 1911), Version == em1_name ),
                   linewidth = 2 ,aes( x = year, y= value, color = Fuel ), alpha = .4 ) +
        geom_line( data = dplyr::filter( dfplot_global_fuel %>% filter(year < 1911), Version == em2_name  ),
                   linewidth = 0.5, aes( x = year, y = value, color = Fuel ), alpha = 1 ) +
        theme_diagnostic+
        version_comparison_formatting
    plot_global_fuel <- ggplot(dfplot_global_fuel, aes(x = year, y = value, color = Fuel, linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_fuel, Version == em1_name ),
                   linewidth = 2 ,aes( x = year, y= value, color = Fuel ), alpha = .4 ) +
        geom_line( data = dplyr::filter( dfplot_global_fuel, Version == em2_name  ),
                   linewidth = 0.5, aes( x = year, y = value, color = Fuel ), alpha = 1 ) +
        theme_diagnostic+
        version_comparison_formatting
    # plot global region
    plot_global_region_modern <- ggplot(dfplot_global_region %>% filter(year>1959),
                                        aes(x = year, y = value, color = Region, linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_region %>% filter(year>1959), Version == em1_name ),
                   linewidth = 2 ,aes( x = year, y= value, color = Region ), alpha = .4 ) +
        geom_line( data = dplyr::filter( dfplot_global_region %>% filter(year>1959), Version == em2_name  ),
                   linewidth = 0.5, aes( x = year, y = value, color = Region ), alpha = 1 ) +
        theme_diagnostic+
        version_comparison_formatting

    plot_global_region_early <- ggplot(dfplot_global_region %>% filter(year < 1911),
                                       aes(x = year, y = value, color = Region, linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_region %>% filter(year < 1911), Version == em1_name ),
                   linewidth = 2 ,aes( x = year, y= value, color = Region ), alpha = .4 ) +
        geom_line( data = dplyr::filter( dfplot_global_region %>% filter(year < 1911), Version == em2_name  ),
                   linewidth = 0.5, aes( x = year, y = value, color = Region ), alpha = 1 ) +
        theme_diagnostic+
        version_comparison_formatting
    plot_global_region_mid <- ggplot(dfplot_global_region %>% filter(year < 1971, year > 1899),
                                     aes(x = year, y = value, color = Region, linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_region %>% filter(year < 1971, year > 1899), Version == em1_name ),
                   linewidth = 2 ,aes( x = year, y= value, color = Region ), alpha = .4 ) +
        geom_line( data = dplyr::filter( dfplot_global_region %>% filter(year < 1971, year > 1899), Version == em2_name  ),
                   linewidth = 0.5, aes( x = year, y = value, color = Region ), alpha = 1 ) +
        theme_diagnostic+
        version_comparison_formatting
    plot_global_region <- ggplot(dfplot_global_region, aes(x = year, y = value, color = Region, linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_region, Version == em1_name ),
                   linewidth = 2 ,aes( x = year, y= value, color = Region ), alpha = .4 ) +
        geom_line( data = dplyr::filter( dfplot_global_region, Version == em2_name  ),
                   linewidth = 0.5, aes( x = year, y = value, color = Region ), alpha = 1 ) +
        theme_diagnostic+
        version_comparison_formatting
    # plot global total
    plot_global_total_modern <- ggplot(dfplot_global_total %>% filter(year>1959),
                                       aes(x = year, y = value,  linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_total %>% filter(year>1959), Version == em1_name ),
                   linewidth = 2 ,aes( x = year, y= value), alpha = .4 , color = global_color) +
        geom_line( data = dplyr::filter( dfplot_global_total %>% filter(year>1959), Version == em2_name  ),
                   linewidth = 0.5, aes( x = year, y = value ), alpha = 1 , color = global_color) +
        theme_diagnostic+
        version_comparison_formatting

    plot_global_total_mid <- ggplot(dfplot_global_total %>% filter(year < 1971, year > 1899),
                                    aes(x = year, y = value,  linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_total %>% filter(year < 1971, year > 1899), Version == em1_name ),
                   linewidth = 2 ,aes( x = year, y= value), alpha = .4 , color = global_color) +
        geom_line( data = dplyr::filter( dfplot_global_total %>% filter(year < 1971, year > 1899), Version == em2_name  ),
                   linewidth = 0.5, aes( x = year, y = value ), alpha = 1 , color = global_color) +
        theme_diagnostic+
        version_comparison_formatting
    plot_global_total_early <- ggplot(dfplot_global_total %>%  filter(year < 1911),
                                      aes(x = year, y = value,  linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_total %>%  filter(year < 1911), Version == em1_name ),
                   linewidth = 2 ,aes( x = year, y= value), alpha = .4 , color = global_color) +
        geom_line( data = dplyr::filter( dfplot_global_total %>%  filter(year < 1911), Version == em2_name  ),
                   linewidth = 0.5, aes( x = year, y = value ), alpha = 1 , color = global_color) +
        theme_diagnostic+
        version_comparison_formatting
    plot_global_total <- ggplot(dfplot_global_total, aes(x = year, y = value,  linetype= Version))+
        geom_line( data = dplyr::filter( dfplot_global_total, Version == em1_name ),
                   linewidth = 2 ,aes( x = year, y= value), alpha = .4 , color = global_color) +
        geom_line( data = dplyr::filter( dfplot_global_total, Version == em2_name  ),
                   linewidth = 0.5, aes( x = year, y = value ), alpha = 1 , color = global_color) +
        theme_diagnostic+
        version_comparison_formatting

    # Plot Regions ---------------------
    plot_region <- function(df_region){

        modern <-  ggplot(df_region %>% filter(year>1959),
                          aes(x = year, y = value, color = Sector, linetype= Version))+
            geom_line( data = dplyr::filter( df_region %>% filter(year>1959), Version == em1_name ),
                       linewidth = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
            geom_line( data = dplyr::filter( df_region %>% filter(year>1959), Version == em2_name  ),
                       linewidth = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
            theme_diagnostic+
            version_comparison_formatting

        mid <-  ggplot(df_region %>% filter(year < 1971, year > 1899),
                       aes(x = year, y = value, color = Sector, linetype= Version))+
            geom_line( data = dplyr::filter( df_region %>% filter(year < 1971, year > 1899), Version == em1_name ),
                       linewidth = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
            geom_line( data = dplyr::filter( df_region %>% filter(year < 1971, year > 1899), Version == em2_name  ),
                       linewidth = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
            theme_diagnostic+
            version_comparison_formatting
        early <- ggplot(df_region %>% filter(year < 1911),
                        aes(x = year, y = value, color = Sector, linetype= Version))+
            geom_line( data = dplyr::filter( df_region %>% filter(year < 1911), Version == em1_name ),
                       linewidth = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
            geom_line( data = dplyr::filter( df_region %>% filter(year < 1911), Version == em2_name  ),
                       linewidth = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
            theme_diagnostic+
            version_comparison_formatting
 out <- list(modern, mid, early)

        return(out)
    }
    printLog("Plot region comparison plots for current vs previous data in compare_emissions_files_summary_visuals()")
    region_plots <- lapply(list(dfplot_Africa_sectors, dfplot_China_sectors, dfplot_North_America_sectors, dfplot_Europe_sectors, dfplot_FSU_sectors, dfplot_Latin_America_sectors, dfplot_Other_sectors),
                           plot_region)

    #output

    summary_plot_output <- c(list(plot_global_sectors_modern,
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
        printLog("Plot individual country comparison plots for current vs previous data in compare_emissions_files_summary_visuals()")
        dfplot_all_countries <- em2 %>%  mutate(Version = em2_name) %>%
            bind_rows( em1 %>%  mutate(Version = em1_name)  ) %>%
            filter(sector != "1A3_International-shipping",
                   iso %in% country_select) %>%
            mutate(Country = iso) %>%
            group_by(Country, Version) %>%
            summarize_if(is.numeric, sum) %>%
            gather(year, value, -Country, -Version) %>%
            mutate(year = as.numeric(str_replace(year, 'X','')))%>%
            mutate(value = value/1000) #Convert from Gg to Tg

        plot_all_countries_modern <- ggplot(dfplot_all_countries %>% filter(year>1959),
                                            aes(x = year, y = value, color = Country, linetype= Version))+
            geom_line( data = dplyr::filter(dfplot_all_countries , Version == em1_name )%>% filter(year>1959),
                       linewidth = 2 ,aes( x = year, y= value, color = Country ), alpha = .4 ) +
            geom_line( data = dplyr::filter( dfplot_all_countries , Version == em2_name )%>% filter(year>1959),
                       linewidth = 0.5, aes( x = year, y = value, color = Country ), alpha = 1 ) +
            version_comparison_formatting+
            theme_diagnostic
        plot_all_countries_mid <- ggplot(dfplot_all_countries %>% filter(year < 1971, year > 1899),
                                         aes(x = year, y = value, color = Country, linetype= Version))+
            geom_line( data = dplyr::filter( dfplot_all_countries , Version == em1_name )%>% filter(year < 1971, year > 1899),
                       linewidth = 2 ,aes( x = year, y= value, color = Country ), alpha = .4 ) +
            geom_line( data = dplyr::filter( dfplot_all_countries , Version == em2_name  )%>% filter(year < 1971, year > 1899),
                       linewidth = 0.5, aes( x = year, y = value, color = Country ), alpha = 1 ) +
            version_comparison_formatting+
            theme_diagnostic
        plot_all_countries_early <- ggplot(dfplot_all_countries %>% filter(year < 1911),
                                           aes(x = year, y = value, color = Country, linetype= Version))+
            geom_line( data = dplyr::filter( dfplot_all_countries , Version == em1_name )%>% filter(year < 1911),
                       linewidth = 2 ,aes( x = year, y= value, color = Country ), alpha = .4 ) +
            geom_line( data = dplyr::filter( dfplot_all_countries , Version == em2_name  )%>% filter(year < 1911),
                       linewidth = 0.5, aes( x = year, y = value, color = Country ), alpha = 1 ) +
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
        dfplot_countries_sectors <- em2 %>%  mutate(Version = em2_name) %>%
            bind_rows( em1 %>%  mutate(Version = em1_name)  ) %>%
            left_join(MSL %>% select(working_sectors_v1, Figure_sector) %>% unique, by = c('sector' = "working_sectors_v1")) %>%
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
                geom_line( data = dplyr::filter( dfplot_country, Version == em1_name ) %>% filter(year > 1959),
                           linewidth = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
                geom_line( data = dplyr::filter( dfplot_country, Version == em2_name  ) %>% filter(year > 1959),
                           linewidth = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
                version_comparison_formatting+
                theme_diagnostic
            plot_country_mid <- ggplot(dfplot_country %>%
                                           filter(year < 1971,
                                                  year > 1899),
                                       aes(x = year, y = value, color = Sector, linetype= Version))+
                geom_line( data = dplyr::filter( dfplot_country, Version == em1_name )%>%
                               filter(year < 1971,
                                      year > 1899),
                           linewidth = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
                geom_line( data = dplyr::filter( dfplot_country, Version == em2_name  )%>%
                               filter(year < 1971,
                                      year > 1899),
                           linewidth = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
                version_comparison_formatting+
                theme_diagnostic
            plot_country_early <- ggplot(dfplot_country %>%
                                             filter(year <1911),
                                         aes(x = year, y = value, color = Sector, linetype= Version))+
                geom_line( data = dplyr::filter( dfplot_country, Version == em1_name ) %>% filter(year <1911),
                           linewidth = 2 ,aes( x = year, y= value, color = Sector ), alpha = .4 ) +
                geom_line( data = dplyr::filter( dfplot_country, Version == em2_name  ) %>% filter(year <1911),
                           linewidth = 0.5, aes( x = year, y = value, color = Sector ), alpha = 1 ) +
                version_comparison_formatting+
                theme_diagnostic

            last_index <- length(summary_plot_output)
            summary_plot_output[[(last_index+1)]] <- plot_country_modern
            summary_plot_output[[(last_index+2)]] <- plot_country_mid
            summary_plot_output[[(last_index+3)]] <- plot_country_early
            names(summary_plot_output)[(last_index+1)] <- paste0( country_select[i], ' - Modern')
            names(summary_plot_output)[(last_index+2)] <- paste0( country_select[i], ' - Mid')
            names(summary_plot_output)[(last_index+3)] <- paste0( country_select[i], ' - Early')

        }
        }


#-----------------------
#print graphs
    printLog("Print region comparison plots for current vs previous data in compare_emissions_files_summary_visuals()")
    plot_list <- summary_plot_output

    pdf( paste0( "../diagnostic-output/Comparison_Plots_",EM,'_',file_indicator,'.pdf' ),
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

    if(!is.na(country_select)){
        if(length(country_select)>6)stop("individual countries selected is more than 6 - summary visualization can't support more than 6")
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


