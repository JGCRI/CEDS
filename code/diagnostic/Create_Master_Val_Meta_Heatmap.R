# ------------------------------------------------------------------------------
# Program Name: Create_Val_Metadata_Heatmap.R
# Author: Ben Goldstein
# Date Last Updated: 14 June 2017
# Program Purpose: Uses the F.create_EF_value_meta_heatmap to create a heatmap
#                  diagnostic of the value metadata for a single country.
# Input Files: F.[em]_scaled_EF-value_metadata.csv
#               
# Output Files: A figure in the diagnostic-output
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
    headers <- c( "data_functions.R",'common_data.R', 
                  'IO_functions.R', 'emissions_scaling_functions.R') # Additional function files may be required.
    log_msg <- "Create value metadata heatmap" # First message to be printed to the log
    script_name <- "Create_Val_Metadata_Heatmap.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "NH3"

# ---------------------------------------------------------------------------
# 0.5 Define functions
    
# Brief: extracts and returns the legend from a ggplot graph
    g_legend <- function( a.gplot ) {
        tmp <- ggplot_gtable( ggplot_build( a.gplot ) )
        leg <- which( sapply( tmp$grobs, function(x) x$name ) == "guide-box" )
        legend <- tmp$grobs[[leg]]
        return( legend ) 
    }

# ------------------------------------------------------------------------------
# createSinglePlot
# Brief: creates a stacked bar chart of ef scaling inventories for a specific
#        region or sector
# Dependencies: None
# Author: Ben Goldstein
# parameters:
#     identifier: the name of the agg sector or region to be plotted 
#     meta_classified: the value metadata (reclassified) to be plotted
#     id_type: does the identifier refer to a region or an agg sector?
#     inventory_colors: the manually-determined list of colors for each inventory
# return: a ggplot of the bar chart 
# input files: reclassified value metadata
# output: none
    createSinglePlot <- function( identifier, meta_classified, id_type = "Region", inventory_colors ) {
    
    # Separate extraction procedure by region vs. sector.
        if ( id_type == "Region" ) {    
        
        # Extract metadata for this region
            meta_this_region <- meta_classified[ which(meta_classified$Region == identifier ), 
                                                 c( "year", "value", "prepost" ) ]
            
        # Count the frequency of each year/value/prepost occurance
            regional_counts <- meta_this_region %>% 
                                    count( year, value, prepost )
            
        # Make the years numeric so we can have a continunous x-axis
            regional_counts$year <- substr( regional_counts$year, 2, 5 ) %>% 
                                      as.numeric()
        
        # Make a geom_col plot of frequency (n, determined in count() above) by year.
        # Fill is determined by inventory (as stated in the colors list) and
        #   transparency is determined by extension direction
            p <- ggplot( regional_counts, aes( year, n ) ) + 
                 geom_col( aes( fill = value, alpha = prepost ), 
                           position = 'stack', width = 1 ) +
                 theme( legend.position = "none" ) +
                 scale_fill_manual( values = inventory_colors ) +
                 theme( axis.title.y = element_blank(),
                        axis.text.y = element_blank(),
                        axis.ticks.y = element_blank(), 
                        axis.title.x = element_blank(),
                        panel.background = element_blank(),
                        panel.border = element_rect( colour = "grey80", 
                                                     fill = NA, size = .8 ) ) +
                 ggtitle( identifier ) +       
                 scale_alpha_discrete( range = c( 1, 0.4 ) ) +
                 theme( text = element_text( size = 6 ) )
            
        } else if ( id_type == "Sector" ) {
        # Extract metadata for this sector
            meta_this_sector <- meta_classified[ which( meta_classified$Figure_sector == identifier ), 
                                                 c( "year", "value", "prepost" ) ]
            
        # Count the frequency of each year/value/prepost occurance
            sectoral_counts <- meta_this_sector %>% 
                                count( year, value, prepost )
            
        # Make the years numeric so we can have a continunous x-axis
            sectoral_counts$year <- substr( sectoral_counts$year, 2, 5 ) %>% 
                                      as.numeric()
            
        # Make a geom_col plot of frequency (n, determined in count() above) by year.
        # Fill is determined by inventory (as stated in the colors list) and
        #   transparency is determined by extension direction
            p <- ggplot( sectoral_counts, aes( year, n ) ) + 
              geom_col( aes( fill = value, alpha = prepost ), 
                        position = 'stack', width = 1 ) +
              theme( legend.position = "none" ) +
              scale_fill_manual( values = inventory_colors ) +
              theme( axis.title.y = element_blank(),
                     axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(), 
                     axis.title.x = element_blank(),
                     panel.background = element_blank(),
                     panel.border = element_rect( colour = "grey80", 
                                                  fill = NA, size = .8 ) ) +
              ggtitle( identifier ) +       
              scale_alpha_discrete( range = c( 1, 0.4 ) ) +
              theme( text = element_text( size = 6 ) )
            
        }
      
      # Return the created plot
      return( p )
        
    }
    
# ------------------------------------------------------------------------------
# createMasterValMetaHeatmap
# Brief: creates a single image of scaling inventory percent breakdowns by
#        aggregate sector or region
# Dependencies: None
# Author: Ben Goldstein
# parameters:
#     meta_notes: the properly formatted, long-form value_metadata 
#     country_map: the Master_Country_List
#     sector_map: the Master_Sector_Level_Map
#     map_by: Should the data be aggregated by agg sector or region?
# return: reclassified metadata by inventory 
# input files: value metadata
# output: ("Inventory scaling percentages of ", em, " by ", map_by).png

    createMasterValMetaHeatmap <- function( meta_notes, country_map, sector_map, map_by = "Sector" ) {
    
    # Map the value_metadata notes to region and sector. Collect a list of
    #   all unique regions and sectors present in the data.
        mapped_meta_notes <- left_join( meta_notes, country_map[ , c( 'iso', 'Region' ) ] )
        all_regions <- unique( mapped_meta_notes$Region )
        all_regions <- all_regions[ which( all_regions != "Global" ) ]

        sector_map <- sector_map[ , c( 'working_sectors_v1', 'Figure_sector' ) ] # Rename for col comparison
        colnames( sector_map )[1] <- "sector"
        
        mapped_meta_notes <- left_join( mapped_meta_notes, sector_map )
        all_sectors <- unique( mapped_meta_notes$Figure_sector )
        all_sectors <- all_sectors[ !is.na( all_sectors ) ]
    
    # Hand the data off to a new df for editing content
        meta_split <- mapped_meta_notes

    # remove the semicolon from the end of all non-default entries
        indices <- grepl( ";", meta_split$comment )
        meta_split$comment <- as.character( meta_split$comment )
        meta_split$comment[ indices ] <- substr( meta_split$comment[ indices ], 0, 
                                                 nchar( meta_split$comment[ indices ] ) - 2 )
    
    # Remove sectors that aren't actually present in CEDS
        sectors_to_remove <- c( "11A_Volcanoes", "11B_Forest-fires", "11C_Other-natural" )
        meta_split <- meta_split[ which( meta_split$sector %!in% sectors_to_remove), ]
        
    # Discard all value metadata notes that occur before the final semicolon,
    #   now that line-end semicolons have been removed
        indices <- grepl( ";", meta_split$comment )
        meta_split$comment[ indices ]  <- sub( ".*; ", "", meta_split$comment[ indices ] )
        meta_split$comment <- as.character( meta_split$comment )
        
    # Reclassify the notes for display purposes using the function in
    #   emissions_scaling_functions.R
        printLog( "Reclassifying value metadata" )
        meta_classified <- F.reclass_metavalue( meta_split )
    
    # Hand off only relevant columns to a dataframe that will be used for
    #   plotting only; operating on notes is finished
        meta_for_plots <- meta_classified[ , c( "Region", "Figure_sector", "year", "value", "prepost" ) ]
    
    # A list of pre-determined colors for each inventory allows us to force
    #   Default to be gray and Zero to be white
        inventory_colors <- c( "Default" = "#cccccc",
                               "Zero emissions" = "#ffffff",
                               "EDGAR 4.3-PEGASOS" = "#026fff",
                               "EMEP_NFR09" = "#00BE67",
                               "REAS 2.1" = "#d966ff",
                               "EMEP_NFR14" = "#73e600",
                               "UNFCCC, 2015" = "#f75555",
                               "Environment Canada, 2013" = "#ff8c1a",
                               "Environment and Climate Change Canada, 2016" = "#ffe11a",
                               "US EPA, 2016" = "#990033",
                               "US" = "#1d3d84",
                               "Li et al., 2017" = "#fcde1e",
                               "TEPA, 2016" = "#1de0cc",
                               "Argentina UNFCCC submission, 2016" = "#ff8484",
                               "Kurokawa et. al, 2013" = "#990606",
                               "South Korea National Institute of Environmental Research, 2016" = "#875c1d",
                               "Australian Department of the Environment, 2016" = "#1c661b",
                               "EDGAR 4.2" = "#80d4ff" )
        
    # For either all_sectors or all_regions, iterate through and create a single
    #   plot (see above function) based on counts for that region/sector and add
    #   to a list of plots
        list_of_plots <- list()
        printLog( paste0( "Generating aggregate metaval heatmaps by ", map_by ) )
        if ( map_by == "Sector" ) {
            list_of_plots <- lapply( all_sectors, 
                                     createSinglePlot, 
                                     meta_classified = meta_for_plots, 
                                     id_type = "Sector",
                                     inventory_colors = inventory_colors )
        } else if ( map_by == "Region" ) {
            list_of_plots <- lapply( all_regions, 
                                     createSinglePlot, 
                                     meta_classified = meta_for_plots, 
                                     id_type = "Region",
                                     inventory_colors = inventory_colors )
        }

    # Get a quick count of all data
        all_counts <- meta_for_plots %>% 
                          count( year, value, prepost )
        
    # Create a single master plot. This plot will never be arranged or 
    #   displayed; its purpose is to generate a unifying legend for all the data,
    #   as it contains all the inventories present
        plot_for_legend <- ggplot(all_counts, aes( year, n ) ) + 
          geom_area(aes(fill = value, alpha = prepost), position = 'stack') +
          scale_fill_manual(values = inventory_colors) +
          labs(fill="Inventory", alpha="Extension") +
          ggtitle("Don't use this plot") +       
          scale_alpha_discrete(range = c(1, 0.4)) +
          theme(text = element_text(size=4))
    
    # Extract the plot's legend
        inventory_legend <- g_legend( plot_for_legend )
    
    # Arrange the list of plots into a grid, next to the legend
        arranged_plots <- grid.arrange( arrangeGrob( grobs=list_of_plots ),
                                        inventory_legend, 
                                        widths = c( 6, 1 ), 
                                        nrow = 1,
                                        top = textGrob( paste0( "Inventory scaling percentages of ", em, 
                                                                " by ", map_by ), 
                                        gp = gpar( fontsize = 15, font = 8 ) ) )
    
    # Save the output file and return
        ggsave( paste0( "../diagnostic-output/value-meta-heatmaps/MasterHeatmapBy", 
                        map_by, ".png" ), 
                arranged_plots, width = 7, height = 4 )
        
        return( meta_classified )
    }

# ---------------------------------------------------------------------------
# 1. Read in data
    library( grid )
    
# Read in and format value metadata for the given emissions species
    value_metadata <- readData( "MED_OUT", paste0( "F.", em, "_", "scaled_EF-value_metadata" ), 
                                meta = FALSE, to_numeric = FALSE )
    value_metadata <- melt( value_metadata, id.vars = c( 'iso', 'sector', 'fuel' ) )
    names( value_metadata ) <- c( "iso", "sector", "fuel", "year", "comment" )
    value_metadata$comment <- as.character( value_metadata$comment )

# We need these two files for mapping to aggregate regions and sectors
    country_map <- readData( "MAPPINGS", "Master_Country_List" )
    sector_map <- readData( "MAPPINGS", "Master_Sector_Level_Map" )
    
# ---------------------------------------------------------------------------
# 2. Exectue function

    classed_meta <- createMasterValMetaHeatmap( value_metadata, country_map, sector_map, map_by = "Sector" )
    classed_meta <- createMasterValMetaHeatmap( value_metadata, country_map, sector_map, map_by = "Region" )
    
