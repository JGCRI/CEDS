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
    

    g_legend <- function(a.gplot) {
        tmp <- ggplot_gtable( ggplot_build( a.gplot ) )
        leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
        legend <- tmp$grobs[[leg]]
        return(legend)}


    createSinglePlot <- function( identifier, meta_classified, id_type = "Region" ) {
        
        inventory_colors <- c( "Default" = "#cccccc",
                               "Zero emissions" = "#000000",
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

        if (id_type == "Region") {    
            meta_this_region <- meta_classified[ which(meta_classified$Region == identifier), c("year","value","prepost") ]
            
            
            regional_counts <- meta_this_region %>% 
                                    count( year, value, prepost )
            colnames(regional_counts)[2] <- "val"
            
            regional_counts$year <- substr(regional_counts$year, 2, 5) %>% 
                                          as.numeric()
            
            first_years <- group_by(regional_counts, val, prepost) %>%
                              summarise(year = min(year) - 1) %>%
                              filter(year > 1960) %>%
                              mutate(n = 0)
            last_years <- group_by(regional_counts, val, prepost) %>%
                              summarise(year = max(year) + 1) %>%
                              filter(year < 2014) %>%
                              mutate(n = 0)
            
            regional_counts <- rbind(first_years, last_years, regional_counts)
            
            p <- ggplot(regional_counts, aes( year, n ) ) + 
                 geom_area(aes(fill = val, alpha = prepost), position = 'stack') +
                 theme(legend.position="none") +
                 scale_fill_manual(values = inventory_colors) +
                 theme(axis.title.y=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks.y=element_blank(), axis.title.x = element_blank(),
                   panel.background=element_blank(),
                   panel.border = element_rect(colour = "grey80", fill=NA, size=.8)) +
                 ggtitle(identifier) +       
                 scale_alpha_discrete(range = c(1, 0.4)) +
                 theme(text = element_text(size=4))
            
            list_of_plots <- c(list_of_plots, p)
            
        } else if (id_type == "Sector") {
            meta_this_sector <- meta_classified[ which(meta_classified$Figure_sector == identifier), c("year","value","prepost") ]
            
            sectoral_counts <- meta_this_sector %>% count( year, value, prepost )
            
            sectoral_counts$year <- substr(sectoral_counts$year, 2, 5) %>% as.numeric()
            
            p <- ggplot(sectoral_counts, aes( year, n ) ) + 
                 geom_area(aes(fill = value, alpha = prepost), position = 'stack') +
                 theme(legend.position="none")
        }
      
      return(p)
        
    }

    createMasterValMetaHeatmap <- function( meta_notes, country_map, sector_map, map_by = "Sector" ) {
        
        mapped_meta_notes <- left_join( meta_notes, country_map[ ,c('iso','Region')] )
        all_regions <- unique( mapped_meta_notes$Region )
        
        print(colnames(mapped_meta_notes))
        sector_map <- sector_map[ ,c('detailed_sectors','Figure_sector') ]
        colnames( sector_map )[1] <- "sector"
        mapped_meta_notes <- left_join( mapped_meta_notes, sector_map )
        all_sectors <- unique( mapped_meta_notes$Figure_sector )
        
        meta_split <- mapped_meta_notes

        # remove the semicolon from the end of all non-default entries
        indices <- grepl( ";", meta_split$comment )
        meta_split$comment <- as.character(meta_split$comment)
        meta_split$comment[ indices ] <- substr(meta_split$comment[indices], 0, nchar(meta_split$comment[indices]) - 2)
        
        sectors_to_remove <- c("11A_Volcanoes", "11B_Forest-fires", "11C_Other-natural")
        meta_split <- meta_split[ which( meta_split$sector %!in% sectors_to_remove), ]
        
        # Discard all value metadata notes that occur before the final semicolon
        indices <- grepl( ";", meta_split$comment )
        meta_split$comment[indices]  <- sub( ".*; ", "", meta_split$comment[indices] )
        meta_split$comment <- as.character(meta_split$comment)
        
        # Reclassify the notes for display purposes
        printLog("Reclassifying value metadata")
        meta_classified <- F.reclass_metavalue( meta_split )
        
        meta_for_plots <- meta_classified[, c("Region","Figure_sector","year","value","prepost")]
        # meta_classified <- meta_classified[ which(!is.na(meta_classified$Figure_sector)), ]
        # meta_classified <- meta_classified[ which(!is.na(meta_classified$Region)), ]
        
        list_of_plots <- list()
        
        if (map_by == "Sector") {
            list_of_plots <- lapply( all_sectors, 
                                     createSinglePlot, 
                                     meta_classified = meta_for_plots, 
                                     id_type="Sector" )
        } else if (map_by == "Region") {
            list_of_plots <- lapply( all_regions, 
                                     createSinglePlot, 
                                     meta_classified = meta_for_plots, 
                                     id_type="Region" )
        }
        
        
        all_counts <- meta_for_plots %>% 
          count( year, value, prepost )
        
        plot_for_legend <- ggplot(all_counts, aes( year, n ) ) + 
          geom_area(aes(fill = value, alpha = prepost), position = 'stack') +
          scale_fill_manual(values = inventory_colors) +
          theme( legend.position="bottom",
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(), axis.title.x = element_blank(),
                panel.background=element_blank(),
                panel.border = element_rect(colour = "grey80", fill=NA, size=.8)) +
          ggtitle("Don't use this plot") +       
          scale_alpha_discrete(range = c(1, 0.4)) +
          theme(text = element_text(size=4))
        
        inventory_legend <- g_legend(plot_for_legend)
      
        n <- length(list_of_plots)
        nCol <- floor(sqrt(n))

        arranged_plots <- grid.arrange( arrangeGrob( grobs=list_of_plots),
                                        inventory_legend, heights=c(10,1))
        
        ggsave("TestOutput.png", arranged_plots)
        
        treturn( meta_classified )
    }

# ---------------------------------------------------------------------------
# 1. Read in data

    value_metadata <- readData( "MED_OUT", paste0( "F.", em, "_", "scaled_EF-value_metadata" ), 
                                meta = FALSE, to_numeric=FALSE)
    value_metadata <- melt(value_metadata, id.vars = c('iso','sector','fuel'))
    names( value_metadata ) <- c( "iso", "sector", "fuel", "year", "comment" )
    value_metadata$comment <- as.character(value_metadata$comment)
    
    country_map <- readData("MAPPINGS", "Master_Country_List")
    sector_map <- readData("MAPPINGS", "Master_Sector_Level_Map")
    
# ---------------------------------------------------------------------------
# 2. Exectue function

    classed_meta <- createMasterValMetaHeatmap( value_metadata, country_map, sector_map, map_by = "Region" )


