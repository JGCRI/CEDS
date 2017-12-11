# ------------------------------------------------------------------------------
# Program Name: Paper_Figures_plot_colors.R
# Author: Rachel Hoesly
# Date Last Updated:
# Program Purpose:
#
# Input Files:
# Output Files:
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
              'common_data.R', 'data_functions.R') # Additional function files may be required.
log_msg <- "Processing colors for figures in paper and supplement" # First message to be printed to the log
script_name <- "Paper_Figures_plot_colors.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# 0. Load Data

Master_Country_List <- readData('MAPPINGS', 'Master_Country_List')
MSLevel <- readData('MAPPINGS', 'Master_Sector_Level_map')
Master_Fuel_list <- readData('MAPPINGS','Master_Fuel_Sector_List','.xlsx')[[2]]
# ---------------------------------------------------------------------------
# 1. Format Plot Color Data frames

# order regions
# regions <- unique(Master_Country_List$Paper_Figure_Region)
# regions <- regions[order(regions)]
# if( 'International' %in% regions ) regions <- regions[which(regions != 'International')]
# regions <- c(regions, 'International')
regions <- c( "China","Other Asia/Pacific","North America","Europe",
              "Latin America", "Africa" ,"Former Soviet Union","International Air-Ship")

# # order sectors
# sectors <- unique(MSLevel$Figure_sector)
# sectors <- sectors[which(!is.na(sectors))]
# sectors <- sectors[order(sectors)]
sectors <- c( "Energy Transf/Ext" , "Industry"  ,"RCO" , "Transportation" ,
              "Agriculture" , "Solvents" , "Waste",  "Shipping","Air")

# order fuels
# fuels <- unique(Master_Fuel_list$fuel)
# fuels <- fuels[which(!is.na(fuels))]
# fuels <- fuels[order(fuels)]
fuels <- c(  "heavy_oil" , "light_oil" , "diesel_oil",  "biomass" ,"natural_gas" ,
              "hard_coal", "brown_coal" ,"coal_coke" ,
             "process" )

n_sectors <- length(sectors)
n_regions <- length(regions)
n_fuels <- length(fuels)

# palette <- c( "#ff8000", #orange
#               "#f75f55", # red #F8766D
#               "#ff80d9", # pink "#FF61CF"
#               "#C77CFF", # purple
#               "#0066ff", # dark blue,
#               "#00A9FF", # blue
#               "#00dee6", # teal "#00BFC4"
#               "#00BE67", # green
#               "#73e600", # yellow green "#7CAE00"
#               "#666666" )# black

palette <- as.data.frame( matrix (c( "#73e600", "yellow green",
                                    "#00BE67", "green",
                                    "#80d4ff", "light blue",
                                    "#0052cc", "dark blue",
                                    "#d966ff", "pink/purple",
                                    "#f75555", "red",
                                    "#ff8c1a", "orange",
                                    "#ffe11a", "yellow",
                                    "#999999" , "light grey",
                                    "#990033" , "wine",
                                    "#333333" , "dark grey") , ncol = 2,  byrow = T ) , stringsAsFactors = F)
names(palette) <- c('color', 'description')

# palette <- c("#E69F00",
#              "#56B4E9",
#              "#009E73",
#              "#F0E442",
#              "#0072B2",
#              "#D55E00",
#              "#CC79A7",
#              "#999999",
#              "#333333") #grey and black

region_colors <- palette[1:n_regions,]
region_colors$region <- regions

sector_colors <- palette[1:n_sectors,]
sector_colors$sector <- sectors

fuel_colors <- palette[1:n_fuels,]
fuel_colors$fuels <- fuels

# ---------------------------------------------------------------------------
# End
logStop()
