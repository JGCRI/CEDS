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

regions <- unique(Master_Country_List$Paper_Figure_Region)
regions <- regions[order(regions)]
sectors <- unique(MSLevel$Figure_sector)
sectors <- sectors[which(!is.na(sectors))]
sectors <- sectors[order(sectors)]

fuels <- unique(Master_Fuel_list$fuel)

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

palette <- c( "#73e600", # yellow green "#7CAE00"
              "#00BE67", # green
              "#00dee6", # teal "#00BFC4"
              "#00A9FF", # blue
              "#0066ff", # dark blue,
              "#C77CFF", # purple
              "#ff80d9", # pink "#FF61CF"
              "#f75f55", # red #F8766D
              "#ff8000", #orange
              "#666666" )# black

region_colors <- data.frame('region' = regions,
                            'color' = palette[1:n_regions], stringsAsFactors = F)

sector_colors <- data.frame('sector' = sectors,
                            'color' = palette[1:n_sectors], stringsAsFactors = F)

fuel_colors <- data.frame('fuel' = fuels,
                            'color' = palette[1:n_fuels], stringsAsFactors = F)

# ---------------------------------------------------------------------------
# End
logStop()


