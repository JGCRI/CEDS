# ------------------------------------------------------------------------------
# Program Name: diag_seasonal_plots.R
# Author(s): Leyang Feng, Hamza Ahsan
# Date Last Updated: April 26, 2021
# Program Purpose: Generates gridding diagnostic line plots for specified cells.
# Input Files:
# Output Files: [em]_[iso]_[region].pdf
# Notes:
# TODO: Change input from row/column of cell array matrix to lat/lon to improve
# robustness for higher resolutions. Add resolution as a new variable. Generalize
# script to allow for user-specified resolution (eg, array dimensions should not
# be hard-coded to 360 x 720)
# ------------------------------------------------------------------------------

# load utility functions
source( '../code/diagnostic/diag_utility_functions.R' )

# specify em species
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"

# load intermediate gridded emission files
nc_file_path <- '../intermediate-output/gridded-emissions'
nc_file_list <- list.files( path = nc_file_path, pattern = paste0( 'CEDS_', em, '_AIR_anthro' ) )
nc_file_list <- grep( '.nc', nc_file_list, fixed = T, value = T )

# select years 1950 onward (earlier years are not very informative)
start_year <- 1950
extract_years <- sapply(strsplit(nc_file_list, "[_]"),function(x) x[5])
nc_file_list <- nc_file_list[which(extract_years >= start_year)]

# Plot global line plots

gridcell_area <- grid_area( 0.5, all_lon = T )
days_in_month <- c( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 )
alt_level_thck <- c(305, rep(610, 24))

global_monthly_sectors <- lapply( nc_file_list, function( nc_file ) {

    year <- unlist( strsplit( nc_file, split = '_' ) )[ 5 ]
    temp_nc <- nc_open( paste0( nc_file_path, '/', nc_file ) )
    AIR_var <- ncvar_get( temp_nc, 'AIR' )
    nc_close( temp_nc )

    AIR_temp_array <- array( dim = c( 360, 720, 25, 12 ) )
    DIM_rep <- rep( days_in_month, dim(AIR_temp_array)[3] / 12 )

    AIR_array <- array( dim = c( 12 ) )

    # Convert to kt and get global totals
    for ( i in 1: dim( AIR_temp_array )[ 4 ] ) {
        for ( j in 1: dim( AIR_temp_array )[ 3 ] ) {
            # Rotate and transpose grid
            AIR_temp_array[ , , j, i ] <- flip_a_matrix( t( AIR_var[ , , j, i ] ) )

            # Convert from flux to mass (kg m-2 s-1 to kt)
            AIR_temp_array[ , , j, i ] <- AIR_temp_array[ , , j, i ] * gridcell_area * ( DIM_rep[ i ] * 24 * 60 * 60 ) * 0.000001
        }
        AIR_array[ i ] <- sum(  AIR_temp_array[ , , , i ] )
    }

    out_df <- data.frame( em = em,
                          year = year,
                          month = 1 : 12,
                          value = AIR_array,
                          stringsAsFactors = F )
} )

global_emission_total <- do.call( 'rbind', global_monthly_sectors )

time_line <- seq(from = as.POSIXct(paste0(min(global_emission_total$year), '-01-01')), to = as.POSIXct(paste0(max(global_emission_total$year), '-12-31')), by = "months")

# Now plot total emissions for that cell
plotting_df <- global_emission_total[ global_emission_total$em == em, ]
plotting_df$time_line <- time_line
plotting_df <- dplyr::select(plotting_df, value, time_line)

write.csv(plotting_df, file = paste0( '../diagnostic-output/gridding-diagnostic-plots/seasonal-line-plots/global_', em, '_Aircraft.csv' ), row.names = FALSE)

ag_plot <- ggplot( plotting_df, aes( x = time_line, y = value ) ) +
    geom_line( size = 0.2 ) +
    ggtitle( paste0( em, ' ', 'Total' ) ) +
    labs( y = 'Emissions (kt)' ) +
    labs( x = 'Date' ) +
    theme(panel.background=element_blank(),
          panel.grid.minor = element_line(colour="gray95"),
          panel.grid.major = element_line(colour="gray88"),
          panel.border = element_rect(colour = "grey80", fill=NA, size=.8))

pdf( paste0( '../diagnostic-output/gridding-diagnostic-plots/seasonal-line-plots/global_', em, '_Aircraft.pdf' ), width = 15, height = 5, paper= 'special' )
grid.arrange( ag_plot, ncol=1 )
dev.off( )


