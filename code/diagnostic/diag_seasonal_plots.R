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
nc_file_list <- list.files( path = nc_file_path, pattern = paste0( 'CEDS_', em, '_anthro' ) )
nc_file_list <- grep( '.nc', nc_file_list, fixed = T, value = T )

# select years 1950 onward (earlier years are not very informative)
start_year <- 1950
extract_years <- sapply(strsplit(nc_file_list, "[_]"),function(x) x[4])
nc_file_list <- nc_file_list[which(extract_years >= start_year)]

if (!is.null(nc_file_list)){

  cell_10_df <- read.csv( './gridding/diagnostics/cells_10.csv', stringsAsFactors = F )
  cell_10_df$id <- paste0( 'X', 11 : 20 )

  cell_10_res_list <- lapply( nc_file_list, function( nc_file ) {
    #print( nc_file )
    year <- unlist( strsplit( nc_file, split = '_' ) )[ 4 ]
    temp_nc <- nc_open( paste0( nc_file_path, '/', nc_file ) )
    AGR_var <- ncvar_get( temp_nc, 'AGR' )
    ENE_var <- ncvar_get( temp_nc, 'ENE' )
    IND_var <- ncvar_get( temp_nc, 'IND' )
    TRA_var <- ncvar_get( temp_nc, 'TRA' )
    SLV_var <- ncvar_get( temp_nc, 'SLV' )
    WST_var <- ncvar_get( temp_nc, 'WST' )
    RCO_var <- ncvar_get( temp_nc, 'RCO' )
    nc_close( temp_nc )
    AGR_array <- array( dim = c( 360, 720, 12 ) )
    ENE_array <- array( dim = c( 360, 720, 12 ) )
    IND_array <- array( dim = c( 360, 720, 12 ) )
    TRA_array <- array( dim = c( 360, 720, 12 ) )
    SLV_array <- array( dim = c( 360, 720, 12 ) )
    WST_array <- array( dim = c( 360, 720, 12 ) )
    RCO_array <- array( dim = c( 360, 720, 12 ) )

    for ( i in 1 : 12 ) {
      AGR_array[ , , i ] <- flip_a_matrix( t( AGR_var[ , , i ] ) )
      ENE_array[ , , i ] <- flip_a_matrix( t( ENE_var[ , , i ] ) )
      IND_array[ , , i ] <- flip_a_matrix( t( IND_var[ , , i ] ) )
      TRA_array[ , , i ] <- flip_a_matrix( t( TRA_var[ , , i ] ) )
      SLV_array[ , , i ] <- flip_a_matrix( t( SLV_var[ , , i ] ) )
      WST_array[ , , i ] <- flip_a_matrix( t( WST_var[ , , i ] ) )
      RCO_array[ , , i ] <- flip_a_matrix( t( RCO_var[ , , i ] ) )
    }

    each_cell_df_list <- lapply( 1 : nrow( cell_10_df ), function( i ) {
      id <- cell_10_df[ i, 'id' ]
      row <- cell_10_df[ i, 'row' ]
      col <- cell_10_df[ i, 'col' ]
      AGR_em <- AGR_array[ row, col, ]
      ENE_em <- ENE_array[ row, col, ]
      IND_em <- IND_array[ row, col, ]
      TRA_em <- TRA_array[ row, col, ]
      SLV_em <- SLV_array[ row, col, ]
      WST_em <- WST_array[ row, col, ]
      RCO_em <- RCO_array[ row, col, ]
      out_df <- data.frame( em = em,
                            id = id,
                            year = year,
                            month = 1 : 12,
                            AGR = AGR_em,
                            ENE = ENE_em,
                            IND = IND_em,
                            TRA = TRA_em,
                            SLV = SLV_em,
                            WST = WST_em,
                            RCO = RCO_em,
                            stringsAsFactors = F )
    } )
    each_cell_df <- do.call( 'rbind', each_cell_df_list )
  } )
  cell_10_res <- do.call( 'rbind', cell_10_res_list )
  cell_10_res <- gather( cell_10_res, sector, value, -em, -id, -year, -month )

  cell_list <- sort( unique( cell_10_res$id ) )
  sector_list <- sort( unique( cell_10_res$sector ) )

  time_line <- seq(from = as.POSIXct(paste0(min(cell_10_res$year), '-01-01')), to = as.POSIXct(paste0(max(cell_10_res$year), '-12-31')), by = "months")
  cell_id <- 1
  for ( cell in cell_list ) {
    iso_em_sector_plot_list <- lapply( sector_list, function ( sector ) {
      plotting_df <- cell_10_res[ cell_10_res$em == em & cell_10_res$id == cell & cell_10_res$sector == sector, ]
      plotting_df$yearmonth <- paste0( plotting_df$year, plotting_df$month )
      plotting_df$time_line <- time_line

      plot <- ggplot( plotting_df, aes( x = time_line, y = value ) ) +
        geom_line( size = 0.2 ) +
        ggtitle( paste0( em, ' ', sector, ' - ', cell_10_df$iso[cell_id], ', ', cell_10_df$region[cell_id], ' (single grid cell)' ) ) +
        labs( y = expression( "Emission Flux (kg " * "m"^-2 * " s"^-1 * ")" ) ) +
        labs( x = 'Date' ) +
        theme(panel.background=element_blank(),
              panel.grid.minor = element_line(colour="gray95"),
              panel.grid.major = element_line(colour="gray88"),
              panel.border = element_rect(colour = "grey80", fill=NA, size=.8))
    } )

    # Now plot total emissions for that cell
    plotting_df <- cell_10_res[ cell_10_res$em == em & cell_10_res$id == cell, ]
    plotting_df$time_line <- time_line
    agr_plotting_df <- plotting_df %>%
      dplyr::select(value, time_line) %>%
      dplyr::group_by(time_line) %>%
      dplyr::summarize(sum(value)) %>%
      dplyr::rename("total" = "sum(value)")

    ag_plot <- ggplot( agr_plotting_df, aes( x = time_line, y = total ) ) +
      geom_line( size = 0.2 ) +
      ggtitle( paste0( em, ' ', 'Total', ' - ', cell_10_df$iso[cell_id], ', ', cell_10_df$region[cell_id], ' (single grid cell)' ) ) +
      labs( y = expression( "Emission Flux (kg " * "m"^-2 * " s"^-1 * ")" ) ) +
      labs( x = 'Date' ) +
      theme(panel.background=element_blank(),
            panel.grid.minor = element_line(colour="gray95"),
            panel.grid.major = element_line(colour="gray88"),
            panel.border = element_rect(colour = "grey80", fill=NA, size=.8))

    pdf( paste0( '../diagnostic-output/gridding-diagnostic-plots/seasonal-line-plots/', em, '_', cell_10_df$iso[cell_id], '_', cell_10_df$region[cell_id], '.pdf' ), width = 10, height = 20, paper= 'special' )
    grid.arrange( ag_plot, iso_em_sector_plot_list[[ 1 ]],
                  iso_em_sector_plot_list[[ 2 ]], iso_em_sector_plot_list[[ 3 ]],
                  iso_em_sector_plot_list[[ 4 ]], iso_em_sector_plot_list[[ 5 ]],
                  iso_em_sector_plot_list[[ 6 ]], iso_em_sector_plot_list[[ 7 ]],
                  ncol=1 )
    dev.off( )
    cell_id <- cell_id + 1
  }
}

# Plot global line plots

gridcell_area <- grid_area( 0.5, all_lon = T )
days_in_month <- c( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 )

global_monthly_sectors <- lapply( nc_file_list, function( nc_file ) {

    year <- unlist( strsplit( nc_file, split = '_' ) )[ 4 ]
    temp_nc <- nc_open( paste0( nc_file_path, '/', nc_file ) )
    AGR_var <- ncvar_get( temp_nc, 'AGR' )
    ENE_var <- ncvar_get( temp_nc, 'ENE' )
    IND_var <- ncvar_get( temp_nc, 'IND' )
    TRA_var <- ncvar_get( temp_nc, 'TRA' )
    SLV_var <- ncvar_get( temp_nc, 'SLV' )
    WST_var <- ncvar_get( temp_nc, 'WST' )
    RCO_var <- ncvar_get( temp_nc, 'RCO' )
    nc_close( temp_nc )

    AGR_temp_array <- array( dim = c( 360, 720, 12 ) )
    ENE_temp_array <- array( dim = c( 360, 720, 12 ) )
    IND_temp_array <- array( dim = c( 360, 720, 12 ) )
    TRA_temp_array <- array( dim = c( 360, 720, 12 ) )
    SLV_temp_array <- array( dim = c( 360, 720, 12 ) )
    WST_temp_array <- array( dim = c( 360, 720, 12 ) )
    RCO_temp_array <- array( dim = c( 360, 720, 12 ) )

    AGR_array <- array( dim = c( 12 ) )
    ENE_array <- array( dim = c( 12 ) )
    IND_array <- array( dim = c( 12 ) )
    TRA_array <- array( dim = c( 12 ) )
    SLV_array <- array( dim = c( 12 ) )
    WST_array <- array( dim = c( 12 ) )
    RCO_array <- array( dim = c( 12 ) )

    # Convert to kt and get global totals by sector
    for ( i in 1 : 12 ) {
        AGR_temp_array[ , , i ] <- flip_a_matrix( t( AGR_var[ , , i ] ) )
        ENE_temp_array[ , , i ] <- flip_a_matrix( t( ENE_var[ , , i ] ) )
        IND_temp_array[ , , i ] <- flip_a_matrix( t( IND_var[ , , i ] ) )
        TRA_temp_array[ , , i ] <- flip_a_matrix( t( TRA_var[ , , i ] ) )
        SLV_temp_array[ , , i ] <- flip_a_matrix( t( SLV_var[ , , i ] ) )
        WST_temp_array[ , , i ] <- flip_a_matrix( t( WST_var[ , , i ] ) )
        RCO_temp_array[ , , i ] <- flip_a_matrix( t( RCO_var[ , , i ] ) )

        AGR_temp_array[ , , i ] <- AGR_temp_array[ , , i ] * gridcell_area * ( days_in_month[ i ] * 24 * 60 * 60 ) * 0.000001
        ENE_temp_array[ , , i ] <- ENE_temp_array[ , , i ] * gridcell_area * ( days_in_month[ i ] * 24 * 60 * 60 ) * 0.000001
        IND_temp_array[ , , i ] <- IND_temp_array[ , , i ] * gridcell_area * ( days_in_month[ i ] * 24 * 60 * 60 ) * 0.000001
        TRA_temp_array[ , , i ] <- TRA_temp_array[ , , i ] * gridcell_area * ( days_in_month[ i ] * 24 * 60 * 60 ) * 0.000001
        SLV_temp_array[ , , i ] <- SLV_temp_array[ , , i ] * gridcell_area * ( days_in_month[ i ] * 24 * 60 * 60 ) * 0.000001
        WST_temp_array[ , , i ] <- WST_temp_array[ , , i ] * gridcell_area * ( days_in_month[ i ] * 24 * 60 * 60 ) * 0.000001
        RCO_temp_array[ , , i ] <- RCO_temp_array[ , , i ] * gridcell_area * ( days_in_month[ i ] * 24 * 60 * 60 ) * 0.000001

        AGR_array[ i ] <- sum(  AGR_temp_array[ , , i ] )
        ENE_array[ i ] <- sum(  ENE_temp_array[ , , i ] )
        IND_array[ i ] <- sum(  IND_temp_array[ , , i ] )
        TRA_array[ i ] <- sum(  TRA_temp_array[ , , i ] )
        SLV_array[ i ] <- sum(  SLV_temp_array[ , , i ] )
        WST_array[ i ] <- sum(  WST_temp_array[ , , i ] )
        RCO_array[ i ] <- sum(  RCO_temp_array[ , , i ] )
    }

        out_df <- data.frame( em = em,
                              year = year,
                              month = 1 : 12,
                              AGR = AGR_array,
                              ENE = ENE_array,
                              IND = IND_array,
                              TRA = TRA_array,
                              SLV = SLV_array,
                              WST = WST_array,
                              RCO = RCO_array,
                              stringsAsFactors = F )

} )

global_emission_total <- do.call( 'rbind', global_monthly_sectors )
global_emission_total <- gather( global_emission_total, sector, value, -em, -year, -month )

sector_list <- sort( unique( global_emission_total$sector ) )

time_line <- seq(from = as.POSIXct(paste0(min(global_emission_total$year), '-01-01')), to = as.POSIXct(paste0(max(global_emission_total$year), '-12-31')), by = "months")

em_sector_plot_list <- lapply( sector_list, function ( sector ) {
    plotting_df <- global_emission_total[ global_emission_total$em == em & global_emission_total$sector == sector, ]
    plotting_df$yearmonth <- paste0( plotting_df$year, plotting_df$month )
    plotting_df$time_line <- time_line

    plot <- ggplot( plotting_df, aes( x = time_line, y = value ) ) +
        geom_line( size = 0.2 ) +
        ggtitle( paste0( em, ' ', sector ) ) +
        labs( y = 'Emissions (kt)' ) +
        labs( x = 'Date' ) +
        theme(panel.background=element_blank(),
              panel.grid.minor = element_line(colour="gray95"),
              panel.grid.major = element_line(colour="gray88"),
              panel.border = element_rect(colour = "grey80", fill=NA, size=.8))
} )

# Now plot total emissions for that cell
plotting_df <- global_emission_total[ global_emission_total$em == em, ]
plotting_df$time_line <- time_line
agr_plotting_df <- plotting_df %>%
    dplyr::select(value, time_line) %>%
    dplyr::group_by(time_line) %>%
    dplyr::summarize(sum(value)) %>%
    dplyr::rename("total" = "sum(value)")

write.csv(agr_plotting_df, file = paste0( '../diagnostic-output/gridding-diagnostic-plots/seasonal-line-plots/global_', em, '_by_sector.csv' ), row.names = FALSE)

ag_plot <- ggplot( agr_plotting_df, aes( x = time_line, y = total ) ) +
    geom_line( size = 0.2 ) +
    ggtitle( paste0( em, ' ', 'Total' ) ) +
    labs( y = 'Emissions (kt)' ) +
    labs( x = 'Date' ) +
    theme(panel.background=element_blank(),
          panel.grid.minor = element_line(colour="gray95"),
          panel.grid.major = element_line(colour="gray88"),
          panel.border = element_rect(colour = "grey80", fill=NA, size=.8))

pdf( paste0( '../diagnostic-output/gridding-diagnostic-plots/seasonal-line-plots/global_', em, '_by_sector.pdf' ), width = 10, height = 20, paper= 'special' )
grid.arrange( ag_plot, em_sector_plot_list[[ 1 ]],
              em_sector_plot_list[[ 2 ]], em_sector_plot_list[[ 3 ]],
              em_sector_plot_list[[ 4 ]], em_sector_plot_list[[ 5 ]],
              em_sector_plot_list[[ 6 ]], em_sector_plot_list[[ 7 ]],
              ncol=1 )
dev.off( )


