# ------------------------------------------------------------------------------
# Program Name: diag_cell_plots.R
# Author(s): Leyang Feng, Hamza Ahsan
# Date Last Updated: April 26, 2021
# Program Purpose: Generates gridding diagnostic line plots for specified cells.
# Input Files:
# Output Files: [em]_[iso]_[cells].pdf, [em]_total_[iso]_[cells].jpeg
# Notes:
# TODO: Change input from cell_id (row/column of cell array matrix) to lat/lon to
# improve robustness for higher resolutions. Add resolution as a new variable.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
source( '../code/diagnostic/diag_cell_functions.R' )
source( '../code/diagnostic/diag_utility_functions.R' )
source( '../code/parameters/gridding_functions.R' )

# ------------------------------------------------------------------------------
# 1. set dirs
input_path <- '../final-emissions/gridded-emissions'
output_path <- './int-out'

# -----------------------------------------------------------------------------
# 2. set species
args_from_makefile <- commandArgs(TRUE)
em <- args_from_makefile[1]
if (is.na(em)) em <- "NOx"

# ------------------------------------------------------------------------------
# 3. read in selected cell mapping
cell_groups <- read.csv( './gridding/diagnostics/selected_cells.csv', stringsAsFactors = F )

# calculate grid cell area (specify grid resolution)
gridcell_area <- grid_area( 0.5, all_lon = T )
gridarea <- flip_a_matrix(t(gridcell_area))
gridarea_df <- data.frame(cell_id = 1:(nrow(gridcell_area)*ncol(gridcell_area)),
                          area_m2 = c(gridcell_area),
                          stringsAsFactors = F)

# ------------------------------------------------------------------------------
# 4. read in gridded data and generate a dataframe with emissions corresponding to
# the cell_id for each sector
# 4.1 define number of days per month
ndays_df <- data.frame(month = 1:12,
                       ndays = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
                       stringsAsFactors = F)

# 4.2 read the single year gridded files
ncs <- list.files( input_path, pattern = paste0(em, '-em-anthro') )
ncs <- grep( '.nc', ncs, value = T, fixed = T )

# select years 1950 onward (earlier years are not very informative)
start_year <- 1950
extract_years <- sapply(strsplit(ncs, "[_-]+"),function(x) x[12])
ncs <- ncs[which(extract_years >= as.numeric(paste0(start_year, '01')))]

if (!is.null(ncs)){

xy_list <- lapply(cell_groups$cell_id, convertCellIndex_122, 0.5)
xy_mat <- do.call(rbind, xy_list)
xy_mat <- cbind(xy_mat, cell_groups$cell_id)

temp_array_list <- list()
for ( i in 1:length(ncs) ) {
  temp_nc <- nc_open( paste0( input_path, '/', ncs[i] ) )
  temp_var <- ncvar_get( temp_nc, varid = paste0(em, '_em_anthro') )
  nc_close( temp_nc )

  res_list_p1 <- lapply(1:dim(temp_var)[4], function(t) {
    #print(t)
    res_list <- lapply(1:dim(temp_var)[3], function(s) {
      #print(s)
      temp_slice <- temp_var[, , s, t]
      temp_slice <- flip_a_matrix(t(temp_slice))
      temp_res_list <- lapply(1:nrow(xy_mat), function(j) {
        xy_pair <- xy_mat[j, ]
        temp_value <- temp_slice[xy_pair[1], xy_pair[2]]
        year <- as.numeric(substr(strsplit(ncs[i], "[_-]+")[[1]][12], 1, 4)) + floor((t-1)/12)
        month <- t %% 12
        month <- ifelse(month == 0, 12, month)
        out_df <- data.frame(sector = s,
                             year = year,
                             month = month,
                             cell_id = xy_pair[3],
                             value = temp_value, unit = 'kgm-2s-1',
                             stringsAsFactors = F)
        return(out_df)
      })
      temp_res <- do.call(rbind, temp_res_list)
      return(temp_res)
    })
    res <- do.call(rbind, res_list)
    return(res)
  })
  res_p1 <- do.call(rbind, res_list_p1)
  temp_array_list[[i]] <- res_p1
}

res <- do.call("rbind", temp_array_list)
rm(temp_array_list)

# extract the sector list
temp_nc <- nc_open( paste0( input_path, '/', ncs[1] ) )
sector_list <- ncatt_get(temp_nc, 'sector', attname = 'ids')
sector_list <- sector_list$value
sector_list <- gsub(': ', '', sector_list)
sector_list <- unlist(strsplit(sector_list, '; '))
sector_list <- gsub('\\d', '', sector_list)
nc_close(temp_nc)

sector_df <- data.frame(sector_name = sector_list,
                        sector = 1:8,
                        stringsAsFactors = F)

# -----------------------------------------------------------------------------
# 5. convert flux to mass
long_df <- left_join(res, ndays_df, by = c('month'))
long_df <- left_join(long_df, gridarea_df, by = c('cell_id'))
long_df$seconds <- long_df$ndays * 24 * 60 * 60
long_df$value_kg <- long_df$value * long_df$area_m2 * long_df$seconds
long_df$value_mt <- long_df$value_kg / 1000000000
long_df <- left_join(long_df, sector_df, by = c('sector'))
long_df <- left_join(long_df, cell_groups, by = c('cell_id'))

long_trim <- long_df[, c('year', 'month', 'cell_id', 'value_mt', 'sector_name', 'location')]

# -----------------------------------------------------------------------------
# 6. plotting
# 6.1 plot aggregate cell emissions by sector
group_df <- aggregate(long_trim$value_mt, by = list(long_trim$year, long_trim$sector_name, long_trim$location), sum)
names(group_df) <- c('year', 'sector_name', 'location', 'value_mt')
sector_list <- sort( unique(group_df$sector_name) )
location <- sort(unique(group_df$location))
for (loc in location) {
  group_sector_plot_list <- lapply( sector_list, function ( sector ) {
    temp_df <- group_df[group_df$sector_name == sector & group_df$location == loc, ]
    plot <- ggplot(temp_df, aes( x = year, y = value_mt ) ) +
      geom_line( size = 0.2 ) +
      ggtitle(paste0(em, ' ', unique(temp_df$sector_name), ' sector, ', unique(temp_df$location),' ','area')) +
      labs( y = 'Emissions (Mt/yr)' ) +
      scale_y_continuous(limits = c(0, max(temp_df$value_mt))) +
      theme(panel.background=element_blank(),
            panel.grid.minor = element_line(colour="gray95"),
            panel.grid.major = element_line(colour="gray88"),
            panel.border = element_rect(colour = "grey80", fill=NA, size=.8))
  })
  pdf( paste0( '../diagnostic-output/gridding-diagnostic-plots/cell_group_by_sector/', em, '_', loc, '_', length(unique(long_trim$cell_id[which(long_trim$location == loc)])), 'cells', '.pdf' ), width = 10, height = 20, paper= 'special' )
  grid.arrange( group_sector_plot_list[[ 1 ]] , group_sector_plot_list[[ 2 ]],
                group_sector_plot_list[[ 3 ]] , group_sector_plot_list[[ 4 ]],
                group_sector_plot_list[[ 5 ]] , group_sector_plot_list[[ 6 ]],
                group_sector_plot_list[[ 7 ]] , group_sector_plot_list[[ 8 ]],
                ncol=1 )
  dev.off( )
}

# 6.2 plot aggregate cell total emissions
group_df <- aggregate(group_df$value_mt, by = list(group_df$year, group_df$location), sum)
names(group_df) <- c('year', 'location', 'value_mt')
for (loc in location) {
  temp_df <- group_df[group_df$location == loc, ]
  plot <- ggplot(temp_df, aes( x = year, y = value_mt ) ) +
    geom_line( size = 0.2 ) +
    ggtitle(paste0(em, ' ', 'total emissions,', ' ', unique(temp_df$location), ' area')) +
    labs( y = 'Emissions (Mt/yr)' ) +
    scale_y_continuous(limits = c(0, max(temp_df$value_mt))) +
    theme(panel.background=element_blank(),
          panel.grid.minor = element_line(colour="gray95"),
          panel.grid.major = element_line(colour="gray88"),
          panel.border = element_rect(colour = "grey80", fill=NA, size=.8))

  out_name <- paste0('../diagnostic-output/gridding-diagnostic-plots/cell_group_totals/', em, '_total_', unique(temp_df$location), '_', length(unique(long_trim$cell_id[which(long_trim$location == loc)])), 'cells', '.jpeg')
  ggsave( out_name,  width = 9.0, height = 2.25 )
}

# 6.3 plot single cell emissions by sector
cell_list <- c(66338, 75341, 213221, 134359, 104596, 129317, 185202, 153763)
single_df <- long_trim[long_trim$cell_id %in% cell_list, ]
# by sector
single_df <- aggregate(single_df$value_mt, by = list(single_df$year, single_df$sector_name, single_df$location), sum)
names(single_df) <- c('year', 'sector_name', 'location', 'value_mt')
location <- sort(unique(single_df$location))
sector_list <- sort( unique(single_df$sector_name) )
for (loc in location) {
  single_sector_plot_list <- lapply( sector_list, function ( sector ) {
    temp_df <- single_df[single_df$sector_name == sector & single_df$location == loc, ]
    plot <- ggplot(temp_df, aes( x = year, y = value_mt ) ) +
      geom_line( size = 0.2 ) +
      ggtitle(paste0(em, ' ', unique(temp_df$sector_name), ' sector, ', unique(temp_df$location), ', ', 'for one grid cell')) +
      labs( y = 'Emissions (Mt/yr)' ) +
      scale_y_continuous(limits = c(0, max(temp_df$value_mt))) +
      theme(panel.background=element_blank(),
            panel.grid.minor = element_line(colour="gray95"),
            panel.grid.major = element_line(colour="gray88"),
            panel.border = element_rect(colour = "grey80", fill=NA, size=.8))
  })
  pdf( paste0( '../diagnostic-output/gridding-diagnostic-plots/single_cell_by_sector/', em, '_', loc, '_cell', '.pdf' ), width = 10, height = 20, paper= 'special' )
  grid.arrange( single_sector_plot_list[[ 1 ]] , single_sector_plot_list[[ 2 ]],
                single_sector_plot_list[[ 3 ]] , single_sector_plot_list[[ 4 ]],
                single_sector_plot_list[[ 5 ]] , single_sector_plot_list[[ 6 ]],
                single_sector_plot_list[[ 7 ]] , single_sector_plot_list[[ 8 ]],
                ncol=1 )
  dev.off( )
}

# 6.4 plot single cell total emissions
single_df <- aggregate(single_df$value_mt, by = list(single_df$year, single_df$location), sum)
names(single_df) <- c('year', 'location', 'value_mt')
for (loc in location) {
  temp_df <- single_df[single_df$location == loc, ]
  plot <- ggplot(temp_df, aes( x = year, y = value_mt ) ) +
    geom_line( size = 0.2 ) +
    ggtitle(paste0(em, ' ', 'total emissions', ', ', unique(temp_df$location), ', for one grid cell')) +
    labs( y = 'Emissions (Mt/yr)' ) +
    scale_y_continuous(limits = c(0, max(temp_df$value_mt))) +
    theme(panel.background=element_blank(),
          panel.grid.minor = element_line(colour="gray95"),
          panel.grid.major = element_line(colour="gray88"),
          panel.border = element_rect(colour = "grey80", fill=NA, size=.8))

  out_name <- paste0('../diagnostic-output/gridding-diagnostic-plots/single_cell_totals/', em, '_total_', unique(temp_df$location), '_cell', '.jpeg')
  ggsave( out_name,  width = 9.0, height = 2.25 )
}
}
