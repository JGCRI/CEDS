# Program Name: Compare_Previous_Grids.R
# Author: Noah Prime
# Date Last Updated: 18 September, 2023
# Program Purpose: Creates plots (maybe .nc file?) of difference grids between
#                  the current and previous CEDS gridding run.
#
# Input Files:
#   - NetCDF files in final-emissions/diagnostics/grids-to-compare
#   - NetCDF files from the current run
# Output Files:
#   - Difference plots
#   - NetCDF difference data ?



# 0. Read in global settings and headers ---------------------------------
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "nc_generation_functions.R") # Additional function files may be required.
log_msg <- "Comparing gridded output with the previous version" # First message to be printed to the log
script_name <- "Compare_Previous_Grids.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# library(raster)
# library(grid)



# 1. Define Paths ---------------------------------------------------

# Grid data paths
previous_versions   <- filePath( "FIN_OUT",  'diagnostics/grids-to-compare/', extension = "" )
by_sector_grid_dir  <- filePath( "MED_OUT",  "gridded-emissions/", extension = "" )
total_grid_dir      <- filePath( "DIAG_OUT", "total-emissions-grids/", extension = "" )

# Shape File Input Directory
shape_path <- filePath( "GRIDDING", "diagnostics/shape-file-data/", extension = "" )

# Output plot path
out_path <- filePath( "FIN_OUT",  'diagnostics/', extension = "" )

# Checksums path
checksums_dir <- filePath( "DIAG_OUT", "", extension = "" )



# 2. Constants -----------------------------------------------------

# Aggregation resolution
agg_resolution <- 5

# Sector definitions
sector_numbers <- 1:7
sector_list_longname <- c('Agriculture', 'Energy Sector', 'Industrial Sector',
                          'Transportation Sector', 'Residential, Commercial, Other',
                          'Solvents production and application', 'Waste', 'International Shipping')
sector_list_shortname <- c('AGR', 'ENE', 'IND',
                           'TRA', 'RCO',
                           'SLV', 'WST', 'SHP')

# Shape file Name
shp.file <- 'ne_110m_coastline.shp'

# Read in shape file
shp.data <- terra::vect(paste0(shape_path, shp.file))

# 3. Define functions for generating output based on input file ------------------

# Plotting function
plot_difference_grid_sector <- function(sector, em, year, filename, grid_resolution){

    # Get Data associated with given sector
    previous_data <- terra::rast( paste0(previous_versions, filename) )
    current_data <- terra::rast( paste0(by_sector_grid_dir, filename) )

    # Aggregate grid to set resolution
    agg_fact <- agg_resolution / grid_resolution
    previous_data <- terra::aggregate( previous_data, fact = agg_fact, fun = sum )
    current_data <- terra::aggregate( current_data, fact = agg_fact, fun = sum )

    # Get data for given sector and January
    month <- 1
    sector_month <- paste0(sector, '_', month)

    # Find the difference grid (previous - current)
    em_diff <- previous_data[[sector_month]] - current_data[[sector_month]]

    # Min/Max diff values
    min_max_diff <- terra::minmax(em_diff)
    bound <- max(abs(min_max_diff[1]), abs(min_max_diff[2]))
    label_seq <- seq(-bound, bound, bound/2)

    # Plot
    if(bound == 0){
        p <- terra::plot(
            em_diff,
            legend = FALSE,
            col = 'white',
            fun = terra::lines(shp.data),
            main = paste0('Difference in ', sector, ' ', em, ' Emission Flux for January ', year)
        )
    }else{
        p <- terra::plot(
            em_diff,
            range = c(-bound, bound),
            col = colorspace::diverge_hcl(n=100,'Blue-Red 3'),
            # mar = c(4,4,4,4),
            plg = list(
                ext = c(-140, 140, -130, -120),
                loc = 'bottom',
                at = label_seq,
                labels = formatC(label_seq, format = 'e', digits = 2)
            ),
            fun = terra::lines(shp.data),
            main = paste0('Difference in ', sector, ' ', em, ' Emission Flux for January ', year)
        )
    }

    return(p)
}

# Managing function for generating plots for data broken up by sector
generate_diff_plot_sectors <- function(filename){

    # Get info from filename
    filename_vector <- str_split(filename, '_')[[1]]
    em <- filename_vector[2]
    year <- filename_vector[4]
    grid_resolution <- as.numeric(gsub('.nc', '', filename_vector[5]))

    # Plot for each sector
    p <- recordPlot()
    par(mfrow=c(3,3))
    plot_list <- lapply(sector_list_shortname, plot_difference_grid_sector, em = em, year = year, filename = filename, grid_resolution = grid_resolution)
    par(mfrow=c(1,1))

    for( i in 1:length(plot_list) ){
        plot_list[i]
    }

    return(p)

}

get_date_from_total_file <- function(total_filename){
    # Get info from filename
    filename_vector <- str_split(total_filename, '_')[[1]]

    # Year string
    year_string <- paste(filename_vector[8], filename_vector[9], filename_vector[10], sep='-')
    # Remove the .nc extension
    year_string <- gsub('.nc', '', year_string)

    # Convert to date and return
    return( as.Date(year_string, format="%Y-%m-%d") )
}

# Managing function for generating plots for data broken up by sector
generate_diff_plot_total <- function(filename){

    # Get info from filename
    filename_vector <- str_split(filename, '_')[[1]]
    em <- filename_vector[2]
    year <- filename_vector[4]
    grid_resolution <- as.numeric(filename_vector[6])

    # Pattern of the file type we want
    filename_pattern <- paste(filename_vector[1], filename_vector[2], filename_vector[3],
                              filename_vector[4], filename_vector[5], filename_vector[6], sep='_')

    # Get most recent total file that matches
    # List of all files of the right form
    possible_files <- list.files(total_grid_dir, pattern=glob2rx(paste0(filename_pattern, '*.nc')))
    if(length(possible_files) == 0) throw('File not found')

    # Get creation dates of each file
    dates <- bind_rows(lapply(possible_files, function(x) data.frame(date = get_date_from_total_file(x))))
    # Get most recent date
    most_recent_date <- dates %>%
        dplyr::arrange(desc(date)) %>%
        dplyr::top_n(1, date) %>%
        dplyr::pull()

    # Format date
    most_recent_date <- gsub('-', '_', most_recent_date)

    # Get full filename
    final_filename <- paste(filename_pattern, 'v', paste0(most_recent_date, '.nc'), sep='_')

    # Get Data associated with given sector
    previous_data <- terra::rast( paste0(previous_versions, filename) )
    current_data <- terra::rast( paste0(total_grid_dir, final_filename) )

    # Aggregate grid to set resolution
    agg_fact <- agg_resolution / grid_resolution
    previous_data <- terra::aggregate( previous_data, fact = agg_fact, fun = sum )
    current_data <- terra::aggregate( current_data, fact = agg_fact, fun = sum )

    # Find the difference grid (previous - current)
    em_diff <- previous_data - current_data

    # Min/Max diff values
    min_max_diff <- terra::minmax(em_diff)
    bound <- max(abs(min_max_diff[1]), abs(min_max_diff[2]))
    label_seq <- seq(-bound, bound, bound/2)

    # Plot
    p <- recordPlot()
    if(bound == 0){
        terra::plot(
            em_diff,
            legend = FALSE,
            col = 'white',
            fun = terra::lines(shp.data),
            main = paste0('Difference in Total ', em, ' Emission Flux for ', year)
        )
    }else{
        terra::plot(
            em_diff,
            range = c(-bound, bound),
            col = colorspace::diverge_hcl(n=100,'Blue-Red 3'),
            # mar = c(4,4,4,4),
            plg = list(
                ext = c(-140, 140, -130, -120),
                loc = 'bottom',
                at = label_seq,
                labels = formatC(label_seq, format = 'e', digits = 2)
            ),
            fun = terra::lines(shp.data),
            main = paste0('Difference in Total ', em, ' Emission Flux for ', year)
        )
    }


    return(p)

}

# Versions of the functions which automatically catch errors, and will return NA if caught
# Execution will continue, so that any grids requested that do not error will be drawn
generate_diff_plot_total_e <- purrr::possibly( generate_diff_plot_total, otherwise = NA )
generate_diff_plot_sectors_e <- purrr::possibly( generate_diff_plot_sectors, otherwise = NA )

# Managing function for generating plots
generate_diff_plot <- function(filename){
    # Is the file for total emissions
    total_file <- grepl('TOTAL', filename)

    if(total_file){
        plot_grid <- generate_diff_plot_total_e(filename)
    }else{
        plot_grid <- generate_diff_plot_sectors_e(filename)
    }

    return(plot_grid)
}


# 4. Generate output file ---------------------------------------------------

# Open connection to output pdf
pdf( paste0( out_path, "Gridding_Comparison_Plots.pdf"),
     width = 18, height = 10, paper ='special' )
grid_list <- lapply( list.files(previous_versions, pattern = '*.nc'), function(x) generate_diff_plot(x) )
dev.off()

# 5. Creating Checksums summary ---------------------------------------------

# 5.1. Files ------------------------------------------------------------
per_01_pattern <- '_checksum_comparison_per_01.csv'
per_01_files <- list.files(path = checksums_dir, pattern = per_01_pattern)

diff_01_pattern <- '_checksum_comparison_diff_01.csv'
diff_01_files <- list.files(path = checksums_dir, pattern = diff_01_pattern)

per_05_pattern <- '_checksum_comparison_per.csv'
per_05_files <- list.files(path = checksums_dir, pattern = per_05_pattern)

diff_05_pattern <- '_checksum_comparison_diff.csv'
diff_05_files <- list.files(path = checksums_dir, pattern = diff_05_pattern)


# 5.2. Function Definition ---------------------------------------------

# Function to generate summary of a given species and resolution
generate_summary <- function(per_file, diff_file){
    # Get the emission species
    species <- gsub( '_.*', '', gsub('G.', '', per_file))

    # Read in data
    per_df <- read.csv(file.path(checksums_dir, per_file))
    diff_df <- read.csv(file.path(checksums_dir, diff_file))

    # Get year range
    first_year <- gsub('X', '', colnames(per_df)[2])
    last_year <- gsub('X', '', colnames(per_df)[length(colnames(per_df))])

    # Get long formats
    per_df_long <- per_df %>%
        tidyr::gather(year, emissions, paste0('X', first_year:last_year)) %>%
        dplyr::mutate( year = gsub('X', '', year) )

    diff_df_long <- diff_df %>%
        tidyr::gather(year, emissions, paste0('X', first_year:last_year)) %>%
        dplyr::mutate( year = gsub('X', '', year) )

    # Get max % difference by sector
    max_pct_diff_df <- per_df_long %>%
        na.omit() %>%
        dplyr::group_by(sector) %>%
        dplyr::summarise( max_pct = max(emissions) ) %>%
        dplyr::ungroup()

    # Cumulative difference
    cumulative_diff_df <- diff_df_long %>%
        na.omit() %>%
        dplyr::group_by(sector) %>%
        dplyr::summarise( cumulative_diff = sum(emissions) ) %>%
        dplyr::ungroup()

    # Join summary dataframes
    summary_df <- max_pct_diff_df %>%
        dplyr::full_join(cumulative_diff_df, by = 'sector') %>%
        replace(is.na(.), 0)

    # Year range for reference (don't have to grid all years)
    year_range_string <- paste0(first_year, "-", last_year)

    # Add species and year range columns
    summary_df['species'] <- species
    summary_df['years'] <- year_range_string

    # Order columns
    summary_df <- summary_df %>%
        dplyr::select(species, sector, years, max_pct, cumulative_diff )

    # Return dataframe
    return( summary_df )

}


# 5.3 Getting output ---------------------------------------------------

summary_05 <- bind_rows(mapply(generate_summary, per_05_files, diff_05_files, SIMPLIFY = FALSE))
summary_01 <- bind_rows(mapply(generate_summary, per_01_files, diff_01_files, SIMPLIFY = FALSE))

# 5.4 Writing output ---------------------------------------------------

writeData(summary_05, "DIAG_OUT", 'G.checksum_summary_05.csv')
writeData(summary_01, "DIAG_OUT", 'G.checksum_summary_01.csv')

# END ---------------------------------------------------

logStop()
