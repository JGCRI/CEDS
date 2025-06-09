# Header -----------------------------------------------------------------------
# Program Name: G0.0.create_seasonality_mapping.R
# Authors: Noah Prime
# Date Last Updated: October 17, 2024
# Program Purpose: Generate a mapping file from iso/CEDS sector/year to seasonality profile
# Input Files:
# Output Files:
# ------------------------------------------------------------------------------


# 0.) Read in global settings and headers --------------------------------------

# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Read in universal header files, support scripts, and start logging
headers <- c( )
log_msg <- "Generating seasaonlity mapping file"
source( paste0( PARAM_DIR, "header.R" ) )
initialize( "G0.0.create_seasonality_mapping.R", log_msg, headers )


# 0.5.) Set-up details for script ----------------------------------------------

# Define emissions species and gridding resolution
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
res <- as.numeric( args_from_makefile[ 2 ] )
if ( is.na( em ) ) em <- "SO2"
if ( is.na( res ) ) res <- 0.5 # default gridding resolution of 0.5

# 1.0) Set Paths ---------------------------------------------------------------

final_emissions_dir <- filePath( 'FIN_OUT',  'current-versions',       extension = '' )
gridding_mappings <- filePath( 'GRIDDING', fn = 'gridding_mappings/', extension = '' )
EDGAR_seasonality_profiles_path <- filePath( 'GRIDDING', domain_extension = 'seasonality/', 'EDGAR_temporal_profiles_r1', extension = '.xlsx')
intermediate_output_dir <- filePath( 'MED_OUT', '', '')

# 2.0) Read Data ---------------------------------------------------------------

# Reading in CEDS final emissions
pattern <- paste0( ".*_", em, '_emissions_by_country_CEDS_sector.*' )
target_filename <- list.files( final_emissions_dir, pattern )
ceds_final_emissions <- readData( "FIN_OUT", domain_extension = "current-versions/", target_filename, meta = FALSE )

# Gridding sector mapping files
ceds_gridding_mapping <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', 'CEDS_sector_to_gridding_sector_mapping', meta = FALSE )
expanded_sectors_map <- readData( domain = 'MAPPINGS', file_name = 'old_to_new_sectors', extension = '.csv', meta = FALSE )
edgar_seasonality_sector_mapping <- readData( domain = 'MAPPINGS', file_name = 'EDGAR_seasonality_sector_mapping', extension = '.csv', meta = FALSE )

# Seasonality data
carbon_monitoring_data_2023_08_24 <- readData( 'GRIDDING', domain_extension = 'seasonality/', 'carbonmonitor-global_datas_2023-08-24', meta = FALSE)
carbon_monitoring_data_2025_03_31 <- readData( 'GRIDDING', domain_extension = 'seasonality/', 'carbonmonitor-global_datas_2025-03-31', meta = FALSE)
edgar_temporal_profiles <- readxl::read_xlsx(EDGAR_seasonality_profiles_path, sheet = 2, skip = 1)
edgar_region_mapping <- readxl::read_xlsx(EDGAR_seasonality_profiles_path, sheet = 3, skip = 0)

# Replace 2019 data from new carbon monitor with older data to keep it the same
carbon_monitoring_data_2023_08_24$country <- dplyr::recode(carbon_monitoring_data_2023_08_24$country,
                                                "UK" = "United Kingdom",
                                                "US" = "United States")

# Keep only the needed columns
carbon_monitoring_base <- carbon_monitoring_data_2023_08_24[, c("country", "date", "sector", "value")]
carbon_monitoring_extended <- carbon_monitoring_data_2025_03_31[, c("country", "date", "sector", "value")]

# Add a "year" column for filtering (using the last 4 characters of the date)
carbon_monitoring_base$year <- substr(carbon_monitoring_base$date, nchar(carbon_monitoring_base$date) - 3, nchar(carbon_monitoring_base$date))
carbon_monitoring_extended$year <- substr(carbon_monitoring_extended$date, nchar(carbon_monitoring_extended$date) - 3, nchar(carbon_monitoring_extended$date))

# Subset base data for years 2019–2021
df_base_subset <- dplyr::filter(carbon_monitoring_base, year %in% c("2019", "2020", "2021"))

# Drop unwanted columns and remove matching years from extended
df_base_subset <- dplyr::select(df_base_subset, country, date, sector, value)
df_extended_clean <- dplyr::filter(carbon_monitoring_extended, !(year %in% c("2019", "2020", "2021"))) %>%
    dplyr::select(country, date, sector, value)

# Combine and sort
carbon_monitoring_data <- dplyr::bind_rows(df_extended_clean, df_base_subset)
carbon_monitoring_data <- dplyr::arrange(carbon_monitoring_data, country, date, sector)

# Master Country list
master_country_list <- readData( 'MAPPINGS', file_name = 'Master_Country_List')

# Manual Profiles Mapping File
manual_mapping <- readData( 'GRIDDING', domain_extension = 'gridding_mappings/', file_name = 'manual_seasonality_profile_mapping', extension = '.csv', meta = FALSE)

# 3.0) Process Data ------------------------------------------------------------

# Get mapping for edgar seasonality sectors (made global so accesible to gridding functions)
names(edgar_seasonality_sector_mapping) <- c("ipcc-1996 Activity sector description", "ipcc-1996", "CEDS-Working-Sector-Name")

# Extract each iso/sector/year combo
x_year_columns <- paste0('X', 1980:end_year)
ceds_combos <- ceds_final_emissions %>%
    dplyr::select( iso, sector, all_of(x_year_columns) ) %>%
    tidyr::gather( year, value, all_of(x_year_columns) ) %>%
    dplyr::select( iso, sector, year ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(year = as.numeric(gsub('X', '', year)))

# Combine gridding mapping and keep relevant columns
ceds_gridding_mapping <- ceds_gridding_mapping %>%
    dplyr::left_join( expanded_sectors_map, by = c('CEDS_working_sector' = 'ceds_sector') ) %>%
    dplyr::mutate( CEDS_working_sector = if_else(is.na(new_sector), CEDS_working_sector, as.character(new_sector)) ) %>%
    dplyr::select( CEDS_working_sector, CEDS_int_gridding_sector_short, CEDS_final_gridding_sector_short )

# Turn Carbon Monitoring into monthly fractions
carbon_monitoring_fracs <- carbon_monitoring_data %>%
    dplyr::mutate(year = as.numeric( substr(date, 7, 10) ), month = as.numeric( substr(date, 4, 5 ) ) ) %>%
    dplyr::group_by( country, sector, year, month ) %>%
    dplyr::mutate( monthly_value = sum(value) ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by( country, sector, year ) %>%
    dplyr::mutate( annual_value = sum(value) ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate( frac = monthly_value / annual_value ) %>%
    dplyr::distinct( country, sector, year, month, frac ) %>%
    dplyr::mutate( country = as.character(country) )

# Add a check if all years (up to and including end_year) have 12 months of data
incomplete_years <- carbon_monitoring_fracs %>%
    dplyr::filter(year <= end_year) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(n_months = dplyr::n_distinct(month), .groups = "drop") %>%
    dplyr::filter(n_months < 12) %>%
    dplyr::pull(year)

if (length(incomplete_years) > 0) {
    stop("The carbon monitoring data on file is missing data. ",
         "Please update the file to the latest version.\n",
         "The following years have missing months: ",
         paste(incomplete_years, collapse = ", "))
}

# Map Carbon Monitoring Data to CEDS iso
country_name <- as.character( c('Brazil', 'China', 'France', 'Germany', 'India', 'Italy', 'Japan', 'Russia', 'Spain', 'United Kingdom', 'United States') )
isos <-          c('bra',    'chn',  'fra',    'deu',     'ind',   'ita',   'jpn',   'rus',    'esp',   'gbr','usa')
cm_iso_map <- data.frame(country = country_name, iso = isos)
carbon_monitoring_fracs <- carbon_monitoring_fracs %>%
    left_join( cm_iso_map %>% dplyr::mutate(country = as.character(country), iso = as.character(iso)), by = 'country' ) %>%
    dplyr::mutate( iso = as.character( ifelse(is.na(iso), country, iso) ) )

master_country_list <- master_country_list %>%
    dplyr::select(iso, Paper_Figure_Region) %>%
    stats::na.omit() %>%
    dplyr::distinct()

carbon_monitoring_fracs_iso <- ceds_final_emissions %>%
    dplyr::select( iso ) %>%
    distinct() %>%
    dplyr::left_join( master_country_list, by = 'iso' ) %>%
    dplyr::mutate( cm_iso = ifelse(iso %in% isos, iso, ifelse(Paper_Figure_Region == 'Europe', 'EU27 & UK', 'ROW') ) ) %>%
    dplyr::left_join( carbon_monitoring_fracs, by = c('cm_iso' = 'iso') ) %>%
    dplyr::select(iso, sector, year, month, frac) %>%
    dplyr::filter(sector %in% c('Power', 'Ground Transport', 'International Aviation', 'Domestic Aviation'), year <= end_year )

# 4.0) Worker Functions --------------------------------------------------------

# Return the seasonality profile vector given the iso, CEDS working secor, and year
# from either EDGAR or Carbon Monitoring
regional_seasonality_application <- function( iso,
                                              working_sector,
                                              year,
                                              seasonality_datasource,
                                              outlier_years,
                                              replace_years,
                                              new_iso) {

    # Replace look-up iso if needed
    if(!is.na(new_iso)){
        iso <- new_iso
    }

    # If using both EDGAR and Carbon Monitoring, use Carbon Monitoring if year >= 2019
    if( seasonality_datasource == 'both' ){
        seasonality_datasource <- 'EDGAR'
        if(year >= 2019){
            seasonality_datasource <- 'Carbon-Monitoring'
        }
    }

    # Call relevant function for seasonality data source
    result = switch(
        seasonality_datasource,
        'EDGAR' = get_EDGAR_seasonality_vec(iso, working_sector, year, outlier_years, replace_years),
        'Carbon-Monitoring' = get_Carbon_Monitoring_seasonality_vec(iso, working_sector, year),
    )

    return(result)
}

# Return the EDGAR seasonality profile vector for given iso, CEDS working sector and year
get_EDGAR_seasonality_vec <- function( iso,
                                       working_sector,
                                       year,
                                       outlier_years,
                                       replace_years)  {

    # EDGAR months
    edgar_months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

    # Get outlier / replacement years
    outlier_years <- as.numeric(str_split(outlier_years, '-')[[1]])
    replace_years <- as.numeric(str_split(replace_years, '-')[[1]])

    # Change look-up year if needed
    # TODO: Potentially remap the years 2016 and 2017 for all EDGAR data since is a common outlier
    if(year %in% outlier_years){
        year <- replace_years[which(year == outlier_years)]
    }

    # default flat profile if cannot find iso/sector/year matching in EDGAR
    default_profile <- rep(1/12, 12)

    # Get the mapping from current sector to EDGAR temporal profile sector
    edgar_sector <- edgar_seasonality_sector_mapping %>%
        dplyr::filter(`CEDS-Working-Sector-Name` == working_sector)

    # Get the temporal profile for the sector
    sector_temporal_profiles <- edgar_sector %>%
        dplyr::left_join(
            edgar_temporal_profiles,
            by = c(
                'ipcc-1996 Activity sector description' = 'Activity sector description',
                'ipcc-1996' = 'IPCC_1996_source_category'
            )
        )

    # Get the temporal profile for the sector/iso
    sector_iso_profiles <- sector_temporal_profiles %>%
        dplyr::filter( `Region/country` == toupper(iso) )

    # If iso not present, look for iso's region
    if(nrow(sector_iso_profiles) == 0){
        iso_region <- edgar_region_mapping %>%
            dplyr::filter( `Country ISO codes` == toupper(iso) ) %>%
            dplyr::select( `World regions (as mapped in  Fig.1)` ) %>%
            dplyr::pull()

        # If region in EDGAR, use that instead
        if(length(iso_region) != 0){
            sector_iso_profiles <- sector_temporal_profiles %>%
                dplyr::filter( `Region/country` == iso_region )
        }
    }

    # If profile exists for given iso/sector, get it for specific year (or representative year)
    if( nrow(sector_iso_profiles) != 0 ){
        # Get the temporal profile for the sector/iso/year
        found_data <- TRUE

        # When there is a representative year
        if(all(sector_iso_profiles$Year == 0)){
            sector_iso_year_profile <- sector_iso_profiles %>%
                dplyr::filter( Year == 0 )
            # When year is before profile starts
        }else if( min(sector_iso_profiles$Year) > year ){
            sector_iso_year_profile <- sector_iso_profiles %>%
                dplyr::filter( Year == min(Year) )
            # When year is after last data point
        }else if( max(sector_iso_profiles$Year) < year ){
            sector_iso_year_profile <- sector_iso_profiles %>%
                dplyr::filter( Year == max(Year) )
            # When the year is present in the data
        }else if( year %in% sector_iso_profiles$Year ){
            sector_iso_year_profile <- sector_iso_profiles %>%
                dplyr::filter( Year == year )
        }else{
            # TODO: Maybe instead of making flat profile, can opt to revert back to using gridded seasonality
            profile_vector <- default_profile
            found_data <- FALSE
        }

        if(found_data){
            # Get seasonality profile vector
            profile_vector <- sector_iso_year_profile %>%
                dplyr::select(Jan:Dec) %>%
                tidyr::gather( month, value, all_of(edgar_months)) %>%
                dplyr::select(value) %>%
                dplyr::pull()
        }
    }else{
        # TODO: Maybe instead of making flat profile, can opt to revert back to using gridded seasonality
        profile_vector <- default_profile
    }

    return(profile_vector)
}

# Return the Carbon Monitoring seasonality profile vector for given iso, CEDS working sector and year
get_Carbon_Monitoring_seasonality_vec <- function( iso,
                                                   working_sector,
                                                   year ) {

    # Default flat profile
    seasonality_profile <- rep(1/12,12)

    # Replace year with earliest Carbon Monitoring year if needed
    if(year < 2019){
        year <- 2019
    }

    # Get Carbon Monitoring Data for ground transport, power, and aviation
    if( working_sector == '1A3b_Road' ){
        seasonality_profile <- carbon_monitoring_fracs_iso %>%
            dplyr::filter(iso == !!iso, sector == 'Ground Transport', year == !!year) %>%
            dplyr::arrange( month ) %>%
            dplyr::select( frac ) %>%
            dplyr::pull()
    }else if(grepl('1A1', working_sector)){
        seasonality_profile <- carbon_monitoring_fracs_iso %>%
            dplyr::filter(iso == !!iso, sector == 'Power', year == !!year) %>%
            dplyr::arrange( month ) %>%
            dplyr::select( frac ) %>%
            dplyr::pull()
    }else if(working_sector == '1A3ai_International-aviation'){
        seasonality_profile <- carbon_monitoring_fracs_iso %>%
            dplyr::filter(iso == !!iso, sector == 'Domestic Aviation', year == !!year) %>%
            dplyr::arrange( month ) %>%
            dplyr::select( frac ) %>%
            dplyr::pull()
    }else if(working_sector == '1A3aii_Domestic-aviation'){
        seasonality_profile <- carbon_monitoring_fracs_iso %>%
            dplyr::filter(iso == !!iso, sector == 'International Aviation', year == !!year) %>%
            dplyr::arrange( month ) %>%
            dplyr::select( frac ) %>%
            dplyr::pull()
    }

    # Edge case that shouldn't happen, no profile match in carbon monitoring
    # for supplied iso, sector, year
    if(length(seasonality_profile) == 0){
        seasonality_profile <- rep(1/12,12)
    }

    return(seasonality_profile)
}

# 4.0) Default Mapping ---------------------------------------------------------

# Default mapping is to use gridded profiles except for transport and energy
# where we will use EDGAR before 2019, and Carbon Monitoring beyond. We map
# international and domestic aviation to carbon monitoring for all years (copy
# 2019 values for previous years).

# Map each iso, sector, year to data source
data_source_mapping <- ceds_combos %>%
    dplyr::left_join( ceds_gridding_mapping, by = c('sector' = 'CEDS_working_sector') ) %>%
    dplyr::mutate( data_source = ifelse(
        CEDS_final_gridding_sector_short %in% c('AGR', 'SHP', 'TANK', 'IND', 'RCO'), 'gridded',
        ifelse( (year >= 2019) & ((sector == '1A3b_Road') | (CEDS_final_gridding_sector_short == 'ENE')), 'Carbon-Monitoring', 'EDGAR' )
    )
    ) %>%
    dplyr::mutate(data_source = ifelse(CEDS_final_gridding_sector_short == 'AIR', 'Carbon-Monitoring', data_source))

# Keep only non-gridded combinations for this mapping
data_source_mapping <- data_source_mapping %>%
    dplyr::filter( data_source != 'gridded' )

# # For testing, just keep n rows for time saving (save full data frame to try different rows)
# temp <- data_source_mapping
# i <- 400
# nrows <- 500
# data_source_mapping <- temp[i:(i+nrows),]
# unique(data_source_mapping$iso)

data_source_mapping <- data_source_mapping %>%
    dplyr::left_join(manual_mapping, by = c('iso' = 'iso', 'CEDS_final_gridding_sector_short' = 'sector')) %>%
    dplyr::mutate( data_source = ifelse(is.na(new_data_source), data_source, new_data_source ) )

# Get seasonality profile for each row that is a unique iso/sector/year combination
# Output is transposed (months are rows, entries are columns)
printLog('Starting to map seasonality profiles')
start_time <- Sys.time()
result <- mapply(regional_seasonality_application,
                 data_source_mapping$iso,
                 data_source_mapping$sector,
                 data_source_mapping$year,
                 data_source_mapping$data_source,
                 data_source_mapping$outlier_years,
                 data_source_mapping$replace_years,
                 data_source_mapping$new_iso)
end_time <- Sys.time()
printLog('Done mapping seasonality profiles')
printLog(end_time - start_time)

# Month Columns
month_columns <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
default_seasonality_mapping <- data_source_mapping
default_seasonality_mapping[month_columns] <- t(result)

# 5.0) User Updated Mapping ---------------------------------------------------------

# TODO: Read in custom edits to default mapping

# 6.0) Write Out ------------------------------------------------------------------

# Stack extension with recent years
seasonality_mapping <- default_seasonality_mapping %>%
    dplyr::select( iso, sector, year, CEDS_int_gridding_sector_short, CEDS_final_gridding_sector_short, data_source, all_of(month_columns)) %>%
    dplyr::arrange( iso, sector, year )

# Check for negative seasonality and print error message
seasonality_mapping_negatives <- seasonality_mapping %>%
    dplyr::filter_at(month_columns, any_vars(. < 0))

if(nrow(seasonality_mapping_negatives) > 0){
    print(paste0("Negative value(s) found in seasonality mapping file for ", em, " as shown in table below:"))
    print(seasonality_mapping_negatives)
    stop("Negative value(s) found in seasonality mapping file.")
}

writeData(seasonality_mapping, fn = paste0(em, '_seasonality_mapping'), domain = 'MED_OUT')
