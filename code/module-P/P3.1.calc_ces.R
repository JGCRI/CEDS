# Header ---------------------------------------------------------------------
# Program Name: P3.1.calc_ces.R
# Author: Noah Prime
# Date Last Updated: October 20, 2022
# Program Purpose:  Calculation of co-emitted species for each point source
# Input Files:  YAML files for each unique point source containing all
#               available input data
# Output Files: YAML files for each unique point source containing time
#               series for each supported CEDS emission species

# 0. Read in global settings and headers ------------------------------------
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"
source( paste0( PARAM_DIR, "header.R" ) )

initialize(script_name = "P3.1.calc_ces.R",
           log_msg = "Consolidating duplicate sources",
           headers = c("data_functions.R", "gridding_functions.R",
                       "interpolation_extension_functions.R",
                       "co-emitted_species_calculation_functions.R",
                       "point_source_util_functions.R"),
           common_data = TRUE)


# 0.1 Set paths -------------------------------------------------------

yml_path <- filePath( domain = 'MED_OUT',
                      domain_extension = 'point_source_yml',
                      fn = '',
                      extension = '' )

out_path <- filePath( domain = 'MED_OUT',
                      domain_extension = 'full_point_source_yml',
                      fn = '',
                      extension = '' )


# 0.2 Mapping Files ---------------------------------------------------

calc_methods <- readData(domain = 'GRIDDING',
                         file_name = 'ces-calculation-methods',
                         extension = '.csv',
                         domain_extension = 'gridding_mappings/',
                         meta = FALSE )
# TODO: Needed temporarily since no fuel specifications as of yet
calc_methods <- calc_methods %>%
    dplyr::mutate( fuel = as.character(fuel))


# 0.2 Data Files -----------------------------------------------------

# Read in WRI's global power plant database
gppdb <- readData(domain = 'GRIDDING',
                  domain_extension = 'point-source/',
                  file_name = 'global_power_plant_database',
                  meta = FALSE)

# Change fuels to match CEDS fuels
gppdb$primary_fuel <- mapvalues( gppdb$primary_fuel,
                                 from = c( 'Gas',         'Coal',      'Oil',       'Biomass', 'Petcoke' ),
                                 to =   c( 'natural_gas', 'hard_coal', 'heavy_oil', 'biomass', 'heavy_oil' ) )


# 1. Read in all sources ---------------------------------------------

# List all yaml files
file_list <- list.files(yml_path, '*.yml')

# Read in each yml
source_df_list <- lapply(file_list, read_yml_all_ems, yml_dir = yml_path)

# Sources combined to one data frame
sources_df <- do.call(rbind, source_df_list)

# Get distinct iso, sector, fuel combos to shrink EF and CEDS inventory
relevant_combs <- sources_df %>%
    dplyr::distinct(iso, CEDS_sector, fuel)

# Remove full data frame from memory. Large and not needed.
remove(sources_df)


# 2. Read in CEDS data ---------------------------------------------

# Emission list
em_list <- c('SO2','CO2','CO','BC','OC','NOx','CH4','NMVOC','NH3','N2O')

# Year list
years <- 1750:2019
Xyears <- paste0( 'X', years )

# 2.1 Emission Factors ---------------------------------------------

# Function for reading in emission factors
get_comb_EF <- function(em){
    all_EF <- readData( domain = 'MED_OUT', file_name = paste0('H.', em, '_total_EFs_extended_adjusted-pathway') )
    non_comb_EF <- all_EF %>%
        dplyr::select(-units) %>%
        dplyr::filter(fuel != 'process') %>%
        gather(year, !!paste0(em,'_EF'), Xyears) %>%
        dplyr::mutate( year = gsub('X', '', year) )
    return(non_comb_EF)
}

# Read in all the EF data into a list by species
EF_list <- lapply(em_list, get_comb_EF)

# Join EF data into one data frame
EF_db <- EF_list %>%
    purrr::reduce(left_join, by = c('iso', 'sector', 'fuel', 'year') )

# Remove list from memory because it's really large
remove(EF_list)

# Keep only relevant EFs
EF_db <- relevant_combs %>%
    dplyr::left_join(EF_db, by = c('iso', 'CEDS_sector' = 'sector', 'fuel')) %>%
    dplyr::rename( 'sector' = 'CEDS_sector' )


# 2.2 CEDS Inventory -----------------------------------------------

# Function for reading in CEDS inventory
get_agg_CEDS_comb <- function(em){
    inventory <- readData( domain = 'MED_OUT', file_name = paste0(em, '_total_CEDS_emissions' ) )
    inventory <- inventory %>%
        dplyr::group_by( iso, sector ) %>%
        dplyr::summarise_at( Xyears, sum ) %>%
        dplyr::ungroup() %>%
        gather(year, !!paste0(em,'_CEDS'), Xyears) %>%
        dplyr::mutate( year = gsub('X', '', year) )
    return(inventory)
}

# Read in all CEDS inventories into list of data frames
CEDS_inv_list <- lapply(em_list, get_agg_CEDS_comb)

# Combine data frames
CEDS_inv <- CEDS_inv_list %>%
    purrr::reduce(full_join, by = c('iso', 'sector', 'year') )

# Remove large list from memory
remove(CEDS_inv_list)

# Keep only relevant inventories
CEDS_inv <- relevant_combs %>%
    dplyr::left_join(CEDS_inv, by = c('iso', 'CEDS_sector' = 'sector')) %>%
    dplyr::rename( 'sector' = 'CEDS_sector' )


# 3. Calculate CES Time Series ---------------------------------------

# TODO: Iterate through each point source in the source_df_list
#       For each point source do:
#           1. Figure out which species needs to be estimated
#           2. For each of those species do:
#               2.1. Use the calc-methods mapping file to determine which
#                    methods to try.
#               2.2. In order of preferences, try to fill the time series
#           3. Return data filled out
#       Call should look like:
#           lapply( source_df_list, fill_species_function )


# Fill species handler function:
calc_ces_manager <- function( point_source ){

    # Merge point source with EF_db just once here
    EFs <- point_source %>%
        dplyr::select(iso, CEDS_sector, fuel) %>%
        dplyr::left_join( EF_db, by = c('iso', 'CEDS_sector' = 'sector', 'fuel' ) ) %>%
        dplyr::rename( 'sector' = 'CEDS_sector' ) %>%
        dplyr::distinct(iso, sector, fuel, year, .keep_all = TRUE)

    # Merge point source with CEDS inv just once here
    CEDS_inv_ps <- point_source %>%
        dplyr::select(iso, CEDS_sector, fuel) %>%
        dplyr::left_join( CEDS_inv, by = c('iso', 'CEDS_sector' = 'sector', 'fuel') ) %>%
        dplyr::rename( 'sector' = 'CEDS_sector' ) %>%
        dplyr::distinct(iso, sector, fuel, year, .keep_all = TRUE)

    # Getting the emission species that already have data, and need data
    existing_ems <- unique( point_source$species )
    needed_ems <- em_list[em_list %!in% existing_ems]

    # Iteratively getting a new time series for each needed emission species
    new_time_series <- lapply(needed_ems, fill_co_emitted_species,
                              point_source, EFs, CEDS_inv_ps)
    new_time_series_df <- do.call(rbind, new_time_series)

    # Combine new data with input data and return
    return( rbind(point_source, new_time_series_df) )
}

# Function to calculate new time series for given emission species
fill_co_emitted_species <- function(em, point_source, EFs, CEDS){

    # Get method preference order
    # This gets the order from the entry that has the fewest NA values,
    # so the most detailed.
    # If there are ties, it prefers sector, then iso, then fuel
    pref_order <- calc_methods %>%
        dplyr::filter( species == em ) %>%
        dplyr::filter( iso == point_source$iso[1] | is.na(iso) ) %>%
        dplyr::filter( sector == point_source$CEDS_sector[1] | is.na(sector) ) %>%
        dplyr::filter( fuel == point_source$fuel[1] | is.na(fuel) ) %>%
        dplyr::mutate( na_count = apply(., 1, function(x){sum(is.na(x))} ) ) %>%
        dplyr::arrange( na_count, sector, iso, fuel ) %>%
        dplyr::select( preference_order ) %>%
        dplyr::slice(1) %>%
        pull()

    # Pref order vector
    pref_order_vec <- strsplit(pref_order, ',')
    pref_order_vec <- lapply(pref_order_vec, str_trim)[[1]]

    # Loop until time series is calculated
    for( method in pref_order_vec){

        # Switch function. Depending on the input method, will call
        # the corresponding function.
        # The last line without a key is effectively an 'else' statement
        result = switch( method,
                         'none' = point_source %>% dplyr::slice(0),
                         'capacity' = ces_cap(em, point_source, CEDS, Xyears),
                         'any' = ces_any(em, em_list, point_source, EFs, Xyears),
                         ces_em(em, method, point_source, EFs, Xyears)
                        )

        # Return the result if non-empty, or method was none
        if( nrow(result) != 0 | method %in% c('none') ){
            return( result )
        }

    }

    # If made it through, return empty data frame
    return( point_source %>% dplyr::slice(0) )

}

# Call manager function for each point source
full_sources_list <- lapply(source_df_list, calc_ces_manager)



# 4. Write out YAML --------------------------------------------------

# Create output directories
dir.create(out_path, showWarnings = FALSE)
for(em in em_list){
    dir.create(paste0(out_path, '/', em), showWarnings = FALSE)
}

# Write out YAML files containing individual species time series
invisible( lapply(full_sources_list, write_yml_by_em, base_yml_dir = out_path) )


# END --------------------------------------------------
logStop()
