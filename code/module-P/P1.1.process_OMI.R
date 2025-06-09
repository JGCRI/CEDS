# Header ---------------------------------------------------------------------
# Program Name: P1.1.process_OMI.R
# Author: Noah Prime
# Date Last Updated: Feb 1, 2023
# Program Purpose:  Disaggregate OMI sources to point sources within given radius
#                   and extend those point sources back in time to their construction
#                   date.Saves these time series as yml files to be used in subsequent
#                   scripts.
# Input Files: Catalogue_SO2_2022_complete_June-12-2023.xls
#              global_power_plant_database.csv
#              H.SO2_total_EFs_extended_adjusted-pathway.csv
#              OMI_to_ceds_sector_mapping.csv
# Output Files: <OMI number>_<GPPDB name>.yml


# 0. Read in global settings and headers ------------------------------------
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"
source( paste0( PARAM_DIR, "header.R" ) )

initialize(script_name = "P1.1.process_OMI.R",
           log_msg = "Reading point source attributes",
           headers = c("data_functions.R"),
           common_data = TRUE)

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]

library(geosphere)
library(yaml)

# 0.5.) Set-up details for script ----------------------------------------------

# Define emissions species
if ( is.na( em ) ) em <- "SO2"

# Years
x_CEDS_years <- paste0('X', historical_pre_extension_year:end_year)

# Don't need to run this script besides when emission species is SO2
if(em != 'SO2'){
    print('OMI only available for SO2. Quitting P1.1.process_OMI.R')
    OMI <- data.frame( NUMBER = character(), NAME = character(),
                       COUNTRY = character(), SOURCE = character(),
                       COMMENT = character() )
    writeData( OMI, domain = 'DIAG_OUT', fn = 'P.OMI_sources_in_CEDS', meta = FALSE )
}else{


# 1.1) Read in input files, define output destination ------------------------

    # OMI catalog (wide)
    OMI_list <- readData( domain = 'GRIDDING', domain_extension = 'point-source/',
                         file_name = 'Catalogue_SO2_2023',
                         extension = '.xlsx', meta = FALSE )
    OMI <- OMI_list$Catalogue

    # OMI years (input as y<year>, and convert to X<year>)
    y_years <- grep('y', colnames(OMI), value = TRUE)
    x_OMI_years <- gsub('y', 'X', y_years)
    omi_years <- as.numeric(gsub('y', '', y_years))
    OMI_start_year <- min(omi_years)
    OMI_end_year <- max(omi_years)

    # OMI to CEDS sector mapping file
    OMI_sector_map <- readData( domain = 'GRIDDING', domain_extension = 'gridding_mappings/',
                                file_name = 'OMI_to_ceds_sector_mapping',
                                extension = '.csv', meta = FALSE ) %>%
        dplyr::filter(!is.na(CEDS_SECTOR))

    # EDGAR -- CEDS gridding sector map
    edgar_ceds_grid_sector_map <- readData( domain = 'GRIDDING', domain_extension = 'gridding_mappings/',
                                            file_name = 'EDGAR_sector_replace_mapping',
                                            extension = '.csv', meta = FALSE ) %>%
        dplyr::select(CEDS_level1_shortname, SO2) %>%
        na.omit()

    # CEDS -- CEDS gridding sector map
    ceds_ceds_grid_sector_map <- readData( domain = 'GRIDDING', domain_extension = 'gridding_mappings/',
                                           file_name = 'CEDS_sector_to_gridding_sector_mapping',
                                           extension = '.csv', meta = FALSE ) %>%
        dplyr::select(CEDS_working_sector, CEDS_int_gridding_sector_short)

    # CEDS new to old sector mapping
    ceds_new_to_old <- readData( domain = 'MAPPINGS',
                                 file_name = 'old_to_new_sectors',
                                 extension = '.csv', meta = FALSE )

    # Global Power Plant Database
    GPPDB <- readData( domain = 'GRIDDING', domain_extension = 'point-source/',
                       file_name = 'global_power_plant_database',
                       extension = '.csv', meta = FALSE )

    # Manually collected supplement data for GPPDB
    GPPDB_sup <- readData( domain = 'GRIDDING', domain_extension = 'point-source/',
                           file_name = 'GPPDB_supplement',
                           extension = '.csv', meta = FALSE )

    # Convert character colunms to UTF-8
    GPPDB[] <- lapply(GPPDB, function(col) {
        if (is.character(col)) {iconv(col, from = "latin1", to = "UTF-8", sub = "")
        } else {col}
        })

    # Manually collected supplement data for missing power plants in GPPDB
    CEDS_gppdb <- readData( domain = 'GRIDDING', domain_extension = 'point-source/',
                            file_name = 'CEDS_power_plant_database',
                            extension = '.csv', meta = FALSE )
    # Combine GPPBD with CEDS supplementary
    GPPDB <- GPPDB %>% dplyr::select(country, country_long, name, gppd_idnr, capacity_mw, latitude, longitude, primary_fuel, commissioning_year, source)
    GPPDB <- dplyr::bind_rows(GPPDB, CEDS_gppdb %>% dplyr::rename(gppd_idnr = CEDS_ID))

    # CEDS inventory
    CEDS_inv <- readData( domain = 'MED_OUT', file_name = 'SO2_total_CEDS_emissions' )

    # Emission Factors
    EF_db <- readData( domain = 'MED_OUT', file_name = 'H.SO2_total_EFs_extended_adjusted-pathway' )

    # iso - country name mapping
    country_iso_map <- readData( domain = 'MAPPINGS', file_name = 'Master_Country_List' )

    # Output directory
    out_dir <- '../intermediate-output/OMI_yml/'
    dir.create( out_dir, showWarnings = FALSE )

    # Create empty dataframe to be populated with power plants that are missing data
    # to be saved to diagnostics as list of items to investigate
    pps_missing_data <- data.frame( name = character(),
                                    distance_km = double(),
                                    capacity = double(),
                                    latitude = double(),
                                    longitude = double(),
                                    construct_date = integer(),
                                    country = character(),
                                    fuel = character(),
                                    NUMBER = integer() )


# 1.2.) Convert OMI to long format ------------------------------------------

    # Change 'Coal' and 'Oil' to 'hard_coal', and 'heavy_oil' to match CEDS fuels
    GPPDB$primary_fuel <- mapvalues( GPPDB$primary_fuel,
                                     from = c( 'Gas',         'Coal',      'Oil',       'Biomass', 'Petcoke' ),
                                     to =   c( 'natural_gas', 'hard_coal', 'heavy_oil', 'biomass', 'heavy_oil' ) )

    # Some countries names in OMI don't match the master counter list. Here we replace
    # those cases in order to get the correct iso.
    OMI <- OMI %>%
        dplyr::filter( TYPE != 'Volcano') %>%
        dplyr::mutate( COUNTRY = case_when(COUNTRY == 'USA' ~ 'United States',                       COUNTRY == 'Santa Cruz Islands' ~ 'Soloman Islands',
                                           COUNTRY == 'UK' ~ 'United Kingdom',                       COUNTRY == 'Tonga Islands' ~ 'Tonga',
                                           COUNTRY == 'Iran' ~ 'Islamic Republic of Iran',           COUNTRY == 'Reunion Island, France' ~ 'France',
                                           COUNTRY == 'Taiwan' ~ 'Chinese Taipei',                   COUNTRY == 'South Sandwich Isl. (UK)' ~ 'United Kingdom',
                                           COUNTRY == 'Montserrat (UK)' ~ 'Montserrat',              COUNTRY == 'Antarctica' ~ 'Antarctica',
                                           COUNTRY == 'Northern Mariana Islands' ~ 'United States',  COUNTRY == 'Macedonia (FYROM)' ~ 'Macedonia',
                                           COUNTRY == 'Trinidad' ~ 'Trinidad and Tobago',            COUNTRY == 'South Korea' ~ 'Republic of Korea',
                                           COUNTRY == 'Papua New Guinea\\xa0' ~ 'Papua New Guinea',  COUNTRY == 'Trukmenistan' ~ 'Turkmenistan',
                                           COUNTRY == 'Shi Lanka' ~ 'Sri Lanka',
                                           TRUE ~ COUNTRY)) %>%
        dplyr::left_join( country_iso_map, by = c('COUNTRY' = 'Country_Name') ) %>%
        dplyr::rename( SOURCE = TYPE ) %>%
        dplyr::select( iso, NUMBER, LATITUDE, LONGITUDE, SOURCE, NAME, COUNTRY, COMMENT, all_of(y_years) )

    # Set CEDS_sector to defaults, or according to OMI mapping file
    OMI <- OMI %>%
        dplyr::left_join( OMI_sector_map %>% dplyr::select(NUMBER, CEDS_SECTOR, FUEL), by = 'NUMBER') %>%
        dplyr::mutate( CEDS_SECTOR = ifelse(!is.na(CEDS_SECTOR), CEDS_SECTOR,
                                            ifelse(SOURCE == 'Smelter', '2C4_Non-Ferrous-other-metals',
                                                   ifelse(SOURCE == 'Oil and Gas', '1B2_Fugitive-petr', '1A1a_Electricity-public') ) )) %>%
        dplyr::distinct( NUMBER, .keep_all = TRUE )

    # OMI catalog (long)
    OMI_long <- OMI %>%
        tidyr::gather( year, em, all_of(y_years) ) %>%
        dplyr::mutate( year = as.numeric( gsub( 'y', '', year ) ) ) %>%
        dplyr::select( iso, NUMBER, LATITUDE, LONGITUDE, SOURCE, CEDS_SECTOR, NAME, year, em, COUNTRY, COMMENT )

# 1.3.) Full sector mapping (CEDS new sector -> CEDS old sector -> CEDS gridding sector -> EDGAR gridding sector) -----------------
    full_sector_map <- ceds_ceds_grid_sector_map %>%
        dplyr::left_join( ceds_new_to_old, by = c('CEDS_working_sector' = 'ceds_sector') ) %>%
        dplyr::mutate( new_sector = ifelse(is.na(new_sector), CEDS_working_sector, new_sector) ) %>%
        dplyr::filter( new_sector %in% OMI$CEDS_SECTOR ) %>%
        dplyr::left_join( edgar_ceds_grid_sector_map, by = c('CEDS_int_gridding_sector_short' = 'CEDS_level1_shortname') ) %>%
        dplyr::mutate( SO2 = case_when( new_sector == '2C4_Non-Ferrous-other-metals' ~ 'nfe',
                                        new_sector == '2C3_Aluminum-production' ~ 'nfe',
                                        new_sector == '2C1_Iron-steel-alloy-prod' ~ 'iro',
                                        new_sector == '2B_Chemical-industry' ~ 'che',
                                        new_sector == '1B2_Fugitive-petr' ~ 'flr',
                                        new_sector == '2C4_Non-Ferrous-other-metals' ~ 'nfe',
                                        TRUE ~ SO2 )) %>%
        dplyr::distinct()

# 2.) Define functions for processing and extending OMI -----------------------

# 2.1.) Returns data for single OMI source in desired format -------------------

    # Function extracts single OMI source in long format
    get_source_long <- function( num ){
        return( OMI_long %>% dplyr::filter( NUMBER == num ) )
    }

    # Function extracts single OMI source in wide format
    get_source_wide <- function( num ){
        return( OMI %>% dplyr::filter( NUMBER == num ) )
    }


# 2.2.) Getting Power Plants within OMI source -----------------------------

    # Returns power plants from GPPDB within given radius (km) of input OMI source
    pp_within_rad <- function( wide_source, rad ){

        lat <- wide_source$LATITUDE
        lon <- wide_source$LONGITUDE

        buff <- 3

        GPPDB_short <- GPPDB %>%
            dplyr::filter( abs( longitude - lon ) < buff | abs( longitude - lon ) > (360-buff),
                           abs( latitude - lat ) < buff  | abs( latitude - lat ) > (180-buff) )


        within_rad <- as.data.frame( geosphere::distm( GPPDB_short %>%
                                                dplyr::select( longitude, latitude ),
                                            wide_source %>%
                                                dplyr::select( LONGITUDE, LATITUDE ),
                                            fun = distVincentyEllipsoid ) ) %>%
            dplyr::mutate( name = GPPDB_short$name,
                           id = GPPDB_short$gppd_idnr,
                           capacity = GPPDB_short$capacity_mw,
                           latitude = GPPDB_short$latitude,
                           longitude = GPPDB_short$longitude,
                           construct_date = GPPDB_short$commissioning_year,
                           fuel = GPPDB_short$primary_fuel,
                           country = GPPDB_short$country ) %>%
            dplyr::filter( V1 < rad * 1000 ) %>%
            dplyr::mutate( distance_km = V1 / 1000 ) %>%
            dplyr::select( id, name, distance_km, capacity, latitude, longitude, construct_date, country, fuel ) %>%
            dplyr::mutate( NUMBER = wide_source$NUMBER )

        return( within_rad )
    }


    # Function to fill in missing information in power plant data
    # and to select only supported fuel type (ie not wind or solar)
    clean_power_plant_data <- function( pp_data ){

        # Supported power plant fuels
        supported_fuels <- c( 'hard_coal', 'heavy_oil', 'natural_gas', 'biomass' )

        # Select only support fuels, and add any information manually gathered into
        # supplemental data file
        pp_data <- pp_data %>%
            dplyr::filter( fuel %in% supported_fuels ) %>%
            left_join( GPPDB_sup %>% dplyr::select(-name), by = c( 'id', 'fuel' = 'old_fuel' ) ) %>%
            dplyr::mutate( capacity = ifelse( !is.na( manual_cap ), manual_cap, capacity ),
                           construct_date = ifelse( !is.na( manual_cd ), manual_cd, construct_date ),
                           fuel = ifelse( !is.na( real_fuel ), real_fuel, fuel ) ) %>%
            dplyr::select( -c( manual_cap, manual_cd, real_fuel, iso ) ) %>%
            dplyr::mutate( construct_date = as.integer( round( construct_date ) ) )

        # TODO: Perhaps come up with a way to infer the construction date
        #       Currently just assuming it's always been there essentially
        # TODO: Power plants need to have capacity data, otherwise impossible to
        #       dis-aggregate in any meaningful way
        # Save plants missing data for diagnostic file
        pps_missing_data <<- rbind( pps_missing_data, pp_data[ !complete.cases( pp_data ), ] )

        # Remove plants with missing capacity data
        pp_data <- pp_data %>%
            dplyr::filter( !is.na(capacity) )

        return( pp_data )

    }


# 2.3.) Disaggregation -----------------------------------------------------

    # Returns the input OMI source disagregated into nearby power plants.
    # Returns in long format time series for each plant (one dataframe).
    disagg_omi <- function( long_source, pps_within_rad ){

        # iso is lower case everywhere in CEDS
        pps_within_rad$country <- tolower( pps_within_rad$country )

        # Remove power plants with missing commission data
        pps_within_rad <- pps_within_rad %>%
            dplyr::filter( !is.na(construct_date) )

        # Creating data frame with each power plant near the OMI source having its
        # fair share of the OMI emissions assigned to it for every OMI year
        disagg_source <- pps_within_rad %>%
            # Start by joining with emission factor database to get emission factors
            # for every year corresponding to the correct fuel
            dplyr::left_join( EF_db %>%
                           dplyr::filter( sector == '1A1a_Electricity-public' ),
                       by = c( 'country' = 'iso', 'fuel' ) ) %>%
            dplyr::select( id:NUMBER, x_OMI_years ) %>%
            # Turn into long format and remove 'X' from year and make numeric type
            tidyr::gather( year, EF, x_OMI_years ) %>%
            dplyr::mutate( year = as.numeric( gsub( 'X', '', year ) ) ) %>%
            # Here we set a power plant's capacity to zero if the year is before its
            # construction date
            dplyr::mutate( capacity = capacity * ( year >= construct_date ) ) %>%
            # Here we get the total capacity present for each fuel type, for each year
            # and iso in case there happens to be multiple isos present
            dplyr::group_by( fuel, country, year, EF ) %>%
            dplyr::summarise( capacity = sum( capacity ) ) %>%
            dplyr::ungroup() %>%
            # Here we get the relative EF weight for each fuel type for each year
            # i.e., Oil has largest EF, and thus will be weighted higher, whereas
            # natural gas has a very low relative EF
            dplyr::group_by( year ) %>%
            dplyr::mutate( EF_pct = EF / sum( EF ) ) %>%
            # Here we use the total capacity for each fuel, and EF eight for each fuel,
            # to find the proportion of OMI emissions that should be attributed to each fuel
            # type for each year
            dplyr::mutate( ratio_fuel_year = capacity * EF_pct / sum( capacity * EF_pct ) ) %>%
            dplyr::mutate( ratio_fuel_year = ifelse( is.na(ratio_fuel_year),0,ratio_fuel_year ) ) %>%
            dplyr::ungroup() %>%
            dplyr::mutate( year = as.numeric( gsub( 'X', '', year ) ) ) %>%
            # Here we join with the OMI source to of course get the emission time series
            dplyr::left_join( long_source, by = 'year' ) %>%
            # Here we use the OMI source, and the proportions previously calculated to
            # get the emissions attributed to each fuel type for each year
            dplyr::mutate( em_fuel_year = ratio_fuel_year * em ) %>%
            dplyr::select( fuel, country, year, em_fuel_year, number = NUMBER, COUNTRY, iso = country ) %>%
            # Here we rejoin with the nearby power plants to distribute the emissions
            # which are now already split by fuel type, to plants of the corresponding fuel type
            dplyr::left_join( pps_within_rad, by = c( 'fuel', 'iso' = 'country' ) ) %>%
            # Again setting plant capacity to zero if the year is before its construction date
            dplyr::mutate( capacity = capacity * ( year >= construct_date ) ) %>%
            # Getting the proportion of emissions attributed to each power plant in its
            # corresponding fuel group based on capacity
            dplyr::group_by( fuel, year, iso ) %>%
            dplyr::mutate( cap_pct = capacity / sum( capacity ) ) %>%
            dplyr::mutate( cap_pct = ifelse(is.na(cap_pct),0,cap_pct) ) %>%
            # Distributing emissions from the fuel groups to plants in that group
            dplyr::mutate( dis_agg_em = em_fuel_year * cap_pct ) %>%
            dplyr::ungroup() %>%
            dplyr::select( id, number, name, year, dis_agg_em, latitude, longitude, construct_date,
                           iso, country = COUNTRY, fuel )


        return( disagg_source )
    }

    infer_pp_emissions <- function( pps_within_rad ){

        pps_within_rad <- pps_within_rad %>%
            dplyr::mutate( country = tolower( country ),
                           name_iso_fuel = paste0( name, '_', country, '_', fuel ) )

        iso_fuel_combinations <- pps_within_rad %>%
            dplyr::select( country, fuel ) %>%
            dplyr::distinct( country, fuel )

        # Get all power plants in the iso and mark them as either within
        # the radius or outside the radius
        GPPDB_info <- iso_fuel_combinations %>%
            dplyr::left_join( GPPDB %>% dplyr::mutate( country = tolower(country) ), by = c( 'country', 'fuel' = 'primary_fuel' ) ) %>%
            dplyr::select( name, capacity = capacity_mw, construct_date = commissioning_year, country, fuel ) %>%
            dplyr::mutate( name_iso_fuel = paste0( name, '_', country, '_', fuel ) ) %>%
            dplyr::mutate( nearby = ifelse( name_iso_fuel %in% pps_within_rad$name_iso_fuel, 'near', 'far' ) )

        # Check that not all power plants in iso are within radius
        # Can happen in small countries
        if( sum(GPPDB_info$nearby == 'far') == 0 ){
            # Calculating (estimating) total emissions from power plants within radius
            # Start by only getting power plants in iso
            tot_pp_emissions <- GPPDB_info %>%
                dplyr::left_join( CEDS_inv %>%
                               dplyr::filter( sector == '1A1a_Electricity-public' ),
                           by = c( 'country' = 'iso', 'fuel' ) ) %>%
                tidyr::gather( year, CEDS_em, x_CEDS_years ) %>%
                dplyr::mutate( year = as.numeric( gsub( 'X', '', year ) ) ) %>%
                dplyr::group_by( year ) %>%
                # Emissions are just the CEDS emissions, since we have all power plants in this iso
                dplyr::summarise( total_pp_em = sum( CEDS_em ) )
        }else{
        # Calculating (estimating) total emissions from power plants within radius
        tot_pp_emissions <- GPPDB_info %>%
            dplyr::left_join( CEDS_inv %>%
                           dplyr::filter( sector == '1A1a_Electricity-public' ),
                       by = c( 'country' = 'iso', 'fuel' ) ) %>%
            tidyr::gather( year, CEDS_em, x_CEDS_years ) %>%
            dplyr::mutate( year = as.numeric( gsub( 'X', '', year ) ) ) %>%
            # Assuming that any plants without construction years were around for all
            # of OMI
            dplyr::mutate( construct_date = ifelse( is.na( construct_date ), 2005, construct_date ) ) %>%
            dplyr::mutate( capacity = capacity * ( year >= construct_date ) ) %>%
            dplyr::group_by( nearby, year, country, fuel, CEDS_em ) %>%
            dplyr::summarise( sum_cap = sum( capacity ) ) %>%
            tidyr::spread( nearby, sum_cap, fill = 0 ) %>%
            # Proportion of CEDS emissions = total capacity in the radius / total capacity of iso
            dplyr::mutate( prop = ifelse( far + near == 0, 0, near / ( far + near ) ) ) %>%
            # Get emissions (multiply CEDS emissions by proportion)
            dplyr::mutate( pp_ems = CEDS_em * prop ) %>%
            dplyr::group_by( year ) %>%
            # Aggregate over fuel, and in the iso
            dplyr::summarise( total_pp_em = sum( pp_ems ) )
        }

        return( tot_pp_emissions )
    }

# 2.4.) Extending sources -------------------------------------------------

    # Function to extend disaggregated power plants using emission factors
    extend_power_plants_EF <- function( disagg_source ){

        # Get list of ids of each plant to extend
        plant_ids <- disagg_source %>%
            dplyr::distinct( id ) %>%
            dplyr::pull()

        # For each plant in OMI radius, apply the extension function
        # Returns a list of data frames with the extended data for each plant
        # Combine all together and with non-extended data to finish
        extensions <- lapply( plant_ids, extend_power_plant_EF, plants_data = disagg_source )

        # Combine extended data data
        comb_extensions <- do.call( rbind, extensions )

        # Combined with OMI data for full data
        full <- rbind( disagg_source, comb_extensions )

        return( full )
    }

    # Function used to extend a single power plant in an OMI radius using emission factors
    extend_power_plant_EF <- function( plant_id, plants_data ){

        # Get the needed info for the singular plant
        plant_data <- plants_data %>%
            dplyr::filter( id == plant_id, year == OMI_start_year )

        # Create data frame for all years before OMI with default emission set to 0
        prev_data <- data.frame( id = plant_data$id,
                                 number = plant_data$number,
                                 name = plant_data$name,
                                 year = historical_pre_extension_year:(OMI_start_year-1),
                                 dis_agg_em = 0,
                                 latitude = plant_data$latitude,
                                 longitude = plant_data$longitude,
                                 construct_date = plant_data$construct_date,
                                 iso = plant_data$iso,
                                 country = plant_data$country,
                                 fuel = plant_data$fuel )

        # If OMI years covers lifespan of plant (ie no need for extension), return
        if( plant_data$construct_date >= OMI_start_year ){
            return( prev_data )
        }

        # Add row to previous data for 2005 for extension
        prev_data <- rbind( prev_data, plant_data )

        # Get appropriate emission factors for extension ( by iso, sector, fuel )
        EFs <- EF_db %>%
            dplyr::filter( iso == tolower( plant_data$iso ), sector == '1A1a_Electricity-public',
                           fuel == plant_data$fuel )

        # Use ratio of EFs between consecutive years to scale emissions backwards in time
        # Here we just calculate those ratios
        prev_data$EF_ratio <- as.numeric( t( EFs[ paste0('X',historical_pre_extension_year:OMI_start_year) ] / EFs[ paste0('X',(historical_pre_extension_year+1):(OMI_start_year+1)) ] ) )

        # Arrange for 2005 to be up top
        prev_data <- prev_data %>%
            dplyr::arrange( dplyr::desc( year ) )

        # Extend data back to construct date using lag/lead variables
        for( i in (OMI_start_year-1):plant_data$construct_date ){
            prev_data <- prev_data %>%
                dplyr::arrange( dplyr::desc( year ) ) %>%
                dplyr::mutate( dis_agg_em = lag( dis_agg_em * dplyr::lead( EF_ratio ), default = dis_agg_em[1] ) )
        }

        # Remove EF column and row for 2005 which is already present in full data
        prev_data <- prev_data %>%
            dplyr::select( -EF_ratio ) %>%
            dplyr::filter( year < OMI_start_year ) %>%
            dplyr::mutate( dis_agg_em = ifelse( is.na(dis_agg_em),0,dis_agg_em ) )

        return( prev_data )
    }

    # Extend non power plant emissions using CEDS inventory
    extend_non_pp <- function( non_pp_data ){

        # Get the needed info from non_pp
        non_pp <- non_pp_data %>%
            dplyr::filter( year == OMI_start_year )

        # Create data frame for all years before OMI with default emission set to 0
        prev_data <- data.frame( NUMBER = non_pp$NUMBER,
                                 NAME = non_pp$NAME,
                                 CEDS_SECTOR = non_pp$CEDS_SECTOR,
                                 year = historical_pre_extension_year:(OMI_start_year-1),
                                 em = 0,
                                 LATITUDE = non_pp$LATITUDE,
                                 LONGITUDE = non_pp$LONGITUDE,
                                 iso = non_pp$iso,
                                 COUNTRY = non_pp$COUNTRY,
                                 COMMENT = non_pp$COMMENT,
                                 SOURCE = non_pp$SOURCE )

        # CEDS inventory for the given iso and CEDS sector
        CEDS_inv_for_source <- CEDS_inv %>%
            dplyr::filter( sector == non_pp$CEDS_SECTOR, iso == non_pp$iso )

        # Specified point source fuel type
        source_fuel <- OMI_sector_map %>%
            dplyr::filter(NUMBER == non_pp$NUMBER) %>%
            dplyr::select(FUEL) %>%
            dplyr::pull()

        # If fuel type unavailable, print warning, and use fuel associated with
        # the greatest amount of emissions in CEDS
        if(length(source_fuel) == 0){
            source_fuel <- CEDS_inv_for_source %>%
                dplyr::mutate( total = rowSums(dplyr::select(., x_CEDS_years)) ) %>%
                dplyr::select(fuel, total) %>%
                dplyr::arrange(desc(total)) %>%
                dplyr::select(fuel) %>%
                dplyr::top_n(1, fuel) %>%
                dplyr::pull()
        } else if(is.na(source_fuel)){
            printLog(paste0('Unspecified Fuel Type for OMI source ', non_pp$NUMBER) )
            source_fuel <- CEDS_inv_for_source %>%
                dplyr::mutate( total = rowSums(select(., x_CEDS_years)) ) %>%
                dplyr::select(fuel, total) %>%
                dplyr::arrange(desc(total)) %>%
                dplyr::select(fuel) %>%
                dplyr::top_n(1, fuel) %>%
                dplyr::pull()
        }

        # THIS EXTENSION METHOD IS NOT REASONABLE BESIDES VERY RECENT YEARS
        extended_non_pp <- rbind( non_pp_data, prev_data ) %>%
            dplyr::left_join( CEDS_inv_for_source %>%
                           dplyr::filter( fuel == source_fuel ) %>%
                           tidyr::gather( year, CEDS_em, all_of(x_CEDS_years) ) %>%
                           dplyr::mutate( year = as.numeric( gsub( 'X', '', year ) ) ),
                       by = c( 'iso', 'year' ) ) %>%
            dplyr::mutate( prop = ifelse(CEDS_em == 0, 0, em / CEDS_em ) ) %>%
            dplyr::mutate( prop = mean( prop[ year %in% OMI_start_year:end_year ] ) ) %>%
            dplyr::mutate( final_em = ifelse( year < OMI_start_year, CEDS_em * prop, em ) ) %>%
            dplyr::arrange( year ) %>%
            dplyr::mutate( final_em = ifelse( is.na(final_em),0,final_em ) )


        return( extended_non_pp )
    }


# 2.4.) Creating yml files ---------------------------------------------

    # Function that creates a yml for each power plant in the provided data
    create_ymls <- function( extended_data ){

        # Get list of names of each plant to extend
        plant_ids <- extended_data %>%
            dplyr::distinct( id ) %>%
            dplyr::pull()

        # For each plant in OMI radius, get extended data and create yml
        lapply( plant_ids, create_yml, plants_data = extended_data )

    }

    # Function that creates a yml for a single power plant
    create_yml <- function( plant_id, plants_data ){

        # Name of yaml file to be saved for the given power plant
        file_name <- paste0('SO2_OMI_', plant_id, '.yml')

        # OMI number(s)
        new_number <- plants_data$number[1]

        # Check to see if plant has been seen in the footprint of a previous OMI source
        if( length( list.files( out_dir, file_name ) ) > 0 ){
            # Read in existing file and add it to new data. Then re-write file
            existing_data <- read_yaml(paste0(out_dir,file_name))
            plants_data <- tidyr::gather( as.data.frame( existing_data$attributes$value ) ) %>%
                dplyr::mutate( year = as.numeric( gsub('X','',key) ) ) %>%
                dplyr::left_join(plants_data, by = 'year') %>%
                dplyr::mutate( dis_agg_em = dis_agg_em + value ) %>%
                dplyr::select(-key,-value)
            existing_number <- gsub('Power plant near OMI source #', '', existing_data$documentation$description)
            new_number <- paste0( existing_number, ' and ', new_number )
        }

        # Data for single plant
        plant_data <- plants_data %>%
            dplyr::filter(id == plant_id ) %>%
            dplyr::arrange( year )

        # Compile each component of the yml recipe file starting with documentation
        header <- list( description = paste0( 'Power plant near OMI source #', new_number ),
                        data_source = 'omi',
                        date = as.character( Sys.Date() ) )
        header_nested <- header %>%
            list( documentation = . )

        # Start yml
        part1 <- as.yaml( header_nested, indent.mapping.sequence = TRUE, line.sep = "\n" )

        # Create list of the time series
        time_series <- setNames( as.list( plant_data$dis_agg_em ), historical_pre_extension_year:OMI_end_year )

        # Compile the attributes section of the yaml file
        name_for_attr <- iconv( gsub( "[[:punct:]]", " ", plant_data$name[1] ), 'UTF-8', 'ASCII', sub = '' )
        atr <- list( name = name_for_attr,
                     id = plant_id,
                     location = plant_data$country[1],
                     longitude = plant_data$longitude[1],
                     latitude = plant_data$latitude[1],
                     emission = 'SO2',
                     units = 'kt',
                     value = time_series,
                     CEDS_sector = '1A1a_Electricity-public',
                     EDGAR_sector = 'ENE',
                     fuel = plant_data$fuel[1],
                     iso = tolower( plant_data$iso[1] ),
                     build_year = as.integer( plant_data$construct_date[1] )
        )

        atr_nested <- atr %>%
            list( attributes = . )

        # Remaining part of yml
        part2 <- as.yaml( atr_nested, indent.mapping.sequence = TRUE, line.sep = "\n")


        # Write out a yaml file for each point source
        write( c(part1, part2), paste0( out_dir, file_name ) )

    }

    # Function that creates a yml for a single smelter
    create_yml_non_pp <- function( non_pp_data ){

        # Compile each component of the yml recipe file starting with documentation
        header <- list( description = paste0( 'OMI source #', non_pp_data$NUMBER[1] ),
                        data_source = 'omi',
                        date = as.character( Sys.Date() ) )
        header_nested <- header %>%
            list( documentation = . )

        # Start yml
        part1 <- as.yaml( header_nested, indent.mapping.sequence = TRUE, line.sep = "\n" )

        # Create list of the time series
        time_series <- setNames( as.list( non_pp_data$final_em ), historical_pre_extension_year:OMI_end_year )

        # Create source ID
        source_id <- formatC( non_pp_data$NUMBER[1], width = 7, flag = '0' )

        # EDGAR sector
        edgar_sector <- full_sector_map %>%
            dplyr::filter(new_sector == non_pp_data$sector[1]) %>%
            dplyr::select(SO2) %>%
            dplyr::pull()
        edgar_sector <- toupper(edgar_sector[1])

        # Compile the attributes section of the yaml file
        name_for_attr <- iconv( gsub( "[[:punct:]]", " ", non_pp_data$NAME[1] ), 'UTF-8', 'ASCII', sub = '' )
        atr <- list( id = paste0('OMI',source_id),
                     name = name_for_attr,
                     location = non_pp_data$COUNTRY[1],
                     longitude = non_pp_data$LONGITUDE[1],
                     latitude = non_pp_data$LATITUDE[1],
                     emission = 'SO2',
                     units = 'kt',
                     value = time_series,
                     CEDS_sector = non_pp_data$sector[1],
                     EDGAR_sector = edgar_sector,
                     fuel = non_pp_data$fuel[1],
                     iso = non_pp_data$iso[1],
                     build_year = -1
        )

        atr_nested <- atr %>%
            list( attributes = . )

        # Remaining part of yml
        part2 <- as.yaml( atr_nested, indent.mapping.sequence = TRUE, line.sep = "\n")


        # Write out a yaml file for each point source
        file_name <- paste0('SO2_OMI_OMI', source_id, '.yml' )
        write( c(part1, part2), paste0( out_dir, file_name ) )

    }

    # Function that calls subsequent functions to take an OMI number and
    # result in yml files being created for power plants OMI sources
    process_OMI <- function( num, rad ){
        source_long <- get_source_long( num )
        source_wide <- get_source_wide( num )
        close_pps <- pp_within_rad( source_wide, rad )
        close_pps_clean <- clean_power_plant_data( close_pps )
        if( nrow( close_pps_clean ) < 1 ){ return() }
        source_disagg <- disagg_omi( source_long, close_pps_clean )
        if( nrow( source_disagg ) < 1 ){ return() }
        source_disagg_extended_EF <- extend_power_plants_EF( source_disagg )
        create_ymls( source_disagg_extended_EF )
    }

    # Function that calls subsequent functions to take an OMI number and
    # result in yml files being created for smelter OMI sources
    process_OMI_non_pp <- function( num, rad ){
        # Get needed versions of OMI data
        source_long <- get_source_long( num )
        source_wide <- get_source_wide( num )
        # Get nearby Power plants
        close_pps <- pp_within_rad( source_wide, rad )
        close_pps_clean <- clean_power_plant_data( close_pps )

        source_adjusted <- source_long
        if( nrow( close_pps_clean ) > 0 ){
            #   - Use CEDS inventory, total capacity, and nearby capacity
            #     to estimate total emissions of nearby plants
            tot_pp_ems <- infer_pp_emissions( close_pps_clean )
            #   - Subtract above from OMI source
            source_adjusted <- source_long %>%
                dplyr::left_join( tot_pp_ems, by = 'year' ) %>%
                # Subtract power plant emissions from smelter, not allowing emissions to go below zero
                dplyr::mutate( em = ifelse( em - total_pp_em  > 0, em - total_pp_em, 0  ) ) %>%
                # If there is a zero between two non-zero years, replace with average of surrounding year
                # This is a common data artifact in OMI
                dplyr::mutate( em = ifelse( em == 0 & lag(em) > 0 & dplyr::lead(em) > 0, (lag(em) + dplyr::lead(em)) / 2, em ) ) %>%
                # Zero on ends more likely to be valid, so let those remain zero
                dplyr::mutate( em = ifelse( is.na( em ), 0, em ) ) %>%
                dplyr::select( -total_pp_em )
        }

        #   - Extend using CEDS inventory
        source_extended <- extend_non_pp( source_adjusted )

        # Check if any NAs exist in required columns
        na_values <- source_extended %>%
            dplyr::select(iso, sector, fuel) %>%
            is.na() %>%
            sum()

        # Do not write out if NAs exist in required columns
        if(na_values != 0) return()

        # Create yml
        create_yml_non_pp( source_extended )
    }


# 3.1) Creating yml files by OMI Power Plant ----------------------------------

    omi_power_plants <- OMI %>%
        dplyr::filter(CEDS_SECTOR == '1A1a_Electricity-public') %>%
        dplyr::select( NUMBER ) %>%
        dplyr::pull()

    # Default radius of 40
    # Radius set to 60 for OMI num 25 in order to match with GPPDB
    # and 90 in ZAF in order to match with OMI comment
    radii <- rep( 40, length( omi_power_plants ) )
    radii[ omi_power_plants == 25 ] <- 60
    radii[ omi_power_plants == 371 ] <- 90

    for( omi_num in omi_power_plants ){
        print( paste0( 'Processing OMI source #', omi_num ) )
        process_OMI( omi_num, radii[ omi_power_plants == omi_num ] )
        print('Done')
    }

# 3.2.) Creating yml files by OMI non power plant ---------------------------------

    omi_non_pp <- OMI %>%
        dplyr::filter( CEDS_SECTOR != '1A1a_Electricity-public' ) %>%
        dplyr::select( NUMBER ) %>%
        dplyr::pull()

    for( omi_num in omi_non_pp ){
        print( paste0( 'Processing OMI source #', omi_num ) )
        process_OMI_non_pp( omi_num, 40 )
        print('Done')
    }


# 4.) Diagnostic -----------------------------------------------------------

    OMI %>%
        dplyr::select( NUMBER, NAME, COUNTRY, SOURCE, COMMENT ) %>%
        writeData( domain = 'DIAG_OUT', fn = 'P.OMI_sources_in_CEDS', meta = FALSE )

    # Write out power plants from GGPDB which have been excluded due to missing data
    writeData( pps_missing_data, domain = 'DIAG_OUT', fn = 'GPPDB_excluded_for_missing_data', meta = FALSE )
}


# End --------------------------------------------------------------

logStop()




