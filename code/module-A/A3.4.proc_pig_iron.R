# ------------------------------------------------------------------------------
# Program Name: A3.4.proc_pig_iron.R  ### I think this file could have a more informative name
# Author: Linh Vu
# Date Last Updated: 14 April 2016
# Program Purpose: Process pig iron production
# Input Files:  Blast_furnace_iron_production_1850-2014.xlsx,
#               Pig_Iron_Production_US.csv, Pig_Iron_Production_Mitchell.csv,
#               A.UN_pop_master.csv
# Output Files: A.Pig_Iron_Production.csv, A.Pig_Iron_Production_full.csv
# TODO: Read in just the primary data (and not rely on the “data” sheet extrapolation)
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "common_data.R", "data_functions.R" ) # Additional function files may be required.
    log_msg <- "Process pig iron production"
    script_name <- "A3.4.proc_pig_iron.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 1. Read input

# 'Data' tab only runs to 1890, so read 1850-1890 from tab 'SPEW_Pig_iron_production'
    spew <- readData( "ACTIVITY_IN", "Blast_furnace_iron_production_1850-2014", ".xlsx",
                      sheet_selection = "Data", domain_extension = "metals/",
                      skip = 2 )[ 1:60, 3:128 ]
    spew_pre <- readData( "ACTIVITY_IN", "Blast_furnace_iron_production_1850-2014", ".xlsx",
                    sheet_selection = "SPEW_Pig_iron_production", domain_extension = "metals/",
                    to_numeric = F )[ 2:10 ]
    us <- readData( "ACTIVITY_IN", "Pig_Iron_Production_US", domain_extension = "metals/" )
    mitchell <- readData( "ACTIVITY_IN", "Pig_Iron_Production_Mitchell", domain_extension = "metals/" )

    un_pop <- readData( "MED_OUT", "A.UN_pop_master", meta = F )

# Define values
    SHORT_TO_METRIC <- .9072  # short ton to metric ton (for US)

# Define functions
# TODO: Update to use function in code/parameter once that's available ### <-Is this a thing? If so, use. If not, either create or comment this function better.
    disaggregate_countries <- function( original_data, aggregate_country, disaggregate_countries, aggregate_end_year,
                                       data_start_year = 1850, id_cols = c( 'iso', 'fuel' ), population ) {

        aggregate_country_data <- original_data[ which( original_data$iso == aggregate_country ), ]
        disaggregate_years <- paste0( 'X', data_start_year:aggregate_end_year )

    # template for extended disaggregate - fill
        disaggregate_extended <- original_data[ which( original_data$iso %in%
                                                       disaggregate_countries ), id_cols ]

    # Driver data - population disaggregate and aggregate_country
    # Part of the ratio - disaggregate population
        disaggregate_population <- disaggregate_extended
        disaggregate_population <- population[ match( disaggregate_population$iso,
                                                     population$iso ),
                                               disaggregate_years ]

    # Part of the ratio - aggregate_country population
        aggregate_country_pop <- population[ which( population$iso %in% disaggregate_countries ),
                                             c( 'iso', disaggregate_years ) ]
        aggregate_country_pop <- rbind( aggregate_country_pop,
                                        c( aggregate_country,
                                           colSums( aggregate_country_pop[ disaggregate_years ] ) ) )
        aggregate_country_pop[ disaggregate_years ] <- sapply( aggregate_country_pop[ disaggregate_years ],
                                                            FUN = as.numeric )
        aggregate_country_population <- disaggregate_extended
        aggregate_country_population[ disaggregate_years ] <-
                        aggregate_country_pop[ match( rep( x = aggregate_country,
                                                           times = nrow( aggregate_country_population ) ),
                                                      aggregate_country_pop$iso ),
                                               disaggregate_years ]

    #TODO: matches by fuel, not robust (will work for this script)
    # multiplyer - aggregate_country CDIAC data
        aggregate_country_cdiac_multiplier <- disaggregate_extended[ id_cols ]
        aggregate_country_cdiac_multiplier[ disaggregate_years ] <-
                        aggregate_country_data[ match( aggregate_country_cdiac_multiplier$fuel,
                                                       aggregate_country_data$fuel ),
                                                disaggregate_years ]

    # Extend Data
        disaggregate_extended[ disaggregate_years ] <-
                                as.matrix( aggregate_country_cdiac_multiplier[ disaggregate_years ] ) *
                                as.matrix( disaggregate_population[ disaggregate_years ] ) /
                                as.matrix( aggregate_country_population[ disaggregate_years ] )

    # add back to full data
        corrected <- replaceValueColMatch( original_data,
                                           disaggregate_extended,
                                           x.ColName = disaggregate_years,
                                           match.x = id_cols,
                                           addEntries = FALSE )

    # remove aggregate_country from final cdiac data to prevent double counting
        corrected <- corrected[ -which( corrected$iso == aggregate_country ), ]
        return( corrected )
    }

# ---------------------------------------------------------------------------
# 2. Process data
# Standardize format, drop rows of NA, short ton to metric ton, etc.
    names( spew ) <- make.names( names( spew ) )
    spew_pre <- group_by( spew_pre, iso ) %>%
            summarise_all( sum )
    spew_pre <- subset( spew_pre, rowSums( spew_pre[, grepl("X", names( spew_pre ) ) ], na.rm = T ) != 0 )  # drop NA rows
    mitchell$units <- NULL
    mitchell$iso[ mitchell$iso == "rus" ] <- "ussr"  # Change rus to ussr
    us$units <- NULL
    us[ grepl( "X", names( us ) ) ] <- us[ grepl( "X", names( us ) ) ] * SHORT_TO_METRIC

# Format population
    un_pop$X_year <- paste0( 'X', un_pop$year)
    un_pop$pop <- as.numeric(un_pop$pop)
    population <- cast( un_pop[which ( un_pop$year %in% historical_pre_extension_year:end_year ) , ] ,
                        iso ~ X_year, value = 'pop')

# Melt data to long format
    spew_long <- melt( spew, id = "iso" ) %>% filter( !is.na( value ), value != 0 )
    names( spew_long ) <- c( "iso", "year", "spew_en" )
    spew_pre_long <- melt( as.data.frame( spew_pre ), id = "iso" ) %>%
      filter( !is.na( value ), value != 0 )
    names( spew_pre_long ) <- c( "iso", "year", "spew_pre_en" )
    mitchell_long <- melt( mitchell, id = "iso" ) %>% filter( !is.na( value ), value != 0 )
    names( mitchell_long ) <- c( "iso", "year", "mitchell_en" )
    us_long <- melt( us, id = "iso" ) %>% filter( !is.na( value ), value != 0 )
    names( us_long ) <- c( "iso", "year", "us_en" )

# Combine all data with priority mitchell > spew > spew_pre (except US where spew > spew_pre > us > mitchell)
    all <- data.frame( year = X_extended_years ) %>%
      merge( data.frame( iso = unique(
        c( us_long$iso, mitchell_long$iso, spew_long$iso, spew_pre_long$iso ) ) ), all = T ) %>%
                                                                   merge( us_long, all = T ) %>%
                                                             merge( mitchell_long, all = T ) %>%
                                                                 merge( spew_long, all = T ) %>%
                                                             merge( spew_pre_long, all = T )
    all$en <- all$mitchell_en
    all$en[ is.na( all$en ) ] <- all$spew_en[ is.na( all$en ) ]
    all$en[ is.na( all$en ) ] <- all$spew_pre_en[ is.na( all$en ) ]
    all$en[ all$iso == "usa" ] <- NA
    all$en[ all$iso == "usa" ] <- all$spew_en[ all$iso == "usa" ]
    all$en[ all$iso == "usa" & is.na( all$en ) ] <- all$spew_pre_en[ all$iso == "usa" & is.na( all$en ) ]
    all$en[ all$iso == "usa" & is.na( all$en ) ] <- all$us_en[ all$iso == "usa" & is.na( all$en ) ]
    all$en[ all$iso == "usa" & is.na( all$en ) ] <- all$mitchell_en[ all$iso == "usa" & is.na( all$en ) ]

# Clean up and cast to wide
    all$sector <- "2C_Metal-production"
    all$fuel <- "process"
    all$units <- "kt"
    all <- dplyr::arrange( all, iso, sector, fuel, units, year )
    all_wide <- cast( all, iso + sector + fuel + units ~ year, value = "en" )
    all_wide$iso <- as.character( all_wide$iso )

# Interpolate NAs
  if (!all_equal(interpolate_NAs(  all_wide[ X_extended_years ]), interpolate_NAs2( all_wide[ X_extended_years ]))) stop()
    all_wide[ X_extended_years ] <- interpolate_NAs( all_wide[ X_extended_years ] )
    all_wide[ is.na( all_wide ) ] <- 0

# Disaggregate countries
    # Czechoslovakia
    iso_csk <- c( 'cze', 'svk' )
    iso_csk_in_data <- iso_csk[ iso_csk %in% all_wide$iso ]
    all_wide_csk <- disaggregate_countries( all_wide, aggregate_country = 'csk',
                                            disaggregate_countries = iso_csk_in_data,
                                            aggregate_end_year = 1991, data_start_year = 1750,
                                            id_cols = c( 'iso', 'sector', 'fuel', 'units' ), population )

    # USSR
    iso_ussr <- c( 'aze', 'arm', 'blr', 'est', 'geo', 'kaz', 'kgz',
                   'lva', 'ltu', 'mda', 'tjk', 'tkm', 'ukr', 'uzb', 'rus')
    iso_ussr_in_data <- iso_ussr[ iso_ussr %in% all_wide_csk$iso ]
    all_wide_ussr <- disaggregate_countries( all_wide_csk, aggregate_country = 'ussr',
                                             disaggregate_countries = iso_ussr_in_data,
                                             aggregate_end_year = 1991, data_start_year = 1750,
                                             id_cols = c( 'iso', 'sector', 'fuel', 'units' ), population )
    # Yug
    iso_yug <- c('bih','hrv','mkd','svn', 'srb','mne')
    iso_yug_in_data <- iso_yug[ iso_yug %in% all_wide_ussr$iso ]
    all_wide_yug <- disaggregate_countries( all_wide_ussr, aggregate_country = 'yug',
                                            disaggregate_countries = iso_yug_in_data,
                                            aggregate_end_year = 1991, data_start_year = 1750,
                                            id_cols = c( 'iso', 'sector', 'fuel', 'units' ), population )

# Remove countries that are all zeroes from full dataset
    Xyears <- names( all_wide )[ grepl( "X", names( all_wide ) ) ]
    all_wide_out <- subset( all_wide_yug, rowSums( all_wide_yug[ Xyears ] ) > 0 )

# Make driver dataset of 1750-1975; remove countries that are all zero from 1750-1970
    Xyears_filter <- paste0( "X", 1750:1970 )
    driver <- select( all_wide_out, iso, sector, fuel, units, X1750:X1975 )
    driver <- subset( driver, rowSums( driver[ Xyears_filter ] ) > 0 )

# ---------------------------------------------------------------------------
# 3. Output
    writeData( driver, "EXT_IN", "A.Pig_Iron_Production", domain_extension = "extension-data/" )
    writeData( all_wide_out, "DIAG_OUT", "A.Pig_Iron_Production_full", meta = F )

logStop()
