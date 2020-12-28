# ------------------------------------------------------------------------------
# Program Name: E.Andrew_emissions.R
# Author(s): Rachel Hoesly, Linh Vu, Patrick O'Rourke, Hamza Ahsan
# Date Last Updated: December 3, 2020
# Program Purpose: To read in & reformat Andrew cement CO2 emissions data.
# Input Files: A.UN_pop_master.csv, Andrew_cement_1900_2018.csv, Andrew_country_map.csv
#              Master_Country_List,csv
# Output Files: E.CO2_Andrew_Cement.csv
#
# TODO: Check if disagg_iso_with_population can be used in other places.
#       At some point, convert disaggregate_country to dplyr.

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R" ) # Any additional function files required
    log_msg <- "Read and format Andrew cement CO2 emissions" # First message to be printed to the log
    script_name <- "E.Andrew_emissions.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# -----------------------------------------------------------------------------------------------------------
# 0.5 Define Functions

# Define function to replicate a row, x, n times and convert to a matrix
    rep.row <- function( x, n ){ matrix( rep( x, each = n ), nrow = n ) }

# -----------------------------------------------------------------------------------------------------------
# 1. Read in files

# Andrew cement inventory
    Andrew_read <- readData( 'EM_INV', domain_extension = "CDIAC/",
                             'Andrew_cement_1900_2018', check.names = FALSE)

# Mapping files
    MCL <- readData( "MAPPINGS", "Master_Country_List" )
    Andrew_country_map <- readData( 'EM_INV', domain_extension = "CDIAC/",
                                    'Andrew_country_map' )

# Set any negative values to 0
    Andrew_read[ Andrew_read < 0 ] <- 0

    # Find all countries with no isos
    unmatched_Andrew_countries <- dplyr::filter( Andrew_country_map, is.na( iso ) ) %>%
      dplyr::select( Andrew ) %>%
      unique()
    unmatched_Andrew_countries[] <- lapply(unmatched_Andrew_countries, as.character)

# Print a warning if some countries are not in the CEDS country list
    if (nrow(unmatched_Andrew_countries) > 0) {
      for (i in 1:nrow(unmatched_Andrew_countries)) {
        if (unname(colSums(Andrew_read[unmatched_Andrew_countries[[1]][i]]) == 0)){
                warning( paste0( "The following country does not correpond to a CEDS country, but has no data:\n",
                                 paste(unmatched_Andrew_countries[[1]][i], collapse = ", " ) ) )
        }
        else if (unname(colSums(Andrew_read[unmatched_Andrew_countries[[1]][i]]) > 0)){
          warning( paste0( "The following country does not correpond to a CEDS country, but has data:\n",
                           paste(unmatched_Andrew_countries[[1]][i], collapse = ", " ) ) )
        }
      }
    }

# UN population from Module A
    un_pop <- readData( "MED_OUT", 'A.UN_pop_master' )

# -----------------------------------------------------------------------------------------------------------
# 2. Formatting Data to ceds format
    final_units <- 'kt-C'

# Process UN population
    un_pop$X_year <- paste0( "X", un_pop$year )
    un_pop$pop <- as.numeric( un_pop$pop )

# Cast to wide by year
    population <-
          cast( un_pop[ which( un_pop$year %in%
                               historical_pre_extension_year:end_year ), ],
                iso ~ X_year, value = 'pop' )

# Process Andrew data
    Andrew_cement_wide <- Andrew_read
    Andrew_cement_wide$Global <- NULL  #Remove 'Global' column

# Cast Andrew to long
    Andrew_long <- gather(Andrew_cement_wide, Country, cement_production, names(Andrew_cement_wide)[2]:names(Andrew_cement_wide)[length(names(Andrew_cement_wide))])

# Create a units row
    Andrew_long$units <- final_units

# Create an Xyear row
    Andrew_long$X_year <- paste0( 'X', Andrew_long$Year )

# Make values numeric
    Andrew_long <- Andrew_long %>%
      dplyr::mutate_at( c( 'Year', 'cement_production' ), as.numeric )

# Add iso
    Andrew_long$iso <- Andrew_country_map[ match( Andrew_long$Country,
                                                     Andrew_country_map$Andrew ),
                                              'iso' ]

# Aggregate cement_production by iso/year/unit
    Andrew_long <- aggregate( Andrew_long[ "cement_production" ],
                                  by = list( iso = Andrew_long$iso,
                                             year = Andrew_long$Year,
                                             X_year = Andrew_long$X_year,
                                             units = Andrew_long$units ),
                                  FUN = sum, na.rm = T )

# Reshape to standard ceds format, select post 1850 years
    Andrew_long <- melt( Andrew_long,
                        id = c( 'iso', 'year', 'X_year', 'units' ) )
    names( Andrew_long )[ which( names( Andrew_long ) == 'variable' ) ] <- 'fuel'

# Cast to wide
    Andrew_wide <- cast( Andrew_long, iso + fuel + units ~ X_year ) %>% as.data.frame()
    Andrew_wide[ is.na( Andrew_wide ) ] <- 0

# ------------------------------------------------------------------------------
# 3. Extend Andrew to 1750

# Force fuel to character
    Andrew_wide$fuel <- as.character( Andrew_wide$fuel )

# Extend Andrew data back to 1750
    extended_Andrew_years_back <- paste0('X', 1750: (Andrew_start_year-1))
    Andrew_wide[,extended_Andrew_years_back] <- 0

# Extend Andrew data forward to end_year
    extended_Andrew_years_forward <- paste0('X', (Andrew_end_year+1):end_year)
    Andrew_wide[,extended_Andrew_years_forward] <- Andrew_wide[,paste0('X', Andrew_end_year)]

# -----------------------------------------------------------------------------
# 4. Split Countries

# This section is for isos which have Andrew data provided for certain
# time periods, but in other time periods their data is within an aggregated iso.

# For each country, use the disaggregate_country function defined in
# data_functions.R to split aggregate country fuels. Use population breakdown
# to split data.

# French Equatorial Africa
    Andrew_FrEqAf_wide <-
      disaggregate_country( original_data = Andrew_wide,
                            id_cols = c( 'iso', 'fuel' ),
                            trend_data = population,
                            trend_match_cols = 'iso',
                            combined_iso = 'FRENCH_EQUATORIAL_AFRICA',
                            dis_end_year = 1958,
                            disaggregate_iso = c( 'caf', 'cog',
                                                  'gab', 'tcd' )  ,
                            allow_dropped_data = T )

# French West Africa
    Andrew_FrWeAf_wide <-
      disaggregate_country( original_data = Andrew_FrEqAf_wide,
                            id_cols = c( 'iso', 'fuel' ),
                            trend_data = population,
                            trend_match_cols = 'iso',
                            combined_iso = 'FRENCH_WEST_AFRICA',
                            method = 2,
                            dis_end_year = 1957,
                            disaggregate_iso = c( 'mrt', 'sen', 'mli',
                                                  'gin', 'civ', 'bfa',
                                                  'ben', 'ner' ) )

# Leeward Islands
    Andrew_LI_wide <-
      disaggregate_country( original_data = Andrew_FrWeAf_wide,
                            id_cols = c( 'iso', 'fuel' ),
                            trend_data = population,
                            trend_match_cols = 'iso',
                            combined_iso = 'LEEWARD ISLANDS',
                            dis_end_year = 1956,
                            dis_start_year = 1950,
                            disaggregate_iso = c( 'kna', 'atg' ),
                            allow_dropped_data = T )

# -----------------------------------------------------------------------------------------------------------
# 5. Split additional isos

# This section is for isos which do not have Andrew data provided directly in any
# Andrew time period, but have their data contained within an aggregated Andrew iso.
# Downscaling to these isos will be done with UN population data.
    extended_years_with_Xs <- paste0('X', historical_pre_extension_year:end_year)

# Cast Andrew to long format
    Andrew_LI_long <- Andrew_LI_wide %>%
      tidyr::gather( key = "Years", value = "Emissions", extended_years_with_Xs )

# Fix Serbia data - disaggregate to srb and srb (Kosovo)
    agg_region_disagg_isos <- c( "srb (kosovo)" )
    agg_region <- "srb"

    Andrew_split_srb_Fixed <- disagg_iso_with_population ( agg_region, agg_region_disagg_isos,
                                                           Andrew_LI_long, population, extended_years_with_Xs )

# Cast back into wide format
    Andrew_split_final <- Andrew_split_srb_Fixed %>%
      tidyr::spread( Years, Emissions )

# -----------------------------------------------------------------------------------------------------------
# 6. Add zero values for nations too small for Andrew
#    Add blank entries for any countries not in Andrew

# Define countries to add to Andrew data
    non_Andrew_countries <- MCL$iso[ MCL$iso %!in%
                                      unique( Andrew_split_final$iso ) ]

# Only keep countries with 1 for the final_data_flag in the MCL, Kosovo, and Guam
# TODO: when Kosovo and Guam issue in Master Country List is resolved, the call to their isos below can be removed
    non_Andrew_countries <-
      non_Andrew_countries[ non_Andrew_countries %in%
                             MCL[ which( MCL$final_data_flag == 1 | MCL$iso %in% c( "srb (kosovo)", "gum" ) ), 'iso' ] ]

    non_Andrew_countries <- unique( non_Andrew_countries )

# Drop "global" country
# TODO: These provides 0 values for the following isos: esh, tkl. Look into whether or not these isos are contained
#       within an aggregate Andrew iso that could be broken out with UN population data
    non_Andrew_countries <- non_Andrew_countries[ non_Andrew_countries %!in%
                                                  'global' ]

# Create a df with all iso/fuel combos to add
    add_zeros_andrew <- data.frame( iso = rep( non_Andrew_countries,
                                        each = length( "cement_production" ) ),
                             fuel = rep( "cement_production",
                                         times = length( non_Andrew_countries ) ),
                             units = final_units )

    add_zeros_andrew[ extended_years_with_Xs ] <- 0

# Combine needed rows to Andrew, so even countries w/o data are represented
    Andrew_disaggregated <- dplyr::bind_rows( add_zeros_andrew, Andrew_split_final )

# Clean and reformat
    Andrew_disaggregated$units <- final_units
    Andrew_final <- arrange_( Andrew_disaggregated,
                                     c( 'iso', 'fuel', 'units',
                                        extended_years_with_Xs ) )
    Andrew_final <- Andrew_final[ , c( 'iso', 'fuel', extended_years_with_Xs ) ]

# -----------------------------------------------------------------------------------------------------------
# 7. Output

# Intermediate output
    writeData( Andrew_final, domain = "MED_OUT",
               fn = paste0( "E.CO2_Andrew_Cement" ) )

    logStop()

# END
