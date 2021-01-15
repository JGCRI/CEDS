# ------------------------------------------------------------------------------
# Program Name: A5.2.add_NC_activity_fossil_fuel_production.R
# Author(s): Hamza Ahsan
# Date Last Modified: January 11, 2021
# Program Purpose: Gather historical oil production data from IEA and Hyde crude
#                  oil production data sets
# Input Files: Master_Country_List.csv, oil_1800-1990.xls, A.en_stat_sector_fuel.csv,
# Output Files: A.crude_oil_production_data, A.IEA_Hyde_average_1971-1973
# TODO:

# -----------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if( "input" %in% dir( ) ) "code/parameters/" else "../code/parameters/"

# Universal header file - provides logging, file support, etc.
headers <- c( 'process_db_functions.R', 'data_functions.R', 'common_data.R',
              'analysis_functions.R', "timeframe_functions.R" )

# First message to be printed to the log
log_msg <- paste0( "Gathering IEA and Hyde crude oil production data..." )

script_name <- "A5.2.add_NC_activity_fossil_fuel_production.R"

source( paste0( PARAM_DIR, "header.R" ) )

initialize( script_name, log_msg, headers )

# disaggregate_country_trend
# Brief:    Generates a trend using a ratio of the average of the last three years (i.e., 1971-1973)
#           of activity (i.e., oil production) of the disaggregate isos. This is an alternative
#           trend to population for the disaggregate_country function and allows for a constant
#           split from trend_start_year to trend_end_year.
# Details:
# Dependencies: None
# Author(s):
# Params:   data_set -          data set to build trend from (i.e., IEA oil production data)
#           disagg_iso_list -   list of disaggregate isos for particular iso being split
#           trend_start_year -  start year for trend
#           trend_end_year -    end year for trend
#           first_yr -          first non-zero data point (i.e., 1971)
#           second_yr -         second non-zero data point (i.e., 1972)
#           third_yr -          third non-zero data point (i.e., 1973)
# Return:   constant trend for disaggregate isos from start year to end year
# Input Files:  None
# Output Files: None

disaggregate_country_trend <- function(data_set, disagg_iso_list, trend_start_year, trend_end_year,
                                       first_yr, second_yr, third_yr){

    # Reformat data_set (i.e., IEA) and calculate percent of total activity
    # (i.e., oil production) for each disaggregated iso
    disagg_iso <- data_set %>%
        dplyr::filter(iso %in% disagg_iso_list)
    disagg_iso$total <- disagg_iso[[first_yr]] + disagg_iso[[second_yr]] + disagg_iso[[third_yr]]
    disagg_iso <- disagg_iso %>%
        dplyr::mutate(avg_per = total/sum(total))

    # Define trend data for disaggregate isos
    disagg_trend <- dplyr::data_frame(iso = disagg_iso$iso)
    disagg_trend[,paste0('X', trend_start_year:trend_end_year)] <- 1
    final_trend <- disagg_trend %>%
        gather(variable, value, -iso) %>%
        group_by(iso) %>%
        mutate(value = value * disagg_iso$avg_per) %>%
        spread(variable, value) %>%
        ungroup()

    # Add isos to trend that appear in disagg_iso_list but not in data_set to match
    # the list of isos the disaggregate_country function splits by
    if (length(disagg_iso_list) > length(disagg_trend$iso)){
        final_trend <- final_trend %>%
          add_row(iso=c(disagg_iso_list[which(!(disagg_iso_list %in% disagg_trend$iso))]))
    }
    final_trend <- final_trend %>%
        replace(is.na(.), 0) %>%
        as.data.frame()

    return(final_trend)
}

# ------------------------------------------------------------------------------

# 1. Define settings and script constants

# IEA oil production years
IEA_EXT_TO_BP_END_YEAR <- paste0( "X", ( IEA_end_year + 1 ): BP_actual_last_year )
IEA_YEARS_x <- paste0( "X", IEA_start_year : BP_actual_last_year )
extended_IEA_back <- paste0('X', 1800: (IEA_start_year-1))

# Define years to scale up Hyde oil production for Bahrain
bhr_scale_years <- paste0('X', 1800:1970)

# Combined Hyde and IEA years
Hyde_IEA_years_X <- paste0('X', 1800:end_year)

# Define aggregate Former Soviet Union (FSU) iso and subregional FSU isos. Used for disaggregating fossil fuel production data
fsu_members <- c( "arm", "aze", "blr", "est", "geo", "kaz", "kgz", "ltu", "lva", "mda", "rus", "tjk", "tkm", "ukr", "uzb" )

# Define aggregate Czechoslovakia iso and subregional isos.
czechosl_members <- c( 'cze', 'svk' )

# Define aggregate Yugoslavia iso and subregional isos.
yugosl_members <- c( 'bih', 'hrv', 'mkd', 'svn', 'scg', 'srb', 'mne' )

# ------------------------------------------------------------------------------

# 2. Input

# Master Country List
MCL <- readData( domain = 'MAPPINGS', file_name = 'Master_Country_List' )

# IEA crude oil production data
# TODO: Production data in this file has the value 0 for some year + iso combinations where the data should likely be NA.
# For example, Norway has 0 production in 1970, but nearly 300kt of production in 1971.
# This should be fixed before this script.
en_stat_sector_fuel <- readData( 'MED_OUT', file_name = 'A.en_stat_sector_fuel' )

# Hyde crude oil production data
oil_1800_1990 <- readData( "ENERGY_IN","Hyde_oil_1800-1975", ".xls", sheet_selection = "country data" )

# CDIAC oil and liquid CO2 emissions data
CDIAC_oil_liquid <- readData( 'MED_OUT', file_name = 'E.CO2_CDIAC_liquid_and_gas' )

# ------------------------------------------------------------------------------

# 3. Reformat mapping files

#   Make list of unique final isos (isos with final_data_flag = 1, srb (kosovo),
#   and gum) and list of final isos with OECD vs Non-OECD flag
MCL_clean <- MCL %>%
    dplyr::select( iso, final_data_flag,  OECD_flag ) %>%
    dplyr::distinct( ) %>%
    dplyr::filter( final_data_flag == 1 | iso %in% c( "srb (kosovo)", "gum" ) ) %>%
    dplyr::filter( iso != "global" )

MCL_unique_with_OECD_flag <- MCL_clean %>%
    dplyr::select( -final_data_flag )

# ------------------------------------------------------------------------------

# 4. Process IEA and Hyde oil production data

# Cleanup IEA oil production data
# Extend final IEA oil production year (2017) to final BP year (actual BP end year, using constant extension)
IEA_oil <- en_stat_sector_fuel %>%
    dplyr::filter( sector == "crude-oil-production" ) %>%
    dplyr::left_join( MCL_unique_with_OECD_flag, by = "iso" ) %>%
    dplyr::mutate_at( .vars = IEA_EXT_TO_BP_END_YEAR, funs( + ( !!rlang::sym( X_IEA_end_year ) ) ) ) %>%
    dplyr::select(iso, units, all_of(IEA_YEARS_x) )

# Cleanup Hyde oil production data
Hyde_oil <- oil_1800_1990
names(Hyde_oil) <- Hyde_oil[3,] # Set data frame header to isos
colnames(Hyde_oil)[1] <- "year"
Hyde_oil <- Hyde_oil[!is.na(names(Hyde_oil))] %>%
    dplyr::select(c("year":"japan")) %>%
    dplyr::slice(5:180) %>% # Extract time series of 1800-1975 to capture Israel blip
    replace(is.na(.), 0)
Hyde_oil <- as.data.frame(sapply(Hyde_oil, as.numeric))

# Cast Hyde to long
Hyde_oil_long <- tidyr::gather(Hyde_oil, country, oil_production, names(Hyde_oil)[2]:names(Hyde_oil)[length(names(Hyde_oil))])

# Create a units row
Hyde_units <- 'kt'
Hyde_oil_long$units <- Hyde_units

# Create an Xyear row
Hyde_oil_long$X_year <- paste0( 'X', Hyde_oil_long$year )

# Add iso
Hyde_oil_long$iso <- MCL[ match( Hyde_oil_long$country,
                                              MCL$HydeName ),
                                       'iso' ]

# Aggregate oil_production by iso/year/unit
Hyde_oil_long <- aggregate( Hyde_oil_long[ "oil_production" ],
                          by = list( iso = Hyde_oil_long$iso,
                                     year = Hyde_oil_long$year,
                                     X_year = Hyde_oil_long$X_year,
                                     units = Hyde_oil_long$units ),
                          FUN = sum, na.rm = T )

# Reshape to standard CEDS format
Hyde_oil_long <- melt( Hyde_oil_long,
                     id = c( 'iso', 'year', 'X_year', 'units' ) )

# Cast to wide
Hyde_oil_wide <- cast( Hyde_oil_long, iso + units ~ X_year, value = "oil_production" ) %>% as.data.frame()

# Disaggregate Hyde's FSU data (from 1860-1970 disaggregate FSU to FSU members) using IEA trend
# for last 3 years (1971-1973).FSU member countries are not listed in raw Hyde data, but total production
# is reported for FSU. Here the code splits the ussr( fsu ) and distributes to fsu member countries.

fsu_trend <- disaggregate_country_trend(IEA_oil, fsu_members,'1750','2019',
                                        'X1971', 'X1972', 'X1973')

# FSU
Hyde_oil_wide_fsu_split <-
    disaggregate_country( original_data = Hyde_oil_wide,
                          trend_data = fsu_trend,
                          trend_match_cols = 'iso',
                          combined_iso = 'ussr',
                          dis_start_year = 1800,
                          dis_end_year = 1970,
                          disaggregate_iso = c( 'aze', 'arm', 'blr',
                                                'est', 'geo', 'kaz',
                                                'kgz', 'lva', 'ltu',
                                                'mda', 'tjk', 'tkm',
                                                'ukr', 'uzb', 'rus' ),
                          write_over_values = T )

# Disaggregate Hyde's Czechoslovakia data (1914-1970) using IEA trend for last 3 years (1971-1973).
# Czechoslovakia member countries are not listed in raw Hyde data, but total production
# is reported. Here the code splits Czechoslovakia and distributes to member countries.

csk_trend <- disaggregate_country_trend(IEA_oil, czechosl_members, '1750', '2019',
                                        'X1971', 'X1972', 'X1973')

# Czechoslovakia
Hyde_oil_wide_csk_split <-
    disaggregate_country( original_data = Hyde_oil_wide_fsu_split,
                          trend_data = csk_trend,
                          trend_match_cols = 'iso',
                          combined_iso = 'csk',
                          dis_start_year = 1800,
                          dis_end_year = 1970,
                          disaggregate_iso = c( 'cze', 'svk' ),
                          write_over_values = T )

# Disaggregate Hyde's Yugoslavia data (1933-1970) using IEA trend for last 3 years (1971-1973).
# Yugoslavia member countries are not listed in raw Hyde data, but total production
# is reported. Here the code splits Yugoslavia and distributes to member countries.

yug_trend <- disaggregate_country_trend(IEA_oil, yugosl_members, '1750', '2019',
                                        'X1971', 'X1972', 'X1973')

# Yugoslavia
Hyde_oil_wide_yug_split <-
    disaggregate_country( original_data = Hyde_oil_wide_csk_split,
                          trend_data = yug_trend,
                          trend_match_cols = 'iso',
                          combined_iso = 'yug',
                          dis_start_year = 1800,
                          dis_end_year = 1970,
                          disaggregate_iso = c( 'bih', 'hrv', 'mkd',
                                                'svn', 'scg', 'srb',
                                                'mne'),
                          write_over_values = T )

# Merge Hyde and IEA with disaggregated data. Remove overlapping years.
Hyde_IEA_split <- left_join(select(IEA_oil, -c(X1960:X1970)), select(Hyde_oil_wide_fsu_split, -c(X1971:X1975)), by = c("iso", "units")) %>%
    left_join(., select(Hyde_oil_wide_csk_split, -c(X1971:X1975)), by = c("iso", "units")) %>%
    left_join(., select(Hyde_oil_wide_yug_split, -c(X1971:X1975)), by = c("iso", "units")) %>%
    select(iso, units, all_of(Hyde_IEA_years_X)) %>%
    replace(is.na(.), 0)

# Scale up Bahrain Hyde data by ratio of average IEA data from 1971-1973 to
# corresponding average of Hyde data
IEA_bhr_1971_1973 <- IEA_oil %>%
    dplyr::filter(iso == "bhr") %>%
    dplyr::select( c(X1971:X1973) ) %>%
    dplyr::mutate(avg = mean(c(X1971, X1972, X1973)))

Hyde_bhr_1971_1973 <- Hyde_oil_wide %>%
    dplyr::filter(iso == "bhr") %>%
    dplyr::select( c(X1971:X1973) ) %>%
    dplyr::mutate(avg = mean(c(X1971, X1972, X1973)))

Hyde_IEA_split[which(Hyde_IEA_split$iso == "bhr"), bhr_scale_years] <- Hyde_IEA_split[which(Hyde_IEA_split$iso == "bhr"), bhr_scale_years] * IEA_bhr_1971_1973$avg/Hyde_bhr_1971_1973$avg

# Replace IEA Israel data from 1971 to 1975 with Hyde data
Israel_blip_years <- paste0('X', 1971:1975)
Hyde_IEA_split[which(Hyde_IEA_split$iso == "isr"), Israel_blip_years] <- Hyde_oil_wide[which(Hyde_oil_wide$iso == "isr"), Israel_blip_years]

# sort and organize
Hyde_IEA_split$activity <- "crude_oil_production"
Hyde_IEA_oil <- Hyde_IEA_split[, c('iso', 'activity', 'units', Hyde_IEA_years_X)]

# Generate hybrid extension data set for fugitive petroleum emissions based on
# oil production and CDIAC oil and liquid CO2

# Extract isos with oil production for the years 1970-1975
oil_prod_non_zero <- Hyde_IEA_oil %>%
    dplyr::select(c(iso, X1970:X1975)) %>%
    dplyr::mutate(sum = rowSums(.[2:7])) %>%
    dplyr::filter(sum != 0)

# Remove corresponding isos from CDIAC oil and liquid
CDIAC_match <- CDIAC_oil_liquid %>%
    dplyr::filter(!(iso %in% oil_prod_non_zero$iso)) %>%
    dplyr::select(-c(fuel))

# Combine oil production with CDIAC oil and liquid
oil_prod_CDIAC <- Hyde_IEA_oil %>%
    dplyr::filter(iso %in% oil_prod_non_zero$iso) %>%
    dplyr::bind_rows(CDIAC_match) %>%
    tidyr::fill(activity) %>%
    dplyr::select(iso, activity, units, all_of(X_extended_years)) %>%
    dplyr::arrange(iso) %>%
    replace(is.na(.), 0)

# Calculate average of 1971-1973 from IEA and Hyde to check consistency
IEA_1971_1973 <- IEA_oil %>%
    dplyr::select( c(iso, X1971:X1973) ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(avg_IEA = mean(c(X1971, X1972, X1973))) %>%
    dplyr::select(c(iso, avg_IEA))

Hyde_1971_1973 <- Hyde_oil_wide %>%
    dplyr::select( c(iso, X1971:X1973) ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(avg_Hyde = mean(c(X1971, X1972, X1973))) %>%
    dplyr::select(c(iso, avg_Hyde))

IEA_Hyde_avg <- dplyr::left_join(IEA_1971_1973,Hyde_1971_1973, by = "iso")

# Add reformatted activity_data to the activity database, extending or truncating it as necessary.
# By default, it will be extended forward to the common end year, but not backwards.
# Only do this if the activityCheck header function determines that the activities in
# the reformatted activity_data are all present in the Master List.
if( activityCheck( Hyde_IEA_oil, check_all = FALSE ) ) {
    addToActivityDb( Hyde_IEA_oil )
}

# ------------------------------------------------------------------------------

# 5. Output

# Oil Production Data
writeData( Hyde_IEA_oil, domain = "MED_OUT", fn = paste0( "A.crude_oil_production_data"  ) )

# Oil Production Extension Data
writeData( oil_prod_CDIAC, domain = "MED_OUT", fn = paste0( "A.crude_oil_production_extension_data"  ) )
meta_names <- c( "Data.Type", "Emission", "Region", "Sector", "Start.Year",
                 "End.Year", "Source.Comment" )
meta_note <- c( "Extension Driver", "All", "All", "1B2_Fugitive-petr",
                1800, "1969", paste( "For petroleum producing countries extend by crude oil production.",
                                     "For all other countries use CDIAC oil + gas CO2 emissions" ) )
source_info <- script_name
addMetaData( meta_note, meta_names, source_info )

# IEA to Hyde ratios of the average oil production from 1971 to 1973
writeData( IEA_Hyde_avg, domain = "MED_OUT", fn = paste0( "A.IEA_Hyde_average_1971-1973" ) )

logStop( )
# END
