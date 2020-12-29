# ------------------------------------------------------------------------------
# Program Name: A5.2.add_NC_activity_fossil_fuel_production.R
# Author(s): Hamza Ahsan
# Date Last Modified: December 18, 2020
# Program Purpose: Gather historical oil production data from IEA and Hyde crude
#                  oil production data sets
# Input Files: Master_Country_List.csv, oil_1800-1990.xls, A.en_stat_sector_fuel.csv,
# Output Files: A.crude_oil_production_driver_data, A.IEA_Hyde_average_1971-1973
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

# ------------------------------------------------------------------------------

# 1. Define settings and script constants

# IEA oil production years
IEA_EXT_TO_BP_END_YEAR <- paste0( "X", ( IEA_end_year + 1 ): BP_actual_last_year )
IEA_YEARS_x <- paste0( "X", IEA_start_year : BP_actual_last_year )
extended_IEA_back <- paste0('X', 1800: (IEA_start_year-1))

# Hyde oil production years historical extension (overlap IEA data for 1971-1973)
# to allow for scaling up Hyde Bahrain and for diagnostic output of ratios for all isos
Hyde_years_X <- paste0('X', 1800:1973)

# Combined Hyde and IEA years
Hyde_IEA_years_X <- paste0('X', 1800:end_year)

# Define aggregate Former Soviet Union (FSU) iso and subregional FSU isos. Used for disaggregating fossil fuel production data
fsu <- 'ussr'
fsu_members <- c( "arm", "aze", "blr", "est", "geo", "kaz", "kgz", "ltu", "lva", "mda", "rus", "tjk", "tkm", "ukr", "uzb" )

# Define aggregate Czechoslovakia iso and subregional isos.
czechosl <- "csk"
czechosl_members <- c( 'cze', 'svk' )

# Define aggregate Yugoslavia iso and subregional isos.
yugosl <- "yug"
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
oil_1800_1990 <- readData( "ENERGY_IN","Hyde_oil_1800-1990", ".xls", sheet_selection = "country data" )

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

# Extract FSU countries from Hyde oil data
Hyde_fsu <- Hyde_oil_wide %>%
    dplyr::filter( iso %in% fsu )

# Reformat IEA data and calculate percent of total ussr oil production for each
# disaggregated iso
IEA_fsu_members <- IEA_oil %>%
    dplyr::filter( iso %in% c( fsu, fsu_members ) ) %>%
    mutate(total = X1971 + X1972 + X1973) %>%
    mutate(avg_per = total/sum(total))

# Extend IEA data back to 1800 (start of Hyde data)
IEA_fsu_members[,extended_IEA_back] <- 0

# Merge Hyde with IEA and disaggregate ussr data from 1800 to 1970
Hyde_IEA_fsu_split <- dplyr::bind_rows( Hyde_fsu, IEA_fsu_members ) %>%
    dplyr::filter(iso != fsu)

Hyde_1800_1970 <- Hyde_fsu %>%
    dplyr::select(c("X1800":"X1970"))

# Calculate fraction of total ussr oil production for disaggregate isos
for (i in 1:(1970-1800+1)) {
    Hyde_IEA_fsu_split[,(i+2)] <- Hyde_IEA_fsu_split$avg_per * Hyde_1800_1970[,i]
}

# Remove last two rows that were used to calculate percent of total ussr oil production
Hyde_IEA_fsu_split <- Hyde_IEA_fsu_split %>%
    dplyr::select( -c(total, avg_per) )

# Disaggregate Hyde's Czechoslovakia data (1914-1970) using IEA trend for last 3 years (1971-1973).
# Czechoslovakia member countries are not listed in raw Hyde data, but total production
# is reported. Here the code splits Czechoslovakia and distributes to member countries.

# Extract Czechoslovakia countries from Hyde oil data
Hyde_csk <- Hyde_oil_wide %>%
    dplyr::filter( iso %in% czechosl )

# Reformat IEA data and calculate percent of total csk oil production for each disaggregated iso
IEA_csk_members <- IEA_oil %>%
    dplyr::filter( iso %in% c( czechosl, czechosl_members ) ) %>%
    mutate(total = X1971 + X1972 + X1973) %>%
    mutate(avg_per = total/sum(total))

# Extend IEA data back to 1800 (start of Hyde data)
IEA_csk_members[,extended_IEA_back] <- 0

# Merge Hyde with IEA and disaggregate csk data from 1800 to 1970
Hyde_IEA_csk_split <- dplyr::bind_rows( Hyde_csk, IEA_csk_members ) %>%
    dplyr::filter(iso != czechosl)

Hyde_1800_1970 <- Hyde_csk %>%
    dplyr::select(c("X1800":"X1970"))

# Calculate fraction of total csk oil production for disaggregate isos
for (i in 1:(1970-1800+1)) {
    Hyde_IEA_csk_split[,(i+2)] <- Hyde_IEA_csk_split$avg_per * Hyde_1800_1970[,i]
}

# Remove last two rows that were used to calculate percent of total csk oil production
Hyde_IEA_csk_split <- Hyde_IEA_csk_split %>%
    dplyr::select( -c(total, avg_per) )

# Disaggregate Hyde's Yugoslavia data (1933-1970) using IEA trend for last 3 years (1971-1973).
# Yugoslavia member countries are not listed in raw Hyde data, but total production
# is reported. Here the code splits Yugoslavia and distributes to member countries.

# Extract Yugoslavia countries from Hyde oil data
Hyde_yug <- Hyde_oil_wide %>%
    dplyr::filter( iso %in% yugosl )

# Reformat IEA data and calculate percent of total csk oil production for each disaggregated iso
IEA_yug_members <- IEA_oil %>%
    dplyr::filter( iso %in% c( yugosl, yugosl_members ) ) %>%
    dplyr::mutate(total = X1971 + X1972 + X1973) %>%
    dplyr::mutate(avg_per = total/sum(total))

# Extend IEA data back to 1800 (start of Hyde data)
IEA_yug_members[,extended_IEA_back] <- 0

# Merge Hyde with IEA and disaggregate csk data from 1800 to 1970
Hyde_IEA_yug_split <- dplyr::bind_rows( Hyde_yug, IEA_yug_members ) %>%
    dplyr::filter(iso != yugosl)

Hyde_1800_1970 <- Hyde_yug %>%
    dplyr::select(c("X1800":"X1970"))

# Calculate fraction of total yug oil production for disaggregate isos
for (i in 1:(1970-1800+1)) {
    Hyde_IEA_yug_split[,(i+2)] <- Hyde_IEA_yug_split$avg_per * Hyde_1800_1970[,i]
}

# Remove last two rows that were used to calculate percent of total yug oil production
Hyde_IEA_yug_split <- Hyde_IEA_yug_split %>%
    dplyr::select( -c(total, avg_per) )

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

Hyde_oil_wide[which(Hyde_oil_wide$iso == "bhr"), Hyde_years_X] <- Hyde_oil_wide[which(Hyde_oil_wide$iso == "bhr"), Hyde_years_X] * IEA_bhr_1971_1973$avg/Hyde_bhr_1971_1973$avg

# Replace IEA Israel data from 1971 to 1975 with Hyde data
Israel_blip_years <- paste0('X', 1971:1975)
IEA_oil[which(IEA_oil$iso == "isr"), Israel_blip_years] <- Hyde_oil_wide[which(Hyde_oil_wide$iso == "isr"), Israel_blip_years]

# Drop overlapping years from Hyde (1971-1975)
Hyde_oil_final <- Hyde_oil_wide %>%
    dplyr::select(-c(X1971:X1975)) %>%
    dplyr::filter(!(iso %in% c(fsu, czechosl, yugosl)))

# Combine Hyde with original IEA raw data and disaggregated ussr, yug, csk
Hyde_IEA_oil <- IEA_oil %>%
    dplyr::select(-c(X1960:X1970)) %>%
    dplyr::filter(!(iso %in% c(fsu_members, yugosl_members, czechosl_members))) %>%
    dplyr::full_join(Hyde_oil_final, by = c("iso", "units")) %>%
    dplyr::bind_rows(Hyde_IEA_fsu_split, Hyde_IEA_csk_split, Hyde_IEA_yug_split) %>%
    replace(is.na(.), 0)

# Sort and organize
Hyde_IEA_oil$activity <- "crude_oil_production"
replace <- Hyde_IEA_oil[ , c( 'iso', 'activity', 'units', Hyde_IEA_years_X ) ]

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
if( activityCheck( replace, check_all = FALSE ) ) {
    addToActivityDb( replace )
}

# ------------------------------------------------------------------------------

# 5. Output

# Oil Production Driver Data
writeData( replace, domain = "MED_OUT", fn = paste0( "A.crude_oil_production_driver_data"  ) )

# IEA to Hyde ratios of the average oil production from 1971 to 1973
writeData( IEA_Hyde_avg, domain = "MED_OUT", fn = paste0( "A.IEA_Hyde_average_1971-1973" ) )

logStop( )
# END
