#------------------------------------------------------------------------------
# Program Name: A2.3.IEA_FSU_energy_fix.R
# Author: Rachel Hoesly, Steve Smith, Patrick O'Rourke
# Date Last Modified: May 29, 2020
# Program Purpose: Fix Former Soviet Union energy data. The breakup of the FSU
#                  creates challenges for historical extension and for creating
#                  continuous, disaggregated emissions for countries whose
#                  borders change.
#
#                  This script uses a series of FSU-specific processes to determine
#                  the breakdown of FSU energy consumption data during the Soviet era among the
#                  nation's constituent countries. The main methods employed are
#                  calculations of share percentages and extension from other trends,
#                  though many techniques are used including country/sector/fuel-level
#                  data adjustment.
# Details: Disaggregate total USSR data:
#           1. by fuel to FSU country according to bp data
#           2. for each FSU country to aggregate fuel based on CEDS data
#           3. for each FSU country and aggregate fuel to aggregate sectors by CEDS data
#           4. blend dissaggregate data with CEDS data (post 1990)
#           5. make hard coded changes (category reporting)
# Input Files: A.en_biomass_fix.csv, Master_Country_List.csv,  Master_Sector_Level_map.csv,
#              Master_Fuel_Sector_List.xlsx, A.UN_pop_master.csv, [BP_data_file_name].xlsx
# Output Files: A.en_biomass_fsu_fix.csv, FSU_corrected_summary.csv, A.FSU_energy_corrected_summary.csv,
#               A.FSU_energy_corrected_summary_before_dissaggregation.csv,
#               A.FSU_energy_corrected_summary_after_coal_dissaggregation.csv
# Notes:
# TODO: Seperate into seperate scripts
# TODO: Script TODOs, replace various functions (melt, cast, match, merge, aggregate), with
#       tidyverse equivalents (gather, spread, dplyr "joins" such as left_join, summarise_all)
#-------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'data_functions.R','interpolation_extension_functions.R' ) # Additional function files required.
    log_msg <- "Fix Former Soviet Union Data..." # First message to be printed to the log
    script_name <- "A2.3.IEA_FSU_energy_fix.R" #

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers, common_data = TRUE )

# -------------------------------------------------------------------------------------------
# 0.5 Define script specific parameters

# Because this code is operating before overall energy data is extended using BP
# statistics, only use BP data up to the last IEA data year
  end_BP_year <- IEA_end_year

# Non-OECD IEA data year start
  nonOECD_start_year <- 1971

# First year of disaggregate FSU IEA data
  disagg_IEA_FSU_start_year <- 1990

# -------------------------------------------------------------------------------------------
# 1. Load Data. Create the initial datasets from mapping and data files.

    activity_data <- readData( "MED_OUT", "A.en_biomass_fix" )
    MCL <- readData( "MAPPINGS", "Master_Country_List" )
    MSLevel <- readData( "MAPPINGS", "Master_Sector_Level_map" )
    MSL <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Sectors" )
    un_pop <- readData( "MED_OUT", "A.UN_pop_master" )
    bp_energy_data <- readData( "ENERGY_IN", BP_data_file_name, ".xlsx")

    # Read in BP energy data
    printLog( "Reading in BP energy consumption data..." )
    bp_oil_full <- bp_energy_data[[ getBPSheetNumber( "oil", "consumption", "tonnes", bp_energy_data ) ]]
    bp_gas_full <- bp_energy_data[[ getBPSheetNumber( "gas", "consumption", "Mtoe", bp_energy_data ) ]]
    bp_coal_full <- bp_energy_data[[ getBPSheetNumber( "coal", "consumption", "mtoe", bp_energy_data ) ]]

# -------------------------------------------------------------------------------------------
# 2. Define some Variable, seperate activity data.

# Define useful variables and maps
    ceds_sector <- unique( MSL[ which( MSL$type == 'comb' ), "sector" ] )
    ceds_fuel <- c( "heavy_oil", "natural_gas", "light_oil", "diesel_oil", "hard_coal", "brown_coal", "coal_coke", 'biomass' )
    agg_fuel <- c( "heavy_oil", "natural_gas", "light_oil",  "diesel_oil", "coal", "coal_coke", "biomass" )
    FSU <- unique( MCL[ which( MCL$Figure_Region == 'FSU' ), 'iso' ] )
    bp_fuel_map <- data.frame( fuel = c( "heavy_oil", "natural_gas", "light_oil",
                                           "diesel_oil", "hard_coal", "biomass",
                                           "brown_coal", "process","coal_coke", 'coal' ),
                                bp_fuel = c( 'oil' , 'gas','oil',
                                             'oil' , 'coal', NA ,
                                             'coal', NA , 'coal','coal' ) )

    FSU_iso <- FSU[ which( FSU %!in% 'ussr' ) ]

# Filter out FSU data, retain other to add back in later
    biomass_activity <- activity_data[ which( activity_data$fuel == 'biomass' ), ]
    original_activity_other <- activity_data[ which( activity_data$iso %!in% FSU ), ]
    original_activity_FSU <- activity_data[ which( activity_data$iso %in% FSU ), ]

# Extract non-combustion activity data for FSU countries
# these data will be added backed to final activity data at the end of the script
    original_activity_FSU_nc <- original_activity_FSU[ which( original_activity_FSU$sector %!in% ceds_sector ), ]

    activity <- original_activity_FSU


# Add zero activity lines to FSU data
    t.nrow <- length(FSU_iso) * length(ceds_fuel) * length(ceds_sector)
    template <- data.frame( iso = rep( FSU_iso, each = length(ceds_fuel) * length(ceds_sector), length.out = t.nrow ),
                            sector = rep( ceds_sector, each = length(ceds_fuel), length.out = t.nrow ),
                            fuel = rep( ceds_fuel, length(FSU_iso) * length(ceds_sector), length.out = t.nrow ),
                            units = rep( 'kt', length.out = t.nrow),
                            stringsAsFactors = FALSE )
    template[ X_IEA_years ] <- 0
    activity <- replaceValueColMatch( template, activity,
                                      x.ColName = X_IEA_years,
                                      match.x = c( 'iso','sector','fuel','units'),
                                      addEntries = FALSE )

# -------------------------------------------------------------------------------------------
# 3. Make any hard code changes to activity data before manipulation/extension
#    Aggregate Data for use later in code
### TODO A general list of changes made in this code block. e.g.
### 1. Extend coal_coke back constantly
### 2. Fix sectoral distributions in Ind-Comb-other and Agriculture (too aggregate)
### 3. Match aggregate sectors and regions and create aggregate datasets (maybe should be in block 2?)
### It may be useful to divide this code block into parts, or move the aggregate step to block 2 for clarity.


# Hard Code Change
    corrected <- activity

# extend coal_coke reporting back constantly. It stops reporting 1978
    corrected[ which( corrected$fuel %in% 'coal_coke' ), paste0('X', nonOECD_start_year : 1977 ) ] <-
                          corrected[ which( corrected$fuel %in% 'coal_coke' ), 'X1978' ] ### This is not robust to source changes

# Diesel
# Add '1A5_Other-unspecified' to '1A2g_Ind-Comb-other'
    corrected[ which( corrected$fuel == 'diesel_oil' &
                      corrected$sector == '1A2g_Ind-Comb-other' ), X_IEA_years ] <-
                          corrected[ which( corrected$fuel == 'diesel_oil' &
                                              corrected$sector == '1A2g_Ind-Comb-other' ), X_IEA_years ] +
                          corrected[ which( corrected$fuel == 'diesel_oil' &
                                              corrected$sector == '1A5_Other-unspecified' ), X_IEA_years ]
    corrected[ which( corrected$fuel == 'diesel_oil' &
                      corrected$sector == '1A5_Other-unspecified' ), X_IEA_years ] <- 0

# Split Agriculture into Agriculture - Industry - Residential
### TODO better explanation of this block. "Distribute agriculture emissions
###   among more specific CEDS sectors for 1960-1989 based on the proportions
###   of these disaggregate sectors in 1990"
    industry_sectors <- MSLevel[ which( MSLevel$FSU_extension %in% c( 'Industry' ) ),"working_sectors_v2" ]
    # iterate through FSU countries
    for ( i in seq_along ( FSU_iso ) ) {
        # Determine proportions of existing sector levels
        res <- corrected[ which( corrected$iso == FSU_iso[i] &
                                 corrected$fuel == 'diesel_oil' &
                                 corrected$sector == '1A4b_Residential' ), 'X1990' ]
        com <- corrected[ which( corrected$iso == FSU_iso[i] &
                                 corrected$fuel == 'diesel_oil' &
                                 corrected$sector == '1A4a_Commercial-institutional' ), 'X1990' ]
        ind <- sum( corrected[ which( corrected$iso == FSU_iso[i] &
                                      corrected$fuel == 'diesel_oil' &
                                      corrected$sector %in% industry_sectors ), 'X1990' ] )
        ag <- corrected[ which( corrected$iso == FSU_iso[i] &
                                corrected$fuel == 'diesel_oil' &
                                corrected$sector == '1A4c_Agriculture-forestry-fishing' ), 'X1990']
        ag_89 <- corrected[ which( corrected$iso == FSU_iso[i] &
                                  corrected$fuel == 'diesel_oil' &
                                  corrected$sector == '1A4c_Agriculture-forestry-fishing'), 'X1989']

#       Compare
        ag_redistribute <- 1 - ag / ag_89

        agriculture_replace <- corrected[ which( corrected$iso == FSU_iso[i] &
                                           corrected$fuel == 'diesel_oil' &
                                           corrected$sector == '1A4c_Agriculture-forestry-fishing') , paste0( 'X', IEA_start_year :( disagg_IEA_FSU_start_year - 1 ) ) ]

        if( ag_redistribute > 0 & is.finite( ag_redistribute ) ){
#       Split

#       Agriculture to res
#       res = agciculture*(ag_redistribute)* res/(res+com+ind) + res
            corrected[ which( corrected$iso == FSU_iso[i] &
                                corrected$fuel == 'diesel_oil' &
                                corrected$sector == '1A4b_Residential' ), paste0('X', IEA_start_year :( disagg_IEA_FSU_start_year - 1 )) ] <-
              corrected[ which( corrected$iso == FSU_iso[i] &
                                corrected$fuel == 'diesel_oil' &
                                corrected$sector == '1A4b_Residential'), paste0('X',IEA_start_year:( disagg_IEA_FSU_start_year - 1 )) ] +
              agriculture_replace * (ag_redistribute) * res / ( res + com + ind )

#       Agriculture to industry
#       industry-other = agciculture*(ag_redistribute)* ind/(res+com+ind) + industry-other
            corrected[ which( corrected$iso == FSU_iso[i] &
                                corrected$fuel == 'diesel_oil' &
                                corrected$sector == '1A2g_Ind-Comb-other'), paste0('X',IEA_start_year:( disagg_IEA_FSU_start_year - 1 )) ] <-
              corrected[ which( corrected$iso == FSU_iso[i] &
                                corrected$fuel == 'diesel_oil' &
                                corrected$sector == '1A2g_Ind-Comb-other'), paste0('X',IEA_start_year:( disagg_IEA_FSU_start_year - 1 )) ] +
              agriculture_replace * (ag_redistribute) * ind / ( res + com + ind )

#       Agriculture to com
#       commericial = agciculture*(ag_redistribute)* com/(res+com+ind) + com
            corrected[ which( corrected$iso == FSU_iso[i] &
                                corrected$fuel == 'diesel_oil' &
                                corrected$sector == '1A4a_Commercial-institutional' ), paste0('X',IEA_start_year:( disagg_IEA_FSU_start_year - 1 )) ] <-
              corrected[ which( corrected$iso == FSU_iso[i] &
                                corrected$fuel == 'diesel_oil' &
                                corrected$sector == '1A4a_Commercial-institutional' ), paste0('X',IEA_start_year:( disagg_IEA_FSU_start_year - 1 )) ] +
              agriculture_replace * (ag_redistribute) * com / ( res + com + ind )

#       Reduce agriculture
            corrected[ which( corrected$iso == FSU_iso[i] &
                              corrected$fuel == 'diesel_oil' &
                              corrected$sector == '1A4c_Agriculture-forestry-fishing') , paste0('X',IEA_start_year:( disagg_IEA_FSU_start_year - 1 ))] <-
              corrected[ which( corrected$iso == FSU_iso[i] &
                                corrected$fuel == 'diesel_oil' &
                                corrected$sector == '1A4c_Agriculture-forestry-fishing') ,
                          paste0('X',IEA_start_year:( disagg_IEA_FSU_start_year - 1 ))] * (ag/ag_89)

        } # END if

    } # END for loop  ### I can't see a better way to do this than a for-loop, but someone else might;
                      ### Wouldn't necessarily be more efficient but if you used for (iso in FSU_iso) it may work better

    activity <- corrected

# Add add aggregate fuel and agg sector
    activity$agg_sector <- MSLevel[ match( activity$sector, MSLevel$working_sectors_v2 ), "FSU_extension" ]
    activity$agg_fuel <- activity$fuel
    activity[ which( activity$fuel %in% c( 'hard_coal','brown_coal' ) ), 'agg_fuel'] <- 'coal'
    activity$bp_fuel <- as.character( bp_fuel_map[ match( activity$fuel, bp_fuel_map$fuel), "bp_fuel" ] )

# Aggregate Data in different ways for use through out script
# USSR refers to total of all former soviet union, FSU refers to former soviet union
# countries individually
    USSR_ceds_sector_fuel <- aggregate( activity[, X_IEA_years],
                                   by = list( fuel = activity$fuel,
                                              sector = activity$sector ),
                                   FUN = sum )

    USSR_sector_fuel <- aggregate( activity[, X_IEA_years],
                           by = list( agg_fuel = activity$agg_fuel,
                                      agg_sector = activity$agg_sector ),
                           FUN = sum )

    USSR_coal_agg_sector <- aggregate( activity[ which( activity$fuel %in% c('hard_coal','brown_coal')),X_IEA_years],
                                       by = list( iso = activity[ which( activity$fuel %in% c( 'hard_coal', 'brown_coal' ) ) , 'iso' ],
                                                  fuel = activity[ which( activity$fuel %in% c( 'hard_coal', 'brown_coal' ) ) , 'fuel' ],
                                                  sector = activity[ which( activity$fuel %in% c( 'hard_coal', 'brown_coal' ) ) , 'agg_sector' ] ),
                                       FUN = sum )
    USSR_BPfuel <- aggregate( activity[, X_IEA_years],
                                   by = list( bp_fuel = activity$bp_fuel ),
                                   FUN = sum )
    USSR_aggfuel <- aggregate( activity[, X_IEA_years],
                              by = list( agg_fuel = activity$agg_fuel ),
                              FUN = sum )
    USSR_sector_bp_fuel <- aggregate( activity[, X_IEA_years],
                                   by = list( bp_fuel = activity$bp_fuel,
                                              agg_sector = activity$agg_sector ),
                                   FUN = sum )

    USSR_fuel <- aggregate( activity[X_IEA_years],
                                  by = list( agg_fuel = activity$agg_fuel ),
                                  FUN = sum )

    CEDS_activity_bp_fuels <- aggregate( activity[, X_IEA_years],
                                         by = list( iso = activity[, 'iso'],
                                                    bp_fuel = activity[, 'bp_fuel'] ),
                                         FUN = sum )
    CEDS_activity_agg_fuels <- aggregate( activity[, X_IEA_years],
                                         by = list( iso = activity[, 'iso'],
                                                    agg_fuel = activity[, 'agg_fuel'] ),
                                         FUN = sum)
    CEDS_activity_agg_fuels_agg_sectors <- aggregate( activity[, X_IEA_years],
                                                      by = list( iso = activity[, 'iso'],
                                                                agg_fuel = activity[, 'agg_fuel'],
                                                                agg_sector = activity[, 'agg_sector'] ),
                                                      FUN = sum )
    CEDS_activity_ceds_fuels_agg_sectors <- aggregate( activity[, X_IEA_years],
                                                       by = list( iso = activity[, 'iso'],
                                                                  fuel = activity[, 'fuel'],
                                                                  agg_sector = activity[, 'agg_sector'] ),
                                                       FUN = sum )

    original_activity_summary_agg <- aggregate( activity[, paste0( 'X', seq(1975,2010,5) ) ],
                                                by = list( fuel = activity$agg_fuel ),
                                                FUN = sum )
    original_activity_summary <- aggregate( activity[, paste0( 'X',seq(1975,2010,5) ) ],
                                            by = list( fuel = activity$fuel ),
                                            FUN = sum )

# -------------------------------------------------------------------------------------------
# 4. Calculate USSR 1971-1990 share percentages and FSU population shares of total USSR

# USSR sector shares of each fuel - For each fuel/aggregate fuel, shares of fuel ues to each
# aggregate sector
    USSR_totals <- USSR_sector_fuel
    USSR_totals[X_IEA_years] <- NA
    USSR_totals[X_IEA_years] <- USSR_fuel[ match( USSR_totals$agg_fuel, USSR_fuel$agg_fuel ),
                                           X_IEA_years ]
    USSR_percentages <- USSR_totals
    USSR_percentages[X_IEA_years] <- NA
    USSR_percentages[X_IEA_years] <- USSR_sector_fuel[X_IEA_years] / USSR_totals[X_IEA_years]
    USSR_percentages[X_IEA_years] <- replace( USSR_percentages[X_IEA_years],
                                              is.na( USSR_percentages[X_IEA_years] ) , 0 )

# FSU population shares as percentages of total FSU pop
    FSU_pop <- un_pop[ which( un_pop$iso %in% FSU & un_pop$year <= end_BP_year & un_pop$year > 1970 ),
                      c( 'iso','year','pop' ) ]
    FSU_pop$year <- paste0( "X", FSU_pop$year )
    FSU_pop <- cast( FSU_pop , iso ~ year, value = 'pop')

    FSU_pop_share <- FSU_pop
    FSU_pop_share[, -1] <- FSU_pop_share[, -1]/matrix( data = rep( colSums( FSU_pop[,-1] ), times = 15 ),
                                          nrow = 15, ncol = length(colSums( FSU_pop[,-1] ) ), byrow = T )

# -------------------------------------------------------------------------------------------
# 5. Wrangle BP data into CEDS format

# Prepare the data and some useful variable lists
    X_BP_years <- paste0( 'X', 1965:end_BP_year )
    bp_data_full <- list( bp_oil_full, bp_gas_full, bp_coal_full )
    names( bp_data_full ) <- c( "bp_oil", "bp_gas", "bp_coal" )
    bp_fuel <- c( "oil", "gas", "coal" )

    bp_data_clean <- list()

#   Add USSR to MCL for newer BP data so that clean function retains USSR data
    ussr_row <- MCL[ which( MCL$iso %in% c( 'ukr' ) ) , ]
    ussr_row$BPName <- "USSR"
    ussr_row$iso <- "ussr"
    MCL <- rbind(MCL, ussr_row )

#   Create MCL version with just BPName and iso with unique
    MCL_unique <- MCL %>% dplyr::select(iso,BPName) %>% dplyr::distinct()
#   Remove Other CIS (this caused mda to stay in data for reasons unknown)
#   TODO - Make sure this is robust
    MCL_unique <- dplyr::filter(MCL_unique, BPName != "Other CIS")

#   Loop through each bp dataset (separated by fuel) to make the relevant corrections.
#   Result is bp_data_clean, which holds the corrected info
    for ( i in seq_along( bp_fuel) ) {
        bp_data_clean[[i]] <- cleanBPDataSheet( bp_data_full[[i]], X_BP_years, MCL )

#       Add iso
         bp_data_clean[[i]] <- dplyr::left_join(bp_data_clean[[i]], MCL_unique, by = 'BPName')

#       Add fuel column
        bp_data_clean[[i]]$bp_fuel <- bp_fuel[[i]]

#       Arrange columns
        bp_data_clean[[i]] <- bp_data_clean[[i]][, c( 'iso','bp_fuel', X_BP_years ) ]

    } # END for loop

#   Aggregate the 3 corrected files into one data frame, bp_data
    bp_data <- do.call( rbind, bp_data_clean )

#   Remove countries that are not in the FSU
    bp_data <- bp_data[ which( bp_data$iso %in% c( FSU, 'ussr' ) ) , ]

#   Remove data from before 1985
    bp_data <- bp_data[, c( 'iso', 'bp_fuel', paste0( 'X', 1985 : end_BP_year ) ) ]

    bp_data$ussr <- 'USSR'
    bp_data[ which( bp_data$iso != 'ussr' ), 'ussr' ] <- 'USSR_sum'

# Fix BP natural gas data in 1992 for Turkmenistan. If the value is 0, reset it to
# the average of 1991 and 1993. BP energy statistics (v2019) record 1992 natural gas consumption
# for Turkmenistan as zero. This is likely because consumption here is 'production - exports',
# a small number (12% of production). In 1992 exports fell "As a result of a dispute with Ukraine
# over payments for gas deliveries" (Glenn E. Curtis, ed. Turkmenistan: A Country Study. Washington: GPO for the Library of Congress, 1996.).
# Evidently 'production - exports' dropped to zero or below in these statistics.
# We assume that apparent domestic consumption remained steady over this year,
# and that irregularities in export and production data account for the mis-match.
# This will also require a change to BP's USSR gas data, as it now must account for this extra fuel consumption
bp_gas_tkm_1992 <- bp_data %>%
  dplyr::filter( iso == "tkm", bp_fuel == "gas" )

if( bp_gas_tkm_1992$X1992 == 0 ){

  printLog( "Correcting BP natural gas consumption data for Turkmenistan in 1992..." )

  bp_data <- bp_data %>%
    dplyr::mutate( X1992 = if_else( iso == "tkm" & bp_fuel == "gas" & X1992 == 0,
                                    ( ( X1991 + X1993 ) / 2 ), X1992 ) )

  new_value_to_add_to_USSR <- bp_data %>%
    dplyr::filter( iso == "tkm", bp_fuel == "gas" ) %>%
    dplyr::select( X1992 )

  add_tkm_1992_to_ussr_1992 <- new_value_to_add_to_USSR$X1992

  bp_data <- bp_data %>%
    dplyr::mutate( X1992 = if_else( iso == "ussr"& bp_fuel == "gas",
                                    X1992 + add_tkm_1992_to_ussr_1992,
                                    X1992 ) )

}

# -------------------------------------------------------------------------------------------
# 6. FSU country total fuel use ratios (energy use by fuel: coal, oil, gas) as a share of USSR.
#    Based on BP data 1985 - IEA_end_year, then extended backward using population. Operates directly
#    on BP countries and infers based on population and unaccounted-for activity to determine
#    activity in non-BP countries. Result is a data frame holding the activity proportions
#    by BP fuel.

# Fuel shares for BP countries

# Calculate percent of total USSR fuel (gas, coal, oil) each FSU country consumes
    FSU_countries_fuel_share_bp <- list()

    for (i in seq_along(bp_fuel)){
        df_fsu <- bp_data[which( bp_data$bp_fuel == bp_fuel[i] &  bp_data$iso != 'ussr') ,
                          c( 'iso','bp_fuel',paste0('X',1985:end_BP_year))]
        df_ussr <- bp_data[which( bp_data$bp_fuel == bp_fuel[i] & bp_data$iso == 'ussr') ,
                           c( 'iso','bp_fuel',paste0('X',1985:end_BP_year))]

        df_percents <- df_fsu
        df_percents[paste0('X',1985:end_BP_year)] <- NA
        for ( n in seq_along( df_fsu$iso ) ) {
            df_percents[ n, paste0('X', 1985:end_BP_year) ] <-  df_fsu[n, paste0( 'X', 1985:end_BP_year) ] / df_ussr[ 1, paste0( 'X', 1985:end_BP_year ) ]
        }
        FSU_countries_fuel_share_bp[[i]] <- df_percents
    }

    FSU_countries_fuel_share_bp <- do.call( rbind, FSU_countries_fuel_share_bp )

# Extend Country Shares Back for BP countries linearly from last
    FSU_countries_fuel_share_bp_extended <- FSU_countries_fuel_share_bp
    FSU_countries_fuel_share_bp_extended[ paste0('X', nonOECD_start_year:1984) ] <- NA
    FSU_countries_fuel_share_bp_extended <- FSU_countries_fuel_share_bp_extended[, c( 'iso', 'bp_fuel', paste0('X', nonOECD_start_year:IEA_end_year) ) ]
    FSU_countries_fuel_share_bp_extended[ paste0('X', nonOECD_start_year:IEA_end_year) ] <-
        t( na.locf( t( FSU_countries_fuel_share_bp_extended[ paste0('X', nonOECD_start_year:IEA_end_year) ] ), fromLast = TRUE ) )

# Dissaggregate leftover share to non-BP countries
    FSU_not_BP <- unique( FSU[ which( FSU %!in% unique(bp_data$iso) ) ] )

# Population shares for each non-BP country of the BP countries
    not_BP_pop <- FSU_pop[ which( FSU_pop$iso %in% FSU_not_BP ), ]
    not_BP_pop_shares <- not_BP_pop
    not_BP_pop_shares[, -1] <- not_BP_pop[, -1]/matrix( data = rep( colSums( not_BP_pop[, -1] ), times = length( FSU_not_BP ) ) ,
                                                        nrow = length( FSU_not_BP ) , ncol = length( colSums( not_BP_pop[,-1] ) ),
                                                        byrow = T )

# Determine a fuel breakdown for the countries that are not separated out in the BP data
# Use the population shares for each not BP country of the BP countries to
#      dissagregate left over.
# Check FSU_leftover_shares not sure if its right
### TODO: This comment makes it sound like this section needs checking.
###       As it stands, "test" does not return a series of 1s as it should

    not_BP_energy_shares <- list()
    for ( i in seq_along( bp_fuel ) ) {
        left_over_share <- as.numeric( 1-colSums( FSU_countries_fuel_share_bp_extended[ which( FSU_countries_fuel_share_bp_extended$bp_fuel == bp_fuel[i] ),
                                                                                        paste0('X',nonOECD_start_year:end_BP_year) ] ) )
        not_BP_energy_shares_df <- not_BP_pop_shares[ , c( 'iso', paste0('X',nonOECD_start_year:end_BP_year) ) ]
        not_BP_energy_shares_df[, paste0( 'X',nonOECD_start_year:end_BP_year ) ] <- not_BP_energy_shares_df[, paste0('X', nonOECD_start_year:end_BP_year) ] * matrix( data = rep(
                                              left_over_share,
                                              times = nrow(not_BP_energy_shares_df)) ,
                                              nrow = nrow(not_BP_energy_shares_df[,-1]) , ncol= length(left_over_share),
                                              byrow = T )

        not_BP_energy_shares_df$bp_fuel <- bp_fuel[i]
        not_BP_energy_shares_df <-  not_BP_energy_shares_df[, c( 'iso', 'bp_fuel', paste0('X', nonOECD_start_year:end_BP_year) ) ]
        not_BP_energy_shares[[i]] <-  not_BP_energy_shares_df
    }

    FSU_countries_fuel_share_ALL <- rbind.fill( FSU_countries_fuel_share_bp_extended ,
                                                do.call( rbind,not_BP_energy_shares ) )

    test <- aggregate( FSU_countries_fuel_share_ALL[ paste0('X', nonOECD_start_year:end_BP_year) ],
                       by = list( bp_fuel = FSU_countries_fuel_share_ALL$bp_fuel ),
                       FUN = sum )

    test <- aggregate( FSU_countries_fuel_share_bp_extended[ paste0('X',nonOECD_start_year:end_BP_year ) ],
                       by = list( bp_fuel = FSU_countries_fuel_share_bp_extended$bp_fuel ),
                       FUN = sum )

# Result: FSU_countries_fuel_share_all = df with proportion of each fuel's use
#         is attributable to each country. This will be used to find absolute
#         activity data.

# -------------------------------------------------------------------------------------------
# 7. FSU countries, calculate sector share of each ceds fuel.

# Calculate USSR aggregate sector share of each fuel
    USSR_sector_shares_by_fuel_list <- list()

    for ( i in seq_along(agg_fuel) ) {
        USSR_sector_by_fuel_df <- USSR_sector_fuel[ which(USSR_sector_fuel$agg_fuel == agg_fuel[i] ), ]
        USSR_sector_by_fuel_df[,paste0('X',nonOECD_start_year:IEA_end_year)] <- USSR_sector_by_fuel_df[,paste0('X',nonOECD_start_year:IEA_end_year)] /
                                                          matrix( data = rep( colSums( USSR_sector_by_fuel_df[, paste0('X', nonOECD_start_year:IEA_end_year) ] ), times = nrow( USSR_sector_by_fuel_df ) ) ,
                                                          nrow = nrow(USSR_sector_by_fuel_df) , ncol= length(paste0('X',nonOECD_start_year:IEA_end_year)),
                                                          byrow = T)
        USSR_sector_shares_by_fuel_list[[i]] <-  USSR_sector_by_fuel_df
    }

    USSR_sector_shares_by_fuel <- do.call( rbind, USSR_sector_shares_by_fuel_list )

# Calculate FSU sector share of each aggregate sector x fuel, by iso
# wrangle activity data
    FSU_iso_agg_sector_agg_fuel <- aggregate( activity[ paste0('X', disagg_IEA_FSU_start_year:IEA_end_year) ],
                                              by = list( iso = activity$iso,
                                                         sector = activity$agg_sector,
                                                         fuel = activity$agg_fuel ),
                                              FUN = sum )

# Reformat the above result: first, create a data frame in the desired shape and fill with 0s...
    agg_fuel <- sort( unique( FSU_iso_agg_sector_agg_fuel$fuel ) )
    agg_sector <- sort( unique( FSU_iso_agg_sector_agg_fuel$sector ) )

    temp_nrow <- length(FSU_iso) * length(agg_fuel) * length(agg_sector)

    temp <- data.frame( iso = rep( FSU_iso, each = length(agg_fuel) * length(agg_sector), len = temp_nrow) ,
                        fuel = rep(   rep(agg_fuel, each = length(agg_sector) ), len = temp_nrow),
                        sector = rep( agg_sector, len = temp_nrow ) )

    temp[ paste0('X', disagg_IEA_FSU_start_year:IEA_end_year) ] <- 0

# ... then replace the values by iso/fuel/sector.
    FSU_iso_agg_sector_agg_fuel <- replaceValueColMatch(x = temp, y = FSU_iso_agg_sector_agg_fuel,
                         x.ColName = paste0('X', disagg_IEA_FSU_start_year:IEA_end_year),
                         match.x = c( 'iso', 'fuel', 'sector'),
                         addEntries = TRUE)

#   Remove temp from the global environment to free space
    rm ( temp, temp_nrow )

#   Calculate shares with fuel and iso loop
    fuel_list <- list()

# Iterate through each fuel and each iso and determine overall shares of each sector
# for each iso/fuel combo.

#   Begin fuel loop
    for ( i in seq_along(ceds_fuel) ) {
         fuel_data <- FSU_iso_agg_sector_agg_fuel[ which( FSU_iso_agg_sector_agg_fuel$fuel == agg_fuel[i] ), ]
         iso_list <- list()

#       Begin iso loop
        for ( n in seq_along(FSU) ) {

            iso_data <- fuel_data[ which( fuel_data$iso == FSU[n] ), ]

            if ( nrow( iso_data ) > 0 ){

                iso_shares <- iso_data
                iso_shares[, paste0('X', disagg_IEA_FSU_start_year:IEA_end_year) ] <- iso_shares[, paste0('X', disagg_IEA_FSU_start_year : IEA_end_year) ] /
                                        matrix(  data = rep( colSums( iso_data[,paste0('X', disagg_IEA_FSU_start_year : IEA_end_year) ] ),
                                                                            times = nrow(iso_data)) ,
                                                                      nrow = nrow(iso_data) ,
                                                                      ncol= length( paste0('X', disagg_IEA_FSU_start_year : IEA_end_year) ),
                                                                      byrow = T )

                iso_shares <- replace( iso_shares, is.na(iso_shares), 0 )

            } else {

                iso_shares <- data.frame( iso = character(0),
                                          sector = character(0),
                                          fuel = character(0) )
            }

            iso_list[[n]] <-  iso_shares
        }

        fuel_list[[i]] <- do.call( rbind.fill, iso_list )
    }

    FSU_iso_sector_shares_by_fuel <- do.call( rbind, fuel_list )

# -------------------------------------------------------------------------------------------
# 8. Extend FSU countries, sector share of each fuel back to 1971 (nonOECD_start_year)

# Hard Fix USSR sector Shares:

# USSR_sector_shares_by_fuel
# heavy oil - 1973-1988
  USSR_sector_shares_by_fuel[ which( USSR_sector_shares_by_fuel$agg_fuel == 'heavy_oil' &
                                       USSR_sector_shares_by_fuel$agg_sector %in%
                                       c( 'Power', 'Industry' ) ),paste0('X',1973:1988) ] <- NA
# diesel_oil
  USSR_sector_shares_by_fuel[ which( USSR_sector_shares_by_fuel$agg_fuel == 'diesel_oil' &
                                       USSR_sector_shares_by_fuel$agg_sector %in%
                                       c( 'Agriculture' , 'Industry' ) ), paste0('X', 1975:( disagg_IEA_FSU_start_year - 1 ))  ] <- NA

# natural_gas
  USSR_sector_shares_by_fuel[ which( USSR_sector_shares_by_fuel$agg_fuel == 'natural_gas' &
                                         USSR_sector_shares_by_fuel$agg_sector ==  'Residential' ), paste0( 'X', nonOECD_start_year : 1974)  ] <-
      USSR_sector_shares_by_fuel[ which( USSR_sector_shares_by_fuel$agg_fuel == 'natural_gas' &
                                           USSR_sector_shares_by_fuel$agg_sector ==  'Other' ), paste0( 'X', nonOECD_start_year : 1974)  ]

  USSR_sector_shares_by_fuel[ which( USSR_sector_shares_by_fuel$agg_fuel == 'natural_gas' &
                                       USSR_sector_shares_by_fuel$agg_sector ==  'Other' ), paste0( 'X', nonOECD_start_year : 1974) ] <- 0

#   Interpolate
    USSR_sector_shares_by_fuel <- interpolateValues( USSR_sector_shares_by_fuel )

# END Hard code fix


# Extend FSU sector shares
    iso_list <- list()
    fuel_list <- list()

# Iterate through each iso and aggregate fuel. For each, use known shares to interpolate to 1971 (nonOECD_start_year)
    for ( i in seq_along(FSU_iso) ) {
        for ( n in seq_along(agg_fuel) ) {
            shares <- FSU_iso_sector_shares_by_fuel[which(FSU_iso_sector_shares_by_fuel$iso == FSU_iso[i] &
                                                          FSU_iso_sector_shares_by_fuel$fuel == agg_fuel[n]  ),]
            shares[ paste0('X', nonOECD_start_year:( disagg_IEA_FSU_start_year - 1 )) ] <- NA
            shares <- shares[, c( 'iso', 'fuel', 'sector', paste0('X',nonOECD_start_year:IEA_end_year) ) ]

            shares <- replaceValueColMatch( shares, USSR_sector_shares_by_fuel,
                                         x.ColName = paste0('X', nonOECD_start_year:1985),
                                         match.x = c( 'fuel', 'sector' ),
                                         match.y = c( "agg_fuel"  , "agg_sector" ),
                                         addEntries = FALSE )
            shares <- interpolateValues(shares)

#           Increase ratios so that they sum to 1
            sum <- colSums( shares[ paste0('X', nonOECD_start_year:IEA_end_year) ] )
            shares[ paste0('X', nonOECD_start_year:IEA_end_year) ] <- shares[ paste0('X', nonOECD_start_year:IEA_end_year) ] /
                                                    matrix( data = rep( sum , times =  nrow(shares) ),
                                                            nrow = nrow( shares ), byrow = TRUE )
           fuel_list[[n]] <- shares

        }

        iso_list[[i]] <- do.call( rbind, fuel_list )
    }

    FSU_iso_sector_shares_by_fuel_extended <- do.call( rbind, iso_list )
    FSU_iso_sector_shares_by_fuel_extended <- replace( FSU_iso_sector_shares_by_fuel_extended, is.na( FSU_iso_sector_shares_by_fuel_extended ), 0 )

# Hard Fix FSU sector Shares
### TODO: Why is this hard fix necessary?
    corrected <- FSU_iso_sector_shares_by_fuel_extended

    corrected[ which( corrected$iso == 'arm' & corrected$fuel == 'coal' ), paste0( "X", '', 1995:2011 ) ] <- NA
    corrected[ which( corrected$iso == 'arm' & corrected$fuel == 'heavy_oil' ), paste0( "X", '', 1997:IEA_end_year ) ] <- NA
    corrected[ which( corrected$iso == 'arm' & corrected$fuel == 'diesel_oil' ), paste0( "X", '', 1991:IEA_end_year ) ] <- NA

    corrected[ which( corrected$iso == 'aze' & corrected$fuel == 'coal' ), paste0( "X", '', 1999:IEA_end_year ) ] <- NA
    corrected[ which( corrected$iso == 'aze' & corrected$fuel == 'diesel_oil' ), paste0( "X", '', 2009:IEA_end_year ) ] <- NA

    corrected[ which( corrected$iso == 'tjk' & corrected$fuel %in% c( 'heavy_oil','diesel_oil', 'coal' ) ), paste0( "X", '', 1985:IEA_end_year ) ] <- NA

    corrected[ which( corrected$iso == 'uzb' & corrected$fuel %in% c( 'diesel_oil') ), paste0( "X", '', 1985:1995 ) ] <- NA

    corrected[ which( corrected$iso == 'mda' & corrected$fuel %in% c( 'diesel_oil' ) ), paste0( "X", '', 1985:1995 ) ] <- NA

    corrected <- interpolateValues( corrected )
    corrected <- extendValues( corrected )

    FSU_iso_sector_shares_by_fuel_extended <- corrected

# -------------------------------------------------------------------------------------------
# 9. Aggregate activity data (no sector breakdown). First aggregate activity by bp fuel,
#    then by ceds aggregate fuel FSU_countries_fuel_share_ALL

#   New activity by BP fuel
#   set up multiplier
    total_fuel_multiplier <- FSU_countries_fuel_share_ALL[, c( "iso", "bp_fuel", paste0( 'X', nonOECD_start_year:end_BP_year ) ) ]
    total_fuel_multiplier[ paste0( 'X', nonOECD_start_year:end_BP_year ) ] <- NA
    total_fuel_multiplier[ paste0( 'X', nonOECD_start_year:end_BP_year ) ] <- USSR_BPfuel[ match( total_fuel_multiplier$bp_fuel, USSR_BPfuel$bp_fuel ),
                                                                   paste0( 'X', nonOECD_start_year:end_BP_year ) ]

#   Calculate
    FSU_fuel_use_by_BPfuel <- FSU_countries_fuel_share_ALL[, c( "iso", "bp_fuel", paste0( 'X', nonOECD_start_year:end_BP_year ) ) ]
    FSU_fuel_use_by_BPfuel[ paste0( 'X', nonOECD_start_year:end_BP_year ) ] <- NA
    FSU_fuel_use_by_BPfuel[ paste0( 'X', nonOECD_start_year:end_BP_year ) ] <- FSU_countries_fuel_share_ALL[ paste0( 'X', nonOECD_start_year:end_BP_year ) ] *
                                                              total_fuel_multiplier[ paste0( 'X', nonOECD_start_year:end_BP_year ) ]

#   New activity by ceds fuel - disaggregate FSU_fuel_use_by_BPfuel to ceds_fuel based on year ratio average
#   CEDS_activity_bp_fuels
    CEDS_activity_agg_fuels$bp_fuel <- bp_fuel_map[ match(CEDS_activity_agg_fuels$agg_fuel, bp_fuel_map$fuel ) , 'bp_fuel' ]
    USSR_aggfuel$bp_fuel <- bp_fuel_map[ match( USSR_aggfuel$agg_fuel, bp_fuel_map$fuel ), 'bp_fuel' ]


    iso_list <- list()
    fuel_list <- list()

#  Begin iso loop
    for ( i in seq_along(FSU_iso) ) {

#       Begin fuel loop
        for ( n in seq_along(bp_fuel) ) {

            iso_name <- FSU_iso[i]
            fuel_name <- bp_fuel[n]
            activity_bp <- FSU_fuel_use_by_BPfuel[ which( FSU_fuel_use_by_BPfuel$iso == iso_name &
                                                           FSU_fuel_use_by_BPfuel$bp_fuel == fuel_name ), ]

#           Share of ceds agg fuel(s) to bp fuel
            breakdown_data <- USSR_aggfuel[ which( USSR_aggfuel$bp_fuel == fuel_name ) , c( 'bp_fuel', 'agg_fuel', paste0( 'X', nonOECD_start_year:end_BP_year ) ) ]

            ratio <- breakdown_data[, c( 'bp_fuel', 'agg_fuel', paste0( 'X', nonOECD_start_year:end_BP_year ) ) ]
            ratio[ paste0( 'X', nonOECD_start_year:end_BP_year ) ] <- breakdown_data[ paste0( 'X', nonOECD_start_year:end_BP_year ) ] / matrix(
                                      data = rep( colSums( breakdown_data[ paste0( 'X', nonOECD_start_year:end_BP_year ) ] ),
                                      times =  nrow(breakdown_data) ),
                                      nrow = nrow(breakdown_data), byrow = TRUE )

#           New activity data
            new_activity <- data.frame( iso = rep( iso_name, length(breakdown_data$agg_fuel ) ),
                                        agg_fuel = breakdown_data$agg_fuel )
            new_activity[ paste0( 'X', nonOECD_start_year:end_BP_year ) ] <- matrix( data = rep( x = as.numeric( activity_bp[ paste0( 'X', nonOECD_start_year:end_BP_year ) ] ) ,
                                                                        times = nrow(ratio) ),
                                                                   nrow = nrow( ratio ), byrow = TRUE ) * ratio[ paste0( 'X', nonOECD_start_year:end_BP_year ) ]
            fuel_list[[n]] <- new_activity

        } # ENDS fuel loop

        iso_list[[i]] <- do.call( rbind, fuel_list )

    } # END iso loop

    FSU_fuel_agg_fuel_fromUSSRbp <- do.call( rbind, iso_list )

# Dissagregated to FSU. fuel totals by agg fuel, no sector

# -------------------------------------------------------------------------------------------
# 10. Disaggregate FSU new activity from fuel totals to aggregate sectors by Sector Share
#     via iteration through each iso and aggregate fuel.

    iso_list <- list()
    fuel_list <- list()

#   Begin iso loop
    for ( i in seq_along(FSU_iso) ) {

#       Begin fuel loop
        for ( n in seq_along(agg_fuel) ) {

            iso_name <- FSU_iso[i]
            fuel_name <- agg_fuel[n]

            total_activity <- FSU_fuel_agg_fuel_fromUSSRbp[ which( FSU_fuel_agg_fuel_fromUSSRbp$iso == iso_name &
                                                                    FSU_fuel_agg_fuel_fromUSSRbp$agg_fuel == fuel_name ) ,]

#           Ratios
            ratios <- FSU_iso_sector_shares_by_fuel_extended[ which( FSU_iso_sector_shares_by_fuel_extended$iso == iso_name &
                                                                       FSU_iso_sector_shares_by_fuel_extended$fuel == fuel_name ), ]
            names(ratios) <- c( 'iso', 'agg_fuel', 'agg_sector', paste0( 'X', nonOECD_start_year:IEA_end_year ) )

#           New activity data
            new_activity <- ratios[, c( 'iso', 'agg_fuel', 'agg_sector' ) ]
            new_activity[ paste0( 'X', nonOECD_start_year:IEA_end_year ) ] <- 0

            if( !identical( ratios$sector, new_activity$sector ) ){

                stop( 'Rows out of order In Fix_FSU.R Rows, please sort...' )

            }

            new_activity[ paste0( 'X', nonOECD_start_year:IEA_end_year ) ] <- ratios[ paste0( 'X', nonOECD_start_year:IEA_end_year ) ] *
              matrix( data = rep( as.numeric( total_activity[ paste0( 'X', nonOECD_start_year:IEA_end_year ) ] ) , times =  nrow(ratios) ),
                      nrow = nrow(ratios), byrow = TRUE)

            fuel_list[[n]] <- new_activity

        } # END fuel loop

        iso_list[[i]] <- do.call( rbind, fuel_list )

    } # END iso loop

    FSU_disaggregated_activity_agg <- do.call( rbind, iso_list )
    FSU_disaggregated_activity_agg <- replace( FSU_disaggregated_activity_agg, is.na( FSU_disaggregated_activity_agg ), 0 )

    check <- FSU_disaggregated_activity_agg
    activity_summary <- aggregate( check[, paste0( 'X', nonOECD_start_year:IEA_end_year ) ],
                                   by = list( fuel = check$agg_fuel ),
                                   FUN = sum )
    original_activity_summary_agg$set <- 'original'
    activity_summary$set <- 'final'
    summary <- rbind.fill( activity_summary, original_activity_summary_agg )
    summary <- melt( summary)
    summary <- cast( summary, fuel + variable ~ set )
    summary$diff <- summary$final - summary$original
    writeData( summary, 'DIAG_OUT', 'A.FSU_energy_corrected_summary_before_dissaggregation', meta = F )

# -------------------------------------------------------------------------------------------
# 11. Blend Diaggregated USSR data and reported CEDS data. Create a period of "smoothing"
#     between disaggregated USSR data and CEDS data were it begins to remove disjoint nature
#     and benefit from accuracy of both datasets where relevant.
### TODO: confirm that my explanatory comments above are accurate

    iso_list <- list()

#   Begin iso loop
    for ( i in seq_along( FSU_iso ) ) {

        iso_name <- FSU_iso[i]

#   Determine which years to merge
        start_merge <- 1991
        if( iso_name %in% c( 'geo' , 'blr' ) ){ start_merge <- 1996 }
        if( iso_name %in% c( 'aze', 'est', 'mda' ) ){ start_merge <- 1993 }
        end_merge <- start_merge + 5
        merge_years <- start_merge:end_merge

#      Add lines for zero values
        temp_nrow <- length(iso_name) * length(agg_fuel) * length(agg_sector)
        temp <- data.frame( iso = rep( iso_name, each = length(agg_fuel) * length(agg_sector) , len = temp_nrow ),
                            agg_fuel = rep( rep( agg_fuel, each = length(agg_sector) ), len = temp_nrow ),
                            agg_sector = rep( agg_sector, len = temp_nrow ) )
        temp[ paste0('X', nonOECD_start_year:IEA_end_year) ] <- 0

#       Add CEDS and FSU data
        CEDS_activity <- replaceValueColMatch( temp, CEDS_activity_agg_fuels_agg_sectors,
                                                  x.ColName = paste0( 'X', nonOECD_start_year : IEA_end_year ),
                                                  match.x = c( 'iso', 'agg_fuel', 'agg_sector'),
                                                  addEntries = FALSE )
        FSU_activity <- FSU_disaggregated_activity_agg[ which( FSU_disaggregated_activity_agg$iso == iso_name ), ]

#       Sort data sets to merge
        CEDS_activity <- CEDS_activity[ with( CEDS_activity, order( iso, agg_fuel, agg_sector ) ), ]
        FSU_activity <- FSU_activity[ with( FSU_activity , order( iso, agg_fuel, agg_sector ) ), ]

#       Make merged data set
#       FSU dissaggregated data untill merge start
        merged_data <- FSU_activity[ , c( 'iso', 'agg_fuel', 'agg_sector', paste0( 'X',nonOECD_start_year : IEA_end_year ) ) ]
        merged_data[, c( paste0('X', start_merge:BP_last_year) ) ] <- NA

#       Merge: slowly switch from FSU dissaggregated to CEDS reported
        for ( n in seq_along(merge_years) ) {

            steps <- 1 / length(merge_years)
            ceds_percent_step <- steps * n
            merged_data[, c( paste0('X', merge_years[n]) ) ] <- FSU_activity[, c( paste0( 'X', merge_years[n]) ) ] * (1 - ceds_percent_step) +
                                                                      CEDS_activity[, c( paste0('X', merge_years[n]) ) ] * (ceds_percent_step)

        }

#       CEDS reported data
        merged_data[, c( paste0( 'X', (end_merge + 1):IEA_end_year ) ) ] <- CEDS_activity[, c( paste0( 'X', (end_merge + 1):IEA_end_year ) ) ]
        iso_list[[i]] <- merged_data

    }

    FSU_activity_agg <- do.call( rbind, iso_list )

# -------------------------------------------------------------------------------------------
# 12. Correct FSU data post 1990 (disagg_IEA_FSU_start_year)

# Replace some country/sectors with dissagregated data
### TODO: how were these chosen?

    replace <- FSU_disaggregated_activity_agg[ which( FSU_disaggregated_activity_agg$iso %in%
                                                       c( 'arm', 'tjk', 'tkm' ) ), ]
    replace <- rbind( replace, FSU_disaggregated_activity_agg[ which( FSU_disaggregated_activity_agg$iso %in% c( 'aze' ) &
                                                         FSU_disaggregated_activity_agg$agg_fuel %in% c( 'coal' ) ), ] )
    replace <- rbind( replace, FSU_disaggregated_activity_agg[ which( FSU_disaggregated_activity_agg$iso %in% c('kaz') &
                                                                        FSU_disaggregated_activity_agg$agg_fuel %in% c('natural_gas') ), ] )
    replace <- rbind( replace, FSU_disaggregated_activity_agg[ which( FSU_disaggregated_activity_agg$iso %in% c('kgz') &
                                                                        FSU_disaggregated_activity_agg$agg_fuel %in% c('heavy_oil') ), ] )
    replace <- rbind( replace, FSU_disaggregated_activity_agg[ which( FSU_disaggregated_activity_agg$iso %in% c('aze') &
                                                                        FSU_disaggregated_activity_agg$agg_fuel %in% c('coal') ), ] )
    replace <- rbind( replace, FSU_disaggregated_activity_agg[ which( FSU_disaggregated_activity_agg$iso %in% c('mda') &
                                                                        FSU_disaggregated_activity_agg$agg_fuel %in% c('diesel') ), ] )
    replace <- rbind( replace, FSU_disaggregated_activity_agg[ which( FSU_disaggregated_activity_agg$iso %in% c('rus') &
                                                                        FSU_disaggregated_activity_agg$agg_fuel %in% c('light_oil') ), ] )

    FSU_activity_agg <- replaceValueColMatch( FSU_activity_agg, replace,
                                              x.ColName = paste0( 'X', nonOECD_start_year : IEA_end_year ),
                                              match.x = c( 'iso', 'agg_fuel', 'agg_sector' ),
                                              addEntries = FALSE )

    corrected <- FSU_activity_agg

# Individual FSU countries post 1990 (disagg_IEA_FSU_start_year)
# mostly diaggregating the 'Other' Categorgy

# Tajikistan
# Naturgal Gas - split other into industry and residential
# Calculate ratio --> use russia
    ratio = USSR_percentages[ which( USSR_percentages$agg_fuel == 'natural_gas' & USSR_percentages$agg_sector == 'Industry' ),
                              paste0( 'X', 1985 )] /
      (USSR_percentages[ which( USSR_percentages$agg_fuel == 'natural_gas' & USSR_percentages$agg_sector == 'Residential' ),
                         paste0( 'X', 1985 ) ] +
         USSR_percentages[ which( USSR_percentages$agg_fuel == 'natural_gas' & USSR_percentages$agg_sector == 'Industry' ),
                           paste0( 'X', 1985 ) ] )

# Correct some tjk values using the calculated ratio
    corrected[ which( corrected$iso == 'tjk' & corrected$agg_fuel == 'natural_gas' &
                        corrected$agg_sector == 'Industry' ), paste0('X', 1986:IEA_end_year) ] <-
      ratio*corrected[ which( corrected$iso == 'tjk' & corrected$agg_fuel == 'natural_gas' &
                                corrected$agg_sector == 'Other' ), paste0('X', 1986:IEA_end_year) ]
    corrected[ which( corrected$iso == 'tjk' & corrected$agg_fuel == 'natural_gas' &
                        corrected$agg_sector == 'Residential' ), paste0('X', 1986:IEA_end_year) ] <-
      (1-ratio)*corrected[ which( corrected$iso == 'tjk' & corrected$agg_fuel == 'natural_gas' &
                                    corrected$agg_sector == 'Other'), paste0('X', 1986:IEA_end_year) ]
    corrected[ which( corrected$iso == 'tjk' & corrected$agg_fuel == 'natural_gas' &
                        corrected$agg_sector == 'Other' ), paste0('X', 1986:IEA_end_year) ] <- 0

# Russia
# heavy_oil. Add Other to Transportation 1995 - 2000
    corrected[ which( corrected$iso == 'rus' & corrected$agg_fuel == 'diesel_oil' &
                        corrected$agg_sector == 'Transportation' ) , paste0('X', 1995:1998) ] <-
                                      corrected[ which( corrected$iso == 'rus' & corrected$agg_fuel == 'diesel_oil' &
                                                          corrected$agg_sector == 'Other') , paste0('X', 1995:1998) ] +
                                      corrected[ which( corrected$iso == 'rus' & corrected$agg_fuel == 'diesel_oil' &
                                                          corrected$agg_sector == 'Transportation' ) , paste0('X', 1995:1998) ]
    corrected[ which( corrected$iso == 'rus' & corrected$agg_fuel == 'diesel_oil' &
                        corrected$agg_sector == 'Other' ), paste0('X', 1995:1998) ] <- 0

# Kaz
# light oil - split other into industry / residential / other
# Calculate ratio -  industry and residential are the same amount
    ratio = USSR_percentages[ which( USSR_percentages$agg_fuel == 'light_oil' & USSR_percentages$agg_sector == 'Agriculture' ),
                              paste0('X', 1985) ] /
      ( USSR_percentages[ which( USSR_percentages$agg_fuel == 'light_oil' & USSR_percentages$agg_sector == 'Agriculture' ),
                         paste0('X', 1985) ] +
         2*USSR_percentages[ which( USSR_percentages$agg_fuel == 'light_oil' & USSR_percentages$agg_sector == 'Industry' ),
                           paste0('X', 1985) ] )

# Correct kaz using calculated ratio
    corrected[ which( corrected$iso == 'kaz' & corrected$fuel == 'light_oil' &
                        corrected$agg_sector == 'Agriculture' ), paste0('X', 1986:IEA_end_year) ] <-
      ratio*corrected[ which( corrected$iso == 'kaz' & corrected$fuel == 'light_oil' &
                                corrected$agg_sector == 'Other' ), paste0('X', 1986:IEA_end_year) ]
    corrected[ which( corrected$iso == 'kaz' & corrected$fuel == 'light_oil' &
                        corrected$agg_sector == 'Residential' ), paste0('X', 1986:IEA_end_year) ] <-
      (1-ratio)/2*corrected[ which( corrected$iso == 'kaz' & corrected$fuel == 'light_oil' &
                                    corrected$agg_sector == 'Other' ), paste0('X', 1986:IEA_end_year) ]
    corrected[ which( corrected$iso == 'kaz' & corrected$fuel == 'light_oil' &
                        corrected$agg_sector == 'Industry' ), paste0('X', 1986:IEA_end_year) ] <-
      (1-ratio)/2*corrected[ which( corrected$iso == 'kaz' & corrected$fuel == 'light_oil' &
                                    corrected$agg_sector == 'Other' ), paste0('X', 1986:IEA_end_year) ]

    corrected[ which( corrected$iso == 'kaz' & corrected$fuel == 'light_oil' &
                        corrected$agg_sector == 'Other' ), paste0('X', 1986:IEA_end_year) ] <- 0

# aze diesel
    corrected[ which( corrected$iso == 'aze' & corrected$agg_fuel == 'diesel_oil' &
                        corrected$agg_sector %in% c( 'Transportation', 'Industry', 'Agriculture' ) ) ,
                        paste0('X', 1997:1998) ] <- NA

# arm natural_gas
    corrected[ which( corrected$iso == 'arm' & corrected$agg_fuel == 'natural_gas' &
                        corrected$agg_sector %in% c( 'Residential' ) ) ,
               paste0( 'X', 1997:1998 ) ] <- NA

    corrected[ paste0( 'X', nonOECD_start_year:IEA_end_year ) ] <- interpolateValues( corrected[ paste0('X', nonOECD_start_year:IEA_end_year) ] )

# The result of our corrections:
    FSU_activity_agg_corrected <- corrected
    FSU_activity_agg_corrected <- FSU_activity_agg_corrected[ c( "iso", "agg_fuel", "agg_sector", paste0('X', nonOECD_start_year:IEA_end_year) ) ]
    if( anyNA( FSU_activity_agg_corrected ) ) stop( "NAs in database please check code" )

# Create & write diagnostic output for checking what corrections were made
    check <- FSU_activity_agg_corrected
    activity_summary <- aggregate( check[, paste0('X', seq( 1975, 2010, 5) ) ],
                                   by = list( fuel = check$agg_fuel ),
                                   FUN = sum)
    original_activity_summary_agg$set <- 'original'
    activity_summary$set <- 'final'
    summary <- rbind.fill( activity_summary, original_activity_summary_agg )
    summary<-melt(summary)
    summary<- cast( summary, fuel + variable ~ set )
    summary$diff <- summary$final - summary$original
    summary <- summary[ -which( summary$fuel %in% c( 'biomass','process') ), ]
    writeData( summary, 'DIAG_OUT', 'A.FSU_energy_corrected_summary_before_dissaggregation', meta=F )

# -------------------------------------------------------------------------------------------
# 13. Dissaggregate corrected data into final CEDS fuels; then, disaggregate into final
#     CEDS sectors.

# Disaggregate coal to brown and hard
    temp_nrow <- length(FSU_iso) * length(ceds_fuel) * length(agg_sector)
    temp <- data.frame( iso = rep( FSU_iso, each = length(ceds_fuel) * length(agg_sector), len = temp_nrow),
                        fuel = rep( rep( ceds_fuel, each = length( agg_sector ) ), len = temp_nrow),
                        sector = rep( agg_sector, len = temp_nrow ) )
    temp[ paste0( 'X', nonOECD_start_year:IEA_end_year ) ] <- NA
    FSU_activity_ceds_fuel_agg_sector <- replaceValueColMatch( temp, FSU_activity_agg_corrected,
                                  x.ColName = paste0( 'X', nonOECD_start_year:IEA_end_year ),
                                  match.x = c( 'iso', 'sector', 'fuel' ),
                                  match.y = c( 'iso', 'agg_sector', 'agg_fuel' ),
                                  addEntries = FALSE )
    disaggregate <- FSU_activity_ceds_fuel_agg_sector[ which( FSU_activity_ceds_fuel_agg_sector$fuel %!in% c( 'brown_coal','hard_coal' ) ), ]
    USSR_coal_breakdown <- aggregate( activity[ which( activity$fuel %in% c('hard_coal','brown_coal')),X_IEA_years],
                                     by = list( fuel = activity[ which( activity$fuel %in% c( 'hard_coal', 'brown_coal' ) ), 'fuel' ],
                                                sector = activity[ which( activity$fuel %in% c( 'hard_coal','brown_coal' ) ), 'agg_sector' ] ),
                                     FUN = sum )

#   Iterate through each iso and agg sector. Use the CEDS breakdown of fuels to
#   determine the appropriate coal breakdown.
### TODO: Some of these lines get a little long. Should we reformat or leave as-is?
### TODO: Some of these variables start to have confusing names ("disaggregate", "disag_new")
    iso_list <- list()
    sector_list <- list()

#   Begin iso loop
    for ( i in seq_along(FSU_iso) ) {

#       Begin agg sector loop
        for( n in seq_along(agg_sector) ) {

            iso_name = FSU_iso[i]
            sector_name = agg_sector[n]
            disag_new <- FSU_activity_ceds_fuel_agg_sector[ which( FSU_activity_ceds_fuel_agg_sector$iso == iso_name &
                                                                   FSU_activity_ceds_fuel_agg_sector$sector == sector_name &
                                                                   FSU_activity_ceds_fuel_agg_sector$fuel %in% c( 'brown_coal', 'hard_coal' ) ), ]

            disag_new <- disag_new[ with( disag_new, order( iso, fuel, sector ) ), ]

            breakdown <- CEDS_activity_ceds_fuels_agg_sectors[ which( CEDS_activity_ceds_fuels_agg_sectors$iso == iso_name &
                                                                        CEDS_activity_ceds_fuels_agg_sectors$agg_sector == sector_name &
                                                                        CEDS_activity_ceds_fuels_agg_sectors$fuel %in% c( 'brown_coal', 'hard_coal' ) ),
                                                                c( 'fuel', paste0('X',nonOECD_start_year:IEA_end_year) ) ]

            if( nrow(breakdown) < 2 | sum( breakdown[ paste0('X',disagg_IEA_FSU_start_year:IEA_end_year) ] ) == 0) {

                breakdown <- USSR_coal_breakdown[which(USSR_coal_breakdown$sector == sector_name), c('fuel', paste0( 'X', nonOECD_start_year : IEA_end_year ) ) ]

            }

            breakdown[ paste0( 'X', nonOECD_start_year : ( disagg_IEA_FSU_start_year - 1 ) ) ] <- breakdown[ paste0( 'X', disagg_IEA_FSU_start_year ) ]

            if( any( colSums( breakdown[ paste0('X',nonOECD_start_year:IEA_end_year) ] ) == 0 ) ) {

                breakdown[, which( colSums( breakdown[ paste0('X', nonOECD_start_year:IEA_end_year) ] ) == 0 ) + 1 ] <- rowMeans( breakdown[ paste0('X', disagg_IEA_FSU_start_year:IEA_end_year) ] )

            }

            ratio <- breakdown[ paste0('X',nonOECD_start_year:IEA_end_year) ] / matrix( data = rep( as.numeric( colSums( breakdown[ paste0('X',nonOECD_start_year:IEA_end_year) ] ) ), times = 2 ),
                                          nrow = 2, byrow = TRUE)

            aggregate <- FSU_activity_agg_corrected[ which( FSU_activity_agg_corrected$iso == iso_name &
                                                              FSU_activity_agg_corrected$agg_sector == sector_name &
                                                              FSU_activity_agg_corrected$agg_fuel %in% c( 'coal' ) ) , ]


            if( nrow(aggregate) == 0 ) { disag_new[ paste0('X', nonOECD_start_year:IEA_end_year) ] <- 0

            } else if ( sum( breakdown[ which( breakdown$fuel == 'hard_coal'), paste0('X', nonOECD_start_year:IEA_end_year) ] ) == 0 ) {

                disag_new[ paste0('X', nonOECD_start_year:IEA_end_year) ] <-  matrix( data = as.numeric( rep( aggregate[ paste0( 'X', nonOECD_start_year:IEA_end_year ) ] , times = 2 ) ), nrow = 2, byrow = TRUE ) * matrix(
                    data = rep( c(1, 0), times = length( paste0('X', nonOECD_start_year:IEA_end_year) ) ), nrow = 2, byrow = FALSE )

            } else if ( sum( breakdown[ which( breakdown$fuel == 'brown_coal' ), paste0('X', nonOECD_start_year:IEA_end_year) ] ) == 0 ) {

                disag_new[ paste0('X',nonOECD_start_year:IEA_end_year) ] <-  matrix( data = as.numeric(rep(aggregate[ paste0('X',nonOECD_start_year:IEA_end_year) ] , times = 2 )), nrow = 2, byrow = TRUE)*matrix(
                  data = rep( c(0, 1), times = length( paste0('X', nonOECD_start_year:IEA_end_year) ) ), nrow = 2, byrow = FALSE)

            } else {

                disag_new[ paste0('X', nonOECD_start_year:IEA_end_year) ] <-  matrix( data = as.numeric( rep( aggregate[ paste0('X', nonOECD_start_year:IEA_end_year) ], times = 2 ) ),
                                                               nrow = 2, byrow = TRUE) * ratio

            }

            if( any( abs( aggregate[ paste0('X', nonOECD_start_year:IEA_end_year) ] - colSums( disag_new[ paste0('X', nonOECD_start_year:IEA_end_year) ] ) ) > 1 ) ){

                stop( 'Disaggregation from coal to hard and brown coal is incorrect. Please Check Code' )
            }

            sector_list[[n]] <-  disag_new

        } #END sector loop

        iso_list[[i]] <- do.call( rbind, sector_list )

    } #END iso loop

    FSU_activity_disaggregate_fuel <- rbind( disaggregate, do.call(rbind, iso_list) )
    names( FSU_activity_disaggregate_fuel )[1:3] <- c( 'iso', 'fuel', 'agg_sector' )
    FSU_activity_disaggregate_fuel <-  FSU_activity_disaggregate_fuel[ with( FSU_activity_disaggregate_fuel, order( iso, fuel, agg_sector ) ), ]

    if ( abs( sum( FSU_activity_agg_corrected[ paste0( 'X',nonOECD_start_year:IEA_end_year) ] ) - sum( FSU_activity_disaggregate_fuel[ paste0('X', nonOECD_start_year:IEA_end_year) ] ) ) > 1 ){

        stop( 'Bad Disaggregation...' ) # TODO Improve this stop description

    }

    if ( anyNA( FSU_activity_disaggregate_fuel  ) ){ stop( 'NAs in FSU data - please check code...' ) }

    check <- FSU_activity_disaggregate_fuel
    activity_summary <- aggregate( check[, paste0('X', seq(1975,2010,5) ) ],
                                   by = list( fuel = check$fuel ),
                                   FUN = sum )

### TODO: This summary process, present in 3 or 4 diff. places, uses the melt() and cast()
###       commands. Should be rewritten with spread() and gather().
    original_activity_summary$set <- 'original'
    activity_summary$set <- 'final'
    summary <- rbind.fill( activity_summary, original_activity_summary )
    summary <- melt(summary)
    summary <- cast( summary, fuel + variable ~ set )
    summary$diff <- summary$final - summary$original
    summary <- summary[ -which( summary$fuel %in% c( 'biomass', 'process' ) ), ]
    writeData( summary, 'DIAG_OUT', 'A.FSU_energy_corrected_summary_after_coal_dissaggregation', meta=F )

### New code block?

# Disaggregate from agg_sectors to CEDS sectors using disagg_IEA_FSU_start_year to IEA_end_year data (1990 - IEA_end_year)
    temp_nrow <- length(FSU_iso) * length(ceds_fuel) * length(ceds_sector)
    temp <- data.frame( iso = rep( FSU_iso, each = length(ceds_fuel) * length(ceds_sector) , len = temp_nrow ),
                        fuel = rep( rep( ceds_fuel, each = length(ceds_sector) ), len = temp_nrow ),
                        sector = rep( ceds_sector, len = temp_nrow ) )
    temp[ paste0('X', nonOECD_start_year:IEA_end_year) ] <- NA
    temp$agg_sector <-  MSLevel[ match( temp$sector, MSLevel$working_sectors_v2 ) ,"FSU_extension" ]

#   Arrange data by iso + sector + fuel before correction loop
    temp <- temp %>%
        dplyr::arrange( iso, sector, fuel )

    activity <- activity %>%
        dplyr::arrange( iso, sector, fuel )

#   Map USSR aggregate data to aggregate sectors
    USSR_ceds_sector_fuel$agg_sector <-  MSLevel[ match( USSR_ceds_sector_fuel$sector, MSLevel$working_sectors_v2 ) ,"FSU_extension"]

    iso_list <- list()
    sector_list <- list()
    fuel_list <- list()

#   Begin iso loop
    for ( i in seq_along(FSU_iso) ) {

#       Begin fuel loop
        for( n in seq_along(ceds_fuel) ) {

#           Begin sector loop
            for( m in seq_along(agg_sector) ) {

                iso_name = FSU_iso[i]
                fuel_name = ceds_fuel[n]
                sector_name = agg_sector[m]
                disag_new <-  temp[ which( temp$iso == iso_name &
                                             temp$agg_sector == sector_name &
                                             temp$fuel == fuel_name ) , c( 'iso', 'sector', 'fuel', paste0( 'X', nonOECD_start_year : IEA_end_year ) ) ]

#               Create breakdown data frame for the iso + fuel + agg_sector in question,
#               made from the original disaggregate activity data
                breakdown <- activity[ which( activity$iso == iso_name &
                                                activity$agg_sector == sector_name &
                                                activity$fuel == fuel_name ) , c( 'sector', 'fuel', paste0('X', disagg_IEA_FSU_start_year:IEA_end_year) ) ]

#               Subset the aggregate activity data which will have its sectors
#               disaggregated, for the FSU iso in question
                aggregate <- FSU_activity_disaggregate_fuel[ which( FSU_activity_disaggregate_fuel$iso == iso_name &
                                                                      FSU_activity_disaggregate_fuel$agg_sector == sector_name &
                                                                      FSU_activity_disaggregate_fuel$fuel == fuel_name ), ]

#               If the breakdown data frame (made from the original disaggregate activity data)
#               has only 1 row OR sums to 0 from disagg_IEA_FSU_start_year to IEA_end_year data (1990 - IEA_end_year),
#               then create breakdowns from the aggregate USSR data
                if( nrow( breakdown ) < 2 |
                     sum( breakdown[ , paste0('X', disagg_IEA_FSU_start_year : IEA_end_year ) ] ) == 0) {

                    breakdown <- USSR_ceds_sector_fuel[ which( USSR_ceds_sector_fuel$agg_sector == sector_name &
                                                                 USSR_ceds_sector_fuel$fuel == fuel_name ) , c( 'sector','fuel' , paste0( 'X', disagg_IEA_FSU_start_year : IEA_end_year ) ) ]
                }

#               If the number of rows for the breakdown data frame does not match the number of rows
#               in the data frame which will contain the new disaggregated data, then
#               add the missing CEDS sectors to the breakdown data frame with values of 0 for all years.
#               Note that It is assumed here that only the breakdown data frame can have a mising sector,
#               and that the disag_new data frame will be complete.
                if( nrow( breakdown )!= nrow( disag_new )) {

                    breakdown <- replaceValueColMatch(disag_new[ , c( 'sector','fuel' , paste0('X', disagg_IEA_FSU_start_year : IEA_end_year) ) ], breakdown,
                                                      x.ColName = paste0( 'X', disagg_IEA_FSU_start_year : IEA_end_year ) ,
                                                      match.x = c( 'sector', 'fuel' ),
                                                      addEntries = FALSE )
                    breakdown[ is.na( breakdown ) ] <- 0

                }

#               If the aggregate FSU data for the given iso + CEDS fuel +  agg_sector
#               has 0 rows OR sums to zero, then assign the disaggregate sectors the value
#               of 0 for consumption of the given CEDS fuel.
                if( nrow(aggregate) == 0 |
                    rowSums( aggregate[ paste0('X', nonOECD_start_year:IEA_end_year) ] ) == 0 ) {

                    disag_new[ paste0('X', nonOECD_start_year:IEA_end_year) ] <- 0

#               Otherwise, disaggregate the aggregate data using the breakdowns
                } else {

#                   Assign breakdown values from 1971 ((or nonOECD_start_year) to 1989 to those of disagg_IEA_FSU_start_year (1990)
                    breakdown[ , paste0( 'X', nonOECD_start_year : ( disagg_IEA_FSU_start_year - 1 ) ) ] <-  breakdown[, paste0( "X", disagg_IEA_FSU_start_year ) ]
                    breakdown <- breakdown[ c( 'sector', 'fuel', paste0( 'X', nonOECD_start_year : IEA_end_year ) ) ]

#                   If any of the breakdown df columns sum to 0 from 1971 - IEA_end_year (or nonOECD_start_year - IEA_end_year)
                    if( length( which( colSums( breakdown[ paste0( 'X', nonOECD_start_year : IEA_end_year ) ] ) == 0 ) ) > 0 ) {

#                        and the number of columns that don't sum to 0 from 1971 ((or nonOECD_start_year) to IEA_end_year is greater than 0,
#                        then use the row means of the column sums
                         if( any( ncol( breakdown[ , which( colSums( breakdown[ paste0( 'X', nonOECD_start_year : IEA_end_year ) ] ) != 0 ) + 2 ] ) > 0 ) ) {

                             breakdown[, which(colSums(breakdown[ paste0( 'X', nonOECD_start_year : IEA_end_year) ] ) == 0 ) + 2 ] <- rowMeans(
                                                                    breakdown[ , ( which( colSums( breakdown[ paste0('X', nonOECD_start_year:IEA_end_year) ] ) != 0 ) + 2 ) ] )

#                        otherwise, use the column sums of the years yeras which are non-zero
                         }else{

                             breakdown[, which( colSums( breakdown[ paste0( 'X', nonOECD_start_year : IEA_end_year ) ] ) == 0 ) + 2] <- breakdown[ ,
                                                                                      ( which( colSums( breakdown[ paste0( 'X', nonOECD_start_year : IEA_end_year) ] ) != 0 ) + 2 ) ]
                         }
                    }

#                   Create a ratio (or share) from the breakdown data frme
#                   ( ratio = disagg_sector / agg_sector, where agg_sector = sum of disagg_sectors for the fuel
                    ratio <- breakdown[ paste0('X',nonOECD_start_year:IEA_end_year)]/matrix(
                                                data = rep(colSums(breakdown[ paste0('X',nonOECD_start_year:IEA_end_year)]) , times = nrow(breakdown) ),
                                                nrow = nrow(breakdown),
                                                byrow = TRUE)

#                   Create the new disaggregate data by multiplying the ratios (or subsector shares)
#                   by the aggregate sector data for the given FSU iso and CEDS fuel
                    disag_new[ paste0('X', nonOECD_start_year:IEA_end_year) ] <- ratio[ paste0('X', nonOECD_start_year:IEA_end_year) ] * matrix(
                                                                data = rep( as.numeric( aggregate[ paste0('X', nonOECD_start_year:IEA_end_year) ] ) , times = nrow(breakdown) ),
                                                                nrow = nrow(breakdown),
                                                                byrow = TRUE )
                }

#               Check if the difference between the original agg_sector data minus the new disaggregate
#               sectoral data has an absolute value > 1. If so, stop as the disaggregation did not work well.
#               TODO: Make this stop more informative
                if( any( abs( aggregate[ paste0( 'X', nonOECD_start_year : IEA_end_year ) ] - colSums( disag_new[ paste0( 'X', nonOECD_start_year : IEA_end_year ) ] ) ) > .1 ) ){

                    stop( 'Disaggregation from agg sectors to ceds sectors is bad' )

                }

#               End sector loop
                sector_list[[m]] <- disag_new
            }

#           End fuel loop
            fuel_list[[n]] <- do.call( rbind, sector_list )
        }

#       End iso loop
        iso_list[[i]] <- do.call( rbind, fuel_list )
    }

#   Bind the disaggregate sectoral FSU iso data
    FSU_final <- do.call( rbind, iso_list )

    if ( abs( sum( FSU_final[ paste0('X', nonOECD_start_year:IEA_end_year) ] ) - sum( FSU_activity_disaggregate_fuel[ paste0('X', nonOECD_start_year:IEA_end_year) ] ) ) > 1 ) stop( 'Bad Disaggregation' )


    FSU_final[ paste0('X', "", IEA_start_year : ( nonOECD_start_year - 1 ) ) ] <- 0
    FSU_final$units <- 'kt'
    FSU_final <- FSU_final[ c( 'iso', 'sector', 'fuel', 'units', paste0('X', "", IEA_start_year : IEA_end_year) ) ]
    if( anyNA( FSU_final ) ) stop( 'NAs in final FSU data, please check code' )


    check <- FSU_final
    activity_summary <- aggregate( check[, paste0('X', seq(1975, 2010, 5) ) ],
                                   by = list( fuel = check$fuel ),
                                   FUN = sum )

    original_activity_summary$set <- 'original'
    activity_summary$set <- 'final'
    summary <- rbind.fill( activity_summary, original_activity_summary )
    summary <- melt(summary)
    summary <- cast( summary, fuel + variable ~ set )
    summary <- summary[ -which( summary$fuel %in% c( 'biomass', 'process' ) ), ]

# -------------------------------------------------------------------------------------------
# 14. Combine New FSU data and other energy data
    final_activity <- rbind ( original_activity_other,
                              FSU_final,
                              original_activity_FSU_nc)

#   Replace biomass with original data
    final_activity[ which( final_activity$fuel == 'biomass' ), X_IEA_years ] <- 0
    final_activity <- replaceValueColMatch( final_activity, biomass_activity,
                                            x.ColName = X_IEA_years,
                                            match.x = c( 'iso', 'sector', 'fuel', 'units' ),
                                            addEntries = FALSE )
#   Remove zero rows
    final_activity <- final_activity[ which( rowSums( final_activity[X_IEA_years] ) != 0 ), ]

#   Sort
    final_activity <- final_activity[ with( final_activity, order( iso, sector, fuel ) ), ]

#   Stop if any activity data is < 0
    negative_activity_check <- final_activity %>%
      dplyr::filter( sector %!in% c( "hard_coal-adl-dom-supply", "natural_gas-adl-dom-supply",
                                     "diesel_oil-adl-dom-supply", "light_oil-adl-dom-supply",
                                     "heavy_oil-adl-dom-supply", "refinery-and-natural-gas" ) )

    if( any( negative_activity_check[, X_IEA_years ] < 0 ) ){

      stop( "There are negative values in the activity data. This should not occur..." )

    }

# -------------------------------------------------------------------------------------------
# 15. Write Output

# Main output
  writeData( final_activity, 'MED_OUT', 'A.en_biomass_fsu_fix' )

# Diagnostics
  writeData( summary, 'DIAG_OUT', 'FSU_corrected_summary' )
  writeData( summary, 'DIAG_OUT', 'A.FSU_energy_corrected_summary', meta = F )

    logStop()

# END
