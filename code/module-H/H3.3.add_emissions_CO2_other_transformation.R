# Program Name: H3.3.add_emissions_CO2_other_transformation.R
# Author: Linh Vu, Presley Muwan
# Date Last Updated: 1st May 2017
# Program Purpose: Compute CO2 other transformation coal:
#     CO2_Conversion = CO2_Coal_Total - CO2_Coal_Combustion - CO2_Coal_NEuse
#                      - CO2_1b1and2c
#   using coal consumption and default EF.
# Input Files: A.IEA_en_stat_ctry_hist.csv, CO2_total_CEDS_emissions.csv,
#   H.Extended_coal_by_fuel_combustion.csv, H.Extended_other_tranformation_coal.csv
#   H.CO2_total_CEDS_emissions_before_other_transformation_replacement.csv,
#   B.CO2_comb_EF_non-bunker.csv, IEA_product_fuel.csv, Master_Fuel_Sector_List.csv
# Output Files: H.CO2_calculated_other_transformation_emissions.csv
# Notes: EFs already multiplied by fraction oxidized.
# TODO:
#   -- Extend total coal consumption using BP\
#   -- Disaggregate extended other transformation coal instead of assuming hard coal
#   -- Fix hard-coded years
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( 'data_functions.R')
log_msg <- "Calculating CO2 emissions conversion for 1A1bc-Other-transformation coal"
script_name <- 'H3.3.add_emissions_CO2_other_transformation.R'

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "CO2"

# Stop script if running for unsupported species
if( em != 'CO2') {
  stop('This script should not run for emission species other than
       CO2, please check H3.1.proc_Extended_Emissions')
}

# ---------------------------------------------------------------------------
# 1. Input
  A.IEA_en_stat_ctry_hist <- readData( "MED_OUT", "A.IEA_en_stat_ctry_hist" )
  A.en_stat_sector_fuel <-readData( "MED_OUT", "A.en_stat_sector_fuel" )
  CO2_total_CEDS_emissions <- readData( "MED_OUT", paste0( 'H.', em,'_total_CEDS_emissions_before_other_transformation_replacement') )
  coal_ef <- readData( "DIAG_OUT", "B.CO2_comb_EF_non-bunker" )
  IEA_product_fuel <- readData( "MAPPINGS", "/energy/IEA_product_fuel" )
  emission_coefficient <- readData( "DEFAULT_EF_IN", "CO2_base_EF", ".xlsx",
                                    sheet_selection = "Emission_Coefficient" )
  fraction_oxidized <- readData( "DEFAULT_EF_IN", "CO2_base_EF", ".xlsx",
                                 sheet_selection = "Fraction_Oxidized" )
  MSL <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Sectors" )
  iea_start <- readData('EXT_IN','IEA_start_date', ".xlsx", sheet_selection = "coal")

  A.full_comb_activity_extended_coal <- readData( "MED_OUT", "A.comb_default_activity_extended.csv" )
  A.total_activity_extended_natural_gas <- readData( "MED_OUT", "A.activity_extended_natural_gas" )

# Define values
  ceds_coal_fuels <- c( "brown_coal", "coal_coke", "hard_coal" )
  ceds_comb_sectors <- MSL$sector[ MSL$type == "comb" ]


#--------------------------------------------------------------------------------
# 2. Function implementations and initializations

  # -----------------------------------------------------------------------------
  # compute_co2_em_from_coal_gases
  # Brief:         calculates the CO2 emission from coal gases
  # Details:       Extract the emission coeficient and the faction oxidized data for the specified coal gas from the 'CO2_base_EF.xlsx' file
  #                and computes the coal gas' emission factor by multiplying them (emission coeficient and the faction oxidized).
  #                The emission factor is then used to calculate the CO2 emission  by multiplying the emission factor by the Coal gas statistic data in
  #                the "A.en_stat_sector_fuel.cvs"
  # Dependencies:
  # Author(s):    Presley Muwan
  # Params:
  #               coal_gas - name of the coal gas
  #               coal_gas_sector - The coal gas' sector
  #               emission_coefficient_df - Dataframe with emission coefficient for the coal gas (read from system's  file)
  #               A.en_stat_sector_fuel_df - Datafram with IEA energy statical data for sectors and countries (read from system's file)
  # Return:
  #               CO2_Coalgases_em - The coal gas' computed emission
  # Input Files:
  # Output Files:

  compute_co2_em_from_coal_gases <- function(coal_gas, coal_gas_sector, emission_coefficient_df, A.en_stat_sector_fuel_df){

    #DEBUG
    #coal_gas = "other_recovered_gases"
    #coal_gas_sector = "other_recovered_gases"
    #emission_coefficient_df = emission_coefficient
    #A.en_stat_sector_fuel_df = A.en_stat_sector_fuel

    #Convert the coal gas' emission factor from kt CO2/TJ to kt CO2/Kt
    # 'other_recovered_gases' is represented by the name 'oxygen_steel_furnace_gas' within the emission_coeficient_df
    #  so 'oxygen_steel_furnace_gas' is used to extract the emission coeficient if the current coal gas is 'other_recovered_gases'.
    #  Else the coal gas' name is used; for other coal gases
    if(coal_gas == "other_recovered_gases"){
      emission_coefficient_df$Emission_Coefficient[emission_coefficient_df$fuel == "oxygen_steel_furnace_gas"] <-
        (emission_coefficient_df$Emission_Coefficient[emission_coefficient_df$fuel == "oxygen_steel_furnace_gas"] *
           conversionFactor_naturalgas_TJ_per_kt)
      emission_coefficient_df$units[ emission_coefficient_df$fuel == "oxygen_steel_furnace_gas" ] <- "kt CO2/kt"

    }else{
      emission_coefficient_df$Emission_Coefficient[emission_coefficient_df$fuel == coal_gas] <-
        (emission_coefficient_df$Emission_Coefficient[emission_coefficient_df$fuel == coal_gas] *
           conversionFactor_naturalgas_TJ_per_kt)
      emission_coefficient_df$units[ emission_coefficient_df$fuel == coal_gas ] <- "kt CO2/kt"

    }#if-else


    # retrieve IEA consumption data for each coal gas in coal_gas_sector above
    IEA_en_coalgas_data <- A.en_stat_sector_fuel_df
    IEA_en_coalgas_data <-  IEA_en_coalgas_data[IEA_en_coalgas_data$sector == coal_gas_sector, ]

    # calulate the Natural gas coeficient
    #obtain natural gas's oxidation fraction
    default_ng_fraction_oxidized <- fraction_oxidized$Fraction_Oxidized[fraction_oxidized$fuel== "natural_gas"]

    #'other_recovered_gases' is represented by the name 'oxygen_steel_furnace_gas' within the emission_coeficient file
    # so 'oxygen_steel_furnace_gas' is used to extract the emission coeficient if the current coal gas is 'other_recovered_gases'.
    #Else the coal gas' name is used; for other coal gases
    if(coal_gas == "other_recovered_gases"){
      default_em_coeficient  <- emission_coefficient_df$Emission_Coefficient[emission_coefficient_df$fuel == "oxygen_steel_furnace_gas"]
    }else{
      default_em_coeficient  <- emission_coefficient_df$Emission_Coefficient[emission_coefficient_df$fuel == coal_gas]

    }#if-else Ends

    #compute the natural gas emission coeficient
    ng_em_coeficient <- (default_em_coeficient * default_ng_fraction_oxidized)

    # Calc CO2 emission from coal gas
    CO2_Coalgases_em <- IEA_en_coalgas_data
    CO2_Coalgases_em[X_IEA_years] <- (CO2_Coalgases_em[X_IEA_years] * ng_em_coeficient)

    return(CO2_Coalgases_em)

  }#compute_co2_em_from_coal_gases() Ends


  # -----------------------------------------------------------------------------
  # extractFlows
  # Brief:         extract relevant flows given iso
  # Details:
  # Dependencies:
  # Author(s):    Rachel Hoesly
  # Params:
  #               CO2_Coal_Total_agg
  #               CO2_Coal_NEuse_agg
  #               CO2_Coal_Combustion_agg
  #               selected_iso
  # Return:
  #              coal_all
  # Input Files:
  # Output Files:
  extractFlows <- function( CO2_Coal_Total_agg, CO2_Coal_NEuse_agg, CO2_Coal_Combustion_agg, selected_iso="global" ) {

    # Extract relevant iso
    if( selected_iso == "global" ) {
      coal_total <- CO2_Coal_Total_agg
      coal_neuse <- CO2_Coal_NEuse_agg
      coal_combustion <- CO2_Coal_Combustion_agg
      coal_total$iso <- selected_iso
      coal_neuse$iso <- selected_iso
      coal_combustion$iso <- selected_iso

    } else if( selected_iso %!in% CO2_Coal_Total_agg$iso ) {
      stop( "extractFlows(): Invalid iso")

    } else {
      coal_total <- filter( CO2_Coal_Total_agg, iso==selected_iso )
      coal_neuse <- filter( CO2_Coal_NEuse_agg, iso==selected_iso )
      coal_combustion <- filter( CO2_Coal_Combustion_agg, iso==selected_iso )
    }#else-if Ends

    # Reformat
    coal_total$flow <- "CO2_Coal_Total"
    coal_neuse$flow <- "CO2_Coal_NEuse"
    coal_combustion$flow <- "CO2_Coal_Combustion"

    # Aggregate
    coal_all <- bind_rows( coal_total, coal_neuse, coal_combustion ) %>%
      group_by( iso, flow, units ) %>%
      summarise_all( funs( sum(., na.rm = T ) ) ) %>%
      data.frame()

    return( coal_all )
  }#extractFlows() Ends

# ---------------------------------------------------------------------------
# 3. Prelim processing
# Extend 1960 coal EF constant back to 1750
# TODO: make robust
  coal_ef_ext <- filter( coal_ef, fuel %in% ceds_coal_fuels )
  years_to_add <- X_extended_years[ X_extended_years %!in% names( coal_ef_ext ) ]
  coal_ef_ext[, years_to_add ] <- coal_ef_ext[, X_start_year ]
  coal_ef_ext <- coal_ef_ext[ c( "iso", "fuel", "units", X_extended_years ) ]

# Melt coal EF to long format
  coal_ef_ext_long <- melt( coal_ef_ext, id=c( "iso", "fuel", "units" ) )
  names( coal_ef_ext_long )[ names( coal_ef_ext_long ) %in% c( "variable", "value" ) ] <-
    c( "year", "coal_EF" )

# Add CEDS fuel column to A.IEA_en_stat_ctry_hist
  A.IEA_en_stat_ctry_hist$fuel <- IEA_product_fuel$fuel[ match(
    A.IEA_en_stat_ctry_hist$PRODUCT, IEA_product_fuel$product ) ]

#-------------------------------------------------------------------------------------------------
# 4. Compute CO2 emission from Coal gases
  # a.Calc CO2 emission for all coal gases, treating them as natural gases (NG); that is using NATURAL GAS' emission coeficient
  #   to calcluate the CO2 emission for all the Coal gases
  coal_gas_sectors <- c("blast_furnace_gas", "other_recovered_gases","coke_oven_gas", "gas_works_gas")

  CO2_Coalgases_em_as_ng_list  <- mapply(FUN = compute_co2_em_from_coal_gases, coal_gas = "natural_gas", coal_gas_sector = coal_gas_sectors,
                             MoreArgs = list(emission_coefficient_df = emission_coefficient, A.en_stat_sector_fuel_df = A.en_stat_sector_fuel),
                             SIMPLIFY = FALSE)
  #Rebind the list and aggregate data for duplicate countries
  CO2_Coalgases_as_ng <- do.call( rbind, CO2_Coalgases_em_as_ng_list) %>%  dplyr::arrange(iso) %>% ddply("iso",numcolwise(sum))

  #Extension driver data
  CO2_Coalgases_driver_trend <- A.total_activity_extended_natural_gas

  # b. Extend CO2 coalgas emmissions backward to 1850, from 1960 for OECD countries and 1971 for Non-OECD countries, and forward from 2013 - 2014
  CO2_Coalgases_as_ng_for_total_natural_gas_list = lapply(unique(iea_start$start_year), function(sy) {
      extend_data_on_trend_range(input_data = CO2_Coalgases_as_ng,
                                 driver_trend = CO2_Coalgases_driver_trend,
                                 start = 1950, end = 1970, expand = F,
                                 id_match.driver = c('iso'),
                                 extend_fwd_by_BP_years = T,
                                 IEA_mode = T, iea_start_year = sy,
                                 iea_start_years_df = iea_start)
  })

  #Rebind the list and aggregate data for duplicate countries
  CO2_Coalgases_as_ng_for_total_natural_gas <- do.call( rbind, CO2_Coalgases_as_ng_for_total_natural_gas_list) %>%
                                                        dplyr::mutate( units = 'kt', fuel = 'process')  %>% dplyr::arrange(iso)
  #Linearly extend CO2 coal gas data from 2013 to 2014
  CO2_Coalgases_as_ng_for_total_natural_gas[paste0('X',BP_years)] <- CO2_Coalgases_as_ng_for_total_natural_gas[paste0('X', 2013)]

  #add columns X1750 to X1849 and initialized them with zero
  excluded_years <- X_extended_years[X_extended_years %!in% names(CO2_Coalgases_as_ng_for_total_natural_gas)]
  CO2_Coalgases_as_ng_for_total_natural_gas[excluded_years] <- 0
  CO2_Coalgases_as_ng_for_total_natural_gas <- CO2_Coalgases_as_ng_for_total_natural_gas[ c('iso','fuel','units', X_extended_years ) ] %>% dplyr::arrange(iso)

# ---------------------------------------------------------------------------
# 5. Compute CO2_Coal_Total
# Mass Balance - Total CO2 emissions from Coal = Primary Energy Coal*EF +
# Secondary Energy Coal (import - exports , Coal Coke)* EF
  # Add other transformation to combustion coal as "hard coal"
  # Calculate Net coal coke from IEA (Imports - Exports) and extended ceds coke
  # (production + imports - exports)

# a. Add other transformation coal as hard coal
  # A.full_comb_activity_extended_coal
  extended_coal <- A.full_comb_activity_extended_coal
  extended_coal <- A.full_comb_activity_extended_coal %>% filter(fuel %in% c('coal_coke', 'brown_coal',
                                                                             'hard_coal'))
  extended_coal <- extended_coal[ c( "iso", "fuel", "units", X_extended_years ) ]
  extended_coal <- group_by( extended_coal, iso, fuel, units ) %>%
    summarise_all( funs( sum(., na.rm = T ) ) ) %>% data.frame()

# b. Extend Coal Coke (imports - exports) with extended coal coke (production + imports - exports)
  # extended total coal coke value
  extended_coke <- A.full_comb_activity_extended_coal %>% filter(fuel == 'coal_coke') %>%
      group_by(iso, fuel, units) %>%
      summarize_if(is.numeric,sum) %>%
    dplyr::mutate(value = 'ceds_coke') %>%
    dplyr::arrange(iso)

  # calculate IEA coke (imports - exports)
  IEA_coke <- filter( A.IEA_en_stat_ctry_hist, FLOW %in% c("IMPORTS", "EXPORTS"), fuel=="coal_coke" ) %>%
    select( -FLOW, -PRODUCT ) %>% group_by( iso, fuel ) %>%
    summarise_all( funs( sum(., na.rm = T ) ) ) %>%
    data.frame() %>%
    mutate(value = 'iea_coke')

  # add countries with zero data
  zero_coke_countries <- unique( extended_coke$iso[ extended_coke$iso %!in% IEA_coke$iso ] )
  if ( length( zero_coke_countries ) > 0 ) {
    add_zero_coke_countries <- data.frame( iso = zero_coke_countries, fuel = 'coal_coke', value = 'iea_coke' )
    add_zero_coke_countries[ X_IEA_years ] <- 0
    IEA_coke <- rbind(IEA_coke, add_zero_coke_countries) %>%
        arrange(iso)
    }
  if ( any( extended_coke$iso != IEA_coke$iso ) ) { stop('Rows do not match for ') }


  #**** extend IEA coke back to 1750, from 1960 for OECD countries and 1971 for Non-OECD countries and forward from 2013 - 2014
  iea_extended_coke_list = lapply(unique(iea_start$start_year), function(sy) {


      extend_data_on_trend_range(input_data = IEA_coke,
                                 driver_trend = extended_coke,
                                 start = 1750, end = (sy-1), expand = F,
                                 id_match.driver = c('iso','fuel'),
                                 extend_fwd_by_BP_years = T,
                                 IEA_mode = T,
                                 iea_start_year = sy,
                                 iea_start_years_df = iea_start)
  })

  #Rebind the list and add the units column to the dataframe
  iea_extended_coke <- do.call( rbind, iea_extended_coke_list) %>%
                          dplyr::mutate( units = 'kt')

  #Linearly extend coke data from 2013 to 2014
  iea_extended_coke[paste0('X',BP_years)] <- iea_extended_coke[paste0('X', 2013)]

  #Re-arange dataframe column
  iea_extended_coke <- iea_extended_coke[ c('iso','fuel','units', X_extended_years ) ] %>% dplyr::arrange(iso)

  # Make new data frame for energy data for calculating total CO2
  total_coal <- rbind( iea_extended_coke ,filter(extended_coal, fuel != 'coal_coke') ) %>% dplyr::arrange(iso, fuel)

# c. Compute CO2 emissions from total coal consumption and EF
  total_coal_long <- melt( total_coal , id = c( "iso", "fuel", "units" ) ) %>%
    select( iso, fuel, year=variable, total_value=value ) %>% data.frame()
  CO2_Coal_Total <- merge( total_coal_long, coal_ef_ext_long, all.x = T ) %>%
    dplyr::mutate( em = total_value * coal_EF )
  CO2_Coal_Total$units <- "kt"
  CO2_Coal_Total <- cast( CO2_Coal_Total, iso+fuel+units~year, value="em" ) %>%
    dplyr::arrange( iso, fuel, units )

  # ---------------------------------------------------------------------------
# 6. Compute CO2_Coal_NEuse = CO2 from IEA NONENUSE coal
# IEA energy data tends to go out back in time. We'll extrapolate IEA Nonenuse
# coal using total coal and fractions of nonenuse coal over total coal at a split
# year:
#   IEA_nonenuse_coal_ext[fuel, year] = total_coal[year] * IEA_nonenuse_coal[fuel, COAL_NEUSE_RATIO_YEAR] /
#                                   total_coal[fuel, COAL_NEUSE_RATIO_YEAR]
# unless there is existing IEA data for that year

  COAL_NEUSE_RATIO_YEAR <- 2007
  X_coal_neuse_data_years <- paste0( "X", COAL_NEUSE_RATIO_YEAR:end_year )
  X_coal_neuse_nodata_years <- X_extended_years[ X_extended_years %!in% X_coal_neuse_data_years ]

# a. First compute NEuse coal consumption
# Get IEA NONENUSE coal
  IEA_neuse_coal <- filter( A.IEA_en_stat_ctry_hist, FLOW == "NONENUSE", fuel %in% ceds_coal_fuels ) %>%
    select( -FLOW, -PRODUCT ) %>% group_by( iso, fuel ) %>%
    summarise_all( funs( sum(., na.rm = T ) ) ) %>% data.frame()
  IEA_neuse_coal$units <- "kt"

# Extend 2013 value to 2014
  IEA_neuse_coal[paste0('X',BP_years)] <- IEA_neuse_coal[paste0('X',IEA_end_year)]

# # To determine what year has most values (2007)
#   df <- IEA_neuse_coal
#   df[ df!=0 ] <- 1
#   df <- group_by(df, iso, fuel, units) %>% summarise_each(funs(sum(., na.rm=T))) %>% data.frame()
#   df <- melt(df, id=c("iso", "fuel","units" )) %>% filter(value==max(value))

# Add rows for all iso+fuel combinations and columns for all years.
# Make values before COAL_NEUSE_RATIO_YEAR zero
  IEA_neuse_coal_ext <- merge( total_coal[ c( "iso", "fuel", "units" ) ], IEA_neuse_coal, all.x = T )
  IEA_neuse_coal_ext[ X_coal_neuse_nodata_years ] <- NA
  IEA_neuse_coal_ext <- IEA_neuse_coal_ext[ c( "iso", "fuel", "units", X_extended_years ) ]
  IEA_neuse_coal_ext[ is.na( IEA_neuse_coal_ext ) ] <- 0

# Melt and add column of total coal
  IEA_neuse_coal_ext_long <- melt( IEA_neuse_coal_ext, id=c( "iso", "fuel", "units" ) )
  names( IEA_neuse_coal_ext_long )[ names( IEA_neuse_coal_ext_long ) %in% c( "variable", "value" ) ] <-
    c( "year", "neuse_value" )
  IEA_neuse_coal_ext_long <- merge( IEA_neuse_coal_ext_long, total_coal_long )

# Calculate IEA_neuse_coal[fuel, COAL_NEUSE_RATIO_YEAR] / total_coal[fuel, COAL_NEUSE_RATIO_YEAR]
  neuse_ratio <- filter( IEA_neuse_coal_ext_long, year == paste0( "X", COAL_NEUSE_RATIO_YEAR ) ) %>%
    dplyr::mutate( ratio = neuse_value / total_value )
  neuse_ratio$ratio[ neuse_ratio$neuse_value == 0 | neuse_ratio$total_value == 0 ] <- 0
  neuse_ratio <- neuse_ratio[ c( "iso", "fuel", "ratio" ) ]

# Extend before COAL_NEUSE_RATIO_YEAR using total_coal and COAL_NEUSE_RATIO_YEAR neuse fractions:
#   IEA_neuse_coal_ext[fuel, year] = total_coal[year] * IEA_neuse_coal[fuel, COAL_NEUSE_RATIO_YEAR] /
#                                   total_coal[fuel, COAL_NEUSE_RATIO_YEAR]
  IEA_neuse_coal_ext_long <- merge( IEA_neuse_coal_ext_long, neuse_ratio )
  IEA_neuse_coal_ext_long$neuse_value_ext <- IEA_neuse_coal_ext_long$total_value *
    IEA_neuse_coal_ext_long$ratio

# Replace extrapolated values with original value, if original exists
  IEA_neuse_coal_ext_long$neuse_value_ext[ IEA_neuse_coal_ext_long$neuse_value != 0 ] <-
    IEA_neuse_coal_ext_long$neuse_value[ IEA_neuse_coal_ext_long$neuse_value != 0 ]
  IEA_neuse_coal_ext_long <- IEA_neuse_coal_ext_long[ c( "iso", "fuel", "year", "neuse_value_ext" ) ]

# b. Compute CO2 emissions from NEuse coal consumption and EF
  CO2_Coal_NEuse <- merge( IEA_neuse_coal_ext_long, coal_ef_ext_long, all.x = T ) %>%
    dplyr::mutate( em = neuse_value_ext * coal_EF )
  CO2_Coal_NEuse$units <- "kt"
  CO2_Coal_NEuse <- cast( CO2_Coal_NEuse, iso+fuel+units~year, value = "em" ) %>%
    dplyr::arrange( iso, fuel, units )

# ---------------------------------------------------------------------------
# 7. Compute CO2_Coal_Combustion = CO2 from Coal from CEDS combustion emission sectors
  CO2_Coal_Combustion <- filter( CO2_total_CEDS_emissions, sector %in% ceds_comb_sectors, fuel %in% ceds_coal_fuels ) %>%
    select( -sector ) %>% group_by( iso, fuel, units ) %>%
    summarise_all( funs( sum(., na.rm = T ) ) ) %>%
    dplyr::arrange( iso, fuel, units ) %>% data.frame()

# ---------------------------------------------------------------------------
# 8. Compute CO2_Conversion = CO2_Coal_Total - CO2_Coal_Combustion - CO2_Coal_NEuse - CO2_Coalgases_as_ng_for_total_natural_gas
# Aggregate all coal flows by iso
  CO2_Coal_Total_agg <- select( CO2_Coal_Total, -fuel ) %>% group_by( iso, units ) %>%
    summarise_all( funs( sum(., na.rm = T ) ) ) %>% dplyr::arrange( iso ) %>% data.frame()
  CO2_Coal_Combustion_agg <- select( CO2_Coal_Combustion, -fuel ) %>% group_by( iso, units ) %>%
    summarise_all( funs( sum(., na.rm = T ) ) ) %>% dplyr::arrange( iso ) %>% data.frame()
  CO2_Coal_NEuse_agg <- select( CO2_Coal_NEuse, -fuel ) %>% group_by( iso, units ) %>%
    summarise_all( funs( sum(., na.rm = T ) ) ) %>% dplyr::arrange( iso ) %>% data.frame()

  CO2_Coalgases_as_ng_for_total_natural_gas_agg <- select( CO2_Coalgases_as_ng_for_total_natural_gas, -fuel ) %>%
    group_by( iso, units ) %>% summarise_all( funs( sum(., na.rm = T ) ) ) %>% dplyr::arrange( iso ) %>% data.frame()

# Check that all 4 dfs have same ID columns
  if( any( paste( CO2_Coal_Total$iso, CO2_Coal_Total$units ) !=
           paste( CO2_Coal_Combustion$iso, CO2_Coal_Combustion$units ) ) |
      any( paste( CO2_Coal_Combustion$iso, CO2_Coal_Combustion$units ) !=
           paste( CO2_Coal_NEuse$iso, CO2_Coal_NEuse$units ) ) |
      any( paste( CO2_Coal_NEuse$iso, CO2_Coal_NEuse$units ) !=
           paste( CO2_Coal_Total$iso, CO2_Coal_Total$units ) ) ) {
    stop( "ID columns do not match.")
  }


# Compute CO2_Conversion from aggregated flows
  CO2_Conversion <- CO2_Coal_Total_agg
  CO2_Conversion[, X_extended_years ] <- CO2_Coal_Total_agg[, X_extended_years] -
    CO2_Coal_Combustion_agg[, X_extended_years] - CO2_Coal_NEuse_agg[, X_extended_years]
  CO2_Conversion[which(CO2_Conversion$iso  %in%  CO2_Coalgases_as_ng_for_total_natural_gas_agg$iso), X_extended_years] <-
    CO2_Conversion[which(CO2_Conversion$iso  %in%  CO2_Coalgases_as_ng_for_total_natural_gas_agg$iso), X_extended_years] -
    CO2_Coalgases_as_ng_for_total_natural_gas_agg[which(CO2_Coalgases_as_ng_for_total_natural_gas_agg$iso %in% CO2_Conversion$iso ), X_extended_years]

# Make negative values zero. Keep diagnostics of negative values
  diag_subzero <- melt( CO2_Conversion, id=c( "iso", "units" ) ) %>%
    filter( value < 0 ) %>% cast()
  diag_subzero[ is.na( diag_subzero ) ] <- ""
  CO2_Conversion[ CO2_Conversion < 0 ] <- 0

# Clean up after aggregation
  CO2_Conversion$fuel <- "process"
  CO2_Conversion$sector <- "1A1bc_Other-transformation"
  CO2_Conversion <- CO2_Conversion[ c( "iso", "sector", "fuel", "units", X_extended_years ) ]

# ---------------------------------------------------------------------------
# 9. Subtract Edgar 1B1 fugitive solid fuel emissions and 2c scaled ceds values

  # aggregate 1B1 and 2C by iso
  CEDS_1B1and2c <- CO2_total_CEDS_emissions[which(CO2_total_CEDS_emissions$sector %in%
                                                    c('1B1_Fugitive-solid-fuels','2C_Metal-production')),]
  CO2_1B1and2c <- aggregate(CEDS_1B1and2c[X_extended_years],
                            by = list(iso = CEDS_1B1and2c$iso), sum)

  # add zeros for gum and srb
  fill_in_countries <- data.frame(mat.or.vec(2,(length(X_extended_years))))
  fill_in_countries <- cbind(c("gum","srb (kosovo)"),fill_in_countries)
  names(fill_in_countries) <- c('iso',X_extended_years)

  CO2_1B1and2c <- rbind(CO2_1B1and2c,fill_in_countries)
  CO2_1B1and2c <- dplyr::arrange(CO2_1B1and2c,iso)

  # Check that all 3 dfs have same ID columns
  if( any( CO2_1B1and2c$iso !=
           CO2_Conversion$iso ) ) {
    stop( "ID columns do not match.")
  }

  # subtract 1B1 and 2c from conversion
  CO2_Conversion_1B1and2c <- CO2_Conversion
  CO2_Conversion_1B1and2c[X_extended_years] <- CO2_Conversion_1B1and2c[X_extended_years] - CO2_1B1and2c[X_extended_years]

  # Make negative values zero. Keep diagnostics of negative values
  diag_subzero_1B1and2c <- melt( CO2_Conversion_1B1and2c, id=c( "iso", "units" ) ) %>%
      filter( value < 0 ) %>% cast()
  diag_subzero_1B1and2c[ is.na( diag_subzero_1B1and2c ) ] <- ""
  CO2_Conversion_1B1and2c[ CO2_Conversion_1B1and2c < 0 ] <- 0


# ---------------------------------------------------------------------------
# 10. Diagnostics
  #a.Extract flows for all countries and some big countries
  global <- extractFlows( CO2_Coal_Total_agg, CO2_Coal_NEuse_agg, CO2_Coal_Combustion_agg, "global" )
  deu <- extractFlows( CO2_Coal_Total_agg, CO2_Coal_NEuse_agg, CO2_Coal_Combustion_agg, "deu" )
  usa <- extractFlows( CO2_Coal_Total_agg, CO2_Coal_NEuse_agg, CO2_Coal_Combustion_agg, "usa" )
  diag_flows <- bind_rows( global, deu, usa )


  # combine components of CO2-other transformation for diagnostics
  CO2_components_other_tranformation <- rbind.fill(
    CO2_Coal_Total %>% dplyr::mutate(value = paste0('Total_',fuel)) %>% select(-fuel),
    CO2_Coal_Combustion %>% dplyr::mutate(value = paste0('combustion_',fuel)) %>% select(-fuel) ,
    CO2_Coal_NEuse %>% dplyr::mutate(value = paste0('NEuse_',fuel)) %>% select(-fuel),
    CO2_1B1and2c %>% dplyr::mutate(value = '1B1and2c'),
    CO2_Conversion_1B1and2c %>% dplyr::mutate(value = 'final_emissions') ) %>% select(iso,value,units,contains('X'))

  #b. Calc CO2 emission for all coal gases using their specific emission factors
  CO2_Coalgases_emission_list  <- mapply(FUN = compute_co2_em_from_coal_gases, coal_gas = coal_gas_sectors, coal_gas_sector = coal_gas_sectors,
                                         MoreArgs = list(emission_coefficient_df = emission_coefficient, A.en_stat_sector_fuel_df = A.en_stat_sector_fuel),
                                         SIMPLIFY = FALSE)
  CO2_Coalgases_emission <- do.call( rbind, CO2_Coalgases_emission_list) %>%  dplyr::arrange(iso)

# ---------------------------------------------------------------------------
# 11. Output
  writeData( CO2_Conversion, "DIAG_OUT", "H.CO2_calculated_other_transformation_emissions_before_1B1and2c_correction" )
  writeData( CO2_Conversion_1B1and2c, "MED_OUT", "H.CO2_calculated_other_transformation_emissions" )
  writeData( CO2_Coal_Total, "DIAG_OUT", "H.CO2_Coal_Total" )
  writeData( CO2_Coal_NEuse, "DIAG_OUT", "H.CO2_Coal_NEuse" )
  writeData( CO2_Coal_Combustion, "DIAG_OUT", "H.CO2_Coal_Combustion" )
  writeData( CO2_components_other_tranformation, "DIAG_OUT", "H.CO2_components_other_tranformation" )
  writeData( CO2_Coalgases_as_ng_for_total_natural_gas, "DIAG_OUT", "H.CO2_Coalgases_as_ng_for_total_natural_gas" )
  writeData( CO2_Coalgases_emission, "DIAG_OUT", "H.CO2_Coalgases_emission" )
  writeData( diag_flows, "DIAG_OUT", "H.CO2_conversion_selected_flows" )

if( nrow( diag_subzero ) > 0 ) {
  writeData( diag_subzero, "DIAG_OUT", "H.CO2_conversion_subzero"  )
}

logStop()
