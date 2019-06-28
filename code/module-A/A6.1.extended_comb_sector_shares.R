# ------------------------------------------------------------------------------
# Program Name: A6.1.extended_comb_sector_shares.R
# Author: Rachel Hoesly, Caleb Braun, Patrick O'Rourke
# Last Updated:    June 4, 2019
# Program Purpose: Calculate default combustion fuel shares for extended historical data
#     Combines default CEDS estimates from IEA with Bond fuel share data to produce
#     sectoral estimates of fossil fuel use (in the form of fuel shares) for the entire period
# Input Files:  CD.Bond_sector_percentages.csv
#               ext_sector_breakdown_assumptions.csv
#               ext_sector_percents_start_assumptions.csv
#               A.comb_activity_with_other.csv
#               IEA_iso_start_data.csv
#               Bond_sector_ext_map.xlsx --> sheet CEDS_to_ext
# Output Files: A.final_sector_shares.csv
# TODO:
#       - better feedstock extension
# ---------------------------------------------------------------------------
# 0. Read in global settings and headers
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files
headers <- c( "data_functions.R","process_db_functions.R" )
log_msg <- "Extending Combustion data with bond and IEA"
script_name <- "A6.1.extended_comb_sector_shares.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )


# ---------------------------------------------------------------------------
# 1. Load files

bond_sector_percentages <-
    readData( 'EXT_IN', 'CD.Bond_sector_percentages.csv' )

ext_sector_breakdown_assumptions <-
    readData( 'EXT_IN', 'ext_sector_breakdown_assumptions')

ext_sector_percents_start_assumptions <-
    readData( 'EXT_IN', 'ext_sector_percents_start_assumptions', meta = F )

activity_all <- readData( 'MED_OUT', 'A.comb_activity_with_other', meta = F )

iea_start_year <- readData( 'ENERGY_IN' , 'IEA_iso_start_data' )

ext_sector_map <- readData( "MAPPINGS", "Bond_sector_ext_map", ".xlsx",
                             sheet_selection = 'CEDS_to_ext', meta = F,
                             domain_extension = "Bond/" )

# ---------------------------------------------------------------------------
# 2. Define Variables, select options, and define functions used within script

# Define the number of years before extended aggregate sector splits are fully from BOND
# data. This is used when transitioning from CEDS to BOND agg. sector splits (extending back in time).
# For example, if for a given iso the IEA start_year is 1960, or the earliest year with IEA data
# (1960 for most OECD countries), and the "years_until_100percent_BOND_split" is 20,
# then in 1940 agg. sector splits will be fully from BOND data.
years_until_100percent_BOND_split <- 20

ceds_extension_fuels <- c( "hard_coal", "brown_coal", "coal_coke", "natural_gas",
                          "heavy_oil", "diesel_oil", "light_oil" )
coal_fuels <- c( "hard_coal", "brown_coal", "coal_coke" )

all_countries <- unique(activity_all$iso)
all_countries <- all_countries[all_countries != 'global']

start_years <- c(1960, 1971) # IEA data starts in either 1960 or 1971
X_start_years <- paste0('X', start_years)
ext_sectors <- unique(ext_sector_map$ext_sector)
extension_end_year <- 1970 # last year of extension, for selecting columns over both iea_years
X_extension_years <- paste0('X', historical_pre_extension_year:extension_end_year)
OECD_years <- paste0("X", 1960:1970) # years for which OECD countries should have splits taken
                                     # directly from CEDS default data

# Define Function to Normalize shares
#   This occurs For all ext years (1750-1970) and all isos - but OECD country data from
#       1960-1970 is taken directly from CEDS default data.
#   For agg_shares, normalize so that a given fuel's agg_shares sum to 1.

normalize_shares <- function(df_in, share_type){

    if(share_type == "agg_splits"){
        shares_summed_col_remove <- "ext_sector"
        shares_summed_group_by <- c("iso", "fuel")

    } else if (share_type == "disagg_splits"){
        shares_summed_col_remove <- "sector"
        shares_summed_group_by <- c("iso", "ext_sector", "fuel")

    } else if (share_type == "final_splits"){
        shares_summed_col_remove <-  "sector"
        shares_summed_group_by <- c("iso", "fuel")

    } else {
        stop('share_type does not have options defined for use in this normalization function.')
    }

    calc_normalized_gather_group <- c(shares_summed_group_by, shares_summed_col_remove)
    calc_normalized_left_join_group <-  c(shares_summed_group_by, "years")
    calc_normalized_mutate_group <- c(calc_normalized_left_join_group, shares_summed_col_remove)
    fix_NaN_NA_cols_retain <- c(calc_normalized_gather_group, "years", "share",
                                "shares_summed_by_fuel", "normalized_share")
    df_final_cols_retain <- c(calc_normalized_gather_group, "years",
                              "normalized_share_fixed_final")
    df_final_spread_group <- calc_normalized_gather_group

#       Create a data frame with the sum of the shares by fuel.
#       NAs will be summed over (i.e. a fuel that is NA for all f the given share level
#       (for a given year) will have the sum of the shares equal to 0).
    shares_summed <- df_in %>%
        dplyr::select(-shares_summed_col_remove) %>%
        dplyr::group_by_at(shares_summed_group_by) %>%
        dplyr::summarize_all(sum, na.rm=TRUE) %>%
        tidyr::gather(key = years, value = shares_summed_by_fuel,
                      X_extension_years) %>%
        dplyr::ungroup()

#       Calculate normalized_shares for each fuel = share / sum of shares
    df_in_corrected <- df_in %>%
        dplyr::group_by_at(calc_normalized_gather_group) %>%
        tidyr::gather(key = years, value = share, X_extension_years) %>%
        dplyr::left_join(shares_summed, by = calc_normalized_left_join_group) %>%
        dplyr::ungroup() %>%
        dplyr::group_by_at(calc_normalized_mutate_group) %>%
        dplyr::mutate(normalized_share = (share/shares_summed_by_fuel ) )

#       Reassign OECD country values to unnormalized CEDS default shares since (they are actually already normalized,
#       since they are dervied from CEDS values).
#       Also fix NaNs and NAs (set them to 0)

    df_in_corrected_fix_NaN_NA <- df_in_corrected %>%
        dplyr::ungroup() %>%
        dplyr::select(fix_NaN_NA_cols_retain) %>%
        dplyr::mutate(normalized_share_fixed = if_else(! ( iso %in% isos_start_1971) & years %in% OECD_years,
                                                       share, normalized_share),
                      normalized_share_fixed_final = if_else(is.na( normalized_share_fixed ) |
                                                             is.nan(normalized_share_fixed ),
                                                             0, normalized_share_fixed ) )

    df_in_corrected <- df_in_corrected_fix_NaN_NA %>%
        dplyr::select(df_final_cols_retain) %>%
        dplyr::group_by_at(df_final_spread_group) %>%
        tidyr::spread(years, normalized_share_fixed_final) %>%
        dplyr::ungroup()

    return(df_in_corrected)
}

# Filter for fuels we are calculating shares for
# (or "ceds_extension_fuels" - the fossil fuels we are extending energy data for).
 activity <- activity_all %>%
     dplyr::filter( fuel != 'biomass')

# ---------------------------------------------------------------------------
# 3. Calculate CEDS default aggregate sector splits
#    Calculate the fraction of CEDS fuel used in each of the aggregate sectors, by country
#    Aggregate sectors: Industry, Power, RCO, Shipping, Transportation, Other_feedstocks,
#    Other_transformation, 1A4c_Agriculture-forestry-fishing
printLog('Calculating CEDS aggregate sector breakdowns')

# Calculate CEDS aggregate sector splits
ceds_aggregate_sectors <- activity %>%
    dplyr::left_join(ext_sector_map, by = "sector") %>%
    dplyr::group_by(iso, fuel, ext_sector) %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    dplyr::ungroup()

ceds_agg_percent_all <- calculate_shares(ceds_aggregate_sectors,
                                         id_columns = c('iso','fuel'),
                                         target_column = c('ext_sector') )

# Add column with the start year percent
#       Note: Germany (iso = deu) is an OECD country, yet will have a start
#             date of 1971 given that IEA data in the prior years (1960-1970)
#             is for West Germany, and does not include East Germany.

isos_start_1971 <- iea_start_year[ iea_start_year$start_year == 1971, 'iso' ]

ceds_agg_percent <- ceds_agg_percent_all %>%
    dplyr::mutate(percent = if_else(iso %in% isos_start_1971, X1971, X1960)) %>%
    dplyr::select(iso, fuel, ext_sector, one_of(X_start_years), percent)

# ---------------------------------------------------------------------------
# 4. If needed, extend Bond agg_splits backwards - if at any date the sum of a fuel's agg_splits
#    are 0, then extend the last year of data for the given fuel backwards (without overwriting
#    non-zero data).

#   Calculate sum of Bond agg_splits for each fuel
    bond_agg_split_totals <- bond_sector_percentages %>%
        dplyr::select(-ext_sector) %>%
        dplyr::group_by(iso, fuel) %>%
        dplyr::summarize_all(sum, na.rm=TRUE) %>%
        tidyr::gather(key = years, value = bond_agg_shares_summed_by_fuel, X_extension_years) %>%
        dplyr::ungroup()

#   Check if fuel agg_split sums are 0 in any extension year.
    any_bond_sum_to_zero <- bond_agg_split_totals %>%
        dplyr::filter(bond_agg_shares_summed_by_fuel == 0)

    if(nrow(any_bond_sum_to_zero) != 0 ){
        stop('Check bond_sector_percentages -- the sum of ext_sector splits by fuel',
             'is 0 for at least 1 year, for at least one fuel. The last year of data',
             'for the fuel(s) needs to be extended backwards so that no fuel',
             'has a sum of ext_sector splits equal to 0 for any year.')
    } else {
        printLog('bond_sector_percentages summed by fuel (across ext_sectors) are not',
               'equal to 0 for any year and do not need further processing.',
               'Continue generating final sector splits.')
    }

# TODO: Add code for extending Bond agg_splits backwards (if at any date the
#          sum of a fuel's agg_splits are 0, then extend the last year of data
#          for the given fuel backwards - without overwriting non-zero data).
#        (* Code has not yet been created for this means, as this is not an issue currently.)

# ---------------------------------------------------------------------------
# 5. Merge Bond Sector Splits and CEDS aggregate Sector Splits
#    Country-fuel breakdown by extension sectors
printLog('Merge Bond and CEDS sector extension sector breakdowns')

# TODO: In the future we need to include the default agg_sector assumptions
#      (ext_sector_percents_start_assumptions) within the combined agg_splits --
#      we will use the default assumptions when Bond data is missing.

# Combine Bond and CEDS aggregate splits. Slowly transition from Bond to CEDS
# sector splits. The loop combines ceds_agg_percent and bond_sector_percentages,
# to create combined_sector_percentages.
combined_sector_percentages_list <- list()
for (start_year in start_years) {

    # bond_merge_start: year when aggregate sectors are 100% Bond data. Method is
    # to transition from Bond to CEDS after this year for agg splits, ending with 100% CEDS
    # default in IEA start year (which is either 1960 or 1971, varies by iso)
    bond_merge_start <- start_year - years_until_100percent_BOND_split

    if (bond_merge_start %!in% bond_start:1959) {
        stop('bond_merge_start must be between ', bond_start, ' and 1959')
    }

    years <- bond_merge_start:start_year
    xyears <- paste0('X', years)
    pre_merge_years <- historical_pre_extension_year:(bond_merge_start - 1)
    countries <- iea_start_year[iea_start_year$start_year == start_year, 'iso']

    combined_percentages <- bond_sector_percentages %>%
        dplyr::select(iso, fuel, ext_sector, num_range('X', pre_merge_years)) %>%
        dplyr::inner_join(dplyr::select(ceds_agg_percent, iso, fuel, ext_sector, percent),
                          by = c('iso', 'fuel', 'ext_sector')) %>%
        dplyr::rename(!!paste0('X', start_year) := percent) %>%
        dplyr::filter(iso %in% countries)

    # Inner join to select the rows in the correct order, combining with the
    # data frame combined_percentages above
    bond_splits <- bond_sector_percentages %>%
        dplyr::inner_join(combined_percentages, by = c('iso', 'fuel', 'ext_sector')) %>%
        dplyr::select(-ends_with('.y')) %>%
        dplyr::rename_at(vars(ends_with('.x')), funs(sub('.x$', '', .))) %>%
        dplyr::select(iso, fuel, ext_sector, xyears)

    ceds_split <- ceds_agg_percent %>%
        dplyr::inner_join(combined_percentages, by = c('iso', 'fuel', 'ext_sector')) %>%
        dplyr::select(percent)

    ceds_fractions <- seq(0, 1, length.out = length(years))
    bond_fractions <- 1 - ceds_fractions

    for (n in seq_along(years)) {
        ceds_fraction <- ceds_fractions[n]
        bond_fraction <- bond_fractions[n]

        # Select bond sector percentages, for the current year.
        bond_split <- bond_splits[ , paste0('X', years[n])]

        # Combine bond and sector splits using the fraction calculated above
        combined_percentages[ , paste0('X', years[n])] <- bond_split * bond_fraction + ceds_split * ceds_fraction
    }

    # If start year is 1960, add in CEDS split for 1960 - 1971
    if( start_year == 1960){
      combined_percentages[paste0('X', (start_year) : (extension_end_year+1))] <-
          ceds_agg_percent_all[ match( paste(combined_percentages$iso, combined_percentages$fuel, combined_percentages$ext_sector),
                                       paste(ceds_agg_percent_all$iso, ceds_agg_percent_all$fuel, ceds_agg_percent_all$ext_sector) ),
                                paste0('X', (start_year):(extension_end_year+1)) ]
    }
    combined_sector_percentages_list[[paste0('X', start_year)]] <- combined_percentages
}

#CR: These 3 lines seem roundabout.
#    - Instead of making the list of 2 dataframes, why not just rbind them
#      together as they are processed?
#    - Using rbind.fill implies the two dfs have different columns, which
#      they don't
#    - The columns being selected are all of them
#    - Why are there NAs? They are only in the second df and it would be
#      clearer to address them where they arise.
combined_sector_percentages <- do.call( rbind, combined_sector_percentages_list)

# Combined_sector_percentages is not complete (there are iso-sector-fuel combinations that are not in that data frame)
# Create a new dataframe with all combinations and add combined_sector percentages. Values that are NA will be
# assigned a value of 0.

# Filter out other_transformation, then add it back in for oil, NG, hard coal, and brown coal
other_transformation_fuels <- c( "brown_coal", "hard_coal", "natural_gas", "oil" )

combined_sector_percentages_template <- rbind( expand.grid( iso = all_countries,
                                                 ext_sector = unique( ext_sector_map %>% filter( ext_sector != "Other_transformation" ) %>% select( ext_sector ) ) %>% unlist,
                                                 fuel = ceds_extension_fuels ),
                                               expand.grid( iso = all_countries,
                                                 ext_sector = "Other_transformation",
                                                 fuel = other_transformation_fuels )  )

combined_sector_percentages_all <- combined_sector_percentages_template %>%
    dplyr::mutate( iso = as.character( iso ),
                   ext_sector = as.character( ext_sector ),
                   fuel = as.character( fuel ) ) %>%
    dplyr::left_join( combined_sector_percentages, by = c( "iso", "ext_sector", "fuel" ) ) %>%
    dplyr::select( iso, ext_sector, fuel, X_extension_years )

if( any( is.na( combined_sector_percentages_all ) ) ){

    stop( paste0( "Combined aggregate sector splits (BOND and CEDS defaults combined) contain NA values. See ", script_name ) )

}

# Normalize agg_shares (so that a given fuel's agg_shares sum to 1 across all agg_shares
combined_agg_sector_percentages_corrected <- normalize_shares(combined_sector_percentages_all,
                                                              "agg_splits")

# ---------------------------------------------------------------------------
# 6. CEDS disaggregate Sector Splits
#    CEDS sectors are more detailed than Bond sectors, so now generate the split from
#    the aggregate sectors to CEDS sectors
#    Country-fuel-ext_sector -> CEDS working sector
printLog('Calculating CEDS detailed sector splits')

# Calculate CEDS_sector splits from CEDS data.
ceds_extsector_percentages_corrected <- activity %>%
    dplyr::left_join(ext_sector_map, by = 'sector') %>%
    calculate_shares(c('iso', 'fuel', 'ext_sector'), 'sector') %>%
    dplyr::select(iso, fuel, ext_sector, sector, X_emissions_years)

# ---------------------------------------------------------------------------
# 7. Combine CEDS disaggregate sector splits (Ext sector -> CEDS sector) and
#    extension assumption back to 1750
printLog("Combining CEDS breakdowns (aggregate sector to CEDS sector) and extending back to 1750")

# Define Extension sector breakdowns in 1850 (natural gas and oil) and in 1750 (coal)
# Add coal breakdown assumptions, and natural gas/oil assumptions
extended_breakdown_coal <- ceds_extsector_percentages_corrected %>%
    dplyr::filter(fuel %in% coal_fuels) %>%
    dplyr::left_join(ext_sector_breakdown_assumptions %>% dplyr::rename(X1750 = breakdown),
                     by = c("ext_sector", "sector"))

extended_breakdown_not_coal <- ceds_extsector_percentages_corrected %>%
    dplyr::filter(fuel %!in% coal_fuels) %>%
    dplyr::left_join(ext_sector_breakdown_assumptions %>% dplyr::rename(X1850 = breakdown),
                     by = c("ext_sector", "sector") )

# Combine and interpolate between the NAs
# Note: 1) The mutate_at() call creates columns, but it requires a function as its
#       second argument, even though we just want to give it a value (NA). To
#       address this we use the `+` function as an identity function that also
#       ensures its argument is numeric.
#       2) Countries with iea_start_years equal to 1971 need to have their disagg splits converted to
#       NA from 1960-1970 (currently all = 0) so that interpolation is between their 1971 value and the default assumptions back to 1750,
#       instead of between 0 and the default assumptions back to 1750
# TODO: If future IEA energy data provides data for non-OECD countries from 1960-1970 the values
#       will not need to be set to NA
nonOECD_NA_years <- paste0( "X", 1960 : extension_end_year )

extended_breakdown <- extended_breakdown_coal %>%
    rbind.fill( extended_breakdown_not_coal ) %>%
    mutate_at( setdiff( X_extended_years, names( . ) ), funs( +NA ) ) %>%
    dplyr::left_join( iea_start_year, by = "iso" ) %>%
    select(iso, fuel, ext_sector, sector, one_of( X_extended_years ), start_year ) %>%
    dplyr::mutate_at( nonOECD_NA_years, funs( if_else( start_year == 1971, NA_real_, . ) ) ) %>%
    dplyr::select( - start_year ) %>%
    interpolate_NAs2( )

# Make sure all combinations are included (other than Global)
#   Create template with all combinations
extended_breakdown_complete_template <- combined_sector_percentages_template %>%
    dplyr::mutate( ext_sector = as.character( ext_sector ) ) %>%
    dplyr::full_join( ext_sector_map, by = "ext_sector" )

extended_breakdown_complete <- extended_breakdown_complete_template %>%
    dplyr::mutate( iso = as.character( iso ),
                   fuel = as.character( fuel ) ) %>%
    dplyr::left_join( extended_breakdown, by = c( "iso", "fuel", "ext_sector", "sector"  ) ) %>%
    dplyr::select( iso, ext_sector, fuel, sector, X_extension_years )

# Normalize disagg shares (so that a given fuel's sector shares sum to 1 within each
# agg. ext_sector)
extended_breakdown_complete_corrected <- normalize_shares( extended_breakdown_complete,
                                                          "disagg_splits" )

# ---------------------------------------------------------------------------
# 8. Combine aggregate splits and disaggregate sector splits
#    final splits = aggregate splits * disaggregate sector splits

# Merge agg splits and disagg splits
final_percentages <- extended_breakdown_complete_corrected[c('iso','sector','fuel')]

combined_template <- extended_breakdown_complete_corrected[c('iso','ext_sector','sector','fuel')]
combined_template[X_extension_years] <- NA
combined_template[X_extension_years] <- combined_agg_sector_percentages_corrected[
  match(paste(combined_template$iso, combined_template$ext_sector, combined_template$fuel),
        paste(combined_agg_sector_percentages_corrected$iso,
              combined_agg_sector_percentages_corrected$ext_sector,
              combined_agg_sector_percentages_corrected$fuel)),
  X_extension_years]
combined_template <- replace(combined_template, is.na(combined_template), 0)

final_percentages[X_extension_years] <- extended_breakdown_complete_corrected[X_extension_years] * combined_template[X_extension_years]

# Normalize  shares - so that a given fuel's sector shares sum to 1 (or 0 if a fuel's individual shares
#   are 0 for all sectors in a given year)
final_percentages_corrected <- normalize_shares(final_percentages, "final_splits")

final_percentages_corrected <- final_percentages_corrected %>%
    dplyr::arrange(sector, iso) %>%
    dplyr::select(iso, sector, fuel, X_extension_years)

# Final check to make sure that all breakdowns add to 100% or 0% to 10 decimal places
 final_test <- final_percentages_corrected %>%
     dplyr::ungroup( ) %>%
     dplyr::group_by( iso, fuel ) %>%
     dplyr::select( -sector ) %>%
     dplyr::summarize_all( funs( sum ) )

 shares_not_0_or_1 <- final_test %>%
     tidyr::gather( key = years, value = shares, X_extension_years ) %>%
     dplyr::ungroup( ) %>%
     # dplyr::filter( !(! ( iso %in% isos_start_1971) & years %in% OECD_years) ) %>% #*** start here, without this a bunch get flagged so there's an issue
     dplyr::mutate(shares_rounded = round( shares, digits = 10 ) ) %>%
     dplyr::filter(shares_rounded != 1 & shares_rounded != 0 )

 if( nrow( shares_not_0_or_1 ) != 0 ){

     stop( 'Final percentage breakdown for all country-fuel combinations do not ',
           'equal 100% or 0%.' )

 } else {

     printLog( 'Finished generating final percentage breakdowns - all country-fuel combinations',
               'across all years equal 100% or 0%.' )

 }

# ---------------------------------------------------------------------------
# 9. Write to database

writeData( final_percentages_corrected, "MED_OUT", "A.final_sector_shares" )

logStop( )
