# ------------------------------------------------------------------------------
# Program Name: A6.1.extended_comb_sector_shares.R
# Author: Rachel Hoesly
# Program Purpose: Calculate combustion shares for extended historical data
#
# Output Files:  A.final_sector_shares.csv
# TO DO:
#       - better feedstock extension
# ---------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R","process_db_functions.R" )
log_msg <- "Extending Combustion data with bond and IEA"
script_name <- "A6.1.extended_comb_sector_shares.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 1. Load files

bond_sector_percentages <-
    readData( 'EXT_IN', 'CD.Bond_sector_percentages.csv', ".zip", 'CD.Bond_sector_percentages')
ext_sector_breakdown_assumptions <-
    readData( 'EXT_IN', 'ext_sector_breakdown_assumptions')
ext_sector_percents_start_assumptions <-
    readData( 'EXT_IN', 'ext_sector_percents_start_assumptions', meta = F )

activity_all <- readData( 'MED_OUT', paste0( 'A.comb_activity_with_other' ), meta = F)
other_transformation <- readData( 'MED_OUT', 'A.Other_transformation_fuel' )

iea_start_year <- readData( 'ENERGY_IN' , 'IEA_iso_start_data')
ext_sector_map <- readData( "MAPPINGS", "Bond_sector_ext_map", ".xlsx",
                            sheet_selection = 'CEDS_to_ext', meta = F,
                            domain_extension = "Bond/" )


# ---------------------------------------------------------------------------
# 2. Define Variables, select options

# bond_merge_start : year when aggregate sectors are 100% Bond data. Will slowly transition
# from Bond to CEDS after this year, 100% CEDS in start year (either 1960 or 1971 varies by iso)

bond_merge_start <- bond_start
if (bond_merge_start %!in% bond_start:1959) stop('bond_merge_start must be between 1850 and 1959')

ceds_extension_fuels <- c("hard_coal", "brown_coal", "coal_coke", "natural_gas",
                          "heavy_oil", "diesel_oil", "light_oil")
coal_fuels <- c("hard_coal", "brown_coal", "coal_coke")

all_countries <- unique(activity_all$iso)
all_countries <- all_countries[all_countries != 'global']

start_years <- c(1960, 1971)
X_start_years <- paste0('X', start_years)
ext_sectors <- unique(ext_sector_map$ext_sector)
extention_end_year <- 1970 #last year of extension, for selecting columns over both iea_years

# add other_transformation data to activity data and filter for fuels
activity <- bind_rows(activity_all, other_transformation ) %>%
    filter( fuel != 'biomass')


# ---------------------------------------------------------------------------
# 3. Calculate CEDS aggregate sector splits
#    Total fuel -> extenstion sectors
#    Industry, Power, RCO, Shipping, Transportation, Other_feedstocks, Other_transformation,
#    1A4c_Agriculture-forestry-fishing
printLog('Calculating CEDS sector breakdowns')

# Calculate CEDS aggregate sector splits
ceds_aggregate_sectors <- activity %>%
    left_join(ext_sector_map, by = "sector") %>%
    group_by(iso, fuel, ext_sector) %>%
    summarize_if(is.numeric, sum)

ceds_agg_percent_all <- calculate_shares(ceds_aggregate_sectors,
                                         id_columns = c('iso','fuel'),
                                         target_column = c('ext_sector') )

# add column with the start year percent
isos_start_1971 <- iea_start_year[ iea_start_year$start_year == 1971, 'iso' ]
ceds_agg_percent <- ceds_agg_percent_all %>%
    mutate(percent = if_else(iso %in% isos_start_1971, X1971, X1960)) %>%
    select(iso, fuel, ext_sector, one_of(X_start_years), percent)


# ---------------------------------------------------------------------------
# 4. Merge Bond Sector Splits and CEDS aggregate Sector Splits
#    Country-fuel breakdown by extension sectors
printLog('Merge Bond and CEDS sector extension sector breakdowns')

# Combine Bond and CEDS aggregate splits. Slowly transition from Bond to CEDS sector splits
# combine ceds_agg_percent and bond_sector_percentages, to create combined_sector_percentages
combined_sector_percentages_list <- list()
for (start_year in start_years) {
    years <- bond_merge_start:start_year
    pre_merge_years <- historical_pre_extension_year:(bond_merge_start - 1)
    countries <- iea_start_year[iea_start_year$start_year == start_year, 'iso']

    combined_percentages <- bond_sector_percentages %>%
        dplyr::select(iso, fuel, ext_sector, num_range('X', pre_merge_years)) %>%
        dplyr::inner_join(dplyr::select(ceds_agg_percent, iso, fuel, ext_sector, percent),
                          by = c('iso', 'fuel', 'ext_sector')) %>%
        dplyr::rename(!!paste0('X', start_year) := percent) %>%
        dplyr::filter(iso %in% countries)

    for (n in seq_along(years)){
        ceds_fraction <- (n-1)*(1/(length(years)-1))
        bond_fraction <- 1-ceds_fraction
        ##CR: this could be clarified by using a dplyr chain and/or comments

        # Select bond sector percentages, for the current year.
        # Use match and paste to select the rows in the correct order, to be combined with the row order of the data frame combined_percentages above
        bond_split <-
            bond_sector_percentages[ match( paste(combined_percentages$iso,    combined_percentages$fuel,    combined_percentages$ext_sector),
                                            paste(bond_sector_percentages$iso, bond_sector_percentages$fuel, bond_sector_percentages$ext_sector) ),
                                     c(paste0('X',years[n]) )]
        # Same selection for ceds sector percentages
        ceds_split <-
            ceds_agg_percent[ match( paste(combined_percentages$iso, combined_percentages$fuel, combined_percentages$ext_sector),
                                     paste(ceds_agg_percent$iso,     ceds_agg_percent$fuel,     ceds_agg_percent$ext_sector) ),
                              c('percent') ]
        # combine bond and sector splits using the fraction calculated above
        combined_percentages[,paste0('X',years[n])] <- bond_split*bond_fraction + ceds_split*ceds_fraction
    }

    # If start year is 1960, add in CEDS split for 1960 - 1971
    if( start_year == 1960){
      combined_percentages[paste0('X', (start_year+1) : (extention_end_year+1))] <-
          ceds_agg_percent_all[ match( paste(combined_percentages$iso, combined_percentages$fuel, combined_percentages$ext_sector),
                                       paste(ceds_agg_percent_all$iso, ceds_agg_percent_all$fuel, ceds_agg_percent_all$ext_sector) ),
                                paste0('X', (start_year+1):(extention_end_year+1)) ]
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

# combined_sector_percentages is not complete (there are iso-sector-fuel combinations that are not in that data frame)
# Create a new dataframe with all combinations and add combined_sector percentages
    combined_sector_percentages_all <- rbind( expand.grid(iso = all_countries,
                                   ext_sector = unique(ext_sector_map %>% filter(ext_sector != "Other_transformation") %>% select(ext_sector)) %>% unlist,
                                   fuel = ceds_extension_fuels),
                       expand.grid(iso = all_countries,
                                   ext_sector = "Other_transformation",
                                   fuel = 'oil')  )
    combined_sector_percentages_all[ paste0('X', historical_pre_extension_year:extention_end_year) ] <- combined_sector_percentages[  match( paste0(combined_sector_percentages_all$iso, combined_sector_percentages_all$ext_sector, combined_sector_percentages_all$fuel ),
                                                                                paste0(combined_sector_percentages$iso, combined_sector_percentages$ext_sector, combined_sector_percentages$fuel )),
                                                                                paste0('X', historical_pre_extension_year:extention_end_year) ]
    combined_sector_percentages_all[is.na(combined_sector_percentages_all)] <- 0

# Renormalize and Correct combined percentages
    combined_agg_sector_percentages_corrected <-
        calculate_correct_shares ( a.input_data = combined_sector_percentages_all,
                                   a.id_columns = c("iso","fuel"),
                                   a.target_column = c('ext_sector'),
                                   a.match_columns = c('fuel', 'ext_sector'),
                                   replace_with_zeros = T,
                                   a.corrections = ext_sector_percents_start_assumptions )

# ---------------------------------------------------------------------------
# 5. CEDS disaggregate Sector Splits
#    Country-fuel-ext_sector -> CEDS working sector
printLog('Calculating CEDS detailed sector splits')

# Calculate CEDS_sector splits from CEDS data.
# id_columns = iso, fuel, ext_sector
# target_column = sector

activity$ext_sector <- ext_sector_map[match(activity$sector, ext_sector_map$sector),'ext_sector']

ceds_extsector_percentages_corrected <- calculate_shares(input_data = activity,
                 id_columns = c('iso', 'fuel', 'ext_sector'),
                 target_column = c('sector') ) %>%
    select(iso, fuel, ext_sector, sector, starts_with('X'))


# ---------------------------------------------------------------------------
# 6. Combine CEDS dissagregate sector splits (Ext sector -> CEDS sector) and extension assumption back to 1750
    printLog("Combining CEDS breakdowns (Ext sector to CEDS sector) and extending back to 1750")

    # Define Extension sector breakdowns in 1850 (nat gas and fuel) and in 1750 (coal)
    # Add coal breakdown assumptions, and nat gas/oil assumptions
    extended_breakdown_coal <- ceds_extsector_percentages_corrected %>%
        filter( fuel %in% coal_fuels) %>%
        left_join(ext_sector_breakdown_assumptions %>% dplyr::rename(X1750 = breakdown),
                  by = c("ext_sector", "sector") )
    extended_breakdown_not_coal <- ceds_extsector_percentages_corrected %>%
        filter( fuel %!in% coal_fuels) %>%
        left_join(ext_sector_breakdown_assumptions %>% dplyr::rename(X1850 = breakdown),
                  by = c("ext_sector", "sector") )

    # combine and interpolate between the NAs
    extended_breakdown <- extended_breakdown_coal %>%
        rbind.fill(extended_breakdown_not_coal) %>%
        mutate_at(setdiff(X_extended_years, names(.)), funs(as.numeric(NA))) %>%
        select(iso, fuel, ext_sector, sector, one_of(X_extended_years)) %>%
        interpolate_NAs()

    # Make sure all combinations are included
    # Create template with all combinations
    ##CR: this template is duplicate code from above (line 147)
    template <- rbind( expand.grid(iso = all_countries,
                                   ext_sector = unique(ext_sector_map %>% filter(ext_sector != "Other_transformation") %>% select(ext_sector)) %>% unlist,
                                   fuel = ceds_extension_fuels),
                       expand.grid(iso = all_countries,
                                   ext_sector = "Other_transformation",
                                   fuel = 'oil')  ) %>%
        dplyr::mutate( ext_sector = as.character(ext_sector)) %>%
        dplyr::full_join(ext_sector_map, by = "ext_sector")

    extended_breakdown_complete <- template
    extended_breakdown_complete[ paste0('X', historical_pre_extension_year:extention_end_year) ] <- extended_breakdown[  match( paste0(template$iso, template$fuel, template$ext_sector, template$sector ),
                                                                                    paste0(extended_breakdown$iso, extended_breakdown$fuel, extended_breakdown$ext_sector, extended_breakdown$sector )),
                                                                             paste0('X', historical_pre_extension_year:extention_end_year) ]
    # Normalize and correct
    extended_breakdown_complete_corrected <- calculate_correct_shares(a.input_data = extended_breakdown_complete,
                                                                     a.id_columns = c('iso', 'fuel', 'ext_sector'),
                                                                     a.target_column = c('sector'),
                                                                     a.corrections = ext_sector_breakdown_assumptions ,
                                                                     replace_with_zeros = T )

# ---------------------------------------------------------------------------
# 7. Combine aggregate splits and disaggregate sector splits
#    final splits = aggregate splits * disaggregate sector splits

# merge agg splits and disagg splits

    final_percentages <- extended_breakdown_complete_corrected[c('iso','sector','fuel')]

    combined_template <- extended_breakdown_complete_corrected[c('iso','ext_sector','sector','fuel')]
    combined_template[paste0('X',historical_pre_extension_year:extention_end_year)] <- NA
    combined_template[paste0('X',historical_pre_extension_year:extention_end_year)] <- combined_agg_sector_percentages_corrected[
      match(paste(combined_template$iso, combined_template$ext_sector, combined_template$fuel),
            paste(combined_agg_sector_percentages_corrected$iso,
                  combined_agg_sector_percentages_corrected$ext_sector,
                  combined_agg_sector_percentages_corrected$fuel)),
      paste0('X',historical_pre_extension_year:extention_end_year)]
    combined_template <- replace( combined_template, is.na(combined_template), 0)

    final_percentages[paste0('X',historical_pre_extension_year:extention_end_year)] <- extended_breakdown_complete_corrected[paste0('X',historical_pre_extension_year:extention_end_year)]*combined_template[paste0('X',historical_pre_extension_year:extention_end_year)]

    final_percentages_corrected <- calculate_shares(input_data = final_percentages,
                                                    id_columns = c('iso', 'fuel'),
                                                    target_column = c('sector'))

# final check to make sure that all breakdown add to 100%
    final_test <- final_percentages_corrected %>%
        ungroup() %>%
        group_by(iso, fuel) %>%
        select(-sector) %>%
        summarize_all(funs( sum)) %>%
        ungroup() %>%
        select(- iso, -fuel)

    if ( !sum(final_test) == nrow(final_test)*ncol(final_test)) stop('Final percentage breakdown for all country-fuel combinations do not equal 100%')

# ---------------------------------------------------------------------------
# 8. Write to database

    writeData( final_percentages_corrected, "MED_OUT", "A.final_sector_shares" )

    logStop()
