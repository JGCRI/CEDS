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
headers <- c( "data_functions.R","process_db_functions.R") # Additional function files may be required.
log_msg <- "Extending Coal data with bond and IEA" # First message to be printed to the log
script_name <- "A6.1.extended_comb_sector_shares_coal.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 1. Load files

bond_sector_percentages <- readData( 'EXT_IN', 'CD.Bond_sector_percentages')
bond_percent_1850 <- readData( 'EXT_IN', 'Bond_sector_extension_percents_1850')
ext_sector_percents_start <- readData( 'EXT_IN', 'ext_sector_percents_start', ".xlsx", sheet_selection = 'coal',meta = F )

activity_all <- readData( 'MED_OUT',paste0('A.comb_activity') , meta = F)
other_transformation <- readData( 'MED_OUT','A.Other_transformation_fuel' )

iea_start_year <- readData( 'ENERGY_IN' , 'IEA_iso_start_data')
sector_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_sector_ext_map", ".xlsx", sheet_selection = 'Bond_to_ext',meta = F )
ext_sector_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_sector_ext_map", ".xlsx", sheet_selection = 'CEDS_to_ext',meta = F )

# ---------------------------------------------------------------------------
# 3. Define Variables, select options

# bond merge year : year when aggregate sectors are 100% bond data. Will slowly transition
# from bond to ceds after this year , 100% ceds in start year (either 1960 or 1971 varies by iso)

bond_merge_start <- 1850
if ( bond_merge_start %!in% 1850:1959) stop('bond_merge_start must be between 1850 and 1959')

ceds_extension_fuels <- c("hard_coal" ,  "brown_coal" , "coal_coke" ,  "natural_gas", "heavy_oil",   "diesel_oil" , "light_oil")

all_countries <- unique(activity_all$iso)
start_years <- c(1960,1971)
ext_sectors <- unique(ext_sector_map$ext_sector)

# add other_transformation data to acticity data and filter for Coal fuels
activity <- bind_rows(activity_all, other_transformation ) %>%
    filter( fuel %in% ceds_extension_fuels)

#----------------------------------------------------------------------
# 2. Calculate IEA feedstock split

# Calculate feedstock shares from CEDS activity data
# map to extension sectors
# extend the shares constantly back to 1750
    feedstock_shares <- calculate_shares(activity, id_columns = c('iso','fuel'),
                                         target_column = c('sector') ) %>%
        filter(sector == "1A1bc_Other-feedstocks") %>%
        left_join(ext_sector_map, by = 'sector')
    feedstock_shares[ paste0('X', 1750:1959)] <- NA
    feedstock_shares[paste0('X', 1750:1971)] <- t(na.locf(t(feedstock_shares[paste0('X', 1750:1971)]), fromLast = T))

    feedstock_shares_historical <- feedstock_shares[c('iso','sector','fuel', paste0('X', 1750:1971) )]

# ---------------------------------------------------------------------------
# 3. Calculate CEDS aggregate sector splits
printLog('Calculating CEDS sector breakdowns')

# Calculate CEDS aggregate sector splits
    ceds_aggregate_sectors <- activity_all %>%
        left_join(ext_sector_map, by = "sector") %>%
        group_by(iso, fuel, ext_sector) %>%
        summarize_if(is.numeric, sum)

    ceds_agg_percent_all <- calculate_shares(ceds_aggregate_sectors, id_columns = c('iso','fuel'),
                                         target_column = c('ext_sector') )

# add column with the start year percent
    ceds_agg_percent <- ceds_agg_percent_all %>%
        mutate(percent = ifelse(iso %in% iea_start_year[which( iea_start_year$start_year == 1971),'iso' ], X1971, X1960)) %>%
        select(iso, fuel, ext_sector, X1960, X1971, percent)

# ---------------------------------------------------------------------------
# 4. Merge Bond Sector Splits and CEDS aggregate Sector Splits
printLog('Merge Bond and CEDS coal sector breakdowns')

# Combine Bond and CEDS aggregate splits. Slowly transition from Bond to CEDS sector splits
#ceds_agg_percent and bond_sector_percentages
combined_sector_percentages_list <- list()
for ( i in seq_along(start_years)) {
  year0 <- bond_merge_start
  years <- year0:start_years[i]
  countries <- iea_start_year[which( iea_start_year$start_year == start_years[i]),'iso' ]

  combined_percentages <- merge(bond_sector_percentages[,c('iso','ext_sector','fuel',paste0('X',1750: (year0 - 1) ))],
                                ceds_agg_percent[,c('iso','ext_sector','fuel','percent')] )
  names(combined_percentages)[which(names(combined_percentages) == 'percent' )] <- paste0('X',start_years[i])

  combined_percentages <- combined_percentages[which( combined_percentages$iso %in% countries),]

  # percent ( year n ) =
      for ( n in seq_along( years)){
        ceds_fraction <- (n-1)*(1/(length(years)-1))
        bond_fraction <- 1-ceds_fraction

        bond_split <- bond_sector_percentages[ match( paste(combined_percentages$iso, combined_percentages$fuel, combined_percentages$ext_sector)  ,
                                                      paste(bond_sector_percentages$iso, bond_sector_percentages$fuel, bond_sector_percentages$ext_sector) ),
                                               c(paste0('X',years[n]) )]
        ceds_split <- ceds_agg_percent[ match( paste(combined_percentages$iso, combined_percentages$fuel, combined_percentages$ext_sector)  ,
                                               paste( ceds_agg_percent$iso,  ceds_agg_percent$fuel, ceds_agg_percent$ext_sector) )
                                        , c('percent') ]

        combined_percentages[,paste0('X',years[n])] <- bond_split*bond_fraction + ceds_split*ceds_fraction

      }

      # If start year is 1960, add in CEDS split for 1960 - 1971
      if( start_years[i]==1960){
        combined_percentages[paste0('X', 1961:1970)] <-      ceds_split <- ceds_agg_percent_all[ match( paste(combined_percentages$iso, combined_percentages$fuel, combined_percentages$ext_sector)  ,
                                                                                                    paste( ceds_agg_percent_all$iso,  ceds_agg_percent_all$fuel, ceds_agg_percent_all$ext_sector) )
                                                                                             , paste0('X', 1961:1970) ]
      }
      combined_sector_percentages_list[[i]] <- combined_percentages

    }
    combined_sector_percentages <- do.call(rbind.fill,combined_sector_percentages_list)
    combined_sector_percentages <- combined_sector_percentages[, c('iso','ext_sector','fuel',paste0('X',1750:1970))]
    combined_sector_percentages[is.na(combined_sector_percentages)] <- 0


# Renormalize and Correct combined percentages
  combined_sector_percentages_corrected <- calculate_correct_shares ( a.input_data = combined_sector_percentages,
                                   a.id_columns = c("iso","fuel"),
                                   a.target_column = c('ext_sector'),
                                   replace_with_zeros = T,
                                   a.corrections = data.frame( ext_sector = c('1A4c_Agriculture-forestry-fishing','Industry','Power','RCO','Shipping','Transportation'),
                                                               breakdown = c(0,0.5,0,0.5,0,0))  )

# ---------------------------------------------------------------------------
# 5. CEDS disaggregate Sector Splits
printLog('Calculating CEDS detailed sector splits')

# Calculate CEDS_sector splits from CEDS data.
# id_columns = iso, fuel, ext_sector
# target_column = sector

    activity_all$ext_sector <- ext_sector_map[match(activity_all$sector, ext_sector_map$sector),'ext_sector']
    activity_all <- activity_all[!duplicated(activity_all[c('iso','sector','fuel')]),]

    CEDS_sector_ext_sector_shares <- calculate_shares(input_data = activity_all,
                     id_columns = c('iso', 'fuel', 'ext_sector'),
                     target_column = c('sector') )%>%
        select(iso, fuel, ext_sector, sector, starts_with('X')) %>%
        unique

    printLog("correcting ceds_coal_extsector_percentages ")
    ceds_coal_extsector_percentages_corrected <- calculate_correct_shares(a.input_data <- CEDS_sector_ext_sector_shares,
                                a.id_columns <- c('iso', 'fuel', 'ext_sector'),
                                a.target_column <- c('sector'),
                                a.corrections <- bond_percent_1850 %>%
                                    dplyr::rename(breakdown = X1850),
                                replace_with_zeros = T )

# ---------------------------------------------------------------------------
# 6. Combine Bond breakdown and CEDS breakdown
    printLog("Combing Bond and CEDS breakdowns")

# CEDS sector splits, aggregate sectors to CEDS sectors in 1960. Defined Splits in 1850, linearly interpolate
    ceds_breakdown <- ceds_coal_extsector_percentages_corrected %>%
        left_join(iea_start_year, by = 'iso')
    ceds_breakdown[which( ceds_breakdown$start_year == '1960'), 'X1960'] <- NA
    ceds_breakdown[which( ceds_breakdown$start_year == '1971'), 'X1960'] <- NA

    ceds_breakdown <- merge( ceds_breakdown, bond_percent_1850, all.x = T, all.y=F)
    ceds_breakdown <- ceds_breakdown[ , c('iso','sector','ext_sector','fuel','X1850','X1960','X1971' )]


    ceds_breakdown[ paste0('X',1850:1971)[ paste0('X',1850:1971)  %!in% names(ceds_breakdown)]  ] <- NA
    ceds_breakdown <- ceds_breakdown[ , c('iso','sector','ext_sector','fuel',paste0('X',1850:1971))]
    ceds_breakdown[ , paste0('X',1850:1971)] <- interpolate_NAs(ceds_breakdown[ ,paste0('X',1850:1971)])
    ceds_breakdown[paste0('X',1750:1849)] <- ceds_breakdown[paste0('X',1850)]
    ceds_breakdown <- ceds_breakdown[ , c('iso','sector','ext_sector','fuel',paste0('X',1750:1971))]

    ceds_breakdown <- replace( ceds_breakdown, is.na(ceds_breakdown) , 0 )

# percentages as a total of iso-fuel
# disaggregate percent, ceds_breakdown
# aggregate category to dissagregate <- combined sector percentages
    final_percentages<-ceds_breakdown
    combined_template <- ceds_breakdown[c('iso','ext_sector','sector','fuel')]
    combined_template[paste0('X',1750:1970)] <- NA
    combined_template[paste0('X',1750:1970)] <-combined_sector_percentages[
      match(paste(combined_template$iso, combined_template$ext_sector, combined_template$fuel),
            paste(combined_sector_percentages$iso, combined_sector_percentages$ext_sector, combined_sector_percentages$fuel)),
      paste0('X',1750:1970)]
    combined_template <- replace( combined_template, is.na(combined_template), 0)

    final_percentages[paste0('X',1750:1970)] <- ceds_breakdown[paste0('X',1750:1970)]* combined_template[paste0('X',1750:1970)]

# Renormalize combined percentages
    final_percentages_corrected <- calculate_shares(input_data = final_percentages,
                                                    id_columns = c('iso', 'fuel', 'ext_sector'),
                                                    target_column = c('sector') )

#---------------------------------------------------------------------------------------------------------
# 7. Add feedstock ratio
    sector_breakdown_final <- final_percentages_corrected %>%
      select( -ext_sector ) %>%
      rbind(feedstock_shares_historical ) %>%
      calculate_shares(id_columns = c('iso', 'fuel'),
                       target_column = c('sector') ) %>%
      arrange(iso, sector, fuel)

# ---------------------------------------------------------------------------
# 8. Write to database

    writeData( sector_breakdown_final, "MED_OUT", "A.final_sector_shares" )

    logStop()
