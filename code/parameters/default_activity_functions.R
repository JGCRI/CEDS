
#-------------------------------------------------------------------------------------------------------------
# merge_extend_UN_CEDS_data
#
# Brief:        Merges CEDS with UN data. Extends that data back on trend
#               with CDIAC CO2 emissions

# Details:      UN data from 1950 - 1959/1970 combined to with CEDS IEA 1960/1971 data.
#               Over a 5 year period, data is slowly merged from CEDS data to UN data. The combined
#               data is then extend backward, using cdiac CO2 data, to a specified year
#               (the year is passed as a parameter)
#
# Dependencies: extend_data_on_trend, replace_Value_col_match
#
# Author(s): Presley Muwan, Rachel Hoesly
#
# Parameters:
#
#
# Return: dataframe of merged/extended energy data
#
# Input Files:
# Output Files: none
merge_extend_UN_CEDS_data <- function(     a.CEDS_data,
                                    a.CDIAC_data,
                                    a.UN_data,
                                    a.ceds_extension_fuels,
                                    a.extension_start_year,
                                    a.iea_start_years,
                                    a.iea_end_year,
                                    a.aggregate_fuel,
                                    a.CDIAC_fuel){

    # a.CEDS_data = A.comb_activity_with_other
    # a.CDIAC_data = cdiac_fuel
    # a.UN_data = UNSD_Energy_Final_Consumption
    # a.ceds_extension_fuels = ceds_extension_fuels
    # a.extension_start_year = 1750
    # a.iea_start_years = iea_start_year
    # a.iea_end_year = end_year
    # a.aggregate_fuel = aggregate_fuel_name
    # a.CDIAC_fuel = cdiac_fuel_name

    #----------------
    # If there is non zero UN data     -->

    if( nrow(a.UN_data)){
    #------------------------
    # Define Useful Variables
    X_UN_years <- a.UN_data %>%
        select(starts_with("X")) %>%
        names()

    #------------------------
    # Aggregate UN and CEDS data to aggregate fuels (coal, oil, or gas)

    UN_aggregate_fuel <-  a.UN_data %>%
        filter( fuel %in% ceds_extension_fuels ) %>%
        mutate(fuel =  a.aggregate_fuel )  %>%
        group_by(iso, fuel) %>%
        summarise_all(funs(sum))

    CEDS_aggregate_fuel <- a.CEDS_data %>%
        filter( fuel %in% ceds_extension_fuels ) %>%
        select(iso, fuel, starts_with("X")) %>%
        mutate(fuel = a.aggregate_fuel) %>%
        group_by(iso, fuel) %>%
        summarise_all(funs(sum))

    #------------------------
    # Determine CEDS Countries with no UN data
    UN_countries <- UN_aggregate_fuel %>%
        filter( fuel == a.aggregate_fuel) %>%
        pull(iso)
    ceds_only_countries <- CEDS_aggregate_fuel %>%
        filter(iso %!in% UN_countries) %>%
        pull(iso)

    printLog(length(ceds_only_countries), " CEDS IEA countries have no UNSD data  ")

    # Retrieve data from countries common to iea and UNSD
    CEDS_UN_common_data <- CEDS_aggregate_fuel %>%
        filter(iso %!in% ceds_only_countries)
    CEDS_only_data <- CEDS_aggregate_fuel %>%
        filter(iso %in% ceds_only_countries)

    #------------------------
    # Combine CEDS/IEA data [1960/71- Present] with UN data [1950 - 60/71]
    # Extend CEDS/IEA data with UN data
    # Merge data over 5 years (according to IEA start date)
    # Define a function to merge the CEDS and UN data based on the IEA start date
    # Apply over start years

    printLog("Merging UNSD data to CEDS IEA data (UN_CEDS DATA)")

    merge_common_UN_CEDS <- function(iea_start){
        # Define the number of years to merge over
        merge_length <- 5

        # Define years from each data set
        un_merge_years <- paste0('X', iea_start:(iea_start+merge_length),".y")
        ceds_merge_years <- paste0('X', iea_start:(iea_start+merge_length),".x")
        UN_CEDS_Years <- paste0('X',  iea_start:(iea_start+merge_length) )
        CEDS_Years <- paste0('X',  (iea_start+merge_length+1):end_year )
        UN_Years <- paste0('X', 1950:(iea_start -1))

        # Define countries for the iea start year
        countries <- iea_start_year %>%
            filter(iso %!in% ceds_only_countries) %>%
            filter(start_year == iea_start) %>%
            pull(iso)

        # combine CEDS and UN data to merge into one data frame
        CEDS_UN_fuel_Data_Merged <- CEDS_UN_common_data %>%
            filter(iso %in% countries) %>%
            select( iso, fuel, one_of( UN_CEDS_Years)) %>%
            left_join( UN_aggregate_fuel %>%
                           filter(iso %in% countries) %>%
                           select( iso, fuel, one_of( UN_CEDS_Years)),
                       by = c( "iso","fuel" ) )

        # Merge CEDS and UN Years
        for(year_index in seq(un_merge_years)){

            ceds_fraction <- (year_index-1)*(1/(length(un_merge_years)-1))
            un_fraction <- (1 - ceds_fraction )

            CEDS_UN_fuel_Data_Merged[ ceds_merge_years[year_index ] ] <- (CEDS_UN_fuel_Data_Merged[ceds_merge_years[year_index]] * un_fraction)
            CEDS_UN_fuel_Data_Merged[ un_merge_years[year_index ] ] <- (CEDS_UN_fuel_Data_Merged[un_merge_years[year_index]] * (year_index/length(un_merge_years)))

            CEDS_UN_fuel_Data_Merged[ UN_CEDS_Years[ year_index ] ] <- (CEDS_UN_fuel_Data_Merged[ ceds_merge_years[ year_index ] ] + CEDS_UN_fuel_Data_Merged[ un_merge_years [ year_index ] ])

        }#for Ends

        # Add UN data, merged data, and CEDS data together
        CEDS_UN_fuel_Data_Merged_final <- CEDS_UN_fuel_Data_Merged %>%
            select(iso, fuel, one_of( UN_CEDS_Years) ) %>%
            left_join( CEDS_UN_common_data %>%
                           filter(iso %in% countries) %>%
                           select( iso, fuel, one_of( CEDS_Years) ), by = c("iso", "fuel") ) %>%
            left_join( UN_aggregate_fuel %>%
                           filter(iso %in% countries) %>%
                           select( iso, fuel, one_of( UN_Years) ), by = c("iso", "fuel") ) %>%
            select( iso, fuel, one_of( UN_Years), one_of( UN_CEDS_Years), one_of( CEDS_Years)) %>%
            arrange(iso, fuel)

        return(CEDS_UN_fuel_Data_Merged_final)

    }# Ends function definition (merge common UN-IEA data trends)
    CEDS_UN_fuel_Data_Merged_list <- lapply(unique(a.iea_start_years$start_year),merge_common_UN_CEDS)

    CEDS_UN_merged_data <- do.call( rbind, CEDS_UN_fuel_Data_Merged_list ) %>%
        arrange(iso, fuel)

    #-------------------------
    # Extend CEDS/UN merged data backward with CDIAC

    printLog("Extending UN_CEDS [", paste(ceds_extension_fuels, collapse =","), "] DATA by CDIAC from 1950 back to ", a.extension_start_year)

    CEDS_UN_data_extended_in <- CEDS_UN_merged_data %>%
        mutate(fuel = a.CDIAC_fuel) %>%
        mutate(sector = 'all') %>%
        select( iso, sector, fuel, everything())
    extention_data <- a.CDIAC_data %>%
        mutate(sector = 'all') %>%
        filter(fuel == a.CDIAC_fuel) %>%
        select( iso, sector, fuel, everything())

    CEDS_UN_data_extended_in[ paste0( 'X', a.extension_start_year:1949 ) ] <- NA
    CEDS_UN_data_extended <- extend_data_on_trend(driver_trend = extention_data,
                                                  input_data = CEDS_UN_data_extended_in,
                                                  start = a.extension_start_year,
                                                  end = 1949) %>%
        select(-sector) %>%
        mutate(fuel = a.aggregate_fuel) %>%
        select('iso','fuel', X_extended_years)

    }else{

      UN_countries <- NULL

      CEDS_aggregate_fuel <- a.CEDS_data %>%
          filter( fuel %in% ceds_extension_fuels ) %>%
          select(iso, fuel, starts_with("X")) %>%
          mutate(fuel = a.aggregate_fuel) %>%
          group_by(iso, fuel) %>%
          summarise_all(funs(sum))

      ceds_only_countries <- CEDS_aggregate_fuel %>%
        filter(iso %!in% UN_countries) %>%
        pull(iso)

      CEDS_only_data <- CEDS_aggregate_fuel %>%
          filter(iso %in% ceds_only_countries)

      CEDS_UN_data_extended <- NULL } # End If statement, if UN data extist

    #--------------------------------------------------------------------------------------------------------
    # Extend countries with no UN data with CDIAC data
    printLog("Extending data for Countries with no UN data, directly, with CDIAC from 1960/1971")

    ceds_only_data_extension_in <- CEDS_only_data %>%
        mutate(fuel = a.aggregate_fuel) %>%
        mutate(sector = 'all') %>%
        group_by(iso, sector,fuel) %>%
        summarise_if(is.numeric, sum)
    ceds_only_data_extension_in[ paste0('X', a.extension_start_year: 1959 ) ] <- NA

    # extend ceds IEA data using cdiac data
    ceds_only_ctry_fuel_data_extended_list <- lapply(unique(a.iea_start_years$start_year),
                                                     function(iea_start){
                                                         countries <- iea_start_year %>%
                                                             filter(start_year == iea_start) %>%
                                                             filter(iso %in% ceds_only_countries) %>%
                                                             pull(iso)



                                                         if(length(countries) > 1){
                                                             drivers <- a.CDIAC_data[which(a.CDIAC_data$iso %in% countries ),] %>%
                                                                 mutate(sector = 'all') %>%
                                                                 filter(fuel == a.CDIAC_fuel) %>%
                                                                 mutate(fuel = a.aggregate_fuel)

                                                             ceds_data <- ceds_only_data_extension_in %>%
                                                                 filter(iso %in% countries)
                                                             ceds_extended <- extend_data_on_trend(driver_trend = drivers,
                                                                                                   input_data = ceds_data,
                                                                                                   start = a.extension_start_year,
                                                                                                   end = iea_start)
                                                         }else{ceds_extended <- NULL}

                                                         return(ceds_extended)

                                                     })#ceds_only_ctry_fuel_data_extended Ends

    ceds_only_ctry_fuel_data_extended <- do.call( rbind, ceds_only_ctry_fuel_data_extended_list )

    # some small countries don't have cdiac or have zero values through extension.
    ceds_only_ctry_fuel_data_extended[ is.na( ceds_only_ctry_fuel_data_extended ) ] <- 0

    ceds_only_ctry_fuel_data_extended <- ceds_only_ctry_fuel_data_extended[c("iso", "fuel",  X_extended_years) ]


    # Combine Extended Data and Return

    # output <- rbind.fill(ceds_only_ctry_fuel_data_extended,
    #                      CEDS_UN_data_extended) %>%
    #     arrange(iso, fuel)

    output <- list(CEDS_UN_data_extended,
                   ceds_only_ctry_fuel_data_extended)
    names(output) <- c('un_ceds', 'ceds_only')

    return( output )

}#merge_UN_CEDS_data() Ends

#-------------------------------------------------------------------------------------------------------------
# Fuel_break_down
#
# Brief:        This function splits aggregated fuel data into disaggregated fuels.
# Details:      The fucntion computes the fuel breakdown ratio for each fuel types
#               The breakdown ratio for each fuel types is multiplied by the aggregate
#               Value the individual fuel types.
#
# Dependencies: process_and_combine_un_ced_data(): Uses the output of this function
#
# Author(s): Presley Muwan
#
# Parameters: CED_UN_fuel_Data_extended: CEDS data to be processed
#             unsd_data: UNSD data
#             ceds_extension_fuels: fuels to be processed
#             extension_start_year: extension start year
#
#
# Return:    CED_UN_disaggregated_fuel
#
# Input Files:
# Output Files: none
fuel_breakdown <- function(  a.UN_data = UNSD_Energy_Final_Consumption,
                             a.CEDS_UN_aggregate = ceds_un_extended_data$un_ceds ,
                             a.CEDS_only_aggregate = ceds_un_extended_data$ceds_only,
                             a.CEDS_comb_with_other = A.comb_activity_with_other,
                             a.ceds_extension_fuels = ceds_extension_fuels,
                             a.extension_start_year = 1750,
                             a.aggregate_fuel = aggregate_fuel_name,
                             a.default_fuel_share = default_fuel_share,
                             a.UN_start = 1950,
                             a.iea_start_years = iea_start_year){

    # a.UN_data = UNSD_Energy_Final_Consumption
    # a.CEDS_UN_aggregate = ceds_un_extended_data$un_ceds
    # a.CEDS_only_aggregate = ceds_un_extended_data$ceds_only
    # a.CEDS_comb_with_other = A.comb_activity_with_other
    # a.ceds_extension_fuels = ceds_extension_fuels
    # a.extension_start_year = 1750
    # a.aggregate_fuel = aggregate_fuel_name
    # a.default_fuel_share = default_fuel_share
    # a.UN_start = 1950
    # a.iea_start_years = iea_start_year

    #-------------------
    # Disaggregate to CEDS fuels for countries with UN data
  # If UN data exists, then process first

   if( nrow(a.UN_data) > 0 ){

    # Calculate fuel breakdowns
    un_fuel_shares <- a.UN_data %>%
        filter( fuel %in% a.ceds_extension_fuels ) %>%
        mutate(agg_fuel = a.aggregate_fuel) %>%
        calculate_shares( id_columns = c('iso'),
                          target_column = 'fuel')
    un_fuel_shares[paste0('X',a.extension_start_year:(a.UN_start - 1))] <- un_fuel_shares[paste0('X',a.UN_start)]
    un_fuel_shares <- un_fuel_shares %>%
        select( iso, fuel, paste0('X', a.extension_start_year:1971)) %>%
        arrange(iso)

    ceds_fuel_shares <- a.CEDS_comb_with_other %>%
      group_by(iso, fuel) %>%
      summarize_if(is.numeric, sum) %>%
      calculate_shares( id_columns = c('iso'),
                        target_column = 'fuel')

    # Write function to disaggregate by iea start date, then apply
    un_disagregate_fuels_by_iea <- function(iea_start){
        # Define useful variables
        disagregate_years <- paste0('X',1750:(iea_start-1))

        # Finish fuel breakdowns based on iea year
        # extend ceds fuel shares
        ceds_fuel_shares_complete <- ceds_fuel_shares
        ceds_fuel_shares_complete[paste0('X',a.extension_start_year:(iea_start - 1))] <-ceds_fuel_shares_complete[paste0('X',iea_start)]
        ceds_fuel_shares_complete <- ceds_fuel_shares_complete %>%
            select( iso, fuel, paste0('X', a.extension_start_year:iea_start)) %>%
            arrange(iso)

        # Add ceds coal coke to un shares and renormalize
        shares_combined <- rbind.fill(un_fuel_shares ,
                                      ceds_fuel_shares_complete %>% filter(fuel == 'coal_coke') ) %>%
            select(iso, fuel, one_of(disagregate_years)) #add coal coke
        un_fuel_shares_complete <- expand.grid( iso = unique(un_fuel_shares$iso),
                                                fuel = a.ceds_extension_fuels, stringsAsFactors=F) %>%
            left_join( shares_combined, by = c('iso','fuel')) # combine
        un_fuel_shares_complete[is.na(un_fuel_shares_complete)] <- 0 # replace nas with 0
        un_fuel_shares_complete <- calculate_correct_shares(un_fuel_shares_complete,
                                                    a.id_columns = 'iso',
                                                    a.target_column = 'fuel',
                                                    a.corrections = a.default_fuel_share) %>%
            arrange(iso) # renmormalize

        # Define Countries
        countries <- iea_start_year %>%
            filter(start_year == iea_start) %>%
            filter(iso %in% un_fuel_shares$iso) %>%
            pull(iso)

        # Combine and disagregate
        CEDS_UN_disaggregate <- un_fuel_shares_complete %>% filter( iso %in% countries) %>%
            left_join(a.CEDS_UN_aggregate %>%
                          filter( iso %in% countries ) %>% select(-fuel) ,
                      by = c('iso'), suffix = c('.share','.total'))
        CEDS_UN_disaggregate[is.na(CEDS_UN_disaggregate)] <- 0
        CEDS_UN_disaggregate[disagregate_years] <- CEDS_UN_disaggregate[ paste0(disagregate_years,'.total') ]* CEDS_UN_disaggregate[ paste0(disagregate_years,'.share') ]

        CEDS_UN_disaggregate <- CEDS_UN_disaggregate %>%
            select( iso, fuel, disagregate_years)

        return(CEDS_UN_disaggregate)
    }

    un_ceds_fuel_breakdown_list <- lapply(unique(a.iea_start_years$start_year),un_disagregate_fuels_by_iea)
    un_ceds_fuel_breakdown <- do.call(rbind.fill, un_ceds_fuel_breakdown_list) %>%
        arrange(iso)

   }else{
     ceds_fuel_shares <- a.CEDS_comb_with_other %>%
       group_by(iso, fuel) %>%
       summarize_if(is.numeric, sum) %>%
       calculate_shares( id_columns = c('iso'),
                         target_column = 'fuel')
     un_ceds_fuel_breakdown <- NULL
   }

    #-------------------
    # Disaggregate to CEDS fuels for countries with only CEDS data (no UN data)

    # Write function to disaggregate by iea start date, then apply
    ceds_disagregate_fuels_by_iea <- function(iea_start){
        # Define useful variables
        disagregate_years <- paste0('X',1750:(iea_start-1))

        # Define Countries
        countries <- iea_start_year %>%
            filter(start_year == iea_start) %>%
            filter(iso %in% ceds_fuel_shares$iso) %>%
            filter(iso %in% a.CEDS_only_aggregate$iso) %>%
            pull(iso)

        if( length(countries)>0){
            # Finish breakdown
            ceds_fuel_shares_extended <- ceds_fuel_shares
            ceds_fuel_shares_extended[disagregate_years] <- apply( ceds_fuel_shares_extended[paste0('X',iea_start:(iea_start+2))], 1, mean )

            ceds_shares_complete <- expand.grid( iso = countries,
                                                 fuel = a.ceds_extension_fuels, stringsAsFactors=F) %>%
                left_join( ceds_fuel_shares_extended, by = c('iso','fuel')) %>%
                select(iso, fuel, disagregate_years) %>%
                arrange(iso) %>%
              calculate_correct_shares(a.id_columns = 'iso',
                                       a.target_column = 'fuel',
                                       a.corrections = a.default_fuel_share)

            ceds_shares_complete[is.na(ceds_shares_complete)] <- 0

            # Combine and disagregate
            CEDS_only_disaggregate <- ceds_shares_complete %>% filter( iso %in% countries) %>%
                left_join(a.CEDS_only_aggregate %>%
                              filter( iso %in% countries ) %>% select(-fuel) ,
                          by = c('iso'), suffix = c('.share','.total'))
            CEDS_only_disaggregate[is.na(CEDS_only_disaggregate)] <- 0
            CEDS_only_disaggregate[disagregate_years] <- CEDS_only_disaggregate[ paste0(disagregate_years,'.total') ]* CEDS_only_disaggregate[ paste0(disagregate_years,'.share') ]

            CEDS_only_disaggregate <- CEDS_only_disaggregate %>%
                select( iso, fuel, disagregate_years)

            out <- CEDS_only_disaggregate
        }else{out <- NULL}
        return(out)
    }
    ceds_fuel_breakdown_list <- lapply(unique(a.iea_start_years$start_year), ceds_disagregate_fuels_by_iea)
    ceds_fuel_breakdown <- do.call(rbind.fill, ceds_fuel_breakdown_list) %>%
        arrange(iso, fuel)

    #-------------------
    # Combine
    all_fuel_breakdown <- rbind.fill( un_ceds_fuel_breakdown, ceds_fuel_breakdown)

    # Check that all countries have data
    if (length(unique(all_fuel_breakdown$iso)) != length(unique(a.iea_start_years$iso))){ stop( 'Not all countries have data in fuel_breakdown(), please check.' )}
    # Check for na's (There will be na's between 1960 and 1971)
    if( anyNA(all_fuel_breakdown[ paste0('X',a.extension_start_year:min(a.iea_start_years$start_year - 1)) ] )) stop( "Data have NA's in fuel_breakdown(), please check.")

    return(all_fuel_breakdown)
} # End of Fuel_break_down()


#-------------------------------------------------------------------------------
# sector_breakdown
#
# Brief:
#   This function splits disaggregate fuel use into sectors.
#
# Details:
#   The function computes the final default combustion activity from
#   country-ceds fuel totals using the share breakdowns from A6.1 scripts
#
# Authors: Rachel Hoesly, Caleb Braun
#
# Parameters:
#   fuel_totals: Extended CEDS_fuel totals by iso until the IEA start year
#   sector_shares: Extended CEDS_sector shares by iso and CEDS_fuel until the
#     IEA start year
#   iea_start_years: Two coulumn data.frame matching iso to IEA start year
#   ceds_extension_fuels: Character vector of CEDS_fuel
#   extension_start_year: Earliest year in the extension
#
# Return: CED_UN_disaggregated_fuel
sector_breakdown <- function(fuel_totals, sector_shares, iea_start_years,
                             ceds_extension_fuels, extension_start_year = 1750) {

    # Typically called with:
    # fuel_totals = all_disaggregate_fuel
    # sector_shares = final_sector_shares_all
    # iea_start_years = iea_start_year
    # ceds_extension_fuels = ceds_extension_fuels
    # extension_start_year = 1750

    # disaggregate by IEA start year
    disaggregate_sector <- function(iea_start_yr) {

        # Get all countries with the given IEA start year
        countries <- iea_start_years %>%
            dplyr::filter(start_year == iea_start_yr) %>%
            dplyr::pull(iso) %>%
            unique()

        disagg_years <- paste0('X', extension_start_year:(iea_start_yr - 1))

        disagg_activity <-
            expand.grid(iso = countries,
                        fuel = ceds_extension_fuels,
                        sector = unique(sector_shares$sector),
                        stringsAsFactors = F) %>%
            dplyr::left_join(sector_shares, by = c('iso', 'sector', 'fuel')) %>%
            dplyr::left_join(fuel_totals %>% select(iso, fuel, disagg_years),
                             by = c('iso', 'fuel'),
                             suffix = c('.share', '.total'))

        disagg_activity[is.na(disagg_activity)] <- 0

        total_years <- paste0(disagg_years, '.total')
        share_years <- paste0(disagg_years, '.share')

        disagg_activity[disagg_years] <- disagg_activity[total_years] *
                                         disagg_activity[share_years]

        dplyr::select(disagg_activity, iso, fuel, sector, disagg_years)
    }

    disagg_sector_list <- lapply(unique(iea_start_years$start_year), disaggregate_sector)
    disagg_sector_activity <- do.call(rbind.fill, disagg_sector_list)

    dplyr::arrange(disagg_sector_activity, iso)
}
