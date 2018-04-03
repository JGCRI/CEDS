

#-------------------------------------------------------------------------------------------------------------
# Disaggregate_to_CEDS_Sectors
#
# Brief:        This function disaggregates fuel data to CEDS Sector
# Details:      This function disaggregates fuel data to CEDS Sector using default sector breakdown values
#               To produce a default actvity file.
#
# Dependencies: Fuel_break_down() : Disaggregate_to_CEDS_Sectors() gets its CED_UN_disaggregated_by_fuel from Fuel_break_down()
#
# Author(s): Presley Muwan
#
# Parameters: default_sector_breakdown: dataframe containing CEDS Sector breakdown values
#             a.comb_activity_data: Dataframe with IEA Energy data
#             CED_UN_disaggregated_by_fuel: Fuel data to be split by sector
#             ceds_extension_fuels: fuels to be processed
#             agg_fuel_name: The collective name given to the fuels (like "Coal" :(brown coal, hard coal and coal coke))
#             all_countries: country isos
#             iea_start_years: IEA start year dataframe
#             start_year: extension start year
#
#
# Return:    dafault_avtivity_data
# Input Files:
# Output Files: none
Disaggregate_to_CEDS_Sectors <- function( a.comb_activity_data , CED_UN_disaggregated_by_fuel, default_sector_breakdown,
                                          ceds_extension_fuels, agg_fuel_name, all_countries, iea_start_years,  extension_start_year= 1750 ){

  # a.comb_activity_data = A.comb_activity_with_other
  # CED_UN_disaggregated_by_fuel = CED_UN_coal_disaggregated_fuel
  # default_sector_breakdown = final_sector_shares_coal
  # ceds_extension_fuels = ceds_extension_fuels
  # agg_fuel_name = "coal"
  # all_countries = unique( CED_UN_coal_disaggregated_fuel$iso )
  # iea_start_years = iea_start_year
  # extension_start_year= 1750

   # Make sure that CEDS_UN data has same isos as CEDS-IEA
   CED_UN_disaggregated_by_fuel <- CED_UN_disaggregated_by_fuel[ CED_UN_disaggregated_by_fuel$iso %in% default_sector_breakdown$iso &
                                                                 CED_UN_disaggregated_by_fuel$fuel %in% ceds_extension_fuels, ]

   default_sector_breakdown <- default_sector_breakdown[ default_sector_breakdown$iso %in% CED_UN_disaggregated_by_fuel$iso &
                                                           default_sector_breakdown$fuel %in% ceds_extension_fuels, ]


  # extract the years
  data_years <- names(default_sector_breakdown)[grepl('X', names(default_sector_breakdown))]

  #default_sector_breakdown <- default_sector_breakdown[default_sector_breakdown$fuel %in% c(ceds_extension_fuels), ]

  printLog("Splitting data among sectors using breakdown values")

  # b. Compute the percentage breakdown for each fuel

  CEDS_Dafault_Actvity <- left_join( default_sector_breakdown, CED_UN_disaggregated_by_fuel, by = c("iso", "fuel")  )

  CEDS_Dafault_Actvity[is.na(CEDS_Dafault_Actvity)] <- 0; #candidate for diagnostics; not all CEDS countries have all fuel types

  CEDS_Dafault_Actvity[, data_years] <- CEDS_Dafault_Actvity[ paste0(data_years,'.y') ] * CEDS_Dafault_Actvity[ paste0(data_years,'.x') ]

  CEDS_Dafault_Actvity <- CEDS_Dafault_Actvity[ c( "iso","sector", "fuel", data_years ) ]


  # Join original CEDS sector data [1960/1971 - 2014] to the CEDS_UN data [1750 - 1959/1970]
  dafault_avtivity_data <- lapply( unique( iea_start_years$start_year ), function( iea_start ){

     # iea_start <- 1971
    countries <- iea_start_year[ which( iea_start_year$start_year == iea_start ),'iso' ]

    ceds_original_data <- a.comb_activity_data[ which( a.comb_activity_data$iso %in% countries &
                                                  a.comb_activity_data$fuel %in% ceds_extension_fuels),
                                           c( 'iso','sector','fuel', paste0( 'X',iea_start:end_year ) ) ]
    ceds_un_processed_data <- CEDS_Dafault_Actvity[(CEDS_Dafault_Actvity$iso %in% countries),
                                                   c('iso','fuel','sector', paste0( 'X', extension_start_year:(iea_start-1) ))] %>%
                              mutate( units = "kt")

    default_activity <- full_join( ceds_un_processed_data, ceds_original_data, by = c("iso","sector","fuel") )

  return( default_activity ) })


  dafault_avtivity_data <- do.call( rbind, dafault_avtivity_data )

  xyears <- names(dafault_avtivity_data)[grepl('X',names(dafault_avtivity_data))]

  dafault_avtivity_data <- dafault_avtivity_data[ c("iso","sector","fuel", "units", xyears) ]

  dafault_avtivity_data[ is.na( dafault_avtivity_data ) ] <- 0


  return(dafault_avtivity_data)

}#Disaggregate_to_CEDS_Sectors() Ends




#-------------------------------------------------------------------------------------------------------------
# Fuel_break_down
#
# Brief:        This function split aggregated fuel data into disaggregated fuels.
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
Fuel_break_down <- function( CED_UN_fuel_Data_extended, CED_only_data_extended,
                             ceds_iea_original_by_fuel, unsd_energy_consumption_data,
                             ceds_extension_fuels, extension_start_year = 1750 , cdiac_fuel,
                             aggregated_fuel = "coal"){

  # CED_UN_fuel_Data_extended <- ceds_un_coal_data$un_ceds
  # CED_only_data_extended <- ceds_un_coal_data$ceds_only
  # ceds_iea_original_by_fuel <- A.comb_activity
  # unsd_energy_consumption_data <- UNSD_Energy_Final_Consumption
  # ceds_extension_fuels <- ceds_extension_fuels
  # extension_start_year  <- 1750
  # cdiac_fuel = cdiac_solid_fuel
  # aggregated_fuel = "coal"


    # CED_UN_fuel_Data_extended = ceds_un_ng_data$un_ceds
    # CED_only_data_extended = ceds_un_ng_data$ceds_only
    # ceds_iea_original_by_fuel = A.comb_activity_with_other
    # unsd_energy_consumption_data =  UNSD_Energy_1950_1990_Final_Consumption
    # ceds_extension_fuels
    # extension_start_year = 1850
    # aggregated_fuel = extension_fuel_category
    # cdiac_fuel = cdiac_liquid_fuel


  printLog("Processing Fuel break down values for extended years")


  # Aggregate data by iso-fuel to get petroleum TOTALS by Countries
  x_years <- names(ceds_iea_original_by_fuel)[grepl("X", names(ceds_iea_original_by_fuel))]
  ceds_iea_original_by_fuel <- aggregate( ceds_iea_original_by_fuel[x_years],
                                          by = list( iso = ceds_iea_original_by_fuel$iso,
                                                     fuel = ceds_iea_original_by_fuel$fuel) ,
                                          FUN = sum)


  iea_start_years <- readData( 'ENERGY_IN' , 'IEA_iso_start_data')

  CED_UN_disaggregated_fuel <- data.frame()

  # If there is UN_CEDS data process fuel break down
  if(nrow(CED_UN_fuel_Data_extended) > 1){

    CED_UN_countries <-  CED_UN_fuel_Data_extended$iso

    # Remove CED_UN Countries fron original iea data
    #ceds_iea_original_by_fuel <- ceds_iea_original_by_fuel[ceds_iea_original_by_fuel$iso %!in% CED_UN_countries, ]

    # Extract UNSD data relevant to the fuel being processed
    unsd_data <- unsd_energy_consumption_data[ ( unsd_energy_consumption_data$fuel %in% ceds_extension_fuels ) &
                                                 (unsd_energy_consumption_data$iso %in% CED_UN_countries), ]

    # aggregate fuel by iso: this value will be used as a divisor in computing fuel ratio
    # TODO: uses unsd end years instead of 1950
    CED_UN_disaggregated_fuel <- split_to_fuel_types( unsd_data, CED_UN_fuel_Data_extended,
                                                extension_start_year, 1950, 1950)

    CED_UN_disaggregated_fuel <- left_join( CED_UN_disaggregated_fuel, unsd_data[c( 'iso','fuel', paste0('X',1951:1971) )] )

  }else{
    printLog("There is no CEDS-UNSD data to process")
  }# else if Ends


  # disaggregate ceds IEA data using cdiac data
  ceds_exclusive_ctry_by_fuel_list <- lapply(unique(iea_start_years$start_year), function(iea_start){

    # iea_start <- 1971
    countries <- iea_start_years[which( iea_start_years$start_year %in% iea_start),'iso']

    CED_only_data_extended_original <- CED_only_data_extended[ CED_only_data_extended$iso %in% countries, ]

    if(nrow(CED_only_data_extended_original) == 0){
      return(NULL)
    }
    ceds_iea_original_by_fuel_oecd <- ceds_iea_original_by_fuel[ ceds_iea_original_by_fuel$iso %in% countries, ]

    # aggregate fuel by iso: this value will be used as a divisor in computing fuel ratio
    ceds_iea_by_fue_return <- split_to_fuel_types( ceds_iea_original_by_fuel_oecd, CED_only_data_extended,
                                extension_start_year,  iea_start  )


    return(ceds_iea_by_fue_return)

  })#ceds_exclusive_ctry_fuel_data_extended Ends

  ceds_exclusive_ctry_by_fuel <- do.call( rbind, ceds_exclusive_ctry_by_fuel_list )
  CED_and_UN_disaggregated_fuel <- do.call( rbind, list( ceds_exclusive_ctry_by_fuel , CED_UN_disaggregated_fuel )  )

  #-----------------------------------------------------------------------------------------------------
  # The section processes CEDS fuel data for UNSD country with no data for these fuel types. For example
  # Coal coke (considered as a secondary fuel) was not included in the UNSD Consumption data, so
  # UNSD countries need this data from CEDS-IEA.
  # Another example is Brown coal, for UNSD Countries missing brown coal need this data from the CEDS

  ceds_fuel_data <- ceds_iea_original_by_fuel

  ceds_iea_template <- ceds_fuel_data
  ceds_iea_template <- distinct(ceds_iea_template)

  # extract coal coke and brown coal data that were not included in the UNSD dataset
  un_missing_coals <-  anti_join(ceds_iea_template , CED_and_UN_disaggregated_fuel[c("iso","fuel")], by=c("iso", "fuel") )

  if(nrow(un_missing_coals) > 1){

      # aggregate by fuel to be exteended  by CDIAC
      XYears <- names(un_missing_coals)[grepl("X", names(un_missing_coals))]
      un_missing_coals_by_iso <-  aggregate( un_missing_coals[XYears],
                                            by = list( iso = un_missing_coals$iso ),
                                            FUN = sum)
      # prepare fuel for extension
      un_missing_coals_by_iso$fuel <- "solid_fuels"

      if(aggregated_fuel %in% c("natural_gas", "petroleum")){
        un_missing_coals_by_iso$fuel <- "liquid_fuels"
      }#if Ends

      un_missing_coals_by_iso$sector <- "all"

      cdiac_fuel <- cdiac_fuel[ cdiac_fuel$fuel %in% c( unique( un_missing_coals_by_iso$fuel)), ]
      cdiac_fuel$sector <- "all"

    # disaggregate ceds IEA data using cdiac data
    un_missing_fuel_list <- lapply(unique(iea_start_years$start_year), function(iea_start){

          # iea_start <- 1971
          countries <- iea_start_years[which( iea_start_years$start_year == iea_start),'iso']

          # Extend un_missing_fuel_list backward to 1750/1850
          drivers <- cdiac_fuel[which(cdiac_fuel$iso %in% countries ),]
          ceds_data <- un_missing_coals_by_iso[ un_missing_coals_by_iso$iso %in% countries, ]


          # Extension years
          extension_year <- names(drivers)[names(drivers) %!in% names(ceds_data)]

          ceds_data[extension_year] <- NA
          ceds_data <- ceds_data[ names(drivers) ]

          if(nrow(ceds_data) > 1){

            un_missing_fuel_extended <- extend_data_on_trend(driver_trend = drivers, input_data = ceds_data,
                                                  start = extension_start_year, end = iea_start)
          }else{return(NULL)}

          #-------------------------------------------------------------------------------------
          # Disaggregate by fuel types

          # CED_only_data_extended_original <- CED_only_data_extended[ CED_only_data_extended$iso %in% countries, ]

          un_missing_coals_oecd <- un_missing_coals[ un_missing_coals$iso %in% countries, ]


          # aggregate fuel by iso: this value will be used as a divisor in computing fuel ratio
          un_missing_by_fuels <- split_to_fuel_types( un_missing_coals_oecd, un_missing_fuel_extended,
                                                         extension_start_year,  iea_start  )


          return(un_missing_by_fuels)

    })#ceds_exclusive_ctry_fuel_data_extended Ends

    un_missing_coals_by_fuel <- do.call( rbind, un_missing_fuel_list)
  }#

  #------------------------------------------
  # Final output
  CED_and_UN_disaggregated_fuel <- do.call(rbind, list(CED_and_UN_disaggregated_fuel, un_missing_coals_by_fuel))


  printLog("Processing Fuel break down values for extended years")


  # Aggregate data by iso-fuel to get petroleum TOTALS by Countries
  x_years <- names(ceds_iea_original_by_fuel)[grepl("X", names(ceds_iea_original_by_fuel))]
  ceds_iea_original_by_fuel <- aggregate( ceds_iea_original_by_fuel[x_years],
                                          by = list( iso = ceds_iea_original_by_fuel$iso,
                                                     fuel = ceds_iea_original_by_fuel$fuel) ,
                                          FUN = sum)


  iea_start_years <- readData( 'ENERGY_IN' , 'IEA_iso_start_data')

  CED_UN_disaggregated_fuel <- data.frame()

  # If there is UN_CEDS data process fuel break down
  if(nrow(CED_UN_fuel_Data_extended) > 1){

    CED_UN_countries <-  CED_UN_fuel_Data_extended$iso

    # Remove CED_UN Countries fron original iea data
    #ceds_iea_original_by_fuel <- ceds_iea_original_by_fuel[ceds_iea_original_by_fuel$iso %!in% CED_UN_countries, ]

    # Extract UNSD data relevant to the fuel being processed
    unsd_data <- unsd_energy_consumption_data[ ( unsd_energy_consumption_data$fuel %in% ceds_extension_fuels ) &
                                                 (unsd_energy_consumption_data$iso %in% CED_UN_countries), ]

    # aggregate fuel by iso: this value will be used as a divisor in computing fuel ratio
    # TODO: uses unsd end years instead of 1950
    CED_UN_disaggregated_fuel <- split_to_fuel_types( unsd_data, CED_UN_fuel_Data_extended,
                                                extension_start_year, 1950, 1950)

    CED_UN_disaggregated_fuel <- left_join( CED_UN_disaggregated_fuel, unsd_data[c( 'iso','fuel', paste0('X',1951:1971) )] )

  }else{
    printLog("There is no CEDS-UNSD data to process")
  }# else if Ends


  # disaggregate ceds IEA data using cdiac data
  ceds_exclusive_ctry_by_fuel_list <- lapply(unique(iea_start_years$start_year), function(iea_start){

    # iea_start <- 1971
    countries <- iea_start_years[which( iea_start_years$start_year %in% iea_start),'iso']

    CED_only_data_extended_original <- CED_only_data_extended[ CED_only_data_extended$iso %in% countries, ]

    if(nrow(CED_only_data_extended_original) == 0){
      return(NULL)
    }
    ceds_iea_original_by_fuel_oecd <- ceds_iea_original_by_fuel[ ceds_iea_original_by_fuel$iso %in% countries, ]

    # aggregate fuel by iso: this value will be used as a divisor in computing fuel ratio
    ceds_iea_by_fue_return <- split_to_fuel_types( ceds_iea_original_by_fuel_oecd, CED_only_data_extended,
                                extension_start_year,  iea_start  )


    return(ceds_iea_by_fue_return)

  })#ceds_exclusive_ctry_fuel_data_extended Ends

  ceds_exclusive_ctry_by_fuel <- do.call( rbind, ceds_exclusive_ctry_by_fuel_list )
  CED_and_UN_disaggregated_fuel <- do.call( rbind, list( ceds_exclusive_ctry_by_fuel , CED_UN_disaggregated_fuel )  )

  #----------------------------------------------------------------------------------------------\
  # Processs Data for fuel whose countries had UNSD data for fuels than them*
  # Coal coke, for example had no (UNSD data for all countries) but USA (as UNSD country) needs
  # its coal coke data from CEDS  (which is why this processing is being done here). Also, the Presence of
  # brown coal did not stop the processing of coal data; UNSD countries with no brown coal have to derive
  # their brown coal data from CEDS-IEA

  ceds_fuel_data <- ceds_iea_original_by_fuel

  ceds_iea_template <- ceds_fuel_data
  ceds_iea_template <- distinct(ceds_iea_template)

  # extract coal coke and brown coal data that were not included in the UNSD dataset
  un_missing_coals <-  anti_join(ceds_iea_template , CED_and_UN_disaggregated_fuel[c("iso","fuel")], by=c("iso", "fuel") )

  if(nrow(un_missing_coals) > 1){

    # aggregate by fuel to be exteended  by CDIAC
    XYears <- names(un_missing_coals)[grepl("X", names(un_missing_coals))]
    un_missing_coals_by_iso <-  aggregate( un_missing_coals[XYears],
                                           by = list( iso = un_missing_coals$iso ),
                                           FUN = sum)

    # prepare fuel for extension
    un_missing_coals_by_iso$fuel <- "solid_fuels"

    if(aggregated_fuel %in% c("natural_gas", "petroleum")){
      un_missing_coals_by_iso$fuel <- "liquid_fuels"
    }#if Ends

    un_missing_coals_by_iso$sector <- "all"


    cdiac_fuel <- cdiac_fuel[ cdiac_fuel$fuel %in% c( unique( un_missing_coals_by_iso$fuel)), ]
    cdiac_fuel$sector <- "all"

    # disaggregate ceds IEA data using cdiac data
    un_missing_fuel_list <- lapply(unique(iea_start_years$start_year), function(iea_start){

          # iea_start <- 1971
          countries <- iea_start_years[which( iea_start_years$start_year == iea_start),'iso']

          # Extend un_missing_fuel_list backward to 1750/1850
          drivers <- cdiac_fuel[which(cdiac_fuel$iso %in% countries ),]
          ceds_data <- un_missing_coals_by_iso[ un_missing_coals_by_iso$iso %in% countries, ]


          # Extension years
          extension_year <- names(drivers)[names(drivers) %!in% names(ceds_data)]

          ceds_data[extension_year] <- NA
          ceds_data <- ceds_data[ names(drivers) ]

          if(nrow(ceds_data) > 1){

            un_missing_fuel_extended <- extend_data_on_trend(driver_trend = drivers, input_data = ceds_data,
                                                  start = extension_start_year, end = iea_start)
          }else{return(NULL)}

          #-------------------------------------------------------------------------------------
          # Disaggregate by fuel types

          # CED_only_data_extended_original <- CED_only_data_extended[ CED_only_data_extended$iso %in% countries, ]

          un_missing_coals_oecd <- un_missing_coals[ un_missing_coals$iso %in% countries, ]


          # aggregate fuel by iso: this value will be used as a divisor in computing fuel ratio
          un_missing_by_fuels <- split_to_fuel_types( un_missing_coals_oecd, un_missing_fuel_extended,
                                                         extension_start_year,  iea_start  )


          return(un_missing_by_fuels)

      })#ceds_exclusive_ctry_fuel_data_extended Ends
    un_missing_coals_by_fuel <- do.call( rbind, un_missing_fuel_list)

    ##### DONT REDEFINE VARIABLES
    CED_and_UN_disaggregated_fuel <- do.call(rbind, list(CED_and_UN_disaggregated_fuel, un_missing_coals_by_fuel))


  }#if Ends


  return(CED_and_UN_disaggregated_fuel)

}#Fuel_break_down Ends



#-------------------------------------------------------------------------------------------------------------
# split_to_fuel_types
#
# Brief:        This function is a helper function to Fuel_break_down; it is used to computed fuel shares
#
# Dependencies:
#
# Author(s): Presley Muwan
#
# Parameters: divisor_data_frame : data from which to derive a divisor
#             data_to_split : data to split
#             ratio_year: year from which to derive ratio
#             iea_end_year:
#             start_year
#
#
# Return:    CED_UN_fuel_Data_extended
#
# Input Files:
# Output Files: none
split_to_fuel_types  <- function(divisor_data_frame, data_to_split, start_year, ratio_year, end_year=1971) {

  # aggregate fuel by iso: this value will be used as a divisor in computing fuel ratio
  divisor <-  aggregate( divisor_data_frame[ paste0("X",ratio_year) ],
                          by = list( iso = divisor_data_frame$iso),
                           FUN = sum)

  divisor["divisor"] <- divisor[paste0("X",ratio_year)]

  # process fuel break down for CED_ONLY data
  # Compute the fuel split ratio using the 1950 data. This split will be used to disaggregate fuel data before 1950 (1949 and below)
  fuel_shares <- left_join( divisor_data_frame, divisor[c("iso", "divisor") ], by= c("iso"))

  fuel_shares["shares"] <- fuel_shares[ paste0( paste0('X',ratio_year)) ]/fuel_shares["divisor"]

  # replate values with zero if..
  fuel_shares$shares[ is.na(fuel_shares$shares ) ] <- 0 #it is not a number
  fuel_shares$shares[ (fuel_shares$shares < 0) ] <- 0 #it is negative

  #---------------------------------------------------------------------------------------------------------
  # Disaggregate by fuel using the fuel split ratio computed above

  # dissagregate the by fuel by multiplying the each fuel data with its corresponding split ratio

  shared_data  <- left_join( data_to_split[ , c("iso",  paste0('X', start_year:(ratio_year)))],
                                                         fuel_shares[c("iso", "fuel", "shares")],
                                                         by = c("iso"))

  shared_data[ paste0('X', start_year:ratio_year)]  <- ( shared_data[ paste0('X', start_year:ratio_year)] *
                                                                   shared_data$shares )

  ceds_iea_by_fuel   <- left_join( shared_data[ c("iso","fuel", paste0('X', start_year:(ratio_year-1)) ) ],
                                       divisor_data_frame[ c("iso","fuel", paste0('X', ratio_year:end_year ) ) ], by=c("iso", "fuel")  )

  return(ceds_iea_by_fuel)

}#split_to_fuel_types() Ends


#-------------------------------------------------------------------------------------------------------------
# process_and_combine_un_ced_data
#
# Brief:        This function reformats ceds and un data into thesame
#               structural format and merges the two by their columns.

# Details:      UN data from 1950 - 1959/1970 combined to with CEDS IEA 1960/1971 data
#               with respect to whether a country is an OECD country or not. The combined
#               data is then extend backward, using cdiac data, to a specified year
#               (the year is passed as a parameter)
#
# Dependencies:
#
# Author(s): Presley Muwan
#
# Parameters: ceds_fuel_data : CEDS data to be processed
#             cdiac_data : data used for extending ceds data
#             un_consumption_data: UNSD consumption data
#             ceds_extension_fuels: fuels to be processed
#             extension_start_year:
#             iea_end_year:
#             iea_start_years
#             start_year
#             cdiac_end_year
#
#
# Return:    CED_UN_fuel_Data_extended
#
# Input Files:
# Output Files: none
process_and_combine_un_ced_data <- function( ceds_fuel_data, cdiac_data, un_consumption_data,
                                             ceds_extension_fuels, extension_start_year,
                                             iea_start_years, iea_end_year,
                                             cdiac_end_year = iea_end_year){

  # ceds_fuel_data <- A.comb_activity_with_other
  # cdiac_data <- cdiac_solid_fuel
  # un_consumption_data <- UNSD_Energy_Final_Consumption
  # extension_start_year = 1750
  # ceds_extension_fuels = ceds_extension_fuels
  # iea_start_years = iea_start_year
  # iea_end_year = end_year
  # a.comb_activity_data = A.comb_activity
  # cdiac_end_year = iea_end_year

  # format ceds data
  ceds_fuel_data$fuel <- 'solid_fuels'
  ceds_fuel_data$sector <- 'all'
  ceds_fuel_data[ is.na( ceds_fuel_data ) ] <- 0

  # format cdiac data
  cdiac_data$sector <- 'all'
  cdiac_data <- cdiac_data[ c('iso','sector','fuel',paste0("X",extension_start_year:cdiac_end_year)) ]



  # B. Format UN data
  UNSD_Final_Consumption <- un_consumption_data[ un_consumption_data$fuel %in% ceds_extension_fuels,
                                                 c( "iso","fuel", paste0( 'X',1950:1976 ) ) ]
  if(nrow(UNSD_Final_Consumption) > 1){
    UNSD_Final_Consumption$fuel <- "solid_fuels"
    UNSD_Final_Consumption$sector <- "all"
    UNSD_Final_Consumption$iso[ UNSD_Final_Consumption$iso == 'srb' ]  <- "srb (kosovo)"

    # aggregate UNSD by iso
    UNSD_Final_Consumption <-  aggregate( UNSD_Final_Consumption[paste0('X',1950:1976)],
                                          by = list( iso = UNSD_Final_Consumption$iso,
                                                     fuel = UNSD_Final_Consumption$fuel,
                                                     sector = UNSD_Final_Consumption$sector),
                                          FUN = sum)
  }#if Ends





  # Process petroluem and natural gas differently
  if( all('liquid_fuels' %in% unique(cdiac_data$fuel)) &
      all( c("light_oil","heavy_oil", "diesel_oil") %in% ceds_extension_fuels )) {

      ceds_fuel_data$fuel <- 'liquid_fuels'
      cdiac_fuel <- c('liquid_fuels')
      cdiac_data <- cdiac_data[ which( cdiac_data$fuel %in% cdiac_fuel ) , ]

      if(nrow(UNSD_Final_Consumption) > 1){
        UNSD_Final_Consumption$fuel <- "liquid_fuels"
      }

  }else if( all('gas_fuels' %in% unique(cdiac_data$fuel)) &
            all (ceds_extension_fuels %in% 'natural_gas') ) {

      ceds_fuel_data$fuel <- 'gas_fuels'
      cdiac_fuel <- c('gas_fuels')
      cdiac_data <- cdiac_data[ which( cdiac_data$fuel %in% cdiac_fuel ) , ]

      if(nrow(UNSD_Final_Consumption) > 1){
        UNSD_Final_Consumption$fuel <- "gas_fuels"
      }

  }#if Else Ends


  # aggregate ceds_fuel_data by iso
  ceds_data_years <- names(ceds_fuel_data)[grepl("X",names(ceds_fuel_data))]

  ceds_fuel_data <-  aggregate( ceds_fuel_data[ ceds_data_years ],
                                        by = list( iso = ceds_fuel_data$iso,
                                                   fuel = ceds_fuel_data$fuel,
                                                   sector = ceds_fuel_data$sector),
                                        FUN = sum)


  # Determine CEDS Countries with no UNSD data
  ceds_exclusive_countries <- ceds_fuel_data$iso[ ceds_fuel_data$iso %!in% UNSD_Final_Consumption$iso ]

  printLog(length(ceds_exclusive_countries), " CEDS IEA countries have no UNSD data  ")

  # Retrieve data from countries common to iea and UNSD
  CED_UN_fuel_Data <- ceds_fuel_data[ ceds_fuel_data$iso %!in% ceds_exclusive_countries, ]
  #-----------------------------------------------------------------------------------------------------------------------
  # C. Combine IEA[1960/71- Present] data to UNSD [1950 - 60/71]data

  # Merge and Process CED_UN data if it is available
  CED_UN_fuel_Data_Merged_Extended <- data.frame()

  if(nrow(CED_UN_fuel_Data) != 0){

    printLog("Merging UNSD data to CEDS IEA data (UN_CEDS DATA)")


    # extend ceds IEA data using cdiac data
    CED_UN_fuel_Data_Merged_list <- lapply(unique(iea_start_years$start_year), function(iea_start){


      # Maintain a smooth continuity from UN to CEDS (Do this seperately for NON-OECD C'tries)
      if(start_year == 1960 ){
        un_merge_years <- paste0('X', 1960:1965,".y")
        ceds_merge_years <- paste0('X', 1960:1965,".x")
        UN_CED_Years <- paste0('X',  1960:1965 )
      }else{
        un_merge_years <- paste0('X', 1970:1976,".y")
        ceds_merge_years <- paste0('X', 1970:1976,".x")
        UN_CED_Years <- paste0('X', 1970:1976 )
      }

        countries <- iea_start_year[which( iea_start_year$start_year == iea_start),'iso']

        CED_UN_fuel_Data_Merged <- CED_UN_fuel_Data[ CED_UN_fuel_Data$iso %in% countries,
                                                     c( "iso","fuel","sector", UN_CED_Years ) ] %>%
        left_join( UNSD_Final_Consumption, by = c( "iso","fuel","sector" ) )

      for(year_index in seq(un_merge_years)){
        # print(un_merge_years[year_index])
        ceds_fraction <- (year_index-1)*(1/(length(un_merge_years)-1))
        un_fraction <- (1 - ceds_fraction )

        CED_UN_fuel_Data_Merged[ ceds_merge_years[year_index ] ] <- (CED_UN_fuel_Data_Merged[ceds_merge_years[year_index]] * un_fraction)
        CED_UN_fuel_Data_Merged[ un_merge_years[year_index ] ] <- (CED_UN_fuel_Data_Merged[un_merge_years[year_index]] * (year_index/length(un_merge_years)))

        CED_UN_fuel_Data_Merged[ UN_CED_Years[ year_index ] ] <- (CED_UN_fuel_Data_Merged[ ceds_merge_years[ year_index ] ] + CED_UN_fuel_Data_Merged[ un_merge_years [ year_index ] ])

      }#for Ends
        CED_UN_fuel_Data_Merged <- CED_UN_fuel_Data_Merged[ c( "iso","fuel","sector", paste0('X',  1950:1976 )) ]


        ceds_only_years <- names(CED_UN_fuel_Data)[ grepl("X", names(CED_UN_fuel_Data)) ]

        ceds_only_years <- ceds_only_years[ceds_only_years %!in% names(CED_UN_fuel_Data_Merged)]

        CED_UN_fuel_Data_Merged[ is.na( CED_UN_fuel_Data_Merged ) ] <- 0

        CED_UN_fuel_Data_Merged <- CED_UN_fuel_Data[ CED_UN_fuel_Data$iso %in%countries,
                                                       c( "iso","fuel","sector", ceds_only_years ) ] %>%
                                  join( CED_UN_fuel_Data_Merged, by = c( "iso","fuel","sector"))

        return(CED_UN_fuel_Data_Merged)

    })#ceds_exclusive_ctry_fuel_data_extended Ends

    CED_UN_fuel_Data_Merged <- do.call( rbind, CED_UN_fuel_Data_Merged_list )




    #-----------------------------------------------------------------------------------------------------------------
    # Extend backward from the most recent data (1950)

    printLog("Extending UN_CEDS [", paste(ceds_extension_fuels, collapse =","), "] DATA by CDIAC from 1950 back to ", extension_start_year)

    CED_UN_fuel_Data_Merged[ paste0( 'X', extension_start_year:1949 ) ] <- NA
    CED_UN_fuel_Data_Merged_Extended <- extend_data_on_trend(driver_trend = cdiac_data, input_data = CED_UN_fuel_Data_Merged,
                                                      start = extension_start_year, end = 1950)

    # some small countries don't have cdiac or have zero values through extension.
    CED_UN_fuel_Data_Merged_Extended[ is.na( CED_UN_fuel_Data_Merged_Extended ) ] <- 0
    CED_UN_fuel_Data_Merged_Extended <- CED_UN_fuel_Data_Merged_Extended[c("iso", "fuel", "sector", paste0("X",extension_start_year: 2014)) ]

  }else{
    printLog("There is no UNSD data to merge for [", paste0(ceds_extension_fuels, collapse = ","), "]")
  }#else if Ends

  #--------------------------------------------------------------------------------------------------------
  # Countries with no UN data shall be extended from 1960/1970
  printLog("Extending data for Countries with no UNSD data, directly, with CDIAC from 1960/1971")

  ceds_exclusive_ctry_fuel_data <- ceds_fuel_data[ ceds_fuel_data$iso %in% ceds_exclusive_countries,  ]
  ceds_exclusive_ctry_fuel_data[ paste0('X', extension_start_year: 1959 ) ] <- NA


  # extend ceds IEA data using cdiac data
  ceds_exclusive_ctry_fuel_data_extended_list <- lapply(unique(iea_start_years$start_year), function(iea_start){
    countries <- iea_start_year[which( iea_start_year$start_year == iea_start),'iso']
    drivers <- cdiac_data[which(cdiac_data$iso %in% countries ),]
    ceds_data <- ceds_exclusive_ctry_fuel_data[ ceds_exclusive_ctry_fuel_data$iso %in% countries, ]

    if(nrow(ceds_data) > 1){
      ceds_extended <- extend_data_on_trend(driver_trend = drivers, input_data = ceds_data,
                                            start = extension_start_year, end = iea_start)
    }else{ceds_extended <- NULL}

      return(ceds_extended)

    })#ceds_exclusive_ctry_fuel_data_extended Ends

  ceds_exclusive_ctry_fuel_data_extended <- do.call( rbind, ceds_exclusive_ctry_fuel_data_extended_list )

  # some small countries don't have cdiac or have zero values through extension.
  ceds_exclusive_ctry_fuel_data_extended[ is.na( ceds_exclusive_ctry_fuel_data_extended ) ] <- 0

  ceds_exclusive_ctry_fuel_data_extended <- ceds_exclusive_ctry_fuel_data_extended[c("iso", "fuel", "sector", paste0("X",extension_start_year: 2014)) ]



  return_list <- list ("ceds_only" = ceds_exclusive_ctry_fuel_data_extended, "un_ceds" = CED_UN_fuel_Data_Merged_Extended)

  return( return_list )

}#process_and_combine_un_ced_data() Ends





#-------------------------------------------------------------------------------------------------------------
# disaggregate_iea_other_by_fuel
#
# Brief:        This function disaggregates Other-transformation and other-feedstockk
#               (produced by A3.3_write_IEA_diff.R) by fuel
# Details:
#
# Dependencies:
#
# Author(s): Presley Muwan
#
# Parameters: a.comb_activty: data from which fuel shares are derrived
#             iea_other_data : data to disaggregated

#
#
# Return:    iea_other_data_by_fuel
#
# Input Files:
# Output Files: none
disaggregate_iea_other_by_fuel <- function(a.comb_activty, iea_other_data){

 # a.comb_activty <- A.comb_activity
 # iea_other_data <- iea_other_coal
 # iea_start_years <- iea_start_year

  iea_other_years <- c(names(iea_other_data))[grepl("X", names(iea_other_data))]

  a.comb_activty <-  aggregate( a.comb_activty[ iea_other_years ],
                                by = list( iso = a.comb_activty$iso,
                                           fuel = a.comb_activty$fuel),
                                FUN = sum)

  # disaggregate ceds IEA other data to fuel types
  iea_other_data_list <- lapply(unique(iea_other_data$sector), function(sector){

      countries <- iea_other_data[ which( iea_other_data$sector %in% sector ),'iso']

      sector <- unique(iea_other_data$sector)

      a.comb_activty_by_oecd <- a.comb_activty[ a.comb_activty$iso %in% countries, ]

      # creates a divisor for computing the split ratio.
      # The divisor a value that constitutes the sum of a country's fuel data
      #a.comb_activty_by_oecd$divisor <- NA

      a.comb_activty_by_oecd_divisor <-  aggregate( a.comb_activty_by_oecd[ iea_other_years ],
                                                  by = list( iso = a.comb_activty_by_oecd$iso),
                                                  FUN = sum)


      # process fuel break down for CED_ONLY data
      # Compute the fuel split ratio using the 1950 data. This split will be used to disaggregate fuel data before 1950 (1949 and below)
      a.comb_activty_by_oecd_shares <- left_join( a.comb_activty_by_oecd, a.comb_activty_by_oecd_divisor, by="iso")
      a.comb_activty_by_oecd_shares[iea_other_years] <- a.comb_activty_by_oecd_shares[paste0(iea_other_years, ".x")]/a.comb_activty_by_oecd_shares[paste0(iea_other_years, ".y")]
      a.comb_activty_by_oecd_shares <- a.comb_activty_by_oecd_shares[c("iso", "fuel",iea_other_years )]

      a.comb_activty_by_oecd_shares[ is.na(a.comb_activty_by_oecd_shares) ] <- 0
      a.comb_activty_by_oecd_shares[ a.comb_activty_by_oecd_shares == "NaN" ] <- 0


      #---------------------------------------------------------------------------------------------------------
      # Disaggregate by fuel using the fuel split ratio computed above


      # Add the data to be disaggregated to the template
      iea_other_data_shared <- inner_join( a.comb_activty_by_oecd_shares, by= c("iso"),
                                           iea_other_data[ iea_other_data$sector %in% sector, c("iso", "sector", iea_other_years) ])

      iea_other_data_shared$sector <- sector

      iea_other_data_shared[ iea_other_years ] <- (iea_other_data_shared[paste0(iea_other_years, ".x") ] * iea_other_data_shared[paste0(iea_other_years, ".y") ])

      iea_other_data_shared <- iea_other_data_shared[c("iso", "fuel", "sector", iea_other_years)]

    return(iea_other_data_shared)

  })#ceds_exclusive_ctry_fuel_data_extended Ends


  iea_other_data_by_fuel <- do.call(rbind, iea_other_data_list)
  iea_other_data_by_fuel <- distinct( iea_other_data_by_fuel )
  iea_other_data_by_fuel$units <- "kt"

  return(iea_other_data_by_fuel)

  # creates a divisor for computing the split ratio.
  # The divisor a value that constitutes the sum of a country's fuel data
  #a.comb_activty_by_oecd$divisor <- NA
  a.comb_activty_by_divisor <-  aggregate( a.comb_activty[ iea_other_years ],
                                                by = list( iso = a.comb_activty$iso),
                                                FUN = sum)


  # process fuel break down for CED_ONLY data
  # Compute the fuel split ratio using the 1950 data. This split will be used to disaggregate fuel data before 1950 (1949 and below)
  a.comb_activty_by_shares <- left_join( a.comb_activty, a.comb_activty_by_divisor, by="iso")
  a.comb_activty_by_shares[iea_other_years] <- a.comb_activty_by_shares[paste0(iea_other_years, ".x")]/a.comb_activty_by_shares[paste0(iea_other_years, ".y")]
  a.comb_activty_by_shares <- a.comb_activty_by_shares[c("iso", "fuel",iea_other_years )]

  a.comb_activty_by_shares[ is.na(a.comb_activty_by_shares) ] <- 0
  a.comb_activty_by_shares[ a.comb_activty_by_shares == "NaN" ] <- 0


  #---------------------------------------------------------------------------------------------------------
  # Disaggregate by fuel using the fuel split ratio computed above


  # Add the data to be disaggregated to the template
  iea_other_data_shared <- inner_join( a.comb_activty_by_shares, by= c("iso"),
                                           iea_other_data[, c("iso", iea_other_years) ])

  iea_other_data_shared[ iea_other_years ] <- (iea_other_data_shared[paste0(iea_other_years, ".x") ] * iea_other_data_shared[paste0(iea_other_years, ".y") ])

  iea_other_data_shared <- iea_other_data_shared[c("iso", "fuel", iea_other_years)]

  iea_other_data_shared <- distinct(iea_other_data_shared)

  return(iea_other_data_shared)
}# disaggregate_iea_other_by_fuel Ends

