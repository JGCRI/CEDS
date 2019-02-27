# ---------------------------------------------------------------------------
# Program Name: B1.1.base_N2O_comb_EF.R
# Author: Patrick O'Rourke
# Date Last Updated: February 27, 2019
# Program Purpose: Generate default emission factors for N2O from US GHG Inventory data
# Input Files: N2O_base_EF-stationary-US_GHG2018.csv, N2O_emissions-elec-US_GHG2018.csv
#              N2O_fuel_consumption-elec-US_GHG2018.csv, N2O_base_EF-mobile_offroad-US_GHG2018.csv,
#              N2O_fuel_consumption-mobile_offroad-US_GHG2018.csv, N2O_emissions-mobile_onroad-US_GHG2018.csv,
#              N2O_fuel_consumption-mobile_onroad-US_GHG2018.csv, N2O_VMT-mobile_onroad-US_GHG2018.csv,
#              N2O_base_EF-mobile_onroad-US_GHG2018.csv, N2O_EF-USGHG2018_mapping.csv,
#              USGHG-fuel_mapping.csv, Master_Country_List.csv
# Output Files: B.N2O_comb_EF_db.csv
# Notes:
#TODO:

# ---------------------------------------------------------------------------

# 0. Read in global settings and headers

# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( 'data_functions.R' )

# First messages to be printed to the log
log_msg <- "Produce N2O emissions factors from US EPA GHG data"
script_name <- 'B1.1.base_N2O_comb_EF.R'

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "N2O"

# Stop script if running for unsupported species
if ( em %!in% c( 'N2O' ) ) {
    stop ( paste( 'not supported for emission species', em ) )
}

# ------------------------------------------------------------------------------

# 1. Read in files for stationary EFs

#   A. Load US EPA N2O stationary emissions factors
     stationary_ef <- readData( "DEFAULT_EF_IN", domain_extension = "N2O/",
                                "N2O_base_EF-stationary-US_GHG2018", ".csv")

#   B. Load US EPA N2O electricity emissions
     elec_emissions <- readData( "DEFAULT_EF_IN", domain_extension = "N2O/",
                                 "N2O_emissions-elec-US_GHG2018", ".csv")

#   C. Load US EPA electricity fuel consumption
     elec_fc <- readData( "DEFAULT_EF_IN", domain_extension = "N2O/",
                          "N2O_fuel_consumption-elec-US_GHG2018", ".csv")

# 2. Read in files for mobile EFs

#   A. Load US EPA N2O mobile off-road emissions factors
     off_road_ef <- readData( "DEFAULT_EF_IN", domain_extension = "N2O/",
                              "N2O_base_EF-mobile_offroad-US_GHG2018", ".csv")

#   B. Load US EPA N2O mobile off-road fuel consumption
     or_consump <- readData( "DEFAULT_EF_IN", domain_extension = "N2O/",
                             "N2O_fuel_consumption-mobile_offroad-US_GHG2018", ".csv")

#   C. Load US EPA N2O emissions for on-road mobile (for gasoline and diesel vehicles)
     onr_emissions <- readData( "DEFAULT_EF_IN", domain_extension = "N2O/",
                                "N2O_emissions-mobile_onroad-US_GHG2018", ".csv",
                                skip_rows = 2)

#   D. Load US EPA on-road mobile Fuel Consumption (gasoline, natural gas, diesel)
     onr_consump <- readData( "DEFAULT_EF_IN", domain_extension = "N2O/",
                              "N2O_fuel_consumption-mobile_onroad-US_GHG2018",  ".csv")

#   E. Load US EPA on-road mobile VMT (natural gas)
     onr_VMT <- readData( "DEFAULT_EF_IN", domain_extension = "N2O/",
                          "N2O_VMT-mobile_onroad-US_GHG2018", ".csv")

#   F. Load US EPA N2O mobile on-road emissions factors (natural gas)
     onr_EF <- readData( "DEFAULT_EF_IN", domain_extension = "N2O/",
                         "N2O_base_EF-mobile_onroad-US_GHG2018", ".csv")

# 3. Read in mapping files

#   A. Load US GHG inventory sector map
     USGHG_sector_map <- readData( "MAPPINGS", "N2O_EF-USGHG2018_mapping",
                                   ".csv" )

#   B. Load US GHG inventory fuel map
     USGHG_fuel_map <- readData( "MAPPINGS", "USGHG-fuel_mapping", ".csv" )

#   C. Load the CEDS master country list
     MCL <- readData( "MAPPINGS", "Master_Country_List" )

# ------------------------------------------------------------------------------
# 2. Define script constants and helper functions

#   A.) Common years to retain (constants)
    USGHG_INVENTORY_YEARS_NO_X <- paste0(1990:2016)
    ALL_YEARS <- paste0("X", 1960:2014)

#   B.) Common columns to retain (constants)
    SEC_FUEL_UNITS_EF <- c("sector", "fuel", "units", "EF")
    SEC_FUEL_UNITS_YEARS <- c("sector", "fuel", "units", USGHG_INVENTORY_YEARS_NO_X)

#   C.) US GHG subsector and fuel lists (constants)
    OFF_ROAD_SUBSEC  <- c("Ships and Boats", "Rail", "Aircraft", "Agricultural Equipmenta",
                         "Construction/Mining Equipmentb", "Lawn and Garden Equipment",
                         "Airport Equipment", "Industrial/Commercial Equipment", "Logging Equipment",
                         "Railroad Equipment", "Recreational Equipment")

    OFF_ROAD_FUEL_SUBSEC <- c("Aircrafta", "Ships and Boats ", "Construction/Mining Equipmentc",
                               "Agricultural Equipmentd", "Rail", "Othere")

    natural_gas_onroad <- c("   CNG ICE", "   LPG ICE", "   LNG")

#   D.) Conversion factors (constants)
#       1.) Unit conversions
        BTU_PER_TBTU <- 1000000000000          # Btu/TBtu
        GJ_PER_BTU <- (1/947086.28903179)      # GJ/Btu
        TJ_PER_GJ <- (1/1000)                  # TJ/GJ
        KG_PER_KT <- 1000000                   # kg/kt
        KT_PER_GRAM <- 0.000000001             # kt/g
        KT_PER_MMT <- 1000                     # ktN2O/MMTN2O
        MJ_PER_GJ <- 1000                      # MJ/GJ
        MMTCO2eq_TO_MMTN2O <- 298              # GWP, MMTCO2eq / (MMTCO2eq/MMTN2O) = MMTCO2eq * (MMNTN2O/MMTCO2eq) (from US GHG report)

#       2.) Energy and weight conversions - from common_data.R unless otherwise stated

#           a.) Coal TJ per kt - assuming 50% brown_coal, and 50% hard_coal
            coal_conversion_factor_list <- c(conversionFactor_hard_coal_TJ_per_kt,
                                         conversionFactor_brown_coal_TJ_per_kt)

            COAL_TJ_PER_KT <- mean(coal_conversion_factor_list)

#           b.) Biomass TJ per kt
            BIOMASS_TJ_PER_KT <- conversionFactor_biomass_TJ_per_kt

#           c.) Diesel Oil TJ per kt
            DIESEL_OIL_TJ_PER_KT <- conversionFactor_diesel_oil_TJ_per_kt

#           d.) Heavy oil TJ per kt
            HEAVY_OIL_TJ_PER_KT <- conversionFactor_heavy_oil_TJ_per_kt

#           e.) Light oil TJ per kt
            LIGHT_OIL_TJ_PER_KT <- conversionFactor_light_oil_TJ_per_kt

#           f.) LPG TJ per kt
            LPG_TJ_PER_KT <- conversionFactor_liquefiedpetroleumgases_TJ_per_kt

#           g.) Natural Gas TJ per kt
            NATURAL_GAS_TJ_PER_KT <- conversionFactor_naturalgassubfuel_TJ_per_kt

#           h.) Wood TJ per kt
            WOOD_TJ_PER_KT <- conversionFactor_woodandwoodwaste_TJ_per_kt

#           i.) Gasoline MJ per gallon (LHV). Source: https://www.extension.iastate.edu/agdm/wholefarm/html/c6-87.html)
            LIGHT_OIL_MJ_PER_GALLON <- 121.7

#           j.) Diesel MJ per gallon (LHV). Source: https://www.extension.iastate.edu/agdm/wholefarm/html/c6-87.html)
            DIESEL_OIL_MJ_PER_GALLON <- 135.8

#           k.) LPG MJ per gallon (LHV). Source: https://www.extension.iastate.edu/agdm/wholefarm/html/c6-87.html)
            LPG_MJ_PER_GALLON <- 88.1

#   E.) Define two functions to replace column names for year columns
     replace_year_colnames <- function(df_in, column_num_replace_start, year_col_start,
                                       year_start, year_end){

       names <- as.character( colnames(df_in))
       names[ column_num_replace_start:( length( names )) ] <- year_start:year_end
       years_list <- paste( "X", names[ year_col_start:( length( names )) ], sep = "" ) # may want to make this a paramter higher up
       names[ year_col_start:( length( names )) ] <- years_list
       names( df_in ) <- names
       years_sorted <- sort(years_list)
       df_out <- df_in %>%
         dplyr::select(sector, fuel, units, years_sorted)

       return(df_out)
     }

     replace_year_colnames_no_x <- function(df_in, column_num_start, year_start, year_end){

       names <- as.character( colnames(df_in))
       names[ column_num_start:( length( names )) ] <- year_start:year_end
       names( df_in ) <- names
       df_out <- df_in

       return(df_out)
     }

# ------------------------------------------------------------------------------

# 3. Reformat Stationary Emissions Factors

#   A. Define relevant fuels from stationary data
    fuels <- c("Coal", "Petroleum", "Natural Gas", "Wood")

#   B.) Create maps for the industrial subsectors and for coal and petroleum fuels
     USGHG_sector_map_industrial <- USGHG_sector_map %>%
       dplyr::filter(sector == "Industrial") %>%
       dplyr::mutate(mn1 = c(3:15))

     USGHG_fuel_map_candp <- USGHG_fuel_map %>%
       dplyr::filter((fuel == "Coal")| (fuel == "Petroleum")) %>%
       dplyr::mutate(mn2 = c(1:3, 5:7))

#   C.) Initial cleaning / formatting for N2O stationary EFs
    colnames(stationary_ef) = stationary_ef[1, ]

    stationary_ef <- stationary_ef %>%
      dplyr::slice(-1) %>%
      dplyr::rename(sector = "Fuel/End-Use Sector") %>%
#     (Macs and PCs read one of the table comments differently)
      dplyr::filter(!(sector ==  "a GJ (Gigajoule) = 109 joules. One joule = 9.486\xd710-4 Btu." |
             sector ==  "a GJ (Gigajoule) = 109 joules. One joule = 9.486?10-4 Btu." |
             sector == "NA (Not Applicable)"|
             sector == "U.S. Territories")) %>%
      dplyr::select(-CH4) %>%

#   D.) Create a variable "fuel" and "units", filter for relevant fuels, rename EF variable
      dplyr::mutate(fuel = if_else(sector %in% fuels, sector, "NA" ),
                    N2O = as.numeric(N2O),
                    fuel = if_else(N2O == 1.5 , "Coal",
                           if_else(N2O == 0.6, "Petroleum",
                           if_else(N2O == 0.1, "Natural Gas",
                           if_else(N2O == 4.0, "Wood", "NA")))),
                    units = "g/GJ") %>%
      dplyr::filter(!(sector %in% fuels)) %>%
      dplyr::rename(EF = N2O) %>%
      dplyr::select(SEC_FUEL_UNITS_EF) %>%

#   E.) Convert emissions factors from g/GJ to kt/kt (to kt/GJ, then kt/TJ, then to kt/kt)
#       and redfine the units variable
      dplyr::mutate(EF = if_else(fuel == "Coal",
                                 EF*KT_PER_GRAM/TJ_PER_GJ*COAL_TJ_PER_KT,
                         if_else(fuel == "Natural Gas",
                                 EF*KT_PER_GRAM/TJ_PER_GJ*NATURAL_GAS_TJ_PER_KT,
                         if_else(fuel == "Wood",
                                 EF*KT_PER_GRAM/TJ_PER_GJ*WOOD_TJ_PER_KT,
                         if_else(fuel == "Petroleum",
                                 EF*KT_PER_GRAM/TJ_PER_GJ*HEAVY_OIL_TJ_PER_KT,
                                 EF) ) ) ),
                    units = "kt/kt")

#   F.) Map sectors to CEDS working_sectors_2

#       I.) Create duplicate lines for the industrial sector
        stationary_ef <-  rbind(stationary_ef,
                stationary_ef %>% dplyr::filter(sector == "Industrial"),
                stationary_ef %>% dplyr::filter(sector == "Industrial"),
                stationary_ef %>% dplyr::filter(sector == "Industrial"),
                stationary_ef %>% dplyr::filter(sector == "Industrial"),
                stationary_ef %>% dplyr::filter(sector == "Industrial"),
                stationary_ef %>% dplyr::filter(sector == "Industrial"),
                stationary_ef %>% dplyr::filter(sector == "Industrial"),
                stationary_ef %>% dplyr::filter(sector == "Industrial"),
                stationary_ef %>% dplyr::filter(sector == "Industrial"),
                stationary_ef %>% dplyr::filter(sector == "Industrial"),
                stationary_ef %>% dplyr::filter(sector == "Industrial"),
                stationary_ef %>% dplyr::filter(sector == "Industrial"))

#       II.) Create a variable to match the industrial subsectors
        stationary_ef <- stationary_ef %>%
          dplyr::arrange(fuel) %>%
          dplyr::mutate(mn1 = c(1:15, 1:15, 1:15, 1:15)) %>%
          dplyr::mutate(sector = if_else (sector == "Industrial", "NA", sector)) %>%

#       III.) Map sectors to CEDS working_sector_v2 for all sectors besides industrial
          dplyr::left_join(USGHG_sector_map, by = "sector") %>%
          dplyr::mutate(sector = working_sectors_v2) %>%
          dplyr::select(SEC_FUEL_UNITS_EF, mn1) %>%

#       IV.) Map over the industrial sector's subsectors
          dplyr::left_join(USGHG_sector_map_industrial, by = "mn1") %>%
          dplyr::mutate(sector.x = if_else(is.na(sector.x), working_sectors_v2, sector.x)) %>%
          dplyr::rename(sector = sector.x) %>%
          dplyr::select(SEC_FUEL_UNITS_EF)

#   H.) Map sectors to CEDS fuels

#       I.) Create duplicate lines for coal and petroleum, as need to map multiple types of
#            coal and petroleum
        stationary_ef <- rbind(stationary_ef,
                               stationary_ef %>% filter((fuel == "Coal") | (fuel == "Petroleum")),
                               stationary_ef %>% filter((fuel == "Coal") | (fuel == "Petroleum"))) %>%

#       II.) Create a variable to match different types of coal and petroleum
            dplyr::arrange(sector, fuel) %>%
            dplyr::mutate(mn2 = c(1:8, 1:8, 1:8, 1:8, 1:8, 1:8, 1:8, 1:8, 1:8, 1:8, 1:8,
                                  1:8, 1:8, 1:8, 1:8)) %>%
            dplyr::mutate(fuel = if_else(fuel == "Coal" | fuel == "Petroleum", "NA", fuel )) %>%

#       III.) Map fuels to CEDS fuels for wood and natural gas
            dplyr::left_join(USGHG_fuel_map, by = "fuel") %>%
            dplyr::mutate(fuel = ceds_fuel) %>% dplyr::select(-ceds_fuel) %>%

#       IV.) Map over coal and petroleum fuels
            dplyr::left_join(USGHG_fuel_map_candp, by = "mn2") %>%
            dplyr::mutate(mn2 = as.character(mn2)) %>%
            dplyr::mutate(fuel.x = if_else(is.na(fuel.x), ceds_fuel, fuel.x)) %>%
            dplyr::rename(fuel = fuel.x) %>%
            dplyr:: select(SEC_FUEL_UNITS_EF) %>%
            dplyr::arrange(sector, fuel)

# ------------------------------------------------------------------------------

# 4. Generate electricity N2O EFs

#   A. Reformat electricity emissions data
        colnames(elec_emissions) = elec_emissions[2, ]

        elec_emissions_clean <- elec_emissions %>%

#     I. Initial cleaning / reformatting, convert emissions denoted as "+" to "NA"
          dplyr::select(-1) %>%
          dplyr::slice(3:7) %>%
          dplyr::rename(fuel = "Sector/Fuel Type") %>%
          tidyr::gather(key = years, value = elec_emissions, USGHG_INVENTORY_YEARS_NO_X) %>%
          dplyr::mutate(elec_emissions = if_else(elec_emissions == "+ ", "", elec_emissions),
                        elec_emissions = as.numeric(elec_emissions)) %>%
          dplyr::filter(fuel != "Electric Power") %>%

#     II. Convert to kt N2O from MMTCO2eq
          dplyr::mutate(Emissions_MMTN2O = elec_emissions/MMTCO2eq_TO_MMTN2O,
                        Emissions_ktN2O = Emissions_MMTN2O*KT_PER_MMT,
                        units = "kt") %>%
          dplyr::select(-elec_emissions, -Emissions_MMTN2O) %>%

#     III. Map to CEDS fuels
          dplyr::left_join(USGHG_fuel_map, by = "fuel") %>%
          dplyr::select(ceds_fuel, years, units, Emissions_ktN2O) %>%
          dplyr::rename(fuel = ceds_fuel)

#   B. Reformat electricity fuel consumption data

#     I. Initial cleaning / reformatting
        colnames(elec_fc) = elec_fc[1, ]

        elec_fc_clean <- elec_fc %>%
          dplyr::slice(2:25) %>%
          dplyr::rename(fuel = "Fuel/End-Use Sector") %>%
          dplyr::filter(!(fuel %in% c("Residential", "Commercial", "Industrial",
                                      "U.S. Territories"))) %>%
          dplyr::mutate(fuel = c("Coal_Total", "Coal",
                                 "Petroleum_Total", "Fuel Oil",
                                 "Natural_Gas_Total", "Natural Gas",
                                 "Wood_Total", "Wood")) %>%
          dplyr::filter(fuel %in% c("Coal", "Fuel Oil", "Natural Gas", "Wood")) %>%
          tidyr::gather(key = years, value = fuel_consumption, USGHG_INVENTORY_YEARS_NO_X) %>%
          dplyr::mutate(fuel_consumption = gsub(",", "", fuel_consumption),
                        fuel_consumption = as.numeric(fuel_consumption)) %>%

#     II. Map to CEDS fuels
          dplyr::left_join(USGHG_fuel_map, by = "fuel") %>%
          dplyr::select(-fuel) %>%
          dplyr::rename(fuel = ceds_fuel) %>%

#     V. Convert fuel from TBtu to kt
          dplyr::mutate(fuel_consumption_GJ_or_MJ = fuel_consumption*BTU_PER_TBTU*GJ_PER_BTU,
                        fuel_consumption_GJ_or_MJ = if_else(fuel == "natural_gas",
                                                    fuel_consumption_GJ_or_MJ*MJ_PER_GJ,
                                                    fuel_consumption_GJ_or_MJ),
                        units = if_else(fuel != "natural_gas", "GJ", "MJ"),
                        fuel_consumption_kt = if_else( fuel %in% c("brown_coal", "hard_coal", "coal_coke"),
                                              fuel_consumption_GJ_or_MJ*TJ_PER_GJ/COAL_TJ_PER_KT,
                                              if_else( fuel == "biomass",
                                              fuel_consumption_GJ_or_MJ*TJ_PER_GJ/BIOMASS_TJ_PER_KT,
                                              if_else( fuel == "heavy_oil",
                                              fuel_consumption_GJ_or_MJ*TJ_PER_GJ/HEAVY_OIL_TJ_PER_KT,
                                              if_else( fuel == "natural_gas",
                                              fuel_consumption_GJ_or_MJ/MJ_PER_GJ*TJ_PER_GJ/NATURAL_GAS_TJ_PER_KT,
                                              fuel_consumption_GJ_or_MJ) ) ) ),
                        units = "kt" ) %>%
          dplyr::select(fuel, years, units, fuel_consumption_kt)

#   C. Generate electricity EFs (emissions kt/ fuel kt)

#     I. Generate EFs
        elec_EFs <- dplyr::left_join(elec_emissions_clean, elec_fc_clean, by = c("fuel", "years")) %>%
          dplyr::mutate(EF = Emissions_ktN2O/fuel_consumption_kt,
                        units = "kt/kt") %>%
          dplyr::select(fuel, units, years, EF) %>%

#     II. Define sectors (public elec and autoproducers of elec)
          dplyr::mutate(sector = "1A1a_Electricity-public",
                        count = 2) %>%
          dplyr::group_by(fuel, units, years, EF, sector) %>%
          expand(count = seq(1:count)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(sector = if_else(count == 2, "1A1a_Electricity-autoproducer",
                                         sector)) %>%
          dplyr::select(-count)

#     III. Assign stationary biomass EF to electricity biomass EF (all subsectors of the industrial
#          sector biomass EF are the same, so any of them can be used for this assignment)
        biomass_ef_add <- stationary_ef[[1, "EF"]]

        elec_EFs_final <- elec_EFs %>%
          dplyr::mutate(EF = if_else(fuel == "biomass", biomass_ef_add, EF ),

#     IV. Extend heavy_oil 2007 EF forward to 2016, and apply these EFs to light_oil and diesel_oil
                        count = if_else(fuel == "heavy_oil" & years == "2007", 10, 1)) %>%
          dplyr::group_by(fuel, units, years, EF, sector) %>%
          expand(count = seq(1:count)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(years = if_else(count == 2, "2008", if_else(count == 3, "2009",
                                if_else(count == 4, "2010", if_else(count == 5, "2011",
                                if_else(count == 6, "2012", if_else(count == 7, "2013",
                                if_else(count == 8, "2014", if_else(count == 9, "2015",
                                if_else(count == 10, "2016", years) ) ) ) ) ) ) ) ) ) %>%
          dplyr::filter(!is.na(EF)) %>%
          dplyr::mutate(count = if_else(fuel == "heavy_oil", 3, 1)) %>%
          dplyr::group_by(fuel, units, years, EF, sector) %>%
          expand(count = seq(1:count)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(fuel = if_else(count == 2, "light_oil",
                               if_else(count == 3, "diesel_oil", fuel))) %>%
          dplyr::select(sector, fuel, units, years, EF )


#     D.) Define heat production EFs --> assign 1A1a_Electricity-autoproducer to 1A1a_Heat-production
        heat_production_efs <- elec_EFs_final %>%
            dplyr::filter( sector == "1A1a_Electricity-autoproducer") %>%
            dplyr::mutate( sector = "1A1a_Heat-production")

# ------------------------------------------------------------------------------

# 4. Add on-road mobile

#   A.) Generate EFs for diesel and gasoline vehicles

#       I.) Reformat on-road emissions data
        colnames( onr_emissions ) <- as.character( unlist( onr_emissions[ 2, ] ) )

#           a.) Initial cleaning
            onr_emissions_clean <- onr_emissions %>%
             dplyr::rename(fuel = "Fuel Type/Vehicle Typea") %>%
             dplyr::slice(2:19) %>%
             dplyr::filter(fuel == "Gasoline On-Roadb" | fuel == "Diesel On-Roadb") %>%

#           b.) Map to ceds fuels and define sector as 1A3b_Road
             dplyr::left_join(USGHG_fuel_map, by = "fuel") %>%
             dplyr::mutate(fuel = ceds_fuel,
                          sector = "1A3b_Road") %>%
             dplyr::select(sector, fuel,USGHG_INVENTORY_YEARS_NO_X) %>%

#           c.) Convert to kt of N2O from MMTCO2eq
             tidyr::gather(key = years, value = Emissions_MMTCO2eq, USGHG_INVENTORY_YEARS_NO_X) %>%
             dplyr::mutate(Emissions_MMTCO2eq = as.numeric(Emissions_MMTCO2eq)) %>%
             dplyr::mutate(Emissions_ktN2O = Emissions_MMTCO2eq/MMTCO2eq_TO_MMTN2O*KT_PER_MMT) %>%
             dplyr::select(-Emissions_MMTCO2eq) %>%
             dplyr::mutate(units = "kt")

#       II.) Reformat on-road fuel consumption data

#           a.) Initial cleaning
            colnames(onr_consump) <- as.character(unlist(onr_consump[1,]))


            onr_consump_clean <- onr_consump %>%
             dplyr::rename( Vehicle_Type = "Fuel/Vehicle Type ",
                           "2007" = "2007a") %>%
             dplyr::slice(2:37) %>%
             dplyr::mutate(fuel = Vehicle_Type) %>%
             dplyr::select(fuel, Vehicle_Type, USGHG_INVENTORY_YEARS_NO_X)

#           b.) Map over to CEDS fuels
            onr_consump_clean <- onr_consump_clean %>%
             dplyr::slice(-(24:29)) %>%
             dplyr::left_join(USGHG_fuel_map, by = "fuel") %>%
             dplyr::mutate(fuel = ceds_fuel) %>%
             dplyr::select(fuel, Vehicle_Type, USGHG_INVENTORY_YEARS_NO_X)

            onr_consump_clean[2:7,1] <- onr_consump_clean[1,1]

            onr_consump_clean[9:15,1] <- onr_consump_clean[8,1]

            onr_consump_clean <- onr_consump_clean %>%
             dplyr::filter(!(is.na(fuel))) %>%

#           c.) Aggregate by fuel
             dplyr::filter(!(Vehicle_Type %in% c("Motor Gasolineb,c", "  Recreational Boatsd",
                                                "Distillate Fuel Oil (Diesel Fuel) b,c",
                                                "  Recreational Boats", "  Ships and Non-Recreational Boats",
                                                "  Raile"))) %>%
             dplyr::select(-Vehicle_Type) %>%
             tidyr::gather(key = years, value = fuel_consumption, USGHG_INVENTORY_YEARS_NO_X) %>%
             dplyr::mutate(fuel_consumption = gsub(",", "", fuel_consumption ),
                          fuel_consumption = as.numeric(fuel_consumption)) %>%
             dplyr::group_by(fuel, years) %>%
             dplyr::summarise_all(sum) %>%
             dplyr::mutate(units = "million gallons") %>%

#           d.) Convert millions of gallons to kt (Milligon gallons -> gallons --> MJ -->  GJ -->  TJ --> kt)
             dplyr::mutate( fuel_consumption = fuel_consumption*1000000,
                            fuel_consumption = if_else( fuel == "light_oil",
                                                        fuel_consumption*LIGHT_OIL_MJ_PER_GALLON/MJ_PER_GJ*TJ_PER_GJ/LIGHT_OIL_TJ_PER_KT,
                                               if_else( fuel == "diesel_oil",
                                                        fuel_consumption*DIESEL_OIL_MJ_PER_GALLON/MJ_PER_GJ*TJ_PER_GJ/DIESEL_OIL_TJ_PER_KT,
                                               if_else( fuel == "natural_gas",
                                                        fuel_consumption*LPG_MJ_PER_GALLON/MJ_PER_GJ*TJ_PER_GJ/LPG_TJ_PER_KT,
                                                        fuel_consumption) ) ),
                           units = "kt")

#       III.) Generate EFs for on-road light_oil and diesel_oil
        onr_gas_diesel_efs <- onr_emissions_clean %>%
            dplyr::left_join(onr_consump_clean, by = c("fuel", "years")) %>%
            dplyr::group_by(sector, fuel, years, units.x, units.y) %>%
            dplyr::mutate(EF = Emissions_ktN2O/fuel_consumption,
                          units = "kt/kt") %>%
            dplyr::ungroup(onr_emissions_clean) %>%
            dplyr::select(sector, fuel, units, years, EF)

#   B.) Generate EFs for natural_gas vehicles

#       I.) Reformat disaggregated natural gas vehicle EFs
        colnames(onr_EF) <- c("fuel", USGHG_INVENTORY_YEARS_NO_X)

        onr_EF <- onr_EF %>%
            dplyr::slice(2:39) %>%
            dplyr::mutate(Vehicle_Type = fuel) %>%
            dplyr::select(Vehicle_Type, fuel, USGHG_INVENTORY_YEARS_NO_X)

        onr_EF[2:8,1] <- onr_EF[1,1]
        onr_EF[10:16,1] <- onr_EF[9,1]
        onr_EF[18:23,1] <- onr_EF[17,1]
        onr_EF[25:31,1] <- onr_EF[24,1]
        onr_EF[33:38,1] <- onr_EF[32,1]

        onr_EF_clean <- onr_EF %>%
            dplyr::filter(Vehicle_Type != fuel,
                          fuel %in% natural_gas_onroad) %>%
            tidyr::gather(key = years, value = EF, USGHG_INVENTORY_YEARS_NO_X) %>%
            dplyr::mutate(EF_Units = "g/mi",
                          Vehicle_Type = if_else(Vehicle_Type == "Light-Duty Cars ",
                                                 "Light-Duty Cars",
                                         if_else(Vehicle_Type == "Heavy-Duty Trucks ",
                                                 "Heavy-Duty Trucks",
                                          if_else(Vehicle_Type == "Buses ", "Buses",
                                                  Vehicle_Type) ) ) )

#       II.) Reformat disaggregated natural gas vehicle VMTs
        colnames(onr_VMT) <- c("fuel", USGHG_INVENTORY_YEARS_NO_X)

        onr_VMT <- onr_VMT %>%
            dplyr::slice(2:47) %>%
            dplyr::mutate(Vehicle_Type = fuel) %>%
            dplyr::select(Vehicle_Type, fuel, USGHG_INVENTORY_YEARS_NO_X)

        onr_VMT[2:12,1] <- onr_VMT[1,1]
        onr_VMT[14:23,1] <- onr_VMT[13,1]
        onr_VMT[25:30,1] <- onr_VMT[24,1]
        onr_VMT[32:37,1] <- onr_VMT[31,1]
        onr_VMT[39:46,1] <- onr_VMT[38,1]

        onr_VMT_clean <- onr_VMT %>%
            dplyr::filter(Vehicle_Type != fuel,
                          fuel %in% natural_gas_onroad) %>%
            tidyr::gather(key = years, value = VMT, USGHG_INVENTORY_YEARS_NO_X) %>%
            dplyr::mutate(VMT = if_else(VMT == "+ ", "", VMT),
                          VMT = as.numeric(VMT),
                          VMT = 1000000*VMT,
                          VMT_units = "miles")

#       III.) Calculate disaggregated natural gas vehicle emissions
        onr_ng_emissions <- onr_EF_clean %>%
            dplyr::left_join(onr_VMT_clean, by = c("Vehicle_Type", "fuel", "years")) %>%
            dplyr::group_by(Vehicle_Type, fuel, years, EF_Units, VMT_units) %>%
            dplyr::mutate(Emissions = EF*VMT,
                          units = "g") %>%
            dplyr::ungroup(onr_EF_clean) %>%
            dplyr::select(fuel, years, Emissions, units) %>%
            dplyr::mutate(Emissions = Emissions*KT_PER_GRAM,
                          units = "kt",
                          fuel = "natural_gas") %>%
            dplyr::group_by(fuel, years, units) %>%
            dplyr::summarise_all(sum, na.rm=TRUE) %>%
            dplyr::ungroup(onr_EF_clean)

#       IV.) Generate natural gas vehicle EFs
        onr_ng_EFs_final <- onr_ng_emissions %>%
            dplyr::left_join(onr_consump_clean, by = c("fuel", "years")) %>%
            dplyr::mutate(EF = Emissions/fuel_consumption,
                          units = "kt/kt",
                          sector = "1A3b_Road") %>%
            dplyr::select(sector, fuel, units, years, EF)

#   C.) Combine mobile on-road dataframes into 1 dataframe
        on_road_final_EFs <- rbind(onr_ng_EFs_final, onr_gas_diesel_efs)

#   D.) Define missing road EFs --> assign on-road diesel_oil EFs to heavy_oil
#       other missing fuels to zero
        missing_road_set_diesel_oil <- on_road_final_EFs %>%
            filter( fuel == "diesel_oil") %>%
            mutate( fuel = "heavy_oil")

        missing_road_set_zero_1 <-rbind(
            missing_road_set_diesel_oil %>%
                mutate( fuel = "biomass") %>%
                mutate( EF = 0),
            missing_road_set_diesel_oil %>%
                mutate( fuel = "coal_coke") %>%
                mutate( EF = 0) )

        missing_road_set_zero_2 <- rbind(
            missing_road_set_zero_1,
            missing_road_set_zero_1 %>%
                filter (fuel == "biomass") %>% mutate( fuel = "brown_coal") %>%
                rbind(missing_road_set_zero_1 %>% filter (fuel == "biomass") %>%
                          mutate( fuel = "hard_coal")))

        missing_road <- rbind(missing_road_set_diesel_oil, missing_road_set_zero_2)

        on_road_final_EFs <- rbind(on_road_final_EFs, missing_road)
# ------------------------------------------------------------------------------

# 5. Reformat mobile off-road emissions factor data

#   A.) Initial cleaning / formatting
    off_road_ef <- off_road_ef %>%
      dplyr::rename(sector = Table.A.112...Emission.Factors.for.N2O.Emissions.from.Non.Road.Mobile.Combustion..g.kg.fuel.) %>%
      dplyr::mutate(sector = if_else(is.na(sector) , "sector", sector))

    colnames(off_road_ef) = off_road_ef[1, ]

    off_road_ef <- off_road_ef %>%
      dplyr::slice(-1) %>%
      dplyr::mutate(units = "g/kg",
                    fuel = sector,
                    fuel = if_else(fuel %in% OFF_ROAD_SUBSEC, "NA", fuel),
                    sector = if_else(sector %in% OFF_ROAD_SUBSEC, sector, "NA")) %>%
      dplyr::select(sector, fuel, units, USGHG_INVENTORY_YEARS_NO_X)

#   B.) Fix fuel and sector variables
    off_road_ef[2:6, 1] <- off_road_ef[1, 1]
    off_road_ef[8, 1] <- off_road_ef[7, 1]
    off_road_ef[10:11, 1] <- off_road_ef[9, 1]
    off_road_ef[13:20, 1] <- off_road_ef[12, 1]
    off_road_ef[22:29, 1] <- off_road_ef[21, 1]
    off_road_ef[31:39, 1] <- off_road_ef[30, 1]
    off_road_ef[41:44, 1] <- off_road_ef[40, 1]
    off_road_ef[46:51, 1] <- off_road_ef[45, 1]
    off_road_ef[53:56, 1] <- off_road_ef[52, 1]
    off_road_ef[58:61, 1] <- off_road_ef[57, 1]
    off_road_ef[63:67, 1] <- off_road_ef[62, 1]

    off_road_ef <- off_road_ef %>%
      dplyr::filter(!(sector == "NA" | fuel == "NA")) %>%

#   C.) Convert emissions factors from g/kg fuel to kt/kt
      tidyr::gather(key = years, value = EF, USGHG_INVENTORY_YEARS_NO_X) %>%
      dplyr::mutate(EF = EF*KT_PER_GRAM*KG_PER_KT,
                    units = "kt/kt") %>%

#   D.) Map sectors to CEDS working_sectors_2
      dplyr::left_join(USGHG_sector_map, by = "sector") %>%
      dplyr::mutate(sector = working_sectors_v2) %>%
      dplyr::filter(!(is.na(sector) | is.na(EF))) %>%

#   E.) Map fuels to CEDS fuels
      dplyr::mutate(helper = fuel) %>%
      dplyr::select(sector, fuel, helper, units, years, EF) %>%
      dplyr::left_join(USGHG_fuel_map, by = "fuel") %>%
      dplyr::mutate(fuel = ceds_fuel) %>%
      dplyr::select(-ceds_fuel)

#   F.) Calculate mean EFs for sector & fuel combinations which have 2 or more EFs (because two or more
#       fuels were mapped to 1 CEDS fuel, but were not broken up in fuel consumption data -
#       e.g. 2 and 4 strokes)
    or_EF_mean <- tidyr::spread( off_road_ef, years, EF) %>%
                  dplyr::select( -helper) %>%
                  dplyr::group_by( sector, fuel, units) %>%
                  dplyr::summarise_all( funs( mean) ) %>%
                  tidyr::gather( key = years, value = mean_EF, USGHG_INVENTORY_YEARS_NO_X)

    off_road_ef <- dplyr::left_join( off_road_ef, or_EF_mean,
                                    by = c( "sector", "fuel", "units" ,"years" ) ) %>%
                   dplyr::mutate( new_ef = if_else( sector == "1A2g_Ind-Comb-Construction" &
                                            fuel %in% c( "light_oil", "diesel_oil" ), mean_EF,
                                           if_else(sector == "1A3di_International-shipping" &
                                            fuel %in% c( "light_oil" ), mean_EF, EF ) ),
                                  helper = if_else(helper %in% c( "CNG", "LPG", "Jet Fuel",
                                            "Aviation Gasoline" ), helper, fuel ) ) %>%
                   dplyr::select(-EF , -mean_EF ) %>%
                   dplyr::rename(EF = new_ef ) %>%
                   dplyr::distinct()

#   G.) Generate weights by fuel consumption for certain sectors which have multiple fuels mapped to  1
#       CEDS_fuel (but where mean EFs were not calculated)

#     I. Initial cleaning / reformatting of off-road fuel consumption data

      colnames(or_consump) = or_consump[1, ]

      or_consump <- or_consump %>%
        dplyr::slice(-1) %>%
        dplyr::rename(sector = "Vehicle Type/Year") %>%
        dplyr::mutate(fuel = sector,
                    fuel = if_else(fuel %in% OFF_ROAD_FUEL_SUBSEC, "NA", fuel),
                    sector = if_else(sector %in% OFF_ROAD_FUEL_SUBSEC, sector, "NA" ) ) %>%
        dplyr::select(sector, fuel, USGHG_INVENTORY_YEARS_NO_X)

      or_consump[2:4, 1] <- or_consump[1, 1]
      or_consump[6:8, 1] <- or_consump[5, 1]
      or_consump[10:13, 1] <- or_consump[9, 1]
      or_consump[15:18, 1] <- or_consump[14, 1]
      or_consump[20, 1] <- or_consump[19, 1]
      or_consump[21, 1] <- "NA"

      or_consump <- or_consump %>%
        dplyr::filter( !( sector == "NA" | fuel == "NA" ) ) %>%
        dplyr::mutate(units = "million gallons") %>%
        dplyr::select( SEC_FUEL_UNITS_YEARS) %>%
        tidyr::gather(key = years, value = consumption, USGHG_INVENTORY_YEARS_NO_X) %>%
        dplyr::mutate(consumption = gsub(",","", consumption, fixed = TRUE),
                      consumption = as.numeric(consumption) ) %>%
        tidyr::spread(years, consumption) %>%

#       II.) Map sectors to CEDS working_sectors_2
        dplyr::left_join( USGHG_sector_map, by = "sector" ) %>%
        dplyr::mutate( sector = working_sectors_v2 ) %>%
        dplyr::select( SEC_FUEL_UNITS_YEARS ) %>%
        dplyr::filter(! ( is.na( sector ) ) ) %>%

#       III.) Map fuels to CEDS fuels
        dplyr::mutate(helper = if_else(fuel == "  Diesel", "Diesel",
                                if_else(fuel == "  Gasoline", "Gasoline",
                                if_else(fuel == "  Jet Fuel", "Jet Fuel",
                                if_else(fuel == "  Aviation Gasoline", "Aviation Gasoline",
                                if_else(fuel == "  Residual", "Residual", fuel ) ) ) ) ) ) %>%
        dplyr::select(SEC_FUEL_UNITS_YEARS, helper) %>%
        dplyr::left_join(USGHG_fuel_map, by = "fuel") %>%
        dplyr::mutate(fuel = ceds_fuel,
                      helper = if_else (helper == "Aviation Gasoline" | helper == "Jet Fuel" ,
                                        helper, fuel)) %>%
        dplyr::select( -ceds_fuel ) %>%
        dplyr::filter( !( is.na( fuel ) ) )

#       IV.) Calculate the percentage of fuel used for each sector, fuel,
#             year combo to provide weights for EFs
        agg_offroad_con <- or_consump %>%
          dplyr::select(-helper) %>%
          dplyr::group_by(sector, fuel, units) %>%
          dplyr::summarise_all(funs(sum))

        # agg_offroad_con <- rbind(agg_offroad_con,
        #                          agg_offroad_con %>% dplyr::filter(sector ==
        #                               "1A3ai_International-aviation" & fuel == "light_oil")) %>%
        agg_offroad_con <- agg_offroad_con %>%
          dplyr::arrange(sector, fuel) %>%
          tidyr::gather(key = years, value = agg_consumption, USGHG_INVENTORY_YEARS_NO_X)

        offroad_fuel_weights <- or_consump %>%
          dplyr::arrange( sector, fuel ) %>%
          tidyr::gather( key = years, value = consumption, USGHG_INVENTORY_YEARS_NO_X ) %>%
          dplyr::left_join( agg_offroad_con, by = c( "sector", "fuel", "units", "years" ) ) %>%
          dplyr::group_by(sector, fuel, units, helper, years) %>%
          dplyr::mutate( weights = consumption / agg_consumption) %>%
          dplyr::ungroup() %>%
          dplyr::select(-consumption, -agg_consumption, -units)

#   H.) Derive emissions factors using weights from fuel consumption data
    off_road_ef <- off_road_ef %>%
      dplyr::left_join(offroad_fuel_weights, by = c("sector", "fuel", "helper",
                      "years")) %>%
      dplyr::group_by(sector, fuel, helper, units, years) %>%
      dplyr::mutate(weighted_EF = EF*weights) %>%
      dplyr::ungroup() %>%
      dplyr::select(-EF, -weights, -helper) %>%
      dplyr::rename(EF = weighted_EF) %>%
      dplyr::group_by(sector, fuel, units, years) %>%
      dplyr::summarise_all(funs(sum)) %>%
      dplyr::ungroup()

#   I.) Duplicate off-road international aviation EFS for domestic aviation and
#       int. Shipping EFS for domestic navigation
      off_road_ef <- rbind(off_road_ef,
                           off_road_ef %>%
                            dplyr::filter(sector == "1A3ai_International-aviation") %>%
                            dplyr::mutate(sector = "1A3aii_Domestic-aviation"))

      off_road_ef <- rbind(off_road_ef,
                           off_road_ef %>%
                            filter(sector == "1A3di_International-shipping") %>%
                            mutate(sector = "1A3dii_Domestic-navigation"))

    off_road_ef_df <- as.data.frame(off_road_ef)

#   J.)  Define missing aviation EFs -->
#        Set heavy_oil and diesel_oil to light_oil aviation EFs and set other fuels to 0
    missing_aviation_set_light_oil <-
        rbind(off_road_ef_df %>%
                  dplyr::filter(sector %in% c("1A3aii_Domestic-aviation",
                                              "1A3ai_International-aviation" )) %>%
                  dplyr::mutate(fuel = "diesel_oil"),
              off_road_ef_df %>%
                  dplyr::filter(sector %in% c("1A3aii_Domestic-aviation",
                                              "1A3ai_International-aviation" )) %>%
                  dplyr::mutate(fuel = "heavy_oil"))

    missing_aviation_set_zero <-
        rbind(heat_production_efs %>%
                  dplyr::filter(fuel %in% c("biomass", "brown_coal", "coal_coke", "hard_coal",
                                            "natural_gas")) %>%
                  dplyr::mutate(sector = "1A3aii_Domestic-aviation") %>%
                  dplyr::mutate(EF = 0),
              heat_production_efs %>%
                  dplyr::filter(fuel %in% c("biomass", "brown_coal", "coal_coke", "hard_coal",
                                            "natural_gas")) %>%
                  dplyr::mutate(sector = "1A3ai_International-aviation") %>%
                  dplyr::mutate(EF = 0)
        )

    missing_aviation <- rbind(missing_aviation_set_zero, missing_aviation_set_light_oil)

#   K.)  Define missing navigation/shipping EFs -->
#         Assign coal EFs to the value of stationary coal EFs (which are all the same),
#         and natural gas to the value of on-road natural gas
    missing_boat_coal <- stationary_ef %>%
        dplyr::filter( sector %in% c("1A4a_Commercial-institutional", "1A4b_Residential"),
                       fuel %in% c("brown_coal", "hard_coal", "coal_coke")) %>%
        dplyr::mutate( sector = if_else(sector == "1A4a_Commercial-institutional",
                       "1A3dii_Domestic-navigation", "1A3di_International-shipping") )

    missing_boat_coal <- cbind(missing_boat_coal, replicate(26, missing_boat_coal$EF ) )

    missing_boat_coal <- replace_year_colnames_no_x(missing_boat_coal, 4, 1990, 2016)

    missing_boat_coal <- tidyr::gather(missing_boat_coal, key = years, value = EF, USGHG_INVENTORY_YEARS_NO_X)

    missing_boat_nat_gas <- rbind(
        on_road_final_EFs %>%
            dplyr::filter( fuel == "natural_gas") %>%
            dplyr::mutate( sector = "1A3dii_Domestic-navigation"),
        on_road_final_EFs %>%
            dplyr::filter( fuel == "natural_gas") %>%
            dplyr::mutate( sector = "1A3di_International-shipping") )

    missing_boat <- rbind(missing_boat_coal, missing_boat_nat_gas)

#   L.)  Define missing navigation/shipping EFs and missing transportation biomass EFs  -->
#        Assign coal EFs to the value of stationary coal EFs (which are all the same),
#        and natural gas and oils to the values of on-road natural gas and oil EFs

    missing_other_transp_coal <- stationary_ef %>%
        dplyr::filter( sector == "1A4a_Commercial-institutional",
                fuel %in% c("brown_coal", "hard_coal", "coal_coke")) %>%
        dplyr::mutate( sector = "1A3eii_Other-transp")

    missing_other_transp_coal <- cbind(missing_other_transp_coal, replicate(26, missing_other_transp_coal$EF ) )

    missing_other_transp_coal <- replace_year_colnames_no_x(missing_other_transp_coal, 4, 1990, 2016)

    missing_other_transp_coal <- tidyr::gather(missing_other_transp_coal, key = years, value = EF, USGHG_INVENTORY_YEARS_NO_X)

    missing_other_transp_ng_oil <- on_road_final_EFs %>%
       dplyr::mutate(sector = "1A3eii_Other-transp") %>%
       dplyr::filter(fuel %in% c("natural_gas", "heavy_oil", "light_oil", "diesel_oil"))

    missing_other_transp <- rbind(missing_other_transp_coal, missing_other_transp_ng_oil)

#   M.) Define missing biomass EFs -->
#       Assign  to stationary_EF biomass values  via electricity public
#       (which has been given the EF value of stationary_EF biomass)

    missing_biomass_off_road <- rbind(
      elec_EFs_final %>%
        dplyr::filter( sector == "1A1a_Electricity-public", fuel == "biomass") %>%
        dplyr::mutate( sector = "1A3c_Rail" ) %>%
        rbind( elec_EFs_final %>%
                 dplyr::filter( sector == "1A1a_Electricity-public", fuel == "biomass") %>%
                 dplyr::mutate( sector = "1A3dii_Domestic-navigation" )),
      elec_EFs_final %>%
        dplyr::filter( sector == "1A1a_Electricity-public", fuel == "biomass") %>%
        dplyr::mutate( sector = "1A3di_International-shipping" ) %>%
        rbind( elec_EFs_final %>%
                 dplyr::filter( sector == "1A1a_Electricity-public", fuel == "biomass") %>%
                 dplyr::mutate( sector = "1A3eii_Other-transp" )))

    missing_biomass_off_road <- rbind(missing_biomass_off_road,
      elec_EFs_final %>%
        dplyr::filter( sector == "1A1a_Electricity-public", fuel == "biomass") %>%
        dplyr::mutate( sector = "1A4c_Agriculture-forestry-fishing" ) %>%
        rbind( elec_EFs_final %>%
               dplyr::filter( sector == "1A1a_Electricity-public", fuel == "biomass") %>%
               dplyr::mutate( sector = "1A5_Other-unspecified" ) ) )

#   N.) Define missing rail EFs -->
#       Assign heavy_oil and light_oil rail to diesel oil rail EF, coal EFs to the value of
#       stationary coal EFs, and natural gas to the value off-road natural gas
    missing_rail_oil <- rbind(
      off_road_ef_df %>%
        dplyr::filter(sector == "1A3c_Rail") %>%
        dplyr::mutate( fuel = "heavy_oil"),
      off_road_ef_df %>%
        dplyr::filter(sector == "1A3c_Rail") %>%
        dplyr::mutate( fuel = "light_oil"))

    missing_rail <- rbind(
      missing_rail_oil,
      missing_other_transp %>%
        dplyr::filter( fuel %in% c("brown_coal", "hard_coal", "coal_coke", "natural_gas")) %>%
        dplyr::mutate( sector = "1A3c_Rail"))

#   O.) Define missing 1A4c_Agriculture-forestry-fishing and 1A5_Other-unspecified EFs -->
#       Set to the value of 1A2g_Ind-Comb-Construction since all stationary_EFs are the same
#       (note biomass was already assigned above)

    missing_1A4c_1A5other<- rbind(
      stationary_ef %>%
        filter( sector == "1A2g_Ind-Comb-Construction") %>%
        mutate( sector = "1A4c_Agriculture-forestry-fishing"),
      stationary_ef %>%
        filter( sector == "1A2g_Ind-Comb-Construction") %>%
        mutate( sector = "1A5_Other-unspecified"))

    missing_1A4c_1A5other <- cbind(missing_1A4c_1A5other,
                                   replicate(26, missing_1A4c_1A5other$EF ) )

    missing_1A4c_1A5other <- replace_year_colnames_no_x(missing_1A4c_1A5other, 4, 1990, 2016)

    missing_1A4c_1A5other <- missing_1A4c_1A5other %>%
      tidyr::gather(key = years, value = EF, USGHG_INVENTORY_YEARS_NO_X) %>%
      dplyr::filter(fuel != "biomass")

    off_road_ef_df <- rbind(off_road_ef_df, missing_aviation, missing_boat,
                            missing_other_transp, missing_biomass_off_road,
                            missing_rail, missing_1A4c_1A5other)

# ------------------------------------------------------------------------------

# 6. Final Data Cleansing

#   A.) Apply the EFs to all relevant years (1960-2014)
    comb_ef <- off_road_ef_df %>%
        rbind(on_road_final_EFs) %>%
        rbind(elec_EFs_final) %>%
        rbind(heat_production_efs) %>%
        dplyr::filter(!(years == "2015"| years == "2016")) %>%
        dplyr::group_by(sector, fuel, units, years) %>%
        tidyr::spread(years, EF) %>%
        dplyr::ungroup()

    comb_ef_1990extd <- cbind(comb_ef, replicate(30, comb_ef[, "1990"]))

    comb_ef_1990extd <- replace_year_colnames(comb_ef_1990extd, 29, 4, 1960, 1989)

#   B.) Aplly stationary_efs to all years and add it to the rest of the EF data
    stationary_ef_1990extd <- cbind(stationary_ef, replicate(54, stationary_ef$EF))

    stationary_ef_1990extd <- replace_year_colnames(stationary_ef_1990extd, 4, 4, 1960, 2014)

    comb_ef_final <- rbind(comb_ef_1990extd, stationary_ef_1990extd)

#   C.) Apply the emissions factors to all CEDS countries
    MCL_list <- MCL %>%
      dplyr::select(iso, final_data_flag) %>%
      dplyr::distinct() %>%
      dplyr::filter(final_data_flag == 1 | iso == "srb (kosovo)" | iso == "gum") %>%
      dplyr::select(iso)

    unique_isos <- as.numeric(nrow(MCL_list))

    unique_sector_fuel <- as.numeric(nrow(comb_ef_final))

    MCL_list <- MCL_list[rep(row.names(MCL_list), unique_sector_fuel), ]

    MCL_list_final <- MCL_list %>%
      as.data.frame() %>%
      dplyr::rename(iso = ".") %>%
      dplyr::arrange(iso)

    comb_ef_final <- comb_ef_final[rep(row.names(comb_ef_final), unique_isos), ]

    comb_ef_final <- dplyr::bind_cols(comb_ef_final, MCL_list_final) %>%
      dplyr::select(iso, sector, fuel, units, ALL_YEARS) %>%
      dplyr::arrange(iso, sector, fuel)

# ------------------------------------------------------------------------------

# 7. Write output
    writeData( comb_ef_final , "MED_OUT", paste0( "B.", em, "_comb_EF_db" ) )

# Every script should finish with this line
  logStop()

# END
