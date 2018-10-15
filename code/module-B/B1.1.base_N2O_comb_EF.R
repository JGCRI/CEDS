# ---------------------------------------------------------------------------
# Program Name: B1.1.base_N2O_comb_EF.R
# Author: Patrick O'Rourke
# Date Last Updated: October 4, 2018
# Program Purpose: Generate default emission factors for NH3 from NEI data
# Input Files: 1) N2O_base_EF-stationary-US_GHG2018.csv (base stationary EFs
#                 estimated by USA EPA)
#              2) N2O-stationary_EF-USGHG2018-_mapping.csv (mapping file for
#                 USGHG inventory ef)
#              3) USGHG-fuel_mapping.csv (mapping file for USGHG inventory fuels)
#              4) Master_Country_List.csv (mapping file for countries)
# Output Files: 1) B.[em]_comb_EF_db.csv
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
log_msg <- "Produce N20 emissions factors from US EPA GHG data"
script_name <- 'B1.1.base_N2O_comb_EF.R'

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "N20"

# Stop script if running for unsupported species
if ( em %!in% c( 'N20' ) ) {
    stop ( paste( 'not supported for emission species', em ) )
}

# ------------------------------------------------------------------------------

# 1. Read in files

#   A. Load US EPA N20 stationary emissions factors
     stationary_ef <- readData( "DEFAULT_EF_IN", "N2O_base_EF-stationary-US_GHG2018",
                                ".csv")

#   B. Load US GHG inventory sector map
     USGHG_sector_map <- readData( "MAPPINGS", "N2O-stationary_EF-USGHG2018_mapping",
                                   ".csv" )

#   C. Load US GHG inventory fuel map
     USGHG_fuel_map <- readData( "MAPPINGS", "USGHG-fuel_mapping", ".csv" )

#   D. Load the CEDS master country list
     MCL <- readData( "MAPPINGS", "Master_Country_List" )

# ------------------------------------------------------------------------------

# 2. Reformat Emissions Factors

#   A. Reformat Stationary emissions factor data
#       i.) Set the first row as column headers
        colnames(stationary_ef) = stationary_ef[1, ]
        stationary_ef <- dplyr::slice(stationary_ef, -1)

#       ii.) Rename first column
        stationary_ef <- dplyr::rename(stationary_ef,  "sector" = "Fuel/End-Use Sector")


#       iii.) Remove table comments that are now the last two rows of the df
        stationary_ef <- dplyr::filter(stationary_ef, !(sector ==
                          "a GJ (Gigajoule) = 109 joules. One joule = 9.486×10-4 Btu." | sector ==
                          "NA (Not Applicable)"))

#       iv.) Remove CH4 emission factors
        stationary_ef <- select(stationary_ef, -CH4)

#       v.) Create variable "fuel"
        fuels <- c("Coal", "Petroleum", "Natural Gas", "Wood")

        stationary_ef$fuel <- dplyr::if_else (stationary_ef$sector %in% fuels,
                                               stationary_ef$sector, "NA" )

        stationary_ef$N2O <- as.numeric(stationary_ef$N2O)

        stationary_ef$fuel <- dplyr::if_else(stationary_ef$N2O == 1.5 , "Coal", if_else(
            stationary_ef$N2O == 0.6, "Petroleum", if_else(
                stationary_ef$N2O == 0.1, "Natural Gas", if_else(
                    stationary_ef$N2O == 4.0, "Wood", "NA"))))

        stationary_ef <- dplyr::filter(stationary_ef, !(sector %in% fuels))

#       vi.) Rename "N20" column to "EF" (emission factor)
        stationary_ef <- dplyr::rename(stationary_ef,  EF = N2O)

#       vii.) Create variable "units"
        stationary_ef <- dplyr::mutate(stationary_ef, units = "g/GJ")

#       viii.) Reorder columns
        stationary_ef <- stationary_ef[, c("sector", "fuel", "units", "EF")]

#       ix.) Remove rows where sector is defined as "U.S. Territories"
        stationary_ef <- dplyr::filter(stationary_ef,!(sector == "U.S. Territories"))

#       x.) Convert emissions factors from g/GJ to kt/kt

#           a.) Convert emissions factors from g/GJ to kt/kt (first to kt/GJ, and then to kt/kt)
            stationary_ef <- dplyr::mutate(stationary_ef, EF = EF*0.000000001*4184)

#           b.) Redefine the variable "units"
            stationary_ef <- dplyr::mutate(stationary_ef, units = "kt/kt")

#       xi.) Map sectors to CEDS working_sectors_2

#           a.) create duplicate lines for industrial sector
            stationary_ef <- rbind(stationary_ef,
                                stationary_ef %>%
                                filter(sector == "Industrial"),
                                stationary_ef %>%
                                    filter(sector == "Industrial"),
                                stationary_ef %>%
                                    filter(sector == "Industrial"),
                                stationary_ef %>%
                                    filter(sector == "Industrial"),
                                stationary_ef %>%
                                    filter(sector == "Industrial"),
                                stationary_ef %>%
                                    filter(sector == "Industrial"),
                                stationary_ef %>%
                                    filter(sector == "Industrial"),
                                stationary_ef %>%
                                    filter(sector == "Industrial"),
                                stationary_ef %>%
                                    filter(sector == "Industrial"),
                                stationary_ef %>%
                                    filter(sector == "Industrial"),
                                stationary_ef %>%
                                    filter(sector == "Industrial"),
                                stationary_ef %>%
                                    filter(sector == "Industrial"))


#           b.) Sort df by fuel
            stationary_ef <- dplyr::arrange(stationary_ef, fuel)

#           c.) Create a variable to match industrial subsectors
            stationary_ef <- dplyr::mutate(stationary_ef, mn1 = c(1:15, 1:15, 1:15, 1:15))

#           d.) Create a map for industrial sector
            USGHG_sector_map_industrial <- dplyr::filter(USGHG_sector_map, sector == "Industrial")

#           e.) Create a variable to match industrial subsectors in industrial mapping file
            USGHG_sector_map_industrial <- dplyr::mutate(USGHG_sector_map_industrial, mn1 = c(3:15))

#           f.) Assign industrial sector to NA
            stationary_ef$sector <- dplyr::if_else (stationary_ef$sector == "Industrial", "NA",
                                                     stationary_ef$sector)

#           g.) Map sectors to CEDS working_sector_v2 for all sectors besides industrial
            drop.cols <- c("working_sectors_v1", "working_sectors_v2", "aggregate_sectors")
            stationary_ef <- dplyr::left_join(stationary_ef, USGHG_sector_map, by = "sector") %>%
                mutate(stationary_ef, sector = working_sectors_v2) %>%
                select(-one_of(drop.cols))

#           h.) Map over the industrial sector's subsectors
            drop.cols2 <- c("mn1", "sector.y", "working_sectors_v1", "working_sectors_v2", "aggregate_sectors")
            stationary_ef <- dplyr::left_join(stationary_ef, USGHG_sector_map_industrial, by = "mn1")
            stationary_ef <- dplyr::mutate(stationary_ef, sector.x = if_else(is.na(stationary_ef$sector.x),
                                     stationary_ef$working_sectors_v2, stationary_ef$sector.x))
            stationary_ef <- dplyr::rename(stationary_ef,  sector = sector.x)
            stationary_ef <- dplyr::select(stationary_ef, -one_of(drop.cols2))

#       xii.) Map fuels to CEDS fuels

#           a.) Create duplicate lines for coal and petroleum, as need to map multiple types of
#               coal and petro
            stationary_ef <- rbind(stationary_ef,
                                stationary_ef %>%
                                    filter((fuel == "Coal") | (fuel == "Petroleum")),
                                stationary_ef %>%
                                    filter((fuel == "Coal") | (fuel == "Petroleum")))

#           b.) Sort df by sector and fuel
            stationary_ef <- dplyr::arrange(stationary_ef, sector, fuel)


#           c.) Create a variable to match different types of coal and petroleum
            stationary_ef <- dplyr::mutate(stationary_ef, mn2 = c(1:8, 1:8, 1:8, 1:8, 1:8, 1:8, 1:8, 1:8,
                                                                    1:8, 1:8, 1:8, 1:8, 1:8, 1:8, 1:8))

#           d.) Create a map for coal and petroleum fuels
            USGHG_fuel_map_candp <- USGHG_fuel_map %>%
                                        filter((fuel == "Coal")| (fuel == "Petroleum"))

#           e.) Create a variable to match coal and petroleum fuels in c and p fuel mapping file
            USGHG_fuel_map_candp <- dplyr::mutate(USGHG_fuel_map_candp, mn2 = c(1:3, 5:7))


#           f.) Assign coal and petro fuels to NA
            stationary_ef$fuel <- dplyr::if_else(stationary_ef$fuel == "Coal" | stationary_ef$fuel == "Petroleum",
                                                  "NA", stationary_ef$fuel )

#           g.) Map fuels to CEDS fuels for wood and natural gas
            stationary_ef <- dplyr::left_join(stationary_ef, USGHG_fuel_map, by = "fuel") %>%
                mutate(stationary_ef, fuel = ceds_fuel) %>%
                select(-ceds_fuel)

#           h.) Map over coal and petroleum fuels
            drop.cols3 <- c("mn2", "fuel.y", "ceds_fuel")
            stationary_ef <- dplyr::left_join(stationary_ef, USGHG_fuel_map_candp, by = "mn2")
            stationary_ef$mn2 <- as.character(stationary_ef$mn2)
            stationary_ef$fuel.x <- dplyr::if_else(is.na(stationary_ef$fuel.x), stationary_ef$ceds_fuel,
                                                   stationary_ef$fuel.x)
            stationary_ef <- dplyr::rename(stationary_ef,  fuel = fuel.x)
            stationary_ef <- dplyr::select(stationary_ef, -one_of(drop.cols3))

#          i.) Sort df by sector and fuel
           stationary_ef <- dplyr::arrange(stationary_ef, sector, fuel)

#       xiii.) Apply the Emissions Factors to all CEDS countries
#           a.) Extract the iso column from MCL, and include only unique ISOs which are in the final data
#               (and srb(kosovo))
            MCL_list <- dplyr::distinct(dplyr::select(MCL, iso, final_data_flag))
            MCL_list <- dplyr::filter(MCL_list, final_data_flag == 1 | iso == "srb (kosovo)" | iso == "gum")

#           b.) Duplicate the list of ISOs so that each iso can be matched with each fuel and
#               sector combination in the US GHG data
            MCL_list <- MCL_list[rep(row.names(MCL_list), 120), ]

#           c.) Remove the final_data_flag column
            MCL_list <- select(MCL_list, -final_data_flag)

#           d.) Duplicate all rows in the emissions factor df so that each iso can be assigned to each
#               fuel and sector combination
            stationary_ef <- stationary_ef[rep(row.names(stationary_ef), 222), ]

#           e.) Sort df by sector and fuel
            stationary_ef <- dplyr::arrange(stationary_ef, sector, fuel)

#           f.) Combine the MCL_list and EF data frame so that EF data frame has all ISOs
            stationary_ef <- dplyr::bind_cols(stationary_ef, MCL_list)

#           g.) Sort by iso, sector, fuel
            stationary_ef <- dplyr::arrange(stationary_ef, iso, sector, fuel)

#           h.) Reorder columns of interest
            stationary_ef <- stationary_ef[, c("iso", "sector", "fuel", "units", "EF")]

#       xiv.) Apply the Emissions Factors to all relevant years (1960-2014)
#           a.) Duplicate the EF column
            stationary_ef_years <- cbind(stationary_ef, replicate(54, stationary_ef$EF))

#           b.) Rename the douplicated columns with the appropriate year title
            names <- as.character( colnames(stationary_ef_years))
            names[ 6:( length( names )) ] <- 1961:2014
            names[ 5 ] <- 1960
            years_list <- paste( "X", names[ 5:( length( names )) ], sep = "" )
            names[ 5:( length( names )) ] <- years_list
            names( stationary_ef_years ) <- names

# ------------------------------------------------------------------------------
# 3. Write output
     writeData( stationary_ef_years , "MED_OUT", paste0( "B.", em, "_comb_EF_db" ) )

# Every script should finish with this line
  logStop()

# END
