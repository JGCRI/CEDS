#-------------------------------------------------------------------------------
# Program Name: GBR_historical_coal.R
# Author: Caleb Braun
# Date: February 2, 2018
#
# Transforms raw UK historical coal data into a CEDS format .csv file.
#
# Prepares historical data on coal consumption in the UK going back to 1913 for
# use as a user-added file in the energy-extension module of CEDS. The original
# data is freely avaialable online, however it is not mapped or formatted in a
# way that is compatable with CEDS, even with a mapping file. This script
# removes the file's meta information, converts the data to wide-form, and
# filters out any data not representing domestic consumption.
#
# Data source: https://www.gov.uk/government/statistical-data-sets/historical-
# coal-data-coal-production-availability-and-consumption-1853-to-2011
#
# Input File: Coal_since_1853.xls
# Output File: GBR_historical_coal.csv
#-------------------------------------------------------------------------------

library(readxl)
library(tidyr)

# for writeData function
# setwd('../../')
PARAM_DIR <- '../code/parameters/'
source(paste0(PARAM_DIR, "header.R"))
initialize('DEU_historical_coal.R', NULL, NULL)

FNAME <- 'Coal_since_1853'
FPATH <- 'user-defined-energy/'
SHEET <- 'Coal Availability & Consumption'

FULLRANGE <- "A5:S131" # The range of cells containing relevant information
DATASTART <- 23        # The row in FULLRANGE where the data actually starts


xl <- readData('EXT_IN', paste0(FPATH, FNAME), '.xls', sheet = SHEET,
               range = FULLRANGE, missing_value = c('', '..', '-'), trim_ws = T)

# Column names are distributed in first two rows of data
cnames <- paste(xl[1, ], xl[2, ])
cnames <- gsub('(NA |\\d|,)', '', cnames)

df <- xl[DATASTART:nrow(xl), ]                # Get dataframe of just the values
yrs <- substr(df[[1]], 1, 4)                  # Get years from first column
df <- t(df)                                   # Convert data to wide-form
df <- as.data.frame(df, stringsAsFactors = F) # Convert back to dataframe
df <- cbind(cnames, df)                       # Put row identifiers back in
df <- df[-1, ]                                # Remove years from first row
colnames(df) <- c("sector", yrs)              # Set years as column names
rownames(df) <- NULL

ceds_sector_map <- c(
#   sector                              agg_sector                     CEDS_sector
#   "Year"                            = "ignore",
    "Supply"                          = "ignore",
    "Colliery Stocks"                 = "ignore",
    "Distributed Stocks"              = "ignore",

    "Collieries"                      = "1A1_Energy-transformation", # "1A1bc_Other-transformation"
    "Electricity"                     = "1A1_Energy-transformation", # "1A1_Electricity-public"
    "Gas"                             = "1A1_Energy-transformation", # "1A1bc_Other-transformation"
    "Coke Ovens & MSF"                = "1A1_Energy-transformation", # "1A1bc_Other-transformation"
    "Railways"                        = "1A3_Transportation",        # "1A3c_Rail"
    "Domestic"                        = "1A4_Stationary_RCO",        # "1A4b_Residential"
    "Industry"                        = "1A2_Industry-combustion",
    "Miscellaneous"                   = "6A_Other-in-total",
    "Miners"                          = "1A2_Industry-combustion",   # "1A2g_Ind-Comb-mining-quarying"
    "Coastwise Bunkers"               = "1A3_Transportation",        # "1A3dii_Domestic-navigation"
    "Other"                           = "6A_Other-in-total",

#   "Northern Ireland"                = NA,
    "Total Inland Consumption"        = "ignore",
    "Overseas Shipments and Bunkers"  = "ignore",
    "Total Consumption and Shipments" = "ignore"
)
ceds_sector_map <- data.frame(sector     = names(ceds_sector_map),
                              agg_sector = ceds_sector_map,
                              stringsAsFactors = F)

df <- dplyr::mutate_at(df, yrs, as.numeric) %>%
      dplyr::mutate_at(yrs, funs(. * 1000)) %>%
      dplyr::mutate_at('sector', as.character) %>%
      dplyr::left_join(ceds_sector_map, by = 'sector') %>%
      dplyr::filter(is.na(agg_sector) | agg_sector != 'ignore') %>%
      dplyr::mutate(iso = 'gbr', agg_fuel = 'coal') %>%
      dplyr::select(iso, agg_fuel, agg_sector, dplyr::everything(), -sector) %>%
      dplyr::group_by(iso, agg_fuel, agg_sector) %>%
      dplyr::summarise_all(sum, na.rm = TRUE)

df[df == 0] <- NA # We can do this because there are no zeros in original data

write.csv(df, 'GBR_historical_coal.csv', row.names = F)
