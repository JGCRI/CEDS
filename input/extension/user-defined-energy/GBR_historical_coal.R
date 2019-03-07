#-------------------------------------------------------------------------------
# Program Name: GBR_historical_coal.R
# Author: Caleb Braun and Patrick O'Rourke
# Date: March 7, 2019
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
# Output Files: GBR_historical_coal.csv
#-------------------------------------------------------------------------------

# for writeData function
setwd('../../')
PARAM_DIR <- '../code/parameters/'
source(paste0(PARAM_DIR, "header.R"))
initialize('GBR_historical_coal.R', NULL, NULL)

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
    "Electricity"                     = "1A1a_Electricity", # "1A1_Electricity-public"
    "Gas"                             = "1A4_Stationary_RCO", # While this is a transformation,
                                                              # town gas is counted in the IEA data as residential
    "Coke Ovens & MSF"                = "1A1_Energy-transformation", # "1A1bc_Other-transformation"
    "Railways"                        = "1A3_Transportation",        # "1A3c_Rail"
    "Domestic"                        = "1A4_Stationary_RCO",
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

df1 <- df %>%
        dplyr::mutate_at(yrs, as.numeric) %>%
        dplyr::mutate_at(yrs, funs(. * 1000)) %>%
        dplyr::mutate_at('sector', as.character) %>%
        dplyr::left_join(ceds_sector_map, by = 'sector') %>%
        dplyr::filter(is.na(agg_sector) | agg_sector != 'ignore') %>%
        dplyr::mutate(iso = 'gbr', agg_fuel = 'coal') %>%
        dplyr::select(iso, agg_fuel, agg_sector, dplyr::everything(), -sector) %>%
        dplyr::group_by(iso, agg_fuel, agg_sector) %>%
        dplyr::summarise_all(sum, na.rm = TRUE)

df1[df1 == 0] <- NA # We can do this because there are no zeros in original data

# For df1 Split the 1A4_Stationary_RCO values by a proportion among 1A4_Stationary_RCO, 1A2_Industry-combustion,
# and 6A_Other-in-total from 1923 to 1942

    df1 <- df1 %>%
      gather(year, value, -iso, -agg_fuel, -agg_sector) %>%
      spread(agg_sector, value) -> GBR_coal

    yrs2 <- 1923:1942

#   Define the function split_RCO

    split_RCO <- function(df_in){
      names(df_in)[names(df_in) == '1A4_Stationary_RCO'] <- 'RCO'
      names(df_in)[names(df_in) == '1A2_Industry-combustion'] <- 'ind'
      names(df_in)[names(df_in) == '6A_Other-in-total'] <- 'other'
      df_in$year <- as.numeric(df_in$year)
      df_in$ind <- ifelse(df_in$year %in% yrs2, (df_in$RCO)*0.40, df_in$ind)
      df_in$other <- ifelse(df_in$year %in% yrs2, (df_in$RCO)*0.16, df_in$other)
      df_in$RCO <- ifelse(df_in$year %in% yrs2, (df_in$RCO)*0.44, df_in$RCO)
      names(df_in)[names(df_in) == 'RCO'] <- '1A4_Stationary_RCO'
      names(df_in)[names(df_in) == 'ind'] <- '1A2_Industry-combustion'
      names(df_in)[names(df_in) == 'other'] <- '6A_Other-in-total'
      df_in$year <- as.character(df_in$year)
      df_in <- as.data.frame(df_in)
    }

#   Use split_RCO
    GBR_coal2 <- split_RCO(GBR_coal)

#   Cast back to wide
    sectors <-c("1A1_Energy-transformation", "1A2_Industry-combustion", "1A3_Transportation", "1A4_Stationary_RCO",
            "6A_Other-in-total", "<NA>")

    GBR_coal2 <- GBR_coal2 %>%
      gather(sectors, value, -iso, -agg_fuel, -year) %>%
      spread(year, value) -> GBR_coal3

#   Make this a data frame
    GBR_coal3 <- as.data.frame(GBR_coal3)

# Reformat df2
    df2 <- df %>%
        dplyr::mutate_at(yrs, as.numeric) %>%
        dplyr::mutate_at(yrs, funs(. * 1000)) %>%
        dplyr::mutate_at('sector', as.character) %>%
        dplyr::left_join(ceds_sector_map, by = 'sector') %>%
        dplyr::filter(is.na(agg_sector) | agg_sector != 'ignore') %>%
        dplyr::mutate(iso = 'gbr', agg_fuel = 'coal') %>%
        dplyr::select(iso, agg_fuel, dplyr::everything(), -agg_sector, -sector) %>%
        dplyr::group_by(iso, agg_fuel) %>%
        dplyr::summarise_all(sum, na.rm = TRUE)

    df2[df2 == 0] <- NA # We can do this because there are no zeros in original data

    df2$sectors <- "Total"
    yrs3 <- 1913:2016
    yrs3 <- as.character(yrs3)
    df2 <- df2[c("iso", "agg_fuel", "sectors", yrs3)]
    GBR_coal4 <- df2
    GBR_coal4 <- as.data.frame(GBR_coal4)

# Combine two data frames
    GBR_coal_final <- rbind(GBR_coal3,GBR_coal4)

# Make values 0 for years not using individual sectors
    # setnames(GBR_coal_final, old = c('1913', '1914', '1915', '1916', '1917', '1918', '1919', '1920', '1921', '1922'),
    #          new = c('x1913', 'x1914', 'x1915', 'x1916', 'x1917', 'x1918', 'x1919', 'x1920', 'x1921', 'x1922'))

    gbr_all_years <- paste0(1913:2016)
    gbr_zero_total_years <- paste0(1913:1922)
    GBR_coal_final <- GBR_coal_final %>%
        tidyr::gather(key = years, value = activity, gbr_all_years) %>%
        dplyr::mutate(activity = if_else(years %in% gbr_zero_total_years & sectors != "Total", 0, activity)) %>%
        tidyr::spread(years, activity) %>%
        dplyr::rename(sector = sectors)

# Save final output
writeData(GBR_coal_final, 'EXT_IN', paste0(FPATH, 'GBR_historical_coal'))

logStop()
