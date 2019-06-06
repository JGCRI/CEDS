#-------------------------------------------------------------------------------
# Program Name: GBR_historical_coal.R
# Author: Caleb Braun and Patrick O'Rourke
# Date: June 6, 2019
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

# 1.) Set constants, load data, make mapping

# for writeData function
setwd( '../../' )
PARAM_DIR <- '../code/parameters/'
source( paste0(PARAM_DIR, "header.R" ) )
initialize( 'GBR_historical_coal.R', NULL, NULL )

FNAME <- 'Coal_since_1853'
FPATH <- 'user-defined-energy/'
SHEET <- 'Coal Availability & Consumption'

FULLRANGE <- "A5:S131" # The range of cells containing relevant information
DATASTART <- 23        # The row in FULLRANGE where the data actually starts
ALL_YEARS <- paste0( 1913:2016 )

xl <- readData( 'EXT_IN', paste0(FPATH, FNAME), '.xls', sheet = SHEET,
                range = FULLRANGE, missing_value = c( '', '..', '-' ), trim_ws = T )

# Column names are distributed in first two rows of data
cnames <- paste( xl[1, ], xl[2, ] )
cnames <- gsub( '(NA |\\d|,)', '', cnames )

df <- xl[DATASTART:nrow( xl ), ]                # Get dataframe of just the values
yrs <- substr( df[[1]], 1, 4 )                  # Get years from first column
df <- t( df )                                   # Convert data to wide-form
df <- as.data.frame( df, stringsAsFactors = F ) # Convert back to dataframe
df <- cbind( cnames, df )                       # Put row identifiers back in
df <- df[-1, ]                                  # Remove years from first row
colnames( df ) <- c( "sector", yrs )            # Set years as column names
rownames( df ) <- NULL

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
    "Miscellaneous"                   = "1A2g_Ind-Comb-other",
    "Miners"                          = "1A2_Industry-combustion",   # "1A2g_Ind-Comb-mining-quarying"
    "Coastwise Bunkers"               = "1A3_Transportation",        # "1A3dii_Domestic-navigation"
    "Other"                           = "1A2g_Ind-Comb-other",

#   "Northern Ireland"                = NA,
    "Total Inland Consumption"        = "ignore",
    "Overseas Shipments and Bunkers"  = "ignore",
    "Total Consumption and Shipments" = "ignore"
)
ceds_sector_map <- data.frame( sector = names( ceds_sector_map ),
                               agg_sector = ceds_sector_map,
                               stringsAsFactors = F )

#-------------------------------------------------------------------------------

#2. Process gbr coal data

# Initial data cleaning - convert units, map sectors
GBR_coal <- df %>%
    dplyr::mutate_at( yrs, as.numeric ) %>%
    dplyr::mutate_at( yrs, funs( . * 1000 ) ) %>%
    dplyr::mutate_at( 'sector', as.character ) %>%
    dplyr::left_join( ceds_sector_map, by = 'sector' ) %>%
    dplyr::filter( is.na( agg_sector ) | agg_sector != 'ignore' ) %>%
    dplyr::mutate( iso = 'gbr', agg_fuel = 'coal' ) %>%
    dplyr::select( iso, agg_fuel, agg_sector, dplyr::everything( ) ) %>%
    tidyr::gather(key = years, value = coal_consumption, ALL_YEARS )

# Fix Domestic, Industry and Miscellaneous values from 1923-1942 (currently all assigned to Domestic)
# based on their relative amounts from 1943
    sectors_to_fix <- c( "Domestic",  "Industry", "Miscellaneous" )
    years_to_fix <- paste0( 1923 : 1942 )
    domestic_1943_share <- 0.44
    industry_1943_share <- 0.4
    misc_1943_share <- 0.16

#   Subset data to fix, distribute data to the 3 sectors based on their shares, map to agg_sectors
    GBR_coal_to_fix <- GBR_coal %>%
        dplyr::select( -iso, -agg_fuel, -agg_sector ) %>%
        dplyr::filter( sector %in% sectors_to_fix,
                       years %in% years_to_fix ) %>%
        tidyr::spread( sector, coal_consumption ) %>%
        dplyr::mutate( Industry = Domestic,
                       Miscellaneous = Domestic ) %>%
        dplyr::mutate( Domestic = Domestic * domestic_1943_share,
                      Industry = Industry * industry_1943_share,
                      Miscellaneous = Miscellaneous * misc_1943_share ) %>%
        dplyr::select( years, Domestic, Industry, Miscellaneous ) %>%
        tidyr::gather( key = sector, value = coal_consumption_disaggregate_to_add, sectors_to_fix ) %>%
        dplyr::left_join( ceds_sector_map, by = "sector" ) %>%
        dplyr::select( agg_sector, years, coal_consumption_disaggregate_to_add )

#   Summarize original GBR data with Domestic turned to NA from 1923 to 1942 (since that data actually the combination of
#   Domestic, Industry, and Misc. that we just disaggregated)
    GBR_coal_summarized <- GBR_coal %>%
        dplyr::mutate( coal_consumption = if_else( sector %in% sectors_to_fix & years %in% years_to_fix,
                                                   NA_real_, coal_consumption ) ) %>%
        dplyr::select( iso, agg_fuel, agg_sector, dplyr::everything( ), -sector ) %>%
        dplyr::group_by( iso, agg_fuel, agg_sector, years ) %>%
        dplyr::summarise_all( sum, na.rm = TRUE )

#   Add disaggregated Domestic, Industry, and Misc. to aggregated data
    GBR_coal_fixed <- GBR_coal_summarized %>%
        dplyr::left_join( GBR_coal_to_fix, by = c( "agg_sector", "years" ) ) %>%
        dplyr::mutate( final_coal_consumption = if_else( is.na( coal_consumption_disaggregate_to_add ),
                                                         coal_consumption,
                                                if_else( !( is.na( coal_consumption_disaggregate_to_add) ),
                                                         coal_consumption + coal_consumption_disaggregate_to_add,
                                                         NA_real_ ) ) ) %>%
        dplyr::select( -coal_consumption, -coal_consumption_disaggregate_to_add ) %>%
        tidyr::spread( years, final_coal_consumption ) %>%
        dplyr::ungroup( ) %>%
        dplyr::rename( sector = agg_sector )

    GBR_coal_fixed[GBR_coal_fixed == 0] <- NA # We can do this because there are no zeros in original data

# Create "Total" consumption data
  GBR_coal_total <- GBR_coal_fixed %>%
        tidyr::gather( key = years, value = coal_consumption, ALL_YEARS ) %>%
        dplyr::mutate( sector = "Total" ) %>%
        dplyr::group_by( iso, agg_fuel, sector, years ) %>%
        dplyr::summarise_all( sum, na.rm = TRUE ) %>%
        tidyr::spread( years, coal_consumption )


# Save final output
writeData( GBR_coal_fixed, 'EXT_IN', paste0( FPATH, 'GBR_historical_coal_aggsec' ) )
writeData( GBR_coal_total, 'EXT_IN', paste0( FPATH, 'GBR_historical_coal_total' ) )


logStop()
