#------------------------------------------------------------------------------
# Program Name: US-EIA_activity.R
# Authors: Ben Goldstein, Caleb Braun
# Date Last Modified: June 29, 2017
#
# To read in & reformat EIA activity data from 1949 to present
#
# Units are initially in btu
#
# Input Files:  All files in the folder EIA-data
# Output Files: US_EIA_inventory.csv
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(readxl)

# for writeData function
# setwd('../../')
PARAM_DIR <- '../code/parameters/'
source( paste0( PARAM_DIR, "header.R" ) )
initialize( 'US-EIA_activity.R', NULL, NULL )

EIA_DIR <- "user-defined-energy/EIA-data/"
EIA_PATH <- filePath( "EXT_IN", EIA_DIR, extension = "" )
CONV_DIR <- paste0( EIA_DIR, "unit-conversion/" )

# Define constants
CONV_TBTU_TJ <- 1055             # Trillion BTU to TJ
CONV_SHORTTON_TONNE <- 0.9072
CONV_TONNE_BARREL <- 7.33        # mass density factor from OPEC 2014
CONV_FACTOR_NATGAS_TJ_T <- 46629 # US natural gas OECD conversion factor TJ to t

# CEDS constants
CONV_FACTOR_BIO_KT_TJ <-    16   # See CEDS conversionFactor_biomass_kt_TJ
CONV_FACTOR_NATGAS_TJ_KT <- 44.2 # See CEDS conversionFactor_naturalgas_TJ_per_kt

YYYYMM_to_Xyear <- function( YYYYMM ) paste0( "X", substr( YYYYMM, 1, 4 ) )


# Read in relevant EIA data files -----------------------------------------

# Prepare to read in all *.csv files from the EIA data folder
files_to_read <- paste0( EIA_DIR, list.files( EIA_PATH, ".*csv$" ) )

# Read each file of reported and combine into a single dataframe
EIA_data_files <- grep( "MER", files_to_read, value = T )
all_EIA_raw_data <- lapply( EIA_data_files, readData, domain = "EXT_IN" )
all_EIA_raw_data <- do.call(rbind, all_EIA_raw_data)

# Read in heat content conversion files
petrol_heat_content <- readData( "EXT_IN", paste0( CONV_DIR, "MER_TA3" ) )
gas_heat_content <-    readData( "EXT_IN", paste0( CONV_DIR, "MER_TA4" ) )
coal_heat_content <-   readData( "EXT_IN", paste0( CONV_DIR, "MER_TA5" ) )


# Bring the EIA data into TJ and year/month form --------------------------

# Retain only annual info, drop not-available cells, and convert to TJ from BTU
EIA_data_formatted <- all_EIA_raw_data %>%
    dplyr::mutate( year  = YYYYMM_to_Xyear( YYYYMM ),
                   month = substr( YYYYMM, 5, 6 ) ) %>%
    dplyr::filter( month == 13,
                   !Value %in% c( "Not Available", "No Data Reported" ) ) %>%
    dplyr::mutate( Value = if_else( Unit == "Trillion Btu",
                                    CONV_TBTU_TJ * as.numeric( Value ),
                                    as.numeric( Value ) ),
                   Unit = gsub( "Trillion Btu", "TJ", Unit ) ) %>%
    dplyr::select( MSN, Value, Description, Unit, year )


# Identify CEDS fuels and sectors from IEA data ---------------------------

# Extract fuel and sector information from EIA descriptions and discard any data
# that does not contain fuel or sector specific info
EIA_data_formatted <- EIA_data_formatted %>%
    dplyr::mutate(
        fuel = case_when(
            grepl( "Coal",        Description ) ~ "coal",
            grepl( "Biomass",     Description ) ~ "biomass",
            grepl( "Natural Gas", Description ) ~ "gas",
            grepl( "Petroleum",   Description ) ~ "oil"
        ),
        sector = case_when(
            grepl( "Residential Sector",    Description ) ~ "Residential",
            grepl( "Commercial Sector",     Description ) ~ "Commercial",
            grepl( "Industrial Sector",     Description ) ~ "Industry",
            grepl( "Transportation Sector", Description ) ~ "Transportation",
            grepl( "Electric Power Sector", Description ) ~ "Electric Power"
        ) ) %>%
    dplyr::filter( !is.na( fuel ), !is.na( sector ) )


# Assign an iso to the dataset and extract only those columns we want to use
# from now on (CEDS info)
EIA_data_formatted <- EIA_data_formatted %>%
    dplyr::mutate( iso = "usa" ) %>%
    dplyr::select( iso, sector, fuel, Unit, year, Value )


# Remove liquid biofuels from biomass estimate ----------------------------
#   CEDS considers liquid biofuels (ethanol) in petroleum, but EIA counts it
#   in renewable biomass. For the purposes of informing CEDS, we must remove
#   liquid biomass from the EIA total biomass energy estimate. This also
#   requires that CEDS has produced an estimate with no liquid biomass in
#   petroleum.

# Read in the sectoral renewable breakdowns, isolating only fuel ethanol columns
ind_trn_file <- paste0( CONV_DIR, "Table_10.2b_Renewable_Energy_Consumption___Industrial_and_Transportation_Sectors.xlsx" )
com_file <- paste0( CONV_DIR, "Table_10.2a_Renewable_Energy_Consumption___Residential_and_Commercial_Sectors.xlsx" )
liquid_biofuels_ind <- readData( "EXT_IN", ind_trn_file, ".xlsx",
                                 sheet_selection = "Annual Data", skip = 10 )[ -1, c( 1, 8 ) ]
liquid_biofuels_trn <- readData( "EXT_IN", ind_trn_file, ".xlsx",
                                 sheet_selection = "Annual Data", skip = 10 )[ -1, c( 1, 12, 13 ) ]
liquid_biofuels_com <- readData( "EXT_IN", com_file,     ".xlsx",
                                 sheet_selection = "Annual Data", skip = 10 )[ -1, c( 1, 12 ) ]

# In transportation, we need to sum diesel and ethanol.
liquid_biofuels_trn <- liquid_biofuels_trn %>%
    dplyr::mutate_at( vars( matches( "(Ethanol|Biodiesel)" ) ),
                      funs( as.numeric( gsub( "Not Available", 0, . ) ) ) ) %>%
    dplyr::mutate( diff_val = rowSums( .[2:3] ), sector = "Transportation" ) %>%
    dplyr::select( year = `Annual Total`, diff_val, sector )

# Unify column names and types for rbind operation
liquid_biofuels_ind <- liquid_biofuels_ind %>%
    setNames( c( "year", "diff_val" ) ) %>%
    dplyr::filter( diff_val != "Not Available" ) %>%
    dplyr::mutate( diff_val = as.numeric( diff_val ) , sector = "Industry" )
liquid_biofuels_com <- liquid_biofuels_com %>%
    setNames( c( "year", "diff_val" ) ) %>%
    dplyr::filter( diff_val != "Not Available" ) %>%
    dplyr::mutate( diff_val = as.numeric( diff_val ), sector = "Commercial" )

# Create a single df storing all values and prepare columns for join
liquid_biofuels <- liquid_biofuels_ind %>%
    rbind( liquid_biofuels_trn, liquid_biofuels_com ) %>%
    dplyr::mutate( diff_val = diff_val * CONV_TBTU_TJ,
                   Unit = "TJ",
                   fuel = "biomass",
                   year = paste0( "X", year ) )

# Remove liquid biomass from the EIA total biomass energy estimate
EIA_data_formatted <- liquid_biofuels %>%
    dplyr::right_join( EIA_data_formatted, by = c( "year", "fuel", "sector", "Unit" ) ) %>%
    dplyr::mutate( diff_val = if_else( is.na( diff_val ), 0, diff_val ),
                   Value = Value - diff_val) %>%
    dplyr::select( iso, sector, fuel, Unit, year, Value )


# Prepare unit conversion -------------------------------------------------
#  - The next two code blocks use heat content conversion files to determine the
#    mass of fuel consumed (given energy EIA data).
#  - Biomass is converted using a constant.
#  - Petroleum is converted from BTU to barrels using the EIA heat content timeseries,
#        then to mass given a density (constant?)
#  - Coal is converted from BTU directly to mass using the EIA heat constant timeseires
#  - Natural gas is CURRENTLY converted using a constant, but this is WRONG and
#        we want to change that.
#  - The result of these two sections is a formatted EIA data frame in mass units

# Extract year data from heat content dataframes
coal_heat_content$year <-   YYYYMM_to_Xyear( coal_heat_content$YYYYMM )
petrol_heat_content$year <- YYYYMM_to_Xyear( petrol_heat_content$YYYYMM )
gas_heat_content$year <-    YYYYMM_to_Xyear( gas_heat_content$YYYYMM )

# Match petroleum to sectors based on ID indicators (manually identified)
petrol_heat_content <- petrol_heat_content %>%
    dplyr::mutate( sector = case_when(
            MSN == "PARCKUS" ~ "Residential",
            MSN == "PACCKUS" ~ "Commercial",
            MSN == "PAICKUS" ~ "Industry",
            MSN == "PAACKUS" ~ "Transportation",
            MSN == "PAEIKUS" ~ "Electric Power"
        ) ) %>%
    dplyr::filter( !is.na( sector ) )

# Match petroleum to sectors based on ID indicators (manually identified)
coal_heat_content <- coal_heat_content %>%
    dplyr::mutate( sector = case_when(
            MSN == "CLHCKUS" ~ "Residential and Commercial",
            MSN == "CLOCKUS" ~ "Industry",
            MSN == "CLEIKUS" ~ "Electric Power"
        ) ) %>%
    dplyr::filter( !is.na( sector ) )

# Coal reports a single heat content value for both residential and commercial
# sectors; we need to make separate entries for these for mapping purposes
coal_heat_content_res <- coal_heat_content %>%
    dplyr::filter( sector == "Residential and Commercial" ) %>%
    dplyr::mutate( sector = "Residential" )
coal_heat_content_com <- coal_heat_content %>%
    dplyr::filter( sector == "Residential and Commercial" ) %>%
    dplyr::mutate( sector = "Commercial" )

# We need to do the same thing to separate transportation from industry (single
# reported value), then bind everything together
coal_heat_content <- coal_heat_content %>%
    dplyr::filter( sector == "Industry" ) %>%
    dplyr::mutate( sector = "Transportation") %>%
    dplyr::bind_rows( coal_heat_content, coal_heat_content_com,
                      coal_heat_content_res )

# Add fuel indicators
coal_heat_content$fuel <- "coal"
petrol_heat_content$fuel <- "oil"

# Convert from (million btu/short ton) to TJ/kt
petrol_heat_content$Value <- as.numeric( petrol_heat_content$Value ) * CONV_TONNE_BARREL
petrol_heat_content$Unit <- "Million BTU/tonne"
# Convert from Million BTU/t to TJ/kt
petrol_heat_content$Value <- as.numeric(petrol_heat_content$Value) * ( CONV_TBTU_TJ / 1e6 ) * 1e3
petrol_heat_content$Unit <- "TJ/kt"

coal_heat_content$Value <- as.numeric(coal_heat_content$Value) *
                                CONV_SHORTTON_TONNE * 1e3 * # Convert short ton to kt
                                ( CONV_TBTU_TJ / 1e6 ) # Convert Million BTU to TJ
coal_heat_content$Unit <- "TJ/kt"

conversion_factors <- rbind( coal_heat_content, petrol_heat_content )


# Execute conversion to kt ------------------------------------------------
#    Executes the conversions described in the code block description above

# Convert biomass to kt using CEDS standard conversion factor
# TODO: This will hopefully change--we need an EIA factor since they use diff. reporting
EIA_data_formatted$Value[ EIA_data_formatted$fuel == "biomass" ] <-
      as.numeric( EIA_data_formatted$Value[ EIA_data_formatted$fuel == "biomass" ] ) / CONV_FACTOR_BIO_KT_TJ
EIA_data_formatted$Unit[ EIA_data_formatted$fuel == "biomass" ] <- "kt"

# Convert natural gas to kt using CEDS standard conversion factor
# TODO: This will hopefully change--we need an EIA factor since they use diff. reporting
EIA_data_formatted$Value[ EIA_data_formatted$fuel == "gas" ] <-
      as.numeric( EIA_data_formatted$Value[ EIA_data_formatted$fuel == "gas" ] ) / CONV_FACTOR_NATGAS_TJ_KT
EIA_data_formatted$Unit[ EIA_data_formatted$fuel == "gas" ] <- "kt"

# Subset the data which can be converted using timeseries EIA conversion data
EIA_convert_subset <- EIA_data_formatted[ EIA_data_formatted$fuel %in% c( "oil", "coal" ), ]

# Join conversion factors to their corresponding data rows, then apply
# timeseries conversions
EIA_convert_subset <- EIA_convert_subset %>%
    dplyr::left_join( conversion_factors[ , c( "Value", "year", "fuel", "sector" ) ],
                      by = c( "fuel", "sector", "year" ) ) %>%
    dplyr::rename( Value = Value.x, Conversion_factor = Value.y ) %>%
    dplyr::mutate( Value = Value / Conversion_factor,
                   Unit = "kt" ) %>%
    dplyr::select( iso, sector, fuel, Unit, year, Value )

# Add converted data back into the main dataframe
EIA_data_formatted[ EIA_data_formatted$fuel %in% c( "oil", "coal" ), ] <- EIA_convert_subset


# Remove coal coke use from industry --------------------------------------
#    Coal coke usage in coal coke manufacture is considered a process activity
#    in CEDS, so it needs to be removed from the EIA estimate

# Read in a supplementary EIA file that will identify coal coke activity
coal_activity_file <- paste0( EIA_PATH, "Table_6.2_Coal_Consumption_by_Sector.xlsx" )
coal_activity_all <- readxl::read_xlsx( coal_activity_file, "Annual Data", skip = 10 )

# Get coal coke subset of the coal activity and convert from thousand short tons
# to kt
coal_coke_consumed <- coal_activity_all[ -1, c( 1, 6 ) ] %>%
    setNames( c( "year", "Value" ) ) %>%
    dplyr::mutate( Value = as.numeric( Value ) * CONV_SHORTTON_TONNE,
                   Unit = "kt" )

# Subtract from coal industry activity
coal_ind_rows <- EIA_data_formatted$fuel == "coal" & EIA_data_formatted$sector == "Industry"
EIA_data_formatted[ coal_ind_rows, "Value" ] <-
      EIA_data_formatted[ coal_ind_rows, "Value" ] - coal_coke_consumed$Value


# Update coal by sector ---------------------------------------------------
#    Replace coal info sector by sector with data from coal-specific mass units

# Get coal sector subset of the coal activity and convert from thousand short
# tons to kt
replace_coal_sector <- function( EIA_all, coal_activity_all, sector, col_i ) {
    stopifnot( coal_activity_all[ 1, col_i ] == "(Thousand Short Tons)" )
    coal_sector <- dplyr::pull( coal_activity_all, col_i )[-1] %>%
        grep( "Not Available", ., value = T, invert = T ) %>%
        as.numeric() * CONV_SHORTTON_TONNE
    sector_rows <- EIA_all$fuel == "coal" & EIA_all$sector == sector
    EIA_all[ sector_rows, 'Value' ] <- coal_sector
    EIA_all
}

# Map the coal sector names to their columns in coal_activity_all
coal_sectors <- c( "Residential" = 2,
                   "Commercial" = 5,
                   "Industry" = 9,
                   "Transportation" = 11,
                   "Electric Power" = 12 )

for ( sector in names( coal_sectors ) ) {
    EIA_data_formatted <- replace_coal_sector( EIA_data_formatted,
                                               coal_activity_all,
                                               sector,
                                               coal_sectors[ sector ] )
}

# Subtract coal coke from industry
coal_ind_rows <- EIA_data_formatted$fuel == "coal" & EIA_data_formatted$sector == "Industry"
EIA_data_formatted[ coal_ind_rows, "Value" ] <-
    EIA_data_formatted[ coal_ind_rows, "Value" ] - coal_coke_consumed$Value


# Cast to wide and write output -------------------------------------------
EIA_final <- tidyr::spread( EIA_data_formatted, key = year, value = Value )

EIA_final_CEDSsec <- EIA_final[EIA_final$sector %in% c("Commercial", "Residential"), ]
EIA_final_aggsec <- EIA_final[!EIA_final$sector %in% c("Commercial", "Residential"), ]

writeData( EIA_final_CEDSsec, 'EXT_IN', 'user-defined-energy/US-EIA_inventory_CEDSsec' )
writeData( EIA_final_aggsec, 'EXT_IN', 'user-defined-energy/US-EIA_inventory_aggsec' )


logStop()
