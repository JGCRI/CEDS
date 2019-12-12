#------------------------------------------------------------------------------
# Program Name: A8.1a.US-EIA_activity.R
# Authors: Ben Goldstein, Caleb Braun
# Date Last Modified: August 27, 2019
# Program Purpose: To read in & reformat EIA activity data from 1949 to present
# Input Files:  All files in the EIA-data subdirectory
# Output Files: A.US-EIA_energy_data_aggsec.csv, A.US-EIA_energy_data_CEDSsec.csv
# Notes: Units are initially in btu
# ------------------------------------------------------------------------------

# 0. Read in global settings and headers, define script constants

# for writeData function
setwd( '../../')
PARAM_DIR <- '../code/parameters/'
source( paste0( PARAM_DIR, "header.R" ) )
initialize( 'A8.1a.US-EIA_activity.R', NULL, NULL )

EIA_DIR <- "EIA-data/"
EIA_PATH <- filePath( "USER_EN_PROCESS", EIA_DIR, extension = "" )
CONV_DIR <- paste0( EIA_DIR, "unit-conversion/" )

# Define constants
CONV_TJ_BBTU            <- 0.9478  # TJ per Billion Btu
CONV_SHORTTON_TONNE     <- 0.9072  # Short Ton (US) per metric tonne
CONV_TONNE_BARREL       <- 7.33    # mass density factor from OPEC 2014

CONV_FACTOR_NATGAS_HHV_LHV <- 0.9  # gross heat content (HHV) to net heat content (LHV)

# CEDS constants
CONV_FACTOR_BIO_KT_TJ    <- 16   # See CEDS conversionFactor_biomass_kt_TJ
CONV_FACTOR_NATGAS_TJ_KT <- 44.2 # See CEDS conversionFactor_naturalgas_TJ_per_kt

YYYYMM_to_Xyear <- function( YYYYMM ) paste0( "X", substr( YYYYMM, 1, 4 ) )


# ------------------------------------------------------------------------------

# 1. Read in relevant EIA data files

# Prepare to read in all coal, oil, and gas .csv files from the EIA data folder
files_to_read <- paste0( EIA_DIR, list.files( EIA_PATH, "(Coal|gas|Oil)\\.csv$" ) )

# Read each file of reported and combine into a single dataframe
all_EIA_raw_data <- lapply( files_to_read, readData, domain = "USER_EN_PROCESS" )
all_EIA_raw_data <- do.call( rbind, all_EIA_raw_data )

# Read in heat content conversion files
# Note that coal is already in physical units, so no energy conversion is needed
petrol_heat_content <- readData( "USER_EN_PROCESS", paste0( CONV_DIR, "EIA_MER_TA3_Oil_heat_content" ) )
gas_heat_content    <- readData( "USER_EN_PROCESS", paste0( CONV_DIR, "EIA_MER_TA4_Natural_gas_heat_content" ) )


# ------------------------------------------------------------------------------

# 2. Select only the annual EIA data

# Retain only annual info and drop not-available cells
EIA_data <- all_EIA_raw_data %>%
    dplyr::mutate( year  = YYYYMM_to_Xyear( YYYYMM ),
                   month = substr( YYYYMM, 5, 6 ) ) %>%
    dplyr::filter( month == 13,
                   !Value %in% c( "Not Available", "No Data Reported" ) ) %>%
    dplyr::mutate( Value = as.numeric( Value ) ) %>%
    dplyr::select( MSN, Value, Description, Unit, year )

# ------------------------------------------------------------------------------

# 3. Identify aggregate fuels and sectors from IEA data

# Extract fuel and sector information from EIA descriptions and discard any data
# that does not contain fuel or sector specific info
EIA_data <- EIA_data %>%
    dplyr::mutate(
        fuel = case_when(
            grepl("CLKCPUS", MSN ) ~ "coal",
            grepl( "Coal.*Coke",  Description ) ~ "coal_coke",
            grepl( "Coal",        Description ) ~ "coal",
            grepl( "Biomass",     Description ) ~ "biomass",
            grepl( "Natural Gas", Description ) ~ "gas",
            grepl( "Petroleum",   Description ) ~ "oil"
        ),
        sector = case_when(
            grepl( "Residential Sector$",       Description ) ~ "Residential",
            grepl( "Commercial Sector$",        Description ) ~ "Commercial",
            grepl( "Commercial.*Total$",        Description ) ~ "Commercial",
            grepl( "Industrial.*Coke Plants$",  Description ) ~ "Process",
            grepl( "Other Industrial.*Total$",  Description ) ~ "Industry",
            grepl( "the Industrial.*Total$",    Description ) ~ "Industry",
            grepl( "Transportation Sector$",    Description ) ~ "Transportation",
            grepl( "Transportation.*Total$",    Description ) ~ "Transportation",
            grepl( "Electric Power Sector$",    Description ) ~ "Electric Power"
        ) ) %>%
    dplyr::filter( !is.na( fuel ), !is.na( sector ) ) %>%
    dplyr::filter( fuel != "coal" | !grepl( "the Industrial.*Total$", Description ),
                   fuel == "coal" | !grepl( "Other Industrial", Description ) )


# Assign an iso to the dataset and extract only those columns we want to use
# from now on (CEDS info)
EIA_data <- EIA_data %>%
    dplyr::mutate( iso = "usa" ) %>%
    dplyr::select( iso, sector, fuel, Unit, year, Value )

stopifnot( !anyDuplicated( EIA_data ) )

# ------------------------------------------------------------------------------

# 4. Remove liquid biofuels from biomass estimate

#   CEDS considers liquid biofuels (ethanol) in petroleum, but EIA counts it
#   in renewable biomass. For the purposes of informing CEDS, we must remove
#   liquid biomass from the EIA total biomass energy estimate. This also
#   requires that CEDS has produced an estimate with no liquid biomass in
#   petroleum.

# Read in the sectoral renewable breakdowns, isolating only fuel ethanol columns
ind_trn_file <- paste0( CONV_DIR, "Table_10.2b_Renewable_Energy_Consumption___Industrial_and_Transportation_Sectors.xlsx" )
com_file <- paste0( CONV_DIR, "Table_10.2a_Renewable_Energy_Consumption___Residential_and_Commercial_Sectors.xlsx" )
liquid_biofuels_ind <- readData( "USER_EN_PROCESS", ind_trn_file, ".xlsx",
                                 sheet_selection = "Annual Data", skip = 10 )[ -1, c( 1, 8 ) ]
liquid_biofuels_trn <- readData( "USER_EN_PROCESS", ind_trn_file, ".xlsx",
                                 sheet_selection = "Annual Data", skip = 10 )[ -1, c( 1, 12, 13 ) ]
liquid_biofuels_com <- readData( "USER_EN_PROCESS", com_file,     ".xlsx",
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
    dplyr::mutate( diff_val = diff_val  / ( CONV_TJ_BBTU / 1e3 ),
                   Unit = "TJ",
                   fuel = "biomass",
                   year = paste0( "X", year ) )

# Remove liquid biomass from the EIA total biomass energy estimate
EIA_data <- liquid_biofuels %>%
    dplyr::right_join( EIA_data, by = c( "year", "fuel", "sector", "Unit" ) ) %>%
    dplyr::mutate( diff_val = if_else( is.na( diff_val ), 0, diff_val ),
                   Value = Value - diff_val) %>%
    dplyr::select( iso, sector, fuel, Unit, year, Value )

# ------------------------------------------------------------------------------

# 5. Prepare unit conversion

#  - The next two code blocks use heat content conversion files to determine the
#    mass of fuel consumed (when given energy EIA data).
#  - Biomass is converted using a constant.
#  - Petroleum is converted from BTU to barrels using the EIA heat content timeseries,
#        then to mass given a density (constant?)
#  - Natural gas is converted from billion ft^3 to kt using the EIA heat content
#        timeseries and conversion constants
#  - The result of these two sections is a formatted EIA data frame in mass units

# Extract year data from heat content dataframes
petrol_heat_content$year <- YYYYMM_to_Xyear( petrol_heat_content$YYYYMM )
gas_heat_content$year <-    YYYYMM_to_Xyear( gas_heat_content$YYYYMM )

# Match gas to sectors - only Electric Power has unique heat content
gas_sectors <- c( "Electric Power", "Residential", "Commercial", "Industry", "Transportation" )
gas_rows <- EIA_data$fuel == "gas"
stopifnot( all( EIA_data[ gas_rows, "sector" ] %in% gas_sectors ) )

gas_unit_conv <- gas_heat_content %>%
    dplyr::mutate( tmp = case_when(
            MSN == "NGEIKUS" ~ "Electric Power",
            MSN == "NGTCKUS" ~ "Other"
        ) ) %>%
    dplyr::left_join( tibble( tmp = c( "Electric Power", rep( "Other", 4 ) ),
                              sector = gas_sectors ), by = "tmp" ) %>%
    dplyr::select( -tmp ) %>%
    dplyr::filter( !is.na( sector) ) %>%
    dplyr::mutate( Value = as.numeric( Value ), fuel = "gas" )

# Convert from Btu/cubic foot to TJ/cubic foot, then to kt/cubic foot
gas_unit_conv <- gas_unit_conv %>%
    dplyr::mutate( conv = Value * CONV_FACTOR_NATGAS_HHV_LHV * CONV_TJ_BBTU /
                       conversionFactor_naturalgas_TJ_per_kt_Net )


# Match petroleum to sectors based on ID indicators (manually identified)
oil_unit_conv <- petrol_heat_content %>%
    dplyr::mutate( sector = case_when(
            MSN == "PARCKUS" ~ "Residential",
            MSN == "PACCKUS" ~ "Commercial",
            MSN == "PAICKUS" ~ "Industry",
            MSN == "PAACKUS" ~ "Transportation",
            MSN == "PAEIKUS" ~ "Electric Power"
        ) ) %>%
    dplyr::filter( !is.na( sector ) ) %>%
    dplyr::mutate( Value = as.numeric( Value ), fuel = "oil" )

# Convert from (million btu/short ton) to TJ/kt then Convert from Million BTU/t to TJ/kt
# petrol_heat_content$Unit <- "Million BTU/tonne"
# TODO: make sure this is right
oil_unit_conv <- oil_unit_conv %>%
    dplyr::mutate( conv = Value * CONV_TONNE_BARREL * ( CONV_TJ_BBTU / 1e6 ) * 1e3 )


coal_unit_conv <- EIA_data %>%
    dplyr::filter( fuel %in% c( "coal", "coal_coke" ) ) %>%
    dplyr::mutate( conv = CONV_SHORTTON_TONNE ) %>%
    dplyr::select( sector, fuel, year, conv )

conversion_factors <- rbind( gas_unit_conv, oil_unit_conv ) %>%
    dplyr::select( sector, fuel, year, conv ) %>%
    dplyr::bind_rows( coal_unit_conv )

# ------------------------------------------------------------------------------

# 6. Execute conversion to kt

#    Executes the conversions described in the code block description above

# Convert biomass to kt using CEDS standard conversion factor
# TODO: This will hopefully change--we need an EIA factor since they use diff. reporting
EIA_data$Value[ EIA_data$fuel == "biomass" ] <- as.numeric( EIA_data$Value[ EIA_data$fuel == "biomass" ] ) / CONV_FACTOR_BIO_KT_TJ
EIA_data$Unit[ EIA_data$fuel == "biomass" ] <- "kt"

# Subset the data which can be converted using timeseries EIA conversion data
# EIA_oil_coal_rows <- EIA_data_formatted$fuel %in% c( "oil", "coal" )

# Join conversion factors to their corresponding data rows, then apply
# timeseries conversions
EIA_convert <- EIA_data %>%
    dplyr::left_join( conversion_factors, by = c( "sector", "fuel", "year" ) ) %>%
    dplyr::mutate( Value = Value * conv, Unit = "kt" ) %>%
    dplyr::select( iso, sector, fuel, Unit, year, Value )

stopifnot( !any( is.na( EIA_convert$Value ) ) )

# ------------------------------------------------------------------------------

# 7. Cast to wide

EIA_final <- tidyr::spread( EIA_convert, key = year, value = Value )

EIA_final_CEDSsec <- EIA_final[ EIA_final$sector %in% c( "Commercial", "Residential" ), ]
EIA_final_aggsec <- EIA_final[ !EIA_final$sector %in% c( "Commercial", "Residential" ), ]

# ------------------------------------------------------------------------------

# 8. Write output

writeData( EIA_final_CEDSsec, 'USER_EN_IN', 'A.US-EIA_energy_data_CEDSsec' )
writeData( EIA_final_aggsec, 'USER_EN_IN', 'A.US-EIA_energy_data_aggsec' )


logStop( )
