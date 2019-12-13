#-------------------------------------------------------------------------------
# Program Name: A8.1a.DEU_historical_coal.R
# Author: Caleb Braun and Patrick O'Rourke
# Date: October 11, 2019
#
# Transforms raw DEU historical coal data into a CEDS format .csv file.
#
# Prepares historical data on coal consumption in Germany going back to 1860 for
# use as a user-added file in the energy-extension module of CEDS. The original
# data is from Primarenergie Elektrische Energie - Germany Table SKE (Schilling
# et al. 1977) and a digital scan of the relevant pages are in this directory.
# The input file "Primarenergie_Elektrische_Energie.csv" was created with the
# OCR tool from Adobe Acrobat DC as well as some transcription by hand.
#
# The data as presented by Schilling et al. is in units of Mt SKE, and must be
# converted before adding to CEDS, which requires units of kt. Further, the
# data must be translated and converted to wide-form.
#
# Input File:  Primarenergie_Elektrische_Energie.csv
# Output File: A.DEU_historical_coal.csv, A.DEU_historical_brown_coal.csv
#-------------------------------------------------------------------------------
# 1.) Set directory, constants, load data, and make mapping

# for writeData function
setwd( '../../' )
PARAM_DIR <- '../code/parameters/'
source( paste0( PARAM_DIR, "header.R" ) )
initialize( 'A8.1a.DEU_historical_coal.R', NULL, NULL )

FNAME <- 'Primarenergie_Elektrische_Energie'

# Source: https://www.unitjuggler.com/convert-energy-from-tSKE-to-kJ.html
T_HARD_COAL_PER_T_SKE = 1
T_BROWN_COAL_PER_T_SKE = 0.26

schilling <- readData( 'USER_EN_PROCESS', FNAME, meta = T )

# Translations for the column names
translate <- c( JAHR = 'year',
                STEINKOHLE = 'hard_coal',
                BRAUNKOHLE = 'brown_coal',
                GESAMT = 'total',
                ERDOEL = 'oil',
                ERDGAS = 'natural_gas',
                WASSERKRAFT.UND.KERNENERGIE = 'hyrdo_and_nuclear' )

#-------------------------------------------------------------------------------

# 2.) Process data

names( schilling ) <- translate[ names( schilling ) ]

hard_and_brown_coal <- schilling %>%
    dplyr::select( year, hard_coal, brown_coal ) %>%
    dplyr::mutate( hard_coal = hard_coal / T_HARD_COAL_PER_T_SKE * 1000 ) %>%
    dplyr::mutate( brown_coal = brown_coal / T_BROWN_COAL_PER_T_SKE * 1000 ) %>%
    dplyr::mutate( iso = 'deu' ) %>%
    tidyr::gather( 'CEDS_fuel', 'value', hard_coal, brown_coal ) %>%
    tidyr::spread( year, value )

# Create data frames for Brown Coal, Hard Coal, and Total Coal (Brown + Hard, has hard is
# assumed to contain the coal consumption for CEDS hard coal and coal coke)
column_names <- colnames( hard_and_brown_coal )
year_columns <- subset( column_names, !( column_names %in% c( "iso", "CEDS_fuel" ) ) )

total_coal <- hard_and_brown_coal %>%
    dplyr::mutate( agg_fuel = "coal" ) %>%
    dplyr::select( -CEDS_fuel ) %>%
    dplyr::group_by( iso, agg_fuel ) %>%
    dplyr::summarise_all( sum, na.rm = TRUE ) %>%
    dplyr::select( iso, agg_fuel, year_columns )

brown_coal <- hard_and_brown_coal %>%
    dplyr::filter( CEDS_fuel == "brown_coal" )

hard_coal <- hard_and_brown_coal %>%
    dplyr::filter( CEDS_fuel == "hard_coal" )

#-------------------------------------------------------------------------------

#3.) Write data

writeData( total_coal, 'USER_EN_IN', 'A.DEU_historical_coal' )
writeData( brown_coal, 'USER_EN_IN', 'A.DEU_historical_brown_coal' )
# writeData( hard_coal, 'USER_EN_IN', 'A.DEU_historical_hard_coal' )

logStop( )
