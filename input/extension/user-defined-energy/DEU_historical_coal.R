#-------------------------------------------------------------------------------
# Program Name: DEU_historical_coal.R
# Author: Caleb Braun
# Date: June 28, 2018
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
# Output File: DEU_historical_coal.csv
#-------------------------------------------------------------------------------

library(dplyr)
library(tidyr)

# for writeData function
setwd('../../')
PARAM_DIR <- '../code/parameters/'
source(paste0(PARAM_DIR, "header.R"))
initialize('DEU_historical_coal.R', NULL, NULL)

FNAME <- 'Primarenergie_Elektrische_Energie'
FPATH <- 'user-defined-energy/'

# Source: https://www.unitjuggler.com/convert-energy-from-tSKE-to-kJ.html
T_HARD_COAL_PER_T_SKE = 1
T_BROWN_COAL_PER_T_SKE = 0.26

schilling <- readData('EXT_IN', paste0(FPATH, FNAME), meta = T)

# Translations for the column names
translate <- c(JAHR = 'year',
               STEINKOHLE = 'hard_coal',
               BRAUNKOHLE = 'brown_coal',
               GESAMT = 'total',
               ERDOEL = 'oil',
               ERDGAS = 'natural_gas',
               WASSERKRAFT.UND.KERNENERGIE = 'hyrdo_and_nuclear')

names(schilling) <- translate[names(schilling)]

out_df <- schilling %>%
    dplyr::select(year, hard_coal, brown_coal) %>%
    dplyr::mutate(hard_coal = hard_coal / T_HARD_COAL_PER_T_SKE * 1000) %>%
    dplyr::mutate(brown_coal = brown_coal / T_BROWN_COAL_PER_T_SKE * 1000) %>%
    dplyr::mutate(iso = 'deu') %>%
    tidyr::gather('CEDS_fuel', 'value', hard_coal, brown_coal) %>%
    tidyr::spread(year, value)

writeData(out_df, 'EXT_IN', 'user-defined-energy/DEU_historical_coal')

logStop()
