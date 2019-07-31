# ----------------------------------------------------------------------------------
# CEDS R header file: common_data.R
# Date Last Updated: July 31, 2019
# Program Purpose:: Provides common constants and conversion factors
# Input Files: None
# Output Files: None
# Notes:
# TODO:
# -----------------------------------------------------------------------------
# 1. Define historical year(s) variables.

#Set of historical years for which the CEDS system has IEA data.
IEA_years <- 1960:2013
X_IEA_years <- paste0( "X", IEA_years)

IEA_end_year  <- 2013  # Latest year of IEA data; used to compare BP and IEA
X_IEA_end_year  <- paste0( "X", IEA_end_year  )

#The set of years for the BP data extendForwards further, and is used to augment the IEA.
BP_years <- 2014 #The years for which there is only BP data
X_BP_years <- paste0("X", BP_years)

#Edgar year
EDGAR_start_year <- 1971
EDGAR_end_year <- 2010

#The combined overall yearset for the ceds system
emissions_years <- c(IEA_years,BP_years)
X_emissions_years <- paste0( "X", emissions_years)

start_year = 1960
X_start_year = paste0("X",start_year)
end_year = 2014
X_end_year = paste0("X",end_year)

# historical extension using CDIAC and RCP
historical_pre_extension_year <- 1750
historical_end_extension_year <- 1965
extended_years <- historical_pre_extension_year:end_year
X_extended_years <- paste0('X',extended_years)

# Bond Data Years
bond_start <- 1850
bond_end <- 2010
bond_years <- seq.int(bond_start,bond_end, by = 5)
X_bond_years <- paste0('X', bond_years)

# CDIAC years
cdiac_start_year <- 1750
cdiac_end_year <- 2011
cdiac_end_year_cement <- 2015



#---------------------------------
# Define conversion factors

# 1.) TJ per kt (or TJ per Gg), by fuel
#   Notes: (1) TJ/kt (or TJ/Gg) --- Divide TJ by heating value to get kt

#   Biomass (16 TJ/kt)
    conversionFactor_biomass_kt_TJ <- 16
#   conversionFactor_biomass_TJ_kt <- 0.0238846  # For kt to TJ (multiply by kt to get TJ)

#   Natural Gas TJ/kt. (44.2 TJ/kt) -- Used only for CO2 currently until CO2 branch merged in
    conversionFactor_naturalgas_TJ_per_kt <- 44.2

#   Natural Gas TJ-net/kt. (48 TJ-net/kt)
#       Source: 2006 IPCC guidelines for National GHG inventories Vol 2 - Energy, Ch 1 - Intro Table 1.2
#       https://www.ipcc-nggip.iges.or.jp/public/2006gl/pdf/2_Volume2/V2_1_Ch1_Introduction.pdf
    conversionFactor_naturalgas_TJ_per_kt_net <- 48

#   Natural Gas TJ-gross/kt.
#       Notes: 52.8 TJ-gross/kt = conversionFactor_naturalgas_TJ_per_kt_net * 1.1
    conversionFactor_naturalgas_TJ_per_kt_gross <- 52.8

# conversionFactor_refinerygas_TJ_per_kt <- 49.5 #Refinery Gas TJ/kt. (Multiply by kt to get TJ)
# 49.5 TJ/Gg- 2006 IPCC guidelines for National GHG inventories Vol 2 - Energy, Ch 1 - Intro Table 1.2

# 2.) C to CO2

conversionFactor_C_CO2 <- 3.664  # multiply C to get CO2

# 3.) TJ per Million tonnes of oil equivalent (Mtoe) --> TJ/Mtoe
# Source: IEA Energy Statistics of OECD Countries: Beyond 2020 Documentation (2014 edition), pg. 61
# Note: Multiply Mtoe by conversion factor below to get TJ
conversionFactor_TJ_per_Mtoe <- 4.1868 * (10^4)





