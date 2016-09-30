#Define historical years variables.

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
# Conversion Factors
conversionFactor_biomass_kt_TJ <- 16  # Biomass - For kt to TJ (multiply by kt to get TJ)    
#conversionFactor_biomass_TJ_kt <- 0.0238846  # For kt to TJ (multiply by kt to get TJ)
# conversionFactor_refinerygas_TJ_per_kt <- 48.5 #Refinery Gas TJ/kt. (Multiply by kt to get TJ)
#49.5 TJ/Gg- 2006 IPCC guidelines for National GHG inventories Vol 2 - Energy, Ch 1 - Intro Table 1.2

conversionFactor_naturalgas_TJ_per_kt <- 44.2 #Natural Gas TJ/kt. (Divide TJ by heating value to get kt)
#44.2 TJ/Gg- 2006 IPCC guidelines for National GHmG inventories Vol 2 - Energy, Ch 1 - Intro Table 1.2

conversionFactor_C_CO2 <- 3.664  # multiply C to get CO2
