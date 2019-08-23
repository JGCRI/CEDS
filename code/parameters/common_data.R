# Define historical years variables.

# Set of historical years for which the CEDS system has IEA data.

# USER SET DATA - Set this to the last year of the IEA data used
IEA_end_year  <- 2013  # Latest year of IEA data; used to compare BP and IEA

IEA_start_year  <- 1960
X_IEA_end_year  <- paste0( "X", IEA_end_year  )
IEA_years <- IEA_start_year:IEA_end_year
X_IEA_years <- paste0( "X", IEA_years)

# USER SET DATA - Set this to the last year of the BP data used
# The set of years for the BP data extendForwards further, and is used to augment the IEA.
BP_last_year <- 2014 # Last years for BP data

# USER SET DATA - Set this to point to the name of the BP statisical data, which should be
# Located in the inputs/energy folder
BP_data_file_name <- "bp-stats-review-2019-all-data"

BP_first_year <- IEA_end_year + 1 # First year for BP only data
BP_years <- BP_first_year : BP_last_year # The years for which there is only BP data

X_BP_years <- paste0( "X", BP_years )
X_BP_last_year <- paste0( "X", BP_last_year )

# If this flag is set then a consistent former USSR line needs to be created during BP sheet processing
BP_FSU_aggregate <- TRUE

# Edgar year
EDGAR_start_year <- 1971
EDGAR_end_year <- 2010

# The combined yearset for historical emission years
emissions_years <- c( IEA_years,BP_years )
X_emissions_years <- paste0( "X", emissions_years )

start_year <- IEA_start_year # In present configuration, start year is first year of IEA data
X_start_year <- paste0( "X", start_year )
end_year <- BP_last_year
X_end_year <- paste0( "X", end_year )

# historical extension using CDIAC and RCP
historical_pre_extension_year <- 1750
historical_end_extension_year <- 1965
extended_years <- historical_pre_extension_year : end_year
X_extended_years <- paste0( 'X', extended_years )

# Bond Data Years
bond_start <- 1850
bond_end <- 2010
bond_years <- seq.int(bond_start,bond_end, by = 5)
X_bond_years <- paste0('X', bond_years)

# CDIAC years
cdiac_start_year <- 1750
cdiac_end_year <- 2011
cdiac_end_year_cement <- 2015

# ---------------------------------
# Define available CEDS releases
# versions include:
# 1) "v2016_07_26"    ---   CEDS Release 1 (CMIP release)
available_CEDS_releases <- c( "v2016_07_26" )


#---------------------------------
# Conversion Factors

conversionFactor_biomass_kt_TJ <- 13.84  # TJ/kt = MJ/kg Biomass - For kt to TJ (multiply by kt of biomass to get TJ)
# Default biomass conversion factor. In general, to be used when other data is not available
# Note that A1.2.Fernandes_biomass.R produces region-sepcific heat contents for residential sector
# Wood conversion factor from Fernandes et al. spreadsheet.
# This corresponds to LHV for wood at a typical 20-25% moisture content (wood stored for one year)
# Wood Fuels Handbook - FAO (Krajnc 2015)

# GAINS defaults (MJ-net/kg):
 GAINS_conversionFactor_fuelwood_MJ_per_kg <- 17.6
# GAINS_conversionFactor_dung_MJ_per_kg <- 13.1
# GAINS_conversionFactor_agricultural_residues_MJ_per_kg <- 14.6
# GAINS_conversionFactor_charcoal_MJ_per_kg - 25.7

# conversionFactor_refinerygas_TJ_per_kt <- 48.5 #Refinery Gas TJ/kt. (Multiply by kt to get TJ)
#49.5 TJ/Gg- 2006 IPCC guidelines for National GHG inventories Vol 2 - Energy, Ch 1 - Intro Table 1.2

conversionFactor_naturalgas_TJ_per_kt_Net <- 48.0 #Natural Gas TJ/kt. (Divide TJ by net heating value (LHV) to get kt)
#48.0 TJ/Gg- 2006 IPCC guidelines for National GHG inventories Vol 2 - Energy, Ch 1 - Intro Table 1.2
#Conversion factor for gross heat content since IEA natural gas is provided in TJ-gross
#Natural Gas Gross/Net ~ 1.1078 - Hydrogen Analysis Resource Center, Lower and Higher Heating Values of Hydrogen and Fuels, source: GREET
conversionFactor_naturalgas_TJ_per_kt_Gross <- 48.0*1.1078 #Natural Gas TJ-gross/kt

GJ_per_tonneOilEquivalent <- 42.0 # GJ
tBtu_to_TJ <- 0.94782 * 10^3 # Trillion Btu -> GJ. Unit definition of BTU

conversionFactor_C_CO2 <- 3.664  # multiply C to get CO2
