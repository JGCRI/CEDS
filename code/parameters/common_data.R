# ----------------------------------------------------------------------------------
# CEDS R header file: common_data.R
# Date Last Updated: July 16, 2020
# Program Purpose:: Provides common constants and conversion factors
# Input Files: None
# Output Files: None
# Notes:
# TODO:
# -----------------------------------------------------------------------------

# Define historical years variables.

# Set of historical years for which the CEDS system has IEA data.

# USER SET DATA - Set this to the last year of the IEA data used
IEA_end_year  <- 2022  # Latest year of IEA data; used to compare BP and IEA

IEA_start_year  <- 1960 # First year of IEA data
X_IEA_end_year  <- paste0( "X", IEA_end_year  ) # First Xyear of IEA data
IEA_years <- IEA_start_year:IEA_end_year # Set of IEA years being utilized
X_IEA_years <- paste0( "X", IEA_years) # Set of IEA Xyears being utilized

# USER SET DATA - Set this to the last year of the BP data used
# The set of years for the BP data extendForwards further, and is used to augment the IEA.
BP_last_year <- 2023

# USER SET DATA - Set this to the last year of the EMEP data used
EMEP_last_year <- 2021

# USER SET DATA - Set this to point to the name of the BP statistical data, which should be
# Located in the inputs/energy folder
BP_data_file_name <- "Statistical_Review_of_World_Energy_2024"
BP_detailed_oil_data_file_name <- "Statistical_Review_of_World_Energy_2024-oil-by-product"
# NOTE - Need to also change this file name in the makefile

# The actual last year of data provided in the BP energy data. User need to set an additional value for this object if
# they are utilizing a BP_data_file_name not listed below:
if( BP_data_file_name == "BP_energy_data.xlsx" ){ BP_actual_last_year <- 2014}
if( BP_data_file_name == "bp-stats-review-2022-all-data" ){ BP_actual_last_year <- 2021}
if( BP_data_file_name == "Statistical_Review_of_World_Energy_2023" ){ BP_actual_last_year <- 2022}
if( BP_data_file_name == "Statistical_Review_of_World_Energy_2024" ){ BP_actual_last_year <- 2023}

BP_first_year <- IEA_end_year + 1 # First year for BP only data
BP_years <- BP_first_year : BP_last_year # The years for which there is only BP data

X_BP_years <- paste0( "X", BP_years )
X_BP_last_year <- paste0( "X", BP_last_year )

# If this flag is set then a consistent former USSR line needs to be created during BP sheet processing
BP_FSU_aggregate <- TRUE

# Edgar year
EDGAR_start_year <- 1970
EDGAR_end_year <- 2022       # In the 2022 update the air pollutants go to 2018
EDGAR_end_year_GHG <- 2022   # In the 2023 update, the GHGS go to 202s
EDGAR_end_year_CO2 <- 2022   # IN case CO2 end year is different

# The combined yearset for historical emission years
emissions_years <- c( IEA_years,BP_years )
X_emissions_years <- paste0( "X", emissions_years )

start_year <- IEA_start_year # In present configuration, start year is first year of IEA data
X_start_year <- paste0( "X", start_year )
end_year <- BP_last_year
X_end_year <- paste0( "X", end_year )

# Historical extension using CDIAC and RCP
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

# Andrew cement years
Andrew_start_year <- 1880
Andrew_end_year <- 2021

# Fernandes years
Fernandes_years <- 1850 : 2000
X_Fernandes_years <- paste0( "X", Fernandes_years )

# GAINS years
GAINS_start_year <- '1990'
GAINS_end_year <- '2050'
GAINS_years <- seq( from = GAINS_start_year, to = GAINS_end_year, by = 5 )
X_GAINS_years <- paste0( "X", GAINS_years )

# UNSD years
UNSD_start_year <- 1950
UNSD_end_year <- 1976
UNSD_years <- UNSD_start_year : UNSD_end_year
X_UNSD_years <- paste0( "X", UNSD_years )

# Hyde years
Hyde_start_year <- 1800

# OMI years
OMI_start_year <- 2005
OMI_end_year <- 2019

# Downscale years
downscale_start_year <- 1980

# ---------------------------------
# Define IEA composite region names used within CEDS. These tend to change
# between IEA versions, so are defined in common_data.R.
FSU_IEA_composite_name <- "Former Soviet Union (if no detail)"
FYUG_IEA_composite_name <- "Former Yugoslavia (if no detail)"
Other_African_composite_name <- "Other Africa"
Other_Americas_composite_name <- "Other non-OECD Americas"
Other_Asia_composite_name <- "Other non-OECD Asia"

# ---------------------------------
# Define standardized UN population scenario name being used in system
historical_pop_scenario <- "Estimates"
# Future scenarios options include:
#    "Medium fertility", "High fertility", "Low fertility",
#    "Constant fertility", "Instant-replacement",
#    "Zero-migration", "Constant-mortality", "No change"
future_pop_scenario <- "Medium fertility"

# ---------------------------------
# Define available CEDS releases
# versions include:
# 1) "v2016_07_26"    ---   CEDS Release 1 (CMIP release)
# 2)
# 3) v_2021_02_05
# 4) v_2021_04_21
#
available_CEDS_releases <- c( "v2016_07_26" , "v_2021_04_20", "v_2024_07_08", "v_2025_03_18")

# ---------------------------------
# Country to remove from grid
# If defined, emissions for this iso are removed from the spatial grid
grid_remove_iso <- ""

# Add user defined optional suffix to gridding outputs
user_defined_suffix <- ""

#---------------------------------
# Define conversion factors

conversionFactor_biomass_kt_TJ <- 13.84  # Units = TJ/kt = MJ/kg Biomass - For kt to TJ (multiply by kt of biomass to get TJ)
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

# conversionFactor_refinerygas_TJ_per_kt <- 49.5 #Refinery Gas TJ/kt. (Multiply by kt to get TJ)
#49.5 TJ/Gg- 2006 IPCC guidelines for National GHG inventories Vol 2 - Energy, Ch 1 - Intro Table 1.2
#https://www.ipcc-nggip.iges.or.jp/public/2006gl/pdf/2_Volume2/V2_1_Ch1_Introduction.pdf

conversionFactor_naturalgas_TJ_per_kt_Net <- 48.0 #Natural Gas TJ/kt. (Divide TJ by net heating value (LHV) to get kt)
#48.0 TJ/Gg- 2006 IPCC guidelines for National GHG inventories Vol 2 - Energy, Ch 1 - Intro Table 1.2
#https://www.ipcc-nggip.iges.or.jp/public/2006gl/pdf/2_Volume2/V2_1_Ch1_Introduction.pdf
#Conversion factor for gross heat content since IEA natural gas is provided in TJ-gross
#Natural Gas Gross/Net ~ 1.1078 - Hydrogen Analysis Resource Center, Lower and Higher Heating Values of Hydrogen and Fuels, source: GREET
conversionFactor_naturalgas_TJ_per_kt_Gross <- 48.0*1.1078 #Natural Gas TJ-gross/kt

GJ_per_tonneOilEquivalent <- 42.0 # GJ
tBtu_to_TJ <- 0.94782 * 10^3 # Trillion Btu -> GJ. Unit definition of BTU

conversionFactor_C_CO2 <- 3.664  # multiply C to get CO2

# TJ per Million tonnes of oil equivalent (Mtoe) --> TJ/Mtoe
# Source: IEA Energy Statistics of OECD Countries: Beyond 2020 Documentation (2014 edition), pg. 61
# Note: Multiply Mtoe by conversion factor below to get TJ
conversionFactor_TJ_per_Mtoe <- 4.1868 * (10^4)
