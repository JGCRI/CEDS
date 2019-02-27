# ----------------------------------------------------------------------------------
# CEDS R header file: common_data.R
# Date Last Updated: February 25, 2019
# Program Purpose:: Provides common constants and conversion factors
# Input Files: None
# Output Files: None
# Notes:
# TODO:
# -----------------------------------------------------------------------------
# 1. Define historical year(s) variables.

# Set of historical years for which the CEDS system has IEA data.
IEA_years <- 1960:2013
X_IEA_years <- paste0( "X", IEA_years)

IEA_end_year  <- 2013  # Latest year of IEA data; used to compare BP and IEA
X_IEA_end_year  <- paste0( "X", IEA_end_year  )

# The set of years for the BP data extendForwards further, and is used to augment the IEA.
BP_years <- 2014 #The years for which there is only BP data
X_BP_years <- paste0("X", BP_years)

# EDGAR years
EDGAR_start_year <- 1971
EDGAR_end_year <- 2010

# The combined overall yearset for the ceds system
emissions_years <- c(IEA_years,BP_years)
X_emissions_years <- paste0( "X", emissions_years)

start_year = 1960
X_start_year = paste0("X",start_year)
end_year = 2014
X_end_year = paste0("X",end_year)

# Historical extension using CDIAC and RCP
historical_pre_extension_year <- 1750
historical_end_extension_year <- 1965
extended_years <- historical_pre_extension_year:end_year
X_extended_years <- paste0('X',extended_years)

# Bond data years
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

conversionFactor_biomass_kt_TJ <- 16  # Biomass - For kt to TJ (multiply by kt to get TJ)
#conversionFactor_biomass_TJ_kt <- 0.0238846  # For kt to TJ (multiply by kt to get TJ)
# conversionFactor_refinerygas_TJ_per_kt <- 48.5 #Refinery Gas TJ/kt. (Multiply by kt to get TJ)
#49.5 TJ/Gg- 2006 IPCC guidelines for National GHG inventories Vol 2 - Energy, Ch 1 - Intro Table 1.2

conversionFactor_naturalgas_TJ_per_kt <- 44.2 #Natural Gas TJ/kt. (Divide TJ by heating value to get kt)
#44.2 TJ/Gg- 2006 IPCC guidelines for National GHmG inventories Vol 2 - Energy, Ch 1 - Intro Table 1.2

conversionFactor_C_CO2 <- 3.664  # multiply C to get CO2

# TJ per kt, by fuel
# Source: 2006 IPCC guidelines for National GHG inventories Vol 2 - Energy, Ch 1 - Intro Table 1.2
#         https://www.ipcc-nggip.iges.or.jp/public/2006gl/pdf/2_Volume2/V2_1_Ch1_Introduction.pdf
# Notes: (1) TJ/kt (or TJ/Gg) --- Divide TJ by heating value to get kt
#        (2) Subfuels included within a given ceds_fuels are based on "IEA_product_fuel.csv" ("input/energy" directory)

#   biomass = average of municipal waste (renewable and not), charcoal, primary solid biofuels,
#     and other primary solid biofuels
#       Note: No value provided in source for "Industrial waste"
        conversionFactor_municipalwasterenewable_TJ_per_kt <- 11.6 # "Municipal Wastes (biomass fraction)" in source
        conversionFactor_municipalwastenonrenewable_TJ_per_kt <- 10 # "Municipal Wastes (non-biomass fraction)" in source
        conversionFactor_Charcoal_TJ_per_kt <- 29.5 # "Charcoal" in source
        conversionFactor_blackliquour_TJ_per_kt <- 11.8 # "Sulphite lyes (black liquor)" in source
        conversionFactor_woodandwoodwaste_TJ_per_kt <- 15.6 # "Wood/Wood Waste" in source

        primary_solid_biofuels_list <- c(conversionFactor_blackliquour_TJ_per_kt,
                                 conversionFactor_woodandwoodwaste_TJ_per_kt)

        conversionFactor_primarysolidbiofuels_TJ_per_kt <- mean(primary_solid_biofuels_list)

        conversionFactor_nonspecifiedprimarybiofuelsandwaste_TJ_per_kt <- 11.6 # "Other Primary Solid Biomass" in source

    biomass_fuel_list <- c(conversionFactor_municipalwasterenewable_TJ_per_kt,
                       conversionFactor_municipalwastenonrenewable_TJ_per_kt,
                       conversionFactor_primarysolidbiofuels_TJ_per_kt,
                       conversionFactor_nonspecifiedprimarybiofuelsandwaste_TJ_per_kt)

    conversionFactor_biomass_TJ_per_kt <- mean(biomass_fuel_list)

#   brown_coal = average of lignite and peat
#     Note: No value for "brown coal" in report (only "Brown Coal Briquettes" (or BKB)
#           which are included as hard_coal).
      conversionFactor_lignite_TJ_per_kt <- 11.9    #     Lignite ("Lignite" in source)
      conversionFactor_peat_TJ_per_kt <- 9.76       #     Peat ("Peat" in source)

    brown_coal_fuels_list <- c(conversionFactor_lignite_TJ_per_kt, conversionFactor_peat_TJ_per_kt)

    conversionFactor_brown_coal_TJ_per_kt <- mean(brown_coal_fuels_list)

#   coal_coke ("Coke Oven Coke and Lignite Coke" in source)
    conversionFactor_coal_coke_TJ_per_kt <- 28.2

#   diesel_oil, set to "Gas/Diesel Oil" value currently
        conversionFactor_gasdieseloil_TJ_per_kt <- 43 # "Gas/Diesel Oil" in source
        conversionFactor_lubricants_TJ_per_kt <- 40.2 # "Lubricants" in source
        conversionFactor_biodiesels_TJ_per_kt <- 27 # "Biodiesels" in source

    conversionFactor_diesel_oil_TJ_per_kt <- conversionFactor_gasdieseloil_TJ_per_kt

#   hard_coal = average of anthracite, coking_coal, other_bituminous_coal,
#       sub_bituminous_coal, patent_fuel, gas_coke, coal_tar, BKB
#       Note: no value for "hard coal" in report
        conversionFactor_anthracite_TJ_per_kt <- 26.7 # "Anthracite" in source
        conversionFactor_coking_coal_TJ_per_kt <- 28.2 # "Coking Coal" in source
        conversionFactor_other_bituminous_coal_TJ_per_kt <- 25.8 # "Other Bituminous Coal" in source
        conversionFactor_sub_bituminous_coal_TJ_per_kt <- 18.9 # "Sub-Bituminous Coal" in source
        conversionFactor_patent_fuel_TJ_per_kt <- 20.7 # "Patent Fuel" in source
        conversionFactor_gas_coke_TJ_per_kt <- 28.2 # "Gas Coke" in source
        conversionFactor_coal_tar_TJ_per_kt <- 28.0 # "Coal Tar" in source
        conversionFactor_BKB_TJ_per_kt <- 20.7 # "Brown Coal Briquettes" in source

    hard_coal_fuel_list <- c(conversionFactor_anthracite_TJ_per_kt, conversionFactor_coking_coal_TJ_per_kt,
                             conversionFactor_other_bituminous_coal_TJ_per_kt, conversionFactor_sub_bituminous_coal_TJ_per_kt,
                             conversionFactor_patent_fuel_TJ_per_kt, conversionFactor_gas_coke_TJ_per_kt,
                             conversionFactor_coal_tar_TJ_per_kt, conversionFactor_BKB_TJ_per_kt)

    conversionFactor_hard_coal_TJ_per_kt <- mean(hard_coal_fuel_list)

#   heavy_oil = average of all subfuels other than "oil shale and oil sands"
#       Note: No "Crude/NGL/feedstocks (if no detail)" in source
        conversionFactor_oilshaleandoilsands_TJ_per_kt <- 8.9 # "Oil Shale and Tar Sands" in source
        conversionFactor_crudeoil_TJ_per_kt <- 42.3 # "Crude Oil" in source
        conversionFactor_bitumen_TJ_per_kt <- 40.2 # "Bitumen" in source
        conversionFactor_paraffinwaxes_TJ_per_kt <- 40.2 # "Paraffin Waxes" in source
        conversionFactor_petroleumcoke_TJ_per_kt <- 32.5 # "Petroleum Coke" in source
        conversionFactor_otheroilproducts_TJ_per_kt <- 40.2 # "Other Petroleum Products" in source
        conversionFactor_fueloil_TJ_per_kt <- 40.4 # "Residual Fuel Oil" in source

    heavy_oil_fuel_list <- c(conversionFactor_crudeoil_TJ_per_kt, conversionFactor_bitumen_TJ_per_kt,
                             conversionFactor_paraffinwaxes_TJ_per_kt, conversionFactor_petroleumcoke_TJ_per_kt,
                             conversionFactor_otheroilproducts_TJ_per_kt, conversionFactor_fueloil_TJ_per_kt)

    conversionFactor_heavy_oil_TJ_per_kt <- mean(heavy_oil_fuel_list)

#   light_oil = average of all subfuels other than biogasoline and other liquid biofuels
#     Notes: (1) No value for "other hydrocarbons" in report
#            (2) No value for "Additives/blending components" in report
      conversionFactor_refineryfeedstocks_TJ_per_kt <- 43 # "Refinery Feedstocks" in source
      conversionFactor_ethane_TJ_per_kt <- 46.4 # "Ethane" in source
      conversionFactor_liquefiedpetroleumgases_TJ_per_kt <- 47.3 # "Liquefied Petroleum Gases" in source
      conversionFactor_motorgasoline_TJ_per_kt <- 44.3 # "Motor Gasoline" in source
      conversionFactor_aviationgasoline_TJ_per_kt <- 44.3 # "Aviation Gasoline" in source
      conversionFactor_jetfuelgasoline_TJ_per_kt <- 44.3 # "Jet Gasoline" in source
      conversionFactor_jetfuelkerosene_TJ_per_kt <- 44.1 # "Jet Kerosene" in source
      conversionFactor_otherkerosene_TJ_per_kt <- 43.8 # "Other Kerosene" in source
      conversionFactor_naphtha_TJ_per_kt <- 44.5 # "Naphtha" in source
      conversionFactor_whitespiritsandSBP_TJ_per_kt <- 40.2 # "White Spirit and SBP" in source
      conversionFactor_biogasoline_TJ_per_kt <- 27 # "Biogasoline" in source
      conversionFactor_otherliquidbiofuels_TJ_per_kt <- 27.4 # "Other Liquid Biofuels" in source

    light_oil_fuel_list <- c(conversionFactor_refineryfeedstocks_TJ_per_kt, conversionFactor_ethane_TJ_per_kt,
                             conversionFactor_liquefiedpetroleumgases_TJ_per_kt, conversionFactor_motorgasoline_TJ_per_kt,
                             conversionFactor_aviationgasoline_TJ_per_kt, conversionFactor_jetfuelgasoline_TJ_per_kt,
                             conversionFactor_jetfuelkerosene_TJ_per_kt, conversionFactor_otherkerosene_TJ_per_kt,
                             conversionFactor_naphtha_TJ_per_kt, conversionFactor_whitespiritsandSBP_TJ_per_kt)

    conversionFactor_light_oil_TJ_per_kt <- mean(light_oil_fuel_list)

#   natural_gas = currently no default setting for aggregate ceds_fuel natural_gas
#     Note: No "Other recovered gases" in source
      conversionFactor_naturalgasliquids_TJ_per_kt <- 44.2 # "Natural Gas Liquids" in source
      conversionFactor_gasworksgas_TJ_per_kt <- 38.7 # "Gas Works Gas" in source
      conversionFactor_cokeovengas_TJ_per_kt <- 38.7 # "Coke Oven Gas" in source
      conversionFactor_blastfurnacegas_TJ_per_kt <- 2.47 # "Blast Furnace Gas" in source
      conversionFactor_naturalgassubfuel_TJ_per_kt <- 48 # "Natural Gas" in source
      conversionFactor_refinerygas_TJ_per_kt <- 49.5 # "Refinery Gas" in source
      conversionFactor_biogases_TJ_per_kt <- 50.4 # "Landfill Gas", "Sludge Gas" or "Other Biogas" in source

    # conversionFactor_naturalgas_TJ_per_kt_new


