# ------------------------------------------------------------------------------
# Program Name: Paper_Figures_emission_ratios_BC_to_CO.R
# Author(s): Rachel Hoesly
# Date Last Updated: 28 Dec 2016
# Program Purpose: Produces diagnostic figures of emissions ratios BC and NOx over CO
# Input Files: [em]_total_CEDS_emissions.csv
# Output Files: Figures in the diagnostic-output
# Notes:
# TODO: 1.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R",
              'common_data.R', 'IO_functions.R')# Any additional function files required
log_msg <- "Paper figures for emission ratios"
script_name <- "Paper_Figures_emission_ratios.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 0.5 Script Option
em <- 'BC'

# ------------------------------------------------------------------------------
# 1. Load Data

MSL <- readData('MAPPINGS', 'Master_Sector_Level_map')

BC_emissions <- readData('MED_OUT', paste0(em, '_total_CEDS_emissions'))
CO_emissions <- readData('MED_OUT', 'CO_total_CEDS_emissions')

BC_reas <-readData('MED_OUT','E.BC_REAS_inventory')
CO_reas <- readData('MED_OUT','E.CO_REAS_inventory')
# ------------------------------------------------------------------------------
# 2. Define Values

# Unit conversions
# Convert to ng BC/m3/ppb

molar_mass_CO <- 28.01

# countries
countries <- c('kor','jpn','twn','chn')

# ------------------------------------------------------------------------------
# 3. REAS emissions ratios
# Total Emissions
CO_total_reas <- aggregate(CO_reas['X2008'], by = list(iso = CO_reas$iso), sum)
BC_total_reas <- aggregate(BC_reas['X2008'], by = list(iso = BC_reas$iso), sum)

#Check order
if(!identical(CO_total_reas$iso, BC_total_reas$iso)) stop('Data frames are not in identical order, please sort')

# Convert BC from Gg to ng (m3 will cancel out)
BC_total_reas['X2008'] <- BC_total_reas['X2008']*10^18

# Convert OC from Gg to ppb (m3 will cancel out)
# 24.45 is from the ideal gas law (PV=nRT)
# (10^9*10^6) is to convert from Gg to ug
CO_total_reas['X2008'] <- CO_total_reas['X2008']*24.45*10^9*10^6/molar_mass_CO

# Calculate
BC_over_CO_total_reas <- BC_total_reas
BC_over_CO_total_reas['X2008'] <- BC_total_reas['X2008']/CO_total_reas['X2008']

BC_over_CO_total_reas <- melt(BC_over_CO_total_reas[c('iso','X2008')], id.vars = c('iso'))
names(BC_over_CO_total_reas)[which(names(BC_over_CO_total_reas) == 'value')] <- 'ratio'
names(BC_over_CO_total_reas)[which(names(BC_over_CO_total_reas) == 'variable')] <- 'year'
reas_results <- BC_over_CO_total_reas[which(BC_over_CO_total_reas$iso %in% countries),]

# ------------------------------------------------------------------------------
# 3. CEDS emissions ratios
# Total Emissions

years <- 2008:2014
X_years <- paste0('X',years)

CO_total <- aggregate(CO_emissions[X_extended_years], by = list(iso = CO_emissions$iso), sum)
BC_total <- aggregate(BC_emissions[X_extended_years], by = list(iso = BC_emissions$iso), sum)

#Check order
if(!identical(CO_total$iso, BC_total$iso)) stop('Data frames are not in identical order, please sort')

# Convert BC from Gg to ng (m3 will cancel out)
BC_total[X_extended_years] <- BC_total[X_extended_years]*10^18

# Convert OC from Gg to ppb (m3 will cancel out)
# 24.45 is from the ideal gas law (PV=nRT)
# (10^9*10^6) is to convert from Gg to ug
CO_total[X_extended_years] <- CO_total[X_extended_years]*24.45*10^9*10^6/molar_mass_CO

# Calculate
BC_over_CO_total <- BC_total
BC_over_CO_total[X_extended_years] <- BC_total[X_extended_years]/CO_total[X_extended_years]

BC_over_CO_ratio <- melt(BC_over_CO_total[which(BC_over_CO_total$iso %in% countries),
                                          c('iso','X2008')], id.vars = c('iso'))
names(BC_over_CO_ratio)[which(names(BC_over_CO_ratio) == 'value')] <- 'ratio'
names(BC_over_CO_ratio)[which(names(BC_over_CO_ratio) == 'variable')] <- 'year'

BC_over_CO_ratio_all <- melt(BC_over_CO_total[which(BC_over_CO_total$iso %in% countries),
                                              c('iso',X_years)], id.vars = c('iso'))
names(BC_over_CO_ratio_all)[which(names(BC_over_CO_ratio_all) == 'value')] <- 'ratio'
names(BC_over_CO_ratio_all)[which(names(BC_over_CO_ratio_all) == 'variable')] <- 'year'
BC_over_CO_ratio_all_all <- aggregate(BC_over_CO_ratio_all['ratio'],
                                      by = list(iso = BC_over_CO_ratio_all$iso), mean)
BC_over_CO_ratio_all_all$year <- '2009-2014'
ceds_results <- rbind.fill(BC_over_CO_ratio,BC_over_CO_ratio_all_all)
ceds_results <- ceds_results[order(ceds_results$iso),]
ceds_results
# ------------------------------------------------------------------------------
# 4. Final results table
ceds_results$Inventory <- 'CEDS'
reas_results$Inventory <- 'REAS2.1'
results <- rbind(ceds_results,reas_results)
results <- results[order(results$iso),]
results


kor_bc_reas <- BC_reas[which(BC_reas$iso == 'kor'),c('sector','X2008')]
kor_bc_reas <- aggregate(kor_bc_reas$X2008, by = list(kor_bc_reas$sector), sum)

kor_bc <- BC_emissions[which(BC_emissions$iso == 'kor'),c('sector','X2008')]
kor_bc$agg_sector <- MSL[match(kor_bc$sector,MSL$working_sectors_v1),'Figure_sector']

kor_bc <- aggregate(kor_bc$X2008, by = list(kor_bc$agg_sector), sum)
kor_bc$percent <-kor_bc$x/sum(kor_bc$x) *100
kor_bc


kor_co <- CO_emissions[which(CO_emissions$iso == 'kor'),c('sector','X2008')]
kor_co$agg_sector <- MSL[match(kor_co$sector,MSL$working_sectors_v1),'Figure_sector']

kor_co <- aggregate(kor_co$X2008, by = list(kor_co$agg_sector), sum)
kor_co$percent <-kor_co$x/sum(kor_co$x) *100
kor_co

kor_ratio <- kor_co[c('Group.1','x')]
kor_ratio$x <- kor_bc$x/kor_co$x*(10^18)/(24.45*10^9*10^6/molar_mass_CO)
kor_ratio
# ------------------------------------------------------------------------------
# 6. End Script

logStop()
