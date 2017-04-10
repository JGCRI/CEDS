# ------------------------------------------------------------------------------
# Program Name: H1.2.add_activity_total_natural_gas.R
# Author: Rachel Hoesly
# Program Purpose: Extend Natural Gas data back for all countries from IEA data start (1960, 1971) 
#                 using CDIAC, Bond data, IEA data
#               
# Output Files:  H.EM_total_activity_extended_db
# TODO: 
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers

# Set working directory
dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
for ( i in 1:length( dirs ) ) {
  setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
  wd <- grep( 'CEDS/input', list.dirs(), value = T )
  if ( length(wd) > 0 ) {
    setwd( wd[1] )
    break
    
  }
}
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R","process_db_functions.R", "ModH_extension_functions.R") # Additional function files may be required.
log_msg <- "Extending natural_gas data with bond and IEA" # First message to be printed to the log
script_name <- "H1.2.add_activity_total_natural_gas.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"

# ---------------------------------------------------------------------------
# 0.5. Script Settings needed for data import

extension_fuel_category <- 'natural_gas' # natural_gas, coal, or petroleum
ceds_extension_fuels <- c('natural_gas') # the ceds fuels in the fuel category

# ---------------------------------------------------------------------------
# 1. Load files

activity <- readData( 'MED_OUT',paste0('H.',em,'_total_activity_extended_db') )

ceds_ext_sector_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_sector_ext_map", ".xlsx", sheet_selection = 'CEDS_to_ext',meta = F )

ext_sector_percent_start <- readData( 'EXT_IN', 'ext_sector_percents_start', ".xlsx", sheet_selection = extension_fuel_category ,meta = F)

sector_percent_start <- readData( 'EXT_IN', 'sector_percents_start', ".xlsx", sheet_selection = extension_fuel_category ,meta = F)

iea_other <- readData( 'MED_OUT','A.IEA_CEDS_natural_gas_difference' )

iea_start_year <- readData( 'EXT_IN', 'IEA_start_date', ".xlsx", sheet_selection = extension_fuel_category ,meta = F)

cdiac_total <- readData( 'MED_OUT' , 'E.CO2_CDIAC_inventory')
# ---------------------------------------------------------------------------
# 2. Define variables for script specific fossil fuels

all_countries <- unique(activity$iso)
ext_sectors = c('Power','Industry','RCO','Shipping','Transportation')

# IEA other fuel
iea_other$fuel <- 'other'

# CDIAC variables
cdiac_fuel <- c('gas_fuels')
cdiac <- cdiac_total[which(cdiac_total$fuel %in% cdiac_fuel) , ]

#Extra Bond values (some in common_data.R)
X_extended_bond_years <- paste0('X',1850:bond_end)
bond_merge_start <- 1930

# Year Selection
extension_start_year <- 1850
extension_end_year <- 1970

# ---------------------------------------------------------------------------
# 3. Bond Data processing
# outputs -
    # bond_iso: bond total fuel by iso
    # bond_iso_fuel: bond total fuel by iso, fuel
    # bond_iso_sector_fuel: bond total fuel by iso. fuel, ext_sector      

printLog('Processing Bond data')

bond_data_list <- H.process_bond_data()
list2env( bond_data_list  , envir = .GlobalEnv )

# ---------------------------------------------------------------------------
# 4. CEDS Data processing
# outputs -
      # ceds_total_iso: CEDS total fuel use, including IEA other fuel use, by iso
      # ceds_total_iso_fuel: CEDS total fuel use, including IEA other fuel use, by iso and fuel
      #                      for example: natural_gas and other
      # ceds_total_iso_fuel_sector: CEDS fuel use, NOT including IEA other fuel use, by iso fuel, and ceds sector
      # ceds_total_iso_fuel_ext_sector: CEDS fuel use, NOT including IEA other fuel use, by iso fuel, and extension sector
      # iea_start_year: data frame with the start year of non zero IEA data, either 1960 or 1971, different for each fuel type.

printLog('Processing CEDS data')

ceds_data_list <- H.process_ceds_data()
list2env( ceds_data_list  , envir = .GlobalEnv )

# ---------------------------------------------------------------------------
# 5. Extend CEDS total fuel use (by iso) with cdiac data. For coal, Merge with Bond total by iso
# outputs -
#       final_total_iso: extended total fuel by iso (ceds, extended with CDIAC, including IEA other fuel values) 
#       if coal - CDIAC values merged with Bond totals 

printLog('Extending CEDS totals with CDIAC... Merging CEDS totals with Bond totals')
final_total_iso <- H.extend_merge_ceds_total()

# ---------------------------------------------------------------------------
# 7. Dissaggregate total fuel (ceds fuel and IEA other) into fuel types using CEDS start year split 
# outputs -
    # other_transformation_fuel - extended IEA other fuel
    # final_iso_fuel -  extension of total fuel use by iso (including IEA other values) - start extension year through IEA start year
    # all_iso_fuel - final_iso_fuel merged with ceds total fuel use values. start extsion year through bond end year (2010)

printLog('Disaggregating total fuel into ceds and other fuel')
fuel_disaggregate_list <- H.disaggregate_total()
list2env( fuel_disaggregate_list   , envir = .GlobalEnv )

# ---------------------------------------------------------------------------
# 8. Calculate and Merge Bond Sector Splits/Breakdowns and CEDS aggregate Sector Splits/Breakdowns
#  Sector breakdowns from total fuel type to ext_sector
# outputs - 
  # ext_sector_breakdown: For each iso, fuel, year: the percent of the total combustion fuel use consumed by each extension sector

printLog('Calculating fuel extension sector breakdowns')
ext_sector_breakdown <- H.ext_sector_breakdown()

# ---------------------------------------------------------------------------
# 9. CEDS disaggregate Sector Splits/Breakdowns.
# Disaggregate ext_sector_breakdown to ceds sector breakdowns using CEDS start year data and
# bond_percent_start - self assigned breakdowns from extension sector to CEDS sectors in start year
  # outputs - 
  # sector_breakdown: For each iso, fuel, year: the percent of the total combustion fuel use consumed by each ceds sector
printLog('Calculating fuel sector breakdowns')
sector_breakdown <- H.sector_breakdown()

# ---------------------------------------------------------------------------
# 10. Dissaggregate total CEDS fuel to CEDS sectors
# Dissagregate final_iso_fuel to fuel use by sector using sector_breakdown
  # outputs - 
  # final_iso_fuel_sector: final combustion fuel use 
printLog('Disaggregating total fuel to ceds sectors')
final_iso_fuel_sector_list <- H.disaggregate_to_final()
list2env( final_iso_fuel_sector_list   , envir = .GlobalEnv )

# ---------------------------------------------------------------------------
# 11. Add to database
  # outputs - 
  # final_activity

printLog('Adding final data to activity database')
final_activity <- H.add_to_database()

# ---------------------------------------------------------------------------
# 12.Diagnostic

summed_total <- rbind.fill( final_iso_fuel_sector_full_extension , other_transformation_fuel)
summed_total <- aggregate( summed_total[ paste0('X',extension_start_year:extension_end_year)],
                           by = list(fuel = summed_total$fuel),
                           FUN = sum, na.rm=T)
all_other_tranformation <-  rbind.fill(other_transformation_fuel,iea_other)
all_other_tranformation <- aggregate(all_other_tranformation[paste0('X',extension_start_year:2013)],
                                     by = list(iso = all_other_tranformation$iso),
                                     sum, na.rm=T)
# ---------------------------------------------------------------------------
# 13. Write to database

writeData(final_total_iso, 'DIAG_OUT', paste0('H.Extended_total_', extension_fuel_category ) , meta = F)
writeData( all_other_tranformation , 'DIAG_OUT', paste0('H.Extended_other_tranformation_', extension_fuel_category) , meta = F)
writeData(final_iso_fuel, 'DIAG_OUT', paste0('H.Extended_by_fuel_', extension_fuel_category) , meta = F)
writeData(all_iso_fuel, 'DIAG_OUT', paste0('H.Extended_by_fuel_full_', extension_fuel_category) , meta = F)
writeData(final_iso_fuel_sector_calculated, 'DIAG_OUT', paste0('H.Extended_by_sector_fuel_calculated_', extension_fuel_category) , meta = F)
writeData(final_iso_fuel_sector_full_extension, 'DIAG_OUT', paste0('H.Extended_by_sector_fuel_full_extension_', extension_fuel_category) , meta = F)
writeData(summed_total, 'DIAG_OUT', paste0('H.Extended_dissagregated_by_sector_fuel_aggregated_', extension_fuel_category), meta = F) 

if( !( (nrow(activity) == nrow(final_activity)) & (ncol(activity) == ncol(final_activity)) ) ){
  stop( "New and old activity do not match")
}else if(( (nrow(activity) == nrow(final_activity)) & (ncol(activity) == ncol(final_activity)) ))
{ writeData( final_activity, "MED_OUT" , paste0('H.',em,'_total_activity_extended_db')) }

logStop()

