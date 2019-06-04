#------------------------------------------------------------------------------
# Program Name: H3.2.proc_Extended_Emissions.R
# Author: Rachel Hoesly
# Date Last Updated: 15 Nov 2016
# Program Purpose: Process extendtion EFs database to finalize and sort CEDS EFs database.
# Input Files: H.[em]_total_EFs_extended_adjusted-pathway.csv,
#              A.total_activity_extended.csv
# Output Files: H.SO2_final_other_tranformation_emissions.csv,
#               H.[em]_total_CEDS_emissions_before_other_transformation_replacement.csv,
#               H.CO2_final_other_transformation_emissions.csv,
#               [em]_total_CEDS_emissions.csv
# Notes:
# TODO:
# ------------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c('data_functions.R') # Additional function files required.
log_msg <- paste0( "Processing CEDS extension EFs database" ) # First message to be printed to the log
script_name <- "H3.2.proc_Extended_Emissions.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "CO2"

# ------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# 1. Load files

EFs <- readData( 'MED_OUT',paste0('H.',em,'_total_EFs_extended_adjusted-pathway') )
activity <- readData( 'MED_OUT', 'A.total_activity_extended' )

# ---------------------------------------------------------------------------
# 2. Sort
# remove other transformation from EFs
EFs <- EFs %>%
    filter( !(sector == '1A1bc_Other-transformation' &
              fuel != 'process') ) %>%
    filter( sector != '1A1bc_Other-feedstocks') %>%
    arrange(iso, sector, fuel)

activity <- activity %>%
    filter( sector != '1A1bc_Other-feedstocks') %>%
    arrange(iso, sector, fuel)


check_iso <- identical( activity$iso, EFs$iso)
check_sector <- identical( activity$sector, EFs$sector)
check_fuel <- identical( activity$fuel, EFs$fuel)

if( !all( c(check_iso, check_sector, check_fuel) )) stop("Activity and EFs databases do not match.")

emissions <- EFs[,c('iso','sector','fuel')]
emissions$units <- 'kt'
emissions[X_extended_years] <- EFs[X_extended_years] * activity[X_extended_years]

# ---------------------------------------------------------------------------
# 5. Replace SO2 and CO2 other transformation emission

# SO2
if( em == 'SO2'){
  MODULE_H <- "../code/module-H/"
  source_child <- function( file_name ){ source( paste( MODULE_H, file_name, sep = "" ) ) }
  source_child ('H3.3.add_emissions_SO2_other_transformation.R' )

  other_transformation_emissions_calculated <- readData('MED_OUT', 'H.SO2_calculated_other_transformation_emissions')

  other_transformation_emissions_extended <- emissions[which(emissions$sector == "1A1bc_Other-transformation"),]

  # Take Max of calculated and extended Other transformation emissions
  both_estimates <- rbind.fill(other_transformation_emissions_calculated,other_transformation_emissions_extended)[
                                          , c('iso','sector','fuel',paste0('X',1750:end_year)) ]

  final_other_transformation <- aggregate(both_estimates[paste0('X',1750:end_year)],
                                         by = list(iso = both_estimates$iso,
                                                   sector = both_estimates$sector,
                                                   fuel = both_estimates$fuel),
                                         FUN = max)

  emissions <- replaceValueColMatch(emissions, final_other_transformation,
                                    x.ColName = paste0('X',1750:end_year),
                                    match.x = c('iso','sector','fuel'),
                                    addEntries = F)

 writeData(final_other_transformation , 'DIAG_OUT', 'H.SO2_final_other_tranformation_emissions')

}

# CO2: Set CO2_1A1bc = max( 1A1bc_extended, CO2_Conversion )
if (em == "CO2") {
  writeData( emissions, "MED_OUT" , paste0( 'H.', em,'_total_CEDS_emissions_before_other_transformation_replacement') )

  MODULE_H <- "../code/module-H/"
  source_child <- function( file_name ){ source( paste( MODULE_H, file_name, sep = "" ) ) }
  source_child ('H3.3.add_emissions_CO2_other_transformation.R' )

  other_transformation_emissions_calculated <- readData('MED_OUT', 'H.CO2_calculated_other_transformation_emissions')

  other_transformation_emissions_extended <- emissions[which(emissions$sector == "1A1bc_Other-transformation"),]

  # Take Max of calculated and extended Other transformation emissions
  both_estimates <- rbind.fill(other_transformation_emissions_calculated,other_transformation_emissions_extended)[
    , c('iso','sector','fuel',paste0('X',1750:end_year)) ]

  final_other_transformation <- aggregate(both_estimates[paste0('X',1750:end_year)],
                                         by = list(iso = both_estimates$iso,
                                                   sector = both_estimates$sector,
                                                   fuel = both_estimates$fuel),
                                         FUN = max)

  emissions <- replaceValueColMatch(emissions, final_other_transformation,
                                    x.ColName = paste0('X',1750:end_year),
                                    match.x = c('iso','sector','fuel'),
                                    addEntries = F)

  writeData(final_other_transformation , 'DIAG_OUT', 'H.CO2_final_other_transformation_emissions')
}


# ---------------------------------------------------------------------------
# 6. Write to file

writeData( emissions, "MED_OUT" , paste0(em,'_total_CEDS_emissions') )
