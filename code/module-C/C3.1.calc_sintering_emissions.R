# ------------------------------------------------------------------------------
# Program Name: C3.1.calc_sintering_emissions.R
# Author: Andrea Mott
# Date Last Updated: 17 August 2021
# Program Purpose: Process sintering emissions based on sintering production data.
# Input Files:  A.Sintering_production.csv
# Output Files: C.(em)_sintering_emissions.csv
# TODO:

# -----------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "common_data.R", "data_functions.R", "analysis_functions.R",
              "process_db_functions.R", "timeframe_functions.R",
              "interpolation_extension_functions.R" ) # Additional function files may be required.
log_msg <- "Process pig iron production"
script_name <- "C3.1.calc_sintering_emissions.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"

# ---------------------------------------------------------------------------

# input kt sinter
kt_sinter <- readData( "MED_OUT", "A.Sintering_production")

kt_sinter_long <- kt_sinter %>%
    select(-c(sector, units)) %>%
    gather(key = "year", value = "kt_sinter", -iso)

EF <- readData('DIAG_OUT', paste0( 'C.', em, '_NC_User_Added_EF'))

EF_long <- EF %>%
    filter( sector == "2C1_Iron-steel-alloy-prod_sintering") %>%
    select(-c(sector,fuel,units)) %>%
    gather(key = "year", value = "EF", -iso)

# Calculate sintering emissions
sint_em <- kt_sinter_long %>%
    left_join(EF_long, by = c("iso","year")) %>%
    mutate(em = kt_sinter * EF) %>%
    select(iso,year,em) %>%
    na.omit() %>%
    spread(key = "year","em") %>%
    mutate(sector = "sintering") %>%
    mutate(units = "kt*kt/kt") %>%
    mutate(fuel = "process") %>%
    select(iso, sector, fuel, units, everything())

# Write out sintering emissions
writeData( sint_em, "MED_OUT", paste0( "C.", em, "_sintering_emissions"))

logStop()

#END
