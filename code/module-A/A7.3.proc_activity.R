#------------------------------------------------------------------------------
# Program Name: A7.3.proc_activity.R
# Author: Rachel Hoesly
# Date Last Updated: March 22, 2016
# Program Purpose: Process extention activity database to finalize and sort CEDS activity database.
# Input Files: None
# Output Files: None
# Notes:
# TODO:
# ------------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "process_db_functions.R" ) # Additional function files required.
log_msg <- paste0( "Processing CEDS extension activity database" ) # First message to be printed to the log
script_name <- "A7.3.proc_activity"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 1. Load files

activity_all <- readData( 'MED_OUT',paste0('A.NC_activity_extended_db') )

# ---------------------------------------------------------------------------
# 2. Replace NAs
non_cdiac_iso <- c("asm" ,"cuw","esh", "global","gmb","gum","reu" ,"srb (kosovo)" ,"ssd", "sxm", "tkl", "vir","pse")

activity_all[which( activity_all$iso %in% non_cdiac_iso),X_extended_years] <-
         replace( activity_all[which( activity_all$iso %in% non_cdiac_iso),X_extended_years],
         is.na(activity_all[which( activity_all$iso %in% non_cdiac_iso),X_extended_years]),
         0)

# natural emission
natural_emissions_sectors <- c("11A_Volcanoes" , "11B_Forest-fires" , "11C_Other-natural")
activity_all[which( activity_all$sector %in% natural_emissions_sectors),X_extended_years] <-
  replace( activity_all[which( activity_all$sector %in% natural_emissions_sectors),X_extended_years],
           is.na(activity_all[which( activity_all$sector %in% natural_emissions_sectors),X_extended_years]),
           0)

# ---------------------------------------------------------------------------
# 2. Sort

if( anyNA( activity_all ) ) {
   printLog( 'NAs found in activity data, converting to 0' )
   activity_all[ is.na( activity_all ) ] <- 0
}

final <- activity_all[ with( activity_all, order( iso, sector, fuel ) ), ]

# ---------------------------------------------------------------------------
# 5. Write to file

writeData( final, "MED_OUT", 'A.NC_default_activity_extended')

logStop()
