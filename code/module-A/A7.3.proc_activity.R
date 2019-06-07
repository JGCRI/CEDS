#------------------------------------------------------------------------------
# Program Name: A7.3.proc_activity.R
# Author: Rachel Hoesly
# Date Last Updated: June 4, 2019
# Program Purpose: Process extention activity database to finalize and sort CEDS activity database.
# Input Files: A.NC_activity_extended_db.csv
# Output Files: A.NC_default_activity_extended.csv
# Notes:

# ------------------------------------------------------------------------------------
# 0. Read in global settings and headers
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
script_name <- "A7.3.proc_activity.R"
log_msg <- "Processing CEDS extension activity database"
headers <- NULL

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 1. Load files

activity_all <- readData( 'MED_OUT', paste0( 'A.NC_activity_extended_db' ),
                          missing_value = "NA" )

# ---------------------------------------------------------------------------
# 2. Replace NAs
#   - The %<>% operator is used x %<>% f(), and is identical to x <- x %>% f()
#   - The coalesce function replaces NAs with the second argument

non_cdiac_iso <- c("asm", "cuw", "esh", "global", "gmb", "gum", "reu",
                   "srb (kosovo)", "ssd", "sxm", "tkl", "vir", "pse")
non_cdiac_rows <- activity_all$iso %in% non_cdiac_iso
activity_all[ non_cdiac_rows, X_extended_years ] %<>% mutate_all( coalesce, 0 )

natural_emissions_sectors <- c("11A_Volcanoes", "11B_Forest-fires", "11C_Other-natural")
natural_em_rows <- activity_all$sector %in% natural_emissions_sectors
activity_all[ natural_em_rows, X_extended_years ] %<>% mutate_all( coalesce, 0 )

# ---------------------------------------------------------------------------
# 3. Sort

if( anyNA( activity_all ) ) {
   printLog( 'NAs found in activity data, converting to 0' )
   activity_all[ is.na( activity_all ) ] <- 0
}

final <- activity_all[ with( activity_all, order( iso, sector, fuel ) ), ]

# ---------------------------------------------------------------------------
# 4. Write to file

writeData( final, "MED_OUT", 'A.NC_default_activity_extended' )

logStop()
