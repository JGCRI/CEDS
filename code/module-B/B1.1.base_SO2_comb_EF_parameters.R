#------------------------------------------------------------------------------
# Program Name: B1.1.base_SO2_comb_EF_parameters.R
# Authors: Leyang Feng, Jon Seibert, Tyler Pitkanen
# Date Last Updated: August 25, 2019
# Program Purpose: Use the default sulfur content, ash retention, control percentage for a
# specified combustion emission species to create default sulfur content, ash retention, control
# percentage databases corresponding to the fuels and sectors in A.final_comb_activity_modern.csv
# Input Files: A.final_comb_activity_modern.csv, Master_Fuel_Sector_List.xlsx, Master_Fuel_Sector_List.xlsx,
#              [em]_base_EF.csv, fuel_sector_ash_retention_mapping.xlsx
# Output Files: B.[em]_S_Content_db.csv, B.[em]_S_AshRet_db.csv
# Notes:
# TODO: For ash retention, use join instead of for loop
#
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "IO_functions.R", "data_functions.R", "analysis_functions.R" ) # Additional function files required.
    log_msg <- "Extrapolating default emissions factors to full dataset" # First message to be printed to the log
    script_name <- "B1.1.base_SO2_comb_EF_parameters.R"
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "SO2"

# ------------------------------------------------------------------------------
# 1. Read in files and pre processing

# Read in Module A activity data
    activity_data <- readData( "MED_OUT", "A.final_comb_activity_modern" )

# Read in mapping files
    fuel_list <- readData( "MAPPINGS", "Master_Fuel_Sector_List",
                           ".xlsx", sheet_selection = "Fuels" )
    sector_list <- readData( "MAPPINGS", "Master_Fuel_Sector_List",
                             ".xlsx", sheet_selection = "Sectors" )
    fuel_S_Content <- readData( "DEFAULT_EF_IN", paste0 ( em, "_base_EF" ) )
    fuel_AshRet <- readData( "MAPPINGS", "fuel_sector_ash_retention_mapping",
                             ".xlsx", sheet_selection = "fuel" )
    sector_AshRet <- readData( "MAPPINGS", "fuel_sector_ash_retention_mapping",
                               ".xlsx", sheet_selection = "sector" )

# Fill out and combine ash_retention map
    fuel_sector_AshRet <- merge( fuel_AshRet, sector_AshRet,
                                 all = TRUE )
    fuel_sector_AshRet <- unique( fuel_sector_AshRet )
# ------------------------------------------------------------------------------
# 2. Create default sulfur content, ash retention, control percentage databases
# List out all fuels and sectors
    all_fuels <- fuel_list[[ "fuel" ]]
    all_sectors <- sector_list[[ 'sector' ]]
# Remove "process" from the list, if present, to avoid errors,
# as it is not in the fuel_efs list.
    all_fuels <- all_fuels[ all_fuels != "process" ]
# Create the default sulfur content database (use to fill in iso/fuel/sector/year
# combinations at end).
# Set up the layout from activity_data
    default_S_Content <- activity_data
    default_S_Content[ , X_emissions_years ] <- 0
# Fill in the default values
    for ( i in seq_along( all_fuels ) ) {
        default_S_Content[ default_S_Content$fuel == all_fuels[[ i ]],
                           X_emissions_years ] <-
        fuel_S_Content[ fuel_S_Content$fuel == all_fuels[[ i ]],
                        paste0( "S", "_percent" ) ]
    }
# Unit for sulfur content persentage
    default_S_Content$units <- 'kt/kt'
# Create default ash retention database
    default_AshRet <- activity_data
    default_AshRet[ , X_emissions_years ] <- 0

    for ( fuel in all_fuels  ) {  ### This can be done much, much faster w/o for loops
        for ( sector in unique( fuel_sector_AshRet$sector ) ) {
            default_AshRet[ default_AshRet$fuel == fuel &
                              default_AshRet$sector == sector,
                            X_emissions_years ] <-
                fuel_sector_AshRet[ fuel_sector_AshRet$fuel == fuel &
                                      fuel_sector_AshRet$sector == sector,
                                    "AshRet" ]
        }
    }

# Unit for sulfur content percentage
    default_AshRet$units <- 'kt/kt'

# ------------------------------------------------------------------------------
# 4. Output
# Write out all three default databases
    writeData( default_S_Content, "MED_OUT", paste0( "B.", em ,"_", "S_Content_db") )
    writeData( default_AshRet, "MED_OUT", paste0( "B.", em ,"_", "AshRet_db") )
# NOTE: Users wishing to add more sulfur content data should create a new script
# entitled B.add_?_S_Content.R in which to do so.
# Every script should finish with this line
    logStop()
# END
