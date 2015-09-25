#------------------------------------------------------------------------------
# Program Name: B1.1.base_SO2_comb_EF.R
# Authors: Jon Seibert, Tyler Pitkanen
# Date Last Updated: July 7, 2015
# Program Purpose: Use the default emissions factors for a specified combustion
# emission species to create an EF file accurately corresponding to the fuels and 
# sectors in A.comb_activity.csv
# Input Files: A.comb_activity.csv, [em]_base_EF.csv, Master_Fuel_Sector_List.xlsx    
# Output Files: B.[em]_comb_EF_db.csv
# Notes: Overwrites a copy of the extended BP data to create matching EF layout.
#        There are not currently any B1.2 scripts to integrate additional process
#        emissions factors. They can be added later without any averse affects.
#        Any such scripts should use the addToEFDb function defined in
#        process_db_functions.R.
# TODO: 
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Before we can load headers we need some paths defined. They may be provided by
#   a system environment variable or may have already been set in the workspace.
    dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
    for ( i in 1:length( dirs ) ) {
        setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
        wd <- grep( 'CEDS/input', list.dirs(), value = T )
        if ( length( wd ) > 0 ) {
            setwd( wd[ 1 ] )
            break
        }
    }
    PARAM_DIR <- "../code/parameters/"
   
# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "analysis_functions.R" ) # Additional function files required.
    log_msg <- "Extrapolating default emissions factors to full dataset" # First message to be printed to the log
    script_name <- "B1.1.base_SO2_comb_EF.R"
    
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Read in files
    # "em" is defined from parent script
    em_lc <- tolower( em )

    activity_data <- readData( "MED_OUT", "A.comb_activity" )
    fuel_efs <- readData( "MAPPINGS", paste0 ( em, "_base_EF" ) ) 
    fuel_list <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Fuels" )

# Check input files for correct naming
    fuelCheck( fuel_efs )

# ------------------------------------------------------------------------------
# 2. Create the base/default emissions factors db (use to fill in iso/fuel/sector/year
# combinations at end).

# List out all fuels
    all_fuels <- fuel_list[[ "fuel" ]]
    
# Remove "process" from the list, if present, to avoid errors,
# as it is not in the fuel_efs list.
    all_fuels <- all_fuels[ all_fuels != "process"]
    
    default_efs <- activity_data

    for ( i in 1:length( all_fuels ) ) {
        default_efs[ default_efs$fuel == all_fuels[[ i ]], X_emissions_years ] <-
            fuel_efs[ fuel_efs$fuel == all_fuels[[ i ]], 
                      paste0( em_lc, "_ef" ) ]
    }
    
    default_efs$units <- paste0( default_efs$units, "/", default_efs$units)

# ------------------------------------------------------------------------------
# 4. Output    

    # Write out the base combustion emissions factors database
    writeData( default_efs, "MED_OUT", paste0( "B.", em ,"_", "comb", "_EF_db" ) )
    
    # NOTE: Users wishing to add more emissions factors data should create a new script
    # entitled B.add_?_EF.R in which to do so.

# Every script should finish with this line
    logStop()
# END
