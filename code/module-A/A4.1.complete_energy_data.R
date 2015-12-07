#------------------------------------------------------------------------------
# Program Name: A4.1.complete_energy_data.R
# Author(s): Jon Seibert
# Date Last Modified: July 22, 2015
# Program Purpose: To expand IEA_BP energy data to include entries for all possible
#                  id combinations.
# Input Files: A.comb_activity.csv 
# Output Files: A.comb_activity.csv 
# Notes: 
# TODO: DEAL WITH BIOMASS NA SECTOR, 
#       DEAL WITH "PROCESS" SECTORS IN ENERGY_DATA- MAKE PROCESS-ONLY / TREAT AS PROCESS DATA?
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Set working directory to the CEDS “input” directory and define PARAM_DIR as the
# location of the CEDS “parameters” directory relative to the new working directory.
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
    log_msg <- "Completion of all combustion data entries" # First message to be printed to the log
    script_name <- "A4.1.complete_energy_data.R"
    
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Read in files

input_path <- paste0( getwd(), "/mappings/" )
energy_data <- readData( "MED_OUT", "A.IEA_BP_energy_ext" ) 
MFL <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Fuels" )
MSL <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Sectors" )
MCL <- readData( "MAPPINGS", "Master_Country_List" )

# ------------------------------------------------------------------------------
# 2. Separate combustion and activity energy data

energy_data_combustion <- energy_data[which(energy_data$fuel != 'process'),]
energy_data_activity <- energy_data[which(energy_data$fuel == 'process'),]

# ------------------------------------------------------------------------------
# 3. Create combustion activity database - Populate missing iso-sector-fuel combinations

# Build empty energy data database with all combinations

# Whether these lists include biomass and/or process sectors is easily editable- 
# left out for now via reasonable assumption.
iso_list <- unique( MCL$iso )
sector_list <- unique( c( MSL$sector[ MSL$activity == "Energy_Combustion" ], 
                          unique( energy_data_combustion$sector[ energy_data_combustion$sector != "NA" ] ) ) )
# fuel_list <- unique( MFL$fuel[ MFL$fuel != "process" & MFL$fuel != "biomass" ] )
fuel_list <- unique( MFL$fuel[ MFL$fuel != "process" ] )

# Remove NA-sector biomass entries for now
energy_data_combustion <- na.omit( energy_data_combustion[ energy_data_combustion$sector != "NA", ] )

# Use header function to generate blank template data frame
template <- buildCEDSTemplate( iso_list, sector_list, fuel_list )

# Insert existing data into blank template
full_energy_data_combustion <- merge(template, energy_data_combustion, by = c("iso","sector","fuel"), all.x = TRUE, suffixes = c(".t",".e"))

original_names <- c( "iso", "sector", "fuel", "units", X_emissions_years)
merge_names <- c( "iso", "sector", "fuel", "units.t", paste0( X_emissions_years, ".e" ) )

# Extract energy data columns and rename
full_energy_data_combustion <- full_energy_data_combustion[ merge_names ]
names( full_energy_data_combustion ) <- original_names

# Replace merge-generated NAs with 0
full_energy_data_combustion[ is.na( full_energy_data_combustion ) ] <- 0

# Sort
full_energy_data_combustion <- full_energy_data_combustion[ with( full_energy_data_combustion, order( iso, sector, fuel ) ), ]

# ------------------------------------------------------------------------------
# 3. Output
# Add comments for each table
comments.A.comb_activity <- c( paste0( "IEA energy statistics", 
        " by intermediate sector / intermediate fuel / historical year,",
        " extended with BP data and filled out with all combustion iso-sector-fuel combinations." ) )
                                            
# write out data
writeData( full_energy_data_combustion, domain = "MED_OUT", fn = "A.comb_activity", comments = comments.A.comb_activity )
writeData( energy_data_activity, domain = "MED_OUT", fn = "A.NC_activity_energy", comments = comments.A.comb_activity )
    
logStop()

# END
