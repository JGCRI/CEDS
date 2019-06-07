# ------------------------------------------------------------------------------
# Program Name: E.EMEP_as_in_models_emissions.R
# Author(s): Patrick O'Rourke
# Date Last Updated: March 14th, 2016
# Program Purpose: To read in & reformat EMEP as-in-models emissions data.
# Input Files: EMEP as-in-models Emissions Data ("EMEP_SNAP_As-in-models_CO.txt")
# Output Files: 1) All Initial EMEP as-in-models txt files resavedas csv files
#                  ("EMEP_SNAP_As-in-models_em.csv") in the input folder.
#               2) All EMEP as-in-models reformatted inventories ("E.em_EMEP_as-in-models_inventory").
# Notes: 1. EMEP as-in-models Emissions are provided as: 1980, 1985, 1991 - 2013
#        2. There is no EMEP as-in-models data for Germany
# TODO:
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "analysis_functions.R" ) # Any additional function files required
    log_msg <- "Initial reformatting of the EMEP as-in-models Emissions" # First message to be printed to the log
    script_name <- "E.EMEP_as_in_models_emissions.R"
    
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )
    
    args_from_makefile <- commandArgs( TRUE )
    em <<- args_from_makefile[1]
    if ( is.na( em ) ) em <- "SO2"
    
# -----------------------------------------------------------------------------------------------------------
# 0.5 Settings/Load Files & Convert all txt files to csv
#     Logging does not support txt files, so convert to csv
    
    MCL <- readData( "MAPPINGS", "Master_Country_List" )
    loadPackage( 'tools' )

# Stop script if running for unsupported emissions species
    if ( em %!in% c( 'BC', 'CO', 'NH3', 'NMVOC', 'NOx', 'SO2' ) ) {
        stop ( paste( 'EMEP script is not supported for emission species', em ) )
    }

    em.read <- em

# Create a List of EMEP Files
    inv_file_name <- paste0( 'EMEP_SNAP_' , em.read, "_As-in-models", ".txt" )
  
# Function used to read in list of txt files
    inv <- read.table( paste0( './emissions-inventories/EMEP/', inv_file_name ), 
                       skip = 0, header = FALSE, sep = ";",  
                       na.strings = c( "", " ", "NA" ) ) # Converts all blank spaces to 'NA'
    names( inv ) <- c( 'ISO2', "year", "sector", "emission_species", 
                       "units", "emissions" )

# Writes each object as same format, but converted to a csv file
    writeData( inv , 'EM_INV', domain_extension = "EMEP/", 
               fn = paste0('EMEP_SNAP_', em.read, "_As-in-models"),
               meta = TRUE )
# -----------------------------------------------------------------------------------------------------------
# 1. Read in files
  
# EMEP inventory
    EMEP <- readData( 'EM_INV', domain_extension = "EMEP/",
                      paste0( 'EMEP_SNAP_', em.read, "_As-in-models" ) )

# -----------------------------------------------------------------------------------------------------------
# 2. Formatting Data
    EMEP_em <- EMEP

# Reorder Columns of Interest
    EMEP_em <- EMEP_em[ , c( "ISO2", "sector", "units", "year", "emissions" ) ]

# Order By ISO2 & sector
    EMEP_em <- dplyr::mutate( EMEP_em, ISO2 = as.character( ISO2 ) )
    EMEP_em <- EMEP_em[ order( EMEP_em$ISO2, EMEP_em$sector ), ]

# Remove Data (Interested in countries, not 'Areas')
    remove_ISO2 <- c( "AOE", "ARE", "ARO", "ASE", "ASM", "ATL", "ATX", 
                      "BAS", "BLS", "CAS", "FFR", "FGD", "KZE", "MED",
                      "NAT", "NOA", "NOS", "RFE", "RUA", "RUO", "RUP",
                      "RUR", "RUX", "TME", "TMO", "UZE", "UZO", "VOL" )
    EMEP_em <- EMEP_em[ -which( EMEP_em$ISO2 %in% remove_ISO2 ), ]

# Mapping EMEP ISO2 to CEDS ISO Codes
    EMEP_em$ISO2 <- MCL[ match( EMEP_em$ISO2, MCL$EMEP ), 'iso' ]
    names( EMEP_em ) [ 1 ] <- "iso"

# Paste value x infront of all Years
    EMEP_em$year <- sprintf( "X%s", EMEP_em$year )

# convert emissions to numeric
    EMEP_em$emissions <- as.numeric( EMEP_em$emissions )

# Remove NA's
    EMEP_em <- EMEP_em[ complete.cases( EMEP_em ), ]

# Cast to wide format
    EMEP_emdf <- cast( EMEP_em, iso + sector + units ~ year, 
                       value = "emissions" )

# Relabel units from Gg to kt (same numerical value, different label)
    EMEP_emdf$units <- 'kt'

# ------------------------------------------------------------------------------
# 3. Intermediate Output

# Write Data: 
    writeData( EMEP_emdf, domain = "MED_OUT", 
               fn = paste0( "E.", em, "_EMEP_as-in-models_inventory" ), 
               meta = TRUE )

# Every script should finish with this line-
    logStop()

# END
