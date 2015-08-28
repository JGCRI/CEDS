#------------------------------------------------------------------------------
# Program Name: D3.1.default_total_EF.R
# Author: Jon Seibert
# Date Last Modified: July 7, 2015
# Program Purpose: To combine process emissions factors with their combustion
#                  counterparts to create total emissions factors.
# Input Files: C.[em]_NC_EF_db.csv, B.[em]_comb_EF_db.csv
# Output Files: D.[em]_total_default_EF.csv, 
# Notes: 
# TODO: calls C.[em]_NC_EF which, currently uses default comb emissions
#                 need to look into
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Before we can load headers we need some paths defined. They may be provided by
#   a system environment variable or may have already been set in the workspace.
# Set variable PARAM_DIR to be the data system directory
    dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
    for ( i in 1:length( dirs ) ) {
        setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
        wd <- grep( 'emissions-data-system/input', list.dirs(), value = T )
        if ( length( wd ) > 0 ) {
            setwd( wd[ 1 ] )
            break
        }
    }
    PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
    headers <- c() # Additional function files required.
    log_msg <- "Combines combustion and process emissions factors" # First message to be printed to the log
    script_name <- "D3.1.default_total_EF.R"
    
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )
    
# -----------------------------------------------------------------------------
# 0.5. Settings

# Location of input files relative to wd
input_path <- paste0( getwd(),"/energy/" )

# ------------------------------------------------------------------------------
# 1. Read in files

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"
em_lc <- tolower( em )

EF_db <- readData( "MED_OUT", paste0( "C.", em, "_", "NC", "_EF_db" ), meta = FALSE )

default_efs <- readData( "MED_OUT", paste0( "B.", em, "_", "comb", "_EF_db" ) )

# ------------------------------------------------------------------------------
# 2. Minor processing

# Combine new emissions factors and data with default and scaled 
# combustion emissions factors and data, and sort.

total_default_efs <- rbind( EF_db, default_efs )
total_default_efs <- total_default_efs[ with( total_default_efs, order( iso, sector, fuel ) ), ]


# ------------------------------------------------------------------------------
# 3. Output

writeData(total_default_efs, domain = "MED_OUT", fn = paste0( "D.", em, "_total_default_EF" ), meta = TRUE )

# Write out the default EFs to the 2nd file called scaled_EFs. This file will be
#   called and scaled later in Module E.
# writeData(total_scaled_efs, domain = "MED_OUT", fn = paste0( "D.", em, "_total_scaled_EF" ), meta = TRUE )

logStop()
# END
