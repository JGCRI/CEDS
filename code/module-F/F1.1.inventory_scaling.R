#------------------------------------------------------------------------------
# Program Name: F1.1.inventory_scaling.R
# Author: Rachel Hoesly adapted from Jon Seibert
# Date Last Updated: Sept 24, 2015
# Program Purpose: To select and run script(s) to scale default emissions and
#           emission factors to inventories.
# Input Files: None
# Output Files: None
# Functions Defined: source_child
# Notes: 
# TODO: Add conditionals and script specifications for other emissions types
#       as they are added to the system.
# ------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
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
  headers <- c( "data_functions.R" ,"emissions_scaling_functions.R" ,
                "interpolation_extention_functions.R") # Additional function files required.
  log_msg <- paste0( "Calling inventory emission scaling stripts" ) # First message to be printed to the log
  script_name <- "F1.1.inventory_scaling.R"
  
  source( paste0( PARAM_DIR, "header.R" ) )
  initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------------
# 0.5 Define Functions
# Create a function that can be applied to source all child scripts for the given
# emissions type.
  source_child <- function( file_name ){ source( paste( MODULE_F, file_name, sep = "" ) ) }

# ------------------------------------------------------------------------------------
# 1. Define emission species and read in files
  
  # Define emissions species variable
  args_from_makefile <- commandArgs( TRUE )
  em <- args_from_makefile[ 1 ]
  if ( is.na( em ) ) em <- "SO2"
  em_lc <- tolower( em ) 
  
  MODULE_F <- "../code/module-F/"

  EF  <- readData( "MED_OUT", paste0( "D.", em, "_default_total_EF" ) )
  emissions <- readData( "MED_OUT", paste0( "D.", em, "_default_total_emissions" ) )

# ------------------------------------------------------------------------------------
# 2. Create base files

# Create a base dataframe of scaled emissions and emissions factors for input into scaling  
# modules - This is the working set of emissions and ef's that the scaling scripts alter.
# Before scripts are run, these are identical to the default emissions.
# Load then write the data

  writeData(EF, domain = "MED_OUT", fn = paste0( "F.", em, "_scaled_EF" ), meta = TRUE )
  writeData(emissions, domain = "MED_OUT", fn = paste0( "F.", em, "_scaled_emissions" ), meta = TRUE )

# Create base value_metadata file
  F.initializeMeta(EF)
  
# ------------------------------------------------------------------------------------
# 3. Call scaling scripts for various species
  scripts <- c()
   
  # EDGAR
  if ( em %in% c('NOx','NMVOC','CO','CH4') ) scripts <- c(scripts, 'F1.1.Edgar_scaling.R')
  
  # UNFCCC
  if ( em %in% c('SO2','CO','NMVOC','NOx') ) scripts <- c(scripts, 'F1.1.UNFCCC_scaling.R')
  
  # EMEP
  if ( em %in% c('CO','NH3','NMVOC','NOx','SO2') ) scripts <- c(scripts, 'F1.1.EMEP_scaling.R')
  
  # CAN
  if ( em %in% c('SO2','NOx','NMVOC','CO','PM10','PM25' ) ) scripts <- c(scripts, 'F1.1.CAN_scaling_olderData.R')
  # Newer data must run latter
  if ( em %in% c('SO2','NOx','NMVOC','CO') ) scripts <- c(scripts, 'F1.1.CAN_scaling_newerData.R')
  
  # USA
  if ( em %in% c('SO2','NOx','NMVOC','CO','NH3','PM10','PM25') ) scripts <- c(scripts, 'F1.1.US_scaling.R')

  # China
  if ( em %in% c('SO2', 'NOx', 'NH3', 'NMVOC', 'CO' ) ) scripts <- c(scripts, 'F1.1.china_scaling.R')
  
  # Argentina
  if ( em %in% c('SO2', 'NOx', 'CO', 'NMVOC' ) ) scripts <- c(scripts, 'F1.1.argentina_scaling.R')

  # Japan
  if ( em %in% c('SO2', 'NOx', 'CO', 'NMVOC', 'NH3' ) ) scripts <- c(scripts, 'F1.1.Japan_scaling.R')
  
  # South Korea
  if ( em %in% c('SO2', 'NOx', 'CO', 'NMVOC' ) ) scripts <- c(scripts, 'F1.1.south_korea_scaling.R')
  
# ------------------------------------------------------------------------------------
# 4. Run all scripts for the given emissions type

  invisible( lapply( scripts, source_child ) )
  
  logStop()
# END
