# ------------------------------------------------------------------------------
# Program Name: E.EMEP_emissions.R
# Author(s): Patrick O'Rourke
# Date Last Updated: December 21st , 2015
# Program Purpose: To read in & reformat EMEP emissions data.
# Input Files: All EMEP Emissions Data
# Output Files: All Initial EMEP txt files resaved as csv files (in input folder), 
              # E.em_EMEP_inventory.csv, E.em_EMEP_inventory_Russia.csv
# Notes: 1. EMEP Emissions are provided from 1980-2013
# TODO: 
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Set working directory to the CEDS “input” directory & define PARAM_DIR as the
# location of the CEDS “parameters” directory, relative to the new working directory.
dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
for ( i in 1:length( dirs ) ) {
  setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
  wd <- grep( 'CEDS/input', list.dirs(), value = T )
  if ( length( wd ) > 0 ) {
    setwd( wd[ 1 ] )
    break
  }
}
INPUT <- paste(getwd())
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R", 'interpolation_extention_functions.R') # Any additional function files required
log_msg <- "Initial reformatting of the EMEP level 2 Emissions" # First message to be printed to the log
script_name <- "E2.EMEP_emissions.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# -----------------------------------------------------------------------------------------------------------
#0.5 Settings/Load Files & Convert all txt files to csv
  #logging does not support txt files, so convert to csv
  MCL <- readData( "MAPPINGS", "Master_Country_List" )
  loadPackage('tools')
  
# Describes which emission species is being analyzed 
  args_from_makefile <- commandArgs( TRUE )
  em <<- args_from_makefile[1]
  if ( is.na( em ) ) em <- "NMVOC"
  
# Stop script if running for unsupported emissions species
  if ( em %!in% c('BC','CO','NH3','NMVOC','NOx','SO2','CH4','OC') ) {
    stop (paste( 'EMEP script is not supported for emission species', em))
  }
  
  em.read <- em
  if(em == "SO2") em.read <- "SOx"

  
# -----------------------------------------------------------------------------------------------------------
  
# run EMEP script for EMEP species
  
if ( em %in% c('BC','CO','NH3','NMVOC','NOx','SO2') ){
    
# SELECT EMEP LEVEL - 'LEVEL1' OR 'LEVEL2'
  level <- 'LEVEL1'
  
# SELECT Data Format
  Em_Format <- "NFR09"
  Em_Format <- "NFR14"

# Create a List of EMEP Files
  inv_file_name <- paste0('EMEP_',Em_Format,'_', level , '_', em.read , ".txt" )
  
# Function used to read in list of txt files
  inv <- read.table(paste0('./emissions-inventories/EMEP/',inv_file_name), skip=0, header = FALSE, sep = ";" ,  
                    na.strings=c( "", " ", "NA" ) ) # Converts all blank spaces to 'NA'
  names(inv) <- c('ISO2',"year","sector", "emission_species", "units","emissions")
  
# Writes each object as same format, but converted to a csv file
  writeData( inv , 'EM_INV', domain_extension = "EMEP/" , fn = paste0('EMEP_',Em_Format,'_', level , '_', em.read ),
             meta = TRUE)
# -----------------------------------------------------------------------------------------------------------
# 1. Read in files
  EMEP <- readData('EM_INV', domain_extension = "EMEP/",  paste0('EMEP_',Em_Format,'_', level , '_', em.read )  ) 
  
# -----------------------------------------------------------------------------------------------------------
# 2. Formatting Data
  EMEP_em <- EMEP

# Reorder Columns of Interest
  EMEP_em <- EMEP_em[,c("ISO2", "sector", "units", "year", "emissions" )]

# Remove All Information from sectors Before "[space]"
# Only for NFR14 format. NFR09 format does not have any spaces
if ( Em_Format == 'NFR14' ) {
  EMEP_em <-mutate( EMEP_em, sector = as.character( sector ) )
  EMEP_em <-mutate(EMEP_em, sector = sapply( strsplit( EMEP_em$sector,
            split = ' ', fixed = TRUE ), function( x ) ( x [ 2 ] ) ) )
}
  
# Order By ISO2 & sector
  EMEP_em <-mutate( EMEP_em, ISO2 = as.character( ISO2 ) )
  EMEP_em <-EMEP_em[ order(EMEP_em$ISO2,EMEP_em$sector), ]

# Remove EU, EU 9, EU 12, EU 15, EU 27 Data (Interested in Countries not EU)
  remove_ISO2 <- c('EU','EU09','EU12','EU15', 'EU27')
  EMEP_em<-EMEP_em[-which(EMEP_em$ISO2 %in% remove_ISO2), ]

# Mapping EMEP ISO2 to CEDS ISO Codes
  EMEP_em$ISO2 <- MCL[ match(EMEP_em$ISO2,MCL$EMEP),'iso']
  names( EMEP_em ) [ 1 ] <- "iso"

# Paste value x infront of all Years
  EMEP_em$year <- sprintf("X%s", EMEP_em$year)
  
# convert emissions to numeric
  EMEP_em$emissions <- as.numeric(EMEP_em$emissions)

# remove NA's
  EMEP_em <- EMEP_em[complete.cases(EMEP_em),]
  # NOTE THIS INTRODUCES NEW NA's (from "C, IE, NE, NO, NR" where:
  # IE = Sources Included elsewhere, NE = Sources not Estimated
  # NO = Not Occuring, NR = Not Reported)
  
# Cast to wide format
  EMEP_emdf <- cast(EMEP_em, iso + sector + units ~ year,  value ="emissions")
  
# Relabel units from Gg to kt (same numerical value, different label)
  EMEP_emdf$units <- 'kt'
  
 # Subset Russian Data
  EMEP_emRUS <- subset(EMEP_emdf, iso == "rus")
  remove_ISO <- c('rus')
  EMEP_emdf<-EMEP_emdf[-which(EMEP_emdf$iso %in% remove_ISO), ]

  

  # interpolate EMEP over problem years, NMVOC for bel
  
  if (em == 'NMVOCxx'){ # Now deal with issues such as this in scaling mapping
    EMEP_emdf[which(EMEP_emdf$iso == 'bel' &
              EMEP_emdf$sector == 'D_Fugitive'), paste0('X',2005:2010)] <- extendValues(EMEP_emdf[which(EMEP_emdf$iso == 'bel' &
                                      EMEP_emdf$sector == 'D_Fugitive'), paste0('X',2005:2010)], pre_ext_year = 2005,
                                      post_ext_year = 2010)
  }
  
} # end processing for EMEP species
  
  # -----------------------------------------------------------------------------------------------------------
  
  # 4. create dummy files for non EMEP species
  if ( em %in% c('CH4', 'OC') ){
  
    EMEP_emdf <- c('iso','sector','year')
    EMEP_emRUS <- c('iso','sector','year')
    
    }  
  

# ------------------------------------------------------------------------------
  # 5. Output
  # Write Data: 
    writeData(EMEP_emdf, domain = "MED_OUT", fn = paste0( "E.", em, "_EMEP_inventory" ), meta = TRUE )
  
  # Write Russian Data:
    writeData(EMEP_emRUS, domain = "MED_OUT", fn = paste0( "E.", em, "_EMEP_inventory_Russia" ),
            meta = TRUE )
  
  # Every script should finish with this line-
    logStop()
    
  # END