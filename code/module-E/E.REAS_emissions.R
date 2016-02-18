# ------------------------------------------------------------------------------
# Program Name: E.REAS_emissions.R
# Author(s): Rachel Hoesly
# Date Last Updated: Feb 11, 2016
# Program Purpose: To read in & reformat REAS emissions data.
# Input Files: All REAS data
# Output Files: E.em_REAS_inventory.csv
# Notes: 
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
log_msg <- "Initial reformatting of REAS Emissions" # First message to be printed to the log
script_name <- "E.REAS_emissions.R"

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
  if ( is.na( em ) ) em <- "NH3"
  
# Stop script if running for unsupported emissions species
  if ( em %!in% c('BC','CH4','CO','CO2','NH3','N2O','NMVOC','NOx','OC','SO2') ) {
    stop (paste( 'REAS script is not supported for emission species', em))
  }
  
  em.read <- em
  if(em == "NMVOC") em.read <- "NMV"

  # -----------------------------------------------------------------------------------------------------------
  # 1. Read in Data
    
  # set wd to REAS folder  
    setwd( './emissions-inventories/REAS_2.1')
    inv_name <- "REAS"
  # create temporary folder to extract zipped files
    zipfile_path <- paste0('./',em.read,'.zip')
    dir.name <- paste0('./',em,'_',inv_name,'_temp_folder')
    dir.create(dir.name)
  # unzip files to temp folder  
    unzip(zipfile_path, exdir = dir.name)
    
  # list files in the folder
    files <- list.files(paste0(dir.name,'/',em.read)  ,pattern = '.txt')

  # define function to read in and process single file

    read_process_reas <- function ( file_name ){
    # read files 
    read_in_data <- read.table( paste0('./',em,'_',inv_name,'_temp_folder/',em.read,'/',file_name) , stringsAsFactors = FALSE,
                        col.names = paste('temp_col',1:22),
                        strip.white = T,
                        fill = T)
    # assign column names
    col_names <- c( 'FUEL_TYPE', unlist( read_in_data[which(read_in_data[,1] == 'Fuel'), 3:22] ) )
    col_names[8] <- 'OTHERS_IND'
    col_names <- tolower(col_names)
    names( read_in_data ) <- col_names
    read_in_data <- read_in_data[,col_names[!is.na(col_names)]]
    names( read_in_data )[which(names( read_in_data ) == 'sub_total')] <- 'total_combustion'
    
    # get country year units info
    country <- read_in_data[which(read_in_data[,1] == 'Country'), 3]
    year <- read_in_data[which(read_in_data[,1] == 'Year'), 3]
    sub_region <- read_in_data[which(read_in_data[,1] == 'Sub-Region'), 3]
    units <- read_in_data[which(read_in_data[,1] == 'Combustion'), 3]
  
    
    # seperate combustion and process data (different shape)
    combustion_data <- read_in_data[6:23,]
    non_combustion_data <- read_in_data[25:42,1:2]
    
    # Format combustion
    combustion_data$fuel_type <- tolower(combustion_data$fuel_type)
    combustion_data <- combustion_data[which(!combustion_data[,1] == 'sub_total'),]
    combustion_data <- melt( combustion_data , id.vars = 'fuel_type')
    names(combustion_data) <- c( 'fuel_type','sector','emissions')
    
    # format non_combustion
    names(non_combustion_data) <- c( 'sector' , 'emissions')
    non_combustion_data <- non_combustion_data[which( !non_combustion_data[,1] == 'Sector'),]
    non_combustion_data <- non_combustion_data[which( !non_combustion_data[,1] %in% 'SUB_TOTAL'),]
    non_combustion_data <- non_combustion_data[which( !non_combustion_data[,2] %in% '[t/year]'),]
    non_combustion_data[which(non_combustion_data$SECTOR == 'total'),'sector'] <- 'total_non_combustion'
    non_combustion_data$fuel_type <- 'process'
    non_combustion_data$sector <- tolower(non_combustion_data$sector)
    
    # combine
    all_data <- rbind.fill(combustion_data, non_combustion_data)
    all_data$year <- paste0('X',year)
    all_data$country <- country
    all_data$sub_region <- sub_region
    
    all_data$emissions <- as.numeric(all_data$emissions)
    all_data$emissions <- all_data$emissions/1000
    all_data$units <- 'kt'
    
    all_data <- all_data[,c( 'sub_region','country','sector','fuel_type','units','year','emissions')]
    
    return (all_data)
    }
    
  # apply function to list of files
   reas_data_list <- lapply( X = files , FUN = read_process_reas) 
   
#    filelist <- list()
#    for ( i in seq_along(files)) filelist[i] <- read_process_reas(files[i])
# #    
   # delete temp folder
   unlink(dir.name,recursive = TRUE)
    
   # reset workign directory
   setwd('../') 
   setwd('../') 
   
   # bind all data together and cast to wide format 
   reas_data <- do.call("rbind", reas_data_list)
   
   # Remove province/state level data and only keep country totals
   reas_data <- reas_data[ which( reas_data$sub_region == "WHOL" ), ]

   
   reas_data$iso <- tolower( reas_data$country )  # lowercase ISO
   
   reas_data_wide <- cast(reas_data, sub_region + iso + sector + fuel_type + units  ~ year,  value = 'emissions')
 
  # ------------------------------------------------------------------------------
  # 5. Output
  # Write Data: 
    writeData(reas_data_wide, domain = "MED_OUT", fn = paste0( "E.", em, "_REAS_inventory" ), meta = TRUE )
  
  # Diagnostic files: 
    writeData( unique( reas_data$iso ) , domain = "DIAG_OUT", fn = paste0( "E.", em, "_REAS_countries" ), meta = FALSE )
    writeData( unique( reas_data$sector ) , domain = "DIAG_OUT", fn = paste0( "E.", em, "_REAS_sectors" ), meta = FALSE )
  
  # Every script should finish with this line-
    logStop()
    
  # END