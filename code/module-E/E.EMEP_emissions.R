# ------------------------------------------------------------------------------
# Program Name: E.EMEP_emissions.R
# Author(s): Patrick O'Rourke
# Date Last Updated: March 21st , 2016
# Program Purpose: To read in & reformat EMEP emissions data.
# Input Files: All EMEP Emissions Data
# Output Files: All Initial EMEP txt files resaved as csv files (in input folder),
              # E.em_EMEP_inventory.csv, E.em_EMEP_inventory_Russia.csv
# Notes: 1. EMEP Emissions are provided from 1980-2013
# TODO:
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "analysis_functions.R", 
                  'interpolation_extension_functions.R' ) # Any additional function files required
    log_msg <- "Initial reformatting of the EMEP Emissions" # First message to be printed to the log
    script_name <- "E.EMEP_emissions.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# -----------------------------------------------------------------------------------------------------------
# 0.5 Settings/Load Files & Convert all txt files to csv
#     Logging does not support txt files, so convert to csv
    MCL <- readData( "MAPPINGS", "Master_Country_List" )
    loadPackage('tools')
  
# Describes which emission species is being analyzed 
    args_from_makefile <- commandArgs( TRUE )
    em <<- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "NMVOC"
  
    em.read <- em
    if( em == "SO2" ) em.read <- "SOx"

# -----------------------------------------------------------------------------------------------------------
# 0.75 Select EMEP level

# SELECT EMEP LEVEL - 'LEVEL1' OR 'LEVEL2'
    level <- 'LEVEL1'
  
# SELECT Data Format
    Em_Format <<- args_from_makefile[ 2 ]
    if ( is.na( Em_Format ) ) Em_Format <- "NFR09"
  
# -----------------------------------------------------------------------------------------------------------
# 1. Read in files

# Create a List of EMEP Files
    inv_file_name <- paste0( 'EMEP_', Em_Format, '_', 
                             level, '_', em.read, ".txt" )
    
    file_path <- filePath( 'EM_INV', inv_file_name, "", "EMEP/" )
  
    cat( "here with inv_file_name: ", inv_file_name ) ### What is the purpose of these string prints?
    cat( "here with inv_file_path:: ", file_path )
  
    if ( file.exists( file_path ) ) {
    
    # Function used to read in list of txt files
        inv <- read.table( paste0( './emissions-inventories/EMEP/',
                                   inv_file_name ),
                           skip = 0, header = FALSE, sep = ";",
                           na.strings = c( "", " ", "NA" ) ) # Converts all blank spaces to 'NA'
        names( inv ) <- c( 'ISO2', "year", "sector", "emission_species",
                           "units", "emissions" )
      
    # Writes each object as same format, but converted to a csv file
        writeData( inv , 'EM_INV', domain_extension = "EMEP/",
                   fn = paste0( 'EMEP_', Em_Format, '_', level, '_', em.read ),
                   meta = TRUE )
    
    # Now read back in
    
        file_name <- paste0( 'EMEP_', Em_Format, '_', level, '_', em.read )
        EMEP <- readData( 'EM_INV', domain_extension = "EMEP/", file_name )
    
    # Create empty list if file does not exist
    } else {
        EMEP <- list()    
    } 
 
# -----------------------------------------------------------------------------------------------------------
# 2. Formatting Data
     if ( length( EMEP ) > 0 ) {
     
    	    EMEP_em <- EMEP
    
    	# Reorder Columns of Interest
    	    EMEP_em <- EMEP_em[ , c( "ISO2", "sector", "units", "year", 
    	                             "emissions" ) ]
    
    	# Remove All Information from sectors Before " "
    	# Only for NFR14 format. NFR09 format does not have any spaces
        	if ( Em_Format == 'NFR14' ) {
          	  EMEP_em <- dplyr::mutate( EMEP_em, sector = as.character( sector ) )
          	  EMEP_em <- dplyr::mutate( EMEP_em, 
          	                     sector = sapply( strsplit( EMEP_em$sector,
          				                                          split = ' ', 
          				                                          fixed = TRUE ), 
          	                                      function( x ) ( x [ 2 ] ) ) )
        	}
      
    	# Order By ISO2 & sector
    	    EMEP_em <- dplyr::mutate( EMEP_em, ISO2 = as.character( ISO2 ) )
    	    EMEP_em <- EMEP_em[ order( EMEP_em$ISO2, EMEP_em$sector ), ]
    
    	# Remove EU, EU 9, EU 12, EU 15, EU 27 Data (Interested in Countries not EU)
    	    remove_ISO2 <- c( 'EU', 'EU09', 'EU12', 'EU15', 'EU27' )
    	    EMEP_em <- EMEP_em[ -which( EMEP_em$ISO2 %in% remove_ISO2 ), ]
    
    	# Mapping EMEP ISO2 to CEDS iso codes
    	    EMEP_em$ISO2 <- MCL[ match( EMEP_em$ISO2, MCL$EMEP ),'iso' ]
    	    names( EMEP_em ) [ 1 ] <- "iso"
    
    	# Convert years to Xyears
    	    EMEP_em$year <- sprintf( "X%s", EMEP_em$year )
      
    	# convert emissions to numeric
    	    EMEP_em$emissions <- as.numeric( EMEP_em$emissions )
    
  	  # remove any rows with NA values
    	    EMEP_em <- EMEP_em[ complete.cases( EMEP_em ), ]
  	  # NOTE THIS INTRODUCES NEW NA's (from "C, IE, NE, NO, NR" where:
  	  # IE = Sources Included elsewhere, NE = Sources not Estimated
  	  # NO = Not Occuring, NR = Not Reported)
      
    	# Cast to wide format
    	    EMEP_emdf <- cast( EMEP_em, iso + sector + units ~ year, 
    	                       value = "emissions" )
      
    	# Relabel units from Gg to kt (same numerical value, different label)
    	    EMEP_emdf$units <- 'kt'
      
      # Subset Russian Data
      	  EMEP_emRUS <- subset( EMEP_emdf, iso == "rus" )
      	  remove_ISO <- c( 'rus' )
      	  EMEP_emdf <- EMEP_emdf[ -which( EMEP_emdf$iso %in% remove_ISO ), ]
    
      # interpolate EMEP over problem years, NMVOC for bel
      	  if ( em == 'NMVOCxx' ) {
      	  # Use the extendValues function to expand data into NAs 
          		EMEP_emdf[ which( EMEP_emdf$iso == 'bel' &
      				                  EMEP_emdf$sector == 'D_Fugitive' ),
      		           paste0( 'X', 2005:2010 ) ] <- 
                extendValues( EMEP_emdf[ which( EMEP_emdf$iso == 'bel' &
					                                      EMEP_emdf$sector == 'D_Fugitive' ),
					                               paste0( 'X', 2005:2010 ) ], 
                              pre_ext_year = 2005,
					                    post_ext_year = 2010 )
      	  }
    # end processing for EMEP species
    } else {
    # Create dummy files for non EMEP species
        EMEP_emdf <- c( 'iso', 'sector', 'year' )
        EMEP_emRUS <- c( 'iso', 'sector', 'year' )
    }  
  
# ------------------------------------------------------------------------------
# 3. Output
# Write Data: 
    writeData( EMEP_emdf, domain = "MED_OUT", 
               fn = paste0( "E.", em, "_EMEP_", Em_Format, "_inventory" ),
               meta = TRUE )
  
# Write Russian Data:
    writeData( EMEP_emRUS, domain = "MED_OUT", 
               fn = paste0( "E.", em, "_EMEP_", Em_Format, "_inventory_Russia" ),
               meta = TRUE )
  
# Every script should finish with this line-
    logStop()
    
# END
