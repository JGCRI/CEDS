# ------------------------------------------------------------------------------
# Program Name: E.UNFCCC_emissions.R
# Author(s): Patrick O'Rourke, Rachel Hoesly, Jon Seibert, Presley Muwan
# Date Last Updated: June 14, 2019
# Program Purpose: To read in and reformat UNFCCC emissions data.
# Input Files: All UNFCCC Emissions Data
# Output Files: E.[EM]_UNFCCC_inventory.csv
#               E.[EM]__UNFCCC_inventory.cvs
#               E.[EM]_UNFCCC_filename_Sector_mapping.cvs
# Notes: UNFCCC  Emissions are provided from 1990-2012.
# TODO:
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "analysis_functions.R" ) # Any additional function files required
    log_msg <- "Initial reformatting of the UNFCCC emissions inventories" # First message to be printed to the log
    script_name <- "E.UNFCCC_emissions.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "CH4"

# -----------------------------------------------------------------------------------------------------------
# 0.5 Settings

    loadPackage( 'tools' )

    domain <- "EM_INV"
    domain_ext <- "UNFCCC/"
    file_name <- em
    extension <- ".zip"

    file_path <- filePath( domain, file_name, extension, domain_ext )

#-------------------------------------------------------------------------------------------------------
# check_uniqueID_and_species_mismatch

# Author:         Presley Muwan

# Brief:          This function prints out a WARNING message if it detects that a file contains data intended for
#                another specie or another file. 

# Details: 
#              * file_name_id: is the unique id, of the file, retrieved from the file's name
#              * content_id: is the unique id, of the file, retrieve from within the file

#              * file_name_specie: is the emission specie retrieved from the file's name
#              * content_specie: is the emission specie column-header within the file

#               This function checks if the file_name_id matches the content_id. If this does not match it implies
#               the files content might contain another file's data; so a WARNING massage is printed

#               This funtion also matches content_specie and file_name_specie. If they do not match a WARNING messages
#               is printed; as this implies that the file may content data intended for a different emission specie.

# Parameter:     this function has two parameters 
#               *UNFCCC_DF: A data frame that holds the (cvs) file(s) content
#               *UNFCCC_FILE_NAMES: A list that holds only the names of the file(s) of the emmission within the

# Return   no return

# Input Files:   no direct input files; however this function retrieves a list of the file content from 'UNFCCC' list variable
#               and the list of files names for 'file_list' array variable

# Output Files: E.[EM]__WARNING_UNFCCC_inventory_mismatch_list.cvs: Generates a diagnostic this file if a specie/id mismatch is detected
#               E.[EM]_UNFCCC_filename_Sector_mapping.cvs: This file contains a list of filename to sector mapping within the emission specie folder

    check_uniqueID_and_species_mismatch <- function( UNFCCC_DF, UNFCCC_FILE_NAMES ) {
      
      
    # stores errors found when checking for uniqueID_and_species_mismatch  
        diagnostic_error_data = list() 
      
    # stores data cotianing a list of filename-sectorname mapping for each file.
        diagnostic_general_data = list()
      
    # keeps count of the number of species/index mismatches found 
        mismatch_count = 0;
      
      
    # iterate through each file  and within the emission specie folder 
    # and check for uniqueID_and_species_mismatch 
      
        for ( i in seq_along( UNFCCC_DF ) ) {
        #Retrieve the specie and the id from the file Name
            fileName <- strsplit( UNFCCC_FILE_NAMES[ i ], 
                                  split = '_', fixed = TRUE )
            
            file_name_specie <- strsplit( fileName[[ 1 ]][ 1 ], 
                                          split = '/', fixed = TRUE )[[ 1 ]][ 2 ]
            
            file_name_id <- toupper( fileName[[ 1 ]][ 2 ] ) 
          
          
        # Retrieve the species and the id from the file colum
          
            content_id <- toupper( strsplit( UNFCCC_DF[[ i ]][ 1, 2 ], 
                                           split = " ", fixed = TRUE )[[ 1 ]][ 1 ] )
            content_specie <- UNFCCC_DF[[ i ]][ 1, 11 ]
          
        # Store diagnostic data; containing list of file name and sector name 
            diagnostic_general_data$file_name[ i ] <- 
                   strsplit( file_list[ i ], split = '/', fixed = TRUE )[[ 1 ]][ 2 ]
            diagnostic_general_data$sector_name[ i ] <- UNFCCC[[ i ]][ 1, 2 ]
          
        # if the id's do not match, check if the file  have an id that does not
        # follow the normal id name convention (like the "Multilateral
        # Operation_netemission .cvs" file whose id is "Multilateral Operation")
            if ( file_name_id != content_id ) {
              
            # use the whole file name (without the specie, and the "netemission"
            # phrase) as a the id; proxy_content_id
              
                proxy_content_id <- toupper( as.character( UNFCCC[[ i ]][ 1, 2 ] ) )
              
              
            # if the emission species do not match, generate a diagnostic data and 
            # log a WARNING message
                if ( file_name_id != proxy_content_id ) {
                    mismatch_count = ( mismatch_count + 1 )
                    problem_description <- "The id in the file name (File_name_id)
                                            does not match the id inside the file 
                                            (content_id)"
                    
                # Add columns to the diagnostics_output list and store
                # information about the file with and id mismatch
                    diagnostic_error_data$file_Name[ mismatch_count ] <- 
                                               UNFCCC_FILE_NAMES[ i ]
                    diagnostic_error_data$File_name_id[ mismatch_count ] <- 
                                                         fileName[[ 1 ]][ 2 ]
                    diagnostic_error_data$content_id[ mismatch_count ] <- content_id
                    diagnostic_error_data$file_name_specie[ mismatch_count ] <- 
                                                                file_name_specie
                    diagnostic_error_data$content_specie[ mismatch_count ] <- 
                                                                content_specie
                    diagnostic_error_data$description[ mismatch_count ] <-
                                                            problem_description
                    
                # Generate and print a log message 
                    printLog( "EMISSION-ID MISMATCH WARNING:\n ",
                              "\tFilename:", UNFCCC_FILE_NAMES[i], 
                              "\n\tFile Content id:",
                              content_id, "\n", problem_description,
                              "\n", ts = TRUE, cr = TRUE )   
                }
              
            }
          
        # If the emission species do not match, generate a diagnostic data and 
        # log a WARNING message
            if ( file_name_specie != content_specie ) {
                mismatch_count = ( mismatch_count + 1 )
                problem_description <- "The specie in the file name (file_name_specie)
                                        does not match the specie inside the file 
                                        (content_specie)"
              
            # Add columns to the diagnostics_output list and store information
            # about the file with and specie mismatch
                diagnostic_error_data$file_Name[ mismatch_count ] <- 
                                                          UNFCCC_FILE_NAMES[ i ]
                diagnostic_error_data$File_name_id[ mismatch_count ] <- 
                                                            fileName[[ 1 ]][ 2 ]
                diagnostic_error_data$content_id[ mismatch_count ] <- content_id
                diagnostic_error_data$file_name_specie[ mismatch_count ] <- 
                                                                file_name_specie
                diagnostic_error_data$content_specie[ mismatch_count ] <- 
                                                                  content_specie
                diagnostic_error_data$description[ mismatch_count ] <- 
                                                             problem_description
              
            # generate and print a log message 
                printLog( "EMISSION-SPECIES MISMATCH WARNING:\n ",
                          "\tFilename:", UNFCCC_FILE_NAMES[ i ], 
                          "\n\tFile Content Species:",
                          content_specie, "\n", problem_description, "\n", 
                          ts = TRUE, cr = TRUE )
            }
        }
      
    # Write the diagnostic data to a diagnostic file
        if ( length( diagnostic_error_data ) > 0 ) {
            writeData( diagnostic_error_data, domain = "DIAG_OUT", 
                       fn = paste0( "E.", em, "_WARNING_UNFCCC_inventory_mismatch_list" ),
                       meta = FALSE )
        }

    # Generate (the second) diagnostic file contianing the list of file names and
    # sector names
        writeData( diagnostic_general_data, domain = "DIAG_OUT", 
                   fn = paste0( "E.", em, "_UNFCCC_filename_Sector_mapping" ), 
                   meta = FALSE )
    }


# -----------------------------------------------------------------------------------------------------------
# 1. Read in files
# Use the readData() function (defined in IO_functions.R) for all input data.

# Read all .csv data files for the relevant emissions species from within the .zip.
# If there is no .zip file for this emissions species, there is no data to process, and
# a dummy file will be created.
# NOTE: MUST USE "header = FALSE" option to avoid errors from format of these particular input files.
# This option is passed through to read.csv via the ... catch parameter- readData does not have its own
# header option.
    if ( file.exists( file_path ) ) {
    
        UNFCCC <- readData( domain, file_name, extension, 
                            domain_extension = domain_ext,
                            extract_all = TRUE, header = FALSE )
        
    # Retrieve full list of internal files ( used to determine sectors )
        file_list <- listZippedFiles( file_path, FALSE )
        
    # Call check_uniqueID_and_specie() function to perform the 2-way specie & id
    # check
        check_uniqueID_and_species_mismatch( UNFCCC, file_list )
        
    } else { 
        UNFCCC <- list() 
    }
    
    MCL <- readData( "MAPPINGS", "Master_Country_List" )


# -----------------------------------------------------------------------------------------------------------
# 2. Formatting Data

# UNFCCC is now a list of data frames. If there was no data, there will be 0
# entries in the list.
    if ( length( UNFCCC ) > 0 ) { 
    # If there is data to process for this emissions species...
      
      	UNFCCC_clean <- UNFCCC
      
    # Iterate through each dataframe (sheet) in the list
      	for ( i in seq_along( UNFCCC_clean ) ) {
        	  df <- UNFCCC_clean[[ i ]]
      
    	  # Make a Variable called Sector
        	  df$sector <- file_list[ i ]
      
    	  # Removes First Row
        	  df <- df[ -1, ]
      
    	  # Reformat Col Names
        	  names <- as.character( unlist ( df[ 1, ] ) )
        	  years<-paste( "X", names[ 3:( length( names ) - 1 ) ], sep = "" )
        	  names[ 3:( length( names ) - 1 ) ] <- years
        	  names[ length( names ) ] <- 'sector'
        	  names[ 1 ] <- 'country'
        	  names( df ) <- names

    	  # Remove First Row
    	      df <- df[ -1, ]

    	  # Creates Column for Units (Gg for SO2)
    	      df$units <- "kt"
    	  # Reorder Columns of Interest
    	      df <- df[ , c( 'country', 'sector', 'units', years ) ]

    	  # Remove All Information from Sectors Before "_" From File Name
    	      df <- dplyr::mutate( df, sector = as.character( sector ) )
    	      df <- dplyr::mutate( df, sector = sapply( strsplit( df$sector, split = '_',
    	                                                   fixed = TRUE ),
    	                                         function( x ) ( x [ 2 ] ) ) )

    	      UNFCCC_clean[[ i ]] <- df
    	  }

  	# Make the List of Files 1 Data Frame
    	  UNFCCCdf <- do.call( rbind, UNFCCC_clean )

  	# Convert Values to Numeric: Remove Commas in Formatting, then Convert to Numeric
    	  UNFCCCdf[ years ] <- apply( X = UNFCCCdf[ years ], MARGIN = 2,
    	                              FUN = sub, pattern = ',', replacement = "" )
    	  UNFCCCdf[ years ] <- as.numeric( as.matrix( UNFCCCdf [ years ] ) )

  	# Mapping Country Names to ISO Codes
    	  UNFCCCdf$iso <- MCL[ match( UNFCCCdf$country, MCL$UNFCCC ), 'iso' ]

  	# Remove Unmapped Lines and Reorder
      	UNFCCCdf <- UNFCCCdf[ complete.cases( UNFCCCdf$iso ), ]

      	UNFCCCdf <- UNFCCCdf[ , c( 'iso', 'sector', 'units', years ) ]
      	UNFCCCdf <- UNFCCCdf[ order( UNFCCCdf$iso, UNFCCCdf$sector ), ]

    # Convert to kt of methane from CO2e
      	if( em == 'CH4'){

      	    UNFCCCdf_convert <- UNFCCCdf %>%
      	        filter(units == 'kt') %>%
      	        mutate_if(is.numeric, funs(./21)) %>% #100yr GWP
      	        mutate(units = 'kt')

      	    UNFCCCdf <- UNFCCCdf %>%
      	        filter(units != 'kt')

      	    UNFCCCdf <- rbind(UNFCCCdf_convert, UNFCCCdf) %>%
      	        arrange(iso, sector)
      	}

# ------------------------------------------------------------------------------
# 3. Removed "Bad" Data

    # Remove Canada, Russian Fed, Luxembourg, and Poland

      	if (em == 'CH4'){

      	    # TODO: The df maniupulation after this block fails if only 'rus' is
      	    #       in the list below since rus is not in the current data. (this may be outdated)
      	    # TODO: Fix to be more robust

      	    remove_iso <- c('rus', 'lux')

      	} else {

      	    # Remove Canada, Russian Fed, Luxembourg, and Poland for other emission
      	    # species

      	    remove_iso <- c( 'can', 'rus', 'pol', 'lux' )
      	}

      	UNFCCC <- UNFCCCdf[ -which( UNFCCCdf$iso %in% remove_iso ), ]

  	# Drop Lines With Only NA Values
      	drop <- which( apply( X = is.na( UNFCCC[ years ] ), MARGIN = 1, FUN = all ) == TRUE )
      	UNFCCC <- UNFCCC[ -drop, ]

# ------------------------------------------------------------------------------
# 4. Dummy files

    } else {
    # If length( UNFCCC) == 0 (if no data to process for this emissions species), create dummy file.
        UNFCCC <- data.frame()
    }

# ------------------------------------------------------------------------------
# 5. Meta Data

    meta_names <- c( "Data.Type", "Emission", "Region", "Sector", 
                     "Start.Year", "End.Year", "Source.Comment" )
    
    meta_note <- c( "Default Emissions", "NA", 
                    "Russian Federation, Monaco & Liechtenstein", "All", "1990",
                    "2012", paste0( "The Russian Federation's emissions are too",
                                    "low to be accurate, and have thus been",
                                    "removed. Additionally Liechtenstein and",
                                    "Monaco emissions have been removed",
                                    "temporarily." ) )
    
    addMetaData( meta_note, meta_names )

# ------------------------------------------------------------------------------
# 6. Output

    writeData( UNFCCC, domain = "MED_OUT", 
               fn = paste0( "E.", em, "_UNFCCC_inventory" ), meta = TRUE )

# Every script should finish with this line
    logStop()

# END
