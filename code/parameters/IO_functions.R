# ----------------------------------------------------------------------------------
# CEDS R header file: Input and Output functions
# Author(s): Ben Bond-Lamberty, Page Kyle, Jon Seibert, Tyler Pitkanen, Linh Vu, Presley
# Last Updated: 16 May 2017

# This file must be sourced by all CEDS R scripts, generally as the second sourced script.
# Functions contained:
#   readData, writeData, sourceData, readMetaData, addMetaData,
#   addMetaNote, clearMeta, logStart, printLog, logStop, addDependency, writeMakeFileDepend,
#   readDomainPathMap, filePath, collapseList, readExcel

# Notes: Many of these functions were originally written for the GCAM data system.
#        Some relics of this past remain, such as the global variable names.

# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# printLog
# Brief:        Time-stamped output
# Details:      Prints a message string to the log along with a timestamp
# Dependencies: None
# Author(s):    Ben Bond-Lamberty
# Params:
#   msg:        String to be printed to the script log [required]
#   ts:         Boolean indicating whether to add a timestamp to the output [default: TRUE]
#   cr:         Boolean indicating whether to add a newline to the end of the output string [default: TRUE]
# Return:       None
# Input Files:  None
# Output Files: None
printLog <- function( msg, ..., ts=TRUE, cr=TRUE ) {
	if( ts ) cat( date(), GCAM_SOURCE_FN[ GCAM_SOURCE_RD ], "[", GCAM_SOURCE_RD, "]", ": " )
	cat( msg, ... )
	if( cr ) cat( "\n")
}

# -----------------------------------------------------------------------------
# logStart
# Brief:        Start a new log (to screen and optionally file)
# Details:      Start the log file for a CEDS R script
# Dependencies: printLog
# Author(s):    Ben Bond-Lamberty
# Params:
#   fn:         String name of script being executed [required]
#   savelog:    Boolean indicating whether to write to disk [default: TRUE]
# Return:       None
# Input Files:  None
# Output Files: None
logStart <- function( fn, savelog=T ) {
	GCAM_SOURCE_RD <<- GCAM_SOURCE_RD + 1		# push
	GCAM_SOURCE_FN[ GCAM_SOURCE_RD ]  <<- fn
	GCAM_LOG_SAVE[ GCAM_SOURCE_RD ] <<- savelog
	logpath <- paste( MODULE_PROC_ROOT, "/../logs/", sep = "")
	if( savelog ) sink( paste( logpath, fn, ".log", sep = "" ), split=T )
	printLog( "-----" )
	printLog( "Starting", fn )

	DEPENDENCIES[[ fn ]] <<- c( NULL ) # Create a new entry in the dependency list
	OUTPUTS[[ fn ]] <<- c( NULL ) # Create a new entry in the output list
}

# -----------------------------------------------------------------------------
# writeMakeFileDepend
# Brief:        Write dependency information for inclusion in makefile
# Details:      Writes the .d file for the current script
# Dependencies: None
# Author(s):    Ben Bond-Lamberty
# Params:
#   outfile:    Path to the .log file for the script being executed [required]
# Return:       None
# Input Files:  None
# Output Files: [fn].log
writeMakeFileDepend <- function(outfile) {
	of_parts <- unlist(strsplit(outfile,"\\."))
	if(length(of_parts) == 1) {
	    of_root <- of_parts[1]
	    of_ext  <- ""
	} else {
	    of_root = paste(of_parts[1:length(of_parts)-1], collapse=".")
	    of_ext  = of_parts[-1]
	}
        fn <- GCAM_SOURCE_FN[ GCAM_SOURCE_RD ]
	dfile <- paste(of_root, ".d", sep="")
	write(paste(outfile,": \\",sep=""),file=dfile)
	cat("  ", DEPENDENCIES[[ fn ]], file=dfile, append=TRUE, sep=" \\\n  ")
}

# -------------------------------------------------------------------------------
# collapseList
# Brief:        Collapse a list into a single character vector
# Details:      Converts a list of strings into a single string with a separator between them
# Dependencies: None
# Author(s):    Jon Seibert
# Params:
#   src_list:   List of string to be collapsed [required]
#   sep:        String to separate the elements of the list [default: "\n"]
# Return:       Collapsed string version of the list of strings
# Input Files:  None
# Output Files: None
collapseList <- function( src_list, sep = "\n" ){
    result <- src_list[[ 1 ]]
    src_list <- src_list[ -1 ]
    for( elem in src_list ){
        result <- paste0( result, sep, elem )
    }
    return( result )
}

# -----------------------------------------------------------------------------
# logStop
# Brief:        Stop the current log (to screen and optionally file)
# Details:      End the current script's log file and write out its IO information
#                   to IO_documentation.csv
# Dependencies: readData, writeData, filePath, collapseList, printLog, writeMakefileDepend
# Author(s):    Ben Bond-Lamberty, Jon Seibert
# Params:       None
# Return:       None
# Input Files:  IO_documentation.csv
# Output Files: IO_documentation.csv, [fn].log
logStop <- function() {

    # Retrieve file name
    fn <- GCAM_SOURCE_FN[ GCAM_SOURCE_RD ]

    # Collapse input list
    if( length( DEPENDENCIES[[ fn ]] ) > 0 ){
        input <- collapseList( DEPENDENCIES[[ fn ]], sep = "\r\n" )
    } else{ input <- "" }

    # Collapse output list
    if( length( OUTPUTS[[ fn ]] ) > 0 ){
        output <- collapseList( OUTPUTS[[ fn ]], sep = "\r\n" )
    } else{ output <- "" }

	# Detect if em exists first then writing/creating species specific [em]_IO_documentation
    # if no agruement is provided in Makfile, em <- 'CM' as CM stands for common
	if ( !exists( 'em', envir = .GlobalEnv ) ) em <- "CM"

    # Full file path for IO documentation
    IO_doc_path <- filePath( "DOCUMENTATION", paste0( em, "_", "IO_documentation" ), ".csv" )

    printLog( "Writing dependency information for", fn, "..." )

    # If IO_documentation exists, read it in. If it does not, create it.
    if( !file.exists( IO_doc_path ) ) {
		IO_doc <- data.frame( file = "", description = "", input_files = "",
                                        output_files = "", output_description = "" )
        IO_doc <- IO_doc[ IO_doc$file == "No such file", ]
	} else {
        IO_doc <- readData( "DOCUMENTATION", paste0( em, "_", "IO_documentation" ), meta = FALSE, mute = TRUE, to_numeric=FALSE )
    }

    sys_doc <- readData( "DOCUMENTATION", "System_Documentation", ".xlsx", meta = FALSE,
                         mute = TRUE, sheet_selection = "Module Documentation" , to_numeric=FALSE)

    # If there is no entry for the current script, make one.
    if( !( fn %in% IO_doc$file ) ){
        new <- IO_doc[1,]
        new$file <- fn
        new$description <- ""
        new$input_files <- ""
        new$output_files <- ""
        new$output_description <- ""
        IO_doc <- rbind( IO_doc, new )
    }

    # Acquire descriptions from System_Documentation.xlsx
    about <- sys_doc[ sys_doc$File == fn, "Description" ][ 1 ]
    about_output <- sys_doc[ sys_doc$File == fn, "Output File Description/Notes" ][ 1 ]

    # If there were no entries in System_Documentation for the script's descriptions
    if( length( about ) == 0 ) about <- ""
    if( length( about_output ) == 0 ) about_output <- ""

    IO_doc[ IO_doc$file == fn, "input_files" ] <- input
    IO_doc[ IO_doc$file == fn, "output_files" ] <- output
    IO_doc[ IO_doc$file == fn, "description" ] <- about
    IO_doc[ IO_doc$file == fn, "output_description" ] <- about_output

    # Sort by file name
    IO_doc <- dplyr::arrange( IO_doc, file )

    writeData( IO_doc, "DOCUMENTATION", paste0( em, "_", "IO_documentation" ), meta = FALSE, mute = TRUE )


	logfile <- paste( MODULE_PROC_ROOT, "/../logs/", fn, ".log", sep="" )
	writeMakeFileDepend(logfile)

	printLog( "All done with", fn, "\n" )
	if( GCAM_SOURCE_RD > 0 ) {
		if( GCAM_LOG_SAVE[ GCAM_SOURCE_RD ] ) sink()
		GCAM_SOURCE_RD <<- GCAM_SOURCE_RD - 1		# pop
	} else {
		printLog( "WARNING: Attempt to close a non-open log file")
	}

}

# -----------------------------------------------------------------------------
# readDomainPathMap
# Brief:        Read the domain path mapping and return a list version of it.
# Details:      The file path map is simply a central (location below) mapping of all files
#                   and where they're to be found.
# Dependencies: read.csv
# Author(s):    Ben Bond-Lamberty
# Params:       None
# Return:       List version of the domain path map file
# Input Files:  domainmappings.csv
# Output Files: None
readDomainPathMap <- function() {
	fn <- DOMAINPATHMAP
	fpm <- tryCatch( {
		 read.csv( fn, comment.char="#" )
#	}, warning=function( war ) {
#		warning( "Warning in read of", fn )
#		warning( war )
#		printLog( as.character( war ) )
	}, error=function( err ) {
		printLog( "Error in read of", fn )
		printLog( as.character( err ) )
		stop( err )		# can't recover from this
#	}, finally={

	} )	# end tryCatch

	if( length( fpm ) > 0 ) {
#		print( "All done reading", fn )
	} else {
		printLog( "Error: zero rows read from", fn )
		stop()
	}

	mylist <- list( NULL )		# make a lookup list of file names and paths
	for( i in 1:nrow( fpm ) )
		mylist[ as.character( fpm[ i, 1 ] ) ] <- as.character( fpm[ i, 2 ] )
	return( mylist )
}

# -----------------------------------------------------------------------------
# filePath
# Brief:                Given a domain name and a file name, return path+name
#                           (the fully qualified name).
# Details:              Default extension is ".csv" but this can be overridden
#                           This is the normal way for callers to get fqns for their files.
# Dependencies:         readDomainPathMap, printLog
# Author(s):            Ben Bond-Lamberty, Jon Seibert
# Params:
#   domain:             CEDS domain name under which the file can be found [required]
#   fn:                 Name of the file [required]
#   extension:          Extension of the file [default: ".csv"]
#   domain_extension:   Any additional path between the CEDS domain and the location of the
#                           file [default: ""]
# Return:               String of the file name appended to the full file path.
# Input Files:          None
# Output Files:         None
filePath <- function( domain, fn, extension = ".csv", domain_extension = "" ) {
	map <- readDomainPathMap()
	if( !domain %in% names( map ) ) {
	    stop( "Couldn't find domain ", domain, " in domain path map" )
	}

    if( endsWith( fn, extension ) ) extension <- ""

	return( paste0( map[ domain ], "/", domain_extension, fn, extension ) )
}

# listZippedFiles
# Brief: Retrieve full list of internal files
listZippedFiles <- function( file_path, remove_extension = FALSE ){
    contents <- unzip( file_path, list = TRUE )
    file_list <- contents$Name[ contents$Length > 0 ]
    if( length( file_list[ grep("MACOSX",file_list) ] ) > 0 ){
        file_list <- file_list[ -grep("MACOSX",file_list) ]
    }
    if( remove_extension ){ file_list <- tools::file_path_sans_ext( file_list ) }
    return( file_list )
}

# -----------------------------------------------------------------------------
# readData
# Brief:            Read an arbitrary data file.
# Details:          Reads a specified .csv, .xlsx, or .xls file (or multiple .csv files inside
#                       a .zip file) into R as a data frame or list of data frames. Handles singular
#                       .csv and .xlsx files (including multiple sheets within a single .xlsx file)
#                       and multiple .csv files within a .zip file. To read multiple unzipped files
#                       simultaneously, use the standard R function "lapply" in conjunction with
#                       readData.
# Dependencies:     filePath, printLog, read.csv, readExcel, lapply, readMetaData
# Author(s):        Page Kyle, Jon Seibert, Linh Vu
# params:
#   domain:             CEDS domain in which the file is located. [required]
#   file_name:          Name of the file to read in, minus the file extension. [required]
#   extension:          File extension (type): readData only accepts .csv, .xlsx, .xls, .zip files.
#                           [default: ".csv"]
#   zipped_selection:   List of one or more names of files to read from within the specified .zip file.
#                           It is not necessary to list all contained files- if one desires to read all
#                           files from within a .zip, use the extract_all option instead. This option
#                           is designed to read singlular or specific lists of files from within a .zip.
#                           For .zip files only. [default: list()]
#   zipped_extension:   Extension of file to read in inside .zip file. Currently only accepts .csv.
#                           For .zip files only. [default: ".csv"]
#   extract_all:        Boolean option indicating whether to read all files from within the specified
#                           .zip file. If FALSE (and attempting to read from a .zip file), a list of
#                           names must be supplied to the zipped_selection parameter. If TRUE,
#                           zipped_selection need not be used, and all contained files will be read.
#                           For .zip files only. [default: FALSE]
#   sheet_selection:    Either "ALL" (to read all sheets), a list of sheet names or numbers
#                           (to read a set of sheets), or single sheet name/number.
#                           For .xlsx .xls files only. [default: "ALL"]
#   trim_ws:            Should leading and trailing whitespace be trimmed?
#                           For .xlsx .xls files only. [default: "ALL"]
#   guess_max:          Maximum number of data rows to use for guessing column types.
#                           For .xlsx .xls files only. [default: 100]
#   domain_extension:   Additional filepath to follow after locating the specified domain.
#                           [default: ""]
#   missing_value:      Missing value. By default readxl converts blank cells to missing data.
#                           Set this value if you have used a sentinel value for missing values.
#                           For .xlsx .xls files only. [default: ""]
#   meta:               Boolean indicating whether to look for and read in metadata for the
#                           input file. [default: TRUE]
#   meta_domain:        CEDS domain in which the file's metadata is located. [default: domain]
#   meta_extension:     File extension for the file's metadata. [default: extension]
#   mute:               Boolean indicating whether to output progress messages. TRUE silences
#                           the function, while FALSE allows the messages. [default: FALSE]
#   to_numeric:         Boolean indicating whether to automatically convert year columns
#                           to numeric values. [default = TRUE]
# Return:           Data frame of the read-in file, or a list of data frames of
#                   multiple sheets from a .xlsx .xls file
# Input Files:      Specified in parameters
# Output Files:     None
#
# Usage examples: readData( "MED_OUT", "A.energy_data" )
#                 readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Sectors" )
#                 readData( "EM_INV", domain_extension = "US_EPA/", "SCCactiveJul2015_NFRmap", ".xlsx", sheet_selection = "SCCsActiveJul2015_NFRmap" )
#                 readData( "MAPPINGS", "Archive", ".zip", zipped_selection = "Master_Fuel_Sector_List" )
#                 readData( "UNFCCC_IN", em, ".zip", extract_all = TRUE, header = FALSE )
#
# TODO: error handling (probably using try/catch)
#       switch to read_csv from readr package
#       read Excel from inside .zip (?not possible: http://stackoverflow.com/questions/26763377/)
readData <- function( domain = NULL, file_name = NULL, extension = ".csv",
                      zipped_selection = list(), zipped_extension = ".csv",
                      extract_all = FALSE, sheet_selection = "ALL", trim_ws = FALSE,
                      guess_max = 100, domain_extension = "", missing_value = "",
                      meta = TRUE, meta_domain = domain, meta_extension = extension,
                      mute = FALSE, to_numeric = TRUE, ... ) {

	if ( is.null( domain ) || is.null( file_name ) ) {
		stop( "readData needs to receive both a filename and domain" )
	}

	full_file_path <- filePath( domain, file_name, extension, domain_extension )

    # Assign em so the correct [em]_IO_documentation can be found
	if ( !exists( 'em', envir = .GlobalEnv ) ) em <- "CM"

	# Update dependency list, if necessary
	deps <- DEPENDENCIES[[ GCAM_SOURCE_FN[ GCAM_SOURCE_RD ] ]]
	if( !( full_file_path %in% deps ) && file_name != "System_Documentation" && file_name != paste0( em, "_", "IO_documentation" ) ) {
		DEPENDENCIES[[ GCAM_SOURCE_FN[ GCAM_SOURCE_RD ] ]] <<- c( deps, full_file_path )
	}

	if( !mute ) printLog( "Reading", full_file_path, "\n", cr=F )
	if( !file.exists( full_file_path ) ) {
		stop( "readData cannot read non-existent file: ", full_file_path )
	}

    if( extension == ".csv" ) {
        x <- read.csv( full_file_path, comment.char = GCAM_DATA_COMMENT,
                       na.strings = missing_value, stringsAsFactors = F, ... )

    } else if( extension == ".xlsx" || extension == ".xls" ) {
        x <- readExcel( full_file_path, sheet_selection, missing_value, trim_ws,
                        guess_max, ... )


    } else if( extension == ".zip" ) {

        # .zip segment only accepts zipped .csv files at this time
        if( zipped_extension != ".csv" ) {
            stop( "Invalid inner file type. readData() only accepts .csv file ",
                  "inside .zip file." )
        }

        if( extract_all ) {
            zipped_selection <- listZippedFiles( full_file_path )
        } else {
            zipped_selection <- paste0( zipped_selection, zipped_extension )
        }

        # Error if no file is specified within zip
        if( length( zipped_selection ) == 0 ) {
            stop( "readData expects zipped file specified for ", file_name )
        }

        # Unzip and read each file into a list
        x <- lapply( zipped_selection, function( zipped_fn ) {
            read.csv( unz( full_file_path, zipped_fn ),
                      na.strings = missing_value, stringsAsFactors = F,
                      comment.char = GCAM_DATA_COMMENT, ... )
        })

    } else { # If extension not recognized
        stop( "Invalid file type. readData() accepts .csv, .xlsx, .xls files, ",
              "or .csv files inside a .zip file." )
    }

	# Pull out only data frame if multiple were expected, but just one was found
    if( !is.data.frame( x ) && length( x ) == 1 ) x <- x[[ 1 ]]

	if( !mute ) { # Print stats
        if( !is.data.frame( x ) ) {  # Multiple sheets read
            lapply( names(x), function( sheet ) {
                dm <- dim( x[[ sheet ]] )
                printLog( paste0( "    Done reading ", sheet, ": ",
                                  dm[1], " rows, ", dm[2], " cols" ), ts = F )
            })
        } else if( extension == ".zip" ) {
            printLog( "    Done reading contents." )
        } else {
            printLog( "    Done: ", nrow( x ), " rows, ", ncol( x ), " cols", ts = F )
        }
    }

    # Get metadata and add to list of all metadata in the script
    if( meta ) {
        metadata <- readMetaData( meta_domain, file_name, extension, meta_domain_ext = domain_extension )
    }

	# Convert years columns to numeric values if to_numeric == TRUE
	if( to_numeric ) {
	    yr_regex <- '^X[12]\\d{3}$'
	    if( is.data.frame( x ) ) {
    	    x <- mutate_at( x, vars( matches( yr_regex ) ), as.numeric )
	    } else {
            x <- lapply( x, function( df ) {
                dplyr::mutate_at( df, vars( matches( yr_regex ) ), as.numeric )
            } )
	    }
    }

	return( x )
}

# -----------------------------------------------------------------------------
# readExcel
# Brief:                Sub-function to read .xlsx .xls files.
# Details:              Handles read logic and parsing of .xlsx .xls files for
#                       readData(). The parameters trim_ws and guess_max
#                       override the defaults for read_excel() because CEDS has
#                       several instances where it expects the default values
#                       (from legacy code).
# Dependencies:         readxl
# Author(s):            Jon Seibert, Caleb Braun
# Params:
#   full_file_path:     Path to the .xlsx .xls file to read in [required]
#   sheet_selection:    Either "ALL" (to read all sheets), a vector of sheet names or numbers
#                           (to read a set of sheets), or single sheet name/number. [default: "ALL"]
#   missing_value:      Missing value. By default readxl converts blank cells to missing data.
#                           Set this value if you have used a sentinel value for missing values.
#                           [default: ""]
#   trim_ws:            Should leading and trailing whitespace be trimmed?
#   guess_max:          Maximum number of data rows to use for guessing column types.
#
# Return:               A list of data frames for each sheet specified by the
#                           parameter `sheet_selection`
#
# Input Files:          Specified in full_file_path
# Output Files:         None
#
# TODO: gsub to remove m-dashes does not work
readExcel <- function( full_file_path, sheet_selection = "ALL",
                       na = "", trim_ws = FALSE, guess_max = 100, ... ) {

    all_sheets <- readxl::excel_sheets( full_file_path )

    # Get sheet names based on the value of sheet_selection
    if ( length( sheet_selection ) == 1 && sheet_selection == "ALL" ) {
        sheet_names <- all_sheets
    } else if ( is.numeric( sheet_selection ) ) {
        sheet_names <- all_sheets[ sheet_selection ]
    } else {
        sheet_names <- all_sheets[ all_sheets %in% sheet_selection ]
    }

    # Error checking
    sheets_dropped <- length( sheet_names ) < length( sheet_selection )
    if ( sheets_dropped || any( is.na( sheet_names ) ) ) {
        stop( "Invalid sheet selection '",
              paste( sheet_selection, collapse = ', ' ),
              "' for file ", full_file_path )
    }

    x <- lapply( sheet_names, function( sheet ) {
        readxl::read_excel( full_file_path, sheet, na = na, trim_ws = trim_ws,
                            guess_max = guess_max, ... )
    })

    # Convert from tbl to data.frame and replace en-dashes/em-dashes with hyphens
    x <- lapply( x, as.data.frame, stringsAsFactors = F ) %>%
        setNames( gsub( "(—|–)", "-", sheet_names ) )

    return( x )
}


# -----------------------------------------------------------------------------
# sourceData
# Brief:        Read a .R file used to store data.
# Details:      Source a CEDS R script used to define shared data or calculate required values.
# Dependencies: printLog
# Author(s):    Ben Bond-Lamberty
# Params:
#   domain:     CEDS domain in which the script can be found [required]
#   fn:         Name of the script [required]
#   extension:  File extension for the script [default: ".R"]
# Return:       None
# Input Files:  None
# Output Files: None
# TODO: error handling (probably using try/catch)
sourceData <- function( domain="none", fn="none", extension=".R", ... ) {

	if( domain=="none" | fn=="none" ) {
		printLog( "ERROR: no domain/file specified", fn )
		stop( "Error: you need to specify both a filename and domain" )
	}

	myfn <- filePath( domain, fn, extension )

	# Update dependency list, if necessary
	deps <- DEPENDENCIES[[ GCAM_SOURCE_FN[ GCAM_SOURCE_RD ] ]]
	if( !( myfn %in% deps ) ) {
		DEPENDENCIES[[ GCAM_SOURCE_FN[ GCAM_SOURCE_RD ] ]] <<- c( deps, myfn )
	}

	printLog( "Reading", myfn, cr=F )
	if( !file.exists( myfn ) ) {
		printLog( "WARNING: file", myfn, "does not appear to exist" )
	}
	source( myfn )
	printLog( "...OK.", ts=F )
}

# -----------------------------------------------------------------------------
# addDependency
# Brief:        Adds a fully qualified name to the dependency list.
# Details:      Determines whether the parameter file name is already in the
#                   dependency list, and adds it if not.
# Dependencies: None
# Author(s):    Page Kyle
# Params:
#   fqn:        File name to add to the list [required]
# Return:       None
# Input Files:  None
# Output Files: None
# TODO: error handling (probably using try/catch)
addDependency <- function( fqn, ... ) {

	# Update dependency list, if necessary
	deps <- DEPENDENCIES[[ GCAM_SOURCE_FN[ GCAM_SOURCE_RD ] ]]
	if( !( fqn %in% deps ) ) {
		DEPENDENCIES[[ GCAM_SOURCE_FN[ GCAM_SOURCE_RD ] ]] <<- c( deps, fqn )
	}
}

# -----------------------------------------------------------------------------
# writeData
# Brief:            Write an arbitrary data file.
# Details:          Write out an R data frame as a .csv file in CEDS standard format.
# Dependencies:
# Author(s):        Page Kyle, Jon Seibert
# params:
#   x:                  Data frame to be output as a .csv file.
#   domain:             CEDS domain in which the file is located. [default: "MED_OUT"]
#   fn:                 Name of the file to write, minus the file extension.
#                       [default: GCAM_SOURCE_FN]
#   fn_sfx:             Suffix to append to the file name. [default: NULL]
#   comments:           Output comments to be included at the top of the output file.
#                       [default: NULL]
#   meta:               Boolean indicating whether to look for and read in metadata for the
#                       input file. [default: TRUE]
#   mute:               Boolean indicating whether to output progress messages. TRUE silences
#                       the function, while FALSE allows the messages. [default: FALSE]
#   domain_extension:   Additional filepath to follow after locating the specified domain.
#                       [default: ""]
#
# Return:               None
# Input Files:          None
# Output Files:         Specified in "fn"
#
# Usage examples: writeData( A.energy_data, "MED_OUT", "A.energy_data" )
#                 writeData( Xwalk_onetab, "EM_INV", domain_extension = "US_EPA/Processed_data/", fn = "Xwalk_onetab" )
# TODO:
writeData <- function( x, domain = "MED_OUT", fn = GCAM_SOURCE_FN, fn_sfx = NULL,
                       comments = NULL, meta = TRUE, mute = FALSE, domain_extension = "", ... ) {

	if( !is.null( fn_sfx ) ) {
		myfn <- paste0( fn, "_", fn_sfx )
		stop( "fn_sfx parameter does not do anything" )
	}
	myfn <- filePath( domain, fn, domain_extension = domain_extension )

    full_fn <- paste0( fn, ".csv" )

	# Assign em so the correct [em]_IO_documentation can be found
	if ( !exists( 'em', envir = .GlobalEnv ) ) em <- "CM"

    # Update dependency list, if necessary
	outs <- OUTPUTS[[ GCAM_SOURCE_FN[ GCAM_SOURCE_RD ] ]]
	if( !( full_fn %in% outs ) && fn != paste0( em, "_", "IO_documentation" ) ) {
		OUTPUTS[[ GCAM_SOURCE_FN[ GCAM_SOURCE_RD ] ]] <<- c( outs, full_fn )
	}

    # Ensure all data entries are strings, not lists, to avoid strange errors in write.csv
    x <- data.frame( lapply( x, as.character ), stringsAsFactors = FALSE )

	if( !mute ) printLog( "Writing", myfn, "w/", length( comments ), "comments" )

	tryCatch( {
		# Write the comments, if any, then the data
		cat( paste( GCAM_DATA_COMMENT, myfn ), file=myfn, sep="\n" )
		cat( paste( GCAM_DATA_COMMENT, "Written by", GCAM_SOURCE_FN[ GCAM_SOURCE_RD ] ), file=myfn, sep="\n", append=T )
		cat( paste( GCAM_DATA_COMMENT, date() ), file=myfn, sep="\n", append=T )
		for( i in seq_along( comments ) ) {
			cat( paste( GCAM_DATA_COMMENT, "\"", comments[ i ], "\"" ), file=myfn, sep="\n", append=T, ... )
		}

		w <- getOption( "warn" )
		options( warn=-1 )		# suppress the warning about columns names and appending
		# write.table( x, file = myfn, sep = ",", row.names = F, col.names = T, append = T, ... )
		write.csv( x, file = myfn, row.names = F, append = T, ... )
		options( warn=w )

		}, error=function( err ) {
			printLog( "Error in write of", fn )
			printLog( as.character( err ) )
			stop( err )		# can't recover from this
		}

	) # tryCatch

    # Write out accumulated metadata and notes if applicable
    if( meta == T ) {
        mymetafn <- filePath( domain = domain, fn = paste0(fn, "-metadata"),
                              domain_extension = domain_extension)
        w <- getOption( "warn" )
        options( warn = -1 )  # suppress the warning
        if( exists( "all_metadata" ) ) {
			# Remove duplicate entries if necessary
			all_metadata <- all_metadata[ !duplicated( all_metadata ), ]

            write.table( all_metadata, file=mymetafn, sep=",", row.names=F,
                         col.names=T, append=F, ... )
        }
        options( warn=w )
    }

}

# ------------------------------------------------------------------------------
# readMetaData
# Brief:        Creates and accumulates metadata in a script
# Details:      When first called, the function creates an object to store all the
#                   metadata of a script. On subsequent calls, the function adds metadata
#                   to this object. The function is automatically called by readdata, and
#                   can also be used as a standalone function. If metadata is not created
#                   for a file, a default template will be used. Additional categories
#                   can be added by adding them directly to the metadata file.
# Dependencies: printLog(), filePath()
# Author(s):       Tyler Pitkanen, Jon Seibert
# parameters:
#   meta_domain:    The domain of the metadata [default: NULL]
#   file_name:      The file name of the data set that corresponds to the metadata. In
#                       standard naming format, the file name of the metadata is the same
#                       as the file name of the regular data, but appended with '-metadata'.
#                       file_name can be used to find the metadata of a specified file. [default: NULL]
#   meta_name:      The file name of the metadata. This can be specified instead of file_name
#                       if preferred. One must be specified. [default: NULL]
# return:       all_metadata
# input files:  metadata file, e.g. 'example-metadata.csv'
# output files: null
readMetaData <- function( meta_domain = NULL, file_name = NULL, file_extension = "csv",
                          meta_name = NULL, meta_domain_ext = "" ) {

    if( is.null( meta_domain ) || ( is.null( file_name ) && is.null( meta_name ) ) ) {
        return( warning( 'Must specify domain and file name' ) )
    }

    file_name <- sub( '\\.csv$', '', file_name )

    # Determine new metadata file name and location
    if( is.null( meta_name ) ) meta_name <- paste0( file_name, "-metadata" )
    mymeta_name <- filePath( meta_domain, meta_name, '.csv', meta_domain_ext )

    # Check if the file exists and set 'new_metadata' object. If the file isn't
    #   found, make a note
    if( file.exists( mymeta_name ) ) {
        new_metadata_exists <- TRUE
        new_metadata <- read.csv( mymeta_name, na.strings="", check.names = FALSE )
        new_metadata <- data.frame( new_metadata, row.names = NULL )
    } else new_metadata_exists <- FALSE

    # If the object all_metadata doesn't exist, create it. This code should run once
    #   per script upon the first addition of metadata.
    if( !exists( 'all_metadata' ) ) {
        # If the new metadata file doesn't exist, use a default metadata placeholder
        if( !new_metadata_exists ) {
            default_names <- c( 'Data-Type', 'Emission', 'Region', 'Sector',
                                'Start-Year', 'End-Year', 'Source/Comment' )
            colsize <- length( default_names )
            new_metadata <- c( rep( "Unknown", colsize - 1 ),
                               paste0( "Metadata input file missing for data file ", file_name ) )
            new_metadata <- data.frame( t( new_metadata ), row.names = NULL )
            colnames( new_metadata ) <- default_names
        }

        #add the source column as a metadata column in the data frame
        new_metadata$Source <- paste0( file_name, file_extension )

        # Since no other metadata exists, set new_metadata as all_metadata
        all_metadata <- new_metadata

        # re-arrange the columns so that the 'Source' column is the fist column
        # in the data frame
        all_metadata <- dplyr::select( all_metadata, Source, everything() )

        # Return the all_metadata object and end the function
        assign( 'all_metadata', all_metadata, .GlobalEnv )
        return( invisible( all_metadata ) )
    } else {

    # If the object all_metadata does already exist, append the new metadata and
    #   update the all_metadata object
        all_metadata <- data.frame( all_metadata, row.names = NULL )
        old_names <- names( all_metadata )

        if( new_metadata_exists ) {
            all_metadata <- bindMetaDataRecord( all_metadata, new_metadata, old_names,
                                                file_name, file_extension )
        } else {
            # If the metadata file doesn't exist, create a default entry with the
            new_metadata <- c( rep( "Unknown", ncol( all_metadata ) - 1 ),
                               paste0( "Metadata input file missing for data file ", file_name ) )
            new_metadata <- data.frame( t( new_metadata ), row.names = NULL )
            colnames( new_metadata ) <- old_names

            new_metadata$Source <- paste0( file_name, file_extension )

            all_metadata <- rbind.fill( all_metadata, new_metadata )
        }

        # re-arrange the columns so that the 'Source' column is the fist column
        # in the data frame and 'Source.Comment' is the last
        all_metadata <- all_metadata %>%
            dplyr::select( Source, everything() ) %>%
            dplyr::select( -Source.Comment, Source.Comment )

        assign( 'all_metadata', all_metadata, .GlobalEnv )
    }
}

# ------------------------------------------------------------------------------
# addMetaData
# Brief: Allows a user to type out a metadata addition within a script
# Details: If called before readMetaData, the function creates an object to store
#          the metadata of a script. On subsequent calls, the function adds
#          metadata to this object. Functionality is similar to readMetaData,
#          except it creates a metadata entry from R objects in a script rather
#          than loading from a csv file.
# Dependencies: printlog()
# Author: Tyler Pitkanen
# parameters:
#   metadata: List of strings containing metadata information [default: NULL]
#   metadata_names: List of strings that will be used as the category names
#                   for the metadata [default: NULL]
# return: all_metadata
# input files: metadata file, e.g. 'example-metadata.csv'
# output files: null
addMetaData <- function( metadata = NULL, metadata_names = NULL, source_info =" " ) {

    # Load defaults for created data and names if unspecified
    if( is.null( metadata_names ) || is.null( metadata ) ) {
        return( warning( 'Metadata and names must be specified' ) )
    } else new_metadata_exists <- TRUE

    # Assign the specified names to the metadata. Return a warning if there isn't
    #   one column name per column
    if( length( metadata ) != length( metadata_names ) ) {
        return( warning( 'Metadata and names must have same length' ) )
    } else {
        # Make inputs into a data.frame and apply names
        metadata <- data.frame( t( metadata ), row.names = NULL )
        colnames( metadata ) <- metadata_names
        new_metadata <- metadata
    }

    # If the file extension is not ".R" set it to the empty string. Remove any
    # ".R" from the source info.
    source_extension <- ifelse( grepl( "\\.R$", source_info, TRUE ), ".R", "" )
    source_info <- sub( "\\.R$", "", source_info, TRUE )

    # If the object all_metadata doesn't exist, create it. This should occur once
    #   per script upon the first addition of metadata.
    if( exists( 'all_metadata' ) == FALSE ) {
        # Since no other metadata exists, set new_metadata as all_metadata
        all_metadata <- new_metadata
        # Return the all_metadata object and end the function
        assign( 'all_metadata', all_metadata, .GlobalEnv )
        return( invisible( all_metadata ) )
    }

    # If the object all_metadata does already exist, append the new metadata and
    #   update the all_metadata object
    all_metadata <- data.frame( all_metadata, row.names = NULL )
    old_metadata <- all_metadata
    old_names <- colnames( old_metadata )

    if( new_metadata_exists ) {
        all_metadata <- bindMetaDataRecord( all_metadata, new_metadata, old_names,
                                            source_info, source_extension )

        # re-arrange the columns so that the 'Source' column is the fist column
        # in the data frame and 'Source.Comment' is the last
        all_metadata <- all_metadata %>%
            dplyr::select( Source, everything() ) %>%
            dplyr::select( -Source.Comment, Source.Comment )

        assign( 'all_metadata', all_metadata, .GlobalEnv )
    }
}

# ------------------------------------------------------------------------------
# writeMetaData
# Brief: Writes out a script's metadata as a csv file
# Details: Writes out the all_metadata object created by readMetaData and/or
#          addMetaData
# Dependencies: all_metadata, filePath()
# Author: Tyler Pitkanen
# parameters:
#   domain: The domain of the printed metadata, often equal to the file's domain
#   file_name: The file name of the data set that corresponds to the metadata. In
#       standard naming format, the file name of the metadata is the same
#       as the file name of the regular data, but appended with '-metadata'.
#       file_name can be used to find the metadata of a specified file. [default: NULL]
#   meta_name: The file name of the metadata. This can be specified instead of file_name
#           if preferred. One must be specified. [default: NULL]
# return: null
# input files: null
# output files: metadata file, e.g. 'example-metadata.csv'

writeMetaData <- function( domain, file_name = NULL, meta_name = NULL ) {

    # Set the metadata file name
    if( is.null( meta_name ) ) mymeta_name <- filePath( domain, paste0( file_name, "-metadata" ) )

    # Suppress the warning from write-out
    w <- getOption( "warn" )
    options( warn = -1 )

    # Write out all_metadata
    if( exists( "all_metadata" ) ) {
        # Remove duplicate entries if necessary
        all_metadata <- all_metadata[ !duplicated( all_metadata ), ]

        write.table( all_metadata, file=mymeta_name, sep=",", row.names=F,
                     col.names=T, append=F, ... )
    }

    options( warn=w )
}


# -----------------------------------------------------------------------------
# bindMetaDataRecord
# Brief:        Bind a record to existing metadata
# Authors:      Caleb Braun
bindMetaDataRecord <- function( all_metadata, new_metadata, old_names, fname, fext ) {
    new_names <- names( new_metadata )

    # Check if any new categories are included in the newly added metadata. If
    #   new columns are included, update the old metadata
    new_cols <- !( new_names %in% old_names )
    if ( any( new_cols ) ) {
        # Print a log message noting the addition of new columns
        printLog( 'New categories added for', fname, 'metadata: ',
                  paste( new_names[ new_cols ], collapse = ',' ) )
    }

    # add the 'Source' column
    new_metadata$Source <- paste0( fname, fext )

    # bind the new metadata record
    rbind.fill( all_metadata, new_metadata )
}


# -----------------------------------------------------------------------------
# clearMeta
# Brief:        Clear the metadata accumulated thus far.
# Details:      Removes the global variable "all_metadata". Should be called at the
#                   beginning of each script.
# Dependencies: None
# Author(s):    Tyler Pitkanen
# Params:       None
# Return:       None
# Input Files:  None
# Output Files: None
clearMeta <- function( ) {

    if( exists( "all_metadata" ) ) {
        rm(all_metadata, pos = ".GlobalEnv")
    }
}

# -----------------------------------------------------------------------------
# savePlot
# Brief:        Save a ggplot.
# Details:      Saves the most recent plot as a .pdf file.
# Dependencies: None
# Author(s):    Caleb Braun
# Params:
#   domain:     CEDS domain in which the file is located.
#   domain_ext: File subdirectory within the specified domain. (Optional)
#   file_name:  Name of the file to write out, minus the file extension.
#   width:      Width of the plot in inches.
#   height:     Height of the plot in inches.
# Return:       None
# Input Files:  None
# Output Files: Specified in "fn".
savePlot <- function( domain, domain_ext, file_name, width, height ) {

    fp <- filePath( domain, file_name, extension = '.pdf', domain_extension = domain_ext)
    ggsave( fp , width = width, height = height )
}
# -----------------------------------------------------------------------------