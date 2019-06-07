# ------------------------------------------------------------------------------
# Program Name: E.REAS_emissions.R
# Author(s): Rachel Hoesly, Leyang Feng
# Date Last Updated: March 16, 2016
# Program Purpose: To read in & reformat REAS emissions data.
# Input Files: All REAS data
# Output Files: E.em_REAS_inventory.csv
# Notes: 1. added step 1.1 to manually remove mmr ncomb-industry-copper emissions:
#           the copper emissions for mmr might be mixed with copper minning so being removed.
# Notes: 2. also removed metal process SO2 emissions since we have better estimates, largely
#           from country reports
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
    log_msg <- "Initial reformatting of REAS Emissions" # First message to be printed to the log
    script_name <- "E.REAS_emissions.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )
# Describes which emission species is being analyzed
    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[1]
    if ( is.na( em ) ) em <- "SO2"

# ------------------------------------------------------------------------------
# 0.5 Settings/Load Files & Convert all txt files to csv
#     logging does not support txt files, so convert to csv

    MCL <- readData( "MAPPINGS", "Master_Country_List" )
    loadPackage( 'tools' )


# Stop script if running for unsupported emissions species
    if ( em %!in% c( 'BC', 'CH4', 'CO', 'CO2', 'NH3', 'N2O', 'NMVOC', 'NOx',
                     'OC', 'SO2' ) ) {
        stop ( paste( 'REAS script is not supported for emission species', em ) )
    }

    em.read <- em
    if ( em == "NMVOC" ) em.read <- "NMV"
    if ( em == "NOx" ) em.read <- "NOX"

# ------------------------------------------------------------------------------
# 1. Read in Data

# create temporary folder to extract zipped files
    reas_dir <- 'emissions-inventories/REAS_2.1/'
    inv_name <- "REAS"
    zipfile_path <- paste0(reas_dir, em.read, '.zip')
    dir.name <- paste0(reas_dir, em, '_', inv_name, '_temp_folder')
    dir.create(dir.name)
# unzip files to temp folder
    unzip( zipfile_path, exdir = dir.name )

# list files in the folder
    files <- list.files( paste0( dir.name, '/', em.read ), pattern = '.txt' )

# define function to read in and process single file

    read_process_reas <- function ( file_name ) {

    # read files
        read_in_data <- read.table( paste0(reas_dir,em,'_',inv_name,
                                           '_temp_folder/',em.read,'/',
                                           file_name) ,
                                    stringsAsFactors = FALSE,
                                    col.names = paste( 'temp_col', 1:50 ),
                                    strip.white = T,
                                    fill = T )

    # Assign column names
        col_names <- c( 'FUEL_TYPE',
                        unlist( read_in_data[ which( read_in_data[ , 1 ] == 'Fuel' ),
                                              3:50 ] ) )

    # Change OTHERS to OTHERS_IND
        if ( length( which( col_names %in% 'OTHERS') ) > 1 ) {
            col_names[ which( col_names %in% 'OTHERS') [ 1 ] ] <- 'OTHERS_IND'
        }

    # get a list of the column names in lowercase and set them to that
        col_names <- tolower( col_names )
        names( read_in_data ) <- col_names
    # drop NA columns
        read_in_data <- read_in_data[ , col_names[ !is.na( col_names ) ] ]
        names( read_in_data )[ which( names( read_in_data ) ==
                                        'sub_total' ) ] <- 'total_combustion'

    # get country year units data
        country <- read_in_data[ which( read_in_data[ , 1 ] == 'Country' ), 3 ]
        year <- read_in_data[ which( read_in_data[ , 1 ] == 'Year' ), 3 ]
        sub_region <- read_in_data[ which( read_in_data[ , 1 ] == 'Sub-Region' ), 3 ]
        units <- read_in_data[ which( read_in_data[ , 1 ] == 'Combustion' ), 3 ]


    # seperate combustion and process data (different shape)
        combustion_data <- read_in_data[ 6:23, ]
        non_combustion_data <- read_in_data[ 25:nrow( read_in_data ), ]

    # Format combustion
        combustion_data$fuel_type <- tolower( combustion_data$fuel_type )
        combustion_data <-
              combustion_data[ which( !combustion_data[ , 1 ] == 'sub_total' ), ]

        combustion_data$total_combustion <- NULL
        combustion_data <- melt( combustion_data , id.vars = 'fuel_type' )
        names(combustion_data) <- c( 'fuel_type', 'sector', 'emissions' )
        combustion_data$sector <- paste0( 'comb', '-', combustion_data$sector )

    # format non_combustion
        non_combustion_data <-
            non_combustion_data[ which( !non_combustion_data[ , 1 ] %in% 'Sector' ), ]
        non_combustion_data <-
            non_combustion_data[ which( !non_combustion_data[ , 1 ] %in% 'SUB_TOTAL' ), ]
        non_combustion_data <-
            non_combustion_data[ which( !non_combustion_data[ , 1 ] %in% 'Total' ), ]
        non_combustion_data <-
            non_combustion_data[ which( !non_combustion_data[ , 1 ] %in% 'TOTAL' ), ]

    # Get the location of the sector indicator
        sector_indicator <- which( non_combustion_data == '[t/year]',
                                                          arr.ind = T )
        sector_indicator <- as.matrix( sector_indicator )
        sector_indicator <- as.data.frame( sector_indicator )
        sector_indicator <- sector_indicator[ order( sector_indicator[ , 1 ] ), ]

        temp_non_combustion_storage <- c()

    # Loop through each sector indicator (shows where sectors begin)
        for ( i in 1 : nrow( sector_indicator ) ) {
        # Which sector?
            sector_name <-
                paste( non_combustion_data[ sector_indicator[ i, 1 ],
                                            1:( sector_indicator[ i, 2 ] - 1 ) ],
                                  collapse = '_' )
        # Which row is the first data row
            data_start_row <- sector_indicator[ i, 1 ] + 1
        # Which row is the last data row
            data_end_row <- ifelse( i == nrow( sector_indicator ),
                                    nrow( non_combustion_data ),
                                    ( sector_indicator[ i + 1, 1 ] - 1 ) )
        # Extract the data and add to the storage dataframe
            temp_data <- non_combustion_data[ data_start_row:data_end_row , 1:2 ]
            temp_data[ , 1 ] <- paste0( sector_name, '-', temp_data[ , 1 ] )
            temp_non_combustion_storage <- rbind( temp_non_combustion_storage,
                                                  temp_data )
        }

    # Rename sectors
        colnames( temp_non_combustion_storage ) <- c( 'sector', 'emissions' )
    # Convert sector to lowercase
        temp_non_combustion_storage$sector <-
                     tolower( temp_non_combustion_storage$sector )
        non_combustion_data <- temp_non_combustion_storage
    # Convert sectors to non_comb and fuels to process
        non_combustion_data$fuel_type <- 'process'
        non_combustion_data$sector <- paste0( 'ncomb', '-',
                                              non_combustion_data$sector )

    # Combine combustion and non-combustion
        all_data <- rbind.fill( combustion_data, non_combustion_data )
        all_data$year <- paste0( 'X', year )
        all_data$country <- country
        all_data$sub_region <- sub_region

    # Convert emissions to numeric, kt from tonnes
        all_data$emissions <- as.numeric( all_data$emissions )
        all_data$emissions <- all_data$emissions / 1000
        all_data$units <- 'kt'

        all_data <- all_data[ , c( 'sub_region', 'country', 'sector',
                                   'fuel_type', 'units', 'year', 'emissions' ) ]

        return ( all_data )
    }


    if ( length( files ) > 0 ) {
    # apply function to list of files
        reas_data_list <- lapply( X = files , FUN = read_process_reas )

    #    filelist <- list()
    #    for ( i in seq_along(files)) filelist[i] <- read_process_reas(files[i])
    # #
   # delete temp folder
        unlink( dir.name, recursive = TRUE )

    # bind all data together and cast to wide format
        reas_data <- do.call( "rbind", reas_data_list )

    # Remove province/state level data and only keep country totals
        reas_data <- reas_data[ which( reas_data$sub_region == "WHOL" ), ]

    # convert iso to lowercase
        reas_data$iso <- tolower( reas_data$country )
    # Cast to wide
        reas_data_wide <- cast( reas_data, sub_region + iso + sector +
                                          fuel_type + units ~ year,
                                value = 'emissions' )

   } else {
   # if no data to process for this emissions species, create dummy file.
       reas_data_wide <- data.frame()
   }

# ------------------------------------------------------------------------------
# 2. Remove some specific emissions

# Additional step to remove mmr ncomb-industry-copper emissions -- see note(1)
    reas_data_wide <-
        reas_data_wide[ !( reas_data_wide$iso == 'mmr' &
                           reas_data_wide$sector == 'ncomb-industry-copper' ), ]

# Additional step to remove kaz metal processing emissions (particularly SO2) --
# see note(2)
    reas_data_wide <-
      reas_data_wide[ !( reas_data_wide$iso == 'kaz' &
                         reas_data_wide$sector == 'ncomb-industry-copper' ), ]
    reas_data_wide <-
      reas_data_wide[ !( reas_data_wide$iso == 'kaz' &
                         reas_data_wide$sector == 'ncomb-industry-lead' ), ]
    reas_data_wide <-
      reas_data_wide[ !( reas_data_wide$iso == 'kaz' &
                         reas_data_wide$sector == 'ncomb-industry-zinc' ), ]
    reas_data_wide <-
      reas_data_wide[ !( reas_data_wide$iso == 'kaz' &
                         reas_data_wide$sector == 'ncomb-industry-aluminum_alumina' ), ]

# ------------------------------------------------------------------------------
# 2. Output
# Write Data:
    writeData( reas_data_wide, domain = "MED_OUT",
               fn = paste0( "E.", em, "_REAS_inventory" ), meta = TRUE )

# Diagnostic files:
    writeData( unique( reas_data$iso ), domain = "DIAG_OUT",
               fn = paste0( "E.", em, "_REAS_countries" ), meta = FALSE )
    writeData( unique( reas_data$sector ), domain = "DIAG_OUT",
               fn = paste0( "E.", em, "_REAS_sectors" ), meta = FALSE )

# Every script should finish with this line-
    logStop()

# END
