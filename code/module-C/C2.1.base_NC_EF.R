#------------------------------------------------------------------------------
# Program Name: C2.1.base_NC_EF.R
# Author: Jon Seibert
# Date Last Modified: July 7, 2015
# Program Purpose: To calculate default emissions factors from the process emissions
#                  and activity databases, and use them to generate the base process
#                  emissions factors database.
# Input Files: A.NC_activity.csv, C.[em]_NC_emissions.csv
# Output Files: C.[em]_NC_EF_db.csv
# Notes: 
# TODO:
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Before we can load headers we need some paths defined. They may be provided by
#   a system environment variable or may have already been set in the workspace.
# Set variable PARAM_DIR to be the data system directory
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
    headers <- c( "data_functions.R" ) # Additional function files required.
    log_msg <- "Generation of process emissions factors" # First message to be printed to the log
    script_name <- "C2.1.base_NC_EF.R"
    
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )
    
# ------------------------------------------------------------------------------
# 1. Read in files

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"
em_lc <- tolower( em )

activity_data <- readData( "MED_OUT", "A.NC_activity" )

emissions_data_read <- readData( "MED_OUT", paste0( "C.", em, "_NC_emissions" ) )

# ------------------------------------------------------------------------------
# 2. Generate emissions factors and extendForward (constant) as necessary

# Check if emissions data and activity data are similar. There may be countries 
# in emissions data, not in activity data - that's ok (like ant) It means that the
# country is in Edgar emissions data - or other process data, but not in IEA energy
# balances

check <- rbind(setdiff(emissions_data_read$sector,activity_data$sector),
               setdiff(emissions_data_read$sector,activity_data$sector))
if( length ( check ) > 0 ) {
  stop(paste("Missing sectors in NC activity and emissions data. Please check",
             "C2.1base_NC_EF.R which uses A.NC_activity.csv and",
             paste0( "C.", em, "_NC_emissions.csv" ) ))
}

# Ef * Activity = Emissions
# Divide emissions data by activity data to generate emissions factors:

# 0/0 must be set to 0.

# Fill out a blank template, identical to activity data, with emissions data
 ef_template <- activity_data
 ef_template[X_emissions_years] <- 0
 ef_template$units <- NA

# populate template with emissions data
 emissions_data <- replaceValueColMatch( ef_template, emissions_data_read,
                                     x.ColName = X_emissions_years,
                                     match.x = c('iso','sector','fuel'),
                                     addEntries = FALSE)
 emissions_data <- replaceValueColMatch( emissions_data, emissions_data_read,
                                              x.ColName = 'units',
                                              match.x = c('iso','sector','fuel'),
                                              addEntries = FALSE)
  
# sort activity and emissions and check
 activity_data <- activity_data[ with ( activity_data, order( iso, sector, fuel ) ), ]
 emissions_data <- emissions_data[ with ( emissions_data, order( iso, sector, fuel ) ), ]
 ef_template <- ef_template[ with ( ef_template, order( iso, sector, fuel ) ), ]
  
 if( ! identical(activity_data[ , c( 'iso' , 'sector' , 'fuel' ) ],
                emissions_data[ , c('iso' , 'sector' , 'fuel' ) ] )){ 
          stop( paste('activity and emissions data do not match. Please check',
                      "C2.1base_NC_EF.R") ) }
 
# calculate EFs
 new_efs <- ef_template
 
 if( ! identical(activity_data[ , c( 'iso' , 'sector' , 'fuel' ) ],
                  new_efs[ , c('iso' , 'sector' , 'fuel' ) ] ) ){ 
   stop( paste('activity and new_efs data do not match. Please check',
               "C2.1base_NC_EF.R" ) ) }
   
 new_efs$units <- paste0( emissions_data$units, "/", activity_data$units )
 new_efs[ X_emissions_years ] <- as.matrix(emissions_data[ X_emissions_years ]) / as.matrix(activity_data[ X_emissions_years ])
 
 # replace NA, Inf with zero
 new_efs[ X_emissions_years ] <- replace( new_efs[ X_emissions_years ] , is.na(new_efs[ X_emissions_years ]), 0)
 new_efs[ X_emissions_years ] <- replace( new_efs[ X_emissions_years ] , new_efs[ X_emissions_years ] == 'Inf', 0)
 
 new_efs_corrected <- new_efs[,c('iso','sector','fuel','units',paste0('X',start_year:EDGAR_end_year))]
 
#Extend EFs as constant after EDGAR end date
new_efs_corrected[,paste0('X',(EDGAR_end_year+1):end_year)] <- new_efs[ , paste0( 'X', EDGAR_end_year )  ]

#Extend EFs as constant before EDGAR start date
new_efs_corrected[ , paste0( 'X', start_year:( EDGAR_start_year - 1 ) ) ] <- new_efs[ , paste0( 'X', EDGAR_start_year )  ]


# --------------------------------------------------------------------------------------------
# 3. Output

writeData( new_efs_corrected, domain = "MED_OUT", fn = paste0( "C.", em, "_", "NC", "_EF_db" ) )

logStop()
# END
