#------------------------------------------------------------------------------
# Program Name: C2.1.base_NC_EF.R
# Author: Jon Seibert, Steve Smith
# Date Last Modified: 15 June 2016
# Program Purpose: To calculate default emissions factors from the process emissions
#                  and activity databases, and use them to generate the base process
#                  emissions factors database.
# Input Files: A.NC_activity.csv, C.[em]_NC_emissions.csv
# Output Files: C.[em]_NC_EF.csv
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
    headers <- c( "data_functions.R",'interpolation_extention_functions.R') # Additional function files required.
    log_msg <- "Generation of process emissions factors" # First message to be printed to the log
    script_name <- "C2.1.base_NC_EF.R"
    
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )
    
# ------------------------------------------------------------------------------
# 1. Read in files

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "NOx"
em_lc <- tolower( em )

library( "zoo" )

activity_data <- readData( "MED_OUT", "A.NC_activity" )

emissions_data_read <- readData( "MED_OUT", paste0( "C.", em, "_NC_emissions" ) )

emissions_replaced <- readData( domain = "DEFAULT_EF_IN" , 
                                domain_extension = "non-combustion-emissions/", 
                                paste0('C.',em,'_NC_emissions_user_added') )

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
 
# ------------------------------------------------------------------------------
# 3. Correct Edgar Emissions Factors
 
 # Fix Edgar EFs
 temp_EDGAR_end_year = EDGAR_end_year
 EDGAR_replace_years <- paste0('X',(temp_EDGAR_end_year+1):end_year)
 
 # Re-set EDGAR end-year if are still using EDGAR 4.2
 if( em == "CH4" ) temp_EDGAR_end_year = 2008
 
 new_efs_corrected <- new_efs[,c('iso','sector','fuel','units',paste0('X',start_year:temp_EDGAR_end_year))]

  #Extend EFs as constant after EDGAR end date
  new_efs_corrected[,EDGAR_replace_years] <- new_efs[ , paste0( 'X', temp_EDGAR_end_year )  ]
  
  #Extend EFs as constant before EDGAR start date
  new_efs_corrected[ , paste0( 'X', start_year:( EDGAR_start_year - 1 ) ) ] <- new_efs[ , paste0( 'X', EDGAR_start_year )  ]
 
# ------------------------------------------------------------------------------
# 4. Re-Correct Non Edgar Emissions Factors (replaced in C1.3.proc_NC_emissions_user_added.R)
#    Extend Non Edgar Emission Factors  (replaced in C1.3.proc_NC_emissions_user_added.R)
  
  # replace EF that we want to remain unchaned
  new_efs_corrected_user_added <-  new_efs_corrected
  replaced_years <- emissions_replaced
  
  if( nrow(replaced_years)>0 ){  
  # use emissions_replaced for a template to grab EFs before edgar correct
  extend_replaced_emissions <- emissions_replaced
  years_emissions_replaced <- names(emissions_replaced)[grep('X', names(emissions_replaced))]
  extend_replaced_emissions[X_emissions_years[ X_emissions_years %!in% years_emissions_replaced ]] <- NA
  extend_replaced_emissions <- extend_replaced_emissions[, c( 'iso','sector','fuel','units', X_emissions_years ) ]
  
  # save position for efs we want to extend (must be NA)
  emissions_NAs <- is.na(extend_replaced_emissions)
  
  extend_replaced_efs <- extend_replaced_emissions 
  extend_replaced_efs[ X_emissions_years ] <- NA
  extend_replaced_efs <- extend_replaced_efs[, c( 'iso','sector','fuel','units', X_emissions_years ) ]
  
  # get orignal EF
  extend_replaced_efs <- replaceValueColMatch( extend_replaced_efs  , new_efs,
                                              match.x = c('iso','sector','fuel'),
                                              x.ColName = c( 'units', X_emissions_years ),
                                              addEntries = FALSE )
  
  # write NAs over EFs we want to extend over
  extend_replaced_efs[ emissions_NAs ] <- NA
  extend_replaced_efs[ extend_replaced_efs == 0 ] <- NA
  
  # drop rows of all NAs
  all.na <- function(x){return(all(is.na(x)))}
  extend_replaced_efs <- extend_replaced_efs[ !apply(extend_replaced_efs[,X_emissions_years], 1, all.na), ]
  
  # keep last data row for every unique iso+sector+fuel+units
  extend_replaced_efs$num_row <- seq( 1, nrow( extend_replaced_efs ) )
  extend_replaced_efs <- group_by( extend_replaced_efs, iso, sector, fuel, units ) %>%
    filter( num_row == max( num_row ) ) %>% data.frame()
  extend_replaced_efs$num_row <- NULL
  
  # interpolate over NAs
  extend_replaced_efs[, X_emissions_years ] <- interpolate_NAs( extend_replaced_efs[, X_emissions_years ] )
  
  # extend last non-NA forward/backward
  extend_replaced_efs <- melt( extend_replaced_efs, id = c( "iso", "sector", "fuel", "units" ) )
  extend_replaced_efs <- ddply( extend_replaced_efs, .( iso, sector, fuel, units ), function( df ){
    df$value <- na.locf( df$value, na.rm = F )
    df$value <- na.locf( df$value, na.rm = F, fromLast = T )
    return( df )
  })
  
  extend_replaced_efs <- cast( extend_replaced_efs )
  writeData(extend_replaced_efs, domain = 'DIAG_OUT', fn = paste0( 'C.',em,'_replacement_process_EFs'),
            meta = F)
  
  # replace in larger data frame
  new_efs_corrected_user_added <- replaceValueColMatch(new_efs_corrected_user_added,extend_replaced_efs,
                                               match.x = c('iso','sector','fuel','units'),
                                               x.ColName = X_emissions_years,
                                               addEntries = FALSE )
}


# Logic Check for negative emissions factors
if (any(new_efs_corrected_user_added < 0) ) stop('There are negative EFs in', paste0( "C.", em, "_", "NC", "_EF" ))
  
# --------------------------------------------------------------------------------------------
# 5. Output
  
writeData( new_efs_corrected_user_added, domain = "MED_OUT", fn = paste0( "C.", em, "_", "NC", "_EF" ) )

logStop()
# END
