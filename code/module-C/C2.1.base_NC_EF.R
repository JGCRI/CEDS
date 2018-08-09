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
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R",'interpolation_extension_functions.R', "timeframe_functions.R") # Additional function files required.
    log_msg <- "Generation of process emissions factors" # First message to be printed to the log
    script_name <- "C2.1.base_NC_EF.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )


# ------------------------------------------------------------------------------
# 0.5. Define function, load Packages

# Define functions
# extendFF(): Interpolate and extend fossil fuel fraction values
# Params:  data frame containing base adjustment values. Should have the following format:
#               tier: high-come, rest
#               ext_backward: constant, none [default: none]
#               ext_forward: constant, none [default: none]
#               Xyears: fossil fraction for base years (ordered)
#          ext_years: vector of extension years
# Returns:  data frame containing fossil fractions, interpolated between base years
#           and extended for ext_years according to ext_backward and ext_forward flags.
extendFF <- function( df, ext_years ){
  id <- names( df )[ !grepl( "X", names( df ) ) ]
  Xyears <- names( df )[ grepl( "X", names( df ) ) ]
  valid_ext <- c( "constant", "none" )

  # validate inputs
  if ( any( df$ext_backward %!in% valid_ext ) ) {
    warning( "Invalid ext_backward -- must be constant or none. Default chosen: none. ")
    df$ext_backward[ df$ext_backward %!in% valid_ext ] <- "none"
  }
  if ( any( df$ext_forward %!in% valid_ext ) ) {
    warning( "Invalid ext_forward -- must be constant or none. Default chosen: none." )
    df$ext_forward[ df$ext_forward %!in% valid_ext ] <- "none"
  }

  # interpolate between Xyears
  df <- interpolateValues( df )

  # get backward and forward extension range
  first_yr <- head( names( df )[ grepl( "X", names( df ) ) ], n = 1 ) %>% xYearToNum()
  bwd_range <- c()
  if ( first_yr > min( ext_years ) )
    bwd_range <- paste0( "X", seq( min( ext_years ), first_yr - 1 ) )
  last_yr <- tail( names( df )[ grepl( "X", names( df ) ) ], n = 1 ) %>% xYearToNum()
  fwd_range <- c()
  if ( last_yr < max( ext_years ) )
    fwd_range <- paste0( "X", seq( last_yr + 1, max( ext_years ) ) )

  # add columns for all ext years
  df[, bwd_range ] <- NA
  df[, fwd_range ] <- NA
  df <- df[, c( id, paste0( "X", ext_years ) ) ]

  # extend backward by method specified in ext_backward
  first_X_yr <- paste0( "X", first_yr )
  df[ df$ext_backward == "none", bwd_range ] <- 1
  if ( any( df$ext_backward == "constant" ) )
    df[ df$ext_backward == "constant", bwd_range ] <- df[ df$ext_backward == "constant", first_X_yr ]

  # extend forward by method specified in ext_foward
  last_X_yr <- paste0( "X", last_yr )
  df[ df$ext_forward == "none", fwd_range ] <- 1
  if ( any( df$ext_forward == "constant" ) )
    df[ df$ext_forward == "constant", fwd_range ] <- df[ df$ext_forward == "constant", last_X_yr ]

  df$ext_backward <- NULL
  df$ext_forward <- NULL

  return( df )
}

# ------------------------------------------------------------------------------
# 1. Read in files

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "CO"

activity_data <- readData( "MED_OUT", "A.NC_activity" )

emissions_data_read <- readData( "MED_OUT", paste0( "C.", em, "_NC_emissions" ) )

emissions_replaced <- readData( domain = "DEFAULT_EF_IN" ,
                                domain_extension = "non-combustion-emissions/",
                                paste0('C.',em,'_NC_emissions_user_added') )

# Define sectors that should not be corrected for EDGAR
excl_sectors <- c()
if (em == "CO2") {
  excl_sectors <- c( excl_sectors, "2A1_Cement-production", "3D_Soil-emissions" )
}

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
             "C2.1.base_NC_EF.R which uses A.NC_activity.csv and",
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
 # Leave out excluded sectors
   new_efs_excl <- filter( new_efs, sector %in% excl_sectors )
   new_efs <- filter( new_efs, sector %!in% excl_sectors )

 # Fix Edgar EFs
 temp_EDGAR_end_year = EDGAR_end_year

 # Re-set EDGAR end-year if are still using EDGAR 4.2
 if( em == "CH4" || em == "CO2" ) temp_EDGAR_end_year = 2008

 EDGAR_replace_years <- paste0('X',(temp_EDGAR_end_year+1):end_year)

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

# Add back unchanged EFs
  new_efs_corrected_user_added <- rbind( new_efs_corrected_user_added, new_efs_excl ) %>%
    dplyr::arrange( iso, sector, fuel, units )

# Logic Check for negative emissions factors
if (any(new_efs_corrected_user_added < 0) ) stop('There are negative EFs in ', paste0( "C.", em, "_", "NC", "_EF" ))


# ------------------------------------------------------------------------------
# 5. For waste CO2, apply fraction of fossil fuel * urban population shares to EF
if ( em == "CO2") {
  ff <- readData( "DEFAULT_EF_IN", "CO2_waste_fossil_fraction" )
  population <- readData( "MED_OUT", "A.UN_pop_master" )
  MCL <- readData( "MAPPINGS", "Master_Country_List" )

  # Extend fossil fuel fractions
  ff_ext <- extendFF( ff, emissions_years )

  # Copy fossil fractions from income tiers to all countries
  iso_high <- unique( MCL$iso[ MCL$WB_income_level == "high-income" ] )
  iso_high <- iso_high[ !is.na( iso_high ) ]
  ff_high <- filter( ff_ext, tier == "high-income" )
  ff_high <- select( ff_high, -tier ) %>% merge( data.frame( iso = iso_high ) )

  iso_rest <- setdiff( MCL$iso, iso_high )
  ff_rest <- filter( ff_ext, tier == "rest" )
  ff_rest <- select( ff_rest, -tier ) %>% merge( data.frame( iso = iso_rest ) )

  ff_all <- bind_rows( ff_high, ff_rest ) %>%
    filter( iso %!in% c( "global", "pse", "ussr", "yug" ) ) %>%
    dplyr::arrange( iso )
  ff_all <- ff_all[ c( "iso", X_emissions_years ) ]
  ff_all[ is.na( ff_all ) ] <- 1
  ff_all$iso <- as.character( ff_all$iso )

  # Get urban population shares
  urban <- select( population, iso, year, urban_share )
  urban$year <- paste0( "X", urban$year )
  urban <- filter( urban, iso %in% ff_all$iso, year %in% names( ff_all ) )
  urban <- cast( urban, iso~year, value="urban_share" ) %>% dplyr::arrange( iso )
  urban <- urban[ c( "iso", X_emissions_years ) ]
  urban[ is.na( urban ) ] <- 1

  # urban and ff_all should have identical format
  if ( any( urban$iso != ff_all$iso ) | any( names( urban ) != names( ff_all ) ) )
    stop( "urban and ff_all must have the same format.")

  # Compute ratio = urban shares * fossil fuel fraction
  ratios <- urban
  ratios[, X_emissions_years ] <- urban[, X_emissions_years ] * ff_all[, X_emissions_years ]

  # Apply ratio to EF
  waste_EF <- filter( new_efs_corrected_user_added, sector == "5C_Waste-incineration" ) %>%
    dplyr::arrange( iso )
  waste_EF_long <- melt( waste_EF, id=c( "iso", "sector", "fuel", "units" ) )


  ratios_long <- melt( ratios, id="iso" )
  names( ratios_long )[ names( ratios_long ) == "value" ] <- "ratio"

  waste_EF_long <- merge( waste_EF_long, ratios_long, all.x = T )
  waste_EF_long$ratio[ is.na( waste_EF_long$ratio ) ] <- 1
  waste_EF_long$value <- waste_EF_long$value * waste_EF_long$ratio

  waste_EF_new <- cast( waste_EF_long, iso+sector+fuel+units~variable )
  if (nrow( waste_EF_new ) != nrow( waste_EF ) ) stop( "New data do not have correct format." )

  # Clean up
  new_efs_corrected_user_added <- filter( new_efs_corrected_user_added, sector != "5C_Waste-incineration" ) %>%
    rbind( waste_EF_new ) %>% dplyr::arrange( iso, sector, fuel, units )

  writeData( ratios, "DIAG_OUT", "C.CO2_waste_fossil_ratios" )
  writeData( waste_EF_new, "DIAG_OUT", "C.CO2_waste_EFs" )
}


# --------------------------------------------------------------------------------------------
# 6. Output

writeData( new_efs_corrected_user_added, domain = "MED_OUT", fn = paste0( "C.", em, "_", "NC", "_EF" ) )

logStop()
# END
