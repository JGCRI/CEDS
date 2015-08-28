#------------------------------------------------------------------------------
# Program Name: A2.2.fix_IEA_biomass.R
# Author: Linh Vu
# Date Last Updated: August 20, 2015
# Program Purpose: Corrects inconsistencies in system's residential biomass consumption
# Input Files: A.en_stat_sector_fuel.csv, A.UN_pop_master.csv
# Output Files: A.en_stat_sector_fuel.csv
# Notes: Variables holding temp values in this code: threshold
# TODO: kwt and sgp has no biomass consumption in recent years
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Before we can load headers we need some paths defined. They may be provided by
#   a system environment variable or may have already been set in the workspace.
    dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
    for ( i in 1:length( dirs ) ) {
      setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
      wd <- grep( 'emissions-data-system/input', list.dirs(), value = T )
      if ( length( wd ) > 0 ) {
        setwd( wd[ 1 ] )
        break
      }
    }
    PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "analysis_functions.R", "process_db_functions.R", 
                  "timeframe_functions.R" ) # Additional function files required.
    log_msg <- "Correct discontinuity in IEA residential biomass consumption" # First message to be printed to the log
    script_name <- "A2.2.fix_IEA_biomass.R"
    
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Read in data and set temp values
    en_stat <- readData( "MED_OUT", "A.en_stat_sector_fuel" )
    UN_pop <- readData( "MED_OUT", "A.UN_pop_master" )

# Biomass per capita threshold, expressed as ratio of current year consumption over next year consumption
#   If pc biomass for one year is zero or drops under the threshold relative to the following year,
#   replace it with pc biomass of the next available year (i.e. with nonzero biomass).
    threshold <- .25

# What UN population scenarios to use?        
    pop_scenarios <- c( "Estimates", "Medium fertility" )

# ------------------------------------------------------------------------------
# 2. Format residential biomass and calculate per capita consumption
# Extract residential biomass
    res_biomass <- filter( en_stat, sector == "rescom_buildings", fuel == "biomass" ) %>%
      melt( id = c( "iso", "sector", "fuel", "units" ), variable_name = "year" )
    names( res_biomass )[ names( res_biomass ) == "value" ] <- "biomass"

# Prepare population data
    pop_df <- filter( UN_pop, scenario %in% pop_scenarios )
    pop_df <- select( pop_df, iso, year, pop )  # drop irrelevant columns
    pop_df <- pop_df[ !duplicated( pop_df ), ]  # drop duplicated rows (year 2010)
    pop_df$year <- paste0( "X", pop_df$year )  # add X to year

# Calculate pc biomass consumption
    res_biomass <- merge( res_biomass, pop_df, by = c( "iso", "year" ), all.x = T ) %>%
      mutate( biomass_pc = biomass / pop )
    #res_biomass$units <- paste0( res_biomass$units, "/thous. people" )

# List of IEA iso codes
    IEA_iso <- as.character( unique( res_biomass$iso ) )

# ------------------------------------------------------------------------------
# 3. Identify and fix discontinuities in pc biomass consumption
#   If pc biomass for one year is zero or drops substantially, replace with pc biomass of the next available year.
#   In pseudo-mathematical notation: If biomass_pc_i == 0, or biomass_pc_(i + 1) > 0 and
#   biomass_pc_i/biomass_pc_(i + 1) < threshold, replace biomass_pc_i with biomass_pc_(i + 1),
#   where i denotes the current year.

    # First create a blank data frame
    res_biomass_fixed <- data.frame()
    
    # Fix the gaps and bind in results country by country
    # Note that [[ i_year - 1]] (not [[ i_year + 1 ]]) corresponds to the next 
    # chronological year, since res_biomass is in descending year order 
    res_biomass <- arrange( res_biomass, iso, desc( year ) )
    for ( i_iso in 1:length( IEA_iso ) ){
      temp <- filter( res_biomass, iso == IEA_iso[[ i_iso ]] )  # subset by country
      for ( i_year in 2:length( X_IEA_years ) ){
        if( temp$biomass_pc[[ i_year ]] == 0 |  
            ( temp$biomass_pc[[ i_year - 1 ]] > 0 & 
              temp$biomass_pc[[ i_year ]] / temp$biomass_pc[[ i_year - 1 ]] < threshold ) ){
          temp$biomass_pc[[ i_year ]] <- temp$biomass_pc[[ i_year - 1 ]]
        }
      }
      res_biomass_fixed <- rbind( res_biomass_fixed, temp )
    }

    # Convert pc biomass to total biomass consumption
    res_biomass_fixed$biomass <- res_biomass_fixed$pop * res_biomass_fixed$biomass_pc
    res_biomass_fixed <- select( res_biomass_fixed, iso, sector, fuel, units, year, biomass )
    
#   # Diagnostics: No biomass consumption for kwt (1997-2010) and sgp (1985-2010)
#    filter( res_biomass_fixed, biomass_pc == 0 | is.na( biomass_pc ) )
    
# ------------------------------------------------------------------------------
# 4. Replace residential biomass in the system with the corrected series
    printLog( "Replace with corrected residential biomass series" )
    
    en_stat_fixed <- melt( en_stat, measure.vars = X_IEA_years, variable_name = "year" ) %>%
      merge( res_biomass_fixed, all.x = T ) %>%
      within( value <- ifelse( is.na( biomass ), value, biomass ) )  # update existing biomass
    en_stat_fixed$biomass <- NULL
    en_stat_fixed <- cast( en_stat_fixed, iso + sector + fuel + units ~ year )
    
# ------------------------------------------------------------------------------
# 5. Write output
    writeData( en_stat_fixed, "MED_OUT", "A.en_stat_sector_fuel" )
    
# Every script should finish with this line:
    logStop()

