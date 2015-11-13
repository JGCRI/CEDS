#------------------------------------------------------------------------------
# Program Name: A2.2.fix_IEA_biomass.R
# Author: Linh Vu
# Date Last Updated: 23 October 2015
# Program Purpose: Corrects inconsistencies in system's residential biomass consumption
# Input Files: A.en_stat_sector_fuel.csv, A.UN_pop_master.csv
# Output Files: A.en_stat_sector_fuel.csv
# Notes:
# TODO: kwt and sgp has no biomass consumption in recent years
#       add metadata
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Before we can load headers we need some paths defined. They may be provided by
#   a system environment variable or may have already been set in the workspace.
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
    headers <- c( "data_functions.R", "analysis_functions.R", "process_db_functions.R", 
                  "timeframe_functions.R" ) # Additional function files required.
    log_msg <- "Correct discontinuity in IEA residential biomass consumption" # First message to be printed to the log
    script_name <- "A2.2.fix_IEA_biomass.R"
    
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Read in data and set temp values
    en_stat <- readData( "MED_OUT", "A.en_stat_sector_fuel", meta = F )
    UN_pop <- readData( "MED_OUT", "A.UN_pop_master", meta = F )

# Biomass per capita threshold, expressed as ratio of current year consumption over 
#   next year consumption. If pc biomass for one year is zero or drops under the 
#   threshold relative to the following year, replace it with pc biomass of the next
#   available year (i.e. with nonzero biomass).
    threshold <- .5

# What UN population scenarios to use?        
    pop_scenarios <- c( "Estimates", "Medium fertility" )

# ------------------------------------------------------------------------------
# 2. Format residential biomass and calculate per capita consumption
# Extract residential biomass
    res_biomass <- filter( en_stat, sector == "rescom_buildings", fuel == "biomass" ) %>%
      melt( id = c( "iso", "sector", "fuel", "units" ), variable_name = "year" )
    names( res_biomass )[ names( res_biomass ) == "value" ] <- "consumption"

# Calculate pc biomass consumption, using rural population when rural population 
#   is available and nonzero, and using total population otherwise
    # Prepare population data
    pop_df <- filter( UN_pop, scenario %in% pop_scenarios ) %>%
      mutate( rural_pop = pop * ( 1 - urban_share ) ) %>%  # compute rural population
      filter( year >= min( IEA_years ), year <= max( IEA_years ) )  # keep IEA years
    
    # What countries do not have rural population?
    # "aia" "bmu" "cym" "fro" "gib" "hkg" "mac" "mco" "nru" "sgp" "sxm" "vat"
    no_rural <- unique( pop_df$iso[ is.na( pop_df$urban_share ) | pop_df$urban_share == 1 ] )
    
    # Create variable pop = rural_pop if rural_pop available and nonzero, = total pop otherwise
    pop_df$rural_pop[ pop_df$iso %in% no_rural ] <- pop_df$pop[ pop_df$iso %in% no_rural ]
    pop_df <- select( pop_df, iso, year, rural_pop ) %>% unique()
    names( pop_df ) <- c( "iso", "year", "pop" )
    pop_df$year <- paste0( "X", pop_df$year )
    
    # Compute pc consumption
    res_biomass <- merge( res_biomass, pop_df, all.x = T ) %>%
      mutate( consumption_pc = consumption / pop )
    #res_biomass$units <- paste0( res_biomass$units, "/thous. people" )
    
# List of IEA iso codes
    IEA_iso <- unique( res_biomass$iso )

# ------------------------------------------------------------------------------
# 3. Identify and fix discontinuities in pc biomass consumption
#   If pc biomass for one year is zero or drops substantially, replace with pc biomass 
#   of the next available year. In pseudo-mathematical notation: If consumption_pc_i == 0, 
#   or consumption_pc_(i + 1) > 0 and consumption_pc_i/consumption_pc_(i + 1) < threshold, 
#   replace consumption_pc_i with consumption_pc_(i + 1), where i denotes the current year.

    # First create a blank data frame
    res_biomass_fixed <- data.frame()

    # Fix the gaps and bind in results country by country
    # Note that [[ i_year - 1]] (not [[ i_year + 1 ]]) corresponds to the next 
    # chronological year, since res_biomass is in descending year order 
    res_biomass <- arrange( res_biomass, iso, desc( year ) )
    for ( i_iso in seq_along( IEA_iso ) ){
      temp <- filter( res_biomass, iso == IEA_iso[[ i_iso ]] )  # subset by country
      # temp$flag <- 0  # flag==1 if inconsistency detected
      for ( i_year in seq_along( X_IEA_years )[-1] ){
        if( temp$consumption_pc[[ i_year ]] == 0 |  
            ( temp$consumption_pc[[ i_year - 1 ]] > 0 & 
              temp$consumption_pc[[ i_year ]] / temp$consumption_pc[[ i_year - 1 ]] < threshold ) ){
          temp$consumption_pc[[ i_year ]] <- temp$consumption_pc[[ i_year - 1 ]]
          # temp$flag[[ i_year ]] <- 1
        }
      }
      res_biomass_fixed <- rbind( res_biomass_fixed, temp )
    }
    
#     # Create diagnostic table: How many inconsistencies were detected with given threshold
#     diag_threshold <- select( res_biomass_fixed, iso, year, flag )
#     diag_threshold$threshold <- threshold
#     diag_threshold <- cast( diag_threshold, iso + threshold ~ year, value = "flag" )
#     diag_threshold[ diag_threshold == 0 ] <- ""

    # Convert pc biomass to total biomass consumption
    res_biomass_fixed$consumption <- res_biomass_fixed$pop * res_biomass_fixed$consumption_pc
    res_biomass_fixed <- select( res_biomass_fixed, iso, sector, fuel, units, year, consumption )
    
#   # Diagnostics: No biomass consumption for kwt (1997-2010) and sgp (1985-2010)
#    filter( res_biomass_fixed, consumption == 0 | is.na( consumption ) )
    
# ------------------------------------------------------------------------------
# 4. Replace residential biomass in the system with the corrected series
    printLog( "Replace with corrected residential biomass series" )
    
    en_stat_fixed <- melt( en_stat, measure.vars = X_IEA_years, variable_name = "year" ) %>%
      filter( sector != "rescom_buildings" | fuel != "biomass" )
    names( en_stat_fixed )[ names( en_stat_fixed ) == "value" ] <- "consumption"
    en_stat_fixed <- bind_rows( en_stat_fixed, res_biomass_fixed ) %>%
      cast( iso + sector + fuel + units ~ year, value = "consumption" )
    
# ------------------------------------------------------------------------------
# 5. Write output
    writeData( en_stat_fixed, "MED_OUT", "A.en_stat_sector_fuel" )
    # writeData( diag_threshold, "DIAG_OUT", paste0("A2.1.diag_threshold", threshold ) )
    
# Diagnostic - write out new biomass time series
# TODO: also write out industrial combustion as well once that is updated
res_biomass_fixed.wide <- cast(res_biomass_fixed, iso+sector+fuel+units ~ year, value="biomass")
writeData( res_biomass_fixed.wide, domain = "DIAG_OUT", fn = "A2.2.residential_biomass_cons", 
           meta = F )
    
# Every script should finish with this line:
    logStop()

