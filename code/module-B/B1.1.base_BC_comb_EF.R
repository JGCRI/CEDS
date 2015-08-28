#------------------------------------------------------------------------------
# Program Name: B1.1.base_BC_comb_EF.R
# Author: Linh Vu
# Date Last Updated: August 24, 2015
# Program Purpose: 1. Produce BC emissions factors from Bond et al data. The
#                  input data (input/emissions-inventories/Bond_*.csv) provide 1990
#                  and 1996 consumption and emissions, disaggregated by 71 sector_fuels
#                  and 17 regions.
#                  2. Map emission factors calculated to CEDS standard sectors, fuels and 
#                  countries. Where there are duplicates after mapping, take average 
#                  emission factors weighted by energy consumptions. Where there are 
#                  missing ef, take average ef of the same country for that fuel. Extend 
#                  1990 default ef backward in time and 1996 ef forward, and linearly 
#                  interpolate in between
# Input Files: Bond_BC1-Central_1990.csv, Bond_BC1-Central_1996.csv,
#              Bond_Fuel-Central_1990.csv, Bond_Fuel-Central_1996.csv,
#              Bond_ctry_mapping.csv, Bond_fuel_mapping.csv, Bond_sector_mapping.csv
# Output Files: B.comb_EF_db.csv
# Notes: 1. Emission factors (ef) are calculated as emissions divided by consumption.
#           Missing (zero or NA) ef are replaced using the following rules, in order:
#           a. replace with ef of the same sector_fuel and region for the other year,
#              if available;
#           b. where ef do not vary substantially across regions, replace with regional
#              median ef of the same sector_fuel;
#           c. replace with ef of the same sector_fuel for the corresponding country
#              group (OECD90 or ROW), and then for the other country group;
#           d. manually replace the remaining sector_fuel.
#        2. Variables with modifiable values: threshold_var.
# TODO: Add OC ef
#       Consolidate ef correction
#       iso codes do not uniquely identify countries
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
    log_msg <- "Produce BC emissions factors from Bond et al data" # First message to be printed to the log
    script_name <- "B1.1.base_BC_comb_EF.R"
    
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Read in files and do preliminary setup
    # "em" is defined from parent script
    #em <- "BC"

    energy_consumption_1990 <- readData( "EM_INV", "Bond_Fuel-Central_1990", meta = F )
    energy_consumption_1996 <- readData( "EM_INV", "Bond_Fuel-Central_1996", meta = F )
    emission_1990 <- readData( "EM_INV", "Bond_BC1-Central_1990", meta = F )
    emission_1996 <- readData( "EM_INV", "Bond_BC1-Central_1996", meta = F )
    mapping_ctry <- readData( "MAPPINGS", "Bond_ctry_mapping", meta = F )
    mapping_fuel <- readData( "MAPPINGS", "Bond_fuel_mapping", meta = F )
    mapping_sector <- readData( "MAPPINGS", "Bond_sector_mapping", meta = F )
    
    # Leave out process sectors (i.e. whose activity is not "Energy_Combustion")
    mapping_sector <- filter( mapping_sector, activity == "Energy_Combustion" )
    mapping_sector$activity <- NULL

# Bind input, reshape and rename columns
    energy_consumption <- rbind( energy_consumption_1990, energy_consumption_1996)
    energy_consumption <- melt( energy_consumption, id = c( "sector_fuel", "units", "year" ) )
    names( energy_consumption ) <- c( "sector_fuel", "units", "year", "region", "consumption" )
    
    emission <- rbind( emission_1990, emission_1996 )
    emission <- melt( emission, id = c( "sector_fuel", "units", "year" ) )
    names( emission ) <- c( "sector_fuel", "units", "year", "region", "emission" )

# Define some useful values
    # threshold_var: variation threshold, expressed as ratio of standard deviation ef over median ef.
    # In step 3b, if sd over median ef is under threshold_var, replace missing ef by regional median ef.
    threshold_var <- .45  # alterable by user -- move to header?
    ef_header <- c( "sector_fuel", "year", "region", "units", "ef" )
    ceds_header <- c( "fuel", "sector", "iso", "units" )
    
    # Bond_OECD90: list containing OECD90 regions (USA, Canada, WEurope, Japan, AustraliaAndNZ).
    # In step 3c, replace missing ef for a region with average ef from the corresponding country
    # group  (OECD90 or ROW), then for the other country group.    
    Bond_regions <- names( emission_1990 ) [ names( emission_1990 ) %!in% ef_header ]
    Bond_OECD90 <- Bond_regions[ 1:5 ]

# ------------------------------------------------------------------------------
# 2. Calculate raw emission factors (ef)
# Raw ef are calculated as emissions divided by energy consumption
    s2 <- merge( emission, energy_consumption, by = c( "sector_fuel", "year", "region" ) )
    s2$ef <- s2$emission * 1000 / s2$consumption  # convert ef from Gg/kt to g/kg
    s2$units <- "g/kg"

# Clean up
    s2$sector_fuel <- str_trim( s2$sector_fuel )  # remove trailing white spaces
    s2[ s2 == 0 | is.na( s2 ) ] <- NA  # replace missing (zero or NaN) ef with NA
    s2 <- subset( s2, select = ef_header )  # keep only columns listed in ef_header
    #s2_cast <- cast( s2, sector_fuel + year + units ~ region, value = "ef" )

# Print out list of sector_fuel with missing ef for all regions as diagnostics
# Sector_fuels on this list will need to have ef manually corrected.
    diag_missingEF <- s2 %>%
      group_by( sector_fuel, year ) %>%
      summarize( flag_total = sum( is.na( ef ) ) ) %>%
      filter( flag_total == length( Bond_regions ) ) %>%
      select( sector_fuel, year )
    missing_ef <- unique( diag_missingEF$sector_fuel )  # list of sector_fuel where ef is all missing

# ------------------------------------------------------------------------------
# 3. Correct for missing ef
# a. Replace with ef of the same sector_fuel and region for the other year where available
    s3a <- s2
    s3a <- cast( s2, sector_fuel + region + units ~ year, value = "ef" )
    s3a <- within( s3a, {  # if ef is NA for one year, replace with ef of the other year
      `1990` <- ifelse( is.na( `1990` & ! is.na( `1996` ) ), `1996`, `1990` )
      `1996` <- ifelse( is.na( `1996` & ! is.na( `1990` ) ), `1990`, `1996` )
    } )
    s3a <- melt( s3a, id = c( "sector_fuel", "region", "units" ), variable_name = "year" )
    names( s3a ) [ names( s3a ) == "value"] <- "ef"
    
    # Keep only relevant columns
    s3a <- subset( s3a, select = ef_header )
    #s3a_cast <- cast( s3a, sector_fuel + year + units ~ region, value = "ef" )

# b. Where ef do not vary substantially across regions, replace with regional median ef 
#    of the same sector_fuel.
    # Print out ef standard deviation over median as diagnostics
    s3b <- s3a
    diag_sd <- s3b %>%
      group_by( sector_fuel, year ) %>%
      summarize( ef_median = median( ef, na.rm = T ), ef_sd = sd( ef, na.rm = T ) )
    diag_sd$sd_over_median <- diag_sd$ef_sd / diag_sd$ef_median
    
    # Replace with regional median ef if threshold_var is satisfied
    s3b <- merge( s3b, diag_sd, by = c( "sector_fuel", "year" ), all.x = T )
    s3b <- within( s3b, {
      ef <- ifelse( is.na( ef ) & !is.na( sd_over_median ) & sd_over_median <= threshold_var, 
                    ef_median, ef )
    } )
    
    # Keep only columns in ef_header
    s3b <- subset( s3b, select = ef_header )
    #s3b_cast <- cast( s3b, sector_fuel + year + units ~ region, value = "ef" )

# c. Define OECD90 ef as average of USA Canada WEurope Japan AustraliaAndNZ, and ROW as everything.
#   else. If a region is missing ef, replace with ef of the same sector_fuel for the corresponding
#   country group (OECD90 or ROW), then for the other country group.
    s3c <- s3b
    s3c$ctry_grp <- ifelse( s3c$region %in% Bond_OECD90, "OECD90", "ROW")
    
    # Calculate average ef by country group (OECD90 or ROW)
    grp_avg_ef <- subset( s3c, !is.na( ef ) ) %>%
      group_by ( sector_fuel, year, ctry_grp ) %>%
      summarize( ef_grp_avg = mean( ef, na.rm = T ) )
    
    # If mean ef is only available for one country group, replace with ef of the other country group
    sector_fuel_yr_grp <- unique( s3c[ c( "sector_fuel", "year", "ctry_grp" ) ] )
    grp_avg_ef <- merge( grp_avg_ef, sector_fuel_yr_grp,  # make sure grp_avg_ef contains all sector_fuel, year and ctry_grp 
                         by = c( "sector_fuel", "year", "ctry_grp" ), all.y = T )
    grp_avg_ef <- cast( grp_avg_ef, sector_fuel + year ~ ctry_grp, value = "ef_grp_avg" )
    grp_avg_ef <- within( grp_avg_ef, {  # if ef is NA for one country group, replace with ef of the other group
      OECD90 <- ifelse( is.na( OECD90 ), ROW, OECD90 )
      ROW <- ifelse( is.na( ROW ), OECD90, ROW )
    } )
    grp_avg_ef <- melt( grp_avg_ef, id = c( "sector_fuel", "year" ) )
    names( grp_avg_ef ) [ names( grp_avg_ef ) == "value" ] <- "ef_grp_avg"
    s3c <- merge( s3c, grp_avg_ef, by = c( "sector_fuel", "year", "ctry_grp" ), all.x = T )
    
    # If a region is missing ef, replace with group average ef
    s3c <- within( s3c, {
      ef <- ifelse( is.na( ef ), ef_grp_avg, ef )
    } )
    
    # Keep only columns in ef_header
    s3c <- subset( s3c, select = ef_header )
    #s3c_cast <- cast( s3c, sector_fuel + year + units ~ region, value = "ef" )

# d. Manually correct the remaining sector_fuels (where ef is missing for all regions)
    # Separate out sector_fuels where ef is missing for all regions
    done <- filter( s3c, sector_fuel %!in% missing_ef )
    missing <- filter( s3c, sector_fuel %in% missing_ef )
    
    # (i) For AirTransport_Diesel, use ef of 0.01 g/kg (Reference: Lee et al. 2009). 
    s3d1 <- filter( missing, sector_fuel == "AirTransport_Diesel" )
    s3d1$ef <- .01
    
    # (ii) For AirTransport_Gasoline, use 0.01 g/kg multiplied by ef ratio of RoadTransport_Gasoline to 
    # RoadTransport_Diesel (using OECD and ROW averages)
    road_transport <- filter( done, sector_fuel == "RoadTransport_Gasoline" 
                              | sector_fuel == "RoadTransport_Diesel" ) %>%
      within( ctry_grp <- ifelse( region %in% Bond_OECD90, "OECD90", "ROW" ) ) %>%  # create new variable ctry_grp
      group_by( sector_fuel, year, ctry_grp ) %>%
      summarise( mean_ef = mean( ef ) ) %>%
      cast( year + ctry_grp ~ sector_fuel, value = "mean_ef")
    road_transport$gasoline_to_diesel <- road_transport$RoadTransport_Gasoline / road_transport$RoadTransport_Diesel
    
    s3d2 <- filter( missing, sector_fuel == "AirTransport_Gasoline" )
    s3d2$ctry_grp <- ifelse( s3d2$region %in% Bond_OECD90, "OECD90", "ROW" )
    s3d2 <- merge( s3d2, road_transport, id = c( "year", "ctry_grp" ) )
    s3d2$ef <- .01 * s3d2$gasoline_to_diesel
    s3d2 <- subset( s3d2, select = ef_header)
    
    # (iii) For *_Coal, use RailTransport_Coal
    rail_coal <- subset( done, sector_fuel == "RailTransport_Coal", select = -sector_fuel )
    names( rail_coal )[ names( rail_coal ) == "ef" ] <- "ef_rail_coal"
    s3d3 <- filter( missing, grepl( "_Coal", sector_fuel ) )
    s3d3 <- merge( s3d3, rail_coal, by = c( "year", "region", "units" ) )
    s3d3$ef <- s3d3$ef_rail_coal
    s3d3 <- subset( s3d3, select = ef_header )
    
    # (iv) For OtherTransport_Gasoline, InterBunkers_Gasoline and RailTransport_Gasoline, use RoadTransport_Gasoline
    road_gasoline <- subset( done, sector_fuel == "RoadTransport_Gasoline", select = -sector_fuel )
    names( road_gasoline )[ names( road_gasoline ) == "ef" ] <- "ef_road_gasoline"
    s3d4 <- filter( missing, sector_fuel == "OtherTransport_Gasoline" | 
                                    sector_fuel == "InterBunkers_Gasoline" | 
                                    sector_fuel == "RailTransport_Gasoline" )
    s3d4 <- merge( s3d4, road_gasoline, by = c( "year", "region", "units" ) )
    s3d4$ef <- s3d4$ef_road_gasoline
    s3d4 <- subset( s3d4, select = ef_header )
    
    # (v) For *_Gas, use residential gas
    res_gas <- subset( done, sector_fuel == "Residential_Gas", select = -sector_fuel )
    names( res_gas )[ names( res_gas ) == "ef" ] <- "ef_res_gas"
    s3d5 <- filter( missing, grepl( "_Gas", sector_fuel ) & !grepl( "_Gasoline", sector_fuel ) )
    s3d5 <- merge( s3d5, res_gas, by = c( "year", "region", "units" ) )
    s3d5$ef <- s3d5$ef_res_gas
    s3d5 <- subset( s3d5, select = ef_header )
    
    # (vi) For Elec_Gasoline, use Industry_Gasoline
    industry_gasoline <- subset( done, sector_fuel == "Industry_Gasoline", select = -sector_fuel )
    names( industry_gasoline )[ names( industry_gasoline ) == "ef" ] <- "ef_industry_gasoline"
    s3d6 <- filter( missing, sector_fuel == "Elec_Gasoline" )
    s3d6 <- merge( s3d6, industry_gasoline, by = c( "year", "region", "units" ) )
    s3d6$ef <- s3d6$ef_industry_gasoline
    s3d6 <- subset( s3d6, select = ef_header )
    
    # (vii) For *_OpenWaste, *_OpenBiomass and Industry_BioLiquid, use 0 for now
    s3d7 <- filter( missing, grepl( "_Open", sector_fuel) | sector_fuel == "Industry_BioLiquid" )
    s3d7$ef <- 0
    s3d7 <- subset( s3d7, select = ef_header )
    
    # Combine into one dataset
    s3d <- rbind( done, s3d1, s3d2, s3d3, s3d4, s3d5, s3d6, s3d7 )
    #s3d_cast <- cast( s3d, sector_fuel + year + units ~ region, value = "ef" )

# Replace all remaining NA ef with 0
    s3 <- within( s3d, {
      ef[ is.na( ef ) ] <- 0
    } )

# ------------------------------------------------------------------------------
# 4. Map sectors, fuels and regions from Bond to CEDS
    printLog( "Map and produce default BC emission factors" )
    
# Rename input columns; separate sector_fuel into sector and fuel columns
    ef <- s3
    names( ef ) <- c( "Bond_sector_fuel", "year", "Bond_reg", "units", "ef" )  
    ef <- cbind( ef, colsplit( ef$Bond_sector_fuel, split = "_", names = c( "Bond_sector", "Bond_fuel" ) ) )

# Attach energy consumption (to calculate consumption-weighted ef average later)
#   Note: Since energy_consumption is Bond's raw data and ef has been interpolated for missing values,
#   there will be observations with non-NA ef and NA consumption.
    energy_consumption[ energy_consumption == 0] <- NA
    energy_consumption$units <- NULL
    energy_consumption$sector_fuel <- str_trim( energy_consumption$sector_fuel )  # remove trailing whitespace  
    names( energy_consumption ) <- c( "Bond_sector_fuel", "year", "Bond_reg", "consumption" )  
    ef_w_consumption <- merge( ef, energy_consumption, by = c( "Bond_sector_fuel", "Bond_reg", "year" ) )

# Map sectors, fuels and regions from Bond to CEDS
    ef_mapped_sector <- merge( ef_w_consumption, filter( mapping_sector, !is.na( Bond_sector ) ),
                                  by = "Bond_sector", all = T )
    ef_mapped_fuel <- merge( ef_mapped_sector, filter( mapping_fuel, !is.na( Bond_fuel ) ), 
                                by = "Bond_fuel", all = T )
    ef_mapped_ctry <- merge( ef_mapped_fuel, filter( mapping_ctry, !is.na( Bond_reg ) ), 
                                by = "Bond_reg", all = T ) 

# Keep only relevant columns, and reorder rows
    s4 <- select( ef_mapped_ctry, iso, IEA_ctry, ceds_sector, ceds_fuel, year, units, ef, consumption )
    s4 <- arrange( s4, year, iso, IEA_ctry, ceds_sector, ceds_fuel )

    
# ------------------------------------------------------------------------------
# 5. Correct for duplicated and missing ef after mapping
# a. For duplicates, take average ef weighted by energy consumption
#   Note: weight.mean() treats non-NA ef with NA consumption as NA, which causes 
#   omission of non-NA ef for unique rows. To avoid that, identify unique rows and assign
#   to energy consumption a dummy weight.
    # Split data into unique and duplicated rows
    dupes <- s4[ duplicated( s4[ 1:5 ] ) | duplicated( s4[ 1:5 ], fromLast = T ), ]
    uniques <- s4[ ! ( duplicated( s4[ 1:5 ] ) | duplicated( s4[ 1:5 ], fromLast = T ) ), ]
    
    # For unique rows, assign energy consumption a dummy weight of 1
    uniques$consumption <- 1
    
    # Merge back into one dataframe
    s5a <- rbind( uniques, dupes )
    s5a <- arrange( s5a, year, iso, IEA_ctry, ceds_sector, ceds_fuel )
    
    # Calculate consumption-weighted average ef
    s5a <- s5a %>%
      within( consumption <- ifelse( is.na( consumption ), 0, consumption ) ) %>%  # replace NA consumption with 0
      group_by( iso, IEA_ctry, ceds_sector, ceds_fuel, year, units ) %>%
      summarise( ef.w_avg = weighted.mean( ef, consumption, na.rm = T ) )
    s5a <- arrange( s5a, year, iso, IEA_ctry, ceds_sector, ceds_fuel )

# b. Correct for missing ef
    # Create a blank data frame with all CEDS sectors, fuels and countries
    template <- merge( unique( mapping_ctry[ c( "iso", "IEA_ctry" ) ] ), 
                        data.frame( ceds_sector = unique( mapping_sector$ceds_sector ) ), 
                        all = T ) %>%
      merge( data.frame( ceds_fuel = unique( mapping_fuel$ceds_fuel ) ), all = T ) %>%
      merge( data.frame( year = c( 1990, 1996 ) ), all = T )
    template$units <- "g/kg"    
    
    # Populate the blank data frame with ef from s5a
    s5b <- merge( template, s5a, all.x = T)
    names( s5b )[ names( s5b ) == "ef.w_avg" ] <- "ef"
    
    # TO-DO: manually fix missing ef in `diag`
    
    # Print out sectors/fuels with missing ef as diagnostics
    # ctry_count: how many countries have missing ef for this sector/fuel combination?
    diag <- filter( s5b, is.na( ef ) )
    diag <- unique( diag[ c( "iso", "ceds_sector", "ceds_fuel" ) ] ) %>%
      group_by( ceds_sector, ceds_fuel ) %>%
      summarise( ctry_count = length( iso ) )
    
    # For missing ef, use average ef of the same country for that fuel
    ef_fuel_avg <- s5b %>%
      group_by( iso, IEA_ctry, year, ceds_fuel ) %>%
      summarise( ef_fuel_avg = mean( ef, na.rm = T ) )
    s5b <- merge( s5b, ef_fuel_avg, by = c( "iso", "IEA_ctry", "year", "ceds_fuel" ) )
    s5b <- within( s5b, {
      ef <- ifelse( is.na( ef ), ef_fuel_avg, ef )
    } )
    
# Replace all remaining NA ef with 0
    s5 <- within( s5b, {
      ef[ is.na( ef ) ] <- 0
    } )
    
# Keep only relevant columns, reorder rows, and rename columns
    s5 <- select( s5, iso, ceds_sector, ceds_fuel, units, year, ef )
    s5 <- cast( s5, iso + ceds_sector + ceds_fuel + units ~ year, value = "ef", fun = mean )
    names( s5 ) <- c( "iso", "sector", "fuel", "units", "1990", "1996" )

    
# ------------------------------------------------------------------------------
# 6. Extend or interpolate default ef to cover missing year
# Extend 1990 ef backward and 1996 ef forward, interpolate between
    s6 <- extendForward( s5, "1996", getForwardExtensionRange( 1996, end_year ) )
    s6 <- extendBackward( s6, "1990", getBackwardExtensionRange( 1990, start_year ) )  # why NAs introduced by coercion?? no NAs in output
    s6 <- interpolate( s6, "1990", "1996", "linear" )
    
    names( s6 ) <- c( "iso", "sector", "fuel", "units", X_emissions_years )
    
# ------------------------------------------------------------------------------
# 7. Write output
    
    writeData( s6, "MED_OUT", paste0( "B.", em ,"_", "comb", "_EF_db" ) )
#     writeData( diag_missingEF, "DIAG_OUT", "B.EF.diag_missingallEF" )
#     writeData( diag_sd, "DIAG_OUT", "B.diag_sd" )
#     writeData( diag, "DIAG_OUT", "B.missing_ef_mapped")

# Every script should finish with this line
    logStop()

# END
