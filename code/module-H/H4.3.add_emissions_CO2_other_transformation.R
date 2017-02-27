# Program Name: H4.3.add_emissions_CO2_other_transformation.R
# Author: Linh Vu
# Date Last Updated: 23 Nov 2016 
# Program Purpose: Compute CO2 other transformation coal:
#     CO2_Conversion = CO2_Coal_Total - CO2_Coal_Combustion - CO2_Coal_NEuse
#                      - CO2_
#   using coal consumption and default EF.
# Input Files: A.IEA_en_stat_ctry_hist.csv, CO2_total_CEDS_emissions.csv, 
#   H.Extended_coal_by_fuel.csv, H.Extended_other_tranformation_coal.csv
#   H.CO2_total_CEDS_emissions_before_other_transformation_replacement.csv,
#   B.CO2_comb_EF_non-bunker.csv, IEA_product_fuel.csv, Master_Fuel_Sector_List.csv
# Output Files: H.CO2_calculated_other_transformation_emissions.csv
# Notes: EFs already multiplied by fraction oxidized.
# TODO: 
#   -- Extend total coal consumption using BP\
#   -- Disaggregate extended other transformation coal instead of assuming hard coal
#   -- Fix hard-coded years
# ---------------------------------------------------------------------------

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
headers <- c( 'data_functions.R' ) 
log_msg <- "Calculating CO2 emissions conversion for 1A1bc-Other-transformation coal" 
script_name <- 'H4.3.add_emissions_CO2_other_transformation.R'

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "CO2"
em_lc <- tolower( em )   

# Stop script if running for unsupported species
if( em != 'CO2') {
  stop('This script should not run for emission species other than 
       CO2, please check H4.1.proc_Extended_Emissions')
}


# ---------------------------------------------------------------------------
# 1. Input
  A.IEA_en_stat_ctry_hist <- readData( "MED_OUT", "A.IEA_en_stat_ctry_hist" )
  H.Extended_coal_by_fuel <- readData( "DIAG_OUT", "H.Extended_coal_by_fuel" )
  H.Extended_other_tranformation_coal <- readData( "DIAG_OUT", "H.Extended_other_tranformation_coal" )
  CO2_total_CEDS_emissions <- readData( "MED_OUT", paste0( 'H.', em,'_total_CEDS_emissions_before_other_transformation_replacement') )
  coal_ef <- readData( "DIAG_OUT", "B.CO2_comb_EF_non-bunker" )
  IEA_product_fuel <- readData( "MAPPINGS", "/energy/IEA_product_fuel" )
  MSL <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Sectors" )
  
# Define values
  ceds_coal_fuels <- c( "brown_coal", "coal_coke", "hard_coal" )
  ceds_comb_sectors <- MSL$sector[ MSL$type == "comb" ]

# ---------------------------------------------------------------------------
# 2. Prelim processing
# Extend 1960 coal EF constant back to 1750
# TODO: make robust
  coal_ef_ext <- filter( coal_ef, fuel %in% ceds_coal_fuels )
  years_to_add <- X_extended_years[ X_extended_years %!in% names( coal_ef_ext ) ]
  coal_ef_ext[, years_to_add ] <- coal_ef_ext[, X_start_year ]
  coal_ef_ext <- coal_ef_ext[ c( "iso", "fuel", "units", X_extended_years ) ]
  
# Melt coal EF to long format
  coal_ef_ext_long <- melt( coal_ef_ext, id=c( "iso", "fuel", "units" ) )
  names( coal_ef_ext_long )[ names( coal_ef_ext_long ) %in% c( "variable", "value" ) ] <-
    c( "year", "coal_EF" )
  
# Add CEDS fuel column to A.IEA_en_stat_ctry_hist
  A.IEA_en_stat_ctry_hist$fuel <- IEA_product_fuel$fuel[ match( 
    A.IEA_en_stat_ctry_hist$PRODUCT, IEA_product_fuel$product ) ]
  
# Add fuel column to H.Extended_other_tranformation_coal. Assumes all coal
# is hard coal
# TODO: Eventually disaggregate
  H.Extended_other_tranformation_coal$fuel <- "hard_coal"

# ---------------------------------------------------------------------------
# 3. Compute CO2_Coal_Total = 
#   1750-1970: CO2 from CEDS extended coal  
#   1971-2013: CO2 from IEA DOMSUP brown coal + IEA DOMSUP hard coal + IEA IMPORTS coal coke
#   2014: CO2 from 2013 IEA coal
# TODO: Extend to 2014 using BP
  COAL_TOTAL_REPLACEMENT_YEAR <- 1970
  X_coal_total_CEDS_years <- paste0( "X", historical_pre_extension_year:( COAL_TOTAL_REPLACEMENT_YEAR ) )
  X_coal_total_IEA_years <- paste0( "X", (COAL_TOTAL_REPLACEMENT_YEAR+1):IEA_end_year )
  
# a. First compute total coal consumption  
# Compute IEA coal consumption (1960-2013)
  IEA_total_coal <- bind_rows( filter( A.IEA_en_stat_ctry_hist, FLOW=="DOMSUP", fuel %in% c( "hard_coal", "brown_coal" ) ),
                               filter( A.IEA_en_stat_ctry_hist, FLOW=="IMPORTS", fuel=="coal_coke" ) ) %>%
    select( -FLOW, -PRODUCT ) %>% group_by( iso, fuel ) %>%
    summarise_each( funs( sum(., na.rm = T ) ) ) %>% data.frame()
  IEA_total_coal$units <- "kt"
  IEA_total_coal <- IEA_total_coal[ c( "iso", "fuel", "units", X_coal_total_IEA_years ) ]

# Compute CEDS extended coal consumption (1750-1970) by summing
# H.Extended_coal_by_fuel and H.Extended_other_tranformation_coal.
  CEDS_total_coal <- bind_rows( H.Extended_coal_by_fuel, H.Extended_other_tranformation_coal )
  CEDS_total_coal$units <- "kt"
  CEDS_total_coal <- CEDS_total_coal[ c( "iso", "fuel", "units", X_coal_total_CEDS_years ) ]
  CEDS_total_coal <- group_by( CEDS_total_coal, iso, fuel, units ) %>%
    summarise_each( funs( sum(., na.rm = T ) ) ) %>% data.frame()

# Combine IEA and CEDS coal. Use CEDS for 1750-1970 and IEA for 1971-2013
# Copy 2013 values to 2014. Make all NA zero
  total_coal <- merge( CEDS_total_coal, IEA_total_coal, all.x = T )
  total_coal[paste0('X',BP_years)] <- total_coal[paste0('X',IEA_end_year)]
  total_coal[ is.na( total_coal ) ] <- 0

# # Diagnostic: Compare CEDS and IEA values at replacement year
#   diag_coal_total_repl_cmp <- filter( total_coal_long, year == paste0( "X", COAL_TOTAL_REPLACEMENT_YEAR ) ) %>%
#     mutate( diff = CEDS_value - IEA_value, diff_pc = diff*100/IEA_value ) 
#   diag_coal_total_repl_cmp$diff_pc[ diag_coal_total_repl_cmp$IEA_value == diag_coal_total_repl_cmp$CEDS_value ] <- 0
    
# b. Compute CO2 emissions from total coal consumption and EF
  total_coal_long <- melt( total_coal, id = c( "iso", "fuel", "units" ) ) %>%
    select( iso, fuel, year=variable, total_value=value ) %>% data.frame()
  CO2_Coal_Total <- merge( total_coal_long, coal_ef_ext_long, all.x = T ) %>%
    mutate( em = total_value * coal_EF )
  CO2_Coal_Total$units <- "kt"
  CO2_Coal_Total <- cast( CO2_Coal_Total, iso+fuel+units~year, value="em" ) %>%
    arrange( iso, fuel, units )

# ---------------------------------------------------------------------------
# 4. Compute CO2_Coal_NEuse = CO2 from IEA NONENUSE coal
# IEA energy data tends to go out back in time. We'll extrapolate IEA Nonenuse
# coal using total coal and fractions of nonenuse coal over total coal at a split
# year:
#   IEA_nonenuse_coal_ext[fuel, year] = total_coal[year] * IEA_nonenuse_coal[fuel, COAL_NEUSE_RATIO_YEAR] / 
#                                   total_coal[fuel, COAL_NEUSE_RATIO_YEAR]
# unless there is existing IEA data for that year

  COAL_NEUSE_RATIO_YEAR <- 2007
  X_coal_neuse_data_years <- paste0( "X", COAL_NEUSE_RATIO_YEAR:end_year )
  X_coal_neuse_nodata_years <- X_extended_years[ X_extended_years %!in% X_coal_neuse_data_years ]

# a. First compute NEuse coal consumption 
# Get IEA NONENUSE coal
  IEA_neuse_coal <- filter( A.IEA_en_stat_ctry_hist, FLOW == "NONENUSE", fuel %in% ceds_coal_fuels ) %>%
    select( -FLOW, -PRODUCT ) %>% group_by( iso, fuel ) %>%
    summarise_each( funs( sum(., na.rm = T ) ) ) %>% data.frame()
  IEA_neuse_coal$units <- "kt"
  
# Extend 2013 value to 2014
  IEA_neuse_coal[paste0('X',BP_years)] <- IEA_neuse_coal[paste0('X',IEA_end_year)]
  
# # To determine what year has most values (2007)  
#   df <- IEA_neuse_coal
#   df[ df!=0 ] <- 1
#   df <- group_by(df, iso, fuel, units) %>% summarise_each(funs(sum(., na.rm=T))) %>% data.frame()
#   df <- melt(df, id=c("iso", "fuel","units" )) %>% filter(value==max(value))
  
# Add rows for all iso+fuel combinations and columns for all years. 
# Make values before COAL_NEUSE_RATIO_YEAR zero
  IEA_neuse_coal_ext <- merge( total_coal[ c( "iso", "fuel", "units" ) ], IEA_neuse_coal, all.x = T )
  IEA_neuse_coal_ext[ X_coal_neuse_nodata_years ] <- NA 
  IEA_neuse_coal_ext <- IEA_neuse_coal_ext[ c( "iso", "fuel", "units", X_extended_years ) ]
  IEA_neuse_coal_ext[ is.na( IEA_neuse_coal_ext ) ] <- 0
  
# Melt and add column of total coal
  IEA_neuse_coal_ext_long <- melt( IEA_neuse_coal_ext, id=c( "iso", "fuel", "units" ) )
  names( IEA_neuse_coal_ext_long )[ names( IEA_neuse_coal_ext_long ) %in% c( "variable", "value" ) ] <-
    c( "year", "neuse_value" )
  IEA_neuse_coal_ext_long <- merge( IEA_neuse_coal_ext_long, total_coal_long )
  
# Calculate IEA_neuse_coal[fuel, COAL_NEUSE_RATIO_YEAR] / total_coal[fuel, COAL_NEUSE_RATIO_YEAR]
  neuse_ratio <- filter( IEA_neuse_coal_ext_long, year == paste0( "X", COAL_NEUSE_RATIO_YEAR ) ) %>%
    mutate( ratio = neuse_value / total_value )
  neuse_ratio$ratio[ neuse_ratio$neuse_value == 0 | neuse_ratio$total_value == 0 ] <- 0
  neuse_ratio <- neuse_ratio[ c( "iso", "fuel", "ratio" ) ]
  
# Extend before COAL_NEUSE_RATIO_YEAR using total_coal and COAL_NEUSE_RATIO_YEAR neuse fractions:
#   IEA_neuse_coal_ext[fuel, year] = total_coal[year] * IEA_neuse_coal[fuel, COAL_NEUSE_RATIO_YEAR] / 
#                                   total_coal[fuel, COAL_NEUSE_RATIO_YEAR]
  IEA_neuse_coal_ext_long <- merge( IEA_neuse_coal_ext_long, neuse_ratio )
  IEA_neuse_coal_ext_long$neuse_value_ext <- IEA_neuse_coal_ext_long$total_value * 
    IEA_neuse_coal_ext_long$ratio
  
# Replace extrapolated values with original value, if original exists
  IEA_neuse_coal_ext_long$neuse_value_ext[ IEA_neuse_coal_ext_long$neuse_value != 0 ] <-
    IEA_neuse_coal_ext_long$neuse_value[ IEA_neuse_coal_ext_long$neuse_value != 0 ]
  IEA_neuse_coal_ext_long <- IEA_neuse_coal_ext_long[ c( "iso", "fuel", "year", "neuse_value_ext" ) ]
  
# b. Compute CO2 emissions from NEuse coal consumption and EF
  CO2_Coal_NEuse <- merge( IEA_neuse_coal_ext_long, coal_ef_ext_long, all.x = T ) %>%
    mutate( em = neuse_value_ext * coal_EF )
  CO2_Coal_NEuse$units <- "kt"
  CO2_Coal_NEuse <- cast( CO2_Coal_NEuse, iso+fuel+units~year, value = "em" ) %>%
    arrange( iso, fuel, units )
  
# ---------------------------------------------------------------------------
# 5. Compute CO2_Coal_Combustion = CO2 from Coal from CEDS combustion emission sectors
  CO2_Coal_Combustion <- filter( CO2_total_CEDS_emissions, sector %in% ceds_comb_sectors, fuel %in% ceds_coal_fuels ) %>%
    select( -sector ) %>% group_by( iso, fuel, units ) %>%
    summarise_each( funs( sum(., na.rm = T ) ) ) %>% 
    arrange( iso, fuel, units ) %>% data.frame()
  
# ---------------------------------------------------------------------------
  # 5. Compute CO2_Coal_Combustion = CO2 from Coal from CEDS combustion emission sectors
  CO2_Coal_Combustion <- filter( CO2_total_CEDS_emissions, sector %in% ceds_comb_sectors, fuel %in% ceds_coal_fuels ) %>%
    select( -sector ) %>% group_by( iso, fuel, units ) %>%
    summarise_each( funs( sum(., na.rm = T ) ) ) %>% 
    arrange( iso, fuel, units ) %>% data.frame()
  
# ---------------------------------------------------------------------------
# 6. Compute CO2_Conversion = CO2_Coal_Total - CO2_Coal_Combustion - CO2_Coal_NEuse
# Aggregate all coal flows by iso
  CO2_Coal_Total_agg <- select( CO2_Coal_Total, -fuel ) %>% group_by( iso, units ) %>%
    summarise_each( funs( sum(., na.rm = T ) ) ) %>% arrange( iso ) %>% data.frame()
  CO2_Coal_Combustion_agg <- select( CO2_Coal_Combustion, -fuel ) %>% group_by( iso, units ) %>%
    summarise_each( funs( sum(., na.rm = T ) ) ) %>% arrange( iso ) %>% data.frame()
  CO2_Coal_NEuse_agg <- select( CO2_Coal_NEuse, -fuel ) %>% group_by( iso, units ) %>%
    summarise_each( funs( sum(., na.rm = T ) ) ) %>% arrange( iso ) %>% data.frame()
  
# Check that all 3 dfs have same ID columns  
  if( any( paste( CO2_Coal_Total$iso, CO2_Coal_Total$units ) != 
           paste( CO2_Coal_Combustion$iso, CO2_Coal_Combustion$units ) ) | 
      any( paste( CO2_Coal_Combustion$iso, CO2_Coal_Combustion$units ) != 
           paste( CO2_Coal_NEuse$iso, CO2_Coal_NEuse$units ) ) | 
      any( paste( CO2_Coal_NEuse$iso, CO2_Coal_NEuse$units ) != 
           paste( CO2_Coal_Total$iso, CO2_Coal_Total$units ) ) ) {
    stop( "ID columns do not match.")
  }
  
# Compute CO2_Conversion from aggregated flows 
  CO2_Conversion <- CO2_Coal_Total_agg
  CO2_Conversion[, X_extended_years ] <- CO2_Coal_Total_agg[, X_extended_years] - 
    CO2_Coal_Combustion_agg[, X_extended_years] - CO2_Coal_NEuse_agg[, X_extended_years]
  
# Make negative values zero. Keep diagnostics of negative values
  diag_subzero <- melt( CO2_Conversion, id=c( "iso", "units" ) ) %>%
    filter( value < 0 ) %>% cast()
  diag_subzero[ is.na( diag_subzero ) ] <- ""
  CO2_Conversion[ CO2_Conversion < 0 ] <- 0

# Clean up after aggregation
  CO2_Conversion$fuel <- "process"
  CO2_Conversion$sector <- "1A1bc_Other-transformation"
  CO2_Conversion <- CO2_Conversion[ c( "iso", "sector", "fuel", "units", X_extended_years ) ]

# ---------------------------------------------------------------------------
# 7. Subtract Edgar 1B1 fugitive solid fuel emissions and 2c scaled ceds values
  
  # aggregate 1B1 and 2C by iso
  CEDS_1B1and2c <- CO2_total_CEDS_emissions[which(CO2_total_CEDS_emissions$sector %in%
                                                    c('1B1_Fugitive-solid-fuels','2C_Metal-production')),]
  CO2_1B1and2c <- aggregate(CEDS_1B1and2c[X_extended_years],
                            by = list(iso = CEDS_1B1and2c$iso), sum)

  # add zeros for gum and srb
  fill_in_countries <- data.frame(mat.or.vec(2,(length(X_extended_years))))
  fill_in_countries <- cbind(c("gum","srb (kosovo)"),fill_in_countries)
  names(fill_in_countries) <- c('iso',X_extended_years)

  CO2_1B1and2c <- rbind(CO2_1B1and2c,fill_in_countries)
  CO2_1B1and2c <- arrange(CO2_1B1and2c,iso)

  # Check that all 3 dfs have same ID columns
  if( any( CO2_1B1and2c$iso !=
           CO2_Conversion$iso ) ) {
    stop( "ID columns do not match.")
  }

  # subtract 1B1 and 2c from conversion
  CO2_Conversion_1B1and2c <- CO2_Conversion
  CO2_Conversion_1B1and2c[X_extended_years] <- CO2_Conversion_1B1and2c[X_extended_years] - CO2_1B1and2c[X_extended_years]
  
# ---------------------------------------------------------------------------
# Diagnostic: Extract flows for all countries and some big countries
# Function to extract relevant flows given iso
  extractFlows <- function( CO2_Coal_Total_agg, CO2_Coal_NEuse_agg, CO2_Coal_Combustion_agg, selected_iso="global" ) {
    # Extract relevant iso
    if( selected_iso == "global" ) {
      coal_total <- CO2_Coal_Total_agg
      coal_neuse <- CO2_Coal_NEuse_agg
      coal_combustion <- CO2_Coal_Combustion_agg
      coal_total$iso <- selected_iso
      coal_neuse$iso <- selected_iso
      coal_combustion$iso <- selected_iso
      
    } else if( selected_iso %!in% CO2_Coal_Total_agg$iso ) {
      stop( "extractFlows(): Invalid iso")
      
    } else {
      coal_total <- filter( CO2_Coal_Total_agg, iso==selected_iso )
      coal_neuse <- filter( CO2_Coal_NEuse_agg, iso==selected_iso )
      coal_combustion <- filter( CO2_Coal_Combustion_agg, iso==selected_iso )
    }
    
    # Reformat
    coal_total$flow <- "CO2_Coal_Total"
    coal_neuse$flow <- "CO2_Coal_NEuse"
    coal_combustion$flow <- "CO2_Coal_Combustion"
    
    # Aggregate
    coal_all <- bind_rows( coal_total, coal_neuse, coal_combustion ) %>%
      group_by( iso, flow, units ) %>%
      summarise_each( funs( sum(., na.rm = T ) ) ) %>%
      data.frame()
    
    return( coal_all )
  }
  
  global <- extractFlows( CO2_Coal_Total_agg, CO2_Coal_NEuse_agg, CO2_Coal_Combustion_agg, "global" )
  deu <- extractFlows( CO2_Coal_Total_agg, CO2_Coal_NEuse_agg, CO2_Coal_Combustion_agg, "deu" )
  usa <- extractFlows( CO2_Coal_Total_agg, CO2_Coal_NEuse_agg, CO2_Coal_Combustion_agg, "usa" )
  diag_flows <- bind_rows( global, deu, usa )
  

# ---------------------------------------------------------------------------
# 7. Output
  writeData( CO2_Conversion, "MED_OUT", "H.CO2_calculated_other_transformation_emissions_before_1B1and2c_correction" )
  writeData( CO2_Conversion_1B1and2c, "DIAG_OUT", "H.CO2_calculated_other_transformation_emissions" )
  writeData( CO2_Coal_Total, "DIAG_OUT", "H.CO2_Coal_Total" )
  writeData( CO2_Coal_NEuse, "DIAG_OUT", "H.CO2_Coal_NEuse" )
  writeData( CO2_Coal_Combustion, "DIAG_OUT", "H.CO2_Coal_Combustion" )
  #writeData( diag_coal_total_repl_cmp, "DIAG_OUT", "H.CO2_conversion_total_coal_replacement_year_comp" )
  writeData( diag_flows, "DIAG_OUT", "H.CO2_conversion_selected_flows" )
  
if( nrow( diag_subzero ) > 0 ) {
  writeData( diag_subzero, "DIAG_OUT", "H.CO2_conversion_subzero" )
}

logStop()