# Program Name: H4.3.add_emissions_CO2_other_transformation.R
# Author: Linh Vu
# Date Last Updated: 23 Nov 2016 
# Program Purpose: Compute CO2 other transformation coal:
#     CO2_Conversion = CO2_Coal_Total - CO2_Coal_Combustion - CO2_Coal_NEuse
#                      - CO2_1b1and2c
#   using coal consumption and default EF.
# Input Files: A.IEA_en_stat_ctry_hist.csv, CO2_total_CEDS_emissions.csv, 
#   H.Extended_coal_by_fuel_combustion.csv, H.Extended_other_tranformation_coal.csv
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
headers <- c( 'data_functions.R') 
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
  A.en_stat_sector_fuel <-readData( "MED_OUT", "A.en_stat_sector_fuel" )
  H.Extended_coal_by_fuel_all <- readData( "DIAG_OUT", "H.Extended_coal_by_fuel_all" )
  CO2_total_CEDS_emissions <- readData( "MED_OUT", paste0( 'H.', em,'_total_CEDS_emissions_before_other_transformation_replacement') )
  coal_ef <- readData( "DIAG_OUT", "B.CO2_comb_EF_non-bunker" )
  IEA_product_fuel <- readData( "MAPPINGS", "/energy/IEA_product_fuel" )
  emission_coefficient <- readData( "DEFAULT_EF_IN", "CO2_base_EF", ".xlsx", 
                                    sheet_selection = "Emission_Coefficient" )
  fraction_oxidized <- readData( "DEFAULT_EF_IN", "CO2_base_EF", ".xlsx", 
                                 sheet_selection = "Fraction_Oxidized" )
  MSL <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Sectors" )
  iea_start <- readData('EXT_IN','iea_start_date', ".xlsx", sheet_selection = "coal")
  H.Extended_total_coal <- readData( "DIAG_OUT", "H.Extended_total_coal" )
  
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
  
# Unit conversion of Natural gas from kt CO2/TJ to kt CO2/Kt 
  emission_coefficient$Emission_Coefficient[emission_coefficient$fuel %in% "natural_gas"] <- 
    (emission_coefficient$Emission_Coefficient[emission_coefficient$fuel=="natural_gas"] * 
    conversionFactor_naturalgas_TJ_per_kt)
  emission_coefficient$units[ emission_coefficient$fuel %in% "natural_gas" ] <- "kt CO2/kt"
  
# Calculate CO2 Emmission from coal gas products; gas_works_gas, blast_furnace_gas,coke_oven_gas and other_recovered_gases
  #secify the coal gases
  coal_gas_sectors <- c("blast_furnace_gas", "other_recovered_gases","coke_oven_gas", "gas_works_gas")
  
# retrieve IEA consumption data for each coal gas in coal_gas_sectors above
  IEA_en_coalgas_data <- A.en_stat_sector_fuel
  IEA_en_coalgas_data <-  IEA_en_coalgas_data[IEA_en_coalgas_data$sector %in% coal_gas_sectors, ]
  
# calulate the Natural gas coeficient 
  #obtain natural gas's oxidation fraction
  default_ng_fraction_oxidized <- fraction_oxidized$Fraction_Oxidized[fraction_oxidized$fuel=="natural_gas" & fraction_oxidized$iso == "default"]
  
  #obtain natural gas's emission coeficient 
  default_em_coeficient  <- emission_coefficient$Emission_Coefficient[emission_coefficient$fuel=="natural_gas"
                                                              & emission_coefficient$sector=="default"]
  #compute the natural gas emission coeficient 
  ng_em_coeficient <- (default_em_coeficient * default_ng_fraction_oxidized)
  
# Calc CO2 emission treating all coal gases as natural gases (NG)
  c_coalgases_as_ng <- IEA_en_coalgas_data
  c_coalgases_as_ng[X_IEA_years] <- (IEA_en_coalgas_data[X_IEA_years] * ng_em_coeficient)

#-----------------------------------------------------------------------------------------
  #Extend CO2 coalgas emmissions forward and backwards 
  driver_trend <- H.Extended_total_coal
  input_data <- c_coalgases_as_ng
  forward_ext_end_year <- paste0('X',2014)
  backward_ext_start_year <- 1750
  backward_ext_end_year <- 1959
  x_back_extension_years <- paste0('X',backward_ext_start_year:backward_ext_end_year)
  ratio_years <- paste0('X', c((backward_ext_end_year+1),(backward_ext_end_year+2),(backward_ext_end_year+3)
                               ,(backward_ext_end_year+4),(backward_ext_end_year+5)))
  
  #linearly extended data template to 2014
  input_data[forward_ext_end_year] <- input_data[X_IEA_end_year]
  
  # select CEDS coalgas data to extend
  ceds_extension_ratios <- input_data[ which(input_data$iso %in% driver_trend$iso) , ]
  ceds_extension_ratios <- ceds_extension_ratios[,c('iso','sector','fuel',ratio_years)]
  
  # add Driver identifyer ratio year
  ceds_extension_ratios <- merge(ceds_extension_ratios, driver_trend[,c("iso", ratio_years)],
                                 by.x = c('iso'),
                                 by.y = c("iso"),
                                 all.x = TRUE, all.y = FALSE)
  
  # calculate ratio
  ceds_extension_ratios[ratio_years] <- ceds_extension_ratios[ paste0(ratio_years,'.x')]/ceds_extension_ratios[ paste0(ratio_years,'.y')]
  
  # make all infinite ratios zero
  ceds_extension_ratios <- replace(ceds_extension_ratios, ceds_extension_ratios == 'NaN', 0)
  ceds_extension_ratios <- replace(ceds_extension_ratios, is.na(ceds_extension_ratios), 0)
  
  ceds_extension_ratios$ratio <-  rowMeans(ceds_extension_ratios[ratio_years])
  
  # add driver data and use ratio to calculate extended value
  CO2_Coal_Extension <- ceds_extension_ratios[,c('iso','fuel','sector','ratio')]
  CO2_Coal_Extension[x_back_extension_years] <- NA
  
  # add to final extension template
  CO2_Coal_Extension <- replaceValueColMatch(CO2_Coal_Extension, driver_trend,
                                             x.ColName = x_back_extension_years,
                                             match.x = c('iso'),
                                             addEntries = FALSE)
  #replace NA's with zeros 
  CO2_Coal_Extension[is.na(CO2_Coal_Extension)] <- 0
  
  # calculate extended data
  CO2_Coal_Extension[ x_back_extension_years ] <- CO2_Coal_Extension$ratio * CO2_Coal_Extension[ x_back_extension_years ]
  
  #final template
  CO2_Coal_Extension <- cbind.data.frame(CO2_Coal_Extension,input_data[X_IEA_years],input_data[forward_ext_end_year] )
  
  
# ---------------------------------------------------------------------------
# 3. Compute CO2_Coal_Total = 
# Mass Balance - Total CO2 emissions from Coal = Primary Energy Coal*EF +
# Secondary Energy Coal (import - exports , Coal Coke)* EF
  # Add other transformation to combustion coal as "hard coal"
  # Calculate Net coal coke from IEA (Imports - Exports) and extended ceds coke 
  # (production + imports - exports)

# a. Add other transformation coal as hard coal
  # H.Extended_coal_by_fuel_all
  extended_coal <- H.Extended_coal_by_fuel_all
  extended_coal[which(extended_coal$fuel == 'coal'),'fuel'] <- 'hard_coal'
  extended_coal$units <- "kt"
  extended_coal <- extended_coal[ c( "iso", "fuel", "units", X_extended_years ) ]
  extended_coal <- group_by( extended_coal, iso, fuel, units ) %>%
    summarise_each( funs( sum(., na.rm = T ) ) ) %>% data.frame()

# a. Extend Coal Coke (imports - exports) with extended coal coke (production + imports - exports)
  # extended total coal coke value
  extended_coke <- H.Extended_coal_by_fuel_all %>% filter(fuel == 'coal_coke') %>% 
    mutate(value = 'ceds_coke') %>% 
    arrange(iso)
  
  # calculate IEA coke (imports - exports)
  IEA_coke <- filter( A.IEA_en_stat_ctry_hist, FLOW %in% c("IMPORTS", "EXPORTS"), fuel=="coal_coke" ) %>%
    select( -FLOW, -PRODUCT ) %>% group_by( iso, fuel ) %>%
    summarise_each( funs( sum(., na.rm = T ) ) ) %>% data.frame() %>% mutate(value = 'iea_coke')
  # add countries with zero data
  zero_coke_countries <- unique( extended_coke$iso[ extended_coke$iso %!in% IEA_coke$iso ] )
  add_zero_coke_countries <- data.frame( iso = zero_coke_countries, fuel = 'coal_coke', value = 'iea_coke' )
  add_zero_coke_countries[ X_IEA_years ] <- 0
  IEA_coke <- rbind(IEA_coke, add_zero_coke_countries) %>% arrange(iso)
  
  if ( any( extended_coke$iso != IEA_coke$iso ) ) { stop('Rows do not match for ') }
  
  # define function to extend coke value for a specific iea start date
  
  extend_coke_fun <- function(start_year){
  
  historical_ratio_years <- c( paste0('X',start_year) ,paste0('X',start_year+1),paste0('X',start_year+2) )
  recent_ratio_years <- c(paste0('X',IEA_end_year), paste0('X',IEA_end_year-1), paste0('X',IEA_end_year-2))
  
  countries <- iea_start[which(iea_start$start_year == start_year),'iso']
  # ceds extended coal
  ceds <- filter(extended_coke, iso %in% countries ) %>% 
   select_('iso', .dots = c(historical_ratio_years , recent_ratio_years ) ) 
 
  ceds$ceds_total <- rowMeans(ceds[ historical_ratio_years] )
  ceds$ceds_total_recent <- rowMeans(ceds[ recent_ratio_years ] )
  
  # iea calculated coke (imports - exports)
  iea <- filter(IEA_coke, iso %in% countries ) %>%
    select_('iso', .dots = c(historical_ratio_years , recent_ratio_years )  )

  iea$iea_total <- rowMeans(iea[ historical_ratio_years] )
  iea$iea_total_recent <- rowMeans(iea[ recent_ratio_years ] )
  
  # calculate coke ratio of total extended coal (ceds)
  ratio <- left_join ( select(ceds, iso, ceds_total, ceds_total_recent) , select( iea, iso, iea_total, iea_total_recent ) )  %>%
    mutate( ratio = iea_total/ceds_total) %>% 
    mutate( ratio_recent = iea_total_recent/ceds_total_recent)
  
  ratio$ratio[!is.finite(ratio$ratio)] <- 0
  ratio$ratio_recent[!is.finite(ratio$ratio_recent)] <- 0
  
  # multiply ratio to get extended iea coke (imports - exports)
  IEA_coke_extended <- filter( IEA_coke, iso %in% countries)
  IEA_coke_years <- names(IEA_coke)[grep('X',names(IEA_coke))]
  
  all_ext_years <- X_extended_years[X_extended_years %!in%  IEA_coke_years]
  historical_ext_years <- paste0('X',1750:(start_year-1))
  forward_ext_years <- paste0('X',BP_years)
  
  IEA_coke_extended[ all_ext_years ] <- NA
  
  IEA_coke_extended[ historical_ext_years ] <- replicate(ratio$ratio, n = length(historical_ext_years)) * extended_coke[ which( extended_coke$iso %in% countries) , historical_ext_years ]
  if( length(BP_years) == 1 ) IEA_coke_extended[ forward_ext_years ] <- as.vector(replicate(ratio$ratio_recent, n = length(forward_ext_years))) * extended_coke[ which( extended_coke$iso %in% countries) , forward_ext_years ]
  if( length(BP_years) > 1 ) IEA_coke_extended[ forward_ext_years ] <- replicate(ratio$ratio_recent, n = length(forward_ext_years)) * extended_coke[ which( extended_coke$iso %in% countries) , forward_ext_years ]
  
  return(IEA_coke_extended)                                                                                                               
}
  
  # extend IEA coke back to 1750
  iea_extended_coke_list <-  lapply(FUN = extend_coke_fun, X = unique(iea_start$start_year) ) 
  iea_extended_coke <- do.call( rbind, iea_extended_coke_list) %>% 
                          mutate( units = 'kt')
  iea_extended_coke <- iea_extended_coke[ c('iso','fuel', X_extended_years ) ] %>% mutate(units = 'kt') %>% arrange(iso)

  # Make new data frame for energy data for calculating total CO2
  
  total_coal <- rbind( iea_extended_coke , 
                        filter(extended_coal, fuel != 'coal_coke') ) %>% 
                 arrange(iso, fuel)

# c. Compute CO2 emissions from total coal consumption and EF

  total_coal_long <- melt( total_coal , id = c( "iso", "fuel", "units" ) ) %>%
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
  

  # combine components of CO2-other transformation for diagnostics
  CO2_components_other_tranformation <- rbind.fill( 
    CO2_Coal_Total %>% mutate(value = paste0('Total_',fuel)) %>% select(-fuel),
    CO2_Coal_Combustion %>% mutate(value = paste0('combustion_',fuel)) %>% select(-fuel) ,
    CO2_Coal_NEuse %>% mutate(value = paste0('NEuse_',fuel)) %>% select(-fuel),
    CO2_1B1and2c %>% mutate(value = '1B1and2c'),
    CO2_Conversion_1B1and2c %>% mutate(value = 'final_emissions') ) %>% select(iso,value,units,contains('X'))
  
# ---------------------------------------------------------------------------
# 7. Output
  writeData( CO2_Conversion, "MED_OUT", "H.CO2_calculated_other_transformation_emissions_before_1B1and2c_correction" )
  writeData( CO2_Conversion_1B1and2c, "MED_OUT", "H.CO2_calculated_other_transformation_emissions" )
  writeData( CO2_Coal_Total, "DIAG_OUT", "H.CO2_Coal_Total" )
  writeData( CO2_Coal_NEuse, "DIAG_OUT", "H.CO2_Coal_NEuse" )
  writeData( CO2_Coal_Combustion, "DIAG_OUT", "H.CO2_Coal_Combustion" )
  writeData( CO2_components_other_tranformation, "DIAG_OUT", "H.CO2_components_other_tranformation" )
  writeData( c_coalgases_as_ng, "DIAG_OUT", "H.C_coalgases_as_ng" )
  writeData( CO2_Coal_Extension, "MED_OUT", "H.C_coalgases_as_ng_extension" )
  
  #writeData( diag_coal_total_repl_cmp, "DIAG_OUT", "H.CO2_conversion_total_coal_replacement_year_comp" )
  writeData( diag_flows, "DIAG_OUT", "H.CO2_conversion_selected_flows" )
  
if( nrow( diag_subzero ) > 0 ) {
  writeData( diag_subzero, "DIAG_OUT", "H.CO2_conversion_subzero" )
}

logStop()
