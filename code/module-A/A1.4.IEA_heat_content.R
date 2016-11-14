# Program Name: A1.4.IEA_heat_content.R
# Author: Linh Vu
# Date Last Updated: 7 Nov 2016
# Program Purpose: Computes heat content from IEA Conversion Factors
#   by country, year and fuel type. Currently doing this for coal.
# Input Files: OECD_Conversion_Factors_Full.csv, NonOECD_Conversion_Factors_Full.csv,
#     IEA_product_fuel.csv, Master_Country_List.csv
# Output Files:  A.coal_heat_content.csv
# Notes: This script handles iso+fuel+year duplicates by summing. This works
#     because all duplicates are currently disaggregated countries and
#     cover different years. The script will print warnings and produce a diagnostic
#     A.coal_heat_content_duplicates.csv when this is no longer the case. If
#     so, revise the handling process to avoid discontinuities.
# TODO:    
# ---------------------------------------------------------------------------
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
headers <- c( 'data_functions.R', 'common_data.R' ) 
log_msg <- "Calculating coal heat content from IEA conversion factors"  # First message to be printed to the log
script_name <- 'A1.4.IEA_heat_content.R'

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )


# ---------------------------------------------------------------------------
# 1. Input
  conversion_OECD <- readData( "ENERGY_IN", "OECD_Conversion_Factors_Full" )
  conversion_NonOECD <- readData( "ENERGY_IN", "NonOECD_Conversion_Factors_Full" )

  IEA_product_fuel <- readData( "MAPPINGS", "IEA_product_fuel", domain_extension = "energy/" )
  MCL <- readData( "MAPPINGS", "Master_Country_List" )

# ---------------------------------------------------------------------------
# 2. Compute heat content (kJ/kg) for each country
# Combine into one df, convert to numeric  
  conversion_all <- bind_rows( conversion_OECD, conversion_NonOECD )
  conversion_all$X2014E <- NULL
  conversion_all[, X_emissions_years ] <- suppressWarnings( lapply( conversion_all[, X_emissions_years ], 
                                                              function(x){ as.numeric(as.character(x)) }))

# Clean up mapping
  IEA_product_fuel$product <- gsub(" \\([kt/TJ].*", "", IEA_product_fuel$product )
  
# Select relevant flow
  conversion_all <- filter( conversion_all, FLOW %in% c( "Average net calorific value" ) )

# Select representative products for each fuel type: 
# -- coal_coke: "Coking coal"
# -- hard_coal: "Hard coal (if no detail)"; "Other bituminous coal"
# -- brown_coal: "Brown coal (if no detail)"; "Lignite" (or "Sub-bituminous coal" if Lignite not available)
  coal_coke_products <- c( "Coking coal" )
  hard_coal_products <- c( "Hard coal (if no detail)", "Other bituminous coal" )
  brown_coal_products <- c( "Brown coal (if no detail)", "Lignite", "Sub-bituminous coal" )
  conversion_coal <- filter( conversion_all, PRODUCT %in% c( coal_coke_products, hard_coal_products, brown_coal_products ) )
  conversion_coal$fuel[ conversion_coal$PRODUCT  %in% coal_coke_products ] <- "coal_coke"
  conversion_coal$fuel[ conversion_coal$PRODUCT  %in% hard_coal_products ] <- "hard_coal"
  conversion_coal$fuel[ conversion_coal$PRODUCT  %in% brown_coal_products ] <- "brown_coal"
  
# For brown coal, keep "Sub-bituminous coal" only if "Lignite" NA for all years
  ctry_no_lignite <- filter( conversion_coal, PRODUCT == "Lignite" )
  ctry_no_lignite <- ctry_no_lignite[ rowSums( is.na( ctry_no_lignite ) ) == length( X_emissions_years ), ]
  conversion_coal <- filter( conversion_coal, PRODUCT != "Sub-bituminous coal" | COUNTRY %in% ctry_no_lignite$COUNTRY )
  
# Compute average heat content by fuel type
  hc_coal <- select( conversion_coal, -FLOW, -PRODUCT ) %>% group_by( COUNTRY, fuel ) %>%
    summarise_each( funs( mean(., na.rm = T ) ) ) %>% data.frame()
    
# Map to MCL countries  
  hc_coal_all <- merge( hc_coal, select( MCL, iso, COUNTRY = IEAName ) )
  hc_coal_all$units <- "kJ/kg"
  hc_coal_all <- hc_coal_all[ c( "iso", "fuel", "units", X_emissions_years ) ]
  
# Drop duplicates and rows of all NAs  
  hc_coal_all <- unique( hc_coal_all )
  hc_coal_all <- hc_coal_all[ rowSums( is.na( hc_coal_all ) ) != length( X_emissions_years ), ]

# Diagnostics: This block prints out any iso+fuel+year combinations that have multiple data points
  dup <- filter( hc_coal_all, duplicated( paste( hc_coal_all$iso, hc_coal_all$fuel ) ) )
  dup <- filter( hc_coal_all, iso %in% dup$iso ) %>%
    arrange( iso, fuel )
  dup_long <- melt( dup, id = c( "iso", "fuel", "units" ) ) %>%
    filter( !is.na( value ) ) %>%
    group_by( iso, fuel, units, variable ) %>%
    summarise( value = length( value ) ) %>%
    filter( value != 1 )
  for ( i in seq_along( dup_long[[1]] ) ) {
    stop( paste( "Duplicates:", dup_long$iso[[i]], dup_long$fuel[[i]], dup_long$variable[[i]] ) )
  }
    
# Remaining iso+fuel duplicates seem mostly composite/broken up countries that cover
# separate years, so okay to combine by summing these rows.
  hc_coal_all <- group_by( hc_coal_all, iso, fuel, units ) %>%
    summarise_each( funs( mean(., na.rm = T ) ) ) %>% data.frame()
  
# Interpolate NAs, extend last non-NAs forward/backward
  hc_coal_all_ext <- hc_coal_all
  hc_coal_all_ext[, X_emissions_years ] <- interpolate_NAs( hc_coal_all_ext[, X_emissions_years ] )
  hc_coal_all_ext <- melt( hc_coal_all_ext, id = c( "iso", "fuel", "units" ) )
  hc_coal_all_ext <- ddply( hc_coal_all_ext, .(iso, fuel, units), function(df){
    df$value <- na.locf( df$value, na.rm = F )
    df$value <- na.locf( df$value, na.rm = F, fromLast = T )
    return( df ) 
  })
  hc_coal_all_ext <- cast( hc_coal_all_ext )
  

# ---------------------------------------------------------------------------
# 3. Output
  writeData( hc_coal_all_ext, "MED_OUT", "A.coal_heat_content" )
  if (nrow( dup_long ) > 0)
    writeData( dup_long, "DIAG_OUT", "A.coal_heat_content_duplicates" )

logStop()