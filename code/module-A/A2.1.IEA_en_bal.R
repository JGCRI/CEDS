# ----------------------------------------------------------------------------
# Program Name: A2.1.IEA_en_bal.R
# Author's Name: Page Kyle, for GCAM; modified for use in CEDS Project by 
#               Steve Smith, Emily Voelker, Tyler Pitkanen, Jon Seibert, 
#               and Rachel Hoesly
# Date Last Modified: August 31, 2015
# Program Purpose: 
# Input Files: A.IEA_en_stat_ctry_hist.csv, IEA_flow_sector.csv, 
#              IEA_product_fuel.csv, Master_Fuel_Sector_List.xlsx, 
#              IEA_energy_balance_factor.csv
# Output Files: A.en_stat_sector_fuel.csv
# Notes: TPES Calculation deleted, not applicable to data in physical units.
# To Do: 
#   Make sure all input files feature fuel units, and that flow, sectors,
#   and units match up across the board. 
# ------------------------------------------------------------------------------

# Before we can load headers we need some paths defined. They may be provided 
#   by a system environment variable or they may have been set in the workspace
# Set variable PARAM_DIR to be the data system directory
    dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
    for ( i in 1:length( dirs ) ) {
        setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
        wd <- grep( 'CEDS/input', list.dirs(), value = T )
        if ( length(wd) > 0 ) {
            setwd( wd[1] )
            break
        }
    }
    PARAM_DIR <- "../code/parameters/"

    
# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "analysis_functions.R" ) # Additional function files required.
    log_msg <- paste0( "Historical energy balances from IEA, aggregated to GCAM",
                       " regions, sectors, and fuels" ) # First message to be printed to the log
    script_name <- "A2.1.IEA_en_bal.R"
    
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Read in files
    A.IEA_en_stat_ctry_hist_full <- readData( "MED_OUT", "A.IEA_en_stat_ctry_hist" )
    IEA_flow_sector <- readData( "ENERGY_IN", "IEA_flow_sector" )
    IEA_product_fuel <- readData( "MAPPINGS", "IEA_product_fuel" )
    fuel_list <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Fuels" )
    IEA_energy_balance_factor <- readData( "ENERGY_IN", "IEA_energy_balance_factor" )

# Check that files contain the proper names
    sectorCheck( IEA_flow_sector )
    fuelCheck( IEA_product_fuel )

# Define conversion factors for use later
    conversionFactor_biomass_kt_TJ <- 16  # Biomass - For kt to TJ (multiply by kt to get TJ)    
    #conversionFactor_biomass_TJ_kt <- 0.0238846  # For kt to TJ (multiply by kt to get TJ)
    conversionFactor_refinerygas_kt_TJ <- 48.5 #Refinery Gas TJ/kt. (Multiply by kt to get TJ)
    #49.5 TJ/Gg- 2006 IPCC guidelines for National GHG inventories Vol 2 - Energy, Ch 1 - Intro Table 1.2
    #IEA
    
# ------------------------------------------------------------------------------
# 2. Map IEA to CEDS sectors.
#    Map IEA to CEDS fuels and perform Unit Conversions
    
# Subset only the relevant years and combine OECD with non-OECD
    printLog( paste0( "Matching intermediate sector names, and intermediate",
                     " fuel names into IEA energy statistics" ) )
    A.IEA_en_stat_ctry_hist <- A.IEA_en_stat_ctry_hist_full
    
# Add CEDS sector names
    A.IEA_en_stat_ctry_hist$sector <- IEA_flow_sector$sector[ match( 
        A.IEA_en_stat_ctry_hist$FLOW, IEA_flow_sector$flow_code ) ]
# Add CEDS fuels
    A.IEA_en_stat_ctry_hist$fuel <- IEA_product_fuel$fuel[ match( 
        A.IEA_en_stat_ctry_hist$PRODUCT, IEA_product_fuel$product ) ]
    
#### Unit conversions - 
    kt_products <- na.omit( IEA_product_fuel[ grep( "(kt)", 
                                                    IEA_product_fuel[, 1 ] ), 1:2 ] ) 
    TJ_products <- na.omit( IEA_product_fuel[ grep( "TJ", 
                                                    IEA_product_fuel[, 1 ] ), 1:2 ] )
# Biomass
    TJ_to_kt_biomass <- TJ_products[ TJ_products$fuel == "biomass", ]
    
    A.IEA_en_stat_ctry_hist[ A.IEA_en_stat_ctry_hist$PRODUCT %in% 
            TJ_to_kt_biomass$product, X_IEA_years ] <- 
            A.IEA_en_stat_ctry_hist[ A.IEA_en_stat_ctry_hist$PRODUCT %in% 
            TJ_to_kt_biomass$product, X_IEA_years ] /  conversionFactor_biomass_kt_TJ

# Refinery Gas - given in kt, mapped to natural gas, so convert to TJ
    A.IEA_en_stat_ctry_hist[ A.IEA_en_stat_ctry_hist$PRODUCT %in% 
                               "Refinery gas (kt)", X_IEA_years ] <- 
                A.IEA_en_stat_ctry_hist[ A.IEA_en_stat_ctry_hist$PRODUCT %in% 
                                 "Refinery gas (kt)", X_IEA_years ] * conversionFactor_refinerygas_kt_TJ
    
# Add units to dataframe
  TJ_fuels <- c( "natural_gas" )
  A.IEA_en_stat_ctry_hist$units[ A.IEA_en_stat_ctry_hist$fuel %in% 
                                   TJ_fuels ] <- "TJ"
  A.IEA_en_stat_ctry_hist$units[ A.IEA_en_stat_ctry_hist$fuel %!in%
                                   TJ_fuels ] <- "kt"
# ------------------------------------------------------------------------------
# 3. Fix Energy Balance

  A.IEA_en_stat_ctry_hist_units <- A.IEA_en_stat_ctry_hist
  printLog( paste0( "Correcting IEA energy balance and negative values" ) )
  
# Add IEA flow conversion factor        
  A.IEA_en_stat_ctry_hist_units$conversion <- IEA_flow_sector$conversion[ match( 
          A.IEA_en_stat_ctry_hist_units$FLOW, IEA_flow_sector$flow_code ) ]
  
# Add IEA balance correction   
  A.IEA_en_stat_ctry_hist_units<-merge(A.IEA_en_stat_ctry_hist_units,IEA_energy_balance_factor,
                                   by=c('FLOW','PRODUCT'),all.x=TRUE,all.y=FALSE)
  
# Keep the sign of IEA Flow conversion factor. Don't retain numeric value
  A.IEA_en_stat_ctry_hist_units$conversion <- 
                A.IEA_en_stat_ctry_hist_units$conversion / abs( A.IEA_en_stat_ctry_hist_units$conversion ) 
  
# Replace balance_correction NA's with 1 so we don't get NAs when applying the conversion factor to data
  A.IEA_en_stat_ctry_hist_units[which(is.na(A.IEA_en_stat_ctry_hist_units$balance_correction)),'balance_correction']<- 
      rep_len(x=1,length.out=length(A.IEA_en_stat_ctry_hist_units[
        which(is.na(A.IEA_en_stat_ctry_hist_units$balance_correction)),'balance_correction']
        )) 
    
# Correct (+/-) Energy Balance 
  A.IEA_en_stat_ctry_hist_units[  X_IEA_years ]<-A.IEA_en_stat_ctry_hist_units[  X_IEA_years ] * 
    A.IEA_en_stat_ctry_hist_units$conversion *  A.IEA_en_stat_ctry_hist_units$balance_correction

# ------------------------------------------------------------------------------
# 3. Other Data Cleaning
  A.IEA_en_stat_bal <- A.IEA_en_stat_ctry_hist_units
  printLog( paste0( "Performing other data cleaning" ) )
  
# Drop rows that don't map to a CEDS sector or fuel
  A.IEA_en_stat_bal <-A.IEA_en_stat_bal[complete.cases(A.IEA_en_stat_bal[,c("sector","fuel")]),]

# Drop rows with negative data (negative energy balance, outputs in energy tranformation)
  negativeBalance<-c()
  positiveBalance<-c()
  for (i in 1:nrow( A.IEA_en_stat_bal )) {
    vals<-A.IEA_en_stat_bal[i,X_IEA_years]
    if (all(vals<=0) ) {negativeBalance<-c(negativeBalance,i)}
    else {positiveBalance<-c(positiveBalance,i)}
  }
  
  DroppedData<-A.IEA_en_stat_bal[negativeBalance,]
  A.IEA_en_stat_bal<-A.IEA_en_stat_bal[positiveBalance,]
  
  
# Replace "xxx_production" sector names with specific product-corresponding 
# native production sector names (fossil fuels only)
  for(i in 1:length(A.IEA_en_stat_bal$sector)){
    target = A.IEA_en_stat_bal$fuel[[i]]
    if(A.IEA_en_stat_bal$sector[[i]] == "xxx_production" && target != "biomass"){
      new_sector = fuel_list[fuel_list$fuel == target,"sector_replacement"]
      A.IEA_en_stat_bal$sector[[i]] = new_sector
    }
  }

#Drop data with unmapped production sector - 'xxx_production'   
  DroppedData<-rbind(DroppedData,
                     A.IEA_en_stat_bal[which(A.IEA_en_stat_bal$sector=='xxx_production'),])
  A.IEA_en_stat_bal<-A.IEA_en_stat_bal[-which(A.IEA_en_stat_bal$sector=='xxx_production'),]
  
# Aggregate by relevant categories   
    
printLog( paste0( "Aggregating energy statistics by intermediate sector",
                      " and intermediate fuel" ) )
#  aggregate() messes up row or column order depending
#   on how you call it, so fix that after    
    
  A.en_stat_sector_fuel <- aggregate( A.IEA_en_stat_bal[ 
    X_IEA_years ],
    by = list( fuel = A.IEA_en_stat_bal$fuel,
             sector = A.IEA_en_stat_bal$sector, 
             iso = A.IEA_en_stat_bal$iso,
             units= A.IEA_en_stat_bal$units), sum ) 
  
# Rearrange first three columns to resemble how they came in from the previous
#   script
    A.en_stat_sector_fuel <- A.en_stat_sector_fuel[ 
        c( 'iso', 'sector','fuel','units', X_IEA_years ) ]

# -----------------------------------------------------------------------------
# 3. Output
# Add comments for each table
    comments.A.en_stat_sector_fuel <- c( paste0( "Energy statistics", 
            " by intermediate sector / intermediate fuel / historical year" ),
        "Units = kt or TJ depending on fuel type" )

# diagnostic-output
    writeData( DroppedData, domain = "DIAG_OUT", 
               fn = "A.en_balace_dropped_data",
               comments = comments.A.en_stat_sector_fuel) 
												
# write tables as CSV files
    writeData( A.en_stat_sector_fuel, domain = "MED_OUT", 
        fn = "A.en_stat_sector_fuel",
        comments = comments.A.en_stat_sector_fuel)

# Every script should finish with this line:
logStop()