# ----------------------------------------------------------------------------
# Program Name: A2.1.IEA_en_bal.R
# Author's Name: Page Kyle, for GCAM; modified for use in CEDS Project by 
#               Steve Smith, Emily Voelker, Tyler Pitkanen, and Jon Seibert
# Date Last Modified: June 16, 2015
# Program Purpose: 
# Input Files: A.IEA_en_stat_ctry_hist.csv, IEA_flow_sector.csv, 
#              IEA_product_fuel.csv, Master_Fuel_Sector_List.xlsx
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
        wd <- grep( 'emissions-data-system/input', list.dirs(), value = T )
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
    
# Check that files contain the proper names
    sectorCheck( IEA_flow_sector )
    fuelCheck( IEA_product_fuel )

# ------------------------------------------------------------------------------
# 2. Perform computations
# Subset only the relevant years and combine OECD with non-OECD
    printLog( paste0( "Matching intermediate sector names, and intermediate",
                     " fuel names into IEA energy statistics" ) )
    A.IEA_en_stat_ctry_hist <- A.IEA_en_stat_ctry_hist_full
    conversionFactor <- 0.0238846  # For kt to TJ (multiply by kt to get TJ)

# Add in the fuel names and sector names
    A.IEA_en_stat_ctry_hist$sector <- IEA_flow_sector$sector[ match( 
        A.IEA_en_stat_ctry_hist$FLOW, IEA_flow_sector$flow_code ) ]
    A.IEA_en_stat_ctry_hist$fuel <- IEA_product_fuel$fuel[ match( 
        A.IEA_en_stat_ctry_hist$PRODUCT, IEA_product_fuel$product ) ]
    A.IEA_en_stat_ctry_hist$conversion <- IEA_flow_sector$conversion[ match( 
        A.IEA_en_stat_ctry_hist$FLOW, IEA_flow_sector$flow_code ) ]
      
# Drop missing values; many of the products and flows are not used
    #A.IEA_en_stat_ctry_hist <- na.omit( A.IEA_en_stat_ctry_hist )
# ISSUE: Above line deletes important rows
    
##Match units of products within fuel groupings. Coal and biomass fuels are 
#   mixed between kt and TJ, so convert to just kt
    TJ_products <- na.omit( IEA_product_fuel[ grep( "TJ", 
        IEA_product_fuel[, 1 ] ), 1:2 ] )
    kt_products <- na.omit( IEA_product_fuel[ grep( "(kt)", 
        IEA_product_fuel[, 1 ] ), 1:2 ] ) 
    TJ_to_kt <- rbind( TJ_products[ TJ_products$fuel == "biomass", ] )
    A.IEA_en_stat_ctry_hist[ A.IEA_en_stat_ctry_hist$PRODUCT %in% 
        TJ_to_kt$product, X_IEA_years ] <- 
            A.IEA_en_stat_ctry_hist[ A.IEA_en_stat_ctry_hist$PRODUCT %in% 
            TJ_to_kt$product, X_IEA_years ] / conversionFactor 

##Ensure any modifications to sector-fuel combinations match units. Sector 
#   changes don't affect unit groupings
# Refined liquids (in sector in_industry_gtl) mapped to gas, so convert liqs
#   in that sector to TJ before they're moved
    A.IEA_en_stat_ctry_hist[ A.IEA_en_stat_ctry_hist$PRODUCT == 
        "refined liquids" & A.IEA_en_stat_ctry_hist$sector == 
        "in_industry_gtl", X_IEA_years ] <- 
    A.IEA_en_stat_ctry_hist[ A.IEA_en_stat_ctry_hist$PRODUCT == 
        "refined liquids" & A.IEA_en_stat_ctry_hist$sector == 
        "in_industry_gtl", X_IEA_years ] * conversionFactor

# Drop some sector-fuel combinations that are not relevant- no emissions.
# Electricity-only fuels in sectors other than electricity generation
    A.IEA_en_stat_ctry_hist$sector[ grepl( "elec_", 
        A.IEA_en_stat_ctry_hist$fuel) & !grepl( "electricity generation", 
        A.IEA_en_stat_ctry_hist$sector ) ] <- NA
    A.IEA_en_stat_ctry_hist$sector[ A.IEA_en_stat_ctry_hist$fuel %in%
        c( "biomass", "heat" ) & grepl( "trn_", 
        A.IEA_en_stat_ctry_hist$sector ) ] <- NA

# Subset the table minus the missing values for further processing
   # A.IEA_en_stat_ctry_hist_clean <- na.omit( A.IEA_en_stat_ctry_hist )
   # ISSUE: Above line deletes important rows
    A.IEA_en_stat_ctry_hist_clean <- A.IEA_en_stat_ctry_hist
    
# Aggregate by relevant categories
    printLog( paste0( "Aggregating energy statistics by intermediate sector",
                     " and intermediate fuel" ) )

# Alter conversion factors to unity for use in emissions data (don't convert 
#   all units to EJ as with GCAM), but keep signs the same 
    A.IEA_en_stat_ctry_hist_clean$conversion <- 
        A.IEA_en_stat_ctry_hist_clean$conversion / abs( 
        A.IEA_en_stat_ctry_hist_clean$conversion )
# Perform the aggregation. aggregate() messes up row or column order depending
#   on how you call it, so fix that after
    A.en_stat_sector_fuel <- aggregate( A.IEA_en_stat_ctry_hist_clean[ 
        X_IEA_years ] * A.IEA_en_stat_ctry_hist_clean$conversion,
        by = list( fuel = A.IEA_en_stat_ctry_hist_clean$fuel,
                   sector = A.IEA_en_stat_ctry_hist_clean$sector, 
                   iso = A.IEA_en_stat_ctry_hist_clean$iso ), sum )
                   
# Add a column of units
    TJ_fuels <- c( "natural_gas" )
    A.en_stat_sector_fuel$units[ A.en_stat_sector_fuel$fuel %in% 
        TJ_fuels ] <- "TJ"
    A.en_stat_sector_fuel$units[ A.en_stat_sector_fuel$fuel %!in%
        TJ_fuels ] <- "kt"
        
# Rearrange first three columns to resemble how they came in from the previous
#   script
    A.en_stat_sector_fuel <- A.en_stat_sector_fuel[ 
        c( 3, 2, 1, length(A.en_stat_sector_fuel), 
            4:( ncol(A.en_stat_sector_fuel) - 1) ) ]

# Replace "xxx_production" sector names with specific product-corresponding 
# native production sector names (fossil fuels only)
for(i in 1:length(A.en_stat_sector_fuel$sector)){
	target = A.en_stat_sector_fuel$fuel[[i]]
	if(A.en_stat_sector_fuel$sector[[i]] == "xxx_production" && target != "biomass"){
		new_sector = fuel_list[fuel_list$fuel == target,"sector_replacement"]
		A.en_stat_sector_fuel$sector[[i]] = new_sector
	}
}

# Remove rows with any remaining "xxx_production" sectors
for(i in 1:length(A.en_stat_sector_fuel$sector)){
	target = A.en_stat_sector_fuel$sector[[i]]
	if(target == "xxx_production"){
		A.en_stat_sector_fuel$sector[[i]] = NA
	}
}
#A.en_stat_sector_fuel = na.omit(A.en_stat_sector_fuel)


# -----------------------------------------------------------------------------
# 3. Output
# Add comments for each table
    comments.A.en_stat_sector_fuel <- c( paste0( "Energy statistics", 
            " by intermediate sector / intermediate fuel / historical year" ),
        "Units = kt or TJ depending on fuel type" )

# writeData( A.IEA_en_stat_ctry_hist_clean, domain = "MED_OUT", 
#         fn = "A.en_stat_sector_fuel_unag",
#         comments = comments.A.en_stat_sector_fuel)

												
# write tables as CSV files
    writeData( A.en_stat_sector_fuel, domain = "MED_OUT", 
        fn = "A.en_stat_sector_fuel",
        comments = comments.A.en_stat_sector_fuel)

# Every script should finish with this line:
logStop()