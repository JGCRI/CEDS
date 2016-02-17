#------------------------------------------------------------------------------
# Program Name: A3.2.Adjust_Shipping_Fuel_Cons.R
# Authors Names: Steve Smith
# Program Purpose: Reads in exogenous time series for global shipping fuel consumption
#                  Adds difference with reported shipping fuel to global international shipping sector
#
#                  The idea here is that emissions from shipping fuel reported by a region
#                  (either domestic or international) will be estimated within that region.
#                  Because, however, the reported data underestimates shipping fuel consumption,
#                  this script adds the difference to a global sector.
#
#                  Note that this additional fuel is not otherwise accounted for. Because 
#                  we are using a sectoral approach, we are implicitly assuming the difference
#                  is within statistical reporting differences
#
# Output Description: Energy data with additional consumption for international shipping 
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
        if ( length(wd) > 0 ) {
            setwd( wd[1] )
            break
        }
    }
    PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "analysis_functions.R", "process_db_functions.R" ) # Additional function files required.
    log_msg <- "Add under-counted shipping emissions to global region" # First message to be printed to the log
    script_name <- "A3.2.Adjust_Shipping_Fuel_Cons.R"
    
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Read in files

    iea_data <- readData( "MED_OUT", "A.IEA_en_stat_ctry_hist" )
    iea_data_extended <- readData( "MED_OUT", "A.IEA_BP_energy_ext" )
    shipping_fuel_org <- readData( "ENERGY_IN", "Shipping_Fuel_Consumption" , ".xlsx", sheet_selection = "Data" ) 
    IEA_product_fuel <- readData( "EN_MAPPINGS", "IEA_product_fuel" )

    fuel_list <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Fuels" )
    
# -----------------------------------------------------------------------------------------
# 2. Clean shipping data
    # Use 4:180 to cut off the many extra rows that excel included
    shipping_fuel_org <- shipping_fuel_org[4:180, 1:14] # Note data also includes goods loaded if that might be useful
    names( shipping_fuel_org ) <- c('Year','hard_coal','heavy_oil','diesel_oil','total_petr','units')
  
    # make a new data frame by binding (cbind) the names
    shipping_fuel <- as.data.frame(cbind( names( shipping_fuel_org ), t( shipping_fuel_org ) ))
    
    # assign column names as the first row
    # names( shipping_fuel ) <- shipping_fuel[ 1, 1:ncol(shipping_fuel)]
    # The above doesn't work. Don't know why.
    
    # Now assign names 
    names( shipping_fuel ) <- c( 'fuel', paste0( 'X' , 1850:2012 ) )
    
    # remove the names that are currently the first row
    shipping_fuel <- shipping_fuel[ -1,  ]
  
# Now have the time series for total shipping fuel from 1850 to some recent year 
# in three categories: hard_coal, heavy_oil, diesel_oil
#                 
    
# -----------------------------------------------------------------------------------------
# 3. Collect other needed data
    
    iea_data$fuel <- IEA_product_fuel$fuel[ match( iea_data$PRODUCT, IEA_product_fuel$product ) ]
    # For purposes here, treat all coal as hard coal
    iea_data$fuel [ which ( iea_data$fuel %in% c( 'hard_coal', 'brown_coal' ) ) ] <- 'hard_coal'
    
    Fishing_fuel <- aggregate( iea_data[ X_IEA_years ],
                    by=list( fuel = iea_data$fuel,
                             FLOW = iea_data$FLOW ), sum )
  
    Fishing_fuel <- Fishing_fuel[ which(Fishing_fuel$FLOW == "FISHING" ), ]
    Fishing_fuel <- Fishing_fuel[ c( "fuel", X_IEA_years ) ]
    
    ceds_iea_data <- aggregate( iea_data_extended[ X_IEA_years ],
                               by=list( fuel = iea_data_extended$fuel,
                                        sector = iea_data_extended$sector ), sum )
    # Again, treat all coal as hard coal
    ceds_iea_data$fuel [ which ( ceds_iea_data$fuel %in% c( 'hard_coal', 'brown_coal' ) ) ] <- 'hard_coal'
    
    Int_Bunker_fuel <- ceds_iea_data[ which(ceds_iea_data$sector == "1A3di_International-shipping" ), ]
    Domestic_Ship_fuel <- ceds_iea_data[ which(ceds_iea_data$sector == "1A3dii_Domestic-naviation" ), ]
    
    Int_Bunker_fuel <- Int_Bunker_fuel[ c( "fuel", X_IEA_years ) ]
    Domestic_Ship_fuel <- Domestic_Ship_fuel[ c( "fuel", X_IEA_years ) ]

    Total_IEA_Ship_Fuel <- rbind( Int_Bunker_fuel, Domestic_Ship_fuel, Fishing_fuel)
    Total_IEA_Ship_Fuel <- aggregate( Total_IEA_Ship_Fuel[ X_IEA_years ],
                                by=list( fuel = Total_IEA_Ship_Fuel$fuel ), sum )
    
    # For purposes here, combine brown and hard coal
    
# -----------------------------------------------------------------------------------------
# 4. Determine amount of additional shipping fuel to be added 
# 
# Add to global international shipping sector fuel consumption equal to the difference between 
# shipping_fuel variable and the total_IEA_shipping fuel.
# Where total_IEA_shipping is > shipping_fuel:
#   diesel_oil : (ignore, as this shouldn't happen)
#   heavy_oil : Account for this in diesel oil (e.g., add less diesel oil)
#   coal : ignore (no adjustment )
# 
# Ignore coking coal, biomass, and natural gas
#
# Desired result is a time series by fuel from 1850 to the end of shipping_fuel data 
# 
# The above will generally be < IEA years. Last step is, for the last year of IEA data, 
# calculate average IEA underreport for last three years,and add that amount.
# 
# Then make constant for any years after IEA years.
#
# For years before IEA years, the global international shipping sector will contain 
# the entire shipping fuel estimate, since there is no IEA data at that point. 

# -----------------------------------------------------------------------------
# 8. Output
    
# write extended energy data
#  writeData( IEA_BP_ext, domain = "MED_OUT", fn = "A.IEA_BP_energy_ext", 
#          comments = comments.A.energy_data_extension )

# writeData( data.wide, "DIAG_OUT", paste0('summary-plots/',em ,'_emissions_scaled_by_fuel'), meta = FALSE )
  
        
# Every script should finish with this line
 logStop()
    
# END
    