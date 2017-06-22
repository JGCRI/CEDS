#------------------------------------------------------------------------------
# Program Name: E.US-EIA_activity.R
# Authors' Names: Tyler Pitkanen, Jon Seibert, Rachel Hoesly, Steve Smith, Ryan Bolt
# Date Last Modified: January 20, 2016
# Program Purpose: To read in & reformat Argentina emissions inventory data
# This data only contains data from 1990 - 2011, missing data from 2000 and 2010.
# Units are initially in metric tonnes
# Input Files: Argentina_Translation.xlsx, Argentina Inventario 1990-2012-ipcc1996.xlsx, 
# Output Files: E.[em]_ARG_inventory.csv
# Notes: 
# TODO: Re-write read-in so that order of years is taken from input data instead of assumed.
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Set working directory to the CEDS “input” directory and define PARAM_DIR as the
# location of the CEDS “parameters” directory relative to the new working directory.
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

# Get emission species first so can name log appropriately
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[1]
if ( is.na( em ) ) em <- "NOx"
  
# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
headers <- c( 'common_data.R',"data_functions.R" ,"emissions_scaling_functions.R",  "analysis_functions.R" ) # Additional function files required.
log_msg <- "Initial reformatting of Argentina emissions" # First message to be printed to the log
script_name <- "E.US-EIA_activity.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )


# ------------------------------------------------------------------------------
# 1. Define parameters for inventory specific script




# ------------------------------------------------------------------------------
# 2. Read in different data frames from EIA folder

    files_to_read <- list.files("activity/EIA-data/")
    files_to_read <- gsub("\\.csv$","", files_to_read[ grep( "*.csv", files_to_read ) ] )
  
    all_EIA_raw_data <- NULL
    
    for (f in files_to_read) {
        
        temp_df <- readData( f, domain = "ACTIVITY_IN", domain_extension = "EIA-data/" )
        
        if ( is.null( all_EIA_raw_data ) ) {
            all_EIA_raw_data <- temp_df
        } else {
            all_EIA_raw_data <- rbind(all_EIA_raw_data, temp_df)
        }
        
    }
    
    coal_conversion <- readData( "MER_TA5", domain = "ACTIVITY_IN", 
                                 domain_extension = "EIA-data/unit-conversion/")

# ------------------------------------------------------------------------------
# 3. Basic reformatting
    
    EIA_data_formatted <- all_EIA_raw_data
    
    EIA_data_formatted$year <- paste0( "X", substr( all_EIA_raw_data$YYYYMM, 1, 4 ) )
    
    EIA_data_formatted$month <- substr( all_EIA_raw_data$YYYYMM, 5, 6 )

    EIA_data_formatted <- EIA_data_formatted[ which ( EIA_data_formatted$month == 13 ), 
                                              c( "MSN", "Value", "Description", 
                                                 "Unit", "year" ) ]
    
    EIA_data_formatted <- EIA_data_formatted[ which ( EIA_data_formatted$Value %!in% c("Not Available", "No Data Reported") ), ]
    
    EIA_data_formatted$Value[which(EIA_data_formatted$Unit == "Trillion Btu")] <- 1055 * as.numeric(EIA_data_formatted$Value)
    EIA_data_formatted$Unit[which(EIA_data_formatted$Unit == "Trillion Btu")] <- "TJ"
    
# ------------------------------------------------------------------------------
# 4. Fuel mapping
    
    EIA_data_formatted$agg_fuel <- NA
    
    EIA_data_formatted$agg_fuel[ grep( "Coal", EIA_data_formatted$Description ) ] <- "coal"
    EIA_data_formatted$agg_fuel[ grep( "Biomass", EIA_data_formatted$Description ) ] <- "biomass"
    EIA_data_formatted$agg_fuel[ grep( "Natural Gas", EIA_data_formatted$Description ) ] <- "gas"
    EIA_data_formatted$agg_fuel[ grep( "Petroleum", EIA_data_formatted$Description ) ] <- "oil"
    
    data_unused <- EIA_data_formatted[ is.na( EIA_data_formatted$agg_fuel ), ]
    printLog(paste0(nrow(data_unused), " valid annual datapoints were not mapped to CEDS fuels"))
    
    EIA_data_formatted <- EIA_data_formatted[ !is.na( EIA_data_formatted$agg_fuel ), ]
    
    
# ------------------------------------------------------------------------------
# 4. Sector mapping
    
    EIA_data_formatted$agg_sector <- NA
    
    EIA_data_formatted$agg_sector[ grep( "Residential Sector", 
                                         EIA_data_formatted$Description ) ] <- "Residential"
    EIA_data_formatted$agg_sector[ grep( "Commercial Sector", 
                                         EIA_data_formatted$Description ) ] <- "Commercial"
    EIA_data_formatted$agg_sector[ grep( "Industrial Sector", 
                                         EIA_data_formatted$Description ) ] <- "Industry"
    EIA_data_formatted$agg_sector[ grep( "Transportation Sector", 
                                         EIA_data_formatted$Description ) ] <- "Transportation"
    EIA_data_formatted$agg_sector[ grep( "Electric Power Sector", 
                                         EIA_data_formatted$Description ) ] <- "Energy Transf/Ext"
    
    data_unused <- EIA_data_formatted[ is.na( EIA_data_formatted$agg_sector ), ]
    printLog( paste0( nrow( data_unused ), " valid annual datapoints were not mapped to CEDS sectors" ) )
    
    EIA_data_formatted <- EIA_data_formatted[ !is.na(EIA_data_formatted$agg_fuel), ]
    
    EIA_data_formatted$iso <- "usa"
    
    EIA_data_formatted <- EIA_data_formatted[ , c( "iso", "agg_sector", "agg_fuel", 
                                                   "Unit", "year", "Value" ) ]
    
    
    
    
    
    