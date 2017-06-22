#------------------------------------------------------------------------------
# Program Name: E.US-EIA_activity.R
# Author: Ben Goldstein
# Date Last Modified: June 22, 2017
# Program Purpose: To read in & reformat EIA activity data from 1949 to 2014
# Units are initially in btu
# Input Files: all files in the folder input/activity/EIA-data
# Output Files: E.[em]_EIA_activity.csv
# Notes: 

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
    petrol_conversion <- readData( "MER_TA3", domain = "ACTIVITY_IN",
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
    
    EIA_data_formatted$fuel <- NA
    
    EIA_data_formatted$fuel[ grep( "Coal", EIA_data_formatted$Description ) ] <- "coal"
    EIA_data_formatted$fuel[ grep( "Biomass", EIA_data_formatted$Description ) ] <- "biomass"
    EIA_data_formatted$fuel[ grep( "Natural Gas", EIA_data_formatted$Description ) ] <- "gas"
    EIA_data_formatted$fuel[ grep( "Petroleum", EIA_data_formatted$Description ) ] <- "oil"
    
    data_unused <- EIA_data_formatted[ is.na( EIA_data_formatted$fuel ), ]
    printLog(paste0(nrow(data_unused), " valid annual datapoints were not mapped to CEDS fuels"))
    
    EIA_data_formatted <- EIA_data_formatted[ !is.na( EIA_data_formatted$fuel ), ]
    
    
# ------------------------------------------------------------------------------
# 4. Sector mapping
    
    EIA_data_formatted$sector <- NA
    
    EIA_data_formatted$sector[ grep( "Residential Sector", 
                                         EIA_data_formatted$Description ) ] <- "Residential"
    EIA_data_formatted$sector[ grep( "Commercial Sector", 
                                         EIA_data_formatted$Description ) ] <- "Commercial"
    EIA_data_formatted$sector[ grep( "Industrial Sector", 
                                         EIA_data_formatted$Description ) ] <- "Industry"
    EIA_data_formatted$sector[ grep( "Transportation Sector", 
                                         EIA_data_formatted$Description ) ] <- "Transportation"
    EIA_data_formatted$sector[ grep( "Electric Power Sector", 
                                         EIA_data_formatted$Description ) ] <- "Energy Transf/Ext"
    
    data_unused <- EIA_data_formatted[ is.na( EIA_data_formatted$sector ), ]
    printLog( paste0( nrow( data_unused ), " valid annual datapoints were not mapped to CEDS sectors" ) )
    
    EIA_data_formatted <- EIA_data_formatted[ !is.na(EIA_data_formatted$fuel), ]
    
    EIA_data_formatted$iso <- "usa"
    
    EIA_data_formatted <- EIA_data_formatted[ , c( "iso", "sector", "fuel", 
                                                   "Unit", "year", "Value" ) ]
    

# ------------------------------------------------------------------------------
# 4. Prepare unit conversion
    
    coal_conversion$year <- paste0( "X", substr( coal_conversion$YYYYMM, 1, 4 ) )
    petrol_conversion$year <- paste0( "X", substr( petrol_conversion$YYYYMM, 1, 4 ) )
    
    petrol_conversion$sector <- NA
    petrol_conversion$sector[ which( petrol_conversion$MSN == "PARCKUS") ] <- "Residential"
    petrol_conversion$sector[ which( petrol_conversion$MSN == "PACCKUS") ] <- "Commercial"
    petrol_conversion$sector[ which( petrol_conversion$MSN == "PAICKUS") ] <- "Industry"
    petrol_conversion$sector[ which( petrol_conversion$MSN == "PAACKUS") ] <- "Transportation"
    petrol_conversion$sector[ which( petrol_conversion$MSN == "PAEIKUS") ] <- "Energy Transf/Ext"

    coal_conversion$sector <- NA
    coal_conversion$sector[ which( coal_conversion$MSN == "CLHCKUS") ] <- "Residential and Commercial"
    coal_conversion$sector[ which( coal_conversion$MSN == "CLOCKUS") ] <- "Industry"
    coal_conversion$sector[ which( coal_conversion$MSN == "CLEIKUS") ] <- "Energy Transf/Ext"

    coal_conversion_res <- coal_conversion[ which( coal_conversion$sector == 
                                                     "Residential and Commercial"), ]
    coal_conversion_res$sector <- "Residential"
    coal_conversion_com <- coal_conversion[ which( coal_conversion$sector == 
                                                     "Residential and Commercial"), ]
    coal_conversion_com$sector <- "Commercial"
    coal_conversion_trans <- coal_conversion[ which( coal_conversion$sector == 
                                                     "Industry"), ]
    coal_conversion_trans$sector <- "Transportation"
    
    
    coal_conversion <- rbind( coal_conversion, coal_conversion_com, coal_conversion_res,
                              coal_conversion_trans)
    
    petrol_conversion <- petrol_conversion[ !is.na(petrol_conversion$sector), ] 
    coal_conversion <- coal_conversion[ !is.na(coal_conversion$sector), ] 
    coal_conversion$fuel <- "coal"
    petrol_conversion$fuel <- "oil"
    
# Convert from (million btu/short ton) to TJ/kt
    petrol_conversion$Value <- as.numeric(petrol_conversion$Value) * 1.163
    petrol_conversion$Unit <- "TJ/kt"
    
    coal_conversion$Value <- as.numeric(coal_conversion$Value) * 1.163
    coal_conversion$Unit <- "TJ/kt"
    
    conversion_factors <- rbind( coal_conversion, petrol_conversion )
    
# ------------------------------------------------------------------------------
# 4. Execute conversion to kt
  
    EIA_data_formatted$Value[ which( EIA_data_formatted$fuel == "biomass" ) ] <-
          as.numeric( EIA_data_formatted$Value[ which( EIA_data_formatted$fuel == "biomass" ) ] ) / 
                  conversionFactor_biomass_kt_TJ
    EIA_data_formatted$Unit[ which( EIA_data_formatted$fuel == "biomass" ) ] <- "kt"
    
    EIA_data_formatted$Value[ which( EIA_data_formatted$fuel == "gas" ) ] <-
          as.numeric( EIA_data_formatted$Value[ which( EIA_data_formatted$fuel == "gas" ) ] ) * 
                  conversionFactor_naturalgas_TJ_per_kt
    EIA_data_formatted$Unit[ which( EIA_data_formatted$fuel == "gas" ) ] <- "kt"
    
    
    EIA_convert_subset <- EIA_data_formatted[ which( EIA_data_formatted$fuel %in%
                                                       c( "oil", "coal" ) ), ]
    
    EIA_convert_subset <- left_join( EIA_convert_subset, 
                                     conversion_factors[ , c( "Value", "year", "fuel", "sector" ) ],
                                     by = c( "fuel", 
                                             "sector",
                                             "year" ) )
    colnames(EIA_convert_subset)[6:7] <- c("Value", "Conversion_factor")
    
    EIA_convert_subset$Value <- as.numeric(EIA_convert_subset$Value) / EIA_convert_subset$Conversion_factor
    EIA_convert_subset$Unit <- "kt"
    

    EIA_data_formatted[ which( EIA_data_formatted$fuel %in%
                                                       c( "oil", "coal" ) ), ] <-
            EIA_convert_subset[, c("iso", "sector", "fuel",
                                   "Unit", "year", "Value")]
    

# ------------------------------------------------------------------------------
# 4. Cast to wide and write output
    
    EIA_final <- spread(EIA_data_formatted, key=year, value=Value)
    
    writeData( EIA_final, domain="MED_OUT", paste0('E.',em,'_US-EIA_inventory'))
    
    
    