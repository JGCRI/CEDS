#------------------------------------------------------------------------------
# Program Name: E.US-EIA_activity.R
# Author: Ben Goldstein
# Date Last Modified: June 26, 2017
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
# 1. Define functions for inventory specific script

    createCompPlot <- function( CEDS_data, EIA_data, CEDS_sectors, EIA_sectors, number ) {
        temp_CEDS_data <- CEDS_data[ which( CEDS_data$sector == 
                                            CEDS_sectors[number]),]
        temp_EIA_data <- EIA_data[ which( EIA_data$sector == 
                                          EIA_sectors[number]),]
      
        p <- ggplot(temp_EIA_data, aes( year, Value ) ) + 
          geom_line(data=temp_CEDS_data, aes( year, Value ), color="blue" ) +
          geom_line(data=temp_EIA_data, aes( year, Value), color="red" ) + 
          theme( legend.position = "right") +
          ggtitle( list_of_EIA_sectors[number] )
        
        return(p)
    }

    g_legend <- function( a.gplot ) {
        tmp <- ggplot_gtable( ggplot_build( a.gplot ) )
        leg <- which( sapply( tmp$grobs, function(x) x$name ) == "guide-box" )
        legend <- tmp$grobs[[leg]]
        return( legend ) 
    }


# ------------------------------------------------------------------------------
# 2. Read in different data frames from EIA folder

    files_to_read <- list.files("activity/EIA-data/")
    files_to_read <- gsub("\\.csv$","", files_to_read[ grep( "*.csv", files_to_read ) ] )
  
    all_EIA_raw_data <- NULL
    
    for (f in files_to_read) {
        if ( grepl( "MER", f ) )
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
    gas_conversion <- readData( "MER_TA4", domain = "ACTIVITY_IN",
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
# 5. Sector mapping
    
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
# 6. Remove Coal Coke use from Industry 
#    Coal coke usage in coal coke manufacture is considered a process activity
#    in CEDS, so it needs to be removed from the EIA estimate
    
    

    

# ------------------------------------------------------------------------------
# 6. Prepare unit conversion
    
    coal_conversion$year <- paste0( "X", substr( coal_conversion$YYYYMM, 1, 4 ) )
    petrol_conversion$year <- paste0( "X", substr( petrol_conversion$YYYYMM, 1, 4 ) )
    gas_conversion$year <- paste0( "X", substr( gas_conversion$YYYYMM, 1, 4 ) )
    
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
    petrol_conversion$Value <- as.numeric(petrol_conversion$Value) * 7.33 # Conversion factor from OPEC 2014
    petrol_conversion$Unit <- "Million BTU/tonne"
    petrol_conversion$Value <- as.numeric(petrol_conversion$Value) * 1.055
    petrol_conversion$Unit <- "TJ/kt"

    
    coal_conversion$Value <- as.numeric(coal_conversion$Value) * 1.163
    coal_conversion$Unit <- "TJ/kt"
    
    conversion_factors <- rbind( coal_conversion, petrol_conversion )
    
# ------------------------------------------------------------------------------
# 7. Execute conversion to kt
  
    EIA_data_formatted$Value[ which( EIA_data_formatted$fuel == "biomass" ) ] <-
          as.numeric( EIA_data_formatted$Value[ which( EIA_data_formatted$fuel == "biomass" ) ] ) / 
                  conversionFactor_biomass_kt_TJ
    EIA_data_formatted$Unit[ which( EIA_data_formatted$fuel == "biomass" ) ] <- "kt"
    
    EIA_data_formatted$Value[ which( EIA_data_formatted$fuel == "gas" ) ] <-
          as.numeric( EIA_data_formatted$Value[ which( EIA_data_formatted$fuel == "gas" ) ] ) / 
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
# 8. Cast to wide and write output
    
    EIA_final <- spread(EIA_data_formatted, key=year, value=Value)
    
    writeData( EIA_final, domain="MED_OUT", paste0('E.',em,'_US-EIA_inventory'))
    
    

# ------------------------------------------------------------------------------
# 9. Prepare data for comparison to CEDS trends
    
    total_activity <- readData( "A.total_activity", domain = "MED_OUT" )
    total_activity <- total_activity[ which( total_activity$iso == "usa" ), ]
    MSL <- readData( "MAPPINGS", "Master_Sector_Level_Map" )[ , c('working_sectors_v1', 
                                                                  'aggregate_sectors')] %>%
                unique()
    MFL <- readData("Master_Fuel_Sector_List", domain = "MAPPINGS", extension = ".xlsx",
                    sheet_selection = "Fuels")
    
    X_CEDS_years <- paste0( "X", 1960:2014 )
    total_activity <- left_join( total_activity, MSL, 
                                 by = c( "sector" = "working_sectors_v1") )
    total_activity <- left_join( total_activity, MFL[ , c('fuel', 'aggregated_fuel')], 
                                 by = c( "fuel") )
    
    total_act_agg <- ddply( total_activity, 
                            c( "aggregated_fuel", "aggregate_sectors" ), 
                            function(x) colSums( x[ X_CEDS_years ] ) )
    
    total_act_agg <- total_act_agg[ which( total_act_agg$aggregated_fuel != "process"), ]
    
    total_act_agg <- gather( total_act_agg, key=year, value=Value,
                             -aggregated_fuel, -aggregate_sectors )
    
    colnames(total_act_agg)[ which(colnames(total_act_agg) == "aggregate_sectors")] <- "sector"
    
    res_and_com_nonagg <- total_activity[ which( total_activity$sector %in% c("1A4b_Residential",
                                                                "1A4a_Commercial-institutional")),
                                           c( "sector", "aggregated_fuel", X_CEDS_years) ]
    res_and_com_nonagg <- ddply( res_and_com_nonagg, 
                                        c( "sector", "aggregated_fuel" ), 
                                        function(x) colSums( x[ X_CEDS_years ] ) )
    res_and_com_nonagg <- gather( res_and_com_nonagg, key=year, value=Value,
                             -aggregated_fuel, -sector )
    
    total_act_agg <- rbind( total_act_agg,
                            res_and_com_nonagg )


# ------------------------------------------------------------------------------
# 9. Compare 4 fuels use across 5 sectors
    list_of_fuels <- c( "coal", "gas", "oil", "biomass")
    
    for (fuel in list_of_fuels) {
    
        EIA_compare_fuel <- EIA_data_formatted[ which(EIA_data_formatted$fuel == fuel), ]
        CEDS_compare_fuel <- total_act_agg[ which(total_act_agg$aggregated_fuel == fuel), ]
        
        EIA_compare_fuel$year <- as.numeric( substr( EIA_compare_fuel$year, 2, 5 ) )
        CEDS_compare_fuel$year <- as.numeric( substr( CEDS_compare_fuel$year, 2, 5 ) )
    
        EIA_compare_fuel$Value <- as.numeric( EIA_compare_fuel$Value )
        CEDS_compare_fuel$Value <- as.numeric( CEDS_compare_fuel$Value )
    
        list_of_EIA_sectors <- c( "Commercial", "Residential", "Industry", 
                                  "Energy Transf/Ext", "Transportation" )
        list_of_CEDS_equivs <- c( "1A4a_Commercial-institutional", "1A4b_Residential",
                                  "1A2_Industry-combustion", "1A1_Energy-transformation",
                                  "1A3_Transportation")
        
        data_for_legend <- data.frame( c( "EIA", "CEDS" ), 
                                       c( 10, 10, 10, 10 ),
                                       c( 20, 30, 40, 50 ) )
        colnames(data_for_legend) <- c( "Inventory", "Value", "year")
        
        plot_for_legend <- ggplot(data_for_legend, aes(year, Value, color = Inventory)) +
                           geom_line() + scale_color_manual( values = c("EIA"="red", "CEDS"="blue") )
        
        inv_legend <- g_legend( plot_for_legend )
        
        list_of_plots <- lapply( 1:5,
                                 createCompPlot,
                                 CEDS_data = CEDS_compare_fuel,
                                 EIA_data = EIA_compare_fuel,
                                 CEDS_sectors = list_of_CEDS_equivs,
                                 EIA_sectors = list_of_EIA_sectors)
        
        layout <- rbind( c(1, 1),
                         c(1, 1),
                         c(1, 2) )
      
        arranged_plots <- grid.arrange( arrangeGrob( grobs=list_of_plots ),
                                        inv_legend, layout_matrix = layout,
                                            top = textGrob( paste0( "Compare CEDS ", fuel, " to EIA ", fuel), 
                                            gp = gpar( fontsize = 15, font = 8 ) ) )
        ggsave( paste0( "../diagnostic-output/ceds-comparisons/Compare-", 
                        fuel, "-CEDS-to-EIA.png" ), 
                arranged_plots,
                width = 5, 
                height = 6 )
    }
    
    
    
    
    
    
    
    