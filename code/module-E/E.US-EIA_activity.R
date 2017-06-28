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
# 1. Define constants and functions for inventory specific script

# Given constants
    convert_TrillionBTU_to_TJ <- 1055
    convert_shortTon_to_tonne <- 0.9072
    convert_tonne_to_barrel <- 7.33 # heat_content factor from OPEC 2014
    
    
# createCompPlot
# Brief: this function is a helper for generating batch comparison
#        graphs between CEDS and EIA data, one sector at a time
# Params:
#     CEDS_data: the CEDS dataset, aggregated to country x agg sector
#     EIA_data: the EIA dataset, aggregated to country x sector
#     CEDS_sectors: an ordered list of CEDS_sectors we want to compare
#     EIA_sectors: an ordered list of EIA_sectors we want to compare
#     number: which index in the sector lists will tell us what we're comparing
# Returns: a single ggplot
    createCompPlot <- function( CEDS_data, EIA_data, CEDS_sectors, EIA_sectors, number ) {
        
    # If the sector is "total", aggregate by year
        if ( CEDS_sectors[ number ] == "Total" ) {
            CEDS_data <- CEDS_data[ CEDS_data$sector %!in%
                                    c( "1A4_Stationary_RCO" ), ]
            temp_CEDS_data <- ddply( CEDS_data, 
                                     "year", 
                                     function(x) colSums( x[ "Value" ] ) )
            temp_EIA_data <- ddply( EIA_data, 
                                    "year", 
                                    function(x) colSums( x[ "Value" ] ) )
            
    # Otherwise, extract data whose sector matches the one asked for
        } else {
            temp_CEDS_data <- CEDS_data[ which( CEDS_data$sector == 
                                                CEDS_sectors[ number ] ), ]
            temp_EIA_data <- EIA_data[ which( EIA_data$sector == 
                                              EIA_sectors[ number ] ), ]
        }
    
    # Adjust units for readability (if over 1000, use Mt instead of kt)
        y_label <- "Consumption [kt]"
        if ( max( temp_CEDS_data$Value ) > 1000 || max( temp_EIA_data$Value ) > 1000 ) {
            temp_CEDS_data$Value <- temp_CEDS_data$Value / 1000
            temp_EIA_data$Value <- temp_EIA_data$Value / 1000
            y_label <- "Consumption [Mt]"
        }
      
    # Create the ggplot. CEDS is blue, EIA is red.
        p <- ggplot( temp_EIA_data, aes( year, Value ) ) + 
          geom_line( data = temp_CEDS_data, aes( year, Value ), color = "blue" ) +
          geom_line( data = temp_EIA_data, aes( year, Value ), color = "red" ) + 
          theme( legend.position = "right" ) +
          ggtitle( list_of_EIA_sectors[ number ] ) +
          ylab( y_label )
        
        return( p )
    }

# g_legend
# Brief: A function that extracts and returns the legend of a ggplot
#        (e.g. for arrangement with a group of plots)
    g_legend <- function( a.gplot ) {
        tmp <- ggplot_gtable( ggplot_build( a.gplot ) )
        leg <- which( sapply( tmp$grobs, function(x) x$name ) == "guide-box" )
        legend <- tmp$grobs[[leg]]
        return( legend ) 
    }


# ------------------------------------------------------------------------------
# 2. Read in different data frames from EIA folder
#    Reads in the relevant files. First creates a single large dataframe containing
#    the reported data, then creates dataframes storing heat content factors

# Prepare to read in all *.csv files from the EIA data folder
    files_to_read <- list.files( "activity/EIA-data/" )
    files_to_read <- gsub( "\\.csv$","", files_to_read[ grep( "*.csv", files_to_read ) ] )

# Initialize an empty dataframe
    all_EIA_raw_data <- NULL

# Read each file and append to a single dataframe
    for (f in files_to_read) {
        if ( grepl( "MER", f ) )
        temp_df <- readData( f, domain = "ACTIVITY_IN", domain_extension = "EIA-data/" )
        
        if ( is.null( all_EIA_raw_data ) ) {
            all_EIA_raw_data <- temp_df
        } else {
            all_EIA_raw_data <- rbind( all_EIA_raw_data, temp_df )
        }
    }

# Read in heat content conversion files  
    coal_heat_content <- readData( "MER_TA5", domain = "ACTIVITY_IN", 
                                 domain_extension = "EIA-data/unit-conversion/" )
    petrol_heat_content <- readData( "MER_TA3", domain = "ACTIVITY_IN",
                                   domain_extension = "EIA-data/unit-conversion/" )
    gas_heat_content <- readData( "MER_TA4", domain = "ACTIVITY_IN",
                                   domain_extension = "EIA-data/unit-conversion/" )
# ------------------------------------------------------------------------------
# 3. Basic reformatting
#    Brings the EIA data into TJ and year/month form.
  
# Extract month and year info from EIA dataset
    EIA_data_formatted <- all_EIA_raw_data
    EIA_data_formatted$year <- paste0( "X", substr( all_EIA_raw_data$YYYYMM, 1, 4 ) )
    EIA_data_formatted$month <- substr( all_EIA_raw_data$YYYYMM, 5, 6 )

# Retain only annual info
    EIA_data_formatted <- EIA_data_formatted[ which ( EIA_data_formatted$month == 13 ), 
                                              c( "MSN", "Value", "Description", 
                                                 "Unit", "year" ) ]

# Drop not-available cells
    EIA_data_formatted <- EIA_data_formatted[ which ( EIA_data_formatted$Value %!in% 
                                                      c( "Not Available", "No Data Reported" ) ), ]

# Convert to TJ from BTU
    EIA_data_formatted$Value[ which( EIA_data_formatted$Unit == "Trillion Btu" ) ] <- 
                          convert_TrillionBTU_to_TJ * as.numeric( EIA_data_formatted$Value )
    EIA_data_formatted$Unit[ which( EIA_data_formatted$Unit == "Trillion Btu" ) ] <- "TJ"
    
# ------------------------------------------------------------------------------
# 4. Fuel mapping
#    Identifies the CEDS fuels corresponding to each row of EIA data and discards
#    irrelevant data.
  
    EIA_data_formatted$fuel <- NA
    
# Extract fuel information from EIA descriptions
    EIA_data_formatted$fuel[ grep( "Coal", EIA_data_formatted$Description ) ] <- "coal"
    EIA_data_formatted$fuel[ grep( "Biomass", EIA_data_formatted$Description ) ] <- "biomass"
    EIA_data_formatted$fuel[ grep( "Natural Gas", EIA_data_formatted$Description ) ] <- "gas"
    EIA_data_formatted$fuel[ grep( "Petroleum", EIA_data_formatted$Description ) ] <- "oil"

# Any data that does not contain fuel-specific info need not be used
    data_unused <- EIA_data_formatted[ is.na( EIA_data_formatted$fuel ), ]
# Report if any cells failed to be matched
    printLog( paste0( nrow( data_unused ), 
                      " valid annual datapoints were not mapped to CEDS fuels" ) )

# Keep only matched data
    EIA_data_formatted <- EIA_data_formatted[ !is.na( EIA_data_formatted$fuel ), ]
    
    
# ------------------------------------------------------------------------------
# 5. Sector mapping
#    Same as fuel mapping, but sectors.
    
    EIA_data_formatted$sector <- NA
    
# Extract sector-level information from EIA descriptions
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
    
# Same as with fuel; identify and report any unmatched rows
    data_unused <- EIA_data_formatted[ is.na( EIA_data_formatted$sector ), ]
    printLog( paste0( nrow( data_unused ), " valid annual datapoints were not mapped to CEDS sectors" ) )
    EIA_data_formatted <- EIA_data_formatted[ !is.na( EIA_data_formatted$fuel ), ]

# Assign an iso to the dataset
    EIA_data_formatted$iso <- "usa"

# Extract only those columns we want to use from now on (CEDS info)
    EIA_data_formatted <- EIA_data_formatted[ , c( "iso", "sector", "fuel", 
                                                   "Unit", "year", "Value" ) ]

# ------------------------------------------------------------------------------
# 6. Prepare unit conversion
#    Code blocks 6 and 7 use heat content conversion files to determine the mass
#    of fuel consumed (given energy EIA data).
#    Biomass is converted using a constant.
#    Petroleum is converted from BTU to barrels using the EIA heat content timeseries, 
#        then to mass given a density (constant?)
#    Coal is converted from BTU directly to mass using the EIA heat constant timeseires
#    Natural gas is CURRENTLY converted using a constant, but this is WRONG and
#        we want to change that.
#    The result of these two sections is a formatted EIA data frame in mass units
    
# Extract year data from heat content dataframes
    coal_heat_content$year <- paste0( "X", substr( coal_heat_content$YYYYMM, 1, 4 ) )
    petrol_heat_content$year <- paste0( "X", substr( petrol_heat_content$YYYYMM, 1, 4 ) )
    gas_heat_content$year <- paste0( "X", substr( gas_heat_content$YYYYMM, 1, 4 ) )

# Match petroleum to sectors based on ID indicators (manually identified)
    petrol_heat_content$sector <- NA
    petrol_heat_content$sector[ which( petrol_heat_content$MSN == "PARCKUS" ) ] <- "Residential"
    petrol_heat_content$sector[ which( petrol_heat_content$MSN == "PACCKUS" ) ] <- "Commercial"
    petrol_heat_content$sector[ which( petrol_heat_content$MSN == "PAICKUS" ) ] <- "Industry"
    petrol_heat_content$sector[ which( petrol_heat_content$MSN == "PAACKUS" ) ] <- "Transportation"
    petrol_heat_content$sector[ which( petrol_heat_content$MSN == "PAEIKUS" ) ] <- "Energy Transf/Ext"
    
# Match petroleum to sectors based on ID indicators (manually identified)
    coal_heat_content$sector <- NA
    coal_heat_content$sector[ which( coal_heat_content$MSN == "CLHCKUS" ) ] <- "Residential and Commercial"
    coal_heat_content$sector[ which( coal_heat_content$MSN == "CLOCKUS" ) ] <- "Industry"
    coal_heat_content$sector[ which( coal_heat_content$MSN == "CLEIKUS" ) ] <- "Energy Transf/Ext"

# Coal reports a single heat content value for both residential and commercial
# sectors; we need to make separate entries for these for mapping purposes
    coal_heat_content_res <- coal_heat_content[ which( coal_heat_content$sector == 
                                                       "Residential and Commercial" ), ]
    coal_heat_content_res$sector <- "Residential"
    coal_heat_content_com <- coal_heat_content[ which( coal_heat_content$sector == 
                                                       "Residential and Commercial" ), ]
    coal_heat_content_com$sector <- "Commercial"
# We need to do the same thing to separate transportation from industry (single
# reported value)
    coal_heat_content_trans <- coal_heat_content[ which( coal_heat_content$sector == 
                                                         "Industry" ), ]
    coal_heat_content_trans$sector <- "Transportation"
    coal_heat_content <- rbind( coal_heat_content, coal_heat_content_com, coal_heat_content_res,
                                coal_heat_content_trans )
    
# Remove unmapped entries
    petrol_heat_content <- petrol_heat_content[ !is.na( petrol_heat_content$sector ), ] 
    coal_heat_content <- coal_heat_content[ !is.na( coal_heat_content$sector ), ]
# Add fuel indicators
    coal_heat_content$fuel <- "coal"
    petrol_heat_content$fuel <- "oil"
    
# Convert from (million btu/short ton) to TJ/kt
    petrol_heat_content$Value <- as.numeric( petrol_heat_content$Value ) / convert_tonne_to_barrel
    petrol_heat_content$Unit <- "Million BTU/tonne"
# Convert from Million BTU/t to TJ/kt 
    petrol_heat_content$Value <- as.numeric(petrol_heat_content$Value) * 
                                    ( convert_TrillionBTU_to_TJ / 10^6 ) * 10^3 
    petrol_heat_content$Unit <- "TJ/kt"

    
    coal_heat_content$Value <- as.numeric(coal_heat_content$Value) * 
                                    convert_shortTon_to_tonne * 10^3 * # Convert short ton to kt
                                    ( convert_TrillionBTU_to_TJ / 10^6 ) # Convert Million BTU to TJ
    coal_heat_content$Unit <- "TJ/kt"
    
    conversion_factors <- rbind( coal_heat_content, petrol_heat_content )
    
# ------------------------------------------------------------------------------
# 7. Execute conversion to kt
#    Executes the conversions described in Code Block 6 description
  
# Convert biomass to kt using CEDS standard conversion factor ### This will hopefully change--we need an EIA factor since they use diff. reporting
    EIA_data_formatted$Value[ which( EIA_data_formatted$fuel == "biomass" ) ] <-
          as.numeric( EIA_data_formatted$Value[ which( EIA_data_formatted$fuel == "biomass" ) ] ) / 
                  conversionFactor_biomass_kt_TJ
    EIA_data_formatted$Unit[ which( EIA_data_formatted$fuel == "biomass" ) ] <- "kt"
    
# Convert natural gas to kt using CEDS standard conversion factor ### This will hopefully change--we need an EIA factor since they use diff. reporting
    EIA_data_formatted$Value[ which( EIA_data_formatted$fuel == "gas" ) ] <-
          as.numeric( EIA_data_formatted$Value[ which( EIA_data_formatted$fuel == "gas" ) ] ) / 
                  conversionFactor_naturalgas_TJ_per_kt
    EIA_data_formatted$Unit[ which( EIA_data_formatted$fuel == "gas" ) ] <- "kt"
    
# Subset the data which can be converted using timeseries EIA conversion data
    EIA_convert_subset <- EIA_data_formatted[ which( EIA_data_formatted$fuel %in%
                                                       c( "oil", "coal" ) ), ]
    
# Join conversion factors to their corresponding data rows
    EIA_convert_subset <- left_join( EIA_convert_subset, 
                                     conversion_factors[ , c( "Value", "year", "fuel", "sector" ) ],
                                     by = c( "fuel", 
                                             "sector",
                                             "year" ) )
    colnames(EIA_convert_subset)[6:7] <- c( "Value", "Conversion_factor" )
    
# Apply timeseries conversions
    EIA_convert_subset$Value <- as.numeric( EIA_convert_subset$Value ) / 
                                          EIA_convert_subset$Conversion_factor
    EIA_convert_subset$Unit <- "kt"

# Add converted data back into the main dataframe
    EIA_data_formatted[ which( EIA_data_formatted$fuel %in%
                                                       c( "oil", "coal" ) ), ] <-
            EIA_convert_subset[, c("iso", "sector", "fuel",
                                   "Unit", "year", "Value")]
   
# ------------------------------------------------------------------------------
# 8. Remove Coal Coke use from Industry 
#    Coal coke usage in coal coke manufacture is considered a process activity
#    in CEDS, so it needs to be removed from the EIA estimate
    
# Read in a supplementary EIA file that will identify coal coke activity
    coal_activity_all <- readData( "Table_6.2_Coal_Consumption_by_Sector", extension = '.xlsx',
                               domain = "ACTIVITY_IN", domain_extension = "EIA-data/",
                               skip_rows = 6, sheet_selection = "Annual Data")
    
# 
    coal_coke_consumed <- coal_activity_all[ , c( "Annual Total", 
                                                 "Coal Consumed by the Industrial Sector, Coke Plants" ) ]
    coal_coke_consumed$Unit <- "Thousand Short Tons"
    coal_coke_consumed <- coal_coke_consumed[ -1, ]
    colnames( coal_coke_consumed ) <- c( "year", "Value", "Unit")

# Convert to kt
    coal_coke_consumed <- coal_coke_consumed[ !is.na( coal_coke_consumed$year ), ]
    coal_coke_consumed$Value <- as.numeric( coal_coke_consumed$Value ) * convert_shortTon_to_tonne
    coal_coke_consumed$Unit <- "kt"
    
# Subtract from coal industry activity
    EIA_data_formatted$Value[ which( EIA_data_formatted$fuel == "coal" &
                                     EIA_data_formatted$sector == "Industry" ) ] <-
          as.numeric( EIA_data_formatted$Value[ which( EIA_data_formatted$fuel == "coal" &
                                           EIA_data_formatted$sector == "Industry" ) ] ) -
          as.numeric( coal_coke_consumed$Value )

    
# ------------------------------------------------------------------------------
# 9. Cast to wide and write output
    
    EIA_final <- spread( EIA_data_formatted, key = year, value = Value )
    writeData( EIA_final, domain = "MED_OUT", paste0( 'E.', em, '_US-EIA_inventory' ) )
    
    

# ------------------------------------------------------------------------------
# 10. Prepare data for comparison to CEDS trends
#     This section of code processes CEDS total activity data for comparison
#     to EIA data. Its input is A.total_activity; its output is CEDS activity
#     data aggregated to EIA-comparable sectors.

# Read in input file A.total_activity from intermediate-output
    total_activity <- readData( "A.total_activity", domain = "MED_OUT" )

# Retain only US data
    total_activity <- total_activity[ which( total_activity$iso == "usa" ), ]
    
# Read in mapping files (retaining only unique mapping combinations to avoid
# data duplication)
    MSL <- readData( "MAPPINGS", "Master_Sector_Level_Map" )[ , c( 'working_sectors_v1', 
                                                                   'aggregate_sectors' ) ] %>%
                unique()
    MFL <- readData( "Master_Fuel_Sector_List", domain = "MAPPINGS", extension = ".xlsx",
                     sheet_selection = "Fuels" )
    
# Map to agg sector, agg fuel
    X_CEDS_years <- paste0( "X", 1960:2014 )
    total_activity <- left_join( total_activity, MSL, 
                                 by = c( "sector" = "working_sectors_v1" ) )
    total_activity <- left_join( total_activity, MFL[ , c( 'fuel', 'aggregated_fuel' ) ], 
                                 by = c( "fuel" ) )
    
# Aggregate activity by agg sector and agg fuel
    total_act_agg <- ddply( total_activity, 
                            c( "aggregated_fuel", "aggregate_sectors" ), 
                            function(x) colSums( x[ X_CEDS_years ] ) )
    
# Remove process fuels
    total_act_agg <- total_act_agg[ which( total_act_agg$aggregated_fuel != "process" ), ]
    
# Melt the data to long form
    total_act_agg <- gather( total_act_agg, key=year, value=Value,
                             -aggregated_fuel, -aggregate_sectors )
    
# rename columns for comparison purposes
    colnames( total_act_agg )[ which( colnames( total_act_agg ) == 
                                      "aggregate_sectors" ) ] <- "sector"
    
# We want to compare EIA "Residential" and "Commercial" sectors to their CEDS
# working sectors, as they don't have aggregate equivalents in the CEDS system
# (they are combined in the RCO aggregate sector). Therefore we need to copy the
# data for CEDS working sectors 1A4a and 1A4b and append them to the dataframe.
    res_and_com_nonagg <- total_activity[ which( total_activity$sector %in% c( "1A4b_Residential",
                                                                "1A4a_Commercial-institutional" ) ),
                                           c( "sector", "aggregated_fuel", X_CEDS_years) ]
    res_and_com_nonagg <- ddply( res_and_com_nonagg, 
                                        c( "sector", "aggregated_fuel" ), 
                                        function(x) colSums( x[ X_CEDS_years ] ) )
    res_and_com_nonagg <- gather( res_and_com_nonagg, key=year, value=Value,
                             -aggregated_fuel, -sector )
    
    total_act_agg <- rbind( total_act_agg,
                            res_and_com_nonagg )
    
### TEST ONLY: transportation = transp + aviation + shipping
    # total_act_agg$Value[ total_act_agg$sector == "1A3_Transportation" ] <-
    #       total_act_agg$Value[ total_act_agg$sector == "1A3_Transportation" ] +
    #       total_act_agg$Value[ total_act_agg$sector == "1A3_International-shipping" ] +
    #       total_act_agg$Value[ total_act_agg$sector == "1A3_Aviation" ]


# ------------------------------------------------------------------------------
# 11. Compare 4 fuels use across 5 sectors
#     This section executes and generates graphs for a fuel-by-fuel comparison
#     between EIA and CEDS data

    list_of_fuels <- c( "coal", "gas", "oil", "biomass")
    
    library(grid)
# Loop through each fuel
    for (fuel in list_of_fuels) {
    
    # Extract this fuel's data for operation
        EIA_compare_fuel <- EIA_data_formatted[ EIA_data_formatted$fuel == fuel, ]
        CEDS_compare_fuel <- total_act_agg[ total_act_agg$aggregated_fuel == fuel, ]
        
    # Convert from Xyears to numeric years
        EIA_compare_fuel$year <- as.numeric( substr( EIA_compare_fuel$year, 2, 5 ) )
        CEDS_compare_fuel$year <- as.numeric( substr( CEDS_compare_fuel$year, 2, 5 ) )
    
    # Make sure values are numeric
        EIA_compare_fuel$Value <- as.numeric( EIA_compare_fuel$Value )
        CEDS_compare_fuel$Value <- as.numeric( CEDS_compare_fuel$Value )
    
    # These lists work in parallel, acting to pair each EIA sector with its CEDS
    # equivalent. The first items in each list match, then the second, etc etc
        list_of_EIA_sectors <- c( "Commercial", "Residential", "Industry", 
                                  "Energy Transf/Ext", "Transportation", "Total" )
        list_of_CEDS_equivs <- c( "1A4a_Commercial-institutional", "1A4b_Residential",
                                  "1A2_Industry-combustion", "1A1_Energy-transformation",
                                  "1A3_Transportation", "Total" )
    
    # Create a useless chart with fake data for the purposes of extracting its legend
        data_for_legend <- data.frame( c( "EIA", "CEDS" ), 
                                       c( 10, 10, 10, 10 ),   # Made up data
                                       c( 20, 30, 40, 50 ) )
        colnames(data_for_legend) <- c( "Inventory", "Value", "year" ) 
        plot_for_legend <- ggplot( data_for_legend, 
                                   aes( year, Value, color = Inventory ) ) +
                           geom_line() + 
                           scale_color_manual( values = c( "EIA" = "red",
                                                           "CEDS" = "blue" ) )
        inv_legend <- g_legend( plot_for_legend )
        
    # Apply down the list of sectors, executing the createCompPlot function
    # defined and explained in Code Block 1
        list_of_plots <- lapply( 1:6,
                                 createCompPlot,
                                 CEDS_data = CEDS_compare_fuel,
                                 EIA_data = EIA_compare_fuel,
                                 CEDS_sectors = list_of_CEDS_equivs,
                                 EIA_sectors = list_of_EIA_sectors )
        
        layout <- rbind( c(1, 1, NA),
                         c(1, 1, 2),
                         c(1, 1, NA) )
      
    # Arrange the plots and save to output
        arranged_plots <- grid.arrange( arrangeGrob( grobs=list_of_plots ),
                                        inv_legend, layout_matrix = layout,
                                            top = textGrob( paste0( "Compare CEDS ", fuel, " to EIA ", fuel), 
                                            gp = gpar( fontsize = 15, font = 8 ) ) )
        ggsave( paste0( "../diagnostic-output/ceds-comparisons/Compare-", 
                        fuel, "-CEDS-to-EIA.png" ), 
                arranged_plots,
                width = 8, 
                height = 5.66 )
    }

    