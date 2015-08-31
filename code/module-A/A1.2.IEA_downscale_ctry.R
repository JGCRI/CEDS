#------------------------------------------------------------------------------
# Program Name: A1.2.IEA_downscale_ctry.R
# Author's Name: Page Kyle, for GCAM; modified for use in CEDS project by
#                Steve Smith, Emily Voelker, Tyler Pitkanen, Jon Seibert, and
#                Linh Vu
# Date Last Modified: August 4, 2015
# Program Purpose: Reads in the initial IEA energy data.
#				   Splits the composite data into individual countries.
# 				   Maps the aggregate coal consumption in earlier years to 
#                      specific coal types.
# Input Files: A.UN_pop_master, OECD_E_stat.csv, NonOECD_E_stat.csv, IEA_ctry.csv
# Output Files: A.IEA_en_stat_ctry_hist.csv
# Notes: IEA_years and X_IEA_years are now defined in 
#           common_data.R, and range from 1960-2010.
# TODO: Add read-in of documentation data for this files
# TODO: Ultimately do a more sophisticated country split using population data
#           in a few places. (Unless its already doing this.)
#		Ultimately modify this to use more detailed data on coking coal 
#           consumption instead of using a constant ratio for the split.
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
    headers <- c( "data_functions.R" ) # Additional function files required.
    log_msg <- "IEA energy balances by all countries and historical years" # First message to be printed to the log
    script_name <- "A1.2.IEA_downscale_ctry.R"
    
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Read in files

# Read in historical years, UN population data, OECD and non-OECD energy
#   statistics, and mappings.
# "Invalid factor" warnings can be ignored- hence the warning suppressor code.
    w <- getOption( "warn" )
    options( warn=-1 )	# suppress the warning about columns names and appending
    
	UN_pop_master <- readData( "MED_OUT", "A.UN_pop_master" )
	OECD_E_Stat <- readData( "ENERGY_IN", "OECD_E_Stat", ".csv" )
	NonOECD_E_Stat <- readData( "ENERGY_IN", "NonOECD_E_Stat", ".csv" )
	IEA_ctry <- readData( "MAPPINGS", "IEA_ctry" )

    options( warn=w )
# ------------------------------------------------------------------------------
# 2. Perform computations

# 2.1 Preparatory Calculations

# subset only the relevant years, and combine OECD data with non-OECD data
	printLog( "Combining OECD and non-OECD databases" )
	IEA_IDcodes  <- c( "COUNTRY", "FLOW", "PRODUCT" )
	A.IEAfull <- rbind( OECD_E_Stat[ c( IEA_IDcodes, X_IEA_years ) ],
        NonOECD_E_Stat[ c( IEA_IDcodes, X_IEA_years ) ] )

# Rename fuels with inconsistent naming between the two databases
# Note: May be necessary to change products in this section in the future.
# At present, all this does it decapitalize the K in "Other Kerosene (kt)"
	A.IEAfull$PRODUCT[ A.IEAfull$PRODUCT == "Other Kerosene (kt)" ] <- 
        "Other kerosene (kt)"

# 2.2 Deal with composite regions (e.g. Other Africa) and historical countries
#     (Former Soviet Union and Former Yugoslavia) that get broken

# Split the country mapping table into composite regions and single-countries
	IEA_composite <- subset( IEA_ctry, IEA_ctry %in% c( 
        "Former Soviet Union (if no detail)", "Former Yugoslavia (if no detail)",
		"Other Africa", "Other Non-OECD Americas", "Other Asia" ) )
	IEA_single <- subset( IEA_ctry, IEA_ctry %!in% IEA_composite$IEA_ctry )

# Split IEA energy statistics into table of single countries and composite 
#   regions (keeping only desired composite regions)
	A.IEAcomp <- subset( A.IEAfull, COUNTRY %in% IEA_composite$IEA_ctry )
	A.IEAsingle <- subset( A.IEAfull, COUNTRY %in% IEA_single$IEA_ctry )
	A.IEAsingle$iso <- IEA_single$iso[ match( A.IEAsingle$COUNTRY, 
        IEA_single$IEA_ctry ) ]

# 2.2.1 First process FSU and former Yugoslavia --------------------------------

# Subset countries that are being downscaled in certain years using historical 
#   energy data in a specified year
# Former Soviet Union and Yugoslavia: use specified flows for each product

# The IEA data has many inter-sectoral inconsistencies between the USSR and 
#   separated countries thereafter. Results in unrealistic fuel shares.

	USSR_Yug_years   <- IEA_years[ IEA_years < 1990 ]
	X_USSR_Yug_years <- paste( "X", USSR_Yug_years, sep = "" )
	postUSSR_Yug_years   <- IEA_years[ IEA_years >= 1990 ]
	X_postUSSR_Yug_years <- paste( "X", postUSSR_Yug_years, sep = "" )
	A.USSR_Yug  <- subset( A.IEAcomp, COUNTRY %in% c( 
            "Former Soviet Union (if no detail)", 
            "Former Yugoslavia (if no detail)" ) )
	
# Re-map the forms of coal from the historical years--called "if no detail" 
#   (this is before 1978)--to the relevant coal types for matching with the
#   more recent years. Extend back to 1960.
    X_no_detail_coal_years <- paste( "X", 1960:1977, sep = "" )
    
# Hard coal needs to split proportionally between coking coal and other 
#   bituminous coal to minimize bias from different country-wise shares of the
#   two fuel types. Note that anthracite is not considered in these regions.

# This split cannot be used as an indicator of coking coal consumption. Coal 
#   coke consumption is available directly.

# NOTE: using round() to avoid NA's for any values whose base value is >1e6

    split_factor <- round( A.USSR_Yug$X1978[ A.USSR_Yug$PRODUCT == 
            "Other bituminous coal (kt)" ] /
        ( A.USSR_Yug$X1978[ A.USSR_Yug$PRODUCT == "Coking coal (kt)" ] +
          A.USSR_Yug$X1978[ A.USSR_Yug$PRODUCT == 
            "Other bituminous coal (kt)" ] + 1e-3 ), 2 )
            
    A.USSR_Yug[ A.USSR_Yug$PRODUCT == "Other bituminous coal (kt)", 
        X_no_detail_coal_years ] <- A.USSR_Yug[ A.USSR_Yug$PRODUCT == 
        "Hard coal (if no detail) (kt)", X_no_detail_coal_years ] * split_factor
	A.USSR_Yug[ A.USSR_Yug$PRODUCT == "Coking coal (kt)", 
        X_no_detail_coal_years ] <- A.USSR_Yug[ A.USSR_Yug$PRODUCT == 
        "Hard coal (if no detail) (kt)", X_no_detail_coal_years ] - 
        A.USSR_Yug[ A.USSR_Yug$PRODUCT == "Other bituminous coal (kt)",
            X_no_detail_coal_years ]
	A.USSR_Yug[ A.USSR_Yug$PRODUCT == "Hard coal (if no detail) (kt)", 
        X_no_detail_coal_years ] <- 0

# Brown coal is simpler, as lignite is the only relevant fuel (sub-bituminous 
#   coal is not considered in these regions)
	A.USSR_Yug[ A.USSR_Yug$PRODUCT == "Lignite (kt)", 
        X_no_detail_coal_years ] <- A.USSR_Yug[ A.USSR_Yug$PRODUCT == 
            "Brown coal (if no detail) (kt)", X_no_detail_coal_years ]       
	A.USSR_Yug[ A.USSR_Yug$PRODUCT == "Brown coal (if no detail) (kt)", 
        X_no_detail_coal_years ] <- 0

	A.USSR_Yug_ctry <- subset( A.IEAsingle, iso %in% 
        IEA_composite$iso[ IEA_composite$IEA_ctry %in%
            c( "Former Soviet Union (if no detail)", 
               "Former Yugoslavia (if no detail)" ) ] )
	A.USSR_Yug_ctry$IEAcomp <- IEA_composite$IEA_ctry[ 
        match( A.USSR_Yug_ctry$iso, IEA_composite$iso ) ]

# Use data from 1990 to estimate what fractions of overall FSU or Yug data 
#   belong to their sub-countries

# Data for Non-specified primary biofuels/waste disappears in 1990 when biofuels
#   start to be categorized in greater detail. Data during FSU and Yug years 
#   most likely comes from Primary solid biofuels, so use the data for that 
#   product to determine 1990 shares

# NOTE: Some combinations of products and flows go uncounted starting in 1990 
#   and their shares cannot be calculated. Note this in the meta-data

    meta_names <- c( "Data.Type", "Emission", "Region", "Sector", "Start.Year",
                               "End.Year", "Source.Comment")

    meta_note <- c( "Energy Consumption", "NA", "Former Soviet Union", "Consumption", 
        "1971", "1990", paste0( "Natural gas liquids (kt) in final consumption go uncounted ",
                          "before 1990 since shares cannot be calculated from 1990 values" ) )
                          
    addMetaData( meta_note, meta_names )
    
    meta_note <- c( "Energy Consumption", "NA", "Former Soviet Union", "Consumption", 
        "1971", "1990", paste0( "Natural gas liquids (kt) in final consumption go uncounted ",
                          "before 1990 since shares cannot be calculated from 1990 values" ) )
                          
    addMetaData( meta_note, meta_names )

# Take the combination of PRODUCT and FLOW columns, and keep only the rows that 
#   contain the combination of PRODUCT and FLOW in IEA_product_downscaling                                                    
                                                    
    A.USSR_Yug_FLOW_PROD <- aggregate( A.USSR_Yug_ctry[ "X1990" ],
		by=list( IEAcomp = A.USSR_Yug_ctry$IEAcomp, FLOW = 
        A.USSR_Yug_ctry$FLOW, PRODUCT = A.USSR_Yug_ctry$PRODUCT ), sum )
    A.USSR_Yug_FLOW_PROD <- A.USSR_Yug_FLOW_PROD[ 
        A.USSR_Yug_FLOW_PROD$X1990 != 0 | 
        A.USSR_Yug_FLOW_PROD$PRODUCT == 
            "Non-specified primary biofuels/waste (TJ-net)", ]

#
    A.USSR_Yug_ctry_FLOW_PROD <- subset( A.USSR_Yug_ctry, 
        paste( PRODUCT, FLOW ) %in% paste( A.USSR_Yug_FLOW_PROD$PRODUCT, 
            A.USSR_Yug_FLOW_PROD$FLOW ) )
                                              
# Calculate the share of each country within the larger USSR aggregate in 1990
    A.USSR_Yug_ctry_FLOW_PROD$X1990_share <- 
        A.USSR_Yug_ctry_FLOW_PROD$X1990 / A.USSR_Yug_FLOW_PROD$X1990[
            match( paste( A.USSR_Yug_ctry_FLOW_PROD$IEAcomp, 
                    A.USSR_Yug_ctry_FLOW_PROD$FLOW, 
                    A.USSR_Yug_ctry_FLOW_PROD$PRODUCT ),
			    paste( A.USSR_Yug_FLOW_PROD$IEAcomp, 
                    A.USSR_Yug_FLOW_PROD$FLOW, 
                    A.USSR_Yug_FLOW_PROD$PRODUCT ) ) ]

# Copy shares for Primary solid biofuels to Non-specified primary biofuels/waste
    spec_bio_shares <- subset( A.USSR_Yug_ctry_FLOW_PROD, 
        ( PRODUCT == "Primary solid biofuels (TJ-net)" ) & 
        ( FLOW %in% c( "DOMSUP", "FINCONS" ) ), c( iso, FLOW, PRODUCT, X1990_share ) )
    A.USSR_Yug_ctry_FLOW_PROD$X1990_share[ 
        A.USSR_Yug_ctry_FLOW_PROD$PRODUCT == 
        "Non-specified primary biofuels/waste (TJ-net)" & 
            ( A.USSR_Yug_ctry_FLOW_PROD$FLOW %in% 
            c( "DOMSUP", "FINCONS" ) ) ] <- spec_bio_shares$X1990_share

# Replace NAs with 0.
	A.USSR_Yug_ctry_FLOW_PROD[ 
        is.na( A.USSR_Yug_ctry_FLOW_PROD ) ] <- 0

# Calculate the fuel use of individual countries during USSR years = total use 
#   of composite region multiplied by the country-wise shares in 1990
	A.USSR_Yug_ctry_stat <- A.USSR_Yug_ctry[ c( "iso", "FLOW", "PRODUCT",
                                                      "IEAcomp" ) ]

	A.USSR_Yug_ctry_stat[ X_USSR_Yug_years ] <- A.USSR_Yug[ match( 
            paste( A.USSR_Yug_ctry_stat$IEAcomp, A.USSR_Yug_ctry_stat$FLOW, 
                A.USSR_Yug_ctry_stat$PRODUCT ),
            paste( A.USSR_Yug$COUNTRY, A.USSR_Yug$FLOW, 
                A.USSR_Yug$PRODUCT ) ),
            X_USSR_Yug_years ] * 
        A.USSR_Yug_ctry_FLOW_PROD$X1990_share[ match( 
            paste( A.USSR_Yug_ctry_stat$iso, A.USSR_Yug_ctry_stat$FLOW, 
                A.USSR_Yug_ctry_stat$PRODUCT ),
			paste( A.USSR_Yug_ctry_FLOW_PROD$iso, 
                A.USSR_Yug_ctry_FLOW_PROD$FLOW, 
                A.USSR_Yug_ctry_FLOW_PROD$PRODUCT ) ) ]
 
# Replace post-USSR Yugoslavia years with the original data.
	A.USSR_Yug_ctry_stat[ X_postUSSR_Yug_years ] <- 
         A.USSR_Yug_ctry[ X_postUSSR_Yug_years ]

# Replace NAs with 0.
# Make sure that rows with NAs are not needed.
	A.USSR_Yug_ctry_stat[ is.na( A.USSR_Yug_ctry_stat ) ] <- 0


# 2.2.2 Now process composite regions ------------------------------------------

# Composite regions where population is used to downscale energy to countries 
#   over all historical years
# Subset composite regions
	A.Afr  <- subset( A.IEAcomp, COUNTRY == "Other Africa" )
	A.LAM  <- subset( A.IEAcomp, COUNTRY == "Other Non-OECD Americas" )
	A.Asia <- subset( A.IEAcomp, COUNTRY == "Other Asia" )

# Repeat by number of countries in each
	A.Afr_repCtry  <- repeatAndAddVector( A.Afr, "iso", 
        IEA_composite$iso[ IEA_composite$IEA_ctry == "Other Africa" ] )
	A.LAM_repCtry  <- repeatAndAddVector( A.LAM, "iso", 
        IEA_composite$iso[ IEA_composite$IEA_ctry == 
        "Other Non-OECD Americas" ] )
	A.Asia_repCtry <- repeatAndAddVector( A.Asia, "iso", 
        IEA_composite$iso[ IEA_composite$IEA_ctry == "Other Asia" ] )

# Combine these into a single data table
	A.Others_repCtry <- rbind( A.Afr_repCtry, A.LAM_repCtry, 
        A.Asia_repCtry )

# Calculate population shares
	A.UN_pop_master <- subset( UN_pop_master, year %in% IEA_years &
        scenario == "Estimates" )
	A.Others_pop <- subset( A.UN_pop_master, iso %in% A.Others_repCtry$iso )
	A.Others_pop$IEAcomp <- IEA_composite$IEA_ctry[ 
        match( A.Others_pop$iso, IEA_composite$iso ) ]

# Aggregate by country-in-composite-region and year to find population shares
	A.Composites_pop <- aggregate( A.Others_pop[ "pop" ], by = list( 
        IEAcomp = A.Others_pop$IEAcomp, year = A.Others_pop$year ), sum )
	A.Others_pop$share <- A.Others_pop$pop / A.Composites_pop$pop[
		match( paste( A.Others_pop$IEAcomp, A.Others_pop$year ),
			paste( A.Composites_pop$IEAcomp, A.Composites_pop$year ) ) ]
	A.Others_pop$Xyear <- paste( "X", A.Others_pop$year, sep = "" )
	A.Others_pop.reshape <- cast( A.Others_pop, iso + IEAcomp ~ Xyear, 
	                              value = "share" )

# Multiply the repeated country databases by the population shares to get the
#   energy statistics by country
	A.Others_ctry_stat <- data.frame( A.Others_repCtry[ c( "iso", "FLOW",
        "PRODUCT" ) ], IEAcomp = A.Others_repCtry$COUNTRY )
	A.Others_ctry_stat[ X_IEA_years ] <- A.Others_repCtry[ 
        X_IEA_years ] * A.Others_pop.reshape[ match( 
            A.Others_ctry_stat$iso, A.Others_pop.reshape$iso ), 
        X_IEA_years ]

# 2.2.3 Now combine data into one database

# Subset final energy statistics to only the rows that aren't zero in all years
	A.IEAsingle <- A.IEAsingle[ rowSums( A.IEAsingle[ 
        X_IEA_years ] ) != 0, ]
	A.IEAsingle_noUSSR_Yug <- subset( A.IEAsingle, iso %!in% 
        A.USSR_Yug_ctry_stat$iso )
	A.USSR_Yug_ctry_stat <- A.USSR_Yug_ctry_stat[ rowSums( 
        A.USSR_Yug_ctry_stat[ X_IEA_years ] ) != 0, ]
	A.Others_ctry_stat <- A.Others_ctry_stat[ rowSums( 
        A.Others_ctry_stat[ X_IEA_years ] ) != 0, ]

# Combine the country-level data tables and write out energy balances
# Use iso codes rather than IEA's country names
	IEA_isoID <- c( "iso", "FLOW", "PRODUCT" )
	A.IEA_en_stat_ctry_hist <- rbind( A.IEAsingle_noUSSR_Yug[ c( IEA_isoID, 
        X_IEA_years ) ], A.USSR_Yug_ctry_stat[ c( IEA_isoID, 
        X_IEA_years ) ], A.Others_ctry_stat[ c( IEA_isoID, 
        X_IEA_years ) ] )

# -----------------------------------------------------------------------------
# 3. Output Final IEA Energy Data

# Add comments for each table
	comments.A.IEA_en_stat_ctry_hist <- c( paste0( "IEA energy statistics ",
        "downscaled to 202 countries (iso / FLOW / PRODUCT / historical year)" ),
        "Units = kt, TJ or GWh" )

# Write tables as CSV files 
	writeData( A.IEA_en_stat_ctry_hist, "MED_OUT", "A.IEA_en_stat_ctry_hist", 
        comments = comments.A.IEA_en_stat_ctry_hist )


# -----------------------------------------------------------------------------	
# 4. Check sums of country data against world data for all products
# Last tested with IEA data published in 2010, for years 1985 and 2005 and for 
#   Primary Supply and Final Consumption flows
    comparison_years <- c( "X1985", "X2005" )
    comparison_flows <- c( "FINCONS", "DOMSUP" )

# 0% error found for all products in 2005, some error found for few secondary 
#   products in 1985. This error is due to the use of some products going to 
#   zero in 1990. This means shares cannot be calculated and overall FSU or Yug
#   data cannot be distributed by country with the current method.

# Pull lists of all products and flows from the data
# Pick 2 years to compare, one before and one after FSU split eg 1985 and 2005
	prod_list <- subset( NonOECD_E_Stat, FLOW == "DOMSUP" & 
        COUNTRY == "World", PRODUCT )
	prod_num <- length( t( prod_list ) )
	flow_list <- subset( NonOECD_E_Stat, ( FLOW == "FINCONS" | 
        FLOW == "DOMSUP" ) & COUNTRY == "World", FLOW )

# Form array with columns for data and errors
	A.IEA_sum_comp <- data.frame( array( NA, dim = c( 2 * prod_num, 8 ) ) )
	names( A.IEA_sum_comp ) <- c( "CTRY_SUM_1985", "WORLD_1985", "DIFF_1985",
        "ERROR(%)_1985", "CTRY_SUM_2005", "WORLD_2005", "DIFF_2005", 
        "ERROR(%)_2005" )

# Extract country data and world data for Primary Supply (FLOW = DOMSUP) and
#   Total Final Consumption (FLOW = FINCONS)
# The sum of the countries doesn't include World marine or aviation bunkers, 
#   so compare country-sum to OECD_tot + nonOECD_tot
	ctry_data <- subset( A.IEA_en_stat_ctry_hist, FLOW %in% comparison_flows,
        c("FLOW", "PRODUCT", comparison_years) )
	OECD_tot <- subset( NonOECD_E_Stat, COUNTRY == "OECD Total" & FLOW %in%
        comparison_flows, comparison_years )
	nonOECD_tot <- subset( NonOECD_E_Stat, COUNTRY == "Memo: Non-OECD Total" &
        FLOW %in% comparison_flows, comparison_years )
	world_tot <- OECD_tot + nonOECD_tot

# Assign world data to their respective columns
	A.IEA_sum_comp$WORLD_1985 <- c( unlist( world_tot )[1:prod_num],
        unlist( world_tot )[ ( prod_num+1 ):( 2 * prod_num ) ] ) 
	A.IEA_sum_comp$WORLD_2005 <- c( 
        unlist( world_tot )[ ( 2 * prod_num + 1 ):( 3 * prod_num ) ], 
        unlist( world_tot )[ ( 3 * prod_num + 1 ):( 4 * prod_num ) ] )

# Sum the data for each product across all considered years and assign 
#   country-sum data to columns
	for ( i in 1:prod_num ) { 
	ctry_sum_sup <- subset( ctry_data, PRODUCT == paste( prod_list[ i, ] ) &
        FLOW == "DOMSUP", comparison_years )
	ctry_sum_fin <- subset( ctry_data, PRODUCT == paste( prod_list[ i, ] ) &
        FLOW == "FINCONS", comparison_years )
	A.IEA_sum_comp$CTRY_SUM_1985[ i ] <- colSums( ctry_sum_sup )[ 1 ] 
	A.IEA_sum_comp$CTRY_SUM_1985[ ( prod_num + i ) ] <- colSums( 
        ctry_sum_fin )[ 1 ]
	A.IEA_sum_comp$CTRY_SUM_2005[ i ] <- colSums( ctry_sum_sup )[ 2 ] 
	A.IEA_sum_comp$CTRY_SUM_2005[ ( prod_num + i ) ] <- colSums(
        ctry_sum_fin )[ 2 ] 
    }

#Calculate differences and percent errors of country-sum from world data
	A.IEA_sum_comp$DIFF_1985 <- A.IEA_sum_comp$CTRY_SUM_1985 -
        A.IEA_sum_comp$WORLD_1985
    A.IEA_sum_comp$DIFF_2005 <- A.IEA_sum_comp$CTRY_SUM_2005 -
        A.IEA_sum_comp$WORLD_2005
	A.IEA_sum_comp[, "ERROR(%)_1985"] <- abs( A.IEA_sum_comp$DIFF_1985 ) /
        A.IEA_sum_comp$WORLD_1985 * 100
	A.IEA_sum_comp[, "ERROR(%)_2005"] <- abs( A.IEA_sum_comp$DIFF_2005 ) /
        A.IEA_sum_comp$WORLD_2005 * 100

# NaN values occur where world data is zero, clear NaNs for clarity
# Add labels for flow and product
	A.IEA_sum_comp2 <- rapply( A.IEA_sum_comp, f=function(x) 
        ifelse( is.nan(x), 0, x ), how="replace" )
	A.IEA_sum_comp <- cbind( flow_list, rbind( prod_list, prod_list ),
        A.IEA_sum_comp2 )

# Add comments for the data and write out as a CSV
	comments.A.IEA_sum_comp <- c( "Comparison: Sums of Country Data vs. 
        OECD+NonOECD Totals", "Flows: Primary Supply and Final Consumption", 
        "Error: percent deviation of Country-Sum from OECD+NonOECD total" )
	writeData( A.IEA_sum_comp, domain = "DIAG_OUT", fn = "A.IEA_sum_comp",
        comments = comments.A.IEA_sum_comp, meta = F )

	
	# Every script should finish with this line
	logStop()
	