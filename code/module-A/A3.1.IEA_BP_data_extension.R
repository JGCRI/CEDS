#------------------------------------------------------------------------------
# Program Name: A3.1.IEA_BP_data_extension.R
# Authors Names: Tyler Pitkanen, Rachel Hoesly, Linh Vu
# Date Last Modified: 26 December 2015
# Program Purpose: Reads in BP data for years not yet covered by IEA data
#                  Alters BP data to agree with IEA data labels
#                  Adds recent BP-projected data to historical years data
# Input Files: A.comb_othertrans_activity.csv, BP_energy_data.xlsx,
#              Master_Country_List.csv, Master_Fuel_Sector_List.xlsx,
# Output Files: A.IEA_BP_sum_comparison.csv, A.IEA_BP_trend_comparison.csv,
#               A.IEA_BP_energy_ext.csv
# Notes: IEA_years, BP_years, end_year and X_ variants defined in common_data.R
# TODO: Clean up formatting and section breaks, add subsections
#       Maybe expand on comments
#       Use removeBlanks() system function for data cleanup?
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "analysis_functions.R", "process_db_functions.R" ) # Additional function files required.
    log_msg <- "Projection of IEA data from more recent BP statistics" # First message to be printed to the log
    script_name <- "A3.1.IEA_BP_data_extension.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Read in files
    ##CR: Remove .csv from filename
    iea_data_full <- readData( "MED_OUT", "A.comb_othertrans_activity" )
    bp_energy_data <- readData( "ENERGY_IN","BP_energy_data", ".xlsx")
    ctry_mapping <- readData( "MAPPINGS", "Master_Country_List" )
    fuel_list <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Fuels" )

    # Read in BP energy data and print out sheet name as a diagnostic
    bp_oil_full  <- readData( "ENERGY_IN","BP_energy_data", ".xlsx", sheet_selection = 7 ) # Oil Consumption- Tonnes
    printLog( c("Read in BP data sheet: ", names( bp_oil_full )[[1]]) )
    bp_gas_full  <- readData( "ENERGY_IN","BP_energy_data", ".xlsx", sheet_selection = 24 ) # Gas Consumption – tonnes
    printLog( c("Read in BP data sheet: ", names( bp_gas_full )[[1]]) )
    bp_coal_full <- readData( "ENERGY_IN","BP_energy_data", ".xlsx", sheet_selection = 33 ) # Coal Consumption -  Mtoe
    printLog( c("Read in BP data sheet: ", names( bp_coal_full )[[1]]) )

# Check that correct sheets were read
    oil_sheet_name <- names( bp_oil_full )[[1]]

# Check input data for proper sector and fuel names
    sectorCheck( iea_data_full )
    fuelCheck( iea_data_full )

# -----------------------------------------------------------------------------------------
# 2. Define some useful functions

# Adds specified columns from table2 to table1, matching by a column shared
#   by the two tables
    addCols <- function( table1, table2, cols, matchcol ) {
        x <- table2[ match( table1[[matchcol]], table2[[matchcol]] ), cols ]
        extendForwardedtable <- cbind( table1, x )
        names( extendForwardedtable ) <- c( names(table1), cols )
        return( extendForwardedtable )
    }

# Replace NaN with 1. For calculating ratios and trends.
    nanratio <- function( x ) {
        list_nans <- apply( x, 2, is.nan )
        mod <- x
        mod[ list_nans ] <- 1
        return( mod )
    }

# Same as is.nan() function but with list functionality
    is.nan.data.frame <- function( x ) {
        do.call( cbind, lapply( x, is.nan ) )
    }

# ------------------------------------------------------------------------------
# 3. Match BP data to IEA data
# Re-format the data to allow for easier manipulation. Also account for
#   differences in naming, resolution, etc.
# ------------------------------------------------------------------------------

    printLog( "Matching BP and IEA data" )

# Input the years of interest
    ext_years <- c( IEA_end_year, BP_years ) # Years used to extendForward data
    X_ext_years <- paste0( "X", ext_years )

# Account for differences in naming and regional associations. Also remove
#   blank rows, titles, etc. in BP data
    bp_data_full <- list( bp_oil_full, bp_gas_full, bp_coal_full )
    names( bp_data_full ) <- c( "bp_oil", "bp_gas", "bp_coal" )

        # CHECK WHETHER THESE ARE STILL TREATED AS ENTRIES IN A LIST- ERRORS IN 3.2
    bp_data_clean <- list()
    bp_data <- list()

    for ( i in 1:length( bp_data_full ) ) {
    # Remove rows without relevant data
        bp_data_clean[[i]] <- bp_data_full[[i]][ bp_data_full[[i]][, 1] %in%
                                                 ctry_mapping$BPName, ]

    # Fix names of columns; they get messed up because BP has useless rows
    #   at the top of their files and R makes the top row the name of the column
        names( bp_data_clean[[i]] ) <- c( "BPName", paste0( "X",
            bp_data_full[[i]][ 2, 2:ncol(bp_data_full[[i]]) ] ) )

    # Take only the years used for calcs and the column with BP ctry names
        bp_data[[i]] <- subset( bp_data_clean[[i]], T,
                                c( "BPName", X_ext_years ) )

    # Account for BP notation. The symbol "-" means 0 Mtoe, "^" means <0.05 Mtoe
    # NOTE: Actual values for countries with "^" not given for coal consumption
    #   only. Not sure why this is. Assume values of "^" = 0.05 for these
    #   instances
        bp_data[[i]][ bp_data[[i]] == "-" ] <- 0
        bp_data[[i]][ bp_data[[i]] == "^" ] <- 0.05
    }

    names( bp_data ) <- c( "bp_oil", "bp_gas", "bp_coal" )

# Reduce IEA data to only what's needed to calculate projections
    iea_data <- iea_data_full[ iea_data_full[, 1] %in% ctry_mapping$iso, ]
    iea_data <- iea_data[, c( "iso", "sector", "fuel", "units", X_IEA_end_year ) ]
    names( iea_data ) <- c( "iso", "sector", "fuel", "units", "end_year" )

    printLog( "Reformatting combined data" )

# Aggregate IEA over fuel -> oil, gas, coal, biomass, other
        oil_fuels <- c( fuel_list[fuel_list$aggregated_fuel == "oil", "fuel"], 'oil' )
        gas_fuels <- fuel_list[fuel_list$aggregated_fuel == "gas", "fuel"]
       coal_fuels <- c( fuel_list[fuel_list$aggregated_fuel == "coal", "fuel"], 'coal' )
    biomass_fuels <- fuel_list[fuel_list$aggregated_fuel == "biomass", "fuel"]
      other_fuels <- subset( unique( iea_data$fuel ), unique( iea_data$fuel )
          %!in% c( oil_fuels, gas_fuels, coal_fuels, biomass_fuels ) )

# IEA data is more detailed than BP data, so aggregate IEA data to BP regions
#   We will calculate fuel use trends for these regions and apply them to
#   IEA's individual countries.

# Combine needed IEA and BP data, matching by country
    iea_data_bpname <- addCols( iea_data, ctry_mapping, "BPName", "iso" )
    bp_drivers   <- list( oil_fuels, gas_fuels, coal_fuels )
    iea_data_agg <- list()
    iea_bp_data  <- list()

    for ( i in 1:length( bp_data_full ) ) {
    # Aggregate over different IEA fuel types of the same BP category (eg sum
    #   over coal, soft coal, and hard coal in the IEA data, and match that
    #   sum to the general coal category in BP data)
        iea_data_agg[[i]] <- subset( iea_data_bpname, iea_data_bpname$fuel %in%
            bp_drivers[[i]], c( "iso", "sector", "fuel", "end_year", "BPName" ) )
    # Add the corresponding BP data. Match by region to eventually calculate
    #   emissions factors for each region
        iea_bp_data[[i]] <- addCols( iea_data_agg[[i]], bp_data[[i]],
                                     X_ext_years, "BPName" )
    }

# ------------------------------------------------------------------------------
# 4. Extend IEA data to latest years in BP data
# Estimate IEA data for years it is not yet available. Do this by calculating
#   the ratio of IEA data to BP data for the latest year IEA data is available
#   and then multiplying this ratio by BP data for the more recent years
#
# In mathematical notation:
#   K(f,s) = IEA(f,s,end_year) / BP(f,end_year)
#   IEA(f,s,BP_years) = K(f,s) * BP(f,BP_years)
#       where K is the set of ratios, f is BP fuels (oil, gas, coal),
#       and s is sectors

    printLog( "Extrapolating IEA data from BP trends" )

# Oil, gas, and coal ratios for BP regions
    iea_proj_data <- list()
    extendForwarded_iea <- list()
    n_BP_years <- length( BP_years )
    for ( i in 1:length( bp_data_full ) ) {
        iea_bp_data[[i]]$ratio <- iea_bp_data[[i]]$end_year / as.numeric(
            iea_bp_data[[i]][, X_IEA_end_year ] )
    # Assume constant where non-existent ratios are formed (by dividing by zero)
        iea_bp_data[[i]][ !is.finite( iea_bp_data[[i]]$ratio ), "ratio" ] <- 1
        iea_proj_data[[i]] <- iea_bp_data[[i]]
    # Fiddle with the data to make the list numeric
        numeric_bp_year_data <- as.numeric( unlist(
            iea_proj_data[[i]][, X_BP_years ] ) )
        array_bp_year_data <- array( numeric_bp_year_data,
            c( length( numeric_bp_year_data ) / n_BP_years, n_BP_years ) )
    # Make the projections of IEA data
        iea_proj_data[[i]][, X_BP_years ] <- iea_bp_data[[i]]$ratio *
            array_bp_year_data
        iea_proj_data[[i]] <- iea_proj_data[[i]][,
            c( "iso", "sector", "fuel", X_BP_years ) ]
        extendForwarded_iea[[i]] <- merge( iea_data_full[ iea_data_full$fuel %in%
            bp_drivers[[i]], ], iea_proj_data[[i]],
            by = c( "iso", "sector", "fuel" ) )
    }

# Biomass usage is projected by population changes (maybe)
# Don't use this yet; assume biomass is constant. Might use later.
#    un_pop_tot$ratio <- un_pop_tot$BP_years_pop_value / un_pop_tot$end_year_pop_value
#    un_pop_BP_years <- split( un_pop_tot, un_pop_tot$BP_years )
#    iea_data_biomass <- iea_data_full[ iea_data_full$fuel %in% biomass_fuels ]
#    for ( i in length( BP_years ) ) {
#        iea_data_biomass <- addCols( iea_data_biomass, un_pop_BP_years[[i]], "ratio", "iso" )
#
#    }

# Assume biomass is constant for now
    biomass_iea_data <- iea_data_full[ iea_data_full$fuel %in% biomass_fuels, ]
    biomass_fuel_ratios <- array( 1, c( nrow( iea_data_full[
        iea_data_full$fuel %in% biomass_fuels, ] ), 1 ) ) %*%
            array( 1, c( 1, length( BP_years ) ) )
    biomass_iea_proj <- biomass_iea_data[, X_IEA_end_year ] * biomass_fuel_ratios
    extendForwarded_iea_biomass <- cbind( biomass_iea_data, biomass_iea_proj )
    names( extendForwarded_iea_biomass ) <- names( extendForwarded_iea[[1]] )

# Other fuels are assumed constant. Form column of ones
    other_iea_data <- iea_data_full[ iea_data_full$fuel %in% other_fuels, ]
    other_fuel_ratios <- array( 1, c( nrow( iea_data_full[
        iea_data_full$fuel %in%
        other_fuels, ] ), 1 ) ) %*% array( 1, c( 1, length( BP_years ) ) )
    other_iea_proj <- other_iea_data[, X_IEA_end_year ] * other_fuel_ratios
    extendForwarded_iea_other <- cbind( other_iea_data, other_iea_proj )
    names( extendForwarded_iea_other ) <- names( extendForwarded_iea[[1]] )

    IEA_BP_ext <- rbind( extendForwarded_iea[[1]], extendForwarded_iea[[2]],
        extendForwarded_iea[[3]], extendForwarded_iea_biomass, extendForwarded_iea_other )
    IEA_BP_ext <- merge( iea_data_full[, c( "iso", "sector", "fuel" ) ],
        IEA_BP_ext, by = c( "iso", "sector", "fuel" ) )

# ------------------------------------------------------------------------------
# 5. Aggregate Data by fuel

# Aggregate Activity data over fuels
 printLog( "Aggregate Energy as Activity Data for energy production sectors" )

 IEA_BP_ext<-aggregate ( IEA_BP_ext[
   X_emissions_years],
   by = list( iso = IEA_BP_ext$iso,
              sector = IEA_BP_ext$sector,
              fuel = IEA_BP_ext$fuel,
              units= IEA_BP_ext$units), sum )

# ------------------------------------------------------------------------------
# 6. Trend Check
# Compare trends of projected IEA emissions for oil, gas, and coal to the trends
#   of BP emissions for oil, gas, and coal
    locatenan <- function( set ) {
        dim( set ) - dim( set[complete.cases( set ), ] )
    }

# Make coal data numeric. It's read in as char for some reason
    for ( i in 1:length(ext_years) ) {
        bp_data[[3]] <- subset( bp_data[[3]], T, c( "BPName", X_ext_years ) )
        bp_data[[3]][, i+1] <- as.numeric( bp_data[[3]][, i+1] )
    }

    iea_trends <- list()
    bp_trends  <- list()
    iea_bp_trends <- list()
    iea_trends_bpname <- list()
    trend_error <- list()

    for ( i in 1:length( bp_data_full ) ) {
    # Find IEA global trends
        iea_trends[[i]] <- subset( IEA_BP_ext, IEA_BP_ext$fuel %in%
            bp_drivers[[i]], c( "iso", "sector", "fuel", X_ext_years ) )
        iea_trends[[i]] <- aggregate( iea_trends[[i]][, X_ext_years ], by =
            list( iea_trends[[i]]$iso ), sum )
        iea_trends[[i]][, X_ext_years ] <- iea_trends[[i]][, X_ext_years ] /
            iea_trends[[i]][, X_IEA_end_year ]
        #iea_trends[[i]][ is.nan.data.frame( iea_trends[[i]] ) ] <- 1
        names( iea_trends[[i]] ) <- c( "iso", X_ext_years )
    # Find BP global trends
        bp_trends[[i]] <- subset( bp_data[[i]], T, c( "BPName", X_ext_years ) )
        bp_trends[[i]][, X_ext_years ] <- as.numeric( unlist(
            bp_trends[[i]][, X_ext_years ] ) ) /
                as.numeric( bp_trends[[i]][, X_IEA_end_year ] )
       # bp_trends[[i]][ is.nan.data.frame( bp_trends[[i]] ) ] <- 1
        names( bp_trends[[i]] ) <- c( "BPName", paste0( "BP", ext_years ) )

    # Match IEA and BP trends by country
        iea_trends_bpname[[i]] <- addCols( iea_trends[[i]], ctry_mapping,
            "BPName", "iso" )
        iea_bp_trends[[i]] <- addCols( iea_trends_bpname[[i]], bp_trends[[i]],
            paste0( "BP", ext_years ), "BPName" )
        iea_bp_trends[[i]][ is.nan.data.frame( iea_bp_trends[[i]] ) ] <- 0
        iea_bp_trends[[i]][ iea_bp_trends[[i]][, X_IEA_end_year ] == 0 &
            iea_bp_trends[[i]][, paste0( "BP", end_year ) ] == 0,
                c( X_ext_years, paste0( "BP", ext_years ) ) ] <- 1
    # Account for division by zero errors (Inf and NaN terms)
        iea_bp_trends[[i]][ iea_bp_trends[[i]][, X_IEA_end_year ] == 0 &
            iea_bp_trends[[i]][, paste0( "BP", end_year ) ] != 0,
            X_ext_years ] <-
                iea_bp_trends[[i]][ iea_bp_trends[[i]][, X_IEA_end_year ] == 0 &
                iea_bp_trends[[i]][, paste0( "BP", end_year ) ] != 0,
                paste0( "BP", ext_years ) ]
        iea_bp_trends[[i]] <- filter( iea_bp_trends[[i]], !is.na( iea_bp_trends[[i]][, paste0( "BP", end_year ) ] ))  # TODO: is this what we wanted?
        iea_bp_trends[[i]][ iea_bp_trends[[i]][, X_IEA_end_year ] != 0 &  # ERROR
            iea_bp_trends[[i]][, paste0( "BP", end_year ) ] == 0,
            paste0( "BP", ext_years ) ] <-
                iea_bp_trends[[i]][ iea_bp_trends[[i]][, X_IEA_end_year ] != 0 &
                iea_bp_trends[[i]][, paste0( "BP", IEA_end_year ) ] == 0,
                X_ext_years ]

    # Calculate percent error of IEA projected trends from BP trends
        bp_trends[[i]] <- subset( iea_bp_trends[[i]], T,
                                  paste0( "BP", ext_years ) )
        iea_trends[[i]] <- subset( iea_bp_trends[[i]], T, X_ext_years )
        trend_error[[i]] <- ( bp_trends[[i]] - iea_trends[[i]] ) /
            bp_trends[[i]] * 100
        trend_error[[i]][ abs( trend_error[[i]] ) < 1e-10 ] <- 0
        trend_error[[i]][ is.nan.data.frame( trend_error[[i]] ) ] <- 0
        names( trend_error[[i]] ) <- paste0( "%error", ext_years )

        iea_bp_trends[[i]] <- cbind( iea_bp_trends[[i]], trend_error[[i]] )
    }

    iea_bp_trend_comparison <- rbind( iea_bp_trends[[1]], iea_bp_trends[[2]],
                                      iea_bp_trends[[3]] )


# ------------------------------------------------------------------------------
# 7. Sum check
# Compare world sums of extendForwarded IEA data and BP data
# Take sums of coal, gas, and oil fuels from extendForwarded BP data set

# Set up categories to aggregate over fuels and sectors
    all_years <- c( IEA_end_year, BP_years )
    X_all_years <- paste0( 'X', all_years )

    secs <- unique( IEA_BP_ext$sector )
    prod_secs  <- grep( 'production', secs, value = T )

    fuel <- unique( IEA_BP_ext$fuel )
    coal_fuel <- grep( 'coal', fuel, value = T )
    gas_fuel  <- grep( 'gas',  fuel, value = T )
    oil_fuel  <- grep( 'oil',  fuel, value = T )

# Take production, consumption, and transfer sums
    iea_coal_prod <- subset( IEA_BP_ext, IEA_BP_ext$fuel %in% coal_fuel &
        IEA_BP_ext$sector %in% prod_secs, X_all_years )
    iea_oil_prod  <- subset( IEA_BP_ext, IEA_BP_ext$fuel %in% oil_fuel  &
        IEA_BP_ext$sector %in% prod_secs, X_all_years )
    iea_gas_prod  <- subset( IEA_BP_ext, IEA_BP_ext$fuel %in% gas_fuel  &
        IEA_BP_ext$sector %in% prod_secs, X_all_years )

# Todo: replace coefficients with generalized versions
    iea_coal_prod_tot <- colSums( iea_coal_prod ) * 0.9934
    iea_oil_prod_tot  <- colSums( iea_oil_prod  ) * 0.5203  # Placeholder co-
        # efficient to correct for the fraction of oil sectors considered by
        # BP (bitumen, crude oil, tight oil, NGLs). Coefficient found by dividing
        # the sum of IEA oil products considered by BP by sum of all IEA oil prods
    iea_gas_prod_tot  <- colSums( iea_gas_prod  ) * 0.8947 # Same for gas

# Select relevant sheets from BP raw data

    sheets <- tolower( names( bp_energy_data ) )
    bp_coal_prod_sheet <- grep( "ton", grep( "prod", grep( "coal", sheets,
        value = T ), value = T ), value = T )  # 33
    bp_oil_prod_sheet  <- grep( "ton", grep( "prod", grep( "oil",  sheets,
        value = T ), value = T ), value = T )  # 7
    bp_gas_prod_sheet  <- grep( "ton", grep( "prod", grep( "gas",  sheets,
        value = T ), value = T ), value = T )  # 24, note: actually Mtoe not Mt
    bp_coal_prod_num <- grep( bp_coal_prod_sheet, sheets )
    bp_oil_prod_num  <- grep( bp_oil_prod_sheet,  sheets )
    bp_gas_prod_num  <- grep( bp_gas_prod_sheet,  sheets )
    bp_coal_prod <- bp_energy_data[[bp_coal_prod_num]]
    bp_oil_prod  <- bp_energy_data[[bp_oil_prod_num]]
    bp_gas_prod  <- bp_energy_data[[bp_gas_prod_num]]

# Select world total data from the data sheets
    bp_coal_prod_tot <- na.omit( bp_coal_prod[ bp_coal_prod[ ,1] ==
        "Total World", bp_coal_prod[2, ] %in% all_years ] )
    bp_oil_prod_tot  <- na.omit( bp_oil_prod[  bp_oil_prod[ ,1]  ==
        "Total World",  bp_oil_prod[2, ]  %in% all_years ] )
    bp_gas_prod_tot  <- na.omit( bp_gas_prod[  bp_gas_prod[ ,1]  ==
        "Total World",  bp_gas_prod[2, ]  %in% all_years ] )
    bp_coal_prod_tot <- bp_coal_prod_tot[ 1:length( all_years ) ]
    bp_oil_prod_tot  <- bp_oil_prod_tot[  1:length( all_years ) ]
    bp_gas_prod_tot  <- bp_gas_prod_tot[  1:length( all_years ) ]

# Convert units from millions of tonnes to thousands of tonnes for coal and oil, Mt to
#   TJ for natural gas
    bp_coal_prod_tot <- as.numeric( bp_coal_prod_tot ) * 1000
    bp_oil_prod_tot  <- as.numeric( bp_oil_prod_tot )  * 1000
    bp_gas_prod_tot  <- as.numeric( bp_gas_prod_tot )  * 1000 * 41.868

    coal_totals <- cbind( c( 'coal', 'coal' ), rbind( iea_coal_prod_tot,
        bp_coal_prod_tot ) )
    oil_totals  <- cbind( c( 'oil', 'oil' ), rbind( iea_oil_prod_tot,
        bp_oil_prod_tot ) )
    gas_totals  <- cbind( c( 'gas', 'gas' ), rbind( iea_gas_prod_tot,
        bp_gas_prod_tot ) )

# Fix naming
    col_names <- c( 'fuel', X_all_years )
    row_labels <- rbind( 'iea_prod', 'bp_prod' )
    colnames( coal_totals ) <- colnames( oil_totals ) <- colnames( gas_totals ) <- col_names
    IEA_BP_sum_comp <- rbind( coal_totals, oil_totals, gas_totals )
    IEA_BP_sum_comp <- cbind( rbind(row_labels, row_labels, row_labels), IEA_BP_sum_comp )


# -----------------------------------------------------------------------------
# 8. Output
# Add comments for each table
  comments.A.energy_data_extension <- c( paste0( "IEA energy statistics",
                                               " by intermediate sector / intermediate fuel / historical year,",
                                               " extendForwarded with BP energy statistics for latest BP years" ) )

# write extended energy data
  writeData( IEA_BP_ext, domain = "MED_OUT", fn = "A.IEA_BP_energy_ext",
           comments = comments.A.energy_data_extension )

# Write out Diagnostic Output
  writeData( iea_bp_trend_comparison, domain = "DIAG_OUT", fn =
               "A.IEA_BP_trend_comparison", comments = NULL, meta = F )

  writeData( IEA_BP_sum_comp, domain = "DIAG_OUT", fn =
               "A.IEA_BP_sum_comparison", comments = NULL, meta = F )

# Every script should finish with this line
 logStop()

# END
