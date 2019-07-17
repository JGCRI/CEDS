#------------------------------------------------------------------------------
# Program Name: A3.1.IEA_BP_data_extension_PRE.R
# Authors Names: Tyler Pitkanen, Rachel Hoesly, Linh Vu
# Date Last Modified: 26 December 2015
# Program Purpose: Reads in BP data for years not yet covered by IEA data
#                  Alters BP data to agree with IEA data labels
#                  Adds historical BP-projected data to historical IEA years data
# Input Files: A.IEA_BP_energy_ext.csv, BP_energy_data.xlsx,
#              Master_Country_List.csv, Master_Fuel_Sector_List.xlsx,
# Output Files: A.comb_activity.csv, A.IEA_BP_sum_comparison.csv, A.IEA_BP_trend_comparison.csv
# Notes: IEA_years, BP_years, start_year and X_ variants defined in common_data.R
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
    log_msg <- "Fill in IEA data from 1960s with BP statistics" # First message to be printed to the log
    script_name <- "A3.1.IEA_BP_data_extension_PRE.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Read in files

    iea_data_full <- readData( "MED_OUT", "A.IEA_BP_energy_ext" )
    iea_data_before_BP_PRE <- iea_data_full
    bp_energy_data <- readData( "ENERGY_IN", BP_data_file_name, ".xlsx")
    ctry_mapping <- readData( "MAPPINGS", "Master_Country_List" )
    fuel_list <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Fuels" )

    # Read in BP energy data
    printLog( c("Reading in BP energy consumption data."))
    bp_oil_full <- bp_energy_data[[ getBPSheetNumber( "oil", "consumption", "tonnes", bp_energy_data ) ]]
    bp_gas_full <- bp_energy_data[[ getBPSheetNumber( "gas", "consumption", "mtoe", bp_energy_data ) ]]
    bp_coal_full <- bp_energy_data[[ getBPSheetNumber( "coal", "consumption", "mtoe", bp_energy_data ) ]]

# Check input data for proper sector and fuel names
    sectorCheck( iea_data_full )  ### TODO: Currently shows all sectors are mislabeled. Is this a problem? The script doesn't do anyhting with this info
    fuelCheck( iea_data_full )

# -----------------------------------------------------------------------------------------
# 2. Define some useful functions

# Adds specified columns from table2 to table1, matching by a column shared
#   by the two tables
    addCols <- function( table1, table2, cols, matchcol ) {
        x <- table2[ match( table1[[matchcol]], table2[[matchcol]] ), cols ]
        extendBackwardedtable <- cbind( table1, x )
        names( extendBackwardedtable ) <- c( names(table1), cols )
        return( extendBackwardedtable )
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
#    Re-format the data to allow for easier manipulation. Also account for
#    differences in naming, resolution, etc.

    printLog( "Matching BP and IEA data" )

# Define useful year variables
    BP_years <- c( 1965:1970 )
    X_BP_years <- paste0( "X", BP_years )
    IEA_start_year <- 1971
    X_IEA_start_year <- 'X1971'

# Separate rows which need extending
    iea_data_full_all <- iea_data_full
    iea_data_full_not_extended <-  iea_data_full[ -which( iea_data_full$X1960==0 & iea_data_full$X1970==0 ), ]
    iea_data_full <-  iea_data_full[ which( iea_data_full$X1960==0 & iea_data_full$X1970==0 ), ]
    iea_data_full_original <- iea_data_full
    iea_data_full <- iea_data_full[ , c( "iso", "sector", "fuel", "units", paste0( "X", IEA_start_year:end_year ) ) ]

# Variable with the years of interest
    ext_years <- c( BP_years, IEA_start_year ) # Years used to extendBackward data
    X_ext_years <- paste0( "X", ext_years )

# Clean-up BP data
    bp_data_full <- list( bp_oil_full, bp_gas_full, bp_coal_full )
    names( bp_data_full ) <- c( "bp_oil", "bp_gas", "bp_coal" )

    bp_data <- list()
    for ( i in 1:length( bp_data_full ) ) {
      bp_data[[i]] <- cleanBPDataSheet( bp_data_full[[i]], X_ext_years, ctry_mapping )
    }
    names( bp_data ) <- c( "bp_oil", "bp_gas", "bp_coal" )

# Reduce IEA data to only what's needed to calculate projections
    iea_data <- iea_data_full[ iea_data_full[, 1] %in% ctry_mapping$iso, ]
    iea_data <- iea_data[, c( "iso", "sector", "fuel", "units", X_IEA_start_year ) ]
    names( iea_data ) <- c( "iso", "sector", "fuel", "units", "start_year" )

    printLog( "Reformatting combined data" )

# Aggregate IEA over fuel -> oil, gas, coal, biomass, other
        oil_fuels <- fuel_list[ fuel_list$aggregated_fuel == "oil", "fuel" ]
        gas_fuels <- fuel_list[ fuel_list$aggregated_fuel == "gas", "fuel" ]
       coal_fuels <- fuel_list[ fuel_list$aggregated_fuel == "coal", "fuel" ]
    biomass_fuels <- fuel_list[ fuel_list$aggregated_fuel == "biomass", "fuel" ]
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
            bp_drivers[[i]], c( "iso", "sector", "fuel", "start_year", "BPName" ) )
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
#   K(f,s) = IEA(f,s,start_year) / BP(f,start_year)
#   IEA(f,s,BP_years) = K(f,s) * BP(f,BP_years)
#       where K is the set of ratios, f is BP fuels (oil, gas, coal),
#       and s is sectors

    printLog( "Extrapolating IEA data from BP trends" )

# Oil, gas, and coal ratios for BP regions
    iea_proj_data <- list()
    extendBackwarded_iea <- list()
    n_BP_years <- length( BP_years )
    for ( i in 1:length( bp_data_full ) ) {
        iea_bp_data[[i]]$ratio <- iea_bp_data[[i]]$start_year / as.numeric(
            iea_bp_data[[i]][, X_IEA_start_year ] )
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
        extendBackwarded_iea[[i]] <- merge( iea_data_full[ iea_data_full$fuel %in%
            bp_drivers[[i]], ], iea_proj_data[[i]],
            by = c( "iso", "sector", "fuel" ) )
    }

# Biomass usage is projected by population changes (maybe)
# Don't use this yet; assume biomass is constant. Might use later.
#    un_pop_tot$ratio <- un_pop_tot$BP_years_pop_value / un_pop_tot$start_year_pop_value
#    un_pop_BP_years <- split( un_pop_tot, un_pop_tot$BP_years )
#    iea_data_biomass <- iea_data_full[ iea_data_full$fuel %in% biomass_fuels ]
#    for ( i in length( BP_years ) ) {
#        iea_data_biomass <- addCols( iea_data_biomass, un_pop_BP_years[[i]], "ratio", "iso" )
#
#    }

# Assume biomass is constant for now, and extend back.
    biomass_iea_data <- iea_data_full[ iea_data_full$fuel %in% biomass_fuels, ]
    biomass_fuel_ratios <- array( 1, c( nrow( iea_data_full[
        iea_data_full$fuel %in% biomass_fuels, ] ), 1 ) ) %*%
            array( 1, c( 1, length( BP_years ) ) )
    biomass_iea_proj <- biomass_iea_data[, X_IEA_start_year ] * biomass_fuel_ratios
    extendBackwarded_iea_biomass <- cbind( biomass_iea_data, biomass_iea_proj )  ### TODO: "Backwarded" variables should be changed to "Backward"
    names( extendBackwarded_iea_biomass ) <- names( extendBackwarded_iea[[1]] )

# Other fuels are assumed constant. Form column of ones
    other_iea_data <- iea_data_full[ iea_data_full$fuel %in% other_fuels, ]
    other_fuel_ratios <- array( 1, c( nrow( iea_data_full[
        iea_data_full$fuel %in%
        other_fuels, ] ), 1 ) ) %*% array( 1, c( 1, length( BP_years ) ) )
    other_iea_proj <- other_iea_data[, X_IEA_start_year ] * other_fuel_ratios

# Extend IEA backward.
    extendBackwarded_iea_other <- cbind( other_iea_data, other_iea_proj )
    names( extendBackwarded_iea_other ) <- names( extendBackwarded_iea[[1]] )

    IEA_BP_ext <- rbind( extendBackwarded_iea[[1]], extendBackwarded_iea[[2]],
        extendBackwarded_iea[[3]], extendBackwarded_iea_biomass, extendBackwarded_iea_other )

    IEA_BP_ext <- merge( iea_data_full_original[, c( "iso", "sector", "fuel", paste0( 'X', start_year:( BP_years[1] - 1 ) ) ) ],  ### Did we decide between merge and join? Between rbind and bind_rows?
        IEA_BP_ext, by = c( "iso", "sector", "fuel" ), all.x = TRUE )

    IEA_BP_ext <- rbind( IEA_BP_ext[ , c( "iso", "sector", "fuel", "units", X_emissions_years ) ],
                        iea_data_full_not_extended )

# Replace NAs with 0
    IEA_BP_ext[ is.na(IEA_BP_ext) ] <- 0

# ------------------------------------------------------------------------------
# 5. Aggregate Data by fuel

# Aggregate Activity data over fuels
    printLog( "Aggregate Energy as Activity Data for energy production sectors" )

    IEA_BP_ext<-aggregate ( IEA_BP_ext[
                            X_emissions_years ],
                            by = list( iso = IEA_BP_ext$iso,
                                       sector = IEA_BP_ext$sector,
                                       fuel = IEA_BP_ext$fuel,
                                       units= IEA_BP_ext$units ),
                            sum )

# -----------------------------------------------------------------------------
# 6. Output
# Add comments for each table
    comments.A.energy_data_extension <-
          c( paste0( "IEA energy statistics",
                     " by intermediate sector / intermediate fuel / historical year,",
                     " extendBackwarded with BP energy statistics for earliest BP years" ) )

# write extended energy data
    writeData( IEA_BP_ext, domain = "MED_OUT", fn = "A.IEA_BP_energy_ext",
             comments = comments.A.energy_data_extension )

# Write out Diagnostic Output
  writeData( iea_data_before_BP_PRE, domain = "DIAG_OUT", fn =
               "A.IEA_BP_energy_ext_before_PRE-ext", comments = NULL, meta = F )

# Every script should finish with this line
  logStop()

# END
