#------------------------------------------------------------------------------
# Program Name: A2.3.write_IEA_diff.R
# Authors Names: Linh Vu, Rachel Hoesly, Patrick O'Rourke
# Date Last Modified: May 4, 2020
# Program Purpose: Writes out difference between IEA DOMSUP and CEDS
#                  consumption for coal, natural gas, oil
# Input Files: A.IEA_en_stat_ctry_hist.csv, en_biomass_fsu_fix.csv,
#              Master_Fuel_Sector_List.xlsx, IEA_product_fuel.csv, IEA_flow_sector.csv
# Output Files: A.IEA_CEDS_hard_coal_difference.csv, A.IEA_CEDS_brown_coal_difference.csv,
#               A.IEA_CEDS_coal_difference_subzero.csv, A.IEA_CEDS_natural_gas_difference.csv,
#               A.IEA_CEDS_natural_gas_difference_subzero.csv, A.IEA_CEDS_oil_difference.csv,
#               A.IEA_CEDS_oil_difference_subzero.csv, A.IEA_CEDS_oil_difference_ratio.csv,
#               A.IEA_en_stat_ctry_hist-NonEnergy-Oil&Gas.csv
# Notes:  The purpose of this script is to quantify how much fuel is consumed in the
#         transformation sector and, therefore, not accounted for in CEDS fuel consumption.
#         This is then used for balancing fuel consumption and extending emissions back in time.
# TODO: 1) Updating flow mapping to avoid hard coding flows (add a column for IEA NE flows).
#          See GIT issue #230
#       2) Updating product mapping to avoid hard coding products (add a primary vs. secondary flag)
#          See GIT issue #230
#-------------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "common_data.R" ) # Additional function files required.
    log_msg <- "Writing out difference between IEA and CEDS consumption..." # First message to be printed to the log
    script_name <- "A2.3.write_IEA_diff.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 0.5 Define functions
# writeDiff(): Function to write out IEA and CEDS consumption difference
#   params:   IEA_data: df containing IEA data
#             CEDS_data: df containing CEDS data
#             IEA_fuel_list: vector of IEA PRODUCT names
#             CEDS_fuel_list: vector of CEDS fuel names
#             fuel_name: fuel name in function's output
#             IEA_flow_list: vector of IEA FLOW to include [default: "DOMSUP"]
#             IEA_flow_list_exclude: vector of IEA FLOW to exclude [default: NA]
#   Returns:  List of two data frames, where df1 contains IEA and CEDS difference and
#             df2 contains diagnostics of where IEA < CEDS
# Note:
#  -- units is kt, so may need to process IEA input for certain fuels
#  -- IEA consumption = IEA_data[FLOW = IEA_flow_list, PRODUCT = IEA_fuel_list]
#                     - IEA_data[FLOW = IEA_flow_list_exclude, PRODUCT = IEA_fuel_list]
#     CEDS consumption = CEDS_data[fuel = CEDS_fuel_list]
#  --> Output diff = max(IEA consumption - CEDS consumption, 0)
    writeDiff <- function( IEA_data, CEDS_data, IEA_fuel_list, CEDS_fuel_list, fuel_name,
                           IEA_flow_list = "DOMSUP", IEA_flow_list_exclude = NA ) {

        IEA_subset <- computeIEASupply( IEA_data, IEA_fuel_list, IEA_flow_list, IEA_flow_list_exclude )
        CEDS_subset <- CEDS_data %>%
            dplyr::filter( fuel %in% CEDS_fuel_list ) %>%
            dplyr::select( -fuel, -sector, -units ) %>%
            dplyr::group_by( iso ) %>%
            dplyr::summarise_all( funs( sum(., na.rm = T ) ) )

        return ( computeDiff( IEA_subset, CEDS_subset, fuel_name ) )

    }

# computeIEASupply(): Helper function to compute IEA supply of specified fuel
#   params:   IEA_data: df containing IEA data
#             IEA_fuel_list: vector of IEA PRODUCT names
#             IEA_flow_list: vector of IEA FLOW to include [default: "DOMSUP"]
#             IEA_flow_list_exclude: vector of IEA FLOW to exclude [default: NA]
#   Returns:  df containing IEA_data[FLOW = IEA_flow_list, PRODUCT = IEA_fuel_list]
#             - IEA_data[FLOW = IEA_flow_list_exclude, PRODUCT = IEA_fuel_list]
    computeIEASupply <- function( IEA_data, IEA_fuel_list, IEA_flow_list = "DOMSUP",
                                  IEA_flow_list_exclude = NA ) {

        # out = IEA_data[FLOW = IEA_flow_list, PRODUCT = IEA_fuel_list]
        out <- IEA_data %>%
            dplyr::filter( FLOW %in% IEA_flow_list, PRODUCT %in% IEA_fuel_list ) %>%
            dplyr::select( -PRODUCT, -FLOW ) %>%
            dplyr::group_by( iso ) %>%
            dplyr::summarise_all( funs( sum(., na.rm = T ) ) ) %>%
            data.frame()

        # If necessary, subtract consumption from IEA_flow_list_exclude
        if ( !is.na ( IEA_flow_list_exclude ) ) {
            out <- out %>%
                tidyr::gather( key = variable, value = value, X_IEA_years )

            # excluded = IEA_data[FLOW = IEA_flow_list_exclude, PRODUCT = IEA_fuel_list]
            excluded <- IEA_data %>%
                dplyr::filter( FLOW %in% IEA_flow_list_exclude, PRODUCT %in% IEA_fuel_list ) %>%  # Select only rows of IEA_flow_list_exclude and IEA_fuel_list
                dplyr::select( -PRODUCT, -FLOW ) %>%  # Drop columns PRODUCT and FLOW
                dplyr::group_by( iso ) %>%   # Group table by iso (ID column)
                dplyr::summarise_all( funs( sum(., na.rm = T ) ) ) %>%
                data.frame() %>%   # Convert grouped table to df to avoid later issues
                tidyr::gather( key = variable, value = value, X_IEA_years )

            names( excluded ) <- c( "iso", "variable", "to_subtract" )

            # output = out - excluded
            out <- merge( out, excluded, all.x = T )
            out$value[ !is.na( out$to_subtract ) ] <- out$value[ !is.na( out$to_subtract ) ] -
                out$to_subtract[ !is.na( out$to_subtract ) ]
            out$to_subtract <- NULL
            out <- out %>% tidyr::spread( variable, value )

        }

        return( out )

    }

# computeDiff(): Helper function to compute IEA and CEDS difference
#   params:   IEA_subset: df of IEA consumption, aggregated by country
#             CEDS_subset: df of CEDS consumption, aggregated by country
#             fuel_name: fuel name in function's output
#   Returns:  List of two data frames, where df1 contains IEA and CEDS difference and
#             df2 contains diagnostics of where IEA < CEDS
    computeDiff <- function( IEA_subset, CEDS_subset, fuel_name ) {

        # Compute difference
        IEA_subset <- dplyr::filter( IEA_subset, iso %in% CEDS_subset$iso ) %>% dplyr::arrange( iso )
        IEA_subset <- IEA_subset[ names( IEA_subset ) %in% names( CEDS_subset ) ]
        CEDS_subset <- dplyr::filter( CEDS_subset, iso %in% IEA_subset$iso ) %>% dplyr::arrange( iso )
        CEDS_subset <- CEDS_subset[ names( CEDS_subset ) %in% names( IEA_subset ) ]
        diff <- IEA_subset
        Xyears <- names( diff )[ grepl( "X", names( diff ) ) ] # TODO: This would be a good utility function
        diff[ Xyears ] <- diff[ Xyears ] - CEDS_subset[ Xyears ]
        diff <- as.data.frame( diff )

        # If difference < 0, bring up to 0 and write out diagnostics
        subzero <- diff %>%
            tidyr::gather( key = years, value = difference, Xyears ) %>%
            dplyr::mutate( difference = if_else( difference >= 0, NA_real_, difference ) ) %>%
            tidyr::spread( years, difference ) %>%
            dplyr::filter_at( .vars = Xyears, any_vars( !is.na( . ) ) ) %>%
            dplyr::select_if( funs( !all.na( . ) ) )

        diff[ diff < 0 ] <- 0

        # Clean up
        diff$sector <- "1A1bc_Other-transformation"
        diff$fuel <- fuel_name
        diff$units <- "kt"
        diff <- diff[ c( "iso", "sector", "fuel", "units", Xyears ) ]

        # Output
        out <- list( diff, subzero )
        names( out ) <- c( "diff", "subzero" )
        return( out )
    }

# ------------------------------------------------------------------------------
# 1. Read in files

    MSL <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Sectors" )
    IEA_en_stat_ctry_hist <- readData( "MED_OUT", "A.IEA_en_stat_ctry_hist" )
    IEA_product_fuel <- readData( "EN_MAPPINGS", "IEA_product_fuel" )

    IEA_flow_sector <- readData( "EN_MAPPINGS", "IEA_flow_sector" )

    A.en_biomass_fsu_fix <- readData( "MED_OUT", "A.en_biomass_fsu_fix" )

# Define values
    # Include only primary fuels (not secondary fuels) for IEA fuel list (using this list with the Domestic Supply Flow)
    IEA_coal_list <- c( "Brown coal (if no detail) (kt)", "Coking coal (kt)",
                        "Hard coal (if no detail) (kt)", "Other bituminous coal (kt)",
                        "Sub-bituminous coal (kt)", "Lignite (kt)", "Anthracite (kt)",
                        "Peat (kt)" )
    # TODO: This could be within the mapping file if we indicated primary fuels in a column
    CEDS_coal_list <- unique( grep( "coal", IEA_product_fuel$fuel, value = TRUE ) )

    IEA_hard_coal_list <- c( "Hard coal (if no detail) (kt)", "Coking coal (kt)", "Anthracite (kt)" ,
                             "Other bituminous coal (kt)", "Sub-bituminous coal (kt)" )
    # TODO: This could be within the mapping file if we indicated primary fuels in a column
    IEA_brown_coal_list <- c( "Brown coal (if no detail) (kt)", "Lignite (kt)", "Peat (kt)" )
    # TODO: This could be within the mapping file if we indicated primary fuels in a column

    ### need to pay attention to primary and secondary fuels - refinery gas is a secondary fuel
    # TODO: Given the above comment, should we delete refinery gas? or any others here?
    # TODO: This could be within the mapping file if we indicated primary fuels in a column (other than NGL?)
    IEA_natural_gas_list <- c( "Gas works gas (TJ-gross)", "Coke oven gas (TJ-gross)",
                               "Blast furnace gas (TJ-gross)", "Other recovered gases (TJ-gross)",
                               "Natural gas (TJ-gross)", "Refinery gas (kt)", "Biogases (TJ-net)",
                               "Natural gas liquids (kt)" )

    CEDS_natural_gas_list <- c( "natural_gas" )

    IEA_oil_list_primary <- c( "Oil shale and oil sands (kt)", "Crude/NGL/feedstocks (if no detail) (kt)", "Crude oil (kt)",
                               "Refinery feedstocks (kt)", "Additives/blending components (kt)",
                               "Other hydrocarbons (kt)" )
    # TODO: This could be within the mapping file if we indicated primary fuels in a column (other than NGL?)
    IEA_oil_list_secondary <- c( "Ethane (kt)", "Liquefied petroleum gases (LPG) (kt)", "Motor gasoline excl. biofuels (kt)",
                                 "Aviation gasoline (kt)", "Gasoline type jet fuel (kt)", "Kerosene type jet fuel excl. biofuels (kt)",
                                 "Bio jet kerosene (kt)", "Other kerosene (kt)", "Gas/diesel oil excl. biofuels (kt)", "Fuel oil (kt)",
                                 "Naphtha (kt)", "White spirit & SBP (kt)", "Lubricants (kt)", "Bitumen (kt)", "Paraffin waxes (kt)",
                                 "Petroleum coke (kt)", "Other oil products (kt)", "Biogasoline (kt)", "Biodiesels (kt)",
                                 "Other liquid biofuels (kt)" )
    # TODO: This could be within the mapping file if we indicated primary fuels in a column

    CEDS_oil_list <- unique( grep( "oil", IEA_product_fuel$fuel, value = TRUE ) )

# Remove other IEA tracked values (not assigned to CEDS sectors)
    en_biomass_fsu_fix <- A.en_biomass_fsu_fix %>%
        dplyr::filter( sector %in% MSL$sector )

# ------------------------------------------------------------------------------
# 2. Write out difference between IEA DOMSUP and CEDS consumption for each fuel

# Prelim natural gas processing: Some IEA natural gas PRODUCTS are in TJ (net and gross), so convert to kt

#   Define IEA gases that require fixing,
    IEA_TJ_gas <- unique( IEA_en_stat_ctry_hist$PRODUCT[ IEA_en_stat_ctry_hist$PRODUCT %in% IEA_natural_gas_list &
                                                          grepl( "TJ", IEA_en_stat_ctry_hist$PRODUCT ) ] )

#   Make separate lists of natural gas fuels with TJ-net or TJ-gross units since the unit for biogas is TJ-net,
#   which will require a different conversion factor than gases in TJ-gross
    IEA_TJ_gas_no_TJnet <- grep( "TJ-net",  IEA_TJ_gas, value = TRUE , invert = TRUE )  # The invert selects all IEA_TJ_gas without "TJ-net" units
    IEA_TJ_gas_TJnet <- grep( "TJ-net",  IEA_TJ_gas, value = TRUE )

#   Convert gases to kt
    IEA_en_stat_ctry_hist <- IEA_en_stat_ctry_hist %>%
        dplyr::group_by( iso, FLOW, PRODUCT) %>%
        tidyr::gather( key = years, value = energy_consumption, X_IEA_years) %>%
        dplyr::mutate( energy_consumption = if_else( PRODUCT %in% IEA_TJ_gas_no_TJnet,
                                                     energy_consumption/conversionFactor_naturalgas_TJ_per_kt_Gross,
                                            if_else( PRODUCT %in% IEA_TJ_gas_TJnet,
                                                     energy_consumption/conversionFactor_naturalgas_TJ_per_kt_Net,
                                                     energy_consumption ) ) ) %>%
        tidyr::spread( years, energy_consumption ) %>%
        dplyr::select( iso, FLOW, PRODUCT, X_IEA_years ) %>%
        dplyr::ungroup( )

# Coal
    out <- writeDiff( IEA_en_stat_ctry_hist, en_biomass_fsu_fix, IEA_coal_list, CEDS_coal_list,
                      "coal", "DOMSUP", NA )
    diff_coal <- out[[ "diff" ]]
    subzero_coal <- out[[ "subzero" ]]

# hard coal
    out <- writeDiff( IEA_en_stat_ctry_hist, en_biomass_fsu_fix, IEA_hard_coal_list, c('hard_coal', 'coal_coke'),
                      "hard_coal", "DOMSUP", NA )
    diff_hard_coal <- out[[ "diff" ]]
    subzero_hard_coal <- out[[ "subzero" ]]

# brown coal
    out <- writeDiff( IEA_en_stat_ctry_hist, en_biomass_fsu_fix,
                      IEA_brown_coal_list, c('brown_coal'),
                      "brown_coal", "DOMSUP", NA )
    diff_brown_coal <- out[[ "diff" ]]
    subzero_brown_coal <- out[[ "subzero" ]]

    # TODO: This could use the object IEA_hard_coal_list maybe? But that currently also includes other bit and sub-bit
    iea <- computeIEASupply( IEA_en_stat_ctry_hist,
                            c( "Hard coal (if no detail) (kt)", "Coking coal (kt)", "Anthracite (kt)" ) ) %>%
        dplyr::filter( iso == 'usa' ) %>%
        tidyr::gather( year, value, -iso )

# Natural gas
    out <- writeDiff( IEA_en_stat_ctry_hist, en_biomass_fsu_fix, IEA_natural_gas_list, CEDS_natural_gas_list,
                      "natural_gas", "DOMSUP", "NONENUSE" )
    diff_natural_gas <- out[[ "diff" ]]
    subzero_natural_gas <- out[[ "subzero" ]]

# Oil
# Take IEA supply = primary oil DOMSUP + secondary oil IMPORTS/EXPORTS
#                   - primary oil NONENUSE - secondary oil NONENUSE
    IEA_oil_primary <- computeIEASupply( IEA_en_stat_ctry_hist, IEA_oil_list_primary, "DOMSUP", "NONENUSE" )
    IEA_oil_secondary <- computeIEASupply( IEA_en_stat_ctry_hist, IEA_oil_list_secondary,
                                           c( "IMPORTS", "EXPORTS" ), "NONENUSE" )
    IEA_oil <- dplyr::bind_rows( IEA_oil_primary, IEA_oil_secondary ) %>%
        dplyr::group_by( iso ) %>%
        dplyr::summarise_all( funs( sum(., na.rm = T ) ) )

    CEDS_oil <- en_biomass_fsu_fix %>%
        dplyr::filter( fuel %in% CEDS_oil_list ) %>%
        dplyr::select( -fuel, -sector, -units ) %>%
        dplyr::group_by( iso ) %>%
        dplyr::summarise_all( funs( sum(., na.rm = T ) ) )

    out <- computeDiff( IEA_oil, CEDS_oil, "oil" )
    diff_oil <- out[[ "diff" ]]
    subzero_oil <- out[[ "subzero" ]]

# Diagnostics: Compute diff/total CEDS
    diag_ratio <- diff_oil %>%
        dplyr::filter( iso %in% CEDS_oil$iso ) %>%
        dplyr::arrange( iso )
    CEDS_oil_temp <- CEDS_oil %>%
        dplyr::filter( iso %in% diag_ratio$iso ) %>%
        dplyr::arrange( iso )
    diag_ratio[ , X_IEA_years ] <- diag_ratio[ , X_IEA_years ] / CEDS_oil_temp[ , X_IEA_years ]

# Diagnostics: Write out IEA Non-Energy consumption for natural gas and oil
    IEA_NE_sectors <- c( "NECHEM", "NEINTREN", "NEOTHER", "NETRANS", "NEFOODPRO",
                         "NEIND", "NEINONSPEC", "NEIRONSTL", "NEMACHINE", "NEMINING",
                         "NENONFERR", "NENONMET", "NEPAPERPRO", "NETEXTILES",
                         "NETRANSEQ", "NEWOODPRO", "NECONSTRUC", "NONENUSE" )
    # TODO This could be automated with the a column in the FLOW mapping file labeling NE sectors

    CEDS_fuels <- unique( grep( "oil|natural_gas", IEA_product_fuel$fuel, value = TRUE ) )

    IEA_nonenergy <- IEA_en_stat_ctry_hist %>%
        dplyr::filter( FLOW %in% IEA_NE_sectors ) %>%
        dplyr::left_join( IEA_flow_sector[, c( "flow_code", "sector" ) ], by = c( "FLOW" = "flow_code" ) ) %>%
        dplyr::left_join( IEA_product_fuel[, c( "product", "fuel" ) ], by = c( "PRODUCT" = "product" ) ) %>%
        dplyr::mutate( units = "kt" )

    # Check if any flows (sectors) didn't get mapped that should have
    na_sectors <- IEA_nonenergy %>%
        dplyr::filter( is.na( sector ) )
    unique_flows_not_mapped <- sort( unique( na_sectors$FLOW ) )
    IEA_flow_sector_not_mapped <- IEA_flow_sector %>%
        dplyr::filter( flow_code %in% unique_flows_not_mapped )
    if( any( !is.na( IEA_flow_sector_not_mapped$sector ) ) ){

        stop( "Some IEA FLOWS which should have been mapped were not mapped. Please see ", script_name )

    }

    # Check if any products (fuels) didn't get mapped that should have
    na_fuels <- IEA_nonenergy %>%
        dplyr::filter( is.na( fuel ) )
    unique_products_not_mapped <- sort( unique( na_fuels$PRODUCT ) )
    IEA_products_sector_not_mapped <- IEA_product_fuel %>%
        dplyr::filter( product %in% unique_products_not_mapped )
    if( any( !is.na( IEA_products_sector_not_mapped$fuel ) ) ){

        stop( "Some IEA PRODUCTS which should have been mapped were not mapped. Please see ", script_name )

    }

    diag_nonenergy <- IEA_nonenergy %>%
        dplyr::filter( fuel %in% CEDS_fuels ) %>%
        dplyr::arrange( iso, FLOW, PRODUCT ) %>%
        dplyr::select( iso, FLOW, PRODUCT, sector, fuel, units, X_IEA_years )

# ------------------------------------------------------------------------------
# 3. Output
    writeData( diff_hard_coal     , "MED_OUT" , "A.IEA_CEDS_hard_coal_difference"           )
    writeData( diff_brown_coal    , "MED_OUT" , "A.IEA_CEDS_brown_coal_difference"          )

    writeData( subzero_coal       , "DIAG_OUT", "A.IEA_CEDS_coal_difference_subzero"        )

    writeData( diff_natural_gas   , "MED_OUT" , "A.IEA_CEDS_natural_gas_difference"         )
    writeData( subzero_natural_gas, "DIAG_OUT", "A.IEA_CEDS_natural_gas_difference_subzero" )

    writeData( diff_oil           , "MED_OUT" , "A.IEA_CEDS_oil_difference"           )
    writeData( subzero_oil        , "DIAG_OUT", "A.IEA_CEDS_oil_difference_subzero"   )

# Diagnostics
    writeData( diag_ratio, "DIAG_OUT", "A.IEA_CEDS_oil_difference_ratio" )
    diag_nonenergy[ is.na( diag_nonenergy ) ] <- ""
    writeData( diag_nonenergy, "DIAG_OUT", "A.IEA_en_stat_ctry_hist-NonEnergy-Oil&Gas" )

    logStop()

# END
