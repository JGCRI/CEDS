# ------------------------------------------------------------------------------
# Program Name: A5.2.add_NC_activity_pulp_paper_consumption.R
# Authors: Linh Vu
# Date Last Modified: 2 May 2019
# Program Purpose: Process FAO wood pulp consumption as driver for waste incineration
# Input Files: FAO_wood_pulp_activity.csv, A.UN_pop_master.csv
#              Master_Country_List.csv, activity_input_mapping.csv, Master_Fuel_Sector_List.xlsx
# Output Files: A.pulp_paper_consumption_full.csv, A.FAO_pulp_paper_all_flows.csv,
#               A.pulp_paper_consumption_driver_replace.csv
# To Do:
# Notes: The following countries will use pulp consumption driver for waste incineration:
#       chn, ind, bra, mex, zaf, egy; remaining countries use regional average

# -----------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Universal header file - provides logging, file support, etc.
    headers <- c( "common_data.R","data_functions.R", "analysis_functions.R",
                  "process_db_functions.R", "timeframe_functions.R" ) # Additional function files required.
    log_msg <- paste0( "Process FAO pulp and paper activity data" ) # First message to be printed to the log
    script_name <- "A5.2.add_NC_activity_pulp_paper_consumption.R"
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# -----------------------------------------------------------------------------
# 0.5. Define functions

# scalePopNA(): Takes df in long format, returns df with NAs scaled with population.
    scalePopNA <- function( df ) {  # df in long format
    # Merge with population
        # df <- merge( df, UN_pop, all.x = T )
        if ( nrow( df ) >= 2 ) {
        # Order in descending years (data is in long form)
            df <- dplyr::arrange( df, dplyr::desc( year ) )
        # Loop through all but the first row and scale to population
            for ( i in seq( 2, nrow( df ) ) ) {
                if ( is.na( df$value[[ i ]] ) )
                  df$value[[ i ]] <- df$value[[ i - 1 ]] *
                    df$pop[[ i ]] / df$pop[[ i - 1 ]]
            }
        # Order by years
            df <- dplyr::arrange( df, year )
        # Loop through each year and scale to population   ### I don't see why this happens twice
            for ( i in seq( 2, nrow( df ) ) ) {
                if ( is.na( df$value[[ i ]] ) )
                  df$value[[ i ]] <- df$value[[ i - 1 ]] *
                    df$pop[[ i ]] / df$pop[[ i - 1 ]]
            }
        }
    # Eliminate the population column
        df$pop <- NULL
        return( df )
    }

# smoothRunningAvg(): Takes df, returns df with val_var column replaced by k-year running average,
# grouped by group_vars columns.
# Params:
#    df: the dataframe
#    k: year range for the running avg
#    val_var: The name of the variable holding the value to smooth
#    group_vars: The columns holding the id variables
#    align: The alignment (to pass to rollapply)
    smoothRunningAvg <- function( df, k, val_var, group_vars, align = "left" ) {
    # Apply the following function line by line to the df
        df <- ddply( df, group_vars, function( df ) {
        # Isolate the value column
            df_zoo <- zoo( df[ , val_var ] )
        # Use rollapply to create running mean
            df_zoo <- rollapply( df_zoo, width = k, FUN = mean, fill = NA,
                                 align = align, partial = T, na.rm = T )
            df[, val_var ] <- as.vector( df_zoo )
            return( df )
        } )
        return( df )
    }

# smoothLoessSingle(): returns Loess-smoothed series
# params:
#   df: long-format df
#   val_var: column name (string) holding data to be smoothed
#   coeff: Loess coefficient (smaller -> smoother)
# Returns: vector containing smoothed val_var series
    smoothLoessSingle <- function( df, val_var, coeff ) {
        df <- dplyr::arrange( df, year )
        smooth <- loess.smooth( df$year, unlist( df[ , val_var ] ),
                                evaluation = length( df$year ),
                                span = coeff )$y
        return( smooth )
    }

# simplePlot(): Simple plotting function for diagnostic purpose
# params:
#   df: long-format df
#   value_var: column holding x-axis value
#   group_var: grouping variable name (e.g. region, ctry)
#   title:  plot title
#   fn: fully-qualified name for output. If not NA, print plot as fn.pdf to figures folder [default: NA]
    simplePlot <- function( df, value_var, group_var, title, fn = NA ) {
    # Create a basic line and point plot
        p <- ggplot( df, aes_string( x = "year", y = value_var, group = group_var ) ) +
             geom_line( aes_string( color = group_var ) ) +
             geom_point( size = 1, aes_string( color = group_var ) ) +
             scale_x_continuous( breaks = pretty( df$year, n = 10 ) ) +
             xlab( "year" ) +
             ylab( paste0( "pulp/paper consumption (", df$units[[ 1 ]], ")" ) ) +
             ggtitle( title )
    # If an output file is specified, save the plot
        if ( !is.na( fn ) )
            ggsave( filename = fn, p, width = 11, height = 8.5 )
        print( p )
    }

# List of countries and regions to use pulp and paper consumption driver
    iso_driver_list <- c( "chn", "ind", "bra", "mex", "zaf", "egy" )
# region_driver_list <- c( "Asia", "Latin America" )


# -----------------------------------------------------------------------------
# 1. Input
    library( "zoo" )

# Read in data files
    fao <- readData( "ACTIVITY_IN", "FAO_wood_pulp_activity" )
    Master_Country_List <- readData( "MAPPINGS", "Master_Country_List" )
    UN_pop <- readData( "MED_OUT", "A.UN_pop_master" )

# Read in mapping files
    act_input <- readData( "MAPPINGS", "activity_input_mapping" )
    MSL <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx",
                     sheet_selection = "Sectors" )

# Set variables with the activity of the input file
    activity_name <- act_input$activity[ act_input$file == "FAO_wood_pulp_activity.csv" ]


# -----------------------------------------------------------------------------
# 2. Process FAO wood pulp data

# Clean up population
    UN_pop <- select( UN_pop, iso, year, pop ) %>%
              filter( year <= max( emissions_years ) )

# The following block would query wood pulp production, imports and exports from the FAO API.
# The script currently reads FAO data from a local copy so that running does not require
# Internet connection. Just know it's an option and update the local copy when new data are
# released.
#   Using the "FAOSTAT" package
#   fao_ls <- list( getFAO( domainCode = "FO", elementCode = 5510, itemCode = 1875 ),  # production
#                       getFAO( domainCode = "FO", elementCode = 5610, itemCode = 1875 ),  # import
#                       getFAO( domainCode = "FO", elementCode = 5910, itemCode = 1875 ) ) # export
#   names( fao_ls ) <- c( "production", "imports", "exports" )
#
# # Port into one df
#   fao <- lapply( seq_along( fao_ls ), function( i ){
#     df <- fao_ls[[ i ]]
#     df <- translateCountryCode( df, "FAOST_CODE", "ISO3_WB_CODE", "FAOST_CODE" )
#     names( df ) <- c( "fao_code", "iso", "year", "value" )
#     df$flow <- names( fao_ls )[[ i ]]
#     df$iso <- tolower( df$iso )
#     df$iso[ df$fao_code == 15 ] <- "blx"
#     df <- filter( df, !is.na( iso ) ) #%>% select( -fao_code )
#     return( df )
#   })
#   fao <- do.call( rbind, fao )
#   fao$value[ fao$flow == "exports" ] <- -fao$value[ fao$flow == "exports" ]
#   fao$iso[ fao$iso == "rom" ] <- "rou"
#   fao$iso[ fao$iso == "tmp" ] <- "tls"
#   fao$iso[ fao$iso == "zar" ] <- "cod"
#   writeData( fao, "ACTIVITY_IN", "FAO_wood_pulp_activity", meta = F )

# Add all years to FAO data
    pulp_years <- seq( min( fao$year ), max( fao$year ) )
    X_pulp_years <- paste0( "X", pulp_years )
# Create a long-form data frame with the necessary years and columns and fao
# activity data
    fao_full <- expand.grid( year = pulp_years,
                              flow = c( "production", "imports", "exports" ),
                              iso = unique( fao$iso ), stringsAsFactors = F ) %>%
        dplyr::left_join( fao, by = c( "year", "flow", "iso" ) )

# Add units
    fao_full$units <- "tonnes"
# Isolate 5 needed columns (the rest were used for merging)
    fao_full <- select( fao_full, iso, year, flow, units, value )
# Replace NAs with 0s
    fao_full$value[ is.na( fao_full$value ) ] <- 0

# Diagnostics: Write out original data and data that get dropped
    diag_FAO_all_original <- cast( fao_full, flow + iso ~ year ) %>%
                             dplyr::arrange( iso, flow )
    diag_FAO_dropped <- filter( diag_FAO_all_original, iso %!in%
                                Master_Country_List$iso )

# Drop countries not in MCL
    fao_full <- filter( fao_full, iso %in% Master_Country_List$iso )

# For now, add up composite countries    ### Does this need to change?
    addComposite <- function( df, iso_comp, iso_members ) {
        added <- filter( df, iso %in% c( iso_comp, iso_members ) ) %>%
                 group_by( year, flow, units ) %>%
                 dplyr::summarise( value = sum( value ) )
        added$iso <- iso_comp
        out <- filter( df, iso %!in% c( iso_comp, iso_members ) ) %>%
          bind_rows( added )
        return( out )
    }

# Apply the addComposite() function to each composite country that needs
# addressing (used to disaggregate countries that were reported in groups)
    fao_full <- addComposite( fao_full, "csk", c( "cze", "svk" ) ) %>%
                addComposite( "scg", c( "srb", "mne" ) ) %>%
                addComposite( "blx", c( "bel", "lux" ) )
    fao_full$value <- fao_full$value / 1000   # tonnes to kt
    fao_full$units <- "kt"

# -----------------------------------------------------------------------------
# 3. Prelim discontinuity correction. Produce consumption series
    fao_full$value[ fao_full$value == 0 ] <- NA

# For production, linearly interpolate in-between years if NA
    prod <- filter( fao_full, flow == "production" )
    prod$year <- paste0( "X", prod$year )
    prod <- cast( prod, iso + flow + units ~ year )
    prod[ X_pulp_years ] <- interpolate_NAs( prod[ X_pulp_years ] )
    prod <- melt( prod, id = c( "iso", "flow", "units" ) )
    prod$year <- xYearToNum( prod$year )

# For imports/exports, scale NA with population then do 4-year running average
    fao_full <- dplyr::left_join( fao_full, UN_pop, by = c( "iso", "year" ) )

    imp <- fao_full %>%
        dplyr::filter( flow == "imports" ) %>%
        dplyr::group_by( iso ) %>%
        dplyr::do( scalePopNA( . ) ) %>%
        dplyr::ungroup() %>%
       smoothRunningAvg( k = 4, val_var = "value",
                         group_vars = c( "iso", "flow" ) )

    exp <- fao_full %>%
        dplyr::filter( flow == "exports" ) %>%
        dplyr::group_by( iso ) %>%
        dplyr::do( scalePopNA( . ) ) %>%
        dplyr::ungroup() %>%
       smoothRunningAvg( k = 4, val_var = "value",
                         group_vars = c( "iso", "flow" ) )

# Combine production, import, exports into one df
    fao_full_fixed <- bind_rows( prod, imp, exp ) %>%
                      select( iso, year, flow, units, value ) %>%
                      dplyr::arrange( iso, flow, year )
    fao_full_fixed$value[ is.na( fao_full_fixed$value ) ] <- 0
    fao_full_fixed$country <-
          Master_Country_List$Country_Name[ match( fao_full_fixed$iso,
                                                   Master_Country_List$iso ) ]
    fao_full_fixed$region <-
          Master_Country_List$Region[ match( fao_full_fixed$iso,
                                             Master_Country_List$iso ) ]

# Diagnostics: Write out smoothed FAO production, import, exports
    diag_FAO_all_fixed <- cast( fao_full_fixed,
                                flow + iso + units ~ year,
                                value = "value" )

# Compute consumption = production + imports - exports and per-capita consumption
    consumption <- fao_full_fixed
    consumption <- group_by( consumption, iso, country, region, units, year ) %>%
                   dplyr::summarise( cons = sum( value ) )
    consumption$pop <- UN_pop$pop[ match( paste0( consumption$iso, consumption$year ),
                                          paste0( UN_pop$iso, UN_pop$year ) ) ]
    consumption <- dplyr::mutate( consumption, cons_pc = cons / pop )

# -----------------------------------------------------------------------------
# 4. Smooth consumption series
# Break out time series or major countries (in iso_driver_list) and regional
    consumption_ctry <- filter( consumption, iso %in% iso_driver_list )
    consumption_region <- filter( consumption, iso %!in% iso_driver_list ) %>%
                          group_by( region, units, year ) %>%
                          dplyr::summarise( cons = sum( cons ), pop = sum( pop ) ) %>%
                          dplyr::mutate( cons_pc = cons / pop ) %>%
                          filter( !is.na( cons_pc ) )

# Smooth per-capita consumption by local regression (loess) with coefficient 1/4
# At the country level
    consumption_ctry_loess <- ddply( consumption_ctry,
                                     .( iso, country, region, units ),
                                     function( df ) {
        within( df, { cons_pc_smooth <- smoothLoessSingle( df, "cons_pc",
                                                           coeff = 1 / 4 ) } )
    } )
# At the region level
    consumption_region_loess <- ddply( consumption_region,
                                       .( region, units ), function( df ) {
        within( df, { cons_pc_smooth <- smoothLoessSingle( df, "cons_pc",
                                                           coeff = 1/4 ) } )
    } )

# FSU countries have big peak in 1994-1995, so make cons_pc before 1999 NA (to be extended later)
    consumption_region_loess$cons_pc_smooth[ consumption_region_loess$region == "FSU" &
                                             consumption_region_loess$year < 1999 ] <- NA

# Where cons_pc_smooth is <= 0, extend nearest available year constant
    consumption_ctry_loess_fixed <- consumption_ctry_loess
# First replace with NA...
    consumption_ctry_loess_fixed$cons_pc_smooth[
                      consumption_ctry_loess_fixed$cons_pc_smooth <= 0 ] <- NA
# ...Then fill
    consumption_ctry_loess_fixed <- ddply( consumption_ctry_loess_fixed,
                                           .( iso, country, region, units ),
                                           function( df ) {
        df$cons_pc_smooth <- na.locf( df$cons_pc_smooth, na.rm = F )
        df$cons_pc_smooth <- na.locf( df$cons_pc_smooth, na.rm = F, fromLast = T )
        return( df )
    } )
# Same as above on the regional level
    consumption_region_loess_fixed <- consumption_region_loess
    consumption_region_loess_fixed$cons_pc_smooth[
                        consumption_region_loess_fixed$cons_pc_smooth <= 0 ] <- NA
    consumption_region_loess_fixed <- ddply( consumption_region_loess_fixed,
                                             .( region, units ),
                                             function( df ) {
        df$cons_pc_smooth <- na.locf( df$cons_pc_smooth, na.rm = F )
        df$cons_pc_smooth <- na.locf( df$cons_pc_smooth, na.rm = F, fromLast = T )
        return( df )
    } )

# Compute smoothed total consumption
    consumption_region_loess_tot <- select( consumption_region_loess_fixed,
                                            region, units, year, cons_pc_smooth ) %>%
          merge( select( Master_Country_List, region = Region, iso ), all.x = T ) %>%  # expand to all countries in region
          filter( iso %!in% iso_driver_list ) %>%  # drop countries that already have specific country series
          merge( UN_pop, all.x = T ) %>%   # attach population
          dplyr::mutate( cons = cons_pc_smooth * pop )     # compute total consumption
    consumption_ctry_loess_tot <- dplyr::mutate( consumption_ctry_loess_fixed,
                                          cons = cons_pc_smooth * pop )

# Combine country and regional df
    consumption_smooth_tot <- bind_rows( consumption_ctry_loess_tot,
                                         consumption_region_loess_tot ) %>%
                              unique()  # drop duplicated
    consumption_smooth_tot$activity <- activity_name
    consumption_smooth_tot$year <- paste0( "X", consumption_smooth_tot$year )

# Cast to wide
    replace <- cast( consumption_smooth_tot,
                     iso + activity + units ~ year,
                     value = "cons" )

# Add reformatted activity_data to the activity database, extending or truncating it as necessary.
# By default, it will be extended forward to the common end year, but not backwards.
# Only do this if the activityCheck header function determines that the activities in
# the reformatted activity_data are all present in the Master List.
    if( activityCheck( replace, check_all = FALSE ) ) {
        addToActivityDb( replace )
    }


# -----------------------------------------------------------------------------
# 5. Create full consumption series by scaling with population
# Make df of population ratios
    pop_ratio <- UN_pop
    pop_ratio$year <- paste0( "X", pop_ratio$year )
    pop_ratio <- cast( pop_ratio, iso ~ year, value = "pop" )
# Isolate years for the transformation year, as well as a ratio year
    X_pop_years <- names( pop_ratio )[ grepl( "X", names( pop_ratio ) ) ]
    X_consumption_years <- names( replace )[ grepl( "X", names( replace ) ) ]
    X_ratio_year <- X_consumption_years[ 1 ]
    X_ext_years <- X_pop_years[ X_pop_years %!in% X_consumption_years ]

    pop_ratio[ , X_ext_years ] <- pop_ratio[ , X_ext_years ] /
                                  pop_ratio[ , X_ratio_year ]

# Add all population years to consumption series
    cons_ext <- replace
    cons_ext[ , X_ext_years ] <- NA
    cons_ext <- cons_ext[ c( "iso", "activity", "units", X_pop_years ) ]

# Scale consumption with population
    cons_ext[ , X_ext_years ] <- cons_ext[ , X_ratio_year ] *
          pop_ratio[ match( cons_ext$iso, pop_ratio$iso ), X_ext_years ]

# -----------------------------------------------------------------------------
# 6. Output
    writeData( diag_FAO_all_fixed, "MED_OUT", "A.FAO_pulp_paper_all_flows" )
    writeData( cons_ext, "MED_OUT", "A.pulp_paper_consumption_full" )
    writeData( replace, "DIAG_OUT", "A.pulp_paper_consumption_driver_replace" )

    logStop()
# END
