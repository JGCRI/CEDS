# Header -----------------------------------------------------------------
# Program Name: P4.1.scale_point_sources.R
# Author: Hamza Ahsan, Noah Prime
# Date Last Updated: November 19, 2024
# Program Purpose:  Read point source yaml attribute files, aggregates by country,
#                   compares with the country inventory, and if necessary adjusts
#                   the point source(s) proportionally down to match inventory,
#                   then generates a yaml file of the complete (extrapolated/interpolated)
#                   time series. Additionally creates a copy of the emissions inventory
#                   without the point sources included.
# Input Files: Point source attribute file (e.g. Inco_sudbury_att.yml)
# Output Files: Inco_Sudbury.yml, [em]_CEDS_emissions_no_point_sources.csv
# TODO:

# 0. Read in global settings and headers --------------------------------------
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"
source( paste0( PARAM_DIR, "header.R" ) )

initialize(script_name = "P4.1.scale_point_sources.R",
           log_msg = "Reconciling point sources with CEDS inventory",
           headers = c("data_functions.R", "interpolation_extension_functions.R",
                       'point_source_util_functions.R'),
           common_data = TRUE)

# 0.5.) Set-up details for script ---------------------------------------------

# Define emissions species and gridding resolution
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
res <- as.numeric( args_from_makefile[ 2 ] )
if ( is.na( em ) ) em <- "SO2"

# Set lower bound for diagnostic plots
MIN_DIAG_PLOT_YEAR <- 1990
WRITE_OUT_DETAILS <- FALSE

# 0.1 Set paths -------------------------------------------------------

yml_path <- filePath( domain = 'MED_OUT',
                      domain_extension = paste0( 'full_point_source_yml/', em),
                      fn = '',
                      extension = '' )

base_out_path <- filePath( domain = 'MED_OUT',
                           domain_extension = 'full_point_source_scaled_yml',
                           fn = '',
                           extension = '' )

# Temporary debugging function to extract specific data from CEDS DFs
 get_debug_output <- function( a_CEDS_df ){
     if ( "X2020" %in% names(a_CEDS_df)) {
         data_out <-  a_CEDS_df %>% filter(iso == "bgd", sector == "1A1a_Electricity-public") %>% select("iso","fuel","sector","fuel","X2020")
     } else {
        data_out <-  a_CEDS_df %>% filter(iso == "bgd", sector == "1A1a_Electricity-public", year == "X2020")
     }
     return(data_out)
 }
# 0.2 Read Mapping Data -------------------------------------------

# CEDS old to new sector mapping
ceds_old_to_new_map <- readData(domain = 'MAPPINGS', file_name = 'old_to_new_sectors', meta = FALSE)

# CEDS sector to gridding sector mapping
CEDS_sector_mapping <- readData(
    domain = 'GRIDDING',
    file_name = 'CEDS_sector_to_gridding_sector_mapping',
    domain_extension = 'gridding_mappings/',
    meta = FALSE
    )

# 1. Read in point source data ------------------------------------

# List all yaml files
file_list <- list.files(yml_path, '*.yml')

# Read in each yml
source_df_list <- lapply(file_list, read_yml_all_ems, yml_dir = yml_path)

# Sources combined to one data frame
source_df <- do.call(rbind, source_df_list)


# 1.3.) Stop if no point sources -----------------------------------------------

# As new sources for point source data is added, include those above. Then at
# this point we check if there are any point sources for this species, and if not
# we simply write out the CEDS inventory and stop the script

if( is.null(source_df) ){

    # Read in total emissions from country inventory
    inventory <- readData("MED_OUT", paste0( em, "_total_CEDS_emissions" ) )

    # Save inventory as inventory 'without the point sources'
    writeData( inventory, "MED_OUT", paste0( em, "_total_CEDS_emissions_no_point_sources" ) )

}else{ # Else do all the scaling and reconciliation routine

    # 2.) Prepare point sources ----------------------------------------------------

    # Year Columns
    year_columns <- paste0('X', 1750:end_year)

    # Aggregate time series by country and gridding sector and compare with country inventory, adjust as
    # needed so sum of point sources do not exceed aggregate

    # Get full sector mapping prepared including new sectors
    CEDS_sector_mapping <- CEDS_sector_mapping %>%
        dplyr::left_join( ceds_old_to_new_map, by = c('CEDS_working_sector' = 'ceds_sector' ) ) %>%
        dplyr::mutate( new_sector = ifelse( is.na(new_sector), CEDS_working_sector, new_sector ) )

    # Calculate the sum of point sources by iso and grid sector
    ptsource_sum_df <- source_df %>%
        dplyr::left_join( CEDS_sector_mapping, by = c('CEDS_sector' = 'new_sector' ) ) %>%
        dplyr::group_by(iso, CEDS_sector, fuel) %>%
        dplyr::summarise_at(vars(year_columns), sum, na.rm = TRUE) %>%
        dplyr::rename( sector = CEDS_sector )

    # write.csv(get_debug_output(ptsource_sum_df), file = "pt_db-total_ptSource_em.csv") #SJSTEMP - debugging

    # 3.) Reconcile with CEDS Inventory ------------------------------------------------
    # Compare and adjust time series to be consistent with CEDS inventory

    # Read in total emissions from country inventory
    CEDS_inventory <- readData("MED_OUT", paste0( em, "_total_CEDS_emissions" ) )

    # Get CEDS totals for given iso, grid sector combinations
    total_CEDS_em <- ptsource_sum_df %>%
        # Select only the iso, sector, fuel's that are in the point source database
        dplyr::select( iso, sector, fuel ) %>%
        dplyr::left_join( CEDS_inventory, by = c('iso', 'sector', 'fuel' ) ) %>%
        dplyr::group_by( iso, sector, fuel ) %>%
        dplyr::summarise_at( vars(year_columns), sum, na.rm = TRUE  ) %>%
        ungroup()

    # write.csv(get_debug_output(total_CEDS_em), file = "pt_db-total_CEDS_em.csv") #SJSTEMP - debugging

    # Covert to long format and join
    Xyears <- names(total_CEDS_em)[grepl("X", names(total_CEDS_em))]
    CEDS_long <- total_CEDS_em %>%
        pivot_longer(cols = Xyears, values_to ="ceds_agg", names_to = "year")

    ptsource_sum_long <- ptsource_sum_df  %>%
        pivot_longer(cols = Xyears, values_to ="ps_agg", names_to = "year")

    # Obtain the difference (i.e. "point source aggregate" - "country inventory")
    diff_df <- CEDS_long %>%
        dplyr::left_join(ptsource_sum_long, by = c('iso', 'sector', 'fuel', 'year')) %>%
        dplyr::group_by( iso, sector, fuel ) %>%
        dplyr::mutate( diff =  ps_agg - ceds_agg ) %>%
        # If the difference is less then zero (i.e. the point source aggregate is less
        # than the country inventory), set it to zero
        dplyr::mutate( diff = ifelse(diff < 0, 0, diff))

    # write.csv(get_debug_output(diff_df), file = "pt_db-diff_df.csv") #SJSTEMP - debugging

    # Convert the point source, point source aggregate, country inventory, and
    # difference data frames to long format
    source_df_long <- tidyr::gather(source_df, year, ps_value, all_of(X_extended_years) ) %>%
        dplyr::rename( sector = CEDS_sector )

    # Join the point source DF with the rest
    final_df <- source_df_long %>%
        dplyr::left_join( diff_df, by = c('iso', 'sector', 'fuel', 'year'))

    # write.csv(get_debug_output(final_df), file = "pt_db-final_df_no-scale.csv") #SJSTEMP - debugging

    # If the point source aggregate (diff_df_long) exceeds the country inventory,
    # run the correction calculation for any given year and number of point sources:
    # p1'=p1*ct/(p1+p2+...+pn),
    # where p1' is the corrected point source value, p1 is the original value, and
    # ct is the country inventory for the corresponding sector/fuel, otherwise keep the original value
    scaledown <- 1-1e-6 # scale down point source value slightly more than equality to avoid negative rounding differences

    final_df <- final_df %>%
        dplyr::mutate( corrected = ifelse( diff > 0, scaledown*ps_value * ceds_agg / ps_agg, ps_value) )

    # write.csv(get_debug_output(final_df), file = "pt_db-final_df_scaled.csv") #SJSTEMP - debugging

    # Save full version for later
    all_time_series <- final_df

    # Widen data back out
    final_df_wide <- final_df %>%
        dplyr::select(-c(ps_value, diff, ceds_agg, ps_agg)) %>%
        tidyr::spread( year, corrected ) %>%
        dplyr::rename( CEDS_sector = sector )


    # 4.) Create final yml files -------------------------------------------------

    # Create directory to save to
    dir.create(base_out_path)
    dir.create(paste0(base_out_path, '/', em))

    # Write out YAML files containing individual species time series
    final_sources_list <- split(final_df_wide, seq(nrow(final_df_wide)))
    lapply(final_sources_list, write_yml_by_em, base_yml_dir = base_out_path)


    # 5.) Diagnostic data -----------------------------------------------------------

    # Export point source time series
    writeData(final_df_wide, "DIAG_OUT", paste0( em, "_Point_source_timeseries") )


    # 6.) Generate CEDS inventory minus point sources ------------------------------

    # Get aggregate of corrected point sources
    final_ag_wide <- final_df_wide %>%
        dplyr::group_by( iso, CEDS_sector, fuel ) %>%
        dplyr::summarise_at(vars(year_columns), sum, na.rm = TRUE) %>%
        dplyr::rename( sector = CEDS_sector )

    # Get CEDS agg inventory
    ceds_ag_wide <- CEDS_inventory %>%
        dplyr::right_join( final_ag_wide %>% dplyr::select(iso, sector, fuel), by = c('iso', 'sector', 'fuel') ) %>%
        dplyr::group_by( iso, sector, fuel ) %>%
        dplyr::summarise_at(vars(year_columns), sum, na.rm = TRUE)

    # Merge aggregate point source data frame and country inventory
    final_merge_df <- dplyr::bind_rows(final_ag_wide, ceds_ag_wide)

    # Obtain the difference (i.e. "country inventory" - "point source aggregate")
    final_diff_df <- final_merge_df %>%
        dplyr::group_by( iso, sector, fuel ) %>%
        dplyr::summarise_at(vars(year_columns), diff, na.rm = TRUE) %>%
        dplyr::ungroup()

    inventory_no_point_sources <- CEDS_inventory
    # Replace rows in inventory with rows minus the point source
    # TODO - presumably this would go faster if done in long format with dplyr
    for( i in 1:dim(final_diff_df)[1] ){
        row <- which( inventory_no_point_sources$iso == final_diff_df$iso[i] & inventory_no_point_sources$sector == final_diff_df$sector[i] & inventory_no_point_sources$fuel == final_diff_df$fuel[i] )
        inventory_no_point_sources[row, year_columns] <- final_diff_df[i, year_columns]
    }

    # Save new inventory without the point sources
    writeData( inventory_no_point_sources, "MED_OUT", paste0( em, "_total_CEDS_emissions_no_point_sources" ) )

    # Check for negative values
    negative_emissions <- inventory_no_point_sources %>%
        dplyr::left_join( CEDS_sector_mapping, by = c('sector' = 'new_sector' ) ) %>%
        dplyr::filter(!is.na(CEDS_int_gridding_sector_short)) %>%
        # dplyr::filter(CEDS_int_gridding_sector_short == 'FLR') %>%
        dplyr::group_by( iso, CEDS_int_gridding_sector_short) %>%
        dplyr::summarise_at(vars(starts_with('X')), sum) %>%
        dplyr::ungroup() %>%
        dplyr::filter_at(vars(starts_with('X')), any_vars(.<0))
    if(nrow(negative_emissions) > 0){
        stop('Negative emissions in resulting inventory minus point sources')
    }


    # 7.) Diagnostic, is CEDS minus point source + point source = CEDS ? ------------------------------

    # See if agg ps + new inv = old inv
    agg_ps <- final_ag_wide %>%
        dplyr::group_by(iso, sector) %>%
        dplyr::summarise_at(vars(year_columns), sum, na.rm = TRUE) %>%
        dplyr::ungroup()

    agg_new_inv <- inventory_no_point_sources %>%
        dplyr::group_by(iso, sector) %>%
        dplyr::summarise_at(vars(year_columns), sum, na.rm = TRUE) %>%
        dplyr::ungroup()

    agg_old_inv <- CEDS_inventory %>%
        dplyr::group_by(iso, sector) %>%
        dplyr::summarise_at(vars(year_columns), sum, na.rm = TRUE) %>%
        dplyr::ungroup()

    # Sum ps and new inv then subtract old inv
    check_sum_CEDS_sector_iso <- bind_rows(agg_ps, agg_new_inv) %>%
        dplyr::group_by( sector ) %>%
        dplyr::summarise_at(vars(year_columns), sum, na.rm = TRUE) %>%
        dplyr::ungroup() %>%
        dplyr::bind_rows(agg_old_inv %>% dplyr::group_by( sector ) %>% dplyr::summarise_at(vars(year_columns), sum, na.rm = TRUE) ) %>%
        dplyr::group_by( sector ) %>%
        dplyr::summarise_at(vars(year_columns), diff, na.rm = TRUE) %>%
        dplyr::ungroup()

    # Save diagnostic
    writeData( check_sum_CEDS_sector_iso, "DIAG_OUT", paste0( 'P.', em, "_CEDS_poin-ps+nonps" ) )

    # More diagnostics
    all_time_series %>%
        dplyr::group_by( iso, sector, year, ceds_agg, ps_agg ) %>%
        dplyr::summarise( corrected = sum( corrected ) ) %>%
        dplyr::ungroup() %>%
        dplyr::rename( pre = ps_agg, CEDS = ceds_agg ) %>%
        dplyr::filter( year >= 1970 ) %>%
        writeData( "DIAG_OUT", paste0( 'P.', em, "_point-source_correction_time-series" ) )


    # 8.) Diagnostic Plots -------------------------------------------------------

    # Plotting point source and inventory
    status_labs <- c( 'Pre-Correction', 'Corrected' )
    names( status_labs ) <- c( 'pre', 'corrected' )

    all_time_series <- all_time_series %>%
        dplyr::mutate( year = as.numeric( gsub( 'X', '', year ) ) )

    # Plots CEDS inventory vs aggregate of point sources for the supplied sector
    # Facets about iso and aggregation before and after scaling
    summary_plot <- function( sector_to_plot ){
        plot_data <- all_time_series %>%
            dplyr::group_by( iso, sector, year, ceds_agg, ps_agg, fuel ) %>%
            dplyr::summarise( corrected = sum( corrected ), .groups = "drop") %>%
            dplyr::group_by(iso, sector, year) %>%
            dplyr::summarise(ceds_agg = sum(ceds_agg), ps_agg = sum(ps_agg), corrected = sum(corrected), .groups = "drop") %>%
            dplyr::rename( CEDS = ceds_agg, pre = ps_agg ) %>%
            pivot_longer( cols = c(pre, corrected), names_to = "status", values_to = "point_source") %>%
            dplyr::filter( sector == sector_to_plot ) %>%
            dplyr::mutate( status = factor( status, levels = c( 'pre', 'corrected' ) ) )

        plot_data %>%
            dplyr::filter( year >= MIN_DIAG_PLOT_YEAR ) %>%
            ggplot() +
            aes( x = as.numeric( year ) ) +
            geom_line( aes( y = point_source ), color = 'red' ) +
            geom_line( aes( y = CEDS ), color = 'black' ) +
            geom_line( data = plot_data %>% dplyr::filter( year <= 2005 ),
                       aes( y = point_source ), color = 'red', linetype = 'dashed' ) +
            geom_line( data = plot_data %>% dplyr::filter( year <= 2005 ),
                       aes( y = CEDS ), color = 'black' ) +
            facet_grid( iso ~ status, scales = 'free',
                        labeller = labeller( status = status_labs ) ) +
            labs( x = 'Year', y = paste('Annual',em,'Emissions (kt)'), title = sector_to_plot ) +
            xlim(MIN_DIAG_PLOT_YEAR,end_year) +
            theme_bw() +
            theme( panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank() )
    }

    # path to diagnostic output summary plots folder
    plot_out <- filePath( 'DIAG_OUT', domain_extension = 'point-source-summary-plots/', fn = '', extension = '')

    # Save plot for each sector with sources
    for(sec in unique(all_time_series$sector)){
        num_isos <- all_time_series %>%
            dplyr::filter( sector == sec ) %>%
            dplyr::distinct( iso ) %>%
            nrow()
        summary_plot(sec)
        ggsave( paste0( em, '_', sec, '.pdf'), path = plot_out,
                width = 16, height = num_isos * 3, limitsize = FALSE )
    }

    # Write out point source data for modern period if set to do do
    # Save diagnostic
    if ( WRITE_OUT_DETAILS ) {
        all_time_series %>% filter(year >= 1980 & ps_value > 0 ) %>%
        writeData( domain = 'DIAG_OUT',
                   fn = paste0('P.', em, '_CEDS_point_source_overlap_details' ),
                   meta = FALSE )
    }

    # Computing proportion of emissions removed during scaling by iso, sector
    all_time_series %>%
        dplyr::group_by( iso, sector ) %>%
        dplyr::summarise( prescaled_total = sum(ps_agg), scaled_total = sum(corrected), CEDS_total = sum(ceds_agg) ) %>%
        dplyr::ungroup() %>%
        writeData( domain = 'DIAG_OUT',
                   fn = paste0( em, '_point_source_scaling_effect' ),
                   meta = FALSE )

    # Computing AUC for aggregate point sources and CEDS inventory
    all_time_series %>%
        group_by( iso, sector, year, ceds_agg, ps_agg ) %>%
        dplyr::summarise( corrected_agg = sum( corrected ) ) %>%
        tidyr::gather( status, value, c( ps_agg, corrected_agg ) ) %>%
        dplyr::filter( status == 'corrected_agg' ) %>%
        dplyr::group_by( status, iso, sector ) %>%
        dplyr::mutate( trap_agg = value * lag( value ) / 2,
                       trap_CEDS = ceds_agg * lag( ceds_agg ) / 2 ) %>%
        dplyr::filter( !is.na(trap_agg) ) %>%
        dplyr::summarise( AUC_agg = sum( trap_agg ), AUC_CEDS = sum( trap_CEDS ),
                          AUC_agg_1900 = sum( trap_agg[ year >= 1900 ] ), AUC_CEDS_1900 = sum( trap_CEDS[ year >= 1900 ] ),
                          AUC_agg_1950 = sum( trap_agg[ year >= 1950 ] ), AUC_CEDS_1950 = sum( trap_CEDS[ year >= 1950 ] ),
                          AUC_agg_2000 = sum( trap_agg[ year >= 2000 ] ), AUC_CEDS_2000 = sum( trap_CEDS[ year >= 2000 ] ) ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate( pct_ems_accounted_for_tot = AUC_agg / AUC_CEDS,
                       pct_ems_accounted_for_1900 = AUC_agg_1900 / AUC_CEDS_1900,
                       pct_ems_accounted_for_1950 = AUC_agg_1950 / AUC_CEDS_1950,
                       pct_ems_accounted_for_2000 = AUC_agg_2000 / AUC_CEDS_2000 ) %>%
        dplyr::select( iso, sector, pct_ems_accounted_for_tot:pct_ems_accounted_for_2000 ) %>%
        writeData( domain = 'DIAG_OUT',
                   fn = paste0( em, '_CEDS_point_source_completeness_metric' ),
                   meta = FALSE )

} # End block with point source processing

# End ----------------------------------------------------------------------
logStop()

