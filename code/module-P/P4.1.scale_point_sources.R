# Header -----------------------------------------------------------------
# Program Name: P4.1.scale_point_sources.R
# Author: Hamza Ahsan, Noah Prime
# Date Last Updated: October 27, 2022
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
                       'gridding_functions.R', 'point_source_util_functions.R'),
           common_data = TRUE)

# 0.5.) Set-up details for script ---------------------------------------------

# Define emissions species and gridding resolution
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
res <- as.numeric( args_from_makefile[ 2 ] )
if ( is.na( em ) ) em <- "SO2"


# 0.1 Set paths -------------------------------------------------------

yml_path <- filePath( domain = 'MED_OUT',
                      domain_extension = paste0( 'full_point_source_yml/', em),
                      fn = '',
                      extension = '' )

base_out_path <- filePath( domain = 'MED_OUT',
                           domain_extension = 'full_point_source_scaled_yml',
                           fn = '',
                           extension = '' )

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

    # Aggregate time series by country and fuel and compare with country inventory, adjust as
    # needed so sum of point sources do not exceed aggregate

    # Calculate the sum of point sources by iso
    sum_df <- source_df %>%
        group_by(iso, CEDS_sector, fuel) %>%
        summarise_at(vars(X1750:X2019), sum, na.rm = TRUE) %>%
        dplyr::rename( sector = CEDS_sector )

    # 3.) Reconcile with CEDS Inventory ------------------------------------------------
    # Compare and adjust time series to be consistent with CEDS inventory

    # Read in total emissions from country inventory
    inventory <- readData("MED_OUT", paste0( em, "_total_CEDS_emissions" ) )

    # Get CEDS inventory totals for given iso, sector, fuel combinations
    total_em <- sum_df %>%
        dplyr::select( iso, sector, fuel ) %>%
        dplyr::left_join( inventory, by = c('iso', 'sector', 'fuel' ) ) %>%
        dplyr::group_by( iso, sector, fuel ) %>%
        dplyr::summarise_at( vars(X1750:X2019), sum, na.rm = TRUE  )


    # Merge aggregate point source data frame and country inventory
    merge_df <- bind_rows(total_em, sum_df)

    # Obtain the difference (i.e. "point source aggregate" - "country inventory")
    diff_df <- merge_df %>%
        group_by(iso,sector,fuel) %>%
        summarise_at(vars(X1750:X2019), diff, na.rm = TRUE)

    # If the difference is less then zero (i.e. the point source aggregate is less
    # than the country inventory), set it to zero
    diff_df[diff_df < 0] <- 0

    # Convert the point source, point source aggregate, country inventory, and
    # difference data frames to long format
    source_df_long <- gather(source_df, year, ps_value, all_of(X_extended_years) ) %>%
        #dplyr::select(id, iso, CEDS_sector, fuel, name, year, ps_value) %>%
        dplyr::rename( sector = CEDS_sector )

    sum_df_long <- gather(sum_df, year, ps_agg, all_of(X_extended_years) )

    total_em_long <- gather(total_em, year, ceds_agg, all_of(X_extended_years) ) %>%
        dplyr::select(iso, sector, fuel, year, ceds_agg)

    diff_df_long <- gather(diff_df, year, diff, all_of(X_extended_years) )

    # Join the data frames together
    final_df <- left_join(source_df_long, total_em_long, by = c("iso", "sector", "fuel", "year") ) %>%
        left_join(sum_df_long, by = c("iso", "sector", "fuel", "year") ) %>%
        left_join(diff_df_long, by = c("iso", "sector", "fuel", "year") )

    # If the point source aggregate (diff_df_long) exceeds the country inventory,
    # run the correction calculation for any given year and number of point sources:
    # p1'=p1*ct/(p1+p2+...+pn),
    # where p1' is the corrected point source value, p1 is the original value, and
    # ct is the country inventory for the corresponding sector/fuel, otherwise keep the original value
    final_df <- final_df %>%
        dplyr::mutate( corrected = ifelse( diff > 0, ps_value * ceds_agg / ps_agg, ps_value) )


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
        group_by( iso, CEDS_sector, fuel ) %>%
        summarise_at(vars(X1750:X2019), sum, na.rm = TRUE) %>%
        dplyr::rename( sector = CEDS_sector )

    # Merge aggregate point source data frame and country inventory
    final_merge_df <- bind_rows(final_ag_wide, total_em)

    # Obtain the difference (i.e. "country inventory" - "point source aggregate")
    final_diff_df <- final_merge_df %>%
        group_by( iso, sector, fuel ) %>%
        summarise_at(vars(X1750:X2019), diff, na.rm = TRUE) %>%
        dplyr::ungroup()

    # Save full inventory
    temp_inv <- inventory

    # Replace rows in inventory with rows minus the point source
    cols <- paste0('X',1750:2019)
    for( i in 1:dim(final_diff_df)[1] ){
        row <- which( inventory$iso == final_diff_df$iso[i] & inventory$sector == final_diff_df$sector[i] & inventory$fuel == final_diff_df$fuel[i] )
        inventory[row, cols] <- final_diff_df[i,cols]
    }

    # Save new inventory without the point sources
    writeData( inventory, "MED_OUT", paste0( em, "_total_CEDS_emissions_no_point_sources" ) )


    # 7.) Diagnostic, is CEDS minus point source + point source = CEDS ? ------------------------------

    # See if agg ps + new inv = old inv
    agg_ps <- final_ag_wide %>%
        dplyr::group_by(iso, sector) %>%
        dplyr::summarise_at(vars(X1750:X2019), sum, na.rm = TRUE) %>%
        dplyr::ungroup()

    agg_new_inv <- inventory %>%
        dplyr::group_by(iso, sector) %>%
        dplyr::summarise_at(vars(X1750:X2019), sum, na.rm = TRUE) %>%
        dplyr::ungroup()

    agg_old_inv <- temp_inv %>%
        dplyr::group_by(iso, sector) %>%
        dplyr::summarise_at(vars(X1750:X2019), sum, na.rm = TRUE) %>%
        dplyr::ungroup()

    # Sum ps and new inv then subtract old inv
    check_sum_CEDS_sector_iso <- bind_rows(agg_ps, agg_new_inv) %>%
        dplyr::group_by( sector ) %>%
        dplyr::summarise_at(vars(X1750:X2019), sum, na.rm = TRUE) %>%
        ungroup() %>%
        dplyr::bind_rows(agg_old_inv %>% dplyr::group_by( sector ) %>% dplyr::summarise_at(vars(X1750:X2019), sum, na.rm = TRUE) ) %>%
        dplyr::group_by( sector ) %>%
        dplyr::summarise_at(vars(X1750:X2019), diff, na.rm = TRUE) %>%
        dplyr::ungroup()

    # Save diagnostic
    writeData( check_sum_CEDS_sector_iso, "DIAG_OUT", paste0( 'P.', em, "_CEDS-ps+nonps" ) )


    # 8.) Diagnostic Plots -------------------------------------------------------

    # Plotting point source and inventory
    status_labs <- c( 'Pre-Correction', 'Corrected' )
    names( status_labs ) <- c( 'pre', 'corrected' )

    all_time_series <- all_time_series %>%
        dplyr::mutate( year = as.numeric( gsub( 'X', '', year ) ) )

    # Plots CEDS inventory vs aggregate of point sources for the supplied sector
    # Facets about iso and aggregation before and after scaling
    summary_plot <- function( sector_to_plot ){
        all_time_series %>%
            dplyr::group_by( iso, sector, fuel, year, ceds_agg, ps_agg ) %>%
            dplyr::summarise( corrected = sum( corrected ) ) %>%
            dplyr::ungroup() %>%
            dplyr::group_by( iso, sector, year ) %>%
            dplyr::summarise( CEDS = sum(ceds_agg), pre = sum(ps_agg), corrected = sum(corrected) ) %>%
            gather( status, point_source, c(pre,corrected) ) %>%
            #dplyr::filter( sector == '1A1a_Electricity-public',
            #               iso %in% c('bgd', 'bgr', 'chn', 'ind', 'usa', 'zaf', 'rus' ) ) %>%
            dplyr::filter( sector == sector_to_plot ) %>%
            dplyr::mutate( status = factor( status, levels = c( 'pre', 'corrected' ) ) ) %>%
            ggplot() +
            aes( x = as.numeric( year ) ) +
            geom_line( aes( y = point_source ), color = 'red' ) +
            geom_line( aes( y = CEDS ), color = 'black' ) +
            facet_grid( iso ~ status, scales = 'free',
                        labeller = labeller( status = status_labs ) ) +
            labs( x = 'Year', y = 'Annual SO2 Emissions (kt)', title = sector_to_plot ) +
            theme_bw() +
            theme( panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank() )
    }

    # path to diagnostic output summary plots folder
    plot_out <- filePath( 'DIAG_OUT', domain_extension = 'summary-plots/', fn = '', extension = '')

    # Save plot for each sector with sources
    for(sec in unique(all_time_series$sector)){
        num_isos <- all_time_series %>%
            dplyr::filter( sector == sec ) %>%
            dplyr::distinct( iso ) %>%
            nrow()
        summary_plot(sec)
        ggsave( paste0( em, '_', sec, '.pdf'), path = plot_out,
                width = 8, height = num_isos * 3, limitsize = FALSE )
    }

    # Computing proportion of emissions removed during scaling by iso, sector
    all_time_series %>%
        dplyr::group_by( iso, sector ) %>%
        dplyr::sumarise( prescaled_total = sum(ps_agg), scaled_total = sum(corrected), CEDS_total = sum(ceds_agg) ) %>%
        dplyr::ungroup() %>%
        writeData( domain = 'DIAG_OUT',
                   fn = paste0( em, '_point_source_scaling_effect' ),
                   meta = FALSE )

    # Computing AUC for aggregate point sources and CEDS inventory
    all_time_series %>%
        group_by( iso, sector, fuel, year, ceds_agg, ps_agg ) %>%
        dplyr::summarise( corrected_agg = sum( corrected ) ) %>%
        gather( status, value, c( ps_agg, corrected_agg ) ) %>%
        dplyr::filter( status == 'corrected_agg' ) %>%
        group_by( status, iso, sector, fuel ) %>%
        dplyr::mutate( trap_agg = value * lag( value ) / 2,
                       trap_CEDS = ceds_agg * lag( ceds_agg ) / 2 ) %>%
        dplyr::filter( !is.na(trap_agg) ) %>%
        dplyr::summarise( AUC_agg = sum( trap_agg ), AUC_CEDS = sum( trap_CEDS ),
                          AUC_agg_1900 = sum( trap_agg[ year >= 1900 ] ), AUC_CEDS_1900 = sum( trap_CEDS[ year >= 1900 ] ),
                          AUC_agg_1950 = sum( trap_agg[ year >= 1950 ] ), AUC_CEDS_1950 = sum( trap_CEDS[ year >= 1950 ] ),
                          AUC_agg_2000 = sum( trap_agg[ year >= 2000 ] ), AUC_CEDS_2000 = sum( trap_CEDS[ year >= 2000 ] ) ) %>%
        ungroup() %>%
        dplyr::mutate( pct_ems_accounted_for_tot = AUC_agg / AUC_CEDS,
                       pct_ems_accounted_for_1900 = AUC_agg_1900 / AUC_CEDS_1900,
                       pct_ems_accounted_for_1950 = AUC_agg_1950 / AUC_CEDS_1950,
                       pct_ems_accounted_for_2000 = AUC_agg_2000 / AUC_CEDS_2000 ) %>%
        dplyr::select( iso, sector, fuel, pct_ems_accounted_for_tot:pct_ems_accounted_for_2000 ) %>%
        writeData( domain = 'DIAG_OUT',
                   fn = paste0( em, '_CEDS_point_source_completeness_metric' ),
                   meta = FALSE )

}

# End ----------------------------------------------------------------------
logStop()

