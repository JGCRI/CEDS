# ------------------------------------------------------------------------------
# Program Name: Compare_to_GAINS.R
# Author: Rachel Hoesly, Patrick O'Rourke
# Date Last Updated: July 14, 2020
# Program Purpose: Generate comparisons between GAINS and CEDS
# Input Files: [em]_total_CEDS_emissions.csv
#              Global by region-detail_emf30_[em]_wSE.csv
#              Master_Country_List.csv, emf-30_ctry_map.csv,
#              Master_Fuel_Sector_List.xlsx,
#              emf-30_comparison_sector_map-comb_vs_process.csv
# Output Files: GAINS_[em]_Global_Comparison.csv, GAINS_[em]_Global_Comparison_Residential.csv,
#               GAINS_[em]_Global_Comparison_Non-Residential.csv, GAINS_[em]_Global_Comparison_Combustion.csv,
#               GAINS_[em]_Regional_Comparison.csv, GAINS_[em]_Regional_Comparison_Residential.csv,
#               GAINS_[em]_Regional_Comparison_Non-Residential.csv, GAINS_[em]_Regional_Comparison_Combustion.csv,
#               GAINS_[em]_Global_Comparison.pdf, GAINS_[em]_Global_Comparison_Residential.pdf,
#               GAINS_[em]_Global_Comparison_Non-Residential.pdf, GAINS_[em]_Global_Comparison_Combustion.pdf,
#               GAINS_[em]_Regional_Comparison_All.pdf, GAINS_[em]_Regional_Comparison_Residential.pdf,
#               GAINS_[em]_Regional_Comparison_Non-Residential.pdf, GAINS_[em]_Regional_Comparison_Combustion.pdf
# TODO: 1. Replace emf-30_comparison_sector_map-comb_vs_process.csv with
#          emf-30_fuel_sector_map.csv
#       2. (Future) Replace functions with tidyverse equivalent where appropriate
#                   ( cast --> spread, melt --> gather, merge and match --> left_join,
#                   aggregate --> dplyr::summarize_all, etc. )
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if( "input" %in% dir( ) ) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", 'common_data.R', 'IO_functions.R' ) # Additional function files may be required.
log_msg <- "Comparing CEDS final emissions to GAINS..." # First message to be printed to the log
script_name <- "Compare_to_GAINS.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "NMVOC"

# Stop script if running for unsupported species
if ( em %!in% c( "BC", "CH4", "CO", "CO2", "NH3", "NMVOC", "NOx", "OC", "SO2" ) ) {

  stop ( paste( 'GAINS EMF is not supported for emission species', em ) )

}

# ---------------------------------------------------------------------------
# 0.5. Script Options

gains_end_year <- 2015 # GAINS historical end year

gains_years <- seq( from = GAINS_start_year, to = gains_end_year, by = 5 )
x_gains_years <- paste0( 'X', gains_years ) # Historical GAINS years

ceds_start_year <- ( as.numeric( GAINS_start_year ) - 5 )
ceds_years <- ceds_start_year : end_year
x_ceds_years <- paste0( 'X', ceds_years )

library( "scales" ) # Load scales, as function 'comma' doesn't work without
                    # doing so currently

# ---------------------------------------------------------------------------
# 1. Load files

# GAINS emissions

#   Define em string to use for GAINS data
    em_use <- em
    if ( em == "NMVOC" ){ em_use <- "VOC" }

#   Define other settings for GAINS data
    domain_use <- "EM_INV"
    domain_ext_use <- "GAINS/"

    emissions_file_name <- paste0( "Global by region-detail_emf30_", em_use, "_wSE" )
    if( em == "SO2" ){ emissions_file_name <- gsub( "_wSE", "_v2_wSE", emissions_file_name ) }

    emissions_rows_to_skip <- 9

#   Read in GAINS data
    gains_emissions <- readData( domain = domain_use, domain_extension = domain_ext_use,
                                 file_name = emissions_file_name, skip = emissions_rows_to_skip )

# CEDS emissions and master country list
ceds_emissions <- readData( domain = 'MED_OUT', file_name =  paste0( em,'_total_CEDS_emissions' ) )

Master_Country_List <- readData( domain = 'MAPPINGS', file_name = 'Master_Country_List' )

MFSL <- readData( domain = "MAPPINGS", file_name = "Master_Fuel_Sector_List", extension = ".xlsx",
                  sheet_selection = "Sectors" )

# Other mapping files
ctry_map <- readData( domain = 'MAPPINGS',  domain_extension = 'GAINS/',
                      file_name ='emf-30_ctry_map' )

GAINS_comb_sector_map <- readData( domain = 'GAINS_MAPPINGS',
                         file_name ='emf-30_comparison_sector_map-comb_vs_process' )

# ---------------------------------------------------------------------------
# 2. Process GAINS

# Remove leading white space from GAINS data and rename sector column
gains_emissions <- gains_emissions %>%
  dplyr::rename( Sector = 'EMF30.kt' ) %>%
  dplyr::select( Region, Sector, all_of(X_GAINS_years) ) %>% # ALl GAINS years
  # GAINS data can have tabs in front of all characters in the ID columns, which would need to be removed
  dplyr::mutate_at( .vars = c( "Region", "Sector" ), .funs = funs( gsub( "\t", "", . ) ) )

# Subset total and residential data
gains <- gains_emissions %>%
  dplyr::filter( Sector == "SUM" | grepl( "End_Use_Residential_", gains_emissions$Sector ) ) %>%
  dplyr::select( Region, Sector, all_of(x_gains_years) ) %>% # Selects only historical years
  dplyr::filter( Region != "Global" )

# Subset residential data
gains_resid <- gains %>%
    dplyr::filter( grepl( "End_Use_Residential_", Sector ) ) %>%
    dplyr::select( -Sector ) %>%
    dplyr::group_by( Region ) %>%
    dplyr::summarise_all( list( ~sum(., na.rm = T ) ) ) %>%
    dplyr::mutate( Sector = "Residential") %>%
    dplyr::arrange( Region ) %>%
    data.frame( )

# Subset total data
gains_tot <- gains %>%
    dplyr::filter( Sector == 'SUM' ) %>%
    dplyr::select( -Sector ) %>%
    dplyr::group_by( Region ) %>%
    dplyr::summarise_all( list( ~sum( ., na.rm = T ) ) ) %>%
    dplyr::mutate( Sector = "All" ) %>%
    dplyr::arrange( Region ) %>%
    data.frame()

# Subset non-residential data
gains_nonresid <- gains_tot %>%
  dplyr::mutate( Sector = "Non-Residential" )

gains_nonresid[, x_gains_years ] <-
  gains_tot[ , x_gains_years] -
  gains_resid[ , x_gains_years ]

# Subset GAINS combustion emissions
gains_comb <- gains_emissions %>%
    dplyr::select( Region, Sector, all_of(x_gains_years) ) %>%
    dplyr::filter( Region != "Global" ) %>%
    dplyr::left_join( GAINS_comb_sector_map, by = "Sector" )

if( any ( is.na( gains_comb$Type ) ) ){

    gains_comb_sectorsNotMapped <- gains_comb %>%
        dplyr::filter( is.na ( Type ) ) %>%
        dplyr::select( Sector, Type ) %>%
        dplyr::distinct( )

    printLog( gains_comb_sectorsNotMapped[[1]] )

    stop( paste0( "The above sectors in the GAINS emissions data have not been mapped to Type. ",
                  "See - emf-30_comparison_sector_map-comb_vs_process.csv - ." ) )

} else{

    printLog( "All sectors in the GAINS emissions data have been mapped to process or combustion emissions..." )

}

gains_comb_final <- gains_comb %>%
    dplyr::filter( Type == "comb" ) %>%
    dplyr::select( -Sector, -Note ) %>%
    dplyr::rename( Sector = Type ) %>%
    dplyr::group_by( Region, Sector ) %>%
    dplyr::summarise_all( list( ~sum( ., na.rm = T ) ) ) %>%
    dplyr::select( Region, all_of(x_gains_years), Sector ) %>%
    dplyr::mutate( Sector = "Combustion" ) %>%
    dplyr::ungroup( )

# Combine Residential, Non-Residential, combustion, and Total data
gains <- dplyr::bind_rows( gains_resid, gains_tot, gains_nonresid, gains_comb_final )
gains[ gains < 0 ] <- 0

# Only CO2 is reported in Mt rather than kt, so multiply by 10^3 to convert Mt to kt
if( em == 'CO2' ){ gains[ , x_gains_years] <- gains[ , x_gains_years] * 1000 }

# Gather to long form
gains_long <- gains %>%
    tidyr::gather( key = variable, value = value, all_of(x_gains_years) ) %>%
    dplyr::mutate( variable = as.factor( variable ) )

# Define variable inv (inventory)
gains_long$inv <- 'GAINS'

# ---------------------------------------------------------------------------
# 3. Process CEDS

# Remve CEDS sectors that do not correspond to GAINS sectors
not_gains_sectors <- c( '1A3ai_International-aviation',
                        '1A3aii_Domestic-aviation',
                        '1A3di_International-shipping',
                        '1A3dii_Domestic-navigation',
                        '1B2d_Fugitive-other-energy',
                        '6A_Other-in-total',
                        '6B_Other-not-in-total',
                        '7A_Fossil-fuel-fires',
                        '11A_Volcanoes',
                        '11B_Forest-fires',
                        '11C_Other-natural',
                        '5C_Waste-incineration',
                        '3D_Soil-emissions' )

ceds <- ceds_emissions[ which( ceds_emissions$sector %!in% not_gains_sectors )
                        , c('iso', 'sector', x_ceds_years )]

# Map to GAINS emf regions
ceds$Region <- ctry_map[ match( ceds$iso, ctry_map$iso ),'Region']
ceds <- dplyr::filter( ceds, !is.na( Region ) )

# Subset Combustion emissions
ceds_comb <- ceds %>%
    dplyr::left_join( MFSL, by = "sector" ) %>%
    dplyr::select( type, Region, all_of(x_ceds_years) ) %>%
    dplyr::group_by( type, Region ) %>%
    dplyr::summarise_all( list( ~sum( ., na.rm = T ) ) ) %>%
    dplyr::ungroup( ) %>%
    dplyr::rename( sector = type ) %>%
    dplyr::filter( sector == "comb") %>%
    dplyr::mutate( sector = "Combustion" )

# Subset data as residential or non-residential emissions
ceds$sector[ ceds$sector != "1A4b_Residential" ] <- "Non-Residential"
ceds$sector[ ceds$sector == "1A4b_Residential" ] <- "Residential"

ceds <- ceds %>%
    dplyr::select( -iso ) %>%
    dplyr::group_by( sector, Region ) %>%
    dplyr::summarise_all( list( ~sum( ., na.rm = T ) ) ) %>%
    data.frame( )

# Calculate total CEDS emissions
ceds_tot <- ceds %>%
    dplyr::mutate( sector = "All" ) %>%
    dplyr::group_by( sector, Region ) %>%
    dplyr::summarise_all( list( ~sum( ., na.rm = T ) ) ) %>%
    data.frame( )

# Combine Residential, Non-Residential, Combustion, and Total Emissions
ceds <- rbind( ceds, ceds_tot, ceds_comb )
names( ceds )[ names( ceds ) == "sector" ] <- "Sector"

# Gather to long format
ceds_long <- ceds %>%
    tidyr::gather( key = variable, value = value, all_of(x_ceds_years) ) %>%
    dplyr::mutate( variable = as.factor( variable ) )

ceds_long$inv <- 'CEDS'

# ---------------------------------------------------------------------------
# 4. Graph Global Comparison

# All Sectors

#   Prepare data
    global_long <- rbind( dplyr::filter( ceds_long, Sector == "All"),
                          dplyr::filter( gains_long, Sector == "All" ) )
    names(global_long) <- c( "Sector", "Region", "year" ,"total","inv" )
    global_long$year <- gsub( 'X', "", global_long$year )
    global_long$year <- as.numeric( global_long$year )

    global <- cast( global_long, inv+Sector ~ year, value = 'total', fun.aggregate = sum )[ c( 'inv', 'Sector', gains_years ) ]

#   Write out table
    writeData( global,'DIAG_OUT', paste0( 'GAINS_', em, '_Global_Comparison' ),
               domain_extension = 'ceds-comparisons/', meta = F )

#   Plot
    df <- global_long[ ,c( 'inv','year','total' )]
    df <- aggregate( df['total'], by = list( inv = df$inv, year = df$year ), sum )
    df$inv <- as.factor( df$inv )
    max <- 1.2*( max( df$total ) )
    plot <- ggplot( df, aes( x = year, y = total, color = inv ) ) +
      geom_point( shape = 19 ) +
      geom_line( data = subset( df, inv == 'CEDS' ), size = 1, aes( x= year, y = total, color = inv ) ) +
      scale_x_continuous( breaks = seq( from = ceds_start_year, to = end_year, by = 5 ) )+
      scale_y_continuous( limits = c( 0, max ), labels = comma )+
      ggtitle( paste( 'Global', em, 'Emissions' ) )+
      labs( x = 'Year', y = paste( em,'Emissions [kt]' ) )
    plot
    ggsave( paste0( '../diagnostic-output/ceds-comparisons/GAINS_', em, '_Global_Comparison.pdf' ),
            width = 7, height = 4 )

# Residential sector

#   Prepare data

    global_long_resid <- rbind( dplyr::filter( ceds_long, Sector == "Residential" ),
                                dplyr::filter( gains_long, Sector == "Residential" ) )
    names( global_long_resid ) <- c( "Sector", "Region", "year" ,"total","inv" )
    global_long_resid$year <- gsub( 'X',"",global_long_resid$year )
    global_long_resid$year <- as.numeric( global_long_resid$year )

    global_resid <- cast( global_long_resid, inv+Sector ~ year, value = 'total', fun.aggregate = sum )[ c( 'inv', 'Sector', gains_years ) ]

#   Write out table
    writeData( global_resid, 'DIAG_OUT', paste0( 'GAINS_', em, '_Global_Comparison_Residential' ),
            domain_extension = 'ceds-comparisons/sector-level/', meta = F )

#   Plot
    df <- global_long_resid[ , c( 'inv','year','total' ) ]
    df <- aggregate(df[ 'total' ], by = list( inv = df$inv, year = df$year ), sum )
    df$inv <- as.factor( df$inv )
    max <- 1.2*( max( df$total ) )
    plot <- ggplot( df, aes( x = year, y = total, color = inv ) ) +
      geom_point( shape = 19 ) +
      geom_line( data = subset( df, inv == 'CEDS' ), size = 1, aes( x = year, y = total, color = inv ) ) +
      scale_x_continuous( breaks= seq( from = ceds_start_year, to = end_year, by = 5 ) )+
      scale_y_continuous( limits = c( 0, max ),labels = comma )+
      ggtitle( paste( 'Global', em, 'Emissions (Residential)' ) )+
      labs( x = 'Year', y = paste( em,'Emissions [kt]' ) )
    plot
    ggsave( paste0('../diagnostic-output/ceds-comparisons/sector-level/GAINS_', em,
                   '_Global_Comparison_Residential.pdf' ) , width = 7, height = 4 )

# Non-Residential

#   Prepare data

    global_long_nonresid <- rbind( dplyr::filter( ceds_long, Sector == "Non-Residential" ),
                                   dplyr::filter( gains_long, Sector == "Non-Residential" ) )
    names(global_long_nonresid) <- c( "Sector", "Region", "year" ,"total","inv" )
    global_long_nonresid$year <- gsub( 'X',"",global_long_nonresid$year )
    global_long_nonresid$year <- as.numeric( global_long_nonresid$year )

    global_nonresid <- cast( global_long_nonresid, inv+Sector ~ year, value = 'total', fun.aggregate = sum )[c( 'inv', 'Sector', gains_years ) ]

#   Write out table
    writeData( global_nonresid,'DIAG_OUT', paste0( 'GAINS_', em, '_Global_Comparison_Non-Residential' ),
               domain_extension = 'ceds-comparisons/sector-level/', meta = F )

#   Plot
    df <- global_long_nonresid[ ,c( 'inv','year','total' ) ]
    df <- aggregate( df[ 'total' ], by = list( inv = df$inv, year = df$year ), sum )
    df$inv <- as.factor( df$inv )
    max <- 1.2*( max( df$total ) )
    plot <- ggplot( df, aes( x = year, y = total, color = inv ) ) +
      geom_point( shape = 19 ) +
      geom_line( data = subset( df, inv == 'CEDS' ), size = 1, aes( x = year, y = total, color = inv ) ) +
      scale_x_continuous( breaks= seq( from = ceds_start_year, to = end_year, by = 5 ) ) +
      scale_y_continuous( limits = c( 0, max ),labels = comma ) +
      ggtitle( paste( 'Global', em, 'Emissions (Non-Residential)') ) +
      labs( x = 'Year',y = paste( em, 'Emissions [kt]') )
    plot
    ggsave( paste0( '../diagnostic-output/ceds-comparisons/sector-level/GAINS_', em,
                    '_Global_Comparison_Non-Residential.pdf') , width = 7, height = 4 )

# Combustion

#   Prepare data
    global_long_comb <- dplyr::bind_rows( dplyr::filter( ceds_long, Sector == "Combustion" ) %>%
                                              dplyr::mutate( variable = as.character( variable ) ),
                                          dplyr::filter( gains_long, Sector == "Combustion" ) %>%
                                              dplyr::mutate( variable = as.character( variable ) ) ) %>%
                        dplyr::rename( year = variable, total = value ) %>%
                        dplyr::mutate( year = gsub( "X", "", year ),
                                       year = as.numeric( year ) )

    global_long_comb_aggregated <- global_long_comb %>%
        dplyr::select( inv, Sector, year, total ) %>%
        dplyr::group_by( inv, Sector, year ) %>%
        dplyr::summarise_all( list( ~sum( ., na.rm = T ) ) ) %>%
        dplyr::ungroup( )

    global_comb <- global_long_comb_aggregated %>%
        tidyr::spread( year, total) %>%
        dplyr::select( inv, Sector, paste( gains_years ) )

#   Write out table
    writeData( global_comb,'DIAG_OUT', paste0( 'GAINS_', em, '_Global_Comparison_Combustion' ),
               domain_extension = 'ceds-comparisons/sector-level/', meta = F )

#   Plot
    df <- global_long_comb_aggregated %>%
        dplyr::select( -Sector ) %>%
        dplyr::mutate( inv = as.factor( inv ) )

    max <- 1.2*( max( df$total ) )

    plot <- ggplot( df, aes( x = year, y = total, color = inv ) ) +
        geom_point( shape = 19 ) +
        geom_line( data = subset( df, inv == 'CEDS' ), size = 1, aes( x = year, y = total, color = inv ) ) +
        scale_x_continuous( breaks= seq( from = ceds_start_year, to = end_year, by = 5 ) ) +
        scale_y_continuous( limits = c( 0, max ),labels = comma ) +
        ggtitle( paste( 'Global', em, 'Emissions (Combustion)') ) +
        labs( x = 'Year',y = paste( em, 'Emissions [kt]') )

    plot

    ggsave( paste0( '../diagnostic-output/ceds-comparisons/sector-level/GAINS_', em,
                    '_Global_Comparison_Combustion.pdf') , width = 7, height = 4 )

# ---------------------------------------------------------------------------
# 4. Graph Regional Comparison

# All sectors

#   Prepare data
    region_long <- global_long
    region_wide <- cast( region_long, Region+Sector+ inv ~ year, value = 'total' )
    region_wide <- region_wide[ c( 'Region', 'Sector', 'inv', gains_years )]

#   Write out table
    writeData( region_wide,'DIAG_OUT', paste0( 'GAINS_', em, '_Regional_Comparison' ),
               domain_extension = 'ceds-comparisons/', meta = F )

#   Plot
    regions_list <- region_long[ which( region_long$year == '2000'), c( 'Region','total' ) ]
    regions_list <- regions_list[ order( -regions_list$total ), ]
    regions_list_order <- unique( regions_list$Region )
    regions_df_order <- data.frame( Region = regions_list_order,
                                    plot = rep( 1 : 6, c( 4, 4, 4, 4, 5, 4 ) ) )
    plot_list <- list( )
    for( i in 1:6 ){

      plot_regions <- regions_df_order [ which( regions_df_order$plot == i ),'Region' ]

      plot_df <- region_long[ which( region_long$Region %in% plot_regions ),c( 'inv','year','Region','total' ) ]
      plot_df$inv <- as.factor( plot_df$inv )
      plot_df$region <- as.factor( plot_df$Region )
      max <- 1.2*( max( plot_df$total ) )

      plot <- ggplot( plot_df, aes( x = year,y = total, color = region, shape = inv ) ) +
        geom_point( data = subset(plot_df, inv == 'GAINS' ), size = 2, aes( x= year, y = total, color = Region ) ) +
        geom_line( data = subset(plot_df, inv == 'CEDS' ), size = 1, aes( x = year, y = total, color = Region ) ) +
        scale_x_continuous( breaks=seq( from = ceds_start_year, to = end_year, by = 5 ) ) +
        scale_y_continuous( limits = c( 0, max ), labels = comma ) +
        scale_shape_discrete( guide = FALSE ) +
        labs( x ='Year', y = paste( em, 'Emissions [kt]' ) ) +
        theme( legend.title = element_blank( ) )
      plot
      plot_list[[i]] <- plot
    }

    pdf( paste0( '../diagnostic-output/ceds-comparisons/GAINS_', em, '_Regional_Comparison_All.pdf'),
         width = 12, height = 10, paper = 'special' )
    grid.arrange( plot_list[[1]], plot_list[[2]],
                  plot_list[[3]], plot_list[[4]],
                  plot_list[[5]], plot_list[[6]], ncol = 2,
                  top = paste( 'GAINS vs CEDS - Regional', em, 'Emissions' ) )
    dev.off( )

# Residential

#   Prepare data
    region_long_resid <- global_long_resid
    region_wide_resid <- cast( region_long_resid, Region + Sector + inv ~ year, value = 'total' )
    region_wide_resid <- region_wide_resid[ c(  'Region', 'Sector', 'inv', gains_years ) ]

#   Write out table
    writeData( region_wide_resid,'DIAG_OUT', paste0( 'GAINS_', em, '_Regional_Comparison_Residential' ),
               domain_extension = 'ceds-comparisons/sector-level/', meta = F )

#   Plot
    regions_list <- region_long_resid[ which( region_long_resid$year == '2000'), c( 'Region','total' ) ]
    regions_list <- regions_list[ order( -regions_list$total ), ]
    regions_list_order <- unique( regions_list$Region )
    regions_df_order <- data.frame( Region = regions_list_order,
                                    plot = rep( 1 : 6, c( 4, 4, 4, 4, 5, 4 ) ) )

    plot_list <- list( )
    for( i in 1 : 6 ){

      plot_regions <- regions_df_order [ which( regions_df_order$plot == i ),'Region' ]

      plot_df <- region_long_resid[ which( region_long_resid$Region %in% plot_regions ),
                                    c( 'inv','year','Region','total' ) ]
      plot_df$inv <- as.factor( plot_df$inv )
      plot_df$region <- as.factor( plot_df$Region )
      max <- 1.2*( max( plot_df$total ) )

      plot <- ggplot( plot_df, aes( x = year, y = total, color = region, shape=inv ) ) +
        geom_point(data = subset( plot_df, inv =='GAINS' ),size = 2, aes( x = year, y = total, color = Region ) ) +
        geom_line(data = subset (plot_df, inv =='CEDS' ), size = 1, aes( x = year, y = total, color = Region ) ) +
        scale_x_continuous( breaks = seq( from = ceds_start_year, to = end_year, by = 5 ) ) +
        scale_y_continuous( limits = c( 0, max ),labels = comma ) +
        scale_shape_discrete( guide = FALSE ) +
        labs( x = 'Year',y = paste( em, 'Emissions [kt]' ) ) +
        theme( legend.title = element_blank( ) )
      plot
      plot_list[[i]] <- plot
    }

    pdf( paste0( '../diagnostic-output/ceds-comparisons/sector-level/GAINS_', em,
                 '_Regional_Comparison_Residential.pdf'), width = 12, height = 10, paper = 'special' )
    grid.arrange( plot_list[[1]], plot_list[[2]],
                  plot_list[[3]], plot_list[[4]],
                  plot_list[[5]], plot_list[[6]], ncol = 2,
                  top = paste( 'GAINS vs CEDS - Regional', em, 'Emissions (Residential)' ) )
    dev.off( )

# Non-Residential

#   Prepare data
    region_long_nonresid <- global_long_nonresid
    region_wide_nonresid <- cast(region_long_nonresid, Region + Sector + inv ~ year, value = 'total')
    region_wide_nonresid <- region_wide_nonresid[ c( 'Region', 'Sector', 'inv', gains_years ) ]

#   Write out table
    writeData( region_wide_nonresid, 'DIAG_OUT', paste0( 'GAINS_',em,'_Regional_Comparison_Non-Residential' ),
               domain_extension = 'ceds-comparisons/sector-level/', meta = F )

#   Plot
    regions_list <- region_long_nonresid[ which( region_long_nonresid$year == '2000' ),c( 'Region','total' ) ]
    regions_list <- regions_list[ order( -regions_list$total ), ]
    regions_list_order <- unique( regions_list$Region )
    regions_df_order <- data.frame( Region = regions_list_order,
                                    plot = rep( 1 : 6, c( 4, 4, 4, 4, 5, 4 ) ) )

    plot_list <- list( )
    for( i in 1 : 6 ){

      plot_regions <- regions_df_order [ which( regions_df_order$plot == i ),'Region' ]

      plot_df <- region_long_nonresid[ which( region_long_nonresid$Region %in% plot_regions ),
                                       c( 'inv','year','Region','total' ) ]
      plot_df$inv <- as.factor( plot_df$inv )
      plot_df$region <- as.factor( plot_df$Region )
      max <- 1.2*( max( plot_df$total ) )

      plot <- ggplot( plot_df, aes( x = year, y = total, color = region, shape = inv ) ) +
        geom_point ( data = subset( plot_df, inv == 'GAINS' ), size = 2, aes( x = year, y = total, color = Region ) ) +
        geom_line( data = subset( plot_df, inv =='CEDS'),size=1,aes(x=year, y = total, color = Region ) ) +
        scale_x_continuous( breaks = seq( from = ceds_start_year, to = end_year, by = 5 ) )+
        scale_y_continuous( limits = c( 0, max ), labels = comma ) +
        scale_shape_discrete( guide = FALSE )+
        labs (x = 'Year',y = paste( em, 'Emissions [kt]' ) ) +
        theme( legend.title = element_blank( ) )
      plot
      plot_list[[i]] <- plot
    }

    pdf( paste0( '../diagnostic-output/ceds-comparisons/sector-level/GAINS_', em,
                 '_Regional_Comparison_Non-Residential.pdf'), width = 12, height = 10, paper ='special' )
    grid.arrange( plot_list[[1]], plot_list[[2]],
                  plot_list[[3]], plot_list[[4]],
                  plot_list[[5]], plot_list[[6]], ncol = 2,
                  top = paste( 'GAINS vs CEDS - Regional', em, 'Emissions (Non-Residential)' ) )
    dev.off( )

# Combustion

#   Prepare data
    region_long_comb <- global_long_comb

    region_wide_comb <- region_long_comb %>%
        tidyr::spread( year, total) %>%
        dplyr::select( Region, Sector, inv, paste( gains_years ) )

#   Write out table
    writeData( region_wide_comb, 'DIAG_OUT', paste0( 'GAINS_',em,'_Regional_Comparison_Combustion' ),
               domain_extension = 'ceds-comparisons/sector-level/', meta = F )

#   Plot
    regions_list <- region_long_comb[ which( region_long_comb$year == '2000' ),c( 'Region','total' ) ]
    regions_list <- regions_list[ order( -regions_list$total ), ]
    regions_list_order <- unique( regions_list$Region )
    regions_df_order <- data.frame( Region = regions_list_order,
                                    plot = c( unlist( lapply ( X = 1 : 4, FUN = rep, times = 4 ) ),
                                              unlist( lapply( X = 5, FUN = rep, times = 5 ) ),
                                              unlist( lapply( X = 6, FUN = rep, times = 4 ) ) ) )

    plot_list <- list( )
    for( i in 1 : 6 ){

        plot_regions <- regions_df_order [ which( regions_df_order$plot == i ),'Region' ]

        plot_df <- region_long_nonresid[ which( region_long_nonresid$Region %in% plot_regions ),
                                         c( 'inv','year','Region','total' ) ]
        plot_df$inv <- as.factor( plot_df$inv )
        plot_df$region <- as.factor( plot_df$Region )
        max <- 1.2*( max( plot_df$total ) )

        plot <- ggplot( plot_df, aes( x = year, y = total, color = region, shape = inv ) ) +
            geom_point (data = subset( plot_df, inv == 'GAINS' ), size = 2, aes( x = year, y = total, color = Region ) ) +
            geom_line( data = subset( plot_df, inv =='CEDS'), size = 1, aes( x = year, y = total, color = Region ) ) +
            scale_x_continuous( breaks = seq( from = ceds_start_year, to = end_year, by = 5 ) )+
            scale_y_continuous( limits = c( 0, max ), labels = comma ) +
            scale_shape_discrete( guide = FALSE )+
            labs ( x = 'Year',y = paste( em, 'Emissions [kt]' ) ) +
            theme( legend.title = element_blank( ) )
        plot
        plot_list[[i]] <- plot
    }

    pdf( paste0( '../diagnostic-output/ceds-comparisons/sector-level/GAINS_', em,
                 '_Regional_Comparison_Combustion.pdf'), width = 12, height = 10, paper ='special' )
    grid.arrange( plot_list[[1]], plot_list[[2]],
                  plot_list[[3]], plot_list[[4]],
                  plot_list[[5]], plot_list[[6]], ncol = 2,
                  top = paste( 'GAINS vs CEDS - Regional', em, 'Emissions (Combustion)' ) )
    dev.off( )

# ---------------------------------------------------------------------------
# 5. End

logStop( )
