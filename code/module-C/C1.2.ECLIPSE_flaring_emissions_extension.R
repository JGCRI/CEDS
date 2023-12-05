#------------------------------------------------------------------------------
# Program Name: C1.2.ECLIPSE_flaring_emissions_extension.R
# Author: Leyang Feng, Patrick O'Rourke, Hamza Ahsan
# Date Last Modified: December 21, 2020
# Program Purpose: Extends ECLIPSE flaring emissions using Hyde (last year 1800)
#                  and IEA crude oil production data
# Input Files: [em]_eclipse_flr_emissions.csv, A.crude_oil_production_data,
#              Master_Country_List.csv, A.UN_pop_master.csv
# Output Files: C.[em]_ECLIPSE_flaring_emissions_extended.csv,
# "             C.[em]_ECLIPSE_flaring_to_crude_oil_production_ratios.csv (for ems other than N2O)
#
# TODO: 1. About 70 countries in [em]_eclipse_flr_emissions.csv get picked out in the routine
#          because IEA/BP oil production data does not include those countries. Update when
#          a more comprehensive database of historical production is available, or when BP/IEA
#          oil production is disaggregated
#-------------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
    headers <- c( 'data_functions.R' ) # Any additional function files required
    log_msg <- "Extension of ECLIPSE flaring emissions" # First message to be printed to the log
    script_name <- "C1.2.ECLIPSE_flaring_emissions_extension.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define emission species and read in files

# Define emissions species variable
    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "N2O"

    MODULE_C <- "../code/module-C/"

    em_use <- em
    if( em == "N2O" ){ em_use <- "NOx" }

# read in the ECLIPSE flaring data
    ECLIPSE_flaring <- readData( "EM_INV", domain_extension = "ECLIPSE-flaring/", file_name = paste0( em_use, '_eclipse_flr_emissions' ) )

# read in crude oil production driver data (based off of IEA and Hyde oil production data)
    oil_production <- readData( 'MED_OUT', file_name = 'A.crude_oil_production_data' )

# read in master country list
    MCL <- readData( 'MAPPINGS', 'Master_Country_List' )

# read in the population data
    pop_raw <- readData( "MED_OUT", "A.UN_pop_master" )

# If em = N2O, load EDGAR v5 NOx and N2O data
  if( em == "N2O" ){

      domain <- "EM_INV" # Input domain
      domain_ext <- "EDGAR/" # Input domain ext.

      EDGAR_N2O <- readData( domain = "MED_OUT", file_name = paste0( "E.N2O_EDGAR" ))

      EDGAR_NOx <-  readData( domain = "MED_OUT", file_name = paste0( "E.NOx_EDGAR" ))

  }

# ------------------------------------------------------------------------------
# 3. Pre-processing
# 3.1. pre-processing of Hyde/IEA data

    oil_production_iso <- oil_production$iso

# 3.2. pre-processing of ECLIPSE flaring data
    flaring <- ECLIPSE_flaring[ c( 'iso', 'X1990', 'X2000', 'X2010' ) ]
    drop_iso_list <- c( )
    for ( row_index in 1 : nrow( flaring ) ) {
      a_row <- flaring[ row_index , ]
      a_row_iso <- a_row$iso
      if ( a_row$X1990 == 0 & a_row$X2000 == 0 & a_row$X2010 == 0 ) { drop_iso_list <- c( drop_iso_list, a_row_iso ) }
    }
    flaring <- flaring[ flaring$iso %!in% drop_iso_list, ]
    flaring_iso <- flaring$iso


# 3.3. combine countries in IEA/Hyde oil production and ECLIPSE_flaring
# extract common countries between ECLIPSE flaring and IEA/Hyde crude oil production data
    flr_IH_iso <- intersect( oil_production_iso, flaring_iso )
    flaring_flr_IH_iso <- flaring[ flaring$iso %in% flr_IH_iso, ]
    flaring_eclipse <- flaring_flr_IH_iso[ order( flaring_flr_IH_iso$iso ),  ]
    IH_flr_IH_iso <- oil_production[ oil_production$iso %in% flr_IH_iso, ]
    flaring_IH <- IH_flr_IH_iso[ order( IH_flr_IH_iso$iso ), ]

    extend_years <- 1800 : end_year
    extend_Xyears <- paste0( 'X', extend_years )

# ------------------------------------------------------------------------------
# 4. Extending
# method: The ECLIPSE flaring only has data for year 1990, 2000, 2010 while the Hyde/IEA crude oil production has
#         time series data from 1800 to end_year. The extending procedure first calculates ratios between
#         ECLIPSE data and Hyde/IEA data for available years ( 1990, 2000, 2010 ), then extends the ratios to
#         all years( 1800 - end_year ) using linear method and multiply the ratios to Hyde/IEA data to have
#         full time series data of ECLIPSE flaring.
    flaring_ratio <- data.frame( iso = flaring_eclipse$iso, X1990 = ( flaring_eclipse$X1990 / flaring_IH$X1990 ),
                                    X2000 = ( flaring_eclipse$X2000 / flaring_IH$X2000 ),
                                    X2010 = ( flaring_eclipse$X2010 / flaring_IH$X2010 ), stringsAsFactors = F )
    flaring_ratio[ is.na( flaring_ratio ) ] <- 0
    temp_matrix <- data.matrix(  flaring_ratio[ , c( 'X1990', 'X2000', 'X2010' ) ] )
    temp_matrix[ is.infinite( temp_matrix ) ] <- 0
    temp_matrix <- as.data.frame( temp_matrix )
    flaring_ratio <- cbind( flaring_ratio$iso, temp_matrix )
    colnames( flaring_ratio ) <- c( 'iso', 'X1990', 'X2000', 'X2010' )
    flaring_ratio$iso <- as.character( flaring_ratio$iso )

    year <- c( 1990, 2000, 2010 )
    flaring_extended_ratios <- data.frame()
    for ( row_index in 1 : nrow( flaring_ratio ) ) {
      ratio <- unlist( flaring_ratio[ row_index, c( 'X1990', 'X2000', 'X2010' ) ] )
      linear_reg <- lm( ratio ~ year )
      extended_ratios <- linear_reg$coefficients[[ 2 ]] * extend_years + linear_reg$coefficients[[ 1 ]]
      flaring_extended_ratios <- rbind( flaring_extended_ratios, extended_ratios )
    }
    flaring_extended <- flaring_IH[ , extend_Xyears ] * flaring_extended_ratios
    flaring_extended[ flaring_extended < 0 ] <- 0
    flaring_extended <- cbind( flaring_ratio$iso, flaring_extended )
    colnames(  flaring_extended ) <- c( 'iso', extend_Xyears )
    flaring_extended$iso <- as.character( flaring_extended$iso )

    flaring_extended$em <- em_use
    flaring_extended$sector <- '1B2c_Venting-flaring-oil-gas'
    flaring_extended$units <- 'kt'
    flaring_extended <- flaring_extended[ , c( 'iso', 'em', 'sector', 'units', extend_Xyears ) ]

    # Clean-up data
    data_columns <- names( flaring_extended )[5:length(names(flaring_extended))]
    flaring_extended <- removeNARows(flaring_extended, data_columns )
    flaring_extended <- NAsToZeros(flaring_extended, data_columns )

# -----------------------------------------------------------------------------
# 5. For N2O emissions, convert NOx emissions using ratio of Edgar 1B2 (Fugitive
#    emissions from oil and gas) NOx to N2O emissions
# TODO: This routine could be functionalized more with the similar routine within
#       C1.2.GAINS_fugitive_petr_gas_emissions.R
# TODO: handle non-final CEDS isos (remove or add to other isos) and final CEDS isos
#       which need to be disaggregated from other data, where appropriate
 if( em == "N2O" ){

    printLog( "Using EDGAR N2O and NOx data to produce default venting and flaring N2O emissions, as ECLIPSE only provides",
              "NOx data for this sector..." )

   #  Make a list of unique final isos (isos with final_data_flag = 1, srb (kosovo),
   #  and gum)
   MCL_clean <- MCL %>%
       dplyr::select( iso, final_data_flag ) %>%
       dplyr::distinct( ) %>%
       dplyr::filter( final_data_flag == 1 | iso %in% c( "srb (kosovo)", "gum" ) ) %>%
       dplyr::filter( iso != "global" ) %>%
       dplyr::select( -final_data_flag )


   #   Calculate Regional/Global/National Ratios to fill in missing data
   EDGAR_N2O_NOx_Regions <- EDGAR_N2O %>% gather(year, N2O, - iso,-sector,-sector_description,-units) %>%
       left_join(EDGAR_NOx %>% gather(year, NOx, - iso,-sector,-sector_description,-units)) %>%
       select(iso, sector, units, sector_description, year, N2O, NOx) %>%
       filter(sector_description == "Fugitive emissions from oil and gas") %>%
       left_join(MCL %>% select(iso, Figure_Region)) %>%
       filter(!is.na(Figure_Region)) %>%  # CHANGE LATER
       group_by(Figure_Region, sector, units, sector_description, year) %>%
       summarize_if(is.numeric, sum, na.rm = TRUE) %>%
       mutate(N2O_NOx_ratio = N2O/NOx) %>%
       filter(N2O_NOx_ratio>0) %>%
       filter(is.finite(N2O_NOx_ratio) ) %>%
       mutate(ratio_region = N2O_NOx_ratio) %>%
       select(Figure_Region, sector, units, sector_description, year, ratio_region)

   EDGAR_N2O_NOx_Global <- EDGAR_N2O %>% gather(year, N2O, - iso,-sector,-sector_description,-units) %>%
       left_join(EDGAR_NOx %>% gather(year, NOx, - iso,-sector,-sector_description,-units)) %>%
       select(sector, units, sector_description, year, N2O, NOx) %>%
       filter(sector_description == "Fugitive emissions from oil and gas") %>%
       group_by(sector, units, sector_description, year) %>%
       summarize_if(is.numeric, sum, na.rm = TRUE) %>%
       mutate(N2O_NOx_ratio = N2O/NOx) %>%
       filter(N2O_NOx_ratio>0) %>%
       filter(is.finite(N2O_NOx_ratio) ) %>%
       mutate(ratio_global = N2O_NOx_ratio) %>%
       ungroup() %>%
       select(sector, units, year, ratio_global)

   EDGAR_N2O_NOx_National <- EDGAR_N2O %>% gather(year, N2O, - iso,-sector,-sector_description,-units) %>%
       left_join(EDGAR_NOx %>% gather(year, NOx, - iso,-sector,-sector_description,-units)) %>%
       filter(sector_description == "Fugitive emissions from oil and gas") %>%
       select(iso, units, year, N2O, NOx) %>%
       group_by(iso, units, year) %>%
       summarize_if(is.numeric, sum, na.rm = TRUE) %>%
       mutate(N2O_NOx_ratio = N2O/NOx) %>%
       filter(N2O_NOx_ratio>0) %>%
       filter(is.finite(N2O_NOx_ratio) ) %>%
       mutate(ratio_national = N2O_NOx_ratio) %>%
       select(iso, units, year, ratio_national)

   #   Calculate iso-sector N2O/NOx ratio
   #   Fill in NA for any uncalculated ratios in the following order:
   #   - Region Sector ratio
   #   - Global Sector ratio
   #   - national ratio
   #   - extend ratio forward

   # Define empty columns to add 1960-1969
   addYearColumns <- paste("X", start_year:(EDGAR_start_year-1), sep = "")

   EDGAR_N2O_and_NOx <- EDGAR_N2O %>% gather(year, N2O, - iso,-sector,-sector_description,-units) %>%
       left_join(EDGAR_NOx %>% gather(year, NOx, - iso,-sector,-sector_description,-units)) %>%
       left_join(MCL %>% select(iso, Figure_Region)) %>%
       filter(sector_description == "Fugitive emissions from oil and gas") %>%
       mutate(N2O_NOx_ratio = N2O/NOx) %>%
       mutate(N2O_NOx_ratio = ifelse(N2O == 0 & NOx == 0, 0, N2O_NOx_ratio)) %>%
       mutate(N2O_NOx_ratio = ifelse(N2O != 0 & NOx == 0, NA, N2O_NOx_ratio)) %>%
       left_join( EDGAR_N2O_NOx_Regions) %>%
       left_join( EDGAR_N2O_NOx_Global) %>%
       left_join( EDGAR_N2O_NOx_National) %>%
       mutate( N2O_NOx_ratio = ifelse(!is.na(N2O_NOx_ratio), N2O_NOx_ratio,ratio_region)) %>%
       mutate( N2O_NOx_ratio = ifelse(!is.na(N2O_NOx_ratio), N2O_NOx_ratio,ratio_global)) %>%
       mutate( N2O_NOx_ratio = ifelse(!is.na(N2O_NOx_ratio), N2O_NOx_ratio,ratio_national)) %>%
       select(iso, sector, units, sector_description, year, N2O_NOx_ratio) %>%
       unique() %>%
       spread(year, N2O_NOx_ratio) %>%
       tibble::add_column(!!!set_names(as.list(rep(NA, length(addYearColumns))),nm=addYearColumns)) %>%
       select(iso, sector, units, paste0('X',start_year:end_year))

   #   Add any missing final CEDS isos
   if( any( MCL_clean$iso %!in% EDGAR_N2O_and_NOx$iso ) ){
       # Missing isos
       missing_isos <- subset( MCL_clean$iso, MCL_clean$iso %!in% EDGAR_N2O_and_NOx$iso )
       # make data frame of missing isos with global n20 ratio
       missing_isos_df <- missing_isos %>%
           dplyr::as_data_frame(  ) %>%
           dplyr::rename( iso = value ) %>%
           bind_cols(do.call("rbind", replicate(length(missing_isos),
                                                EDGAR_N2O_NOx_Global %>% spread(year, ratio_global),
                                                simplify = FALSE)))
       # Add missing isos to final N20 ratio
       EDGAR_ratio_final <- EDGAR_N2O_and_NOx %>%
           dplyr::bind_rows( missing_isos_df )
       # Check all countries are in df
       if( any( MCL_clean$iso %!in% EDGAR_ratio_final$iso ) ) stop('Not all isos are in the N20-NOx ratio data frame - even after replacing.')
   }else(EDGAR_ratio_final <-  EDGAR_N2O_and_NOx)

   #   Extend ratios forwards and backwards to cover all CEDS years
   EDGAR_ratio_final[ paste0('X',EDGAR_start_year:end_year) ] <- t(na.locf(t(EDGAR_ratio_final[ paste0('X',EDGAR_start_year:end_year) ])))
   EDGAR_ratio_final[ paste0('X',start_year:end_year) ] <- t(na.locf(t(EDGAR_ratio_final[ paste0('X',start_year:end_year) ]),fromLast = TRUE))

   if( any(is.na(EDGAR_ratio_final)) ) stop("NA's in final EDGAR N20-NOx ratio")

   #   Apply Ratio to emissions
   flaring_extended <- flaring_extended %>%
       select(-em) %>%
       gather(year, NOx_emissions,-iso,-sector,-units) %>%
       dplyr::left_join( EDGAR_ratio_final %>% gather(year, ratio,-iso,-sector,-units) ) %>%
       dplyr::mutate( N2O_emissions = NOx_emissions * ratio ) %>%
       dplyr::select( -NOx_emissions, -ratio ) %>%
       mutate(em = 'N2O') %>%
       tidyr::spread( year, N2O_emissions )

 } #end N2O if statement
##################
# -----------------------------------------------------------------------------
# 6. Write output
    writeData( flaring_extended , "MED_OUT", paste0( "C.", em, "_ECLIPSE_flaring_emissions_extended" ) )

if( em != "N2O" ){

    writeData( flaring_ratio , "DIAG_OUT", paste0( "C.", em, "_ECLIPSE_flaring_to_crude_oil_production_ratios" ) )

}

if( em == "N2O" ){

    writeData( EDGAR_ratio_final , "MED_OUT", paste0( "C.", em, "_EDGAR_NOx_N2O_fugitive_oil_NG_ratios" ) )

}

# Every script should finish with this line:
    logStop()

