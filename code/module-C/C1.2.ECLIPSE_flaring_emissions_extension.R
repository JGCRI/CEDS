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
    mcl <- readData( 'MAPPINGS', 'Master_Country_List' )

# read in the population data
    pop_raw <- readData( "MED_OUT", "A.UN_pop_master" )

# If em = N2O, load EDGAR v5 NOx and N2O data
  if( em == "N2O" ){

      vn <- "5.0" # EDGAR data version number
      domain <- "EM_INV" # Input domain
      domain_ext <- "EDGAR/" # Input domain ext.

      EDGAR_years <- EDGAR_start_year : EDGAR_end_year
      X_EDGAR_years <- paste0( 'X', EDGAR_start_year : EDGAR_end_year )

      # N2O
      fn <- c( paste0( "v",  gsub( "[.]", "", vn ), "_", em, "_", EDGAR_start_year, "_",
                       EDGAR_end_year ), ".xls")
      sheet_to_use <- paste0( "v", vn, "_EM_", em, "_IPCC1996" )

      # NOx
      fn_NOx <- c( paste0( "v",  gsub( "[.]", "", vn ), "_", "NOx", "_", EDGAR_start_year, "_",
                           EDGAR_end_year ), ".xls")
      sheet_to_use_NOx <- paste0( "v", vn, "_EM_", "NOx", "_IPCC1996" )


      rows_to_skip <- 9

      EDGAR_N2O <- readData( domain, domain_extension = domain_ext,
                         file_name = fn[ 1 ], extension = fn[ 2 ],
                         sheet_selection = sheet_to_use, skip = rows_to_skip,
                         missing_value = c( "", "NULL" ) )

      EDGAR_NOx <- readData( domain, domain_extension = domain_ext,
                             file_name = fn_NOx[ 1 ], extension = fn_NOx[ 2 ],
                             sheet_selection = sheet_to_use_NOx, skip = rows_to_skip,
                             missing_value = c( "", "NULL" ) )

  }

# ------------------------------------------------------------------------------

# 2. Define constants used within this script
if( em == "N2O" ){

#   EDGAR inventory years
    EDGAR_INV_START_YEAR <- EDGAR_start_year # from common_data.R
    EDGAR_INV_END_YEAR <- EDGAR_end_year # from common_data.R
    EDGAR_INV_YEARS <- EDGAR_INV_START_YEAR : EDGAR_INV_END_YEAR # Currently: 1970-2008
    X_EDGAR_INV_YEARS <- paste0( "X", EDGAR_INV_YEARS )          # Currently: X1970-X2008

#   EDGAR extended years (years beyond EDGAR years which are in CEDS final emissions).
#   Used for estimating fugitive N2O emissions from fugitive NOx emissions using EDGAR emissions ratios.
    MISSING_EDGAR_EARLY_YEARS <- subset( emissions_years, emissions_years < EDGAR_INV_START_YEAR ) # Currently: 1960-1969
    X_MISSING_EDGAR_EARLY_YEARS <- paste0( "X", MISSING_EDGAR_EARLY_YEARS )                        # Currently: X1960-X1969
    MISSING_EDGAR_LATE_YEARS <- subset( emissions_years, emissions_years > EDGAR_INV_END_YEAR )    # Currently: 2009-2014
    X_MISSING_EDGAR_LATE_YEARS <- paste0( "X", MISSING_EDGAR_LATE_YEARS )                          # Currently: X2009-X2014

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
   MCL_clean <- mcl %>%
       dplyr::select( iso, final_data_flag ) %>%
       dplyr::distinct( ) %>%
       dplyr::filter( final_data_flag == 1 | iso %in% c( "srb (kosovo)", "gum" ) ) %>%
       dplyr::filter( iso != "global" ) %>%
       dplyr::select( -final_data_flag )

#   Define function for cleaning EDGAR data
    EDGAR_clean_for_ratio <- function( EDGAR_data_in, EDGAR_years, MCL_final_isos, EDGAR_em ){
#       TODO: Note the scg work below - this may not be necessary in future EDGAR versions

        EDGAR_clean <- EDGAR_data_in %>%
            dplyr::select( ISO_A3, Name, IPCC, IPCC_description, EDGAR_years ) %>%
            dplyr::rename( iso = ISO_A3 ) %>%
            dplyr::mutate( iso = tolower( iso ) ) %>%
            # As of EDGAR v5, this is the only petr. and gas fugitive EDGAR sector for N2O and NOx
            dplyr::filter( IPCC == "1B2", IPCC_description == "Fugitive emissions from oil and gas" )

#       Apply EDGAR region aggregate Serbia and Montengro data to each CEDS relevant iso, so that both isos get their aggregate region ratio
        EDGAR_scg_fix <- EDGAR_clean %>%
            dplyr::bind_rows( EDGAR_clean %>%
                                  dplyr::filter( iso == "scg" ) %>%
                                  dplyr::mutate( iso = "srb", Name =  "Serbia" ) ) %>%
            dplyr::bind_rows( EDGAR_clean %>%
                                  dplyr::filter( iso == "scg" ) %>%
                                  dplyr::mutate( iso = "srb (kosovo)", Name =  "Kosovo" ) ) %>%
            dplyr::mutate( iso = if_else( iso == "scg",  "mne", iso ),
                           Name = if_else( Name == "Serbia and Montenegro", "Montenegro", Name ) ) %>%
            dplyr::filter( iso %in% MCL_final_isos$iso ) %>%
            dplyr::filter_at( .vars = EDGAR_years, any_vars( !is.na( . ) ) ) %>%
            tidyr::gather( year, emissions, EDGAR_years ) %>%
            rename_at( .vars = "emissions",  funs( paste0( EDGAR_em, "_emissions" ) ) )

            return( EDGAR_scg_fix )

}

#   Process Edgar N2O and NOx data
    names( EDGAR_N2O ) <- c( names( EDGAR_N2O[ 1 : 6 ] ),
                               paste0( "X", names( EDGAR_N2O[ 7 : length( EDGAR_N2O ) ] ) ) ) # Add X's to year columns for EDGAR v5
    names( EDGAR_NOx ) <- c( names( EDGAR_NOx[ 1 : 6 ] ),
                             paste0( "X", names( EDGAR_NOx[ 7 : length( EDGAR_NOx ) ] ) ) ) # Add X's to year columns for EDGAR v5

    EDGAR_N2O_clean <- EDGAR_clean_for_ratio( EDGAR_N2O, X_EDGAR_INV_YEARS, MCL_clean, "N2O" )
    EDGAR_NOx_clean <- EDGAR_clean_for_ratio( EDGAR_NOx, X_EDGAR_INV_YEARS, MCL_clean, "NOx" )

#   Combine EDGAR N2O and NOx data into 1 data frame
    EDGAR_N2O_and_NOx <- EDGAR_N2O_clean %>%
        dplyr::full_join( EDGAR_NOx_clean, by = c( "iso", "Name", "IPCC", "IPCC_description", "year" ) )

    if( any( EDGAR_N2O_clean$iso %!in% EDGAR_N2O_and_NOx$iso ) | any( EDGAR_NOx_clean$iso %!in% EDGAR_N2O_and_NOx$iso ) ){

        stop( "Combined EDGAR NOx and N2O data is missing isos from either the NOx data or the N2O data..." )

    }

#   Overwrite NOx or NO2 data with NA for a given iso + year combination if either NOx or N2O data is NA for the combination,
#   as ratios will not be able to be computed for such combinations and a global default for the year will be provided.
    emissions_columns <- c( "N2O_emissions", "NOx_emissions")

    EDGAR_N2O_and_NOx_NA_fix <- EDGAR_N2O_and_NOx %>%
        dplyr::mutate_at( emissions_columns, funs( if_else( is.na( N2O_emissions ) |
                                                            is.na( NOx_emissions ), NA_real_, . ) ) )

#   Make global summed emissions for each year and global ratio
#   TODO: use regional ratios for isos which do not have ratio from EDGAR, instead of global ratio
    EDGAR_N2O_NOx_global <- EDGAR_N2O_and_NOx_NA_fix %>%
        dplyr::select( -iso, -Name ) %>%
        dplyr::filter( !is.na( N2O_emissions ), !is.na( NOx_emissions ) ) %>%
        dplyr::group_by( IPCC, IPCC_description, year ) %>%
        dplyr::summarise_all( sum, na.rm = TRUE ) %>%
        dplyr::ungroup( ) %>%
        dplyr::mutate( Ratio_N2O_per_NOx_global = N2O_emissions / NOx_emissions ) %>%
        dplyr::select( -N2O_emissions, -NOx_emissions )

#   Make ratio for each iso + year combo
    EDGAR_national_ratio <- EDGAR_N2O_and_NOx_NA_fix %>%
        dplyr::mutate( Ratio_N2O_per_Nox = N2O_emissions / NOx_emissions )

#   Add any missing final CEDS isos
    if( any( MCL_clean$iso %!in% EDGAR_national_ratio$iso ) ){

        missing_isos <- subset( MCL_clean$iso, MCL_clean$iso %!in% EDGAR_national_ratio$iso )

        missing_isos_df <- missing_isos %>%
            as.data.frame( ) %>%
            dplyr::rename( iso = "." ) %>%
            dplyr::mutate_at( X_EDGAR_INV_YEARS, funs( identity( NA_real_ ) ) ) %>%
            tidyr::gather( year, Ratio_N2O_per_Nox, X_EDGAR_INV_YEARS ) %>%
            dplyr::mutate( IPCC = "1B2", IPCC_description = "Fugitive emissions from oil and gas" ) %>%
            dplyr::mutate( iso = as.character( iso ) )

        EDGAR_national_ratio <- EDGAR_national_ratio %>%
            dplyr::bind_rows( missing_isos_df )


    }

#   Add global ratios where NA and for isos not in EDGAR
    EDGAR_national_ratio_with_global_ratio_fix <- EDGAR_national_ratio %>%
        dplyr::left_join( EDGAR_N2O_NOx_global, by = c( "IPCC", "IPCC_description", "year" ) ) %>%
        dplyr::mutate( Ratio_N2O_per_Nox = if_else( is.na( Ratio_N2O_per_Nox ),
                                                    Ratio_N2O_per_NOx_global, Ratio_N2O_per_Nox ) ) %>%
        dplyr::select( iso, year, Ratio_N2O_per_Nox )

#   Extend ratios forwards and backwards to cover all CEDS years
    EDGAR_ratios_extended <- EDGAR_national_ratio_with_global_ratio_fix %>%
        tidyr::spread( year, Ratio_N2O_per_Nox ) %>%
        dplyr::mutate_at( X_MISSING_EDGAR_EARLY_YEARS, funs( identity( !!rlang::sym( first( X_EDGAR_INV_YEARS ) ) ) ) ) %>%
        dplyr::mutate_at( X_MISSING_EDGAR_LATE_YEARS, funs( identity( !!rlang::sym( last( X_EDGAR_INV_YEARS ) ) ) ) ) %>%
        dplyr::select( iso, X_emissions_years ) %>%
        tidyr::gather( year, Ratio_N2O_per_Nox,  X_emissions_years )

#   Apply Ratio to emissions
    flaring_extended <- flaring_extended %>%
        tidyr::gather( year, NOx_emissions, extend_Xyears ) %>%
        dplyr::left_join( EDGAR_ratios_extended, by = c( "iso", "year" ) ) %>%
        dplyr::mutate( N2O_emissions = NOx_emissions * Ratio_N2O_per_Nox ) %>%
        dplyr::mutate( em = "N2O" ) %>%
        dplyr::select( -NOx_emissions, -Ratio_N2O_per_Nox ) %>%
        tidyr::spread( year, N2O_emissions )

#   Spread ratios back to wide format
    EDGAR_N2O_NOx_ratios_wide <- EDGAR_ratios_extended %>%
        tidyr::spread( year, Ratio_N2O_per_Nox ) %>%
        dplyr::mutate( units = "EDGAR fugitive oil and gas ratio - N2O / NOx " ) %>%
        dplyr::select( iso, units, X_emissions_years )

 }

# -----------------------------------------------------------------------------
# 6. Write output
    writeData( flaring_extended , "MED_OUT", paste0( "C.", em, "_ECLIPSE_flaring_emissions_extended" ) )

if( em != "N2O" ){

    writeData( flaring_ratio , "DIAG_OUT", paste0( "C.", em, "_ECLIPSE_flaring_to_crude_oil_production_ratios" ) )

}

if( em == "N2O" ){

    writeData( EDGAR_N2O_NOx_ratios_wide , "MED_OUT", paste0( "C.", em, "_EDGAR_NOx_N2O_fugitive_oil_NG_ratios" ) )

}

# Every script should finish with this line:
    logStop()

