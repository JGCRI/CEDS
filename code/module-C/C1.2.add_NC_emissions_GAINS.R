# ------------------------------------------------------------------------------
# Program Name: C1.2.add_NC_emissions_GAINS.R
# Author(s): Patrick O'Rourke
# Date Last Modified: October 7, 2019
# Program Purpose: To reformat the non-combustion fugitive oil and gas emissions
#                  from GAINS.
# Input Files: GAINS_EMF30_EMISSIONS_extended_Ev5a_CLE_Nov2015.xlsx,
#              emf-30_ctry_map.csv, emf-30_fuel_sector_map.csv,
#              Master_Country_List.csv, E.CO2_CDIAC_inventory.csv,
#              BP_energy_data.xlsx, A.en_stat_sector_fuel.csv,
#              A.UN_pop_master.csv, EDGAR42_NOx.csv, EDGAR42_N2O.csv
# Output Files: C.GAINS_NC_Emissions_[em].csv, C.BP_and_IEA_oil_production.csv,
#               C.BP_and_IEA_natural_gas_production.csv, C.[em]_NC_emissions_db.csv
# TODO: 1) Address remaining doc TODOs
#       2) Needs updating once uses new BP or IEA data
# Notes:

# -----------------------------------------------------------------------------

# 0. Read in global settings and headers
#   Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
#   to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

#   Universal header file - provides logging, file support, etc.
    headers <- c( 'process_db_functions.R', 'data_functions.R',
                  'interpolation_extension_functions.R', 'common_data.R',
                  'analysis_functions.R' )

#   First message to be printed to the log
    log_msg <- paste0( "Processing GAINS non-combustion default oil and gas fugitive emissions data..." )

    script_name <- "C1.2.add_NC_emissions_GAINS.R"

    source( paste0( PARAM_DIR, "header.R" ) )

    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------

# 1. Define settings, script constants, and functions internal to this script

#   Define emissions species variable
    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "N2O"

#   Stop script if running for unsupported species
    if ( ! ( em %in% c( 'BC', 'CH4', 'CO', 'CO2', 'N2O', 'NMVOC', 'NOx', 'OC', 'SO2' ) ) ) {

        stop ( paste0( 'GAINS EMF-30 is not supported for emission species (', em,
                       '). Remove this em from this script within C1.2.add_NC_emissions.R ',
                       'and/or makefile' ) )

    }

#   Define emissions sheet for select ems that the sheet name doesn't match the em
    e_sheet <- em
    if ( em == "NMVOC" ){ e_sheet <- "VOC" }
    if ( em == "N2O" ){ e_sheet <- "NOx" } # Use NOx for N2O (as no N2O GAINS data)

#   Define years of interest

#       Years within GAINS emissions data, GAINS 1st year, and GAINS last year for extension and interpolation
        GAINS_EMISS_YEARS <- c( "2000", "2005", paste0( seq( 2010, 2030, 10 ) ), "2050" ) # 2000, 2005, 2010, 2020, 2030, 2050
        GAINS_START_YEAR <- GAINS_EMISS_YEARS[[1]]                                        # 2000
        GAINS_START_YEAR_X <- paste0( "X", GAINS_START_YEAR )                             # X2000
        LAST_GAINS_YEAR_FOR_INTERP <- 2020

#       Years within GAINS emissions data which are in CEDS, years needed to interpolate GAINS data, and GAINS data with interpolated years
#       Note: Keeping GAINS 2020 data for interpolation purposes
        GAINS_EMISS_YEARS_KEEP <- subset( GAINS_EMISS_YEARS, GAINS_EMISS_YEARS %in% start_year : LAST_GAINS_YEAR_FOR_INTERP ) # Currently: 2000, 2005, 2010, 2020
        GAINS_EMISS_YEARS_KEEP_X <- paste0( "X", GAINS_EMISS_YEARS_KEEP )                                                     # Currently: X2000, X2005, X2010, X2020
        GAINS_INTERP_YEARS_X <- paste0( "X", GAINS_START_YEAR : LAST_GAINS_YEAR_FOR_INTERP )                                  # Currently: X2000-X2020
        MISSING_GAINS_YEARS_TO_MAKE <- subset( GAINS_INTERP_YEARS_X, GAINS_INTERP_YEARS_X %!in% GAINS_EMISS_YEARS_KEEP_X )    # Currently: X2001-X2004, X2006-X2009, X2011-2019

#       Final GAINS years retained (after interpolation between 2010 and 2020, drop years after CEDS end year)
        GAINS_FINAL_YEARS_WITH_X <- paste0( "X", min( as.numeric( GAINS_EMISS_YEARS_KEEP ) ):end_year ) # Currently: X2000-X2014

#       CDIAC Years that are within CEDS years
        CDIAC_YEARS_X <- paste0( "X", start_year : cdiac_end_year ) # Currently X1960-X2011

#       CDIAC Years extended years (1960-2020) and years which = cdiac_end_year after extension
        CDIAC_YEARS_EXTEND_TO <- paste0( "X", ( cdiac_end_year + 1 ) :  LAST_GAINS_YEAR_FOR_INTERP )                     # Currently: X2012-X2020
        CDIAC_EXTENDED_YEARS_X <- c( CDIAC_YEARS_X, paste0( "X", ( cdiac_end_year + 1 ) : LAST_GAINS_YEAR_FOR_INTERP ) ) # Currently: X1960-X2020

#       BP and IEA years - for Oil and NG production
        BP_OIL_YEARS <- historical_end_extension_year : BP_years                   # Currently: 1965:2014
        BP_OIL_YEARS_x <- paste0( 'X', BP_OIL_YEARS )                              # Currently: X1965-X2014
        BP_GAS_YEARS <- 1970 : BP_years                                            # Currently: 1970-2014
        BP_GAS_YEARS_x <- paste0( 'X', BP_GAS_YEARS )                              # Currently: X1970-X2014
        IEA_PROD_YEARS_x <- X_emissions_years                                      # Currently: X1960-X2014, for after extending to 2014 (constant extension)
        IEA_NA_YEARS <- paste0( "X", start_year : 1970 )                           # Currently: X1960-X1970 the data is 0 for all isos for crude oil production
                                                                                   # TODO: This above note about NA IEA years may change with updated IEA data

#   Define GAINS fugitive sectors for oil and gas
    GAINS_FUGITIVE_SECTORS <- c( "Losses_Distribution_Use" , "Losses_Prod_Conventional_Gas",
                                 "Losses_Prod_Shale_Gas", "Losses_Prod_Oil",
                                 "Losses_Prod_Oil", "Losses_Prod_Oil" )

#   Define function to check if all values are NA for interpolating
#   Params: x -- an R atomic vector or list (including data frames)
#   TODO: This  function is within the master branch already as "all.na."
#         This can be deleted once this branch is updated with the master branch and its use replaced with all.na.
    all.na_new  <- function(x) return( all( is.na(x) ) )

#   Define function interpolate_NAs_new
#   Brief: Linearly interpolate over NA values
#   Author: Rachel hoesly and Caleb Braun
#   Parameters: df -- data frame of numeric values (no id columns)
#   Return: data frame of same size as df, with interpolated values
#   Input files: none
#   Output files: none
#   TODO: This function is within the master branch already as interpolate_NAs2.
#         This can be deleted once this branch is updated with the master branch and its use replaced with all.na.

    interpolate_NAs_new <- function(df) {

        if( !is.data.frame( df ) ) {

            warning( "interpolate_NAs expects a data frame; attempting to convert" )

            df <- as.data.frame( df )

        }

#       I.) Convert columns that are all NA to numeric
        df <- dplyr::mutate_if( df, function( x ) all.na_new( x ), as.numeric )

        if( length( rle( sapply( df, is.numeric ) )$lengths ) > 2) {

            warning( "interpolate_NAs found mixed numeric and non-numeric columns;
                    make sure all value columns are numeric")

        }

        value.cols <- sapply( df, is.numeric )
        df_flipped <- t( df[value.cols] )
        df[ , value.cols] <- t( na.approx( df_flipped, na.rm = F ) )

        return(df)

    }

# ------------------------------------------------------------------------------

# 2. Input

#   GAINS data and mapping files
    GAINS_emiss <- readData( domain = 'EM_INV', domain_extension = 'GAINS/',
                         file_name = 'GAINS_EMF30_EMISSIONS_extended_Ev5a_CLE_Nov2015',
                        ".xlsx", sheet_selection = e_sheet )

    GAINS_country_map <- readData( domain = 'MAPPINGS', domain_extension = 'GAINS/',
                      file_name ='emf-30_ctry_map' )

    GAINS_sector_map <- readData( domain = 'MAPPINGS', domain_extension = 'GAINS/',
                             file_name = 'emf-30_fuel_sector_map' )

#   Master Country List
    MCL <- readData( domain = 'MAPPINGS', file_name = 'Master_Country_List' )

#   CDIAC CO2 inventory
    CDIAC_CO2_inventory <- readData( domain = 'MED_OUT',
                                     file_name = 'E.CO2_CDIAC_inventory' )

#   BP oil (in millions of tonnes), NG production (Million tonnes oil equivalent)
    BP_oil_production <- readData( 'ENERGY_IN', file_name = 'BP_energy_data',
                                    extension = ".xlsx", sheet_selection = "Oil Production – Tonnes", skip_rows = 2 )

    BP_gas_production <- readData( 'ENERGY_IN', file_name = 'BP_energy_data',
                                   extension = ".xlsx", sheet_selection = "Gas Production – tonnes", skip_rows = 2 )

#   IEA crude oil and NG production data
    en_stat_sector_fuel <- readData( 'MED_OUT', file_name = 'A.en_stat_sector_fuel' )

#   UN Population data
    pop <- readData( "MED_OUT", "A.UN_pop_master" )

#   If em = N2O, load EDGAR 4.2 NOx and N2O data and mapping file
    if( em == "N2O" ){

        EDGAR_NOx_fle  <- paste0( 'EDGAR42_', e_sheet )
        EGAR_N2O_file <- paste0( 'EDGAR42_', em )
        inv_data_folder <- "EM_INV"

        EDGAR_NOx <- readData( inv_data_folder, domain_extension = "EDGAR/", EDGAR_NOx_fle )
        EDGAR_N2O <- readData( inv_data_folder, domain_extension = "EDGAR/", EGAR_N2O_file )

    }

# ------------------------------------------------------------------------------

# 3. Reformat mapping files

#   Reformat GAINS sector map
    GAINS_fugitive_sectors_df <- GAINS_sector_map %>%
        dplyr::select( emf_sector, ceds_sector ) %>%
        dplyr::filter( emf_sector %in% GAINS_FUGITIVE_SECTORS ) %>%
        dplyr::distinct( ) %>%
        dplyr::rename( Sector = emf_sector )

#   Mast list of unique final isos (isos with final_data_flag = 1, srb (kosovo),
#   and gum) and list of final isos with OECD vs Non-OECD flag
    MCL_clean <- MCL %>%
        dplyr::select( iso, final_data_flag,  OECD_flag ) %>%
        dplyr::distinct( ) %>%
        dplyr::filter( final_data_flag == 1 | iso %in% c( "srb (kosovo)", "gum" ) ) %>%
        dplyr::filter( iso != "global" )

    MCL_unique_with_OECD_flag <- MCL_clean %>%
        dplyr::select( -final_data_flag )

    MCL_clean <- MCL_clean %>%
        dplyr::select( -OECD_flag )

#   Reformat GAINS region map
    GAINS_country_map_clean <- GAINS_country_map %>%
        dplyr::select( emf_name, iso ) %>%
        dplyr::filter( !( is.na ( iso ) ) ) %>%
        dplyr::distinct( ) %>%
        dplyr::rename( Region = emf_name )

#   Check if all isos listed in GAINS region map are within the MCL, and
#   that all isos in the MCL are within the GAINS region map
#   TODO: Change this to the updated iso_check function once available
#         (iso_check in analysis_functions.R)
    unique_GAINS_map_isos <- sort( unique( GAINS_country_map_clean$iso ) )
    unique_MCL_isos <- sort( unique( MCL_clean$iso ) )

#   A.) Check if all isos listed in GAINS region map are within the MCL
    GAINS_map_isos_not_in_MCL <- subset( unique_GAINS_map_isos,
                                        !( unique_GAINS_map_isos %in% unique_MCL_isos ) )

    if( length( GAINS_map_isos_not_in_MCL ) != 0 ){

        printLog( GAINS_map_isos_not_in_MCL )

        stop( paste0( "The above isos are in the GAINS region mapping file but not ",
                          "final CEDS isos in Master_Country_List.csv... " ) )

    } else {

        printLog( "All isos in the in the GAINS region mapping are",
                      "final CEDS isos in Master_Country_List.csv..." )

    }

#   B.) Check if all isos in MCL (with final_data_flag of 1, srb(kosovo), and gum)
#   Are within the GAINS region mapping file
    MCL_isos_not_in_GAINS_map <- subset( unique_MCL_isos,
                                        !( unique_MCL_isos %in% unique_GAINS_map_isos ) )

    if( length( MCL_isos_not_in_GAINS_map ) != 0 ){

        printLog( MCL_isos_not_in_GAINS_map )

        stop( paste0( "The above isos are final CEDS isos in Master_Country_List.csv ",
                              "but not found in the GAINS region mapping file...") )

    } else {

            printLog( "All final CEDS isos in Master_Country_List.csv are within",
                      "the GAINS region mapping file...")

    }

#   Check if there are any duplicated CEDS isos left in the GAINS region
#   map (more than one GAINS region mapped to 1 CEDS iso).
    if ( nrow ( GAINS_country_map_clean) !=  length ( unique_GAINS_map_isos ) ){

        duplicated_GAINS_map_isos <- GAINS_country_map_clean %>%
            dplyr::count( iso ) %>%
            dplyr::filter( n > 1)

        printLog ( "Duplicated CEDS isos:", duplicated_GAINS_map_isos[[1]] )

        stop (  paste0 ("The above isos are duplicated within ",
                            "the GAINS region mapping file (emf-30_ctry_map)." ) )

    } else{

            printLog ( "There are no duplicated isos in the GAINS region mapping file",
                       "(emf-30_ctry_map)." )

    }

# ------------------------------------------------------------------------------

# 4. Reformat GAINS emissions data

#   Initial Reformatting
    GAINS_emiss_clean <- GAINS_emiss %>%
        tidyr::gather( key = years, value = Emissions, GAINS_EMISS_YEARS ) %>%
        dplyr::filter( years %in% GAINS_EMISS_YEARS_KEEP,
                       Region != "Global" )

#   Provide units - note that CO2 emissions are provided as Tg rather than GG or kt
#   (as noted in B1.1.base_comb_GAINS_EMF-30.R) and must be converted.
    if( e_sheet == 'CO2' ){

        kt_per_Tg <- 1000

        GAINS_emiss_UnitsFixed <- GAINS_emiss_clean %>%
            dplyr::mutate( Emissions_convert_unit = Emissions * kt_per_Tg ) %>%
            dplyr::select( -Emissions ) %>%
            dplyr::rename( Emisssions = Emissions_convert_unit)

    } else {

        GAINS_emiss_UnitsFixed <- GAINS_emiss_clean

    }

    GAINS_emiss_Units <- GAINS_emiss_UnitsFixed %>%
        dplyr::mutate( units = "kt" )

#   Select fugitive emissions only, map to CEDS sector, aggregate
    GAINS_FugEmiss_SecMapped <- GAINS_emiss_Units %>%
        dplyr::left_join( GAINS_fugitive_sectors_df, by = "Sector" ) %>%
        dplyr::filter( !( is.na( ceds_sector ) ) ) %>%
        dplyr::select( -Sector ) %>%
        dplyr::group_by( Region, years, ceds_sector, units ) %>%
        dplyr::summarise_all( funs( sum (., na.rm = TRUE) ) ) %>%
        dplyr::ungroup( )

#   Map to CEDS isos
#       I.) Check if all regions listed in GAINS region map are within the GAINS
#       emissions data regions, and that all regions in the GAINS emissions
#       data are within the GAINS region map
#       TODO: Change this to the updated iso check function once available potentially
#             (iso_check in analysis_functions.R)
        unique_GAINS_map_regions <- sort( unique( GAINS_country_map_clean$Region ) )
        unique_GAINS_emiss_regions <- sort( unique( GAINS_FugEmiss_SecMapped$Region ) )

#       Check if all regions listed in GAINS region map are within the GAINS emissions data
        GAINS_map_regions_not_in_GAINS_emiss_data <- subset( unique_GAINS_map_regions,
                                                             !( unique_GAINS_map_regions
                                                                %in% unique_GAINS_emiss_regions ) )

        if( length( GAINS_map_regions_not_in_GAINS_emiss_data ) != 0 ){

            printLog( GAINS_map_regions_not_in_GAINS_emiss_data )

            stop( "The above regions are in the GAINS region mapping file but ",
                     "not within the GAINS emissions data...")

        } else {

            printLog( "All regions in the GAINS mapping file are within the GAINS emissions data...")

        }

#       Check if all regions listed in GAINS emissions data  are within the GAINS region map
        GAINS_emiss_regions_not_in_GAINS_map <- subset( unique_GAINS_emiss_regions,
                                                        !( unique_GAINS_emiss_regions %in%
                                                        unique_GAINS_map_regions) )

        if( length( GAINS_emiss_regions_not_in_GAINS_map ) != 0 ){

            printLog( GAINS_emiss_regions_not_in_GAINS_map )

            stop( paste0( "The above regions are in the GAINS emissions data but not ",
                          "in the GAINS region map... " ) )

        } else {

            printLog( "All regions in the GAINS emissions data are within the GAINS region map..." )

        }

#       II.) Map GAINS emissions data to CEDS isos
        GAINS_FugEmiss_isoMapped <- GAINS_FugEmiss_SecMapped %>%
            dplyr::left_join( GAINS_country_map_clean, by = "Region" )

        if( any ( is.na( GAINS_FugEmiss_isoMapped$iso ) | GAINS_FugEmiss_isoMapped$iso == "" ) ){

            GAINS_FugEmiss_isoMapped_missing_isos <- GAINS_FugEmiss_isoMapped %>%
                dplyr::filter( is.na ( iso ) | iso == "" ) %>%
                dplyr::select( Region  ) %>%
                dplyr::distinct( )

            printLog( GAINS_FugEmiss_isoMapped_missing_isos[[1]] )

            stop ( paste0( "The above GAINS region does not have an iso defined in ",
                           "the mapping file (emf-30_ctry_map.csv)." ) )

        } else {

            printLog( "All GAINS regions in GAINS emissions data were mapped to CEDS isos..." )

        }

#   Add X's in front of years
    GAINS_FugEmiss_fixyears <- GAINS_FugEmiss_isoMapped %>%
        dplyr::mutate( years = paste0( "X", years ) )

#   Seperate emissions for processing - (1) NG distribution fug. emiss,
#   (2) NG production ug. emiss, and (3) Oil production fug. emiss
    GAINS_fug_NG_distribution <- GAINS_FugEmiss_fixyears %>%
        dplyr::filter( ceds_sector == "1B2b_Fugitive-NG-distr" )

    GAINS_fug_NG_prod <- GAINS_FugEmiss_fixyears %>%
        dplyr::filter( ceds_sector == "1B2b_Fugitive-NG-prod" )

    GAINS_fug_oil_prod <- GAINS_FugEmiss_fixyears %>%
        dplyr::filter( ceds_sector == "1B2_Fugitive-petr" )

# ------------------------------------------------------------------------------

# 5. Reformat CDIAC Driver data

# Subset NG Emissions and retain appropriate years (1960-2011).
# Note that CDIAC inventory only goes to 2011 (other than cement which was extended during initial CDIAC processing),
# so after subsetting appropriate years, extend CDIAC emisisons to 2020 for EF (constant, equal to 2011 values).
  X_cdiac_end_year <- paste0( "X",  cdiac_end_year )

  CDIAC_CO2_gas <- CDIAC_CO2_inventory %>%
    dplyr::filter( fuel == "gas_fuels" ) %>%
    dplyr::select( iso, fuel, CDIAC_YEARS_X ) %>%
    dplyr::filter( iso != "global" ) %>%
    dplyr::mutate_at( CDIAC_YEARS_EXTEND_TO, funs( identity( !!rlang::sym( X_cdiac_end_year ) ) ) )

#  Check if all isos in CDIAC data are within the MCL, and
#  that all isos in the MCL are within the CDIAC data
#  TODO: Change this to the updated iso check function once available
#  (iso_check in analysis_functions.R)
    unique_CDIAC_NG_emissions_isos <- sort( unique( CDIAC_CO2_gas$iso ) )
    unique_MCL_isos <- sort( unique( MCL_clean$iso ) )

#   A.) Check if all isos listed in CDIAC data are within the MCL
    CDIAC_isos_not_in_MCL <- subset( unique_CDIAC_NG_emissions_isos,
                                    !( unique_CDIAC_NG_emissions_isos %in% unique_MCL_isos ) )

    if( length( CDIAC_isos_not_in_MCL ) != 0 ){

        printLog( CDIAC_isos_not_in_MCL )

        stop( paste0( "The above isos are in the CDIAC data but not ",
                         "final CEDS isos in Master_Country_List.csv... " ) )

    } else {

        printLog( "All isos in the in the CDIAC data are",
                      "final CEDS isos in Master_Country_List.csv..." )

    }

#   B.) Check if all isos in MCL (with final_data_flag of 1, srb(kosovo), and gum)
#   Are within the GAINS region mapping file
    MCL_isos_not_in_CDIAC <- subset( unique_MCL_isos,
                                         !( unique_MCL_isos %in% unique_CDIAC_NG_emissions_isos ) )

    if( length( MCL_isos_not_in_CDIAC ) != 0 ){

        printLog( MCL_isos_not_in_CDIAC )

        stop( paste0( "The above isos are final CEDS isos in Master_Country_List.csv ",
                          "but not found in the CDIAC data...") )

    } else {

        printLog( "All final CEDS isos in Master_Country_List.csv are within",
                      "the CDIAC data...")

    }

# ------------------------------------------------------------------------------

# 6. Downscale fugitive NG distribution emissions by CDIAC NG CO2 emissions
    printLog( "Downscaling GAINS fugitive natural gas distribution emissions with",
              "CDIAC natural gas CO2 emissions...")

#   Check if all isos listed in GAINS NG distribution fugitive emissions data
#   are within the CDIAC gas CO2 emissions data, and that all isos in CDIAC
#   gas CO2 emissions are within GAINS GAINS NG distribution fugitive emissions data.
#   TODO: Change this to the updated iso_check function once available
#   (iso_check in analysis_functions.R)
    unique_GAINS_NGdist_emissions_isos <- sort( unique( GAINS_fug_NG_distribution$iso ) )

#   A.) Check if all isos listed in GAINS NG distribution fugitive emissions data
#   are within the CDIAC gas CO2 emissions data.
    GAINS_ngDistEmiss_isos_not_in_CDIAC_NGemiss_isos <- subset( unique_GAINS_NGdist_emissions_isos,
                                                                !( unique_GAINS_NGdist_emissions_isos
                                                                %in% unique_CDIAC_NG_emissions_isos ) )

    if( length( GAINS_ngDistEmiss_isos_not_in_CDIAC_NGemiss_isos ) != 0 ){

        printLog( GAINS_ngDistEmiss_isos_not_in_CDIAC_NGemiss_isos )

        stop( "The above isos are in the GAINS NG distribution fugitive emissions data but ",
              "not within the CDIAC NG CO2 emissions data...")

    } else {

        printLog( "All isos in the GAINS NG distribution fugitive emissions data ",
                      "are within the CDIAC gas CO2 emissions data..." )

    }

#   B.) Check if all isos listed in CDIAC gas CO2 emissions data
#   are within the GAINS NG distribution fugitive emissions data.
    CDIAC_NGemiss_isos_not_in_GAINS_ngDistEmiss_isos <- subset( unique_CDIAC_NG_emissions_isos,
                                                                !( unique_CDIAC_NG_emissions_isos
                                                                %in% unique_GAINS_NGdist_emissions_isos ) )

    if( length( CDIAC_NGemiss_isos_not_in_GAINS_ngDistEmiss_isos ) != 0 ){

        printLog( CDIAC_NGemiss_isos_not_in_GAINS_ngDistEmiss_isos )

        stop( "The above isos are in the CDIAC NG CO2 emissions data but ",
              "not within the GAINS NG distribution fugitive emissions data...")

    } else {

        printLog( paste0( "All isos in the CDIAC gas CO2 emissions data ",
                          "are within the GAINS NG distribution fugitive emissions data...") )

    }

#   Map to GAINS regions
    CDIAC_CO2_gas_mapped <- CDIAC_CO2_gas %>%
        dplyr::left_join( GAINS_country_map_clean, by = "iso" ) %>%
        dplyr::select( Region, iso, fuel, CDIAC_EXTENDED_YEARS_X ) %>%
        dplyr::filter( !( is.na( Region ) ) ) %>%
        tidyr::gather( key = years, value = CDIAC_Emissions, CDIAC_EXTENDED_YEARS_X )

#   Calculate aggregate CDIAC NG CO2 emissions by GAINS Region
    CDIAC_CO2_gas_aggRegion <- CDIAC_CO2_gas_mapped %>%
        dplyr::select( -iso ) %>%
        dplyr::group_by( Region, fuel, years ) %>%
        dplyr::summarise_all( funs( sum (., na.rm = TRUE) ) ) %>%
        dplyr::ungroup( ) %>%
        dplyr::rename( CDIAC_aggReg_NG_CO2Emiss = CDIAC_Emissions )

#   Create regional emissions factor for GAINS Regions
#   regional EF = (GAINS fugitive NG dist. emiss by GAINS reg. / CDIAC NG CO2 emissions by GAINS reg.)
#   Note that GAINS regional fug. NG dist. emissions are repeated for each iso in the GAINS region,
#   in order to provide each iso their regional EF
    NG_distribution_EF <- GAINS_fug_NG_distribution %>%
        dplyr::rename( GAINS_fug_NG_dis_emissions = Emissions ) %>%
        dplyr::left_join( CDIAC_CO2_gas_aggRegion, by = c("Region", "years" ) ) %>%
        dplyr::select( -units, -fuel ) %>%
        dplyr::mutate( EF = GAINS_fug_NG_dis_emissions / CDIAC_aggReg_NG_CO2Emiss,
                       EF_units = "(GAINS_NGdist_Emiss / CDIAC_ngCO2_emiss) by GAINS reg." ) %>%
        dplyr::select( Region, iso, ceds_sector, EF_units, years, EF )

#   Interpolate the EF between GAINS years, retain only CEDS years
    NG_distribution_EF_interp <- NG_distribution_EF %>%
        tidyr::spread( years, EF ) %>%
        dplyr::mutate_at( MISSING_GAINS_YEARS_TO_MAKE, funs( identity( NA ) ) ) %>%
        dplyr::select( Region, iso, ceds_sector, EF_units, GAINS_INTERP_YEARS_X ) %>%
        interpolate_NAs_new( ) %>%
        dplyr::select( Region, iso, ceds_sector, EF_units, GAINS_FINAL_YEARS_WITH_X )

#   Extend earliest EF year backwards to start_year (currently 2000 extended back to 1960, constant extension )
    missing_years_for_extending <- paste0('X', start_year:( as.numeric( GAINS_START_YEAR ) - 1 ) ) # Currently: 1960-1999

    NG_distribution_EF_extended <- NG_distribution_EF_interp %>%
        dplyr::mutate_at( missing_years_for_extending, funs( identity( X2000 ) ) ) %>%
        dplyr::select( Region, iso, ceds_sector, EF_units, X_emissions_years ) %>%
        tidyr::gather( key = years, value = EFs, X_emissions_years )

#   Downscale GAINS fugitive NG distribution emissions by multiplying EF by CDIAC gas emissions (by iso)
    GAINS_fug_NGdist_final_emiss <- NG_distribution_EF_extended %>%
        dplyr::left_join( CDIAC_CO2_gas_mapped, by = c( "Region", "iso", "years" ) ) %>%
        dplyr::mutate( Down_scaled_emissions = EFs * CDIAC_Emissions,
                       units = "kt" ) %>%
        dplyr::select( iso, ceds_sector, units, years, Down_scaled_emissions ) %>%
        tidyr::spread( years, Down_scaled_emissions ) %>%
        dplyr::rename( sector = ceds_sector )

# ------------------------------------------------------------------------------

# 7. Process BP and IEA oil production data - BP( favored over IEA (IEA provides only what BP is missing)
#TODO: confirm this detail (BP favored)
#TODO: disaggregate BP data for "other region" isos ("other africa"...), and split historical data for certain regions (for example: Sudan and S. Sudan)
#TODO: Align this with the MCL BP_oil name column (so only one mapping file is needed - MCL may need to be fixed)

#   Initial cleaning of BP data (clean, map, convert units)
    not_BP_countries <- c( "                 European Union #", "                 Non-OECD",
                           "                 Non-OPEC £", "                 OPEC",
                           " # Excludes Estonia, Latvia and Lithuania prior to 1985 and Slovenia prior to 1991.",
                           " ^ Less than 0.05.", " £ Excludes Former Soviet Union.",
                           "* Includes crude oil, tight oil, oil sands and NGLs (the liquid content of natural gas where this is recovered separately). Excludes liquid fuels from other sources such as biomass and derivatives of",
                           "coal and natural gas.", "n/a not available.", "of which: OECD", "Other Africa", "Other Asia Pacific",
                           "Other Europe & Eurasia", "Other Middle East", "Other S. & Cent. America",  "Total Africa",
                           "Total Asia Pacific", "Total Europe & Eurasia",  "Total Middle East", "Total North America",
                           "Total S. & Cent. America", "Total World", "w Less than 0.05%." )

    BP_oil_data <- BP_oil_production %>%
        dplyr::select( -"2013__1", -"of total"  ) %>%
        dplyr::rename( BPName_Oil_production = "Million tonnes" ) %>%
        dplyr::mutate( BPName_Oil_production = if_else( BPName_Oil_production == "                 Former Soviet Union",
                                                        "Former Soviet Union", BPName_Oil_production ) )

    colnames( BP_oil_data ) <- c( 'BPName_Oil_production', BP_OIL_YEARS_x )

    BP_oil <- BP_oil_data %>%
        dplyr::filter_at( .vars = colnames( BP_oil_data ), any_vars( !is.na( . ) ) ) %>%
        dplyr::filter( BPName_Oil_production %!in% not_BP_countries ) %>%
        dplyr::left_join( MCL, by = "BPName_Oil_production" ) %>%
        tidyr::gather( key = years, value = Oil_production, BP_OIL_YEARS_x ) %>%
        dplyr::mutate( Oil_production = as.numeric( Oil_production ),
                       Oil_production = Oil_production * 1000000 * ( 1 / 1000 ) ) %>% # Convert from million tonnes to kt
        tidyr::spread( years, Oil_production ) %>%
        dplyr::select( iso, BP_OIL_YEARS_x ) %>%
        dplyr::arrange( iso )

#   Disaggregate BP's FSU data (from 1965-1984 disaggregate FSU to FSU members).
#   FSU member countries have no data before 1985 in raw BP data, but total production is reported for FSU.
#   Here the code splits the ussr( fsu ) and distributes to fsu member countries
    fsu <- 'ussr'
    fsu_members <- c( "arm", "aze", "blr", "est", "geo", "kaz", "kgz", "ltu", "lva", "mda", "rus", "tjk", "tkm", "ukr", "uzb" )

    BP_split_Xyears <- paste0( 'X', historical_end_extension_year : 1984 ) # Currently: X1965-X1984

#   A.) Extract FSU countries from BP_oil data
    BP_fsu <- BP_oil %>%
        dplyr::filter( iso %in% fsu )

    BP_fsu_members <- BP_oil %>%
        dplyr::filter( iso %in% fsu_members )

#   B.) Re-format the pop data
    pop_wide <- pop %>%
        dplyr::filter( iso %in% c( fsu, fsu_members ) ) %>%
        dplyr::mutate( year = paste0( "X", year ) ) %>%
        dplyr::filter( year %in% BP_OIL_YEARS_x,
                       scenario == "Estimates" ) %>%
        dplyr::select( iso, year, pop ) %>%
        spread( year, pop )

#   C.) Split using disaggregate_country function
    BP_fsu_split <- dplyr::bind_rows( BP_fsu, BP_fsu_members )
    BP_fsu_split$sector <- 'oil_production' # Neded by the function but has no other effect

    oil_production <- disaggregate_country( original_data = BP_fsu_split,
                                            trend_data = pop_wide,
                                            trend_match_cols = 'iso',
                                            combined_iso = fsu,
                                            disaggregate_iso = BP_fsu_members$iso,
                                            dis_start_year = 1965,
                                            dis_end_year = 1984 )

#   D.) Replace FSU member data in BP_oil with disaggregated FSU data and remove 'ussr'
    for ( BP_fsu_member in BP_fsu_members$iso ) {

        BP_oil[ BP_oil$iso == BP_fsu_member, BP_split_Xyears ] <- oil_production[ oil_production$iso == BP_fsu_member, BP_split_Xyears ]

    }

    BP_oil_fixed <- BP_oil %>%
        dplyr::filter( iso != "ussr" ) %>%
        dplyr::mutate( units = "kt",
                       sector = "oil_production",
                       data_source = "BP",
                       fuel = "process" ) %>%
        dplyr::select( data_source, iso, sector, fuel, units, BP_OIL_YEARS_x )

    BP_oil_isos <- sort( unique( BP_oil_fixed$iso ) )

#   Initial cleaning of IEA oil production data:
#   Select isos that are not in the BP dataSet,
#   set production to NA from 1960-1970 (since all data is currently 0 (for all isos),
#   and extend 2013 (final IEA production year) to 2014 (final CEDS year).
#   TODO: This may not be 0 for 1960-1970 in future IEA data - this should be checked when updating IEA data versions.
    IEA_oil <- en_stat_sector_fuel %>%
        dplyr::filter( sector == "crude-oil-production",
                       !( iso %in% BP_oil_isos ) ) %>%
        dplyr::left_join( MCL_unique_with_OECD_flag, by = "iso" ) %>%
        dplyr::mutate_at( IEA_NA_YEARS, funs( identity( NA ) ) )  %>%  # All data in these years is currently 0 for countries not in BP, and begins in 1971 (when non-oecd data begins)
        dplyr::mutate( !!X_end_year := !!rlang::sym( X_IEA_end_year ),
                       sector = "oil_production",
                       data_source = "IEA" ) %>%
        dplyr::select( data_source, iso, sector, fuel, units, X_emissions_years )

#   Combine IEA and BP oil data
    Oil_production <- dplyr::bind_rows( IEA_oil, BP_oil_fixed )

# ------------------------------------------------------------------------------

# 8. Downscale fugitive oil production emissions using BP and IEA oil production data
    printLog( "Downscaling GAINS fugitive oil emissions with BP and IEA oil production activity data...")

#   Check if all isos listed in IEA and BP oil production data are within GAINS fugitive
#   oil production emissions data
#   TODO: Change this to the updated iso check function once available
#         (iso_check in analysis_functions.R) - with a stop not a warning
    unique_GAINS_fug_oil_prod <- sort( unique( GAINS_fug_oil_prod$iso ) )
    unique_Oil_production <- sort( unique( Oil_production$iso ) )

    Oil_production_isos_not_in_GAINS_fug_oil_prod_isos <- subset( unique_Oil_production,
                                                                 !( unique_Oil_production
                                                                 %in% unique_GAINS_fug_oil_prod ) )

    if( length( Oil_production_isos_not_in_GAINS_fug_oil_prod_isos ) != 0 ){

         printLog( Oil_production_isos_not_in_GAINS_fug_oil_prod_isos )

         warning( "The above isos are in the Oil production data but ",
                "not within the GAINS oil production fugitive emissions data...")

     } else {

         printLog( "All isos in the Oil production data are",
                   "within the GAINS oil production fugitive emissions data..." )

     }

#   Map oil production data to GAINS regions
    Oil_production_region_mapped <- Oil_production %>%
        dplyr::left_join( GAINS_country_map_clean, by = "iso" ) %>%
        dplyr::select( Region, iso, sector, fuel, units, X_emissions_years )

#   Aggregate by region for each year, extend aggregate data forward to 2020 for EF
#   Note: We are aggregating data for which some isos might be NA (IEA's first reported year of data is 1971, so only
#   isos for a given GAINS region which had data from BP are aggregated. However, this aggregate info isn't used below,
#   as the aggRegion EF is created only for years within "GAINS_EMISS_YEARS_KEEP_X" (currently 2000, 2005, 2010, and 2020)
    oil_production_extension_years <- paste0( "X", ( end_year + 1 ): LAST_GAINS_YEAR_FOR_INTERP )

    Oil_production_aggRegion_for_EF <- Oil_production_region_mapped %>%
        dplyr::select( -iso ) %>%
        dplyr::group_by( Region, sector, fuel, units ) %>%
        dplyr::summarise_all( funs( sum ( ., na.rm = TRUE) ) ) %>%
        dplyr::ungroup( ) %>%
        dplyr::mutate_at( oil_production_extension_years, funs( identity( !!rlang::sym( X_end_year ) ) ) ) %>%
        dplyr::rename( units_production = units ) %>%
        dplyr::select( Region, sector, fuel, units_production, GAINS_EMISS_YEARS_KEEP_X ) %>%
        tidyr::gather( key = years, value = Oil_production, GAINS_EMISS_YEARS_KEEP_X )

#   Create emissions factor by GAINS Regions
#   Regional EF = (GAINS fugitive oil production dist. emiss by GAINS reg. / IEA and BP Oil production activity by GAINS reg.)
    Oil_prod_EF <- GAINS_fug_oil_prod %>%
          dplyr::rename( GAINS_fug_oil_prod_emissions = Emissions ) %>%
          dplyr::left_join( Oil_production_aggRegion_for_EF, by = c("Region", "years" ) ) %>%
          dplyr::select( Region, iso, years, ceds_sector, units, GAINS_fug_oil_prod_emissions, sector, units_production, Oil_production  ) %>%
          dplyr::mutate( EF = GAINS_fug_oil_prod_emissions / Oil_production,
                         EF_units = "(GAINS_OilProd_Emiss / IEAbp_oil_prod), by GAINS reg." )

#   If region's EF = 0 or if regional EF is NaN (0/0), assign global weighted average EF
    global_total_production <- Oil_prod_EF %>%
        dplyr::select( years, Oil_production ) %>%
        dplyr::distinct( ) %>%                         # distinct is needed, as currently regional production is repeated for each iso within the given region
        dplyr::group_by( years ) %>%
        dplyr::summarise_all( funs( sum( ., na.rm = T ) ) ) %>%
        dplyr::ungroup( ) %>%
        dplyr::rename( global_total_production = Oil_production )

    Weighted_EF <- Oil_prod_EF %>%
        dplyr::select( Region, years, ceds_sector, sector, Oil_production, units_production, EF, EF_units ) %>%
        dplyr::distinct( ) %>%  # Needed as regions are duplicated currently (Oil_prod_EF has regional EFs duplicated for each iso within the given region)
        dplyr::left_join( global_total_production, by = "years" ) %>%
        dplyr::mutate( EF_replacement_weights = Oil_production / global_total_production ) %>%
        dplyr::mutate( Weighted_EF_contribution = EF_replacement_weights * EF ) %>%
        dplyr::select( years, Weighted_EF_contribution ) %>%
        dplyr::group_by( years ) %>%
        dplyr::summarise_all( funs( sum( ., na.rm = T ) ) ) %>%
        dplyr::ungroup( ) %>%
        dplyr::rename( Weighted_EF = Weighted_EF_contribution )

    Oil_prod_EF_fixed <- Oil_prod_EF %>%
        dplyr::left_join( Weighted_EF, by = "years" ) %>%
        dplyr::mutate( EF = if_else( is.na( EF ) | EF == 0, Weighted_EF, EF ) ) %>%
        dplyr::select( Region, iso, ceds_sector, sector, EF_units, years, EF )

#   Interpolate the EF between GAINS years, retain only CEDS years (2000-2014)
    Oil_prod_EF_interp <- Oil_prod_EF_fixed %>%
        tidyr::spread( years, EF ) %>%
        dplyr::mutate_at( MISSING_GAINS_YEARS_TO_MAKE, funs( identity( NA ) ) ) %>%
        dplyr::select( Region, iso, ceds_sector, EF_units, GAINS_INTERP_YEARS_X ) %>%
        interpolate_NAs_new( ) %>%
        dplyr::select( Region, iso, ceds_sector, EF_units, GAINS_FINAL_YEARS_WITH_X )

#   Extend earliest EF year backwards to start_year (2000 back to 1960)
    Oil_prod_EF_extended <- Oil_prod_EF_interp %>%
        dplyr::mutate_at( missing_years_for_extending, funs( identity( !!rlang::sym( GAINS_START_YEAR_X ) ) ) ) %>%
        dplyr::select( Region, iso, ceds_sector, EF_units, X_emissions_years ) %>%
        tidyr::gather( key = years, value = EFs, X_emissions_years )

#   Downscale GAINS fugitive oil production emissions by multiplying EF by IEA and BP oil production data
#   Note, production values are set to 0 after 1965 for isos which are not within the combined IEA and BP production data (assumes NA because 0 production )
    Oil_production_region_mapped_long <- Oil_production_region_mapped %>%
        tidyr::gather( key = years, value = oil_production, X_emissions_years )

    Oil_prod_EF_extended_isos <- sort( unique( Oil_production_region_mapped_long$iso ) )

    GAINS_fug_oil_prod_final_emissions <- Oil_prod_EF_extended %>%
          dplyr::left_join( Oil_production_region_mapped_long, by = c( "Region", "iso", "years" ) ) %>%
          dplyr::mutate( oil_production = if_else( iso %!in% Oil_prod_EF_extended_isos & years %in% BP_OIL_YEARS_x, 0,  oil_production ) ) %>%
          dplyr::mutate( Down_scaled_emissions = EFs * oil_production ) %>%
          dplyr::select( iso, ceds_sector, units, years, Down_scaled_emissions ) %>%
          tidyr::spread( years, Down_scaled_emissions ) %>%
          dplyr::rename( sector = ceds_sector )

# ------------------------------------------------------------------------------

# 9. Process BP and IEA natural gas production data - BP( favored over IEA (IEA provides only what BP is missing)
    printLog( "Downscaling GAINS fugitive natural gas production emissions with BP and IEA gas production activity data...")

#TODO: Confirm this detail (BP favored)
#TODO: Disaggregate BP data for "other region" isos ("other africa"...), and split historical data for certain regions (for example: Sudan and S. Sudan)
#TODO: Align this with the MCL BP_oil name column (so can just use the mapping file - MCL may need to be fixed)

#   Initial cleaning of BP data (clean, map, convert units)
    not_BP_countries <- c( not_BP_countries,
                           "*Excludes gas flared or recycled. Includes natural gas produced for Gas-to-Liquids transformation." )

    BP_gas_data <- BP_gas_production %>%
        dplyr::select( -"2013__1", -"of total"  ) %>%
        dplyr::rename( BPName_Gas_production = "Million tonnes oil equivalent" ) %>%
        dplyr::mutate( BPName_Gas_production = if_else( BPName_Gas_production == "                 Former Soviet Union",
                                                        "Former Soviet Union", BPName_Gas_production ) )

    colnames( BP_gas_data ) <- c( 'BPName_Gas_production', BP_GAS_YEARS_x )

    MCL_BP_gas_map <- MCL %>%
        dplyr::select( iso, BPName_Gas_production ) %>%
        dplyr::distinct( )

    BP_gas <- BP_gas_data %>%
        dplyr::filter_at( .vars = colnames( BP_gas_data ), any_vars( !is.na( . ) ) ) %>%
        dplyr::filter( BPName_Gas_production %!in% not_BP_countries ) %>%
        dplyr::left_join( MCL, by = "BPName_Gas_production" ) %>%
        tidyr::gather( key = years, value = gas_production, BP_GAS_YEARS_x ) %>%
        dplyr::mutate( gas_production = as.numeric( gas_production ),
                       gas_production = gas_production * conversionFactor_TJ_per_Mtoe * ( 1 / conversionFactor_naturalgas_TJ_per_kt_net ) ) %>% # Convert from Mtoe to kt NG
        tidyr::spread( years, gas_production ) %>%
        dplyr::select( iso, BP_GAS_YEARS_x ) %>%
        dplyr::arrange( iso )

    if( any( is.na( BP_gas$iso ) ) ){

        stop( paste0( "There should be no NAs for iso after mapping BP natural gas production data to CEDS isos. ",
                      "Check the Master_Country_List.csv file BPName_Gas_production column and verify there are entries for each",
                      "applicable region in the BP natural gas production data." ) )

    }

#   Disaggregate BP's FSU data (from 1970-1984 disaggregate FSU to FSU members).
#   FSU member countries have no data before 1985 in raw BP data, but total production is reported for FSU.
#   Here the code splits the ussr( fsu ) and distributes to fsu member countries
    BP_split_Xyears <- paste0( 'X', 1970 : 1984 ) # Currently: X1965-X1984

#   A.) Extract FSU countries from BP_gas data
    BP_fsu <- BP_gas %>%
        dplyr::filter( iso %in% fsu )

    BP_fsu_members <- BP_gas %>%
        dplyr::filter( iso %in% fsu_members )

#   B.) Re-format the pop data
    pop_wide <- pop %>%
        dplyr::filter( iso %in% c( fsu, fsu_members ) ) %>%
        dplyr::mutate( year = paste0( "X", year ) ) %>%
        dplyr::filter( year %in% BP_OIL_YEARS_x,
                       scenario == "Estimates" ) %>%
        dplyr::select( iso, year, pop ) %>%
        spread( year, pop )

#   C.) Split using disaggregate_country function
    BP_fsu_split <- dplyr::bind_rows( BP_fsu, BP_fsu_members )
    BP_fsu_split$sector <- 'gas_production' # Neded by the function but has no other effect

    gas_production <- disaggregate_country( original_data = BP_fsu_split,
                                            trend_data = pop_wide,
                                            trend_match_cols = 'iso',
                                            combined_iso = fsu,
                                            disaggregate_iso = BP_fsu_members$iso,
                                            dis_start_year = 1970,
                                            dis_end_year = 1984 )

#   D.) Replace FSU member data in BP_oil with disaggregated FSU data and remove 'ussr'
    for ( BP_fsu_member in BP_fsu_members$iso ) {

        BP_gas[ BP_gas$iso == BP_fsu_member, BP_split_Xyears ] <- gas_production[ gas_production$iso == BP_fsu_member, BP_split_Xyears ]

    }

    BP_gas_fixed <- BP_gas %>%
        dplyr::filter( iso != "ussr" ) %>%
        dplyr::mutate( units = "kt",
                       sector = "gas_production",
                       data_source = "BP",
                       fuel = "process" ) %>%
        dplyr::select( data_source, iso, sector, fuel, units, BP_GAS_YEARS_x )

    BP_gas_isos <- sort( unique( BP_gas_fixed$iso ) )

#   Initial cleaning of IEA gas production data:
#   Select isos that are not in the BP dataSet,
#   set production to NA from 1960-1970 for non-OECD (since all data is currently 0 for Non-OECD isos not present in BP - data begins in 1971 for Non-OECD),
#   and extend 2013 (final IEA production year) to 2014 (final CEDS year).
#   TODO: This may not be 0 for 1960-1970 in future IEA data - this should be checked when updating IEA data versions.
#   TODO: Should we set 1960-1970 NA for the OECD countries that are all 0 for these years? (seem to be missing data for these isos too)
    IEA_gas <- en_stat_sector_fuel %>%
        dplyr::filter( sector == "natural_gas-production",
                       !( iso %in% BP_gas_isos ) ) %>%
        dplyr::left_join( MCL_unique_with_OECD_flag, by = "iso" ) %>%
        dplyr::mutate_at( IEA_NA_YEARS, funs( if_else( OECD_flag == "NonOECD", NA_real_ , . ) ) )  %>%
        dplyr::mutate( !!X_end_year := !!rlang::sym( X_IEA_end_year ),
                       sector = "gas_production",
                       data_source = "IEA" ) %>%
        dplyr::select( data_source, iso, sector, fuel, units, X_emissions_years )

#   Combine IEA and BP gas data
    Gas_production <- dplyr::bind_rows( IEA_gas, BP_gas_fixed )

# ------------------------------------------------------------------------------

# 10. Downscale fugitive natural gas production emisisons using BP and IEA natural gas production data

#   Check if all isos listed in IEA and BP NG production data are within GAINS fugitive
#   gas production emissions data
#   TODO: Change this to the updated iso check function once available
#         (iso_check in analysis_functions.R) - with a stop not a warning
    unique_GAINS_fug_gas_prod <- sort( unique( GAINS_fug_NG_prod$iso ) )
    unique_gas_production <- sort( unique( Gas_production$iso ) )

    GAS_production_isos_not_in_GAINS_fug_gas_prod_isos <- subset( unique_gas_production,
                                                                 !( unique_gas_production
                                                                 %in% unique_GAINS_fug_gas_prod ) )

    if( length( GAS_production_isos_not_in_GAINS_fug_gas_prod_isos ) != 0 ){

        printLog( GAS_production_isos_not_in_GAINS_fug_gas_prod_isos )

        warning( "The above isos are in the gas production data but ",
                 "not within the GAINS gas production fugitive emissions data...")

    } else {

        printLog( "All isos in the gas production data are",
                  "within the GAINS gas production fugitive emissions data..." )

    }

#   Map gas production data to GAINS regions
    Gas_production_region_mapped <- Gas_production %>%
        dplyr::left_join( GAINS_country_map_clean, by = "iso" ) %>%
        dplyr::select( Region, iso, sector, fuel, units, X_emissions_years )

#   Aggregate by region for each year, extend aggregate data forward to 2020 for EF (currently extending 2014 to 2020)
#   Note: We are aggregating data for which some isos might be NA (IEA's first reported year of data is 1971, so only
#   isos for a given GAINS region which had data from BP are aggregated. However, this aggregate info isn't used below,
#   as the aggRegion EF is created only for years within "GAINS_EMISS_YEARS_KEEP_X" (currently 2000, 2005, 2010, and 2020)
    gas_production_extension_years <- paste0( "X", ( end_year + 1 ): LAST_GAINS_YEAR_FOR_INTERP )

    Gas_production_aggRegion_for_EF <- Gas_production_region_mapped %>%
        dplyr::select( -iso ) %>%
        dplyr::group_by( Region, sector, fuel, units ) %>%
        dplyr::summarise_all( funs( sum ( ., na.rm = TRUE) ) ) %>%
        dplyr::ungroup( ) %>%
        dplyr::mutate_at( gas_production_extension_years, funs( identity( !!rlang::sym( X_end_year ) ) ) ) %>%
        dplyr::rename( units_production = units ) %>%
        dplyr::select( Region, sector, fuel, units_production, GAINS_EMISS_YEARS_KEEP_X ) %>%
        tidyr::gather( key = years, value = gas_production, GAINS_EMISS_YEARS_KEEP_X )

#   Create emissions factor by GAINS Regions
#   Regional EF = (GAINS fugitive gas production dist. emiss by GAINS reg. / IEA and BP gas production activity by GAINS reg.)
    gas_prod_EF <- GAINS_fug_NG_prod %>%
        dplyr::rename( GAINS_fug_gas_prod_emissions = Emissions ) %>%
        dplyr::left_join( Gas_production_aggRegion_for_EF, by = c("Region", "years" ) ) %>%
        dplyr::select( Region, iso, years, ceds_sector, units, GAINS_fug_gas_prod_emissions, sector, units_production, gas_production  ) %>%
        dplyr::mutate( EF = GAINS_fug_gas_prod_emissions / gas_production,
                       EF_units = "(GAINS_gasProd_Emiss / IEAbp_gas_prod), by GAINS reg." )

#   If region's EF = 0 or if regional EF is NaN (0/0), assign global weighted average EF
    global_total_production <- gas_prod_EF %>%
        dplyr::select( years, gas_production ) %>%
        dplyr::distinct( ) %>%                         # distinct is needed, as currently regional production is repeated for each iso within the given region
        dplyr::group_by( years ) %>%
        dplyr::summarise_all( funs( sum( ., na.rm = T ) ) ) %>%
        dplyr::ungroup( ) %>%
        dplyr::rename( global_total_production = gas_production )

    Weighted_EF <- gas_prod_EF %>%
        dplyr::select( Region, years, ceds_sector, sector, gas_production, units_production, EF, EF_units ) %>%
        dplyr::distinct( ) %>%  # Needed as regions are duplicated currently (Oil_prod_EF has regional EFs duplicated for each iso within the given region)
        dplyr::left_join( global_total_production, by = "years" ) %>%
        dplyr::mutate( EF_replacement_weights = gas_production / global_total_production ) %>%
        dplyr::mutate( Weighted_EF_contribution = EF_replacement_weights * EF ) %>%
        dplyr::select( years, Weighted_EF_contribution ) %>%
        dplyr::group_by( years ) %>%
        dplyr::summarise_all( funs( sum( ., na.rm = T ) ) ) %>%
        dplyr::ungroup( ) %>%
        dplyr::rename( Weighted_EF = Weighted_EF_contribution )

    gas_prod_EF_fixed <- gas_prod_EF %>%
        dplyr::left_join( Weighted_EF, by = "years" ) %>%
        dplyr::mutate( EF = if_else( is.na( EF ) | EF == 0, Weighted_EF, EF ) ) %>%
        dplyr::select( Region, iso, ceds_sector, sector, EF_units, years, EF )

#   Interpolate the EF between GAINS years, retain only CEDS years (2000-2014)
    gas_prod_EF_interp <- gas_prod_EF_fixed %>%
        tidyr::spread( years, EF ) %>%
        dplyr::mutate_at( MISSING_GAINS_YEARS_TO_MAKE, funs( identity( NA ) ) ) %>%
        dplyr::select( Region, iso, ceds_sector, EF_units, GAINS_INTERP_YEARS_X ) %>%
        interpolate_NAs_new( ) %>%
        dplyr::select( Region, iso, ceds_sector, EF_units, GAINS_FINAL_YEARS_WITH_X )

#   Extend earliest EF year gas_prod_EF_interp to start_year (2000 back to 1960)
    gas_prod_EF_extended <- gas_prod_EF_interp %>%
        dplyr::mutate_at( missing_years_for_extending, funs( identity( !!rlang::sym( GAINS_START_YEAR_X ) ) ) ) %>%
        dplyr::select( Region, iso, ceds_sector, EF_units, X_emissions_years ) %>%
        tidyr::gather( key = years, value = EFs, X_emissions_years )

#   Downscale GAINS fugitive gas production emissions by multiplying EF by IEA and BP gas production data
#   Note: production values are set to 0 for isos which are not within the combined IEA and BP production data (assumes theyu were NA because 0 production )
    gas_production_region_mapped_long <- Gas_production_region_mapped %>%
        tidyr::gather( key = years, value = gas_production, X_emissions_years )

    gas_prod_EF_extended_isos <- sort( unique( gas_production_region_mapped_long$iso ) )

    GAINS_fug_gas_prod_final_emissions <- gas_prod_EF_extended %>%
        dplyr::left_join( gas_production_region_mapped_long, by = c( "Region", "iso", "years" ) ) %>%
        dplyr::mutate( gas_production = if_else( iso %!in% gas_prod_EF_extended_isos & years %in% X_emissions_years, 0,  gas_production ) ) %>%
        dplyr::mutate( Down_scaled_emissions = EFs * gas_production ) %>%
        dplyr::select( iso, ceds_sector, units, years, Down_scaled_emissions ) %>%
        tidyr::spread( years, Down_scaled_emissions ) %>%
        dplyr::rename( sector = ceds_sector )

# ------------------------------------------------------------------------------

# 10. Combine data into 1 data frame, provide units and fuel (process)

GAINS_fugitive_emissions_final <- dplyr::bind_rows( GAINS_fug_gas_prod_final_emissions,
                                                    GAINS_fug_oil_prod_final_emissions,
                                                    GAINS_fug_NGdist_final_emiss ) %>%
        dplyr::mutate( units = "kt",
                       fuel = "process") %>%
        dplyr::select( iso, sector, fuel, units, X_emissions_years )

# ------------------------------------------------------------------------------

# 11. If em is N2O, apply ratio of Edgar 1B2 (Fugitive emissions from oil and gas) NOx to N2O emissions
if( em == "N2O" ){

  printLog( "Using EDGAR N2O and NOx data to produce default fugitive oil and gas emissions, as GAINS only provides",
            "NOx data for these sectors..." )

# Define function for cleaning EDGAR data
  EDGAR_clean_for_ratio <- function( EDGAR_data_in, EDGAR_years, MCL_final_isos, EDGAR_em ){
#     TODO: Note the scg work below - this may not be necessary in future EDGAR versions
      EDGAR_clean <- EDGAR_data_in %>%
          dplyr::select( ISO_A3, Name, IPCC, IPCC_description, EDGAR_years ) %>%
          dplyr::rename( iso = ISO_A3 ) %>%
          dplyr::mutate( iso = tolower( iso ) ) %>%
          dplyr::filter( IPCC == "1B2", IPCC_description == "Fugitive emissions from oil and gas" )

#     Apply EDGAR region aggregate Serbia and Montengro data to each CEDS relevant iso, so that both isos get their aggregate region ratio
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
    EDGAR_inv_start_year <- 1970
    EDGAR_inv_end_year <- 2008
    EDGAR_inv_years <- EDGAR_inv_start_year : EDGAR_inv_end_year
    X_EDGAR_inv_years <- paste0( "X", EDGAR_inv_years )
    EDGAR_N2O_clean <- EDGAR_clean_for_ratio( EDGAR_N2O, X_EDGAR_inv_years, MCL_clean, "N2O" )
    EDGAR_NOx_clean <- EDGAR_clean_for_ratio( EDGAR_NOx, X_EDGAR_inv_years, MCL_clean, "NOx" )

#   Combine EDGAR N2O and NOx data into 1 data frame
    EDGAR_N2O_and_NOx <- EDGAR_N2O_clean %>%
        dplyr::left_join( EDGAR_NOx_clean, by = c( "iso", "Name", "IPCC", "IPCC_description", "year" ) )

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
#   TODO: use regional ratios for isos which do not have ratio from EDGAR
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
    if( any( MCL_clean$iso %!in% EDGAR_national_ratio ) ){

        missing_isos <- subset( MCL_clean$iso, MCL_clean$iso %!in% EDGAR_national_ratio$iso )

        missing_isos_df <- missing_isos %>%
            dplyr::as_data_frame(  ) %>%
            dplyr::rename( iso = value ) %>%
            dplyr::mutate_at( X_EDGAR_inv_years, funs( identity( NA_real_ ) ) ) %>%
            tidyr::gather( year, Ratio_N2O_per_Nox, X_EDGAR_inv_years ) %>%
            dplyr::mutate( IPCC = "1B2", IPCC_description = "Fugitive emissions from oil and gas" )

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
    missing_early_years_EDGAR <- subset( emissions_years, emissions_years < EDGAR_inv_start_year )
    X_missing_early_years_EDGAR <- paste0( "X", missing_early_years_EDGAR )
    missing_late_years_EDGAR <- subset( emissions_years, emissions_years > EDGAR_inv_end_year )
    X_missing_late_years_EDGAR <- paste0( "X", missing_late_years_EDGAR )

    EDGAR_ratios_extended <- EDGAR_national_ratio_with_global_ratio_fix %>%
        tidyr::spread( year, Ratio_N2O_per_Nox ) %>%
        dplyr::mutate_at( X_missing_early_years_EDGAR, funs( identity( !!rlang::sym( first( X_EDGAR_inv_years ) ) ) ) ) %>%
        dplyr::mutate_at( X_missing_late_years_EDGAR, funs( identity( !!rlang::sym( last( X_EDGAR_inv_years ) ) ) ) ) %>%
        dplyr::select( iso, X_emissions_years ) %>%
        tidyr::gather( year, Ratio_N2O_per_Nox,  X_emissions_years )


#   Apply Ratio to emissions
    GAINS_fugitive_emissions_final <- GAINS_fugitive_emissions_final %>%
        dplyr::ungroup( ) %>%
        tidyr::gather( year, NOx_emissions, X_emissions_years ) %>%
        dplyr::left_join( EDGAR_ratios_extended, by = c( "iso", "year" ) ) %>%
        dplyr::mutate( N2O_emissions = NOx_emissions * Ratio_N2O_per_Nox ) %>%
        dplyr::select( -NOx_emissions, -Ratio_N2O_per_Nox ) %>%
        tidyr::spread( year, N2O_emissions )

}

# ------------------------------------------------------------------------------

# 12. Output
writeData( GAINS_fugitive_emissions_final, domain = "DIAG_OUT", fn = paste0( "C.GAINS_NC_Emissions_", em ) )
writeData( Oil_production, domain = "DIAG_OUT", fn = paste0( "C.BP_and_IEA_oil_production"  ) )
writeData( Gas_production, domain = "DIAG_OUT", fn = paste0( "C.BP_and_IEA_natural_gas_production" ) )

addToEmissionsDb( GAINS_fugitive_emissions_final, em = em, type = 'NC', ext_backward = FALSE, ext_forward = FALSE )

logStop()
# END
