# ------------------------------------------------------------------------------
# Program Name: C1.2.GAINS_fugitive_petr_gas_emissions.R
# Author(s): Patrick O'Rourke
# Date Last Modified: August 13, 2020
# Program Purpose: To reformat the non-combustion fugitive oil and gas emissions
#                  from GAINS and create gain fugitive subsector shares of total
#                  fugitive oil and gas related emissions
# Input Files: GAINS_EMF30_EMISSIONS_extended_Ev5a_CLE_Nov2015.xlsx,
#              emf-30_ctry_map.csv, emf-30_fuel_sector_map.csv,
#              Master_Country_List.csv, E.CO2_CDIAC_inventory.csv,
#              BP_energy_data.xlsx, A.en_stat_sector_fuel.csv,
#              A.UN_pop_master.csv, EDGAR42_NOx.csv (if em is N2O), EDGAR42_N2O.csv (if em is N2O)
# Output Files: C.GAINS_NC_Emissions_[em].csv, C.BP_and_IEA_oil_production.csv,
#               C.BP_and_IEA_natural_gas_production.csv, C.[em]_GAINS_fug_oil_gas_shares.csv
# TODO: Address remaining doc TODOs
#       Review when updating to new BP, IEA, GAINS, or EDGAR data
#       Make this backwards compatible with previous version of BP energy data (which goes to 2014)
# Notes:

# -----------------------------------------------------------------------------

# 0. Read in global settings and headers
#   Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
#   to the "input" directory.
    PARAM_DIR <- if( "input" %in% dir( ) ) "code/parameters/" else "../code/parameters/"

#   Universal header file - provides logging, file support, etc.
    headers <- c( 'process_db_functions.R', 'data_functions.R',
                  'interpolation_extension_functions.R', 'common_data.R',
                  'analysis_functions.R' )

#   First message to be printed to the log
    log_msg <- paste0( "Processing GAINS non-combustion default oil and gas fugitive emissions data..." )

    script_name <- "C1.2.GAINS_fugitive_petr_gas_emissions.R"

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
        GAINS_EMISS_YEARS <- GAINS_years # Currently: 1990-2050, every 5 years
        GAINS_START_YEAR <- GAINS_EMISS_YEARS[[1]]
        GAINS_START_YEAR_X <- paste0( "X", GAINS_START_YEAR )
        LAST_GAINS_YEAR_FOR_INTERP <- min( GAINS_EMISS_YEARS[ GAINS_years >=BP_last_year ] )

#       Years within GAINS emissions data which are in CEDS, years needed to interpolate GAINS data, and GAINS data with interpolated years
#       Note: Keeping GAINS 2020 data for interpolation purposes
        GAINS_EMISS_YEARS_KEEP <- subset( GAINS_EMISS_YEARS, GAINS_EMISS_YEARS %in% start_year : LAST_GAINS_YEAR_FOR_INTERP ) # Currently: 2000, 2005, 2010, 2020
        GAINS_EMISS_YEARS_KEEP_X <- paste0( "X", GAINS_EMISS_YEARS_KEEP )                                                     # Currently: X2000, X2005, X2010, X2020
        GAINS_INTERP_YEARS_X <- paste0( "X", GAINS_START_YEAR : LAST_GAINS_YEAR_FOR_INTERP )                                  # Currently: X2000-X2020
        MISSING_GAINS_YEARS_TO_MAKE <- subset( GAINS_INTERP_YEARS_X, GAINS_INTERP_YEARS_X %!in% GAINS_EMISS_YEARS_KEEP_X )    # Currently: X2001-X2004, X2006-X2009, X2011-2019

#       Final GAINS years retained (after interpolation between 2010 and 2020, drop years after CEDS end year)
        GAINS_FINAL_YEARS_WITH_X <- paste0( "X", min( as.numeric( GAINS_EMISS_YEARS_KEEP ) ): end_year ) # Currently: X2000-X2014

#       CDIAC Years that are within CEDS years
        CDIAC_YEARS_X <- paste0( "X", start_year : cdiac_end_year ) # Currently X1960-X2011

#       CDIAC end year with X
        X_CDIAC_END_YEAR <- paste0( "X",  cdiac_end_year ) # Currently 2011

#       CDIAC Years extended years (years beyond the final CDIAC year and all CDIAC years with extension)
        CDIAC_YEARS_EXTEND_TO <- paste0( "X", ( cdiac_end_year + 1 ) :  LAST_GAINS_YEAR_FOR_INTERP )                     # Currently: X2012-X2020
        CDIAC_EXTENDED_YEARS_X <- c( CDIAC_YEARS_X, paste0( "X", ( cdiac_end_year + 1 ) : LAST_GAINS_YEAR_FOR_INTERP ) ) # Currently: X1960-X2020

#       BP and IEA years - for Oil and NG production
        BP_OIL_YEARS_x <- paste0( 'X', historical_end_extension_year : BP_actual_last_year )  # Currently: X1965-X2018
        BP_GAS_YEARS_x <- paste0( 'X', 1970 : BP_actual_last_year )                           # Currently: X1970-X2018
        IEA_NA_YEARS <- paste0( "X", start_year : 1970 )                                      # Currently: X1960-X1970 the data is 0 for all isos for crude oil production
                                                                                              # TODO: This above note about NA IEA years may change with updated IEA data
        IEA_EXT_TO_BP_END_YEAR <- paste0( "X", ( IEA_end_year + 1 ): BP_actual_last_year )    # Currently: 2018
        production_extension_GAINS_years <- paste0( "X", ( BP_actual_last_year + 1 ): LAST_GAINS_YEAR_FOR_INTERP ) # Currently: X2019-X2020, Used for extending oil and natural gas production data to final GAINS year
        IEA_AND_BP_YEARS_x <- paste0( "X", IEA_start_year : BP_actual_last_year )

#       EDGAR inventory years
        EDGAR_INV_START_YEAR <- EDGAR_start_year # common_data.R object
        EDGAR_INV_END_YEAR <- EDGAR_end_year     # common_data.R object
        X_EDGAR_INV_YEARS <- paste0( "X", EDGAR_INV_START_YEAR : EDGAR_INV_END_YEAR )          # Currently: X1970-X2015

#       EDGAR extended years (years beyond EDGAr years which are in CEDS final emissions). Used for estimating
#       fugitive N2O emissions from fugitive NOx emissions using EDGAR emissions ratios.
        X_MISSING_EDGAR_EARLY_YEARS <- paste0( "X", subset( emissions_years, emissions_years < EDGAR_INV_START_YEAR ) ) # Currently: X1960-X1969
        X_MISSING_EDGAR_LATE_YEARS <- paste0( "X", subset( emissions_years, emissions_years > EDGAR_INV_END_YEAR ) )   # Currently: X2009-X2014

#   Define GAINS fugitive sectors for oil and gas
#   TODO: Use mapping file in future to define this list of GAINS loss sectors
    GAINS_FUGITIVE_SECTORS <- c( "Losses_Distribution_Use" , "Losses_Prod_Conventional_Gas",
                                 "Losses_Prod_Shale_Gas", "Losses_Prod_Oil",
                                 "Losses_Prod_Oil", "Losses_Prod_Oil" )

#   Define aggregate Former Soviet Union (FSU) iso and subregional FSU isos. Used for disaggregating fossil fuel production data
    fsu <- 'ussr'
    fsu_members <- c( "arm", "aze", "blr", "est", "geo", "kaz", "kgz", "ltu", "lva", "mda", "rus", "tjk", "tkm", "ukr", "uzb" )
    #TODO: BP states the following: "USSR includes Georgia, Ukraine and the Baltic States." Check that the above matches with this definition.
# ------------------------------------------------------------------------------

# 2. Input

#   GAINS data and mapping files

    # Define  settings for GAINS data
    domain_use <- "EM_INV"
    domain_ext_use <- "GAINS/"

    emissions_file_name <- paste0( "Global by region-detail_emf30_", e_sheet, "_wSE" )
    if( em == "SO2" ){ emissions_file_name <- gsub( "_wSE", "_v2_wSE", emissions_file_name ) }

    emissions_rows_to_skip <- 9

    # Read in GAINS data
    GAINS_emiss <- readData( domain = domain_use, domain_extension = domain_ext_use,
                                 file_name = emissions_file_name, skip = emissions_rows_to_skip )

    # Read un GAINS maps
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
    bp_energy_data <- readData( "ENERGY_IN",BP_data_file_name, ".xlsx", skip = 2 )
    BP_oil_production <- bp_energy_data[[ getBPSheetNumber( "oil", "production", "tonnes", bp_energy_data ) ]]
    BP_gas_production <- bp_energy_data[[ getBPSheetNumber( "gas", "production", "EJ", bp_energy_data ) ]]

#   IEA crude oil and NG production data
#   TODO: Production data in this file has the value 0 for some year + iso combinations where the data should likely be NA.
#         For example, Norway has 0 production in 1970, but nearly 300kt of production in 1971.
#         This should be fixed before this script.
    en_stat_sector_fuel <- readData( 'MED_OUT', file_name = 'A.en_stat_sector_fuel' )

#   UN Population data
    pop <- readData( "MED_OUT", "A.UN_pop_master" )

# If em = N2O, load EDGAR NOx and N2O data
    if( em == "N2O" ){

      EDGAR_N2O <- readData( domain = "MED_OUT", file_name = paste0( "E.N2O_EDGAR" ))

      EDGAR_NOx <-  readData( domain = "MED_OUT", file_name = paste0( "E.NOx_EDGAR" ))

    }

# ------------------------------------------------------------------------------

# 3. Reformat mapping files

#   Reformat GAINS sector map
    GAINS_fugitive_sectors_df <- GAINS_sector_map %>%
        dplyr::select( emf_sector, ceds_sector ) %>%
        dplyr::filter( emf_sector %in% GAINS_FUGITIVE_SECTORS ) %>%
        dplyr::distinct( ) %>%
        dplyr::rename( Sector = emf_sector )

#   Make list of unique final isos (isos with final_data_flag = 1, srb (kosovo),
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
        dplyr::select( Region, iso ) %>%
        dplyr::filter( !( is.na ( iso ) ) ) %>%
        dplyr::distinct( )

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
        tidyr::gather( key = years, value = Emissions, paste0( "X", GAINS_EMISS_YEARS ) ) %>%
        dplyr::filter( years %in% GAINS_EMISS_YEARS_KEEP_X,
                       Region != "Global" )  %>%
        dplyr::rename( Sector = EMF30.kt )

#   Provide units - note that CO2 emissions are provided as Tg rather than GG or kt
#   (as noted in B1.1.base_comb_GAINS_EMF-30.R) and must be converted.
    if( e_sheet == 'CO2' ){

        printLog( "Converting GAINS CO2 data from Mt to kt (only CO2 is provided in Mt from GAINS..." )

        kt_per_Tg <- 1000

        GAINS_emiss_UnitsFixed <- GAINS_emiss_clean %>%
            dplyr::mutate( Emissions_convert_unit = Emissions * kt_per_Tg ) %>%
            dplyr::select( -Emissions ) %>%
            dplyr::rename( Emissions = Emissions_convert_unit)

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
        dplyr::summarise_all( list( ~sum (., na.rm = TRUE) ) ) %>%
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

#   Seperate emissions for processing - (1) NG distribution fug. emiss,
#   (2) NG production ug. emiss, and (3) Oil production fug. emiss
    GAINS_fug_NG_distribution <- GAINS_FugEmiss_isoMapped %>%
        dplyr::filter( ceds_sector == "1B2b_Fugitive-NG-distr" )

    GAINS_fug_NG_prod <- GAINS_FugEmiss_isoMapped %>%
        dplyr::filter( ceds_sector == "1B2b_Fugitive-NG-prod" )

    GAINS_fug_oil_prod <- GAINS_FugEmiss_isoMapped %>%
        dplyr::filter( ceds_sector == "1B2_Fugitive-petr" )

# ------------------------------------------------------------------------------

# 5. Reformat CDIAC Driver data

# Subset NG Emissions and retain appropriate years (1960-2011).
# Note that CDIAC inventory only goes to 2011 (other than cement which was extended during initial CDIAC processing),
# so after subsetting appropriate years, extend CDIAC emissions to 2020 for EF (constant, equal to 2011 values).
  CDIAC_CO2_gas <- CDIAC_CO2_inventory %>%
    dplyr::filter( fuel == "gas_fuels" ) %>%
    dplyr::select( iso, fuel, all_of(CDIAC_YEARS_X) ) %>%
    dplyr::filter( iso != "global" ) %>%
    dplyr::mutate_at( CDIAC_YEARS_EXTEND_TO, funs( identity( !!rlang::sym( X_CDIAC_END_YEAR ) ) ) )

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
        dplyr::select( Region, iso, fuel, all_of(CDIAC_EXTENDED_YEARS_X) ) %>%
        dplyr::filter( !( is.na( Region ) ) ) %>%
        tidyr::gather( key = years, value = CDIAC_Emissions, all_of(CDIAC_EXTENDED_YEARS_X) )

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
        dplyr::select( Region, iso, ceds_sector, EF_units, all_of(GAINS_INTERP_YEARS_X) ) %>%
        interpolate_NAs2( ) %>%
        dplyr::select( Region, iso, ceds_sector, EF_units, all_of(GAINS_FINAL_YEARS_WITH_X) )

#   Extend earliest EF year backwards to start_year (currently 2000 extended back to 1960, constant extension )
    missing_years_for_extending <- paste0('X', start_year:( as.numeric( GAINS_START_YEAR ) - 1 ) ) # Currently: 1960-1999

    NG_distribution_EF_extended <- NG_distribution_EF_interp %>%
        dplyr::mutate_at( missing_years_for_extending, funs( identity( X2000 ) ) ) %>%
        dplyr::select( Region, iso, ceds_sector, EF_units, all_of(X_emissions_years) ) %>%
        tidyr::gather( key = years, value = EFs, all_of(X_emissions_years) )

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

# TODO: Disaggregate BP data for "other region" isos ("other africa"...), and split historical data for certain regions (for example: Sudan and S. Sudan)
# TODO: Align this with the MCL BP_oil name column (so only one mapping file is needed, and not this listing below - MCL may need to be fixed)

#   Initial cleaning of BP data (clean, map, convert units)
    not_BP_countries <- c( "Total North America", "Other S. & Cent. America", "Total S. & Cent. America", "Other Europe",
                           "Total Europe", "Other CIS", "Total CIS", "Other Middle East", "Total Middle East", "Other Africa",
                           "Total Africa", "Other Asia Pacific", "Total Asia Pacific", "Total World", "of which: OECD",
                           "                 Non-OECD", "                 OPEC", "                 Non-OPEC ",
                           "                 European Union #", " * Includes crude oil, shale oil, oil sands, condensates (both lease condensate and gas plant condensate) and NGLs (natural gas liquids - ethane, LPG and naptha separated from the production of natural gas). ",
                           "Excludes liquid fuels from other sources such as biomass and derivatives of coal and natural gas.",
                           " ^ Less than 0.05.", "w Less than 0.05%.", "n/a not applicable.", "USSR includes Georgia, Ukraine and the Baltic States.",
                           " # Excludes Estonia, Latvia and Lithuania prior to 1985 and Croatia and Slovenia prior to 1990.",
                           "Notes: Annual changes and shares of total are calculated using million tonnes figures."  )

    BP_oil_data <- BP_oil_production %>% dplyr::select( -matches("_|-") )

    colnames( BP_oil_data ) <- c( 'BPName_Oil_production', BP_OIL_YEARS_x )

    BP_oil <- BP_oil_data %>%
        dplyr::select( BPName_Oil_production, all_of(BP_OIL_YEARS_x) ) %>%
        dplyr::filter_at( .vars = colnames( BP_oil_data ), any_vars( !is.na( . ) ) ) %>%
        dplyr::left_join( MCL, by = "BPName_Oil_production" ) %>%
        dplyr::filter( !is.na( BPName ) ) %>%
        tidyr::gather( key = years, value = Oil_production, all_of(BP_OIL_YEARS_x) ) %>%
        dplyr::mutate( Oil_production = as.numeric( Oil_production ),
                       Oil_production = Oil_production * 1000000 * ( 1 / 1000 ) ) %>% # Convert from million tonnes to kt
        tidyr::spread( years, Oil_production ) %>%
        dplyr::select( iso, all_of(BP_OIL_YEARS_x) ) %>%
        dplyr::arrange( iso )

#   Disaggregate BP's FSU data (from 1965-1984 disaggregate FSU to FSU members).
#   FSU member countries have no data before 1985 in raw BP data, but total production is reported for FSU.
#   Here the code splits the ussr( fsu ) and distributes to fsu member countries
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
    BP_oil$sector <- "oil_production"
    BP_oil_combined <- rbind( oil_production,
                              dplyr::filter(BP_oil, !(iso %in% BP_fsu_members$iso ) ) )

    BP_oil_fixed <- BP_oil_combined %>%
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
#   and extend 2013 (final IEA production year) to final BP year (actual BP end year, using constant extension)
#   TODO: This may not be 0 for 1960-1970 in future IEA data - this should be checked when updating IEA data versions.
      IEA_oil <- en_stat_sector_fuel %>%
        dplyr::filter( sector == "crude-oil-production",
                       !( iso %in% BP_oil_isos ) ) %>%
        dplyr::left_join( MCL_unique_with_OECD_flag, by = "iso" ) %>%
        dplyr::mutate_at( IEA_NA_YEARS, funs( identity( NA ) ) )  %>%  # All data in these years is currently 0 for countries not in BP, and begins in 1971 (when non-oecd data begins)
        dplyr::mutate_at( .vars = IEA_EXT_TO_BP_END_YEAR, funs( + ( !!rlang::sym( X_IEA_end_year ) ) ) ) %>%
        dplyr::mutate( sector = "oil_production",
                       data_source = "IEA" ) %>%
        dplyr::select( data_source, iso, sector, fuel, units, all_of(IEA_AND_BP_YEARS_x) )

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
        dplyr::select( Region, iso, sector, fuel, units, all_of(IEA_AND_BP_YEARS_x) )

#   Aggregate by region for each year, extend aggregate data forward to 2020 for EF
#   Note: We are aggregating data for which some isos might be NA (IEA's first reported year of data is 1971, so only
#   isos for a given GAINS region which had data from BP are aggregated. However, this aggregate info isn't used below,
#   as the aggRegion EF is created only for years within "GAINS_EMISS_YEARS_KEEP_X" (currently 2000, 2005, 2010, and 2020)
    Oil_production_aggRegion_for_EF <- Oil_production_region_mapped %>%
        dplyr::select( -iso ) %>%
        dplyr::group_by( Region, sector, fuel, units ) %>%
        dplyr::summarise_all( funs( sum ( ., na.rm = TRUE) ) ) %>%
        dplyr::ungroup( ) %>%
        dplyr::mutate_at( production_extension_GAINS_years, funs( identity( !!rlang::sym( paste0( "X", BP_actual_last_year ) ) ) ) ) %>%
        dplyr::rename( units_production = units ) %>%
        dplyr::select( Region, sector, fuel, units_production, all_of(GAINS_EMISS_YEARS_KEEP_X) ) %>%
        tidyr::gather( key = years, value = Oil_production, all_of(GAINS_EMISS_YEARS_KEEP_X) )

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
        interpolate_NAs2( ) %>%
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

# TODO: Disaggregate BP data for "other region" isos ("other africa"...), and split historical data for certain regions (for example: Sudan and S. Sudan)
# TODO: Align this with the MCL BP_oil name column (so can just use the mapping file - MCL may need to be fixed)

#   Initial cleaning of BP data (clean, map, convert units)
    not_BP_countries <- c( not_BP_countries,
                           " * Excludes gas flared or recycled. Includes natural gas produced for Gas-to-Liquids transformation.",
                           " n/a not available.",
                           " # Excludes Estonia, Latvia and Lithuania prior to 1985 and Croatia and Slovenia prior to 1990.",
                           "Note: Annual changes and shares of total are calculated using million tonnes oil equivalent figures." )

    BP_gas_data <- BP_gas_production %>% dplyr::select( -matches("_|-") )
    colnames( BP_gas_data ) <- c( 'BPName_Gas_production', BP_GAS_YEARS_x )

    MCL_BP_gas_map <- MCL %>%
        dplyr::select( iso, BPName_Gas_production ) %>%
        dplyr::distinct( )

    conversionFactor_TJ_per_EJ <- 1E6

    BP_gas <- BP_gas_data %>%
        dplyr::filter_at( .vars = colnames( BP_gas_data ), any_vars( !is.na( . ) ) ) %>%
        dplyr::left_join( MCL, by = "BPName_Gas_production" ) %>%
        dplyr::filter( !is.na( BPName ) ) %>%
        tidyr::gather( key = years, value = gas_production, all_of(BP_GAS_YEARS_x) ) %>%
        dplyr::mutate( gas_production = as.numeric( gas_production ),
                       gas_production = gas_production * conversionFactor_TJ_per_EJ * ( 1 / conversionFactor_naturalgas_TJ_per_kt_Net ) ) %>% # Convert from EJ to kt NG
        tidyr::spread( years, gas_production ) %>%
        dplyr::select( iso, all_of(BP_GAS_YEARS_x) ) %>%
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
        dplyr::filter( year %in% BP_GAS_YEARS_x,
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
    BP_gas$sector <- "gas_production"
    BP_gas_combined <- rbind( gas_production,
                              dplyr::filter(BP_gas, !(iso %in% BP_fsu_members$iso ) ) )

    BP_gas_fixed <- BP_gas_combined %>%
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
#   and extend 2013 (final IEA production year) to final BP year (actual BP end year, using constant extension)
#   TODO: This may not be 0 for 1960-1970 in future IEA data - this should be checked when updating IEA data versions.
#   TODO: Should we set 1960-1970 NA for the OECD countries that are all 0 for these years? (seem to be missing data for these isos too)
      IEA_gas <- en_stat_sector_fuel %>%
        dplyr::filter( sector == "natural_gas-production",
                       !( iso %in% BP_gas_isos ) ) %>%
        dplyr::left_join( MCL_unique_with_OECD_flag, by = "iso" ) %>%
        dplyr::mutate_at( IEA_NA_YEARS, funs( if_else( OECD_flag == "NonOECD", NA_real_ , . ) ) )  %>%
        dplyr::mutate_at( .vars = IEA_EXT_TO_BP_END_YEAR, funs( + ( !!rlang::sym( X_IEA_end_year ) ) ) ) %>%
        dplyr::mutate( sector = "gas_production",
                       data_source = "IEA" ) %>%
        dplyr::select( data_source, iso, sector, fuel, units, all_of(IEA_AND_BP_YEARS_x) )

#   Combine IEA and BP gas data
    Gas_production <- dplyr::bind_rows( IEA_gas, BP_gas_fixed )

# ------------------------------------------------------------------------------

# 10. Downscale fugitive natural gas production emissions using BP and IEA natural gas production data

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
        dplyr::select( Region, iso, sector, fuel, units, all_of(IEA_AND_BP_YEARS_x) )

#   Aggregate by region for each year, extend aggregate data forward to 2020 for EF (currently extending 2014 to 2020)
#   Note: We are aggregating data for which some isos might be NA (IEA's first reported year of data is 1971, so only
#   isos for a given GAINS region which had data from BP are aggregated. However, this aggregate info isn't used below,
#   as the aggRegion EF is created only for years within "GAINS_EMISS_YEARS_KEEP_X" (currently 2000, 2005, 2010, and 2020)
    Gas_production_aggRegion_for_EF <- Gas_production_region_mapped %>%
        dplyr::select( -iso ) %>%
        dplyr::group_by( Region, sector, fuel, units ) %>%
        dplyr::summarise_all( funs( sum ( ., na.rm = TRUE) ) ) %>%
        dplyr::ungroup( ) %>%
        dplyr::mutate_at( production_extension_GAINS_years, funs( identity( !!rlang::sym( paste0( "X", BP_actual_last_year ) ) ) ) ) %>%
        dplyr::rename( units_production = units ) %>%
        dplyr::select( Region, sector, fuel, units_production, all_of(GAINS_EMISS_YEARS_KEEP_X) ) %>%
        tidyr::gather( key = years, value = gas_production, all_of(GAINS_EMISS_YEARS_KEEP_X) )

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
        dplyr::select( Region, iso, ceds_sector, EF_units, all_of(GAINS_INTERP_YEARS_X) ) %>%
        interpolate_NAs2( ) %>%
        dplyr::select( Region, iso, ceds_sector, EF_units, all_of(GAINS_INTERP_YEARS_X) )

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
# 11. GAINS doesn't have N2O data. If N2O, then derive an emissions N2O/NOx
#   emissions ratio from EDGAR, and apply to GAINS NOx emissions.
#   N2O emissions are small and they are both combustion data so should be fine
#   For uncalculated ratio (zero NOx emissions, or no data):
#   - If later year ratio - extend ratio from previous year
#   - Replace with Global Ratio
#    - Replace with

if( em == "N2O" ){

loadPackage('zoo')

  printLog( "Using EDGAR N2O and NOx data to produce default fugitive oil and gas emissions, as GAINS only provides",
            "NOx data for these sectors..." )

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
    GAINS_fugitive_emissions_final <- GAINS_fugitive_emissions_final %>%
        gather(year, NOx_emissions,-iso,-sector,-fuel,-units) %>%
        dplyr::left_join( EDGAR_ratio_final %>% gather(year, ratio,-iso,-sector,-units) ) %>%
        dplyr::mutate( N2O_emissions = NOx_emissions * ratio ) %>%
        dplyr::select( -NOx_emissions, -ratio ) %>%
        tidyr::spread( year, N2O_emissions )

}

# ------------------------------------------------------------------------------

# 12. Create a data frame of the shares of total oil and gas fugitive emissions for each of the
#     3 disaggregate sectors (B2_Fugitive-petr, 1B2b_Fugitive-NG-prod, 1B2b_Fugitive-NG-distr)

#   Given that production data does not exist for all isos in all years, for any iso that has
#   NA emissions for any of the 3 fugitive oil and gas subsectors, set all emissions to NA in the year.
#   This will allow the given year to have emission shares from the closest year that has no NAs for the
#   3 subsectors, or to have shares interpolated between the two closest years that have no NAs for the 3 subsectors.
    GAINS_fugitive_emissions_spread_sector <- GAINS_fugitive_emissions_final %>%
        tidyr::gather( key = years, value = Emissions, all_of(X_emissions_years) ) %>%
        tidyr::spread( sector, Emissions )

    new_fugitive_sectors <- c( "1B2_Fugitive-petr", "1B2b_Fugitive-NG-distr", "1B2b_Fugitive-NG-prod" )

    GAINS_fugitive_emissions_splits_fixed <- GAINS_fugitive_emissions_spread_sector %>%
        dplyr::filter_at( .vars = new_fugitive_sectors, any_vars( is.na( . ) ) ) %>%
        dplyr::mutate_at( .vars = new_fugitive_sectors, funs( identity( NA_real_ ) ) )

    GAINS_fugitive_emissions_for_splits <- GAINS_fugitive_emissions_spread_sector %>%
        dplyr::filter_at( .vars = new_fugitive_sectors, all_vars( !is.na( . ) ) ) %>%
        dplyr::bind_rows( GAINS_fugitive_emissions_splits_fixed ) %>%
        tidyr::gather( key = sector, value = Emissions, new_fugitive_sectors ) %>%
        tidyr::spread( years, Emissions )

    if( nrow( GAINS_fugitive_emissions_for_splits ) != nrow( GAINS_fugitive_emissions_final ) ){

        stop( "Correcting emissions data prior to creating fugitive subsector splits has produced a data frame with less ",
              "rows than the original emissions data. This should not happen..." )
    }

#   Reset NAs to 0
    GAINS_fugitive_emissions_final_add_zeroes <- GAINS_fugitive_emissions_for_splits %>%
        dplyr::mutate_at( X_emissions_years, funs( if_else( is.na( . ),  0, .) ) )

    GAINS_fug_emiss_final_add_zero_long <- GAINS_fugitive_emissions_final_add_zeroes %>%
        tidyr::gather( year, disagg_fugitive_emissions, all_of(X_emissions_years) )

#   Create data frames of total fugitive oil and gas emissions - by iso
    GAINS_fugitive_total_oil_and_gas <- GAINS_fugitive_emissions_final_add_zeroes %>%
        dplyr::mutate( sector = "total fugitive oil and gas emissions" ) %>%
        dplyr::group_by( iso, sector, fuel, units ) %>%
        dplyr::summarise_all( sum, na.rm = TRUE ) %>%
        dplyr::ungroup( )

    GAINS_fugitive_total_oil_and_gas_long <- GAINS_fugitive_total_oil_and_gas %>%
        tidyr::gather( year, total_fugitive_oil_and_gas_emissions, all_of(X_emissions_years) )

#   Create data frames of iso specific shares of total fugitive oil and gas emissions for each oil and gas fugitive subsector
    GAINS_fug_oil_gas_splits_by_iso <- GAINS_fug_emiss_final_add_zero_long %>%
        dplyr::left_join( GAINS_fugitive_total_oil_and_gas_long, by = c( "iso", "fuel", "units", "year" ) ) %>%
        dplyr::mutate( share = disagg_fugitive_emissions / total_fugitive_oil_and_gas_emissions ) %>%
        dplyr::rename( sector = sector.x ) %>%
        dplyr::select( -sector.y )

#   Subset splits based on whether a row has (1) no NAs in X_emissions_years, (2) at least 1 NA value in X_emissions_years,
#   or (3) NA values for each column within X_emissions_years.
    GAINS_fug_oil_gas_splits_by_iso <- GAINS_fug_oil_gas_splits_by_iso %>%
        dplyr::select( -disagg_fugitive_emissions, -total_fugitive_oil_and_gas_emissions ) %>%
        tidyr::spread( year, share ) %>%
        dplyr::select( iso, sector, fuel, units, all_of(X_emissions_years) )

    GAINS_fug_splits_no_NA <- GAINS_fug_oil_gas_splits_by_iso %>%
        dplyr::mutate_at( X_emissions_years, funs( if_else( is.nan( . ), NA_real_, . ) ) ) %>%
        dplyr::filter_at( .vars = X_emissions_years, all_vars( !is.na( .) ) )

    GAINS_fug_splits_all_NA <- GAINS_fug_oil_gas_splits_by_iso %>%
        dplyr::mutate_at( X_emissions_years, funs( if_else( is.nan( . ), NA_real_, . ) ) ) %>%
        dplyr::filter_at( .vars = X_emissions_years, all_vars( is.na( .) ) )

    GAINS_fug_splits_some_NA <- GAINS_fug_oil_gas_splits_by_iso %>%
        dplyr::mutate_at( X_emissions_years, funs( if_else( is.nan( . ), NA_real_, . ) ) ) %>%
        dplyr::filter_at( .vars = X_emissions_years, any_vars( is.na( .) ) ) %>%
        dplyr::filter_at( .vars = X_emissions_years, any_vars( !is.na( .) ) )

#   For splits which have some NAs, extend first and last non-NA values backwards and
#   forwards (and interpolates NAs if needed)
    if( nrow( GAINS_fug_splits_some_NA ) != 0 ){

        GAINS_fug_splits_some_NA_fixed <- extend_and_interpolate( GAINS_fug_splits_some_NA , X_emissions_years )

    }

#   Check and make sure that none of the isos with all NAs for splits produce oil or NG, as those isos
#   should have emissions in at least one of the fugitive production subsectors, given that production data
#   was used to downscale GAINS regional emissions data.
#   Note: This may not be true for an emission species that doesn't get emitted by fugitive production subsectors.
#         Currently all emissions using this script ( GAINS emissions [and N2O]: BC, CH4, CO, CO2, NOx,
#         N2O, NMVOC, OC, SO2) all have emissions for oil and/or NG production, making this check work, but adding future
#         ems may mean this check needs to change (could add an if statement - if any isos have non-NA or non-zero values
#         for the production related fugitive emissions, then continue with the check...)
    if( nrow( GAINS_fug_splits_all_NA ) != 0 ){

    all_na_isos <- sort( unique( GAINS_fug_splits_all_NA$iso ) )

    Oil_producing_isos <- Oil_production %>%
        dplyr::filter_at( .vars = X_emissions_years, any_vars( !(is.na( .) ) ) ) %>%
        dplyr::select( iso ) %>%
        dplyr::distinct( )

    Gas_producing_isos <- Gas_production %>%
        dplyr::filter_at( .vars = X_emissions_years, any_vars( !(is.na( .) ) ) ) %>%
        dplyr::select( iso ) %>%
        dplyr::distinct( )

    all_na_isos_that_produce_oil_or_ng <- subset(  all_na_isos, all_na_isos %in%
                                                   c( Gas_producing_isos, Oil_producing_isos ) )

    if( length( all_na_isos_that_produce_oil_or_ng ) != 0 ){

        stop( "isos which produce oil or gas should not have all NA share splits ",
              "for fugitive oil and gas emissions subsectors...See C1.2.GAINS_fugitive_petr_gas_emissions.R")

    }

# For isos with all NA shares for fugitive oil and gas subsector emissions:
#   If em is BC, CO, CO2, NMVOC, NOx, N2O, OC, or SO2 - set default values to "1B2_Fugitive-petr" = 1,
#       "1B2b_Fugitive-NG-prod" = 0, and "1B2b_Fugitive-NG-distr" = 0, as only oil production emits these emissions species
#       (according to  the GAINS inventory). When these splits are applied to EDGAR-ECLIPSE aggregate fugitive oil and gas
#       emissions, these defaults will provide emissions to the right sectors for isos which have no BP or IEA production
#       data but have emissions from aggregative fugitive oil and gas emissions for an em which only can come from oil production.
    if( em %in% c( "BC", "CO", "CO2", "NMVOC", "NOx", "N2O", "OC", "SO2" ) ){

        GAINS_fug_splits_all_NA_fixed <- GAINS_fug_splits_all_NA %>%
            dplyr::mutate_at( X_emissions_years, funs( if_else( sector == "1B2_Fugitive-petr", 1,
                                                       if_else( sector == "1B2b_Fugitive-NG-prod", 0,
                                                       if_else( sector == "1B2b_Fugitive-NG-distr", 0,
                                                                NA_real_ ) ) ) ) )

    } else if( em == "CH4" ){

#   If em is CH4 - set default values to "1B2_Fugitive-petr" = 0, "1B2b_Fugitive-NG-prod" = 0,
#       and "1B2b_Fugitive-NG-distr" = 1, as these isos are not supposed to be producing oil or gas, meaning
#       the fugitive emissions should only come from NG distribution. While it is possible that some of the CH4
#       emissions could come from oil and NG production (implying we don't have production data for an iso which
#       does produce NG or oil), since we don't have production data for these isos we wouldn't know what split
#       the 2 production subsectors should receive.
        GAINS_fug_splits_all_NA_fixed <- GAINS_fug_splits_all_NA %>%
            dplyr::mutate_at( X_emissions_years, funs( if_else( sector == "1B2_Fugitive-petr", 0,
                                                   if_else( sector == "1B2b_Fugitive-NG-prod", 0,
                                                   if_else( sector == "1B2b_Fugitive-NG-distr", 1,
                                                            NA_real_ ) ) ) ) )
    } else {

        stop( "options have not been set for how to replace all NA row fugitive oil and gas subsector emissions for ",
              em, ". See C1.2.GAINS_fugitive_petr_gas_emissions.R" )

    }

}

#   Combine all of the GAINS fugitive split data frames

#       If there was at originally least 1 row that was all NA for X_emissions_years,
#       and at least one row which was had at least one NA value for X_emissions_years (but not all NA),
#       rbind these 2 fixed splits dfs to the df of no NAs.
        if( nrow( GAINS_fug_splits_all_NA ) != 0 & nrow( GAINS_fug_splits_some_NA ) != 0 ){

            GAINS_fug_oil_gas_splits_final <- dplyr::bind_rows( GAINS_fug_splits_no_NA,
                                                                GAINS_fug_splits_some_NA_fixed,
                                                                GAINS_fug_splits_all_NA_fixed )

#       If there was originally at least 1 row that was all NA, but no rows which had at 1 least NA (but not all NA),
#       rbind the 1 fixed split df to the df of no NAs
        } else if( nrow( GAINS_fug_splits_all_NA ) != 0 & nrow( GAINS_fug_splits_some_NA ) == 0 ){

            GAINS_fug_oil_gas_splits_final <- dplyr::bind_rows( GAINS_fug_splits_no_NA,
                                                                GAINS_fug_splits_all_NA_fixed )

#       If there were originally no rows that were all NA, but was at least 1 row which had at
        # least one NA (but not all NA), rbind the 1 fixed split df to the df of no NAs
        } else if( nrow( GAINS_fug_splits_all_NA ) == 0 & nrow( GAINS_fug_splits_some_NA ) != 0 ) {

            GAINS_fug_oil_gas_splits_final <- dplyr::bind_rows( GAINS_fug_splits_no_NA,
                                                                GAINS_fug_splits_some_NA_fixed )

#       If there were no rows that were all NA, and no rows that were at least 1 NA (but not all NA),
#       rename the data frame with no NAs as the final splits df.
        } else if( nrow( GAINS_fug_splits_all_NA ) == 0 & nrow( GAINS_fug_splits_some_NA ) == 0 ){

            GAINS_fug_oil_gas_splits_final <- GAINS_fug_splits_no_NA

        }

#   Check that the final splits have the same number of rows and the same column names as the original splits
#   df
    GAINS_fug_oil_gas_splits_final <- GAINS_fug_oil_gas_splits_final %>%
        dplyr::arrange( iso, sector )

    if( nrow( GAINS_fug_oil_gas_splits_final ) != nrow( GAINS_fug_oil_gas_splits_by_iso ) ){

        stop( "Fixed fugitive subsector emission splits created from GAINS do not have the same ",
              "number of rows as the original splits..." )
    }

    if( any( colnames( GAINS_fug_oil_gas_splits_final ) != colnames( GAINS_fug_oil_gas_splits_by_iso ) ) ){

        stop( "Fixed fugitive subsector emission splits created from GAINS do not have the same ",
              "column names as the original splits..." )
    }

#   Check that no final shares are NA or NaN and that all shares sum to 1 for each year
    check_NA <- GAINS_fug_oil_gas_splits_final %>%
        dplyr::filter_at( .vars = X_emissions_years, any_vars( is.na( . ) ) )

    check_NaN <- GAINS_fug_oil_gas_splits_final %>%
        dplyr::filter_at( .vars = X_emissions_years, any_vars( is.nan( . ) ) )

    if( nrow( check_NA ) != 0 | nrow( check_NaN ) ){

        stop( "No final fugitive oil and gas subsector share should be Na or NaN.... see C1.2.GAINS_fugitive_petr_gas_emissions.R" )

    }

    GAINS_share_check <- GAINS_fug_oil_gas_splits_final %>%
        dplyr::select( -sector ) %>%
        dplyr::group_by( iso, fuel, units ) %>%
        dplyr::summarise_all( sum, na.rm = TRUE )

    if( any( round( GAINS_share_check[ , X_emissions_years] , 15 ) != 1 ) ){

        stop( "Final GAINS fugitive oil and gas subsector shares should sum to 1.... see C1.2.GAINS_fugitive_petr_gas_emissions.R..." )

    }

#   Fix units
    GAINS_fug_oil_gas_splits_final <- GAINS_fug_oil_gas_splits_final %>%
        dplyr::mutate( units = "diaggregate fugitive oil and gas shares" )

# ------------------------------------------------------------------------------

# 13. Output

# Final GAINS emissions
writeData( GAINS_fugitive_emissions_final, domain = "DIAG_OUT", fn = paste0( "C.GAINS_NC_Emissions_", em ) )

#  BP & IEA Oil Production Data
writeData( Oil_production, domain = "DIAG_OUT", fn = paste0( "C.BP_and_IEA_oil_production"  ) )

#  BP & IEA Gas Production Data
writeData( Gas_production, domain = "DIAG_OUT", fn = paste0( "C.BP_and_IEA_natural_gas_production" ) )

#  GAINS fugitive oil and gas subsector emission shares (relative to total fugitivce oil and gas emissions)
writeData( GAINS_fug_oil_gas_splits_final, domain = "MED_OUT", fn = paste0( "C.", em, "_GAINS_fug_oil_gas_shares" ) )

logStop( )
# END
