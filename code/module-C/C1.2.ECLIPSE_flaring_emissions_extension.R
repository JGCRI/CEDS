#------------------------------------------------------------------------------
# Program Name: C1.2.ECLIPSE_flaring_emissions_extension.R
# Author: Leyang Feng, Patrick O'Rourke
# Date Last Modified: November 18, 2019
# Program Purpose: Extends ECLIPSE flaring emissions to period 1965 - last BP year using IEA
#                  and BP crude oil production data
# Input Files: [em]_eclipse_flr_emissions.csv, A.en_stat_sector_fuel.csv,
#              [BP_data_file_name].csv, Master_Country_List.csv,
#              A.UN_pop_master.csv
# Output Files: C.[em]_ECLIPSE_flaring_emissions_extended.csv,
# "             C.[em]_ECLIPSE_flaring_to_crude_oil_production_ratios.csv (for ems other than N2O)
# Notes: In section 3.2 the 1970 data are manually 'extended' using 1971 data,
#        the data to last BP year are manually 'extended' using 2013 data.
#
# TODO: 1. About 70 countries in [em]_eclipse_flr_emissions.csv get picked out in the routine
#          because IEA/BP oil production data does not include those countries. Update when
#          a more comprehensive database of historical production is availiable, or when BP/IEA
#          oil production is disaggregated
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
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
# read in the en_stat_sector_fuel.csv to extract IEA crude oil production data
    en_stat_sector_fuel <- readData( 'MED_OUT', file_name = 'A.en_stat_sector_fuel' )
# read in the BP oil production data since this is a longer more consistent time series than IEA
    bp_energy_data <- readData( 'ENERGY_IN', file_name = BP_data_file_name, extension = ".xlsx", skip = 2 )
    bp_oil_prod  <- bp_energy_data[[ getBPSheetNumber( "oil", "production", "tonnes", bp_energy_data ) ]]

# read in master country list
    mcl <- readData( 'MAPPINGS', 'Master_Country_List' )

# read in the population data
    pop_raw <- readData( "MED_OUT", "A.UN_pop_master" )

# If em = N2O, load EDGAR 4.2 NOx and N2O data, as well as the master
  if( em == "N2O" ){

     EDGAR_NOx_fle  <- paste0( 'EDGAR42_', em_use )
     EGAR_N2O_file <- paste0( 'EDGAR42_', em )
     inv_data_folder <- "EM_INV"

     EDGAR_NOx <- readData( inv_data_folder, domain_extension = "EDGAR/", EDGAR_NOx_fle )
     EDGAR_N2O <- readData( inv_data_folder, domain_extension = "EDGAR/", EGAR_N2O_file )

  }

# ------------------------------------------------------------------------------

# 2. Define constants used within this script

if( em == "N2O" ){

#   EDGAR 4.2 inventory years
    EDGAR_INV_START_YEAR <- 1970
    EDGAR_INV_END_YEAR <- 2008
    EDGAR_INV_YEARS <- EDGAR_INV_START_YEAR : EDGAR_INV_END_YEAR # Currently: 1970-2008
    X_EDGAR_INV_YEARS <- paste0( "X", EDGAR_INV_YEARS )          # Currently: X1970-X2008

#   EDGAR 4.2 extended years (years beyond EDGAr years which are in CEDS final emissions). Used for estimating
#   fugitive N2O emissions from fugitive NOx emissions using EDGAR emissions ratios.
    MISSING_EDGAR_EARLY_YEARS <- subset( emissions_years, emissions_years < EDGAR_INV_START_YEAR ) # Currently: 1960-1969
    X_MISSING_EDGAR_EARLY_YEARS <- paste0( "X", MISSING_EDGAR_EARLY_YEARS )                        # Currently: X1960-X1969
    MISSING_EDGAR_LATE_YEARS <- subset( emissions_years, emissions_years > EDGAR_INV_END_YEAR )    # Currently: 2009-2014
    X_MISSING_EDGAR_LATE_YEARS <- paste0( "X", MISSING_EDGAR_LATE_YEARS )                          # Currently: X2009-X2014

}

# BP oil years
  BP_oil_years <- 1965 : BP_last_year
  BP_Xyears <- paste0( 'X', BP_oil_years )

# ------------------------------------------------------------------------------
# 3. Pre-processing
# 3.1. pre-processing of BP data
# 3.1.1 cleaning from raw data

    # Remove last columns, which are not time series data
    BP_data <- bp_oil_prod[ , 1 : ( ncol( bp_oil_prod ) - 2 ) ]
    colnames( BP_data ) <- c( 'BPName_Oil_production', BP_Xyears )

    # Take only the years used for calcs and the column with BP ctry names
    BP_data <- subset( BP_data, T,c( "BPName_Oil_production", BP_Xyears ) )

    # Add USSR to MCL for newer BP data so that clean function retains USSR data
    ussr_row <- mcl[ which( mcl$iso %in% c( 'ukr' ) ) , ]
    ussr_row$BPName_Oil_production <- "USSR"
    MCL_with_ussr <- rbind(mcl, ussr_row )

    fsu_ussr <- c( "arm", "aze", "blr", "est", "geo", "kaz", "kgz", "ltu", "lva", "mda", "rus", "tjk", "tkm", "ukr", "uzb", "ussr")
    MCL_ussr <- MCL_with_ussr[ MCL_with_ussr$iso %in% fsu_ussr, ]
    FSU_countries <- na.omit(MCL_ussr$BPName_Oil_production)

    # Convert to numeric
    BP_data[ BP_Xyears ] <- sapply( X = BP_data[ BP_Xyears ], FUN = as.numeric )

    BP_data <- sumAggregateRegion_Oil_production( BP_data, "", FSU_countries, "ussr" )

    # Note - BP data has a different set of countries for oil production as compared to their energy
    #        consumption data, so this is listed in a different column, BPName_Oil_production, in
    #        the master country list
    BP_data <- BP_data[ BP_data$BPName_Oil_production %in% c(mcl$BPName_Oil_production,"ussr"), ]
    BP_countries <- na.omit(mcl$BPName_Oil_production)
    mcl_bp <- mcl[ mcl$BPName_Oil_production %in% BP_countries, c( 'iso', 'BPName_Oil_production' ) ]
    mcl_bp <- mcl_bp[ !duplicated( mcl_bp ), ]

    BP_merge <- merge( BP_data, mcl_bp, by = c( "BPName_Oil_production" ), all.y = T )
    BP_merge[ BP_merge == 'n/a' ] <- 0
    BP_oil <- BP_merge[ , c( 'iso', BP_Xyears ) ]
    BP_oil[, BP_Xyears ] <- sapply( BP_oil[ , BP_Xyears ], as.numeric )
    BP_oil[, BP_Xyears ] <- BP_oil[, BP_Xyears ] * 0.001 # convert the unit tonnes to kt

# 3.1.2. special treatment for fsu countries
# for fsu member countries there's no data before 1985 in raw BP, but total number of fsu does exist.
# Here the code splits the ussr( fsu ) and distributes to fsu member countries
    fsu <- 'ussr'
    fsu_members <- c( "arm", "aze", "blr", "est", "geo", "kaz", "kgz", "ltu", "lva", "mda", "rus", "tjk", "tkm", "ukr", "uzb" )

    BP_split_Xyears <- paste0( 'X', 1965 : 1984 )

    #extract fsu countries from BP_oil data
    BP_fsu <- BP_oil[ BP_oil$iso %in% fsu, ]
    BP_fsu_members <- BP_oil[ BP_oil$iso %in% fsu_members, ]

    # re-format the pop data
    pop_fsu <- pop_raw[ pop_raw$iso %in% c( fsu, fsu_members ), ]
    pop_fsu$year <- paste0( 'X', pop_fsu$year )
    pop_wide <- cast( pop_fsu, iso ~ year, value = 'pop', fun.aggregate = sum )
    pop_wide <- pop_wide[ , c( 'iso', BP_Xyears ) ]

    # split using disaggregate_country function
    BP_fsu_split <- rbind( BP_fsu, BP_fsu_members )
    BP_fsu_split$sector <- 'oil_production' # needed by the function but has no other effect

    oil_production <- disaggregate_country( original_data = BP_fsu_split,
                                  trend_data = pop_wide,
                                  trend_match_cols = 'iso',
                                  combined_iso = fsu,
                                  disaggregate_iso = BP_fsu_members$iso, #c('aze' , 'kaz' , 'rus' , 'tkm' , 'uzb'),
                                  dis_start_year = 1965,
                                  dis_end_year = 1984 )

# 3.1.3. replace data of fsu countries in BP_oil with BP_fsu_members and remove 'ussr'
    for ( BP_fsu_member in BP_fsu_members$iso ) {
      BP_oil[ BP_oil$iso ==BP_fsu_member, BP_split_Xyears ] <- oil_production[ oil_production$iso == BP_fsu_member, BP_split_Xyears ]
      }

    BP_oil <- BP_oil[ BP_oil$iso != 'ussr', ]

    BP_iso <- BP_oil$iso

# 3.2. pre-processing of IEA data
    IEA_crude <- en_stat_sector_fuel[ en_stat_sector_fuel$sector == 'crude-oil-production', ]
    # exclude isos in BP data since BP data for those isos are more complete
    IEA_crude <- IEA_crude[ IEA_crude$iso %!in% BP_iso, ]
    IEA_Xyears <- colnames( IEA_crude )[ grep( 'X', colnames( IEA_crude ) ) ]
    IEA_iso <- IEA_crude$iso

# 3.3. pre-processing of ECLIPSE flaring data
    flaring <- ECLIPSE_flaring[ c( 'iso', 'X1990', 'X2000', 'X2010' ) ]
    drop_iso_list <- c( )
    for ( row_index in 1 : nrow( flaring ) ) {
      a_row <- flaring[ row_index , ]
      a_row_iso <- a_row$iso
      if ( a_row$X1990 == 0 & a_row$X2000 == 0 & a_row$X2010 == 0 ) { drop_iso_list <- c( drop_iso_list, a_row_iso ) }
    }
    flaring <- flaring[ flaring$iso %!in% drop_iso_list, ]
    flaring_iso <- flaring$iso


# 3.4. combine countries in BP and IEA and ECLIPSE_flaring
# extract common countries between ECLIPSE flaring and IEA crude oil production data
# and ECLIPSE flaring and BP oil production data
    flr_bp_iso <- intersect( BP_iso, flaring_iso )
    flaring_flrbpiso <- flaring[ flaring$iso %in% flr_bp_iso, ]
    flaring_flrbpiso <- flaring_flrbpiso[ order( flaring_flrbpiso$iso ),  ]
    BP_flrbpiso <- BP_oil[ BP_oil$iso %in% flr_bp_iso, ]
    BP_flrbpiso <- BP_flrbpiso[ order( BP_flrbpiso$iso ), ]


    flr_iea_iso <- intersect( IEA_iso, flaring_iso )
    flaring_flrieaiso <- flaring[ flaring$iso %in% flr_iea_iso, ]
    flaring_flrieaiso <- flaring_flrieaiso[ order( flaring_flrieaiso$iso ), ]
    IEA_flrieaiso <- IEA_crude[ IEA_crude$iso %in% flr_iea_iso, ]
    IEA_flrieaiso <- IEA_flrieaiso[ order( IEA_flrieaiso$iso ), ]

# combine BP_flrbpiso and IEA_flrieaiso together as unified layout
    # Extend data out to last year by copying last IEA year
    IEA_flrieaiso <- IEA_flrieaiso %>%
      dplyr::mutate_at( X_BP_years, funs( identity( !!rlang::sym( X_IEA_end_year ) ) ) )

    IEA_flrieaiso$X1970 <- IEA_flrieaiso$X1971

    extend_years <- 1965 : BP_last_year
    extend_Xyears <- paste0( 'X', extend_years )

    flaring_bp_iea <- rbind( BP_flrbpiso[ , c( 'iso', extend_Xyears ) ], IEA_flrieaiso[ , c( 'iso', extend_Xyears ) ] )
    flaring_bp_iea <- flaring_bp_iea[ order( flaring_bp_iea$iso ), ]

    flaring_eclipse <- rbind( flaring_flrbpiso, flaring_flrieaiso )
    flaring_eclipse <- flaring_eclipse[ order( flaring_eclipse$iso ), ]


# ------------------------------------------------------------------------------
# 4. Extending
# method: The ECLIPSE flaring only has data for year 1990, 2000, 2010 while the BP/IEA crude oild production has
#         time series data from 1965 to last BP year. The extending procedure first calculates ratios between
#         ECLIPSE data and BP/IEA data for avaliable years ( 1990, 2000, 2010 ), then extends the ratios to
#         all years( 1965 - last BP year ) using linear method and multiply the ratios to BP/IEA data to have
#         full time series data of ECLIPSE flaring.
    flaring_ratio <- data.frame( iso = flaring_eclipse$iso, X1990 = ( flaring_eclipse$X1990 / flaring_bp_iea$X1990 ),
                                    X2000 = ( flaring_eclipse$X2000 / flaring_bp_iea$X2000 ),
                                    X2010 = ( flaring_eclipse$X2010 / flaring_bp_iea$X2010 ), stringsAsFactors = F )
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
    flaring_extended <- flaring_bp_iea[ , extend_Xyears ] * flaring_extended_ratios
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
# 5. For N2O emissions, convert NOx emissinos using ratio of Edgar 1B2 (Fugitive
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
    EDGAR_N2O_clean <- EDGAR_clean_for_ratio( EDGAR_N2O, X_EDGAR_INV_YEARS, MCL_clean, "N2O" )
    EDGAR_NOx_clean <- EDGAR_clean_for_ratio( EDGAR_NOx, X_EDGAR_INV_YEARS, MCL_clean, "NOx" )

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
    EDGAR_ratios_extended <- EDGAR_national_ratio_with_global_ratio_fix %>%
        tidyr::spread( year, Ratio_N2O_per_Nox ) %>%
        dplyr::mutate_at( X_MISSING_EDGAR_EARLY_YEARS, funs( identity( !!rlang::sym( first( X_EDGAR_INV_YEARS ) ) ) ) ) %>%
        dplyr::mutate_at( X_MISSING_EDGAR_LATE_YEARS, funs( identity( !!rlang::sym( last( X_EDGAR_INV_YEARS ) ) ) ) ) %>%
        dplyr::select( iso, X_emissions_years ) %>%
        tidyr::gather( year, Ratio_N2O_per_Nox,  X_emissions_years )


#   Apply Ratio to emissions
    flaring_extended <- flaring_extended %>%
        tidyr::gather( year, NOx_emissions, BP_Xyears ) %>%
        dplyr::left_join( EDGAR_ratios_extended, by = c( "iso", "year" ) ) %>%
        dplyr::mutate( N2O_emissions = NOx_emissions * Ratio_N2O_per_Nox ) %>%
        dplyr::mutate( em = "N2O" ) %>%
        dplyr::select( -NOx_emissions, -Ratio_N2O_per_Nox ) %>%
        tidyr::spread( year, N2O_emissions )

 }

# -----------------------------------------------------------------------------
# 6. Write output
    writeData( flaring_extended , "MED_OUT", paste0( "C.", em, "_ECLIPSE_flaring_emissions_extended" ) )

if( em != "N2O" ){

    writeData( flaring_ratio , "DIAG_OUT", paste0( "C.", em, "_ECLIPSE_flaring_to_crude_oil_production_ratios" ) )

}

# Every script should finish with this line:
    logStop()

