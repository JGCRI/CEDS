# ------------------------------------------------------------------------------
# Program Name: C1.2.Adipic_nitric_acid_default_process_emissions.R
# Author(s): Patrick O'Rourke
# Date Last Modified: January 22, 2020
# Program Purpose: To extend adipic and nitric acid production process emissions, and
#                  to remove these emissions from the larger 2B_Chemical-industry default
#                  emissions to avoid double counting
# Input Files: Master_Country_List.csv (if em is not N2O),
#              C.[em]_EPA_NC_adipic_and_nitric_acid.csv (N2O only)
#              C.[em]_EDGAR_chemical_industry_emissions.csv (N2O only)
# Output Files: C.[em]_NC_emissions_db.csv (For N2O only currently),
#               C.[em]_EPA_NC_extended_adipic_nitric_acids_emissions.csv
# Notes:
# TODO:
# -----------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if( "input" %in% dir( ) ) "code/parameters/" else "../code/parameters/"

# Universal header file - provides logging, file support, etc.
headers <- c( "common_data.R", "analysis_functions.R", "data_functions.R",
              "process_db_functions.R", "timeframe_functions.R" ) # Additional function files required.
log_msg <- paste0( "Extending EPA adipic and nitric acid default process emissions ",
                   "using EDGAR chemical industry emissions. Other emissions species",
                   "will have data created with all 0 values for these sectors..." ) # First message to be printed to the log
script_name <- "C1.2.Adipic_nitric_acid_default_process_emissions.R"
source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Settings and script constants

# Define emissions species variable
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "N2O"

# ------------------------------------------------------------------------------
# 2. Load inputs and define script constants

# If the emissions species is not N2O, load the master country list in order to
# create a dataframe of all 0 values for adipic and nitric acid production
if( em != "N2O"){

    MCL <- readData( "MAPPINGS", "Master_Country_List" )

}

# Begin N2O specific processing
if( em == "N2O" ){

# EPA adipic and nitric acid emissions
EPA_acid_emissions <- readData( "MED_OUT", paste0( "C.", em,  "_EPA_NC_adipic_and_nitric_acid" ) )

# EDGAR 2B chemical industry process emissions - for extending EPA adipic and nitric acid
# back in time
EDGAR_2B <- readData( "MED_OUT", paste0( "C.", em, "_EDGAR_chemical_industry_emissions" ) )

# EPA years
EPA_START_YEAR <- 1990
X_EPA_START_YEAR <- paste0( "X", EPA_START_YEAR )
X_EPA_YEARS <- paste0( "X", EPA_START_YEAR : end_year )

# EDGAR years
X_EDGAR_YEARS <- paste0( "X", EDGAR_start_year : 2008 )

# EPA extended years
X_EPA_EXTENSION_YEARS <- paste0( "X", EDGAR_start_year : ( EPA_START_YEAR - 1 ) )
X_EPA_EXTENDED_YEARS <- paste0( "X", EDGAR_start_year : end_year )

# ------------------------------------------------------------------------------
# 3. Check that EPA and EDGAR data have all CEDS isos

# A. Add "global" iso to EPA and EDGAR data with all values set to 0, if the "global"
#    iso is missing
    if( "global" %!in% EPA_acid_emissions$iso ){

        EPA_acid_emissions <- dplyr::bind_rows( EPA_acid_emissions,
                                                EPA_acid_emissions %>%
                                                    dplyr::filter( iso == "usa" ) %>%
                                                    dplyr::mutate( iso = "global" ) %>%
                                                    dplyr::mutate_at( .vars = X_EPA_YEARS,
                                                                    funs( identity( 0 ) ) ) ) %>%
                              dplyr::arrange( iso, sector )


    }

    if( "global" %!in% EDGAR_2B$iso ){

        EDGAR_2B <- dplyr::bind_rows( EDGAR_2B,
                                      EDGAR_2B %>%
                                        dplyr::filter( iso == "usa" ) %>%
                                        dplyr::mutate( iso = "global" ) %>%
                                        dplyr::mutate_at( .vars = X_EDGAR_YEARS,
                                                          funs( identity( 0 ) ) ) ) %>%
                    dplyr::arrange( iso, sector )


    }

# B. Check EPA data for final CEDS isos
    printLog( "Checking EPA adipic and nitric acid emissions data for CEDS isos..." )

    EPA_acid_emissions_isos <- EPA_acid_emissions %>%
        dplyr::select( iso )

    iso_check( data_to_check = EPA_acid_emissions_isos,
            data_to_check_iso_colname = "iso",
            provided_data_needs_all_ceds_isos = T,
            provided_data_contains_unique_isos_only = F )

# C. Check EDGAR data for final CEDS isos
    printLog( "Checking EPA adipic and nitric acid emissions data for CEDS isos..." )

    EDGAR_2B_isos <- EDGAR_2B %>%
        dplyr::select( iso )

    iso_check( data_to_check = EDGAR_2B_isos,
               data_to_check_iso_colname = "iso",
               provided_data_needs_all_ceds_isos = T,
               provided_data_contains_unique_isos_only = T )

# ------------------------------------------------------------------------------
# 4. Extend EPA acid emissions backwards using trend from EDGAR 2B

# A. Check that if all isos with non-zero values in the first EPA year (1990) of EPA data have a
#   non-zero values in EDGAR 2B for the first EPA year as well (given that the EDGAR
#   data will be used for extending the EPA data, a non-zero value in needed in the first EPA year [1990]
#   within the EDGAR data). Also, if there are any rows which have a 0 value for the first
#   EPA year, inform the user they will be extended back with all 0 values.
    EPA_not_0_EPA_start_year <- EPA_acid_emissions %>%
        dplyr::filter( !!rlang::sym( X_EPA_START_YEAR ) != 0 )

    EPA_0_EPA_start_year <- EPA_acid_emissions %>%
        dplyr::filter( !!rlang::sym( X_EPA_START_YEAR ) == 0 )

    EDGAR_not_0_EPA_start_year <- EDGAR_2B %>%
        dplyr::filter( !!rlang::sym( X_EPA_START_YEAR ) != 0 )

    EPA_not_0_EPA_start_but_0_in_EDGAR <- EPA_not_0_EPA_start_year %>%
        dplyr::filter( iso %!in% unique( EDGAR_not_0_EPA_start_year$iso ) )

    if( nrow( EPA_not_0_EPA_start_but_0_in_EDGAR ) != 0 ){

        printLog( "The following isos have non-zero values for adipic and/or nitric acid production emissions in the",
                  "first year of EPA non-CO2 GHG data (", EPA_START_YEAR, "), but emissions for this year",
                  "are 0 within the EDGAR 2B chemical production sector. These emissions will be extended",
                  "using the global chemical sector trend from EDGAR...",
                  unique( EPA_not_0_EPA_start_but_0_in_EDGAR$iso ) )

    }

    if( nrow( EPA_0_EPA_start_year ) != 0 ){

        printLog( "The following isos have a value of zero for adipic and/or nitric acid production emissions in the",
                  "first year of EPA non-CO2 GHG data (", EPA_START_YEAR, "). Emissions for years before the",
                  "first EPA year will be set to 0 kt...", unique( EPA_0_EPA_start_year$iso ) )

    }

#   Filter out rows which:
#   TODO: Confirm metodology - Isos which have a 0 value in the first EPA year (1990) - these will have constant
#                 extension backwards (all pre-1990 values will be set to 0)
#   TODO: Confirm metodology - Isos which are non-0 in EPA start year, but are 0 in EDGAR 2B chemical sector
#                 for the EPA start year will be extended backwards using global trend (could do
#                 regional trend as well without much extra effort)
    EPA_acid_emissions_not_0_start_year_EPA_and_EDGAR <- EPA_acid_emissions %>%
        dplyr::filter( !!rlang::sym( X_EPA_START_YEAR ) != 0,
                       iso %!in% unique( EPA_not_0_EPA_start_but_0_in_EDGAR$iso ) )

# B. Extend emissions backwards
#   1) Extend isos which are non-zero for EPA_START_YEAR in both EPA and EDGAR data
#   TODO: Confirm metodology - that this was the right function to use and range bit

    id_cols <- c( "iso", "fuel" )
#   Note: Extending the two acid production emissions sectors together didn't work as expected
#         (which is why they are extended separately)
    EPA_emissions_not_0_start_year_EPA_and_EDGAR_adipic_ext <-
        extend_data_on_trend_range( driver_trend = EDGAR_not_0_EPA_start_year,
                                    input_data = ( EPA_acid_emissions_not_0_start_year_EPA_and_EDGAR %>%
                                                     dplyr::filter( sector == "2B3_Chemicals-Adipic-acid" ) ),
                                                   start = EDGAR_start_year,
                                                   end = ( EPA_START_YEAR - 1 ),
                                                   range = 5,
                                                   id_match.driver = id_cols,
                                                   id_match.input = id_cols )

      EPA_emissions_not_0_start_year_EPA_and_EDGAR_nitric_ext <-
          extend_data_on_trend_range( driver_trend = EDGAR_not_0_EPA_start_year,
                                      input_data = ( EPA_acid_emissions_not_0_start_year_EPA_and_EDGAR %>%
                                                        dplyr::filter( sector == "2B2_Chemicals-Nitric-acid" ) ),
                                                     start = EDGAR_start_year,
                                                     end = ( EPA_START_YEAR - 1 ),
                                                     range = 5,
                                                     id_match.driver = id_cols,
                                                     id_match.input = id_cols )

      EPA_emissions_not_0_start_year_EPA_and_EDGAR_ext <- dplyr::bind_rows(
            EPA_emissions_not_0_start_year_EPA_and_EDGAR_adipic_ext,
            EPA_emissions_not_0_start_year_EPA_and_EDGAR_nitric_ext ) %>%
          dplyr::arrange( iso, sector )

#   2) Isos which are non-0 in EPA start year, but are 0 in EDGAR 2B chemical sector
#      for the first EPA year will be extended backwards using global EDGAR chemical production trend
#      TODO: Confirm metodology - also, we could do regional trend in future as well without much extra effort
    if( nrow( EPA_not_0_EPA_start_but_0_in_EDGAR ) > 0 ){

#       Aggregate all EDGAR chemical production emissions
        isos_for_global_extension <- as.data.frame( EPA_not_0_EPA_start_but_0_in_EDGAR$iso ) %>%
            dplyr::mutate( note = "global_sum" ) %>%
            dplyr::rename( iso = "EPA_not_0_EPA_start_but_0_in_EDGAR$iso" )

        EDGAR_2B_global_sum <- EDGAR_2B %>%
            dplyr::select( -iso ) %>%
            dplyr::group_by( sector, fuel, units ) %>%
            dplyr::summarize_all( funs( sum( ., na.rm = TRUE ) ) ) %>%
            dplyr::ungroup( ) %>%
            dplyr::mutate( note = "global_sum" ) %>%
            dplyr::full_join( isos_for_global_extension, by = "note" ) %>%
            dplyr::select( iso, sector, fuel, units, note, X_EDGAR_YEARS )

#       Extend with global trend
        EPA_not_0_EPA_start_but_0_in_EDGAR_ext <-
            extend_data_on_trend_range( driver_trend = EDGAR_2B_global_sum,
                                        input_data = EPA_not_0_EPA_start_but_0_in_EDGAR,
                                        start = EDGAR_start_year,
                                        end = ( EPA_START_YEAR - 1 ),
                                        range = 5,
                                        id_match.driver = id_cols,
                                        id_match.input = id_cols )

    }


#   3) Isos which have a 0 value in the first EPA year (1990) - constant
#      extension backwards (all pre-1990 values will be set to 0)
#      TODO: Confirm metodology
    if( nrow( EPA_0_EPA_start_year ) != 0 ){

        EPA_0_EPA_start_year_ext <- EPA_0_EPA_start_year %>% # remove the 2 when verified
            dplyr::mutate_at( .vars = X_EPA_EXTENSION_YEARS, funs( identity( +0 ) ) ) %>%
            dplyr::select( iso, sector, fuel, units, X_EPA_EXTENDED_YEARS )

    }

# B. Combine extended emissions into 1 data frame
     EPA_acid_emissions_extended <- EPA_emissions_not_0_start_year_EPA_and_EDGAR_ext

     if( nrow( EPA_not_0_EPA_start_but_0_in_EDGAR_ext ) > 0 ){

         EPA_acid_emissions_extended <- dplyr::bind_rows( EPA_acid_emissions_extended,
                                                          EPA_not_0_EPA_start_but_0_in_EDGAR_ext )

     }

     if( nrow( EPA_0_EPA_start_year_ext ) > 0 ){

         EPA_acid_emissions_extended <- dplyr::bind_rows( EPA_acid_emissions_extended,
                                                          EPA_0_EPA_start_year_ext )

     }

     # Final extended acid emissions
     EPA_acid_emissions_extended <- EPA_acid_emissions_extended %>%
         dplyr::select( iso, sector, fuel, units, X_EPA_EXTENDED_YEARS ) %>%
         dplyr::arrange( iso, sector )

# D. Check that extended data is still identical for post-extension years (EPA Years)
     check_EPA_acid_emissions_extended <-  EPA_acid_emissions_extended %>%
        dplyr::select( iso, sector, fuel, units, X_EPA_YEARS )

      if( !identical( check_EPA_acid_emissions_extended,
                      EPA_acid_emissions ) ){

          stop( "Extended EPA adipic and nitric acid emissions are not identical to pre-extended data",
                "for the original years:", paste0( EPA_START_YEAR, "-",  "X", "", end_year ),
                " . Please check data and ", script_name )

      }

     printLog( "Extension of EPA adipic and nitric acid default process emissions is complete..." )

# ------------------------------------------------------------------------------
# 5. Subtract the extended nitric and adipic acid emissions from
#    CEDS sector 2B_chemical-industry process emissions, as nitric and adipic acid emissions
#    are currently nested within this 'aggregate' CEDS sector, and thus must be
#    be subtracted in order to avoid double counting.
printLog( "Subtracting adipic and nitric acid production emissions from CEDS 2B_chemical-industry",
              "emissions in order to avoid double counting..." )

#  A. Aggregate EPA data for subtraction, for EDGAR years
   EPA_acid_emissions_extended_agg <- EPA_acid_emissions_extended %>%
       dplyr::mutate( sector = "2B_Chemicals-NAA" ) %>%
       dplyr::group_by( iso, sector, fuel, units ) %>%
       dplyr::summarize_all( funs( sum( ., na.rm = TRUE ) ) ) %>%
       dplyr::ungroup( ) %>%
       dplyr::select( iso, fuel, units, X_EDGAR_YEARS ) %>%
       tidyr::gather( key = years, value = agg_acid_production_emissions, X_EDGAR_YEARS )

#  B. Subtract acid production emissions from CEDS sector 2B_Chemical-industry
   EDGAR_2B_less_acid_prod_emiss <- EDGAR_2B %>%
       tidyr::gather( key = years, value = chemical_production_emissions, X_EDGAR_YEARS ) %>%
       dplyr::left_join( EPA_acid_emissions_extended_agg, by = c( "iso", "fuel", "units", "years" ) ) %>%
       dplyr::mutate( final_2B_emissions = chemical_production_emissions - agg_acid_production_emissions )

#  C. If emissions become negative for aggregate chemical sector (2B), reset them to 0 and printLog message
#  TODO: Confirm this methodology
    if( any( EDGAR_2B_less_acid_prod_emiss$final_2B_emissions < 0 ) ){

        printLog( "Some chemical industry (sector 2B) emissions have become negative after subtracting",
                  "EPA's adipic and nitric acid emissions. These emissions will be reset to 0..." )

        EDGAR_2B_less_acid_prod_emiss <- EDGAR_2B_less_acid_prod_emiss %>%
            dplyr::mutate( final_2B_emissions = if_else( final_2B_emissions < 0, 0, final_2B_emissions ) )

    }

   if( any( EDGAR_2B_less_acid_prod_emiss$final_2B_emissions < 0 ) ){

       stop( "No 2B_chemical-industry emissions should still be negative. See ", script_name )

   }

#  C. Final processing of 2B_chemical-industry emissions
   EDGAR_2B_final_emissions <- EDGAR_2B_less_acid_prod_emiss %>%
       dplyr::select( iso, sector, fuel, units, years, final_2B_emissions ) %>%
       tidyr::spread( years, final_2B_emissions )

}

# ------------------------------------------------------------------------------
#  6. Make the acid production sectors for other emissions species - set to all 0 values
#  currently (other process emissions from adipic and nitric acid production are currently
#  still nested within CEDS 2B_chemical-industry process emissions.)
#  TODO: (For the future) Seperate other process emissions that exist (NOx)
if( em != "N2O" ){

#   Make list of unique final isos (isos with final_data_flag = 1, srb (kosovo),
#   and gum) and list of final isos with OECD vs Non-OECD flag
#   TODO: (For the future) When the issue in the Master Country List is resolved for these
#         isos this can be simplified
    MCL_clean <- MCL %>%
        dplyr::select( iso, final_data_flag,  OECD_flag ) %>%
        dplyr::distinct( ) %>%
        dplyr::filter( final_data_flag == 1 | iso %in% c( "srb (kosovo)", "gum" ) ) %>%
        dplyr::select( iso )

    EPA_acid_emissions_extended <- dplyr::bind_rows(
            MCL_clean %>% dplyr::mutate( sector = "2B3_Chemicals-Adipic-acid" ),
            MCL_clean %>% dplyr::mutate( sector = "2B2_Chemicals-Nitric-acid" ) ) %>%
        dplyr::arrange( iso, sector ) %>%
        dplyr::mutate( fuel = "process",
                       units = "kt" ) %>%
        dplyr::mutate_at( .vars = X_emissions_years, .funs = funs( identity( 0 ) ) )

}

# ------------------------------------------------------------------------------
# 7. Output data

#  Extended acid production emissions
writeData( EPA_acid_emissions_extended,  domain = "DEFAULT_EF_IN",
           domain_extension = "non-combustion-emissions/",
           fn = paste0( "C.",em, "_EPA_NC_extended_adipic_nitric_acids_emissions" ) )

# Chemical industry emissions with acid emissions subtracted
if( em == "N2O" ){

    addToEmissionsDb( EDGAR_2B_final_emissions, em = em, type = 'NC',
                      ext_backward = FALSE, ext_forward = FALSE )
    # TODO: Confirm - should this be extended forward and backward? (mod C. EDGAR script too)

}

logStop( )
# END


