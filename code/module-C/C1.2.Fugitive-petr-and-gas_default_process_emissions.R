#------------------------------------------------------------------------------
# Program Name: C1.2.Fugitive-petr-and-gas_default_process_emissions.R
# Author: Leyang Feng, Patrick O'Rourke
# Date Last Modified: August 13, 2020
# Program Purpose: Generates default process emissions for fugitive oil and
#                  gas production ( 1B2_Fugitive-petr, 1B2b_Fugitive-NG-distr,
#                  and 1B2b_Fugitive-NG-prod )
# Input Files: C.[em]_ECLIPSE_flaring_emissions_extended.csv,
#              relevant EDGAR emissions data ( EDGAR v5 = v50_[em]_1970_[edgar_end_year].xls )
#              NC_EDGAR_sector_mapping.csv, C.[em]_GAINS_fug_oil_gas_shares.csv, Master_Country_List.csv,
#              A.UN_pop_master.csv, C.N2O_EDGAR_NOx_N2O_fugitive_oil_NG_ratios.csv
# Output Files: C.[em]_Fugitive-petr-and-gas_aggregate_emissions.csv,
#               C.[em]_Fugitive-petr-and-gas_default_process_emissions.csv
# Notes:
# TODO: Script TODOs found below
#-------------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
    headers <- c( 'data_functions.R', 'common_data.R' ) # Any additional function files required
    log_msg <- "Generating fugitive petr. and gas default process emissions..." # First message to be printed to the log
    script_name <- "C1.2.Fugitive-petr-and-gas_default_process_emissions.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 0. Define utility functions

# Return difference of dataframes rounded to 1 decimal
return_diff <- function( dataframe1, dataframe2 ) {

    numeric_cols1 <- sapply( dataframe1, mode ) == 'numeric'
    numeric_cols2 <- sapply( dataframe2, mode ) == 'numeric'

    dataframe1[ numeric_cols1 ] <-  round(dataframe1[ numeric_cols1 ], 1)
    dataframe2[ numeric_cols2 ] <-  round(dataframe2[ numeric_cols2 ], 1)

    return( setdiff( dataframe1, dataframe2 ) )
}

# ------------------------------------------------------------------------------
# 1. Define emission species as well as script constants,  and read in files

# Define emissions species variable
    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "CH4"

# Read in the extended flaring data
    flaring <- readData( "MED_OUT", file_name = paste0( 'C.', em, '_ECLIPSE_flaring_emissions_extended' ) )

# Read in EDGAR non-combustion sector mapping file
    NC_sector_map_EDGAR <- readData( "MAPPINGS", "NC_EDGAR_sector_mapping" )

# Read in EDGAR data

#   Define EDGAR version using and other input settings
    vn <- "5.0"

#   Defineinput domain
    domain_use <- "EM_INV"
    domain_ext <- "EDGAR/"

#   Read in EDGAR v4.2 - original CEDS release used this only for CH4
    if( as.numeric( vn ) == 4.2 ){

      EDGAR_raw <- readData( domain = domain_use, domain_extension = domain_ext,
                             file_name = paste0( "EDGAR", gsub( "[.]", "", vn ), "_", em  ) )

#     If em is CH4, read in additional EDGAR v4.2 file
      if ( em == 'CH4' ) {

        skip_rows <- 9
        extension_use <- ".xlsx"
        EDGAR_FT_raw <- readData( domain = domain_use,  domain_extension = domain_ext,
                                  file_name = paste0( 'v', gsub( "[.]", "", vn ), "FT_", em, "_2000_2010" ),
                                  extension = extension_use, sheet_selection = 1, skip = skip_rows )

      }

#   Read in EDGAR v4.3 - original CEDS released used this for all ems other than CH4 (and CO2, since this
#   script is not run for CO2)
    } else if( as.numeric( vn ) == 4.3 ){

      em_use <- em
      if( em == "N2O" ){ em_use <- "NOx" }

      skip_rows <- 8
      extension_use <- ".xlsx"
      sheet_name <- paste0( 'NEW_v', vn, '_EM_', em_use, '_ref' )
      EDGAR_raw <- readData( domain = domain_use, domain_extension = domain_ext,
                             file_name = paste0( 'JRC_PEGASOS_', em, '_TS_REF' ),
                             extension = extension_use, sheet_selection = sheet_name, skip = skip_rows )

#   Readin EDGAR v5
    } else if( as.numeric( vn ) == 5 ){

      fn <- c( paste0( "v",  gsub( "[.]", "", vn ), "_", em, "_", EDGAR_start_year, "_",
                       EDGAR_end_year ), ".xls")
      sheet_to_use <- paste0( "v", vn, "_EM_", em, "_IPCC1996" )
      skip_rows <- 9

#     EDGAR v5 has a special file naming convention for CO2
      if( em == "CO2" ){

        EDGAR_CO2_real_end_year <- 2018
        fn <- c( paste0( "v",  gsub( "[.]", "", vn ), "_", em, "_", "excl_short-cycle_org_C_",
                         EDGAR_start_year, "_", EDGAR_CO2_real_end_year ), ".xls")

      }

      EDGAR_raw <- readData( domain = domain_use, domain_extension = domain_ext,
                         file_name = fn[ 1 ], extension = fn[ 2 ],
                         sheet_selection = sheet_to_use, skip = skip_rows,
                         missing_value = c( "", "NULL" ) )

#   Naming pattern for other EDGAR versions
    } else {

      stop( script_name, " has not been formatted to process Edgar v", vn, ". Please ",
            "reformat the script and rerun." )

    }

# If EDGAR version 4.3 is used here and em is N2O, then read in EDGAR fugitive
# oil and gas NOx to N2O ratios data, which will be used to convert JRC_PEGASOS NOx data to N2O
if( em == "N2O" & as.numeric( vn ) == 4.3 ){

    EDGAR_N2O_NOx_fug_oil_NG_ratios <- readData( "MED_OUT", file_name = paste0( "C.", em, "_EDGAR_NOx_N2O_fugitive_oil_NG_ratios" ) )

}

# For all ems besides NH3, load the GAINS fugitive oil and gas subsector emissions shares.
# TODO: GAINS now has NH3, so we could make shares for NH3, even if we don't use GAINS NH3 data
#       for default combustion EFs
if( em != "NH3" ){

    GAINS_fug_subsec_shares <- readData( "MED_OUT", file_name = paste0( "C.", em, "_GAINS_fug_oil_gas_shares" ) )

}

# Read Master_Country_List.csv
    MCL <- readData( "MAPPINGS", file_name = "Master_Country_List" )

# Read UN Population Data
   population <- readData( "MED_OUT", file_name = "A.UN_pop_master" )

# ------------------------------------------------------------------------------

# 2. Define functions used within the script

#   Define function to downscale fugitive emissions data from one iso to multiple isos (using
#   UN population data) before applying GAINS fugitive subsector splits to the emissions
#   Note: this function may not be needed in the future, when small isos are handled in prior to this script
    disagg_iso <- function( agg_iso_region, additional_iso_to_create,
                           emissions_data_in, population_data, years_to_disaggregate ){

       agg_region_all_isos <- c( additional_iso_to_create, agg_iso_region)

#      Process population data
       population_clean <- population_data %>%
           dplyr::mutate( year = paste0( "X", year ) ) %>%
           dplyr::filter( scenario %in% c( historical_pop_scenario, future_pop_scenario ),
                          year %in% years_to_disaggregate ) %>%
           dplyr::select( iso, year, pop ) %>%
           tidyr::spread( year, pop )

#      Calculate aggregate UN region population
       agg_region_iso_populations <- population_clean %>%
           dplyr::filter( iso %in% agg_region_all_isos )

       aggregated_population <- agg_region_iso_populations %>%
           dplyr::select( -iso ) %>%
           dplyr::summarise_all( funs( sum ( ., na.rm = TRUE ) ) ) %>%
           tidyr::gather( key = Years, value = agg_reg_population, years_to_disaggregate )

#      Calculate UN population share for each sub-iso
       agg_region_iso_pop_shares <- agg_region_iso_populations %>%
           tidyr::gather( key = Years, value = Population, years_to_disaggregate ) %>%
           dplyr::left_join( aggregated_population, by = "Years" ) %>%
           dplyr::mutate( iso_share_of_agg_region_pop = Population / agg_reg_population ) %>%
           dplyr::select( -Population, -agg_reg_population  )

#      Filter out the agg iso and iso to disaggegate  if needed
       emissions_without_isos_being_replaced <- emissions_data_in %>%
           dplyr::filter( iso %!in% agg_region_all_isos )

#      Filter for agg region emissions data
       agg_region_of_interest <- emissions_data_in %>%
           dplyr::filter( iso == agg_iso_region )

#      Define function to add duplicate row of agg_region data with iso renamed as the
#      missing iso whose data is being created by disaggregating the agg_region
       add_isos_for_disagg <- function( iso_in ){

           new_iso <- agg_region_of_interest %>%
               dplyr::mutate( iso = paste0( iso_in ) )

       }

#      Create disagg iso rows
       new_isos_for_disagg <- lapply( additional_iso_to_create, add_isos_for_disagg ) %>%
           dplyr::bind_rows(  )

#      Disaggregate the emissions data for the agg region
       emissions_data_in_all_region_isos <- agg_region_of_interest %>%
           dplyr::bind_rows( new_isos_for_disagg ) %>%
           tidyr::gather( key = Years, value = Emissions, years_to_disaggregate ) %>%
           dplyr::left_join( agg_region_iso_pop_shares, by = c( "iso", "Years" ) ) %>%
           dplyr::mutate( disagg_emissions = Emissions * iso_share_of_agg_region_pop ) %>%
           dplyr::select( iso, sector, fuel, units, Years, disagg_emissions )

#      Check that there are no NAs or NaNs for new downscaled emissions
       if( any( is.na( emissions_data_in_all_region_isos$disagg_emissions ) |
                is.nan( emissions_data_in_all_region_isos$disagg_emissions ) ) ){

           stop( paste0( "Some downscaled emissions are now NA. Check population and emissions data.") )

       }

#      Check that disagg regions summed = agg region data for each sector - rounded to 10 decimals
       downscaled_check <- emissions_data_in_all_region_isos %>%
           dplyr::select( -iso ) %>%
           dplyr::group_by( sector, fuel, units, Years ) %>%
           dplyr::summarise_all( funs( sum ( ., na.rm = TRUE ) ) ) %>%
           dplyr::arrange( sector, fuel, units, Years ) %>%
           dplyr::mutate( disagg_emissions = round( disagg_emissions, digits = 10 ) ) %>%
           dplyr::ungroup( ) %>%
           dplyr::rename( Emissions = disagg_emissions )

       agg_region_of_interest_long <- agg_region_of_interest %>%
           tidyr::gather( key = Years, value = Emissions, years_to_disaggregate ) %>%
           dplyr::select( -iso ) %>%
           dplyr::mutate( Emissions = round( Emissions, digits = 10 ) )

       diff1 <- return_diff( agg_region_of_interest_long, downscaled_check)
       diff2 <- return_diff( downscaled_check, agg_region_of_interest_long)

       if( nrow( diff1 ) != 0 | nrow( diff2 ) != 0 ){

           stop( paste0( "Downscaled emissions do not equal aggregate region emissions for all sectors. ",
                         "See C1.2.Fugitive-petr-and-gas_default_process_emissions.R... " ) )

       }

#      Replace the original agg region data
       emissions_data_in_all_region_isos_wide <- emissions_data_in_all_region_isos %>%
           tidyr::spread( Years, disagg_emissions )

       final_disagg_emissions <- emissions_without_isos_being_replaced %>%
           dplyr::filter( iso != agg_iso_region ) %>%
           dplyr::bind_rows( emissions_data_in_all_region_isos_wide )

       return( final_disagg_emissions )

   }

# ------------------------------------------------------------------------------
# 3. Pre-processing

# Define EDGAR fugitive oil and gas sector
    EDGAR_fug_oil_gas_mapping <- 	NC_sector_map_EDGAR %>%
      dplyr::filter( ceds_sector == "1B2_Fugitive-petr-and-gas" )

    EDGAR_fug_oil_gas_sec <- sort( unique( EDGAR_fug_oil_gas_mapping$edgar_sector ) )
    EDGAR_fug_oil_gas_sec_desc <- sort( unique( EDGAR_fug_oil_gas_mapping$edgar_descr ) )

# Define the output years
    all_Xyear <- paste0( 'X', EDGAR_start_year : end_year )

# Extract year_list in flaring data
    flaring_Xyear <- colnames( flaring )[ grep( 'X', colnames( flaring ) ) ]

# Extract year_list in EDGAR data
  edgar_year <- colnames( EDGAR_raw )[ c( grep( '19', colnames( EDGAR_raw ) ), grep( '20', colnames( EDGAR_raw ) ) ) ]
  if ( vn %in% c( "4.3", "5.0" ) ){ edgar_Xyear <- paste0( 'X', edgar_year ) }
  if ( em == "4.2" ){ edgar_Xyear <- edgar_year }
  edgar_year <- as.numeric( sub( 'X', "", edgar_year ) )

# Define final CEDS fuel, sector, and units
  final_fuel <- 'process'
  final_sector <- '1B2_Fugitive-petr-and-gas' # Note, this is actually the final aggregate sector, before breaking into 3 fugitive sectors
  final_units <- 'kt'

# If methane and EDGAR version is v4.2, process EDGAR FT and combine with edgar
if ( em == 'CH4' & vn == "4.2" ){

  edgarFT_year <- colnames( EDGAR_FT_raw )[ c( grep( '19', colnames( EDGAR_FT_raw ) ), grep( '20', colnames( EDGAR_FT_raw ) ) ) ]
  edgarFT_Xyear <- paste0 ( 'X' , edgarFT_year )
  edgarFT <- EDGAR_FT_raw
  names(edgarFT)[ which( names( edgarFT ) %in% edgarFT_year ) ] <- edgarFT_Xyear
  FT_ext_year <- edgarFT_Xyear[ which ( edgarFT_Xyear %!in% edgar_Xyear ) ]
  EDGAR_raw <- dplyr::left_join(EDGAR_raw, edgarFT[ c( 'ISO_A3' , 'IPCC' , FT_ext_year ) ], by = c( "ISO_A3", "IPCC" ) )
  edgar_Xyear <- c(edgar_Xyear, FT_ext_year)

}

# Extract years that are not in EDGAR data
    edgar_missing_Xyear <- all_Xyear[ which( all_Xyear %!in% edgar_Xyear ) ]

# Generate a dummy layout using flaring data in case there's no desired info in edgar_raw
    dummy_layout <- flaring[ , c( 'iso', 'sector', all_Xyear ) ]
    dummy_layout[ , all_Xyear ] <- 0

# Clean EDGAR data
    if( vn %in% c( "4.3", "5.0" ) ){ edgar <- EDGAR_raw[ , c('ISO_A3', 'IPCC', 'IPCC_description', edgar_year ) ] }
    if( vn == "4.2" ){ edgar <- EDGAR_raw[ , c( 'ISO_A3', 'IPCC', 'IPCC_description', edgar_Xyear ) ] }
    edgar$ISO_A3 <- tolower( edgar$ISO_A3 )
    colnames( edgar ) <- c( 'iso', 'sector', 'sector_description', edgar_Xyear )

# Remove rows with all NA's
    edgar <- edgar[ apply( X = edgar[ , edgar_Xyear ],
                           MARGIN = 1, function( x ) ( !all.na( x ) ) ) ,]

# Turn NAs to zeros
    edgar[ is.na( edgar ) ] <- 0

# Make negative emissions zero
    neg_rows <- apply( edgar[, edgar_Xyear ], 1, function( row ) any( row < 0 ) )
    edgar_neg <- edgar[ neg_rows, ]
    edgar[ edgar < 0 ] <- 0

# Extend EDGAR to flaring years using the last available EDGAR year (2015 for all ems other than CO2 in EDGAR v5)
    edgar[ , edgar_missing_Xyear ] <- edgar[ last( edgar_Xyear ) ]
    edgar <- edgar[ edgar$sector %in% EDGAR_fug_oil_gas_sec, ]
    edgar <- edgar[ !is.na( edgar$iso ), ]
    edgar <- edgar[ , c( 'iso', 'sector', 'sector_description', all_Xyear ) ]

# If the em is N2O and EDGAR version 4.3 is being used, then convert NOx EDGAR data to N2O
# as EDGAR v4.3 does not have N2O data available.
    if( em == "N2O" & as.numeric( vn ) == 4.3 ){

        printLog( "Using EDGAR N2O and NOx data to produce convert EDGAR PEGASOS NOx emissions to N2O emissions, as PEGASOS only provides",
                  "NOx data for this sector..." )

        EDGAR_N2O_NOx_fug_oil_NG_ratios_long <- EDGAR_N2O_NOx_fug_oil_NG_ratios %>%
            dplyr::select( iso, all_Xyear ) %>%
            tidyr::gather( year, Ratio_N2O_per_Nox, all_Xyear )

        edgar <- edgar %>%
            tidyr::gather( year, NOx_emissions, all_Xyear ) %>%
            dplyr::left_join( EDGAR_N2O_NOx_fug_oil_NG_ratios_long, by = c( "iso", "year" ) ) %>%
            dplyr::mutate( N2O_emissions = NOx_emissions * Ratio_N2O_per_Nox ) %>%
            dplyr::select( -NOx_emissions, -Ratio_N2O_per_Nox ) %>%
            tidyr::spread( year, N2O_emissions )

        if( any( is.na( edgar[ , all_Xyear ] ) ) ){

            stop( "There should be no NAs after converting EDGAR NOx fugitive oil and gas emissions to N2O...")

        }

    }

#     Split EDGAR data into three parts
#     As of EDGAR v5, there are 5 1B2 sub-sectors in EGDAR: Fugitive emissions from oil and gas (fossil fuels OR biofuels)
#                                                           Fugitive emissions from gaseous fuels (fossil fuels OR biofuels)
#                                                           Fugitive emissions from liquid fuels
#     For most of the EDGAR data ( except for CH4 and NMVOC ), only Fugitive emissions from oil and gas exists.
#     Eventually, we want the flaring default process emissions to be generated as ( in the next section ):
#     1B2_Fugitive-petr-and-gas default emissions = max( ECLIPSE, EDGAR 1B2_ Fugitive emissions from oil and gas ) +
#                                                   EDGAR 1B2_Fugitive emissions from gaseous fuels +
#                                                   EDGAR 1B2_ Fugitive emissions from liquid fuels
#     So we split the EDGAR data here first

#   Extract EDGAR Fugitive emissions from oil and gas
    edgar_oil_string <- 'oil and gas'
    edgar_fug_oil_sectors <- grep( edgar_oil_string,  EDGAR_fug_oil_gas_sec_desc, value = TRUE  )
    edgar_fug_oil_sectors <- gsub( " \\s*\\([^\\)]+\\)", "",  edgar_fug_oil_sectors ) %>%
      unique( )

    edgar_gas_string <- 'gaseous fuels'
    edgar_fug_gas_sectors <- grep( edgar_gas_string,  EDGAR_fug_oil_gas_sec_desc, value = TRUE  )
    edgar_fug_gas_sectors <- gsub( " \\s*\\([^\\)]+\\)", "",  edgar_fug_gas_sectors ) %>%
      unique( )

    edgar_liquid_string <- 'liquid fuels'
    edgar_fug_liq_sectors <- grep( edgar_gas_string,  EDGAR_fug_oil_gas_sec_desc, value = TRUE  )
    edgar_fug_liq_sectors <- gsub( " \\s*\\([^\\)]+\\)", "",  edgar_fug_liq_sectors ) %>%
      unique( )

    edgar_oil_gas <- edgar %>%
      dplyr::filter( sector_description %in% edgar_fug_oil_sectors ) %>%
      dplyr::select( iso, sector, all_of(all_Xyear) ) %>%
      dplyr::mutate( sector = if_else( grepl( "x", sector ), # Remove the "x" from biofuel fugitive sector string
                                       gsub( "x", "", sector ), sector ) ) %>%
      dplyr::group_by( iso, sector ) %>%
      dplyr::summarise_all( list( ~sum(., na.rm = T ) ) ) %>% # Aggregate across all relevant 1B2 sectors
      dplyr::ungroup( )

    edgar_gas_fuel <- edgar %>%
      dplyr::filter( sector_description %in% edgar_fug_gas_sectors ) %>%
      dplyr::select( iso, sector, all_of(all_Xyear) )  %>%
      dplyr::mutate( sector = if_else( grepl( "x", sector ), # Remove the "x" from biofuel fugitive sector string
                                       gsub( "x", "", sector ), sector ) ) %>%
      dplyr::group_by( iso, sector ) %>%
      dplyr::summarise_all( list( ~sum(., na.rm = T ) ) ) %>% # Aggregate across all relevant 1B2 sectors
      dplyr::ungroup( )

    edgar_liquid_fuel <- edgar %>%
      dplyr::filter( sector_description %in% edgar_fug_liq_sectors ) %>%
      dplyr::select( iso, sector, all_of(all_Xyear) )  %>%
      dplyr::mutate( sector = if_else( grepl( "x", sector ), # Remove the "x" from biofuel fugitive sector string
                                       gsub( "x", "", sector ), sector ) ) %>%
      dplyr::group_by( iso, sector ) %>%
      dplyr::summarise_all( list( ~sum(., na.rm = T ) ) ) %>% # Aggregate across all relevant 1B2 sectors
      dplyr::ungroup( )

#   If no '1B2-Fugitive emissions from oil and gas' in certain species use the dummy_layout
    if( dim( edgar_oil_gas )[ 1 ] == 0 ){ edgar_oil_gas <- dummy_layout }

#   Extend flaring and edgar_oil_gas to have all countries, and provide countries which are added
#   values of 0 for all years
    flaring_data <- flaring[ , c( 'iso', all_Xyear ) ]
    edgar_data <- edgar_oil_gas[ , c( 'iso', all_Xyear ) ]
    merge_table <- merge( flaring_data, edgar_data, by = 'iso', all = T )
    merge_table[ is.na( merge_table ) ] <- 0

#   Generate falring and EDGAR comparing matrices
    flaring_mat <- merge_table[ , paste0( all_Xyear, '.x' ) ]
    edgar_mat <- merge_table[ , paste0( all_Xyear, '.y' ) ]

# ------------------------------------------------------------------------------
# 4. Generates the flaring default process emissions
#    The final flaring default process emissions takes the max value between EDGAR
#    '1B2-Fugitive emissions from oil and gas'  and flaring extended

    comparing_mat <- flaring_mat >= edgar_mat

    flaring_mask <- ifelse( comparing_mat == T, 1, 0 )
    edgar_mask <- ifelse( comparing_mat  == T, 0, 1 )

    emissions_mat <- flaring_mat * flaring_mask + edgar_mat * edgar_mask

    emissions <- cbind( merge_table$iso, emissions_mat )
    colnames( emissions ) <- c( 'iso', all_Xyear )
    emissions$fuel <- final_fuel
    emissions$sector <- final_sector
    emissions$units <- final_units

    emissions <- emissions[ , c( 'iso', 'sector', 'fuel', 'units', all_Xyear ) ]

# 3.1 optional routine if '1B2-Fugitive emissions from gaseous fuels' does exits
    if ( dim( edgar_gas_fuel )[ 1 ] != 0 ) {

      edgar_gas_fuel <- edgar_gas_fuel %>%
        dplyr::mutate( fuel = final_fuel,
                       sector = final_sector,
                       units = final_units ) %>%
        dplyr::select( iso, sector, fuel, units, all_of(all_Xyear) )

      emissions <- emissions %>%
        dplyr::mutate( iso = as.character( iso ) ) %>%
        dplyr::bind_rows( edgar_gas_fuel ) %>%
        dplyr::group_by( iso, sector, fuel, units ) %>%
        dplyr::summarise_all( list( ~sum(., na.rm = T ) ) ) %>%
        dplyr::ungroup( )

    }

# 3.2 optional routine if '1B2-Fugitive emissions from liquid fuels' does exits
    if ( dim( edgar_liquid_fuel )[ 1 ] != 0 ) {

      edgar_liquid_fuel <- edgar_liquid_fuel %>%
        dplyr::mutate( fuel = final_fuel,
                       sector = final_sector,
                       units = final_units ) %>%
        dplyr::select( iso, sector, fuel, units, all_of(all_Xyear) )

      emissions <- emissions %>%
        dplyr::mutate( iso = as.character( iso ) ) %>%
        dplyr::bind_rows( edgar_liquid_fuel ) %>%
        dplyr::group_by( iso, sector, fuel, units ) %>%
        dplyr::summarise_all( list( ~sum(., na.rm = T ) ) ) %>%
        dplyr::ungroup( )

    }

# -----------------------------------------------------------------------------

# 5. Assign values of non-final CEDS isos to appropriate regions if needed (disaggregate aggregate isos,
#    provide all NAs for missing final CEDS isos, and add non-final CEDS isos to aggregate regions...)
# TODO: This section could be done in a separate PR

#   Subset isos which aren't final CEDS isos and have emissions other than 0 in any year
    MCL_final_isos <- MCL %>%
        dplyr::filter( final_data_flag == 1 | iso %in% c( "gum", "srb (kosovo)" ) ) %>%  #TODO: When this issue ("gum", "srb (kosovo)" ) is fixed in the MCL this rule can be removed
        dplyr::select( iso ) %>%
        dplyr::distinct( )

    emissions <- emissions %>%
        dplyr::mutate( iso = as.character( iso ) )

    not_final_CEDS_isos <- emissions %>%
        dplyr::filter( iso %!in% MCL_final_isos$iso )

    not_final_CEDS_isos_nonzero <- not_final_CEDS_isos %>%
        dplyr::filter_at( .vars = all_Xyear, any_vars( . != 0 ) )

#   Assign values if any non-final CEDS isos have emissions and it is appropriate to do so
    if( nrow( not_final_CEDS_isos_nonzero ) != 0 ){

#       TODO: The following isos had emissions all equal to 0 as of 10.22.19 for CH4, but they could be non-zero in future years.
#             Other ems may have other isos which will needed to be delt with as well.
#         aia (Anguilla), ata (Antarctica), atf (French Southern Territories),
#         bvt (Bouvet Island ), cck (Cocos (Keeling) Islands), cxr (Christmas Island),
#         hmd (Heard Island and McDonald Islands), iot (British Indian Ocean Territory),
#         mnp (Northern Mariana Islands, myt (Mayotte), nfk (Norfolk Island), nru (Nauru),
#         pcn (Pitcairn), sgs (South Georgia and the South Sandwich Islands), tuv (Tuvalu),
#         umi (United States Minor Outlying Islands)

#       TODO: These isos have CH4 emissions which may need to be assigned to isos still, but perhaps should be disaggregated
#             before this point (before the EDGAR and ECLIPSE fugitive emissions are combined). Other ems may have other isos
#             which will needed to be delt with as well.
#         ant - Netherlands Antilles - unclear if some emissions should be
#               assigned to abw, cuw, and sxm (as abw has some emissions already beginning in 1976)
#         scg - Serbia and Montenegro - unclear if some emissions should be
#               split amongst srb, srb (Kosovo), and mne (as srb already has some
#               some emissions beginning in 1970)
#         sea - Int. Shipping

#       shn - Saint Helena - emissions can be added to the UK, if they exist
        if( "shn" %in% not_final_CEDS_isos_nonzero$iso ){

            shn_emissions <- not_final_CEDS_isos_nonzero %>%
                dplyr::filter( iso == "shn" ) %>%
                dplyr::mutate( iso = "gbr" )

            uk_emissions <- emissions %>%
                dplyr::filter( iso == "gbr" ) %>%
                dplyr::bind_rows( shn_emissions ) %>%
                dplyr::group_by( iso, sector, fuel, units ) %>%
                dplyr::summarise_all( funs( sum( .,  na.rm = TRUE ) ) )

            emissions <- emissions %>%
                dplyr::filter( iso %!in% c( "shn", "gbr" ) ) %>%
                dplyr::bind_rows( uk_emissions )

        }

#       Filter out non-final isos
        emissions <- emissions %>%
            dplyr::filter( iso %!in% c( not_final_CEDS_isos_nonzero$iso, not_final_CEDS_isos$iso ) )


    }

#   If any final CEDS isos are not in the emissions data add them (other than global emissions), with the appropriate method
    final_isos_not_in_EDGAR_ECLIPSE_data <- subset( MCL_final_isos$iso, ( !( MCL_final_isos$iso %in%
                                                                               emissions$iso ) &
                                                                              MCL_final_isos$iso != "global" ) )

    if( length( final_isos_not_in_EDGAR_ECLIPSE_data ) != 0 ){

#        If the following isos are missing, add them to the data frame with NA for emissions.
#           TODO: Some of these isos could likely be created by disaggregating from more aggregate regions. There are different
#           missing isos for different ems, which should also be considered here (this list refers isos missing from the CH4 emission data)
#           See below notes:
#           "cuw"             Curacao       - unclear if should disaggregate ant, see above note
#           "lie"             Liechtenstein
#           "mne"             Montenegro    - unclear if should disaggregate scg, see above note
#           "pse"             Palestine     - unclear if should disaggregate isr, likely should be done beforehand
#           "sxm"             Sint Maarten  - unclear if should disaggregate ant, see above note
#           "srb (kosovo)"    Kosovo        - unclear if should disaggregate scg or srb, see above notes

#       Define function to add isos to the fugitive emissions data
        add_isos <- function( iso_in ){

            new_iso <- emissions %>%
                dplyr::slice( 1 ) %>%
                dplyr::mutate( iso = paste0( iso_in ) ) %>%
                dplyr::mutate_at( all_Xyear, funs( identity( NA_real_ ) ) )

        }

#       Add each missing iso(s) with all NA entries
        new_isos <- lapply( final_isos_not_in_EDGAR_ECLIPSE_data, add_isos ) %>%
            dplyr::bind_rows(  )

        emissions <- emissions %>%
            dplyr::bind_rows( new_isos )


#       If South Sudan was a missing iso, disaggregate Sudan into Sudan (sdn) and South Sudan (ssd) using UN population data
        if( "ssd" %in% final_isos_not_in_EDGAR_ECLIPSE_data & "sdn" %!in% final_isos_not_in_EDGAR_ECLIPSE_data ){

            printLog( "Disaggregating sdn fugitive emissions to sdd and ssd using UN population data..." )

            emissions <- disagg_iso( agg_iso_region = "sdn",  additional_iso_to_create = "ssd",
                                      emissions_data_in = emissions, population_data = population,
                                      years_to_disaggregate = all_Xyear )

        }

    }


# Check that the emissions data now only has final CEDS isos
  if( nrow( emissions ) < nrow( MCL_final_isos %>% dplyr::filter( iso != "global" ) ) ){

      stop( "Fugitive emissions data is missing at least one CEDS final isos...")

  } else if( nrow( emissions ) > nrow( MCL_final_isos %>% dplyr::filter( iso != "global" ) ) ){

      stop( "Fugitive emissions data contains at least one iso which is not a final CEDS isos..." )


  }
# -----------------------------------------------------------------------------

# 6. Disaggregate EDGAR-ECLIPSE fugitive oil and gas emissions to fugitive oil,
#    fugitive NG production, and fugitive NG distribution using GAINS fugitive subsector splits

#   Create default splits to be used for NH3 - as NH3 is not reported by GAINS.
#   Fugitive oil share = 1
#   Fugitive NG production share = 0
#   Fugitive NG distribution share = 0
if( em == "NH3" ){

        printLog( "Creating default NH3 fugitive oil and gas subsector emissions shares. ",
                  "All aggregate fugitive oil and gas emissions will be assigned to fugitive oil emissions for this em...")

#       Define function to add isos to the fugitive emissions data
        add_splits <- function( iso_in ){

            new_iso_petr_split <- emissions %>%
                dplyr::slice( 1 ) %>%
                dplyr::mutate( iso = paste0( iso_in ),
                               sector = "1B2_Fugitive-petr" ) %>%
                dplyr::mutate_at( all_Xyear, funs( identity( 1 ) ) )

            new_iso_splits <- new_iso_petr_split %>%
                dplyr::bind_rows( new_iso_petr_split %>%
                                      dplyr::mutate( sector = "1B2b_Fugitive-NG-distr" ) %>%
                                      dplyr::mutate_at( all_Xyear, funs( identity( 0 ) ) ) ) %>%
                dplyr::bind_rows( new_iso_petr_split %>%
                                      dplyr::mutate( sector = "1B2b_Fugitive-NG-prod" ) %>%
                                      dplyr::mutate_at( all_Xyear, funs( identity( 0 ) ) ) )

        }

#       Add each missing iso(s), other than global
        MCL_final_isos_no_global <- MCL_final_isos %>%
            dplyr::filter( iso != "global" )

        NH3_default_splits <- lapply( MCL_final_isos_no_global$iso, add_splits ) %>%
            dplyr::bind_rows(  )

        GAINS_fug_subsec_shares <- NH3_default_splits # Rename so this can flow into the code below

}

#   Disaggregate emissions
    emissions_long <- emissions %>%
        dplyr::select( -sector ) %>%
        tidyr::gather( key = years, value = total_fugitive_emissions, all_Xyear )

    disaggregated_EDGAR_ECLIPSE <- GAINS_fug_subsec_shares %>%
        dplyr::select( iso, sector, fuel, units, all_Xyear ) %>%
        tidyr::gather( key = years, value = shares, all_Xyear ) %>%
        dplyr::rename( units_shares = units ) %>%
        dplyr::left_join( emissions_long, by = c( "iso", "fuel", "years" ) ) %>%
        dplyr::mutate( disaggregate_emissions = shares * total_fugitive_emissions ) %>%
        dplyr::select( iso, sector, fuel, units, years, disaggregate_emissions ) %>%
        tidyr::spread( years, disaggregate_emissions )

#   Check that the only final CEDS isos with NAs are those which were added to the data in the above section
    disaggregated_EDGAR_ECLIPSE_NAs <- disaggregated_EDGAR_ECLIPSE %>%
        dplyr::filter_at( .vars = all_Xyear, any_vars( is.na( . ) ) )

    if( any( disaggregated_EDGAR_ECLIPSE_NAs$iso %!in% final_isos_not_in_EDGAR_ECLIPSE_data ) ){

        stop( "The disaggregated fugitive emisisons data has NA values for isos which were not added to the data as all NA. ",
              "This should not occur..." )

    }

#   Check results - disaggregated emissions should sum back to the aggregate fugitive emissions for each iso (to 3 decimals)
    disaggregated_EDGAR_ECLIPSE_summed <- disaggregated_EDGAR_ECLIPSE %>%
        dplyr::mutate( sector = "1B2_Fugitive-petr-and-gas" ) %>%
        dplyr::group_by( iso, sector, fuel, units ) %>%
        dplyr::summarise_all( funs( sum( ., na.rm = FALSE ) ) ) %>%
        dplyr::mutate_at( all_Xyear, funs( round( ., digits = 3 ) ) ) %>%
        dplyr::ungroup( )

    emissions_for_check <- emissions %>%
        dplyr::arrange( iso ) %>%
        dplyr::mutate_at( all_Xyear, funs( round( ., digits = 3 ) ) )

    diff1 <- return_diff( emissions_for_check, disaggregated_EDGAR_ECLIPSE_summed )
    diff2 <- return_diff( disaggregated_EDGAR_ECLIPSE_summed , emissions_for_check )

    if( nrow( diff1 ) != 0 | nrow( diff2 ) != 0 ){

        stop( paste0( "Fugitive subsector emissions do not equal aggregate sector emissions for all isos. ",
                      "See C1.2.Fugitive-petr-and-gas_default_process_emissions.R... " ) )

    }

# -----------------------------------------------------------------------------
# 7. Write output
#   Add a metadata note for the outputs of this script
    meta_names <- c( "Data.Type", "Emission", "Region", "Sector", "Start.Year",
                     "End.Year", "Source.Comment")
    meta_note <- c( "Default process emissions for 1B2",
                    em, "All", "Fugitive petr. and gas emissions", EDGAR_start_year, end_year,
                    paste0( "Max value between EDGAR", vn,  "data and extended ",
                            "ECLIPSE flaring emissions is taken as default process ",
                            "emissions for country-year combinations for the aggregate fugivitve sector ",
                            "then disaggregated to fugitive oil, fugitive NG production, and fugitive ",
                            "NG distribution using GAINS fugitive subsector shares.") )
    source_info <- script_name
    addMetaData( meta_note, meta_names, source_info )

#   Output disaggregate EDGAR-ECLIPSE fugitive emissions to the default non-combustion emissions directory
    writeData( disaggregated_EDGAR_ECLIPSE , "DEFAULT_EF_IN", domain_extension = 'non-combustion-emissions/',
               paste0( "C.", em, "_Fugitive-petr-and-gas_default_process_emissions" ) )

#   Output aggregate EDGAR-ECLIPSE fugitive emissions to the diagnostic directory
    writeData( emissions , "DIAG_OUT", paste0( "C.", em, "_Fugitive-petr-and-gas_aggregate_emissions" ) )

# Every script should finish with this line:
    logStop()

