# ------------------------------------------------------------------------------
# Program Name: C1.2.add_NC_emissions_EDGAR.R
# Author(s): Jon Seibert, Rachel Hoesly, Steve Smith, Patrick O'Rourke, Noah Prime
# Date Last Modified: June 21, 2021
# Program Purpose: To reformat the non-combustion sections of the EDGAR default emissions
#                      data and add it to the database for the relevant emissions species.
# Input Files: Master_Country_List.csv, Master_EDGAR_sector_mapping
#             bp-stats-review-2019-all-data.xlsx, Master_Fuel_Sector_List.xlsx
#             relevant EDGAR emissions data ( EDGAR = E.[em]_EDGAR_v6.1.csv )
# Output Files: C.CH4_EDGAR_NC_Emissions_fugitive_solid_fuels.csv, C.EDGAR_NC_Emissions_[em].csv,
#               C.EDGAR_NC_Emissions_[em]_negative.csv, C.[em]_NC_emissions_db.csv,
#               C.EDGAR_NC_Emissions_[em]_not_final_isos.csv
# TODO:
#      ext_backward = TRUE extended back only one year. (extend forward worked)
#      Extend forward should extend forward with constant EFs, not linear trend
#      TODOs within the script
# Notes:
# -----------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Universal header file - provides logging, file support, etc.
    headers <- c( "common_data.R","data_functions.R", "analysis_functions.R",
                  "process_db_functions.R", 'timeframe_functions.R') # Additional function files required.
    log_msg <- paste0( "Processing EDGAR non-combustion default emissions data..." ) # First message to be printed to the log
    script_name <- "C1.2.add_NC_emissions_EDGAR.R"
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Settings

# Define emissions species variable
  args_from_makefile <- commandArgs( TRUE )
  em <- args_from_makefile[ 1 ]
  if ( is.na( em ) ) em <- "CH4"

# EDGAR data version number
vn <- "6.1"

# Input domain
domain <- "EM_INV"
domain_ext <- "EDGAR/"

fuel <- "process"
id_cols <- c( "iso", "sector", "fuel", "units" )

# Define EDGAR years
# CO2 end year in v5 is 2018, else 2015
if( em == "CO2" ){

  EDGAR_end_year <- 2018

}

# Global constants defined in common_data.R
EDGAR_years <- EDGAR_start_year : EDGAR_end_year
X_EDGAR_years <- paste0( 'X', EDGAR_start_year : EDGAR_end_year )

# Define sectors that should not use EDGAR (also have to modify C2.1.base_NC_EF.R)
excl_sectors <- c( "2C_Metal-production" )

if( em == "CO2" ) {

  excl_sectors <- c( excl_sectors, "2A1_Cement-production", "3D_Soil-emissions" )

}

if (em == "BC" || em == "OC") {
    excl_sectors <- c( excl_sectors, "2A1_Cement-production",
                                     "2A2_Lime-production",
                                     "1B1_Fugitive-solid-fuels",
                                     "1A1bc_Other-transformation",
                                     "1B2_Fugitive-petr-and-gas",
                                     "2C_Metal-production",
                                     "2B_Chemical-industry",
                                     "2H_Pulp-and-paper-food-beverage-wood",
                                     "5C_Waste-incineration",
                                     "2A6_Other-minerals")
}

# ------------------------------------------------------------------------------
# 2. Input

# Read in pre-formatted EDGAR data from intermediate-output (uses default extension .csv)
edgar <- readData( domain = "MED_OUT", file_name = paste0( "E.", em, "_EDGAR" ))

# Read in master country list mapping file
Master_Country_List <- readData( "MAPPINGS", 'Master_Country_List' )


# Read in EDGAR sector mapping file
Master_sector_map <- readData("MAPPINGS", "Master_EDGAR_sector_mapping")


# Read in master fuel sector list mapping
MFSL <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Sectors" , meta = F ) # TODO: PR comment: this can be deleted if we stick with current unit mapping

# If em is CH4, read in BP coal production data
if ( em == 'CH4' ){

  bp_energy_data <- readData( "ENERGY_IN", BP_data_file_name, ".xlsx")
  BP_coal_production_raw <- bp_energy_data[[ getBPSheetNumber( "coal", "production", "tonnes", bp_energy_data ) ]]
  # First coal production data year is 1981
  X_BP_years <- paste0( 'X', 1981 : BP_last_year )
  BP_coal_production <- cleanBPDataSheet( BP_coal_production_raw, X_BP_years, Master_Country_List )

}

# Read UN Population Data
population <- readData( "MED_OUT", file_name = "A.UN_pop_master" )

# GAINS subsector splits
if( em == "CO2" ){

  GAINS_fug_subsec_shares <- readData( "MED_OUT", file_name = paste0( "C.", em, "_GAINS_fug_oil_gas_shares" ) )

}

# ------------------------------------------------------------------------------
# 3. Define functions used within this script

#   Define function to downscale fugitive oil and gas emissions data from one iso to multiple isos (using
#   UN population data) before applying GAINS fugitive subsector splits to the emissions to create
#   the new 3 fugitive oil and gas subsectors.
#   Note: this function may not be needed in the future, when small isos are handled in prior to this script
#   TODO: combine this function with the function used in C1.2.Fugitive-petr-and-gas_default_process_emissions.R
    disagg_iso <- function( agg_iso_region, additional_iso_to_create,
                            emissions_data_in, population_data, years_to_disaggregate ){

        agg_region_all_isos <- c( additional_iso_to_create, agg_iso_region)

#       Process population data
        population_clean <- population_data %>%
            dplyr::mutate( year = paste0( "X", year ) ) %>%
            dplyr::filter( scenario %in% c( historical_pop_scenario, future_pop_scenario ),
                           year %in% years_to_disaggregate ) %>%
            dplyr::select( iso, year, pop ) %>%
            tidyr::spread( year, pop )

#       Calculate aggregate UN region population
        agg_region_iso_populations <- population_clean %>%
            dplyr::filter( iso %in% agg_region_all_isos )

        aggregated_population <- agg_region_iso_populations %>%
            dplyr::select( -iso ) %>%
            dplyr::summarise_all( funs( sum ( ., na.rm = TRUE ) ) ) %>%
            tidyr::gather( key = Years, value = agg_reg_population, years_to_disaggregate )

#       Calculate UN population share for each sub-iso
        agg_region_iso_pop_shares <- agg_region_iso_populations %>%
            tidyr::gather( key = Years, value = Population, years_to_disaggregate ) %>%
            dplyr::left_join( aggregated_population, by = "Years" ) %>%
            dplyr::mutate( iso_share_of_agg_region_pop = Population / agg_reg_population ) %>%
            dplyr::select( -Population, -agg_reg_population  )

#       Filter out the agg iso and iso to disaggegate, if needed
        emissions_without_isos_being_replaced <- emissions_data_in %>%
            dplyr::filter( iso %!in% agg_region_all_isos )

#       Filter for agg region emissions data
        agg_region_of_interest <- emissions_data_in %>%
            dplyr::filter( iso == agg_iso_region )

#       Define function to add duplicate row of agg_region data with iso renamed as the
#       missing iso whose data is being created by disaggregating the agg_region
        add_isos_for_disagg <- function( iso_in ){

            new_iso <- agg_region_of_interest %>%
                dplyr::mutate( iso = paste0( iso_in ) )

        }

#       Create disaggregate iso rows
        new_isos_for_disagg <- lapply( additional_iso_to_create, add_isos_for_disagg ) %>%
            dplyr::bind_rows(  )

#       Disaggregate the emissions data for the agg region
        emissions_data_in_all_region_isos <- agg_region_of_interest %>%
            dplyr::bind_rows( new_isos_for_disagg ) %>%
            tidyr::gather( key = Years, value = Emissions, years_to_disaggregate ) %>%
            dplyr::left_join( agg_region_iso_pop_shares, by = c( "iso", "Years" ) ) %>%
            dplyr::mutate( disagg_emissions = Emissions * iso_share_of_agg_region_pop ) %>%
            dplyr::select( iso, sector, fuel, units, Years, disagg_emissions )

#       Check that there are no NAs or NaNs for new downscaled emissions
        if( any( is.na( emissions_data_in_all_region_isos$disagg_emissions ) |
                is.nan( emissions_data_in_all_region_isos$disagg_emissions ) ) ){

            stop( paste0( "Some downscaled emissions are now NA. Check population and emissions data ",
                          "in ", script_name, "..." ) )

        }

#       Check that disagg. regions summed = agg. region data for each sector - rounded to 10 decimals
        downscaled_check <- emissions_data_in_all_region_isos %>%
            dplyr::select( -iso ) %>%
            dplyr::group_by( sector, fuel, units, Years ) %>%
            dplyr::summarise_all( funs( sum ( ., na.rm = TRUE ) ) ) %>%
            dplyr::arrange( sector, fuel, units, Years ) %>%
            dplyr::mutate( disagg_emissions = as.numeric(round( disagg_emissions, digits = 10 ) )) %>%
            dplyr::ungroup( ) %>%
            dplyr::rename( Emissions = disagg_emissions )

        agg_region_of_interest_long <- agg_region_of_interest %>%
            tidyr::gather( key = Years, value = Emissions, years_to_disaggregate ) %>%
            dplyr::select( -iso ) %>%
            dplyr::group_by( sector, fuel, units, Years ) %>%
            dplyr::summarise_all( funs( sum ( ., na.rm = TRUE ) ) ) %>%
            dplyr::mutate( Emissions = as.numeric(round( Emissions, digits = 10 ) ))

        check <- downscaled_check %>%
            left_join(agg_region_of_interest_long, by = c('sector','fuel','units','Years')) %>%
            mutate(test = all.equal(Emissions.x,Emissions.y, tolerance = .00000001))
        #use near equal test because of floating point precision

        if(! all(check$test) ){

            stop( paste0( "Downscaled emissions do not equal aggregate region emissions for all sectors. ",
                          "See ", script_name, "..." ) )

        }

#       Replace the original agg region data
        emissions_data_in_all_region_isos_wide <- emissions_data_in_all_region_isos %>%
            dplyr::group_by( iso, sector, fuel, units, Years ) %>%
            dplyr::summarise_all( funs( sum ( ., na.rm = TRUE ) ) ) %>%
            tidyr::spread( Years, disagg_emissions )

        final_disagg_emissions <- emissions_without_isos_being_replaced %>%
            dplyr::bind_rows( emissions_data_in_all_region_isos_wide )

        return( final_disagg_emissions )

    }

# ------------------------------------------------------------------------------
# 4. Initial Reformatting

# reformatting of sector mapping object
Master_sector_map <- Master_sector_map %>%
  dplyr::filter( edgar_sector %in% c('4D3') | type == 'NC') %>%
  dplyr::mutate( edgar_descr = gsub( " \\s*\\([^\\)]+\\)", "",  edgar_descr )) %>%
  dplyr::rename( sector_description = edgar_descr )

# rename sector -> edgar_sector
edgar <- edgar %>%
  dplyr::rename(edgar_sector = sector)

# Add fuel column
edgar$fuel <- fuel

# ------------------------------------------------------------------------------
# 5. Account for EDGAR 4D3 Indirect N2O from agriculture - for N2O only

if ( em == 'N2O' ){

# Subset EDGAR 4D3 Indirect N2O from agriculture, as this will be split to multiple
# CEDS sectors later

temp_edgar_years <- paste( X_EDGAR_years )

edgar_4D3_data <- edgar %>%
    dplyr::filter( edgar_sector == "4D3" ) %>%
    dplyr::select( iso, edgar_sector, fuel, temp_edgar_years ) %>%
    tidyr::gather( key = Year, value = Emissions_4D3, temp_edgar_years ) %>%
    dplyr::select( -edgar_sector )

edgar <- edgar %>%
    dplyr::filter( edgar_sector != "4D3" )

# Add ceds_sector column and units from sector mapping file
edgar <- edgar %>%
  dplyr::left_join( Master_sector_map %>% dplyr::select( edgar_sector, sector_description, ceds_sector ),
                  by = c("edgar_sector", "sector_description") ) %>%
  dplyr::rename( sector = ceds_sector )


# Add back "4D3 - Indirect N2O from agriculture" by splitting it among CEDS sectors "3B_Manure-management"
# and "3D_Soil-emissions", based on the relative amount of emissions between these two sectors
# for each iso

#   Subset EDGAR data for 3B and 3D
    edgar_3Band3D <- edgar %>%
        dplyr::filter( sector %in% c( "3B_Manure-management", "3D_Soil-emissions" ) ) %>%
        dplyr::select( iso, edgar_sector, sector, fuel, temp_edgar_years ) %>%
        tidyr::gather( key = Year, value = Emissions, temp_edgar_years )

#   Aggregate EDGAR data for 3B and 3D
    edgar_3Band3D_summed <- edgar_3Band3D %>%
        dplyr::select( -edgar_sector, -sector ) %>%
        dplyr::filter( ! ( is.na( Emissions ) ) ) %>%
        dplyr::group_by( iso, fuel, Year ) %>%
        dplyr::summarise_all( sum ) %>%
        dplyr::ungroup( ) %>%
        dplyr::rename( Aggregated_3Band3D_emissions = Emissions )

#   Calculate weights for EDGAR data
    edgar_3Band3D_weights <- edgar_3Band3D %>%
        dplyr::left_join( edgar_3Band3D_summed, by = c( "iso", "fuel",  "Year" ) ) %>%
        dplyr::mutate( Weights = Emissions / Aggregated_3Band3D_emissions )

#   Add isos that are missing from 3B and 3D data but present in 4D3 data, and assign each of their
#   3B and 3D sectors an equal amount of emissions (1/3 weight, since 3 EDGAR sectors map to these 2 CEDS
#   sectors). Here we assume that missing data values are equivalent to 0 emissions.
#   We also this assumption  when other isos are NA for Emissions or Aggregated 3B & 3D emissions
    unique_3B3D_isos <- sort( unique( edgar_3Band3D_weights$iso ) )

    isos_4D3_not_in_3B3D <- edgar_4D3_data %>%
        dplyr::filter( !( iso %in% unique_3B3D_isos ) ) %>%
        dplyr::select( -Emissions_4D3 ) %>%
        dplyr::mutate( Weights = (1 / 3) ) %>% # since there are 3 sectors to split evenly among (4B, 4D1, 4D2)
        dplyr::mutate( Emissions = 0, Aggregated_3Band3D_emissions = 0 )

    missing_isos_4B <- isos_4D3_not_in_3B3D %>%
        dplyr::mutate( edgar_sector = "4B",
                       sector = "3B_Manure-management" )

    missing_isos_4D2 <- missing_isos_4B %>%
        dplyr::mutate( edgar_sector = "4D2" )

    missing_isos_4D1 <- isos_4D3_not_in_3B3D %>%
        dplyr::mutate( edgar_sector = "4D1",
                       sector = "3D_Soil-emissions" )

    missing_isos_3B3D <- dplyr::bind_rows(missing_isos_4B, missing_isos_4D2, missing_isos_4D1 ) %>%
        dplyr::select( iso, edgar_sector, sector, fuel, Year, Emissions, Aggregated_3Band3D_emissions,
                       Weights )

    edgar_3Band3D_weights_with_missing_isos <- edgar_3Band3D_weights %>%
        dplyr::bind_rows( missing_isos_3B3D ) %>%
        dplyr::group_by( iso, edgar_sector, sector, fuel, Year ) %>%
        dplyr::mutate( Weights = if_else( is.na( Emissions ) & is.na( Aggregated_3Band3D_emissions ) &
                                                is.na( Weights ) ,
                                          ( 1 / 3 ), Weights ) ) %>%
        dplyr::mutate( Emissions = if_else( is.na( Emissions ) & is.na( Aggregated_3Band3D_emissions ) &
                                                Weights == ( 1 / 3 ),
                                           0, Emissions ) ) %>%
        dplyr::mutate( Aggregated_3Band3D_emissions = if_else( Emissions == 0 &
                                                               is.na( Aggregated_3Band3D_emissions ) &
                                                               Weights == ( 1 / 3 ),
                                                               0, Aggregated_3Band3D_emissions ) ) %>%
        dplyr::mutate( Emissions = if_else( is.na( Emissions ) & is.na( Weights) &
                                                !( is.na( Aggregated_3Band3D_emissions) ),
                                            0, Emissions ) ) %>%
        dplyr::mutate( Weights = if_else ( Emissions == 0 & is.na( Weights ) &
                                               !(is.na( Aggregated_3Band3D_emissions ) ),
                                            0, Weights) )

#   Calculate new values for 3B and 3D sectors (old values + weighted 4D3)
    edgar_3Band3D_new <- edgar_3Band3D_weights_with_missing_isos %>%
        dplyr::left_join( edgar_4D3_data, by = c( "iso", "fuel", "Year" ) ) %>%
        dplyr::mutate( Emissions_4D3_weighted = Weights * Emissions_4D3 ) %>%
        dplyr::mutate( Emissions_new3B3D = Emissions + Emissions_4D3_weighted ) %>%
        dplyr::select( iso, edgar_sector, sector, fuel, Year, Emissions_new3B3D )

#   Add EDGAR 4D3 to EDGAR 3B and 3D based on the relative amount of emissions between these
#   two sectors for each iso (note that this is really for the relative amount of emissions between
#   more than 2 EDGAR sectors, as mulitple EDGAR sectors map to 1 CEDS sector). If GAINS values were
#   previously NA and are now 0 (for the GAINS sectors which map to CEDS 3B and 3D), make the new values
#   also NA (as no data in sector 4D3 from EDGAR 4.2 was actually reported as 0, the zeroes came in while
#   making calculations over NAs ).

    edgar_long <- edgar %>%
        tidyr::gather( key = Year, value = Emissions, temp_edgar_years ) %>%
        dplyr::left_join( edgar_3Band3D_new, by = c( "iso", "edgar_sector", "sector",
                                                    "fuel", "Year" ) ) %>%
        dplyr::ungroup( ) %>%
        dplyr::mutate( Emissions_new3B3D = if_else( is.na( Emissions ) & Emissions_new3B3D == 0.000000,
                                                NA_real_, Emissions_new3B3D ) ) %>%
        dplyr::mutate( Emissions = if_else( sector %in% c( "3B_Manure-management", "3D_Soil-emissions" ),
                                            Emissions_new3B3D, Emissions ) ) %>%
        dplyr::select( -Emissions_new3B3D )

    edgar <- edgar_long %>%
        tidyr::spread( Year, Emissions) %>%
        dplyr::select( iso, edgar_sector, fuel, sector, units, temp_edgar_years )

} else{

  # Add ceds_sector column from sector mapping file
  edgar <- edgar %>%
    dplyr::left_join( Master_sector_map %>% dplyr::select( edgar_sector, sector_description, ceds_sector ),
                      by = c("edgar_sector", "sector_description") ) %>%
    dplyr::rename( sector = ceds_sector )


}

# ------------------------------------------------------------------------------

# 6. Finish processing EDGAR data

# Leave out excluded sectors and filter out sectors which did not map to CEDS sectors
edgar <- edgar %>%
  dplyr::filter( sector %!in% excl_sectors ) %>%
  dplyr::filter( !is.na( sector ) & sector != 'NA')

# Rearrange columns to proper order (iso-sector-fuel-units-data)
len <- ncol( edgar )

edgar <- edgar %>%
  dplyr::select( id_cols, edgar_sector, X_EDGAR_years ) %>%
  dplyr::arrange( iso, sector, edgar_sector, fuel )


# Convert data to class numeric, from class character
edgar <- edgar %>%
  dplyr::mutate_at( .vars =  X_EDGAR_years,
                    .funs = list( ~as.numeric( . ) ) )

# There are instances where multiple EDGAR sectors map to 1 CEDS sector. Now that these
# sectors have been mapped they can be aggregated
data_to_be_summarized <- edgar[ duplicated( edgar[ , id_cols ] ), ] %>%
  dplyr::select( id_cols ) %>%
  # Up to this point, the df still contains the edgar_sector column, but it will only retain
  # the first row (i.e.) first edgar_sector that corresponds to a CEDS_sector. So we need
  # to rejoin the edgar_data again to obtain all edgar_sectors that correspond to each CEDS sector
  dplyr::left_join( edgar %>% dplyr::select( id_cols, edgar_sector ), by = c( id_cols ) ) %>%
  dplyr::mutate( summarise_needed_flag = 1 )

edgar <- edgar %>%
  dplyr::left_join( data_to_be_summarized, by = c( id_cols, "edgar_sector" ) ) %>%
  dplyr::mutate( summarise_needed_flag = if_else( is.na( summarise_needed_flag ), 0, summarise_needed_flag ) ) %>%
  dplyr::select( -edgar_sector ) %>%
  dplyr::distinct( )

data_to_be_summarized <- edgar %>%
  dplyr::filter( summarise_needed_flag == 1 ) %>%
  dplyr::select( -summarise_needed_flag )

aggregated_data <- data_to_be_summarized %>%
  dplyr::group_by_at( id_cols ) %>%
  dplyr::summarise_all( list( ~sum(., na.rm = T ) ) ) %>% # Note: This means if an iso has NA for the multiple EDGAR sectors for the same year,
                                                         #       the NA values will be aggregated to 0 ( example: CH4 fugitive petr and oil for cyp in 1970 )
  dplyr::ungroup( )

edgar <- edgar %>%
  dplyr::filter( summarise_needed_flag != 1 ) %>%
  dplyr::select( -summarise_needed_flag ) %>%
  dplyr::bind_rows( aggregated_data ) %>%
  dplyr::arrange( iso, sector, fuel )

# Remove rows with all NA's
edgar <- edgar[ apply( X = edgar[ , X_EDGAR_years ],
                       MARGIN = 1, function( x ) ( !all.na( x ) ) ) ,]

# Turn NAs to zeros
edgar[ is.na( edgar ) ] <- 0

# Make negative emissions zero
neg_rows <- apply( edgar[, X_EDGAR_years ], 1, function( row ) any( row < 0 ) )
edgar_neg <- edgar[ neg_rows, ]
edgar[ edgar < 0 ] <- 0

# Filter for isos in Master Country List
# Note: this currently leaves in isos which are in the MCL, but not final CEDS isos
# TODO: In the future, add data for isos which are not in MCL as well as isos which are
#       not final CEDS isos to a CEDS final iso, if appropriate.
edgar_iso_not_in_MCL <- edgar %>%
  dplyr::filter( iso %!in% Master_Country_List$iso )

if( nrow( edgar_iso_not_in_MCL ) > 0 ){

  printLog( "The following EDGAR isos were not mapped to a CEDS iso",
            "in", script_name, ":",
            sort( unique( edgar_iso_not_in_MCL$iso ) ) )

}

edgar <- edgar %>%
    dplyr::filter( iso %in% Master_Country_List$iso )

# ------------------------------------------------------------------------------
# 7. Extend Fugitive Solid Fuels for methane, if EDGAR emissions do not go the last
#    available BP year

if( em == 'CH4' & ( EDGAR_end_year < BP_last_year ) ){

# Seperate fugitive solid fuels and other emissions
  fugitive_solid <- edgar %>%
      dplyr::filter( sector == '1B1_Fugitive-solid-fuels' )

  edgar <- edgar %>%
      dplyr::filter( sector != '1B1_Fugitive-solid-fuels' )

# Process BP data for extension
  bp <- Master_Country_List %>%
    dplyr::select( iso, BPName ) %>%
    dplyr::filter( !is.na( BPName ), BPName != 'ussr' ) %>%
    dplyr::distinct( ) %>%
    dplyr::filter( !duplicated( iso ) ) %>%
    dplyr::distinct( ) %>%
    dplyr::left_join( BP_coal_production, by = 'BPName' ) %>%
    tidyr::gather( year, value, -iso, -BPName ) %>%
    dplyr::mutate( year = gsub( "X", "", year ) ) %>%
    dplyr::mutate( year = as.numeric( year ) ) %>%
    dplyr::filter( !is.na( year ), !is.na( value ) ) %>%
    dplyr::mutate( year = paste0( 'X', year ) ) %>%
    dplyr::mutate( value = as.numeric( value ) ) %>%
    tidyr::spread( year, value ) %>%
    dplyr::select( -BPName )

# Extend fugitive solid emissions
  fugitive_solid_extended <- extend_data_on_trend_range( driver_trend = bp,
                                                         input_data = fugitive_solid,
                                                         start = ( EDGAR_end_year + 1 ), end = BP_last_year,
                                                         ratio_start_year = ( EDGAR_end_year - 1 ),
                                                         expand = T,
                                                         range = 2,
                                                         id_match.driver = c( 'iso' ) )

  fugitive_solid_extended <- fugitive_solid_extended[ , c( 'iso','sector','fuel','units',
                                                           paste0( 'X', EDGAR_start_year : BP_last_year ) ) ]

}

# ------------------------------------------------------------------------------
# 8. Assign values of non-final CEDS isos to appropriate regions if needed (disaggregate aggregate isos,
#    provide all NAs for missing final CEDS isos, and add non-final CEDS isos to aggregate regions...)

#   Subset isos which aren't final CEDS isos and have emissions other than 0 in any year
MCL_final_isos <- Master_Country_List %>%
  dplyr::filter( final_data_flag == 1 | iso %in% c( "gum", "srb (kosovo)" ) ) %>%  #TODO: when this issue ("gum", "srb (kosovo)" ) is fixed in the MCL this rule can be removed
  dplyr::select( iso ) %>%
  dplyr::distinct( )

not_final_CEDS_isos <- edgar %>%
  dplyr::filter( iso %!in% MCL_final_isos$iso )

not_final_CEDS_isos_nonzero <- not_final_CEDS_isos %>%
    dplyr::filter_at( .vars = X_EDGAR_years, any_vars( . != 0 ) )

#   Assign values if any non-final CEDS isos have emissions and it is appropriate to do so
if( nrow( not_final_CEDS_isos_nonzero ) != 0 ){

#       TODO: These isos have CO2 emissions which may need to be assigned to isos still. Other ems may have other isos
#             which will needed to be delt with as well (this list refers isos missing from the CO2 emission data, as of EDGAR v4.2)
#         ant - Netherlands Antilles - unclear if some emissions should be
#               assigned to abw, cuw, and sxm (as abw has some emissions already beginning in 1976 for CH4)
#         scg - Serbia and Montenegro - unclear if some emissions should be
#               split amongst srb, srb (Kosovo), and mne (as srb already has some
#               some emissions beginning in 1970 for CH4)

#       Filter out non-final isos
    edgar <- edgar %>%
        dplyr::filter( iso %!in% c( not_final_CEDS_isos_nonzero$iso, not_final_CEDS_isos$iso ) )
}

#  If any final CEDS isos are not in the emissions data add them (other than global emissions), with the appropriate method
final_isos_not_in_EDGAR_data <- subset( MCL_final_isos$iso, ( !( MCL_final_isos$iso %in%
                                                                edgar$iso ) &
                                                              MCL_final_isos$iso != "global" ) )

if( length( final_isos_not_in_EDGAR_data ) != 0 ){

#        If the following isos are missing, add them to the data frame with NA for emission for each sector
#           TODO: Some of these isos could likely be created by disaggregating from more aggregate regions. There are different
#           missing isos for different ems, which should also be considered here (this list refers isos missing from the CO2 emission data)
#           See below notes: (As of EDGAR v4.2)
#               "cok"             Cook Islands                      - unclear if should disaggregate nzl
#               "cuw"             Curacao                           - unclear if should disaggregate ant, see above note
#               "fsm"             Federated States of Micronesia
#               "lie"             Liechtenstein
#               "mhl"             Marshall Islands                  - unclear if should disaggregate the USA
#               "mne"             Montenegro                        - unclear if should disaggregate scg, see above note
#               "niu"             Niue                              - unclear if should disaggregate nzl
#               "plw"             Palau                             - unclear if should disaggregate the USA
#               "pse"             Palestine                         - unclear if should disaggregate isr
#               "srb"             Serbia                            - unclear if should disaggregate scg, see above note
#               "srb (kosovo)"    Kosovo                            - unclear if should disaggregate scg, see above note
#               "sxm"             Sint Maarten                      - unclear if should disaggregate ant, see above note
#               "tca"             Turks and Caicos Islands          - unclear if should disaggregate gbr
#               "tkl"             Tokelau                           - unclear if should disaggregate nzl, has some emissions for CH4 already
#               "wlf"             Wallis and Futuna Islands         - unclear if shold disaggregate fra, has some emissions for CH4 already
#       Define function to add isos to the emissions data for each relevant sector
    add_isos <- function( iso_in ){

      new_iso <- edgar %>%
          dplyr::mutate( iso = NA_character_ ) %>%
          dplyr::mutate_at( X_EDGAR_years, funs( identity( NA_real_ ) ) ) %>%
          dplyr::distinct( ) %>%
          dplyr::mutate( iso = paste0( iso_in ) )

    }

#       Add each missing iso(s) with all NA entries
    new_isos <- lapply( final_isos_not_in_EDGAR_data, add_isos ) %>%
        dplyr::bind_rows(  )

    edgar <- edgar %>%
        dplyr::bind_rows( new_isos )

#       If South Sudan was a missing iso, disaggregate Sudan into Sudan (sdn) and South Sudan (ssd) using UN population data
    if( "ssd" %in% final_isos_not_in_EDGAR_data & "sdn" %!in% final_isos_not_in_EDGAR_data ){

      printLog( "Disaggregating sdn EDGAR", em, "emissions to sdd and ssd using UN population data..." )

      edgar <- disagg_iso( agg_iso_region = "sdn",
                           additional_iso_to_create = "ssd",
                           emissions_data_in = edgar,
                           population_data = population,
                           years_to_disaggregate = X_EDGAR_years )

    }

}

#   Check that the emissions data now only has final CEDS isos
edgar_final_unique_isos <- edgar %>%
    dplyr::select( iso ) %>%
    dplyr::distinct( )

MCL_final_isos_no_global <- MCL_final_isos %>%
    dplyr::filter( iso != "global" )

if( nrow( edgar_final_unique_isos ) < nrow( MCL_final_isos_no_global ) ){

    stop( "Emissions data is missing at least one CEDS final isos. See ", script_name )

} else if( nrow( edgar_final_unique_isos ) > nrow( MCL_final_isos_no_global ) ){

    stop( "Emissons data contains at least one iso which is not a final CEDS isos. See ", script_name )

}


#   Add sectors that are missing for certain isos as all NAs
edgar_iso_sector_combos <- edgar %>%
    expand( iso, sector )

edgar <- edgar %>%
    dplyr::full_join( edgar_iso_sector_combos, by = c( "iso", "sector" ) ) %>%
    dplyr::mutate( fuel = "process", units = "kt" )

# ------------------------------------------------------------------------------

# 9. If em is CO2, disaggregate Fugitive oil and petr. emissions using GAINS subsector shares
#    (Fugitive oil, fugitive NG production, fugitive NG distribution). Otherwise, remove fugitive
#    oil and gas emissions from the data
#    TODO: This could be functionalized to flexible for use with the similar section of code within
#           C1.2.Fugitive-petr-and-gas_default_process_emissions.R

if( em == "CO2" ){

#   Subset fugitive oil and gas emissions
    edgar_fugitive_oil_gas <- edgar %>%
        #dplyr::filter( sector == "1B2_Fugitive-petr-and-gas" )
        dplyr::filter( sector == "1B2_Fugitive-petr" )

    edgar_no_fugitive_oil_gas <- edgar %>%
        dplyr::filter( sector != "1B2_Fugitive-petr" )
        #dplyr::filter( sector != "1B2_Fugitive-petr-and-gas" )

#   Disaggregate fugitive oil and gas emissions
    edgar_fugitive_oil_gas_long  <- edgar_fugitive_oil_gas %>%
        dplyr::select( -sector ) %>%
        tidyr::gather( key = years, value = total_fugitive_emissions, X_EDGAR_years )

    disaggregated_EDGAR_fug <- GAINS_fug_subsec_shares %>%
        dplyr::select( iso, sector, fuel, units, X_EDGAR_years ) %>%
        tidyr::gather( key = years, value = shares, X_EDGAR_years ) %>%
        dplyr::rename( units_shares = units ) %>%
        dplyr::left_join( edgar_fugitive_oil_gas_long, by = c( "iso", "fuel", "years" ) ) %>%
        dplyr::mutate( disaggregate_emissions = shares * total_fugitive_emissions ) %>%
        dplyr::select( iso, sector, fuel, units, years, disaggregate_emissions ) %>%
        tidyr::spread( years, disaggregate_emissions )

#   Check results - disaggregated emissions should sum back to the aggregate fugitive emissions for each iso (to 5 decimals)
    disaggregated_EDGAR_fug_summed <- disaggregated_EDGAR_fug %>%
        dplyr::mutate( sector = "1B2_Fugitive-petr" ) %>%
        #dplyr::mutate( sector = "1B2_Fugitive-petr-and-gas" ) %>%
        dplyr::group_by( iso, sector, fuel, units ) %>%
        dplyr::summarise_all( funs( sum( ., na.rm = FALSE ) ) ) %>%
        dplyr::mutate_at( X_EDGAR_years, funs( round( ., digits = 10 ) ) ) %>%
        dplyr::ungroup( )

    emissions_for_check <- edgar_fugitive_oil_gas %>%
        dplyr::arrange( iso ) %>%
        dplyr::mutate_at( X_EDGAR_years, funs( round( ., digits = 10 ) ) )

    diff1 <- setdiff( emissions_for_check, disaggregated_EDGAR_fug_summed )
    diff2 <- setdiff( disaggregated_EDGAR_fug_summed , emissions_for_check )

    if( nrow( diff1 ) != 0 | nrow( diff2 ) != 0 ){

        stop( paste0( "Fugitive subsector emissions do not equal aggregate sector emissions for all isos. ",
                      "See ", script_name ) )

    }

#   Rename the edgar data base without fugitive oil and gas emissions, as these will be output seperately
    edgar <- edgar_no_fugitive_oil_gas

} else{

    edgar <- edgar %>%
        dplyr::filter( sector != "1B2_Fugitive-petr" )
        #dplyr::filter( sector != "1B2_Fugitive-petr-and-gas" )

}

# ------------------------------------------------------------------------------
# 10. Aggregate CEDS sectors, as multiple EDGAR sectors are mapped to 1 CEDS
#    sector (such as EDGAR sectors 4B and 4D2 mapping to CEDS 3B_Manure-management )
#    Note: This is done because addToEmissionsDb expects there to only be 1 value
#          for each CEDS sector (for each CEDS sec, fuel, and iso combination),
#          thus without doing this addToEmissionsDb would only copy the first value for
#          a CEDS sector, fuel, and iso combination, when there could be more than 1 value
# TODO: This part may not be needed any longer, as emissions are aggregated above as well
edgar <- edgar %>%
    dplyr::group_by( iso, sector, fuel, units ) %>%
    dplyr::summarize_all( funs( sum(., na.rm = TRUE ) ) )

# ------------------------------------------------------------------------------
# 11. If em is N2O, subset the 2B_Chemical-industry emissions, which will be utilized
#     later in CEDS while finalizing adipic and nitric acid emissions
if( em == "N2O" ){

    # Remove these from EDGAR for now
    # Will be added back in later minus specific adiptic and nitric acid sector emissions
    edgar_final_emissions <- edgar %>%
        dplyr::filter( sector != "2B_Chemical-industry" )

    edgar_2B_chemical_industry_emissions <- edgar %>%
        dplyr::filter( sector == "2B_Chemical-industry" )


} else {

    edgar_final_emissions <- edgar

}

# ------------------------------------------------------------------------------

# 12. Output

# If em is CH4, output extended fugitive solid emissions
if ( em == 'CH4' ){

  writeData( fugitive_solid_extended,  domain = "DEFAULT_EF_IN", domain_extension = "non-combustion-emissions/",
             fn = paste0( "C.",em, "_EDGAR_NC_Emissions_fugitive_solid_fuels" ) )

}

# Add EDGAR NC emissions to CEDS default NC emissions database
addToEmissionsDb( edgar_final_emissions, em = em, type = 'NC', ext_backward = FALSE, ext_forward = FALSE )
writeData( edgar, domain = "DIAG_OUT", fn = paste0( "C.EDGAR_NC_Emissions_",em ) ) # Output includes 2B_Chemical-industry, even for N2O

#   Output disaggregate EDGAR fugitive emissions to the default non-combustion emissions directory, if em is CO2
if ( em == "CO2" ){

    writeData( disaggregated_EDGAR_fug , "DEFAULT_EF_IN", domain_extension = 'non-combustion-emissions/',
               paste0( "C.", em, "_Fugitive-petr-and-gas_default_EDGAR_process_emissions" ) )

}

#   Output EDGAR 2B_Chemical-industry emissions, if em is N2O (used to extend EPA adipic
#   and nitric acid emissions)
if( em == "N2O" ){

    writeData( edgar_2B_chemical_industry_emissions , "MED_OUT",
               paste0( "C.", em, "_EDGAR_chemical_industry_emissions" ) )

}

# Diagnostic outputs:

#   Ouptut EDGAR data which had at least one negative value (that was then set to 0)
    if ( nrow( edgar_neg ) > 0 ){

      writeData( edgar_neg, domain = "DIAG_OUT", fn = paste0( "C.EDGAR_NC_Emissions_",em, "_negative" ) )

    }

#   Output EDGAR data for isos not defined in CEDS MCL
    if ( nrow( edgar_iso_not_in_MCL ) > 0 ){

      writeData( edgar_iso_not_in_MCL, domain = "DIAG_OUT", fn = paste0( "C.EDGAR_NC_Emissions_",em, "_not_final_isos" ) )

    }


logStop( )

# END
