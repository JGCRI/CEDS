# ------------------------------------------------------------------------------
# Program Name: C1.2.add_NC_emissions_EDGAR.R
# Author(s): Jon Seibert, Rachel Hoesly, Steve Smith, Patrick O'Rourke
# Date Last Modified: June 11, 2020
# Program Purpose: To reformat the non-combustion sections of the EDGAR default emissions
#                      data and add it to the database for the relevant emissions species.
# Input Files: NC_EDGAR_sector_mapping.csv, Master_Country_List.csv,
#             bp-stats-review-2019-all-data.xlsx, Master_Fuel_Sector_List.xlsx
#             relevant EDGAR emissions data ( EDGAR v5 = v50_[em]_1970_[edgar_end_year].xls )
# Output Files: C.CH4_EDGAR_NC_Emissions_fugitive_solid_fuels.csv, C.EDGAR_NC_Emissions_[em].csv,
#               C.EDGAR_NC_Emissions_[em]_negative.csv, C.[em]_NC_emissions_db.csv,
#               C.EDGAR_NC_Emissions_[em]_not_final_isos.csv
# TODO:
#      ext_backward = TRUE extended back only one year. (extend forward worked)
#      Extend forward should extend forward with constant EFs, not linear trend
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
vn <- "5.0"

# Input domain
domain <- "EM_INV"
domain_ext <- "EDGAR/"

fuel <- "process"
id_cols <- c( "iso", "sector", "fuel", "units" )

# Temporary assignment for script development
#em <- "CO2"

# Define EDGAR years
if( em == "CO2" & as.numeric( vn ) == 5.0 ){

  EDGAR_end_year <- 2018

}

EDGAR_years <- EDGAR_start_year : EDGAR_end_year
X_EDGAR_years <- paste0( 'X', EDGAR_start_year : EDGAR_end_year )

# Define sectors that should not use EDGAR (also have to modify C2.1.base_NC_EF.R)
excl_sectors <- c( )

if( em == "CO2" ) {

  excl_sectors <- c( excl_sectors, "2A1_Cement-production", "3D_Soil-emissions" )

}

# ------------------------------------------------------------------------------
# 2. Input

# Determine file settings for different EDGAR versions

#   Naming pattern for EDGAR v4.2
    if( as.numeric( vn ) == 4.2 ){

      fn <- c( paste0( "EDGAR", gsub( "[.]", "", vn ), "_", em  ), ".csv" )

#   Naming pattern for EDGAR v4.3
    } else if( as.numeric( vn ) == 4.3 ){

      fn <- c( paste0( 'JRC_PEGASOS_',em,'_TS_REF' ), ".xlsx" )
      sheet_name <- paste0( 'NEW_v', vn, '_EM_', em, '_ref' )
      rows_to_skip <- 8

#   Naming pattern for EDGAR v5
    } else if( as.numeric( vn ) == 5 ){

      fn <- c( paste0( "v",  gsub( "[.]", "", vn ), "_", em, "_", EDGAR_start_year, "_",
                       EDGAR_end_year ), ".xls")
      sheet_to_use <- paste0( "v", vn, "_EM_", em, "_IPCC1996" )
      rows_to_skip <- 9

#     EDGAR v5 has a special file naming convention for CO2
      if( em == "CO2" ){

        EDGAR_CO2_real_end_year <- 2018

        fn <- c( paste0( "v",  gsub( "[.]", "", vn ), "_", em, "_", "excl_short-cycle_org_C_",
                         EDGAR_start_year, "_", EDGAR_CO2_real_end_year ), ".xls")

      }

#   Naming pattern for other EDGAR versions
  } else {

    stop( script_name, " has not been formatted to process Edgar v", vn, ". Please ",
          "reformat the script and rerun." )

  }

# Read in EDGAR data
  if( as.numeric( vn ) == 4.2 ){

    edgar <- readData( domain, fn[[ 1 ]], fn[[ 2 ]], domain_extension = domain_ext )

  } else if( as.numeric( vn ) == 4.3 ){

    edgar <- readData( domain, domain_extension = domain_ext,
                       file_name = fn[ 1 ], extension = fn[ 2 ],
                       sheet_selection = sheet_to_use, skip = rows_to_skip )

  } else if( as.numeric( vn ) == 5 ){

    edgar <- readData( domain, domain_extension = domain_ext,
                       file_name = fn[ 1 ], extension = fn[ 2 ],
                       sheet_selection = sheet_to_use, skip = rows_to_skip,
                       missing_value = c( "", "NULL" ) )

  }

# Read in master country list mapping file
Master_Country_List <- readData( "MAPPINGS", 'Master_Country_List' )

# Read in EDGAR non-combustion sector mapping file
NC_sector_map <- readData( "MAPPINGS", "NC_EDGAR_sector_mapping" )

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

# ------------------------------------------------------------------------------
# 3. Reformatting

# Add iso column and group sector column with it at the end
edgar$iso <- tolower( edgar[ , "ISO_A3" ] )
edgar$edgar_sector <- edgar[ , "IPCC" ]

data_start <- findDataStart( edgar )

# Remove unnecessary columns
len <- ncol( edgar )
edgar <- edgar[ data_start: len ]

# Add fuel column
edgar$fuel <- fuel

# Add ceds_sector column and units from sector mapping file
edgar <- edgar %>%
  dplyr::left_join( NC_sector_map %>% dplyr::select( edgar_sector, ceds_sector ) %>% dplyr::distinct( ),
                    by = "edgar_sector" ) %>%
  dplyr::rename( sector = ceds_sector )

# Define units as kt (as EDGAR data is in Gg, which is the same as kt)
# TODO: TAKE FROM MASTER SECTOR LIST INSTEAD
# TODO: PR comment: using the MFSL results in different units, as they are mostly defined as "1000" in that map,
#                   while currently they are all set to "kt" (commented out below)
# edgar <- edgar %>%
#   dplyr::left_join( MFSL %>% dplyr::select( sector, units ) %>% dplyr::distinct( ),
#                     by = "sector" )

edgar$units <- NC_sector_map$units[ match( edgar$sector, NC_sector_map$ceds_sector ) ]

# Leave out excluded sectors and filter out sectors which did not map to CEDS sectors
edgar <- edgar %>%
  dplyr::filter( sector %!in% excl_sectors ) %>%
  dplyr::filter( !is.na( sector ) )

# Rearrange columns to proper order (iso-sector-fuel-units-data)
len <- ncol( edgar )

edgar <- edgar %>%
  dplyr::select( id_cols, edgar_sector, 1 : ( len - 5 ) ) %>%
  dplyr::arrange( iso, sector, edgar_sector, fuel )

# If EDGAR v4.3 or v5 are being used, add X's to the column names
if( as.numeric( vn ) %in% c( 4.3, 5 ) ){

  len <- ncol ( edgar )
  names( edgar ) <- c( names( edgar[ 1 : 5 ] ), paste0( "X", names( edgar[ 6 : len ] ) ) ) # Add X's to year columns for EDGAR v5

}

edgar <- edgar[ , c( id_cols, 'edgar_sector', X_EDGAR_years ) ]

# Convert data to to classs numeric, from class character
edgar <- edgar %>%
  dplyr::mutate_at( .vars =  X_EDGAR_years,
                    .funs = funs( as.numeric( . ) ) )

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
  dplyr::summarise_all( funs( sum(., na.rm = T ) ) ) %>% # Note: This means if an iso has NA for the multiple EDGAR sectors for the same year,
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
# 4. Extend Fugitive Solid Fuels for methane, if EDGAR emissions do not go the last
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
    dplyr::left_join( BP_coal_production, by = 'BPName' ) %>%
    tidyr::gather( year, value, -iso, -BPName ) %>%
    dplyr::filter( !is.na( value ) ) %>%
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
# 5. Output

# If em is CH4, output extended fugitive solid emissions
if ( em == 'CH4'){

  writeData( fugitive_solid_extended,  domain = "DEFAULT_EF_IN", domain_extension = "non-combustion-emissions/",
             fn = paste0( "C.",em, "_EDGAR_NC_Emissions_fugitive_solid_fuels" ) )

}

# Add EDGAR NC emissions to CEDS default NC emissions database
addToEmissionsDb( edgar, em = em, type = 'NC', ext_backward = FALSE, ext_forward = FALSE )
writeData( edgar, domain = "DIAG_OUT", fn = paste0( "C.EDGAR_NC_Emissions_",em ) )

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
