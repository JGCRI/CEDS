# ------------------------------------------------------------------------------
# Program Name: C.1.2.add_NC_emissions_EDGAR.R
# Author(s): Jon Seibert, Rachel Hoesly, Patrick O'Rourke
# Date Last Modified: 4 April 2019
# Program Purpose: To reformat the non-combustion sections of the EDGAR default emissions
#                      data and add it to the database for the relevant emissions species.
# Input Files: NC_EDGAR_sector_mapping.csv, BP_energy_data.xlsx, Master_Country_List.csv,
#              EDGAR42_[em].csv
# Output Files: C.CH4_EDGAR_NC_Emissions_fugitive_solid_fuels.csv,
#               C.EDGAR_NC_Emissions_[em].csv, C.EDGAR_NC_Emissions_[em]_negative.csv
# To Do:
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
    log_msg <- paste0( "Processing EDGAR non-combustion default emissions data." ) # First message to be printed to the log
    script_name <- "C.1.2.add_NC_emissions_EDGAR.R"
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------

# 1. Settings

# Define emissions species variable
  args_from_makefile <- commandArgs( TRUE )
  em <- args_from_makefile[ 1 ]
  if ( is.na( em ) ) em <- "N2O"

# EDGAR data version number
vn <- "4.2"

# Input domain
domain <- "EM_INV"
domain_ext <- "EDGAR/"

fuel <- "process"
id_cols <- c( "iso", "sector", "fuel", "units" )

EDGAR42_end_year = 2008
EDGAR_years_keep <- paste0( "X", 1970:2008 )

# Define sectors that should not use EDGAR (also have to modify C2.1.base_NC_EF.R)
excl_sectors <- c()
    if (em == "CO2") {
        excl_sectors <- c( excl_sectors, "2A1_Cement-production", "3D_Soil-emissions" )
    }

# ------------------------------------------------------------------------------
# 2. Input

# Determine full file name and path
fn <- c( paste0( "EDGAR", gsub( "[.]", "", vn ), "_", em  ), ".csv" )

NC_sector_map <- readData( "MAPPINGS", "NC_EDGAR_sector_mapping" )
edgar <- readData( domain, fn[[ 1 ]], fn[[ 2 ]], domain_extension = domain_ext )

if ( em == 'CH4' ){
  BP_energy_data <- readData( 'ENERGY_IN', file_name = 'BP_energy_data', extension = ".xlsx",
                              sheet_selection = 'Coal Production - Tonnes', skip_rows = 2 )
  names(BP_energy_data)[1] <- 'BPName'

}

Master_Country_List <- readData("MAPPINGS", 'Master_Country_List')

# ------------------------------------------------------------------------------
# 3. Initial Reformatting

# Add iso column and group sector column with it at the end
edgar$iso <- tolower( edgar[ , "ISO_A3" ] )
edgar$edgar_sector <- edgar[ , "IPCC" ]

data_start <- findDataStart( edgar )

# Remove unnecessary columns
len <- ncol( edgar )
edgar <- edgar[ data_start:len ]

# Add fuel column
edgar$fuel <- fuel

# ------------------------------------------------------------------------------
# 4. Account for EDGAR 4D3 Indirect N2O from agriculture - for N2O only

if ( em == 'N2O' ){

# Subset EDGAR 4D3 Indirect N2O from agriculture, as this will be split to multiple
# CEDS sectors later
edgar_4D3_data <- edgar %>%
    dplyr::filter( edgar_sector == "4D3" ) %>%
    dplyr::select( iso, edgar_sector, fuel, EDGAR_years_keep ) %>%
    tidyr::gather( key = Year, value = Emissions_4D3, EDGAR_years_keep ) %>%
    dplyr::select( -edgar_sector )

edgar <- edgar %>%
    dplyr::filter( edgar_sector != "4D3" )

# Add ceds_sector column from sector mapping file
edgar$sector <- NC_sector_map$ceds_sector[ match( edgar$edgar_sector, NC_sector_map$edgar_sector ) ]

# Add back "4D3 - Indirect N2O from agriculture" by splitting it among CEDS sectors "3B_Manure-management"
#       and "3D_Soil-emissions", based on the relative amount of emissions between these two sectors
#       for each iso

#   Subset EDGAR data for 3B and 3D
    edgar_3Band3D <- edgar %>%
        dplyr::filter( sector %in% c( "3B_Manure-management", "3D_Soil-emissions" ) ) %>%
        dplyr::select( iso, edgar_sector, sector, fuel, EDGAR_years_keep ) %>%
        tidyr::gather( key = Year, value = Emissions, EDGAR_years_keep )

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
        tidyr::gather( key = Year, value = Emissions, EDGAR_years_keep ) %>%
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
        dplyr::select( EDGAR_years_keep, iso, edgar_sector, fuel, sector )

} else{

#   Add ceds_sector column from sector mapping file
    edgar$sector <- NC_sector_map$ceds_sector[ match( edgar$edgar_sector, NC_sector_map$edgar_sector ) ]
}

# ------------------------------------------------------------------------------

# 5. Finish processing EDGAR data

# Add units from sector mapping file
# docTODO: TAKE FROM MASTER SECTOR LIST INSTEAD
edgar$units <- NC_sector_map$units[ match( edgar$sector, NC_sector_map$ceds_sector ) ]

# Remove rows with NA values- interferes with database functions
edgar <- na.omit( edgar )

# Rearrange columns to proper order (iso-sector-fuel-units-data)
edgar <- dplyr::select( edgar, iso, sector, fuel, units, EDGAR_years_keep )

# Sort the data
edgar <- edgar[ with( edgar, order( iso, sector, fuel ) ), ]

# Get rid of 2008 and 2009. Strange Values
#     ***** docTODO: This doesn't get rid of any values though?
edgar <- edgar[,c('iso','sector','fuel','units', paste0('X',EDGAR_start_year:EDGAR42_end_year))]

# Leave out excluded sectors
  edgar <- filter( edgar, sector %!in% excl_sectors )

# make negative emissions zero
  X_edgar_years <- names( edgar )[ grepl( "X", names( edgar )  ) ]
  neg_rows <- apply( edgar[, X_edgar_years ], 1, function( row ) any( row < 0 ) )
  edgar_neg <- edgar[ neg_rows, ]
  edgar[ edgar < 0 ] <- 0

# Filter isos in Master Country List
  edgar <- edgar %>% filter(iso %in% Master_Country_List$iso)

# ------------------------------------------------------------------------------
# 6. Extend Fugitive Solid Fuels for methane

if ( em == 'CH4' ){

  # Seperate fugitive solid fuels and other emissions
  fugitive_solid <- edgar %>% filter(sector == '1B1_Fugitive-solid-fuels')
  edgar <- edgar %>% filter(sector != '1B1_Fugitive-solid-fuels')

  # Process BP data for extension
  bp <- Master_Country_List %>%
    select(iso, BPName) %>%
    filter(!is.na(BPName), BPName != 'ussr') %>%
    unique() %>%
    filter(!duplicated(iso)) %>%
    unique() %>%
    left_join( BP_energy_data, by = 'BPName') %>%
    gather(year, value, -iso,-BPName) %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    filter(!is.na(year), !is.na(value)) %>%
    dplyr::mutate(year = paste0('X',year)) %>%
    dplyr::mutate(value = as.numeric(value)) %>%
    spread(year, value) %>%
    select(-BPName)

  fugitive_solid_extended <- extend_data_on_trend_range(driver_trend = bp,
                                                        input_data = fugitive_solid,
                                                        start = 2009, end = 2014,
                                                        ratio_start_year = 2007,
                                                        expand = T,
                                                        range = 2,
                                                        id_match.driver = c('iso'))
  fugitive_solid_extended <- fugitive_solid_extended[ , c('iso','sector','fuel','units',paste0('X',1971:2014) ) ]

}

# ------------------------------------------------------------------------------
# 7. For N2O, aggregate CEDS sectors, as multile EDGAR sectors mapped to 1 CEDS
#    sector (such as EDGAR sectors 4B and 4D2 mapping to CEDS 3B_Manure-management )
#    Note: This is done because addToEmissionsDb expects there to only be 1 value
#          for each CEDS sector (for each CEDS sec, fuel, and iso combination),
#          thus without doing this addToEmissionsDb would only copy the first value for
#          a CEDS sector, fuel, iso combination, when there could be more than 1
#    docTODO: It should be checked to see if this needs to occur for other ems in this script
#             as well as the Module C EDGAR PEGASOS script for any ems
if ( em == 'N2O' ){

    edgar <- edgar %>%
        dplyr::group_by( iso, sector, fuel, units ) %>%
        summarize_all( funs( sum(., na.rm = TRUE ) ) )

} else {

    edgar <- edgar
}

# ------------------------------------------------------------------------------

# 8. Output

if ( em == 'CH4' ){

  writeData( fugitive_solid_extended,  domain = "DEFAULT_EF_IN", domain_extension = "non-combustion-emissions/",
             fn = paste0( "C.",em, "_EDGAR_NC_Emissions_fugitive_solid_fuels" ) )

}

addToEmissionsDb( edgar, em = em, type = 'NC', ext_backward = FALSE, ext_forward = FALSE )
writeData( edgar, domain = "DIAG_OUT", fn = paste0( "C.EDGAR_NC_Emissions_",em ) )

if ( nrow( edgar_neg ) > 0 )
  writeData( edgar_neg, domain = "DIAG_OUT", fn = paste0( "C.EDGAR_NC_Emissions_",em, "_negative" ) )


logStop()
# END
