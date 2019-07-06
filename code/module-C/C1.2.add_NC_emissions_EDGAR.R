# ------------------------------------------------------------------------------
# Program Name: C.1.2.add_NC_emissions_EDGAR.R
# Author(s): Jon Seibert, Rachel Hoesly
# Date Last Modified: 20 April 2017
# Program Purpose: To reformat the non-combustion sections of the EDGAR default emissions
#                      data and add it to the database for the relevant emissions species.
# Input Files:
# Output Files:
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
  if ( is.na( em ) ) em <- "CH4"

  # EDGAR data version number
vn <- "4.2"

# Input domain
domain <- "EM_INV"
domain_ext <- "EDGAR/"

fuel <- "process"
id_cols <- c( "iso", "sector", "fuel", "units" )

# Temporary assignment for script development
#em <- "CO2"

EDGAR42_end_year = 2008

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
Master_Country_List <- readData("MAPPINGS", 'Master_Country_List')

if ( em == 'CH4' ){
  bp_energy_data <- readData( "ENERGY_IN",BP_data_file_name, ".xlsx")
  BP_coal_production_raw <- bp_energy_data[[ getBPSheetNumber( "coal", "production", "tonnes", bp_energy_data ) ]]
  # First coal production data year is 1981
  X_BP_years <- paste0( 'X', 1981:BP_last_year )
  BP_coal_production <- cleanBPDataSheet( BP_coal_production_raw, X_BP_years, Master_Country_List, x_years_flag = FALSE )
}

# ------------------------------------------------------------------------------
# 3. Reformatting

# Add iso column and group sector column with it at the end
edgar$iso <- tolower( edgar[ , "ISO_A3" ] )
edgar$edgar_sector <- edgar[ , "IPCC" ]

data_start <- findDataStart( edgar )

# Remove unnecessary columns
len <- ncol( edgar )
edgar <- edgar[ data_start:len ]

# Add fuel column
edgar$fuel <- fuel

# Add ceds_sector column  and units from sector mapping file
edgar$sector <- NC_sector_map$ceds_sector[ match( edgar$edgar_sector, NC_sector_map$edgar_sector ) ]
# TAKE FROM MASTER SECTOR LIST INSTEAD
edgar$units <- NC_sector_map$units[ match( edgar$sector, NC_sector_map$ceds_sector ) ]

# Remove rows with NA values- interferes with database functions
edgar <- na.omit( edgar )

# Rearrange columns to proper order (iso-sector-fuel-units-data)
len <- ncol( edgar )
edgar <- cbind( edgar[ id_cols ], edgar[ 1:( len - 5 ) ] )

# Sort the data
edgar <- edgar[ with( edgar, order( iso, sector, fuel ) ), ]

# get rid of 2008 and 2009. Strange Values
edgar <- edgar[,c('iso','sector','fuel','units', paste0('X',EDGAR_start_year:EDGAR42_end_year))]

# leave out excluded sectors
  edgar <- filter( edgar, sector %!in% excl_sectors )

# make negative emissions zero
  X_edgar_years <- names( edgar )[ grepl( "X", names( edgar )  ) ]
  neg_rows <- apply( edgar[, X_edgar_years ], 1, function( row ) any( row < 0 ) )
  edgar_neg <- edgar[ neg_rows, ]
  edgar[ edgar < 0 ] <- 0

# filter isos in Master Country List
  edgar <- edgar %>% filter(iso %in% Master_Country_List$iso)
# ------------------------------------------------------------------------------
# 4. Extend Fugitive Solid Fuels for methane

if ( em == 'CH4' ){

  # seperate fugitive solid fuels and other emissions
  fugitive_solid <- edgar %>% filter(sector == '1B1_Fugitive-solid-fuels')
  edgar <- edgar %>% filter(sector != '1B1_Fugitive-solid-fuels')

  # process BP data for extension
  bp <- Master_Country_List %>%
    select(iso, BPName) %>%
    filter(!is.na(BPName), BPName != 'ussr') %>%
    unique() %>%
    filter(!duplicated(iso)) %>%
    unique() %>%
    left_join( BP_coal_production, by = 'BPName') %>%
    gather(year, value, -iso,-BPName) %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    filter(!is.na(year), !is.na(value)) %>%
    dplyr::mutate(year = paste0('X',year)) %>%
    dplyr::mutate(value = as.numeric(value)) %>%
    spread(year, value) %>%
    select(-BPName)

  fugitive_solid_extended <- extend_data_on_trend_range(driver_trend = bp,
                                                        input_data = fugitive_solid,
                                                        start = 2009, end = BP_last_year,
                                                        ratio_start_year = 2007,
                                                        expand = T,
                                                        range = 2,
                                                        id_match.driver = c('iso'))
  fugitive_solid_extended <- fugitive_solid_extended[ , c('iso','sector','fuel','units',paste0('X',1971:BP_last_year) ) ]

}

# ------------------------------------------------------------------------------
# 5. Output

if ( em == 'CH4'){

  writeData( fugitive_solid_extended,  domain = "DEFAULT_EF_IN", domain_extension = "non-combustion-emissions/",
             fn = paste0( "C.",em, "_EDGAR_NC_Emissions_fugitive_solid_fuels" ) )

}
addToEmissionsDb( edgar, em = em, type = 'NC', ext_backward = FALSE, ext_forward = FALSE )
writeData( edgar, domain = "DIAG_OUT", fn = paste0( "C.EDGAR_NC_Emissions_",em ) )

if ( nrow( edgar_neg ) > 0 )
  writeData( edgar_neg, domain = "DIAG_OUT", fn = paste0( "C.EDGAR_NC_Emissions_",em, "_negative" ) )


logStop()
# END
