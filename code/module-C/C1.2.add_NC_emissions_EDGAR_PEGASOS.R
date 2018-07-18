# ------------------------------------------------------------------------------
# Program Name: C.1.2.add_NC_emissions_EDGAR.R
# Author(s): Jon Seibert
# Date Last Modified: 5 January 2016
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
    script_name <- "C1.2.add_NC_emissions_EDGAR_PEGASOS.R"
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Settings ( "em" already set to correct species by parent script )

# EDGAR data version number
vn <- "4.2"

# Input domain
domain <- "EM_INV"
domain_ext <- "EDGAR/"

fuel <- "process"
id_cols <- c( "iso", "sector", "fuel", "units" )

# Temporary assignment for script development
# em <- "NH3"

# ------------------------------------------------------------------------------
# 2. Input

inventory_data_file <- paste0('JRC_PEGASOS_',em,'_TS_REF')
sheet_name = paste0( 'NEW_v4.3_EM_', em, '_ref' )

edgar <-  readData( domain, domain_extension = domain_ext,
				    inventory_data_file,  ".xlsx",
					sheet_selection = sheet_name, skip = 8 )

NC_sector_map <- readData( "MAPPINGS", "NC_EDGAR_sector_mapping" )

# ------------------------------------------------------------------------------
# 3. Reformatting

inv_years<-c(EDGAR_start_year:EDGAR_end_year)

# Clean rows and columns to standard format
edgar$units <- 'kt'
edgar <- edgar[,c('ISO_A3','IPCC','units', inv_years ) ]
names(edgar) <- c('iso','sector', 'units', paste0('X',inv_years))
edgar$iso <- tolower(edgar$iso)

#remove rows with all NA's
edgar <- edgar[ apply( X=edgar[,paste0("X",inv_years)],
                                         MARGIN = 1, function(x) (!all(is.na(x))) ) ,]


# Add fuel column
edgar$fuel <- fuel

# Add ceds_sector column  and units from sector mapping file
edgar$sector <- NC_sector_map$ceds_sector[ match( edgar$sector, NC_sector_map$edgar_sector ) ]

#Aggregate to CEDS sectors
# Total Emissions by Sector and Country
edgar <- aggregate( edgar[ paste0("X",inv_years) ],
                               by=list(iso = edgar$iso,
                                       sector = edgar$sector,
                                       units = edgar$units,
                                       fuel = edgar$fuel ), sum )

# Turn NAs to zeros
edgar[ is.na( edgar ) ] <- 0

# Rearrange columns to proper order (iso-sector-fuel-units-data)
len <- ncol( edgar )
edgar <- cbind( edgar[ id_cols ], edgar[ paste0("X",inv_years) ] )
# duplicates instead of re-orders

# Sort the data
edgar <- edgar[ with( edgar, order( iso, sector, fuel ) ), ]

# get rid of any values outside of main data range.
edgar <- edgar[,c('iso','sector','fuel','units', paste0('X',EDGAR_start_year:EDGAR_end_year))]

# ------------------------------------------------------------------------------
# 4. Output
addToEmissionsDb( edgar, em = em, type = 'NC', ext_backward = FALSE, ext_forward = FALSE )

writeData( edgar, domain = "DIAG_OUT", fn = paste0( "C.EDGAR_NC_Emissions_",em ) )

logStop()
# END
