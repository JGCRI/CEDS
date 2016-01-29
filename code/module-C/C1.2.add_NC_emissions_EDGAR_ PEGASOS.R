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

# Before we can load headers we need some paths defined. They may be provided by
#   a system environment variable or may have already been set in the workspace.
# Set variable PARAM_DIR to be the data system directory
    dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
    for ( i in 1:length( dirs ) ) {
      setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
      wd <- grep( 'CEDS/input', list.dirs(), value = T )
      if ( length( wd ) > 0 ) {
        setwd( wd[ 1 ] )
        break
      }
    }
    PARAM_DIR <- "../code/parameters/"
# Universal header file - provides logging, file support, etc.

    headers <- c( "common_data.R","data_functions.R", "analysis_functions.R", 
                  "process_db_functions.R", 'timeframe_functions.R') # Additional function files required.
    log_msg <- paste0( "Processing EDGAR non-combustion default emissions data." ) # First message to be printed to the log
    script_name <- "C.1.2.add_NC_emissions_EDGAR.R" 
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
# em <- "SO2"

# ------------------------------------------------------------------------------
# 2. Input

inventory_data_file <- paste0('JRC_PEGASOS_',em,'_TS_REF')
sheet_name = paste0( 'NEW_v4.3_EM_', em, '_hind' )

edgar <-  readData( domain, domain_extension = "Edgar/",
				    inventory_data_file,  ".xlsx", 
					sheet_selection = sheet_name, skip_rows = 8 ) 

NC_sector_map <- readData( "MAPPINGS", "NC_EDGAR_sector_mapping" )

# ------------------------------------------------------------------------------
# 3. Reformatting

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
edgar$sector <- NC_sector_map$ceds_sector[ match( edgar$edgar_sector, NC_sector_map$edgar_sector ) ]
# TAKE FROM MASTER SECTOR LIST INSTEAD
#edgar$units <- NC_sector_map$units[ match( edgar$sector, NC_sector_map$ceds_sector ) ]

# Remove rows with NA values- interferes with database functions
# edgar <- na.omit( edgar )
# sjs; Don't want to do this. Hopefully will still work.

# Rearrange columns to proper order (iso-sector-fuel-units-data)
#len <- ncol( edgar )
edgar <- cbind( edgar[ id_cols ], edgar[ 1:( len - 5 ) ] )
# duplicates instead of re-orders

# Sort the data
edgar <- edgar[ with( edgar, order( iso, sector, fuel ) ), ]

# get rid of any values outside of main data range.
edgar <- edgar[,c('iso','sector','fuel','units', paste0('X',EDGAR_start_year:EDGAR_end_year))]

# ------------------------------------------------------------------------------
# 4. Output
addToEmissionsDb( edgar, em = em, type = 'NC', ext_backward = TRUE, ext_forward = TRUE )
  
writeData( edgar, domain = "DIAG_OUT", fn = "C.EDGAR_test")
 
logStop()
# END