#------------------------------------------------------------------------------
# Program Name: C1.3.proc_NC_emissions_user_added_inventories.R
# Author(s): Rachel Hoesly
# Date Last Modified: August 19, 2015
# Program Purpose: To fill out missing sections in the process emissions database
# Input Files: C.[em]_NC_emissions.csv
# Output Files:  C.[em]_NC_emissions.csv
# Notes:
# TODO:
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "process_db_functions.R",
              'interpolation_extension_functions.R' ) # Additional function files required.
log_msg <- "Integration of inventory process emissions data with insturctions from user" # First message to be printed to the log
script_name <- "C1.3.proc_NC_emissions_user_added_inventories.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"

# ------------------------------------------------------------------------------
# 1. Read in files

  MSL <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Sectors" )
  instructions <- readData( domain = "DEFAULT_EF_IN" , domain_extension = "non-combustion-emissions/",
                            'add_inventory_instructions')

# ---------------------------------------------------------------------------
# 1.
  instructions <- instructions[which( instructions$em == em),]

  if ( nrow(instructions) > 0 ){

  # Read in inventory files

  files_list <- unique(instructions$inv)
  inv_list <- lapply ( X = files_list, FUN = readData,
                             domain = "MED_OUT" )
  names(inv_list) <- files_list
# ---------------------------------------------------------------------------
# 2. Interpolate, convert list to one df
  process_sectors <- MSL[which(MSL$type == 'NC'),'sector']

  replacement_data <- c( )
  combustion_sectors <- c( )
  for (i in seq_along( names ( inv_list ) ) ) {
    inv_name <- names(inv_list)[ i ]
    inv_data <- inv_list[[ i ]]
    inv_instructions <- instructions[which(instructions$inv == inv_name) , ]

    # Get rid of any blank columns
    inv_data$X <- NULL

    years <- names(inv_data)[grep('X',names(inv_data))]

    replace_inv_data <- merge( inv_instructions[,c('iso', 'inv_sector' , 'ceds_sector' ) ],
                               inv_data, by.x = c( 'iso', 'inv_sector' ),
                               by.y = c( 'iso','sector' ),
                               all.x = TRUE, all.y = FALSE)

   # Deal with case where units column not present
    if ( !( "units" %in% names( replace_inv_data ) ) ) replace_inv_data$units <- 'kt'

    replace_inv_data <- replace_inv_data[ !is.na( replace_inv_data$units ) , ]
    replace_inv_data$inv_sector <- NULL

    names( replace_inv_data )[ which(names( replace_inv_data ) == 'ceds_sector' )  ] <- 'sector'

    #check for combustion sectors
    combustion_sectors <- rbind.fill(combustion_sectors ,
                                     replace_inv_data[ which( replace_inv_data$sector %!in% process_sectors ) , ] )

    replace_inv_data <- replace_inv_data[ which( replace_inv_data$sector %in% process_sectors ) , ]

    # Check that still have valid data
    if ( nrow( replace_inv_data ) > 0 ) {
      # add fuel - process
      replace_inv_data$fuel <- 'process'
      replace_inv_data <- replace_inv_data[ , c('iso','sector','fuel','units', years )]

      # interpolate
      replace_inv_data[ years ] <- interpolateValues( replace_inv_data[ years ]  )

      replacement_data <- rbind.fill( replacement_data, replace_inv_data )
    }
  }

  if( nrow (combustion_sectors) > 0 ){

    printLog(paste('Combustion sectors specified in instructions file for adding inventory data to process emissions.',
                   'Please check file. Combustion data selected printed to diagnostic-output'))
    writeData(combustion_sectors, 'DIAG_OUT',paste0('C.',em,'combustion_data_added_to_process_emissions_inventories'),
              meta=F)
  }


# write out to process data folder
  writeData(replacement_data, 'DEFAULT_EF_IN', domain_extension = 'non-combustion-emissions/',
            paste0('C.',em,'_NC_inventory_emissions_user_added'))

  } # end logic if no user added inventory instructions

  logStop()
# END
