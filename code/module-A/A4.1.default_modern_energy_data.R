#------------------------------------------------------------------------------
# Program Name: A4.1.default_modern_energy_data.R
# Author(s): Jon Seibert, Linh Vu, Rachel Hoesly
# Date Last Modified: December 9, 2019
# Program Purpose: To expand IEA_BP energy data to include entries for all possible
#                  id combinations.
# Input Files: A.IEA_BP_energy_ext.csv, Master_Fuel_Sector_List.xlsx,
#              Master_Country_List.csv,
# Output Files: A.default_comb_activity_with_other.csv, A.Other_transformation_fuel.csv,
#               A.total_agg_fuel.csv, A.other_IEA_energy_values.csv
# Notes:
# TODO:
#-------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if( "input" %in% dir( ) ) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "analysis_functions.R" ) # Additional function files required.
    log_msg <- "Completion of all combustion data entries" # First message to be printed to the log
    script_name <- "A4.1.default_modern_energy_data.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Read in files

    energy_data <- readData( "MED_OUT", "A.IEA_BP_energy_ext" )

    MFL <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Fuels" )
    MSL <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Sectors" )
    MCL <- readData( "MAPPINGS", "Master_Country_List" )

# ------------------------------------------------------------------------------
# 2. Separate combustion and activity energy data, separate other transformation

# Check to make sure all sectors are in the input data
    check_sectors <- c(MSL[ which( MSL$activity %in% c( 'Energy_Combustion' ) ), 'sector' ],
                       '1A1bc_Other-transformation','1A1bc_Other-feedstocks' )

  if( any( check_sectors %!in% unique( energy_data$sector ) ) ) {
      missing_sectors <- check_sectors [ check_sectors %!in% unique( energy_data$sector ) ]
      stop( paste( 'Energy Data does not have all sectors. The following are missing: ', paste( missing_sectors, collapse=", " ) ) )
  }

# Separate CEDS combustion and other transformation from other tracked
#    IEA energy trends (ex: domestic supply)
    IEA_other_energy_trends <- energy_data %>%
        dplyr::filter( sector %!in% MSL$sector ) %>%
        dplyr::mutate_at( grep( 'X\\d{4}', names( energy_data ) ),
                   funs( if_else( sector == 'refinery-and-natural-gas' & . < 0, 0, . ) ) )

    other_transformation <- energy_data %>%
      dplyr::filter( sector %in% c( "1A1bc_Other-feedstocks", "1A1bc_Other-transformation" ) )

    energy_data <- energy_data %>%
      dplyr::filter( sector %in% MSL$sector )

    energy_data_combustion <- energy_data[ energy_data$fuel != 'process', ]

# ------------------------------------------------------------------------------
# 3. Create combustion activity database - Populate missing iso-sector-fuel combinations

# Build empty energy data database with all combinations

# Whether these lists include biomass and/or process sectors is easily editable-
# left out for now via reasonable assumption.
    iso_list <- sort( unique( MCL$iso[ !grepl( "historical",  MCL$note ) ] ) )
    sector_list <- unique( c( MSL$sector[ MSL$activity == "Energy_Combustion" ],
                              unique( energy_data_combustion$sector[
                                      energy_data_combustion$sector != "NA" ] ) ) )

# fuel_list <- unique( MFL$fuel[ MFL$fuel != "process" & MFL$fuel != "biomass" ] )
    fuel_list <- unique( MFL$fuel[ MFL$fuel != "process" ] )

# Remove NA-sector biomass entries for now
    energy_data_combustion <-
        na.omit( energy_data_combustion[ energy_data_combustion$sector != "NA", ] )

# Use header function to generate blank template data frame
    template <- buildCEDSTemplate( iso_list,
                                   sector_list[sector_list != "1A1bc_Other-transformation"],
                                   fuel_list ) %>%
        bind_rows( expand.grid( iso = iso_list,
                               sector = "1A1bc_Other-transformation",
                               fuel = c( 'oil','natural_gas','hard_coal','brown_coal' ),
                               units = 'kt' ) )

# Insert existing data into blank template
    full_energy_data_combustion <- merge( template,    ### Could use a dplyr join instead
                                          energy_data_combustion,
                                          by = c( "iso", "sector", "fuel" ),
                                          all.x = TRUE,
                                          suffixes = c( ".t", ".e" ) )

    original_names <- c( "iso", "sector", "fuel", "units", X_emissions_years )
    merge_names <- c( "iso", "sector", "fuel", "units.t", paste0( X_emissions_years, ".e" ) )

# Extract energy data columns and rename
    full_energy_data_combustion <- full_energy_data_combustion[ merge_names ]
    names( full_energy_data_combustion ) <- original_names

# Replace merge-generated NAs with 0
    full_energy_data_combustion[ is.na( full_energy_data_combustion ) ] <- 0

# Sort
    full_energy_data_combustion <-
        full_energy_data_combustion[ with( full_energy_data_combustion,
                                           order( iso, sector, fuel ) ), ]
# ------------------------------------------------------------------------------
# 4. Check for NAs in energy combustion data, remove other transformation and feedstocks

    if( anyNA( full_energy_data_combustion ) ) stop( 'NAs in final energy data, please check.' )

    energy_data_combustion_no_other <- full_energy_data_combustion %>%
        dplyr::filter( sector %!in% c( "1A1bc_Other-feedstocks", "1A1bc_Other-transformation" ) )


# ------------------------------------------------------------------------------
# 4. Aggregate fuel summary
    other_transformation_aggregate <- other_transformation %>%
        dplyr::mutate( fuel = replace( fuel, fuel == 'hard_coal', 'coal' ) ) %>%
        dplyr::mutate( fuel = replace( fuel, fuel == 'brown_coal', 'coal' ) ) %>%
        dplyr::mutate( fuel = replace( fuel, fuel == 'natural_gas', 'gas' ) ) %>%
        # dplyr::mutate(fuel = replace(fuel, fuel == 'petroleum', 'oil')) %>%
        dplyr::arrange( iso, sector, fuel ) %>%
        dplyr::group_by( iso, fuel, sector, units ) %>%
        dplyr::summarize_all( funs( sum ) )

    # Add aggregate oil category to MFL so can aggreate correctly below
    new_oil_row <- cbind("oil","oil","","aggreagte oil") %>%
        as.data.frame( )
    names_for_oil <- colnames( MFL )
    names( new_oil_row ) = names_for_oil
    MFL <- rbind( MFL, new_oil_row )

    total_fuel_consumption <- full_energy_data_combustion %>%
        dplyr::left_join( MFL[ c( 'fuel','aggregated_fuel' ) ], by = "fuel" ) %>%
        dplyr::select( -sector ) %>%
        dplyr::select( -fuel ) %>%
        dplyr::group_by(iso, aggregated_fuel, units ) %>%
        dplyr::summarize_all( funs( sum ) )

# ------------------------------------------------------------------------------
# 5. Output
# Add comments for each table
    comments.A.default_comb_activity <- c( paste0( "IEA energy statistics",
            " by intermediate sector / intermediate fuel / historical year,",
            " extended with BP data and filled out with all combustion",
            " iso-sector-fuel combinations." ) )
    comments.A.other_IEA_energy_values <- c( paste0( "IEA energy statistics values that are not",
                                                 " specific energy trends by sector/fuel (ie domestic supply),",
                                                 " extended with BP data and filled out with all combustion",
                                                 " iso-sector-fuel combinations." ) )
# write out data

    writeData( full_energy_data_combustion, domain = "MED_OUT",
               fn = "A.default_comb_activity_with_other", comments = comments.A.default_comb_activity )

    writeData( other_transformation, domain = "MED_OUT",
               fn = "A.Other_transformation_fuel" )

    # A.total_agg_fuel contains total consumption of each fuel, including other transformation and feedstocks for reference
    writeData( total_fuel_consumption, domain = "MED_OUT",
               fn = "A.total_agg_fuel" )

    writeData( IEA_other_energy_trends, domain = "MED_OUT",
               fn = "A.other_IEA_energy_values",
               comments = comments.A.other_IEA_energy_values )

##CR: just in general we need a consistent naming convention for files wrt capitalization

    logStop( )

# END
