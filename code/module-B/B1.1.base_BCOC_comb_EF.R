#------------------------------------------------------------------------------
# Program Name: B1.1.base_BCOC_comb_EF.R
# Author: Rachel Hoesly, Linh Vu, Patrick O'Rourke
# Date Last Updated: 10 May 2019
# Program Purpose: 1. Produce BC and OC emissions factors from SPEW (i.e. Bond) data.
# Input Files:  A.comb_activity.csv
#               CD.SPEW_iso_map.csv
#               CD.[em]_bond_country_sector_fuel_2001.csv
#               CD.[em]_bond_EF_region.csv
#               CD.[em]_bond_EF_agg_sector_by_region.csv
#               CD.[em]_bond_EF_region_fuel.csv
#               CD.[em]_bond_EF_fuel_agg_sector.csv
#               CD.[em]_bond_EF_fuel.csv
#               CD.[em]_bond_EF_residential_biomass.csv
#               CD.[em]_bond_EF_country.csv
# Output Files: B.[em]_comb_EF_db.csv
#               B.[em]_SPEW_comb_EF.csv
# Notes: Emission factors (ef) are calculated as emissions divided by
#        consumption. Missing (zero or NA) ef are replaced using the following
#        rules, in order:
#          a. FSU residential coal-oil-gas replaced with FSU industrial coal oil gas
#          b. Resdiential biomass replaced with iso sector fuel where available
#          c. Others replaced with Region sector fuel EF where available
#          d. Then replace with region fuel average
#          e. Then replace with global sector fuel average
#          f. Then replace with global fuel average
#
# ------------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "analysis_functions.R", "process_db_functions.R",
                  "interpolation_extension_functions.R" )
    log_msg <- "Produce OC BC emissions factors from SPEW data" # First message to be printed to the log
    script_name <- "B1.1.base_BCOC_comb_EF.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "OC"

# ------------------------------------------------------------------------------
# 0.5 Define functions for later use
    loadPackage( 'zoo' )

# interpolate_extend()
# This function is used to
    interpolate_extend <- function ( df ) {

      years <- names( df )[ grep( 'X', names( df ) ) ]
      interpolate <-  apply( X = df[ years ], MARGIN = 1,
                             FUN = function( x )
                               any( is.na( na.trim( x ) ) ) )
      row.all.na <- apply( X = df[ years ],
                           MARGIN = 1 ,
                           FUN = all.na )

      # interpolate, constant extend forward and back
      df[ interpolate, years ] <- t( na.approx( t( df[ interpolate, years ] ),
                                                na.rm = FALSE ) )
      df[ , years ] <-t( na.locf( t( df[ , years ] ),
                                  na.rm = FALSE ) )
      df[ , years ] <- t( na.locf( t( df[ , years ] ),
                                   fromLast = TRUE,
                                   na.rm = FALSE ) )
      return( df )
    }

# ------------------------------------------------------------------------------
# 1. Read in files and do preliminary setup

# Read in combustion activity data
    activity_data <- readData( "MED_OUT", "A.comb_activity" )

# Read in mapping files
    MCL <-     readData( "MAPPINGS", "Master_Country_List" )
    MSLevel <- readData( "MAPPINGS", "Master_Sector_Level_map" )
    MSL <-     readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx",
                         sheet_selection = "Sectors" , meta = F )

# Read in Bond data and region mapping
    bond_remove_values <- readData( "DEFAULT_EF_IN", paste0("CD.",em,"_bond_country_sector_fuel_2001" ) )
    bond_EF_region <- readData( "DEFAULT_EF_IN",  paste0("CD.",em, "_bond_EF_region" ) )
    bond_EF_agg_sector <- readData( "DEFAULT_EF_IN", paste0("CD.",em, "_bond_EF_agg_sector_by_region" ) )
    bond_EF_region_fuel <- readData( "DEFAULT_EF_IN", paste0("CD.",em, "_bond_EF_region_fuel" ) )
    bond_EF_fuel_agg_sector <- readData( "DEFAULT_EF_IN", paste0("CD.",em, "_bond_EF_fuel_agg_sector" ) )
    bond_EF_fuel <- readData( "DEFAULT_EF_IN", paste0("CD.",em, "_bond_EF_fuel" ) )
    EF_residential_biomass <- readData( "DEFAULT_EF_IN", paste0("CD.",em, "_bond_EF_residential_biomass" ) )
    bond_EF_country <- readData( "DEFAULT_EF_IN",  paste0("CD.",em,"_bond_EF_country") )

    bond_iso_map <-    readData( "MAPPINGS", "CD.SPEW_iso_map")

# ------------------------------------------------------------------------------
# 2. Map to CEDS sectors and countries
#    This block of code adds emissions factors calculated at the various levels
#    handled above. Rows are sequentially inserted by specificity priority; for example,
#    all rows with data at the regional level get that data, then all rows at the
#    aggregate sector level, etc

# Create year sequences for handling input data
    Xyears_full <- paste0( "X", 1850:end_year )
    Xyears <- names( bond_EF_country )[ grep( "X",
                                              names( bond_EF_country ) ) ] %>%
        sort()

# Make EF template by copying info columns of activity data
    ef_template <- activity_data[ , c( 'iso', 'sector', 'fuel' ) ]
    ef_template$units <- 'kt/kt'
    ef_template[ Xyears ] <- NA

# Map Region, sector information to the EF template
    ef_template$Region <- bond_iso_map[ match( ef_template$iso,
                                               bond_iso_map$iso ), 'Region' ]
    ef_template$agg_sector <- MSLevel[ match( ef_template$sector,
                                              MSLevel$working_sectors_v1 ),
                                       "aggregate_sectors" ]

# Make natural gas ef = 0
    ef_template[ which( ef_template$fuel == 'natural_gas' ), Xyears ] <- 0

# Calculate the number of rows in the EF template
    nrow_all <- nrow( ef_template )

# Add EFs for residential biomass by iso
    EF <- replaceValueColMatch( ef_template, EF_residential_biomass,
                                x.ColName = Xyears,
                                match.x = c( 'iso', 'sector', 'fuel' ),
                                addEntries = FALSE )

# Subset data which does or does not have 1960 data
    EF_nas <- EF[ is.na( EF$X1960 ), ]
    EF_final <- EF[ !is.na( EF$X1960 ), ]

# Add EFs by region, sector, fuel to those which didn't have 1960 data
    EF <- replaceValueColMatch( EF_nas, bond_EF_region,
                                x.ColName = Xyears,
                                match.x = c( 'Region', 'sector', 'fuel' ),
                                addEntries = FALSE )

# Subset those rows that STILL don't have 1960 data and add the rest back in
    EF_nas <- EF[ is.na( EF$X1960 ), ]
    EF_final <- rbind( EF_final , EF[ !is.na( EF$X1960 ), ] )

# Add EFs by region, aggregate Sector,fuel
    EF <- replaceValueColMatch( EF_nas, bond_EF_agg_sector,
                                x.ColName = Xyears,
                                match.x = c( 'Region', 'agg_sector', 'fuel' ),
                                addEntries = FALSE )

# Subset those rows that STILL don't have 1960 data and add the rest back in
    EF_nas <- EF[ is.na( EF$X1960 ), ]
    EF_final <- rbind( EF_final , EF[ !is.na( EF$X1960 ), ] )


# Add EFs by region fuel
    EF <- replaceValueColMatch( EF_nas , bond_EF_region_fuel,
                                x.ColName = Xyears,
                                match.x = c( 'Region', 'fuel' ),
                                addEntries = FALSE )

# Subset those rows that STILL don't have 1960 data and add the rest back in
    EF_nas <- EF[ is.na( EF$X1960 ), ]
    EF_final <- rbind( EF_final, EF[ !is.na( EF$X1960 ), ] )

# Add EFs by fuel, aggregate sector
    EF <- replaceValueColMatch( EF_nas, bond_EF_fuel_agg_sector,
                                x.ColName = Xyears,
                                match.x = c( 'agg_sector', 'fuel' ),
                                addEntries = FALSE )

# Subset those rows that STILL don't have 1960 data and add the rest back in
    EF_nas <- EF[ is.na( EF$X1960 ), ]
    EF_final <- rbind( EF_final, EF[ !is.na( EF$X1960 ), ] )

# Add EFs by global fuel average
    EF <- EF_nas
    EF[ Xyears ] <- bond_EF_fuel[ match( EF$fuel, bond_EF_fuel$fuel ), Xyears ]

    EF_nas <- EF[ is.na( EF$X1960 ), ]
    EF_final <- rbind( EF_final, EF[ !is.na( EF$X1960 ), ] )

# ------------------------------------------------------------------------------
# 3. Final Processing
#    Create the final output dataframe by reordering and interpolating EF_final

# If there are still NAs, or if some rows were dropped during the data addition
# process, throw an error.
    if ( nrow( EF_nas ) > 0 |
         nrow( EF_final ) != nrow_all )
        stop( 'NAs in BC-OC efs. Please check code.' )

# Reorder emissions factors and move to final data frame
    EF_final <- EF_final[ with( EF_final, order( iso, sector, fuel ) ), ]
    final_full <- EF_final
    final_full$units <- 'kt/kt'

# Prepare to interpolate by ensuring that all non-data years are NAs
    final_full[ , Xyears_full[ Xyears_full %!in% names( final_full ) ] ] <- NA
    final_full <- final_full[ , c('iso', 'sector', 'fuel', 'units', Xyears_full ) ]
    final_full[ final_full == 0 ] <- NA

# Interpolate/extend
    final_full <- interpolate_extend( final_full )
    final_full[ is.na( final_full ) ] <- 0

# Subset X_emissions_years
    final_out <- final_full[ , c( 'iso', 'sector', 'fuel',
                                  'units', X_emissions_years ) ]
# Removed values are recent bond values for cases that only had 2005 or 2010 values)
# This input file only extends to 2014, so copy values out to last data year
	X_Extend_Years <- paste0("X", 2015:BP_last_year)
    bond_remove_values <- bond_remove_values %>%
      dplyr::mutate_at( X_Extend_Years, funs( identity( !!rlang::sym( "X2014" ) ) ) )

# Replace bond values that were removed
    final_out <- replaceValueColMatch( final_out, bond_remove_values,
                                       x.ColName = paste0( 'X', 2001:end_year ),
                                       match.x = c( 'iso', 'sector', 'fuel' ),
                                       addEntries = F )
    final_out <- final_out %>%
        dplyr::arrange(iso, sector, fuel)

# ------------------------------------------------------------------------------
# 4. Write output

    writeData( final_out , "MED_OUT", paste0( "B.", em, "_comb_EF_db" ) )
    writeData( final_full, "EXT_IN", paste0( "B.", em, "_SPEW_comb_EF" ),
               domain_extension = "extension-data/" )

# Every script should finish with this line
    logStop()

# END

