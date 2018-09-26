#------------------------------------------------------------------------------
# Program Name: B1.1.base_BCOC_comb_EF.R
# Author: Rachel Hoesly, Linh Vu
# Date Last Updated: 21 April 2016
# Program Purpose: 1. Produce OC emissions factors from SPEW (i.e. Bond) data.
#
# Input Files:  A.comb_activity.csv
#               Bond_country_map.csv
#               CD.Bond_mapped_sector_fuel.csv
# Output Files: B.[em]_comb_EF_db.csv
#               B.[em]_SPEW_comb_EF.csv
#               B.[em]_SPEW_NC_em.csv
# Notes: Emission factors (ef) are calculated as emissions divided by
#        consumption. Missing (zero or NA) ef are replaced using the following
#        rules, in order:
#          a. FSU residential coal-oil-gas replaced with FSU industrial coal oil gas
#          b. resdiential biomass replaced with iso sector fuel where available
#          c. others replaced with Region sector fuel EF where available
#          d. then replace with region fuel average
#          e. then global sector fuel average
#          f. then global fuel average
# ------------------------------------------------------------------------------

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
    if ( is.na( em ) ) em <- "BC"

# ------------------------------------------------------------------------------
# 0.5 Define functions for later use
    loadPackage( 'zoo' )

# interpolate_extend()
# This function is used to
    interpolate_extend <- function ( df ) {

        years <- names( df )[ grep( 'X', names( df ) ) ]
        interpolate <-  apply( X = df[ years ], MARGIN = 1,
                               FUN = function( x )
                                  anyNA( na.trim( x  ) )

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
    bcoc_historical <- readData( "EXT_IN", "CD.Bond_mapped_sector_fuel" )
    bond_iso_map <-    readData( "MAPPINGS", domain_extension = "Bond/",
                                 "Bond_country_map", meta = F )

# ------------------------------------------------------------------------------
# 2. Bond EFs
#    Process and extract Bond data. Begins with raw bond data; ends with
#    handled and interpolated dataframe.

    if ( em == 'BC') em_col <- 'BC_kt'
    if ( em == 'OC') em_col <- 'OC_kt'

# Create year sequences for handling input data
    X_bond_years_recent <- paste0( 'X', seq( 2005, 2010, 5 ) )
    X_bond_years <- paste0( 'X', seq( 1850, 2000, 5 ) )
    Xyears_full <- paste0( "X", 1850:2014 )

# Remove weird data and data we don't use (this data used in section 6)
    bond_everything <- dplyr::filter( bcoc_historical, BC_kt > 0, OC_kt > 0 )

# 1. Set aside only process and natural gas processes
# 2. Remove estimates for biomass after 2000 (Bond data doesn't update emission
#    factors after 2000)
# 3. Remove small values rows (they make bad efs when the values are so small)
# 4. Reformat year column to Xyears
    bond <- bond_everything %>%
        dplyr::filter( fuel %!in% c( 'process', 'natural_gas' ),
                       fuel != 'biomass' | Year <= 2000,
                       Fuel_kt > 7) %>%
        dplyr::mutate( Year = paste0( 'X', Year ) )

# Map CEDS agg_sector to Bond dataframe
    bond$agg_sector <- MSLevel[ match( bond$sector,
                                       MSLevel$working_sectors_v1 ),
                                "aggregate_sectors" ]

    bond_all <- bond

# Calculate emissions factors from Bond data by dividing emissions totals by
# fuel totals
    bond_all$EF <- bond_all[ , em_col ] / bond_all[ , 'Fuel_kt' ]

    bond_all_wide <- bond_all %>%
        dplyr::group_by( iso, fuel, sector, Year ) %>%
        dplyr::summarise( EF = mean( EF, na.rm = T ) ) %>%
        dplyr::ungroup() %>%
        tidyr::spread( Year, EF )

# Remove data that doesn't have values for 2005 or 2010 but not for earlier years
    bond_remove_values <-
        bond_all_wide[ which( ( !is.na( bond_all_wide$X2005 ) |
                                !is.na( bond_all_wide$X2010 ) ) &
                                ( apply( X = bond_all_wide[ X_bond_years ],
                                         FUN = all.na, MARGIN = 1 ) ) ),
                       c( 'iso', 'sector', 'fuel', X_bond_years_recent ) ]

# Add and reorder yearly columns
    bond_remove_values [ paste0( 'X', 2001:end_year )[
                         paste0( 'X', 2001:end_year ) %!in%
                           X_bond_years_recent ] ] <- NA
    bond_remove_values <- bond_remove_values[ c( 'iso', 'sector', 'fuel',
                                                 paste0( 'X', 2001:end_year ) ) ]
# Interpolate NAs in the data frame, filling in yearly data
    bond_remove_values <- interpolate_extend( bond_remove_values )

# Prepare to handle all of the other data rows
    bond <- bond[ which( paste( bond$iso, bond$fuel, bond$sector ) %!in%
                           paste( bond_remove_values$iso,
                                  bond_remove_values$fuel,
                                  bond_remove_values$sector ) ), ]

# ------------------------------------------------------------------------------
# 3. Reformat and create average EFs
#    This code block aggregates emissions factors to many different
#    aggregate levels.

# Aggregate remaining data by country
    bond_country <- aggregate( bond[ , c( "Fuel_kt" ,"BC_kt", "OC_kt" ) ],
                               by = list(  iso = bond$iso,
                                           fuel = bond$fuel ,
                                           sector =  bond$sector,
                                           Year = bond$Year) ,
                               FUN = sum )
# Calculate emissions factors from Bond data by dividing emissions totals by
# fuel totals
    bond_country$EF <- bond_country[ , em_col ] / bond_country[ , 'Fuel_kt' ]
    bond_EF_country <- cast( bond_country,
                             iso + fuel + sector ~ Year,
                             value = 'EF' )
# Replace non-Bond years with NAs
    bond_EF_country [ X_emissions_years[ X_emissions_years %!in%
                                           X_bond_years ] ] <- NA
# Retrieve the XYears ### Use function isXYear()
    Xyears <- names( bond_EF_country )[ grep( "X",
                                              names( bond_EF_country ) ) ] %>%
                sort()
# Reorder columns
    bond_EF_country <- bond_EF_country[ , c( "iso", "fuel", "sector", Xyears ) ]
# Interpolate all the NAs in the dataframe based on the bond data
    bond_EF_country <- interpolate_extend( bond_EF_country )
# Retain only complete cases of identifiers
    bond_EF_country <-
        bond_EF_country[ complete.cases( bond_EF_country[ Xyears ] ), ]

# Aggregate by Region and repeat analyses
    bond_region <- aggregate( bond[ , c( "Fuel_kt" ,"BC_kt", "OC_kt" ) ],
                              by = list( Region = bond$Region,
                                         fuel = bond$fuel,
                                         sector =  bond$sector,
                                         Year = bond$Year ),
                              FUN = sum )
    bond_region$EF <- bond_region[ , em_col ] / bond_region[ , 'Fuel_kt' ]
    bond_EF_region <- cast( bond_region,
                            Region + fuel + sector ~ Year,
                            value = 'EF' )
    bond_EF_region [ X_emissions_years[ X_emissions_years %!in%
                                        X_bond_years ] ] <- NA
    bond_EF_region <- bond_EF_region[ , c( "Region", "fuel", "sector", Xyears ) ]
    bond_EF_region <- interpolate_extend( bond_EF_region )
    bond_EF_region <- bond_EF_region[ complete.cases( bond_EF_region[ Xyears ] ), ]

# aggregate by Region, aggregate sector and repeat analyses
    bond_agg_sector <- aggregate( bond[ , c( "Fuel_kt" ,"BC_kt", "OC_kt" ) ],
                                  by = list( Region = bond$Region,
                                             fuel = bond$fuel,
                                             agg_sector = bond$agg_sector,
                                             Year = bond$Year ),
                                  FUN = sum )
    bond_agg_sector$EF <- bond_agg_sector[ , em_col ] /
                          bond_agg_sector[ , 'Fuel_kt' ]
    bond_EF_agg_sector <- cast( bond_agg_sector,
                                Region + fuel + agg_sector ~ Year,
                                value = 'EF' )
    bond_EF_agg_sector [ X_emissions_years[ X_emissions_years %!in%
                                              X_bond_years ] ] <- NA
    bond_EF_agg_sector <- bond_EF_agg_sector[ , c( "Region", "fuel",
                                                   "agg_sector", Xyears ) ]
    bond_EF_agg_sector <- interpolate_extend( bond_EF_agg_sector )

# aggregate by Region, fuel and repeat analyses
    bond_region_fuel <- aggregate( bond[ , c( "Fuel_kt" ,"BC_kt", "OC_kt" )],
                                   by = list(  Region = bond$Region ,
                                               fuel = bond$fuel ,
                                               Year = bond$Year) ,
                                   FUN = sum)
    bond_region_fuel$EF <- bond_region_fuel[ , em_col ] /
                           bond_region_fuel[ , 'Fuel_kt' ]
    bond_EF_region_fuel <- cast( bond_region_fuel,
                                 Region + fuel ~ Year,
                                 value = 'EF' )
    bond_EF_region_fuel [ X_emissions_years[ X_emissions_years
                                             %!in% X_bond_years ] ] <- NA
    bond_EF_region_fuel <- bond_EF_region_fuel[ , c( "Region", "fuel", Xyears ) ]
    bond_EF_region_fuel <- interpolate_extend( bond_EF_region_fuel )

# aggregate by fuel, aggregate sector and repeat analyses
    bond_fuel_agg_sector <- aggregate( bond[ , c( "Fuel_kt", "BC_kt", "OC_kt" ) ],
                                       by = list( fuel = bond$fuel,
                                                  agg_sector = bond$agg_sector,
                                                  Year = bond$Year ),
                                       FUN = sum )
    bond_fuel_agg_sector$EF <- bond_fuel_agg_sector[ , em_col ] /
                               bond_fuel_agg_sector[ , 'Fuel_kt' ]
    bond_EF_fuel_agg_sector <- cast( bond_fuel_agg_sector,
                                     agg_sector + fuel  ~ Year,
                                     value = 'EF' )
    bond_EF_fuel_agg_sector [ X_emissions_years[ X_emissions_years %!in%
                                                 X_bond_years ] ] <- NA
    bond_EF_fuel_agg_sector <- bond_EF_fuel_agg_sector[ , c( "agg_sector",
                                                             "fuel", Xyears ) ]
    bond_EF_fuel_agg_sector <- interpolate_extend( bond_EF_fuel_agg_sector )

# aggregate by fuel, aggregate sector and repeat analyses
    bond_fuel <- aggregate( bond[ , c( "Fuel_kt" ,"BC_kt", "OC_kt" ) ],
                            by = list( fuel = bond$fuel ,
                                       Year = bond$Year) ,
                            FUN = sum )
    bond_fuel$EF <- bond_fuel[ , em_col ] /
                    bond_fuel[ , 'Fuel_kt' ]
    bond_EF_fuel <- cast( bond_fuel, fuel  ~ Year, value = 'EF' )
    bond_EF_fuel [ X_emissions_years[ X_emissions_years %!in%
                                        X_bond_years ] ] <- NA
    bond_EF_fuel <- bond_EF_fuel[ , c( "fuel", Xyears ) ]
    bond_EF_fuel <- interpolate_extend( bond_EF_fuel )

# Subset residential biomass activity
    EF_residential_biomass <-
        bond_EF_country[ bond_EF_country$fuel   == "biomass" &
                         bond_EF_country$sector == '1A4b_Residential', ]


# ------------------------------------------------------------------------------
# 4. Map to CEDS sectors and countries
#    This block of code adds emissions factors calculated at the various levels
#    handled above. Rows are sequentially inserted by specificity priority; for example,
#    all rows with data at the regional level get that data, then all rows at the
#    aggregate sector level, etc

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

# make natural gas ef = 0
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

# add EFs by region, sector, fuel to those which didn't have 1960 data
    EF <- replaceValueColMatch( EF_nas, bond_EF_region,
                                x.ColName = Xyears,
                                match.x = c( 'Region', 'sector', 'fuel' ),
                                addEntries = FALSE )

# Subset those rows that STILL don't have 1960 data and add the rest back in
    EF_nas <- EF[ is.na( EF$X1960 ), ]
    EF_final <- rbind( EF_final , EF[ !is.na( EF$X1960 ), ] )

# add EFs by region, aggregate Sector,fuel
    EF <- replaceValueColMatch( EF_nas, bond_EF_agg_sector,
                                x.ColName = Xyears,
                                match.x = c( 'Region', 'agg_sector', 'fuel' ),
                                addEntries = FALSE )
# Subset those rows that STILL don't have 1960 data and add the rest back in
    EF_nas <- EF[ is.na( EF$X1960 ), ]
    EF_final <- rbind( EF_final , EF[ !is.na( EF$X1960 ), ] )


# add EFs by region fuel
    EF <- replaceValueColMatch( EF_nas , bond_EF_region_fuel,
                                x.ColName = Xyears,
                                match.x = c( 'Region', 'fuel' ),
                                addEntries = FALSE )
# Subset those rows that STILL don't have 1960 data and add the rest back in
    EF_nas <- EF[ is.na( EF$X1960 ), ]
    EF_final <- rbind( EF_final, EF[ !is.na( EF$X1960 ), ] )

# add EFs by fuel , aggregate sector
    EF <- replaceValueColMatch( EF_nas, bond_EF_fuel_agg_sector,
                                x.ColName = Xyears,
                                match.x = c( 'agg_sector', 'fuel' ),
                                addEntries = FALSE )

# Subset those rows that STILL don't have 1960 data and add the rest back in
    EF_nas <- EF[ is.na( EF$X1960 ), ]
    EF_final <- rbind( EF_final, EF[ !is.na( EF$X1960 ), ] )

# add EFs by global fuel average
    EF <- EF_nas
    EF[ Xyears ] <- bond_EF_fuel[ match( EF$fuel, bond_EF_fuel$fuel ), Xyears ]

    EF_nas <- EF[ is.na( EF$X1960 ), ]
    EF_final <- rbind( EF_final, EF[ !is.na( EF$X1960 ), ] )

# ------------------------------------------------------------------------------
# 5. Final Processing
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

# replace bond values that were removed in section 2
    final_out <- replaceValueColMatch( final_out, bond_remove_values,
                                       x.ColName = paste0( 'X', 2001:end_year ),
                                       match.x = c( 'iso', 'sector', 'fuel' ),
                                       addEntries = F )

# ------------------------------------------------------------------------------
# 6. Handle process emissions

# Identify Bond years
    X_bond_process_years <- paste0( 'X', seq( 1850, 2010, 5 ) )

# Return to dataframe set aside earlier; convert years to XYears
    bond_everything$Year <- paste0( 'X', bond_everything$Year )

# Aggregate to iso/sector/fuel/year
    bond_everything <- aggregate( bond_everything[ paste0( em, '_kt' ) ],
                                  by = list( iso = bond_everything$iso,
                                             sector = bond_everything$sector,
                                             fuel = bond_everything$fuel,
                                             Year = bond_everything$Year ),
                                  FUN = sum )

# Drop columns without emissions data
    bond_everything <-
        bond_everything[ which( !is.na( bond_everything[ , em_col ] ) ), ]

# Cast to wide
    bond_process <- cast( bond_everything,
                          iso + sector + fuel ~ Year,
                          value = paste0( em, '_kt' ) )

# Extract non-combustion sectors from Bond
    process_sectors <- MSL[ which( MSL$type == 'NC' ), 'sector' ]
    bond_process <- bond_process[ which( bond_process$sector %in%
                                           process_sectors ), ]

# Set non data years to NA
    bond_process [ X_emissions_years[ X_emissions_years %!in%
                                        X_bond_process_years ] ] <- NA

# organize and interpolate process data
    bond_process <- bond_process[ , c( 'iso', 'sector', 'fuel', Xyears ) ]

    bond_process <- bond_process[ rowSums( is.na( bond_process[ Xyears ] ) ) !=
                                    length( Xyears ), ]
    bond_process <- replace( bond_process, bond_process == 0, NA )
    bond_process_extend <- interpolateValues( bond_process )

    bond_process_extend[ is.na( bond_process_extend ) ] <- 0

# Bond stops at 2010. Copy 2010 process activity values to 2011-2014
    bond_process_extend[ , paste0( "X", 2011:2014 ) ] <- bond_process_extend$X2010

# relabel fuel and process and add units
    bond_process_extend$fuel <- 'process'
    bond_process_extend$units <- 'kt'

# Trim to final years
    bond_process_extend <- bond_process_extend[ , c( 'iso', 'sector', 'fuel',
                                                     'units', Xyears_full ) ]

# ------------------------------------------------------------------------------
# 7. Write output

    writeData( final_out , "MED_OUT", paste0( "B.", em, "_comb_EF_db" ))
    writeData( final_full, "MED_OUT", paste0( "B.", em, "_SPEW_comb_EF" ))
    writeData( bond_process_extend, "MED_OUT",
               fn = paste0( "B.", em, "_SPEW_NC_em" ) , meta = F )

# Every script should finish with this line
    logStop()

# END
