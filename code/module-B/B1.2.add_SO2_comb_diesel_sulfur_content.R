# ------------------------------------------------------------------------------
# Program Name: B1.2.add_SO2_comb_diesel_sulfur_content.R
# Author: Rachel Hoesly, Linh Vu
# Date Last Updated: 21 December 2015
# Program Purpose: Add Sulfur Standards to default sulfur EF.
# values from 2005 back to 1975. Does not take user input data.
#
# Input Files: Diesel_transport_S_trend.xlsx
#
# Output Files: B.SO2_diesel_s_content.csv
# TODO:
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "analysis_functions.R",'process_db_functions.R',
                  'common_data.R', 'IO_functions.R', 'data_functions.R', 'timeframe_functions.R') # Additional function files may be required.
    log_msg <- "Adding sulfur standards to diesel SO2 EF" # First message to be printed to the log
    script_name <- "B1.2.add_SO2_comb_diesel_sulfur_content.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 0.5 Load Packages

    loadPackage( 'zoo' )

# ---------------------------------------------------------------------------
# 1. Reading data

# Read in S trends in diesel
    diesel_standards_ppm <- readData( "DEFAULT_EF_IN", "Diesel_transport_S_trend",
                                      ".xlsx", sheet_selection = "ppm",
                                      col_types = c(rep('text', 3), rep('numeric', 52)))
# Mapping file
    MCL <- readData( "MAPPINGS", "Master_Country_List" )

# -------------------------------------------------------------------------------
# 2. Format to CEDS format

# Extract region/iso info
    regions <- diesel_standards_ppm[ , 1:2 ]

# select and rename columns
    col.names <- names( diesel_standards_ppm )
# Subset all numeric column names
    years <- col.names[ which( suppressWarnings(is.na( as.numeric( col.names ) ) == FALSE )) ]
    start_year <- years[ 1 ]
# Paste to Xyears
    X_years <- paste0( 'X', years )
# Only take iso and data columns
    diesel_standards_ppm <- diesel_standards_ppm[ , c( 'iso', years ) ]
# Force to Xyears
    names( diesel_standards_ppm ) <- c( 'iso', X_years )

# Select data from CEDS start to 2015
    X_standard_years <- paste( "X", start_year:2015, sep = "" )
    diesel_standards_ppm <- diesel_standards_ppm[ , c( 'iso',
                                                       X_standard_years ) ]

# Drop data with NA isos
    diesel_standards_ppm <-
         diesel_standards_ppm[ !is.na( diesel_standards_ppm$iso ), ]

# -------------------------------------------------------------------------------
# 3. Fill in default and interpolate between data estimates
#    Begin with default S content standardsw where available and NAs where not.
#    End with a value for every year based on the most recent non-NA value.
    printLog( 'Interpolate diesel standards over time' )

# The default starting standard is 8000ppm; if a row has no data in 1970, use
# this default.    ### Where does this number come from?
    diesel_standards_ppm[ which( is.na(
           diesel_standards_ppm$X1970 ) ), 'X1970' ] <- 8000

# extend most recent standard through 2015
    diesel_standard_extend <- t( diesel_standards_ppm[ , -1 ] )
# Replace each NA with the most recent observation
    diesel_standard_extend <- na.locf( diesel_standard_extend )
# Transpose across the entire matric
    diesel_standards_ppm_filled <- diesel_standards_ppm
    diesel_standards_ppm_filled[ , -1 ] <- t( diesel_standard_extend )

# -------------------------------------------------------------------------------
# 4. Fill in regional average for countries with no data
#    The given diesel standards are only available for some isos. Find regional
#    averages and use these values for all other isos.

    printLog( 'Extrapolate diesel standards over regions' )

# Obtain a list of all isos that are not in the diesel standards DF yet.
    iso.region.average <- setdiff( unique( MCL$iso ),
                                   diesel_standards_ppm_filled$iso )

# Merge all isos into the diesel_standards working df. This will result in rows
# of NAs for any new isos.
    diesel_standards_ppm_all <- merge( diesel_standards_ppm_filled,
                                       unique( MCL[ , c( 'iso', 'Region' ) ] ),
                                       all = TRUE )
# Make a dataframe of those rows whose isos have no values and thus need
# regional averages
    diesel_standards_region_tofill <-
          diesel_standards_ppm_all[ which( diesel_standards_ppm_all$iso %in%
                                             iso.region.average ), ]

# Begin calculating regional averages from available data:
# Melt the complete rows of diesel data into long form
    diesel_standards_region_wide <- melt( diesel_standards_ppm_filled, id.vars ='iso' ) ### this is long form, not wide. Rename this dataframe

# Aggregate to each iso x year to ensure a single value per country  ### Check if this is necessary; is it possible to have duplicates here?
    region_average_long <- ddply( diesel_standards_region_wide,
                                  .( iso, variable ), summarize,
                                  average = mean( value ) )

# Cast to wide and merge with region tags
    region_average_wide <- cast( region_average_long,
                                 iso ~ variable, value = 'average' )
    region_average_wide <- merge( MCL[ , c( 'iso', 'Region' ) ],
                                  region_average_wide )

    diesel_standards_region_filled <- diesel_standards_region_tofill
# Replace values from regional avg dataframe into the cells needing data
    diesel_standards_region_filled[ , X_standard_years ] <-
      region_average_wide[ match( diesel_standards_region_filled$Region, ### This does not work!! Regions have not been averaged; these are just values from countries
                                  region_average_wide$Region ),
                           X_standard_years ]

# Combine newly-filled dataframe with dataframe holding original values
    diesel_standards_ppm_complete <-
      rbind( diesel_standards_region_filled[ , c( 'iso', X_standard_years ) ],
             diesel_standards_ppm_filled )

# select only countries in Master Country List (excluding historical/extinct countries)
    diesel_standards_ppm_complete <-
        diesel_standards_ppm_complete[ diesel_standards_ppm_complete$iso
                            %in% MCL$iso[ !grepl( "historical", MCL$note ) ], ]

# -------------------------------------------------------------------------------
# 5. Calculate EF, fill in sectors and fuel

    printLog( "Calculating diesel sulfer content" )

# Convert from ppm to fraction (per 1)
    diesel_Standard_as_S_fraction <- diesel_standards_ppm_complete
    diesel_Standard_as_S_fraction[ , X_standard_years ] <-
           diesel_Standard_as_S_fraction[ , X_standard_years ] / 10^6

# Add fuel, sector, fraction tags to data
    diesel_Standard_as_S_fraction$fuel <- 'diesel_oil'
    diesel_Standard_as_S_fraction$sector <- '1A3b_Road'
    diesel_Standard_as_S_fraction$units <- 'fraction'

# Reorder columns and trim to desired years
    diesel_Standard_as_S_fraction <-
        diesel_Standard_as_S_fraction[ , c( 'iso', 'sector', 'fuel', 'units',
               X_standard_years[ X_standard_years %in% X_emissions_years ] ) ]

# Retain only complete cases
    diesel_Standard_as_S_fraction <-
        diesel_Standard_as_S_fraction[ complete.cases( diesel_Standard_as_S_fraction ), ]

# -------------------------------------------------------------------------------
# 6. Output
#    Write final data to output

    printLog( "Adding diesel sulfer standard SO2 EFs to B.SO2_comb_EF_db" )

    writeData( diesel_Standard_as_S_fraction, domain = "DEFAULT_EF_PARAM" , 'B.SO2_diesel_s_content' )

    logStop()
# END
