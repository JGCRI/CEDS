#------------------------------------------------------------------------------
# Program Name: B1.2.add_SO2_recent_control_percent.R
# Author: Leyang Feng
# Date Last Updated: 18 May 2016
# Program Purpose: Adding EF trends for SO2 control percentage for recent years
# Input Files: B.[em]_comb_EF_GAINS_EMF30, B.[em]_ControlFrac_db
# Output Files: B.[em]_ControlFrac_db
# Notes:

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "analysis_functions.R" ) # Additional function files required.
    log_msg <- "Adding EF trends for SO2 control percentage
                for years after inventory data" # First message to be printed to the log
    script_name <- "B1.2.add_SO2_recent_control_percent.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "SO2"

# ------------------------------------------------------------------------------
# 1. Read in files and do preliminary setup

# Read in GAINS EFs
    gains_ef_db <- readData( 'DIAG_OUT',
                             paste0( 'B.', em, '_comb_EF_GAINS_EMF30' ) )
# Read in GAINS mapping files
    last_inv_year_csv <- readData( 'MAPPINGS',
                                   'SO2_control_frac_last_inv_year',
                                   extension = '.xlsx', sheet_selection = 1 )
    exclude_sector_fuel_combination <-
               readData( 'MAPPINGS', 'SO2_control_frac_exclude_fuel_sector' )

# read in the control percentage db
    control_db <- readData( 'MED_OUT', paste0( 'B.', em, '_ControlFrac_db' ) )
# ------------------------------------------------------------------------------
# 2. Recent year Gains EF ratio calculation
# equation: Ratio = EF( GAINS_year ) / EF( GAINS_lastinvyear)

# define recent years
    recent_years <- as.character( min( last_inv_year_csv$last_inv_year ) : 2014 )
    recent_Xyears <- paste0( 'X', recent_years )

# remove undesired sector fuel combination from gains_ef_db
    for ( row_index in 1 : nrow( exclude_sector_fuel_combination ) ) { ### This does not need to be a for loop!
        gains_ef_db <-
            gains_ef_db[ !( gains_ef_db$sector ==
                              exclude_sector_fuel_combination[ row_index, ]$sector &
                            gains_ef_db$fuel ==
                              exclude_sector_fuel_combination[ row_index, ]$fuel ), ]
    }

# extract countries with data in the last_inv_year_csv dataframe
    gains_ef_db <- gains_ef_db[ gains_ef_db$iso %in% last_inv_year_csv$iso, ]

# Extract recent year gains ef
    gains_recent <- gains_ef_db[ , c( 'iso', 'sector', 'fuel',
                                      'units', recent_Xyears ) ]

# extract the last year ef value for each country
    last_year_ef_list <- c( )
    for ( row_index in 1 : nrow( gains_recent ) ) {
        iso_name <- gains_recent[ row_index, 'iso' ]
        last_inv_year <- last_inv_year_csv[ last_inv_year_csv$iso == iso_name,
                                            'last_inv_year' ]
        last_year_ef <- gains_recent[ row_index, paste0( 'X', last_inv_year ) ]
        last_year_ef_list <- c( last_year_ef_list, last_year_ef )
    }

# make into matrix form
    gains_last_year_ef <- matrix( rep( last_year_ef_list,
                                       length( recent_years ) ),
                                  ncol = length( recent_years ) )

# calculate the ef ratio between gains recent year ef and gains last inv year ef
    recent_ratio <- gains_recent[ , recent_Xyears  ] / gains_last_year_ef
# make ratios before last_inv_year ( including last_inv_year ) 0 in recent_ratio
    last_inv_year_list <- c()
    for ( row_index in 1 : nrow( recent_ratio ) ) {
        iso_name <- gains_recent[ row_index, 'iso' ]
        last_inv_year <- last_inv_year_csv[ last_inv_year_csv$iso == iso_name,
                                            'last_inv_year' ]
        last_inv_year_list <- c( last_inv_year_list, last_inv_year )
        years_before_lastinvyear <-
            as.numeric( recent_years[ 1 ] ) : last_inv_year
        recent_ratio[ row_index, paste0( 'X', years_before_lastinvyear ) ] <- 0
    }
# make any ratio > 1 to 1
    recent_ratio[ recent_ratio > 1 ] <- 1
# make any NAs into 0
    recent_ratio[ is.na( recent_ratio ) ] <- 0

# Bind recent ration to iso x sector x fuel
    recent_ratio <- cbind( gains_recent[ , c( 'iso', 'sector',
                                              'fuel', 'units' ) ],
                           last_inv_year_list,
                           recent_ratio )

    colnames( recent_ratio ) <- c( 'iso', 'sector', 'fuel',
                                   'units', 'last_inv_year', recent_Xyears )

# ------------------------------------------------------------------------------
# 3. Recent year% control calculation
# calculate recent controls
# equations: c%(year) = ( 1 - R ) + R * C%(last_inv_year)
#            R = EF( GAINS_year ) / EF( GAINS_lastinvyear)
#            R is calculated as recent_ratio
    control_db_merge <- merge( control_db[ , c( 'iso', 'sector',
                                                'fuel', recent_Xyears ) ],
                               recent_ratio,
                               by = c( 'iso', 'sector', 'fuel' ) )

# extract ratio mat from control_db_merge
    ratio_mat <- control_db_merge[ , paste0( recent_Xyears, '.y' ) ]
# generate a calculation mask out of ratio_mat
    calc_mask <- ifelse( ratio_mat == 0, 0, 1 )

# generate a last year control fraction matrix
# extract the last year ef value for each country
    last_year_cf_list <- c( )
    for ( row_index in 1 : nrow( control_db_merge ) ) {
        last_inv_year <- control_db_merge[ row_index, 'last_inv_year' ]
        last_year_cf <- control_db_merge[ row_index,
                                          paste0( 'X', last_inv_year, '.x' ) ]
        last_year_cf_list <- c( last_year_cf_list, last_year_cf )
    }
# make into matrix form
    control_db_last_year_cf <- matrix( rep( last_year_cf_list,
                                            length( recent_years ) ),
                                       ncol = length( recent_years ) )

# generate an all 1 matrix
    all_one_mat <- matrix( 1, dim( control_db_last_year_cf )[ 1 ],
                              dim( control_db_last_year_cf )[ 2 ] )

# calculate recent controls
    recent_control <- all_one_mat - ratio_mat +
                      ratio_mat * control_db_last_year_cf
# apply the calc_mask to mask out the new controls that should not be calculated
    recent_control <- recent_control * calc_mask

    recent_controls <- cbind( control_db_merge[ , c( 'iso', 'sector', 'fuel' ) ],
                              recent_control )
    colnames( recent_controls ) <- c( 'iso', 'sector', 'fuel', recent_Xyears )

# ------------------------------------------------------------------------------
# 4. Replace the control percentage in control_db suing values in recent_control

    db_years <- colnames( control_db )[ grep( 'X', colnames( control_db ) ) ]
    recent_years <- recent_years

    merge_table <- merge( control_db, recent_controls,
                          by = c( 'iso', 'sector', 'fuel' ),
                          all = T )

    merge_table$units.x <- NULL

# remove NAs generated by unexisiting iso-sector-fuel combination in control_db ( if any )
    merge_no_na <- merge_table[ !is.na( merge_table$X1960 ), ]

    recent_mat <- merge_no_na[ , paste0( recent_Xyears, '.y' ) ]
    recent_mat[ is.na( recent_mat ) ] <- 0

    recent_mask <- ifelse( recent_mat == 0, 0, 1 )
    original_mask <- ifelse( recent_mat == 0, 1, 0 )

    merge_no_na[ , paste0( 'X', recent_years, '.x') ] <-
                  merge_no_na[ , paste0( 'X', recent_years, '.x') ] *
                  original_mask + recent_mat * recent_mask

    final_control <- merge_no_na
    drop_names <- paste0( 'X', recent_years, '.y' )
    final_control <- final_control[ ,colnames( final_control ) %!in% drop_names ]
    col_names <- gsub( '.x', '', colnames( final_control ) )
    colnames( final_control ) <- col_names

    final_control$units <- 'percent'
    final_control <- final_control[ , c( 'iso', 'sector', 'fuel',
                                         'units', db_years ) ]

# ------------------------------------------------------------------------------
# 5. Write out and Stop

    writeData( final_control, 'MED_OUT', paste0( 'B.', em, '_ControlFrac_db' ) )
# Every script should finish with this line
    logStop()

# END
