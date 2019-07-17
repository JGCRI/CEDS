#------------------------------------------------------------------------------
# Program Name: C1.2.ECLIPSE_flaring_emissions_extension.R
# Author: Leyang Feng
# Date Last Modified: June 28, 2016
# Program Purpose: Extends ECLIPSE flaring emissions to period 1960 - last BP year using IEA
#                  and BP crude oil production data
# Input Files: [em]_eclipse_flr_emissions.csv
# Output Files: C.[em]_ECLIPSE_flaring_emissions_extended.csv
# Notes: In section 3.2 the 1970 data are manually 'extended' using 1971 data,
#        the data to last BP year are manually 'extended' using 2013 data.
#
# TODO: 1. About 70 countries in [em]_eclipse_flr_emissions.csv get picked out in the routine
#          because IEA/BP oil production data does not include those countries. Update when
#          a more comprehensive database of historical production is availiable.
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
    headers <- c( 'data_functions.R' ) # Any additional function files required
    log_msg <- "Extension of ECLIPSE flaring emissions" # First message to be printed to the log
    script_name <- "C1.2.ECLIPSE_flaring_emissions_extension.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define emission species and read in files

# Define emissions species variable
    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "BC"

    MODULE_C <- "../code/module-C/"

# read in the ECLIPSE flaring data
    ECLIPSE_flaring <- readData( "EM_INV", domain_extension = "ECLIPSE-flaring/", file_name = paste0( em, '_eclipse_flr_emissions' ) )
# read in the en_stat_sector_fuel.csv to extract IEA crude oil production data
    en_stat_sector_fuel <- readData( 'MED_OUT', file_name = 'A.en_stat_sector_fuel' )
# read in the BP oil production data since this is a longer more consistent time series than IEA
    bp_energy_data <- readData( 'ENERGY_IN', file_name = BP_data_file_name, extension = ".xlsx", skip = 2 )
    bp_oil_prod  <- bp_energy_data[[ getBPSheetNumber( "oil", "production", "tonnes", bp_energy_data ) ]]

# read in master country list
    mcl <- readData( 'MAPPINGS', 'Master_Country_List' )

# read in the population data
    pop_raw <- readData( "MED_OUT", "A.UN_pop_master" )

# ------------------------------------------------------------------------------
# 2. Pre-processing
# 2.1. pre-processing of BP data
# 2.1.1 cleaning from raw data
    BP_oil_years <- 1965 : BP_last_year
    BP_Xyears <- paste0( 'X', BP_oil_years )
    # Remove last columns, which are not time series data
    BP_data <- bp_oil_prod[ , 1 : ( ncol( bp_oil_prod ) - 2 ) ]
    colnames( BP_data ) <- c( 'BPName_Oil_production', BP_Xyears )

    # Take only the years used for calcs and the column with BP ctry names
    BP_data <- subset( BP_data, T,c( "BPName_Oil_production", BP_Xyears ) )

    # Add USSR to MCL for newer BP data so that clean function retains USSR data
    ussr_row <- mcl[ which( mcl$iso %in% c( 'ukr' ) ) , ]
    ussr_row$BPName_Oil_production <- "USSR"
    MCL_with_ussr <- rbind(mcl, ussr_row )

    fsu_ussr <- c( "arm", "aze", "blr", "est", "geo", "kaz", "kgz", "ltu", "lva", "mda", "rus", "tjk", "tkm", "ukr", "uzb", "ussr")
    MCL_ussr <- MCL_with_ussr[ MCL_with_ussr$iso %in% fsu_ussr, ]
    FSU_countries <- na.omit(MCL_ussr$BPName_Oil_production)

    # Convert to numeric
    BP_data[ BP_Xyears ] <- sapply( X = BP_data[ BP_Xyears ], FUN = as.numeric )

    BP_data <- sumAggregateRegion_Oil_production( BP_data, "", FSU_countries, "ussr" )

    # Note - BP data has a different set of countries for oil production as compared to their energy
    #        consumption data, so this is listed in a different column, BPName_Oil_production, in
    #        the master country list
    BP_data <- BP_data[ BP_data$BPName_Oil_production %in% c(mcl$BPName_Oil_production,"ussr"), ]
    BP_countries <- na.omit(mcl$BPName_Oil_production)
    mcl_bp <- mcl[ mcl$BPName_Oil_production %in% BP_countries, c( 'iso', 'BPName_Oil_production' ) ]
    mcl_bp <- mcl_bp[ !duplicated( mcl_bp ), ]

    BP_merge <- merge( BP_data, mcl_bp, by = c( "BPName_Oil_production" ), all.y = T )
    BP_merge[ BP_merge == 'n/a' ] <- 0
    BP_oil <- BP_merge[ , c( 'iso', BP_Xyears ) ]
    BP_oil[, BP_Xyears ] <- sapply( BP_oil[ , BP_Xyears ], as.numeric )
    BP_oil[, BP_Xyears ] <- BP_oil[, BP_Xyears ] * 0.001 # convert the unit tonnes to kt

# 2.1.2. special treatment for fsu countries
# for fsu member countries there's no data before 1985 in raw BP, but total number of fsu does exist.
# Here the code splits the ussr( fsu ) and distributes to fsu member countries
    fsu <- 'ussr'
    fsu_members <- c( "arm", "aze", "blr", "est", "geo", "kaz", "kgz", "ltu", "lva", "mda", "rus", "tjk", "tkm", "ukr", "uzb" )

    BP_split_Xyears <- paste0( 'X', 1965 : 1984 )

    #extract fsu countries from BP_oil data
    BP_fsu <- BP_oil[ BP_oil$iso %in% fsu, ]
    BP_fsu_members <- BP_oil[ BP_oil$iso %in% fsu_members, ]

    # re-format the pop data
    pop_fsu <- pop_raw[ pop_raw$iso %in% c( fsu, fsu_members ), ]
    pop_fsu$year <- paste0( 'X', pop_fsu$year )
    pop_wide <- cast( pop_fsu, iso ~ year, value = 'pop', fun.aggregate = sum )
    pop_wide <- pop_wide[ , c( 'iso', BP_Xyears ) ]

    # split using disaggregate_country function
    BP_fsu_split <- rbind( BP_fsu, BP_fsu_members )
    BP_fsu_split$sector <- 'oil_production' # needed by the function but has no other effect

    oil_production <- disaggregate_country( original_data = BP_fsu_split,
                                  trend_data = pop_wide,
                                  trend_match_cols = 'iso',
                                  combined_iso = fsu,
                                  disaggregate_iso = BP_fsu_members$iso, #c('aze' , 'kaz' , 'rus' , 'tkm' , 'uzb'),
                                  dis_start_year = 1965,
                                  dis_end_year = 1984 )

# 2.1.3. replace data of fsu countries in BP_oil with BP_fsu_members and remove 'ussr'
    for ( BP_fsu_member in BP_fsu_members$iso ) {
      BP_oil[ BP_oil$iso ==BP_fsu_member, BP_split_Xyears ] <- oil_production[ oil_production$iso == BP_fsu_member, BP_split_Xyears ]
      }

    BP_oil <- BP_oil[ BP_oil$iso != 'ussr', ]

    BP_iso <- BP_oil$iso

# 2.2. pre-processing of IEA data
    IEA_crude <- en_stat_sector_fuel[ en_stat_sector_fuel$sector == 'crude-oil-production', ]
    # exclude isos in BP data since BP data for those isos are more complete
    IEA_crude <- IEA_crude[ IEA_crude$iso %!in% BP_iso, ]
    IEA_Xyears <- colnames( IEA_crude )[ grep( 'X', colnames( IEA_crude ) ) ]
    IEA_iso <- IEA_crude$iso

# 2.3. pre-processing of ECLIPSE flaring data
    flaring <- ECLIPSE_flaring[ c( 'iso', 'X1990', 'X2000', 'X2010' ) ]
    drop_iso_list <- c( )
    for ( row_index in 1 : nrow( flaring ) ) {
      a_row <- flaring[ row_index , ]
      a_row_iso <- a_row$iso
      if ( a_row$X1990 == 0 & a_row$X2000 == 0 & a_row$X2010 == 0 ) { drop_iso_list <- c( drop_iso_list, a_row_iso ) }
    }
    flaring <- flaring[ flaring$iso %!in% drop_iso_list, ]
    flaring_iso <- flaring$iso


# 2.4. combine countries in BP and IEA and ECLIPSE_flaring
# extract common countries between ECLIPSE flaring and IEA crude oil production data
# and ECLIPSE flaring and BP oil production data
    flr_bp_iso <- intersect( BP_iso, flaring_iso )
    flaring_flrbpiso <- flaring[ flaring$iso %in% flr_bp_iso, ]
    flaring_flrbpiso <- flaring_flrbpiso[ order( flaring_flrbpiso$iso ),  ]
    BP_flrbpiso <- BP_oil[ BP_oil$iso %in% flr_bp_iso, ]
    BP_flrbpiso <- BP_flrbpiso[ order( BP_flrbpiso$iso ), ]


    flr_iea_iso <- intersect( IEA_iso, flaring_iso )
    flaring_flrieaiso <- flaring[ flaring$iso %in% flr_iea_iso, ]
    flaring_flrieaiso <- flaring_flrieaiso[ order( flaring_flrieaiso$iso ), ]
    IEA_flrieaiso <- IEA_crude[ IEA_crude$iso %in% flr_iea_iso, ]
    IEA_flrieaiso <- IEA_flrieaiso[ order( IEA_flrieaiso$iso ), ]

# combine BP_flrbpiso and IEA_flrieaiso together as unified layout
    # Extend data out to last year by copying last IEA year
    IEA_flrieaiso <- IEA_flrieaiso %>%
      dplyr::mutate_at( X_BP_years, funs( identity( !!rlang::sym( X_IEA_end_year ) ) ) )

    IEA_flrieaiso$X1970 <- IEA_flrieaiso$X1971

    extend_years <- 1965 : BP_last_year
    extend_Xyears <- paste0( 'X', extend_years )

    flaring_bp_iea <- rbind( BP_flrbpiso[ , c( 'iso', extend_Xyears ) ], IEA_flrieaiso[ , c( 'iso', extend_Xyears ) ] )
    flaring_bp_iea <- flaring_bp_iea[ order( flaring_bp_iea$iso ), ]

    flaring_eclipse <- rbind( flaring_flrbpiso, flaring_flrieaiso )
    flaring_eclipse <- flaring_eclipse[ order( flaring_eclipse$iso ), ]


# ------------------------------------------------------------------------------
# 3. Extending
# method: The ECLIPSE flaring only has data for year 1990, 2000, 2010 while the BP/IEA crude oild production has
#         time series data from 1965 to last BP year. The extending procedure first calculates ratios between
#         ECLIPSE data and BP/IEA data for avaliable years ( 1990, 2000, 2010 ), then extends the ratios to
#         all years( 1965 - last BP year ) using linear method and multiply the ratios to BP/IEA data to have
#         full time series data of ECLIPSE flaring.
    flaring_ratio <- data.frame( iso = flaring_eclipse$iso, X1990 = ( flaring_eclipse$X1990 / flaring_bp_iea$X1990 ),
                                    X2000 = ( flaring_eclipse$X2000 / flaring_bp_iea$X2000 ),
                                    X2010 = ( flaring_eclipse$X2010 / flaring_bp_iea$X2010 ), stringsAsFactors = F )
    flaring_ratio[ is.na( flaring_ratio ) ] <- 0
    temp_matrix <- data.matrix(  flaring_ratio[ , c( 'X1990', 'X2000', 'X2010' ) ] )
    temp_matrix[ is.infinite( temp_matrix ) ] <- 0
    temp_matrix <- as.data.frame( temp_matrix )
    flaring_ratio <- cbind( flaring_ratio$iso, temp_matrix )
    colnames( flaring_ratio ) <- c( 'iso', 'X1990', 'X2000', 'X2010' )
    flaring_ratio$iso <- as.character( flaring_ratio$iso )

    year <- c( 1990, 2000, 2010 )
    flaring_extended_ratios <- data.frame()
    for ( row_index in 1 : nrow( flaring_ratio ) ) {
      ratio <- unlist( flaring_ratio[ row_index, c( 'X1990', 'X2000', 'X2010' ) ] )
      linear_reg <- lm( ratio ~ year )
      extended_ratios <- linear_reg$coefficients[[ 2 ]] * extend_years + linear_reg$coefficients[[ 1 ]]
      flaring_extended_ratios <- rbind( flaring_extended_ratios, extended_ratios )
    }
    flaring_extended <- flaring_bp_iea[ , extend_Xyears ] * flaring_extended_ratios
    flaring_extended[ flaring_extended < 0 ] <- 0
    flaring_extended <- cbind( flaring_ratio$iso, flaring_extended )
    colnames(  flaring_extended ) <- c( 'iso', extend_Xyears )
    flaring_extended$iso <- as.character( flaring_extended$iso )

    flaring_extended$em <- em
    flaring_extended$sector <- '1B2c_Venting-flaring-oil-gas'
    flaring_extended$units <- 'kt'
    flaring_extended <- flaring_extended[ , c( 'iso', 'em', 'sector', 'units', extend_Xyears ) ]

    # Clean-up data
    data_columns <- names( flaring_extended )[5:length(names(flaring_extended))]
    flaring_extended <- removeNARows(flaring_extended, data_columns )
    flaring_extended <- NAsToZeros(flaring_extended, data_columns )

# -----------------------------------------------------------------------------
# 4. Write output
    writeData( flaring_extended , "MED_OUT", paste0( "C.", em, "_ECLIPSE_flaring_emissions_extended" ) )
    writeData( flaring_ratio , "DIAG_OUT", paste0( "C.", em, "_ECLIPSE_flaring_to_crude_oil_production_ratios" ) )

# Every script should finish with this line:
    logStop()

