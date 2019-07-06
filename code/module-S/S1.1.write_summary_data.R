
# ------------------------------------------------------------------------------
# Program Name: S1.1.write_summary_data.R
# Authors: Rachel Hoesly, Steve Smith, Linh Vu, Presley Muwan, Leyang Feng,
#          Caleb Braun, Patrick O'Rourke
# Date Last Updated: April 25, 2019
# Program Purpose: Produces summary output
# Input Files: Master_Country_List.csv
#              [em]_total_CEDS_emissions.csv
#              Master_Sector_Level_map.csv
# Output Files: data in final-emissions folder
# TODO:
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers

# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
  PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R", 'process_db_functions.R',
              "summary_functions.R", 'common_data.R', 'timeframe_functions.R')
log_msg <- "Writes Final summary data" # First message to be printed to the log
script_name <- "S1.1.write_summary_data.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "SO2"

# ---------------------------------------------------------------------------
# 0.5. Script Options
write_years <- 1750:end_year

# Option to also write out data by CEDS sectors
WRITE_CEDS_SECTORS = TRUE

# Define functions to move a list of files (full path name)
moveFile <- function( fn, new_dir ) {
  fn_base <- basename( fn )
  file.rename( fn, paste0( new_dir, fn_base ) )
}

moveFileList <- function( fn_list, new_dir ) {
  lapply( fn_list, function( fn ) moveFile( fn, new_dir ) )
}

# ---------------------------------------------------------------------------
# 1. Load files

Master_Country_List <- readData( "MAPPINGS", "Master_Country_List")
final_emissions_read <- readData ( "MED_OUT" , paste0(em,'_total_CEDS_emissions') )
Master_Sector_Level_map <- readData(domain = 'MAPPINGS', file_name = 'Master_Sector_Level_map')

# ---------------------------------------------------------------------------
# Data processing

X_write_years <- paste0('X',write_years)
final_emissions <- final_emissions_read[,c('iso','sector','fuel','units',X_write_years)]
final_emissions$em <- em

# remove sectors that are not supplied in CEDS
empty_sectors <- c( "11A_Volcanoes", "11B_Forest-fires", "11C_Other-natural" )
final_emissions <- final_emissions[ -which( final_emissions$sector %in% empty_sectors ) , ]

# save shipping and aviation emissions
shp_av_sectors <- c( "1A3di_International-shipping", "1A3aii_Domestic-aviation",
                     "1A3ai_International-aviation" )
bunker_emissions <- final_emissions[ final_emissions$sector %in% shp_av_sectors, ]

# remove international shipping and aviation emissions
final_emissions <- final_emissions[ final_emissions$sector %!in% shp_av_sectors, ]

# add summary sectors
final_emissions$summary_sector <- Master_Sector_Level_map[match(final_emissions$sector,
                                  Master_Sector_Level_map$working_sectors_v1),'aggregate_sectors']
bunker_emissions$summary_sector <- Master_Sector_Level_map[match(bunker_emissions$sector,
                                  Master_Sector_Level_map$working_sectors_v1),'aggregate_sectors']

#simplify units
# This assumes units are correct. Need to add a more comprehensive unit conversion function (since drivers will come in various units)
final_emissions$units <- 'kt'
bunker_emissions$units <- 'kt'

#sum bunker emissions to global values
Bunker_global <- aggregate( bunker_emissions[ X_write_years ],
                            by=list( em = bunker_emissions$em,
                                     fuel = bunker_emissions$fuel,
                                     summary_sector = bunker_emissions$summary_sector,
                                     sector = bunker_emissions$sector,
                                     units = bunker_emissions$units ), sum )
Bunker_global$iso <- "global"

# reorder columns
final_emissions <- final_emissions[,c("iso", "summary_sector", "sector", "fuel","em","units",X_write_years)]
bunker_emissions <- bunker_emissions[,c("iso", "summary_sector","sector", "fuel","em","units",X_write_years)]

writeData( bunker_emissions, "MED_OUT", paste0( "S.", em, "_bunker_emissions" ),  meta = F )

final_emissions <- rbind( final_emissions, Bunker_global )

# ---------------------------------------------------------------------------
# 2. Produce summary outputs
# NOTE: This block should not write any summary outputs. Put any writeData()
# in function writeSummary() in block 3.

#Total emissions by Country
Em_by_Country<-aggregate(final_emissions[X_write_years],
                         by=list(iso=final_emissions$iso,
                                 em= final_emissions$em,
                                 units=final_emissions$units),sum )

#Sort
Em_by_Country <- Em_by_Country[ with( Em_by_Country, order( iso ) ), ]

#Total emissions by fuel
Summary_Emissions <- aggregate(final_emissions[X_write_years],
                               by=list(fuel=final_emissions$fuel,
                                       em= final_emissions$em,
                                       units=final_emissions$units),sum )

#Sort
Summary_Emissions <- Summary_Emissions[ with( Summary_Emissions, order( fuel ) ), ]

# Total Emissions by Sector and Country
Em_by_Country_Sector <- aggregate(final_emissions[X_write_years],
                               by=list(iso=final_emissions$iso,
                                       sector=final_emissions$summary_sector,
                                       em= final_emissions$em,
                                       units=final_emissions$units),sum )
#Sort
Em_by_Country_Sector <- Em_by_Country_Sector[ with( Em_by_Country_Sector, order( iso , sector ) ), ]

# Create files for emissions by country and CEDS sector
if ( WRITE_CEDS_SECTORS ) {
	# Aggregate emissions by CEDS sector, country, and species total
    Em_by_Country_CEDS_Sector <- final_emissions %>%
        dplyr::group_by( iso, sector, em, units ) %>%
        dplyr::summarise_at( vars( X_write_years ), sum ) %>%
        dplyr::arrange( iso, sector )

    Em_by_CEDS_Sector <- Em_by_Country_CEDS_Sector %>%
        dplyr::group_by( sector, em, units ) %>%
        dplyr::summarise_at( vars( X_write_years ), sum )

    Em_global_total <- Em_by_CEDS_Sector %>%
        dplyr::group_by( em, units ) %>%
        dplyr::summarise_at( vars( X_write_years ), sum )

	# Define the interval years
	global_sector_years <- paste0( "X", c( seq( 1750, 1950, 50 ),
	                                       seq( 1960, 2010, 10 ), end_year ) )

	# Create global_EM_emissions_by_CEDS_sector files
	fname <- paste0( "diagnostics/global_", em, "_emissions_by_CEDS_sector" )
	tidyr::gather( Em_by_CEDS_Sector, "year", "value", X_write_years ) %>%
	dplyr::filter( year %in% global_sector_years ) %>%
	writeData( "FIN_OUT", fname, meta = F )

	# Create global_total_emissions_for_EM files
	fname <- paste0( "../final-emissions/diagnostics/global_total_emissions_for_", em, ".Rd" )
	names( Em_global_total ) <- sub( "X", "", names( Em_global_total) )
    saveRDS( Em_global_total, file = fname ) # Save as R data object
}

# ---------------------------------------------------------------------------
# 3. Write summary and diagnostics outputs
# Compare emissions summary from the current run and the last run. If values
# change over a threshold, move last-run files to previous-versions, write out
# current-run files, and write out comparison diagnostics.

# Create output folders (if not already exist) and define values
  dir.create( "../final-emissions/current-versions", showWarnings = F )
  dir.create( "../final-emissions/previous-versions", showWarnings = F )
  dir.create( "../final-emissions/diagnostics", showWarnings = F )
  base_fn <- paste0( "CEDS_", em ,"_emissions_by_country_sector" )
  summary_fn <- paste( "CEDS", em , "emissions_by_country_sector", version_stamp, sep = "_" )
  summary_fn1 <- paste( "CEDS", em , "emissions_by_country", version_stamp, sep = "_" )
  summary_fn2 <- paste( "CEDS", em , "global_emissions_by_fuel", version_stamp, sep = "_" )
  summary_fn3 <- paste( "CEDS", em , "emissions_by_country_CEDS_sector", version_stamp, sep = "_" )
  summary_fn4 <- paste( "CEDS", em , "global_emissions_by_CEDS_sector", version_stamp, sep = "_" )
  THRESHOLD_PERCENT <- 1

# Define function to write summary files
  writeSummary <- function() {
    printLog( "Write emissions summary" )
    writeData( Em_by_Country_Sector, "FIN_OUT", summary_fn, domain_extension = "current-versions/", meta = F )
    writeData( Em_by_Country, "FIN_OUT", summary_fn1, domain_extension = "current-versions/", meta = F )
    writeData( Summary_Emissions, "FIN_OUT", summary_fn2, domain_extension = "current-versions/", meta = F )
    if ( WRITE_CEDS_SECTORS ) {
      writeData( Em_by_Country_CEDS_Sector, "FIN_OUT", summary_fn3, domain_extension = "current-versions/", meta = F )
      writeData( Em_by_CEDS_Sector, "FIN_OUT", summary_fn4, domain_extension = "current-versions/", meta = F )
    }
  }

# If no summary file exists, write out current-run files and exit
if ( length( list.files( "../final-emissions/current-versions/", pattern = paste0( "_", em, "_" ) ) ) == 0 ) {
  writeSummary()

# Else compare current-run and last-run emissions summary
} else {
  printLog( "Comparing emissions summary from current run and last run..." )

  # move last-run files to a temp folder [em]_last-run
  dir.create( paste0( "../final-emissions/", em, "_last-run" ), showWarnings = F )
  fl <- list.files( "../final-emissions/current-versions/", pattern = paste0( "_", em, "_" ), full.names = T )
  moveFileList( fl, paste0( "../final-emissions/", em, "_last-run/" ) )

  # write out current-run
  writeSummary()

  # read current-run and last-run emissions summary
  em_current <- readData( "FIN_OUT", summary_fn, domain_extension = "current-versions/", meta = F )
  em_last_fn <- list.files( paste0( "../final-emissions/", em, "_last-run/" ), pattern = base_fn )
  stopifnot( length( em_last_fn ) == 1 )
  em_last <- readData( "FIN_OUT", paste0( em, "_last-run/", em_last_fn ), meta = F )
  id_cols <- names( em_current )[ !grepl( "X", names( em_current ) ) ]
  id_cols_last <- names( em_last )[ !grepl( "X", names( em_last ) ) ]

  # if current-run and last-run have different ID columns, do nothing
  if ( any( sort( id_cols ) != sort( id_cols_last ) ) ) {
    warning( paste( "Current and last versions of", base_fn,
                     "have different ID columns. Cannot run comparison." ) )

  # if current-run and last-run are identical, delete current-run and move last-run to current-versions/
  } else if ( identical( em_current, em_last ) ) {
    warning("**************************************************************************" )
    warning( paste( base_fn, "did not change from last run." ) )
    warning("**************************************************************************" )
    unlink( dir( "../final-emissions/current-versions/",
                 pattern = paste0( "_", em, "_" ), full.names = T ) )
    fl <- list.files( paste0( "../final-emissions/", em, "_last-run/" ), pattern = paste0( "_", em, "_" ), full.names = T )
    moveFileList( fl, "../final-emissions/current-versions/" )

  # else run comparison diagnostics
  } else {
    # delete relevant emissions from previous-versions and diagnostics
      unlink( dir( "../final-emissions/previous-versions/",
                   pattern = paste0( "_", em, "_" ), full.names = T ) )
      unlink( dir( "../final-emissions/diagnostics/",
                   pattern = paste0( "_", em, "_" ), full.names = T ) )

    # move content of last-run to previous-versions
      fl <- list.files( paste0( "../final-emissions/", em, "_last-run" ), full.names = T )
      moveFileList( fl, "../final-emissions/previous-versions/" )

    # make df of added/dropped data
      dropped_rows <- em_last[ do.call( paste0, em_last[ id_cols ] ) %!in%
                                do.call( paste0, em_current[ id_cols ] ), ]
      added_rows <- em_current[ do.call( paste0, em_current[ id_cols ] ) %!in%
                              do.call( paste0, em_last[ id_cols ] ), ]
      dropped_cols <- em_last[, names( em_last ) %in% id_cols |
                                    names( em_last ) %!in% names( em_current ) ]
      added_cols <- em_current[, names( em_current ) %in% id_cols |
                                 names( em_current ) %!in% names( em_last ) ]

    # compare current-run and last-run
      em_current <- melt( em_current, id = id_cols )
      names( em_current )[ names( em_current ) %in% c( "variable", "value" ) ] <-
        c( "year", "current" )

      em_current_total <- em_current %>%
        dplyr::mutate( sector = "Total" ) %>%
        dplyr::group_by( iso, sector, em, units, year ) %>%
        dplyr::summarize_all( funs( sum( ., na.rm = TRUE ) ) )

      em_current <- em_current %>%
          dplyr::bind_rows( em_current_total ) %>%
          dplyr::arrange( iso, year, sector )

      em_last <- melt( em_last, id = id_cols )
      names( em_last )[ names( em_last ) %in% c( "variable", "value" ) ] <-
        c( "year", "previous" )

      em_last_total <- em_last %>%
          dplyr::mutate( sector = "Total" ) %>%
          dplyr::group_by( iso, sector, em, units, year ) %>%
          dplyr::summarize_all( funs( sum( ., na.rm = TRUE ) ) )

      em_last <- em_last %>%
          dplyr::bind_rows( em_last_total ) %>%
          dplyr::arrange( iso, year, sector )

      em_comp <- merge( em_current, em_last ) %>%
        mutate( diff = current - previous,
                change_percent = diff*100 / current )
      em_comp$change_percent[ em_comp$current == em_comp$previous ] <- 0

    # make df of where current-run and last-run differ by more than THRESHOLD_PERCENT
      em_comp_out <- filter( em_comp, abs( change_percent ) >= THRESHOLD_PERCENT )

    # make df of absolute difference in wide format
      x_var <- id_cols[ "year" %!in% id_cols ] %>%
        lapply( function(x) paste0( x, "+" ) ) %>% paste( collapse = '' )
      x_var <- substr( x_var, 0, nchar( x_var ) - 1 )
      abs_diff <- filter( em_comp, diff != 0 ) %>%
        cast( as.formula( paste( x_var, "year", sep = "~" ) ), value = "diff" )

    # make df of absolute difference as percentage of country total
      abs_diff_percent <- filter( em_comp, diff != 0 ) %>%
        mutate( diff_percent = round( diff*100 / current ) ) %>%
        cast( as.formula( paste( x_var, "year", sep = "~" ) ), value = "diff_percent" )

    # write out diagnostics
      if ( nrow( dropped_rows ) > 0 )
        writeData( dropped_rows, "FIN_OUT", paste0( "diagnostics/", summary_fn, "_dropped-rows" ), meta = F )
      if ( nrow( added_rows ) > 0 )
        writeData( added_rows, "FIN_OUT", paste0( "diagnostics/", summary_fn, "_added-rows" ), meta = F )
      if ( ncol( dropped_cols ) > length( id_cols ) )
        writeData( dropped_cols, "FIN_OUT", paste0( "diagnostics/", summary_fn, "_dropped-cols" ), meta = F )
      if ( ncol( added_cols ) > length( id_cols ) )
        writeData( added_cols, "FIN_OUT", paste0( "diagnostics/", summary_fn, "_added-cols" ), meta = F )
      if ( nrow( em_comp_out ) > 0 )
        writeData( em_comp_out, "FIN_OUT", paste0( "diagnostics/", summary_fn, "_comparison" ), meta = F )
      if ( !all( em_comp$diff == 0 ) ) {
        abs_diff[ is.na( abs_diff ) ] <- ""
        abs_diff_percent[ is.na( abs_diff_percent ) ] <- ""
        writeData( abs_diff, "FIN_OUT", paste0( "diagnostics/", summary_fn, "_diff" ), meta = F )
        writeData( abs_diff_percent, "FIN_OUT", paste0( "diagnostics/", summary_fn, "_diff-percent" ), meta = F )
      }
  }

  # delete the temp folder last-run
  unlink( paste0( "../final-emissions/", em, "_last-run" ), recursive = T )
}

# ---------------------------------------------------------------------------

# source figure and comparison files to print figures
# source('../code/diagnostic/Figures.R')

if (em != 'CO2')  source('../code/diagnostic/Compare_to_RCP.R')
if( em %!in% c( 'CO2', 'NH3' ) )  source('../code/diagnostic/Compare_to_GAINS.R')

logStop()
