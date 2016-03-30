# ------------------------------------------------------------------------------
# Program Name: S1.1.write_summary_data.R
# Author: Rachel Hoesly, Steve Smith, Linh Vu
# Date Last Updated: 4 Mar 2016
# Program Purpose: Produces summary output
#               
# Output Files: data in final-emissions folder
# TODO: 
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers

# Set working directory
dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
for ( i in 1:length( dirs ) ) {
  setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
  wd <- grep( 'CEDS/input', list.dirs(), value = T )
  if ( length(wd) > 0 ) {
    setwd( wd[1] )
    break
    
  }
}
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R",'process_db_functions.R',
              'common_data.R', 'IO_functions.R', 'data_functions.R', 'timeframe_functions.R') # Additional function files may be required.
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

# Define functions to move a list of files (full path name)
moveFile <- function( fn, new_dir ) {
  fn_base <- basename( fn )
  file.rename( fn, paste0( new_dir, fn_base ) )
}

moveFileList <- function( fn_list, new_dir ) {
  lapply( fn_list, function( fn ) moveFile( fn, new_dir ) )
}

# Option to also write out data by CEDS sectors
WRITE_CEDS_SECTORS = TRUE

# writeSummary()  # defined in 3


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

# save shipping and aviation emissions
bunker_emissions <- final_emissions[ which( final_emissions$sector %in% 
                    c( "1A3ai_International-aviation", "1A3aii_Domestic-aviation",
                       "1A3di_International-shipping", "1A3dii_Domestic-naviation" ) ) , ]

# remove international shipping and aviation emissions
final_emissions <- final_emissions[ -which( final_emissions$sector %in% 
                   c( "1A3ai_International-aviation", "1A3aii_Domestic-aviation",
                      "1A3di_International-shipping", "1A3dii_Domestic-naviation" ) ) , ]

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

FILENAME_POSTSCRIPT = "_FOR-REVIEW-ONLY"

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

# Emissions by country and CEDS sector
if ( WRITE_CEDS_SECTORS ) {
	# Total Emissions by CEDS Sector and Country
	Em_by_Country_CEDS_Sector <- aggregate(final_emissions[X_write_years],
								   by=list(iso=final_emissions$iso,
										   sector=final_emissions$sector,
										   em= final_emissions$em,
										   units=final_emissions$units),sum )
	#Sort
	Em_by_Country_CEDS_Sector <- Em_by_Country_CEDS_Sector[ with( Em_by_Country_CEDS_Sector, order( iso , sector ) ), ]

    file_name <- paste0( em , "_em_country_CEDS_sector", FILENAME_POSTSCRIPT )
    writeData( Em_by_Country_CEDS_Sector, "FIN_OUT", file_name, meta = F )

	# Global Emissions by CEDS Sector 
	Em_by_CEDS_Sector <- aggregate(final_emissions[X_write_years],
								   by=list(sector=final_emissions$sector,
										   em= final_emissions$em,
										   units=final_emissions$units),sum )
	#Sort
	Em_by_CEDS_Sector <- Em_by_CEDS_Sector[ with( Em_by_CEDS_Sector, order( sector ) ), ]

    file_name <- paste0( em , "_gbl_em_by_CEDS_sector", FILENAME_POSTSCRIPT )
    writeData( Em_by_CEDS_Sector, "FIN_OUT", file_name, meta = F )

}

# ---------------------------------------------------------------------------
# 3. Write summary and diagnostics outputs
# Compare emissions summary from the current run and the last run. If values 
# change over a threshold, move last-run files to previous-versions, write out 
# current-run files, and write out comparison diagnostics.

# Create output folders (if not already exist) and define values
  dir.create( "../final-emissions/diagnostics", showWarnings = F )
  dir.create( "../final-emissions/previous-versions", showWarnings = F )
  summary_fn <- paste0( em , "_emissions_by_country_sector", FILENAME_POSTSCRIPT )
  summary_ctry_fn <- paste0( em , "_emissions_by_country", FILENAME_POSTSCRIPT )
  summary_global_fn <- paste0( em , "_global_emissions_by_fuel", FILENAME_POSTSCRIPT )
  diag_fn <- paste0( em ,"_emissions_by_country_sector" )
  THRESHOLD_PERCENT <- 1
  
# Define function to write summary files
  writeSummary <- function() {
    printLog( "Write emissions summary" )
    writeData( Em_by_Country_Sector, "FIN_OUT", summary_fn, meta = F )
    writeData( Em_by_Country, "FIN_OUT", summary_ctry_fn, meta = F )
    writeData( Summary_Emissions, "FIN_OUT", summary_global_fn, meta = F )
  }
  
# If no summary file exists, write out current-run files and exit
if ( !file.exists( paste0("../final-emissions/", summary_fn, ".csv" ) ) ) {
  writeSummary()

# Else compare current-run and last-run emissions summary
} else {
  printLog( "Compare emissions summary from current run and last run" )
  
  # move last-run files to a temp folder [em]_last-run
  dir.create( paste0( "../final-emissions/", em, "_last-run" ), showWarnings = F )
  fl <- list.files( "../final-emissions/", pattern = paste0( em, ".*", FILENAME_POSTSCRIPT ), full.names = T )
  moveFileList( fl, paste0( "../final-emissions/", em, "_last-run/" ) )

  # write out current-run
  writeSummary()
  
  # read current-run and last-run emissions summary
  em_current <- readData( "FIN_OUT", summary_fn, meta = F )
  em_last <- readData( "FIN_OUT", paste0( em, "_last-run/", summary_fn ), meta = F )
  id_cols <- names( em_current )[ !grepl( "X", names( em_current ) ) ]
  id_cols_last <- names( em_last )[ !grepl( "X", names( em_last ) ) ]
  
  # if current-run and last-run have different ID columns, do nothing
  if ( any( sort( id_cols ) != sort( id_cols_last ) ) ) {
    warning( paste0( "Current and last versions of ", summary_fn, 
                     ".csv have different ID columns. Cannot run comparison." ) )
  
  # if current-run and last-run are identical, do nothing
  } else if ( identical( em_current, em_last ) ) {
    warning( paste0( summary_fn, ".csv did not change from last run." ) )

  # else run comparison diagnostics
  } else {
    # delete relevant emissions from previous-versions and diagnostics
      unlink( dir( "../final-emissions/previous-versions/", 
                   pattern = paste0( em, ".*", FILENAME_POSTSCRIPT ), full.names = T ) )
      unlink( dir( "../final-emissions/diagnostics/", 
                   pattern = diag_fn, full.names = T ) )
      
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
      em_last <- melt( em_last, id = id_cols )
      names( em_last )[ names( em_last ) %in% c( "variable", "value" ) ] <-
        c( "year", "previous" ) 
      em_comp <- merge( em_current, em_last ) %>% 
        mutate( diff = current - previous,
                change_percent = diff*100/previous )
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
        writeData( dropped_rows, "FIN_OUT", paste0( "diagnostics/", diag_fn, "_dropped-rows" ), meta = F )
      if ( nrow( added_rows ) > 0 )
        writeData( added_rows, "FIN_OUT", paste0( "diagnostics/", diag_fn, "_added-rows" ), meta = F )
      if ( ncol( dropped_cols ) > length( id_cols ) )
        writeData( dropped_cols, "FIN_OUT", paste0( "diagnostics/", diag_fn, "_dropped-cols" ), meta = F )
      if ( ncol( added_cols ) > length( id_cols ) )
        writeData( added_cols, "FIN_OUT", paste0( "diagnostics/", diag_fn, "_added-cols" ), meta = F )
      if ( nrow( em_comp_out ) > 0 )
        writeData( em_comp_out, "FIN_OUT", paste0( "diagnostics/", diag_fn, "_comparison" ), meta = F )
      if ( !all( em_comp$diff == 0 ) ) {
        abs_diff[ is.na( abs_diff ) ] <- ""
        abs_diff_percent[ is.na( abs_diff_percent ) ] <- ""
        writeData( abs_diff, "FIN_OUT", paste0( "diagnostics/", diag_fn, "_diff" ), meta = F )
        writeData( abs_diff_percent, "FIN_OUT", paste0( "diagnostics/", diag_fn, "_diff-percent" ), meta = F )
      }
  }
  
  # delete the temp folder last-run
  unlink( paste0( "../final-emissions/", em, "_last-run" ), recursive = T )
}

# ---------------------------------------------------------------------------

# source figure and comparison files to print figures
# source('../code/diagnostic/Figures.R') 
# source('../code/diagnostic/Compare_to_RCP.R')
#  

logStop()

# END










