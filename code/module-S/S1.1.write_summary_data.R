# ------------------------------------------------------------------------------
# Program Name: S1.1.write_summary_data.R 
# Author: Rachel Hoesly, Steve Smith, Linh Vu, Presley Muwan
# Date Last Updated: 31 May 2017
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
if ( is.na( em ) ) em <- "CO"

#load xlsx library
library("xlsx")

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


# -----------------------------------------------------------------------------------
# format_xlsx_numeric_data
# Brief:    This function formats numeric data cells of an xlsx workbook
# Details: This function formats the numeric values of an xlsx cell to use comma seperator
#           for values greater than 1000, and hides the decimal places of values greater than 0. 
# Dependencies: None
# Author(s): Presley Muwan  
# Params:   workbook - the xlsl file to be formatted (use loadworkbook(filepath) function from xlsx package
#                       to extract the xlsl workbook)
#           sheetName - name of the sheet, within the workbook, whose cells will be formated
#           rowIndex  - Index (indices) of the row(s) to be formatted
#           columnIndext - index (indices) of the column(s) to be formatted
#        
# Return: xlsx_workbook  
# Input Files:  none
# Output Files: none
format_xlsx_numeric_data <- function (workbook, sheetName, rowIndex, columnIndext){
  
  # style to use for cells whose value is greater than one
  comma_seperator_style <- xlsx::CellStyle(workbook, dataFormat = DataFormat("#,###0"))
  
  # style to use for cells with zero value
  highlight_zoro_style <- xlsx::CellStyle(workbook, font = Font(workbook, color = "#BDBDBD"))
  
  # get the sheet from the workbook (xlsx file)
  sheet <- xlsx::getSheets(workbook)[[sheetName]]
  
  # get the rows and rows' cells, along with their values 
  rows <- xlsx::getRows(sheet, rowIndex)
  cells <- xlsx::getCells(rows,columnIndext)
  cell_values <- lapply(cells, xlsx::getCellValue)
  
  # variables to hold index of cells whose value are greater than one,
  # and less than one, respectively
  cells_greater_than_one <- '<1'
  cell_less_than_one <- ">1"
  
  # seperate the cells based on their values and store then in their respective vectors 
  for ( index in names(cell_values) ) {
     cell_value <- as.numeric(cell_values[index])
     if ( cell_value > 0  & !is.na(cell_value) ) {
        cells_greater_than_one <- c(cells_greater_than_one, index)
     }else if( cell_value == 0 & !is.na(cell_value) ){
        cell_less_than_one  <- c( cell_less_than_one, index )
     }#else if    
  }#for ends 
  
  # remove the value at the first index of each vector
  cells_greater_than_one <- cells_greater_than_one[-1]
  cell_less_than_one <- cell_less_than_one[-1]
  
  # apply the formatting style to the cells 
  lapply(names(cells[cells_greater_than_one]),
         function( cell_index) xlsx::setCellStyle(cells[[cell_index]],comma_seperator_style) )
  lapply(names( cells[cell_less_than_one] ),
         function( cell_index ) xlsx::setCellStyle(cells[[cell_index]], highlight_zoro_style) )
  
  #return the workbook
  return( workbook )
}#format_xlsx_data() Ends 


# -----------------------------------------------------------------------------------
# create_tab_of_global_emission_by_sector
# Brief:  This function creates data tabs (or sheets) containing emission by ceds sector
#         within an xlsx file    
# Details: The tab (sheet) created has the name "year" (specified by the parameter). 
#          The tabe is formated to have emission species as columns names and CEDS sectors 
#          as row names 
# Dependencies: None
# Author(s): Presley Muwan  
# Params:   year - the year from which data is extracted to create the tab (or sheet)
#           Em_by_CEDS_Sector_tabs - Long formated data frame will emission data arranged 
#                                    for all CEDS sector and all years 
#        
# Return: none  
# Input Files:  global_emissions_by_CEDS_sector.xlsx - if it exist
# Output Files: global_emissions_by_CEDS_sector.xlsx 
create_tab_of_global_emission_by_sector <- function( year , Em_by_CEDS_Sector_tabs ){
  
  #select dataset only for this year
  emission_tab <- Em_by_CEDS_Sector_long[ which(Em_by_CEDS_Sector_long$year == year ),]

  #get emission type
  emission <- unique( emission_tab$em )
  
  #get the tab (sheet) name (year)
  tab_name <- gsub( "x", "", year, ignore.case = T ) 
  
  #re-arrange dataframe columns
  names(emission_tab) <- c( "sector", "em","units","year", emission )
  
  #remove the 'em' and the 'year' column
  emission_tab <- select( emission_tab, -em, -year, -units )
  
  #------------------------if file exist ------------------------#
  #check if file exist
  global_em_workbook_path <- "../final-emissions/diagnostics/global_emissions_by_CEDS_sector.xlsx"
  if( file.exists( global_em_workbook_path ) ){
    
    #retrieve excel file and extract its sheets
    global_em_workbook <- xlsx::loadWorkbook(global_em_workbook_path)
    sheets <- getSheets(global_em_workbook)
    
    #append data to existing sheet
    if( tab_name %in% names(sheets) ){
      
      #read the global_em_by_CEDS_sector file
      #IMPORTANT: Using the readData function resulted to and error; "Error during wrapup: Couldn't find 'xl/worksheets/sheet1.xml"
      #global_em_by_CEDS_sector <- readData( domain = "FIN_OUT", file_name = 'global_emissions_by_CEDS_sector',
      #                                    domain_extension = "diagnostics/", extension = ".xlsx", sheet_selection = tab_name )
      printLog( "Reading sheet: '",tab_name,"' from", global_em_workbook_path ) 
      global_em_by_CEDS_sector <- xlsx::read.xlsx( file = global_em_workbook_path, sheetName = tab_name )
      
      #remove old emission data and replace with the current one
      if( em %in% colnames(global_em_by_CEDS_sector) ){
          global_em_by_CEDS_sector <- select( global_em_by_CEDS_sector, -contains(em) )
      }#if ends 
      
      #add new column to exisiting data 
      emission_tab <- full_join( global_em_by_CEDS_sector, emission_tab, by = c("sector") ) %>% 
                      mutate( units = "kt" ) #add the "unit" column and initialize it with "kt"
      
      em_species <- names(emission_tab)[(names(emission_tab) %!in% c("sector", "units"))]
      emission_tab <- emission_tab[ c( c("sector", "units", em_species) ) ]
      
      #delete the sheet for this year (to prevent conflicting error when writing new data)
      xlsx::removeSheet( global_em_workbook, sheetName = tab_name )
      xlsx::saveWorkbook( global_em_workbook,global_em_workbook_path )
      
    }#if Ends 
    
  }#if Ends 
  
  #write out the data 
  printLog( "Writing sheet : '",tab_name,"' to", global_em_workbook_path )
  xlsx::write.xlsx(emission_tab,global_em_workbook_path, sheetName=tab_name, append=T, row.names = F)
  
  #format data; remove decimal points and use comma sperator 
  global_em_workbook <- xlsx::loadWorkbook(global_em_workbook_path)
  global_em_workbook  <- format_xlsx_numeric_data( global_em_workbook, tab_name, 
                                                   rowIndex = 2:(nrow(emission_tab)+1),
                                                   columnIndext = 3:ncol(emission_tab) )
  xlsx::saveWorkbook( global_em_workbook, global_em_workbook_path )
  
}#create_tab_of_global_emission_by_sector() Ends 


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

# remove sectors that are not supplied in CEDS
empty_sectors <- c( "11A_Volcanoes", "11B_Forest-fires", "11C_Other-natural" )
final_emissions <- final_emissions[ -which( final_emissions$sector %in% empty_sectors ) , ]

# save shipping and aviation emissions
bunker_emissions <- final_emissions[ which( final_emissions$sector %in% 
                    c( "1A3ai_International-aviation", "1A3aii_Domestic-aviation",
                       "1A3di_International-shipping" ) ) , ]

# remove international shipping and aviation emissions
final_emissions <- final_emissions[ -which( final_emissions$sector %in% 
                   c( "1A3ai_International-aviation", "1A3aii_Domestic-aviation",
                      "1A3di_International-shipping" ) ) , ]

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

	# Global Emissions by CEDS Sector 
	Em_by_CEDS_Sector <- aggregate(final_emissions[X_write_years],
								   by=list(sector=final_emissions$sector,
										   em= final_emissions$em,
										   units=final_emissions$units),sum )
	#Sort
	Em_by_CEDS_Sector <- Em_by_CEDS_Sector[ with( Em_by_CEDS_Sector, order( sector ) ), ]

	#define the interval years 
	all_years <- c(paste0("X",1750+(50*(0:4))), paste0("X",c(1950+(10*(1:6)),2014)))
	
	Em_by_CEDS_Sector_long <- melt( Em_by_CEDS_Sector, id = c("sector", "em", "units"), variable_name = "year" )
	
	#create  global_emission_by_sector xlsx workbook with tabs 
	lapply(all_years, FUN = create_tab_of_global_emission_by_sector, Em_by_CEDS_Sector_long)
	
	#freeze row names (sector column) and years (column headers)
	global_em_wb_sheets <- c(paste0("",1750+(50*(0:4))), paste0("",c(1950+(10*(1:6)),2014)))
	global_em_workbook_path <- "../final-emissions/diagnostics/global_emissions_by_CEDS_sector.xlsx"
	global_em_workbook <- XLConnect::loadWorkbook(global_em_workbook_path)
	lapply(global_em_wb_sheets, function(sheet){
	  XLConnect::createFreezePane(global_em_workbook, sheet,2, 2 )
	  XLConnect::setColumnWidth( global_em_workbook,sheet,1, width = -1 )
	  XLConnect::saveWorkbook(global_em_workbook, global_em_workbook_path )})
	
	#Global Emmission by specie 
	global_total_emission <- aggregate( final_emissions[X_write_years],
	                                    by=list(em= final_emissions$em,
	                                    units=final_emissions$units),sum )
	
	#remove 'X' from  global_total_emission header 
	xColumnYears <- names(global_total_emission)[names( global_total_emission ) %!in% c( "em","units" )]
	columnYears <- sapply(xColumnYears, FUN = function( xColumnYear ){gsub("X","",xColumnYear, ignore.case = T)} )
	names(global_total_emission) <- c("em", "units", columnYears)
	
	#Read global_total_emission_for_species file (if it exist) and append the new specie record to it
	global_total_emission_for_species_path <- "../final-emissions/diagnostics/global_total_emission_for_species.xlsx"
	global_em_for_species_sheet <- "global_total_emission"
	if( file.exists( global_total_emission_for_species_path ) ){
	 
	  global_total_emission_for_species <- readData( domain = "FIN_OUT", file_name = 'global_total_emission_for_species',
	                                                domain_extension = "diagnostics/" ,extension = ".xlsx", 
	                                                sheet_selection = global_em_for_species_sheet )
	  #remove sheet to avoid write-coanflict
	  global_em_workbook <- loadWorkbook( global_total_emission_for_species_path )
	  removeSheet( global_em_workbook, sheetName = global_em_for_species_sheet )
	  saveWorkbook( global_em_workbook,global_total_emission_for_species_path )
	  
	  #remove existing specie row
	  global_total_emission_for_species <- global_total_emission_for_species[ which( global_total_emission_for_species$em != em),]
	  
	  #add the em specie's record to the data frame
	  global_total_emission <- dplyr::bind_rows( global_total_emission_for_species, global_total_emission )
	  
	}#if Ends 
	
	#write out global_total_emission data 
	printLog( "Writing ", global_total_emission_for_species_path )
	write.xlsx(global_total_emission,global_total_emission_for_species_path, 
	           sheetName= global_em_for_species_sheet, append=F, row.names = F )
	
	#freeze row names (sector column) and years (column headers)
	global_total_emission_wb <- XLConnect::loadWorkbook(global_total_emission_for_species_path)
	XLConnect::createFreezePane( global_total_emission_wb, global_em_for_species_sheet,2, 2 )
	XLConnect::saveWorkbook( global_total_emission_wb, global_total_emission_for_species_path )
	
	#format global_total_emission_for_species; remove decimal points and use comma sperator for values greateer than 1
	#For values less than 1, show only two decimal places 
	global_total_emission_wb <- loadWorkbook(global_total_emission_for_species_path)
	global_total_emission_wb  <- format_xlsx_numeric_data( global_total_emission_wb, global_em_for_species_sheet, 
	                                                       rowIndex = 2:(nrow(global_total_emission)+1),
	                                                       columnIndext = 3:ncol(global_total_emission) )
	#safe global_total_emission_wb
	saveWorkbook( global_total_emission_wb, global_total_emission_for_species_path )

}#if Ends 

# ---------------------------------------------------------------------------
# 3. Write summary and diagnostics outputs
# Compare emissions summary from the current run and the last run. If values 
# change over a threshold, move last-run files to previous-versions, write out 
# current-run files, and write out comparison diagnostics.
  FILENAME_POSTSCRIPT <- paste( "_v", substr( Sys.Date(), 6, 7 ), substr( Sys.Date(), 9, 10 ), 
                                substr( Sys.Date(), 1, 4 ), sep = "_" )  # "_v_mm_dd_yyyy"

# Create output folders (if not already exist) and define values
  dir.create( "../final-emissions/current-versions", showWarnings = F )
  dir.create( "../final-emissions/previous-versions", showWarnings = F )
  dir.create( "../final-emissions/diagnostics", showWarnings = F )
  base_fn <- paste0( "CEDS_", em ,"_emissions_by_country_sector" )
  summary_fn <- paste0( "CEDS_", em , "_emissions_by_country_sector", FILENAME_POSTSCRIPT )
  summary_fn1 <- paste0( "CEDS_", em , "_emissions_by_country", FILENAME_POSTSCRIPT )
  summary_fn2 <- paste0( "CEDS_", em , "_global_emissions_by_fuel", FILENAME_POSTSCRIPT )
  summary_fn3 <- paste0( "CEDS_", em , "_emissions_by_country_CEDS_sector", FILENAME_POSTSCRIPT )
  summary_fn4 <- paste0( "CEDS_", em , "_global_emissions_by_CEDS_sector", FILENAME_POSTSCRIPT )
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
  printLog( "Compare emissions summary from current run and last run" )
  
  # move last-run files to a temp folder [em]_last-run
  dir.create( paste0( "../final-emissions/", em, "_last-run" ), showWarnings = F )
  fl <- list.files( "../final-emissions/current-versions/", pattern = paste0( "_", em, "_" ), full.names = T )
  moveFileList( fl, paste0( "../final-emissions/", em, "_last-run/" ) )

  # write out current-run
  writeSummary()
  
  # read current-run and last-run emissions summary
  em_current <- readData( "FIN_OUT", summary_fn, domain_extension = "current-versions/", meta = F )
  em_last_fn <- list.files( paste0( "../final-emissions/", em, "_last-run/" ), pattern = base_fn ) %>% file_path_sans_ext()
  em_last <- readData( "FIN_OUT", paste0( em, "_last-run/", em_last_fn ), meta = F )
  id_cols <- names( em_current )[ !grepl( "X", names( em_current ) ) ]
  id_cols_last <- names( em_last )[ !grepl( "X", names( em_last ) ) ]
  
  # if current-run and last-run have different ID columns, do nothing
  if ( any( sort( id_cols ) != sort( id_cols_last ) ) ) {
    warning( paste( "Current and last versions of", base_fn, 
                     "have different ID columns. Cannot run comparison." ) )
  
  # if current-run and last-run are identical, delete current-run and move last-run to current-versions/
  } else if ( identical( em_current, em_last ) ) {
    warning( paste( base_fn, "did not change from last run." ) )
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
      em_last <- melt( em_last, id = id_cols )
      names( em_last )[ names( em_last ) %in% c( "variable", "value" ) ] <-
        c( "year", "previous" ) 
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
source('../code/diagnostic/Figures.R') 
  
if (em != 'CO2')  source('../code/diagnostic/Compare_to_RCP.R')
if( em %!in% c( 'CO2', 'NH3' ) )  source('../code/diagnostic/Compare_to_GAINS.R')

logStop()

# END










