# ----------------------------------------------------------------------------------
# CEDS R header file: File Summary functions
# Author(s): Presley Muwan
# Last Updated: 19 June 2017

# This file must be sourced by CEDS summary R scripts
# Functions contained:
#   format_xlsx_numeric_data, create_tab_of_global_emission_by_sector, update_readme_sheet

# Notes: This functions are specifically written to serve CEDS Summary script(s) in Module S 

# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------------
# format_xlsx_numeric_data
# Brief:    This function formats numeric data cells of an xlsx workbook
# Details: This function formats the numeric values of an xlsx cell to use comma seperator
#           for values all values greater than 0. And grays out values equall 0. 
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
  cell_with_zero <- ">1"
  
  # seperate the cells based on their values and store then in their respective vectors 
  for ( index in names(cell_values) ) {
    
    cell_value <- as.numeric(cell_values[index])
    
    if( cell_value == 0 & !is.na(cell_value) ){
      cell_with_zero  <- c( cell_with_zero, index )
    }else cells_greater_than_one <- c(cells_greater_than_one, index)
  }#for ends 
  
  # remove the value at the first index of each vector
  cells_greater_than_one <- cells_greater_than_one[-1]
  cell_with_zero <- cell_with_zero[-1]
  
  # apply the formatting style to the cells 
  lapply(names(cells[cells_greater_than_one]),
         function( cell_index) xlsx::setCellStyle(cells[[cell_index]],comma_seperator_style) )
  lapply(names( cells[cell_with_zero] ),
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
  #year <- "X1750"
  #Em_by_CEDS_Sector_tabs <- Em_by_CEDS_Sector_long
  
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
  
  # add a row that holds the total sum of the emission data 
  em_year_total <- data.frame(sector="Total", emission=sum(emission_tab[emission]))
  names(em_year_total) <- names(emission_tab)
  emission_tab <- bind_rows(emission_tab, em_year_total)
  
  #------------------------if file exist ------------------------#
  #check if file exist
  global_em_workbook_path <- "../final-emissions/diagnostics/global_emissions_by_CEDS_sector.xlsx"
  if( file.exists( global_em_workbook_path ) ){
    
    #retrieve excel file and extract its sheets
    global_em_workbook <- xlsx::loadWorkbook(global_em_workbook_path)
    sheets <- xlsx::getSheets(global_em_workbook)
    
    #append data to existing sheet
    if( tab_name %in% names(sheets) ){
      
      #read the global_em_by_CEDS_sector file
      #IMPORTANT: Using the readData function resulted to and error; "Error during wrapup: Couldn't find 'xl/worksheets/sheet1.xml"
      #global_em_by_CEDS_sector <- readData( domain = "FIN_OUT", file_name = 'global_emissions_by_CEDS_sector',
      #                                    domain_extension = "diagnostics/", extension = ".xlsx", sheet_selection = tab_name )
      printLog( "Reading sheet: '",tab_name,"' from", global_em_workbook_path ) 
      global_em_by_CEDS_sector <- xlsx::read.xlsx( file = global_em_workbook_path, sheetName = tab_name )
      
      global_em_by_CEDS_sector <- select( global_em_by_CEDS_sector,-contains("NA.") )
                                          
      #remove old emission data and replace with the current one
      if( em %in% colnames(global_em_by_CEDS_sector) ){
        global_em_by_CEDS_sector <- select( global_em_by_CEDS_sector, -contains(em) )
      }#if ends 
      
      #add new column to exisiting data 
      emission_tab <- left_join( global_em_by_CEDS_sector, emission_tab, by = c("sector") ) %>% 
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



# -----------------------------------------------------------------------------------
# update_readme_sheet
# Brief:  this function adds a README sheet to a workbook   
# Details: The README Sheet contains the version number of the latest run and CEDS url website
# Dependencies: None
# Author(s): Presley Muwan  
# Params:   workbook - the targeted excel file
#           CEDS_version - The version of the current run
#           website - CEDS web url
#        
# Return: Excel workbook  
# Input Files:  any excel workbook
# Output Files: 
update_readme_sheet <- function(workbook, website, CEDS_version){
  
  #add a "README" tab to global_total_emission_wb
  old_sheets <- xlsx::getSheets(workbook)
  
  #initialize CEDS_version column 
  ceds_version_df <- data.frame(CEDS_Version = c(CEDS_version))
  
  #Initialize website data 
  ceds_web_df <- data.frame(Project_Website = c(website))
  
  #check if tab exist
  if(names(old_sheets) %!in% c("README")){
    #create new sheet
    readme_sheet <-  xlsx::createSheet(global_total_emission_wb, "README")
    
  }else readme_sheet <-  xlsx::getSheets(global_total_emission_wb, "README")
  
  #add it if it does not exist 
  xlsx::addDataFrame(ceds_web_df, readme_sheet, startColumn = 2)
  xlsx::addDataFrame(ceds_version_df, readme_sheet)
  
  return(workbook)
  
}#updata_readme_sheet() Ends 
