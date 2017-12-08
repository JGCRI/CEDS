# ----------------------------------------------------------------------------------
# CEDS R header file: File Summary functions
# Author(s): Presley Muwan
# Last Updated: 19 June 2017

# This file must be sourced by CEDS summary R scripts
# Functions contained:
#   format_xlsx_numeric_data, create_tab_of_global_emission_by_sector, update_readme_sheet

# Notes: This functions are specifically written to serve CEDS Summary script(s) in Module S

# -----------------------------------------------------------------------------
# Load Packages
loadPackage('openxlsx')

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
#           rowIndex  - Index of the row to be formatted
#           columnIndext - index  of the column to be formatted
#        
# Return: xlsx_workbook  
# Input Files:  none
# Output Files: none
format_xlsx_numeric_data <- function ( workbook, rowIndices=c(2:58),  columnIndices){

  # style to use for cells whose value is greater than one
  comma_seperator_style <- openxlsx::createStyle( numFmt = "NUMBER")
  
  # style to use for cells with zero value
  highlight_zoro_style <- openxlsx::createStyle(fontColour = "#BDBDBD" )
  
  # top border style to be added to the last row ('Total' row)
  topborder <- openxlsx::createStyle( border = "top",borderStyle = 'thin')
  sheets <- names(workbook)
  # apply conditional formating to each sheet 
  for(sheet in sheets){
    # get the rows and rows' cells, along with their values 
    openxlsx::conditionalFormatting(workbook, sheet , cols=columnIndices, rows = rowIndices, rule="==0", style = highlight_zoro_style)
    openxlsx::conditionalFormatting(workbook, sheet , cols=columnIndices, rows = rowIndices, rule=">0", style = comma_seperator_style)
    
    # add freeze pane 
    openxlsx::freezePane(workbook, sheet, firstActiveRow = 2, firstActiveCol = 3)
    
    # add top border to the last row ('Total' row)
    openxlsx::addStyle( workbook, sheet , cols=c(1,2,columnIndices), rows = 58, style = topborder)
    
  }#For Ends 
  #return the workbook
  return(workbook)
  
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
#           Em_by_CEDS_Sector_tabs - Long formated data frame with emission data arranged 
#                                    for all CEDS sector and all years 
#        
# Return: none  

# Input Files:  global_emissions_by_CEDS_sector.xlsx - if it exist
# Output Files: global_emissions_by_CEDS_sector.xlsx
create_tab_of_global_emission_by_sector <- function( year , Em_by_CEDS_Sector_tabs ){
  #year <- "X1750"
  #Em_by_CEDS_Sector_tabs <- Em_by_CEDS_Sector_long

  #select dataset only for this year
  emission_tab <- Em_by_CEDS_Sector_tabs[ which(Em_by_CEDS_Sector_tabs$year %in% year ),]
  
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
    
    global_em_workbook <- openxlsx::loadWorkbook(global_em_workbook_path)
    
    sheets <- names(global_em_workbook)
    #append data to existing sheet
    if( tab_name %in% sheets ){
      
      #read the global_em_by_CEDS_sector file
      printLog( "Reading sheet: '",tab_name,"' from", global_em_workbook_path ) 
     
       # global_em_by_CEDS_sector <- xlsx::read.xlsx( file = global_em_workbook_path, sheetName = tab_name )
      global_em_by_CEDS_sector <- openxlsx::read.xlsx( xlsxFile = global_em_workbook_path, sheet = tab_name )
      

      global_em_by_CEDS_sector <- select( global_em_by_CEDS_sector,-contains("NA.") )

      #remove old emission data and replace with the current one
      if( em %in% colnames(global_em_by_CEDS_sector) ){
        global_em_by_CEDS_sector <- select( global_em_by_CEDS_sector, -contains(em) )
      }#if ends

      #add new column to exisiting data
      emission_tab <- full_join( global_em_by_CEDS_sector, emission_tab, by = c("sector") ) %>%
        dplyr::mutate( units = "kt" ) #add the "unit" column and initialize it with "kt"

      em_species <- names(emission_tab)[(names(emission_tab) %!in% c("sector", "units"))]
      emission_tab <- emission_tab[ c( c("sector", "units", em_species) ) ]

      #delete the sheet for this year (to prevent conflicting error when writing new data)

      openxlsx::removeWorksheet( global_em_workbook, sheet = tab_name )
      
      #TODO:check if FileOverride boolean can do the job of the line above 
      openxlsx::saveWorkbook( global_em_workbook, global_em_workbook_path, overwrite = T )
    
    }
    
    openxlsx::addWorksheet(global_em_workbook, tab_name)
    
    
  }else{
    global_em_workbook <- openxlsx::createWorkbook() 
    openxlsx::addWorksheet(global_em_workbook, tab_name)
  } 
  
  
  #write out the data 
  printLog( "Writing sheet : '",tab_name,"' to", global_em_workbook_path )
  
  openxlsx::writeData(x=emission_tab, wb=global_em_workbook, sheet = tab_name)
  
  # update workbook's cell conditional formatting 
  formatted_workbook <- format_xlsx_numeric_data( workbook=global_em_workbook, columnIndices = 2:ncol(emission_tab))
  
  openxlsx::saveWorkbook( formatted_workbook, file=global_em_workbook_path, overwrite = T )
  
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
  old_sheets <- names(workbook)
  
  #initialize CEDS_version column 
  ceds_version_df <- data.frame(CEDS_Version = c(CEDS_version), Project_Website = c(website))

 
  #check if tab exist
  if( length(old_sheets) < 2 ){
    #create new sheet

    readme_sheet <-  openxlsx::addWorksheet(global_total_emission_wb, "README")
    
  }
  
  #add it if it does not exist 
  openxlsx::writeData(workbook, sheet = "README", x= ceds_version_df, startCol = 2)
  

  return(workbook)

}#updata_readme_sheet() Ends
