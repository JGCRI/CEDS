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
format_xlsx_numeric_data <- function ( workbook, rowIndices=c(2:58), columnIndices) {

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
# Brief:  This function creates data tabs (or sheets) containing emission by
#         ceds sector within an xlsx file
# Details: The tab (sheet) created has the name "year" (specified by the
#          parameter). The tab is formatted to have emission species as column
#          names and CEDS sectors as row names
# Dependencies: None
# Author(s): Presley Muwan, Caleb Braun
# Params:   year - the year from which data is extracted to create the tab (or sheet)
#           Em_by_CEDS_Sector_tabs - Long formated data frame with emission data arranged
#                                    for all CEDS sector and all years
#
# Return: none
# Input Files:  global_emissions_by_CEDS_sector.xlsx - if it exists
# Output Files: global_emissions_by_CEDS_sector.xlsx
create_tab_of_global_emission_by_sector <- function( year, Em_by_CEDS_Sector_tabs ) {

  # get emission type (e.g. "CO")
  emission <- unique( Em_by_CEDS_Sector_tabs$em )

  # select dataset only for this year and select sector and emission value
  emission_tab <- dplyr::filter( Em_by_CEDS_Sector_tabs, year == !!year ) %>%
      dplyr::mutate( !!emission := value ) %>%
      dplyr::select( sector, units, !!emission )

  # add a row that holds the total sum of the emission data
  emission_tab <- rbind( emission_tab, c( "Total", unique( emission_tab$units ),
                                          sum( emission_tab[emission] ) ) )

  # load the file or create a new workbook if it doesn't exist
  global_em_workbook_path <- "../final-emissions/diagnostics/global_emissions_by_CEDS_sector.xlsx"
  if( file.exists( global_em_workbook_path ) ) {
    global_em_workbook <- openxlsx::loadWorkbook( global_em_workbook_path )
  }
  else {
    global_em_workbook <- openxlsx::createWorkbook()
  }

  # the tab (sheet) name is just the year; remove leading 'X'
  tab_name <- substr( year, 2, 5 )

  # join data to existing sheet if it exists
  if( tab_name %in% names( global_em_workbook ) ) {
    printLog( "Reading sheet: '", tab_name, "' from", global_em_workbook_path )
    global_em_by_CEDS_sector <- openxlsx::read.xlsx( global_em_workbook, sheet = tab_name )
    global_em_by_CEDS_sector[emission] <- emission_tab[emission]
    emission_tab <- global_em_by_CEDS_sector
  }
  else {
    openxlsx::addWorksheet(global_em_workbook, tab_name)
  }

  # write out the data
  printLog( "Writing sheet: '", tab_name, "' to", global_em_workbook_path )
  openxlsx::writeData( global_em_workbook, tab_name, emission_tab )

  # update workbook's cell conditional formatting
  printLog( "Updating fmat: '", tab_name, "' to", global_em_workbook_path )
  formatted_workbook <- format_xlsx_numeric_data( workbook = global_em_workbook,
                                                  columnIndices = 2:ncol( emission_tab ) )

  printLog( "Saving sheet : '", tab_name, "' to", global_em_workbook_path )
  openxlsx::saveWorkbook( formatted_workbook, file=global_em_workbook_path, overwrite = T )

} #create_tab_of_global_emission_by_sector() Ends


# -----------------------------------------------------------------------------------
# update_readme_sheet
# Brief:  this function adds a README sheet to a workbook
# Details: The README Sheet contains the version number of the latest run and CEDS url website
# Dependencies: None
# Author(s): Presley Muwan
# Params:   workbook - the targeted excel file
#           website - CEDS web url
#           CEDS_version - The version of the current run
#
# Return: Excel workbook
# Input Files:  any excel workbook
# Output Files:
update_readme_sheet <- function(workbook, website, CEDS_version) {

  # add a "README" tab to global_total_emission_wb
  old_sheets <- names(workbook)

  # initialize CEDS_version column
  ceds_version_df <- data.frame(CEDS_Version = c(CEDS_version), Project_Website = c(website))

  # if tab does not exist, create new sheet
  if( length( old_sheets ) < 2 ) {
    workbook <- openxlsx::addWorksheet(workbook, "README")
  }

  # write sheet to workbook
  openxlsx::writeData(workbook, sheet = "README", x = ceds_version_df, startCol = 2)

  return(workbook)
}
