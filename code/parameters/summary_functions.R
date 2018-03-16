# ----------------------------------------------------------------------------------
# CEDS R header file: File Summary functions
# Authors: Presley Muwan, Caleb Braun
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
#           for values all values greater than 0. And grays out values equal to 0.
# Dependencies: None
# Author(s): Presley Muwan
# Params:   workbook - the xlsl file to be formatted (use loadworkbook(filepath) function from xlsx package
#                       to extract the xlsl workbook)
#           rowIndices  - indeces of the rows to be formatted
#           columnIndices - indeces of the columns to be formatted
#
# Return: xlsx_workbook
# Input Files:  none
# Output Files: none
format_xlsx_numeric_data <- function ( workbook, rowIndices=c(2:58), columnIndices ) {

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
# write_global_emissions_by_sector
# Brief:  Create an excel file containing emissions by sector, with separate
#         sheets for each year provded.
# Details: Each sheet (tab) created is named by the year that it has data for.
#          The sheet is formatted to have emission species as column names and
#          CEDS sectors as row names
# Dependencies: None
# Author(s): Presley Muwan, Caleb Braun
# Params: em_by_sector - Data for a single emissions species; expected to be in
#                        the following form:
#                        | sector |  em  | units | year | value |
# Return: none
# Input Files:  global_emissions_by_CEDS_sector.xlsx - if it exists
# Output Files: global_emissions_by_CEDS_sector.xlsx
write_global_emissions_by_sector <- function( em_by_sector ) {

  global_em_workbook_path <- "../final-emissions/diagnostics/global_emissions_by_CEDS_sector.xlsx"

  # load the file or create a new workbook if it doesn't exist
  if( file.exists( global_em_workbook_path ) ) {
    global_em_workbook <- openxlsx::loadWorkbook( global_em_workbook_path )
  }
  else {
    global_em_workbook <- openxlsx::createWorkbook()
  }

  all_years <- unique( em_by_sector$year )
  emission <- unique( em_by_sector$em )
  ncols <- 3

  # split data into tabs for each year specified
  lapply( all_years, function( y ) {
    tab <- dplyr::filter( em_by_sector, year == y ) %>%
      dplyr::select( sector, units, value ) %>%
      rbind( c( "Total", unique( .$units ), sum( .$value ) ) ) %>%
      dplyr::mutate( value = as.numeric( value ) ) %>%
      dplyr::rename( !!emission := value )

    # the tab (sheet) name is just the year; remove leading 'X'
    tab_name <- substr( y, 2, 5 )

    # join data to existing sheet if it exists
    if( tab_name %in% names( global_em_workbook ) ) {
      temp <- openxlsx::read.xlsx( global_em_workbook, sheet = tab_name )
      temp[emission] <- tab[emission]
      tab <- temp
      ncols <<- ncol( tab )
    }
    else {
      printLog( "Creating sheet: '", tab_name, "' in", global_em_workbook_path )
      openxlsx::addWorksheet( global_em_workbook, tab_name )
    }

    # write out the data
    openxlsx::writeData( global_em_workbook, sheet = tab_name, x = tab )
  })

  # update workbook's cell conditional formatting
  formatted_workbook <- format_xlsx_numeric_data( workbook = global_em_workbook,
                                                  columnIndices = 3:ncols )

  printLog( "Updating", emission, "emissions in", global_em_workbook_path )
  openxlsx::saveWorkbook( formatted_workbook, file = global_em_workbook_path,
                          overwrite = T )

} #create_tab_of_global_emission_by_sector() Ends


# -----------------------------------------------------------------------------------
# write_global_emissions_by_species
# Brief:  This function creates or modifies an xlsx file containing aggregate
#         emissions by species and a README sheet
# Details: Data for the species are expected to be in the following form (where
#          2014 should be the CEDS end year):
#          |  em  | units | 1750 | 1751 | ... | 2014 |
# Dependencies: None
# Author(s): Caleb Braun
# Params: em_by_species - a data frame containing total emissions for one or
#                         more species
#
# Output Files: global_total_emissions_by_species.xlsx
write_global_emissions_by_species <- function( em_by_species ) {

	# Read global_total_emissions_for_species file (if it exists) and update it
	# with the new data
	em_by_species_path <- "../final-emissions/diagnostics/global_total_emissions_for_species.xlsx"
	main_sheet <- "global_totals"

	# load the file or create a new workbook if it doesn't exist
	if ( file.exists( em_by_species_path ) ) {
	    global_em_workbook <- openxlsx::loadWorkbook( em_by_species_path )
	}
	else {
	    global_em_workbook <- openxlsx::createWorkbook()
	}

	# join data to existing sheet if it exists
	if ( main_sheet %in% names( global_em_workbook ) ) {
	    old <- openxlsx::read.xlsx( global_em_workbook, sheet = main_sheet )
	    old[ old$em %in% em_by_species$em, ] <- em_by_species
	    em_by_species <- old
	} else {
	    printLog( "Creating sheet: '", main_sheet, "' in", em_by_species_path )
	    openxlsx::addWorksheet( global_em_workbook, main_sheet )
	}

	# write out the data
	openxlsx::writeData( global_em_workbook, sheet = main_sheet, x = em_by_species )

	# format global_total_emission_for_species: remove decimal points, use
	# commas for values greater than 1, and show only two decimal places
    formatted_workbook <- format_xlsx_numeric_data( workbook = global_em_workbook,
                                                    rowIndices = 2:nrow( em_by_species ),
                                                    columnIndices = 3:ncol( em_by_species ) )

	# update the file's README tab
	formatted_workbook <- update_readme_sheet( formatted_workbook )

    printLog( "Updating", em_by_species_path )
    openxlsx::saveWorkbook( formatted_workbook, file = em_by_species_path,
                            overwrite = T )
}


# -----------------------------------------------------------------------------------
# update_readme_sheet
# Brief:  this function adds a README sheet to a workbook
# Details: The README Sheet contains the version number of the latest run and CEDS url website
# Dependencies: None
# Author(s): Presley Muwan
# Params: workbook - the targeted excel file
# Return: Excel workbook
# Input Files:  any excel workbook
# Output Files:
update_readme_sheet <- function( workbook ) {

  # initialize CEDS_version column
  ceds_website <- "http://www.globalchange.umd.edu/ceds/"
  ceds_version_df <- data.frame( CEDS_Version = version_stamp, # a global var
                                 Project_Website = ceds_website )

  # add a "README" tab to global_total_emission_wb
  old_sheets <- names(workbook)
  readme <- "README"

  # if tab does not exist, create new sheet
  if( readme %!in% old_sheets ) {
    openxlsx::addWorksheet(workbook, readme)
  }

  # write sheet to workbook
  openxlsx::writeData(workbook, sheet = readme, x = ceds_version_df, startCol = 2)

  return(workbook)
}
