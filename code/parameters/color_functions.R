# ----------------------------------------------------------------------------------
# CEDS R header file: Color selection functions
# Author(s): Ben Goldstein
# Last Updated: 23 May 2017

# This file may be used to select from CEDS standard color palettes.
# Functions contained:
#
# Notes: Many of these functions were originally written for the GCAM data system.
#        Some relics of this past remain, such as the global variable names.

# -----------------------------------------------------------------------------

# selectPalette
# Brief: returns color palette
# Details: retrieves color palettes from color_palettes.csv for selection
# Dependencies: None
# Author(s): Ben Goldstein
# Params:
#   palette_name: a string containing the name of the palette selected
#   type: the color format to be returned
# Return: a dataframe containing palette values and corresponding color names
# Input files: color_palettes.csv
# Output files: None
# TODO: support non-hex color types

selectPalette <- function( palette_name, type = 'hex' ) {
  if (type != 'hex') {
    printLog( "ERROR in selectPalette: unsupported color type ", type )
    stop( "ERROR in readData: 'hex' only supported color type" )
  }
  all_palettes <- read.csv("general/color_palettes.csv")
  
  palette <- all_palettes[ ,grep( as.character(palette_name), colnames(all_palettes) )]
  
  colnames( palette ) <- c( "value" , "name" )
  
  return(palette)
}
