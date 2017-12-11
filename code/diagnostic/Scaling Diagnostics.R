#------------------------------------------------------------------------------
# Program Name: Scaling Diagnostics
# Authors: Rachel Hoesly
# Date Last Updated: Nov 4, 2015
# Program Purpose: performs some scaling diagnostics (scaling factors)
# Input Files:   D.SO2_default_total_emissions.csv, F.SO2_scaled_emissions.csv
# Output Files:
# Notes:
# TODO:
# ------------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
  PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
  headers <- c( "data_functions.R", "analysis_functions.R" ) # Any additional function files required
  log_msg <- "Performing some scaling diagnostics" # First message to be printed to the log
  script_name <- "Scaling Diagnostics.R"

  source( paste0( PARAM_DIR, "header.R" ) )
  initialize( script_name, log_msg, headers )

  MED_OUT <- '../intermediate-output/'

# ------------------------------------------------------------------------------
# 1. Load Files

  default.in <- readData('MED_OUT', 'D.SO2_default_total_emissions')
  scaled.in <- readData('MED_OUT', 'F.SO2_scaled_emissions')

# Redefine list of regions/iso if "all"
  if (exists('country') && country == 'all')  country <- unique(default.in$iso)

# ------------------------------------------------------------------------------
# 2. Define Country to examine by iso code

  country <- c( "aus" , "aut" , "bel" , "bgr" , "blr" , "che" , "cyp" , "cze" , "deu" , "dnk" , "esp",
              "est" , "fin" , "fra" , "gbr" , "grc" , "hrv" , "hun" , "irl" , "isl" , "ita" , "jpn",
              "ltu" , "lva" , "mlt" , "nld" , "nor" , "nzl" , "prt" , "rou" , "svk" , "svn" , "swe" ,
              "tur" , "ukr" , 'usa' ,  'can')

# ------------------------------------------------------------------------------
# 3. Analysis

  default<-default.in[which(default.in$iso %in% country),]
  scaled<-scaled.in[which(scaled.in$iso %in% country),]
  ratio<-default
  ratio[,5:58]<-as.matrix(scaled[,5:58])/as.matrix(default[,5:58])
  ratio<-ratio[,-4]
# the data frame ratio gives the values of the scaled emission/default emission

# ------------------------------------------------------------------------------
# 4. Output
  writeData(ratio, domain = 'DIAG_OUT', fn = 'scaling_diagnostics', meta = F)
