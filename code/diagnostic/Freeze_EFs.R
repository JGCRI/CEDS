# ------------------------------------------------------------------------------
# Program Name: Freeze_EFs.R
# Author: Rachel Hoesly
# Date Last Updated: 26 October, 2017
# Program Purpose: Produces new output where EF are frozen going forward from a given point
#
# Input Files:
# Output Files: figures in the diagnostic-output
# Note:
# TODO:
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R") # Additional function files may be required.
log_msg <- "Freeze_EFs" # First message to be printed to the log
script_name <- "Freeze_EFs.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )


# ---------------------------------------------------------------------------
# 0.Load Packages

library('zoo')

# ---------------------------------------------------------------------------
# 0.5 Script Options

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "BC"

freeze_year <- 1980

# Run H4.1.apply_EF_pathway.R
run_ef_pathway <- T
run_summary_script <- T
run_other_tranformation_correction <- T

# ---------------------------------------------------------------------------
# 1. Load files

EFs <- readData('MED_OUT', paste0('H.', em, '_total_EFs_extended'))

# ---------------------------------------------------------------------------
# 2. Process

EFs[ paste0('X', freeze_year:end_year) ] <- NA

EFs[ paste0('X', (freeze_year-1):end_year) ] <- t(na.locf(t(EFs[ paste0('X', (freeze_year-1):end_year) ]) ))

# ---------------------------------------------------------------------------
# 3. Output

writeData(EFs,'MED_OUT', paste0('H.', em, '_total_EFs_extended'))

# ---------------------------------------------------------------------------
# 4. Run Module H Correction Scripts and Summary Scripts

if ( run_ef_pathway == T ) source('../code/module-H/H4.1.apply_EF_pathway.R')

source('../code/module-H/H4.2.proc_Extended_Emissions.R')

if( run_other_tranformation_correction == T ){
if (em == 'SO2')  source('../code/module-H/H4.3.add_emissions_SO2_other_transformation.R')
if (em == 'CO2')  source('../code/module-H/H4.3.add_emissions_CO2_other_transformation.R') }

if ( run_summary_script == T ) source('../code/module-S/S1.1.write_summary_data.R')


logStop()

