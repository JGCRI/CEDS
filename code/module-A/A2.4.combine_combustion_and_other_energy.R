#------------------------------------------------------------------------------
# Program Name: A2.4.combine_combustion_and_other_energy.R
# Author(s): Rachel Hoesly
# Date Last Modified: Feb 22, 2018
# Program Purpose: Combine IEA combustion data with calculated other transformation
#                  other feedstock fuel use
# Input Files: A.en_biomass_fsu_fix.csv
# Output Files:
# Notes:
# TODO:

#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R" ) # Additional function files required.
log_msg <- "Add other transformation and feedstocks to combustion energy." # First message to be printed to the log
script_name <- "A2.4.combine_combustion_and_other_energy.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Read in files


energy_data <- readData( "MED_OUT", "A.en_biomass_fsu_fix" )
diff_hard_coal <- readData( "MED_OUT", "A.IEA_CEDS_hard_coal_difference" )
diff_brown_coal <- readData( "MED_OUT", "A.IEA_CEDS_brown_coal_difference" )
diff_nat_gas <- readData( "MED_OUT", "A.IEA_CEDS_natural_gas_difference" )
diff_oil <- readData( "MED_OUT", "A.IEA_CEDS_oil_difference" )

# ------------------------------------------------------------------------------
# 2. Combine

combined <- energy_data %>%
  rbind(diff_hard_coal, diff_brown_coal, diff_nat_gas, diff_oil) %>%
  arrange(iso, sector, fuel)

# ------------------------------------------------------------------------------
# 3. Output
# Add comments for each table
comments.A.comb_othertrans_activity <- c( paste0( "IEA energy statistics",
                                       " by intermediate sector / intermediate fuel / historical year,",
                                       " including other tranformation and other feedstocks." ) )
# write out data
writeData( combined, domain = "MED_OUT",
           fn = "A.comb_othertrans_activity", comments = comments.A.comb_othertrans_activity )


logStop()

# END