# Program Name: Compare_branches.R
# Author: Rachel Hoesly
# Date Last Updated: November 3, 2022
# Program Purpose: Compare output files from 2 branches for debugging
# Input Files:
# Output Files:
# Notes:

# ---------------------------------------------------------------------------
# 0.0 Load Packages

library(tidyr)
library(dplyr)

# ---------------------------------------------------------------------------
# 0.5 Script Parameters

# Emission Species
em <- 'SO2'

# Define iso/sector/fuel to compare over
compare_iso <- "arg"
compare_sector <- c('1A4b_Residential')
compare_fuel <- "light_oil"




# Identify Branches and location of the intermediate-output folder
branch_name_1 <- 'master 11.09'
branch_location_1 <- '/Users/rach919/OneDrive - PNNL/Desktop/master 11.09/intermediate-output'
branch_name_2 <- 'expanded_sector 11.10'
branch_location_2 <- '/Users/rach919/OneDrive - PNNL/Documents/CEDS/CEDS Git Folders/CEDS/intermediate-output'
    # '/Users/rach919/OneDrive - PNNL/Desktop/expanded sectors 11.10.2022/intermediate-output'

# output folder
output_folder <- '/Users/rach919/OneDrive - PNNL/Desktop/'

# File for comparison
comparison_file <- 'D.SO2_default_total_emissions.csv'
    # 'H.SO2_total_EFs_extended.csv'
   #
#


# ---------------------------------------------------------------------------
# 1. Input
branch_1_file <- read.csv(paste0(branch_location_1, '/',comparison_file))
branch_2_file <- read.csv(paste0(branch_location_2, '/',comparison_file))

# ---------------------------------------------------------------------------
# 2. Compare

branch_1_selected <- branch_1_file %>%
    mutate(run = branch_name_1) %>%
    filter(iso %in% compare_iso) %>%
    filter(sector %in% compare_sector) %>%
    select(run, iso, sector, fuel, units, starts_with('X'))

branch_2_selected <- branch_2_file %>%
    mutate(run = branch_name_2) %>%
    filter(iso %in% compare_iso) %>%
    filter(sector %in% compare_sector) %>%
    select(run, iso, sector, fuel, units, starts_with('X'))

all_values <- branch_1_selected %>%
    bind_rows(branch_2_selected) %>%
    filter(iso %in% compare_iso) %>%
    filter(sector %in% compare_sector) %>%
    arrange(fuel, sector, run)

diff <- branch_1_selected %>% select(-run) %>% gather(year, value, -iso, -sector, -fuel, -units) %>%
    left_join(branch_2_selected %>% select(-run) %>% gather(year, value, -iso, -sector, -fuel, -units), by = c('iso', 'sector', 'fuel', 'units', 'year')) %>%
    mutate(diff = value.x-value.y) %>%
    filter(diff != 0)

# ---------------------------------------------------------------------------
# 2. Compare

write.csv(all_values, paste0(output_folder, '/','All Values Comparison - ',comparison_file))

if(nrow(diff)>0){
    write.csv(diff, paste0(output_folder, '/','Diff Comparison - ',comparison_file))
    write.csv(all_values, paste0(output_folder, '/','All Values Comparison - ',comparison_file))
}


