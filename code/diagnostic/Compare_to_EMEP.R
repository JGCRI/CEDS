# ------------------------------------------------------------------------------
# Program Name: Compare_to_EMEP.R
# Author(s): Patrick O'Rourke
# Date Last Updated: 4 April 2016
# Program Purpose: To generate figures of comparison for the EMEP 'as in models' & CEDS national
#                  totals.
# Input Files: EMEP 'as in models' Emissions Inventory, CEDS Final Emissions Inventory, EMEP
#               Level 1 Emissions Inventory ("E.em_EMEP_em_Format_inventory" )
# Output Files: 1)  "em_group#.pdf"  (CEDS & EMEP National Total Graphs - Saved in
#                   "diagnostic-output/EMEP")
#               2) "em_CEDS_EMEP_NT_Differences.csv" (CEDS & EMEP Nationtal Total
#                   Differences- Saved in "diagnostic-output/EMEP")
#               3) "em_CEDS_national_totals_File_postscript"  (CEDS National Totals Long -
#                   Saved in "intermediate-output")
#               4) "em_CEDS_wide_nat_File_postscript.tot"  (CEDS National Totals Wide - Saved in
#                  "intermediate-output")
#               5) "em_EMEP_models_national_totals.csv" (EMEP as in models
#                   National Totals Long - Saved in "intermediate-output"))
#               6) "em_EMEP_models_wide_nat.tot.csv" (EMEP as in models National
#                   Totals Wide - Saved in "intermediate-output")
# Notes: 1. EMEP 'as in models' Emissions are provided as: 1980, 1985, 1991 - 2013
#        2. CEDS Emissions are here provided as: 1980-2014
#        3. Packages Used: "tidyr", "ggplot2", "plyr", "scales"
#        4. Functions Created in Section 0.5:  'reformat_EMEP' (used in Section 2), 'reformat_CEDS' (used in Section 2),
#                               'make_ceds_total' (used in Section 2), 'reformat_EMEPlvl1' (used in Section 2),
#                               'make_emeplvl1_total' (used in Section 2), 'make_nt_df' (used in Section 2),
#                               'graph' which uses ggplot2 (used in Section 3), 'make_difference' (used in Section 4)
# TODO:
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R" ) # Any additional function files required
log_msg <- "Comparing CEDS Final National Totals to EMEP 'as in models' National Totals"
          # First message to be printed to the log
script_name <- "Compare_to_EMEP.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
#0.5 Settings/Load Files & Convert all txt files to csv
source( "../code/module-E/E.EMEP_as_in_models_emissions.R" )


#logging does not support txt files, so convert to csv
MCL <- readData( "MAPPINGS", "Master_Country_List" )
loadPackage('tools')

# Load Packages for reformatting data in Section 2
library(tidyr)

# Load Packages for plots in Section 3
library('ggplot2')
library('plyr')
library('scales')

# Define values
LAST_PLOT_YEAR <- 1980

# Define region of  Countries scaled by EMEP scaling script
region <- c("alb", "aut", "bel", "bgr", "bih", "blr", "che", "cyp", "cze", "dnk",
            "esp", "est", "fin", "fra", "gbr", "geo", "grc", "hrv", "hun", "irl",
            "isl", "ita", "ltu", "lux", "lva", "mda", "mkd", "mlt", "nld", "nor",
            "pol", "prt", "rou", "srb", "svk", "svn", "swe", "tur", "ukr")

# Define Function to Reformat the EMEP as-in-models data (Section 2)
reformat_EMEP <- function(df_in, df_totals){
  df_totals <- subset(EMEP, sector == "SNAP NATIONAL")  # subset EMEP National Totals
  df_totals <- subset(df_totals, iso %in% region)  # remove nations not scaled by EMEP scaling script
  df_totals$units <- NULL   #      Remove Unneeded Column - 'Units'
  df_totals$sector <- NULL  #      Remove Unneeded Column -  sector'
  df_totals$iso <- MCL[ match(df_totals$iso,MCL$iso),'Country_Name']    # Map ISO Country Name
  names( df_totals ) [ 1 ] <- "Country"       # Rename Column ISO to Country Name
  df_totals$DataSource <- "EMEP"      # Create Column named 'DataSource' where all entries = 'EMEP'
  # Make List of Years Available in EMEP data frame
  year_names_emep <- colnames(df_totals)
  remove_non_emep_years <- c("Country", "DataSource" )
  year_names_emep <- year_names_emep[-which( year_names_emep %in% remove_non_emep_years)]
  df_totals <- df_totals[,c("Country","DataSource", year_names_emep )]  # Reorder Columns of Interest
  df_totals <- df_totals[ order(df_totals$Country), ]      # Sort by Country
}

# Define Function to Reformat the CEDS data (Section 2)
  reformat_CEDS <- function(df_in, df_out ){
    df_in <- subset(df_in, iso %in% region)  # Remove  Countries not scaled by EMEP scaling script
    df_in$fuel <- NULL   # Remove Unneeded Column: 'fuel'
    df_in$units <- NULL  # Remove Unneeded Column: units'
    df_in$iso <- MCL[ match(df_in$iso, MCL$iso),'Country_Name']  # Map ISO to Country Name
    names( df_in ) [ 1 ] <- "Country"   # Change CEDS ISO to Country Name
  # Remove unneeded years
    year_names_emep <- colnames(df_in)
    remove_non_emep_years <- c("Country", "sector",  "X1960", "X1961",  "X1962", "X1963", "X1964",
                             "X1965", "X1966", "X1967", "X1968", "X1969", "X1970", "X1971", "X1972",
                             "X1973", "X1974", "X1975", "X1976", "X1977", "X1978", "X1979"  )
    year_names_emep <- year_names_emep[-which( year_names_emep %in% remove_non_emep_years)]
    columns_CEDS_keep <- c('Country', 'sector', year_names_emep)
    df_out <- subset(df_in [, (columns_CEDS_keep)])
    df_out <- df_out[ order(df_out$Country, df_out$sector), ] # Sort by Country & Sector
    remove <- c("1A3ai_International-aviation", "1A3aii_Domestic-aviation",
              "1A3dii_Domestic-navigation","1A3di_International-shipping",
              "6B_Other-not-in-total", "11A_Volcanoes", "11B_Forest-fires",
              "11C_Other-natural")  #        Remove CEDS sectors not included in National Totals
    df_out <- df_out[-which( df_out$sector %in% remove), ]
}

# Define Function to make national totals for CEDS data (Section 2)
  make_ceds_total <- function(df_in, df_totals ){
    # Make List of Years Available left in the CEDS data frame
    year_names_ceds <- colnames(df_in)
    remove_non_years_ceds <- c("Country", "sector" )
    year_names_ceds <- year_names_ceds[-which( year_names_ceds %in% remove_non_years_ceds)]
    CEDS_National_Totals <- by(df_in, df_in$Country, function(x) colSums(x[, year_names_ceds])) # Calculates the National Totals
    df_totals <- data.frame(matrix(unlist(CEDS_National_Totals), nrow=39, byrow=T),
                          stringsAsFactors=FALSE)   #  Make CEDS National Totals a data frame
    names(df_totals) <- c(year_names_ceds) # Rename Columns of this data fame (years)
    # Rename rows of this data frame (Country Column)
    df_totals$Country <- c("Albania", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina",
                         "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia",
                         "Finland", "France", "Georgia", "Greece", "Hungary", "Iceland",
                         "Ireland", "Italy", "Latvia", "Lithuania","Luxembourg", "Macedonia",
                         "Malta", "Moldova", "Netherlands", "Norway", "Poland", "Portugal",
                         "Romania", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden",
                         "Switzerland", "Turkey", "Ukraine", "United Kingdom")
    df_totals$DataSource <- "CEDS"   # Add column "DataSource" with all entries as "CEDS"
    df_totals <- df_totals[,c("Country","DataSource", year_names_ceds)]   # Reorder columns of interest
}

# Define Function to make reformat EMEP level 1 data (Section 2)
  reformat_EMEPlvl1 <- function(df_in, df_totals ){
    df_in <- subset(df_in, iso %in% region) #  Remove all Countries not scaled by  EMEP scaling script
    df_in$units <- NULL  #     Remove Unneeded Columns: 'units'
    df_in$iso <- MCL[ match(df_in$iso, MCL$iso),'Country_Name'] # Map ISO to Country Name
    names( df_in ) [ 1 ] <- "Country" # Rename column  'iso' to 'Country_Name'
    df_in <- df_in[ order(df_in$Country, df_in$sector), ] # Sort by Country & Sector
    # Remove EMEPlvl1 sectors not included in National Totals
    remove_emep <- c("O_AviCruise", "P_IntShipping",  "z_Memo",  "N_Natural")
    df_totals <- df_in[ -which( df_in$sector %in% remove_emep), ]
  }

# Define Function to make national totals for EMEP lvl1 data (Section 2)
  make_emeplvl1_total <- function(df_in, df_totals ){
    # Make List of Years Available left in the CEDS data frame
    year_names_ceds <- colnames(CEDS_NT)
    remove_non_years_ceds <- c("Country", "DataSource" )
    year_names_ceds <- year_names_ceds[-which( year_names_ceds %in% remove_non_years_ceds)]
    year_names_emeplvl1 <- year_names_ceds # Make List of Years Available left in the CEDS data frame
    year_names_emeplvl1 <- year_names_emeplvl1[-which( year_names_emeplvl1 %!in% colnames(EMEPlvl1))]   # removes years that CEDS had but EMEP level 1 didn't have
    # Calculates the EMEP lvl1 National Totals
    EMEPl1_National_Totals <- by(df_in, df_in$Country, function(x) colSums(x[ ,year_names_emeplvl1]))
    df_totals <- data.frame(matrix(unlist(EMEPl1_National_Totals), nrow=34, byrow=T),
                            stringsAsFactors=FALSE)   # Make EMEP lvl1 National Totals a data frame
    names(df_totals) <- c(year_names_emeplvl1)          # Rename Columns of this data fame (years)
    # Rename rows of this data frame (Country Column)
    df_totals$Country <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
                           "Denmark", "Estonia", "Finland", "France", "Georgia", "Hungary", "Iceland",
                           "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Macedonia", "Malta",
                           "Moldova", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Serbia",
                           "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey",
                           "United Kingdom")
    df_totals$DataSource <- "EMEP_lvl1" # Add column "DataSource" with all entries as "CEDS"
    df_totals <- df_totals[,c("Country","DataSource", year_names_emeplvl1)]   # Reorder columns of interest
}

# Define Function to make 1 national totals data frame for CEDS & EMEP as in models data (Section 2)
  make_nt_df <- function(CEDS_in, EMEP_in, df, df_out ){
    df <- rbind(CEDS_in, EMEP_in)
    df <- df[ order(df$Country, df$Year), ]  # Order by Country, Year
    df$year<- substr(df$Year, 2,5)  # Get rid of X infront of years
    df$Year <- NULL    #  Remove XYear column
    df <- df[,c("Country","DataSource", "year", "Emission")]  #Reorder xolumns
    df$DataSource <- as.factor(df$DataSource) # Change "Datasource" to a factor
    df$Country <- as.factor(df$Country) # Change "Country" to a factor
    df$year <- as.numeric(df$year) #  Change "year" to numeric
    df_out <- df
  }

# Define function 'graph' which plots & saves graphs of comparison (Section 3)
  graph <- function(df_in, grouping, plot_name ){
    subset_df <- subset(df_in, Country %in% grouping & year >= LAST_PLOT_YEAR ) # Subset Group
    plot <- function(x){                           # Define the function 'plot'- CEDS data as lines, EMEP data as symbols, colors by Country
      ggplot(x, aes(x = year, y = Emission, group = interaction(DataSource, Country), colour = Country)) +
        geom_point(data = x, aes(x = year, y = Emission, group = interaction(DataSource,Country),
                                 shape = DataSource, size = DataSource )) +
        geom_line(data = x, aes(x = year, y = Emission, group = interaction(DataSource, Country),
                                colour = Country, linetype = DataSource)) +
        scale_linetype_manual(values=c("solid", "blank")) +
        scale_shape_manual(values = c( 1 , 17)) +
        scale_size_manual(values=c(.25, 2.5))
    }
    plot_name <- plot(subset_df)                   # Plot & Save Group 1 to "/diagnostic-output/EMEP" in pdf form
    print(plot_name)                               # Show plot
  }

# Define Function which makes Table of differences - CEDS & EMEP as in models for 1980 & 1990 (Section)
  make_difference <- function(CEDS_in, EMEP_in, df, df2, nt_df2, df_difference, df_out){
    # reformat CEDS
    columns_names_keep1 <- c("Country", "DataSource", "X1980", "X1990" )
    df <- subset(CEDS_in [, (columns_names_keep1 )])
    names(df) <- c("Country", "DataSource", "CEDS_1980", "CEDS_1990")   # Rename Years Columns for CEDS Data
    df$DataSource <- NULL # Remove Data Source Column for CEDs Data
    # reformat EMEP as in models
    columns_names_keep1 <- c("Country", "DataSource", "X1980", "X1990" )
    df2 <- subset(EMEP_in [, (columns_names_keep1 )])
    names(df2) <- c("Country", "DataSource", "EMEP_1980", "EMEP_1990") # Rename Years Columns for EMEP Data
    df2$DataSource <- NULL  # Remove Data Source Column for EMEP Data
    # Combine New CEDS df with new EMEP df
    nt_df2 <- cbind(df, df2)
    nt_df2 <- nt_df2[ order(nt_df2$Country), ] # Order Combined df by Country
    nt_df2 <- nt_df2[,-(4)] # Remove Second Country Column
    # Make df of Differences & Ratios
    df_difference <- nt_df2
    df_difference$Diff_1980 = (df_difference$CEDS_1980 - df_difference$EMEP_1980) #   Make a Column for 1980 Difference (Difference = CEDS - EMEP)
    df_difference$Diff_1990 = (df_difference$CEDS_1990 - df_difference$EMEP_1990)   #   Make a column for 1990 Differences (Difference = CEDS - EMEP)
    df_difference$Ratio_1980 = (df_difference$CEDS_1980/df_difference$EMEP_1980) #   Make a Column for 1980 Ratio (Ratio = CEDS/EMEP)
    df_difference$Ratio_1990 = (df_difference$CEDS_1990/df_difference$EMEP_1990) #   Make a column for 1990 Ratio (Ratio = CEDS/EMEP)

    #   Make columns for if 1980 & 1990 were scaled by CEDS system ('No' if not in scaling script or
    #   if not EMEP level 1 Data for that nation, 'Yes' if scaled)
    df_difference$Scaled_1980 <- ""  # 1980 Scaled Column
    df_difference$Scaled_1990 <- ""   # 1990 Scaled Column
    df_difference <- df_difference[,c("Country", "EMEP_1980", "Diff_1980", "Ratio_1980", "Scaled_1980", "EMEP_1990", "Diff_1990", "Ratio_1990" ,"Scaled_1990")]  #   Reorder Columns of Interest
    # List nations that were scaled to EMEP data --> Germany not in EMEP, Moldova not Scaled
    emep_scale <- c("aut", "bel", "bgr", "che", "cyp", "cze", "dnk", "esp", "est", "fin", "fra",
                  "gbr", "geo", "hrv", "hun", "irl", "isl", "ita", "ltu", "lux", "lva", "mkd",
                  "mlt", "nld", "nor", "pol", "prt", "rou", "srb", "svk", "svn", "swe", "tur")
    emep_scale <- cbind (emep_scale)  #     cbind the list to make it a column
    emep_scale <- MCL[ match(emep_scale, MCL$iso),'Country_Name'] # Map 'iso' to 'Country_Name'
    emep_scale <- cbind (emep_scale)   # cbind the list to make it a column
    emep_scale <- emep_scale[ order(emep_scale), ]   # Order Countries Alphabetically
    emep_scale <- cbind (emep_scale)  # cbind the list to make it a column
    # Scaled 1980
    #     If the Country Was Scaled by the EMEP Scaling Script then Scaled_1980 = "Yes", if not  = "Not Scaled"
    df_difference$Scaled_1980 <- ifelse(df_difference$Country %in% emep_scale, "Yes" , "Not Scaled")
    #     If the Country has EMEP data for 1980 then Scaled_1980 = "Yes", if not = "No Data", if no as-in-models data = NA
    no_emep_1980 <- subset(EMEPlvl1_NT, is.na(X1980))
    no_emep_data_1980 <- no_emep_1980$Country
    df_difference$Scaled_1980 <- ifelse(df_difference$Country %in% no_emep_data_1980, "No Data", df_difference$Scaled_1980)
    # Scaled 1990
    #     If the Country Was Scaled by the EMEP Scaling Script then Scaled_1990 = "Yes", if not  = "Not Scaled"
    df_difference$Scaled_1990 <- ifelse(df_difference$Country %in% emep_scale, "Yes" , "Not Scaled")
    #     If the Country has EMEP data for 1990 then Scaled_1990 = "Yes", if not = "No Data" , if no as-in-models data = NA
    no_emep_1990 <- subset(EMEPlvl1_NT, is.na(X1990))
    no_emep_data_1990 <- no_emep_1990$Country
    df_difference$Scaled_1990 <- ifelse(df_difference$Country %in% no_emep_data_1990, "No Data", df_difference$Scaled_1990)
    df_out <- df_difference
}

# Describes which emission species is being analyzed
args_from_makefile <- commandArgs( TRUE )
em <<- args_from_makefile[1]
if ( is.na( em ) ) em <- "SO2"

# Stop script if running for unsupported emissions species
if ( em %!in% c('BC','CO','NH3','NMVOC','NOx','SO2') ) {
  stop (paste( 'EMEP script is not supported for emission species', em))
}

# Add option to pick which dataset to compare to.
CEDS_Data = "Default"
CEDS_Data = "Final"

File_postscript = "_EMEP_Comparison"
if ( CEDS_Data == "Default" ) File_postscript = "_EMEP_Comparison_Default"

# ------------------------------------------------------------------------------
# 1. Read in files
#   A. Read in EMEP data

#       Create a list of EMEP as-in-models files
        inv1_file_name <- paste0('E.', em, '_', 'EMEP_as-in-models_inventory')

#       Read in the EMEP files
        EMEP <- readData( "MED_OUT", inv1_file_name)

#   B. Read in CEDS data

#       Create a list of CEDS files
if ( CEDS_Data ==  "Final" ) {
        inv2_file_name <- paste0( em, '_total_CEDS_emissions' )
} else {
        inv2_file_name <- paste0('D.', em, '_', 'default_total_emissions' )
}

#       Read in the CEDS files
        CEDS <- readData( "MED_OUT", inv2_file_name)

#   C. Read in EMEP level 1 data
#       SELECT Data Format NFR14 or NFR09
        Em_Format <<- args_from_makefile[2]
        if ( is.na( Em_Format ) ) Em_Format <- "NFR14"

#       Create a list of EMEP level 1 files
        inv3_file_name <- paste0( "E.", em, "_EMEP_", Em_Format, "_inventory" )

#       Read in the EMEP level 1 files
        EMEPlvl1 <- readData( "MED_OUT", inv3_file_name)
# ------------------------------------------------------------------------------
# 2. Formatting EMEP & CEDS Data for 'ggplot' function
# A. Reformat  EMEP data
     EMEP_NT <- reformat_EMEP(EMEP, EMEP_NT)

#     Change from wide to long format
      EMEPlong <- gather(EMEP_NT, Year, Emission, (X1980:X2013))
      names( EMEPlong )[ names( EMEPlong ) %in% c( "variable", "value" ) ] <- c( "Year", "Emission")
      EMEPlong <- EMEPlong[ order(EMEPlong$Country, EMEPlong$Year), ] # Sort by Country & Sector

# B. Reformat  CEDS data
     CEDS_new <- reformat_CEDS(CEDS, CEDS_new)

#    Create CEDS National Totals
     CEDS_NT <- make_ceds_total(CEDS_new, CEDS_NT)

#    Change from wide to long format
     CEDSlong <- gather(CEDS_NT, Year, Emission, X1750:X2014)
     names( CEDSlong )[ names( CEDSlong ) %in% c( "variable", "value" ) ] <- c( "Year", "Emission")
     CEDSlong <- CEDSlong[ order(CEDSlong$Country, CEDSlong$Year), ] # Order long format by Country & Year

# C. Reformat EMEP level 1 data
     EMEPlvl1_new <- reformat_EMEPlvl1(EMEPlvl1, EMEPlvl1_NT)

#   Make EMEP level national totals
     EMEPlvl1_NT <- make_emeplvl1_total(EMEPlvl1_new, EMEPlvl1_NT)

# D. Merge CEDS & EMEP data frames
     nt_df <- make_nt_df(CEDSlong, EMEPlong, df, nt_df)

# ------------------------------------------------------------------------------
# 3. Make Scattplots Comparing CEDS & EMEP for each Emissions Species

# Groups for plots are by constructed by relative size of emissions
  # SO2 Groups:
  if(em == "SO2") group1 <- c("Luxembourg", "Malta", "Iceland", "Cyprus", "Switzerland", "Norway", "Albania")
  if(em == "SO2") group2 <- c( "Latvia", "Austria", "Macedonia", "Croatia", "Ireland",  "Slovenia" )
  if(em == "SO2") group3 <- c("Lithuania", "Denmark","Estonia", "Georgia", "Portugal", "Moldova")
  if(em == "SO2") group4 <- c("Belgium", "Serbia", "Finland", "Sweden", "Netherlands", "Bosnia and Herzegovina",
                            "Greece")
  if(em == "SO2") group5 <- c("Belarus" ,"Hungary", "Slovakia",  "Bulgaria", "Czech Republic", "Romania")
  if(em == "SO2") group6 <- c("France", "Turkey", "Italy", "Spain", "Poland", "United Kingdom", "Ukraine" )

  # NOx Groups
  if(em == "NOx") group1 <- c( "Malta", "Cyprus", "Iceland", "Macedonia", "Luxembourg", "Slovenia", "Albania",
                             "Bosnia and Herzegovina")
  if(em == "NOx") group2 <- c("Croatia", "Ireland", "Switzerland", "Serbia", "Norway", "Moldova")
  if(em == "NOx") group3 <- c("Austria","Slovakia", "Portugal", "Hungary", "Finland", "Denmark", "Belarus")
  if(em == "NOx") group4 <- c("Latvia", "Lithuania", "Georgia", "Estonia", "Sweden", "Bulgaria", "Belgium")
  if(em == "NOx") group5 <- c( "Romania", "Czech Republic", "Turkey", "Netherlands", "Greece")
  if(em == "NOx") group6 <- c( "Poland", "France", "Italy", "United Kingdom", "Ukraine", "Spain")

  # NMVOC Groups
  if(em == "NMVOC") group1 <- c( "Malta", "Iceland", "Cyprus", "Luxembourg", "Macedonia", "Albania")
  if(em == "NMVOC") group2 <- c("Bosnia and Herzegovina", "Slovenia", "Estonia", "Lithuania", "Croatia", "Ireland",
                              "Serbia")
  if(em == "NMVOC") group3 <- c("Latvia", "Slovakia", "Denmark", "Finland", "Portugal", "Hungary", "Moldova")
  if(em == "NMVOC") group4 <- c("Austria", "Norway", "Switzerland", "Czech Republic", "Sweden", "Belarus", "Netherlands")
  if(em == "NMVOC") group5 <- c("Belgium", "Georgia", "Romania", "Bulgaria", "Poland", "Turkey", "Greece")
  if(em == "NMVOC") group6 <- c("Spain", "Italy", "France", "United Kingdom", "Ukraine")

  # CO Groups
  if(em == "CO") group1 <- c("Malta", "Iceland", "Cyprus", "Macedonia", "Estonia", "Albania")
  if(em == "CO") group2 <- c("Slovenia", "Ireland", "Latvia", "Croatia", "Luxembourg", "Serbia", "Bosnia and Herzegovina",
                           "Moldova")
  if(em == "CO") group3 <- c("Lithuania", "Slovakia", "Finland", "Norway", "Denmark", "Bulgaria", "Portugal",
                           "Switzerland")
  if(em == "CO") group4 <- c("Netherlands", "Hungary", "Austria", "Sweden", "Czech Republic", "Belarus")
  if(em == "CO") group5 <- c("Belgium", "Georgia", "Romania", "Turkey", "Greece")
  if(em == "CO") group6 <- c("Spain", "Poland", "Italy", "United Kingdom", "France", "Ukraine")

#   Plot & Save Groups to "/diagnostic-output/EMEP" in pdf form
    graph(nt_df, group1, g1 ) # Group 1
    savePlot("DIAG_OUT", "EMEP/", paste0( em, "_Group1", File_postscript), width = 10, height = 5.5)

    graph(nt_df, group2, g2 ) # Group 2
    savePlot("DIAG_OUT", "EMEP/", paste0( em, "_Group2", File_postscript), width = 10, height = 5.5)

    graph(nt_df, group3, g3 ) # Group 3
    savePlot("DIAG_OUT", "EMEP/", paste0( em, "_Group3", File_postscript), width = 10, height = 5.5)

    graph(nt_df, group4, g4 ) # Group 4
    savePlot("DIAG_OUT", "EMEP/", paste0( em, "_Group4", File_postscript), width = 10, height = 5.5)

    graph(nt_df, group5, g5 ) # Group 5
    savePlot("DIAG_OUT", "EMEP/", paste0( em, "_Group5", File_postscript), width = 10, height = 5.5)

    graph(nt_df, group6, g6 ) # Group 6
    savePlot("DIAG_OUT", "EMEP/", paste0( em, "_Group6", File_postscript), width = 10, height = 5.5)

# ------------------------------------------------------------------------------
# 4. Making Table of differences  Between CEDS Final Emissions & EMEP Emissions for 1980 & 1990
    diff <- make_difference(CEDS_NT, EMEP_NT, df, df2, nt_df2, df_difference, diff)

# ------------------------------------------------------------------------------
# 5. Save all tables created

# A. Write Difference Table
    writeData(diff,  domain = "DIAG_OUT", domain_extension = "EMEP/",
              fn = paste0( em, "_CEDS_EMEP_NT_Differences", File_postscript ), meta = FALSE)

# # B. Write tables of the wide & long formatted National Totals
#   # Changing working directory to "intermediate-output"
#
# # Write CEDS long data (a csv file of CEDS national totals)
#   writeData(CEDSlong, domain = "MED_OUT", fn = paste0( em, "_CEDS_national_totals", File_postscript ),
#             meta = FALSE )
#
# # Write CEDS wide data (a csv file of CEDS national totals)
#   writeData(CEDS_NT, domain = "MED_OUT", fn = paste0( em, "_CEDS_wide_nat", File_postscript,".tot" ),
#             meta = FALSE )
#
# # Write EMEP 'as in models' long data (a csv file of EMEP national totals)
#   writeData(EMEPlong, domain = "MED_OUT", fn = paste0( em, "_EMEP_models_national_totals" ),
#             meta = FALSE )
#
# # Write EMEP 'as in models' wide data (a csv file of EMEP national totals)
#   writeData(EMEP_NT, domain = "MED_OUT", fn = paste0( em, "_EMEP_models_wide_nat.tot" ),
#             meta = FALSE )

#   Every script should finish with this line-
logStop()

# END
