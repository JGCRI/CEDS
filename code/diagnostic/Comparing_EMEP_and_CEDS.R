# ------------------------------------------------------------------------------
# Program Name: Comparing_SNAP_and_CEDS.R
# Author(s): Patrick O'Rourke
# Date Last Updated: February 1st , 2016
# Program Purpose: To generate figures of comparison for the SNAP & CEDS national
#                  totals.
# Input Files: SNAP Emissions Inventory, CEDS Scaled Emissions Inventory, EMEP 
#               Level 1 Emissions Inventory
# Output Files: 1)  "em_group#.pdf"  (CEDS & SNAP National Total Graphs - Saved in 
#                   "diagnostic-output/EMEP")
#               2) "em_CEDS_SNAP_NT_Differences.csv" (CEDS & SNAP Nationtal Total
#                   Differences- Saved in "diagnostic-output/EMEP")
#               3) "F.em_CEDS_national_totals.csv" (CEDS National Totals Long - 
#                   Saved in "intermediate-output")
#               4) "F.em_CEDS_wide_nat.tot.csv" (CEDS National Totals Wide - Saved in
#                  "intermediate-output")
#               5) "F.em_EMEP_models_national_totals.csv" (EMEP as in models 
#                   National Totals Long - Saved in "intermediate-output"))
#               6) "F.em_EMEP_models_wide_nat.tot.csv" (EMEP as in models National
#                   Totals Wide - Saved in "intermediate-output")
# Notes: 1. SNAP Emissions are provided as: 1980, 1985, 1991 - 2013
#        2. CEDS Emissions are here provided as: 1980-2014
#        3. Packages Used: "tidyr", "ggplot2", "plyr", "scales"
#        4. Function Created: "plot" using "ggplot2" package
# TODO: 
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Set working directory to the CEDS “input” directory & define PARAM_DIR as the
# location of the CEDS “parameters” directory, relative to the new working directory.
dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
for ( i in 1:length( dirs ) ) {
  setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
  wd <- grep( 'CEDS/input', list.dirs(), value = T )
  if ( length( wd ) > 0 ) {
    setwd( wd[ 1 ] )
    break
  }
}
INPUT <- paste(getwd())
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R" ) # Any additional function files required
log_msg <- "Comparing CEDS Scaled National Totals to SNAP National Totals" 
          # First message to be printed to the log
script_name <- "Comparing_SNAP_and_CEDS.R.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
#0.5 Settings/Load Files & Convert all txt files to csv
#logging does not support txt files, so convert to csv
MCL <- readData( "MAPPINGS", "Master_Country_List" )
loadPackage('tools')

# Load Packages for reformatting data in Section 2
library(tidyr)

# Load Packages for plots in Section 3
library('ggplot2')
library('plyr')
library('scales')

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
CEDS_Data = "Scaled"

File_postscript = ""
if ( CEDS_Data == "Default" ) File_postscript = "_Default" 

# ------------------------------------------------------------------------------
# 1. Read in files

#   A. Read in SNAP data

#       Location of input files relative to working directory --> Need to set new
#       directory
        setwd( "../intermediate-output")
  
#       Create a list of SNAP files
        inv1_file_name <- paste0('E.', em, '_', 'SNAP_inventory')

#       Read in the SNAP files
        SNAP <- readData( "MED_OUT", inv1_file_name)   
        
#   B. Read in CEDS data
        
#       Create a list of CEDS files
if ( CEDS_Data ==  "Scaled" ) {
        inv2_file_name <- paste0('F.', em, '_', 'scaled_emissions' )
} else {
        inv2_file_name <- paste0('D.', em, '_', 'default_total_emissions' )
}
    
#       Read in the CEDS files
        CEDS <- readData( "MED_OUT", inv2_file_name)   

#   C. Read in EMEP level 1 data
        
#       Create a list of EMEP level 1 files
        inv3_file_name <- paste0("E.", em, "_EMEP_NFR14_inventory")
        
#       Read in the EMEP level 1 files
        EMEPlvl1 <- readData( "MED_OUT", inv3_file_name)   
        
# ------------------------------------------------------------------------------
# 2. Formatting SNAP & CEDS Data for 'ggplot' function
        
# A. Reformat  SNAP data
#       Make a new data frame which is a subset containing only SNAP National
#       Totals
        SNAP_NT <- subset(SNAP, sector == "SNAP NATIONAL")  
        
#       Remove all Countries who are not scaled by the EMEP scaling script
        region <- c("alb", "aut", "bel", "bgr", "bih", "blr", "che", "cyp", "cze", "dnk",
                    "esp", "est", "fin", "fra", "gbr", "geo", "grc", "hrv", "hun", "irl",
                    "isl", "ita", "ltu", "lux", "lva", "mda", "mkd", "mlt", "nld", "nor",
                    "pol", "prt", "rou", "srb", "svk", "svn", "swe", "tur", "ukr")
        
        SNAP_NT <- subset(SNAP_NT, iso %in% region)
        
#      Remove Unneeded Columns: 'Units', 'sector'
       SNAP_NT$units <- NULL  
       SNAP_NT$sector <- NULL  
       
#      Change ISO to Country Name
       SNAP_NT$iso <- MCL[ match(SNAP_NT$iso,MCL$iso),'Country_Name']
       names( SNAP_NT ) [ 1 ] <- "Country"
       
#      Create Column named 'DataSource' with all entries saying EMEP
       SNAP_NT$DataSource <- "EMEP"      
       
#      Reorder Columns of Interest
       SNAP_NT <- SNAP_NT[,c("Country","DataSource", "X1980", "X1985", "X1990",
                             "X1991", "X1992", "X1993", "X1994", "X1995", "X1996",
                             "X1997", "X1998", "X1999", "X2000", "X2001","X2002",
                             "X2003", "X2004", "X2005", "X2006", "X2007", "X2008",
                             "X2009", "X2010", "X2011", "X2012","X2013" )]
       
#     Sort by Country
      SNAP_NT <- SNAP_NT[ order(SNAP_NT$Country), ]
       
#     Change from wide to long format
      SNAPlong <- gather(SNAP_NT, Year, Emission, (X1980:X2013))
       
#     Sort by Country & Sector
      SNAPlong <- SNAPlong[ order(SNAPlong$Country, SNAPlong$Year), ]
       
# B. Reformat  CEDS data

#     Remove all Countries who are not scaled by the EMEP scaling script
      CEDS <- subset(CEDS, iso %in% region)     
        
#     Remove Unneeded Columns: 'fuel', 'units'
      CEDS$fuel <- NULL  
      CEDS$units <- NULL  
        
#     Change CEDS ISO to Country Name
      CEDS$iso <- MCL[ match(CEDS$iso, MCL$iso),'Country_Name']
      names( CEDS ) [ 1 ] <- "Country"
        
#     Remove Unneeded Years: 1960 - 1979
      CEDS <- CEDS [, -(3:22)]
        
#     Sort by Country & Sector
      CEDS <-CEDS[ order(CEDS$Country, CEDS$sector), ]
       
#     Create a CEDS National Totals data frame
       
#        Remove CEDS sectors not included in National Totals
         remove <- c("1A3ai_International-aviation", "1A3aii_Domestic-aviation",
                     "1A3dii_Domestic-naviation","1A3di_International-shipping",
                     "6B_Other-not-in-total", "11A_Volcanoes", "11B_Forest-fires",
                     "11C_Other-natural") 
          
         CEDS <- CEDS[-which( CEDS$sector %in% remove), ]
       
#        Calculates the National Totals
         CEDS_National_Totals <- by(CEDS, CEDS$Country, function(x) colSums(x[,3:37]))
          
#        Make CEDS National Totals a data frame
         CEDS_NT <- data.frame(matrix(unlist(CEDS_National_Totals), nrow=39, byrow=T), 
                    stringsAsFactors=FALSE)
          
#        Rename Columns of this data fame (years)
         names(CEDS_NT) <- c("X1980", "X1981", "X1982", "X1983", "X1984", "X1985", "X1986",
                             "X1987", "X1988", "X1989", "X1990", "X1991", "X1992", "X1993",
                             "X1994", "X1995", "X1996", "X1997", "X1998", "X1999", "X2000",
                             "X2001", "X2002", "X2003", "X2004", "X2005", "X2006", "X2007",
                             "X2008", "X2009", "X2010", "X2011", "X2012", "X2013", "X2014")
         
#        Rename rows of this data frame (Country Column)
         CEDS_NT$Country <- c("Albania", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina",
                              "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia",
                              "Finland", "France", "Georgia", "Greece", "Hungary", "Iceland",
                              "Ireland", "Italy", "Latvia", "Lithuania","Luxembourg", "Macedonia",
                              "Malta", "Moldova", "Netherlands", "Norway", "Poland", "Portugal",
                              "Romania", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden",
                              "Switzerland", "Turkey", "Ukraine", "United Kingdom")

#        Add column "DataSource" with all entries as "CEDS"
         CEDS_NT$DataSource <- "CEDS" 
         
#        Reorder columns of interest
         CEDS_NT <- CEDS_NT[,c("Country","DataSource", "X1980", "X1981", "X1982", "X1983", "X1984",
                               "X1985", "X1986", "X1987", "X1988", "X1989", "X1990", "X1991", "X1992",
                               "X1993", "X1994", "X1995", "X1996", "X1997", "X1998", "X1999", "X2000", 
                               "X2001","X2002", "X2003", "X2004", "X2005", "X2006", "X2007", "X2008",
                               "X2009", "X2010", "X2011", "X2012","X2013", "X2014"  )]
         
#       Change from wide to long format
        CEDSlong <- gather(CEDS_NT, Year, Emission, X1980:X2014)
         
#       Order long format by Country & Year
        CEDSlong <- CEDSlong[ order(CEDSlong$Country, CEDSlong$Year), ]

# C. Reformat EMEP level 1 data
        
#     Remove all Countries who are not scaled by the EMEP scaling script
      EMEPlvl1 <- subset(EMEPlvl1, iso %in% region)
        
#     Remove Unneeded Columns: 'units'
      EMEPlvl1$units <- NULL  
        
#     Change EMEPlvl1 'iso' to 'Country_Name'
      EMEPlvl1$iso <- MCL[ match(EMEPlvl1$iso, MCL$iso),'Country_Name']
      names( EMEPlvl1 ) [ 1 ] <- "Country"
        
#     Sort by Country & Sector
      EMEPlvl1 <- EMEPlvl1[ order(EMEPlvl1$Country, EMEPlvl1$sector), ]
        
#     Create a EMEPlvl1 National Totals data frame
        
#        Remove EMEPlvl1 sectors not included in National Totals
         remove_emep <- c("O_AviCruise", "P_IntShipping",  "z_Memo",  "N_Natural") 
        
         EMEPlvl1 <- EMEPlvl1[ -which( EMEPlvl1$sector %in% remove_emep), ]
        
#        Calculates the EMEP lvl1 National Totals
         EMEP_National_Totals <- by(EMEPlvl1, EMEPlvl1$Country, function(x) colSums(x[,3:36]))
        
#        Make EMEP lvl1 National Totals a data frame
         EMEP_NT <- data.frame(matrix(unlist(EMEP_National_Totals), nrow=34, byrow=T), 
                              stringsAsFactors=FALSE)
        
#        Rename Columns of this data fame (years)
         names(EMEP_NT) <- c("X1980", "X1981", "X1982", "X1983", "X1984", "X1985", "X1986",
                            "X1987", "X1988", "X1989", "X1990", "X1991", "X1992", "X1993",
                            "X1994", "X1995", "X1996", "X1997", "X1998", "X1999", "X2000",
                            "X2001", "X2002", "X2003", "X2004", "X2005", "X2006", "X2007",
                            "X2008", "X2009", "X2010", "X2011", "X2012", "X2013")
        
#        Rename rows of this data frame (Country Column)
         EMEP_NT$Country <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
                             "Denmark", "Estonia", "Finland", "France", "Georgia", "Hungary", "Iceland",
                             "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Macedonia", "Malta",
                             "Moldova", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Serbia",
                             "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey",
                             "United Kingdom")
        
#        Add column "DataSource" with all entries as "CEDS"
         EMEP_NT$DataSource <- "EMEP_lvl1" 
        
#        Reorder columns of interest
         EMEP_NT <- EMEP_NT[,c("Country","DataSource", "X1980", "X1981", "X1982", "X1983", "X1984",
                              "X1985", "X1986", "X1987", "X1988", "X1989", "X1990", "X1991", "X1992",
                              "X1993", "X1994", "X1995", "X1996", "X1997", "X1998", "X1999", "X2000", 
                              "X2001","X2002", "X2003", "X2004", "X2005", "X2006", "X2007", "X2008",
                              "X2009", "X2010", "X2011", "X2012","X2013")]
        
# D. Merge CEDS & SNAP data frames
      nt_df <- rbind(CEDSlong, SNAPlong)
        
#       Order by Country, Year
        nt_df <- nt_df[ order(nt_df$Country, nt_df$Year), ]

#       Get rid of X infront of years
        nt_df$year<- substr(nt_df$Year, 2,5)
        
#       Remove XYear column
        nt_df$Year <- NULL  
        
#       Reorder xolumns
        nt_df <- nt_df[,c("Country","DataSource", "year", "Emission")]
        
#     Change "Datasource" & "Country" to factors
        nt_df$DataSource <- as.factor(nt_df$DataSource)
        nt_df$Country <- as.factor(nt_df$Country)
        
#     Change "year" to numeric
        nt_df$year <- as.numeric(nt_df$year)
        
# ------------------------------------------------------------------------------
# 3. Make Scattplots Comparing CEDS & EMEP for each Emissions Species
    
# Set Working Directory to "EMEP" Folder in "diagnostic-output" so that figures are
# saved in this folder

setwd( "..")
setwd( "./diagnostic-output/EMEP")
        
# Groups for plots are by constructed by size of emissions 
            
# Group 1 nations by emission species:
if(em == "SO2") group1 <- c("Luxembourg", "Malta", "Iceland", "Cyprus", "Switzerland",
                            "Norway", "Albania")
if(em == "NOx") group1 <- c( "Malta", "Cyprus", "Iceland", "Macedonia", "Luxembourg",
                             "Slovenia", "Albania", "Bosnia and Herzegovina")
if(em == "NMVOC") group1 <- c( "Malta", "Iceland", "Cyprus", "Luxembourg", "Macedonia", "Albania")
if(em == "CO") group1 <- c("Malta", "Iceland", "Cyprus", "Macedonia", "Estonia", "Albania")

#   Subset by group1   
group <- group1
nt_group1 <- subset(nt_df, Country %in% group) 

#   Define the function 'plot'- CEDS scaled data as lines, SNAP data as symbols, colors by Country
plot <- function(x){
  ggplot(x, aes(x = year, y = Emission,group = interaction(DataSource, Country), colour = Country)) +
    geom_point(data = x, aes(x = year, y = Emission, group = interaction(DataSource,Country),
                             shape = DataSource, size = DataSource )) +
    geom_line(dasta = x, aes(x = year, y = Emission, group = interaction(DataSource, Country), 
                             colour = Country, linetype = DataSource)) +
    scale_linetype_manual(values=c("solid", "blank")) +
    scale_shape_manual(values = c( 1 , 17)) +
    scale_size_manual(values=c(.25, 2.5))
}

#     Plot group 1     
g1 <- plot(nt_group1)

#     Save group 1 plot to "/diagnostic-output/EMEP" in pdf form
print( g1 )
ggsave( paste0( em, "_group1", File_postscript, ".pdf" ), width=10.0, height=5.5)

# Group 2 nations by emission species:
if(em == "SO2") group2 <- c( "Latvia", "Austria", "Macedonia", "Croatia", "Ireland",  "Slovenia" )
if(em == "NOx") group2 <- c("Croatia", "Ireland", "Switzerland", "Serbia", "Norway", "Moldova")
if(em == "NMVOC") group2 <- c("Bosnia and Herzegovina", "Slovenia", "Estonia", "Lithuania", "Croatia",
                              "Ireland", "Serbia")
if(em == "CO") group2 <- c("Slovenia", "Ireland", "Latvia", "Croatia", "Luxembourg",
                           "Serbia", "Bosnia and Herzegovina", "Moldova")
#   Subset by group2   
group <- group2
nt_group2 <- subset(nt_df, Country %in% group)

#   Plot group 2    
g2 <- plot(nt_group2)

#   Save group 2 plot to "/diagnostic-output/EMEP" in pdf form
print( g2 )
ggsave( paste0( em, "_group2", File_postscript, ".pdf" ), width=10.0, height=5.5)

# Group 3 nations by emission species:
if(em == "SO2") group3 <- c("Lithuania", "Denmark","Estonia", "Georgia", "Portugal",
                            "Moldova")
if(em == "NOx") group3 <- c("Austria","Slovakia", "Portugal", "Hungary", "Finland",
                            "Denmark", "Belarus")
if(em == "NMVOC") group3 <- c("Latvia", "Slovakia", "Denmark", "Finland", "Portugal",
                              "Hungary", "Moldova")
if(em == "CO") group3 <- c("Lithuania", "Slovakia", "Finland", "Norway", "Denmark",
                           "Bulgaria", "Portugal", "Switzerland")

#   Subset by group3
group <- group3
nt_group3 <- subset(nt_df, Country %in% group)

#   Plot group 3     
g3 <- plot(nt_group3)

#   Save group 3 plot to "/diagnostic-output/EMEP" in pdf form
print( g3 )
ggsave( paste0( em, "_group3", File_postscript, ".pdf" ), width=10.0, height=5.5)

# Group 4 nations by emission species:
if(em == "SO2") group4 <- c("Belgium", "Serbia", "Finland", "Sweden", "Netherlands",
                            "Bosnia and Herzegovina", "Greece")
if(em == "NOx") group4 <- c("Latvia", "Lithuania", "Georgia", "Estonia", "Sweden",
                            "Bulgaria", "Belgium")
if(em == "NMVOC") group4 <- c("Austria", "Norway", "Switzerland", "Czech Republic",
                              "Sweden", "Belarus", "Netherlands")
if(em == "CO") group4 <- c("Netherlands", "Hungary", "Austria", "Sweden", "Czech Republic",
                           "Belarus")

#   Subset by group4   
group <- group4
nt_group4 <- subset(nt_df, Country %in% group)

#   Plot group 4     
g4 <- plot(nt_group4)

#   Save group 4 plot to "/diagnostic-output/EMEP" in pdf form
print( g4 )
ggsave( paste0( em, "_group4", File_postscript, ".pdf" ), width=10.0, height=5.5)    

# Group 5 nations by emission species:
if(em == "SO2") group5 <- c("Belarus" ,"Hungary", "Slovakia",  "Bulgaria", "Czech Republic",
                            "Romania")
if(em == "NOx") group5 <- c( "Romania", "Czech Republic", "Turkey", "Netherlands", "Greece")
if(em == "NMVOC") group5 <- c("Belgium", "Georgia", "Romania", "Bulgaria", "Poland", "Turkey",
                              "Greece")
if(em == "CO") group5 <- c("Belgium", "Georgia", "Romania", "Turkey", "Greece")

#   Subset by group5  
group <- group5
nt_group5 <- subset(nt_df, Country %in% group)

#   Plot group 5    
g5 <- plot(nt_group5)

#   Save group 5 plot to "/diagnostic-output/EMEP" in pdf form
print( g5 )
ggsave( paste0( em, "_group5", File_postscript, ".pdf" ), width=10.0, height=5.5)   

# Group 6 nations by emission species:
if(em == "SO2") group6 <- c("France", "Turkey", "Italy", "Spain", "Poland", "United Kingdom",
                            "Ukraine" )
if(em == "NOx") group6 <- c( "Poland", "France", "Italy", "United Kingdom", "Ukraine", "Spain")
if(em == "NMVOC") group6 <- c("Spain", "Italy", "France", "United Kingdom", "Ukraine")
if(em == "CO") group6 <- c("Spain", "Poland", "Italy", "United Kingdom", "France", "Ukraine")

#   Subset by group6
group <- group6
nt_group6 <- subset(nt_df, Country %in% group)

#   Plot group 6
g6 <- plot(nt_group6)

#   Save group 6 plot to "/diagnostic-output/EMEP" in pdf form
print( g6 )
ggsave( paste0( em, "_group6", File_postscript, ".pdf" ), width=10.0, height=5.5)    
    
# ------------------------------------------------------------------------------
# 4. Making Table of differences  Between CEDS Scaled Emissions & SNAP Emissions
#    for 1980 & 1990

# Remove Years 1981 - 1989 for CEDS Data
CEDS_NT2 <- CEDS_NT [, -(4:12)]

# Remove Years 1991 - 2014 for CEDS Data
CEDS_NT2 <- CEDS_NT2 [, -(5:28)]

# Rename Years Columns for CEDS Data
names(CEDS_NT2) <- c("Country", "DataSource", "CEDS_1980", "CEDS_1990")

# Remove Data Source Column for CEDs Data
CEDS_NT2$DataSource <- NULL

# Remove Year 1985 for SNAP Data
SNAP_NT2 <- SNAP_NT [, -(4)]

# Remove Years 1991 - 2013 for SNAP Data
SNAP_NT2 <- SNAP_NT2 [, -(5:27)]

# Rename Years Columns for SNAP Data
names(SNAP_NT2) <- c("Country", "DataSource", "SNAP_1980", "SNAP_1990")

# Remove Data Source Column for SNAP Data
SNAP_NT2$DataSource <- NULL

# Combine News CEDS df with new SNAP df 
nt_df2 <- cbind(CEDS_NT2, SNAP_NT2)

# Order Combined df by Country
nt_df2 <- nt_df2[ order(nt_df2$Country), ]

# Remove Second Country Column
nt_df2 <- nt_df2[,-(4)]

# Make df of Differences
Diff <- nt_df2

#   Make a Column for 1980 Difference ( Difference = CEDS - SNAP)
    Diff$Diff_1980 = (Diff$CEDS_1980 - Diff$SNAP_1980)

#   Make a column for 1990 Differences (Difference = CEDS - SNAP)
    Diff$Diff_1990 = (Diff$CEDS_1990 - Diff$SNAP_1990)

#   Remove Columns for CEDS & SNAP 1980 as well as 1990 Data
    Diff <- Diff [, -(2:5)]

#   Make columns for if 1980 & 1990 were scaled by CEDS system ('No' if not in scaling script or
#   if not EMEP level 1 Data for that nation, 'Yes' if scaled)

#       1980 Scaled Column
        Diff$Scaled_1980 <- "" 

#       1990 Scaled Column
        Diff$Scaled_1990 <- "" 

#   Reorder Columns of Interest
    Diff <- Diff[,c("Country", "Diff_1980", "Scaled_1980", "Diff_1990", "Scaled_1990")]

# List nations that were scaled to EMEP data --> Germany not in SNAP, Moldova not Scaled
  emep_scale <- c("aut", "bel", "bgr", "che", "cyp", "cze", "dnk", "esp", "est", "fin", "fra",
                "gbr", "geo", "hrv", "hun", "irl", "isl", "ita", "ltu", "lux", "lva", "mkd",
                "mlt", "nld", "nor", "pol", "prt", "rou", "srb", "svk", "svn", "swe", "tur")
  
#     cbind the list to make it a column
      emep_scale <- cbind (emep_scale)
    
#     Map 'iso' to 'Country_Name'
      emep_scale <- MCL[ match(emep_scale, MCL$iso),'Country_Name']
    
#     cbind the list to make it a column
      emep_scale <- cbind (emep_scale)
    
#     Order Countries Alphabetically  
      emep_scale <- emep_scale[ order(emep_scale), ]

#     cbind the list to make it a column
      emep_scale <- cbind (emep_scale)

# Scaled 1980
#     If the Country Was Scaled by the EMEP Scaling Script then Scaled_1980 = "Yes", if not  = "Not Scaled"
      Diff$Scaled_1980 <- ifelse(Diff$Country %in% emep_scale, "Yes" , "Not Scaled") 

#     If the Country has EMEP data for 1980 then Scaled_1980 = "Yes", if not = "No Data"
      no_emep_1980 <- subset(EMEP_NT, is.na(X1980))
      no_emep_data_1980 <- no_emep_1980$Country 
      Diff$Scaled_1980 <- ifelse(Diff$Country %in% no_emep_data_1980, "No Data", Diff$Scaled_1980)

# Scaled 1990
#     If the Country Was Scaled by the EMEP Scaling Script then Scaled_1990 = "Yes", if not  = "Not Scaled"
      Diff$Scaled_1990 <- ifelse(Diff$Country %in% emep_scale, "Yes" , "Not Scaled") 

#     If the Country has EMEP data for 1990 then Scaled_1980 = "Yes", if not = "No Data"
      no_emep_1990 <- subset(EMEP_NT, is.na(X1990))
      no_emep_data_1990 <- no_emep_1990$Country 
      Diff$Scaled_1990 <- ifelse(Diff$Country %in% no_emep_data_1990, "No Data", Diff$Scaled_1990)
# ------------------------------------------------------------------------------
# 5. Save all tables created
      
#  Reset working directory
    setwd( "..")
      
# A. Write Difference Table
    writeData(Diff,  domain = "DIAG_OUT", domain_extension = "EMEP/",
              fn = paste0( em, "_CEDS_SNAP_NT_Differences", File_postscript ), meta = FALSE)
      
# B. Write tables of the wide & long formatted National Totals
  # Changing working directory to "intermediate-output"
  setwd( "..")
  setwd( "./intermediate-output")
  
# Write CEDS long data as a csv file of CEDS national totals)
  writeData(CEDSlong, domain = "MED_OUT", fn = paste0( "F.", em, "_CEDS_national_totals, File_postscript" ),
            meta = FALSE )
             
# Write CEDS wide data (a csv file of CEDS national totals)  
  writeData(CEDS_NT, domain = "MED_OUT", fn = paste0( "F.", em, "_CEDS_wide_nat",File_postscript,".tot" ),
            meta = FALSE )
                
# Write EMEP 'as in models' long data (a csv file of SNAP national totals)
  writeData(SNAPlong, domain = "MED_OUT", fn = paste0( "F.", em, "_EMEP_models_national_totals" ),
            meta = FALSE )
                
# Write EMEP 'as in models' wide data (a csv file of SNAP national totals)               
  writeData(SNAP_NT, domain = "MED_OUT", fn = paste0( "F.", em, "_EMEP_models_wide_nat.tot" ),
            meta = FALSE )

#   Every script should finish with this line-
logStop()

# END
        