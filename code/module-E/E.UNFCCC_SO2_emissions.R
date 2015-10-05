# ------------------------------------------------------------------------------
# Program Name: E1.UNFCCC_SO2_emissions.R
# Author(s): Patrick O'Rourke
# Date Last Updated: October 5th, 2015
# Program Purpose: To read in and reformat UNFCCC SO2 emissions data.
# Input Files: All UNFCCC SO2 Emissions Data
# Output Files: E.SO2_UNFCCC_ctry_emissions_.csv
# Notes: 1. UNFCCC SO2 Emissions are provided from 1990-2012.
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
INPUT<-paste(getwd())
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R" ) # Any additional function files required
log_msg <- "Initial reformatting of the UNFCCC S02 Emissions inventories" # First message to be printed to the log
script_name <- "E.UNFCCC_SO2_emissions.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# -----------------------------------------------------------------------------------------------------------
#0.5 Settings/Load Files

MCL <- readData( "MAPPINGS", "Master_Country_List" )
loadPackage('tools')

# Location of input files relative to working directory/set new directory (so that only UNFCCC SO2 data
# is read in)

setwd( "./emissions-inventories/UNFCCC")


# -----------------------------------------------------------------------------------------------------------
# 1. Read in files
# Use the readData() function (defined in IO_functions.R) for all input data

# Create a List of the UNFCCC S02 Files
file.list = list.files( pattern = "*SO2" )

# Removes Extension of File Name
file.list2 <- file_path_sans_ext( file.list ) 

# Naviagte Back to Home Directory (/input)
setwd(INPUT)

# Location of Input Files Relative to the Working Directory
input_domain <- "UNFCCC_SO2_IN" 

# Imports & Gives List of data Frames (works)
UNFCCC_SO2 <- lapply ( X = file.list2, FUN = readData, domain = "UNFCCC_SO2_IN",
header = FALSE)


# -----------------------------------------------------------------------------------------------------------
# 2. Formatting Data

UNFCCC_SO2_clean<-UNFCCC_SO2

for (i in seq_along(UNFCCC_SO2_clean)){
  df<-UNFCCC_SO2_clean[[i]]
  
  # Make a Variable called Sector
  df$sector <- file.list2[i]
  
  # Removes First Row
  df <- df[-1,]
  
  # Reformat Col Names
  names <- as.character( unlist ( df[ 1, ] ) )
  years<-paste("X",names[3:(length(names)-1)],sep="")
  names[3:(length(names)-1)]<-years
  names[length(names)]<-'sector'
  names[1]<-'country'
  names(df)<-names
  
  # Remove First Row
  df <- df[-1,]
  
  # Creates Column for Units (Gg for SO2)
  df$units <- "kt"
  # Reoorder Columns of Interest
  df<-df[,c('country','sector','units',years)]
  
  # Remove All Information from Sectors Before "_" From File Name
  df <- mutate( df, sector = as.character( sector ) )
  df <- mutate( df, sector = sapply( strsplit( df$sector, split = '_', fixed = TRUE ), function( x ) ( x [ 2 ] ) ) )
  
  UNFCCC_SO2_clean[[i]]<-df
  }

# Make the List of Files 1 Data Frame
UNFCCC_SO2df <- do.call( rbind, UNFCCC_SO2_clean)

# Convert Values to Numeric: Remove Commas in Formatting, then Convert to Numeric
UNFCCC_SO2df[years]<-apply(X=UNFCCC_SO2df[years],MARGIN=2,FUN=sub,pattern=',',replacement="")
UNFCCC_SO2df[years]<-as.numeric(as.matrix(UNFCCC_SO2df[years]))

# Mapping Country Names to ISO Codes
UNFCCC_SO2df$iso<-MCL[match(UNFCCC_SO2df$country,MCL$UNFCCC),'iso']

# Remove Unmapped Lines and Reorder
UNFCCC_SO2df<-UNFCCC_SO2df[complete.cases(UNFCCC_SO2df$iso),]

UNFCCC_SO2df<-UNFCCC_SO2df[,c('iso','sector','units',years)]
UNFCCC_SO2df<-UNFCCC_SO2df[order(UNFCCC_SO2df$iso,UNFCCC_SO2df$sector),]

# ------------------------------------------------------------------------------
# 3. Removed "Bad" Data

# Remove Canada, Russian Fed, Luxembourg, and Poland
remove_iso<-c('can','rus','pol','lux')
UNFCCC_SO2<-UNFCCC_SO2df[-which(UNFCCC_SO2df$iso %in% remove_iso),]

# Drop Lines With Only NA Values
drop<-which(apply(X=is.na(UNFCCC_SO2[years]),MARGIN=1,FUN=all)==TRUE)
UNFCCC_SO2<-UNFCCC_SO2[-drop,]

# ------------------------------------------------------------------------------
# 4. Meta Data

meta_names <- c( "Data.Type", "Emission", "Region", "Sector", "Start.Year", "End.Year", 
                 "Source.Comment" )

meta_note <- c( "Default SO2 Emissions", "NA", "Russian Federation, Monaco & Liechtenstein", "All", "1990", 
                "2012", paste0( "The Russian Federation's emissions are too low to be accurate," , 
                "and have thus been removed. Additionally Liechtenstein and Monaco emissions", 
                "have been removed temporarily."))
addMetaData( meta_note, meta_names)

# ------------------------------------------------------------------------------
# 5. Output

writeData(UNFCCC_SO2, domain = "MED_OUT", fn = paste0( "E.", "SO2", "_UNFCCC_inventory" ),
meta = TRUE )

# Every script should finish with this line-
logStop()

# END
