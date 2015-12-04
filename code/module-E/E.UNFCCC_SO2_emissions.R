# ------------------------------------------------------------------------------
# Program Name: E1.UNFCCC_emissions.R
# Author(s): Patrick O'Rourke, Rachel Hoesly
# Date Last Updated: October 30, 2015
# Program Purpose: To read in and reformat UNFCCC emissions data.
# Input Files: All UNFCCC Emissions Data
# Output Files: E. em _UNFCCC_inventory
# Notes: UNFCCC  Emissions are provided from 1990-2012.
# TODO: This has only been tested with SO2 emissions.

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
script_name <- "E.UNFCCC_emissions.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# -----------------------------------------------------------------------------------------------------------
#0.5 Settings/Load Files

MCL <- readData( "MAPPINGS", "Master_Country_List" )
loadPackage('tools')


# -----------------------------------------------------------------------------------------------------------
# 1. Read in files
# Use the readData() function (defined in IO_functions.R) for all input data

# Emission Species
args_from_makefile <- commandArgs( TRUE )
em <<- args_from_makefile[1]
if ( is.na( em ) ) em <- "SO2"

# Create a List of the UNFCCC S02 Files
file.list = list.files(path = "./emissions-inventories/UNFCCC", pattern = paste0('*',em) )

# Removes Extension of File Name
file.list2 <- file_path_sans_ext( file.list ) 

# Location of Input Files Relative to the Working Directory
input_domain <- "UNFCCC_IN" 

# Imports & Gives List of data Frames (works)
UNFCCC <- lapply ( X = file.list2, FUN = readData, domain = "UNFCCC_IN",
header = FALSE)


# -----------------------------------------------------------------------------------------------------------
# 2. Formatting Data

UNFCCC_clean<-UNFCCC

for (i in seq_along(UNFCCC_clean)){
  df<-UNFCCC_clean[[i]]
  
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
  
  UNFCCC_clean[[i]]<-df
  }

# Make the List of Files 1 Data Frame
UNFCCCdf <- do.call( rbind, UNFCCC_clean)

# Convert Values to Numeric: Remove Commas in Formatting, then Convert to Numeric
UNFCCCdf[years]<-apply(X=UNFCCCdf[years],MARGIN=2,FUN=sub,pattern=',',replacement="")
UNFCCCdf[years]<-as.numeric(as.matrix(UNFCCCdf[years]))

# Mapping Country Names to ISO Codes
UNFCCCdf$iso<-MCL[match(UNFCCCdf$country,MCL$UNFCCC),'iso']

# Remove Unmapped Lines and Reorder
UNFCCCdf<-UNFCCCdf[complete.cases(UNFCCCdf$iso),]

UNFCCCdf<-UNFCCCdf[,c('iso','sector','units',years)]
UNFCCCdf<-UNFCCCdf[order(UNFCCCdf$iso,UNFCCCdf$sector),]

# ------------------------------------------------------------------------------
# 3. Removed "Bad" Data

# Remove Canada, Russian Fed, Luxembourg, and Poland
remove_iso<-c('can','rus','pol','lux')
UNFCCC<-UNFCCCdf[-which(UNFCCCdf$iso %in% remove_iso),]

# Drop Lines With Only NA Values
drop<-which(apply(X=is.na(UNFCCC[years]),MARGIN=1,FUN=all)==TRUE)
UNFCCC<-UNFCCC[-drop,]

#debug
UNFCCCdf<-UNFCCCdf[,-4]

# ------------------------------------------------------------------------------
# 4. Meta Data

meta_names <- c( "Data.Type", "Emission", "Region", "Sector", "Start.Year", "End.Year", 
                 "Source.Comment" )

meta_note <- c( "Default Emissions", "NA", "Russian Federation, Monaco & Liechtenstein", "All", "1990", 
                "2012", paste0( "The Russian Federation's emissions are too low to be accurate," , 
                "and have thus been removed. Additionally Liechtenstein and Monaco emissions", 
                "have been removed temporarily."))
addMetaData( meta_note, meta_names)

# ------------------------------------------------------------------------------
# 5. Output

writeData(UNFCCC, domain = "MED_OUT", fn = paste0( "E.", em, "_UNFCCC_inventory" ),
meta = TRUE )


# Every script should finish with this line-
logStop()

# END
