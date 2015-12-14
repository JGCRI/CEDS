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



#Set directory and paths
dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
for ( i in 1:length( dirs ) ) {
  setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
  wd <- grep( 'CEDS/input', list.dirs(), value = T )
  if ( length(wd) > 0 ) {
    setwd( wd[1] )
    break
  }
} 
MED_OUT <- "../intermediate-output/"

# ------------------------------------------------------------------------------
#Define Country to examine by iso code

country <- c( "aus" , "aut" , "bel" , "bgr" , "blr" , "che" , "cyp" , "cze" , "deu" , "dnk" , "esp",
             "est" , "fin" , "fra" , "gbr" , "grc" , "hrv" , "hun" , "irl" , "isl" , "ita" , "jpn",  
             "ltu" , "lva" , "mlt" , "nld" , "nor" , "nzl" , "prt" , "rou" , "svk" , "svn" , "swe" ,
             "tur" , "ukr" , 'usa' ,  'can')

# country <- c("can" )


# ------------------------------------------------------------------------------

#Load Files
default.in <- read.csv(paste(MED_OUT, "D.SO2_default_total_emissions.csv", sep=""), stringsAsFactors=FALSE)
scaled.in <- read.csv(paste(MED_OUT, "F.SO2_scaled_emissions.csv", sep=""), stringsAsFactors=FALSE)

# Redefine list of regions/iso if "all"
if (country == 'all')  country <- unique(default.in$iso)

setwd('../diagnostic-output')

# ------------------------------------------------------------------------------

#Analysis
default<-default.in[which(default.in$iso %in% country),]
scaled<-scaled.in[which(scaled.in$iso %in% country),]
ratio<-default
ratio[,5:58]<-as.matrix(scaled[,5:58])/as.matrix(default[,5:58])
ratio<-ratio[,-4]
# the data frame ratio gives the values of the scaled emission/default emission

write.csv(ratio,'scaling_diagnostics.csv')

