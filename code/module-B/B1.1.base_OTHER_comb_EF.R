# Program Name: B1.1.base_OTHER_comb_EF.R
# Author: Rachel Hoesly
# Date Last Updated: 19 Jan 2016 
# Program Purpose: Generate base emission factors from global GAINS EMF-30 data
#                  for NOx, NMVOC, CH4, CO
#
# Input Files:    files in the EF_parameters folder contailing control_percent and em
#               
# Output Files:  
# Notes: transportation_rail only hase a 2020 values, so interpolated values are constant
#           extended back from 2020 to 2011
# TODO:    # Replace iso-sector-fuel with data in the following order
# 1. Region Average
# 2. Aggregate Sector
# 3. Region, OECD flag over all sectors
#        
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers

# Before we can load headers we need some paths defined. They may be provided by
#   a system environment variable or may have already been set in the workspace.
dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
for ( i in 1:length( dirs ) ) {
  setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
  wd <- grep( 'CEDS/input', list.dirs(), value = T )
  if ( length( wd ) > 0 ) {
    setwd( wd[ 1 ] )
    break
  }
}
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
headers <- c( 'process_db_functions.R','data_functions.R',
              'interpolation_extention_functions.R','common_data.R', 'analysis_functions.R') 
#                 Additional function files may be required.
log_msg <- "Processing GAINS EMF-30 data. Using as base comb EF where appropriate" 
# First message to be printed to the log
script_name <- 'B1.1.base_OTHER_comb_EF.R'

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "NMVOC"
em_lc <- tolower( em )   

# Stop script if running for unsupported species
if ( em %!in% c('NOx','NMVOC','CH4', 'CO','CH4') ) {
  stop (paste( 'GAINS EMF-30 is not supported for emission species', em, 'remove from script
               list in B1.2.add_comb_EF.R and/or makefile'))
}
# ---------------------------------------------------------------------------
# 1. Load Data

activity_data <- readData( "MED_OUT", "A.comb_activity" )

Master_Country_List <- readData(domain = 'MAPPINGS', file_name = 'Master_Country_List')
Master_Sector_Level_map <- readData(domain = 'MAPPINGS', file_name = 'Master_Sector_Level_map')

gainsEMF30_comb <- readData(domain = "MED_OUT", file_name = paste0('B.',em,'_comb_EF_GAINS_EMF30'))

if (em == 'NOx') aviation_EF_load <- readData(domain = 'DEFAULT_EF_IN', file_name = 'Aviation_base_EF', '.xlsx',
                                              sheet_selection = 'EF')

# ---------------------------------------------------------------------------
# 1.5 Check master country list

check.country.list <- Master_Country_List[!is.na(Master_Country_List$iso),]

if(any(is.na(check.country.list$OECD_flag)) | any(is.na(check.country.list$Region)))
{
  stop('NAs in OECD flag and Region columns in Master Country List. Cannot estimate base
       emission factors. Please Check Master Country List.')
}

# ---------------------------------------------------------------------------
# 2. Extend EMF30 data

printLog('Extending gains EMF-30 data')
emf_ext_years <- paste0('X',start_year:1999)
default_extended <- gainsEMF30_comb
default_extended[,emf_ext_years] <- gainsEMF30_comb[,'X2000']
default_extended <- default_extended[,c('iso','sector','fuel','units',X_emissions_years)]

index <- which(apply(X=default_extended[,X_emissions_years],MARGIN=1, function(x) any(is.na(x)) ) ) 

default_extended <- rbind(default_extended[-index,], 
                          extendValues(default_extended[index,], pre_ext_default = 'constant',
                                 post_ext_default = 'constant'))

# ---------------------------------------------------------------------------
# 2. Create Default Database
printLog('Creating default database. Estimating missing emission factors.')
activity_data <- activity_data [ with( activity_data , order( iso, sector, fuel ) ), ]
default_wide <- merge(activity_data[,c('iso','sector','fuel')], default_extended, all.x = TRUE, sort=F)

default_efs <- default_wide[,c('iso','sector','fuel','units',X_emissions_years)]


#Replace aviation

default_efs[which(default_efs$sector %in% c('1A3ai_International-aviation', '1A3aii_Domestic-aviation')),X_emissions_years] <- 0

if (em == 'NOx'){
  aviation_EF <- ddply(aviation_EF_load, .(fuel,units,years), summarize,
                       ef= mean(NOx))
  aviation_EF$sector <- '1A3ai_International-aviation'
  aviation_EF2 <- aviation_EF
  aviation_EF2$sector <- '1A3aii_Domestic-aviation'
  aviation_EF <- rbind(aviation_EF,aviation_EF2)
  aviation_EF_wide <- cast(aviation_EF, fuel+units+sector~years, value = 'ef')
  aviation_EF_wide_extended <- extendValues(aviation_EF_wide)
  
  default_efs <- replaceValueColMatch ( default_efs,aviation_EF_wide_extended,
                                         x.ColName = X_emissions_years,
                                         y.ColName = X_emissions_years,
                                         match.x= c('sector','fuel','units'),
                                         match.y= c('sector','fuel','units'),
                                         addEntries = FALSE)
}


# Replace iso-sector-fuel with data in the following order
# 1. Region Average
# 2. Aggregate Sector
# 3. Region, OECD flag over all sectors

# Add Aggregate Identifiers (Region, OECD, aggregate sector)

default_efs <- merge(default_efs, unique(Master_Country_List[,c('iso','Region')]),
                      all.x=TRUE, all.y = FALSE)
default_efs <- merge(default_efs, unique(Master_Sector_Level_map[,c('working_sectors_v1','aggregate_sectors')]),
                      by.x='sector', by.y='working_sectors_v1',
                      all.x=TRUE, all.y = FALSE)
# rename aviation it's own sector
default_efs[which(default_efs$sector %in% c('1A3ai_International-aviation', 
                                              '1A3aii_Domestic-aviation')),'aggregate_sectors'] <- 'Aviation'
default_efs <- merge(default_efs, unique(Master_Country_List[,c('iso','OECD_flag')]),
                      all.x=TRUE, all.y = FALSE)

default_efs$units <- 'kt/kt'

# Original Default EF's, seperate na and values
original_default_efs <- default_efs[which(!is.na(default_efs$X1960)),]
default_efs_values <- default_efs[which(!is.na(default_efs$X1960)),]
default_efs_na <- default_efs[which(is.na(default_efs$X1960)),]

# Add region estimate (region, sector, fuel, year)
printLog('Adding region estimates')

region_estimates <- aggregate( original_default_efs[ X_emissions_years ],
  by = list( Region = original_default_efs$Region,
             sector = original_default_efs$sector, 
             fuel = original_default_efs$fuel,
             units = original_default_efs$units), mean ) 

default_efs <- replaceValueColMatch ( default_efs_na,region_estimates,
                                       x.ColName = X_emissions_years,
                                       y.ColName = X_emissions_years,
                                       match.x= c('Region','sector','fuel','units'),
                                       match.y= c('Region','sector','fuel','units'),
                                       addEntries = FALSE)

default_efs_values <- rbind(default_efs_values, default_efs[which(!is.na(default_efs$X1960)),])
default_efs_na <- default_efs[which(is.na(default_efs$X1960)),]

# Add aggregate sector estimate (iso, aggregate_sector, fuel, year)
printLog('Adding aggregate sector estimates')
aggregate_sector_estimates <- aggregate( original_default_efs[ X_emissions_years ],
                                by = list( iso = original_default_efs$iso,
                                           aggregate_sectors = original_default_efs$aggregate_sectors, 
                                           fuel = original_default_efs$fuel,
                                           units = original_default_efs$units), mean )

default_efs <- replaceValueColMatch ( default_efs_na,aggregate_sector_estimates,
                                       x.ColName = X_emissions_years,
                                       y.ColName = X_emissions_years,
                                       match.x= c('iso','aggregate_sectors','fuel','units'),
                                       match.y= c('iso','aggregate_sectors','fuel','units'),
                                       addEntries = FALSE)

default_efs_values <- rbind(default_efs_values, default_efs[which(!is.na(default_efs$X1960)),])
default_efs_na <- default_efs[which(is.na(default_efs$X1960)),]

# Add aggregate sector estimate (region, aggregate_sector, fuel, year)
printLog('Adding aggregate sector estimates')
Region_aggregate_sector_estimates <- aggregate( original_default_efs[ X_emissions_years ],
                                         by = list( Region = original_default_efs$Region,
                                                    aggregate_sectors = original_default_efs$aggregate_sectors, 
                                                    fuel = original_default_efs$fuel,
                                                    units = original_default_efs$units), mean )

default_efs <- replaceValueColMatch ( default_efs_na,Region_aggregate_sector_estimates,
                                      x.ColName = X_emissions_years,
                                      y.ColName = X_emissions_years,
                                      match.x= c('Region','aggregate_sectors','fuel','units'),
                                      match.y= c('Region','aggregate_sectors','fuel','units'),
                                      addEntries = FALSE)

default_efs_values <- rbind(default_efs_values, default_efs[which(!is.na(default_efs$X1960)),])
default_efs_na <- default_efs[which(is.na(default_efs$X1960)),]

# Add average OECD region fuel (Region, OECD, year) (not sector)
printLog('Adding region over all sector estimates')
OECD_Region_estimates <- aggregate( original_default_efs[ X_emissions_years ],
                                         by = list( Region = original_default_efs$Region,
                                                    OECD_flag = original_default_efs$OECD_flag, 
                                                    fuel = original_default_efs$fuel,
                                                    units = original_default_efs$units), mean )

default_efs <- replaceValueColMatch ( default_efs_na,OECD_Region_estimates,
                                       x.ColName = X_emissions_years,
                                       y.ColName = X_emissions_years,
                                       match.x= c('Region','OECD_flag','fuel','units'),
                                       match.y= c('Region','OECD_flag','fuel','units'),
                                       addEntries = FALSE)

default_efs_values <- rbind(default_efs_values, default_efs[which(!is.na(default_efs$X1960)),])
default_efs_na <- default_efs[which(is.na(default_efs$X1960)),]


# Final Processing
base_efs <- default_efs_values[,c('iso','sector','fuel','units',X_emissions_years)]
  
if( nrow( default_efs_na ) > 0 ) stop( paste(
  'NA in default', em,'EFs. Please check B1.1.base_OTHER_comb_EF.R') )
if ( any( base_efs[X_emissions_years] == Inf) ) stop( paste(
  'Inf in default', em,'EFs. Please check B1.1.base_OTHER_comb_EF.R') )

if( nrow( base_efs ) != nrow( activity_data )) stop( paste(
  'Not the same number of rows in activity data and new default emissions for ', em,'. Please check B1.1.base_OTHER_comb_EF.R') )

# Sort
printLog('Sorting')
base_efs <-  base_efs[ with( default_efs_values, order( iso, sector, fuel ) ), ]


if(!all(activity_data[,1]==base_efs[,1]) |
   !all(activity_data[,2]==base_efs[,2]) |
   !all(activity_data[,3]==base_efs[,3]) ) stop('Default Emissions do not match Activity Data. 
                                                Check B1.1base_OTHER_comb_EF.R' )

# ---------------------------------------------------------------------------
# 6. Output

writeData(base_efs, domain = "MED_OUT", fn = paste0('B.',em,'_comb_EF_db'))


logStop()

# END

