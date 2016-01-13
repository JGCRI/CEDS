# Program Name: B1.1.base_OTHER_comb_EF.R
# Author: Rachel Hoesly
# Date Last Updated: 8 Jan 2016 
# Program Purpose: Generate base emission factors from global GAINS EMF-30 data
#                  for NOx, CO, 
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
if ( is.na( em ) ) em <- "NOx"
em_lc <- tolower( em )   

# Stop script if running for unsupported species
if ( em %!in% c('NOx','NMVOC','CO','CH4') ) {
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
# 2. Extend EMF30 data
  #this command takes about 6 minutes to run
  printLog('Extending gains EMF-30 data')
  default_wide_extended <- extendValues(ext_data = gainsEMF30_comb, 
                                        pre_ext_default = 'constant', pre_ext_year = start_year)
  writeData(default_wide_extended, 'DIAG_OUT', paste('TEMP.',em,'_GAINS_extended'))
  # default_wide_extended <- readData('DIAG_OUT', paste('TEMP.',em,'_GAINS_extended') )
# ---------------------------------------------------------------------------
# 2. Create Default Database
  printLog('Creating default database. Estimating missing emission factors.')
  activity_data <- activity_data [ with( activity_data , order( iso, sector, fuel ) ), ]
  default_wide <- merge(activity_data[,c('iso','sector','fuel')], default_wide_extended, all.x = TRUE, sort=F)
  default_wide$units <- 'kt/kt'
  
  default_long <- melt(default_wide, id.vars = c('iso', 'sector', 'fuel', 'units'))
  names(default_long) <- c("iso","sector","fuel","units","year","ef")
  
  #Replace aviation
  
  default_long[which(default_long$sector %in% c('1A3ai_International-aviation', '1A3aii_Domestic-aviation')),'ef'] <- 0
  
  if (em == 'NOx'){
    
    aviation_EF <- ddply(aviation_EF_load, .(fuel,units,years), summarize,
                         ef= mean(NOx))
    aviation_EF$sector <- '1A3ai_International-aviation'
    aviation_EF2 <- aviation_EF
    aviation_EF2$sector <- '1A3aii_Domestic-aviation'
    aviation_EF <- rbind(aviation_EF,aviation_EF2)
    aviation_EF_wide <- cast(aviation_EF, fuel+units+sector~years, value = 'ef')
    aviation_EF_wide_extended <- extendValues(aviation_EF_wide)
    
    aviation_EF_long <- melt(aviation_EF_wide_extended, id.vars = c('fuel','sector','units'))
    
    default_long <- replaceValueColMatch ( default_long,aviation_EF_long,
                                           x.ColName = 'ef',
                                           y.ColName = 'value',
                                           match.x= c('sector','fuel','units','year'),
                                           match.y= c('sector','fuel','units','variable'),
                                           addEntries = FALSE)
  }
  
  
  # Replace iso-sector-fuel with data in the following order
  # 1. Region Average
  # 2. Aggregate Sector
  # 3. Region, OECD flag over all sectors

  # Add Aggregate Identifiers (Region, OECD, aggregate sector)

  default_long <- merge(default_long, unique(Master_Country_List[,c('iso','Region')]),
                        all.x=TRUE, all.y = FALSE)
  default_long <- merge(default_long, unique(Master_Sector_Level_map[,c('working_sectors_v1','aggregate_sectors')]),
                        by.x='sector', by.y='working_sectors_v1',
                        all.x=TRUE, all.y = FALSE)
  # rename aviation it's own sector
  default_long[which(default_long$sector %in% c('1A3ai_International-aviation', 
                        '1A3aii_Domestic-aviation')),'aggregate_sectors'] <- 'Aviation'
  default_long <- merge(default_long, unique(Master_Country_List[,c('iso','OECD_flag')]),
                        all.x=TRUE, all.y = FALSE)

  # Seperate values and nas
  
  default_long_values <- default_long[which(!is.na(default_long$ef)),]
  default_long <- default_long[which(is.na(default_long$ef)),]
  
  # Add region estimate (region, sector, fuel, year)
  printLog('Adding region estimates')
  region_estimates <- ddply (default_long_values, .( Region, sector,fuel, year), summarize,
                             averageEF= mean(ef, rm.na = TRUE))
  default_long <- replaceValueColMatch ( default_long,region_estimates,
                                         x.ColName = 'ef',
                                         y.ColName = 'averageEF',
                                         match.x= c('Region','sector','fuel','year'),
                                         match.y= c('Region','sector','fuel','year'),
                                         addEntries = FALSE)
  
  default_long_values <- rbind.fill(default_long_values,
                                default_long[which(!is.na(default_long$ef)),])
  default_long <- default_long[which(is.na(default_long$ef)),]
  
  # Add aggregate sector estimate (iso, aggregate_sector, fuel, year)
  printLog('Adding aggregate sector estimates')
  aggregate_sector_estimates <- ddply (default_long_values, .( iso, aggregate_sectors,fuel, year), summarize,
                             averageEF= mean(ef, rm.na = TRUE))
  default_long <- replaceValueColMatch ( default_long,aggregate_sector_estimates,
                                         x.ColName = 'ef',
                                         y.ColName = 'averageEF',
                                         match.x= c('iso','aggregate_sectors','fuel','year'),
                                         match.y= c('iso','aggregate_sectors','fuel','year'),
                                         addEntries = FALSE)
  default_long_values <- rbind.fill(default_long_values,
                                    default_long[which(!is.na(default_long$ef)),])
  default_long <- default_long[which(is.na(default_long$ef)),]
  
  # FIll out all missing: Add average OECD region fuel (Region, OECD, year) (not sector)
  printLog('Adding region over all sector estimates')
  OECD_Region_estimates <- ddply (default_long_values, .( Region, OECD_flag, fuel,year), summarize,
                                       averageEF= mean(ef, rm.na = TRUE))
  default_long <- replaceValueColMatch ( default_long,OECD_Region_estimates,
                                         x.ColName = 'ef',
                                         y.ColName = 'averageEF',
                                         match.x= c('Region','OECD_flag','fuel','year'),
                                         match.y= c('Region','OECD_flag','fuel','year'),
                                         addEntries = FALSE)
  
  default_long_values <- rbind.fill(default_long_values,
                                    default_long[which(!is.na(default_long$ef)),])
  default_long <- default_long[which(is.na(default_long$ef)),]
  
    
  if( nrow( default_long ) >0 ) stop( paste(
    'NA in default', em,'emissions. Please check B1.1.base_OTHER_comb_EF.R') )
 
  printLog('Casting to wide format')   
  default_wide<- as.data.frame(cast(default_long_values, iso + sector + fuel + units ~ year, value = 'ef'),stringsAsFactors=FALSE)

# Sort
  printLog('Sorting')
  base_efs <-  default_wide[ with( default_wide, order( iso, sector, fuel ) ), ]

if(!all(activity_data[,1]==base_efs[,1]) |
  !all(activity_data[,2]==base_efs[,2]) |
  !all(activity_data[,3]==base_efs[,3]) ) stop('Default Emissions do not match Activity Data. 
                                               Check B1.1base_OTHER_comb_EF.R' )
  
# ---------------------------------------------------------------------------
# 6. Output

  writeData(default_wide, domain = "MED_OUT", fn = paste0('B.',em,'_comb_EF_db'))
  
  
  logStop()
  
# END

