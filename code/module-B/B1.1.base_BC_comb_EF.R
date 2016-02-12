#------------------------------------------------------------------------------
# Program Name: B1.1.base_BC_comb_EF.R
# Author: Rachel Hoesly
# Date Last Updated: Feb 2016
# Program Purpose: 1. Produce BC emissions factors from Bond et al data.
#              Data priority - newest to oldes, 2010, 1996, 1990
# Input Files: 
#              Bond_ctry_mapping.csv, Bond_fuel_mapping.csv, Bond_sector_mapping.csv,
#              A.comb_activity.csv 
# Output Files: B.comb_EF_db.csv
# Notes: 1. Emission factors (ef) are calculated as emissions divided by consumption.
#           Missing (zero or NA) ef are replaced using the following rules, in order:
#           a. replace with ef of the same sector_fuel and region for the same year,
#              if available;
#           b. 
#           c. 
#           d. 
#        2. Variables with modifiable values: threshold_var.
# TODO: Add OC ef
#       Consolidate ef correction
#       iso codes do not uniquely identify countries
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
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
headers <- c( "data_functions.R", "analysis_functions.R", "process_db_functions.R", 
             "interpolation_extention_functions.R" ) # Additional function files required.
log_msg <- "Produce BC emissions factors from Bond et al data" # First message to be printed to the log
script_name <- "B1.1.base_BC_comb_EF.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "BC"

# ------------------------------------------------------------------------------
# 0.5 Define functions for later use

loadPackage('zoo')
all.na <- function(x){
  return(all(is.na(x)))}

interpolate_extend <- function (df){

  years <- names(df)[grep('X',names(df))] 
  interpolate <-  apply(X= df[years] , MARGIN= 1,FUN= function(x) any(is.na(na.trim(x))) )
  row.all.na <- apply(X= df[years], MARGIN = 1 ,FUN = all.na)
  
  # interpolate, constant extend forward and back
  df[interpolate,years] <- t (na.approx(t(df[interpolate,years]), na.rm=FALSE ) )
  df[,years] <-t( na.locf( t( df[,years] ), na.rm=FALSE ) )
  df[,years] <- t( na.locf( t( df[,years] ), fromLast = TRUE, na.rm=FALSE ) )
  
  return(df)
}


# ------------------------------------------------------------------------------
# 1. Read in files and do preliminary setup

activity_data <- readData( "MED_OUT", "A.comb_activity" )
MSL <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Sectors" , meta = F)
sector_level_map <- readData( "MAPPINGS", "Master_Sector_Level_map", meta = F )
MCL <- readData( "MAPPINGS", "Master_Country_List" )
  

bcoc_historical <- readData( "EM_INV", domain_extension = "Bond-BCOC/"  ,"Bond_BCOC_1925-2010", meta = F )
sector_map <- readData( "EM_INV", domain_extension = "Bond-BCOC/" , "Bond_BCOC_fuel_sector_map", meta = F )
iso_map <- readData( "EM_INV", domain_extension = "Bond-BCOC/" , "Bond_country_map", meta = F )

# ------------------------------------------------------------------------------
# 2. Calculate EFs

names(bcoc_historical) <- c("Region","Country", "Fuel" ,"Tech","Sector" ,"Year" ,
                            "Fuel_kt" ,"BC_kt", "OC_kt" )

bcoc_historical <- bcoc_historical[which( bcoc_historical$Fuel_kt >= 0 &
                                          bcoc_historical$BC_kt >= 0 &
                                          bcoc_historical$OC_kt >= 0     )  , ]

bcoc_historical_regions <- aggregate( bcoc_historical[,c("Fuel_kt" ,"BC_kt", "OC_kt")] , 
                               by = list( Region =  bcoc_historical$Region,
                                          Fuel =  bcoc_historical$Fuel,
                                          Tech =  bcoc_historical$Tech,
                                          Sector =  bcoc_historical$Sector,
                                          Year =  bcoc_historical$Year) ,
                               FUN = sum )

region_country_map <- unique(bcoc_historical[,c("Region","Country")])
iso_region_country_map <- iso_map
iso_region_country_map$Region  <- region_country_map[ match(iso_region_country_map$Country, region_country_map$Country ) , 
                                                      'Region']

bcoc_historical_efs <- bcoc_historical_regions[which(bcoc_historical_regions$Year >= 1960),]
bcoc_historical_efs$BC_ef <- bcoc_historical_efs$BC_kt/bcoc_historical_efs$Fuel_kt
bcoc_historical_efs$OC_ef <- bcoc_historical_efs$OC_kt/bcoc_historical_efs$Fuel_kt
bcoc_historical_efs <- bcoc_historical_efs[, c( "Region","Fuel" ,"Tech",
                                                "Sector" ,"Year", "BC_ef", "OC_ef") ]

bcoc_historical_efs$Year <- paste0('X',bcoc_historical_efs$Year)

X_bond_years <- sort(unique(bcoc_historical_efs$Year))

# ------------------------------------------------------------------------------
# 3. Map to Sectors/Fuels

bcoc_efs_sectors <- merge( bcoc_historical_efs , sector_map , all = TRUE )

bcoc_efs_sectors <- aggregate( bcoc_efs_sectors[,c("BC_ef", "OC_ef")] , 
                               by = list( Region = bcoc_efs_sectors$Region,
                                          Year = bcoc_efs_sectors$Year,
                                          sector = bcoc_efs_sectors$sector,
                                          fuel = bcoc_efs_sectors$fuel) ,
                               FUN = mean )

bc_sectors <- cast( bcoc_efs_sectors , Region + sector + fuel ~ Year, value = 'BC_ef')
bc_sectors <- interpolate_extend(bc_sectors)
bc_sectors <- bc_sectors[complete.cases(bc_sectors[, X_bond_years]),]

bc_sectors$aggregate_sector <- sector_level_map[  match(bc_sectors$sector , sector_level_map$working_sectors_v1 )
                                                        ,'aggregate_sectors']

bc_agg_sectors_fuel <- aggregate( bc_sectors[,X_bond_years] , 
                               by = list( agg_sector = bc_sectors$aggregate_sector,
                                          fuel = bc_sectors$fuel) ,
                               FUN = mean )

bc_agg_sectors <- aggregate( bc_sectors[,X_bond_years] , 
                                  by = list( Region = bc_sectors$Region,
                                             agg_sector = bc_sectors$aggregate_sector,
                                             fuel = bc_sectors$fuel) ,
                                  FUN = mean )

# ------------------------------------------------------------------------------
# 4. Map to iso and region

# Map from Bond Region to Bond Country

# Country and OECD
bc_iso <- merge(merge(bc_sectors, region_country_map, all = TRUE), 
                      iso_map, all = TRUE)
bc_iso$OECD <- MCL[ match(bc_iso$iso, MCL$iso) ,'OECD_flag'] 

bc_OECD <- aggregate( bc_iso[, X_bond_years ] , 
                            by = list( OECD = bc_iso$OECD,
                                       sector = bc_iso$sector,
                                       fuel = bc_iso$fuel) ,
                            FUN = mean )

bc_OECD_agg_sector <- aggregate( bc_iso[, X_bond_years ] , 
                      by = list( OECD = bc_iso$OECD,
                                 agg_sector = bc_iso$aggregate_sector,
                                 fuel = bc_iso$fuel) ,
                      FUN = mean )

bc_OECD_fuel <- aggregate( bc_iso[, X_bond_years ] , 
                           by = list( OECD = bc_iso$OECD,
                                      fuel = bc_iso$fuel) ,
                           FUN = mean )

bc_fuel <- aggregate( bc_iso[, X_bond_years ] , 
                           by = list( fuel = bc_iso$fuel) ,
                           FUN = mean )

bc_iso <- aggregate( bc_iso[, X_bond_years ] , 
                     by = list( iso = bc_iso$iso,
                                sector = bc_iso$sector,
                                fuel = bc_iso$fuel) ,
                     FUN = mean )

# ------------------------------------------------------------------------------
# 4. Make emission template from activity df

bc_ef_template <- activity_data[ , c('iso','sector','fuel')]
bc_ef_template[ X_bond_years ] <- NA

# Add by aggregate flags
bc_ef_template$Region <- iso_region_country_map[ match(bc_ef_template$iso , iso_region_country_map$Region)   ,'Region']
bc_ef_template$OECD <- MCL[ match(bc_ef_template$iso, MCL$iso)    ,'OECD_flag'] 
bc_ef_template$agg_sector <- sector_level_map[  match(bc_ef_template$sector , sector_level_map$working_sectors_v1 ) ,'aggregate_sectors']

# fill template with Bond efs
bc_ef <- replaceValueColMatch(bc_ef_template , bc_iso ,
                              x.ColName = X_bond_years ,
                              match.x = c('iso','sector','fuel'), 
                              addEntries = FALSE)

bc_ef_nas <- bc_ef[is.na(bc_ef$X1960),]
bc_ef <- bc_ef[!is.na(bc_ef$X1960),]

# ------------------------------------------------------------------------------
# 5. Fill missing EFs

# fill with region agg sector fuel
bc_ef_nas <- replaceValueColMatch( bc_ef_nas , bc_agg_sectors ,
                              x.ColName = X_bond_years ,
                              match.x = c('Region','agg_sector','fuel'), 
                              addEntries = FALSE)

bc_ef <- rbind( bc_ef , bc_ef_nas[!is.na(bc_ef_nas$X1960),] )
bc_ef_nas <- bc_ef_nas[is.na(bc_ef_nas$X1960),]

# fill with average OECD, and sector ,fuel 
bc_ef_nas <- replaceValueColMatch( bc_ef_nas , bc_OECD ,
                                   x.ColName = X_bond_years ,
                                   match.x = c('OECD','sector','fuel'), 
                                   addEntries = FALSE)

bc_ef <- rbind( bc_ef , bc_ef_nas[!is.na(bc_ef_nas$X1960),] )
bc_ef_nas <- bc_ef_nas[is.na(bc_ef_nas$X1960),]

# fill with average OECD, and agg_sector ,fuel 
bc_ef_nas <- replaceValueColMatch( bc_ef_nas , bc_OECD_agg_sector ,
                                   x.ColName = X_bond_years ,
                                   match.x = c('OECD','agg_sector','fuel'), 
                                   addEntries = FALSE)

bc_ef <- rbind( bc_ef , bc_ef_nas[!is.na(bc_ef_nas$X1960),] )
bc_ef_nas <- bc_ef_nas[is.na(bc_ef_nas$X1960),]

# fill with global, agg_sector ,fuel 
bc_ef_nas <- replaceValueColMatch( bc_ef_nas , bc_agg_sectors_fuel ,
                                   x.ColName = X_bond_years ,
                                   match.x = c('agg_sector','fuel'), 
                                   addEntries = FALSE)

bc_ef <- rbind( bc_ef , bc_ef_nas[!is.na(bc_ef_nas$X1960),] )
bc_ef_nas <- bc_ef_nas[is.na(bc_ef_nas$X1960),]

# fill with OECD ,fuel 
bc_ef_nas <- replaceValueColMatch( bc_ef_nas , bc_OECD_fuel ,
                                   x.ColName = X_bond_years ,
                                   match.x = c('OECD','fuel'), 
                                   addEntries = FALSE)

bc_ef <- rbind( bc_ef , bc_ef_nas[!is.na(bc_ef_nas$X1960),] )
bc_ef_nas <- bc_ef_nas[is.na(bc_ef_nas$X1960),]

# fill with fuel 
bc_ef_nas[ X_bond_years ] <- bc_fuel[ match( bc_ef_nas$fuel,bc_fuel$fuel ) , X_bond_years ] 
bc_ef <- rbind( bc_ef , bc_ef_nas[!is.na(bc_ef_nas$X1960),] )
bc_ef_nas <- bc_ef_nas[is.na(bc_ef_nas$X1960),]

# ------------------------------------------------------------------------------
# 7. Final Processing

bc_ef <- bc_ef[ with( bc_ef, order( iso, sector, fuel ) ), ]

final <- bc_ef
final$units <- 'kt/kt'
final[X_emissions_years] <- NA
final[X_bond_years] <- bc_ef[ X_bond_years ]

final <- final[,c('iso','sector','fuel', 'units', X_emissions_years)]

final <- interpolate_extend(final)


# ------------------------------------------------------------------------------
# 7. Write output

writeData( final, "MED_OUT", paste0( "B.",em,"_comb_EF_db" ) )

# Every script should finish with this line
logStop()

# END
