#------------------------------------------------------------------------------
# Program Name: B1.1.base_BCOC_comb_EF.R
# Author: Rachel Hoesly, Linh Vu
# Date Last Updated: 14 April 2016
# Program Purpose: 1. Produce OC emissions factors from SPEW (i.e. Bond) data.
#              
# Input Files: Bond_ctry_mapping.csv, Bond_fuel_mapping.csv, Bond_sector_mapping.csv,
#              A.comb_activity.csv 
# Output Files: B.[em]_comb_EF_db.csv, B.[em]_SPEW_comb_EF.csv, B.[em]_SPEW_NC_em.csv
# Notes: 1. Emission factors (ef) are calculated as emissions divided by consumption.
#           Missing (zero or NA) ef are replaced using the following rules, in order:
#           a. FSU residential coal-oil-gas replaced with FSU industrial coal oil gas
#           b. resdiential biomass replaced with iso sector fuel where available
#           c. others replaced with Region sector fuel EF where available
#           d. then replace with region fuel average
#           e. then global sector fuel average
#           f. then global fuel average

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
log_msg <- "Produce OC BC emissions factors from SPEW data" # First message to be printed to the log
script_name <- "B1.1.base_BCOC_comb_EF.R"

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
MSLevel <- readData( "MAPPINGS", "Master_Sector_Level_map" )

bcoc_historical <- readData( "EM_INV" ,"160227_SPEW_BCOCemission", 
                             ".xlsx", meta = F )
sector_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_sector_map", meta = F )
iso_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_country_map", meta = F )
fuel_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_fuel_map", meta = F )

# ------------------------------------------------------------------------------
# 2. Bond EFs

if( em == 'BC') em_col <- 'BC_kt'
if( em == 'OC') em_col <-  'OC_kt'

X_bond_years <- paste0('X',seq(1850,2000,5))
Xyears_full <- paste0( "X", 1850:2014 )


bond <- bcoc_historical
names(bond) <- c("Region","Country", "Fuel" ,"Tech","Sector" ,"Year" ,
                            "Fuel_kt" ,"BC_kt", "OC_kt" )

# convert natural gas from TJ to kt
bond[ which( bond$Fuel == " Natural Gas    "), 'Fuel_kt'] <- bond[ 
  which( bond$Fuel == " Natural Gas    "), 'Fuel_kt']/conversionFactor_naturalgas_TJ_per_kt

# map to ceds fuel
# map to ceds sector
bond$fuel <- fuel_map[ match(bond$Fuel,fuel_map$Fuel),'fuel']
bond <- merge( bond, sector_map, all=TRUE)
bond <- bond[which(bond$fuel != 'NA'),]

# remove weird data and data we don't use
bond <- bond[which( bond$Fuel_kt > 0 &
                      bond$BC_kt > 0 &
                      bond$OC_kt > 0     )  , ]

bond_everything <- bond
bond <- bond[which(bond$fuel %!in% c( 'process','natural_gas')),]
# Remove estimates for biomass after 2000 (bond data doesn't update emission factors after 2000)
bond <- bond[- which(bond$fuel %in% 'biomass' & bond$Year > 2000 ),]
bond$Year <- paste0('X', bond$Year)

# remove small values rows (they make bad efs because the values are so small)
bond <- bond[which(bond$Fuel_kt >7), ]

# add iso
bond <- merge( bond , iso_map[,c('iso','Country')], all= TRUE)
bond <- bond[complete.cases(bond),]

# add agg_sector
bond$agg_sector <- MSLevel[ match(bond$sector, MSLevel$working_sectors_v1), "aggregate_sectors" ]



# ------------------------------------------------------------------------------
# 2. Reformat and create average EFs

# aggregate by country
bond_country <- aggregate( bond[ , c( "Fuel_kt" ,"BC_kt", "OC_kt" )],
                           by = list(  iso = bond$iso,
                                       fuel = bond$fuel ,
                                       sector =  bond$sector,
                                       Year = bond$Year) ,
                           FUN = sum)
bond_country$EF <- bond_country[ , em_col ] / bond_country[ , 'Fuel_kt' ]
bond_EF_country <- cast( bond_country , iso + fuel + sector ~ Year , value = 'EF')
bond_EF_country [ X_emissions_years[X_emissions_years %!in% X_bond_years] ] <- NA
Xyears <- names( bond_EF_country )[ grep("X", names( bond_EF_country ) ) ] %>% sort()
bond_EF_country <- bond_EF_country[, c( "iso", "fuel" , "sector", Xyears)]
bond_EF_country <- interpolate_extend(bond_EF_country)
bond_EF_country <- bond_EF_country[complete.cases(bond_EF_country[Xyears]) , ]


# aggregate by Region
bond_region <- aggregate( bond[ , c( "Fuel_kt" ,"BC_kt", "OC_kt" )],
                          by = list(  Region = bond$Region ,
                                      fuel = bond$fuel ,
                                      sector =  bond$sector,
                                      Year = bond$Year) ,
                          FUN = sum)
bond_region$EF <- bond_region[ , em_col ] / bond_region[ , 'Fuel_kt' ]
bond_EF_region <- cast( bond_region , Region + fuel + sector ~ Year , value = 'EF')
bond_EF_region [ X_emissions_years[X_emissions_years %!in% X_bond_years] ] <- NA
bond_EF_region <- bond_EF_region[, c( "Region", "fuel" , "sector", Xyears)]
bond_EF_region <- interpolate_extend(bond_EF_region)
bond_EF_region <- bond_EF_region[complete.cases(bond_EF_region[Xyears]) , ]

# aggregate by Region, aggregate sector
bond_agg_sector <- aggregate( bond[ , c( "Fuel_kt" ,"BC_kt", "OC_kt" )],
                          by = list(  Region = bond$Region ,
                                      fuel = bond$fuel ,
                                      agg_sector = bond$agg_sector,
                                      Year = bond$Year) ,
                          FUN = sum)
bond_agg_sector$EF <- bond_agg_sector[ , em_col ] / bond_agg_sector[ , 'Fuel_kt' ]
bond_EF_agg_sector <- cast( bond_agg_sector , Region + fuel + agg_sector ~ Year , value = 'EF')
bond_EF_agg_sector [ X_emissions_years[X_emissions_years %!in% X_bond_years] ] <- NA
bond_EF_agg_sector <- bond_EF_agg_sector[, c( "Region", "fuel" , "agg_sector", Xyears)]
bond_EF_agg_sector <- interpolate_extend(bond_EF_agg_sector)

# aggregate by Region, fuel
bond_region_fuel <- aggregate( bond[ , c( "Fuel_kt" ,"BC_kt", "OC_kt" )],
                              by = list(  Region = bond$Region ,
                                          fuel = bond$fuel ,
                                          Year = bond$Year) ,
                              FUN = sum)
bond_region_fuel$EF <- bond_region_fuel[ , em_col ] / bond_region_fuel[ , 'Fuel_kt' ]
bond_EF_region_fuel <- cast( bond_region_fuel , Region + fuel  ~ Year , value = 'EF')
bond_EF_region_fuel [ X_emissions_years[X_emissions_years %!in% X_bond_years] ] <- NA
bond_EF_region_fuel <- bond_EF_region_fuel[, c( "Region", "fuel", Xyears)]
bond_EF_region_fuel <- interpolate_extend(bond_EF_region_fuel)

# aggregate by fuel , aggregate sector
bond_fuel_agg_sector <- aggregate( bond[ , c( "Fuel_kt" ,"BC_kt", "OC_kt" )],
                               by = list(  fuel = bond$fuel ,
                                           agg_sector = bond$agg_sector ,
                                           Year = bond$Year) ,
                               FUN = sum)
bond_fuel_agg_sector$EF <- bond_fuel_agg_sector[ , em_col ] / bond_fuel_agg_sector[ , 'Fuel_kt' ]
bond_EF_fuel_agg_sector <- cast( bond_fuel_agg_sector , agg_sector + fuel  ~ Year , value = 'EF')
bond_EF_fuel_agg_sector [ X_emissions_years[X_emissions_years %!in% X_bond_years] ] <- NA
bond_EF_fuel_agg_sector <- bond_EF_fuel_agg_sector[, c( "agg_sector", "fuel", Xyears)]
bond_EF_fuel_agg_sector <- interpolate_extend(bond_EF_fuel_agg_sector)

# aggregate by fuel , aggregate sector
bond_fuel <- aggregate( bond[ , c( "Fuel_kt" ,"BC_kt", "OC_kt" )],
                                   by = list(  fuel = bond$fuel ,
                                               Year = bond$Year) ,
                                   FUN = sum)
bond_fuel$EF <- bond_fuel[ , em_col ] / bond_fuel[ , 'Fuel_kt' ]
bond_EF_fuel <- cast( bond_fuel , fuel  ~ Year , value = 'EF')
bond_EF_fuel [ X_emissions_years[X_emissions_years %!in% X_bond_years] ] <- NA
bond_EF_fuel <- bond_EF_fuel[, c( "fuel", Xyears)]
bond_EF_fuel <- interpolate_extend(bond_EF_fuel)

# residential biomass
EF_residential_biomass <- bond_EF_country[which(bond_EF_country$fuel %in% c("biomass") &
                                                bond_EF_country$sector == '1A4b_Residential'),]


# ------------------------------------------------------------------------------
# 2. Map to CEDS sectors and countries

# make ef template from activity data
ef_template <- activity_data[ , c('iso','sector','fuel')]
ef_template$units <- 'kt/kt'
ef_template[ Xyears ] <- NA

# add aggregate flags
ef_template$Region <- iso_map[ match( ef_template$iso, iso_map$iso)  ,'Region']
ef_template$agg_sector <- MSLevel[ match(ef_template$sector, MSLevel$working_sectors_v1), "aggregate_sectors" ]

# make natural gas ef = 0
ef_template[which( ef_template$fuel == 'natural_gas'), Xyears] <- 0

nrow_all <- nrow(ef_template)

#  add EFs for residential biomass by iso
EF <- replaceValueColMatch(ef_template , EF_residential_biomass ,
                           x.ColName = Xyears ,
                           match.x = c('iso','sector','fuel'), 
                           addEntries = FALSE)

EF_nas <- EF[is.na(EF$X1960),]
EF_final <- EF[!is.na(EF$X1960),]

# add EFs by region, sector, fuel
EF <- replaceValueColMatch(EF_nas , bond_EF_region ,
                              x.ColName = Xyears ,
                              match.x = c('Region','sector','fuel'), 
                              addEntries = FALSE)

EF_nas <- EF[is.na(EF$X1960),]
EF_final <- rbind( EF_final , EF[!is.na(EF$X1960),] )

# add EFs by region, aggregate Sector,fuel
EF <- replaceValueColMatch(EF_nas , bond_EF_agg_sector ,
                           x.ColName = Xyears ,
                           match.x = c('Region','agg_sector','fuel'), 
                           addEntries = FALSE)

EF_nas <- EF[is.na(EF$X1960),]
EF_final <- rbind( EF_final , EF[!is.na(EF$X1960),] )


# add EFs by region fuel 
EF <- replaceValueColMatch(EF_nas , bond_EF_region_fuel ,
                           x.ColName = Xyears ,
                           match.x = c('Region','fuel'), 
                           addEntries = FALSE)

EF_nas <- EF[is.na(EF$X1960),]
EF_final <- rbind( EF_final , EF[!is.na(EF$X1960),] )

# add EFs by fuel , aggregate sector
EF <- replaceValueColMatch(EF_nas , bond_EF_fuel_agg_sector ,
                           x.ColName = Xyears ,
                           match.x = c('agg_sector','fuel'), 
                           addEntries = FALSE)

EF_nas <- EF[is.na(EF$X1960),]
EF_final <- rbind( EF_final , EF[!is.na(EF$X1960),] )

# add EFs by global fuel average 
EF <-EF_nas
EF[Xyears] <- bond_EF_fuel[ match(EF$fuel,bond_EF_fuel$fuel), Xyears]

EF_nas <- EF[is.na(EF$X1960),]
EF_final <- rbind( EF_final , EF[!is.na(EF$X1960),] )

# ------------------------------------------------------------------------------
# 7. Final Processing
if ( nrow(EF_nas)>0 |
     nrow(EF_final) != nrow_all ) stop('NAs in BC-OC efs. Please check code.')
EF_final <- EF_final[ with( EF_final, order( iso, sector, fuel ) ), ]

final_full <- EF_final
final_full$units <- 'kt/kt'

# Interpolate/extend
final_full[, Xyears_full[ Xyears_full %!in% names( final_full ) ] ] <- NA
final_full <- final_full[,c('iso','sector','fuel', 'units' , Xyears_full)]
final_full[ final_full == 0 ] <- NA
final_full <- interpolate_extend( final_full )
final_full[ is.na( final_full ) ] <- 0

# Subset X_emissions_years
final_out <- final_full[,c('iso','sector','fuel', 'units' , X_emissions_years ) ]

# ------------------------------------------------------------------------------
# 8. Process emissions
bond_everything$Year <- paste0('X',bond_everything$Year)
bond_everything <- merge( bond_everything , iso_map[,c('iso','Country')], all= TRUE)

bond_everything <- aggregate( bond_everything[paste0(em,'_kt')],
                                      by = list( iso = bond_everything$iso,
                                                 sector = bond_everything$sector,
                                                 fuel = bond_everything$fuel,
                                                 Year = bond_everything$Year),
                                      FUN = sum)
bond_everything <- bond_everything[which(!is.na(bond_everything[,em_col] )),]

# Cast and select process sectors
bond_process <- cast( bond_everything , iso + sector + fuel ~ Year , value = paste0(em,'_kt'))

process_sectors <- MSL[which(MSL$type == 'NC'),'sector']
bond_process <- bond_process[which(bond_process$sector %in% process_sectors), ]
bond_process [ X_emissions_years[X_emissions_years %!in% X_bond_years] ] <- NA

 
# organize and extend
bond_process <- bond_process[ , c( 'iso','sector','fuel',Xyears)]

bond_process <- bond_process[ rowSums(is.na(bond_process[Xyears]))!=
                                                      length(Xyears), ]
bond_process <- replace(bond_process, bond_process == 0, NA)
bond_process_extend <- interpolateValues(bond_process)
bond_process_extend[ is.na( bond_process_extend ) ] <- 0

# relabel fuel and process and add units
bond_process_extend$fuel <- 'process'
bond_process_extend$units <- 'kt'
bond_process_extend <- bond_process_extend[ , c( 'iso','sector','fuel','units',Xyears_full)]


# ------------------------------------------------------------------------------
# 7. Write output

writeData( final_out , "MED_OUT", paste0( "B.",em,"_comb_EF_db" ) )
writeData( final_full, "DEFAULT_EF_IN", paste0( "B.",em,"_SPEW_comb_EF" ), domain_extension = "EF_parameters/" )
writeData( bond_process_extend , "DEFAULT_EF_IN", domain_extension = 'non-combustion-emissions/', 
           fn = paste0( "B.",em,"_SPEW_NC_em" ) , meta = F)

# Every script should finish with this line
logStop()

# END





