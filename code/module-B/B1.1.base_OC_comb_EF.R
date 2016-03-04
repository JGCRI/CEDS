#------------------------------------------------------------------------------
# Program Name: B1.1.base_OC_comb_EF.R
# Author: Rachel Hoesly
# Date Last Updated: Feb 11, 2016
# Program Purpose: 1. Produce OC emissions factors from Bond et al data.
#              
# Input Files: 
#              Bond_ctry_mapping.csv, Bond_fuel_mapping.csv, Bond_sector_mapping.csv,
#              A.comb_activity.csv 
# Output Files: B.comb_EF_db.csv
# Notes: 1. Emission factors (ef) are calculated as emissions divided by consumption.
#           Missing (zero or NA) ef are replaced using the following rules, in order:
#           a. replace with ef of the same sector_fuel and region for the same year,
#              if available;
#           b. then replace with region fuel average
#           c. then global sector fuel average
#           d. then global fuel average
#        2. Variables with modifiable values: threshold_var.
# TODO: correct biomass
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
script_name <- "B1.1.base_BCOC_comb_EF.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "OC"

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
sector_map <- readData( "EM_INV", domain_extension = "Bond-BCOC/" , "Bond_sector_map_oc", meta = F )
iso_map <- readData( "EM_INV", domain_extension = "Bond-BCOC/" , "Bond_country_map_oc", meta = F )
fuel_map <- readData( "EM_INV", domain_extension = "Bond-BCOC/" , "Bond_fuel_map_oc", meta = F )

# ------------------------------------------------------------------------------
# 2. Bond EFs

X_bond_years <- paste0('X',seq(1960,2000,5))

bond <- bcoc_historical
names(bond) <- c("Region","Country", "Fuel" ,"Tech","Sector" ,"Year" ,
                            "Fuel_kt" ,"BC_kt", "OC_kt" )

# convert natural gas from TJ to kt
bond[ which( bond$Fuel == " Natural Gas    "), 'Fuel_kt'] <- bond[ 
  which( bond$Fuel == " Natural Gas    "), 'Fuel_kt']/conversionFactor_naturalgas_TJ_per_kt

# map to ceds_fuel
bond$ceds_fuel <- fuel_map[ match(bond$Fuel,fuel_map$Fuel),'fuel']

# remove wierd data and data we don't use
bond <- bond[which( bond$Fuel_kt > 0 &
                      bond$BC_kt > 0 &
                      bond$OC_kt > 0     )  , ]
bond <- bond[which(bond$Year >1959 & bond$Year < 2001),]
bond_everything <- bond
bond <- bond[which(bond$ceds_fuel %!in% c( 'process','natural_gas')),]


# aggregate by Region

bond_region <- aggregate( bond[ , c( "Fuel_kt" ,"BC_kt", "OC_kt" )],
                          by = list( Region = bond$Region ,
                                      Fuel = bond$Fuel ,
                                      Tech = bond$Tech ,
                                      Sector =  bond$Sector,
                                      Year = bond$Year) ,
                          FUN = sum)
  
# remove small values rows (they make bad efs because the values are so small)
bond_region <- bond_region[which(bond_region$Fuel_kt >7), ]

# calculate EFs
if( em == 'BC') em_col <- 'BC_kt'
if( em == 'OC') em_col <-  'OC_kt'
bond_region$EF <- bond_region[ , em_col ] / bond_region[ , 'Fuel_kt' ]
bond_region$Year <- paste0('X', bond_region$Year)

# ------------------------------------------------------------------------------
# 2. Reformat and create average EFs

# cast to wide and interpolate/extend
bond_EFs <- cast( bond_region , Region + Fuel + Tech + Sector ~ Year , value = 'EF')
bond_EFs [ X_emissions_years[X_emissions_years %!in% X_bond_years] ] <- NA

bond_EFs <- bond_EFs[, c( "Region", "Fuel" , "Tech" , "Sector", X_emissions_years)]
bond_EFs <- interpolate_extend(bond_EFs)
bond_EFs <- bond_EFs[complete.cases(bond_EFs[X_emissions_years]) , ]

# map ceds fuels
bond_EFs$fuel <- fuel_map[ match(bond_EFs$Fuel, fuel_map$Fuel), 'fuel']
bond_EFs <- bond_EFs[which(!is.na(bond_EFs$fuel)),]
bond_EFs <- bond_EFs[which((bond_EFs$fuel != 'NA')),]

EF_Region_fuel_Sector_average <- aggregate( bond_EFs[X_emissions_years],
                                     by = list( Region = bond_EFs$Region,
                                                fuel = bond_EFs$fuel,
                                                Sector = bond_EFs$Sector),
                                     FUN = mean)

EF_fuel_Sector_average <- aggregate( bond_EFs[X_emissions_years],
                                            by = list( fuel = bond_EFs$fuel,
                                                       Sector = bond_EFs$Sector),
                                            FUN = mean)

EF_Region_fuel_average <- aggregate( bond_EFs[X_emissions_years],
                                            by = list( Region = bond_EFs$Region,
                                                       fuel = bond_EFs$fuel),
                                            FUN = mean)
EF_fuel_average <- aggregate( bond_EFs[X_emissions_years],
                                     by = list( fuel = bond_EFs$fuel),
                                     FUN = mean)
EF_fuel_average <- aggregate( bond_EFs[X_emissions_years],
                              by = bond_EFs[ "fuel" ],
                              FUN = mean, na.rm= TRUE)

# ------------------------------------------------------------------------------
# 2. Map to CEDS sectors and countries

# map ceds sectors
bond_EFs_iso_sector <- merge( bond_EFs, sector_map, all=TRUE)
bond_EFs_iso_sector <- bond_EFs_iso_sector[complete.cases(bond_EFs_iso_sector$sector),]

# map to iso
bond_EFs_iso_sector <- merge( bond_EFs_iso_sector, iso_map[,c('iso','Country')], all=TRUE)
bond_EFs_iso_sector <- bond_EFs_iso_sector[complete.cases(bond_EFs_iso_sector$iso),]

# make ef template from activity data
ef_template <- activity_data[ , c('iso','sector','fuel')]
ef_template$units <- 'kt/kt'
ef_template[ X_emissions_years ] <- NA

# add aggregate flags
ef_template$Region <- iso_map[ match( ef_template$iso, iso_map$iso)  ,'Region']
ef_template$Sector <- sector_map[ match( ef_template$sector, sector_map$sector)  ,'Sector']

# make natural gas ef = 0
ef_template[which( ef_template$fuel == 'natural_gas'), X_emissions_years] <- 0

# add EFs by iso,sector,fuel
EF <- replaceValueColMatch(ef_template , bond_EFs_iso_sector ,
                              x.ColName = X_emissions_years ,
                              match.x = c('iso','sector','fuel'), 
                              addEntries = FALSE)

EF_nas <- EF[is.na(EF$X1960),]
EF_final <- EF[!is.na(EF$X1960),]


# add EFs by region fuel 
EF <- replaceValueColMatch(EF_nas , EF_Region_fuel_average ,
                           x.ColName = X_emissions_years ,
                           match.x = c('Region','fuel'), 
                           addEntries = FALSE)

EF_nas <- EF[is.na(EF$X1960),]
EF_final <- rbind( EF_final , EF[!is.na(EF$X1960),] )


# add EFs by region fuel 
EF <- replaceValueColMatch(EF_nas , EF_fuel_Sector_average ,
                           x.ColName = X_emissions_years ,
                           match.x = c('Sector','fuel'), 
                           addEntries = FALSE)

EF_nas <- EF[is.na(EF$X1960),]
EF_final <- rbind( EF_final , EF[!is.na(EF$X1960),] )

# add EFs by region 
EF <-EF_nas
EF[X_emissions_years] <- EF_fuel_average[ match(EF$fuel,EF_fuel_average$fuel), X_emissions_years]

EF_nas <- EF[is.na(EF$X1960),]
EF_final <- rbind( EF_final , EF[!is.na(EF$X1960),] )

# ------------------------------------------------------------------------------
# 7. Final Processing

EF_final <- EF_final[ with( EF_final, order( iso, sector, fuel ) ), ]

final <- EF_final
final$units <- 'kt/kt'

final <- final[,c('iso','sector','fuel', 'units' , X_emissions_years)]

bond_process <- cast( bond_region , Region + Fuel + Tech + Sector ~ Year , value = paste0(em,'_kt'))
bond_process [ X_emissions_years[X_emissions_years %!in% X_bond_years] ] <- NA
# ------------------------------------------------------------------------------
# 8. Process emissions

#map to iso
bond_process <- merge( bond_everything, iso_map[,c('iso','Country')], all.x=TRUE, all.y = FALSE)
bond_process <- bond_process[complete.cases(bond_process$iso),]

# map to fuel
bond_process$fuel <- fuel_map[ match(bond_process$Fuel, fuel_map$Fuel), 'fuel']

# map to sector
bond_process <- merge( bond_process, sector_map, all.x=TRUE, all.y = FALSE)
process_sectors <- MSL[which(MSL$type == 'NC'),'sector']
bond_process <- bond_process[which(bond_process$sector %in% process_sectors), ]

# organize and extend
bond_process <- replace(bond_process, is.na(bond_process), 0)
bond_process <- aggregate( bond_process[X_emissions_years],
                           by = list( iso = bond_process$iso,
                                      sector = bond_process$sector,
                                      fuel = bond_process$fuel),
                           FUN = sum)
bond_process <- replace(bond_process, bond_process == 0, NA)
bond_process <- interpolateValues(bond_process)
bond_process <- extendValues(bond_process)
bond_process$units <- 'kt'
bond_process <- bond_process[ , c( 'iso','sector','fuel','units',X_emissions_years)]


# ------------------------------------------------------------------------------
# 7. Write output

writeData( final , "MED_OUT", paste0( "B.",em,"_comb_EF_db" ) )
writeData( bond_process , "DEFAULT_EF_IN", domain_extension = 'non-combustion-emissions/', 
           fn = paste0( "B.",em,"_bond_NC_em" ) , meta = F)

# Every script should finish with this line
logStop()

# END





