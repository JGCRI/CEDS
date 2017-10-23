# ------------------------------------------------------------------------------
# Program Name: H1.2.add_activity_Bond.R
# Author: Rachel Hoesly
# Program Purpose: Extend coal and biomass back with Bond data
#
# Output Files:
# TODO:
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R","process_db_functions.R") # Additional function files may be required.
log_msg <- "Extending coal and biomass activity_data before 1960 with CDIAC-Bond data" # First message to be printed to the log
script_name <- "H1.2.add_activity_Bond.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "NH3"

loadPackage('zoo')
# ---------------------------------------------------------------------------
# 1. Load files

activity_all <- readData( 'MED_OUT',paste0('H.',em,'_total_activity_extended_db') )

bond_historical <- readData( "EM_INV", domain_extension = "Bond-BCOC/" ,"Bond-BCOC160227_SPEW_BCOCemission", ".xlsx", meta = F )
sector_map <- readData( "MAPPINGS", "Bond_sector_map", meta = F )
iso_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_country_map", meta = F )
fuel_map <- readData( "MAPPINGS", domain_extension = "Bond/" , "Bond_fuel_map", meta = F )


# ---------------------------------------------------------------------------
# 2. Select data to extend based on extension drivers

activity <- activity_all

# ---------------------------------------------------------------------------
# 3. Data processing

# map to iso sector fuel
bond <- merge( bond_historical, sector_map)
bond <- merge( bond, fuel_map)
bond <- merge( bond, iso_map[,c('iso','Country')])
bond <- bond[which( bond$fuel %in% c("brown_coal" ,"hard_coal" , "biomass" , "coal_coke" ) ),
             c('iso','fuel',"sector","Year","Fuel (kt)")]
bond <- bond[which( bond$sector %!in% c('1A4a_Commercial-institutional','1A4b_Residential')),]

bond <- bond[which( bond[,'Fuel (kt)'] > 0),]
bond$Year <- paste0('X',bond$Year)
bond <- aggregate(bond["Fuel (kt)"],
                  by = list(iso = bond$iso,
                            sector = bond$sector,
                            fuel = bond$fuel,
                            Year = bond$Year),
                  FUN = sum)

bond <- cast(bond, iso+sector+fuel~Year, value = "Fuel (kt)")

bond[ paste0("X",1850:2010)[paste0("X",1850:2010) %!in% names(bond)] ] <- NA
bond <- bond[ , c('iso','sector','fuel',paste0("X",1850:2010))]
bond <- bond[which(apply( X= bond[paste0("X",1850:2010)], MARGIN= 1, FUN = function(x ) length(which(!is.na(x)))>1  )) , ]
bond[paste0("X",1850:2010)] <- t(na.approx(t(bond[paste0("X",1850:2010)])))

biomass_industry <- bond[which( bond$fuel == 'biomass'),]


# coking
bond_coke <- bond_historical[which(bond_historical$Tech == " *Coking        " ),]
bond_coke$sector <- '1A1bc_Other-transformation'
bond_coke <- merge( bond_coke, fuel_map)
bond_coke <- merge( bond_coke, iso_map[,c('iso','Country')])
# bond_coke <- bond_coke[which( bond_coke$Year < 1966),]
bond_coke <- bond_coke[which( bond_coke[,'Fuel (kt)'] > 0),]
bond_coke$Year <- paste0('X',bond_coke$Year)
bond_coke <- aggregate(bond_coke["Fuel (kt)"],
                       by = list(iso = bond_coke$iso,
                                 sector = bond_coke$sector,
                                 fuel = bond_coke$fuel,
                                 Year = bond_coke$Year),
                       FUN = sum)

bond_coke <- cast(bond_coke, iso+sector+fuel~Year, value = "Fuel (kt)")
bond_coke[ paste0("X",1850:2010)[paste0("X",1850:2010) %!in% names(bond_coke)] ] <- NA
bond_coke <- bond_coke[ , c('iso','sector','fuel',paste0("X",1850:2010))]
bond_coke[which(is.na(bond_coke$X1850)),'X1850'] <- 0
bond_coke <- bond_coke[which(apply( X= bond_coke[paste0("X",1850:2010)], MARGIN= 1, FUN = function(x ) length(which(!is.na(x)))>1  )) , ]
bond_coke[paste0("X",1850:2010)] <- t(na.approx(t(bond_coke[paste0("X",1850:2010)])))
bond_coke$fuel <- 'process'

# ---------------------------------------------------------------------------
# 3. Industry Biomass

extension_driver_data <- biomass_industry

ratio_year <- 1970
ext_start_year <- 1850
extension_years <- paste0('X',ext_start_year:1964)

# select extension data for current method
sectors <- extension_driver_data[, c('iso','sector','fuel') ]
sectors <- paste(sectors$iso,sectors$sector,sectors$fuel,sep='-')

# select ceds data to extend
ceds_extension_ratios <- activity[ which( paste(activity$iso, activity$sector, activity$fuel, sep="-") %in% sectors  ) , ]

#extended data template
ceds_extension_ratios <- ceds_extension_ratios[,c('iso','sector','fuel',paste0('X',ratio_year))]
names(ceds_extension_ratios)[which(names(ceds_extension_ratios) == paste0('X',ratio_year))] <- 'CEDS_ratio_year'

# add Driver identifyer ratio year
ceds_extension_ratios <- merge(ceds_extension_ratios, extension_driver_data[,c("iso", paste0('X',ratio_year))],
                               by.x = c('iso'),
                               by.y = c("iso"),
                               all.x = TRUE, all.y = FALSE)
names(ceds_extension_ratios)[which(names(ceds_extension_ratios) == paste0('X',ratio_year))] <- 'driver_ratio_year'


# calculate ratio
ceds_extension_ratios$ratio <- ceds_extension_ratios$CEDS_ratio_year/ceds_extension_ratios$driver_ratio_year
# make all infinite ratios zero
ceds_extension_ratios[!is.finite(ceds_extension_ratios$ratio) , 'ratio'] <- 0

# add driver data and use ratio to calculate extended value
ceds_extended <- ceds_extension_ratios[,c('iso','fuel','sector','ratio')]
ceds_extended[extension_years] <- extension_driver_data[ match( ceds_extended$iso , extension_driver_data$iso )
                                                         ,extension_years]
ceds_extended[is.na(ceds_extended)] <- 0

# calculate extended data
ceds_extended[ extension_years ] <- ceds_extended$ratio * ceds_extended[ extension_years ]

# add to final extension template
activity <- replaceValueColMatch(activity, ceds_extended,
                                 x.ColName = extension_years,
                                 match.x = c('iso','sector','fuel'),
                                 addEntries = FALSE)


# ---------------------------------------------------------------------------
# 7. Bond coke

extension_driver_data <- bond_coke

ratio_year <- 1970
ext_start_year <- 1850
extension_years <- paste0('X',ext_start_year:1964)

# select extension data for current method
sectors <- extension_driver_data[, c('sector','fuel') ]
sectors <- unique(paste(sectors$sector,sectors$fuel,sep='-'))

# select ceds data to extend
ceds_extension_ratios <- activity[ which( paste(activity$sector, activity$fuel, sep="-") %in% sectors  ) , ]

#extended data template
ceds_extension_ratios <- ceds_extension_ratios[,c('iso','sector','fuel',paste0('X',ratio_year))]
names(ceds_extension_ratios)[which(names(ceds_extension_ratios) == paste0('X',ratio_year))] <- 'CEDS_ratio_year'

# add Driver identifyer ratio year
ceds_extension_ratios <- merge(ceds_extension_ratios, extension_driver_data[,c("iso", paste0('X',ratio_year))],
                               by.x = c('iso'),
                               by.y = c("iso"),
                               all.x = TRUE, all.y = FALSE)
names(ceds_extension_ratios)[which(names(ceds_extension_ratios) == paste0('X',ratio_year))] <- 'driver_ratio_year'

ceds_extension_ratios <- ceds_extension_ratios[complete.cases(ceds_extension_ratios),]

# calculate ratio
ceds_extension_ratios$ratio <- ceds_extension_ratios$CEDS_ratio_year/ceds_extension_ratios$driver_ratio_year
# make all infinite ratios zero
ceds_extension_ratios[!is.finite(ceds_extension_ratios$ratio) , 'ratio'] <- 0

# add driver data and use ratio to calculate extended value
ceds_extended <- ceds_extension_ratios[,c('iso','fuel','sector','ratio')]
ceds_extended[extension_years] <- extension_driver_data[ match( ceds_extended$iso , extension_driver_data$iso )
                                                         ,extension_years]
ceds_extended[is.na(ceds_extended)] <- 0

# calculate extended data
ceds_extended[ extension_years ] <- ceds_extended$ratio * ceds_extended[ extension_years ]

# add to final extension template
activity <- replaceValueColMatch(activity, ceds_extended,
                                 x.ColName = extension_years,
                                 match.x = c('iso','sector','fuel'),
                                 addEntries = FALSE)





# ---------------------------------------------------------------------------
# 6. Write to database

if( !(nrow(activity_all) == nrow(activity) & ncol(activity_all) == ncol(activity) ) ){
  stop( "New and old activity do not match") } else{
    writeData( activity, "MED_OUT" , paste0('H.',em,'_total_activity_extended_db')) }

logStop()
