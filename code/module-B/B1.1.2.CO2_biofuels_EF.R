# ----------------------------------------------------------------------------
# Program Name: B1.1.2.CO2_biofuels_EF.R
# Author's Name:  
#               Rachel Hoesly
# Date Last Modified: 14 December 2016
# Program Purpose: 
# Input Files: 
# Output Files: 
# Notes: 
# To Do: 

# ------------------------------------------------------------------------------

# Before we can load headers we need some paths defined. They may be provided 
#   by a system environment variable or they may have been set in the workspace
# Set variable PARAM_DIR to be the data system directory
dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
for ( i in 1:length( dirs ) ) {
  setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
  wd <- grep( 'CEDS/input', list.dirs(), value = T )
  if ( length(wd) > 0 ) {
    setwd( wd[1] )
    break
  }
}
PARAM_DIR <- "../code/parameters/"


# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R" ) # Additional function files required.
log_msg <- paste0( "Historical energy balances from IEA, aggregated to CEDS",
                   " sectors, and fuels" ) # First message to be printed to the log
script_name <- "B1.1.2.CO2_biofuels_EF.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "CO2"
em_lc <- tolower( em )   

# Stop script if running for unsupported species
if ( em %!in% c('CO2') ) {
  stop (paste( 'not supported for emission species', em, 'remove from script
               list in B1.2.add_comb_EF.R and/or makefile'))
}

# ------------------------------------------------------------------------------
# 1. Read in files

IEA_energy <- readData('MED_OUT','A.IEA_en_stat_ctry_hist')
CO2_ef <- readData('MED_OUT', paste0( "B.", em, "_comb_EF_db" ) )

IEA_product_fuel <- readData( "EN_MAPPINGS", "IEA_product_fuel" )


# ------------------------------------------------------------------------------
# 2. Calculate biofuels fraction

# add fuels, biofuels notation to IEA energy
IEA_energy$fuel <-  IEA_product_fuel[match(IEA_energy$PRODUCT,IEA_product_fuel$product ),'fuel']
IEA_energy$bio_flag <-  IEA_product_fuel[match(IEA_energy$PRODUCT,IEA_product_fuel$product ),'biofuel_flag']

# aggregate sums by iso, fuel
IEA_energy_bio <- IEA_energy[which(IEA_energy$bio_flag == 1),]

IEA_aggregate_biofuels <- aggregate(IEA_energy_bio[X_IEA_years],
                                    by = list(iso = IEA_energy_bio$iso,
                                              fuel = IEA_energy_bio$fuel ), sum )

IEA_aggregate <- aggregate(IEA_energy[X_IEA_years],
                                    by = list(iso = IEA_energy$iso,
                                              fuel = IEA_energy$fuel ), sum )

# Calculate fraction of biofuels
IEA_biofuels_fraction <- IEA_aggregate[,c('iso','fuel')]
IEA_biofuels_fraction[X_IEA_years] <- IEA_aggregate_biofuels[ match(paste(IEA_biofuels_fraction$iso, IEA_biofuels_fraction$fuel),
                                                                   paste(IEA_aggregate_biofuels$iso, IEA_aggregate_biofuels$fuel)) , 
                                                             X_IEA_years] /IEA_aggregate[X_IEA_years]

IEA_biofuels_fraction <- IEA_biofuels_fraction[which(!is.na(IEA_biofuels_fraction$X2013)),]
IEA_biofuels_fraction <- replace(IEA_biofuels_fraction, is.na(IEA_biofuels_fraction), 0 ) 
IEA_biofuels_fraction <- IEA_biofuels_fraction[-which(IEA_biofuels_fraction$fuel == 'biomass'),]

X_extension_years <- X_emissions_years[X_emissions_years %!in% X_IEA_years]
IEA_biofuels_fraction[X_extension_years] <- IEA_biofuels_fraction[X_IEA_end_year]

# ------------------------------------------------------------------------------
# 3. Remove biofuels fraction

final_ef <- CO2_ef[, c("iso","sector","fuel","units")]

multiplier <- CO2_ef[, c("iso","sector","fuel","units")] 
multiplier[ X_emissions_years ] <- IEA_biofuels_fraction[match(paste(final_ef$iso, final_ef$fuel),
                                          paste(IEA_biofuels_fraction$iso, IEA_biofuels_fraction$fuel)),
                                         X_emissions_years]
multiplier <- replace(multiplier, is.na(multiplier),0)

final_ef[ X_emissions_years ] <- CO2_ef[ X_emissions_years ]*(1-multiplier[ X_emissions_years ])


# -----------------------------------------------------------------------------
# 5. Output
# Add comments for each table

comment.final_ef <- 'Base CO2 combustion EFs taking into account the fraction of liquid and gas fuels that are from biofuels'

writeData( final_ef, "MED_OUT", paste0( "B.", em, "_comb_EF_db" ),
           comments = comment.final_ef)

# Every script should finish with this line:
logStop()



