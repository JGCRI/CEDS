#------------------------------------------------------------------------------
# Program Name: F1.1.Edgar_scaling.R
# Authors' Names: Tyler Pitkanen, Jon Seibert, Rachel Hoesly
# Date Last Modified: Dec 27, 2015
# Program Purpose: To create scaling factors and update emissions estimate for
# the CAN region from latest emissions working copy by using aggregate 
# CAN trends inventory data.
# Input Files: emissions_scaling_functions.R, F.[em]_scaled_EF.csv, 
#              F.[em]_scaled_emissions.csv, CAN_sector_mapping.csv, 
#              national_tier1_caps.xlsx
# Output Files: F.[em]_total_scaled_EF.csv, F.[em]_total_scaled_emissions.csv
# Notes: 
# TODO:
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Set working directory to the CEDS “input” directory and define PARAM_DIR as the
# location of the CEDS “parameters” directory relative to the new working directory.
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
headers <- c( 'common_data.R',"data_functions.R" ,"emissions_scaling_functions.R" ) # Additional function files required.
log_msg <- "Edgar inventory scaling" # First message to be printed to the log
script_name <- "F1.1.Edgar_scaling.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define parameters for inventory specific script

  args_from_makefile <- commandArgs( TRUE )
  em <- args_from_makefile[1]
  if ( is.na( em ) ) em <- "SO2"
  
  # Stop script if running for unsupported species
  if ( em %!in% c('SO2','NOx','NMVOC','CO', 'CH4') ) {
    stop (paste( ' Edgar scaling is not supported for emission species', em, 'remove from script
                 list in F1.1.inventory_scaling.R'))
  }
  
  
# For each Module E script, define the following parameters:
# Inventory parameters. Provide the inventory and mapping file names, the
#   mapping method (by sector, fuel, or both), and the regions covered by
#   the inventory (as a vector of iso codes)

  inventory_data_file <- paste0('EDGAR42_',em)
  inv_data_folder <- "EM_INV"
  sector_fuel_mapping <- 'Edgar_scaling_mapping'
  mapping_method <- 'sector'
  inv_name <- 'EDGAR' #for naming diagnostic files
  region <-  c("can", "spm", "usa", "mex", "abw", "aia", "ant", "atg", "bhs", "blz", "bmu", "brb", "cri", "cub", "cym",
               "dma", "dom", "glp", "grd", "gtm", "hnd", "hti", "jam", "kna", "lca", "msr", "mtq", "nic", "pan", "pri",
               "slv", "tca", "tto", "vct", "vgb", "bra", "arg", "bol", "chl", "col", "ecu", "flk", "guf", "guy", "per",
               "pry", "sur", "ury", "ven", "dza", "egy","esh", "lby", "mar", "tun", "ben", "bfa", "caf", "civ", "cmr",
               "cod", "cog", "cpv", "gab", "gha", "gin", "gmb", "gnb", "gnq", "lbr", "mli", "mrt", "ner", "nga", "sen",
               "shn", "sle", "stp", "tcd", "tgo", "bdi", "com", "dji", "eri", "eth", "ken", "mdg", "mus", "reu", "rwa",
               "sdn", "som", "syc", "uga", "ago", "bwa", "lso", "moz", "mwi", "nam", "swz", "tza", "zaf", "zmb", "zwe",
               "aut", "bel", "che", "deu", "dnk", "esp", "fin", "fra", "fro", "gbr", "gib", "grc", "grl", "irl", "isl",
               "ita", "lux", "nld", "nor", "prt", "swe", "alb", "bgr", "bih", "cyp", "cze", "est", "hrv", "hun", "ltu",
               "lva", "mkd", "mlt", "pol", "rou", "scg", "svk", "svn", "tur", "blr", "mda", "ukr", "kaz", "kgz", "tjk",
               "tkm", "uzb", "arm", "aze", "geo", "rus", "are", "bhr", "irn", "irq", "isr", "jor", "kwt", "lbn", "omn",
               "qat", "sau", "syr", "yem", "afg", "bgd", "btn", "ind", "lka", "mdv", "npl", "pak", "kor", "prk", "chn",
               "hkg", "mac", "mng", "twn", "brn", "khm", "lao", "mmr", "mys", "phl", "sgp", "tha", "tls", "vnm", "idn",
               "png", "jpn", "aus", "cok", "fji", "kir", "ncl", "nzl", "plw", "pyf", "slb", "ton", "vut", "wsm", "sea",
               "air")
    
  inv_years<-c(1970:2008)

# ------------------------------------------------------------------------------
# 1.5 Inventory in Standard Form (iso-sector-fuel-years, iso-sector-years, etc)
  
  # Import file
  inv_data_sheet <- readData( inv_data_folder, domain_extension = "Edgar/",
                              inventory_data_file ) 
  
  # Clean rows and columns to standard format
  inv_data_sheet$units <- 'kt'
  inv_data_sheet <- inv_data_sheet[,c('ISO_A3','IPCC','units', paste0('X',inv_years))]
  names(inv_data_sheet) <- c('iso','sector', 'units', paste0('X',inv_years))
  inv_data_sheet$iso <- tolower(inv_data_sheet$iso)
  
  # write standard form inventory
  writeData( inv_data_sheet , domain = "MED_OUT", paste0('E.',em,'_',inv_name,'_inventory'))
  inventory_data_file <- paste0('E.',em,'_',inv_name,'_inventory')
  inv_data_folder <- 'MED_OUT'
  
  # ------------------------------------------------------------------------------
  # 2. Read In Data with scaling functions
  
  # Read in the inventory data, mapping file, the specified emissions species, and
  # the latest versions of the scaled EFs  
  
  scaling_data <- F.readScalingData( inventory = inventory_data_file, inv_data_folder,
                                     mapping = sector_fuel_mapping, method = mapping_method,
                                     region, inv_name, inv_years)
  list2env(scaling_data , envir = .GlobalEnv) 
  
  
  # ------------------------------------------------------------------------------
  # 3. Arrange the CEDS emissions data to match the inventory data
  
  # Aggregate inventory data to scaling sectors/fuels 
  inv_data <- F.invAggregate( std_form_inv , region )
  
  # Aggregate ceds data to scaling sectors/fuels
  ceds_data <- F.cedsAggregate( input_em, region, mapping_method )
  
  # ------------------------------------------------------------------------------
  # 4. Calculate Scaling Factors, reaggregate to CEDS sectors  
  
  # Calculate and extend scaling factors
  scaling_factors_list <- F.scaling( ceds_data, inv_data, region, 
                                     replacement_method = 'replace', 
                                     max_scaling_factor = 100,
                                     replacement_scaling_factor = 100)
  list2env(scaling_factors_list , envir = .GlobalEnv)
  
  # Apply Scaling Factors to Ceds data
  scaled <- F.applyScale(scaling_factors)
  scaled_ef <- scaled[[1]]
  scaled_em <- scaled[[2]]
  
  # ------------------------------------------------------------------------------
  # 5. Encorporate scaled em and EF and
  # Write Scaled emissions and emission factors
  
  F.addScaledToDb( scaled_ef, scaled_em, meta_notes )

  
  # Every script should finish with this line
  logStop()
  # END
  