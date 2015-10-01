#------------------------------------------------------------------------------
# Program Name: F1.1.UNFCC_scaling.R
# Authors' Names: Tyler Pitkanen, Jon Seibert
# Date Last Modified: July 8, 2015
# Program Purpose: To create scaling factors and update emissions estimate for
# the US region from latest emissions working copy by using aggregate 
# US emission trends inventory data.
# Input Files: emissions_scaling_functions.R, F.[em]_scaled_EF.csv, 
#              F.[em]_scaled_emissions.csv, UNFCCC_sector_mapping.csv, 
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
    log_msg <- "Modifying emissions factors from UNFCCC inventory data" # First message to be printed to the log
    script_name <- "F1.1.UNFCCC_scaling.R"
    
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )
    
# ------------------------------------------------------------------------------
# 1. Define parameters read in files
    
# For each Module E script, define the following parameters:
    # Inventory parameters. Provide the inventory and mapping file names, the
    #   mapping method (by sector, fuel, or both), and the regions covered by
    #   the inventory (as a vector of iso codes)
        inventory_data_file <- 'E.SO2_UNFCCC_inventory'
        sector_fuel_mapping <- 'UNFCCC_sector_mapping'
        mapping_method <- 'sector'
        
        
    # Interpolation/Extrapolation settings. Interpolation can be "linear" or
    #   "none". Extrapolation can be "constant", "linear", or "none", and
    #   can be defined differently before and after the inventory time range.
        interpolation <- "linear"
        extrapolation_before <- "constant"
        extrapolation_after  <- "constant"
    
# Read in the inventory data, mapping file, the specified emissions species, and
#   the latest versions of the scaled EFs and 
#   rework F.readScalingData to use with UNFCCC data
    
    inventory = inventory_data_file 
    mapping = sector_fuel_mapping 
    method = mapping_method
    
    # For UNFCCC only SO2 for now
    em_species<- "SO2"
    
    # Determine scaling method and set params
    if( method == 'sector' ) {
      scaling_name  <<- "scaling_sector"
      inv_matchcol_name  <<- 'inv_sector'
      ceds_matchcol_name <<- 'ceds_sector'
    } else if( method == 'fuel'   ) {
      scaling_name  <<- "scaling_fuel"
      inv_matchcol_name <<- 'inv_fuel'
      ceds_matchcol_name <<- 'ceds_fuel'
    } else if( method == 'both'   )  {
      scaling_name  <<- "scaling_activity"
      inv_matchcol_name <<- 'inv_activity'
      ceds_matchcol_name <<- 'ceds_activity'
    }

    
    # Read in data    
    sourceData( "PARAM", "common_data", ".R" )
    inv_data_full <<- readData( "MED_OUT", inventory)
    mapping_data <<- readData( "MAPPINGS", mapping )  
    
    ef_file <- paste0( "F.", em_species, "_scaled_EF" )
    em_file <- paste0( "F.", em_species, "_scaled_emissions" )
    input_ef_read <<- readData( "MED_OUT", ef_file )
    input_em_read <<- readData( "MED_OUT", em_file )
        

#define variables  specifically for UNFCCC to help with scaling for multiple countries    
UNFCCCregions <- c( "aus" , "aut" , "bel" , "bgr" , "blr" , "che" , "cyp" , "cze" , "deu" , "dnk" , "esp",
             "est" , "fin" , "fra" , "gbr" , "grc" , "hrv" , "hun" , "irl" , "isl" , "ita" , "jpn",  
             "ltu" , "lva" , "mlt" , "nld" , "nor" , "nzl" , "prt" , "rou" , "svk" , "svn" , "swe" ,
              "tur" , "ukr" ) 

scaling_years<-c(1990:2012)
X_scaling_years<-paste("X",scaling_years,sep="")
input_ef <- input_ef_read
input_em <- input_em_read

# ------------------------------------------------------------------------------
# Start begining of Function 
 
  unfccc_scaling<-function(unfccc_country){
# ------------------------------------------------------------------------------
# 2. Select the inventory data and arrange it into the standard CEDS format

    std_form_inv<-inv_data_full
    #country of interest
    std_form_inv<-std_form_inv[which(std_form_inv$iso==unfccc_country),]
    #columns
    std_form_inv<-std_form_inv[,-c(1,3)]
    names(std_form_inv)[1]<-'inv_sector'

    # Remove redundant and blank rows, and aggregate by the mapping file 
    inv_data <- F.invAggregate( std_form_inv )
    
# ------------------------------------------------------------------------------
# 3. Arrange the CEDS emissions data to match the inventory data
 
# Take the ceds data for the regions of interest and aggregate with the
#   specified mapping method
    F.cedsAggregate( input_ef, input_em, region, mapping_method )

# ------------------------------------------------------------------------------
# 4. Create scaling factors and scaled emissions factors

# Get ratio of inventory data to CEDS data
 
    #debug
    ceds_data = ceds_em_data
    inv_data=inv_data 
    scaling_years=scaling_years 
    int_method = interpolation 
    pre_ext_method = extrapolation_before 
    post_ext_method = extrapolation_after
    
    
    
       F.scale( ceds_em_data, inv_data, scaling_years, int_method = interpolation,
        pre_ext_method = extrapolation_before, 
        post_ext_method = extrapolation_after, region )


       
    
} ##END of function

##########
# ------------------------------------------------------------------------------
# Apply function over the UNFCCC countries

for (i in seq_along(UNFCCCregions)){
  region <- UNFCCCregions[i]
  unfccc_scaling(unfccc_country=region)
  input_ef <- ef_output
  input_em <- em_output
}


# ------------------------------------------------------------------------------
# 5. Output
# Write out the complete set of emissions and emission factors.
# Only those sectors/fuels/country that have been modified by this routine are changed

    
F.write( ef_output, em_output, domain = "MED_OUT" ) 

    logStop()
