#------------------------------------------------------------------------------
# Program Name: H.Extend_CH4_RCP.R
# Author's Name: Ben Goldstein
# Date Last Modified: 7 July 2017
# Program Purpose: Back-extend the CEDS CH4 data based on RCP emissions data.
#                  Provides a supplemental output for CH4 estimates.
# Note: Meant to run as supplemental; not called in the body of the CEDS system
# TODO: 
# 
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# 
# Before we can load headers we need some paths defined. They may be provided 
#   by a system environment variable or they may have been set in the workspace
# Set variable PARAM_DIR to be the data system directory
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"


# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R", "analysis_functions.R" ) # Additional function files required.
    log_msg <- paste0( "Historical energy balances from IEA, aggregated to CEDS",
                       " sectors, and fuels" ) # First message to be printed to the log
    script_name <- "H.Extend_CH4_RCP.R"
    
    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )
    em <- "CH4"
# ---------------------------------------------------------------------------
# 0.5. Script Options
# years
    rcp_start_year <- 1850
    rcp_end_year <- 2000
    CEDS_start_year <- 1850
    CEDS_end_year <- end_year
    
    rcp_years <- seq(from=rcp_start_year,to=rcp_end_year,by=10)
    x_rcp_years <- paste0('X',rcp_years)


# ------------------------------------------------------------------------------
# 1. Read in files

    rcp_dir <- './emissions-inventories/RCP/'

# create temporary folder to extract zipped files
    zipfile_path <- paste0(rcp_dir, em, '.zip')
    dir.name <- paste0(rcp_dir, em, '_RCP_temp_folder')
    dir.create(dir.name)
# unzip files to temp folder  
    unzip(zipfile_path, exdir = dir.name)

# list files in the folder
    files <- list.files(paste0(dir.name,'/',em)  ,pattern = '.dat')
    files <- paste0(dir.name,'/',em,'/',files)
    
    rcp_files <- list()
    for (i in seq_along(rcp_years)){
      rcp_files[i] <- files[grep(rcp_years[i], files)] 
    }
    rcp_files <- unlist(rcp_files)
    
    RCP_df_list <- lapply( X=rcp_files,FUN=read.table,strip.white = TRUE, header=TRUE, 
                           skip = 4,fill=TRUE, stringsAsFactors = FALSE )
    
    for (i in seq_along(rcp_years)){
      RCP_df_list[[i]]$year <- rcp_years[i]
    }
    RCP_df <- do.call("rbind", RCP_df_list)

#create a dataframe for RCP emissions and provide intelligible column headers
    RCP <- RCP_df
    names(RCP)[which(names(RCP)== 'Tot.')] <- "Tot_Ant"
    names(RCP)[which(names(RCP)== 'Ant.')] <- "Region_Name_1"
    names(RCP)[which(names(RCP)== 'Region.1')] <- "Region_Name_2"
    
    RCP$Region_Name_2 <- gsub("(Rest","",RCP$Region_Name_2,fixed=TRUE)
    RCP$Region_Name <- paste(RCP$Region_Name_1,RCP$Region_Name_2)

# Extract the following columns for later use:
    RCP <- RCP[,c('Region','Subregion',"Region_Name","ENE","IND","TRA",
                  "DOM","SLV","AGR","AWB","WST","Tot_Ant",'year')]

# delete temp folder
    unlink(dir.name,recursive = TRUE)
    
# Create mapping files for matching CEDS and RCP sectors and regions 
    Map_region_codes <- readData( "EM_INV", domain_extension = 'RCP/', "RCP Region Mapping", 
                                  ".xlsx", sheet_selection = 'Reg Codes',
                                  meta=FALSE)
    Map_iso_codes <- readData( "EM_INV", domain_extension = 'RCP/', "RCP Region Mapping", 
                               ".xlsx", sheet_selection = 'EDGAR32 & IEA',
                               meta=FALSE)
    Map_sector <- readData( "EM_INV", domain_extension = 'RCP/', 
                            "RCP_CEDS_sector_map",
                            meta=FALSE)
    Master_Country_List <- readData('MAPPINGS', 'Master_Country_List')

# Read in CEDS CH4 emissions
    Total_emissions <- readData( 'MED_OUT', paste0( em, '_total_CEDS_emissions' ) )

# ---------------------------------------------------------------------------
# Create the initial data needed for processing (sector and region mapping)
# Non Comparable Sectors
    rcp_remove_sectors <- c( 'AWB', 'Tot_Ant' )
    ceds_remove_sectors <- c( "1A3ai_International-aviation",
                              "1A3di_International-shipping",
                              '1A3aii_Domestic-aviation',
                              '7A_Fossil-fuel-fires',
                              '3F_Agricultural-residue-burning-on-fields',
                              '11A_Volcanoes', 
                              '11B_Forest-fires', 
                              '11C_Other-natural', 
                              '6B_Other-not-in-total' )


# if current em does not have ship emissions
# for the RCP shipping emissions data Historicalshipemissions_IPCC_FINAL_Jan09_updated_1850.xlsx 
# it doesn't contain data for NH3
    has_ship <- em != "NH3"
    
    if ( has_ship ) {
        ceds_remove_sectors_global <- c( '7A_Fossil-fuel-fires',
                                         '3F_Agricultural-residue-burning-on-fields',
                                         '11A_Volcanoes', 
                                         '11B_Forest-fires', 
                                         '11C_Other-natural', 
                                         '6B_Other-not-in-total' )
      
    } else {
        ceds_remove_sectors_global <- ceds_remove_sectors
      
    }
# Create complete region map for CEDS to RCP
    complete_region_map <- merge( Map_iso_codes, Map_region_codes,
                                  by.x = "RCP Template Reg #",
                                  by.y =, 'RCP Template Reg Code' )
    complete_region_map$Region <- gsub( " [(]Rest of[)]", "", complete_region_map$Region )
    complete_region_map$Region <- gsub( " [(]Estonia, Latvia, Lithuania[)]", "", complete_region_map$Region )
    complete_region_map$Region <- gsub( " [(]Republic of Korea[)]", "", complete_region_map$Region )
    complete_region_map$Region <- gsub( " [(]Democratic People's Republic of Korea[)]", "", complete_region_map$Region )
    complete_region_map[ which( complete_region_map$Code == 'GRL' ), 'Region' ] <- 'Greenland'
    complete_region_map$Region <- gsub( " $", "", complete_region_map$Region, perl = T )

#Correct inconsistencies in regional mapping
    complete_region_map$Name[grep("Germany", complete_region_map$Name)] <- "Germany"
    complete_region_map$Name[grep("United States", complete_region_map$Name)] <- "United States"
    complete_region_map$Name[grep("Korea, Dem", complete_region_map$Name)] <- "North Korea"
    complete_region_map$Name[grep("Korea, Rep", complete_region_map$Name)] <- "South Korea"
    complete_region_map$Name[grep("Taiw", complete_region_map$Name)] <- "Taiwan"
    complete_region_map$Code <- sapply(complete_region_map$Code, tolower)
    colnames(complete_region_map)[ which( colnames(complete_region_map) == "Code" )] <- "iso"

# Create sector map for CEDS to RCP
    sector_map <- Map_sector[complete.cases(Map_sector[,c('CEDS','RCP')]),c('CEDS','RCP')]


# ---------------------------------------------------------------------------
# 2. Map CEDS total emissions to aggregate regions and sectors, get CEDS totals
#    for each sector/region combo

    CEDS_CH4_emissions <- Total_emissions

# Certain sectors are not scalable to RCP; we will extract these and re-add them
# at the end
    sectors_on_hold <- CEDS_CH4_emissions[ CEDS_CH4_emissions$sector %in% 
                                             ceds_remove_sectors, ]
    CEDS_CH4_emissions <- CEDS_CH4_emissions[ CEDS_CH4_emissions$sector %!in% 
                                                ceds_remove_sectors, ]

# Map RCP sectors to CEDS sectors
    CEDS_CH4_emissions$RCP_Sector <- sector_map[ match( CEDS_CH4_emissions$sector, 
                                                        sector_map$CEDS ), 'RCP']
    CEDS_1970_emissions <- CEDS_CH4_emissions[ , c( "iso", "sector", "fuel", 
                                                    "units", "X1970" , "RCP_Sector" ) ]

    CEDS_1970_emissions <- left_join( CEDS_1970_emissions, 
                                      complete_region_map[ c( "iso", "Region", "Sub-region code" ) ], 
                                      by = "iso" )
    colnames( CEDS_1970_emissions ) <- c( "iso", "sector", "fuel", "units", "CEDS_1970_emissions", "RCP_Sector", 
                                          "RCP_Region", "RCP_Sub-region" )
    
# Remove rows that have NAs for sector or region (disconsider CEDS sectors and isos that weren't in RCP)
    CEDS_1970_emissions <- CEDS_1970_emissions[ which( !is.na( CEDS_1970_emissions$RCP_Sector ) ), ]
    eliminated <- CEDS_1970_emissions[ which( is.na( CEDS_1970_emissions$RCP_Region ) ), ]
    CEDS_1970_emissions <- CEDS_1970_emissions[ which( !is.na( CEDS_1970_emissions$RCP_Region ) ), ]

# ---------------------------------------------------------------------------
# 3. Extract RCP regional/sectoral data and cast by year

# Extract all rows that contain data from 1970 or earlier
    RCP_sectors <- c( "ENE", "IND", "TRA", "DOM", "SLV", "AGR", "WST" )
    RCP <- RCP[ !is.na( RCP$Tot_Ant ), ]
    RCP <- RCP[ which( RCP$year <= 1970 ), ]

#Remove a non-comparable sector
    RCP <- RCP[ , -which( names( RCP ) == "AWB" ) ]

# Clean up anomalous 'Asia-"Stan"' non-equivalent region name
    RCP$Region_Name[ grep( "Stan", RCP$Region_Name ) ] <- "Asia-Stan"

# Create a list of years to look at
    RCP_years <- seq( 1850, 1970, by = 10 )
    X_RCP_years <- paste0( "X", RCP_years )

# Melt the RCP data down from sectoral columns and re-cast by year
    RCP_to_melt <- RCP[ , c( "Region_Name", "year", RCP_sectors ) ]
    RCP_melt <- gather( RCP_to_melt, RCP_Sector, value, -Region_Name, -year )
    RCP_melt <- RCP_melt[ which( RCP_melt$RCP_Sector != "AWB" ), ] # AWB is a non-comparable sector
    RCP_melt$year <- paste0( "X", RCP$year )
    RCP_cast_to_year <- spread( RCP_melt, year, value )
    colnames( RCP_cast_to_year )[1] <- "RCP_Region"

# Remove trailing whitespace from RCP_Region for later joining
    RCP_cast_to_year$RCP_Region <- trimws( RCP_cast_to_year$RCP_Region )

# Calculate the proportion of each year's emissions to the 1970 RCP emissions
    RCP_trends <- RCP_cast_to_year
    RCP_trends[ , X_RCP_years ] <- lapply( RCP_trends[ , X_RCP_years ], as.numeric )
    RCP_trends[ , X_RCP_years ] <- RCP_trends[ , X_RCP_years ] / RCP_trends$X1970

# Replace NaN rows with 0s
    RCP_trends[ which( is.na( RCP_trends$X1970 ) ), X_RCP_years ] <- 0

# ---------------------------------------------------------------------------
# 4. Apply RCP trends to CEDS data

# Join the RCP trends to the CEDS emissions columns they'll apply to
    CEDS_1970_and_RCP_factors <- left_join(CEDS_1970_emissions, RCP_trends, by=c("RCP_Region", "RCP_Sector"))

# Multiply the CEDS 1970 data by the RCP trends across the relevant years to generate CEDS back-extensions
    CEDS_1970_and_RCP_factors[, X_RCP_years] <- CEDS_1970_and_RCP_factors[, X_RCP_years] * 
                                                 CEDS_1970_and_RCP_factors$CEDS_1970_emissions

# If data is 0 in RCP 1970 but non-zero in CEDS 1970, we will include these
# datapoints as their CEDS values in order to retain the total emissions for the
# year (makes a 6kt difference in 1970)
    indices_to_replace <- which( CEDS_1970_and_RCP_factors$CEDS_1970_emissions != 0 &
                                 CEDS_1970_and_RCP_factors$X1970 == 0 )
    CEDS_1970_and_RCP_factors$X1970[ indices_to_replace ] <- CEDS_1970_and_RCP_factors$CEDS_1970_emissions[ indices_to_replace ]
    
# Map 'sectors on hold' to RCP years, and assign them to their own sectors
    sectors_on_hold <- sectors_on_hold[ , c( "iso", "sector", "fuel", "units", X_RCP_years ) ]
    sectors_on_hold$RCP_Sector <- NA
    sectors_on_hold$RCP_Sector[ which( sectors_on_hold$sector %in% c( "1A3ai_International-aviation",
                                                                      '1A3aii_Domestic-aviation' ) ) ] <- "Aviation"
    sectors_on_hold$RCP_Sector[ which( sectors_on_hold$sector %in% c( "7A_Fossil-fuel-fires" ) ) ] <- "Fossil-fuel-fires"
    sectors_on_hold$RCP_Sector[ which( sectors_on_hold$sector %in% c( "1A3di_International-shipping" ) ) ] <- "International-shipping"

# Map 'sectors on hold' to RCP regions
    sectors_on_hold <- left_join( sectors_on_hold, 
                                  complete_region_map[ c( "iso", "Region" ) ], 
                                  by = "iso" )
    colnames( sectors_on_hold )[ which( colnames( sectors_on_hold ) == "Region" )] <- "RCP_Region"
    
# Recombine 'sectors on hold' with CEDS extended data to create an output dataframe
    CEDS_backextended_emissions <- rbind( CEDS_1970_and_RCP_factors[ , c("iso", "sector", "fuel", 
                                                                     "units", "RCP_Sector", "RCP_Region", X_RCP_years) ], 
                                      sectors_on_hold )
    CEDS_backextended_emissions <- CEDS_backextended_emissions[ !is.na( CEDS_backextended_emissions$RCP_Sector ), ]
    CEDS_backextended_emissions <- CEDS_backextended_emissions[ !is.na( CEDS_backextended_emissions$RCP_Region ), ]

# ---------------------------------------------------------------------------
# 5. Re-aggregate back-extended data into RCP regions for comparison

# Sum all CEDS data for each year by RCP sector/region combination
    groupColumns <- c("RCP_Sector", "RCP_Region")
    dataColumn = X_RCP_years

# Aggregate CEDS and RCP data frames by large region
    CEDS_aggregate_to_region <- ddply(CEDS_1970_and_RCP_factors, "RCP_Region", function(x) colSums(x[dataColumn]))
    RCP_cast_to_year[, X_RCP_years] <- lapply(RCP_cast_to_year[, X_RCP_years], as.numeric)
    RCP_aggregate_to_region <- ddply(RCP_cast_to_year, "RCP_Region", function(x) colSums(x[dataColumn]))
# Remove Antarctica from RCP summary
    RCP_aggregate_to_region <- RCP_aggregate_to_region[ 2:nrow(RCP_aggregate_to_region ), ]

# Add a column specifying the inventory of origin
    CEDS_aggregate_to_region$inv <- "CEDS"
    RCP_aggregate_to_region$inv <- "RCP"

# Bind the two data frames and reorder columns
    reagg_regional_comparison <- bind_rows(CEDS_aggregate_to_region, RCP_aggregate_to_region)
    reagg_regional_comparison <- reagg_regional_comparison[, c("RCP_Region", "inv", X_RCP_years)]


# ---------------------------------------------------------------------------
# 5. Re-aggregate back-extended data into RCP sectors for comparison

    groupColumns <- c("RCP_Sector", "RCP_Region")
    dataColumn = X_RCP_years

# Aggregate CEDS and RCP data frames by large sector
    CEDS_aggregate_to_sector <- ddply(CEDS_1970_and_RCP_factors, "RCP_Sector", function(x) colSums(x[dataColumn]))
    RCP_aggregate_to_sector <- ddply(RCP_cast_to_year, "RCP_Sector", function(x) colSums(x[dataColumn]))

# Add a column specifying the inventory of origin
    CEDS_aggregate_to_sector$inv <- "CEDS"
    RCP_aggregate_to_sector$inv <- "RCP"

# Bind the two data frames and reorder columns
    reagg_sectoral_comparison <- bind_rows(CEDS_aggregate_to_sector, RCP_aggregate_to_sector)
    reagg_sectoral_comparison <- reagg_sectoral_comparison[, c("RCP_Sector", "inv", X_RCP_years ) ]

# ---------------------------------------------------------------------------
# 6. Write data to supplemental output

    writeData(CEDS_backextended_emissions, domain = "MED_OUT", "H.CH4_RCP_Back-Extended")
    writeData(reagg_regional_comparison, domain = "MED_OUT", "H.CH4_Back-Ext_Compare_to_RCP_by_Region")
    writeData(reagg_sectoral_comparison, domain = "MED_OUT", "H.CH4_Back-Ext_Compare_to_RCP_by_Sector")

# Every script should finish with this line
    logStop()
    
# END
