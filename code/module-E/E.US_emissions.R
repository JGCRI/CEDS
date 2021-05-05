# ------------------------------------------------------------------------------
# Program Name: E.US_emissions.R
# Authors' Names: Tyler Pitkanen, Jon Seibert, Rachel Hoesly, Andrea Mott
# Date Last Modified: January 24, 2021
# Program Purpose: To read in & reformat US emissions inventory data
# Input Files: national_tier1_caps.xlsx
# Output Files: E.[em]_US_inventory.csv, E.[em]_US_inventory_country_total.csv
# Notes:
# TODO:
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Get emission species first so can name log appropriately
    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[1]
    if ( is.na( em ) ) em <- "CH4"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'common_data.R', "data_functions.R",
                  "emissions_scaling_functions.R", "analysis_functions.R",
                  "interpolation_extension_functions.R" ) # Additional function files required.
    log_msg <- "Initial reformatting of US emissions" # First message to be printed to the log
    script_name <- "E.US_emissions.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Define parameters for inventory-specific script

    inventory_data_file <- 'USA/national_tier1_caps'
    inv_data_folder <- "EM_INV"
    inv_name <- 'US' #for naming diagnostic files
    # Set last year for inventory data here
    # This can be either before or after the last year for the data system
    # Any differences between the two are to be resolved in the module F script.
    # The job of the module E scripts is simply to supply available emissions data
    last_year <- 2019

    inv_years<-c( 1970, 1975, 1980, 1985, 1990:last_year )

# ------------------------------------------------------------------------------
# 2. Inventory in Standard Form (iso-sector-fuel-years, iso-sector-years, etc)


# Import Sheet
    sheet_name <- em
    if ( em == 'NOx' ) sheet_name <- 'NOX'
    if ( em == 'NMVOC' ) sheet_name <- 'VOC'
    if ( em == 'PM25' ) sheet_name <- 'PM25Primary'
    if ( em == 'PM10' ) sheet_name <- 'PM10Primary'
    if (em %in% c ('BC','OC')) {sheet_name <- 'PM25Primary'}

    inv_data_sheet <- readData( inv_data_folder, inventory_data_file , ".xlsx" )


# Process given emission if inventory data exists
    if ( sheet_name %in% names( inv_data_sheet ) ) {
    # Extract the data sheet corresponding to the emissions species
        inv_data_sheet <- data.frame( inv_data_sheet[ sheet_name ] )

    # Clean rows and columns to standard format; different emissions species
    # require different columns and year ranges
        if ( em %in% c('NH3','BC','OC')) {
            inv_years <- c( 1990:last_year )
            number_of_years <- last_year - 1988
            inv_data_sheet <- inv_data_sheet[ -1:-3, 1:number_of_years ]
        } else if ( em == 'NMVOC' ) {
            number_of_years <- last_year - 1984
            inv_data_sheet <- inv_data_sheet[ -1:-3, 1:number_of_years ]
        } else {
          number_of_years <- last_year - 1984
          inv_data_sheet <- inv_data_sheet[ -1:-4,  1:number_of_years ]
        }

    # Name columns
        names( inv_data_sheet ) <- c( 'sector', paste0( 'X', inv_years ) )

    # Convert years to Xyears
        X_inv_years <- paste0( 'X', inv_years )

    # Set iso
        inv_data_sheet$iso <- 'usa'
        inv_data_sheet <- inv_data_sheet[ , c( 'iso', 'sector',
                                               paste0( 'X', inv_years ) ) ] ### use X_inv_years

    # Remove rows with all NAs
        remove.na <- which( apply( inv_data_sheet[ , paste0( 'X', inv_years ) ],
                                   1, function( x ) all.na( x ) ) )
        inv_data_sheet <- inv_data_sheet[ -remove.na, ]

    # Make numeric, convert "NA" to NA
        inv_data_sheet[ , paste0( 'X', inv_years ) ] <-
                suppressWarnings( sapply( inv_data_sheet[ , paste0( 'X',
                                                                    inv_years ) ],
                                          as.numeric ) )

    # Remove "Source Category" sector to prevent confusion
        inv_data_sheet <- inv_data_sheet %>%
            filter(!sector == "Source Category")

    # Remove wildfire emissions; identify the row with sector "Wildfires" and
    # subtract from "Miscellaneous"
        wildfire_emissions <-
               inv_data_sheet[ which( inv_data_sheet$sector == 'Wildfires' ),
                               X_inv_years ]

        # TODO: Since wildfire emissions are not broken out for first few years, insert NA
        # instead for misc category in cases where wildfire emissions that are > 20%
        # of Miscellaneous total. (only the case for NOx at present)
        wildfire_emissions[ is.na( wildfire_emissions ) ] <- 0

        inv_data_sheet[ which( inv_data_sheet$sector == 'MISCELLANEOUS' ),
                                                            X_inv_years ] <-
           inv_data_sheet[ which( inv_data_sheet$sector == 'MISCELLANEOUS' ),
                           X_inv_years ] - wildfire_emissions

    # Convert to metric tonnes
        inv_data_sheet[ , paste0( 'X', inv_years ) ] <-
          as.matrix( inv_data_sheet[ , paste0( 'X', inv_years ) ] ) * 0.9072

    # Seems this introduces too many NAs for NH3, so only run for other species
        if ( em != 'NH3' ) {
        # Remove values that are the are constant carried forward
            check_years <- length( X_inv_years ):2
            check_against <- ( length( X_inv_years ) - 1 ):1
            for ( i in seq_along( check_years ) ) {

                for ( n in seq_along( inv_data_sheet[ , 1 ] ) ) {
                    if ( any( inv_data_sheet[ n, X_inv_years[ check_years[ i ] ] ] ==
                              inv_data_sheet[ n, X_inv_years[ check_against[ i ] ] ],
                              na.rm = TRUE ) )
                      inv_data_sheet[ n, X_inv_years[ check_years[ i ] ] ] <- NA
                }
            }
        }

        # Write out inventory country totals
        # remove sectors included in country level CEDS.
        # Note: the following sectors are included in the output file but not scaling file:
        #           "Miscellaneous without wildfires", "Stationary fuel combustion"
        #           "Industrial and other processes","Transportation", "Miscellaneous"?

        if (em %!in% c("BC","OC")){
            country_total <- inv_data_sheet %>%
                filter( !sector %in% c("Wildfires","Total","Total without wildfires",
                                       "Miscellaneous without wildfires", "Stationary fuel combustion",
                                       "Industrial and other processes","Transportation", "Miscellaneous","Source Category","MISCELLANEOUS") )

            writeData( country_total, domain = "DIAG_OUT", domain_extension = "country-inventory-compare/",
                       paste0('inventory_',em,'_', inv_name))
            # if (em == "NMVOC") {
            #     country_total <- country_total %>%
            #         filter( !sector %in% c("MISCELLANEOUS"))
            # }

            country_total <- country_total %>%
                select(-sector)%>%
                replace(is.na(.), 0) %>%
                group_by(iso) %>%
                summarize_each(funs(sum))

            writeData( country_total, domain = "MED_OUT",
                       paste0('E.',em,'_', inv_name, '_inventory_country_total'))
        }

    } else {
    # Write out blank df if no inventory data exists for given emission
        inv_data_sheet <- data.frame()
    }


# ------------------------------------------------------------------------------
# Find BC and OC emissions
# NOte: removed BC and OC scaling in mod F due to jump in US inv PM2.5 data in early 2000s.
    # kept this for future BC,OC scaling.

    # Define parameters for BC and OC specific script

    ceds_sector <- "1A3b_Road"
    inv_iso <- "usa"
    inv_sector_name <- "HIGHWAY VEHICLES"
 #   X_inv_years <- X_inv_years
    PM <- "PM25"

# Calculate BC and OC emissions

 if (em %in% c ('BC','OC') ) {

         inv_data_sheet <- F.Estimate_BC_OC_emissions(em, PM, inv_iso,ceds_sector,inv_sector_name,X_inv_years)
     }

# ------------------------------------------------------------------------------

# Set mod E output NMVOC to zero before 2004 for the misc sector

    if (em == "NMVOC") {
        inv_data_sheet_misc <- inv_data_sheet %>%
            filter(sector == "MISCELLANEOUS") %>%
            replace(3:20,0)

    #rejoin misc sector and remove old misc sector
    merged_dfs <- rbind(inv_data_sheet,inv_data_sheet_misc)
    df_to_remove <- subset(merged_dfs, sector == "MISCELLANEOUS" & X1970 != 0)
    inv_data_sheet <- anti_join(merged_dfs,df_to_remove)

    }

# ------------------------------------------------------------------------------
# 3. Write standard form inventory
    writeData( inv_data_sheet, domain = "MED_OUT",
               paste0( 'E.', em, '_', inv_name, '_inventory' ) )



# Every script should finish with this line
    logStop()
# END
