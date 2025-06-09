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
    if ( is.na( em ) ) em <- "BC"

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
    last_year <- 2023

    inv_years<-c( 1970, 1975, 1980, 1985, 1990:last_year )

# ------------------------------------------------------------------------------
# 2. Inventory in Standard Form (iso-sector-fuel-years, iso-sector-years, etc)


# Import Sheet
    sheet_name <- em
    if ( em == 'NOx' ) sheet_name <- 'NOX'
    if ( em == 'NMVOC' ) sheet_name <- 'VOC'
    if ( em == 'PM25' ) sheet_name <- 'PM25Primary'
    if ( em == 'PM10' ) sheet_name <- 'PM10Primary'

# Big loop to double process BC emissions so can write out new EPA BC
# emissions in standard format. First pass write transportation as before
# second pass write all sectors in the newer data

MaxLoop = 1
if ( ( em %in% c ('BC', 'OC') ) ) { MaxLoop = 2 }
inv_data_workbook <- readData( inv_data_folder, inventory_data_file , ".xlsx" )
inv_years_to_ignore <- readData( inv_data_folder, "USA/US_Trends_Years_to_Ignore" , ".csv" )

for (LoopCounter in 1:MaxLoop) {

    if ( ( em %in% c ('BC','OC')) && (LoopCounter == 1) ) {sheet_name <- 'PM25Primary'}
    if ( LoopCounter == 2 ) {
        if (em== 'BC') sheet_name <- "Black Carbon"
        if (em== 'OC') sheet_name <- "Organic Carbon"
    }

# Process given emission if inventory data exists
    if ( sheet_name %in% names( inv_data_workbook ) ) {
    # Extract the data sheet corresponding to the emissions species
        inv_data_sheet <- data.frame( inv_data_workbook[ sheet_name ] )

    # Clean rows and columns to standard format; different emissions species
    # require different columns and year ranges
    if ( LoopCounter == 1 ) {
            if ( em %in% c('NH3','BC','OC')) { # Emissions available only from 1990 onward
            inv_years <- c( 1990:last_year )
            number_of_years <- last_year - 1990 + 2
            inv_data_sheet <- inv_data_sheet[ -1:-3, 1:number_of_years ]
        } else if ( em == 'NMVOC' ) { # Available for entire time period, but different header
            number_of_years <- last_year - 1984
            inv_data_sheet <- inv_data_sheet[ -1:-3, 1:number_of_years ]
        } else { # Available for entire time period
          number_of_years <- last_year - 1984
          inv_data_sheet <- inv_data_sheet[ -1:-4,  1:number_of_years ]
        }
    } else {
        inv_years <- c( 2002:last_year )
        number_of_years <- last_year - 2002 + 2
        inv_data_sheet <- inv_data_sheet[ -1:-3, 1:number_of_years ]
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

    # Note, now exact "Miscellaneous" detail has been added in the inventory sheet from
    # EQUATES from 2002 forward.
    # Note that in the NEI there are substantial non-wildfire burning emissions from
    # Fires - Agricultural Field Burning and Fires - Prescribed Fires. These are open burning
    # and are not included in CEDS

    # Convert to metric tonnes
        inv_data_sheet[ , paste0( 'X', inv_years ) ] <-
          as.matrix( inv_data_sheet[ , paste0( 'X', inv_years ) ] ) * 0.9072

    # Remove duplicate rows
    inv_data_sheet %>% dplyr::mutate(id = row_number()) %>% group_by(sector) %>% arrange(sector) %>%
                   slice_tail() %>% ungroup()  %>% arrange(id) %>% select(-id) -> inv_data_sheet

        # Write out inventory country totals
        # remove sectors included in country level CEDS.
        # Note: the following sectors are included in the output file but not scaling file:
        #           "Miscellaneous without wildfires", "Stationary fuel combustion"
        #           "Industrial and other processes","Transportation", "Miscellaneous"?

        if (em %!in% c("BC","OC")){
            if (em %!in% c("NH3", "NMVOC")){

            country_total <- inv_data_sheet %>%
                filter( !sector %in% c("Wildfires","Total","Total without wildfires",
                                       "Miscellaneous without wildfires", "Stationary fuel combustion", "Total without miscellaneous",
                                       "Industrial and other processes","Transportation", "Miscellaneous","Source Category","MISCELLANEOUS") )
            } else {
                country_total <- inv_data_sheet %>%
                    filter( !sector %in% c("Wildfires","Total","Total without wildfires",
                                           "Miscellaneous without wildfires", "Stationary fuel combustion",
                                           "Industrial and other processes","Transportation", "Miscellaneous","Source Category") )
            }

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

        # Where there is no real trend data by sector, set to NA

        # Seems this introduces too many NAs for NH3, so only run for other species
        if ( em != 'NH3' ) {
            # Remove values that are the are constant carried forward
            # Should update this to check for changes < 0.5% for non-zero cells
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

        # With new EQUATES-based methods, checking for equality no longer works reliably
        # So also use species specific rules by sector
        ignore_years <- subset(inv_years_to_ignore, select = c("sector",sub(" ",".",sheet_name))) # Sub takes care of species that have spaces in their sheet name
        names(ignore_years)[2] <- "nyears"

        # Set to NA years that are largeley extrapolated so they won't be used for scaling
        inv_data_sheet_long <- inv_data_sheet %>%
            # Join with data on which years are not actual data
            left_join(ignore_years, by = "sector") %>%
            # Put in long form
            pivot_longer(cols = X_inv_years, values_to ="emission", names_to = "year") %>%
            mutate( year = as.numeric( substr(year, 2, 5) ) ) %>%
            # Do calc and set latest data to NA depending on species (from input file) and sector
            mutate( emission = ifelse(year > last_year - nyears, NA, emission ) ) %>%
            # Back to CEDS Xyear format
            mutate( year = paste0("X",year) ) %>% select(-nyears)

        # We are done! Back to wide form
        inv_data_sheet <- pivot_wider( inv_data_sheet_long, values_from = emission, names_from = year)

    } else {
    # Write out blank df if no inventory data exists for given emission
        printLog( paste('WARNING: sheet',sheet_name,'not found in workbook, Writting out blank dataframe'))
        inv_data_sheet <- data.frame()
    }


# ------------------------------------------------------------------------------
# Estimate BC and OC emissions from transportation
# Note: removed BC and OC scaling in mod F due to jump in US inv PM2.5 data in early 2000s.
    # kept this for potential future BC,OC scaling.

 if (em %in% c ('BC','OC') &&  (LoopCounter == 1) ) {

    # Define parameters for BC and OC specific script

    ceds_sector <- "1A3b_Road"
    inv_iso <- "usa"
    inv_sector_name <- "HIGHWAY VEHICLES"
 #   X_inv_years <- X_inv_years
    PM <- "PM25"

# Calculate BC and OC emissions

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
   writeoutName = inv_name
   # use different name for BC inventory since we're not using that for scaling yet
   # since we don't also have OC in the same format.
   # Write out for convenience to be able to compare to CEDS values
   # (and potentially use as defaults)
   if ( LoopCounter == 2 ) writeoutName = paste0(writeoutName,"_full")

   writeData( inv_data_sheet, domain = "MED_OUT",
              paste0( 'E.', em, '_', writeoutName, '_inventory' ) )

} # End of BC double loop

# Every script should finish with this line
    logStop()
# END
