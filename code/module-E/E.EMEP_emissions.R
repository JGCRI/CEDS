# ------------------------------------------------------------------------------
# Program Name: E.EMEP_emissions.R
# Author(s): Patrick O'Rourke, Andrea Mott, Harrison Suchyta, Noah S. Prime
# Date Last Updated: October 17, 2023
# Program Purpose: To read in & reformat EMEP emissions data.
# Input Files: All EMEP emissions data, Master_Country_List.csv
# Output Files: All Initial EMEP txt files resaved as csv files (in input folder),
#               E.em_EMEP_inventory.csv, E.em_EMEP_Russia.csv
#               E.em_EMEP_NFRX_inventory_country_total.csv, E.em_EMEP_NFRX_Russia_inventory_country_total.csv
# Notes: 1. EMEP Emissions are provided from 1980-2017
# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
    headers <- c( 'common_data.R',"emissions_scaling_functions.R","data_functions.R", "analysis_functions.R",
                  'interpolation_extension_functions.R' ) # Any additional function files required
    log_msg <- "Initial reformatting of the EMEP Emissions" # First message to be printed to the log
    script_name <- "E.EMEP_emissions.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# -----------------------------------------------------------------------------------------------------------
# 0.5 Settings/Load Files & Convert all txt files to csv
#     Logging does not support txt files, so convert to csv
    MCL <- readData( "MAPPINGS", "Master_Country_List" )
    loadPackage('tools')

# Describes which emission species is being analyzed
    args_from_makefile <- commandArgs( TRUE )
    em <<- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "NH3"

#   TODO: When level 2 NFR14 data is used, this will need to include an if else statement - as level 2 NFR14
#         data for SO2 is now labelled as SO2, not SOx. It will also need to be moved lower, so that
#         script objects 'level' and 'Em_Format' have been defined for the if else statement.
    em.read <- em
    if( em == "SO2" ) em.read <- "SOx" # TODO: The EMEP NFR14 level 2 file might be named SO2, not SOx
    if (em %in% c ('BC','OC')) em.read <- "PM25"


# -----------------------------------------------------------------------------------------------------------
# 0.75 Select EMEP level

# Select EMEP level - 'LEVEL1' OR 'LEVEL2'
    level <- 'LEVEL1'

# Select data format (NFR14 or NFR09)
    Em_Format <<- args_from_makefile[ 2 ]
    if ( is.na( Em_Format ) ) Em_Format <- "NFR14"


# -----------------------------------------------------------------------------------------------------------
# 1. Read in files

# Create a List of EMEP Files
    inv_file_name <- paste0( 'EMEP_', Em_Format, '_',
                             level, '_', em.read, ".txt" )

    file_path <- filePath( 'EM_INV', inv_file_name, "", "EMEP/" )

    if ( file.exists( file_path ) ) {

    # Function used to read in list of txt files
        inv <- read.table( paste0( './emissions-inventories/EMEP/',
                                   inv_file_name ),
                           skip = 0, header = FALSE, sep = ";",
                           na.strings = c( "", " ", "NA" ) ) # Converts all blank spaces to 'NA'
        names( inv ) <- c( 'ISO2', "year", "sector", "emission_species",
                           "units", "emissions" )


    # Writes each object as same format, but converted to a csv file
        writeData( inv, 'EM_INV', domain_extension = "EMEP/",
                   fn = paste0( 'EMEP_', Em_Format, '_', level, '_', em.read ),
                   meta = FALSE ) # Don't write metadata, as it is provided as an input

    # Now read back in

        file_name <- paste0( 'EMEP_', Em_Format, '_', level, '_', em.read )
        EMEP <- readData( 'EM_INV', domain_extension = "EMEP/", file_name )

    # Create empty list if file does not exist
    } else {

        EMEP <- list( )

    }


# -----------------------------------------------------------------------------------------------------------
# 2. Formatting Data

    # NFR09 does not have Road Transport broken out, so don't go through BC/OC estimation routine
    continue_process <- (length(EMEP) > 0) & !((em.read == 'PM25') & (Em_Format == 'NFR09'))

    if ( continue_process ) {

        EMEP_em <- EMEP

    	# Reorder Columns of Interest
	    EMEP_em <- EMEP_em %>%
	        dplyr::select(ISO2, sector, units, year, emissions)

    	# Order By ISO2 & sector
	    EMEP_em <- dplyr::mutate( EMEP_em, ISO2 = as.character( ISO2 ) )
	    EMEP_em <- EMEP_em %>%
	        dplyr::arrange(ISO2, sector)

    	# Remove EU, EU 9, EU 12, EU 15, EU 27 Data (Interested in Countries not EU)
	    remove_ISO2 <- c( 'EU', 'EU09', 'EU12', 'EU15', 'EU27', 'EU28' )
	    EMEP_em <- EMEP_em %>%
	        dplyr::filter( !(ISO2 %in% remove_ISO2) )

    	# Mapping EMEP ISO2 to CEDS iso codes
	    EMEP_em <- EMEP_em %>%
	        left_join(MCL %>% dplyr::select(iso, EMEP), by = c('ISO2' = 'EMEP') ) %>%
	        dplyr::select(iso, sector, units, year, emissions) %>%
	        dplyr::distinct()

    	# Convert years to Xyears
	    EMEP_em <- EMEP_em %>%
	        dplyr::mutate( year = paste0('X', year))

    	# Convert emissions to numeric
    	EMEP_em$emissions <- as.numeric( EMEP_em$emissions )

  	    # Remove any rows with NA values
	    EMEP_em <- EMEP_em[ complete.cases( EMEP_em ), ]

        # NOTE THIS INTRODUCES NEW NA's (from "C, IE, NE, NO, NR" where:
        # IE = Sources Included elsewhere, NE = Sources not Estimated
        # NO = Not Occuring, NR = Not Reported)

    	# Cast to wide format
	    EMEP_emdf <- EMEP_em %>%
	        tidyr::spread(key=year, value=emissions, fill = 0) %>%
	        filter(sector != "N_Natural")


    	# Relabel units from Gg to kt (same numerical value, different label)
	    EMEP_emdf$units <- 'kt'


	    # ------------------------------------------------------------------------------
	    # Find BC and OC emissions

	    # This section is because BC/OC/PM2.5 contributions are quite mixed in many sectors
	    # and hard to break out, but for road transport mostly diesel comb

	    # Define parameters for BC and OC specific script
	    ceds_sector <- "1A3b_Road"
	    inv_iso <- EMEP_em$iso
	    inv_sector_name <- c("F_RoadTransport")
	    X_inv_years <- grep("X", colnames(EMEP_emdf), value=TRUE)
	    PM <- "PM25"

	    # clean PM25 inv sheet for BC and OC script
	    inv_data_sheet <- EMEP_emdf %>%
	      select(-units)

	    # Calculate BC and OC emissions
	    if (em %in% c("BC","OC") ) {
            em_emissions <- F.Estimate_BC_OC_emissions(em, PM, inv_iso,ceds_sector,inv_sector_name,X_inv_years)
            EMEP_emdf <- em_emissions
            EMEP_emdf <- EMEP_emdf
	    }

	    # ------------------------------------------------------------------

        # Subset Russian Data
        EMEP_emRUS <- EMEP_emdf %>%
	        dplyr::filter( iso == 'rus' )

	    # Get remaining non-Russian data
	    EMEP_emdf <- EMEP_emdf %>%
	        dplyr::filter( iso != 'rus' )

    # End processing for EMEP species
    } else {

        # Create dummy files for non EMEP species
        EMEP_emdf <- c( 'iso', 'sector', 'year' )
        EMEP_emRUS <- c( 'iso', 'sector', 'year' )

    }

# ------------------------------------------------------------------------------
# 3. Report BC totals (actual BC emissions not PM inferred emissions)

    if( em == 'BC'){

        # Create a List of EMEP Files
        inv_file_name <- paste0( 'EMEP_', Em_Format, '_',
                                 level, '_', em, ".txt" )

        file_path <- filePath( 'EM_INV', inv_file_name, "", "EMEP/" )

        if ( file.exists( file_path ) ) {

            # Function used to read in list of txt files
            inv <- read.table( paste0( './emissions-inventories/EMEP/',
                                       inv_file_name ),
                               skip = 0, header = FALSE, sep = ";",
                               na.strings = c( "", " ", "NA" ) ) # Converts all blank spaces to 'NA'
            names( inv ) <- c( 'ISO2', "year", "sector", "emission_species",
                               "units", "emissions" )


            # Writes each object as same format, but converted to a csv file
            writeData( inv, 'EM_INV', domain_extension = "EMEP/",
                       fn = paste0( 'EMEP_', Em_Format, '_', level, '_', em ),
                       meta = FALSE ) # Don't write metadata, as it is provided as an input

            # Now read back in

            file_name <- paste0( 'EMEP_', Em_Format, '_', level, '_', em )
            EMEP_BC <- readData( 'EM_INV', domain_extension = "EMEP/", file_name )

            EMEP_BC_em <- EMEP_BC

            # Reorder Columns of Interest
            EMEP_BC_em <- EMEP_BC_em %>%
                dplyr::select(ISO2, sector, units, year, emissions)

            # Order By ISO2 & sector
            EMEP_BC_em <- dplyr::mutate( EMEP_BC_em, ISO2 = as.character( ISO2 ) )
            EMEP_BC_em <- EMEP_BC_em %>%
                dplyr::arrange(ISO2, sector)

            # Remove EU, EU 9, EU 12, EU 15, EU 27 Data (Interested in Countries not EU)
            remove_ISO2 <- c( 'EU', 'EU09', 'EU12', 'EU15', 'EU27', 'EU28' )
            EMEP_BC_em <- EMEP_BC_em %>%
                dplyr::filter( !(ISO2 %in% remove_ISO2) )

            # Mapping EMEP ISO2 to CEDS iso codes
            EMEP_BC_em <- EMEP_BC_em %>%
                left_join(MCL %>% dplyr::select(iso, EMEP), by = c('ISO2' = 'EMEP') ) %>%
                dplyr::select(iso, sector, units, year, emissions) %>%
                dplyr::distinct()

            # Convert years to Xyears
            EMEP_BC_em <- EMEP_BC_em %>%
                dplyr::mutate( year = paste0('X', year))

            # Convert emissions to numeric
            EMEP_BC_em$emissions <- as.numeric( EMEP_BC_em$emissions )

            # Remove any rows with NA values
            EMEP_BC_em <- EMEP_em[ complete.cases( EMEP_BC_em ), ]

            # NOTE THIS INTRODUCES NEW NA's (from "C, IE, NE, NO, NR" where:
            # IE = Sources Included elsewhere, NE = Sources not Estimated
            # NO = Not Occuring, NR = Not Reported)


            # print out all BC inventory
            EMEP_BC_emdf_sector <- EMEP_BC_em %>%
                filter(!is.na(year)) %>%
                group_by(iso, sector, year) %>%
                dplyr::summarize(emissions = sum(emissions, na.rm = TRUE)) %>%
                mutate(units = 'kt') %>%
                tidyr::spread(key=year, value=emissions, fill = 0) %>%
                arrange(iso, sector)
            writeData( EMEP_BC_emdf_sector, domain = "MED_OUT",
                       fn = paste0( "E.", em, "_EMEP_", Em_Format, "_inventory_country_sector_all" ),
                       meta = TRUE )

            # aggregate and Cast to wide format
            EMEP_BC_emdf <- EMEP_BC_em %>%
                filter(!is.na(year)) %>%
                filter(sector %!in% c('z_Memo', 'P_IntShipping', 'N_Natural')) %>%
                group_by(iso, year) %>%
                dplyr::summarize(emissions = sum(emissions, na.rm = TRUE)) %>%
                mutate(units = 'kt') %>%
                tidyr::spread(key=year, value=emissions, fill = 0)
            # Relabel units from Gg to kt (same numerical value, different label)
            BC_filename <- paste0( "E.", em, "_EMEP_", Em_Format, "_inventory_country_total" )
            writeData( EMEP_BC_emdf, domain = "MED_OUT",
                       fn = BC_filename,
                       meta = TRUE )

        }


    }

# ------------------------------------------------------------------------------
# 3. Output

    filename <- paste0( "E.", em, "_EMEP_", Em_Format, "_inventory" )
    if ( level == 'LEVEL2' ){ filename <- paste0( filename, "_level2" ) } # TODO: (future) When ready to use, add level 2 output to Makefile


    # Write Data:
    writeData( EMEP_emdf, domain = "MED_OUT",
               fn = filename,
               meta = TRUE )

    # Write Russian Data:
    writeData( EMEP_emRUS, domain = "MED_OUT",
               fn = paste0( filename, "_Russia" ),
               meta = TRUE )

    # Write out inventory country totals. Do not include emissions not in inventory data.
    if (em %!in% c('BC','OC','CH4','CO2','N2O')){
        # remove sectors not scaled in mod F.
        country_total <- EMEP_emdf %>%
            dplyr::filter( !(sector %in% c("P_IntShipping","H_Aviation","O_AviCruise","M_Other","N_Natural","z_Memo")) )

        # Save data to diagnostics folder before aggregation
        writeData( country_total, domain = "DIAG_OUT", domain_extension = "country-inventory-compare/",
                    paste0('inventory_',em,'_EMEP'))

        country_total_reg <- country_total %>%
            left_join( MCL %>% select(c(iso, Region)) %>% unique )

        writeData( country_total_reg, domain = "DIAG_OUT", domain_extension = "country-inventory-compare/",
                   paste0('inventory_',em,'_EMEP_with_EW_regions'))

        # Aggregate emissions by country
        country_total <- country_total %>%
            select(-c(sector,units))%>%
            group_by(iso) %>%
            summarize_each(sum)

        # Save country totals to intermediate
        writeData( country_total, domain = "MED_OUT",
            paste0('E.',em,'_EMEP_', Em_Format, '_inventory_country_total'))

    }
# Every script should finish with this line-
    logStop()

# END
