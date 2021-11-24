# ---------------------------------------------------------------------------
# Program Name: C1.2.add_NC_default_EF.R
# Author: Rachel Hoesly, Andrea Mott
# Date Last Updated: 4 June 2021
# Program Purpose: Adds default EF data from the EF_parameters folder to the
#                  C.[em]_NC_EF.csv
# Input Files: files in the EF_parameters folder containing default EFs
# Output Files: C.[em]_NC_User_Added_EF.csv
# Notes:

# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( 'process_db_functions.R', 'data_functions.R', "analysis_functions.R",
                  'interpolation_extension_functions.R', 'common_data.R' )
# Additional function files may be required.
    log_msg <- "Adding additional emission factors" # First message to be printed to the log
    script_name <- "C1.2.add_NC_default_EF.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

    args_from_makefile <- commandArgs( TRUE )
    em <- args_from_makefile[ 1 ]
    if ( is.na( em ) ) em <- "SO2"

# ---------------------------------------------------------------------------
# 0.5 Load Packages

    loadPackage( 'tools' )

# ---------------------------------------------------------------------------
# 1. Reading data and mappings into script

# Read in all files in the EF_parameters folder
    files_list <- list.files( path = './default-emissions-data/EF_parameters/process_EFs',
                              pattern = '*.csv' )
    files_list <- tools::file_path_sans_ext( files_list )

# select files with "_EF"
    EF_file_list <- files_list[ grep( pattern = "_EF", files_list ) ]

# Deselect files with "metadata"
    EF_file_list <- EF_file_list[ -grep( pattern = "metadata", EF_file_list ) ]

# Files with the emission in the name
    EF_file_list <- EF_file_list[ grep( pattern = paste0( '\\.', em ), EF_file_list ) ]

# Order files alphabetically (case insensitive)
    EF_file_list <- EF_file_list[str_order(EF_file_list, locale = "en")]

# Read in all files
    EF_list <- lapply ( X = EF_file_list, FUN = readData,
                        domain = "DEFAULT_EF_PARAM_NC" )
# ---------------------------------------------------------------------------
# 2. Expand "all" variable and extend over time, convert list to one df

# Expand all, interpolate and Extend forward and back
    EF_extended <- lapply( X = EF_list, FUN = extendDefaultEF,
                           pre_ext_method_default = 'none' )

# Add all EFs to a single dataframe
    EF_df <- do.call( "rbind.fill", EF_extended )

    EF_df$units <- 'kt/kt'

# Remove iso-sectors that have user-added emissions data
    user_added_emissions_df <- readData('DEFAULT_EF_IN', domain_extension = 'non-combustion-emissions/',
                                        paste0('C.',em,'_NC_emissions_user_added'))

    # Organize user-added emissions data
    user_added_emissions_iso_sector <- user_added_emissions_df %>%
        select(iso, sector) %>%
        dplyr::rename(iso2 = iso) %>%
        dplyr::rename(sector2 = sector)

    # Remove iso-sectors from EF dataframe that have user added emissions
    EF <- EF_df %>%
        anti_join(user_added_emissions_iso_sector, by = c("iso" = "iso2", "sector" = "sector2"))

    # Write warning for those iso-sectors removed:
    overlap <- EF_df %>%
        semi_join(user_added_emissions_iso_sector, by = c("iso" = "iso2", "sector" = "sector2"))
    if (nrow(overlap) >0){
        warning("The following iso-sector's user added emissions are removed", paste0(overlap$iso,", ",overlap$sector, "; "))
    }
# ---------------------------------------------------------------------------
# 2. Add to existing parameter Dbs

# If there are any EFs, create a dataframe. Also write out
# a diagnostic file recording the user-defined EFs Otherwise, print a
# message that no EFs were added.
    # Also, remove sintering EFs since we added sintering to the EFs,
    # but it's not a sector defined in MSL.
    if ( length( EF_list ) > 0 ) {
        printLog( paste( 'Adding new data to existing emission factor',
                         'data base for', em ) )
        writeData( EF, 'DIAG_OUT',
                   paste0( 'C.', em, '_NC_User_Added_EF' ) )
        addToDb_overwrite( new_data = EF, em = em, module = "C",
                           file_extension = 'NC_EF' )
    } else {
        printLog( paste( 'No data to be added to existing EF database for ', em ) )
    }

    logStop()
# END
