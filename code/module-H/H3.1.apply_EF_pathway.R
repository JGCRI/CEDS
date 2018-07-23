# Program Name: H3.1.apply_EF_pathway.R
# Author: Linh Vu
# Date Last Updated: 8 Nov 2016
# Program Purpose: Apply minimum/maximum EF pathway to extended EF
# Input Files: H.[em]_total_EFs_extended.csv, Master_Country_List.csv
# Output Files: H.[em]_total_EFs_extended_adjusted-pathway.csv
# Notes:
# TODO:
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( 'data_functions.R', 'timeframe_functions.R', 'common_data.R')
log_msg <- "Apply emissions factor pathway"
script_name <- "H3.1.apply_EF_pathway.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "NOx"

# ---------------------------------------------------------------------------
# 0.5 Load Packages
    library( "tools" )

# ---------------------------------------------------------------------------
# 1. Read input
    Master_Country_List <- readData( "MAPPINGS", "Master_Country_List" )
    ef <- readData( "MED_OUT" , paste0( "H.", em, "_total_EFs_extended" ) )

# Read EF pathway for selected emission
    fl <- list.files( path = "./extension/EF-pathway", pattern = "EF_pathway.csv" )
    fl <- tools::file_path_sans_ext( fl )
    fl <- fl[ grepl( em, fl ) ]
    if (em == "OC" )
      fl <- fl[ !grepl( "NMVOC", fl ) ]
    fl <- lapply( fl, FUN = readData, domain = "EXT_IN",
                  domain_extension = "EF-pathway/" )

# ---------------------------------------------------------------------------
# 2. Expand EF pathway to all years
if( length( fl ) > 0 ){

# Bind all EF pathways into one df
    pathway <- do.call( rbind.fill, fl )
    id_cols <- names( pathway )[ !grepl( "X", names( pathway ) ) ]
    pathway_Xyears <- names( pathway )[ grepl( "X", names( pathway ) ) ]

# Add columns for all years
    Xyears_to_add <- X_extended_years[ X_extended_years %!in% pathway_Xyears ]
    pathway_full <- pathway
    pathway_full[, Xyears_to_add ] <- NA
    pathway_full <- pathway_full[ c( id_cols, X_extended_years ) ]

# Extend EF pathway backward/forward
# TODO: add interpolation
    pathway_full_long <- melt( pathway_full, id = id_cols ) %>%
      dplyr::arrange( iso, sector, fuel )
    pathway_full_long <- group_by( pathway_full_long, iso, sector, fuel ) %>%
      dplyr::mutate( value = na.locf( value, fromLast = T, na.rm = F ),
              value = na.locf( value, na.rm = F ) )
    pathway_full <- cast( pathway_full_long )

# Add rows for all iso
  if ( any( pathway_full$iso == "all" ) )
  {
    all_iso <- unique( Master_Country_List$iso )
    iso_to_add <- all_iso[ all_iso %!in% pathway_full$iso ]
    rows_to_add <- filter( pathway_full, iso == "all" )
    rows_to_add$iso <- NULL
    rows_to_add <- merge( rows_to_add, data.frame( iso = iso_to_add ) )
    pathway_full <- rbind( pathway_full, rows_to_add ) %>%
      filter( iso != "all" ) %>%
      dplyr::arrange( iso, sector, fuel )
  }

# Recast into more convenient format
    pathway_full_long <- melt( pathway_full, id = id_cols )
    names( pathway_full_long )[ names( pathway_full_long ) %in% c( "variable", "value" ) ] <- c( "year", "pathway_val" )
    pathway_id_cols <- select( pathway_full_long, -pathway_type, -pathway_val ) %>% unique()

    pathway_min <- filter( pathway_full_long, pathway_type == "minimum" )
    names( pathway_min )[ names( pathway_min ) == "pathway_val" ] <- "min_ef"
    pathway_max <- filter( pathway_full_long, pathway_type == "maximum")
    names( pathway_max )[ names( pathway_max ) == "pathway_val" ] <- "max_ef"

    pathway_full_long <- merge( pathway_id_cols, pathway_min, all = T ) %>%
      merge( pathway_max, all = T )
    pathway_full_long$min_ef[ is.na( pathway_full_long$min_ef ) ] <- -Inf
    pathway_full_long$max_ef[ is.na( pathway_full_long$max_ef ) ] <- Inf
    pathway_full_long <- select( pathway_full_long, iso, sector, fuel, year, min_ef, max_ef )

# ---------------------------------------------------------------------------
# 3. Apply EF pathway to selected emissions
    printLog( paste( "Apply EF pathway for", em ) )

# Subset relevant iso+sector+fuel in EF data
    ef_subset <- filter( ef, paste0( iso, sector, fuel ) %in% paste0(
      pathway_full_long$iso, pathway_full_long$sector, pathway_full_long$fuel ) )

# Melt to long format
    id_cols_db <- names( ef_subset )[ !grepl( "X", names( ef_subset ) ) ]
    ef_subset_long <- melt( ef_subset, id = id_cols_db )
    names( ef_subset_long )[ names( ef_subset_long ) %in% c( "variable", "value" ) ] <- c( "year", "ef" )

# Add columns of min and max EF
    ef_subset_long <- merge( ef_subset_long, pathway_full_long, all.x = T )

# Apply EF pathway
    ef_subset_long$ef[ ef_subset_long$ef < ef_subset_long$min_ef ] <-
      ef_subset_long$min_ef[ ef_subset_long$ef < ef_subset_long$min_ef ]
    ef_subset_long$ef[ ef_subset_long$ef > ef_subset_long$max_ef ] <-
      ef_subset_long$max_ef[ ef_subset_long$ef > ef_subset_long$max_ef ]
    ef_subset_long <- select( ef_subset_long, -min_ef, -max_ef )

# Recast to wide format
    ef_subset <- cast( ef_subset_long, iso + sector + fuel + units ~ year, value = "ef" )

# Combine with rest of EF database
    ef_pathway_applied <- filter( ef, paste0( iso, sector, fuel ) %!in% paste0(
      ef_subset$iso, ef_subset$sector, ef_subset$fuel ) ) %>%
      bind_rows( ef_subset ) %>% dplyr::arrange( iso, sector, fuel )


# Do nothing if there are no EF pathway files in directory
} else {
    printLog( paste( "No EF pathway existed for", em, ". No modification made." ) )
    ef_pathway_applied <- ef
}

# ---------------------------------------------------------------------------
# 4. Write out updated EF and emissions
    writeData( ef_pathway_applied, "MED_OUT", paste0( "H.", em, "_total_EFs_extended_adjusted-pathway" ) )

logStop()
# END
