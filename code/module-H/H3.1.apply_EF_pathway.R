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
    ef_in <- readData( "MED_OUT" , paste0( "H.", em, "_total_EFs_extended" ) )

    ef <- ef_in

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

# TODO - Move extension here before binding. Otherwise, unexpected results can occur.
#      - Or write warning if final year of pathways differ

# Bind all EF pathways into one df
    pathway <- do.call( rbind.fill, fl )
    id_cols <- names( pathway )[ !grepl( "X", names( pathway ) ) ]
    pathway_Xyears <- names( pathway )[ grepl( "X", names( pathway ) ) ]

    if  (max(pathway_Xyears) < max(X_extended_years)) {
        warning('EF_Pathway files do not extend to last year. Extending pathways constantly.')
        printLog('EF_Pathway files do not extend to last year. Extending pathways constantly.')

   min_pathway_year <- pathway_Xyears %>% min %>% str_replace('X','') %>% as.numeric()
   min_pathway_extension_year <- pathway_Xyears %>% max %>% str_replace('X','') %>% as.numeric()
   max_pathway_extension_year <- X_extended_years %>% max %>% str_replace('X','') %>% as.numeric()
   pathway_extension_years <- (min_pathway_extension_year+1) : max_pathway_extension_year

   pathway[ paste0('X', pathway_extension_years) ] <- do.call(bind_cols,
                                                             replicate( length(pathway_extension_years),
                                                                        pathway[ paste0('X',  min_pathway_extension_year ) ] ) ) %>% unname()

    }

# Add columns for all years
    Xyears_to_add <- X_extended_years[ X_extended_years %!in% pathway_Xyears ]
    pathway_full <- pathway
    pathway_full[, Xyears_to_add ] <- NA
    pathway_full <- pathway_full[ c( id_cols, X_extended_years ) ]

# Eliminate any duplicate rows
    pathway_full <- 
        pathway_full %>% distinct(iso,sector,fuel, .keep_all = TRUE)

# Extend EF pathway backward/forward
# TODO: add interpolation
    pathway_full_long <- melt( pathway_full, id = id_cols ) %>%
      dplyr::arrange( iso, sector, fuel, pathway_type )
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
    pathway_full_long <- pathway_full %>% pivot_longer(-id_cols, names_to = "year", values_to = "pathway_val")


  }

# Recast into more convenient format
    pathway_id_cols <- select( pathway_full_long, -pathway_type, -pathway_val ) %>% unique()

    # TODO: With newer packages, the code below doesn't work (there is no "pathway_val" label so min_ef and max_ef end up in the wrong place)

    pathway_min <- filter( pathway_full_long, pathway_type == "minimum" )
    names( pathway_min )[ names( pathway_min ) == "pathway_val" ] <- "min_ef"
    pathway_max <- filter( pathway_full_long, pathway_type == "maximum")
    names( pathway_max )[ names( pathway_max ) == "pathway_val" ] <- "max_ef"
    pathway_min <- select(pathway_min,-c("pathway_type"))
    pathway_max <- select(pathway_max,-c("pathway_type"))

    # FIX THIS

    pathway_full_long <-  pathway_id_cols %>%
       left_join(pathway_max) %>%
       left_join(pathway_min) %>%
       replace_na(list(min_ef = -Inf, max_ef = Inf)) %>%
       select(iso, sector, fuel, year, min_ef, max_ef)


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
# 4. Check and Write out updated EF and emissions

    # Check input ef_db and output ef_db
    ef <- ef %>%
        arrange(iso, sector, fuel, units)
    ef_pathway_applied <- ef_pathway_applied %>%
        arrange(iso, sector, fuel, units)
    if( ! identical(ef[c('iso', 'sector','fuel')],ef_pathway_applied[c('iso', 'sector','fuel')]) ){
        stop('input and outpu EFs in H3.1 apply EF pathway are not identical. Check.')
    }

    writeData( ef_pathway_applied, "MED_OUT", paste0( "H.", em, "_total_EFs_extended_adjusted-pathway" ) )

logStop()
# END
