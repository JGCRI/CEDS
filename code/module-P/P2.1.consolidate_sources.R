# Header ---------------------------------------------------------------------
# Program Name: P2.1.consolidate_sources.R
# Author: Noah Prime
# Date Last Updated: October 4, 2022
# Program Purpose:  Identify identical point sources from different input data
#                   and consolidate. Note that point sources should be already
#                   processed for every emission species by the time this script
#                   is run
# Input Files:  YAML files from each input data source
#               Equivalency Mapping
#               Preference Mapping
# Output Files: YAML files for each unique point source


# 0. Read in global settings and headers ------------------------------------
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"
source( paste0( PARAM_DIR, "header.R" ) )

initialize(script_name = "P2.1.consolidate_sources.R",
           log_msg = "Consolidating duplicate sources",
           headers = c("data_functions.R",
                       "interpolation_extension_functions.R",
                       "co-emitted_species_calculation_functions.R",
                       "point_source_util_functions.R"),
           common_data = TRUE)

# Needed in case netcdf package is not available
library("geosphere")

# 0.1 Set paths -------------------------------------------------------

raw_yaml_path <- filePath( domain = 'GRIDDING',
                           domain_extension = 'point-source/raw_point_source',
                           fn = '',
                           extension = '' )
omi_yaml_path <- filePath( domain = 'MED_OUT',
                           domain_extension = 'OMI_yml',
                           fn = '',
                           extension = '' )

# 0.2 Read in mapping files ------------------------------------------
ps_equiv_mapping <- readData(domain = 'GRIDDING',
                             domain_extension = 'gridding_mappings/',
                             file_name = 'point_source_equivalency',
                             meta = FALSE)


# 1. Read in all sources ---------------------------------------------

# 1.1 Read in raw yaml -----------------------------------------------
raw_files <- list.files(raw_yaml_path, '*.yml', recursive = TRUE, full.names = TRUE)
raw_data <- readInRawYml(raw_files)

# 1.1 Read in OMI ----------------------------------------------------
omi_files <- list.files(omi_yaml_path, '*.yml', full.names = TRUE)
omi_data <- readInOmiYml(omi_files)

# TODO: When adding new sources just add, 1.X Read in <input source>

# Combine sources
all_sources <- rbind(raw_data, omi_data)


# 2. Replace known matches -------------------------------------------

# Iterate through matches
for(i in 1:nrow(ps_equiv_mapping)){

    # Getting preferred data source
    preferred_data <- all_sources %>%
        dplyr::right_join( ps_equiv_mapping[i,],
                           by = c( 'id' = 'prefer_id',
                                   'data_source' = 'prefer_source' ) ) %>%
        dplyr::select(-c(replace_id, replace_source, method))

    # Getting data to replace
    replaceable_data <- all_sources %>%
        dplyr::right_join( ps_equiv_mapping[i,],
                           by = c( 'id' = 'replace_id',
                                   'data_source' = 'replace_source' ) ) %>%
        dplyr::select(-c(prefer_id, prefer_source, method))

    # get new entries
    new_entries <- reconcile_identical_sources(preferred_data, replaceable_data, ps_equiv_mapping$method[i])

    # Remove old entries from full data
    all_sources <- setdiff( all_sources, rbind(preferred_data, replaceable_data) )

    # bind new entries with full data
    all_sources <- rbind(all_sources, new_entries)
}


# 3. Replace same IDs -----------------------------------------------

# Default preference order
input_source_default_pref <- c('raw', 'omi')

# TODO: Log warning that time series is being dropped for default preference
all_sources %>%
    dplyr::group_by(id, species) %>%
    dplyr::summarise( number_of_input_sources = n() ) %>%
    dplyr::arrange( desc(number_of_input_sources) ) %>%
    dplyr::filter( number_of_input_sources > 1 ) %>%
    ungroup() %>%
    writeData( domain = 'DIAG_OUT', fn = 'P.unhandled_duplicate_IDs' )

# Now that we've dealt with pre-fixed matches, look for any duplicates of
# ID and species, and use default preference order for which to keep.
all_sources <- all_sources %>%
    dplyr::group_by(id, species) %>%
    dplyr::arrange( match(data_source, input_source_default_pref) ) %>%
    dplyr::top_n(1, data_source) %>%
    dplyr::ungroup()


# 4. Check for possible conflicts -----------------------------------

# Function that returns pairs of point source ID's that are within 10km of each other
is_close <- function(sources){
    # Get each id, lat, lon
    ids <- sources$id
    lats <- as.numeric(sources$latitude)
    lons <- as.numeric(sources$longitude)

    # Get each combination of each
    expanded_ids <- expand.grid(ids, ids)
    expanded_lats <- expand.grid(lats, lats)
    expanded_lons <- expand.grid(lons, lons)
    expanded <- cbind(expanded_ids, expanded_lats, expanded_lons)

    # Apply distance function over combinations
    dists <- apply(expanded, 1, function(x){
        d <- distm( as.numeric(x[c(5,3)]),
                    as.numeric(x[c(6,4)]),
                    fun = distVincentyEllipsoid ) / 1000

        return(d)

    })

    # Pairs where distance is less than 10km
    possible_indexes <- which(unlist(dists) < 10)

    # Remove pairs that are just the same ID repeated
    possible_ids <- expanded_ids[possible_indexes,]
    possible_ids <- possible_ids[which(possible_ids[,1] != possible_ids[,2]),]
    possible_indexes <- as.numeric(row.names((possible_ids)))

    # Get remaining pairs
    possible_conflicts <- cbind( expanded_ids[possible_indexes,],
                                 expanded_lats[possible_indexes,],
                                 expanded_lons[possible_indexes,] )

    # Column names
    colnames(possible_conflicts) <- c('id.1', 'id.2',
                                      'lat.1', 'lat.2',
                                      'lon.1', 'lon.2')

    # Remove duplicates
    possible_conflicts <- possible_conflicts %>%
        # Ensuring ID's are character strings so that we can use <, > to infer
        # alphabetical order
        dplyr::mutate( id.1 = as.character(id.1), id.2 = as.character(id.2) ) %>%
        # Putting ID's in alphabetical ordered columns
        dplyr::mutate( first_id = ifelse(id.1 < id.2, id.1, id.2),
                       second_id = ifelse(id.1 > id.2, id.2, id.1) ) %>%
        # Then we can use distinct on the sorted columns to remove duplicate
        # ID pairs
        dplyr::distinct(first_id, second_id, .keep_all = TRUE) %>%
        dplyr::select(-first_id, -second_id)


    # Return
    return(possible_conflicts)
}

# TODO: Write out found pairs as diagnostic
# TODO: Create file for pairs which aren't actually matches
# TODO: Perhaps think about how to run this faster, or to not run unless
#       prompted.

#       Now that we've dealt with known matches, let's look through the data
#       by:
#           - sector
#           - species
#           - location
#       and find potential un-dealt-with matches. Then we can use default
#       preferences to reconcile these, or just assume they are truly different.
#       Probably actually just log these as potential conflicts that the user
#       should specify in the equiv file.

start <- Sys.time()
test <- all_sources %>%
    dplyr::group_by(species, CEDS_sector, iso, fuel) %>%
    dplyr::group_map(~is_close(.x))
end <- Sys.time()
end - start


# 5. Write YML files -----------------------------------------------
output_dir <- filePath( domain = 'MED_OUT',
                        domain_extension = 'point_source_yml',
                        extension = '',
                        fn = '' )

# Write out the individual files into intermediate directory
all_sources %>%
    dplyr::group_by(id) %>%
    dplyr::group_walk( ~ write_yml_all_ems(.x, id = .y$id, yml_dir = output_dir) )


# End --------------------------------------------------------------

logStop()









