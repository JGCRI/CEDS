# Header ---------------------------------------------------------------------
# Program Name: point_source_util_functions.R
# Author's Name: Noah Prime
# Date Last Modified: October 20, 2022
# Program Purpose: Common functions used in the point source gridding process

# Special Packages -----------------------------------------------------------
library( 'yaml' )


# write_yml_by_em -----------------------------------------------------
# Brief: Creates a YAML file for the given point source for each species
#        time series available for that source (one file per species)
# Dependencies:
# Author: Noah Prime
# parameters:
#   - full_source: data frame for a unique source with all it's available data
#   - base_yml_dir: directory which contains sub-directories for each emission
#                   species to save final YAML file in
# input files:
# output files: yml_dir/yml_file.yml
write_yml_by_em <- function(full_source, base_yml_dir){

    # Get list of emission species to write out
    em_avail <- unique( full_source$species )

    # If time series is zero (can happen for multiple reasons)
    # return without saving
    tot_ems <- sum(full_source[paste0('X',1750:2019)])
    if(!(tot_ems > 0)){
        return()
    }

    # Iterate through species and write out YAML files for the selected data
    lapply(em_avail, function(em){
        em_source <- full_source %>%
            dplyr::filter(species == em)
        write_yml_all_ems(em_source, yml_dir = paste0(base_yml_dir, '/', em) )
    })
}


# write_yml_all_ems -----------------------------------------------------
# Brief: Creates a YAML file for the given point source that contains a
#        time series for each emission species available for that source
# Dependencies:
# Author: Noah Prime
# parameters:
#   - full_source: data frame for a unique source with all it's available data
#   - yml_file: name of the output YAML file
#   - yml_dir: directory to save file
# input files:
# output files: yml_dir/yml_file.yml
write_yml_all_ems <- function(full_source, id = '', yml_dir){

    # If no id explicitly given, look in df
    if(id == ''){
        id = full_source$id[1]
    }

    # Turn to long format
    full_source_long <- full_source %>%
        gather(year, value, paste0('X', 1750:2019) ) %>%
        dplyr::mutate( year = gsub('X', '', year) )

    # File name set to source ID
    yml_file <- id

    # Header of the yml file
    header <- list( description = full_source$description[1],
                    date = as.character(Sys.Date()) ) %>%
        list(documentation = .)
    # As yml object
    part1 <- as.yaml(header, indent.mapping.sequence=TRUE,line.sep = "\n")

    # Emissions in the data
    unique_ems <- full_source %>%
        dplyr::distinct(species) %>%
        pull()

    # Getting time series for each emission and data_source for that time series
    time_series <- list()
    for(em in unique_ems){
        spec_df <- full_source_long %>%
            dplyr::select(species, year, value, data_source) %>%
            dplyr::filter(species == em)

        # Create list of the time series values from the filtered data frame
        time_series_em <- setNames(as.list(spec_df$value), spec_df$year)

        # Put into time series list
        time_series[[em]][['values']] <- time_series_em

        # data_source list
        time_series[[em]][['data_source']] <- spec_df$data_source[1]
    }

    # Put in time series section
    time_series_nested <- time_series %>%
        list(time_series = .)

    # Convert to yaml string
    part3 <- as.yaml(time_series_nested, indent.mapping.sequence = TRUE )

    # List of attributes
    atr <- list(id = id,
                name = full_source$name[1],
                location = full_source$location[1],
                longitude = full_source$longitude[1],
                latitude = full_source$latitude[1],
                units = full_source$units[1],
                CEDS_sector = full_source$CEDS_sector[1],
                EDGAR_sector = full_source$EDGAR_sector[1],
                fuel = full_source$fuel[1],
                iso = full_source$iso[1],
                build_year = as.integer(full_source$build_year[1]))

    # Put in attributes section
    atr_nested <- atr %>%
        list(attributes = .)

    # Convert to yaml string
    part2 <- as.yaml(atr_nested, indent.mapping.sequence = TRUE, line.sep = '\n' )

    # write out yml file
    write( c(part1,part2,part3),
           paste0(yml_dir, '/', yml_file, '.yml') )


}



# read_yml_all_ems -----------------------------------------------------
# Brief: Creates a data frame from a YAML file for the given point source
#        which includes any to all of the CEDS supported emissions species
# Dependencies:
# Author: Noah Prime
# parameters:
#   - yml_file: name of the output YAML file
#   - yml_dir: directory to save file
# returns:
#   - source_df: data frame with rows being the time series of each species
# input files: yml_dir/yml_file.yml
# output files:
read_yml_all_ems <- function(yml_file, yml_dir){
    # Reads in yml file as a list
    yml_list <- read_yaml( paste0( yml_dir, '/', yml_file ) )

    # Every entry in the yml file on one row
    super_wide_df <- cbind( data.frame(yml_list$attributes),
                            data.frame(yml_list$documentation),
                            data.frame(yml_list$time_series) )

    # Columns that provide data source for the given time series
    ref_cols <- grep('data_source', colnames(super_wide_df))
    references <- super_wide_df[ref_cols] %>%
        dplyr::mutate_all(as.character) %>%
        gather(key, value) %>%
        dplyr::mutate( species = gsub( '.data_source', '', key) ) %>%
        dplyr::select(species, data_source = value )

    # Columns that contain the time series
    ts_cols <- grep('values', colnames(super_wide_df[-ref_cols]))

    # Long format data
    source_df <- super_wide_df[-ref_cols] %>%
        dplyr::mutate_all(as.character) %>%
        gather(key, value, all_of(ts_cols)) %>%
        dplyr::mutate( year = gsub('.*.values.', '', key),
                       species = gsub( '.values.*', '', key) ) %>%
        dplyr::select( -key ) %>%
        dplyr::left_join( references, by = 'species' ) %>%
        dplyr::mutate( year = paste0('X', year) ) %>%
        dplyr::mutate( value = as.numeric(value) ) %>%
        tidyr::spread( year, value )

    return( source_df )
}



# ------------------------------------------------------------------------------
# readInRawYml
# Brief: Returns the coordinates at the center of input cell
# Dependencies:
# Author: Noah Prime
# parameters:
#   - target_filenames - list of file paths to the input yml files
# return:
#   - ps_df - dataframe of manual input sources in particular form
# input files: Null
# output files: Null
readInRawYml <- function( target_filenames ){
    # Read in ymls
    yml_input <- lapply(target_filenames, read_yaml)

    # Generate list of sources for each point source
    time_series_list <- lapply(yml_input, function(x) names(x[["attributes"]][["time series"]]))

    # Initialize data frame that will contain data for all point sources
    ps_df <- data.frame(description=character(),
                        data_source=character(),
                        id=character(),
                        name=character(),
                        location=character(),
                        longitude=double(),
                        latitude=double(),
                        species=character(),
                        units=character(),
                        CEDS_sector=character(),
                        EDGAR_sector=character(),
                        fuel=character(),
                        iso=character(),
                        build_year=double(),
                        stringsAsFactors=FALSE)

    # Fill ps_df
    for(i in 1:length(yml_input)) {
        comb_list <- list()
        for (j in 1:length(time_series_list[[i]])) {
            comb_list[[j]] <- bind_rows(yml_input[[i]][["attributes"]][["time series"]][[time_series_list[[i]][j]]][["values"]])
        }
        comb_df <- bind_cols(comb_list)

        # Append an X to all column names
        comb_df <- comb_df %>% rename_all( ~ paste0("X", .x))

        # Compute missing years and set to NA
        missing_yrs <- setdiff(X_extended_years, names(comb_df))
        comb_df[,missing_yrs] <- NA

        # Reorder columns from earliest year to most recent
        comb_df <- comb_df[c(sort(names(comb_df)))]

        # Interpolate missing years
        comb_df <- interpolateValues(comb_df)

        # Add columns for location, iso, sector, em species
        comb_df$description <- yml_input[[i]][["documentation"]][["description"]]
        comb_df$data_source <- yml_input[[i]][["documentation"]][["data_source"]]
        comb_df$id <- yml_input[[i]][["attributes"]][["id"]]
        comb_df$name <- yml_input[[i]][["attributes"]][["name"]]
        comb_df$location <- yml_input[[i]][["attributes"]][["location"]]
        comb_df$longitude <- yml_input[[i]][["attributes"]][["longitude"]]
        comb_df$latitude <- yml_input[[i]][["attributes"]][["latitude"]]
        comb_df$species <- yml_input[[i]][["attributes"]][["emissions species"]]
        comb_df$units <- yml_input[[i]][["attributes"]][["units"]]
        comb_df$CEDS_sector <- yml_input[[i]][["attributes"]][["CEDS_sector"]]
        comb_df$EDGAR_sector <- yml_input[[i]][["attributes"]][["EDGAR_sector"]]
        comb_df$fuel <- yml_input[[i]][["attributes"]][["fuel"]]
        comb_df$iso <- yml_input[[i]][["attributes"]][["iso"]]
        comb_df$build_year <- yml_input[[i]][["attributes"]][["build date"]]

        ps_df <- bind_rows(ps_df, comb_df)
    }

    # Extend backwards and forwards to fill in missing years
    ps_df <- extend_and_interpolate(ps_df, X_extended_years)

    return(ps_df)
}


# ------------------------------------------------------------------------------
# readInOmiYml
# Brief: Returns the coordinates at the center of input cell
# Dependencies:
# Author: Noah Prime
# parameters:
#   - target_filenames - list of file paths to the input yml files
# return:
#   - OMI_sources_df - dataframe of OMI sources in particular form
# input files: Null
# output files: Null
readInOmiYml <- function( target_filenames ){

    # OMI input
    OMI_input <- lapply(target_filenames, read_yaml)

    # As data frame
    OMI_sources_df <- do.call( bind_rows,
                               lapply( OMI_input, function( x ){
                                   df.a <- as.data.frame( x$attributes )
                                   colnames( df.a ) <- gsub( 'value.', 'X', colnames( df.a ) )
                                   df.b <- as.data.frame( x$documentation )
                                   df <- cbind( df.a, df.b )
                                   df <- df %>%
                                       dplyr::select( description, data_source, id, name, emission,
                                                      CEDS_sector, EDGAR_sector,
                                                      fuel, iso, units, location,
                                                      latitude, longitude,
                                                      build_year, X1750:X2019 ) %>%
                                       dplyr::rename( 'species' = 'emission' )
                                   return( df )
                               } ) )

    return(OMI_sources_df)
}
