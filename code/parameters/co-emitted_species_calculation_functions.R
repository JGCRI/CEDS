# Header ---------------------------------------------------------------------
# Program Name: co-emitted_species_calculation_functions.R
# Author's Name: Noah Prime
# Date Last Modified: October 5, 2022
# Program Purpose: Core functions for calculating co-emitted species

# Special Packages -----------------------------------------------------------


# Core Functions -------------------------------------------------------------


# ces_em -----------------------------------------------------
# Brief: Estimates the time series of a given emission species using observed
#        data from another. Uses CEDS emission factors to make conversion.
# Author: Noah Prime
# parameters:
#   - to_calc: the emission species to estimate
#   - to_use: the emission species used to estimate
#   - point_source: all the data for a given point source
#   - EF_db: CEDS emission factors data frame
#   - year_cols: list of column names for the data for each year
# input files:
# output files:
ces_em <- function( to_calc, to_use, point_source, EFs, year_cols ){

    # If process, EFs do not apply
    if(point_source$fuel[1] == 'process'){
        return( point_source %>% dplyr::slice(0) )
    }

    # Get data from point source from emission to use
    point_source <- point_source %>%
        dplyr::filter( species == to_use )

    # If there's not a single time series, return empty df
    if( nrow(point_source) != 1){
        return( point_source %>% dplyr::slice(0) )
    }

    # Column names
    base_ef_col <- paste0( to_use, '_EF' )
    calc_ef_col <- paste0( to_calc, '_EF')

    # Get relevant emission factors
    merged_data <- point_source %>%
        gather(year, base_em, all_of(year_cols) ) %>%
        dplyr::mutate( year = gsub('X', '', year) ) %>%
        left_join( EFs, by = c('iso', 'CEDS_sector' = 'sector', 'fuel', 'year' ) )

    # Perform calculation to get new time series vector
    new_em <- as.numeric(merged_data[,'base_em']) *
        as.numeric(merged_data[,calc_ef_col]) / as.numeric(merged_data[,base_ef_col])

    # Attribute columns
    attrs <- point_source %>%
        dplyr::select(-year_cols)

    # Data columns
    data_cols <- data.frame(matrix(new_em, nrow = 1))
    colnames(data_cols) <- year_cols

    # Combine attributes and data
    new_data <- cbind(attrs, data_cols)


    # Update emission species and data source
    new_data$species <- to_calc
    new_data$data_source <- paste0('Calculated from co-emitted species ', to_use)

    return(new_data)
}


# ces_any -----------------------------------------------------
# Brief: Estimates the time series of a given emission species using observed
#        data from the first available species in the input data.
#        Uses CEDS emission factors to make conversion.
# Author: Noah Prime
# parameters:
#   - to_calc: the emission species to estimate
#   - em_list: list of possible emissions, in order that they will be used
#   - point_source: all the data for a given point source
#   - EF_db: CEDS emission factors data frame
#   - year_cols: list of column names for the data for each year
# input files:
# output files:
ces_any <- function( to_calc, em_list, point_source, EFs, year_cols ){
    existing_ems <- unique( point_source$species )
    to_use <- em_list[em_list %in% existing_ems][1]
    return( ces_em( to_calc, to_use, point_source, EFs, year_cols ) )
}


# TODO: Implement this function
# ces_cap -----------------------------------------------------
# Brief: Estimates the time series of a given emission species using
#        relative capacities in the given sector/iso
# Author: Noah Prime
# parameters:
#   - to_calc: the emission species to estimate
#   - point_source: all the data for a given point source
# input files:
# output files:
ces_cap <- function( to_calc, point_source, CEDS, year_cols ){

    # Source must be a power plant
    if( point_source$CEDS_sector[1] != '1A1a_Electricity-public' ){
        return( point_source %>% dplyr::slice(0) )
    }

    # Find source in WRI database
    wri_plant_data <- gppdb %>%
        dplyr::select( iso = country, gppd_idnr, capacity_mw,
                       commissioning_year, fuel = primary_fuel ) %>%
        dplyr::filter( gppd_idnr == point_source$id[1] ) %>%
        na.omit()

    # If no match in WRI, or is missing capacity or commissioning year
    if( nrow(wri_plant_data) != 1  ){
        return( point_source %>% dplyr::slice(0) )
    }

    # Get total capacity in iso
    total_capacity <- gppdb %>%
        dplyr::filter( country == wri_plant_data$iso[1],
                       primary_fuel == wri_plant_data$fuel[1]) %>%
        dplyr::summarise( total_cap = sum(capacity_mw) ) %>%
        dplyr::pull()

    # If total capacity is zero, return empty
    if(total_capacity <= 0){
        return( point_source %>% dplyr::slice(0) )
    }

    # Proportion of capacity from given source
    proportion <- wri_plant_data$capacity_mw / total_capacity

    # Proportion should be on 0-1
    if(proportion > 1 | proportion < 0){
        return( point_source %>% dplyr::slice(0) )
    }

    # Get CEDS emissions for given
    to_calc_col <- paste0( to_calc, '_CEDS' )
    emissions <- CEDS[to_calc_col]

    # Multiply emissions by proportion to get new time series
    new_ems <- emissions * proportion
    colnames(new_ems) <- 'value'

    # Set ems to zero before commissioning year, and spread data to wide
    new_ems$year <- CEDS$year
    data_cols <- new_ems %>%
        dplyr::mutate( value = value * (year >= wri_plant_data$commissioning_year) ) %>%
        dplyr::mutate( year = paste0('X', year) ) %>%
        tidyr::spread(year, value)

    # Get attribute cols
    attr_cols <- point_source %>%
        dplyr::select(-year_cols)
    attr_cols$data_source <- 'Calculated from CEDS inventory in proportion to capacity in WRI\'s GPPDB'
    attr_cols$species <- to_calc


    # Bind columns
    new_data <- cbind(attr_cols, data_cols)

    return( new_data )
}


# Other Functions ------------------------------------------------------------

# replace_data ------------------------------------------------------------
# Brief: According to the replacement method, returns time series of the correct
#        data source.
# Dependencies:
# Author: Noah Prime
# parameters:
#   - attrs - attribute columns from the preferred data
#   - preferred_data_em - data from the data source that is preferred (emission specific)
#   - replaceable_data_em - data from the data source that is replaceable (emission specific)
#   - replace_method - says wither to simply replace, add, splice or other method
# input files:
# output files:
replace_data <- function(attrs, preferred_data_em, replaceable_data_em, replace_method){

    # Different methods for different replacement

    # If simply replace the data
    if(replace_method == 'replace'){

        # Just return the preferred data
        return(preferred_data_em)

    # If adding the data
    }else if(replace_method == 'sum'){

        # Add data columns
        data_cols <- as.numeric(preferred_data_em[paste0('X',1750:2019)]) +
            as.numeric( replaceable_data_em[paste0('X',1750:2019)] )
        data_cols <- as.data.frame(matrix(data_cols,nrow=1))
        colnames(data_cols) <- paste0('X',1750:2019)

        # Append to attribute columns
        new_data <- cbind(attrs, data_cols)

        # Update data source
        new_data$description <- paste0( preferred_data$data_source[1],
                                        '+',
                                        replaceable_data$data_source[1] )


    # If splicing the data together
    }else if(grepl('splice', replace_method)){

        # Year to do the splice
        splice_year <- as.numeric(gsub('splice', '', replace_method))

        # If the year after before the splice, start with preferred data
        if(substr(replace_method,1,1) == 's'){

            # Early year data columns come from 'preferred' data
            early_data_cols <- preferred_data %>%
                dplyr::select( paste0('X', 1750:splice_year) )

            # Later year data columns come from 'replaceable' data
            late_data_cols <- replaceable_data %>%
                dplyr::select( paste0('X', (splice_year+1):2019) )

            # Merge all columns together
            new_data <- cbind(attrs, early_data_cols, late_data_cols)

            # Update the data source
            new_data$description <- paste0(preferred_data$data_source[1],
                                           ' spliced ',
                                           replaceable_data$data_source[1],
                                           ' ',
                                           splice_year )


        # If year before after 'splice' start with replaceable data
        }else{

            # Early year data columns come from 'preferred' data
            early_data_cols <- replaceable_data %>%
                dplyr::select( paste0('X', 1750:splice_year) )

            # Later year data columns come from 'replaceable' data
            late_data_cols <- preferred_data %>%
                dplyr::select( paste0('X', (splice_year+1):2019) )

            # Merge all columns together
            new_data <- cbind(attrs, early_data_cols, late_data_cols)

            # Update the data source
            new_data$description <- paste0(replaceable_data$data_source[1],
                                           ' spliced ',
                                           preferred_data$data_source[1],
                                           ' ',
                                           splice_year )

        }

    }

    # Description
    new_data$data_source <- preferred_data$data_source[1]

    # Return
    return(new_data)
}


# unique_entries ------------------------------------------------------------
# Brief: Returns a single time series entry for the given species from the input
#        preferred data source, or if unavailable, the replaceable data source,
#        or if otherwise specified, a special method like splicing or adding,...
# Dependencies:
# Author: Noah Prime
# parameters:
#   - em - emission species
#   - preferred_data - data from the data source that is preferred
#   - replaceable_data - data from the data source that is replaceable
#   - replace_method - says wither to simply replace, add, splice or other method
# input files:
# output files:
unique_entry <- function(em, preferred_data, replaceable_data, replace_method){

    # Attributes from the preferred input source
    attrs <- preferred_data %>%
        dplyr::select( id:build_year ) %>%
        dplyr::slice(1)

    # Substitute Correct Species
    attrs$species <- em

    # Data for given species in the preferred data
    preferred_data_em <- preferred_data %>%
        dplyr::filter(species == em)

    # Data for given species in the replaceable data
    replaceable_data_em <- replaceable_data %>%
        dplyr::filter(species == em)

    # Check that don't have more than one time series for a given point source,
    # from a single input data source
    if(nrow(preferred_data_em) > 1 | nrow(replaceable_data_em) > 1){
        id_pref <- preferred_data_em$id[1]
        id_rep <- replaceable_data_em$id[1]
        stop( paste0( 'Check point source ', id_pref, ' (also knwon as ', id_rep,
                      ') for multiple time series from same input source. Emissions species: ',
                      em) )
    }

    # If data exists for given species in both sources
    if(nrow(preferred_data_em) > 0 & nrow(replaceable_data_em) > 0){
        # Do replace method
        new_entry <- replace_data(attrs, preferred_data_em, replaceable_data_em, replace_method)

        # If the data only exists in the 'replaceable' source, keep that data
        # but update the attributes
    }else if(nrow(replaceable_data_em) > 0){
        # Get time series, and source info
        data_columns <- replaceable_data_em %>%
            dplyr::select( -id:build_year )
        # Combine with common attributes
        new_entry <- cbind(attrs, data_columns)

        # Now if only the preferred data exists, just keep that entry
    }else{
        new_entry <- preferred_data_em
    }

    # Return data frame with single row
    return(new_entry)
}


# reconcile_identical_sources -----------------------------------------------------
# Brief: Estimates the time series of a given emission species using observed
#        data from another. Uses CEDS emission factors to make conversion.
# Dependinces:
# Author: Noah Prime
# parameters:
#   - preferred_data - data from the preferred source
#   - replaceable_data - data from source which is replaceable but may be used if pref not available
#   - replace_method - whether to simply replace, add, splice, or other
# input files:
# output files:
reconcile_identical_sources <- function(preferred_data, replaceable_data, replace_method){

    # Every emission which we have data for we iterate through
    em_list <- unique( c(preferred_data$species, replaceable_data$species) )
    entries <- lapply(em_list, unique_entry, preferred_data, replaceable_data, replace_method)
    new_df <- do.call(rbind, entries)

    return(new_df)

}

