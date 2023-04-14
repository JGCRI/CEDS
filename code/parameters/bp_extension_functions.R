# ----------------------------------------------------------------------------------
# R header file:  bp_extension_functions.R
# Authors: Rachel Hoesly
# Last Updated: April 1, 2023
# Functions contained:check_iea_bp, extend_iea_growth_detailed, extend_iea_growth_aggregate
#   mean_method, iea_to_bp_conversion_factor, bp_to_iea_trend, bp_to_iea_trend,
#   extension_correction
# Notes: Function that help with the BP extension and detailed petroleum BP extension
# ------------------------------------------------------------------------------
# Special Packages
source( '../code/parameters/IO_functions.R' )

# -----------------------------------------------------------------------------
# check_iea_bp
# Brief: check to make sure the data frames are set up identically (same order,
#        iso, sector, fuel, etc. But different trend values)
# Details: checks that the iso/sector/fuel columns are the same for both data files
#        ensures that data frame order is identical
# Dependencies: 0
# Author(s): Rachel Hoesly
# Params: iea: iea data
#         bp:bp data
# Return: none
# -----------------------------------------------------------------------------

check_iea_bp <- function(iea, bp){
# check data order

if( !identical(iea %>% select(any_of(c('iso', 'sector', 'fuel')))  ,
               bp %>% select(any_of(c('iso', 'sector', 'fuel')))   )){
    stop('iea data and BP extension data not idential. Check data frames.')
}
}

# -----------------------------------------------------------------------------
# extend_iea_growth_detailed
# Brief: Extend with Growth Method (annual growth in bp data) for detailed bp data
# Details: extends the iea data with the noormal method (growth in the bp data). If
#       limit is specified, limits the bp growth ratio
# Dependencies: 0
# Author(s): Rachel Hoesly
# Params: iea: iea data
#         bp: bp data
#         bp_growth_limit: upper growth limit for bp growth ratio
# Return: iea data with extended value

# -----------------------------------------------------------------------------
extend_iea_growth_detailed <- function(iea, bp, bp_growth_limit = NA){

    printLog("Extending IEA/iea data with bp growth")
    check_iea_bp(iea, bp)

# Extension ratios
# ratio(n) = BP(n)
# extension(n) = IEA(n-1)*BP(n)/BP(n-1)
    bp_extension_ratio <- iea %>%
        select(iso, sector, fuel)

    bp_extension_ratio[X_BP_years] <- bp[paste0('X', BP_years)]/bp[paste0('X', BP_years-1)]

    if(!is.na(bp_growth_limit)){
    bp_extension_ratio[bp_extension_ratio > bp_growth_limit] <- bp_growth_limit }

#apply ratio
    iea_extended_growth <- iea
# need to use for loop because if BP_years is more than 1, need to calculate n-1 value before n value
    for(i in seq_along(BP_years)){
        iea_extended_growth[paste0('X',BP_years[i])] <- iea_extended_growth[paste0('X',(BP_years[i]-1))]*bp_extension_ratio[paste0('X',BP_years[i])]
    }
# clean up
    iea_extended_growth <- iea_extended_growth %>%
        select(iso, sector, fuel, units, paste0('X', c(IEA_years,BP_years)))

#diagnostic_out
    writeData(bp_extension_ratio, "DIAG_OUT", 'A.BP_petroleum_detailed_extension_ratios.csv')

    return(iea_extended_growth)
}
# -----------------------------------------------------------------------------
# extend_iea_growth_aggregate
# Brief: Extend with Growth Method (annual growth in bp data) for aggregate bp data
# Details: extends the iea data with the normal method (growth in the bp data). If
#       limit is specified, limits the bp growth ratio. For aggregate bp data.
# Dependencies: 0
# Author(s): Rachel Hoesly
# Params: iea: iea data
#         bp: bp data
#         bp_growth_limit: upper growth limit for bp growth ratio
#         fuel: aggregate fue type used to write diagnostic data if specified
# Return: iea data with extended value
# -----------------------------------------------------------------------------
extend_iea_growth_aggregate <- function(iea, iea_aggregate,
                                        bp,
                                        bp_growth_limit = NA,
                                        fuel=NA){

    printLog("Extending IEA data with BP growth")
    check_iea_bp(iea_aggregate, bp)

    # Extension ratios
    # ratio(n) = BP(n)
    # extension(n) = IEA(n-1)*BP(n)/BP(n-1)
    bp_extension_ratio <- iea_aggregate %>%
            select(iso)
    ratios <- bp[paste0('X', BP_years)]/bp[paste0('X', BP_years-1)]

    if(!is.na(bp_growth_limit)){

        ratios[ratios > bp_growth_limit] <- bp_growth_limit }

    bp_extension_ratio[X_BP_years] <- ratios

    bp_extension_ratio_all <- iea %>%
        select(iso, sector, fuel) %>%
        left_join(bp_extension_ratio)

    #apply ratio
    iea_extended_growth <- iea
    # need to use for loop because if BP_years is more than 1, need to calculate n-1 value before n value
    for(i in seq_along(BP_years)){
        iea_extended_growth[paste0('X',BP_years[i])] <- iea_extended_growth[paste0('X',(BP_years[i]-1))]*bp_extension_ratio_all[paste0('X',BP_years[i])]
    }
    # clean up
    iea_extended_growth <- iea_extended_growth %>%
        select(iso, sector, fuel, units, paste0('X', c(IEA_years,BP_years)))

    #diagnostic_out
    if(is.na(fuel)){writeData(bp_extension_ratio_all, "DIAG_OUT", paste0('A.BP_detailed_petroleum_extension_ratios.csv'))}else{
    writeData(bp_extension_ratio_all, "DIAG_OUT", paste0('A.BP_',fuel,'_extension_ratios.csv'))}

    return(iea_extended_growth)
}
# -----------------------------------------------------------------------------
# mean_method
# Brief: Remove outliers, remove zero values, calculate mean of iea/bp conversion
# Details:
# remove outliers that are more than 3 st from the mean (only large outliers,
# because we only have problems with large values)
# Dependencies: 0
# Author(s): Rachel Hoesly
# Params: set of values on which to remove outliers and mean
# Return: geometric mean calculated without outliers

# -----------------------------------------------------------------------------
# Define function to create "average conversion" -
# Given a vector of values, detect and remove values, return (some kind of mean)
mean_method <- function(x){

    mean <- mean(x)
    std <- sd(x)
    # remove outliers, remove zeros
    no_outliers <- x[which(x < (mean+std*2))]
    no_outliers_zeros <- no_outliers[which(no_outliers > 0)]
    # calculate and return geometric mean
    geometric_mean <-exp(mean(log(no_outliers_zeros)))

    out <- ifelse(geometric_mean == 0, 1, geometric_mean)
    return ( out )
}

# -----------------------------------------------------------------------------
# iea_to_bp_conversion_factor
# Brief: calculate "conversion factor" to transform bp to iea "units
# Details:
# Dependencies: 0
# Author(s): Rachel Hoesly
# Params: iea: iea data
#         bp: bp data
# Return: data frame with "conversion factor" for each row in the iea/bp data

# -----------------------------------------------------------------------------
iea_to_bp_conversion_factor <- function(iea, bp){

    printLog("Creating bp to iea conversion factor")
    check_iea_bp(iea, bp)

    iea_bp_trend_conversion <- iea  %>%
        select(iso, sector, fuel, units)
    iea_bp_trend_conversion[paste0('X',(BP_first_year - 5):IEA_end_year,'.iea/bp')] <- iea[paste0('X',(BP_first_year - 5):IEA_end_year)]/
        bp[paste0('X',(BP_first_year - 5):IEA_end_year)]
# Take the geometric mean to create the "conversion factor" between bp and iea unit
    iea_bp_trend_conversion['X_average_conversion'] <- apply( iea_bp_trend_conversion[paste0('X',(BP_first_year - 5):IEA_end_year,'.iea/bp')],
                                                          1, mean_method)
# Replace NA values with 1 - when bp is zero and iea is non zerio
    iea_bp_trend_conversion['X_average_conversion'][is.na(iea_bp_trend_conversion['X_average_conversion'])] <- 1

return(iea_bp_trend_conversion)
}

# -----------------------------------------------------------------------------
# bp_to_iea_trend
# Brief: transforms bp data to iea "units
# Details: Transforms bp data to iea "units" using the "conversion factor" calculated
#        "iea_to_bp_conversion_factor" function above
# Dependencies: 0
# Author(s): Rachel Hoesly
# Params: bp: bp data
#         iea_to_bp_conversion: conversion factor data frame created with the
#               the function "iea_to_bp_conversion"
# Return: bp data in iea units

# -----------------------------------------------------------------------------

bp_to_iea_trend <- function(bp,
                            iea_to_bp_conversion){

    printLog('Converting bp trend to "iea trend units"')
    check_iea_bp(iea_to_bp_conversion, bp)

# 3.2 Create BP trend
bp_trend_iea_unit <- bp  %>%
    select(iso, sector, fuel)
bp_trend_iea_unit[paste0('X',(BP_first_year - 5):BP_last_year)] <- bp[paste0('X',(BP_first_year - 5):BP_last_year)]*
    do.call("cbind", replicate(length(paste0('X',(BP_first_year - 5):BP_last_year)), iea_to_bp_conversion['X_average_conversion'], simplify = FALSE))

return(bp_trend_iea_unit)}

# -----------------------------------------------------------------------------
# extension_correction
# Brief: Evaluates the iea trends created with the growth method and the iea conversion
#       and corrects the groth method trend where unreasonable
# Details:
# Dependencies: 0
# Author(s): Rachel Hoesly
# Params: iea: iea data
#        iea_bp_trend_conversion: iea trend with bp growth method
#        bp_trend_iea_unit: bp trend in iea units
#        iea_bp_ratio_limit: limit for the allowable difference between the two methods
#        fuel: specified fuel if applicable to write diagnostics
# Return: corrected IEA data
# -----------------------------------------------------------------------------

extension_correction <- function(iea,
                                 iea_bp_trend_conversion,
                                 bp_trend_iea_unit,
                                 iea_bp_ratio_limit,
                                 fuel=NA){ #think we can remove the NA option and the logic here

    printLog("Checking and correcting iea extension.")
    check_iea_bp(iea, bp_trend_iea_unit)
# Compare regular extension with trended BP in iea unit
# Ratio: regular growth extension/bp trend in iea unit
# compare_extensions <- BP_trend_data  %>%
#     select(iso, sector, fuel, units, Product, BP_Oil_product)
# compare_extensions[X_BP_years] <- IEA_extended_growth[X_BP_years]/
#     bp_trend_iea_unit[X_BP_years]

# Compare regular extension with trended BP in iea unit
iea_extension_correction <- iea  %>%
    select(iso, sector, fuel, units)
iea_extension_correction[paste0('X',(BP_first_year - 5):BP_last_year,'.iea_with_bp_growth')] <- iea[paste0('X',(BP_first_year - 5):BP_last_year)]
iea_extension_correction[paste0('X',(BP_first_year - 5):BP_last_year,'.bp_in_iea_units')] <- bp_trend_iea_unit[paste0('X',(BP_first_year - 5):BP_last_year)]
iea_extension_correction['X_average_conversion'] <- iea_bp_trend_conversion['X_average_conversion']
iea_extension_correction[paste0('X',(BP_first_year - 5):BP_last_year,'.comparison')] <- iea[X_BP_years]/
    bp_trend_iea_unit[X_BP_years]
iea_extension_correction[is.na(iea_extension_correction)] <- 0

# if any of the BP_years comparisons are outside the threshold, use the bp trend
iea_extension_correction["decision_flag"] <- ifelse( apply(iea_extension_correction[paste0('X',BP_years,'.comparison')] > iea_bp_ratio_limit , 1, any),
                                                     "trend","growth")
iea_extension_correction[X_BP_years] <- NA
for (i in 1:nrow(iea_extension_correction)){
    iea_extension_correction[i, X_BP_years] <- if (iea_extension_correction[i, "decision_flag"] == "growth")
    {iea_extension_correction[i, paste0('X',BP_years,'.iea_with_bp_growth')]} else{
        iea_extension_correction[i, paste0('X',BP_years,'.bp_in_iea_units')]}}

diagnostic_column_selection <- c('iso','sector','fuel','units',
             'decision_flag','X_average_conversion',
             paste0('X',(BP_first_year - 5):BP_last_year,'.iea_with_bp_growth'),
             paste0('X',(BP_first_year - 5):BP_last_year,'.bp_in_iea_units'),
             paste0('X',BP_last_year,'.comparison'))

iea_extension_correction_diagnostics <- iea_extension_correction[diagnostic_column_selection]
iea_extension_correction_diagnostics[paste0('X',(BP_first_year - 5):IEA_end_year)] <- iea_extension_correction[paste0('X',(BP_first_year - 5):IEA_end_year,'.iea_with_bp_growth')]
iea_extension_correction_diagnostics[X_BP_years]<- iea_extension_correction[X_BP_years]

if(is.na(fuel)){
writeData(iea_extension_correction_diagnostics, 'DIAG_OUT','A.BP_detailed_extension_comparison_correction.csv')}
if(!is.na(fuel)){
    writeData(iea_extension_correction_diagnostics, 'DIAG_OUT',paste0('A.BP_',fuel,'_extension_comparison_correction.csv'))}

out <- iea[c('iso', 'sector', 'fuel', 'units', X_IEA_years)]
out[X_BP_years]  <- iea_extension_correction[X_BP_years]


return(out)}
