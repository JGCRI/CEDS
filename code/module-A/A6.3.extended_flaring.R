#------------------------------------------------------------------------------
# Program Name: A6.3.extended_flaring.R
# Authors Names: Hamza Ahsan
# Date Last Modified: 27 March 2024
# Program Purpose: Generates a composite time series of flaring volume using WB
#                  (2012-2022) and EI (1975-2011) data and extends back to 1800 using
#                  flaring intensity (based on average flaring and oil production
#                  for 1975-1977).
# Input Files: World_Bank_Gas_Flaring_Data_2012-2022.xlsx,
#              Statistical_Review_of_World_Energy_2023.xlsx, Master_Country_List.csv,
#              Master_Fuel_Sector_List.xlsx, A.crude_oil_production_data.csv
# Output Files: A.extended_flaring.csv

#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c("data_functions.R", "bp_extension_functions.R")
log_msg <- "Flaring extension from EI and World Bank data" # First message to be printed to the log
script_name <- "A6.5.extended_flaring.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1. Read in files

MCL <- readData( "MAPPINGS", "Master_Country_List" )

# Read in World Bank flaring volume data
printLog( c("Reading in World Bank flaring volume data."))
WB_data <- readData( "ENERGY_IN", "World_Bank_Gas_Flaring_Data_2012-2022", ".xlsx")

# Read in oil production data
printLog( c("Reading in oil production data."))
oil_production <- readData( "MED_OUT" , 'A.crude_oil_production_data' )

# Read in EI energy data
printLog( c("Reading in EI flaring volume data."))
EI_energy_data <- readData( "ENERGY_IN", BP_data_file_name, ".xlsx",  skip = 2)

# ------------------------------------------------------------------------------
# 2. Process input flaring data

printLog( c("Generating composite timeseries of flare gas volume using WB and EI data."))
# Use World Bank data back to 2012 and EI data before that to 1975

# World Bank flaring data
WB_flaring <- WB_data[[ 'Flaring Volume' ]] %>%
    rename_at(vars(as.character(2012:BP_last_year)), ~ paste0('X',2012:BP_last_year)) %>%
    mutate(WB_flaring = `Country, mcm`) %>%  # This could be more robust
    select(WB_flaring, paste0('X',2012:BP_last_year)) %>%
    filter(!is.na(X2012)) %>%
    left_join(MCL %>% select(iso, WB_flaring) %>% unique) %>%
    select(iso, paste0('X',2012:BP_last_year)) %>%
    filter(!is.na(iso))

# Convert World Bank flaring to billion cubic meters
divide.by.1000 <- function(x, na.rm=FALSE) (x/1000)
WB_flaring <- mutate_at(WB_flaring, paste0('X',2012:BP_last_year), divide.by.1000)

# EI (previously BP) flaring data
EI_flaring <- EI_energy_data[[ getBPSheetNumber( "natural", "gas", "flaring", EI_energy_data ) ]] %>%
    rename_at(vars(as.character(1975:BP_last_year)), ~ paste0('X',1975:BP_last_year)) %>%
    mutate(BPName_flaring = `Billion cubic metres`) %>%  # This could be more robust
    select(BPName_flaring, paste0('X',1975:BP_last_year)) %>%
    filter(!is.na(X1975)) %>%
    left_join(MCL %>% select(iso, BPName_flaring) %>% unique) %>%
    select(iso, BPName_flaring, paste0('X',1975:2011)) %>%
    filter(!is.na(iso))

# Filter the Other country categories
EI_flaring_other <- filter(EI_flaring, grepl('Other', BPName_flaring))

EI_flaring_other_long <- tidyr::gather(EI_flaring_other, key="year", value="total_volume", "X1975":"X2011")%>%
    select(-iso) %>%
    distinct()

# ------------------------------------------------------------------------------
# 3. Downscale EI flaring ("Other" country categories) using oil production

# Downscale the aggregate EI flaring volume to the individual counties in proportion to their oil production
# OtherAfrica country i, flaring(i) = flaring (OtherAfrica) * oil_Production(country_i)/oil_Production(OtherAfrica)
EI_flaring_oil <- left_join(EI_flaring_other %>%
                                select(c("iso", "BPName_flaring")),
                            oil_production %>%
                                select(c("iso", paste0('X',1975:2011)))) %>%
    replace(is.na(.), 0) %>%
    tidyr::gather(key="year", value="production", "X1975":"X2011")

oil_production_total <- EI_flaring_oil %>%
    select(-iso) %>%
    group_by(BPName_flaring, year) %>%
    dplyr::summarise(total_production = sum(production))

oil_production_fraction <- left_join(EI_flaring_oil, oil_production_total) %>%
    mutate(fraction = production/total_production) %>%
    select(-c("production", "total_production"))

EI_flaring_long <- left_join(EI_flaring_other_long, oil_production_fraction) %>%
    mutate(volume = total_volume * fraction) %>%
    select(-c("total_volume", "fraction"))

EI_flaring_final <- EI_flaring_long %>% spread(key=year, value=volume) %>%
    full_join(EI_flaring %>% filter(!grepl('Other', BPName_flaring))) %>%
    select(-BPName_flaring)

# Combine EI and WB flaring data
flaring_final <- full_join(EI_flaring_final, WB_flaring) %>%
    replace(is.na(.), 0)

# ------------------------------------------------------------------------------
# 4. Extend flaring back to 1800 using oil production

# Extend flaring data back to 1800 (from 1974) using average flaring volume and oil production
# of last three flaring years (1975, 1976, 1977) to get an average flaring intensity
# for each country

flaring_avg <- flaring_final %>%
    select(c("iso", paste0("X", 1975:1977))) %>%
    mutate(flaring_avg = rowMeans(cbind(X1975, X1976, X1977), na.rm=T)) %>%
    select(c("iso", "flaring_avg"))

oil_prod_avg <- oil_production %>%
    select(c("iso", paste0("X", 1975:1977))) %>%
    mutate(oil_avg = rowMeans(cbind(X1975, X1976, X1977), na.rm=T)) %>%
    select(c("iso", "oil_avg"))

flaring_intensity <- left_join(flaring_avg, oil_prod_avg) %>%
    mutate(FI = flaring_avg/oil_avg) %>%
    select(c("iso", "FI")) %>%
    replace(is.na(.), 0)

flaring_extended_inital <- left_join(oil_production, flaring_intensity) %>%
    select(c("iso", "FI", paste0("X", 1800:1974))) %>%
    mutate_at(vars(starts_with("X")), ~.*FI) %>%
    select(-"FI") %>%
    left_join(flaring_final) %>%
    mutate(activity = "flaring_volume") %>%
    mutate(units = "bcm") %>%
    select(c("iso", "activity", "units", paste0("X", 1800:BP_last_year)))

# ------------------------------------------------------------------------------
# 5. Update flaring extension to address wide range in flaring intensity in World Bank data

# Calculate flaring intensity (FI = flaring volume over oil production)
flaring_long <- flaring_extended_inital %>%
    dplyr::select(c(iso, X1800:X2022 )) %>%
    tidyr::gather(key="year", value="volume", "X1800":"X2022")

oil_prod_long <- oil_production %>%
    dplyr::select(c(iso, X1800:X2022 )) %>%
    tidyr::gather(key="year", value="volume", "X1800":"X2022")

FI <- cbind(flaring_long[1:2],flaring_long$volume/oil_prod_long$volume)

FI <- FI %>%
    dplyr::rename(FI = names(FI)[3])

FI_wide <- FI %>%
    spread(key=year, value=FI)

FI_wide[] <- lapply(FI_wide, function(x) {
    replace(x, is.na(x) | is.infinite(x), 0)
})

# Calculate the median of all the non-zero FI values from the WB data time period for all WB years
FI_WB_list <- as.vector(as.matrix(FI_wide[,paste0("X", 2012:2022)]))

FI_WB_median <- median(FI_WB_list[FI_WB_list>0])

# Calculate the ratio of FI to this median FI for all the WB data points

FI_WB_ratio <- cbind(FI_wide[1], FI_wide[, paste0("X", 2012:2022)]/FI_WB_median)

# For all values of FI > 10 * median FI replace FI with a new value for the world bank time period
# a) If FI > 10 * median for all WB years, replace with median value
# b) If FI > 10 * median for just some WB years, replace with max value that isn’t > 10 * median for that country
FI_threshold <- 10

FI_WB_ratio_long <- FI_WB_ratio %>%
    tidyr::gather(key="year", value="FI_ratio", "X2012":"X2022") %>%
    dplyr::mutate(FI_new_a = dplyr::case_when(FI_ratio > FI_threshold ~ FI_WB_median, TRUE ~ FI_ratio)) %>% # part a)
    dplyr::mutate(FI_new_b = dplyr::case_when(FI_ratio > FI_threshold ~ NA_real_, TRUE ~ FI_ratio)) %>% # part b)
    group_by(iso) %>%
    dplyr::mutate(FI_new_c = dplyr::if_else(is.na(FI_new_b), max(FI_new_b, na.rm = TRUE), FI_new_b)) %>%
    dplyr::mutate(FI_new_c = FI_new_c * FI_WB_median) %>% # convert from FI ratio to FI
    dplyr::mutate(FI = dplyr::case_when(sum(FI_new_a) == 11*FI_WB_median ~ FI_WB_median, TRUE ~ FI_new_c)) %>%
    dplyr::ungroup() %>%
    dplyr::select(iso, year, FI)

FI_WB_new <- FI_WB_ratio_long %>%
    spread(key=year, value=FI)


# Replace original FI with updated WB FI

FI_new <- dplyr::left_join(FI_wide[c("iso", paste0("X", 1800:2011))], FI_WB_new, by = "iso")

# If FI is zero for a country (in either WB or earlier data years) do one of the following
# a) If there are some non-zero values in the FI time series, replace zeros with average value for that country
# b) If there are no flaring data for a country, conservatively replace with 1/2 of WB median value. (Except for Estonia - their oil production is oil sands, no flaring has been detected there nor do we necessarily expect it. Leave that as zero.)

FI_new_long <- FI_new %>%
    tidyr::gather(key="year", value="FI", "X1800":"X2022") %>%
    dplyr::group_by(iso) %>%
    dplyr::mutate(FI_avg = mean(FI)) %>%
    dplyr::mutate(FI_a = dplyr::case_when(sum(FI) == 0 ~ 0.5*FI_WB_median, TRUE ~ FI)) %>%
    dplyr::mutate(FI_b = dplyr::case_when(FI == 0 ~ FI_avg, TRUE ~ FI)) %>%
    dplyr::mutate(FI_c = dplyr::case_when(FI == 0 ~ FI_a + FI_b, TRUE ~ FI_b)) %>%
    dplyr::ungroup() %>%
    dplyr::select(iso, year, FI_c) %>%
    dplyr::rename("FI" = "FI_c")

# Set Estonia FI to zero
FI_new_long$FI[FI_new_long$iso == "est"] <- 0

# Re-calculate flaring volume using the adjusted FI * oil production
FI_final <- FI_new_long %>%
    tidyr::spread(key=year, value=FI)

oil_production_final <- oil_production %>%
    dplyr::select(-c(activity, units))

x <- as.matrix(FI_final[-1])
y <- as.matrix(oil_production_final[-1])

flaring_extended <- cbind(FI_final[1], as.data.frame(x * y)) %>%
    mutate(activity = "flaring_volume") %>%
    mutate(units = "bcm") %>%
    select(c("iso", "activity", "units", paste0("X", 1800:BP_last_year)))


# ------------------------------------------------------------------------------
# 6. Output


# write extended flaring data
writeData( flaring_extended , "MED_OUT", "A.extended_flaring" )

writeData( FI_final , "DIAG_OUT", "A.flaring_intensity" )

logStop()
