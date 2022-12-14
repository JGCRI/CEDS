# ------------------------------------------------------------------------------
# Program Name: A3.5.proc_sintering.R
# Author: Andrea Mott
# Date Last Updated: 8 November 2021
# Program Purpose: Process sintering emissions based on pig iron production data
#                   for the iron-steel sector.
# Input Files:  A.Pig_Iron_Production_full.csv
# Output Files: A.Sintering_Production.csv
# TODO:

# -----------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "common_data.R", "data_functions.R", "analysis_functions.R",
                  "process_db_functions.R", "timeframe_functions.R",
                  "interpolation_extension_functions.R" ) # Additional function files may be required.
    log_msg <- "Process pig iron production"
    script_name <- "A3.5.proc_sintering.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# ---------------------------------------------------------------------------
# 1. Read input

# input pig iron data
    pig_iron_data <- readData( "DIAG_OUT", "A.Pig_Iron_Production_full", meta = F )

# input sintering % iron for regions based on XX.
    # this is the percent of input into the blast furnace that is sinter.
    sintering_percent <- readData( "ACTIVITY_IN", "percent_sintering", ".xlsx",
              sheet_selection = "input", domain_extension = "metals/")

# read in Master Country List and remove isos in sintering_percent and "global".
    # This is because these countries (and global) already have data specific to sintering.
    MCL <- readData( "MAPPINGS", "Master_Country_List", ".csv")
    MCL <- MCL %>%
      select(c(iso, Paper_Figure_Region)) %>%
      filter(iso %!in% c("global","fra","gbr","swe","pol","chn","jpn","usa")) %>%
      unique()

# ------------------------------------------------------------------------------
# 2. Calculate kt of sintering for all countries

    # Extend data forward
    # SJS TODO - make this work probably need to add columns with NA's for years 
    # after the sintering_percent data is available and the last CEDS year.
    # At that point I think the extend_and_interpolate function should work:
    # Or maybe add that as an optional functionality with extend_and_interpolate,
    # since this is probably not the only place this will be an issue? (or maybe
    # there's already a function that does this?)
    end_year <- BP_last_year
    start_year <- 1950
    disaggregate_years <- paste0( 'X', start_year:end_year )
    sintering_percent_v2 <- extend_and_interpolate(sintering_percent,disaggregate_years)
    sintering_percent_v2[ is.na( sintering_percent_v2 ) ] <- 0

    # Add all other countries to "sintering_percent".
    join <- sintering_percent %>%
      left_join(MCL, by = c("iso" = "Paper_Figure_Region")) %>%
      mutate(iso = if_else(!is.na(iso.y), iso.y, iso)) %>%
      select(-iso.y)

    # Replace NAs from countries that we don't have sintering data for with
    # global data.
    Fsint <- join %>%
      full_join(MCL, by = "iso") %>%
      dplyr::rename(region = iso)

    # countries where we dot have sintering data (NAs)
    Fsint %>% filter(is.na(X1920)) -> df_w_na
    # countries that have sintering data (no NAs)
    Fsint %>% filter(!is.na(X1920)) -> df_wo_na
    # separate out global data
    Fsint %>% filter(region == "global ") -> df_global

    na_regions <- data.frame(iso_w_na = unique(df_w_na$region)) %>%
      mutate(region = "global ")

    # replace countries with NAs with global average sintering data
    df_global %>% left_join(na_regions, by = "region") %>%
      mutate(region = iso_w_na) %>%
      select(-iso_w_na) -> df2

    # rebind
    Fsint_final <- bind_rows(df_wo_na, df2)
    names(Fsint_final)[1] <- "iso"

# Calculate kt of sinter

    # Make pig iron and sintering data long. Then join by iso and year.
    pig_iron_long <- pig_iron_data %>%
      select(-c(fuel,units,sector)) %>%
      gather(key = "year", value = "pig_iron", -c(iso))
    sintering_long <- Fsint_final %>%
      gather(key = "year", value = "Fsint", -c(iso))
    sintering_long$Fsint <- as.numeric(sintering_long$Fsint)

    pig_and_sint <- left_join(pig_iron_long, sintering_long, by = c("iso","year"))

# Equation: kt sinter = kt pig iron x Fsint/0.576
    perc_iron <- 0.576 # each kt of sinter is 57.6% iron
    calc_kt_sint <- pig_and_sint %>%
      mutate(kt_sint = Fsint*pig_iron/perc_iron) %>%
      na.omit()

# Spread kt of sinter wide
    kt_sint_wide <- calc_kt_sint %>%
      select(-c(pig_iron,Fsint)) %>%
      spread(key = "year", value = "kt_sint") %>%
      mutate(sector = "sintering") %>%
      mutate(units = "kt") %>%
      select(iso, sector, units, everything())

# Fill in historical data with 0
    kt_sint_wide[ paste0('X', historical_pre_extension_year: 1919)] <- 0
    kt_sint_wide <- kt_sint_wide[ c( 'iso' , 'sector' , 'units' , X_extended_years ) ]

# ---------------------------------- --------------------------------------------
# 3. Read out kt of sinter
    writeData( kt_sint_wide, "MED_OUT", "A.Sintering_production", meta = F )
    writeData( kt_sint_wide, "EXT_IN", "A.Sintering_production", domain_extension = "extension-data/")

    logStop()
    # END









