# ------------------------------------------------------------------------------
# Program Name: D1.2.BC_OC_to_PM2.5_default_ratios.R
# Authors' Names: Andrea Mott
# Date Last Modified: Oct 13, 2020
# Program Purpose: To create BC and OC to PM2.5 default ratios
# Input Files: D.BC_default_comb_emissions, D.OC_default_comb_emissions
# Output Files: BC_to_PM25_defaultratio.csv, OC_to_PM25_defaultratio.csv
# Notes:
# TODO: 1) when calculating % change over years, ignore jumps that go from a value
#       to NaN.(Note: this doesn't matter for Road sector).
#       2) Clean plotting diagnostic files.

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Get emission species first so can name log appropriately
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[1]
if ( is.na( em ) ) em <- "BC"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( 'common_data.R', "data_functions.R",
              "emissions_scaling_functions.R", "analysis_functions.R",
              "interpolation_extension_functions.R" ) # Additional function files required.
log_msg <- "Initial reformatting of US emissions" # First message to be printed to the log
script_name <- "E.US_emissions.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 1.  Calculate emission ratio of default comb emissions BC/PM2.5 and OC/PM2.5

if (em %in% c ('BC','OC') ) {

    # Input BC and OC default combustion emissions
# TODO: double check it's reading in the right file. Might want D.BC_ not CD.BC (they are the same file)
    CEDS_default_comb_em_BC <- readData( "DEFAULT_EF_IN", 'CD.BC_default_comb_emissions' , ".csv" )
    CEDS_default_comb_em_OC <- readData( "DEFAULT_EF_IN", 'CD.OC_default_comb_emissions' , ".csv" )

    # Select iso and sector to keep. Add all BC and OC emissions for each year.
    BC_default <- CEDS_default_comb_em_BC %>%
        group_by(iso,sector) %>%
        summarise_if(is.numeric,sum) %>%
        ungroup()

    OC_default <- CEDS_default_comb_em_OC %>%
        group_by(iso,sector) %>%
        summarise_if(is.numeric,sum) %>%
        ungroup()

    # Remove iso and sector for matrix multiplication
    iso_sector <- BC_default[,1:2]
    BC_default <- BC_default %>% select (-iso,-sector)
    OC_default <- OC_default %>% select (-iso,-sector)


    # Convert OC to OM
    OMconstant = 1.3 # OM:OC ratio assumed for any fossil fuel at this point
    OM = OMconstant* OC_default  #organic matter portion of OC

    # Calculate PM2.5 emissions
    # Assumption: 95% of PM2.5 emissions is OM and BC for diesel.
        # This would need to be generalized if applied beyond mobile diesel sources.
    eff <- 0.95
    PM25_default <- (OM+BC_default)/eff

    # Ratio of BC and OC to PM2.5
    BC_to_PM25_defaultratio <- BC_default/PM25_default
    OC_to_PM25_defaultratio <- OC_default/PM25_default

    # Re-add iso and sector to be able to understand data
    BC_to_PM25_defaultratio <- cbind(iso_sector,BC_to_PM25_defaultratio)
    OC_to_PM25_defaultratio <- cbind(iso_sector,OC_to_PM25_defaultratio)

    }
# ------------------------------------------------------------------------------
# 2. Detect discontinuities - calculate the % change from the previous year

Calculate_discontinuities <- function(em) {

    if (em == "BC") {
        em_to_PM25_defaultratio <- BC_to_PM25_defaultratio
    } else {em_to_PM25_defaultratio <- OC_to_PM25_defaultratio}

    # convert to long format
     em_long <- em_to_PM25_defaultratio %>%
        gather(key = "Year", value = "Value", -c(iso,sector))%>%
        mutate(Year = as.numeric(substr(Year,2,5)))

     # replace NaN Values with 0
     em_long[is.na(em_long)] <- 0


# Find discontinuities by calculating percent change between years.
     # assumed 50% jump qualifies discontinuity
     # TODO: ignore jumps that go from a value to NaN.(Note: this doesn't matter for Road sector)
    joined <- full_join(em_long, em_long, by = c('iso','sector')) %>% filter(Year.y - 1 == Year.x)
    with_diff <- joined %>% mutate(perc_diff = abs((Value.y-Value.x)/Value.x))
    em_to_PM25_anamolies <- with_diff %>%
        filter(perc_diff > 0.5)
      #  mutate(perc_diff = replace(perc_diff, perc_diff==1,0))

# remove multiple discontinuities per iso/sector combination. Keep the most recent discontinuity.
    em_to_PM25_anamolies_removeduplicates <- em_to_PM25_anamolies[!duplicated(em_to_PM25_anamolies[1:2], fromLast=T),]

# repeat rows in anomaly df for all iso-sector combinations (x27)
    # make this cleaner. Is it even necessary to arrange?
    n <- 27 #number of times each row is repeated
    repeated_discontinuities <- do.call("rbind", replicate(n, em_to_PM25_anamolies_removeduplicates, simplify = FALSE))
    repeated_discontinuities %>% select(-c(perc_diff, Year.x, Value.x))

 # join repeated discontinuity dataframe and em_long dataframe
    combined_dfs <- repeated_discontinuities %>%
        left_join(em_long, by =c('iso','sector')) %>%
        distinct()

# replace values in previous years if discontinuity occurs
    replaced_values <- combined_dfs %>%
        mutate(Value.flag = factor(ifelse(Year < Year.y, Value.y, Value))) %>%
        select(-c(Year.y,Value.y,Value)) %>%
        dplyr::rename(Value_replaced = "Value.flag") %>%
        distinct()
    replaced_values$Value_replaced <- as.numeric(as.character(replaced_values$Value_replaced))

# join em_long and replaced_values
    join_together <- em_long %>%
        left_join(replaced_values, by = c('iso','sector','Year')) %>%
        mutate(final.Value = factor(ifelse(is.na(Value_replaced) , Value, Value_replaced))) %>%
        select(-c(Value, Value_replaced))
    join_together$final.Value <- as.numeric(as.character(join_together$final.Value))
    join_together$Year <- paste0('X',join_together$Year)

# convert to wide format
    em_to_PM25_defaultratio_removeddiscont <- spread(join_together, key = "Year", value = "final.Value")
    em_to_PM25_defaultratio_removeddiscont[is.na(em_to_PM25_defaultratio_removeddiscont)] <- 0

# ------------------------------------------------------------------------
# 3 - Detect Outliers After Removing Discontinuities

# calculate the median default ratio for every sector across all isos
    median_all_sectors_isos <- em_to_PM25_defaultratio_removeddiscont %>%
        mutate( median_value = apply(em_to_PM25_defaultratio_removeddiscont[,4:20],1,median,na.rm=TRUE)) %>%
        select(c(-Value.x,-Year.x,-perc_diff))
       # filter( sector == "1A3b_Road") - perhaps do this entire outliers step separately just for road?

# convert to long format
    median_long <- median_all_sectors_isos %>%
        gather(key = "Year", value = "Value", -c(iso,sector,median_value)) %>%
        mutate(Year = as.numeric(substr(Year,2,5)))

 # Subtract each Year column from the median value column
    subtracted_median <- median_long %>%
        mutate( dif = subtract(median_long$Value,median_long$median_value))

# Replace outliers with median value if the difference > 0.4 and the median value doesn't equal 0
    # TODO: the median value should just never equal 0 if there are other values
    replace_outliers <- subtracted_median %>%
        mutate( newvalue = factor(ifelse((abs(dif > 0.4) & dif != 0), median_value, Value))) %>%
        select(-c(median_value,Value,dif))
    replace_outliers$newvalue <- as.numeric(as.character(replace_outliers$newvalue))
    replace_outliers$Year <- paste0('X',replace_outliers$Year)

# make wide
    em_to_PM25_defaultratio_removed_outliers <- spread(replace_outliers, key = "Year", value = "newvalue")

    return(em_to_PM25_defaultratio_removed_outliers)
}

# create function
em_to_PM25_defaultratio_removed_outliers <- Calculate_discontinuities( em )

# -----------------------------------------------------------------------------
# 4. Output
#TODO: write out other diagnostic outputs? Turn into graphs? should this entire script be in the makefile?

# Output files, replace NA with 0s
   if (em == "BC") {
       write.csv(em_to_PM25_defaultratio_removed_outliers, "CD.BC_to_PM25_defaultratio.csv",na = "0")
   } else {
    write.csv(em_to_PM25_defaultratio_removed_outliers, "CD.OC_to_PM25_defaultratio.csv",na = "0") }


# ------------------------------------------------------------------------------
# 5. Plot

em_final <- em_to_PM25_defaultratio_removed_outliers

# sort by sector
    em_final <- em_final %>%
        filter(sector == "1A3b_Road") %>%
        select(-sector)

# Long and messy plot

#set working directory to save pdf files
    setwd( "../diagnostic-output/BC-OC" )

pdf( paste0(em,"_to_PM2.5_graphs.pdf"))

if (em == "BC") {
    em_final_1 <- em_final[1:50,] %>%
        gather(key = "Year", value = "Value",-c(iso)) %>%
        mutate(Year = as.numeric(substr(Year,2,5)))
    em_final_1_plot <- em_final_1 %>% ggplot(aes(x=Year,y=Value,color=iso)) +  geom_line() +
        ylim(0,0.75) +
        ggtitle("BC/PM2.5 Default Ratio_Plot1") +
        ylab("BC/PM2.5")
    print(em_final_1_plot)

    em_final_2 <- em_final[51:100,] %>%
        gather(key = "Year", value = "Value",-c(iso)) %>%
        mutate(Year = as.numeric(substr(Year,2,5)))
    em_final_2_plot <- em_final_2 %>% ggplot(aes(x=Year,y=Value,color=iso)) +  geom_line() +
        ylim(0,0.75) +
        ggtitle("BC/PM2.5 Default Ratio_Plot 2") +
        ylab("BC/PM2.5")
    print(em_final_2_plot)

    em_final_3 <- em_final[101:150,] %>%
        gather(key = "Year", value = "Value",-c(iso)) %>%
        mutate(Year = as.numeric(substr(Year,2,5)))
    em_final_3_plot <- em_final_3 %>% ggplot(aes(x=Year,y=Value,color=iso)) +  geom_line() +
        ylim(0,0.75) +
        ggtitle("BC/PM2.5 Default Ratio Plot3") +
        ylab("BC/PM2.5")
    print(em_final_3_plot)

    em_final_4 <- em_final[151:200,] %>%
        gather(key = "Year", value = "Value",-c(iso)) %>%
        mutate(Year = as.numeric(substr(Year,2,5)))
    em_final_4_plot <- em_final_4 %>% ggplot(aes(x=Year,y=Value,color=iso)) +  geom_line() +
        ylim(0,0.75) +
        ggtitle("BC/PM2.5 Default Ratio Plot4") +
        ylab("BC/PM2.5")
    print(em_final_4_plot)

    em_final_5 <- em_final[201:250,] %>%
        gather(key = "Year", value = "Value",-c(iso)) %>%
        mutate(Year = as.numeric(substr(Year,2,5)))
    em_final_5_plot <- em_final_2 %>% ggplot(aes(x=Year,y=Value,color=iso)) +  geom_line() +
        ylim(0,0.75) +
        ggtitle("BC/PM2.5 Default Ratio Plot5") +
        ylab("BC/PM2.5")
    print(em_final_5_plot)

} else {
    em_final_1 <- em_final[1:50,] %>%
        gather(key = "Year", value = "Value",-c(iso)) %>%
        mutate(Year = as.numeric(substr(Year,2,5)))
    em_final_1_plot <- em_final_1 %>% ggplot(aes(x=Year,y=Value,color=iso)) +  geom_line() +
        ylim(0,0.75) +
        ggtitle("OC/PM2.5 Default Ratio") +
        ylab("OC/PM2.5")
    print(em_final_1_plot)

    em_final_2 <- em_final[51:100,] %>%
        gather(key = "Year", value = "Value",-c(iso)) %>%
        mutate(Year = as.numeric(substr(Year,2,5)))
    em_final_2_plot <- em_final_2 %>% ggplot(aes(x=Year,y=Value,color=iso)) +  geom_line() +
        ylim(0,0.75) +
        ggtitle("OC/PM2.5 Default Ratio") +
        ylab("OC/PM2.5")
    print(em_final_2_plot)

    em_final_3 <- em_final[101:150,] %>%
        gather(key = "Year", value = "Value",-c(iso)) %>%
        mutate(Year = as.numeric(substr(Year,2,5)))
    em_final_3_plot <- em_final_3 %>% ggplot(aes(x=Year,y=Value,color=iso)) +  geom_line() +
        ylim(0,0.75) +
        ggtitle("OC/PM2.5 Default Ratio") +
        ylab("OC/PM2.5")
    print(em_final_3_plot)

    em_final_4 <- em_final[151:200,] %>%
        gather(key = "Year", value = "Value",-c(iso)) %>%
        mutate(Year = as.numeric(substr(Year,2,5)))
    em_final_4_plot <- em_final_4 %>% ggplot(aes(x=Year,y=Value,color=iso)) +  geom_line() +
        ylim(0,0.75) +
        ggtitle("OC/PM2.5 Default Ratio") +
        ylab("OC/PM2.5")
    print(em_final_4_plot)

    em_final_5 <- em_final[201:250,] %>%
        gather(key = "Year", value = "Value",-c(iso)) %>%
        mutate(Year = as.numeric(substr(Year,2,5)))
    em_final_5_plot <- em_final_2 %>% ggplot(aes(x=Year,y=Value,color=iso)) +  geom_line() +
        ylim(0,0.75) +
        ggtitle("OC/PM2.5 Default Ratio") +
        ylab("OC/PM2.5")
    print(em_final_5_plot)
}
dev.off()



