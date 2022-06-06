#Add HTAP USA Data
emission_file <- paste0("HTAP_emissions_summary_2002-2017")
inventory_data_file <- "../emissions-inventories"
Edgar_HTAP_USA_data <- readData(domain = "EM_INV",
                                domain_extension = "EDGAR_HTAP_USA/",
                                file_name = emission_file,
                                extension = ".csv")

# Read in data and merge into a dataframe and rename the columns
#EDGAR_HTAP_USA_data <- read.csv(file=Edgar_HTAP_USA)
colnames(Edgar_HTAP_USA_data) <- c("Year","Month","Species","Sector","Cumulative_emissions","Country")

#Replace the commas in the numbers (as factors at this point) with nothing
EDGAR_USA_new <- gsub(",", "", Edgar_HTAP_USA_data$Cumulative_emissions)

#Convert the numbers from factors to numeric
Edgar_HTAP_USA_data$Cumulative_emissions <- as.numeric(as.character( EDGAR_USA_new )) /1000

#Renames VOC to NMVOC
if (em == "NMVOC") {
    Edgar_HTAP_USA_data %>% mutate(Species = str_replace(Species,"VOC","NMVOC")) -> Edgar_HTAP_USA_data
}

if (em == "NOx"){
    Edgar_HTAP_USA_data %>% mutate(Species = str_replace(Species,"NOX","NOx")) -> Edgar_HTAP_USA_data
}

#Only draws the annual pollutant data from the csv file
Edgar_HTAP_USA_data %>% filter(Month == "annual") %>% filter(Species == em) %>% filter(Sector == "USA_Total") -> EDGAR_HTAP_USA_long

#Removes the Species, Month, and Sector columns. Also renames the remaining three. Converts country to lowercase. Reorders the columns
EDGAR_HTAP_USA_long <- subset(EDGAR_HTAP_USA_long, select = -c(Month,Species,Sector))
colnames(EDGAR_HTAP_USA_long) <- c("year","EDGAR_HTAPv3","iso")
EDGAR_HTAP_USA_long$iso <- tolower(EDGAR_HTAP_USA_long$iso)
EDGAR_HTAP_USA_long <- EDGAR_HTAP_USA_long[c("year","iso","EDGAR_HTAPv3")]

#EDGAR_HTAP Korea files

#Read in the EDGAR_HTAP files
emission_file <- paste0("EDGAR_HTAP_KOR_", em)
inventory_data_file <- "../emissions-inventories"
Edgar_HTAP_KOR <- readData(domain = "EM_INV",
                           domain_extension = "EDGAR_HTAP_KOR/",
                           file_name = emission_file,
                           extension = ".csv")
Edgar_HTAP_KOR$EDGAR_HTAPv3 <- Edgar_HTAP_KOR$EDGAR_HTAPv3/1000000


#Combines the EDGAR_HTAP and EDGAR_HTAP_USA dataframes
EDGAR_HTAP_long <- merge(EDGAR_HTAP_long,EDGAR_HTAP_USA_long,all=TRUE)
EDGAR_HTAP_long <- merge(EDGAR_HTAP_long,Edgar_HTAP_KOR,all=TRUE)

