
em <- "NMVOC"

#load in EDGAR_HTAP_KOR files
modE_dir <- paste0('../emissions-inventories/EDGAR_HTAP_KOR')

setwd(modE_dir)

sheets <- readxl::excel_sheets("Summary_HTAP_v3_SKorea_210923.xlsx")
tibble <- lapply(sheets, function(x) readxl::read_excel("Summary_HTAP_v3_SKorea_210923.xlsx", sheet = x))
data_frame <- lapply(tibble, as.data.frame)

#Separate the list of data frame data by
EDGAR_HTAP_KOR_CO <- data_frame[[2]]
EDGAR_HTAP_KOR_NOx <- data_frame[[3]]
EDGAR_HTAP_KOR_SO2 <- data_frame[[4]]
EDGAR_HTAP_KOR_PM10 <- data_frame[[5]]
EDGAR_HTAP_KOR_PM2.5 <- data_frame[[6]]
EDGAR_HTAP_KOR_NMVOC <- data_frame[[7]]
EDGAR_HTAP_KOR_NH3 <- data_frame[[8]]

year <- (2000:2017)
iso <- "kor"


EDGAR_HTAPv3 <- colSums(Filter(is.numeric,EDGAR_HTAP_KOR_CO))
HTAP_KOR_CO <- data.frame(year,iso,EDGAR_HTAPv3)


EDGAR_HTAPv3 <- colSums(Filter(is.numeric,EDGAR_HTAP_KOR_NOx))
HTAP_KOR_NOx <- data.frame(year,iso,EDGAR_HTAPv3)


EDGAR_HTAPv3 <- colSums(Filter(is.numeric,EDGAR_HTAP_KOR_PM10))
HTAP_KOR_PM10 <- data.frame(year,iso,EDGAR_HTAPv3)


EDGAR_HTAPv3 <- colSums(Filter(is.numeric,EDGAR_HTAP_KOR_PM2.5))
HTAP_KOR_PM2.5 <- data.frame(year,iso,EDGAR_HTAPv3)


EDGAR_HTAPv3 <- colSums(Filter(is.numeric,EDGAR_HTAP_KOR_NMVOC))
HTAP_KOR_NMVOC <- data.frame(year,iso,EDGAR_HTAPv3)

EDGAR_HTAPv3 <- colSums(Filter(is.numeric,EDGAR_HTAP_KOR_NH3))
HTAP_KOR_NH3 <- data.frame(year,iso,EDGAR_HTAPv3)

EDGAR_HTAPv3 <- colSums(Filter(is.numeric,EDGAR_HTAP_KOR_SO2))
HTAP_KOR_SO2 <- data.frame(year,iso,EDGAR_HTAPv3)

#move the data frame information into separate csvs for each pollutant
write.csv(HTAP_KOR_CO, "C:/users/such559/Documents/CEDS-Dev/input/emissions-inventories/EDGAR_HTAP_KOR/EDGAR_HTAP_KOR_CO.csv",row.names=FALSE)
write.csv(HTAP_KOR_NH3, "C:/users/such559/Documents/CEDS-Dev/input/emissions-inventories/EDGAR_HTAP_KOR/EDGAR_HTAP_KOR_NH3.csv",row.names=FALSE)
write.csv(HTAP_KOR_NMVOC, "C:/users/such559/Documents/CEDS-Dev/input/emissions-inventories/EDGAR_HTAP_KOR/EDGAR_HTAP_KOR_NMVOC.csv",row.names=FALSE)
write.csv(HTAP_KOR_NOx, "C:/users/such559/Documents/CEDS-Dev/input/emissions-inventories/EDGAR_HTAP_KOR/EDGAR_HTAP_KOR_NOx.csv",row.names=FALSE)
write.csv(HTAP_KOR_PM10, "C:/users/such559/Documents/CEDS-Dev/input/emissions-inventories/EDGAR_HTAP_KOR/EDGAR_HTAP_KOR_PM10.csv",row.names=FALSE)
write.csv(HTAP_KOR_PM2.5, "C:/users/such559/Documents/CEDS-Dev/input/emissions-inventories/EDGAR_HTAP_KOR/EDGAR_HTAP_KOR_PM2.5.csv",row.names=FALSE)
write.csv(HTAP_KOR_SO2, "C:/users/such559/Documents/CEDS-Dev/input/emissions-inventories/EDGAR_HTAP_KOR/EDGAR_HTAP_KOR_SO2.csv",row.names=FALSE)
