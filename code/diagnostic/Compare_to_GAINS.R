# ------------------------------------------------------------------------------
# Program Name: Compare_to_GAINS.R
# Author: Rachel Hoesly
# Date Last Updated: 30 April 2019
# Program Purpose:
# Input Files: [em]_total_CEDS_emissions.csv
# Output Files: figures in the diagnostic-output
# TODO:
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R",'common_data.R', 'IO_functions.R') # Additional function files may be required.
log_msg <- "Compare_to_GAINS" # First message to be printed to the log
script_name <- "Compare_to_GAINS.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "NOx"

# Stop script if running for unsupported species
if ( em %!in% c('SO2','NOx','NMVOC','BC','OC','CH4','CO','CO2') ) {
  stop (paste( 'GAINS EMF-30 is not supported for emission species', em))
}

# ---------------------------------------------------------------------------
# 0.5 Load Packages

library('ggplot2')
library('plyr')
library('scales')
library('gridExtra')

# ---------------------------------------------------------------------------
# 0.5. Script Options

gains_start_year <- 2000
gains_end_year <- 2010
CEDS_start_year <- 1960
CEDS_end_year <- end_year

gains_years <- seq(from=gains_start_year,to=gains_end_year,by=5)
x_gains_years <- paste0('X',gains_years)

ceds_start_year <- (gains_start_year - 5)
ceds_years <- ceds_start_year:end_year
x_ceds_years <- paste0('X',ceds_years)


# ---------------------------------------------------------------------------
# 1. Load files

# GAINS emissions
e.sheet <- em
if(em == 'NMVOC') e.sheet <- 'VOC'
gains_emissions <- readData( domain = 'EM_INV', domain_extension = 'GAINS/', file_name = 'GAINS_EMF30_EMISSIONS_extended_Ev5a_CLE_Nov2015',
                       ".xlsx", sheet_selection = e.sheet)

# CEDS emissions
Master_Country_List <- readData('MAPPINGS', 'Master_Country_List')
ceds_emissions <- readData('MED_OUT', paste0(em,'_total_CEDS_emissions'))

#mapping files
ctry_map <- readData(domain = 'MAPPINGS',  domain_extension = 'GAINS/', file_name ='emf-30_ctry_map')
sector_map <- readData(domain = 'MAPPINGS',  domain_extension = 'GAINS/', file_name ='GAINS_sector_mapping')


# ---------------------------------------------------------------------------
# 2. Process GAINS

gains <- gains_emissions[ which(gains_emissions$Sector == 'Sum' |
                                  grepl( "End_Use_Residential_", gains_emissions$Sector ) ),
                          c("Region" ,"Sector", "2000" ,  "2005" ,  "2010")]
names(gains) <- c("Region", "Sector", "X2000", "X2005", "X2010")
gains <- gains[ gains$Region != "Global", ]

gains_resid <- filter( gains, grepl( "End_Use_Residential_", Sector ) ) %>%
  select( -Sector ) %>% group_by( Region ) %>%
  summarise_each( funs( sum(., na.rm = T ) ) ) %>%
  dplyr::mutate( Sector = "Residential") %>%
  dplyr::arrange( Region ) %>% data.frame()
gains_tot <- filter( gains, Sector == 'Sum' ) %>%
  select( -Sector ) %>% group_by( Region ) %>%
  summarise_each( funs( sum(., na.rm = T ) ) ) %>%
  dplyr::mutate( Sector = "All" ) %>%
  dplyr::arrange( Region ) %>% data.frame()
gains_nonresid <- gains_tot %>%
  dplyr::mutate( Sector = "Non-Residential" )
gains_nonresid[, c( "X2000", "X2005", "X2010" ) ] <-
  gains_tot[, c( "X2000", "X2005", "X2010" ) ] -
  gains_resid[, c( "X2000", "X2005", "X2010" ) ]

gains <- rbind( gains_resid, gains_tot, gains_nonresid )
gains[ gains < 0 ] <- 0

# only CO2 is reported in Tg rather than Gg, so multiply by 10^3 to convert Tg to Gg
if(em == 'CO2') gains[,years] <- gains[,years]*10^3

gains_long <- melt(gains)
gains_long$inv <- 'GAINS'

# ---------------------------------------------------------------------------
# 3. Process CEDS

not_gains_sectors <- c('1A3ai_International-aviation',
                        '1A3aii_Domestic-aviation',
                        '1A3di_International-shipping',
                        '1A3dii_Domestic-navigation',
                        '1B2d_Fugitive-other-energy',
                        '6A_Other-in-total',
                        '6B_Other-not-in-total',
                        '7A_Fossil-fuel-fires',
                        '11A_Volcanoes',
                        '11B_Forest-fires',
                        '11C_Other-natural',
                        '5C_Waste-incineration',
                        '3D_Soil-emissions')

ceds <- ceds_emissions[ which( ceds_emissions$sector %!in% not_gains_sectors )
                        ,c('iso', 'sector', x_ceds_years)]
ceds$Region <- ctry_map[match(ceds$iso, ctry_map$iso),'emf_name']
ceds <- filter( ceds, !is.na( Region ) )
ceds$sector[ ceds$sector != "1A4b_Residential" ] <- "Non-Residential"
ceds$sector[ ceds$sector == "1A4b_Residential" ] <- "Residential"
ceds <- select( ceds, -iso ) %>% group_by( sector, Region ) %>%
  summarise_each( funs( sum(., na.rm = T ) ) ) %>%
  data.frame()
ceds_tot <- dplyr::mutate( ceds, sector = "All" ) %>% group_by( sector, Region ) %>%
  summarise_each( funs( sum(., na.rm = T ) ) ) %>%
  data.frame()
ceds <- rbind( ceds, ceds_tot )
names( ceds )[ names( ceds ) == "sector" ] <- "Sector"

ceds_long <- melt(ceds)
ceds_long$inv <- 'CEDS'

# ---------------------------------------------------------------------------
# 4. Graph Global Comparison

# All Sectors
global_long <- rbind( filter( ceds_long, Sector == "All"),
                      filter( gains_long, Sector == "All" ) )
names(global_long) <- c("Sector", "Region", "year" ,"total","inv" )
global_long$year <- gsub('X',"",global_long$year)
global_long$year <- as.numeric(global_long$year)

global <- cast(global_long, inv+Sector ~ year, value = 'total', fun.aggregate = sum)[c('inv', 'Sector', gains_years)]

#writeout
writeData(global,'DIAG_OUT', paste0('GAINS_',em,'_Global_Comparison'),domain_extension = 'ceds-comparisons/',meta=F)

#Plot
df <- global_long[,c('inv','year','total')]
df <- aggregate(df['total'], by = list(inv = df$inv, year = df$year), sum)
df$inv <- as.factor(df$inv)
max <- 1.2*(max(df$total))
plot <- ggplot(df, aes(x=year,y=total, color = inv)) +
  geom_point(shape=19) +
  geom_line(data = subset(df, inv=='CEDS'),size=1,aes(x=year,y=total, color = inv)) +
  scale_x_continuous(breaks= seq(from=ceds_start_year,to=end_year,by=5) )+
  scale_y_continuous(limits = c(0,max ),labels = comma)+
  ggtitle( paste('Global',em,'Emissions') )+
  labs(x='Year',y= paste(em,'Emissions [kt]') )
plot
ggsave( paste0('../diagnostic-output/ceds-comparisons/GAINS_',em,'_Global_Comparison.pdf') , width = 7, height = 4)


# Residential
global_long_resid <- rbind( filter( ceds_long, Sector == "Residential"),
                      filter( gains_long, Sector == "Residential" ) )
names(global_long_resid) <- c("Sector", "Region", "year" ,"total","inv" )
global_long_resid$year <- gsub('X',"",global_long_resid$year)
global_long_resid$year <- as.numeric(global_long_resid$year)

global_resid <- cast(global_long_resid, inv+Sector ~ year, value = 'total', fun.aggregate = sum)[c('inv', 'Sector', gains_years)]

#writeout
writeData(global_resid,'DIAG_OUT', paste0('GAINS_',em,'_Global_Comparison_Residential'),domain_extension = 'ceds-comparisons/sector-level/',meta=F)

#Plot
df <- global_long_resid[,c('inv','year','total')]
df <- aggregate(df['total'], by = list(inv = df$inv, year = df$year), sum)
df$inv <- as.factor(df$inv)
max <- 1.2*(max(df$total))
plot <- ggplot(df, aes(x=year,y=total, color = inv)) +
  geom_point(shape=19) +
  geom_line(data = subset(df, inv=='CEDS'),size=1,aes(x=year,y=total, color = inv)) +
  scale_x_continuous(breaks= seq(from=ceds_start_year,to=end_year,by=5) )+
  scale_y_continuous(limits = c(0,max ),labels = comma)+
  ggtitle( paste('Global',em,'Emissions (Residential)') )+
  labs(x='Year',y= paste(em,'Emissions [kt]') )
plot
ggsave( paste0('../diagnostic-output/ceds-comparisons/sector-level/GAINS_',em,'_Global_Comparison_Residential.pdf') , width = 7, height = 4)


# Non-Residential
global_long_nonresid <- rbind( filter( ceds_long, Sector == "Non-Residential"),
                            filter( gains_long, Sector == "Non-Residential" ) )
names(global_long_nonresid) <- c("Sector", "Region", "year" ,"total","inv" )
global_long_nonresid$year <- gsub('X',"",global_long_nonresid$year)
global_long_nonresid$year <- as.numeric(global_long_nonresid$year)

global_nonresid <- cast(global_long_nonresid, inv+Sector ~ year, value = 'total', fun.aggregate = sum)[c('inv', 'Sector', gains_years)]

#writeout
writeData(global_nonresid,'DIAG_OUT', paste0('GAINS_',em,'_Global_Comparison_Non-Residential'),domain_extension = 'ceds-comparisons/sector-level/',meta=F)

#Plot
df <- global_long_nonresid[,c('inv','year','total')]
df <- aggregate(df['total'], by = list(inv = df$inv, year = df$year), sum)
df$inv <- as.factor(df$inv)
max <- 1.2*(max(df$total))
plot <- ggplot(df, aes(x=year,y=total, color = inv)) +
  geom_point(shape=19) +
  geom_line(data = subset(df, inv=='CEDS'),size=1,aes(x=year,y=total, color = inv)) +
  scale_x_continuous(breaks= seq(from=ceds_start_year,to=end_year,by=5) )+
  scale_y_continuous(limits = c(0,max ),labels = comma)+
  ggtitle( paste('Global',em,'Emissions (Non-Residential)') )+
  labs(x='Year',y= paste(em,'Emissions [kt]') )
plot
ggsave( paste0('../diagnostic-output/ceds-comparisons/sector-level/GAINS_',em,'_Global_Comparison_Non-Residential.pdf') , width = 7, height = 4)




# ---------------------------------------------------------------------------
# 4. Graph Regional Comparison

# All sectors
region_long <- global_long
region_wide <- cast(region_long, Region+Sector+ inv ~ year, value = 'total')
region_wide <- region_wide[c( 'Region', 'Sector', 'inv', gains_years)]

#write out table
writeData(region_wide,'DIAG_OUT', paste0('GAINS_',em,'_Regional_Comparison'),domain_extension = 'ceds-comparisons/',meta=F)

regions_list <- region_long[which(region_long$year == '2000'),c('Region','total')]
regions_list <- regions_list[order(-regions_list$total),]
regions_list_order <- unique(regions_list$Region)
regions_df_order <- data.frame(Region=regions_list_order,
                               plot = c(unlist(lapply(X=1:4,FUN=rep, times=4)),
                                        unlist(lapply(X=5,FUN=rep, times=5)),
                                        unlist( lapply( X = 6, FUN = rep, times =  3 ) ) ) )

plot_list <- list()
for(i in 1:6){

  plot_regions <- regions_df_order [which(regions_df_order$plot == i),'Region']

  plot_df <- region_long[which(region_long$Region %in% plot_regions),c('inv','year','Region','total')]
  plot_df$inv <- as.factor(plot_df$inv)
  plot_df$region <- as.factor(plot_df$Region)
  max <- 1.2*(max(plot_df$total))

  plot <- ggplot(plot_df, aes(x=year,y=total, color = region, shape=inv)) +
    geom_point(data = subset(plot_df, inv =='GAINS'),size=2,aes(x=year,y=total, color = Region)) +
    geom_line(data = subset(plot_df, inv =='CEDS'),size=1,aes(x=year,y=total, color = Region)) +
    scale_x_continuous(breaks=seq(from=ceds_start_year,to=end_year,by=5))+
    scale_y_continuous(limits = c(0,max ),labels = comma)+
    scale_shape_discrete(guide=FALSE)+
    labs(x='Year',y= paste(em,'Emissions [kt]'))+
    theme(legend.title=element_blank())
  plot
  plot_list[[i]]<-plot
}

pdf(paste0('../diagnostic-output/ceds-comparisons/GAINS_',em,'_Regional_Comparison_All.pdf'),width=12,height=10,paper='special')
grid.arrange(plot_list[[1]],plot_list[[2]],
             plot_list[[3]],plot_list[[4]],
             plot_list[[5]],plot_list[[6]], ncol=2,
             top = paste('GAINS vs CEDS - Regional',em,'Emissions'))
dev.off()


# Residential
region_long_resid <- global_long_resid
region_wide_resid <- cast(region_long_resid, Region + Sector + inv ~ year, value = 'total')
region_wide_resid <- region_wide_resid[c( 'Region', 'Sector', 'inv', gains_years)]

#write out table
writeData(region_wide_resid,'DIAG_OUT', paste0('GAINS_',em,'_Regional_Comparison_Residential'),domain_extension = 'ceds-comparisons/sector-level/',meta=F)

regions_list <- region_long_resid[which(region_long_resid$year == '2000'),c('Region','total')]
regions_list <- regions_list[order(-regions_list$total),]
regions_list_order <- unique(regions_list$Region)
regions_df_order <- data.frame(Region=regions_list_order,
                               plot = c(unlist(lapply(X=1:4,FUN=rep, times=4)),
                                        unlist(lapply(X=5,FUN=rep, times=5)),
                                        unlist( lapply( X = 6, FUN = rep, times =  3 ) ) ) )

plot_list <- list()
for(i in 1:6){

  plot_regions <- regions_df_order [which(regions_df_order$plot == i),'Region']

  plot_df <- region_long_resid[which(region_long_resid$Region %in% plot_regions),c('inv','year','Region','total')]
  plot_df$inv <- as.factor(plot_df$inv)
  plot_df$region <- as.factor(plot_df$Region)
  max <- 1.2*(max(plot_df$total))

  plot <- ggplot(plot_df, aes(x=year,y=total, color = region, shape=inv)) +
    geom_point(data = subset(plot_df, inv =='GAINS'),size=2,aes(x=year,y=total, color = Region)) +
    geom_line(data = subset(plot_df, inv =='CEDS'),size=1,aes(x=year,y=total, color = Region)) +
    scale_x_continuous(breaks=seq(from=ceds_start_year,to=end_year,by=5))+
    scale_y_continuous(limits = c(0,max ),labels = comma)+
    scale_shape_discrete(guide=FALSE)+
    labs(x='Year',y= paste(em,'Emissions [kt]'))+
    theme(legend.title=element_blank())
  plot
  plot_list[[i]]<-plot
}

pdf(paste0('../diagnostic-output/ceds-comparisons/sector-level/GAINS_',em,'_Regional_Comparison_Residential.pdf'),width=12,height=10,paper='special')
grid.arrange(plot_list[[1]],plot_list[[2]],
             plot_list[[3]],plot_list[[4]],
             plot_list[[5]],plot_list[[6]], ncol=2,
             top = paste('GAINS vs CEDS - Regional',em,'Emissions (Residential)'))
dev.off()



# Non-Residential
region_long_nonresid <- global_long_nonresid
region_wide_nonresid <- cast(region_long_nonresid, Region + Sector + inv ~ year, value = 'total')
region_wide_nonresid <- region_wide_nonresid[c( 'Region', 'Sector', 'inv', gains_years)]

#write out table
writeData(region_wide_nonresid,'DIAG_OUT', paste0('GAINS_',em,'_Regional_Comparison_Non-Residential'),domain_extension = 'ceds-comparisons/sector-level/',meta=F)

regions_list <- region_long_nonresid[which(region_long_nonresid$year == '2000'),c('Region','total')]
regions_list <- regions_list[order(-regions_list$total),]
regions_list_order <- unique(regions_list$Region)
regions_df_order <- data.frame(Region=regions_list_order,
                               plot = c(unlist(lapply(X=1:4,FUN=rep, times=4)),
                                        unlist(lapply(X=5,FUN=rep, times=5)),
                                        unlist( lapply( X = 6, FUN = rep, times =  3 ) ) ) )

plot_list <- list()
for(i in 1:6){

  plot_regions <- regions_df_order [which(regions_df_order$plot == i),'Region']

  plot_df <- region_long_nonresid[which(region_long_nonresid$Region %in% plot_regions),c('inv','year','Region','total')]
  plot_df$inv <- as.factor(plot_df$inv)
  plot_df$region <- as.factor(plot_df$Region)
  max <- 1.2*(max(plot_df$total))

  plot <- ggplot(plot_df, aes(x=year,y=total, color = region, shape=inv)) +
    geom_point(data = subset(plot_df, inv =='GAINS'),size=2,aes(x=year,y=total, color = Region)) +
    geom_line(data = subset(plot_df, inv =='CEDS'),size=1,aes(x=year,y=total, color = Region)) +
    scale_x_continuous(breaks=seq(from=ceds_start_year,to=end_year,by=5))+
    scale_y_continuous(limits = c(0,max ),labels = comma)+
    scale_shape_discrete(guide=FALSE)+
    labs(x='Year',y= paste(em,'Emissions [kt]'))+
    theme(legend.title=element_blank())
  plot
  plot_list[[i]]<-plot
}

pdf(paste0('../diagnostic-output/ceds-comparisons/sector-level/GAINS_',em,'_Regional_Comparison_Non-Residential.pdf'),width=12,height=10,paper='special')
grid.arrange(plot_list[[1]],plot_list[[2]],
             plot_list[[3]],plot_list[[4]],
             plot_list[[5]],plot_list[[6]], ncol=2,
             top = paste('GAINS vs CEDS - Regional',em,'Emissions (Non-Residential)'))
dev.off()


# ---------------------------------------------------------------------------
# 5. End

logStop()
