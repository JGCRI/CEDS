# Program Name: Compare_to_CDIAC_plot.R
# Author: Linh Vu
# Date Last Updated: March 20, 2020
# Program Purpose: Generate figures and tables to compare CEDS to CDIAC for CO2
# Input Files: CO2_total_CEDS_emissions.csv, E.CO2_CDIAC_inventory.csv,
#              Master_Country_List.csv, RCP Region Mapping.xlsx,
#              IEA_product_fuel.csv
# Output Files: CDIAC_CO2_Regional_Comparison_[plot_regions].pdf,
#               CDIAC_CO2_Regional_Comparison-FigureRegion[i].pdf,
#               CDIAC_CO2_Regional_Comparison_[i].pdf, CO2_CEDS_vs_CDIAC_global_ratio.pdf,
#               CDIAC_CO2_Regional_Comparison_All.pdf, CDIAC_[em]_Regional_Comparison_All_diff.pdf,
#               CO2_Regional_Comparison_CDIAC-all.csv, CO2_CEDS_vs_CDIAC_by_RCP_region.csv,
#               CO2_CEDS_vs_CDIAC_by_figure_region.csv, CO2_CEDS_vs_CDIAC_ctry_fuel.csv,
#               CO2_CEDS_vs_CDIAC_fuel_pc.csv, CO2_CEDS_vs_CDIAC_fuel_abs.csv,
#               CO2_CEDS_vs_CDIAC_global_ratio.csv, CO2_Regional_Comparison_CDIAC-[plot_regions].csv
# Notes:
# TODO: (Future) Change to tidyverse (cast, melt, match, merge)
# TODO: (Future) Include at the end of CO2 runs?
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( 'data_functions.R' )
#                 Additional function files may be required.
log_msg <- "Comparing CEDS and CDIAC final emissions for CO2..."
script_name <- 'Compare_to_CDIAC_plot.R'

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "CO2"

# ---------------------------------------------------------------------------
# 0.5 Load Packages
library('scales') # TODO: (Future) Potentially just add this to CEDS required packages in global_settings.R
#                                 as it is loaded elsewhere as well in CEDS / CEDS_Data.

# ---------------------------------------------------------------------------
# 1. Input
ceds_in <- readData( "MED_OUT", "CO2_total_CEDS_emissions" )
cdiac_in <- readData( "MED_OUT", "E.CO2_CDIAC_inventory" )
MCL <- readData( "MAPPINGS", "Master_Country_List" )
Map_region_codes <- readData( "EM_INV", domain_extension = 'RCP/',"RCP Region Mapping", ".xlsx", sheet_selection = 'Reg Codes' )
Map_iso_codes <- readData( "EM_INV", domain_extension = 'RCP/',"RCP Region Mapping", ".xlsx", sheet_selection = 'EDGAR32 & IEA' )
IEA_product_fuel <- readData( "MAPPINGS", "IEA_product_fuel", domain_extension = "energy/" )

bunker_sectors <- c( "1A3di_International-shipping", "1A3aii_Domestic-aviation", "1A3ai_International-aviation",
                     "1A3dii_Domestic-navigation", "1A3di_Oil_tanker_loading" )

cdiac_fuels <- c( "biomass", "solid_fuels", "liquid_fuels", "gas_fuels",
                  "cement_production", "bunker_fuels" )

# ---------------------------------------------------------------------------
# 2. Compare
X_common_years <- paste0( "X", cdiac_start_year : cdiac_end_year )

# Clean up CDIAC. Add other_process = 0
cdiac <- cdiac_in[ c( "iso", "fuel", X_common_years ) ]
cdiac <- dplyr::filter( cdiac, iso %in% ceds_in$iso, fuel %in% cdiac_fuels )
cdiac_add <- cdiac[ c( "iso", "fuel" ) ]
cdiac_add$fuel <- "other_process"
cdiac_add <- unique(cdiac_add)
cdiac_add[ X_common_years ] <- 0
cdiac <- dplyr::bind_rows( cdiac, cdiac_add ) %>% dplyr::arrange( iso, fuel )
cdiac[, X_common_years ] <- cdiac[, X_common_years ] * conversionFactor_C_CO2

# Clean up ceds
ceds <- ceds_in[ c( "iso", "sector", "fuel", "units", X_common_years)]
ceds$fuel_ref <- ceds$fuel

ceds <- ceds %>%
    dplyr::left_join( IEA_product_fuel[ , c( "cdiac_fuel", "fuel" ) ] %>% dplyr::distinct( ),
                      by = "fuel" ) %>%
    dplyr::select( -fuel ) %>%
    dplyr::rename( fuel = cdiac_fuel ) %>%
    dplyr::select( iso, sector, fuel, units, X_common_years, fuel_ref )

ceds$fuel[ ceds$sector == "2A1_Cement-production" ] <- "cement_production"
ceds$fuel[ ceds$sector %in% bunker_sectors &
                ceds$fuel == "liquid_fuels" ] <- "bunker_fuels"
ceds <- dplyr::filter( ceds, iso %in% cdiac$iso, fuel %in% cdiac$fuel | fuel_ref == "process" )
ceds$fuel[ is.na( ceds$fuel ) ] <- "other_process"
ceds$fuel_ref <- NULL
# ceds <- ceds[-which(ceds$sector == '7A_Fossil-fuel-fires'),]

# Aggregate ceds by fuel
ceds <- ceds %>%
    dplyr::select( iso, fuel, X_common_years ) %>%
    dplyr::group_by( iso, fuel ) %>%
    dplyr::summarize_all( funs( sum( ., na.rm = T ) ) ) %>%
    data.frame()

# Total emissions by country+fuel
cdiac_long <- melt( cdiac, id=c( "iso", "fuel" ) )# TODO: (Future) Replace melt with gather
names( cdiac_long ) <- c( "iso", "fuel", "year", "cdiac" )

ceds_long <- melt( ceds, id=c( "iso", "fuel" )) # TODO: (Future) Replace melt with gather
names( ceds_long ) <- c( "iso", "fuel", "year", "ceds" )

cmp_ctry_fuel <- merge( cdiac_long, ceds_long ) %>% # TODO: (Future) Replace merge with left_join
  dplyr::mutate( diff = ceds - cdiac,
                 diff_pc = round((diff/ceds)*100) )
cmp_ctry_fuel$diff_pc[ cmp_ctry_fuel$ceds == cmp_ctry_fuel$cdiac ] <- 0
cmp_ctry_fuel <- dplyr::filter( cmp_ctry_fuel, iso != "global" )
#cmp_ctry_fuel_wide <- cast( cmp_ctry_fuel, iso+fuel~year, value = "diff_pc" )

# Total emissions by fuel
cmp_fuel <- dplyr::group_by( cmp_ctry_fuel, fuel, year ) %>%
  dplyr::summarise( cdiac = sum( cdiac ), ceds = sum( ceds ) ) %>%
  dplyr::mutate( diff = ceds - cdiac,
                 diff_pc = round((diff/ceds)*100) )
cmp_fuel$diff_pc[ cmp_fuel$ceds == cmp_fuel$cdiac ] <- 0
cmp_fuel_wide_pc <- cast( cmp_fuel, fuel~year, value = "diff_pc" ) # TODO: (Future) Replace cast with spread
cmp_fuel_wide_abs <- cast( cmp_fuel, fuel~year, value = "diff" )   # TODO: (Future) Replace cast with spread

# Total emissions by ctry
cmp_ctry <- dplyr::group_by( cmp_ctry_fuel, iso, year ) %>%
  dplyr::summarise( cdiac = sum( cdiac ), ceds = sum( ceds ) ) %>%
  dplyr::mutate( diff = ceds - cdiac,
                 diff_pc = round((diff/ceds)*100) )
cmp_ctry$diff_pc[ cmp_ctry$ceds == cmp_ctry$cdiac ] <- 0
cmp_ctry_wide <- cast( cmp_ctry, iso~year, value = "diff_pc" ) # TODO: (Future) Replace cast with spread

# ---------------------------------------------------------------------------
# Make plots: CEDS vs CDIAC, with and without Germany
cmp_global_all <- dplyr::group_by( cmp_ctry_fuel, year ) %>%
  dplyr::summarise( cdiac = sum( cdiac ), ceds = sum( ceds ) ) %>%
  dplyr::mutate( ceds_over_cdiac_all = ceds/cdiac )
cmp_global_all$ceds_over_cdiac_all[ cmp_global_all$cdiac == cmp_global_all$ceds ] <- 1

cmp_global_no_deu <- dplyr::filter( cmp_ctry_fuel, iso != "deu" ) %>%
  dplyr::group_by( year ) %>%
  dplyr::summarise( cdiac = sum( cdiac ), ceds = sum( ceds ) ) %>%
  dplyr::mutate( ceds_over_cdiac_no_deu = ceds/cdiac )
cmp_global_no_deu$ceds_over_cdiac_no_deu[ cmp_global_no_deu$cdiac == cmp_global_no_deu$ceds ] <- 1

cmp_global_both <- merge( select( cmp_global_all, year, ceds_over_cdiac_all ),   # TODO: (Future) Replace merge with left_join
                          select( cmp_global_no_deu, year, ceds_over_cdiac_no_deu ) )
cmp_global_both <- melt( cmp_global_both, id="year" ) # TODO: (Future) Replace melt with gather
names( cmp_global_both ) <- c( "year", "flow", "ceds_over_cdiac" )
cmp_global_both$year <- substr(cmp_global_both$year, 2, 5)
cmp_global_both$year <- as.numeric( as.character( cmp_global_both$year ) )

CEDS_v_CDIAC_glb_ratio_plot <- ggplot(cmp_global_both, aes(x=year,y=ceds_over_cdiac, color = flow)) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks=seq(from=cdiac_start_year,to=cdiac_end_year,by=20)) +
  labs(x='Year',y= paste("CEDS/CDIAC")) +
  theme( legend.position="bottom" )

# ---------------------------------------------------------------------------
# Plot by RCP regions

# Create complete region map for ceds to RCP
complete_region_map <- merge(Map_iso_codes, Map_region_codes, # TODO: (Future) Replace merge with left_join
                             by.x= "RCP Template Reg #",
                             by.y=, 'RCP Template Reg Code')
complete_region_map$Region <- gsub(" [(]Rest of[)]","",complete_region_map$Region)
complete_region_map$Region <- gsub(" [(]Estonia, Latvia, Lithuania[)]","",complete_region_map$Region)
complete_region_map$Region <- gsub(" [(]Republic of Korea[)]","",complete_region_map$Region)
complete_region_map$Region <- gsub(" [(]Democratic People's Republic of Korea[)]","",complete_region_map$Region)
complete_region_map[which(complete_region_map$Code == 'GRL'),'Region'] <- 'Greenland'
complete_region_map$Region <- gsub(" $","", complete_region_map$Region, perl=T)

complete_region_map[which( complete_region_map$Region %in% c('Russia+','Ukraine+')),'Region'] <- 'FSU'


# Drop bunker sectors. Aggregate by RCP Region
cmp_region <- dplyr::filter( cmp_ctry_fuel, fuel != "bunker_fuels" ) %>%
  dplyr::group_by( iso, year ) %>%
  dplyr::summarise( cdiac = sum( cdiac ), ceds = sum( ceds ) )
cmp_region$Region <- complete_region_map[match(cmp_region$iso,tolower(complete_region_map$Code)),'Region'] # TODO: (Future) Replace match with left_join
cmp_region[which(is.na(cmp_region$Region)),'Region']<- 'Not Mapped'
cmp_region <- dplyr::group_by( cmp_region, Region, year ) %>%
dplyr::summarise( ceds = sum( ceds ), cdiac = sum( cdiac ) ) %>% data.frame()
region_long <- melt( cmp_region, id = c( "Region", "year" ) ) # TODO: (Future) Replace melt with gather
names( region_long ) <- c( "region", "year", "inv", "total_emissions" )
region_long$year <- gsub( "X", "", region_long$year )
region_long$year <- as.numeric( region_long$year )
region_long <- dplyr::filter( region_long, year>=1850 )
region_long <- dplyr::arrange( region_long, inv, year )

# Plot
regions_list <- region_long[,c('region','total_emissions')]
regions_list <- regions_list[order(-regions_list$total_emissions),]
regions_list_order <- unique(regions_list$region)

#5 seperate graphs, saved individually
df_all_regions <- data.frame()
for(i in 1:6){
  plot_regions <- regions_list_order[(i*6-5):(i*6)]

  plot_df <- region_long[which(region_long$region %in% plot_regions),c('inv','year','region','total_emissions')]
  plot_df$inv <- as.factor(plot_df$inv)
  plot_df$region <- as.factor(plot_df$region)
  plot_df$total_emissions <-plot_df$total_emissions/1000
  max <- 1.2*(max(plot_df$total_emissions))

  plot <- ggplot(plot_df, aes(x=year,y=total_emissions, color = region, shape=inv)) +
    geom_line(data = subset(plot_df, inv =='cdiac'),size=.7,aes(x=year,y=total_emissions, color = region),linetype = 1) +
    geom_line(data = subset(plot_df, inv =='ceds'),size=.7,aes(x=year,y=total_emissions, color = region),linetype = 2) +
    scale_x_continuous(breaks=seq(from=1850,to=cdiac_end_year,by=30))+
    scale_y_continuous(limits = c(0,max ),labels = comma)+
    scale_shape_discrete(guide=FALSE)+
    labs(x='Year',y= paste(em,'Emissions [Tg]'))+
    theme(legend.title=element_blank())
  plot

  # calculate difference and arrange table
  df_temp <- as.data.frame(cast(plot_df, region+year~inv, value = 'total_emissions')) # TODO: (Future) Replace cast with spread
  df_temp$diff <- df_temp$ceds - df_temp$cdiac
  df_temp <- melt(df_temp, measure.vars = c("cdiac", "ceds","diff")) # TODO: (Future) Replace melt with gather
  names(df_temp)[which(names(df_temp) == 'variable')] <- 'inv'

  df_out <- cast(df_temp, region+inv~year, value = 'total_emissions') # TODO: (Future) Replace cast with spread

  writeData(df_out, 'DIAG_OUT', paste0('CO2_Regional_Comparison_CDIAC-',
                           paste(plot_regions,collapse ='-' )), meta = F)
  df_all_regions <- rbind.fill(df_all_regions,df_out)

  ggsave( paste0('../diagnostic-output/ceds-comparisons/CDIAC_',em,'_Regional_Comparison_',
                 paste(plot_regions,collapse ='-' ),
                 '.pdf') , width = 7, height = 4)
}

#5 seperate graphs, saved together
plot_list <- list()
for(i in 1:6){

  plot_regions <- regions_list_order[(i*6-5):(i*6)]

  plot_df <- region_long[which(region_long$region %in% plot_regions),c('inv','year','region','total_emissions')]
  plot_df$inv <- as.factor(plot_df$inv)
  plot_df$region <- as.factor(plot_df$region)
  max <- 1.2*(max(plot_df$total_emissions))

  plot <- ggplot(plot_df, aes(x=year,y=total_emissions, color = region, shape=inv)) +
    geom_line(data = subset(plot_df, inv =='cdiac'),size=.7,aes(x=year,y=total_emissions, color = region),linetype = 1) +
    geom_line(data = subset(plot_df, inv =='ceds'),size=.7,aes(x=year,y=total_emissions, color = region),linetype = 2) +
    scale_x_continuous(breaks=seq(from=1850,to=cdiac_end_year,by=30))+
    scale_y_continuous(limits = c(0,max ),labels = comma)+
    scale_shape_discrete(guide=FALSE)+
    labs(x='Year',y= paste(em,'Emissions [kt]'))+
    theme(legend.title=element_blank())
  plot

  plot_list[[i]]<-plot
}

# ---------------------------------------------------------------------------
# Plot by Figure Regions

# Drop bunker sectors. Aggregate by Figure Region
cmp_region_figure <- dplyr::filter( cmp_ctry_fuel, fuel != "bunker_fuels" ) %>%
  dplyr::group_by( iso, year ) %>%
  dplyr::summarise( cdiac = sum( cdiac ), ceds = sum( ceds ) )
cmp_region_figure$Region <- MCL$Figure_Region[match(cmp_region_figure$iso, MCL$iso)] # TODO: (Future) Replace match with left_join
cmp_region_figure <- dplyr::group_by( cmp_region_figure, Region, year ) %>%
dplyr::summarise( ceds = sum( ceds ), cdiac = sum( cdiac ) ) %>% data.frame()
region_long_figure <- melt( cmp_region_figure, id = c( "Region", "year" ) ) # TODO: (Future) Replace melt with gather
names( region_long_figure ) <- c( "region", "year", "inv", "total_emissions" )
region_long_figure$year <- gsub( "X", "", region_long_figure$year )
region_long_figure$year <- as.numeric( region_long_figure$year )
region_long_figure <- dplyr::filter( region_long_figure, year>=1850 )
region_long_figure <- dplyr::arrange( region_long_figure, inv, year )

# Plot
regions_list <- region_long_figure[,c('region','total_emissions')]
regions_list <- regions_list[order(-regions_list$total_emissions),]
regions_list_order <- unique(regions_list$region)

# Seperate graphs, saved individually
for(i in 1:2){
  plot_regions <- regions_list_order[(i*5-4):(i*5)]

  plot_df <- region_long_figure[which(region_long_figure$region %in% plot_regions),c('inv','year','region','total_emissions')]
  plot_df$inv <- as.factor(plot_df$inv)
  plot_df$region <- as.factor(plot_df$region)
  max <- 1.2*(max(plot_df$total_emissions))

  plot <- ggplot(plot_df, aes(x=year,y=total_emissions, color = region, shape=inv)) +
    geom_line(data = subset(plot_df, inv =='cdiac'),size=1,aes(x=year,y=total_emissions, color = region),linetype = 1) +
    geom_line(data = subset(plot_df, inv =='ceds'),size=1,aes(x=year,y=total_emissions, color = region),linetype = 2) +
    scale_x_continuous(breaks=seq(from=1850,to=cdiac_end_year,by=30))+
    scale_y_continuous(limits = c(0,max ),labels = comma)+
    scale_shape_discrete(guide=FALSE)+
    labs(x='Year',y= paste(em,'Emissions [kt]'))+
    theme(legend.title=element_blank())
  plot

  ggsave( paste0('../diagnostic-output/CDIAC_',em,'_Regional_Comparison',
                 '-FigureRegion', i, '.pdf') , width = 7, height = 4)
}

# }
#

# ---------------------------------------------------------------------------
# cdiac comparison by region
FSU <- unique( MCL[which(MCL$Figure_Region == 'FSU'),'iso'] )
FSU_iso <- FSU[which( FSU %!in% 'ussr')]

cmp_ctry <- dplyr::group_by( cmp_ctry_fuel, iso, year ) %>%
  dplyr::summarise( cdiac = sum( cdiac ), ceds = sum( ceds ) ) %>%
  dplyr::mutate( diff = cdiac-ceds,
          diff_pc = round((diff/ceds)*100) )

compare <- cmp_ctry

compare[which(compare$iso %in% FSU_iso),'iso'] <- 'FSU'
compare <- compare %>%
  dplyr::group_by(iso,year) %>%
  dplyr::summarise_if(is.numeric,sum) %>%
  dplyr::select(iso, year, diff) %>%
  dplyr::mutate(year = as.numeric(gsub('X',"",year))) %>%
  dplyr::filter(year >1900)

#5 seperate graphs, saved together
plot_list <- list()

# Plot
regions_list <- compare[,c('iso','diff')]
regions_list <- regions_list[order(-abs(regions_list$diff)),]
regions_list_order <- unique(regions_list$iso)

for(i in 1:6){

  plot_regions <- regions_list_order[(i*6-5):(i*6)]
  plot_df <- compare[which(compare$iso %in% plot_regions),c('year','iso','diff')]
  max <- 1.2*(max(plot_df$diff))
  min <- 1.2*(min(plot_df$diff))

  plot <- ggplot(plot_df, aes(x=year,y=diff, color = iso)) +
    geom_line()+
    scale_x_continuous(breaks=seq(from=1900,to=cdiac_end_year,by=20))+
    scale_y_continuous(limits = c(min,max ),labels = comma)+
    scale_shape_discrete(guide=FALSE)+
    labs(x='Year',y= paste(em,'Emissions [kt]'))+
    theme(legend.title=element_blank())
  plot
  ggsave(paste0('../diagnostic-output/ceds-comparisons/CDIAC_',em,'_Regional_Comparison_',i,'.pdf'),
          width=6, height=4)
  plot_list[[i]]<-plot
}

# ---------------------------------------------------------------------------
# Output

# Plots

#   CO2_CEDS_vs_CDIAC_global_ratio.pdf
    ggsave( '../diagnostic-output/CO2_CEDS_vs_CDIAC_global_ratio.pdf', plot = CEDS_v_CDIAC_glb_ratio_plot,
        width = 14, height = 7 )

#   CDIAC_[em]_Regional_Comparison_All.pdf
    pdf(paste0('../diagnostic-output/ceds-comparisons/CDIAC_',em,'_Regional_Comparison_All.pdf'),width=12,height=10,paper='special')
    grid.arrange(plot_list[[1]],plot_list[[2]],
             plot_list[[3]],plot_list[[4]],
             plot_list[[5]],plot_list[[6]], ncol=2,
             top = paste('CDIAC vs CEDS - Regional',em,'Emissions'))
    dev.off()

#   CDIAC_[em]_Regional_Comparison_All_diff.pdf
    pdf(paste0('../diagnostic-output/ceds-comparisons/CDIAC_',em,'_Regional_Comparison_All_diff.pdf'),width=12,height=10,paper='special')
    grid.arrange(plot_list[[1]],plot_list[[2]],
             plot_list[[3]],plot_list[[4]],
             plot_list[[5]],plot_list[[6]], ncol=2,
             top = paste('CDIAC-CEDS : Regional',em,'Differences'))
    dev.off()

# Data
writeData(df_all_regions, 'DIAG_OUT', '/ceds-comparisons/CO2_Regional_Comparison_CDIAC-all' )
writeData( region_long, "DIAG_OUT", "/CO2_CEDS_vs_CDIAC_by_RCP_region" )
writeData( region_long_figure, "DIAG_OUT", "/CO2_CEDS_vs_CDIAC_by_figure_region" )
writeData( cmp_ctry_fuel, "DIAG_OUT", "/CO2_CEDS_vs_CDIAC_ctry_fuel" )
writeData( cmp_fuel_wide_pc, "DIAG_OUT", "/CO2_CEDS_vs_CDIAC_fuel_pc" )
writeData( cmp_fuel_wide_abs, "DIAG_OUT", "/CO2_CEDS_vs_CDIAC_fuel_abs" )
writeData( cmp_global_both, "DIAG_OUT", "/CO2_CEDS_vs_CDIAC_global_ratio" )


logStop()
