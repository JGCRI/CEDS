# ------------------------------------------------------------------------------
# Program Name: Compare_to_RCP_GAINS_EDGAR.R
# Author(s): Huong Nguyen, Leyang Feng
# Date Last Updated: 14 November 2016
# Program Purpose: Produces diagnostic summary figures of global emissions for four inventories
# Input Files: [em]_total_CEDS_emissions.csv, emissions_scaling_functions.R, F.[em]_scaled_EF.csv, F.[em]_scaled_emissions.csv, JRC_PEGASOS_[em]_TS_REF.xlsx

# Output Files: Figures in the diagnostic-output
# Notes:
# TODO: 1. add CO2 into CEDS em list once avaiable
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers

# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provides logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R",'process_db_functions.R',
              'common_data.R', 'IO_functions.R', 'data_functions.R', 'timeframe_functions.R')# Any additional function files required
log_msg <- "Compare CEDS emissions to RCP, GAINS and EDGAR emissions"
script_name <- "Compare_to_RCP_GAINS_EDGAR.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

# ------------------------------------------------------------------------------
# 0.5 Load Packages

library('ggplot2')
library('plyr')
library('scales')
library('gridExtra')

# 0.5. Script Options

# Get emission species first so can name log appropriately
args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "CH4"

rcp_start_year <- 1850
rcp_end_year <- 2000
edgar_start_year <- 1970
edgar_end_year <- 2010
if (em == 'CH4') edgar_end_year <- 2008
ceds_start_year <- 1850
ceds_end_year <- end_year
gains_start_year <- 2000
gains_end_year <- 2020

rcp_years <- seq( from = rcp_start_year, to = rcp_end_year, by = 10 )
x_rcp_years <- paste0( 'X', rcp_years )
edgar_years<- edgar_start_year : edgar_end_year
x_edgar_years<- paste0( 'X', edgar_years )
gains_years <- c( 2000, 2005, 2010, 2020 )
x_gains_years <- paste0( 'X', gains_years )
ceds_years <- ceds_start_year : ceds_end_year
x_ceds_years <- paste0( 'X', ceds_years )

plot_start_year <- 1970
plot_end_year <- 2020
plot_years <- plot_start_year : plot_end_year
x_plot_years <- paste0( 'X', plot_years )

# --------------------------------------------
# 0.8 default emission setup and emission availability check

ceds_em_list <- c( 'SO2', 'NOx', 'NH3', 'NMVOC', 'OC', 'BC', 'CO','CH4' )
gains_em_list <- c( 'SO2', 'NOx', 'NMVOC', 'BC', 'OC', 'CH4', 'CO', 'CO2' ,'CH4')
edgar_em_list <- c( 'BC', 'CO', 'NH3', 'NMVOC', 'NOx', 'OC', 'SO2' ,'CH4')
rcp_em_list <- c( 'SO2', 'NOx', 'NH3', 'NMVOC', 'OC', 'BC', 'CO','CH4' )
rcp_shipping_em_list <- c( "NOx", "SO2", "NMVOC", "BC", "OC", "CO" )

ceds_em_flag <- em %in% ceds_em_list
gains_em_flag <- em %in% gains_em_list
edgar_em_flag <- em %in% edgar_em_list
rcp_em_flag <- em %in% rcp_em_list
rcp_shipping_em_flag <- em %in% rcp_shipping_em_list
# ------------------------------------------------------------------------------
# 1. Read in and load files for CEDS, EDGAR and GAINS ( RCP will be loaded in section 2 )
# if the em is not supportted by any of the emissions inventories, a dummy data frame will be created
# ------------------------------------------------------------------------------
# 1.1 read in CEDS total emissions
if ( ceds_em_flag == T ) {
ceds_emissions <- readData( 'MED_OUT', paste0( em, '_total_CEDS_emissions' ) )
} else {
  printLog( paste0( em, ' is not supportted by CEDS, dummy data created. ' ) )
  ceds_dummy <- data.frame( em = em, inventory = 'CEDS', year = plot_years, total_emissions = NA )
  }
#-------------------------------------------------------------------------------
# 1.2 Read in and load files for EDGAR
if ( edgar_em_flag == T ) {
  if (em != 'CH4'){
  # construct sheet name, for BC and OC the sheet name is slightly different
  edgar_sheet_name <- paste0( "NEW_v4.3_EM_" ,em, "_ref" )
  if ( em %in% c( 'BC', 'OC' ) ) { edgar_sheet_name <- paste0( "NEW_v4.3_EM_" ,em, "_hindc") }
  # read in the edgar data
  edgar_emissions <- readData( domain = "EM_INV",
                               domain_extension = "EDGAR/",
                               file_name = paste0( "JRC_PEGASOS_" ,em, "_TS_REF" ),
                               extension = ".xlsx",
                               sheet_selection = edgar_sheet_name,
                               skip = 8 )
  }
  if (em == 'CH4'){
    edgar_emissions <- readData( domain = "EM_INV",
                                 domain_extension = "EDGAR/",
                                 file_name = 'EDGAR42_CH4')

  }
  } else {
    printLog( paste0( em, ' is not supportted by EDGAR, dummy data created. ' ) )
    edgar_dummy <- data.frame( em = em, inventory = 'EDGAR', year = plot_years, total_emissions = NA )
    }

#-------------------------------------------------------------------------------
# 1.3 Read in and load files for GAINS
if ( gains_em_flag == T ) {
  # construct sheet name, the sheet name for NMVOC is slightly different than others
  gains_sheet_name <- em
  if ( em == 'NMVOC' ) { gains_sheet_name <- 'VOC' }
  # read in GAINS data
  gains_emissions <- readData( domain = 'EM_INV',
                               domain_extension = 'GAINS/',
                               file_name = 'GAINS_EMF30_EMISSIONS_extended_Ev5a_CLE_Nov2015',
                               extension = ".xlsx",
                               sheet_selection = gains_sheet_name )
} else {
  printLog( paste0( em, ' is not supportted by GAINS, dummy data created. ' ) )
  gains_dummy <- data.frame( em = em, inventory = 'GAINS', year = plot_years, total_emissions = NA )
}

# ------------------------------------------------------------------------------
# 2. Read in and load files for RCP
# 2.1 Load RCP emissions ( shipping emissions excluded which will be load in section 2.2 )
# Load and process RCP files
# set wd to RCP folder
if ( rcp_em_flag == T ) {
  rcp_dir <- './emissions-inventories/RCP/'

  # create temporary folder to extract zipped files
  zipfile_path <- paste0(rcp_dir, em, '.zip')
  dir.name <- paste0(rcp_dir, em, '_RCP_temp_folder')
  dir.create(dir.name)
  # unzip files to temp folder
  unzip(zipfile_path, exdir = dir.name)

  # list files in the folder
  files <- list.files(paste0(dir.name,'/',em)  ,pattern = '.dat')
  files <- paste0(dir.name,'/',em,'/',files)

  rcp_files <- list()
  for (i in seq_along(rcp_years)){
    rcp_files[i] <- files[grep(rcp_years[i], files)]
  }
  rcp_files <- unlist(rcp_files)

  RCP_df_list <- lapply(X=rcp_files,FUN=read.table,strip.white = TRUE,header=TRUE,skip = 4,fill=TRUE, stringsAsFactors = FALSE)

  for (i in seq_along(rcp_years)){
    RCP_df_list[[i]]$year <- rcp_years[i]
  }
  RCP_emissions <- do.call("rbind", RCP_df_list)

  # delete temp folder
  unlink(dir.name,recursive = TRUE)

} else {
  printLog( paste0( em, ' is not supportted by RCP, dummy data created. ' ) )
  rcp_dummy <- data.frame( em = em, inventory = 'RCP', year = plot_years, total_emissions = NA )
}
# ----------------------------------------------------
# 2.2 load RCP international shipping emissions
if ( rcp_shipping_em_flag == T ) {
rcp_shipping_emissions <- readData( domain = 'EM_INV',
                                    file_name = 'Historicalshipemissions_IPCC_FINAL_Jan09_updated_1850',
                                    domain_extension = 'RCP/',
                                    extension = '.xlsx',
                                    sheet_selection = 'CO2Emis_TgC',
                                    skip = 8 )[ 1:140, 1:12 ]
} else {
  printLog( paste0( em, ' is not supportted by RCP shipping data, dummy data created. ' ) )
}
# ---------------------------------------------------------------------------
# 3. setup sectors need to be dropped for each inventory

# Non Comparable Sectors
rcp_remove_sectors <- c('AWB','Tot_Ant')
ceds_remove_sectors <- c("1A3ai_International-aviation",
                         '1A3aii_Domestic-aviation',
                         '7A_Fossil-fuel-fires',
                         '3F_Agricultural-residue-burning-on-fields',
                         '11A_Volcanoes',
                         '11B_Forest-fires',
                         '11C_Other-natural',
                         '6B_Other-not-in-total')
# RCP shipping is not available for NH3
if ( rcp_shipping_em_flag == FALSE ) {
  ceds_remove_sectors <- c( "1A3ai_International-aviation",
                            "1A3di_International-shipping",
                            '1A3aii_Domestic-aviation',
                            '7A_Fossil-fuel-fires',
                            '3F_Agricultural-residue-burning-on-fields',
                            '11A_Volcanoes',
                            '11B_Forest-fires',
                            '11C_Other-natural',
                            '6B_Other-not-in-total')

}

edgar_remove_sectors <- c( '1A3a',
                           '4F',
                           '1C1' )

gains_remove_sectors <- c( 'Sum' )

# --------------------------------------------------------------------------
# 4. process data for each inventory
#---------------------------------------------------------------------------
# 4.1 process data for CEDS
if ( ceds_em_flag ) {
  # drop uncomparable sectors
  ceds_comparable <- ceds_emissions
  ceds_comparable$em <- em
  ceds_comparable <- ceds_comparable[ -which( ceds_comparable$sector %in% ceds_remove_sectors ), ]
  # generate ceds_global emissions
  ceds_global <- aggregate( ceds_comparable[ , x_ceds_years ],
                            by = list( ceds_comparable$em ),
                            FUN=sum )
  colnames( ceds_global ) <- c( 'em', x_ceds_years )
  # change the ceds_gloabl in to plot_years layout
  x_years_to_add <- x_plot_years[ which( x_plot_years %!in% x_ceds_years ) ]
  ceds_global[ , x_years_to_add ] <- NA
  ceds_global <- ceds_global[ , c( 'em', x_plot_years ) ]

  ceds_global$inventory <- 'CEDS'
  ceds_global_long <- melt( ceds_global, id.vars = c( 'em', 'inventory' ) )
  ceds_global_long$variable <- as.numeric( substr( ceds_global_long$variable, 2, 5 ) )
  colnames( ceds_global_long ) <- c( 'em', 'inventory', 'year', 'total_emissions' )

  ceds_plot <- ceds_global_long

  # additional step to extract ceds shipping emissions for later use in section 4.4
  ceds_shipping <- ceds_emissions
  ceds_shipping$em <- em
  ceds_shipping <- ceds_shipping[ ceds_shipping$sector == "1A3di_International-shipping", ]
  if ( nrow( ceds_shipping ) > 0 ) {
    ceds_shipping <- aggregate( ceds_shipping[ , x_ceds_years ],
                                by = list( ceds_shipping$em ),
                                FUN = sum  )
    colnames( ceds_shipping ) <- c( 'em', x_ceds_years )
    # change the ceds_shipping in to plot_years layout
    x_years_to_add <- x_plot_years[ which( x_plot_years %!in% x_ceds_years ) ]
    ceds_shipping[ , x_years_to_add ] <- NA
    ceds_shipping <- ceds_shipping[ , c( 'em', x_plot_years ) ]
    ceds_shipping<- melt( ceds_shipping, id.vars = c( 'em' ) )
    ceds_shipping$variable <- as.numeric( substr( ceds_shipping$variable, 2, 5 ) )
    colnames( ceds_shipping ) <- c( 'em', 'year', 'shipping_em' )
    ceds_shipping <- ceds_shipping[ , c( 'year', 'shipping_em' ) ]
  } else {
    ceds_shipping <- data.frame( year = plot_years, shipping_em = NA )
  }
} else {
  ceds_plot <- ceds_dummy
  ceds_shipping <- data.frame( year = plot_years, shipping_em = NA )
}
# ------------------------------------------------------------------------------
# 4.2 process data for RCP
# ------------------------------------------------------------------------------
# 4.2.1 process RCP data ( shipping excluded )
if ( rcp_em_flag == T ) {

  # ECP data cleaning up
  RCP <- RCP_emissions
  names(RCP)[which(names(RCP)== 'Tot.')] <- "Tot_Ant"
  names(RCP)[which(names(RCP)== 'Ant.')] <- "Region_Name_1"
  names(RCP)[which(names(RCP)== 'Region.1')] <- "Region_Name_2"

  RCP$Region_Name_2 <- gsub("(Rest","",RCP$Region_Name_2,fixed=TRUE)
  RCP$Region_Name <- paste(RCP$Region_Name_1,RCP$Region_Name_2)

  RCP <- RCP[,c('Region','Subregion',"Region_Name","ENE","IND","TRA","DOM","SLV","AGR","AWB","WST","Tot_Ant",'year')]

  RCP <- RCP[which(complete.cases(RCP)),]

  RCP$ENE <- as.numeric(RCP$ENE)
  RCP$year <- paste0('X',RCP$year)

  RCP_long <- melt(RCP, id.vars = c('Region','Subregion','Region_Name','year'))
  RCP <- cast( RCP_long , Region + Subregion + Region_Name + variable ~ year)
  RCP$em <- em

  names(RCP)[which(names(RCP) == 'Region')] <- 'Region_code'
  names(RCP)[which(names(RCP) == 'Subregion')] <- 'Subregion_code'
  names(RCP)[which(names(RCP) == 'Region_Name')] <- 'Region'
  names(RCP)[which(names(RCP) == 'variable')] <- 'Sector'

  RCP[grep('Stan',RCP$Region),'Region'] <- "Asia-Stan"
  RCP$Region <- gsub(" $","", RCP$Region, perl=T)

  # Remove sectors not comparable with CEDS.
  RCP <- RCP[ , c( 'em', 'Region', 'Sector', x_rcp_years ) ]
  rcp_comparable <- RCP[which(RCP$Sector %!in% rcp_remove_sectors),]

  # generate rcp_global emissions
  rcp_global <- aggregate( rcp_comparable[ , x_rcp_years ],
                           by = list( rcp_comparable$em ),
                           FUN=sum )
  colnames( rcp_global ) <- c( 'em', x_rcp_years )
  # change the rcp_gloabl in to plot_years layout
  x_years_to_add <- x_plot_years[ which( x_plot_years %!in% x_rcp_years ) ]
  rcp_global[ , x_years_to_add ] <- NA
  rcp_global <- rcp_global[ , c( 'em', x_plot_years ) ]

  rcp_global$inventory <- 'RCP'
  rcp_global_long <- melt( rcp_global, id.vars = c( 'em', 'inventory' ) )
  rcp_global_long$variable <- as.numeric( substr( rcp_global_long$variable, 2, 5 ) )
  colnames( rcp_global_long ) <- c( 'em', 'inventory', 'year', 'total_emissions' )

  rcp_plot <- rcp_global_long
} else {
  rcp_plot <- rcp_dummy
}
# ------------------------------------------------------------------------------
# 4.2.2 add shipping for RCP_plot
if ( rcp_shipping_em_flag == T ) {
  # clean up the data
  names( rcp_shipping_emissions ) <- c( "year", "CO2", "fleet", "NOx", "SO2", "PM", "NMVOC", "CH4", "BC", "OC", "Refrigerants", "CO" )
  rcp_shipping_emissions <- rcp_shipping_emissions[ , c( "year", rcp_shipping_em_list ) ]
  # convert unit from TG to kt
  rcp_shipping_emissions [ , rcp_shipping_em_list ] <- rcp_shipping_emissions [ , rcp_shipping_em_list ] * 1000
  rcp_shipping_emissions$units <- "kt"
  rcp_shipping_emissions$SO2 <- rcp_shipping_emissions$SO2 * 2  #Convert from S to SO2 for SO2
  rcp_shipping_emissions$NOx <- rcp_shipping_emissions$NOx * 3.285  # Convert from N to NO2 for NOx

  # add rcp_shipping_emissions to rcp_plot
  rcp_shipping_global <- rcp_shipping_emissions[ , c( "year", em ) ]
  rcp_plot <- merge( rcp_plot, rcp_shipping_global, by = c( "year" ), all.x = T  )
  rcp_plot$total_emissions <- rcp_plot$total_emissions + rcp_plot[ , em ]
  rcp_plot <- rcp_plot[ , c( 'em', 'inventory', 'year', 'total_emissions' ) ]
} else {
  rcp_plot <- rcp_plot
}

# -----------------------------------------------------------------------------
# 4.3 process EDGAR data
if ( edgar_em_flag == T ) {
  # Clean rows and columns to standard format
  edgar_emissions$units <- 'kt'
  if (em != 'CH4') edgar_emissions <- edgar_emissions[ ,c( 'ISO_A3', 'IPCC', 'units', edgar_years ) ]
  if (em == 'CH4') edgar_emissions <- edgar_emissions[ ,c( 'ISO_A3', 'IPCC', 'units', x_edgar_years ) ]
  names( edgar_emissions ) <- c ('iso','sector','units', x_edgar_years )
  edgar_emissions$iso <- tolower( edgar_emissions$iso )

  #remove rows with all NA's
  edgar_emissions <- edgar_emissions[ apply( X = edgar_emissions[ , x_edgar_years ],
                         MARGIN = 1,
                         function( x ) ( !all.na( x ) ) ) , ]
  edgar_emissions$em <- em
  # drop sectors
  edgar_comparable <- edgar_emissions[ -which( edgar_emissions$sector %in% edgar_remove_sectors ), ]
  # generate ceds_global emissions
  edgar_global <- aggregate( edgar_comparable[ , x_edgar_years ],
                            by = list( edgar_comparable$em ),
                            FUN=sum, na.rm = T )
  colnames( edgar_global ) <- c( 'em', x_edgar_years )
  # change the edgar_gloabl in to plot_years layout
  x_years_to_add <- x_plot_years[ which( x_plot_years %!in% x_edgar_years ) ]
  edgar_global[ , x_years_to_add ] <- NA
  edgar_global <- edgar_global[ , c( 'em', x_plot_years ) ]

  edgar_global$inventory <- 'EDGAR'
  edgar_global_long <- melt( edgar_global, id.vars = c( 'em', 'inventory' ) )
  edgar_global_long$variable <- as.numeric( substr( edgar_global_long$variable, 2, 5 ) )
  colnames( edgar_global_long ) <- c( 'em', 'inventory', 'year', 'total_emissions' )

  edgar_plot <- edgar_global_long
} else {
  edgar_plot <- edgar_dummy
}

# -----------------------------------------------------------------------------
# 4.4 process GAINS data
if ( gains_em_flag == T ) {
  # clean up the data
  gains_emissions <- gains_emissions[ gains_emissions$Region != 'Global' , ]
  gains_emissions <- gains_emissions[ c('Region', 'Sector', gains_years ) ]
  colnames( gains_emissions ) <- c( 'region', 'sector', x_gains_years )

  # only CO2 is reported in Tg rather than Gg, so multiply by 10^3 to convert Tg to Gg
  if( em == 'CO2') {
    gains_emissions[ ,x_gains_years ] <- gains_emissions[ , x_gains_years] * 1000
  }

  gains_comparable <- gains_emissions[ -which( gains_emissions$sector %in% gains_remove_sectors ), ]
  gains_comparable$em <- em
  # generate gains_global emissions
  gains_global <- aggregate( gains_comparable[ , x_gains_years ],
                            by = list( gains_comparable$em ),
                            FUN = sum, na.rm = T )
  colnames( gains_global ) <- c( 'em', x_gains_years )
  # change the ceds_gloabl in to plot_years layout
  x_years_to_add <- x_plot_years[ which( x_plot_years %!in% x_gains_years ) ]
  gains_global[ , x_years_to_add ] <- NA
  gains_global <- gains_global[ , c( 'em', x_plot_years ) ]

  gains_global$inventory <- 'GAINS'
  gains_global_long <- melt( gains_global, id.vars = c( 'em', 'inventory' ) )
  gains_global_long$variable <- as.numeric( substr( gains_global_long$variable, 2, 5 ) )
  colnames( gains_global_long ) <- c( 'em', 'inventory', 'year', 'total_emissions' )

  gains_plot <- gains_global_long

  # addtional step to add ceds_shipping to gains_plot
  gains_plot <- merge( gains_plot, ceds_shipping, by = 'year', all.x = T )
  gains_plot$shipping_em <- ifelse( is.na( gains_plot$shipping_em ), 0, gains_plot$shipping_em )
  gains_plot$total_emissions <- gains_plot$total_emissions + gains_plot$shipping_em
  gains_plot <- gains_plot[ , c( 'year', 'em', 'inventory', 'total_emissions' ) ]

} else {
  gains_plot <- gains_dummy
}

# -------------------------------------------------------------------
# 5. combine all plot data together
df_list <- list( ceds_plot, edgar_plot, gains_plot, rcp_plot )
df <- do.call( 'rbind.fill', df_list )

# --------------------------------------------------------------------
# 6. plotting
df$total_emissions <- df$total_emissions / 1000 #convert from Gg to Tg

max <- 1.2 * ( max( df$total_emissions ) )

x_axis_start <- 1970
x_axis_end <-2020
# Specify the color for Inventory
ceds_color <- 'black'
edgar_color <- 'dodgerblue1'
rcp_color <- 'black'
gains_color <- 'red1'

#Specify the values of integers in the "scale_linetype_manual" for built-in types of lines:
#(0=blank, 1=solid, 2=dashed, 3=dotted, 4=dotdash...)
ceds_line_style <- 1
edgar_line_style <- 2
rcp_line_style <- 0
gains_line_style <- 0

# Specify the values of integer in the "scale_shape_manual" for plotting symbols:
#(15=black square, 19=black bullet point, 17=black triangle, 22=blue square...)
ceds_shape_symbol <- NA
edgar_shape_symbol <- NA
rcp_shape_symbol <- 19
gains_shape_symbol <- 15

plot <- ggplot( df, aes(x=year,y=total_emissions,
                       colour=inventory, linetype = inventory, shape = inventory)) +
  geom_line(data = subset(df, inventory=='CEDS'),size=1) +
  geom_line(data = subset(df, inventory=='EDGAR'),size=1 ) +
  geom_point(data = subset(df, inventory=='RCP')) +
  geom_point(data = subset(df, inventory=='GAINS'))  +
  scale_x_continuous(limits = c(x_axis_start,x_axis_end ),
                     breaks= seq(from=x_axis_start, to=x_axis_end, by=10),
                     minor_breaks = seq(from=x_axis_start, to=x_axis_end, by=5)) +
  scale_y_continuous(limits = c(0,max ),labels = comma)+
  ggtitle( paste('Global',em,'Inventories') )+
  labs(x= "" , y= paste(em ,'Emissions [Tg/yr]') )+
  theme(panel.background=element_blank(),
        panel.grid.minor = element_line(colour="gray95"),
        panel.grid.major = element_line(colour="gray88"),
        panel.border = element_rect(colour = "gray80", fill=NA, size=1)) +
  scale_color_manual(name = 'Inventory',
                     values = c('CEDS'= ceds_color,
                                'EDGAR'= edgar_color,
                                'RCP'= rcp_color,
                                'GAINS'= gains_color)) +
  scale_linetype_manual(name= 'Inventory',
                        values = c('CEDS' = ceds_line_style,
                                   'EDGAR'= edgar_line_style,
                                   'RCP'= rcp_line_style,
                                   'GAINS'= gains_line_style)) +
  scale_shape_manual(name= 'Inventory',
                     values = c('CEDS' = ceds_shape_symbol,
                                'EDGAR' = edgar_shape_symbol,
                                'RCP' = rcp_shape_symbol,
                                'GAINS' = gains_shape_symbol))

savePlot('DIAG_OUT', 'ceds-comparisons', paste0('CEDS_RCP_GAINS_EDGAR_', em, '_Global_Comparison.pdf'),
         width = 7, height = 4, plot = plot)

# ------------------------------------------------------------------------------
# 7. Write out and end

df <- na.omit( df )
writeData( df,
           domain = 'DIAG_OUT', domain_extension = 'ceds-comparisons/',
           fn = paste0( 'CEDS_RCP_GAINS_EDGAR_',em,'_Global_Comparison' ) )

logStop()
