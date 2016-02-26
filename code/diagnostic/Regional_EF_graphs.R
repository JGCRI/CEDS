# ------------------------------------------------------------------------------
# Program Name: Regional_EF_graphs.R
# Author: Steve Smith
# Program Purpose: Produces diagnostic file
#               
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers

# Set working directory
dirs <- paste0( unlist( strsplit( getwd(), c( '/', '\\' ), fixed = T ) ), '/' )
for ( i in 1:length( dirs ) ) {
  setwd( paste( dirs[ 1:( length( dirs ) + 1 - i ) ], collapse = '' ) )
  wd <- grep( 'CEDS/input', list.dirs(), value = T )
  if ( length(wd) > 0 ) {
    setwd( wd[1] )
    break
    
  }
}
PARAM_DIR <- "../code/parameters/"

# Call standard script header function to read in universal header files - 
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R",'process_db_functions.R',
              'common_data.r', 'IO_functions.R', 'data_functions.R', 'timeframe_functions.R') # Additional function files may be required.
log_msg <- "Figures" # First message to be printed to the log
script_name <- "Regional_EF_graphs.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "NOx"

# ---------------------------------------------------------------------------
# 0.1 Load Packages

library('ggplot2')
library('plyr')
library('scales')
library('gridExtra')

# ---------------------------------------------------------------------------
# 1.0 Script Options

start_year <- 1970
USE_DEFAULTS <- FALSE
PRINT_EMISSIONS <- FALSE
INVENTORY_YEARS_ONLY <- TRUE

# Set sectors that want to report
anaylsis_sectors <- c( "1A3c_Rail" )
anaylsis_sectors <- c( "1A4b_Residential" )
anaylsis_sectors <- c( "1A3b_Road" )

# Set fuels that want to report
anaylsis_fuels <- c( "diesel_oil", "hard_coal" )
anaylsis_fuels <- c( "biomass", "diesel_oil")
anaylsis_fuels <- c( "diesel_oil", "light_oil" )

# Directory for graphs
EF_directory <- "EF-diagnostics/"

# ---------------------------------------------------------------------------
# 1. Load files

Diagnostic_Country_List <- readData( "SCALE_MAPPINGS", "Diagnostic_Country_Mapping")
Diagnostic_Country_List$Notes <- NULL  # remove since gets in the way later
Scaled_EFs  <- readData('MED_OUT', paste0('F.',em,'_scaled_EF'))

if ( USE_DEFAULTS ) Default_EFs <- readData('MED_OUT', paste0('D.',em,'_default_total_EF'))
if ( PRINT_EMISSIONS ) Scaled_Emissions <- readData('MED_OUT', paste0( 'F.', em, '_scaled_emissions' ) )

setwd('../diagnostic-output')

# ---------------------------------------------------------------------------

# 1. Prepare Data 

Scaled_EFs <- Scaled_EFs[which( Scaled_EFs$sector %in% anaylsis_sectors & 
                                Scaled_EFs$fuel %in% anaylsis_fuels ),]

Scaled_EFs$Region <- Diagnostic_Country_List[match(Scaled_EFs$iso,Diagnostic_Country_List$iso),'Region']
Scaled_EFs$Country <- Diagnostic_Country_List[match(Scaled_EFs$iso,Diagnostic_Country_List$iso),'Country_Name']

if ( PRINT_EMISSIONS ) {
  Scaled_Emissions <- Scaled_Emissions[which(Scaled_Emissions$sector %in% anaylsis_sectors & Scaled_Emissions$fuel %in% anaylsis_fuels ),]
  Scaled_Emissions$Region <- Diagnostic_Country_List[match(Scaled_Emissions$iso,Diagnostic_Country_List$iso),'Region']
  Scaled_Emissions$Country <- Diagnostic_Country_List[match(Scaled_Emissions$iso,Diagnostic_Country_List$iso),'Country_Name']

}
#-----
if ( USE_DEFAULTS ){
   Default_EFs <- Default_EFs[ which( Default_EFs$sector %in% anaylsis_sectors & 
                                      Default_EFs$fuel %in% anaylsis_fuels ), ]

   Default_EFs$Region <- Diagnostic_Country_List[match(Default_EFs$iso,Diagnostic_Country_List$iso),'Region']
   Default_EFs$Country <- Diagnostic_Country_List[match(Default_EFs$iso,Diagnostic_Country_List$iso),'Country_Name']
}

# ---------------------------------------------------------------------------
# 2. Write Tables

writeData( Scaled_EFs, "DIAG_OUT", paste0(EF_directory, em ,'_selected_scaled_EFs'), meta = FALSE )

if ( USE_DEFAULTS ){
  writeData( Default_EFs, "DIAG_OUT", paste0(EF_directory, em ,'_selected_default_EFs'), meta = FALSE )
}

if ( PRINT_EMISSIONS ){
  writeData( Scaled_Emissions, "DIAG_OUT", paste0(EF_directory, em ,'_selected_scaled_emissions'), meta = FALSE )
}

# ---------------------------------------------------------------------------
# 3. Draw Graphs

# Set to correct directory
setwd('../diagnostic-output')

# Set countries to report
anaylsis_isos <- unique(Diagnostic_Country_List$iso)

File_Prefix = ""

# Set-up for inventory only years
if ( INVENTORY_YEARS_ONLY ) {
  USE_DEFAULTS = FALSE # Since this is not compatable with this option
  File_Prefix = "_inv-years"

  # Get list of countries with inventory data
  Inventory_countries <- Diagnostic_Country_List[!(is.na( Diagnostic_Country_List$Earliest_Inventory_Year) ),]
  
  Inventory_isos <- unique( Inventory_countries$iso )
  EFs_to_Plot <- Scaled_EFs[which( Scaled_EFs$iso %in% Inventory_isos ),]
}

# Now limit to only countries we are interested in
if ( USE_DEFAULTS ){
  EFs_to_Plot <- Default_EFs[which( Default_EFs$iso %in% anaylsis_isos ),]
  File_Prefix = "_default"
} else {
  EFs_to_Plot <- Scaled_EFs[which( Scaled_EFs$iso %in% anaylsis_isos ),]
}

# Regions to graph
anaylsis_regions <- unique(Diagnostic_Country_List$Region)

x_years<-paste( 'X', start_year:2014, sep="")
plot_list <- list()

for( Sector in 1:length( anaylsis_sectors ) ){
  for( Fuel in 1:length( anaylsis_fuels ) ) {
    # Get graph maximum
    All_Data <- EFs_to_Plot[which( EFs_to_Plot$sector %in% anaylsis_sectors[ Sector ] & 
                                   EFs_to_Plot$fuel %in% anaylsis_fuels[ Fuel ] ),]
    # Write out table for this sector and fuel
    writeData( All_Data, "DIAG_OUT", paste0( EF_directory, em, '_', anaylsis_sectors[ Sector ], "_", 
                                                     anaylsis_fuels[ Fuel ], File_Prefix, '_EFs'), meta = FALSE )
    
    All_Data_vector <- as.vector( as.matrix( All_Data[ x_years ] ) )
    Y_Axis_Max <- quantile( All_Data_vector, 0.97 )
     
    # Now loop through regions and plot each
    for( Region in 1:length( anaylsis_regions ) ) {
      Emission_Factors <- EFs_to_Plot[which( EFs_to_Plot$sector %in% anaylsis_sectors[ Sector ] & 
                                            EFs_to_Plot$fuel %in% anaylsis_fuels[ Fuel ] & 
                                            EFs_to_Plot$Region %in% anaylsis_regions[ Region ] ),  ]
      Emission_Factors <- Emission_Factors[ c( 'iso', 'Country', x_years ) ]
      
      EFs_long <- melt(Emission_Factors, id.vars = c('Country', 'iso' ))
      EFs_long$year <- as.numeric( gsub( "X","",EFs_long$variable) )
      
      # If plotting inventory years, filter to include only those years
      if ( INVENTORY_YEARS_ONLY ) {
        EFs_long$Earliest_Year <- Inventory_countries[match(EFs_long$iso,Inventory_countries$iso),'Earliest_Inventory_Year']
        EFs_long <- filter( EFs_long, EFs_long$year >= Earliest_Year) 
      }
      
      # Now plot graph
      plot <- ggplot(EFs_long, aes(x=year,y=value, color = Country )) +
        geom_line( size=1, aes(x=year,y=value, color = Country)) +
        scale_x_continuous(breaks=c( 1970,1980,1990,2000,2010, 2015 ))+
        scale_y_continuous(limits = c( 0, Y_Axis_Max ), labels = comma )+
        scale_shape_discrete(guide=FALSE)+
        labs(x='Year',y= paste(em,' Emission Factor [g/g]'))+
        theme(legend.title=element_blank())
      plot
      plot_list[[ Region ]]<-plot
      
    } # End Region Loop
    
    # Save the group of plots for this Fuel and Sector
    File_name <- paste0( em, "_", anaylsis_sectors[ Sector ], "_", anaylsis_fuels[ Fuel ], File_Prefix )
    
    pdf(paste0( EF_directory, File_name, '_EFs.pdf'),width=12,height=10,paper='special')
    
    # Need to be edited to match the number of regions if are to get all the graphs
    grid.arrange(plot_list[[1]],plot_list[[2]],
                 plot_list[[3]],plot_list[[4]], ncol=2,
                 top = paste0(em, ' Scaled Emission Factors - ', anaylsis_sectors[ Sector ],", ", anaylsis_fuels[ Fuel ]) )
    dev.off()
    
    # Now plot just the Marker Countries 
    # TODO: Add lower bound pathway
    if ( INVENTORY_YEARS_ONLY & 1==1 ) {
      
    Marker_Countries <- unique( Inventory_countries$Marker_Country )
    Emission_Factors <- EFs_to_Plot[which( EFs_to_Plot$sector %in% anaylsis_sectors[ Sector ] & 
                                             EFs_to_Plot$fuel %in% anaylsis_fuels[ Fuel ] ),  ]
    Emission_Factors <- Emission_Factors[ c( 'iso', 'Country', x_years ) ]
    
    Emission_Factors <- Emission_Factors[ which( Emission_Factors$iso %in% Marker_Countries ), ]
    EFs_long <- melt( Emission_Factors, id.vars = c('Country', 'iso' ) )
    EFs_long$year <- as.numeric( gsub( "X","",EFs_long$variable ) )
    
    EFs_long$Earliest_Year <- Inventory_countries[match(EFs_long$iso,Inventory_countries$iso),'Earliest_Inventory_Year']
    EFs_long <- filter( EFs_long, EFs_long$year >= Earliest_Year) 
    
    writeData( EFs_long, "DIAG_OUT", paste0(EF_directory, em ,'_marker_country_EFs'), meta = FALSE )
    
    plot <- ggplot(EFs_long, aes(x=year,y=value, color = Country )) +
      geom_line( size=1, aes(x=year,y=value, color = Country)) +
      scale_x_continuous(breaks=c( 1970,1980,1990,2000,2010, 2015 ))+
      scale_y_continuous(limits = c( 0, Y_Axis_Max ), labels = comma )+
      scale_shape_discrete(guide=FALSE)+
      labs(x='Year',y= paste(em,' Emission Factor [g/g]'))+
      theme(legend.title=element_blank())
    plot
 
    File_name <- paste0( EF_directory, em, "_", anaylsis_sectors[ Sector ], "_", anaylsis_fuels[ Fuel ], "-Bounding_EFs.pdf" )
    ggsave( File_name , width = 5, height = 3 )
     
    }
    
  } # End of fuel block
}



logStop()

# END

