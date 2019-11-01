# ------------------------------------------------------------------------------
# Program Name: Regional_EF_graphs.R
# Author: Steve Smith
# Program Purpose: Produces diagnostic file
#
# ---------------------------------------------------------------------------

# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
headers <- c( "data_functions.R", "analysis_functions.R",'process_db_functions.R',
              'common_data.R', 'IO_functions.R', 'data_functions.R', 'timeframe_functions.R') # Additional function files may be required.
log_msg <- "Figures" # First message to be printed to the log
script_name <- "Regional_EF_graphs.R"

source( paste0( PARAM_DIR, "header.R" ) )
initialize( script_name, log_msg, headers )

args_from_makefile <- commandArgs( TRUE )
em <- args_from_makefile[ 1 ]
if ( is.na( em ) ) em <- "CO"

# ---------------------------------------------------------------------------
# 0.1 Load Packages

library('ggplot2')
library('plyr')
library('scales')
library('gridExtra')

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# 0. Define core function that process data, writes tables and graphs
# Note: Function is not "clean" as it uses many common variables

emission_factor_function <- function( analysis_sectors, analysis_fuels ) {

# 0.1. Prepare Data

Scaled_EFs <- Scaled_EFs[which( Scaled_EFs$sector %in% analysis_sectors &
                                Scaled_EFs$fuel %in% analysis_fuels ),]

Scaled_EFs$Region <- Diagnostic_Country_List[match(Scaled_EFs$iso,Diagnostic_Country_List$iso),'Region']
Scaled_EFs$Country <- Diagnostic_Country_List[match(Scaled_EFs$iso,Diagnostic_Country_List$iso),'Country_Name']

if ( PRINT_EMISSIONS ) {
  Scaled_Emissions <- Scaled_Emissions[which(Scaled_Emissions$sector %in% analysis_sectors & Scaled_Emissions$fuel %in% analysis_fuels ),]
  Scaled_Emissions$Region <- Diagnostic_Country_List[match(Scaled_Emissions$iso,Diagnostic_Country_List$iso),'Region']
  Scaled_Emissions$Country <- Diagnostic_Country_List[match(Scaled_Emissions$iso,Diagnostic_Country_List$iso),'Country_Name']

}
#-----
if ( GRAPH_DEFAULTS ) {
	Default_EFs <- Default_EFs[ which( Default_EFs$sector %in% analysis_sectors &
									  Default_EFs$fuel %in% analysis_fuels ), ]

	Default_EFs$Region <- Diagnostic_Country_List[match(Default_EFs$iso,Diagnostic_Country_List$iso),'Region']
	Default_EFs$Country <- Diagnostic_Country_List[match(Default_EFs$iso,Diagnostic_Country_List$iso),'Country_Name']
}

# ---------------------------------------------------------------------------
# 0.2. Write Tables
if ( PRINT_GENERAL_TABLES ) {
	writeData( Scaled_EFs, "DIAG_OUT", paste0(EF_directory, em ,'_selected_scaled_EFs'), meta = FALSE )

	if ( GRAPH_DEFAULTS ) writeData( Default_EFs, "DIAG_OUT", paste0(EF_directory, em ,'_selected_default_EFs'), meta = FALSE )

	if ( PRINT_EMISSIONS ){
	  writeData( Scaled_Emissions, "DIAG_OUT", paste0(EF_directory, em ,'_selected_scaled_emissions'), meta = FALSE )
	}
}

# ---------------------------------------------------------------------------
# 0.3. Draw Graphs

# Set countries to report
analysis_isos <- unique(Diagnostic_Country_List$iso)

File_Prefix = ""

# Set-up for inventory only years
if ( INVENTORY_YEARS_ONLY ) {
  GRAPH_DEFAULTS = FALSE # Since this is not compatable with this option
  File_Prefix = "_inv-years"

  Inventory_isos <- unique( Inventory_countries$iso )
  EFs_to_Plot <- Scaled_EFs[which( Scaled_EFs$iso %in% Inventory_isos ),]
}

# Now limit to only countries we are interested in
if ( GRAPH_DEFAULTS ){
  EFs_to_Plot <- Default_EFs[which( Default_EFs$iso %in% analysis_isos ),]
  File_Prefix = "_default"
} else {
  EFs_to_Plot <- Scaled_EFs[which( Scaled_EFs$iso %in% analysis_isos ),]
}

# Regions to graph
analysis_regions <- unique(Diagnostic_Country_List$Region)

x_years<-paste( 'X', start_year:2014, sep="")
plot_list <- list()

for( Sector in 1:length( analysis_sectors ) ){
  for( Fuel in 1:length( analysis_fuels ) ) {
    # Get graph maximum
    All_Data <- EFs_to_Plot[which( EFs_to_Plot$sector %in% analysis_sectors[ Sector ] &
                                   EFs_to_Plot$fuel %in% analysis_fuels[ Fuel ] ),]
    if ( PRINT_GENERAL_TABLES ) {
		# Write out table for this sector and fuel
		writeData( All_Data, "DIAG_OUT", paste0( EF_directory, em, '_', analysis_sectors[ Sector ], "_",
														 analysis_fuels[ Fuel ], File_Prefix, '_EFs'), meta = FALSE )
    }

    All_Data_vector <- as.vector( as.matrix( All_Data[ x_years ] ) )
    Y_Axis_Max <- quantile( All_Data_vector, 0.97 )

    # Now loop through regions and plot each
    for( Region in 1:length( analysis_regions ) ) {
      Emission_Factors <- EFs_to_Plot[which( EFs_to_Plot$sector %in% analysis_sectors[ Sector ] &
                                            EFs_to_Plot$fuel %in% analysis_fuels[ Fuel ] &
                                            EFs_to_Plot$Region %in% analysis_regions[ Region ] ),  ]
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
    File_name <- paste0( em, "_", analysis_sectors[ Sector ], "_", analysis_fuels[ Fuel ], File_Prefix )

    savePlot('DIAG_OUT', EF_directory, paste0(File_name, '_EFs.pdf'), width = 12, height = 10)

    # Need to be edited to match the number of regions if are to get all the graphs
    grid.arrange(plot_list[[1]],plot_list[[2]],
                 plot_list[[3]],plot_list[[4]], ncol=2,
                 top = paste0(em, ' Scaled Emission Factors - ', analysis_sectors[ Sector ],", ", analysis_fuels[ Fuel ]) )
    dev.off()

    # Now plot just the Marker Countries
    # TODO: Add lower bound pathway
    # TODO: Add median, min, and max pathways
    if ( !( GRAPH_DEFAULTS ) ) {
		Marker_Countries <- unique( Inventory_countries$Marker_Country )
		Emission_Factors <- EFs_to_Plot[which( EFs_to_Plot$sector %in% analysis_sectors[ Sector ] &
												 EFs_to_Plot$fuel %in% analysis_fuels[ Fuel ] &
												 EFs_to_Plot$iso %in% Marker_Countries ),  ]
		Emission_Factors <- Emission_Factors[ c( 'iso', 'Country', x_years ) ]

		EFs_long <- melt( Emission_Factors, id.vars = c('Country', 'iso' ) )
		EFs_long$year <- as.numeric( gsub( "X","",EFs_long$variable ) )

		EFs_long$Earliest_Year <- Inventory_countries[match(EFs_long$iso,Inventory_countries$iso),'Earliest_Inventory_Year']
		EFs_long <- filter( EFs_long, EFs_long$year >= Earliest_Year)
		EFs_long$type <- "Scaled"

		Min_Values = 3
		Filtered_EFs <- group_by( EFs_long, year ) %>%  filter( length( iso ) >= 3)
		min_EF <- group_by( Filtered_EFs, year) %>% filter( value == min( value, na.rm = TRUE ) )
		min_EF$iso <- "minimum"
		min_EF$Country <- "na"
		max_EFs <- group_by( Filtered_EFs, year) %>% filter( value == max( value, na.rm = TRUE ) )
		max_EFs$iso <- "maximum"
		max_EFs$Country <- "na"
		median_EFs <- group_by( Filtered_EFs, year) %>% filter( value == median( value, na.rm = TRUE ) )
		median_EFs$iso <- "median"
		median_EFs$Country <- "na"

		Marker_Country_EFs <- rbind.fill( EFs_long, min_EF, max_EFs, median_EFs )
		Marker_Country_EFs <- cast( Marker_Country_EFs, iso+Country ~ year, mean, value="mean")

		File_name <- paste0( EF_directory, em, "_", analysis_sectors[ Sector ], "_", analysis_fuels[ Fuel ], "_marker_country_EFs" )
		writeData( Marker_Country_EFs, "DIAG_OUT", File_name, meta = FALSE )

		User_Line_Types <- c("solid" )
		if ( GRAPH_DEFAULTS ) {
  		# Process Default EFs and add to the plot
  		Marker_Default_EFs <- Default_EFs[  which( Default_EFs$sector %in% analysis_sectors[ Sector ] &
  												   Default_EFs$fuel %in% analysis_fuels[ Fuel ] &
  												   Default_EFs$iso %in% Marker_Countries ) ,]
  		Marker_Default_EFs <- Marker_Default_EFs[ c( 'iso', 'Country', x_years ) ]
  		Marker_Default_EFs_long <- melt( Marker_Default_EFs, id.vars = c('Country', 'iso' ) )
  		Marker_Default_EFs_long$year <- as.numeric( gsub( "X","",Marker_Default_EFs_long$variable ) )
  		Marker_Default_EFs_long$type <- "Default"
  		Marker_Default_EFs_long$Earliest_Year <- 1970

		  EFs_long <- rbind.fill( EFs_long, Marker_Default_EFs_long )
		  User_Line_Types <- c("dotted", "solid" )
    }

		plot <- ggplot(EFs_long, aes(x=year,y=value, color = Country )) +
		  geom_line( size=1, aes(x=year,y=value, color = Country, linetype = type )) +
		  scale_x_continuous(breaks=c( 1970,1980,1990,2000,2010, 2015 ))+
		  scale_linetype_manual( values = User_Line_Types ) +
		  guides( size = "legend", linetype = "none" ,
		          color=guide_legend(ncol=2)) +
		  scale_y_continuous(limits = c( 0, Y_Axis_Max ), labels = comma )+
		  scale_shape_discrete(guide=FALSE)+
		  labs(x='Year',y= paste(em,' Emission Factor [g/g]'))+
		  theme(legend.title=element_blank())+
		  theme(panel.background=element_blank(),
		        panel.grid.minor = element_line(colour="gray95"),
		        panel.grid.major = element_line(colour="gray88"),
		        panel.border = element_rect(colour = "grey80", fill=NA, size=.8))

		File_name <- paste0( em, "_", analysis_sectors[ Sector ], "_", analysis_fuels[ Fuel ], "-Bounding_EFs.pdf" )

		if (File_name == paste0( 'CO_1A3b_Road_light_oil-Bounding_EFs'))
          savePlot('DIAG_OUT', '/paper-figures/Paper/', 'CO_EF', width = 7, height = 3, plot = plot)

		savePlot( 'DIAG_OUT', EF_directory, File_name, width = 6, height = 3, plot = plot )

    }

  } # End of fuel loop
} # End of sector loop

} # End of graphing function
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# 1.0 Script Options

start_year <- 1970
GRAPH_DEFAULTS <- FALSE
PRINT_EMISSIONS <- FALSE
INVENTORY_YEARS_ONLY <- FALSE
PRINT_GENERAL_TABLES <- FALSE

# Set sectors and fuels that we want to report
# Put last the set that is to be used

# Directory for graphs
EF_directory <- "EF-diagnostics/"

# ---------------------------------------------------------------------------
# 1. Load files

Diagnostic_Country_List <- readData( "SCALE_MAPPINGS", "Diagnostic_Country_Mapping")
Diagnostic_Country_List$Notes <- NULL  # remove since gets in the way later

# Get list of countries with inventory data
Inventory_countries <- Diagnostic_Country_List[!(is.na( Diagnostic_Country_List$Earliest_Inventory_Year) ),]

# Read in scaled and default EFs
All_Scaled_EFs  <- readData('MED_OUT', paste0('F.',em,'_scaled_EF'))
if ( GRAPH_DEFAULTS ) All_Default_EFs <- readData('MED_OUT', paste0('D.',em,'_default_total_EF'))

if ( PRINT_EMISSIONS ) Scaled_Emissions <- readData('MED_OUT', paste0( 'F.', em, '_scaled_emissions' ) )

# MAIN PROGRM LOOP

# Loop through all the cases
for ( DataSet in 1:4 ) {

	Scaled_EFs <- All_Scaled_EFs
	if ( GRAPH_DEFAULTS ) Default_EFs <- All_Default_EFs

	if ( DataSet == 1 ) {
		analysis_sectors <- c( "1A4b_Residential" )
		analysis_fuels <- c( "biomass", "diesel_oil", "natural_gas", "hard_coal" )
	} else if ( DataSet == 2 ) {
		analysis_sectors <- c( "1A3b_Road" )
		analysis_fuels <- c( "diesel_oil", "light_oil" )
	} else if ( DataSet == 3 ) {
		analysis_sectors <- c( "1A3c_Rail" )
		analysis_fuels <- c( "diesel_oil", "hard_coal" )
	} else if ( DataSet == 4 ) {
		analysis_sectors <- c( "1A1a_Electricity-public", "1A2g_Ind-Comb-other" )
		analysis_fuels <- c( "biomass", "diesel_oil", "heavy_oil", "hard_coal", "brown_coal", "natural_gas" )
	}

	if( em == "NH3" ) {
		if ( DataSet == 3 ) {
			analysis_sectors <- c( "5D_Wastewater-handling" )
			analysis_fuels <- c( "process" )
		}
	}

	emission_factor_function ( analysis_sectors, analysis_fuels )
}

logStop()

# END
