#------------------------------------------------------------------------------
# Program Name: A5.3.proc_activity.R
# Author: Jon Seibert
# Date Last Modified: June 16, 2015
# Program Purpose: To process and reformat non-combustion (process) activity activity_data
#                  and combine it with combustion activity activity_data to create a total
#                  activity database.
# Input Files: A.comb_activity.csv, A.NC_activity_db.csv, Master_Fuel_Sector_List.xlsx,
#              Master_Country_List.csv
# Output Files: A.NC_activity.csv, A.total_activity.csv, A.NC_missing_sectors.csv,
#               A.NC_dropoff_sectors.csv
# Notes:
# TODO:
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0. Read in global settings and headers
# Define PARAM_DIR as the location of the CEDS "parameters" directory, relative
# to the "input" directory.
    PARAM_DIR <- if("input" %in% dir()) "code/parameters/" else "../code/parameters/"

# Call standard script header function to read in universal header files -
# provide logging, file support, and system functions - and start the script log.
    headers <- c( "data_functions.R" ) # Additional function files required.
    log_msg <- "Final reformatting of process activity activity_data" # First message to be printed to the log
    script_name <- "A5.3.proc_activity.R"

    source( paste0( PARAM_DIR, "header.R" ) )
    initialize( script_name, log_msg, headers )

# -----------------------------------------------------------------------------
# 0.5. Settings

# Location of input files relative to wd
input_path <- paste0( getwd(),"/mappings/" )

fuel <- "process"

# ------------------------------------------------------------------------------
# 1. Read in files

energy_data <- readData( "MED_OUT","A.comb_activity" )
activity_data <- readData( "MED_OUT", "A.NC_activity_db", meta = FALSE )

# Master Sector List
MSL <- readData( "MAPPINGS", "Master_Fuel_Sector_List", ".xlsx", sheet_selection = "Sectors" )
MCL <- readData( "MAPPINGS", "Master_Country_List" ) # Master Country List

# ------------------------------------------------------------------------------
# 2. Reformatting - fill out activity database with zeros if no data

# Prepare a blank template row for use in filling in missing activities

iso <- unique( MCL[which(MCL$final_data_flag == '1') ,'iso'] )
sector <- unique( MSL[which(MSL$type == 'NC') ,'sector'] )

# build blank NC template
nc_blank <- buildCEDSTemplate(iso_list = iso, sector_list = sector, fuel_list = c('process'))

nc_blank$activity <- MSL[  match(nc_blank$sector, MSL$sector) , 'activity' ]

nc_activity <- replaceValueColMatch(nc_blank,activity_data,
                             x.ColName = X_emissions_years,
                             match.x = c('iso','activity','units'),
                             addEntries = FALSE)

# Split out global process sectors and give them a dummy activity
global_nc_activity <-  subset( nc_activity, iso %in% c( "global" ) )
global_nc_activity[ , paste0('X', start_year:end_year) ] <- 1

# Combine back together
iso_nc_activity <-  subset( nc_activity, iso %!in% c( "global" ) )
nc_activity  <- rbind( iso_nc_activity,  global_nc_activity )

# Drop unneccessary columns
nc_activity <- nc_activity[,c('iso','sector','fuel','units',X_emissions_years)]

# Sort nc_activity by iso, sector, and then fuel
nc_activity <- nc_activity[ with( nc_activity, order( iso, sector, fuel ) ), ]

# Combine nc_activity with A.comb_activity
total_activity <- rbind( energy_data,nc_activity )

# Sort final by iso, sector, and then fuel
total_activity <- total_activity[ with( total_activity, order( iso, sector, fuel ) ), ]

# ------------------------------------------------------------------------------
# 4. Output

writeData( nc_activity, domain = "MED_OUT", fn = "A.NC_activity", meta = TRUE )
writeData( total_activity, domain = "MED_OUT", fn = "A.total_activity", meta = TRUE )

# ------------------------------------------------------------------------------
# 5. Diagnostic

# This diagnostic block is not robust and can fail. Skip until fixed.
if ( 1 == 2 ) {
	# This diagnostic output lists all iso-sector combinations for which all activity_data entries are 0.
	# This serves to indicate which countries either did not report any activity corresponding to
	# those sectors or reported zero activity for those sectors.
	#
	# This diagnostic may return false positives, as some countries may have reported no activity
	# for said sectors, which would have resulted in a row of all 0s just as if no report was made.
	iso_list <- unique( nc_activity$iso )
	sector_list <- unique( nc_activity$sector )

	# Reset the blank row for adding to the no-activity_data diagnostic file.
	blank <- nc_activity[ 1, ]
	blank$iso <- "NA"
	blank$units <- "NA"
	blank$sector <- "NA"

	# Include only the iso, sector, and fuel columns for no_data
	no_data <- blank[ 1:3 ]
	dropoff_data <- blank

	data_start <- findDataStart( nc_activity )

	# Check whether ALL activity_data entries for each row are 0.
	# A row is only added to the list if it has no nonzero activity_data.
	for( country in iso ){
		super <- subset( nc_activity, nc_activity$iso == country )
		for( sector_name in sector_list ){
			set <- subset( super, super$sector == sector_name )
			num_set <- set
			num_set[ 1, ] <- as.numeric( num_set[ 1, ] )
			if( !( FALSE %in% ( num_set[ data_start:length( num_set ) ] == 0 ) ) ){
				no_data <- rbind( no_data, set[ 1:3 ] )
			}
			else if( ( TRUE %in% ( num_set[ length( num_set ) ] == 0 ) ) ||
					 ( TRUE %in% ( num_set[ data_start ] == 0 ) ) ){ # For next diagnostic
				dropoff_data <- rbind( dropoff_data, set )
			}
		}
	}

	# Remove first row, as it contains only NA.
	no_data <- no_data[ -1, ]
	dropoff_data <- dropoff_data[ -1, ]

	# The second diagnostic files would be one that lists countries where the driver
	# drops to zero at some point and stays there (indicating that there might be missing activity_data).
	# This will happen in 1970 for most of the developing countries, but there might be other issues.

	# Add columns to hold years at which the runs of 0s begin, remove units column.
	# Note that the "dropoff_beg" is the year in which the opening 0s end, not the first year with activity_data.
	# Similarly, the "dropoff_end" is the first year in the ending run of 0s.
	L <- length( dropoff_data )
	dropoff_data$dropoff_beg <- ""
	dropoff_data$dropoff_end <- ""
	dropoff_data <- cbind( dropoff_data[ 1:( data_start - 2 ) ],
		dropoff_data[ ( L + 1 ):( L + 2 ) ], dropoff_data[ data_start:L ] )

	data_start <- findDataStart( dropoff_data )

	for( r in 1:nrow( dropoff_data ) ){
		row <- dropoff_data[ r, ]
		num_row <- as.numeric( row )
		L <- length( row )

		zero_start <- L
		zero_end <- data_start
		prior_zero <- FALSE
		seen_nonzero <- FALSE

		for( j in data_start:L ){
			if( prior_zero ){
				if( num_row[[ j ]] == 0 ){
					if( !( seen_nonzero ) ){ zero_end <- j }
				}
				else{
					prior_zero <- FALSE
					seen_nonzero <- TRUE
				}
			}else{
				if( num_row[[ j ]] == 0 ){
					prior_zero <- TRUE
					zero_start <- j
					if( !( seen_nonzero ) ){ zero_end <- j }
				}
				else{
					seen_nonzero <- TRUE
				}
			}
		}

		if( TRUE %in% ( num_row[ data_start ] == 0 ) ){
			dropoff_data[ r,"dropoff_beg"] <- substr( names( dropoff_data )[[ zero_end ]], 2, 5 )
		}
		if( TRUE %in% ( num_row[ L ] == 0 ) ){
			dropoff_data[ r,"dropoff_end"] <- substr( names( dropoff_data )[[ zero_start ]], 2, 5 )
		}
	}

	# Drop activity_data columns- no longer needed
	dropoff_data <- dropoff_data[ 1:( data_start - 1 ) ]

	# Add human-readable country name column from Master Country List mappings.
	no_data$country <- MCL$Country_Name[ match( no_data$iso, MCL$iso ) ]
	dropoff_data$country <- MCL$Country_Name[ match( dropoff_data$iso, MCL$iso ) ]

	# Reorder columns so that the name column is first
	no_data <- cbind( no_data[ length( no_data ) ], no_data[ 1:( length( no_data ) - 1 ) ] )
	dropoff_data <- cbind( dropoff_data[ length( dropoff_data ) ],
						   dropoff_data[ 1:( length( dropoff_data ) - 1 ) ] )

	# Comments for diagnostic files
	comments.no_data <- c( paste0( "Listing of those country-sector-fuel combinations with no activity_data." ) )
	comments.dropoff_data <- paste0( "Listing of those country-sector-fuel combinations",
									 " whose activity_data cuts off at a certain year. All activity_data before (inclusive)",
									 " the dropoff_beg is 0 and all activity_data after (inclusive) the dropoff_end is 0.",
									 " A lack of entry for either indicates nonzero activity_data at that end.")

	# Output
	writeData( no_data, domain = "DIAG_OUT", fn = "A.NC_missing_sectors",
			   meta = F, comments = comments.no_data )
	writeData( dropoff_data, domain = "DIAG_OUT", fn = "A.NC_dropoff_sectors",
			   meta = F, comments = comments.dropoff_data )
}

logStop()
# END
