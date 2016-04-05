# ----------------------------------------------------------------------------------
# CEDS R header file:  ModH_extention_functions.R
# Authors: Rachel Hoesly
# Last Updated: April 1, 2016

# This file should be sourced by any R script doing heavy-duty reformatting of CEDS data.
# Functions contained:
#   %!in%, replaceValueColMatch ,gsub2, repeatAndAddVector, addCols, findDataStart, naReplace, addCols,
#   buildCEDSTemplate, removeBlanks
# Notes:
# -----------------------------------------------------------------------------
# Brief:        
# Details:      
# Dependencies: 
# Author(s):    
# Params:       
#  
# Return:       
# Input Files:  
# Output Files: 

# -----------------------------------------------------------------------------
 select_EF_drivers <- function(trend_name){
 
   # Expand fuels - all-comb
   expand <- extension_drivers_EF[which(extension_drivers_EF$fuel == 'all-comb' ) ,]
   extension_drivers_EF <- extension_drivers_EF[which(extension_drivers_EF$fuel != 'all-comb' ) ,]
   comb_fuels <- c('biomass', 'hard_coal','brown_coal','coal_coke','natural_gas','heavy_oil','diesel_oil','light_oil')
   for (i in seq_along(comb_fuels)){
     expand$fuel <- rep(comb_fuels[i], times= nrow(expand) )
     extension_drivers_EF <- rbind( extension_drivers_EF, expand )
   }
   extension_drivers_EF <- extension_drivers_EF[ which( extension_drivers_EF$em %in% c(em , 'all' )), ]
   
   # delete, all row for a sector-fuel if there is a sector-fuel entry for the specific emission species
   driver_em <- extension_drivers_EF[which( extension_drivers_EF$em == em), ]
   if( nrow(driver_em) > 0 ){
     em_instruction <- unique( paste( driver_em$sector,driver_em$fuel,driver_em$start_year,driver_em$end_year  ,sep = '-'))
     extension_drivers_EF <- extension_drivers_EF[ which( 
       paste( extension_drivers_EF$sector, extension_drivers_EF$fuel , extension_drivers_EF$start_year, extension_drivers_EF$end_year, extension_drivers_EF$em, sep = '-') %!in%  
         paste( em_instruction ,'all' ,sep = '-') ), ]
 }
 
   # select em
   extension_drivers_EF$em <- em
   
   # select method
   extension_drivers_EF <- extension_drivers_EF[ which( extension_drivers_EF$method == trend_name ) ,]
   extension_drivers_EF <-  extension_drivers_EF[order( - extension_drivers_EF$start_year),]
   return( extension_drivers_EF) 
 }
 
 
 extend_data_on_trend <- function(driver_trend, input_data, start, end){
  # input_data <- new_EFs 
   
   # Expand fuels - all-comb
   expand <- driver_trend[which(driver_trend$fuel == 'all' ) ,]
   driver_trend <- driver_trend[which(driver_trend$fuel != 'all' ) ,]
   comb_fuels <- c('biomass', 'hard_coal','brown_coal','coal_coke','natural_gas','heavy_oil','diesel_oil','light_oil')
   for (i in seq_along(comb_fuels)){
     expand$fuel <- rep(comb_fuels[i], times= nrow(expand) )
     driver_trend <- rbind( driver_trend, expand )
   }
   
   ratio_years <- paste0('X',c(end+1,end+2,end+3,end+4,end+5))
   ext_start_year <- start
   ext_end_year <- end
   extention_years <- paste0('X',ext_start_year:ext_end_year)
   
   # select extension data for current method
   driver_lines <- driver_trend[, c('iso','sector','fuel') ]
   driver_lines <- unique(paste(driver_lines$iso,driver_lines$sector,driver_lines$fuel,sep='-'))
   
   # select ceds data to extend
   ceds_extention_ratios <- input_data[ which( paste(input_data$iso,input_data$sector, input_data$fuel, sep="-") %in% driver_lines  ) , ]
   
   #extended data template
   ceds_extention_ratios <- ceds_extention_ratios[,c('iso','sector','fuel',ratio_years)]

   # add Driver identifyer ratio year
   ceds_extention_ratios <- merge(ceds_extention_ratios, driver_trend[,c("iso", 'sector','fuel', ratio_years)],
                                  by.x = c('iso', 'sector','fuel'),
                                  by.y = c("iso", 'sector','fuel'),
                                  all.x = TRUE, all.y = FALSE)

   ceds_extention_ratios[ ratio_years ] <- ceds_extention_ratios[ paste0(ratio_years,'.x')]/ceds_extention_ratios[ paste0(ratio_years,'.y')]
   ceds_extention_ratios <- replace(ceds_extention_ratios, ceds_extention_ratios == 'NaN', 0)
   ceds_extention_ratios <- replace(ceds_extention_ratios, is.na(ceds_extention_ratios), 0) 
   
   ceds_extention_ratios$ratio <-  rowMeans(ceds_extention_ratios[ ratio_years ])
   
   # add driver data and use ratio to calculate extended value
   ceds_extended <- ceds_extention_ratios[,c('iso','fuel','sector','ratio')]
   ceds_extended [ extention_years ] <- NA
   ceds_extended <- replaceValueColMatch(ceds_extended, driver_trend,
                                      x.ColName = extention_years,
                                      match.x = c('iso','sector','fuel'),
                                      addEntries = FALSE)
   
   ceds_extended[is.na(ceds_extended)] <- 0
   
   # calculate extended data
   ceds_extended[ extention_years ] <- ceds_extended$ratio * ceds_extended[ extention_years ]
   
   # add to final extention template
   input_data <- replaceValueColMatch(input_data, ceds_extended,
                                    x.ColName = extention_years,
                                    match.x = c('iso','sector','fuel'),
                                    addEntries = FALSE)
   
   return(input_data)
 }
 
 
 