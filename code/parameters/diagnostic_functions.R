# ----------------------------------------------------------------------------------
# CEDS R header file: Diagnostic Functions
# Authors: Rachel Hoesly
# Last Updated: 2 May 2016

#
# Functions contained:
#   compare_data

# ----------------------------------------------------------------------------------
# compare_data
# Brief:         Compares two dataframes with the same identifier-Xyear format
# Details:       compares the identifier-Xyear values of two dataframes and returns a
#                summary of the different values
# Dependencies:
# Author(s):     Rachel Hoesly
# Params:
#   data_1:      first data frame to compare in identifier-Xyear format (ex: iso-sector-Xyear or iso-Xyear)
#   data_2:      second data frame to compare in identifier-Xyear format
#
# Return:        dataframe with different values, in identifier-Xyear long format
# Input Files:   none
# Output Files:  none


compare <- function(data_1 , data_2){


  comb_id <- names( data_1)[!grepl("X", names(data_1))]
  Xyears <- names( data_1)[grepl("X", names(data_1))]

  print('melting data 1')
  data_1_long <- melt(as.data.frame(data_1), id.vars = comb_id)
  print('melting data 2')
  data_2_long <- melt(as.data.frame(data_2), id.vars = comb_id)
  print('merging')
  data_1_diff <- merge(data_1_long,data_2_long,
                    by = c(comb_id, 'variable'),
                    suffixes = c(".data_1",".data_2"), all =T)
  data_1_diff$diff <- data_1_diff$value.data_2 - data_1_diff$value.data_1

  names(data_1_diff)[ which(names(data_1_diff) == ("variable"))] <- c("year")

  out <- data_1_diff[-which(data_1_diff$diff == 0),]
  out$percent_of_val_1 <- out$diff/out$value.data_1*100

  return(out)

}
