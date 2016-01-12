#Define historical years variables.

#Set of historical years for which the CEDS system has IEA data.
IEA_years <- 1960:2013
X_IEA_years <- paste0( "X", IEA_years)

IEA_end_year  <- 2013  # Latest year of IEA data; used to compare BP and IEA
X_IEA_end_year  <- paste0( "X", IEA_end_year  )

#The set of years for the BP data extendForwards further, and is used to augment the IEA.
BP_years <- 2014 #The years for which there is only BP data
X_BP_years <- paste0("X", BP_years)

#The combined overall yearset.
emissions_years <- c(IEA_years,BP_years)
X_emissions_years <- paste0( "X", emissions_years)

start_year = 1960
X_start_year = paste0("X",start_year)
end_year = 2014
X_end_year = paste0("X",end_year)