library(tidyverse)
library(tidycensus)



#---Static Data---#
# PGH Tracts
tracts = read_csv('Data/Tract.csv') # Tracts from HFPA Estimate Sheet
pgh.tracts = as.character(tracts$Tract) # convert to vector

# no walkshed
no.walkshed = read_csv('Data/no_walkshed.csv')
no.walkshed = mutate(no.walkshed, GEOID = as.character(GEOID))

# Census API Key
API_KEY = "ENTER KEY HERE"
census_api_key(API_KEY)

# get.census.var funcion
get.census.var = function(var.id, year = 2020, var.name, geometry = FALSE) {
  var.df = get_acs(
    geography = 'tract',
    variables = var.id, # This is the var.id from https://api.census.gov/data/2020/acs/acs5/variables.html
    year = year, # Supplied Year
    state = 'PA',
    county = 'Allegheny',
    geometry = geometry
  )
  
  var.df = var.df %>%
    select(GEOID, estimate) %>%
    filter(GEOID %in% pgh.tracts) %>%
    rename(!!var.name := estimate) %>%
    mutate(year = year) # Add column for year
  
  return(var.df)
}

# Normalized Score Function
normalized.score = function(unstandardized) {
  standardized = (unstandardized - mean(unstandardized, na.rm = T)) / sd(unstandardized, na.rm = T)
  normalized = (standardized - min(standardized, na.rm = T)) * (10 / (max(standardized, na.rm = T) - min(standardized, na.rm = T)))
  
  return(normalized)
}

# Utilization Metrics Function
# 500 Cities: Census Tract-level Data
# Create a way to get the closest year's data for example passing 2020 would get 2019 data, passing in 2015 would get 2016 data

# This is a hardcoded dataframe. To find new URL endpoints for different years go here:
# https://chronicdata.cdc.gov/browse?category=500+Cities+%26+Places&q=500+Cities%3A+Census+Tract-level+Data&sortBy=relevance
file.names = c('k86t-wghb.csv', 'k25u-mg9b.csv', 'kucs-wizg.csv', '5mtz-k78d.csv')
years = c(2019, 2018, 2017, 2016)
API.endpoints = data.frame(year = years, file.name = file.names)

get.500.cities.endpoint = function(supplied.year) {
  closest.year = which(abs(API.endpoints$year - supplied.year) == min(abs(API.endpoints$year - supplied.year))) # Closest year
  endpoint = API.endpoints[closest.year, 'file.name']
  return(endpoint)
}

# Get current year and lat 5 and 10 years
current.year = as.numeric(format(Sys.Date(), '%Y'))
last.10.years = seq(from = current.year - 2, to = current.year - 10, by = -1)
last.5.years = seq(from = current.year - 2, to = current.year - 6, by = -1)


get.data = function() {
  # Empty HFPA.data data frame
  HFPA.data = data.frame()
  #-----------------#
  
  for (year in last.10.years) { # Change back to last.10.years
    # Start with availability
    year.data = get.census.var(var.id = 'B08201_001', year = year, var.name = 'Total_HH', geometry = TRUE) # total_HH
    year.data = year.data %>%
      inner_join(y = select(get.census.var(var.id = 'B08014_002', year = year, var.name = 'No_Vehicle_Avail'), 
                            c(GEOID, No_Vehicle_Avail)), by = 'GEOID') # Join in No_Vehicle_Avail
    year.data = year.data %>%
      mutate(pct_no_veh_avail = Total_HH/No_Vehicle_Avail,
             pct_no_veh_avail = replace(pct_no_veh_avail, is.nan(pct_no_veh_avail) | is.infinite(pct_no_veh_avail), NA), # Replace NaN and Inf with NA
             No_Vehicle_Score = normalized.score(pct_no_veh_avail)) # get no_vehicle_score
    year.data = year.data %>%
      inner_join(y = no.walkshed, by = 'GEOID') %>% # Join no_walkshed
      mutate(Walkability_Score = normalized.score(no_walkshed)) # Get walkability_score
    year.data = year.data %>%
      mutate(Availability = sqrt(No_Vehicle_Score * Walkability_Score)) # get final Availiability score
    
    # Join in the Access metrics
    # Get pop below 185% Poverty Level
    year.data = year.data %>%
      inner_join(y = select(get.census.var(var.id = 'S1701_C01_041', year = year, var.name = 'pop_below_185'), c(GEOID, pop_below_185)), by = 'GEOID') %>%
      inner_join(y = select(get.census.var(var.id = 'S0101_C01_001', year = year, var.name = 'total_pop'), c(GEOID, total_pop)), by = 'GEOID')
    
    # Create the rest of the columns
    year.data = year.data %>%
      mutate(pct_below_185 = pop_below_185 / total_pop,
             Access = normalized.score(pct_below_185)) # get the normalized pct_below_185
    
    # Join in the Utilization metrics
    # create the url from the get.500.cities.endpoint function
    URL = paste('https://chronicdata.cdc.gov/resource/', get.500.cities.endpoint(year), '?placename=Pittsburgh', sep='')
    utilization = read_csv(URL) # create temp dataframe with just that year's data
    # create necessary normalized columns for the year
    utilization = utilization %>%
      mutate(GEOID = as.character(tractfips)) %>%  # Change to GEOID
      select(GEOID, obesity_crudeprev, chd_crudeprev, diabetes_crudeprev) %>% # Keep only necessary columns 
      mutate(obesity_norm = normalized.score(obesity_crudeprev), # Normalize the columns
             chd_norm = normalized.score(chd_crudeprev),
             diabetes_norm = normalized.score(diabetes_crudeprev),
             Utilization = obesity_norm + chd_norm + diabetes_norm,
             Utilization = normalized.score(Utilization)) # Calculate utilization!
    # Join it in with the rest
    year.data = year.data %>%
      left_join(y = utilization, by = 'GEOID')
    
    # Bind everything above to the main HFPA.data dataframe and make final HFPA column
    HFPA.data = rbind(HFPA.data, year.data)
  }
  
  HFPA.data = HFPA.data %>%
    mutate(HFPA = Availability + Access + Utilization) %>%
    group_by(year) %>%
    mutate(Priority.Area = case_when(HFPA >= (mean(HFPA, na.rm = T) + sd(HFPA, na.rm = T)) ~ '1 SD Above Mean',
                                     HFPA <= (mean(HFPA, na.rm = T) - sd(HFPA, na.rm = T)) ~ '1 SD Below Mean',
                                     TRUE ~ 'Not Anomalous'
    )) %>%
    ungroup() %>%
    mutate(Priority.Area = ordered(x = Priority.Area, levels = c('1 SD Above Mean', 'Not Anomalous', '1 SD Below Mean')))
  
  
  return (HFPA.data)
}

#HFPA.data = get.data()
