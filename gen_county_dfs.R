library(tidycensus)
library(ggplot2)
library(terra)
library(r2r)
library(stringr)

# Setting working directory
setwd("/cloud/project/visualizeAmericaCities")

# Census API Key
census_api_key("c6b08260100da512461c050868ee3ff16629f4ca", install=TRUE, overwrite=TRUE)

# Hash maps for all the information
citiesMap_df = read_xlsx('county_state_data.xlsx', col_names=FALSE, sheet = 'citiesMap')
citiesMap_df = citiesMap_df %>% 
  rename(
    V1 = ...1,
    V2 = ...2
  )
vec1 = as.numeric(citiesMap_df$V1)
vec2 = as.numeric(citiesMap_df$V2)
citiesMap = hashmap()
citiesMap[vec1] = vec2

statesMap_df = read_xlsx('county_state_data.xlsx', col_names=FALSE, sheet = 'statesMap')
statesMap_df = statesMap_df %>% 
  rename(
    V1 = ...1,
    V2 = ...2
  )
vec1 = as.numeric(statesMap_df$V1)
vec2 = as.numeric(statesMap_df$V2)
statesMap = hashmap()
statesMap[vec1] = vec2

countyNameMap_df = read_xlsx('county_state_data.xlsx', col_names=FALSE, sheet = 'countyNameMap')
countyNameMap_df = countyNameMap_df %>% 
  rename(
    V1 = ...1,
    V2 = ...2
  )
vec1 = as.numeric(countyNameMap_df$V1)
vec2 = countyNameMap_df$V2
countyNameMap = hashmap()
countyNameMap[vec1] = vec2

stateNameMap_df = read_xlsx('county_state_data.xlsx', col_names=FALSE, sheet = 'stateNameMap')
stateNameMap_df = stateNameMap_df %>% 
  rename(
    V1 = ...1,
    V2 = ...2
  )
vec1 = as.numeric(stateNameMap_df$V1)
vec2 = stateNameMap_df$V2
stateNameMap = hashmap()
stateNameMap[vec1] = vec2

# Identify variables for mapping
race_vars <- c(
  Hispanic = "P2_002N",
  White = "P2_005N",
  Black = "P2_006N",
  Asian = "P2_008N"
)


# For loop iterating through all 50 counties and adding to their own dataframes
df_vector = c()
for (i in 1:length(countyNameMap)) {
  city_race <- get_decennial(
    geography = "tract",
    variables = race_vars,
    state = statesMap[as.numeric(i)],
    county = citiesMap[as.numeric(i)],
    geometry = TRUE,
    year = 2020
  )
  dummy_df = city_race
  # # paste(countyNameMap[as.character(i)], stateNameMap[as.character(i)], sep='_')
  # dummy_df = data.frame(GEOID=city_race$GEOID, NAME=city_race$NAME, variable=city_race$variable, 
  #                       value=city_race$value, geometry=city_race$geometry)
  # # Splitting state and county into separate variables
  # dummy_df[c('TRACT', 'COUNTY', 'STATE')] = str_split_fixed(dummy_df$NAME, ', ', 3)
  # # Rearrange columns and remove old NAME column
  # dummy_df = dummy_df[c('GEOID', 'TRACT', 'COUNTY', 'STATE', 'variable', 'value', 'geometry')]
  county_name = countyNameMap[as.numeric(i)]
  state_name = stateNameMap[as.numeric(i)]
  arg_name = do.call("substitute", list(county_name)) # Get argument name
  var_name = paste(arg_name, state_name, "df", sep="_") # Construct the name
  assign(var_name, dummy_df, env=.GlobalEnv)
  # Append to vector used to make rda file
  df_vector = append(df_vector, var_name)
}

# Save all the dataframes to rda file
save(list=df_vector, file="counties_dataframes.rda")



# # Convert city_race to a DF and store into RDA
# # city_name = countyNameMap[user_input]
# create_city_df <- function(city_name, city_race) {
#   dummy_df = data.frame(GEOID=city_race$GEOID, NAME=city_race$NAME, RACE=city_race$variable, 
#                         COUNT=city_race$value, GEOMETRY=city_race$geometry)
#   arg_name = do.call("substitute", list(city_name)) # Get argument name
#   var_name = paste(arg_name, "df", sep="_") # Construct the name
#   assign(var_name, dummy_df, env=.GlobalEnv)
#   do.call("save", list(paste(arg_name, "df", sep="_"), file="cityData.rda"))
# }
# create_city_df(countyNameMap[user_input], city_race)
# 
# # Add current city df to existing RDA
# e <- new.env(parent = emptyenv())
# load("cityData.rda", envir = e)
# do.call("save", c(ls(envir = e), list(envir = e, file = "cityData.rda")))
