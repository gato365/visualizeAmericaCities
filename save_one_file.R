# Installing packages
library(plotly)
library(bslib)
library(shiny)
library(readxl)
library(tidycensus)
library(ggplot2)
library(terra)
library(r2r)
library(stringr) 
library(tidyverse)
library(sf)
library(kableExtra)


# Setting working directory
#setwd("/cloud/project/visualizeAmericaCities")
setwd("D:/Old Desktop/Desktop/Cal Poly/Frost SURP/visualizeAmericaCities")
user_input = 1

# Census API Key
census_api_key("c6b08260100da512461c050868ee3ff16629f4ca", install=TRUE, overwrite=TRUE)

###############################################
## Main County Mapping
###############################################
# Hash map mapping number to county ANSI code
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

# Hash map mapping number to state FISP code
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

# Hash map mapping number to county name
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

# Hash map mapping number to state USPS code
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

###############################################
## Borough Mapping
###############################################
# Hash map mapping number to county ANSI code
boroughMap_df = read_xlsx('borough_state_data.xlsx', col_names=FALSE, sheet = 'boroughMap')
boroughMap_df = boroughMap_df %>% 
  rename(
    V1 = ...1,
    V2 = ...2
  )
vec1 = as.numeric(boroughMap_df$V1)
vec2 = as.numeric(boroughMap_df$V2)
boroughMap = hashmap()
boroughMap[vec1] = vec2

# Hash map mapping number to state FISP code
bStatesMap_df = read_xlsx('borough_state_data.xlsx', col_names=FALSE, sheet = 'bStatesMap')
bStatesMap_df = bStatesMap_df %>% 
  rename(
    V1 = ...1,
    V2 = ...2
  )
vec1 = as.numeric(bStatesMap_df$V1)
vec2 = as.numeric(bStatesMap_df$V2)
bStatesMap = hashmap()
bStatesMap[vec1] = vec2

# Hash map mapping number to county name
boroughNameMap_df = read_xlsx('borough_state_data.xlsx', col_names=FALSE, sheet = 'boroughNameMap')
boroughNameMap_df = boroughNameMap_df %>% 
  rename(
    V1 = ...1,
    V2 = ...2
  )
vec1 = as.numeric(boroughNameMap_df$V1)
vec2 = boroughNameMap_df$V2
boroughNameMap = hashmap()
boroughNameMap[vec1] = vec2

# Hash map mapping number to state USPS code
bStateNameMap_df = read_xlsx('borough_state_data.xlsx', col_names=FALSE, sheet = 'bStateNameMap')
bStateNameMap_df = bStateNameMap_df %>% 
  rename(
    V1 = ...1,
    V2 = ...2
  )
vec1 = as.numeric(bStateNameMap_df$V1)
vec2 = bStateNameMap_df$V2
bStateNameMap = hashmap()
bStateNameMap[vec1] = vec2

# Identify variables for mapping
race_vars <- c(
  Hispanic = "P2_002N",
  White = "P2_005N",
  Black = "P2_006N",
  Asian = "P2_008N"
)

city_race <- get_decennial(
  geography = "tract",
  variables = race_vars,
  state = statesMap[user_input],
  county = citiesMap[user_input],
  geometry = TRUE,
  year = 2020
)

# Removing empty sf objects
city_race = city_race[!st_is_empty(city_race$geometry),]
city_race = city_race %>% 
  group_by(NAME) %>% 
  mutate(total_pop = sum(value)) %>% 
  mutate(hispanic_pct = sum(ifelse(variable=="Hispanic", value/total_pop, 0))*100,
         white_pct = sum(ifelse(variable=="White", value/total_pop, 0))*100,
         black_pct = sum(ifelse(variable=="Black", value/total_pop, 0))*100,
         asian_pct = sum(ifelse(variable=="Asian", value/total_pop, 0))*100)

Data_Frame <- data.frame (
  Training = c("Strength", "Stamina", "Other"),
  Pulse = c(100, 150, 120),
  Duration = c(60, 30, 45)
)

saveRDS(city_race, file = "test_data.rds")
