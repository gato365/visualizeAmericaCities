library(tidycensus)
library(ggplot2)
library(terra)
library(r2r)
library(stringr)

# Setting working directory
setwd("/cloud/project")

# Census API Key
census_api_key("c6b08260100da512461c050868ee3ff16629f4ca", install=TRUE, overwrite=TRUE)

# Hash map for get_decennial county
citiesMap = hashmap()
citiesMap[c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17",
            "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33",
            "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50")] =
  c(061, 037, 031, 201, 013, 101, 029, 073, 113, 085,
    015, 031, 439, 049, 097, 119, 075, 033, 031, 109,
    037, 141, 043, 025, 003, 051, 163, 111, 157, 005,
    079, 001, 019, 019, 067, 013, 209, 121, 055, 041,
    183, 810, 037, 086, 001, 053, 143, 029, 173, 439)
# Hash map for get_decennial state
statesMap = hashmap()
statesMap[c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17",
            "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33",
            "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50")] =
  c(36, 06, 17, 48, 04, 42, 48, 06, 48, 06, 48, 12, 48, 39, 18, 37, 06, 53, 08, 40, 47, 48, 24, 25, 32, 41, 26, 21,
    47, 24, 55, 35, 06, 04, 06, 04, 29, 13, 31, 08, 37, 51, 06, 12, 06, 27, 40, 06, 20, 48)
# Hash map for df naming
countyNameMap = hashmap()
countyNameMap[c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17",
                "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33",
                "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50")] =
  c("new_york", "los_angeles", "chicago", "houston", "phoenix", "philadelphia", "san_antonio", "san_diego", "dallas", 
    "san_jose", "austin", "jacksonville", "fort_worth", "columbus", "indianapolis", "charlotte", "san_francisco",
    "seattle", "denver", "oklahoma_city", "nashville", "el_paso", "washington", "boston", "las_vegas", "portland", 
    "detroit", "louisville", "memphis", "baltimore", "milwaukee", "albuquerque", "fresno", "tucson", "sacramento",
    "mesa", "kansas_city", "atlanta", "omaha", "colorado_springs", "raleigh", "virginia_beach", "long_beach", "miami",
    "oakland", "minneapolis", "tulsa", "bakersfield", "wichita", "arlington")
stateNameMap = hashmap()
stateNameMap[c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17",
               "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33",
               "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50")] =
  c("NY", "CA", "IL", "TX", "AZ", "PA", "TX", "CA", "TX", "CA", "TX", "FL", "TX", "OH", "IN", "NC", "CA", "WA", "CO",
    "OK", "TN", "TX", "MD", "MA", "NV", "OR", "MI", "KY", "TN", "MD", "WI", "NM", "CA", "AZ", "CA", "AZ", "MO", "GA", 
    "NE", "CO", "NC", "VA", "CA", "FL", "CA", "MN", "OK", "CA", "KS", "TX")
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
    state = statesMap[as.character(i)],
    county = citiesMap[as.character(i)],
    geometry = TRUE,
    year = 2020
  )
  # paste(countyNameMap[as.character(i)], stateNameMap[as.character(i)], sep='_')
  dummy_df = data.frame(GEOID=city_race$GEOID, NAME=city_race$NAME, RACE=city_race$variable, 
                        COUNT=city_race$value, GEOMETRY=city_race$geometry)
  # Splitting state and county into separate variables
  dummy_df[c('TRACT', 'COUNTY', 'STATE')] = str_split_fixed(dummy_df$NAME, ', ', 3)
  # Rearrange columns and remove old NAME column
  dummy_df = dummy_df[c('GEOID', 'TRACT', 'COUNTY', 'STATE', 'RACE', 'COUNT', 'geometry')]
  county_name = countyNameMap[as.character(i)]
  state_name = stateNameMap[as.character(i)]
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
