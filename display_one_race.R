library(plotly)
library(tidycensus)
library(ggplot2) 
library(terra)
library(r2r)
library(sf)
library(readxl)



# Setting working directory
#setwd("/cloud/project/visualizeAmericaCities")
setwd("D:/Old Desktop/Desktop/Cal Poly/Frost SURP/visualizeAmericaCities")

# Census API Key
census_api_key("c6b08260100da512461c050868ee3ff16629f4ca", install=TRUE, overwrite=TRUE)
# Get user input for city
# !!!!!!IMPORTANT!!!!!! RUN THIS LINE SEPARATELY -----
user_input = 1 # indianpolis = 15, baltimore = 30
# -----

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


# countyNameMap[c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17",
#                 "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33",
#                 "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50")] =
#   c("new_york", "los_angeles", "chicago", "houston", "phoenix", "philadelphia", "san_antonio", "san_diego", "dallas", 
#     "san_jose", "austin", "jacksonville", "fort_worth", "columbus", "indianapolis", "charlotte", "san_francisco",
#     "seattle", "denver", "oklahoma_city", "nashville", "el_paso", "washington", "boston", "las_vegas", "portland", 
#     "detroit", "louisville", "memphis", "baltimore", "milwaukee", "albuquerque", "fresno", "tucson", "sacramento",
#     "mesa", "kansas_city", "atlanta", "omaha", "colorado_springs", "raleigh", "virginia_beach", "long_beach", "miami",
#     "oakland", "minneapolis", "tulsa", "bakersfield", "wichita", "arlington")

load(file="counties_dataframes.rda")

# Identify variables for mapping
race_vars <- c(
  Hispanic = "P2_002N",
  White = "P2_005N",
  Black = "P2_006N",
  Asian = "P2_008N"
)
# race_vars <- c(
#   Hispanic = "P2_002N"
# )

# state = statesMap[user_input],
# county = citiesMap[user_input],
# Get data from tidycensus
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
  filter(!(variable %in% c("White", "Black", "Asian")))


# # Splitting tract and state
# city_race[c('tract', 'county', 'state')] = str_split_fixed(city_race$NAME, ', ', 3)
# # Rearrange columns and remove old NAME column
# city_race = city_race[c('GEOID', 'tract', 'county', 'state', 'variable', 'value', 'geometry')]

city_race3 <- get("chicago_IL_df")
# Convert data to dots
city_dots <- as_dot_density(
  city_race,
  value = "value",
  # Best values for each city: 
  values_per_dot = 400, # 100 -> 800
  group = "variable"
)
# Use one set of polygon geometries as a base layer
city_base <- city_race[city_race$variable == "Hispanic", ]
# Map with ggplot2
p <- ggplot() +
  geom_sf(data = city_base,
          fill = "white",
          color = "grey") +
  geom_sf(data = city_dots,  
          aes(color = variable), # variable -> "red"
          size = 0.3) + # 0.01 -> 0.3
  theme_void() +
  scale_color_manual(values = c("Hispanic" = "orange"))

plot(p)

#grouped_df = city_race[1:5000,]
# grouped_df = city_race
# grouped_df = grouped_df %>% 
#   group_by(NAME) %>% 
#   mutate(total_population = sum(value)) %>% 
#   mutate(race_percent = (value/total_population) * 100)
# 
# 
# city_dots2 <- as_dot_density(
#   grouped_df,
#   value = "value",
#   # Best values for each city: 
#   values_per_dot = 400, # 100 -> 800
#   group = "variable"
# )
# 
# grouped_df_base <- grouped_df[grouped_df$variable == "Hispanic", ]
# 
# p3 <- ggplot() +
#   geom_sf(data = grouped_df_base,
#           fill = "white",
#           color = "grey") +
#   geom_sf(data = city_dots2,  
#           aes(color = variable), # variable -> "red"
#           size = 0.3) + # 0.01 -> 0.3
#   theme_void() +
#   scale_color_manual(values = c("Black" = "blue",
#                                 "Asian" = "red",
#                                 "White" = "green",
#                                 "Hispanic" = "orange"))
# 
# p2 <- ggplot(data=grouped_df) +
#   geom_sf(data = grouped_df_base,
#           aes(text=
#                 paste(
#                   paste(
#                     "Hispanic: ", round(hispanic_pct, digits=2), "%", sep=""
#                   ),
#                   paste(
#                     "White: ", round(white_pct, digits=2), "%", sep=""
#                   ),
#                   paste(
#                     "Black: ", round(black_pct, digits=2), "%", sep=""
#                   ),
#                   paste(
#                     "Asian: ", round(asian_pct, digits=2), "%", sep=""
#                   ),
#                   sep="\n"
#                 ),
#               color=NAME
#           )
#           #mapping = aes(fill = AREA),
#           #fill = "white",
#           #color = "grey"
#   ) +
#   geom_sf(data = city_dots2,  
#           aes(color = variable), # variable -> "red"
#           size = 0.3) + # 0.01 -> 0.3
#   
#   
#   # geom_sf_text(data = grouped_df,
#   #              mapping = aes(label=NAME)) +
#   #geom_text(aes(label=NAME), data = grouped_df) +
#   theme_void() +
#   scale_color_manual(values = c("Black" = "blue",
#                                 "Asian" = "red",
#                                 "White" = "green",
#                                 "Hispanic" = "orange"))
# 
# gg_2 <- ggplotly(p2)
# 
# gg_3 <- gg_2 %>% 
#   style(
#     hoveron = "fills",
#     # override the color mapping
#     # line.color = toRGB("gray40"),
#     # don't apply these style rules to the first trace, which is the background graticule/grid
#     traces = seq.int(3, length(gg_2$x$data))
#   ) %>%
#   hide_legend()

# summarise(total_population = sum(value))