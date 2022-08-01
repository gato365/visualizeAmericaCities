library(tidycensus)
library(tidyverse)
library(readxl)
library(plotly)
library(ggplot2) 
library(terra)
library(r2r)
library(sf)


## Set working directory
setwd("D:/Old Desktop/Desktop/Cal Poly/Frost SURP/visualizeAmericaCities")
#setwd("/cloud/project/visualizeAmericaCities")

###############################################
## Main County Mapping
###############################################
## Hash map mapping number to county ANSI code
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

## Hash map mapping number to state FISP code
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

## Hash map mapping number to county name
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

## Hash map mapping number to state USPS code
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

dropDownVector = c()
for (i in 1:length(countyNameMap)) {
  ## Format County
  county_name = countyNameMap[as.numeric(i)] # extract county name
  state_name = stateNameMap[as.numeric(i)] # extract state name
  split_county = strsplit(as.character(county_name), "_") # split by underscore
  temp_vector = c()
  for (i in 1:length(split_county[[1]])) {
    temp_vector = append(temp_vector, split_county[[1]][i]) # append each word
  }
  split_county = paste(temp_vector, collapse=" ") # combine to a string
  split_county = str_to_title(split_county) # uppercase first letter of each word for formatting
  
  ## Format State
  state_name = state_name[[1]][1]
  county_state_name = paste(split_county, state_name, sep=", ")
  
  ## Append to main vector
  dropDownVector = append(dropDownVector, county_state_name)
} 
## BOROUGHS
dropDownVector2 = c()
for (i in 1:length(boroughNameMap)) {
  ## Format Borough
  borough_name = boroughNameMap[as.numeric(i)] # extract county name
  state_name = bStateNameMap[as.numeric(i)] # extract state name
  split_borough = strsplit(as.character(borough_name), "_") # split by underscore
  temp_vector = c()
  for (i in 1:length(split_borough[[1]])) {
    temp_vector = append(temp_vector, split_borough[[1]][i]) # append each word
  }
  split_borough = paste(temp_vector, collapse=" ") # combine to a string
  split_borough = str_to_title(split_borough) # uppercase first letter of each word for formatting
  
  ## Format State
  state_name = state_name[[1]][1]
  county_state_name = paste(split_borough, state_name, sep=", ")
  
  ## Append to main vector
  dropDownVector2 = append(dropDownVector2, county_state_name)
}

## COUNTIES
rda_strings = c()
for (i in 1:length(dropDownVector)) {
  temp_string = dropDownVector[i]
  split_string = str_trim(unlist(strsplit(temp_string,",")))
  split_string[1] = tolower(split_string[1])
  split_string[1] = str_replace(split_string[1], " ", "_")
  split_string = append(split_string, "df")
  joined_string = paste(split_string, collapse="_")
  rda_strings = append(rda_strings, joined_string)
}

## BOROUGHS
borough_rda_strings = c()
for (i in 1:length(dropDownVector2)) {
  temp_string = dropDownVector2[i]
  split_string = str_trim(unlist(strsplit(temp_string,",")))
  split_string[1] = tolower(split_string[1])
  split_string[1] = str_replace(split_string[1], " ", "_")
  split_string = append(split_string, "df")
  joined_string = paste(split_string, collapse="_")
  borough_rda_strings = append(borough_rda_strings, joined_string)
}

## Join all the strings
all_rda_strings = c(rda_strings, borough_rda_strings)
## Remove vectors that have boroughs
all_rda_strings = all_rda_strings[! all_rda_strings %in% c('new_york_NY_df')]

## Load all the dataframes
load(file="counties_dataframes.rda")
load(file="boroughs_dataframes.rda")

dist_race = "White"
dist_percent = c(0.35, 0.59, 0.01, 0.05)
dist_sample_size = 1000

# test_race = get("san_jose_CA_df")
# test_race = test_race %>% 
#   mutate(hispanic_count = round((total_pop * (hispanic_pct/100)), digits=0),
#          white_count = round((total_pop * (white_pct/100)), digits=0),
#          black_count = round((total_pop * (black_pct/100)), digits=0),
#          asian_count = round((total_pop * (asian_pct/100)), digits=0))
# test_race = test_race %>% 
#   rowwise() %>% 
#   mutate(
#     test_stat = chisq.test(c(hispanic_count, white_count, black_count, asian_count), p=dist_percent)$statistic,
#     p_val = chisq.test(c(hispanic_count, white_count, black_count, asian_count), p=dist_percent)$p.value
#   )
# ordered_race = test_race %>% 
#   group_by(NAME) %>% 
#   arrange(desc(p_val))
# 
# ordered_race = head(ordered_race[!duplicated(ordered_race$NAME),], 10)
# ordered_race = ordered_race[c("NAME", "total_pop", "hispanic_count", "white_count",
#                               "black_count", "asian_count", "p_val")]
# 
# top_10_df = data.frame()
# top_10_df = rbind(top_10_df, ordered_race)
# top_10_df = head(top_10_df[order(top_10_df$p_val),], 10)

top_10_df = data.frame()
## Loop to traverse every single tract
for (i in 1:length(all_rda_strings)) {
  #city_race = get(all_rda_strings[i])
  print(all_rda_strings[i])
  city_race = get("philadelphia_PA_df")
  ## Generate necessary columns
  city_race = city_race %>% 
    mutate(hispanic_count = round((total_pop * (hispanic_pct/100)), digits=0),
           white_count = round((total_pop * (white_pct/100)), digits=0),
           black_count = round((total_pop * (black_pct/100)), digits=0),
           asian_count = round((total_pop * (asian_pct/100)), digits=0))
  ## Drop NAs
  city_race = na.omit(city_race)
  ## Perform chi-squared testing
  city_race = city_race %>% 
    rowwise() %>% 
    mutate(
      test_stat = chisq.test(c(hispanic_count, white_count, black_count, asian_count), p=dist_percent)$statistic,
      p_val = chisq.test(c(hispanic_count, white_count, black_count, asian_count), p=dist_percent)$p.value
    )
  ## Sort by descen ding p-value
  ordered_race = city_race %>% 
    group_by(NAME) %>% 
    arrange(desc(p_val))
  #print(ordered_race)
  ## Only keep top 10 p-values, and subset dataframe
  ordered_race = head(ordered_race[!duplicated(ordered_race$NAME),], 10)
  ordered_race = ordered_race[c("NAME", "total_pop", "hispanic_count", "white_count",
                                "black_count", "asian_count", "p_val", "test_stat")]
  ## Keep top 10
  top_10_df = rbind(top_10_df, ordered_race)
  top_10_df = top_10_df[order(top_10_df$p_val),]
  print(top_10_df)
  #top_10_df = top_10_df[!duplicated(top_10_df),]
  top_10_df = head(top_10_df, 10)
  #print(top_10_df)
}


## IDEA FOR HOW ALGORITHM WORKS
## vectorized, perform chi squared test on each row
## get 10 smallest p values? 
## add to the solution dataframe; if the length of the solution dataframe > 10
##    remove the largest p-value

grouped_df = city_race
grouped_df = grouped_df %>% 
  group_by(NAME) %>% 
  mutate(total_population = sum(value)) %>% 
  mutate(race_percent = (value/total_population) * 100)

city_dots2 <- as_dot_density(
  grouped_df,
  value = "value",
  # Best values for each city: 
  values_per_dot = 400, # 100 -> 800
  group = "variable"
)

grouped_df_base <- grouped_df[grouped_df$variable == "Hispanic", ]


p2 <- ggplot(data=grouped_df) +
  geom_sf(data = grouped_df_base,
          aes(text=
                paste(
                  paste(
                    "Hispanic: ", round(hispanic_pct, digits=2), "%", sep=""
                  ),
                  paste(
                    "White: ", round(white_pct, digits=2), "%", sep=""
                  ),
                  paste(
                    "Black: ", round(black_pct, digits=2), "%", sep=""
                  ),
                  paste(
                    "Asian: ", round(asian_pct, digits=2), "%", sep=""
                  ),
                  sep="\n"
                ),
              color=NAME,
              fill=test_stat
          )
          #mapping = aes(fill = AREA),
          #fill = "white",
          #color = "grey"
  ) +
  geom_sf(data = city_dots2,  
          aes(color = variable), # variable -> "red"
          size = 0.3) + # 0.01 -> 0.3
  
  
  # geom_sf_text(data = grouped_df,
  #              mapping = aes(label=NAME)) +
  #geom_text(aes(label=NAME), data = grouped_df) +
  theme_void() +
  scale_color_manual(values = c("Black" = "blue",
                                "Asian" = "red",
                                "White" = "green",
                                "Hispanic" = "orange"))

gg_2 <- ggplotly(p2)

gg_3 <- gg_2 %>% 
  style(
    hoveron = "fills",
    # override the color mapping
    # line.color = toRGB("gray40"),
    # don't apply these style rules to the first trace, which is the background graticule/grid
    traces = seq.int(3, length(gg_2$x$data))
  ) %>%
  hide_legend()






























city_race = get("manhattan_NY_df")