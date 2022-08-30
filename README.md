# visualizeAmericaCities




# 1. General Information



## 1a. Race and Their Codes

- Asian = B02001_005E
- African American = B02001_003E
- White = B02001_002E 
- American Indian and Alaska Native = B02001_004E 
- Native Hawaiian and Other Pacific Islander = B02001_002E


## 1b. State - County - City

 - California - Santa Clara = CA - Santa Clara County  - San Jose
 - Maryland - Baltimore County = MD - Baltimore County -  Baltimore
 - Georgia - Fulton County = GA - Fulton County - Atlanta
 - Massachusetts - Suffolk County = MA - Suffolk County - Boston
 - Illinois - Cook County = IL - Cook County - Chicago
 - Michigan - Wayne County = MI - Wayne County - Detroit
 - Texas - Harris County = TX - Harris County - Dallas
 - California - Los Angeles County = CA - Los Angeles County - Los Angeles
 - Florida - Miami-Dade County = FL - Miami-Dade County - Miami
 - New York - New York County = NY - New York County - New York
 - Philadelphia - Philadelphia County = PHI - Philadelphia County - Philadelphia
 - Arizona - Maricopa County = AZ - Maricopa County - Phoenix
 - California - San Francisco County = CA - San Francisco County - San Francisco
 - DC - District of Columbia County = DC - District of Columbia County - Washington DC
 
 
# 2. Summer 2022 Plans
 
## 2a. Data Wrangling:

0. [potentially] Obtain a API Key from (here)[https://api.census.gov/data/key_signup.html]
0. You will *select* one of the cities based on county info
1. You will then *Extract* data from Census via R's API OR Python's API OR directly from Census  website (last option will most likely not occur, but it will be a last option)
2. *Store* data locally (on your machine or in the cloud), yours first
3. *Plot* data using R
4. *Add* aesthetics to plot (different races, with random points)
5. *Get* remaining cities data
6. *Develop* R shiny (web application)to be able to display races by city

-------

## 2b. Statistical Analysis:

- Chi-Square Goodness of Fit
- Chi-Square Test of Homogeneity

# 3. Summer 2022 Recap

## 3a. Completed Tasks:
 - R Shiny webapp
 - Interactive map of demographics for the top-50 most populated counties in the United States
 - Interactive map of demographics where user can select one race to display
 - Chi-Square goodness of fit table
 - Interactive map of top-10 and bottom-10 counties corresponding to a distribution

## 3b. Future Tasks:
 - Speed up the runtime of ggplotly.
   - Could consider doing this task in Python, but data would need to be saved in a different way.
 - UI improvements could be made to the R Shiny webapp
 - Instead of only having maps for the top-50 counties, maybe find a way to map the entire continental US