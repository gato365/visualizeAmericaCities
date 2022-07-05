library(tidycensus)
library(ggplot2)
library(htmltab)
# install.packages('htmltab')


df = htmltab('https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population',5)
