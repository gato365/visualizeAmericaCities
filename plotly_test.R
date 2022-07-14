library(plotly)
library(sf)

p <=     
  if (require("maps")) {
    data(us.cities)
    capitals <- subset(us.cities, capital == 2)
    ggplot(capitals, aes(long, lat)) +
      borders("state") +
      geom_point(aes(size = pop)) +
      scale_size_area() +
      coord_quickmap()
  }

plotly::ggplotly(p)

# test north carolina map (counties)
nc = st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) %>% 
  mutate(x = factor(c(rep("a", 50), rep("b", 50))),
         TEXT = paste(NAME, AREA))

# It works when I've got a continuous fill scale
p1 = nc %>% 
  ggplot() +
  geom_sf(aes(fill = AREA, text = TEXT))
#> Warning: Ignoring unknown aesthetics: text

ggplotly(p1, tooltip = "text") %>% 
  style(hoverlabel = list(bgcolor = "white"), hoveron = "text")

# test for scatter plot
p = ggplot(mtcars, aes(wt, mpg))
p =  p + geom_point(aes(text=rownames(mtcars)))
ggplotly(p)







