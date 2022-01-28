library(sp)
NLD <- readRDS("NLD.rds")
library(tibble) # better printing
library(dplyr)  # obviously
NLD@data %>% as_tibble()
library(ggplot2)

bird <- read.csv("nestData//2002-2013.csv")
bird <- bird[bird[,"latitude"] > 51,]
birdupdated <- cbind(bird, predicted_behaviors_oldRF[10087:(nrow(bird)+10086),])
  


ggplot(NLD) +
  theme_minimal()+  # no backgroundcolor
  geom_polygon( aes(x = long, y = lat, group = group),
                color = "white",   # color is the lines of the region
                fill = "#9C9797")+ # fill is the fill of every polygon.
  coord_map()+
  geom_point(data = bird, aes(x=longitude, y =latitude))
