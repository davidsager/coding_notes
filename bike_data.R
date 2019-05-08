################################################################################
#Tags. Spread, Gather, bike, clean names, janitor, 


library(tidyverse)
library(dplyr)
library(ggmap)
library(here)
library(plyr)
install.packages("leaflet")
library(leaflet)

bike <- read_csv(here("Desktop", "metro-bike-share-trip-data.csv")) %>%
  janitor::clean_names()

bike %>%
  janitor::clean_names() %>%
  mutate(duration = duration/60) %>%
  arrange(desc(duration)) %>%
  head() %>%
  ggmap +
  geom_point(aes(x=starting_station_lat, y=starting_station_longitude))

bike %>%
  group_by(starting_station_latitude, starting_station_longitude) %>%
  summarise(n = n()) %>%
  group_by(starting_station_latitude, starting_station_longitude) %>%
  summarise(n = sum(n)) %>%
  data.frame() %>%
  round(5)

bike %>%
  head %>%
  data.frame()

leaflet(data = bike[1:20,]) %>% addTiles() %>%
  addMarkers(bike$starting_station_latitude, bike$starting_station_longitude, popup = ~as.character(mag), label = ~as.character(mag))

################################################################################

#buid a matrix. col 1 is brand names (cars) 24 hours rows. Name each col hr1, hr2, etc.
#randomly fill each cell w a random integer (between 0-100). turn it into df and name rows/col. gather it so all hrs in 1 col.
#graph it by car.

m <- matrix((sample(1:100, 8*24, replace=T)), nrow = 8, ncol = 24)
#sample(1:100, 8*24, replace=T)
rownames(m) <- c("audi", "kia", "hyundai", "jeep", "dodge", "ram", "toyota", "mercedes")
colnames(m) <- c(paste0("hr", 1:24))
names <- rownames(m)
rownames(m) <- NULL
data <- cbind(names,m)
data <- data.frame(data)
tidy_data <- data %>%
  gather(hr, x, 2:25)
tidy_data %>%
  spread(hr, x)



