# Tags: maps, scrape, golf/pga, jitter, load/read-in from desktop

library(here)
library(tidyverse)
library(readxl)
install.packages("ggmap")
install.packages("mapdata")
install.packages("maps")
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)

jib <- read_excel(here("Desktop", "R", "jib", "location_data", "data", "JIB Location Listings 8.1.17.xlsx"), sheet = 2)
jib

usa <- map_data("usa")
states <- map_data("state")
dim(states)
jib_states <- subset(states, region %in% c("arizona", "california", "washington", "colorado", 
                                           "florida", "georgia", "idaho", "utah",
                                           "illinois", "indiana", "kansas", "louisiana",
                                           "maryland", "michigan", "missouri", "north carolina",
                                           "nevada", "new mexico", "oklahoma", "texas", "oregon",
                                           "colorado", "kentucky", "tennessee", "south carolina"))
jib_lat_lon <- jib %>%
  select(Latitude, Longitude)
jib_lat_lon
ggplot(data = jib_states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") + 
  coord_fixed(1.3) +
  geom_point(data = jib, aes(x = Longitude, y = Latitude), color = "black", size = .0001)
  
############################################################################################

# go to pgatour.com. scrape all of the data for driving distance. multiple pages. 1980-2018
# then plot it out x=year, y=distance. each player for each year to have a dot. plot smoothing line
# take that graph and say what happened when it happened and why.

url <- "https://www.pgatour.com/stats/stat.101.1980.html"
library(rvest)
library(tidyverse)

prefix <- "https://www.pgatour.com/stats/stat.101."
suffix <- ".html"

years <- seq(1980, 2018, 1)
pga_data <- NULL
for (year in years) {
  #create equation/defining urls
  new_url <- paste0(prefix, year, suffix)
  # creating a data frame to fill each time
  temp <- new_url %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="statsTable"]') %>%
    html_table() %>%
    flatten_df() %>%
    mutate(year = year)
  # creating full data set
  pga_data <- rbind(pga_data, temp)
  temp <- NULL
}

pga_data <- pga_data %>%
  janitor::clean_names()

pga_data %>%
  ggplot(aes(x=year, y=avg)) +
  geom_jitter(size=.75, alpha = .4) +
  theme_classic() +
  labs(x="Year", y="Avg Driving Distance") +
  geom_smooth(color = "red")

pga_data %>%
  group_by(year) %>%
  summarize(avg = mean(avg)) %>%
  ggplot(aes(x=year, y=avg)) +
  coord_cartesian(ylim = c(250, 300)) + 
  geom_bar(stat="identity") +
  theme_classic() +
  labs(x="Year", y="Avg Driving Distance")

######################## Scraping Practice #################################
url <- "https://www.baseball-reference.com/leagues/MLB/2018.shtml"
library(rvest)
library(tidyverse)

prefix <- "https://www.baseball-reference.com/leagues/MLB/"
suffix <- ".shtml"

years <- seq(2000, 2018, 1)
baseball_data <- NULL
for (year in years) {
  #create equation/defining urls
  new_url <- paste0(prefix, year, suffix)
  # creating a data frame to fill each time
  temp <- new_url %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="teams_standard_batting"]') %>%
    html_table() %>%
    flatten_df() %>%
    mutate(year = year)
  # creating full data set
  baseball_data <- rbind(baseball_data, temp)
  temp <- NULL
  #It kept putting the column names as a row every iteration, so this will filter the extra ones out
  baseball_data <- baseball_data %>% filter(Tm != "Tm")
}

#Read in same table from above from desktop
test <- read_csv("/Users/david.sager/Desktop/testing_testing.csv")
test



