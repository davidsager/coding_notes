library(tidyverse)

getwd()
setwd("~/Desktop")
batting <- read_csv("baseballdatabank-master/core/Batting.csv")

head(batting)

batting %>%
  group_by(yearID) %>%
  summarise(total_bb = sum(BB), avg_bb = mean(BB)) %>%
  mutate(total_players = total_bb/avg_bb) %>%
  arrange(desc(total_bb)) %>%
  ggplot(aes(x = yearID, y = total_bb)) + 
  geom_point() + 
  geom_smooth()

getwd()
pitching <- read_csv("baseballdatabank-master/core/Pitching.csv")

head(pitching)
dim(pitching)
arrange(pitching, yearID, SHO)

head(pitching)
pitching %>% 
  filter(yearID == 1968) %>%
  summarise(sum(SHO))

select(pitching, yearID = 1968, SHO)

sum(pitching$SHO[pitching$yearID==1968])

pitching[]

_____
nycflights13::airlines
library(nycflights13)
dim(flights)

nycflights13::planes
head(planes)
arrange(planes, year:model)

two <- c("AA", "AS")
lut <- c("AA" = "American", 
         "AS" = "Alaska", 
         "B6" = "JetBlue")
two <- lut[two]
two
#####
two <- tibble(ac = c("AA", "AS"),
              num = c(854, 896))
lut <- tibble(ac = c("AA", "AS", "B6"),
              airline = c("American", "Alaska", "JetBlue"))

joined <- lut %>% left_join(two)

names <- c("name1", "name2", "name3")
newname <- c("name1" = "Stacy", "name2" = "Jeremy", "name3" = "Rachel")
names <- newname[names]
names
