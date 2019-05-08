library(here)
library(tidyverse)

teams <- read_csv(here("data", "core", "Teams.csv"))


head(teams)
glimpse(teams)

teams %>%
  # group_by(yearID, lgID) %>%
  filter(yearID == 1968) %>%
  summarise(total_bb = sum(BB), 
            teams = n()) %>%
  mutate(bb_team = total_bb/teams) %>%
  tail()
  # ggplot(aes(x = yearID, y = bb_team)) + 
  # geom_point()

#########
getwd()
pitching <- read_csv("baseballdatabank-master/core/Pitching.csv")
######
pitching %>%
  filter(yearID == 1968) %>%
  summarise(total_so = sum(SHO),
            total_games = sum(GS)) %>%
  mutate(percent = (total_so/total_games)*100)
#1968 SHO/Game percentage = 8.58
pitching %>%
  filter(yearID == 2012) %>%
  summarise(total_so = sum(SHO),
            total_games = sum(GS)) %>%
  mutate(percent = (total_so/total_games)*100)
#2012 SHO/Game percentage = 1.42
####### FIND the avg career lenght using People.csv
getwd()
#people <- read_csv("baseballdatabank-master/core/People.csv")
people <- read_csv(here("data", "core", "People.csv"))
head(people)
glimpse(people)


people %>% 
  select(playerID, debut, finalGame) %>%
  mutate(debut = lubridate::date(debut), 
         finalGame = lubridate::date(finalGame),
         decade = lubridate::year(debut),
         decade = decade - (decade %% 10)) %>%
  mutate(career = difftime(finalGame, debut, units = "days")) %>%
  mutate(career_years = career/365) %>%
  group_by(decade) %>%
  summarise(avg_career = mean(career_years, na.rm = TRUE),
            sd_career = sd(career_years))
