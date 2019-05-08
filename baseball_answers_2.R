#1. How has the rate of walks (per team for nine innings) changed over the history of baseball? GGplot this.
#2. What fraction of baseball games in 1968 were shutouts? Compare this fraction with the fraction of shutouts in the 2012 baseball season.
#3. How has the avg career span changed throughout the decades?

#1. First, look at just 1 team
head(teams)
teams %>%
  select(yearID, teamIDBR, AB, BB) %>%
  mutate(BB_percent = BB/AB) %>%
  filter(teamIDBR == "BOS") %>%
ggplot(aes(x = yearID, y = BB_percent)) +
  geom_point()

#1.2. Then, look at all teams
head(teams)
teams %>%
  select(yearID, teamIDBR, AB, BB) %>%
  mutate(BB_percent = BB/AB,
         decade = yearID - (yearID %% 10)) %>%
  group_by(teamIDBR, decade) %>%
  summarise(BB_percent = mean(BB_percent)) %>%
  ggplot(aes(x = teamIDBR, y = BB_percent, group = decade, col = factor(decade))) +
  geom_point()
  
  
  

#2
head(pitching)
pitching %>%
  filter(yearID == 1968) %>%
  summarise(total_SHO = sum(SHO), 
            total_games = sum(GS)) %>%
  mutate(percent = total_SHO/total_games)
# total % 1968 = 8.58%

pitching %>%
  # NEXT. Make a graph over the years.
  group_by(yearID) %>%
  summarise(total_SHO = sum(SHO), 
            total_games = sum(GS)) %>%
  mutate(percent = total_SHO/total_games) %>%
  ggplot(aes(x = yearID, y = percent)) + 
  geom_point()

#3 
head(people)
glimpse(people)
people %>%
  select(playerID, debut, finalGame) %>%
  mutate(debut = lubridate::date(debut), 
         finalGame = lubridate::date(finalGame),
         decade = lubridate::year(debut),
         decade = decade - (decade %% 10)) %>%
  mutate(career = difftime(finalGame, debut, units = "days")) %>%
  mutate(career_years = career / 365) %>%
  group_by(decade) %>%
  summarise(avg_career = mean(career_years, na.rm = TRUE),
            sd_career = sd(career_years)) %>%
  ggplot(aes(x = decade, y = avg_career)) +
  geom_point()


####
pitching %>%
  filter(str_detect(playerID, "gibsobo01")) %>%
  filter(yearID == 1968) %>%
  mutate(gc_frac = CG/GS, ratio = SO/BB) %>%
  data.frame()

pitching %>%
  filter(str_detect(playerID, "gibsobo01")) %>%
  filter(yearID == 1968) %>%
  mutate(innings_pitched = IPouts/3) %>%
  data.frame()
  