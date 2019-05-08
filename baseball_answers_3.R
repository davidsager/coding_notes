#1. How has the rate of walks (per team for nine innings) changed over the history of baseball? GGplot this.
#2. What fraction of baseball games in 1968 were shutouts? Compare this fraction with the fraction of shutouts in the 2012 baseball season.
#3. How has the avg career span changed throughout the decades?
#4. Bob Gibson 1968: Gibson started 34 games for the Cardinals in 1968. What fraction of these games were completed by Gibson? (b) What was Gibson’s ratio of strikeouts to walks this season?

#1
head(teams)
teams %>%
select(yearID, name, AB, BB) %>%
#do for 1 team
  mutate(BB_percent = BB/AB,
         decade = yearID - (yearID %% 10)) %>%
  group_by(name, decade) %>%
  summarise(BB_percent = mean(BB_percent)) %>%
filter(name == "Chicago Cubs") %>%
ggplot(aes(x = decade, y = BB_percent)) +
  geom_point()
#This is the average BB for Chicago Cubs every 10 years

#Now do for all teams
teams %>%
  select(yearID, name, AB, BB) %>%
  mutate(BB_percent = BB/AB,
         decade = yearID - (yearID %% 10)) %>%
  group_by(decade) %>%
  summarise(BB_percent = mean(BB_percent)) %>%
  ggplot(aes(x = decade, y = BB_percent, group = decade, col = factor(decade))) +
  geom_point()

#2. What fraction of baseball games in 1968 were shutouts? Compare this fraction with the fraction of shutouts in the 2012 baseball season.
head(pitching)
pitching %>%
  filter(yearID == "1968") %>%
  summarise(total_SHO = sum(SHO),
            total_games = sum(GS)) %>%
  mutate(SHO_percent = total_SHO/total_games)
# Find 2012 SHO
pitching %>%
  filter(yearID == "2012") %>%
  summarise(total_SHO = sum(SHO),
            total_games = sum(GS)) %>%
  mutate(SHO_percent = total_SHO / total_games)

#3. How has the avg career span changed throughout the decades?
head(people)
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
  ggplot(aes(x = decade, y = avg_career, group = decade, col = factor(decade))) +
  geom_point()

#4. Gibson started 34 games for the Cardinals in 1968. What fraction of these games were completed by Gibson?
head(pitching)
glimpse(pitching)
pitching %>%
  filter(str_detect(playerID, "gibsobo01")) %>%
  filter(yearID == 1968) %>%
  mutate(gc_frac = CG/GS, ratio = SO/BB) %>%
  data.frame

#4.2 (b) What was Gibson’s ratio of strikeouts to walks this season?
pitching %>%
  filter(str_detect(playerID, "gibsobo01")) %>%
  filter(yearID == 1968) %>%
  mutate(SO_W = SO/W) %>%
  data.frame

#4.2. One can compute Gibson’s innings pitched by dividing IPouts by three. How many innings did Gibson pitch this season? 
pitching %>%
  filter(str_detect(playerID, "gibsobo01")) %>%
  filter(yearID == 1968) %>%
  mutate(innings_pitched = IPouts / 3) %>%
  select(innings_pitched)

#4.3. (d) A modern measure of pitching effectiveness is WHIP, the average number of hits and walks allowed per inning. What was Gibson’s WHIP for the 1968 season?
pitching %>%
  filter(str_detect(playerID, "gibsobo01")) %>%
  filter(yearID == 1968) %>%
  mutate(WHIP = h + w)



W <- c(8, 21, 15, 21, 21, 22, 14)
L <- c(5, 10, 12, 14, 17, 14, 19)
Win.Pct <- W/(W+L)*100
Win.Pct
Year <- seq(1946, 1952)
Year
Age <- Year - 1921
Age
plot(x = Age, y = Win.Pct)
mean(Win.Pct)
100*sum(W)/(sum(W)+sum(L))
sort(W)
Cum_total <- cumsum(W)
plot(Age, Cum_total)

###########
#teams <- read_csv(here("data", "core", "Teams.csv")) %>% janitor::clean_names()

#1. What is the average number of home runs per game recorded in each decade? 
#1.1Does the rate of strikeouts show any correlationwith the rate of home runs?
library(here)
library(tidyverse)

batting <- read.csv(here("data", "core", "Batting.csv"))
#1
teams %>%
  select(yearID, teamID, G, HR) %>% 
  group_by(yearID) %>%
  summarize(yr_HR = sum(HR),
            yr_game = sum(G)) %>%
  mutate(hr_g = (yr_HR/yr_game)) %>%
  mutate(decade = yearID - (yearID %% 10)) %>%
  group_by(decade) %>% 
  summarise(hr_g = mean(hr_g)) %>%
  ggplot(aes(x = decade, y = hr_g)) + 
  geom_point()

#1.1
teams %>%
  select(yearID, G, SO) %>%
  mutate(decade = yearID - (yearID %% 10)) %>%
  group_by(decade) %>%
  summarise(decade_SO = sum(SO),
            decade_game = sum(G)) %>%
  ggplot(aes(x = decade, y = decade_SO)) + 
  geom_point()

teams %>%
  mutate(decade = yearID - (yearID %% 10)) %>%
  filter(decade == "1990")

#2. What effect has the introduction of the Designated Hitter (DH) in the American League 
#had in the difference in run scoring between the American and National Leagues?
  
teams %>%
  filter(yearID >= "1950" & yearID <= "1999") %>%
  group_by(yearID, lgID) %>%
  summarise(runs_game = sum(R)/sum(G)) %>%
  spread(lgID, runs_game) %>%
  mutate(diff = AL-NL) %>%
  ggplot(aes(x = yearID, y = diff)) +
  geom_point() + 
  geom_line() + 
  geom_hline(yintercept = 0, col = "blue") +
  geom_vline(xintercept = 1973, linetype="dashed", col = "red")
  

teams[which(!is.na(teams$lgID)),]

teams %>% filter(lgID == "AL") %>% arrange(yearID)
teams_no_na <- teams[complete.cases(teams$lgID),]

#3. How does the percentage of games completed by the starting pitcher from 2000 to 2010 
#compare to the percentage of games 100 years before?
#pitching <- read_csv(here("data", "core", "Pitching.csv"))

glimpse(pitching)
pitching %>%
  mutate(decade = yearID - (yearID %% 10)) %>%
  filter(decade == "2000" | decade == "1900") %>%
  group_by(decade) %>%
  summarise(CG = sum(CG), GS = sum(GS)) %>%
  mutate(CG_pct = CG/GS)

# Since 1970 1) for each year what player from each league has highest stolen base % (w minimum 15 attempts)
#            2) Find and plot league stolen base % for each year for each league that have attempted more than 15


#1
batting %>%
  filter(yearID >= "1970") %>%
  mutate(attempts = (SB + CS)) %>%
  filter(attempts >= 25) %>%
  mutate(sbp = (SB/attempts*100)) %>%
  select(playerID, lgID, yearID, attempts, SB, sbp) %>%
  arrange(desc(yearID), desc(sbp)) %>%
  group_by(yearID, lgID) %>%
  top_n(1)

#2
batting %>%
  filter(yearID >= "1970" & yearID <= "2017") %>%
  mutate(attempts = (SB + CS)) %>%
  filter(attempts >= 25) %>%
  mutate(sbp = (SB/attempts*100)) %>%
  group_by(yearID, lgID) %>%
  summarize(total_sbp = mean(sbp)) %>%
  ggplot(aes(x = yearID, y = total_sbp, group = lgID, col = lgID)) +
  geom_line()
