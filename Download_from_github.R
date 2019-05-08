######################################
###   Downloading files from       ###
###           GitHub               ###
###                                ###
######################################
# Tags: Scrape, scraping, nfl, strings - str_sub, geom_tile, t.test, as.Date, github

#1. Find link (and change it to "Raw" on the website)
https://github.com/devstopfix/nfl_results/blob/master/nfl%201978.csv"

# install.packages("readr")
library(readr)
# getwd()
# setwd(https://github.com/devstopfix/nfl_results/blob/master/nfl%201978.csv")


#   ____________________________________________________________________________
#   Scraping scores 1978-2014                                               ####

# This is the first of the series of links we are pulling from
a <- read_csv("https://raw.githubusercontent.com/devstopfix/nfl_results/master/nfl%201978.csv")

# Use either of these as a way to concatinate strings
# paste, paste0
# str_c()

#2 Create the parts of the string that stays constant
url1 <- "https://raw.githubusercontent.com/devstopfix/nfl_results/master/nfl%20"
suff <- ".csv"

#3 Create the equation that will put all of the string components together
url <- paste0(url1, "years", suff)

#4 Create the part of the string that changes (bc we're repetedly pulling from 1978 to 2014) 
years <- seq(1978,2014,1)

nfl_scores <- NULL
for(year in years) {
  url <- paste0(url1, year, suff)
  temp <- read_csv(url)
  nfl_scores <- rbind(nfl_scores, temp)
  temp <- NULL
}

# Example: This is how to select specific parts of a string
103 %% 10
st <- "123456789"
str_sub(st, -1)
str_sub(st, 1, 5)

# Now, play with all the data

library(tidyverse)

full_nfl %>%
  mutate(home_digit = str_sub(home_score, -1),
         visit_digit = str_sub(visitors_score, -1)) %>%
  count(home_digit, visit_digit) %>%
  arrange(desc(n)) %>%
  mutate(pct = n/sum(n)*100) %>%
  mutate(pct2 = paste0(round(pct, digits = 2), "%")) %>%
  ggplot(aes(x=home_digit, y=reorder(visit_digit, desc(visit_digit)))) +
  geom_tile(aes(fill = pct)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 1.5) +
  geom_text(aes(label=pct2), size=2.5) +
  labs(x= "Home Score", y = "Visit Score") +
  scale_x_discrete(position = "top") +
  theme(legend.position="none")
ggsave(here::here("nfl_score_prob.png"), dpi = 300, bg = "transparent")

full_nfl %>%
  mutate(home_win = ifelse(home_score > visitors_score, "W", "L")) %>%
  count(home_win) %>%
  mutate(home_win_pct = n/sum(n))

#Test ratio of W/L & score from home/away and test significance
successes <- c(5865, 4233)
games <- c(4233+5865, 4233+5865) 
prop.test(successes, games)
t.test(full_nfl$home_score, full_nfl$visitors_score)

# Scrape for years 2015 - 2018
library(rvest)

prefix <- "https://www.pro-football-reference.com/years/"
suffix <- "/games.htm"

url2 %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="games"]') %>%
  html_table() %>%
  flatten_df()

new_years <- seq(2015,2018,1)

recent_nfl_scores <- NULL
for(year in new_years) {
  new_url <- paste0(prefix, year, suffix)
  temp <- new_url %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="games"]') %>%
    html_table() %>%
    flatten_df() %>%
    janitor::clean_names() %>%
    mutate(season = year)
  recent_nfl_scores <- rbind(recent_nfl_scores, temp)
  temp <- NULL
}
recent_nfl_scores <- recent_nfl_scores %>%
  filter(week != "Week") %>%
  mutate(home_team = ifelse(x == "@", loser_tie, winner_tie),
         visiting_team = ifelse(x == "@", winner_tie, loser_tie),
         home_score = ifelse(x == "@", pts_l, pts_w),
         visitors_score = ifelse(x == "@", pts_w, pts_l),
         kickoff = paste0(date, "-", season),
         kickoff = as.Date(kickoff, format ="%B %d-%Y")) %>%
    select(season, week, kickoff, home_team,  home_score, visitors_score, visiting_team) %>%
    mutate(week = as.numeric(week),
           home_score = as.numeric(home_score),
           visitors_score = as.numeric(visitors_score))

recent_nfl_scores <- recent_nfl_scores[complete.cases(recent_nfl_scores),]
full_nfl <- rbind(nfl_scores, recent_nfl_scores)
full_nfl

#HOW TO DO as.Date (data formats here - https://www.statmethods.net/input/dates.html)
tmp_date <- "September 10, 2015"
as.Date(tmp_date, format = "%B %d, %Y")
#paste date w season. mutate kickoff = as.date


 

