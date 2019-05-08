#Tags: lm (linear models), coef, distinct, inner_join, left_join, labs

#batting. everybody w more than 5k AB. plot in scatter SO against HR

batting %>%
  group_by(playerID) %>%
  summarize(total_ab = sum(AB),
            total_so = sum(SO),
            total_hr = sum(HR)) %>%
  filter(total_ab >= 5000) %>%
  ggplot(aes(x = total_hr, y = total_so)) +
  geom_point() +
  geom_smooth()


################## from 1950+, by decade, look at run differential and win %
my_data <- teams %>%
  filter(yearID >= "1950") %>%
  mutate(rd = (R - RA),
         winpct = (W/(W+L)))

fit <- lm(winpct ~ rd, data=my_data)
summary(fit)

coef(fit)[2]*100

###################
head(batting)
hof <- read_csv(here("data", "core", "HallOfFame.csv"))
hofin <- hof %>%
  filter(inducted == "Y") %>%
  select(test = playerID, inducted)

batting %>%
  left_join(hofin, by = c("playerID" = "test")) %>%
  filter(inducted == "Y") %>%
  distinct(playerID, .keep_all= TRUE) %>%
  arrange(desc(yearID))


thomeji01
bagweje01
rodriiv01

batting %>%
  filter(playerID == "thomeji01" | playerID == "bagweje01" | playerID == "rodriiv01") %>%
  group_by(playerID) %>%
  mutate(total_hr = cumsum(HR)) %>%
  ggplot(aes(x = yearID, y = total_hr, group = playerID, col = playerID)) +
  geom_line()

# more than 500 hr, graph y axis is player name, x axis is career OPS (OB% + Slugging Avg), 
#scatter plot, in order from high to low ops
hr500 <- batting %>%
  group_by(playerID) %>%
  summarise(total_HR = sum(HR)) %>%
  filter(total_HR >= 500)
  
batting %>%
  inner_join(hr500, by = c("playerID")) %>%
  left_join(player_names, by = c("playerID" = "playerID")) %>%
  # filter(debut >= "1973-04-06") %>%
  group_by(name, playerID, debut) %>%
  summarise_at(vars(G:GIDP), sum, na.rm = TRUE) %>%
  mutate(singles = (H - (X2B + X3B + HR)),
         slugging = (singles + (2*X2B) + (3*X3B) + (4*HR))/(AB),
         obp = (H + BB)/(AB + BB + SF),
         ops = obp + slugging) %>%
  select(playerID, slugging, obp, ops, HR, debut) %>%
  mutate(decade = lubridate::year(debut) - floor(lubridate::year(debut)) %% 10) %>%
  arrange(desc(ops)) %>%
  ggplot(aes(x=ops, y=reorder(name, ops), group = factor(decade), color = factor(decade))) + 
  geom_point() +
  labs(x = "On Base + Slugging", y = "Player Name")

#make player id player name. use left join. people
glimpse(people)
player_names <- people %>%
  mutate(name = paste0(nameFirst, " ", nameLast)) %>%
  select(playerID, name, debut)

batting %>%
  left_join(player_names, by = c("playerID" = "playerID"))
  



