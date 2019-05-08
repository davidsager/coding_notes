library(dplyr)
library(ggplot2)
library(tidyverse)
library(here)
library(janitor)


cos1 <- read_csv(here("R", "COS", "google_analytics", "data", "cos_users_dma.csv"))
landing1 <- read_csv(here("R", "COS", "google_analytics", "data", "landing_pages.csv"))
source1 <- read_csv(here("R", "COS", "google_analytics", "data", "ga_source.csv"))
device1 <- read_csv(here("R", "COS", "google_analytics", "data", "device_type.csv"))

head(cos)

#find number of users each day
cos %>%
  mutate(Date = as.Date(cos$Date, "%m/%d/%y")) %>%
  mutate(socal = LA + SD) %>%
  select(-3:-4) %>%
  gather(city, users, 3:7) %>%
  ggplot(aes(x=Date, y = All)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(as.Date("2019-02-01")), linetype=4) +
  geom_text(aes(x=as.Date("2019-01-25"), y=6000), label = "N o   M e d  i a", angle = 90, col = "red") +
  geom_text(aes(x=as.Date("2019-01-25"), y=1200), label = "2-1-19", angle = 90) +
  geom_text(aes(x=as.Date("2019-02-09"), y=6000), label = "W i t h  M e d i a", angle = -90, col = "red") +
  labs(title = "Daily Site Users",
       caption = "Source: Google Analytics",
       y="Users") +
  theme_minimal()
ggsave(here('Desktop', 'R', 'COS', 'ga_graphs', 'users_by_date.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)


tdata <- cos1 %>%
  mutate(Date = as.Date(cos$Date, "%m/%d/%y")) %>%
  mutate(socal = LA + SD) %>%
  select(-3:-4) %>%
  gather(city, users, 3:7) %>%
  mutate(media = ifelse(Date >= "2019-02-01", 1, 0))

t.test(tdata[tdata$media == 1,]$All, tdata[tdata$media == 0,]$All)
# mean before, mean after. significance

#do prop test for each of the cities. total users pre media
cos1 %>%
  mutate(Date = as.Date(cos1$Date, "%m/%d/%y")) %>%
  mutate(socal = LA + SD) %>%
  select(-3:-4) %>%
  # gather(city, users, 3:7) %>%
  mutate(media = ifelse(Date >= "2019-02-01", 1, 0)) %>%
  mutate(dmas = Chi + NY + Dallas + Orlando + socal) %>%
  group_by(media) %>%
  summarise(all = sum(All), dma = sum(dmas))

prop.test(c(46187, 40856), c(400842, 294455))
#increasing share of traffic. better job of getting ppl to site and true effictiveness and those cities
#11.5% of old

#show changes in landing pages to infusion. show top landing page b4 and after.
#higher % of iphones to site for example. more mobile is good.

# find number of users landing on each infusions for media and no media
landing %>%
  clean_names() %>%
  mutate(Media = ifelse(date_range == "Feb 1, 2019 - Apr 24, 2019", "Media", "No Media")) %>%
  select(-6:-12) %>%
  slice(1:6) %>%
  mutate(Media = 67332 + 32375,
         No_Media = 818) %>%
  slice(1) %>%
  gather(media, users, 6:7) %>%
  ggplot(aes(x= date_range, y = users, col = media)) +
  geom_bar(stat="identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Number of User Landing on Infusions",
       caption = "Source: Google Analytics",
       y= "Users", x = "") +
  theme(legend.title = element_blank())
ggsave(here('Desktop', 'R', 'COS', "google_analytics", 'ga_graphs', 'infusions_landing.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

#find % change of top 5 landing pages. (use GA's pie chart/graphs)
    
#find pct of users coming from each source
source <- source %>% clean_names()
source %>%
  mutate(Media = ifelse(date_range == "Feb 1, 2019 - Apr 24, 2019", "Media", "No Media")) %>%
  filter(source_medium != "yahoo / organic",
         source_medium != "imasdk.googleapis.com / referral",
         source_medium != "tpc.googlesyndication.com / referral") %>%
  group_by(Media) %>%
  mutate(pct = users/(sum(users))) %>%
  ggplot(aes(x=reorder(source_medium, -users), y=pct, group = Media, col = Media)) +
  geom_bar(stat = "identity", position = "dodge") +
  # ylim(0, 150000) +
  hrbrthemes::scale_y_percent() +
  labs(title = "Top Traffic Sources With vs. Without Media",
       caption = "Source: Google Analytics",
       x = "Traffic Source",
       y= "Percent of Users") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.title = element_blank())
ggsave(here('R', 'COS', "google_analytics", 'ga_graphs', 'traffic_sources.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)


device <- device %>% clean_names()
device %>%
  mutate(Media = ifelse(date_range == "Feb 1, 2019 - Apr 24, 2019", "Media", "No Media")) %>%
  slice(1:4) %>%
  mutate(pct = users/184367) %>%
  ggplot(aes(x= mobile_device_info, y=pct, group = Media, col = Media)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Percent of Users on Each Mobile Device",
       caption = "Source: Google Analytics",
       x = "Mobile Device", y = "Percent of Users") +
    theme(legend.title = element_blank()) +
    hrbrthemes::scale_y_percent()
ggsave(here('Desktop', 'R', 'COS', "google_analytics", 'ga_graphs', 'mobile_device.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)
  











