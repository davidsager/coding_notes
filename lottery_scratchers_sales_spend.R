library(readxl)
library(here)
library(tidyverse)
library(janitor)

#load and clean data to what I want to look at

states <- readRDS("/Users/david.sager/Downloads/states.rds")

sales1 <- read_excel(here("Desktop", "R", "Lottery", "data", "sales_fy13_fy18.xlsx"), sheet = 1)
sales2 <- read_excel(here("Desktop", "R", "Lottery", "data", "sales_fy13_fy18.xlsx"), sheet = 2)
sales3 <- read_excel(here("Desktop", "R", "Lottery", "data", "sales_fy13_fy18.xlsx"), sheet = 3)
sales4 <- read_excel(here("Desktop", "R", "Lottery", "data", "sales_fy13_fy18.xlsx"), sheet = 4)
sales5 <- read_excel(here("Desktop", "R", "Lottery", "data", "sales_fy13_fy18.xlsx"), sheet = 5)
sales6 <- read_excel(here("Desktop", "R", "Lottery", "data", "sales_fy13_fy18.xlsx"), sheet = 6)

sales <- rbind(sales1, sales2, sales3, sales4, sales5, sales6)


m1 <- read_excel(here("Desktop", "R", "Lottery", "data", "media_spend_fy13_fy18.xlsx"), sheet = 1)
m2 <- read_excel(here("Desktop", "R", "Lottery", "data", "media_spend_fy13_fy18.xlsx"), sheet = 2)
m3 <- read_excel(here("Desktop", "R", "Lottery", "data", "media_spend_fy13_fy18.xlsx"), sheet = 3)
m4 <- read_excel(here("Desktop", "R", "Lottery", "data", "media_spend_fy13_fy18.xlsx"), sheet = 4)
m5 <- read_excel(here("Desktop", "R", "Lottery", "data", "media_spend_fy13_fy18.xlsx"), sheet = 5)
m6 <- read_excel(here("Desktop", "R", "Lottery", "data", "media_spend_fy13_fy18.xlsx"), sheet = 6)

media <- rbind(m1, m2, m3, m4, m5, m6)

# rm(sales1, sales2, sales3, sales4, sales5, sales6)

media <- media %>%
  clean_names()

media %>%
  names()

scratchers_spend <- media %>%
  select(aggregation, level_3, spend, dma, wk_start_mon) %>%
  filter( level_3 == "Scratchers")

sales <- sales %>% clean_names()

sales %>%
  names

scratchers_sales <- sales %>%
  filter(channel == "Scratchers") %>%
  select(dollars, dma, wk_start_mon)

# join the 2 data sets
scratchers_spend <- scratchers_spend %>%
  filter(wk_start_mon >= "2013-02-11")

scratchers_sales <- scratchers_sales %>%
  filter(wk_start_mon >= "2013-02-03 00:00:00")

#test it on just LA
laSpend <- scratchers_spend %>%
  filter(dma == "Los Angeles") %>%
  group_by(wk_start_mon) %>%
  summarise(spend = sum(spend))

laSales <- scratchers_sales %>%
  filter(dma == "Los Angeles") %>%
  group_by(wk_start_mon) %>%
  filter(!is.na(dollars)) %>%
  summarise(dollars = sum(dollars))

laSpend %>%
  full_join(laSales) %>%
  arrange(wk_start_mon) %>% 
  mutate(spend = coalesce(spend, 0))

#do it for all dmas

spend <- scratchers_spend %>%
  group_by(wk_start_mon) %>%
  summarise(spend = sum(spend))

sales <- scratchers_sales %>%
  group_by(wk_start_mon) %>%
  filter(!is.na(dollars)) %>%
  summarise(dollars = sum(dollars))

spend %>%
  full_join(sales) %>%
  arrange(wk_start_mon) %>% 
  mutate(spend = coalesce(spend, 0)) %>%
  mutate(mark = ifelse(spend > 0, 1, 0)) %>% 
  mutate(year = lubridate::year(wk_start_mon)) %>%
  filter(year == 2017) %>%
  ggplot(aes(x=wk_start_mon, y=spend)) +
  geom_line()


#ingest media spend
lot_media <- read_excel(here("Desktop", "R", "Lottery", "data", "lottery_media_spend_dates.xlsx"))
nms <- c("wk_start_mon" , "tv", "radio", "ooh", "digital")
lot_media <- lot_media %>% clean_names() %>% select(1:5) %>% setNames(nms)
lot_media[is.na(lot_media)] <- 0

#make it so there is a 1 for media spend that week and a 0 if no media spend that week
any_media <- lot_media %>%
  mutate(media = tv+radio+ooh+digital) %>%
  mutate(yes = ifelse(media > 0, 1, 0)) %>%
  select(-2:-6)

sales_any_media <- sales %>%
  left_join(any_media) %>%
  filter(!(is.na(yes)))

sales_any_media %>%
  ggplot(aes(x= wk_start_mon, y=dollars, color = yes)) +
  geom_line() +
  theme(legend.position = "none") +
  scale_color_gradient(low = "black", high = "red")
options(scipen = 999)

install.packages("forecast")
library(forecast)
#get rid of memory
#read rob hydman's book on forecasting
ar_dollars <- auto.arima(sales_any_media$dollars)
autoplot(ar_dollars$residuals)
# manually fit arima 
fit2 = arima(sales_any_media$dollars, order=c(2,1,0))
acf(sales_any_media$dollars)
dol <- ts(sales_any_media$dollars)
acf(diff(dol))
#is there a correlation between media and dollars (sales)?
linearMod <- lm(ar_dollars$residuals ~ yes, data=sales_any_media)
summary(linearMod)

#see if the 2 media data frames line up.
spend %>%
  left_join(any_media) %>%
  filter(!(is.na(yes))) %>%
  ggplot(aes(x=wk_start_mon, y=spend, color = yes)) +
  geom_line() +
  theme(legend.position = "none") +
  scale_color_gradient(low = "black", high = "red")

  

 


