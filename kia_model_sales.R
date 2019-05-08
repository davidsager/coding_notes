kia_sales <- readRDS("kia_sales.RDS")


fct <- c("Total", "CE", "EA", "SO", "SW", "WE")

dates <- seq.Date(as.Date("2018-11-18"), as.Date("2019-04-22"), "day")
dates <- rep(dates, each = 6)
dates <- tibble(date = dates, 
                Region = rep(c("Total", "CE", "EA", "SO", "SW", "WE"), times = 936/6))

tmp <- dates %>% 
  left_join(kia_sales)

#the above works, but is insufficient bc the way the data is collected...
#shows big spikes in days after breaks, rendering daily sales unattainable


tmp[is.na(tmp)] <- 0

  # mutate(Region = factor(Region, level = fct)) %>%
  # mutate(day = lubridate::day(date),
  #        month = lubridate::month(date)) %>%
 
tmp %>% 
  filter(Region == "Total") %>%
  arrange(date) %>%
  mutate(test = ifelse(Total >= lag(Total), 0, 1)) %>%
  mutate_at(vars(RIO:Total), funs(ifelse(test == 0, . - lag(.), .))) %>%
  select(date, Region, RIO) %>% filter(date > "2018-12-19") %>% data.frame
  ggplot(aes(x = date, y = RIO)) + 
  geom_line()

kia_sales %>% 
  filter(Region == "Total") %>%
  select(-c(OPTH, OPTP, K900, SOLE, NROP, 
            NROE, CDZ, SDN)) %>%
  mutate_at(vars(RIO:NRO), funs(./Total)) %>%
  gather(model, pct, 3:11) %>%
  arrange(model, date) %>%
  ggplot(aes(x = date, y = pct, group = model, col = model)) +
  geom_point(size = .9) + 
  geom_smooth(se = FALSE) + 
  hrbrthemes::scale_y_percent()


#monthly sales
kia_sales %>%
  filter(Region == "Total") %>%
  select(1,20) %>%
  mutate(test = ifelse(Total >= lag(Total), 0, 1)) %>%
  mutate_at(vars(Total), funs(ifelse(test == 0, . - lag(.), .))) %>%
  filter(test == 0) %>%
  filter(date != "2019-01-30") %>%
  ggplot(aes(x=date, y=Total)) +
  geom_bar(stat = "identity")

  
  
  

  
  
  
  
  

