library(DBI)
library(dplyr)
library(lubridate)

pacman::p_load(tidyverse, RPostgreSQL)

## connect to sql server 
con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), 
                      user=Sys.getenv("POSTGRES_USER"), password=Sys.getenv("POSTGRES_PW"),
                      dbname='analyticsdb', host='sql01.dngla.com')

#Now create a variable that is all of the tables
tables <- dbListTables(con)
#call this variable
tables

lottery <- tbl(con, tables[2])

sales <- lottery %>%
  collect()

# disconnecting from the server
dbDisconnect(con)

sales %>%
  filter(game == "Scratchers-Consumer-Sales") %>%
  mutate(qtr = lubridate::quarter(date),
         year = lubridate::year(date)) %>%
  group_by(year, qtr) %>%
  summarise(tot = sum(actual),
            goal_tot = sum(goal)) %>%
  mutate(pct_change = round((tot - lag(tot))/lag(tot),2)) %>%
  mutate(fy_yr = ifelse(qtr > 2, year + 1, year),
         fy_qrtr = ifelse(qtr <3, qtr+2, qtr-2),
         fy_qtr = paste0(fy_yr,"-", fy_qrtr)) %>%
  filter(fy_qtr != "2019-3") %>%
  ggplot(aes(x=fy_qtr, y=tot, group = 1, col = "tot")) +
  geom_line(size = 1) +
  geom_line(aes(x= fy_qtr, y=goal_tot, col = "goal_tot"), size = 1) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 13)) +
  theme(axis.text.y = element_text(size = 13)) +
  theme(axis.title.x = element_text(size=14)) +
  theme(axis.title.y = element_text(size=14)) +
  theme(plot.title = element_text(size = 18, face = "bold")) +
  theme(legend.position = c(0.8, 0.2)) +
  theme(legend.title = element_blank()) +
  labs(title = "Quarterly Scratchers Sales and Goals", x = "FY Quarter", y= "Total Sales (MM)", caption = "Source: CA Lottery")
  
# Summer month hypothesis. Look at the 3 summer months side by side to see if any months are particularily bad
#this data had 4 inclomplete weeks, so we took those out in scratchers_filtered below
sales %>%
  filter(game == "Scratchers-Consumer-Sales") %>%
  # mutate(month = lubridate::floor_date(date, unit = "month")) %>%
  # mutate(month = lubridate::month(date, label = TRUE)) %>%
  # mutate(year = lubridate::year(date)) %>%
  # mutate(qtr = lubridate::quarter(date),
  #        year = lubridate::year(date)) %>%
  # filter(month == "Jul" | month == "Aug" | month == "Sep") %>%
  # mutate(fy_yr = ifelse(qtr > 2, year + 1, year),
  #        fy_qrtr = ifelse(qtr <3, qtr+2, qtr-2),
  #        fy_qtr = paste0(fy_qrtr,"-", fy_yr)) %>%
  # mutate(month_year = paste0(month, "_", fy_yr)) %>%
  ggplot(aes(x=date, y=actual)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 13))

scratchers_filtered <- sales %>%
  filter(game == "Scratchers-Consumer-Sales") %>% 
  group_by(fy) %>%
  mutate(week_ct = row_number(),
         day = lubridate::day(date)) %>%
  mutate(drop = ifelse(week_ct == 1 & day < 7, 1, 0)) %>%
  filter(drop != 1)

#use this geom_rect if only need to rect 1 date
scratchers_filtered %>%
  mutate(qtr = lubridate::quarter(date)) %>%
  ggplot(aes(x=date, y=actual)) +
  # geom_rect(
  #   fill="gray",alpha=0.5, 
  #   mapping=aes_string(x="date", y="actual"), 
  #   xmin=as.numeric(as.Date(c("2014-07-1"))),
  #   xmax=as.numeric(as.Date(c("2014-09-30"))),
  #   ymin=-Inf,
  #   ymax=Inf
  # ) +
  geom_line()
  

#use this geom_rect if need to rect multiple dates

rect_data <- scratchers_filtered %>%
  mutate(qtr = lubridate::quarter(date))

ggplot(rect_data, aes(x=date, y=actual)) +
geom_rect(data = subset(rect_data, qtr %in% c(1,3)),
          aes(ymin = -Inf, ymax = Inf, xmin = date-weeks(1), xmax = date),
          fill = "blue", alpha = .3) +
  geom_line() +
  theme_minimal()

ggplot(rect_data, aes(x = date, y = actual)) + 
  geom_line() + 
  coord_cartesian(ylim = c(60, 115)) +
  # geom_ribbon(aes(ymin = 60, ymax = actual), 
  #             fill = "darkblue", alpha = .3) +
  geom_rect(data = subset(rect_data, qtr %in% c(3)),
            aes(ymin = actual, ymax = Inf, xmin = date-weeks(1), xmax = date),
            fill = "red", alpha = .3) +
  geom_rect(data = subset(rect_data, qtr %in% c(2)),
            aes(ymin = actual, ymax = Inf, xmin = date-weeks(1), xmax = date),
            fill = "green", alpha = .3) +
  geom_line() +
  geom_smooth(col = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 13)) +
  theme(axis.text.y = element_text(size = 13)) +
  theme(axis.title.x = element_text(size=14)) +
  theme(axis.title.y = element_text(size=14)) +
  theme(plot.title = element_text(size = 18, face = "bold")) +
  theme(legend.position = c(0.8, 0.2)) +
  theme(legend.title = element_blank()) +
  labs(title = "Weekly Scratchers Sales", x = "Year", y= "Weekly Sales (MM)", caption = "Source: CA Lottery")

d <- as.Date("2019-02-28")

d - days(4)

#row finder
rect_data[rect_data$qtr %in% c(1,3), ]
#row finder
rect_data %>% 
  filter(qtr %in% c(1,3))

###################################
#insight #3
sales %>%
  filter(game == "Scratchers-Consumer-Sales") %>%
  mutate(qtr = lubridate::quarter(date),
         year = lubridate::year(date)) %>%
  mutate(pct_goal_decimal = pct_goal/100) %>%
  # mutate(avg = mean(pct_goal_decimal)) %>%
  # data.frame()
  # group_by(year, qtr) %>%
  # summarise(tot = sum(actual),
  #           goal_tot = sum(goal)) %>%
  # mutate(pct_change = round((tot - lag(tot))/lag(tot),2)) %>%
  # mutate(fy_yr = ifelse(qtr > 2, year + 1, year),
  #        fy_qrtr = ifelse(qtr <3, qtr+2, qtr-2),
  #        fy_qtr = paste0(fy_yr,"-", fy_qrtr)) %>%
  # group_by(fy_qtr) %>%
  # summarise(qtr_pct_goal = mean(pct_goal_calc)) %>%
  mutate(diff = pct_goal_calc - 1,
         abovezero = ifelse(diff > 0, 1, 0)) %>%
  ggplot(aes(x = date, y = diff, col = factor(abovezero))) +
  geom_segment(aes(x=date, xend=date-0, y=0, yend=diff)) +
  # geom_bar(stat = "identity", width = 1) + 
  # scale_fill_gradient2(low='red', mid='white', high='blue', space='Lab',
  #                      midpoint = 0)
  scale_color_manual(values = c("red", "blue")) + 
  geom_hline(yintercept = 0 ) +
  labs(title = "Percent Difference from Goals Each Week", x = "Year", y= "Percent Difference from Goal", caption = "Source: CA Lottery") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 13)) +
  theme(axis.text.y = element_text(size = 13)) +
  theme(axis.title.x = element_text(size=14)) +
  theme(axis.title.y = element_text(size=14)) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 18, face = "bold")) +
  geom_smooth()
  
  
  
  ggplot(aes(x=fy_qtr, y = qtr_pct_goal)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 13)) +
  geom_hline(yintercept = 1) +
  geom_ribbon(aes(ymin = .2, ymax = .4), fill = "red")
  
  ###
  # use hrbrthemes::scale_y_comma for graph on slide 3



