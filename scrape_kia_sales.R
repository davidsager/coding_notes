#on the website goodcarbadcar.com, they have sales data for every model of car. Scrape this#

pacman::p_load(tidyverse, readxl, janitor, here, glue, rvest)

#first, test if you are allowed to scrape#
install.packages("robotstxt")
library(robotstxt)
paths_allowed("http://www.goodcarbadcar.net/2013/05/kia-cadenza-sales-figures-usa-canada/")

urls <- c("http://www.goodcarbadcar.net/2013/05/kia-",
             "http://www.goodcarbadcar.net/2011/01/kia-",
             "http://www.goodcarbadcar.net/2014/03/kia-",
             "http://www.goodcarbadcar.net/2016/10/kia-",
             "http://www.goodcarbadcar.net/2011/01/kia-",
             "http://www.goodcarbadcar.net/2011/01/kia-",
             "http://www.goodcarbadcar.net/2011/01/kia-",
             "http://www.goodcarbadcar.net/2011/01/kia-",
             "http://www.goodcarbadcar.net/2011/01/kia-",
             "http://www.goodcarbadcar.net/2011/01/kia-",
             "http://www.goodcarbadcar.net/2016/10/kia-")
  
models <- c("cadenza", "forte", "k900", "niro", "optima", "rio", "sedona", "sorento", "soul", "sportage", "stinger")

suffix <- "-sales-figures/"

model = NULL
for (i in 1:length(urls)) {
 url <- paste0(urls[i], models[i], suffix)
   temp <- url %>% 
    read_html %>%
    html_nodes(xpath = '//*[@id="table_1"]') %>%
    html_table() %>%
    data.frame() %>%
    gather(month, sales, 2:13) %>%
    unite("date", c("month", "X.")) %>%
    mutate(models = models[i])
  model <- rbind(model, temp)
  temp <- NULL
}

model %>%
  # spread(models, sales) %>%
  mutate(date = str_replace_all(date, "_", "-01-")) %>%
  mutate(date = as.Date(date, format = "%b-%d-%Y")) %>%
  arrange((date)) %>%
  mutate(sales = str_replace_all(sales, ",", "")) %>%
  mutate(sales = as.numeric(sales)) %>%
  ggplot(aes(x=date, y=sales, group = models, col = models)) +
  geom_line()





