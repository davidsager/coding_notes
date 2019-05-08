url <- "https://en.wikipedia.org/wiki/Hybrid_electric_vehicles_in_the_United_States"
library(rvest)
library(tidyverse)

tbl_text <- url %>%
  read_html() %>%
  html_nodes('td , th') %>%
  html_text() 

tbl_list <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table(fill = TRUE)
str(tbl_list)

tbl_list[[1]]
tbl_text[1:10]

test <- matrix(data = tbl_text[2:1301], nrow = 72, ncol = 18, byrow = TRUE,
       dimnames = NULL) %>%
  data.frame()


