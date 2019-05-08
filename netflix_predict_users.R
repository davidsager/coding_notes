"~/Desktop/Netflix.xlsx"
library(readxl)
netflix_data <- read_excel("~/Desktop/Netflix.xlsx")
netflix_data <- netflix_data %>%
  janitor::clean_names()
netflix_data
