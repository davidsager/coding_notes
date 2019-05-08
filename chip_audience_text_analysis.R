# Tags: export to csv, rbind, 

library(dplyr)
library(tidytext)

pacman::p_load(tidyverse, readxl, janitor)
chip1 <- read_excel("/Users/david.sager/Desktop/Chip_Audience_1.xlsx") %>%
  clean_names()
chip2 <- read_excel("/Users/david.sager/Desktop/Chip_Audience_2.xlsx") %>%
  clean_names()
chip3 <- read_excel("/Users/david.sager/Desktop/Chip_Audience_3.xlsx") %>%
  clean_names()
chip4 <- read_excel("/Users/david.sager/Desktop/Chip_Audience_4.xlsx") %>%
  clean_names()


chip <- rbind(chip1, chip2, chip3, chip4) # this failed

a <- names(chip1)
b <- names(chip2)
a == b
names(chip1)[11]
names(chip2)[11]
names(chip3)[11]
names(chip4)[11]

colnames(chip2)[11] <- "published_date_gmt_00_00_london"
colnames(chip4)[11] <- "published_date_gmt_00_00_london"

chip <- rbind(chip1, chip2, chip3, chip4)
chip <- chip %>%
  filter(!is.na(post_id)) %>%
  filter(post_type != "Retweets and Reblogs")

#Looking for most RT content (above 55 rt) and exporting it to desktop
chip %>% 
  count(sound_bite_text) %>% arrange(desc(n)) %>%
  filter(n > 500) %>% 
  write_csv("/Users/david.sager/Desktop/top_rt.csv")

#Now, looking for specific word usage
chip %>%
  unnest_tokens(word, sound_bite_text) %>%
  count(word) %>%
  anti_join(stop_words) %>%
  arrange(desc(n))

install.packages("udpipe")
install.packages("cleanNLP")
library(cleanNLP)
library(udpipe)




