library(dplyr)
library(readxl)

cos <- read_excel(here("Desktop", "test.xlsx"), sheet = 1, skip = 3) %>%
  janitor::clean_names()
cos <- cos %>%
  select(-c(x2,x3,x5,x7,x9))

table(is.na(cos$location))

cos <- cos %>% 
  filter(!is.na(location)) 

head(cos, 10)
id <- rep(seq(1, 1250, 1), each = 3)

cos <- cos %>% 
  cbind(id) %>%
  as_tibble()

cos <- cos %>%
  group_by(id) %>%
  tidyr::fill(x6, .direction = c("up"))

cos <- cos %>% 
  group_by(id) %>%
  tidyr::fill(x8, .direction = c("up"))

cos <- cos %>%
  select(-c(regional_director, district_manager))

cos <- cos %>%
  select(-c(special_services, county))

cos <- cos %>% 
  group_by(id) %>%
  tidyr::fill(store_phone_number, .direction = c("down"))

cos <- cos %>%
  select(-store_no)
head(cos)

cos %>%
  group_by(id) %>%
  spread(id, location)

cos

stores <- matrix(cos$location, nrow = 1250, ncol =3, byrow = TRUE)
colnames(stores) <- c("loc", "address", "city")
zip <- cos$x8[seq(1,3750,3)]
zip <- data.frame(zip = zip)
phone <- cos %>% distinct(store_phone_number)
state <- cos %>% distinct(x6)
stateclean_data <- data.frame(stores)
clean_data <- cbind(clean_data, zip)
clean_data <- cbind(clean_data, phone[,1])
clean_data <- cbind(clean_data, state)

write_csv(clean_data, "clean_data2.csv")
clean_data


############################################################

k <- read_excel(here("Desktop", "Store Locater_2.4.19.xlsx"), sheet = 3) %>%
  janitor::clean_names()

k <- k %>%
  select(-c(sunday))

k <- k %>%
  mutate(first = str_sub(postcode, 1, 5)) %>%
  mutate(second = str_sub(postcode, -4)) %>%
  mutate(full = paste0(first, "-", second))

write_csv(k, "postcodes.csv")


