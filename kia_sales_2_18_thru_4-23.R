
?read_delim

#file names are first_real, second, third, and fourth

first <- read_delim("~/Desktop/first_real.txt", delim = " ", col_names = FALSE) %>%
  data.frame

head(first)

nms <- c("RIO", "FRT", "OPT", "OPTH", "OPTP", "CDZ", "STG", "K900",
         "SPG", "SRT", "TLR", "SOL", "SOLE", "NRO", "NROP", "NROE", "SDN", "Total")

col1 <- rep(c("Total", "CE", "EA", "SO", "SW", "WE"), times = 47)

first <- first %>%
  setNames(nms) %>%
  mutate_at(vars(RIO:SDN), funs(str_replace_all(., ",", ""))) %>%
  mutate_at(vars(RIO:SDN), funs(str_replace_all(., "\\(", "-"))) %>%
  mutate_at(vars(RIO:SDN), funs(str_replace_all(., "\\)", ""))) %>%
  mutate_if(is.character, as.numeric)

first <- col1 %>%
  cbind(first)

colnames(first)[1] <- "Region"
head(first)


first_dates <- list.files(here("Desktop", "data")) %>%
  as.data.frame() %>%
  setNames("date") %>%
  mutate(date = str_replace_all(date, "Daily Sales Report_", ""),
         date = str_replace_all(date, ".pdf", "")) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= "2019-02-18") %>%
  pull(date)

first_dates <- rep(first_dates, each = 6)
first_dates <- rev(first_dates)

first <- first %>%
  mutate(date = first_dates)

first %>% slice(c(1, n()))

# check on NA
first[which(is.na(first$Total)),]

###
kia_sales

saveRDS(kia_sales, "kia_sales.RDS")


#the rest of the 3 txt file dataframes got deleted.







