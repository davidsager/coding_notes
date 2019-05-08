## building the brand tracker data ## 
pacman::p_load(tidyverse, here, glue, janitor)
library(readxl)

jib1 <- read_excel(here("R", "jib", "data", "jitb_brand_tracker_clean_data", "jib_au_brand_tracker.xlsx"), sheet = 1)
jib2 <- read_excel(here("R", "jib", "data", "jitb_brand_tracker_clean_data", "jib_au_brand_tracker.xlsx"), sheet = 2)
jib3 <- read_excel(here("R", "jib", "data", "jitb_brand_tracker_clean_data", "jib_au_brand_tracker.xlsx"), sheet = 3)
jib7 <- read_excel(here("R", "jib", "data", "jitb_brand_tracker_clean_data", "jib_au_brand_tracker.xlsx"), sheet = 7)
jib10 <- read_excel(here("R", "jib", "data", "jitb_brand_tracker_clean_data", "jib_au_brand_tracker.xlsx"), sheet = 10)
jib_char <- read_excel(here("R", "jib", "data", "jitb_brand_tracker_clean_data", "jib_au_brand_tracker.xlsx"), sheet = 15)
jib_compet1 <- read_excel(here("R", "jib", "brand_tracker", "data", "jitb_brand_tracker_clean_data", "jitb_compet_brand_tracker_data.xlsx"), sheet = 1)
jib_compet6 <- read_excel(here("R", "jib", "data", "jitb_brand_tracker_clean_data", "jitb_compet_brand_tracker_data.xlsx"), sheet = 6)

saveRDS(jib_char, "jib_char.RDS")

#JIB1 
#Just pull up row_options and plut in which one you want into formula below
fct <- c("AMJ17", "JAS17", "OND17", "JFM18", "AMJ18", "JAS18", "OND18")

pal <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a")

jib1 <- jib1 %>% clean_names()
jib2 <- jib2 %>% clean_names()
jib3 <- jib3 %>% clean_names()
jib7 <- jib7 %>% clean_names()
jib10 <- jib10 %>% clean_names()
jib_char <- jib_char %>% clean_names()

names(jib1)
row_options <- jib1 %>%
  group_by(awareness_and_usage_measures) %>% 
  select(awareness_and_usage_measures) %>% 
  distinct(awareness_and_usage_measures) %>%
  data.frame()

row_options

jib1 %>%
  filter(awareness_and_usage_measures == "Past Three Month Purchase Level") %>%
  gather(chain, pct, 2:11) %>%
  group_by(chain) %>%
  mutate(quarter = factor(quarter, levels = fct)) %>%
  mutate(pct = as.numeric(pct)) %>%
  # filter(str_detect(chain, "jack")) %>%
  ggplot(aes(x=quarter, y=pct, group = chain, col = chain, label= pct)) +
  geom_line(size=1) +
  hrbrthemes::scale_y_percent(limits=c(0, 1)) +
  scale_color_manual(values = pal) +
  theme_minimal() +
  labs(x= "Quarter", y = "Awareness and Usage P3M",
       title = "Past 3 Month Awareness and Usage of Each Brand")
ggsave(here("Desktop", "R", "jib", "jib_brand_tracker_graphs", "p3m_au.png"), dpi = 300, bg = "transparent",
       width = 7.07, height = 7.58)

#JIB2
names(jib2)
fct <- c("AMJ17", "JAS17", "OND17", "JFM18", "AMJ18", "JAS18", "OND18")

#change column name. not setNames, rather use colnames
colnames(jib2)[colnames(jib2) == "amj17"] <- "quarter"
jib2 %>% 
  mutate(brand_awareness_unaided = as.numeric(brand_awareness_unaided)) %>%
  mutate(brand_awareness_total = as.numeric(brand_awareness_total)) %>%
  mutate(ad_awareness_unaided = as.numeric(ad_awareness_unaided)) %>%
  mutate(ad_awareness_total = as.numeric(ad_awareness_total)) %>%
  mutate(purchase_levels_ever = as.numeric(purchase_levels_ever)) %>%
  mutate(purchase_levels_past_3_mos = as.numeric(purchase_levels_past_3_mos)) %>%
  mutate(purchase_levels_past_mo = as.numeric(purchase_levels_past_mo)) %>%
  mutate(past_mo_frequency = as.numeric(past_mo_frequency)) %>%
  mutate(past_mo_share = as.numeric(past_mo_share)) %>%
  mutate(overall_rating = as.numeric(overall_rating)) %>%
  mutate(quarter = factor(quarter, levels = fct)) %>%
  select(1, 2, 12) %>%
  filter(brand_awareness_unaided >= .18) %>%
  group_by(chain) %>%
  ggplot(aes(x=quarter, y=brand_awareness_unaided, group = chain, col = chain)) +
  geom_line(size = 1) +
  hrbrthemes::scale_y_percent(limits=c(0, .75)) +
  scale_color_manual(values = pal)

#share of past month occasions - late night snacks
fct <- c("AMJ17", "JAS17", "OND17", "JFM18", "AMJ18", "JAS18", "OND18")

jib3 %>%
  mutate_at(vars(c(breakfast, late_night_snacks, delivery)), list(~as.numeric(.))) %>%
  select(1, late_night_snacks, quarter) %>%
  filter(late_night_snacks >=.04) %>%
  mutate(quarter = factor(quarter, levels = fct)) %>%
  ggplot(aes(x=quarter, y=late_night_snacks, group = chain, col = chain)) +
  geom_line(size=1.5) +
  hrbrthemes::scale_y_percent(limits=c(0, .2)) +
  scale_color_manual(values = pal)

#share of past month occasions - all
fct <- c("AMJ17", "JAS17", "OND17", "JFM18", "AMJ18", "JAS18", "OND18")

jib3 %>%
  mutate_at(vars(c(breakfast, late_night_snacks, delivery)), list(~as.numeric(.))) %>%
  select(1, all, quarter) %>%
  filter(all >=.03) %>%
  mutate(quarter = factor(quarter, levels = fct)) %>%
  ggplot(aes(x=quarter, y=all, group = chain, col = chain)) +
  geom_line(size=1.5) +
  hrbrthemes::scale_y_percent(limits=c(0, .2))


#pct_brand_aware: taste/flavor
pal <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a")
fct <- c("AMJ17", "JAS17", "OND17", "JFM18", "AMJ18", "JAS18", "OND18")
jib7 %>%
  filter(attribute == "Taste or flavor of the food") %>%
  gather(chain, pct, 2:11) %>%
  mutate(quarter = factor(quarter, levels = fct)) %>%
  ggplot(aes(x=quarter, y=pct, group = chain, col = chain)) +
  geom_line(size = 1) +
  hrbrthemes::scale_y_percent(limits=c(.5, 1)) +
  scale_color_manual(values = pal)



# DO CORRELATION MATRIX 
install.packages("corrplot")
library(corrplot)




#use jib8 to see what attributes we've grown the most over the last 7 qtrs
#whhere we've decreased the most also
#do same w jib7

fct <- c("AMJ17", "JAS17", "OND17", "JFM18", "AMJ18", "JAS18", "OND18")

base <- jib8 %>%
  filter(quarter =="AMJ17" | quarter == "OND18") %>%
  filter(str_detect(attribute, "Base")) %>%
  select(base = jack_in_the_box, quarter)

attribute <- jib8 %>%
  filter(quarter =="AMJ17" | quarter == "OND18") %>%
  filter(attribute != "Base:") %>%
  mutate(quarter = factor(quarter, levels = fct)) %>%
  select(attribute, 2, quarter) %>%
  spread(quarter, jack_in_the_box) %>%
  mutate(base_amj17 = 882, base_ond18 = 831) %>%
  mutate(success1 = AMJ17*base_amj17, success2 = OND18*base_ond18) %>%
  na.omit()
  

#1. build a loop that finds the difference between the 1st quarter and last quarter and sees if there is a sig difference
x <- NULL
for (i in 1:nrow(attribute)) {
  y <- prop.test(c(attribute$success1[i], attribute$success2[i]), c(882, 831))$p.value
  x <- rbind(x, y)
}
x <- tibble("pval" = x)
attribute %>% bind_cols(x) %>%
  filter(pval < .10)
#these 3 attributes have significant changes. Let's map them out below.
# TEST - taste/flavor
nms <- c("attribute", "JIB", "McDonalds", "Burger_King", "Wendys", "Carls_Jr", "Taco_Bell", "Sonic", "In_N_Out", "Whataburger", "Chick_fil_A", "quarter")
jib8 %>%
  setNames(nms) %>%
  filter(attribute != "Base:") %>%
  filter(attribute != "Base") %>%
  mutate(quarter = factor(quarter, levels = fct)) %>%
  # select(1,2, 12) %>%
  gather(chain, pct, 2:11) %>%
  filter(attribute == "Taste or flavor of the food") %>%
  ggplot(aes(x=quarter, y=pct, group = chain, color = chain)) +
  geom_line(size=1.2) +
  scale_color_manual(values = pal) +
  theme_minimal() +
  hrbrthemes::scale_y_percent(limits=c(.7, 1)) +
  labs(x= "Quarter", y = "Taste and Flavor Rating of Very Good or Excellent",
       title = "% of Past 3 Mo. Users Giving Rating of \nExcellent or Very Good to Taste and Flavor")
ggsave(here("Desktop", "R", "jib", "jib_brand_tracker_graphs", "compet_taste_flavor_p3m.png"), dpi = 300, bg = "transparent",
       width = 7.07, height = 7.58)

# 1 - Affordability of prices
nms <- c("attribute", "JIB", "McDonalds", "Burger_King", "Wendys", "Carls_Jr", "Taco_Bell", "Sonic", "In_N_Out", "Whataburger", "Chick_fil_A", "quarter")
jib8 %>%
  setNames(nms) %>%
  filter(attribute != "Base:") %>%
  filter(attribute != "Base") %>%
  mutate(quarter = factor(quarter, levels = fct)) %>%
  # select(1,2, 12) %>%
  gather(chain, pct, 2:11) %>%
  filter(attribute == "Affordability of the prices") %>%
  ggplot(aes(x=quarter, y=pct, group = chain, color = chain)) +
  geom_line(size=1.2) +
  scale_color_manual(values = pal) +
  theme_minimal() +
  hrbrthemes::scale_y_percent(limits=c(.65, .95)) +
  labs(x= "Quarter", y = "Taste and Flavor Rating of Very Good or Excellent",
       title = "% of Past 3 Mo. Users Giving Rating of \nExcellent or Very Good to Affordability of the Prices")
ggsave(here("Desktop", "R", "jib", "jib_brand_tracker_graphs", "compet_affordability_prices_p3m.png"), dpi = 300, bg = "transparent",
       width = 7.07, height = 7.58)

# 2 - Freshness of food
nms <- c("attribute", "JIB", "McDonalds", "Burger_King", "Wendys", "Carls_Jr", "Taco_Bell", "Sonic", "In_N_Out", "Whataburger", "Chick_fil_A", "quarter")
jib8 %>%
  setNames(nms) %>%
  filter(attribute != "Base:") %>%
  filter(attribute != "Base") %>%
  mutate(quarter = factor(quarter, levels = fct)) %>%
  # select(1,2, 12) %>%
  gather(chain, pct, 2:11) %>%
  filter(attribute == "Freshness of food") %>%
  ggplot(aes(x=quarter, y=pct, group = chain, color = chain)) +
  geom_line(size=1.4) +
  scale_color_manual(values = pal) +
  theme_minimal() +
  hrbrthemes::scale_y_percent(limits=c(.6, 1)) +
  labs(x= "Quarter", y = "Freshness of Food Rating of Very Good or Excellent",
       title = "% of Past 3 Mo. Users Giving Rating of \nExcellent or Very Good to Freshness of Food")
ggsave(here("Desktop", "R", "jib", "jib_brand_tracker_graphs", "compet_freshness_p3m.png"), dpi = 300, bg = "transparent",
       width = 7.07, height = 7.58)

# 3 - Is a great place to go for lunch
nms <- c("attribute", "JIB", "McDonalds", "Burger_King", "Wendys", "Carls_Jr", "Taco_Bell", "Sonic", "In_N_Out", "Whataburger", "Chick_fil_A", "quarter")
jib8 %>%
  setNames(nms) %>%
  filter(attribute != "Base:") %>%
  filter(attribute != "Base") %>%
  mutate(quarter = factor(quarter, levels = fct)) %>%
  # select(1,2, 12) %>%
  gather(chain, pct, 2:11) %>%
  filter(attribute == "Is a great place to go for lunch") %>%
  ggplot(aes(x=quarter, y=pct, group = chain, color = chain)) +
  geom_line(size=1.4) +
  scale_color_manual(values = pal) +
  theme_minimal() +
  hrbrthemes::scale_y_percent(limits=c(.7, 1)) +
  labs(x= "Quarter", y = "Great Place to go for Lunch Rating of Very Good or Excellent",
       title = "% of Past 3 Mo. Users Giving Rating of \nExcellent or Very Good to Great Place to go for Lunch")
ggsave(here("Desktop", "R", "jib", "jib_brand_tracker_graphs", "compet_great_for_lunch_p3m.png"), dpi = 300, bg = "transparent",
       width = 7.07, height = 7.58)

###### do same exercise with jib10
jib10 %>%
  filter(quarter =="AMJ17" | quarter == "OND18") %>%
  filter(attribute != "Sample Size [n]") %>%
  mutate(quarter = factor(quarter, levels = fct)) %>%
  select(attribute, 2, quarter) %>%
  spread(quarter, jack_in_the_box) %>%
  na.omit() %>%
  mutate(dif = OND18 - AMJ17) %>%
  arrange((dif)) %>% data.frame()

#Biggest increase: Is a place I like to go to eat inside the restaurant
#Biggest decreases: Freshness of food, Serves food I crave, Variety of menu items to choose from, Affordability of the prices
#1
nms <- c("attribute", "JIB", "McDonalds", "Burger_King", "Wendys", "Carls_Jr", "Taco_Bell", "Sonic", "In_N_Out", "Whataburger", "Chick_fil_A", "quarter")
jib10 %>%
  setNames(nms) %>%
  filter(attribute != "Sample Size [n]") %>%
  mutate(quarter = factor(quarter, levels = fct)) %>%
  # select(1,2, 12) %>%
  gather(chain, pct, 2:11) %>%
  filter(attribute == "Is a place I like to go to eat inside the restaurant") %>%
  ggplot(aes(x=quarter, y=pct, group = chain, color = chain)) +
  geom_line(size=1.4) +
  scale_color_manual(values = pal) +
  theme_minimal() +
  # hrbrthemes::scale_y_percent(limits=c(.7, 1)) +
  labs(x= "Quarter", y = "Index",
       title = "Index Based on % of Past 3 Mo. Users \nGiving Rating of Excellent or Very Good on \n'Is a place I like to go to eat inside the restaurant'")
ggsave(here("Desktop", "R", "jib", "jib_brand_tracker_graphs", "compet_index_eat_inside_restaurant_p3m.png"), dpi = 300, bg = "transparent",
       width = 7.07, height = 7.58)

#2
nms <- c("attribute", "JIB", "McDonalds", "Burger_King", "Wendys", "Carls_Jr", "Taco_Bell", "Sonic", "In_N_Out", "Whataburger", "Chick_fil_A", "quarter")
jib10 %>%
  setNames(nms) %>%
  filter(attribute != "Sample Size [n]") %>%
  mutate(quarter = factor(quarter, levels = fct)) %>%
  # select(1,2, 12) %>%
  gather(chain, pct, 2:11) %>%
  filter(attribute == "Freshness of food") %>%
  ggplot(aes(x=quarter, y=pct, group = chain, color = chain)) +
  geom_line(size=1.4) +
  scale_color_manual(values = pal) +
  theme_minimal() +
  # hrbrthemes::scale_y_percent(limits=c(.7, 1)) +
  labs(x= "Quarter", y = "Index",
       title = "Index Based on % of Past 3 Mo. Users \nGiving Rating of Excellent or Very Good on \n'Freshness of food'")
ggsave(here("Desktop", "R", "jib", "jib_brand_tracker_graphs", "compet_index_freshness_of_food_p3m.png"), dpi = 300, bg = "transparent",
       width = 7.07, height = 7.58)

#3
nms <- c("attribute", "JIB", "McDonalds", "Burger_King", "Wendys", "Carls_Jr", "Taco_Bell", "Sonic", "In_N_Out", "Whataburger", "Chick_fil_A", "quarter")
jib10 %>%
  setNames(nms) %>%
  filter(attribute != "Sample Size [n]") %>%
  mutate(quarter = factor(quarter, levels = fct)) %>%
  # select(1,2, 12) %>%
  gather(chain, pct, 2:11) %>%
  filter(attribute == "Serves food I crave") %>%
  ggplot(aes(x=quarter, y=pct, group = chain, color = chain)) +
  geom_line(size=1.4) +
  scale_color_manual(values = pal) +
  theme_minimal() +
  # hrbrthemes::scale_y_percent(limits=c(.7, 1)) +
  labs(x= "Quarter", y = "Index",
       title = "Index Based on % of Past 3 Mo. Users \nGiving Rating of Excellent or Very Good on \n'Serves food I crave'")
ggsave(here("Desktop", "R", "jib", "jib_brand_tracker_graphs", "compet_index_food_I_crave_p3m.png"), dpi = 300, bg = "transparent",
       width = 7.07, height = 7.58)

#4
nms <- c("attribute", "JIB", "McDonalds", "Burger_King", "Wendys", "Carls_Jr", "Taco_Bell", "Sonic", "In_N_Out", "Whataburger", "Chick_fil_A", "quarter")
jib10 %>%
  setNames(nms) %>%
  filter(attribute != "Sample Size [n]") %>%
  mutate(quarter = factor(quarter, levels = fct)) %>%
  # select(1,2, 12) %>%
  gather(chain, pct, 2:11) %>%
  filter(attribute == "Variety of menu items to choose from") %>%
  ggplot(aes(x=quarter, y=pct, group = chain, color = chain)) +
  geom_line(size=1.4) +
  scale_color_manual(values = pal) +
  theme_minimal() +
  # hrbrthemes::scale_y_percent(limits=c(.7, 1)) +
  labs(x= "Quarter", y = "Index",
       title = "Index Based on % of Past 3 Mo. Users \nGiving Rating of Excellent or Very Good on \n'Variety of menu items to choose from'")
ggsave(here("Desktop", "R", "jib", "jib_brand_tracker_graphs", "compet_index_menu_variety_p3m.png"), dpi = 300, bg = "transparent",
       width = 7.07, height = 7.58)

#5
nms <- c("attribute", "JIB", "McDonalds", "Burger_King", "Wendys", "Carls_Jr", "Taco_Bell", "Sonic", "In_N_Out", "Whataburger", "Chick_fil_A", "quarter")
jib10 %>%
  setNames(nms) %>%
  filter(attribute != "Sample Size [n]") %>%
  mutate(quarter = factor(quarter, levels = fct)) %>%
  # select(1,2, 12) %>%
  gather(chain, pct, 2:11) %>%
  filter(attribute == "Affordability of the prices") %>%
  ggplot(aes(x=quarter, y=pct, group = chain, color = chain)) +
  geom_line(size=1.4) +
  scale_color_manual(values = pal) +
  theme_minimal() +
  # hrbrthemes::scale_y_percent(limits=c(.7, 1)) +
  labs(x= "Quarter", y = "Index",
       title = "Index Based on % of Past 3 Mo. Users \nGiving Rating of Excellent or Very Good on \n'Affordability of the prices'")
ggsave(here("Desktop", "R", "jib", "jib_brand_tracker_graphs", "compet_index_affordability_of_prices_p3m.png"), dpi = 300, bg = "transparent",
       width = 7.07, height = 7.58)

#Now, we're going to average the indexes and see where each competitor is solidified.
jib10 %>%
  setNames(nms) %>%
  filter(attribute != "Sample Size [n]") %>%
  mutate(quarter = factor(quarter, levels = fct)) %>%
  # select(1,2, 12) %>%
  group_by(attribute) %>%
  # colMeans()
  summarise(jib_avg = mean(JIB), mc_avg = mean(McDonalds), bk_avg = mean(Burger_King),  wendy_avg = mean(Wendys), carl_avg = mean(Carls_Jr),
            innout_avg = mean(In_N_Out), what_avg = mean(Whataburger), cfa_avg = mean(Chick_fil_A)) %>%
  arrange((jib_avg))

#On AVERAGE...
#JIB has the highest index and no one else beating us with "Offers a menu with everything available anytime, day or night"
#JIB has significant overindexes on Variety of menu items to choose from , Is a great place to go for late night, Is a great place to go for breakfast
#JIB has the lowest index and no one worse with "Is a place I like to go to eat inside the restaurant"
#JIB has significant underindexes on Cleanliness of the restaurant, Employees appear to be well-trained, 
#cont... Employee appearance is neat and well-groomed, The restaurants are staffed to handle the workload, Quality of the ingredients
#cont... Friendliness and courtesy of the employees, Being a restaurant I recommend, Freshness of food 
#Now make charts of these ^^^^

jib10 %>%
  setNames(nms) %>%
  filter(attribute != "Sample Size [n]") %>%
  mutate(quarter = factor(quarter, levels = fct)) %>%
  # select(1,2, 12) %>%
  gather(chain, pct, 2:11) %>%
  filter(attribute == "Freshness of food") %>%
  ggplot(aes(x=quarter, y=pct, group = chain, color = chain)) +
  geom_line(size=1.4) +
  scale_color_manual(values = pal) +
  theme_minimal() +
  # hrbrthemes::scale_y_percent(limits=c(.7, 1)) +
  labs(x= "Quarter", y = "Index",
       title = "Index Based on % of Past 3 Mo. Users \nGiving Rating of Excellent or Very Good on \n'Freshness of food'")
ggsave(here("Desktop", "R", "jib", "jib_brand_tracker_graphs", "compet_index_freshenss_of_food_p3m.png"), dpi = 300, bg = "transparent",
       width = 7.07, height = 7.58)

#Jack the character
fct <- c("AMJ17", "JAS17", "OND17", "JFM18", "AMJ18", "JAS18", "OND18")
nmes <- c("question", "AMJ17", "JAS17", "OND17", "JFM18", "AMJ18", "JAS18", "OND18", "jack_character")

jib_char %>%
  setNames(nmes) %>%
  filter(question == "Much More Likely") %>%
  gather(quarter, pct, 2:8) %>% 
  mutate(quarter = factor(quarter, levels = fct)) %>%
  ggplot(aes(x=quarter, y=pct, group = jack_character, col = jack_character)) +
  geom_line(size = 1.4) +
  theme_minimal() +
  hrbrthemes::scale_y_percent(limits=c(.1, .25)) +
  labs(x= "Quarter", y = "Percent",
       title = "Consideration Much More Likely Based on Jack the Character",
       color = "Jack the Character")
ggsave(here("Desktop", "R", "jib", "jib_brand_tracker_graphs", "jack_char_much_more_likely.png"), dpi = 300, bg = "transparent",
       width = 7.07, height = 7.58)
  

####### cor plots ########
library(corrplot)
library(RColorBrewer)

#turn data into only numerics
tst <- jib1 %>%
  clean_names() %>%
  filter(awareness_and_usage_measures == "Past Month Purchase Level (Any Occasion)") %>%
  select(-1,-12) %>%
  mutate(in_n_out = as.numeric(in_n_out))

#correlation option 1
M <-cor(tst)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

#correlation option 2
require("corrplot")
rquery.cormat(tst)

source("http://www.sthda.com/upload/rquery_cormat.r")
require("corrplot")
rquery.cormat(tst)

tst
cor.test(tst$jack_in_the_box, tst$burger_king)

### breakfast cor
jib_compet6 <- jib_compet6 %>% clean_names()

tst2 <- jib_compet6 %>%
  mutate_at(vars(jas16:jas17), funs(as.numeric(.))) %>%
  filter(time_of_day == "breakfast") %>%
  gather(date, val, 2:10) %>%
  spread(chain, val) %>%
  select(-1, -2, -6)
  
cor(tst2)
rquery.cormat(tst2)
ggsave(here("Desktop", "breakfast_corr.png"), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

# questions from the other data set, sheet 1

jib_compet1 <- jib_compet1 %>% clean_names()

corr_menu <- jib_compet1 %>% 
  filter(question == "Value for the money") %>%
  select(-c(question, quarter))

#write a function/loop that spits out a graph for all of the questions in jib_compet1
qs <- jib_compet1 %>% distinct(question) %>% pull(question)
for (i in 2:31) {
  tmp <- jib_compet1 %>%
    filter(question == qs[i]) %>%
    select(-c(question, quarter))
  res1 <- cor.mtest(tmp, conf.level = 0.95)
  M <- cor(tmp)
  corrplot(M, method = "number", type = "lower", p.mat = res1$p, sig.level = 0.05, insig = "blank",
           title = qs[i],
           mar = c(0, 0, 3, 2))
}

cor(tmp)
rquery.cormat(corr_menu)
cor.test(corr_menu)
# ggsave(here("Desktop", "breakfast_corr.png"), dpi = 300, bg = "transparent",
#        width = 7.07, height = 6.58)

res1 <- cor.mtest(corr_menu, conf.level = 0.95)



  