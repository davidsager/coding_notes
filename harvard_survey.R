# Tags: harvard questionairre, count, spread, survey

#https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi%3A10.7910/DVN/GDF6Z0

library(dplyr)
library(ggplot2)

names(x)
x %>% count(pew_bornagain)

## filter for pew boragains

# Find % of born again that think abortion is not important
x %>% count(CC16_301b, pew_bornagain) %>%
  arrange(pew_bornagain)

# Find %
x %>% 
  count(religpew, CC16_305_2) %>% data.frame() %>%
  arrange(religpew) %>%
  mutate(pct_jew_lost_job = (1367/sum(n)*100)) %>%
  ggplot(aes(x=religpew, y=pct_jew_lost_job))

# 1. 
x %>% 
  count(CC16_305_2, religpew) %>%
  spread(CC16_305_2, n) %>%
  mutate(pct = Yes/(Yes+No)) %>%
  ggplot(aes(x=reorder(religpew, pct), y=pct)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_point(color = "darkblue") +
  labs(x = "Religion", y = "Percent Lost Job")

# 2. Faminc, reads newspaper
tmp <- x %>%
  count(faminc, CC16_300_3) %>%
  spread(CC16_300_3, n) %>%
  mutate(pct = Yes/(Yes+No)*100) %>%
  filter(faminc != "$150,000 or more") %>%
  filter(faminc != "Prefer not to say")
  
ggplot(tmp, aes(x=faminc, y=pct)) +
  geom_point(col = "darkblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = mean(tmp$pct), linetype="dashed") +
  labs(x = "HH Income", y = "Percent Read Newspaper") +
  annotate("text", label = "mean = 52%", x = 2, y = 54, size = 4, colour = "red")
    

# 3. Education & Post on social media
# educ, CC16_300d_1, ideo5
x %>%
  count(ideo5, CC16_300d_1) %>%
  spread(CC16_300d_1, n) %>%
  mutate(pct_post = Yes/(Yes+No)*100) %>%
  ggplot(aes(x=reorder(ideo5, pct_post), y=pct_post)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_point()

#3.5. Political leaning and posting on social
# ideo5, CC16_300d_1
x %>%
  count(ideo5, CC16_300d_1) %>%
  spread(CC16_300d_1, n) %>%
  mutate(pct_post = Yes/(Yes+No)*100) %>%
  ggplot(aes(x=ideo5, y=pct_post)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_point()

#3.75. religpew, CC16_300d_1
x %>%
  count(religpew, CC16_300d_1) %>%
  na.omit %>%
  spread(CC16_300d_1, n) %>%
  mutate(pct_post = Yes/(Yes+No)*100) %>%
  ggplot(aes(x=religpew, y=pct_post)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_point()

#4. Race vs ideo5 vs Should deport
# CC16_331_7, ideo5, race
x %>%
  count(CC16_331_7, ideo5, race) %>%
  spread(CC16_331_7, n) %>%
  filter(race == "White" | race == "Black" | race == "Hispanic") %>%
  mutate(pct_deport = Yes/(Yes+No)*100) %>%
  ggplot(aes(x=ideo5, y=pct_deport, group = race, color = race)) +
  geom_point(size = 3) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values=c("black", "red", "yellow"))

  #who voted for. CC16_410a,
  #what party registered with. CC16_360
  #rate yourself politically. CC16_340a
  #gay marriage. CC16_335 

#######################################################################################
#######################################################################################

#1. Map out people from which political party voted for which candidate
#2. Then put it on a map to see which state flipped the most
#1.
tmp <- x %>%
  select(voted_for = CC16_410a, 
         #use column inputstate
         pid = CC16_360, state = inputstate)
flippers <- tmp %>%
  count(voted_for, pid, state) %>%
  #Filter by only dems who voted for trump
  filter(pid == "Democratic Party") %>%
  filter(voted_for != "Hillary Clinton (Democrat)") %>%
  filter(voted_for != "<NA>" & voted_for != "I didn't vote in this election" &
           voted_for != "I'm not sure") %>%
  group_by(state) %>%
  summarize(num_flipped = sum(n))
#Call this group "flippers"
#left_join these 2
flippers
us@data

us@data <- us@data %>%
  left_join(flippers, by = c("name" = "state"))

#2. Mapping...
#left join this group to us@data
# add "flippers" into gg


#there is state data. Map out the people who voted against their party.

#Load in albersusa. https://github.com/hrbrmstr/albersusa
devtools::install_github("hrbrmstr/albersusa")
library(albersusa)
library(sf)
library(sp)
library(rgeos)
library(maptools)
library(ggplot2)
install.packages("ggalt")
devtools::install_github("hrbrmstr/ggalt")
library(ggalt)
install.packages("ggthemes")
library(ggthemes)
install.packages("viridis")
library(viridis)
library(scales)

us <- usa_composite()
us_map <- fortify(us, region="name")

gg <- ggplot()

gg <- gg + geom_map(data=us_map, map=us_map,
                    aes(x=long, y=lat, map_id=id),
                    color="#2b2b2b", size=0.1, fill=NA)
gg <- gg + theme_map()


gg + 
  geom_map(data=us@data, map=us_map,
           aes(fill=num_flipped, map_id=name),
           color="white", size=0.1) +
  coord_proj(us_laea_proj) +
  scale_fill_viridis(name="2014 Populaton Estimates", labels=comma) +
  theme(legend.position="top", 
        legend.key.width=unit(3, "lines"))


########################################################################

x %>%
  count(CC16_335, CC16_340a) %>%
  spread(CC16_335, n) %>%
  mutate(pct_favor = Favor/(Favor+Oppose)*100) %>%
  ggplot(aes(x=CC16_340a, y=pct_favor)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_point()
  
  
#pg 129 = pre
#pg 134 = post
