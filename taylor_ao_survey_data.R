pacman::p_load(tidyverse, janitor, glue, here, readxl)

# read in data and clean #
ao <- read_excel(here("desktop", "R", "AOI", "data", "ao_agent_survey.xlsx")) %>%
  clean_names()

# get rid of values read in as dates and then missed by R ingest
ao[,3:79][ao[,3:79] == "43626"] <- "6-10"
ao[,3:79][ao[,3:79] == "43470"] <- "1-5"
ao[,3:79][ao[,3:79] == "43789"] <- "11-20"
ao[,3:79][ao[,3:79] == "43529"] <- "3-5"
ao[,3:79][ao[,3:79] == "43467"] <- "1-2"
#   ____________________________________________________________________________
#   Functions to pull data                                                  ####

##  ............................................................................
##  Single Column                                                           ####

colPull <- function(column, graph = c("n", "pct"), relevel = NULL) {
  # get the title for the graph (this is question)
  title <- ao %>% 
    select(column) %>%
    names(.) %>%
    str_to_sentence(.) %>%
    str_replace_all(., "_", " ") %>%
    enframe(name = NULL) %>%
    mutate(value = ifelse(str_count(value, "\\S+") > 10,
                          str_replace_all(value, word(value, str_count(value, "\\S+")/2 + 1),
                                          paste0("\n", word(value, str_count(value, "\\S+")/2 + 1))), value)) %>%
    pull(value)
  
  if (is.null(relevel)) {
    # pull the column, summarise, and get percentage
    tmp <- ao %>%
      select((column)) %>%
      rename(q = names(.)) %>%
      count(q) %>%
      mutate(q = ifelse(str_count(q, "\\S+") > 6,
                        str_replace_all(q, word(q, str_count(q, "\\S+")/2+1),
                                        paste0("\n", word(q, str_count(q, "\\S+")/2+1))), q)) %>%
      filter(!is.na(q)) %>%
      mutate(frac = n/sum(n)) %>%
      arrange(desc(frac))
    print(tmp)
  } else {
    tmp <- ao %>%
      select((column)) %>%
      rename(q = names(.)) %>%
      count(q) %>%
      mutate(q = ifelse(str_count(q, "\\S+") > 6,
                        str_replace_all(q, word(q, str_count(q, "\\S+")/2+1),
                                        paste0("\n", word(q, str_count(q, "\\S+")/2+1))), q)) %>%
      filter(!is.na(q)) %>%
      mutate(q = factor(q, levels = relevel)) %>%
      mutate(frac = n/sum(n)) %>%
      arrange(desc(frac))
    print(tmp)
  }
  
  pal <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#999999')
  
  if (graph == "pct") {
    # plot it out 
    ggplot(tmp, aes(x = q, y = frac, label = scales::percent(round(frac, 4)))) + 
      geom_point(size = 3, col = "#154577") + 
      hrbrthemes::scale_y_percent() +
      coord_flip() + 
      ggrepel::geom_label_repel() + 
      scale_color_manual(values = pal) + 
      labs(x = "", y="", title = title,
           caption = paste0(sum(tmp$n), " Respondents answering")) +
      theme_minimal()
    # theme(axis.text.x = element_text(angle = 45, hjust = 1))
  } else {
    ggplot(tmp, aes(x = q, y = n, label = n)) + 
      geom_point() + 
      hrbrthemes::scale_y_comma() +
      coord_flip() + 
      ggrepel::geom_label_repel() + 
      labs(x = "", y="", title = title) +
      theme_minimal() +
      # theme(plot.title = element_text(hjust = 0.5))
  }
}


##  ............................................................................
##  Cross Tabs                                                              ####

crossTab <- function(segment, question, relevel = NULL) {
  
  q <- ao %>% 
    select(question) %>%
    names(.) %>%
    str_to_sentence(.) %>%
    str_replace_all(., "_", " ") %>%
    enframe(name = NULL) %>%
    mutate(value = ifelse(str_count(value, "\\S+") > 10, 
                          str_replace_all(value, word(value, str_count(value, "\\S+")/2 +1), 
                                          paste0("\n", word(value, str_count(value, "\\S+")/2 + 1))), value)) %>%
    pull(value)
  
  seg <- ao %>% 
    select(segment) %>%
    names(.) %>%
    str_to_sentence(.) %>%
    str_replace_all(., "_", " ") %>%
    enframe(name = NULL) %>%
    mutate(value = ifelse(str_count(value, "\\S+") > 10, 
                          str_replace_all(value, word(value, str_count(value, "\\S+")/2 +1), 
                                          paste0("\n", word(value, str_count(value, "\\S+")/2 + 1))), value)) %>%
    pull(value)
  
  if (is.null(relevel)) {
    ct_tmp <- ao %>%
      select(c(segment, question)) %>%
      rename(q1 = names(.[1]), q2 = names(.[2])) %>%
      count(q1, q2) %>%
      filter(!is.na(q1)) %>%
      filter(!is.na(q2)) %>%
      group_by(q1) %>%
      mutate(frac = n/sum(n)) 
  } else {
    ct_tmp <- ao %>%
      select(c(segment, question)) %>%
      rename(q1 = names(.[1]), q2 = names(.[2])) %>%
      count(q1, q2) %>%
      filter(!is.na(q1)) %>%
      filter(!is.na(q2)) %>%
      mutate(q1 = factor(q1, levels = relevel)) %>%
      group_by(q1) %>%
      mutate(frac = n/sum(n)) 
  }
  print(data.frame(ct_tmp))
  pal <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#999999')
  
  gg <- ggplot(ct_tmp, aes(x = q2, y = frac, group = q1, col = q1, 
                     label = scales::percent(round(frac,4)))) +
    geom_point() + 
    facet_wrap(~q1, ncol = 2) +
    hrbrthemes::scale_y_percent() + 
    scale_color_manual(values = pal) + 
    coord_flip() + 
    ggrepel::geom_label_repel(size = 2.5) + 
    theme_minimal() + 
    theme(legend.position = "none",
          strip.background =element_rect(fill="lightgray", 
                                         color = "transparent")) + 
    labs(x = "", y = "",
         title = q,
         subtitle = paste0('Segmented by: ', seg),
         caption = paste0(sum(ct_tmp$n), " Respondents answering"))
  return(gg)
  # ggsave(here('Desktop', paste0('plot_', question,'.png')), plot = gg, device = NULL, dpi = 300, bg = "transparent",
          # width = 15.07, height = 15.58)
}

crossTab(13, 67)

# get the distinct values of each # 
answer_pull <- function(column) {
  tmp <- ao %>%
    select((column)) %>%
    rename(q = names(.)) %>%
    distinct(q) %>%
    mutate(q = ifelse(str_count(q, "\\S+") > 6, 
                      str_replace_all(q, word(q, str_count(q, "\\S+")/2 + 1), 
                                      paste0("\n", word(q, str_count(q, "\\S+")/2 + 1))), q)) %>%
    filter(!is.na(q)) %>%
    pull(q) 
  dput(tmp)
}
answer_pull(col)

#   ____________________________________________________________________________
#   data check                                                              ####

# I NEED TO PERFORM A SANITY CHECK ON SOME OF THIS DATA
# THERE ARE TOO MANY IP ADDRESSES THAT ARE THE SAME 
# WHICH DOESN'T NECESSARILY MEAN ANYTHING...BUT STILL

# looking at IP addresses
ao %>% count(ip_address) %>% arrange(desc(n))
ao %>% filter(ip_address == "204.155.62.177") %>%
  count(.[[7]], .[[3]])


#   ____________________________________________________________________________
#   Single Column Looks                                                     ####

# to do manually 
# columns (22, 23, 24, 25, 26, 27, 40, 41)
names(ao)[3]
colPull(3, "n") # 303 didn't finish; 629 completed
crossTab(9, 7)

colPull(7, "pct") # states -- might need to create region 
colPull(9, "pct") # title - what is producer? 
colPull(11, "n") # age
colPull(12, "n") # state located 
colPull(13, "n") # region (urban to suburban)
colPull(15, "n") # duration of agency
colPull(16, "n") # number of producers
colPull(17, "n") # account executives
colPull(18, "n") # % pc for personal
colPull(19, "n") # pc for commercial
colPull(20, "n") # number insurance carriers place business with
colPull(21, "n") # number preferred carriers, flexibility etc
colPull(28, "n") # not helpful (have to bucket into deciles)
colPull(29, "n") # not helpful (have to bucket into deciles)
colPull(30, "n") # growing client base
colPull(32, "n") # agency marketing budget (need to bucket)
colPull(33, "n") # coop dollars from ao (need to bucket)
colPull(34, "n") # coop dollars from others (need to bucket)
# have to combine the below into one # 
colPull(35, "n") # importance of cold calling
colPull(36, "n") # importance of paid advertising
colPull(37, "n") # importance of referrals 
colPull(38, "n") # importance of working with partners
colPull(39, "n") # importance of community involvement
colPull(40, "n") # other (40, 41)
# have to combine these into one # 
colPull(42, "n") # billboards bus stops
colPull(43, "n") # local tv
colPull(44, "n") # radio
colPull(45, "n") # newspaper
colPull(46, "n") # direct mail
colPull(47, "n") # digital 
colPull(48, "n") # SEO
colPull(49, "n") # non paid organic
colPull(50, "n") # community involvement
colPull(51, "n") # other (51, 52)
# have to combine these into one # 
colPull(53, "n") # customer, direct to consumer
colPull(54, "n") # customer, captive insurance
colPull(55, "n") # other indie agency
colPull(56, "n") # no insurance
# have to combine these into one # 
colPull(57, "n") # negative claims exp
colPull(58, "n") # better coverage
colPull(59, "n") # more affordable
colPull(60, "n") # negative with agent
colPull(61, "n") # understand coverages
colPull(62, "n") # other (62, 63)
#
colPull(64, "n") # use comparative rater personal
colPull(65, "n") # demo changes personal with ao
colPull(66, "n") # if yes, what> 
#
colPull(67, "n") # contact for updates  (personal)
colPull(68, "n") # review personal, other
colPull(69, "n") # contact for updates (commercial)
colPull(70, "n") # review commercial (other )
colPull(71, "n") # empty#

colPull(72, "n") # education (print newsletter)
colPull(73, "n") # education (e-newsletter)
colPull(74, "n") # education (auto email)
colPull(75, "n") # education (none)
colPull(76, "n") # other (76, 77)
colPull(72, "n") # other 
table(ao[,78]) # wishlist (AO does an annual credit check?) 
# column 78 is a good opportunity for either udpipe language type or monkeylearn
# then segment by something (state, age, what is it that motivates criticism vs compliments?) 
table(ao[,79])
names(ao)[81]

# building out the factor list 
answer_pull(11)


#   ____________________________________________________________________________
#   ANALYSIS                                                                ####

colSums(is.na(ao[,1:79]))

# questions to use as segments # 
# 9 - role in company
answer_pull(10)
fct <- c("Principal", "Producer","Marketing Manager", "Account Executive/CSR", 
         "Other - Write In")
colPull(9, 'pct', relevel = fct)
ggsave(here('figures', 'agent_role.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

ao %>%
  select((10)) %>%
  rename(q = names(.)) %>%
  mutate(q = tolower(q)) %>%
  count(q) %>% arrange(desc(n)) %>% data.frame

#11 age
fct <- c("Between 20 and 30", "Between 30 and 40", "Between 40 and 50", "Between 50 and 60", "60+")
colPull(11, "pct", relevel = fct)
ggsave(here('figures', 'agent_age.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

# cross tab age and role
fct <- c("Between 20 and 30", "Between 30 and 40", "Between 40 and 50", "Between 50 and 60", "60+")
crossTab(11, 9, relevel = fct)
ggsave(here('figures', 'agent_age_position.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)
ao %>%
  select(c(10, 11)) %>%
  rename(q1 = names(.[1]), q2 = names(.[2])) %>%
  filter(q2 == "Between 20 and 30") %>%
  filter(!is.na(q1))

# 12 state
colPull(12, "pct")
ggsave(here('figures', 'agent_state.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

# 13 region 
answer_pull(13)
colPull(13, 'pct')
ggsave(here('figures', 'agent_regions.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

#15 
names(ao[15])
colPull(15, 'pct')
ggsave(here('figures', 'agent_agency_age.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

# 16
cat <- c( "1-5", "6-10","11-20","21-30","More than 30")
answer_pull(16)
names(ao[16])
colPull(16, 'pct', relevel = cat)
ggsave(here('figures', 'agent_size.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

# 17 (account execs)
names(ao[17])
cat <- c( "1-5", "6-10","11-20","21-30","More than 30")
colPull(17, 'pct', relevel = cat)
ggsave(here('figures', 'agent_ea_size.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

#18 pc personal
names(ao[18])
answer_pull(18)
cat <- c("Less than 20%","20%-39%", "40%-59%", "60% or more",   "Don't know", 
         "Not applicable")
colPull(18, 'pct',relevel = cat)
ggsave(here('figures', 'agent_pc_personal.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

##19 pc commercial
names(ao[19])
answer_pull(19)
cat <- c("Less than 20%","20%-39%", "40%-59%", "60% or more",   "Don't know", 
         "Not applicable")
colPull(19, 'pct',relevel = cat)
ggsave(here('figures', 'agent_pc_commercial.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

#20 
names(ao[col])
answer_pull(col)
cat <- c("1-2","3-5",  "6-10", "More than 10")
colPull(col, 'pct',relevel = cat)
ggsave(here('figures', 'agent_carriers_used.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

#21
col <- 21
names(ao[col])
answer_pull(col)
cat <- c("I have 1 preferred carrier, \nbut place business with other carriers", 
         "I have 1 or 2 carriers \nthat I prefer for specific products", 
         "I have 2-3 preferred carriers, \nbut place business with other carriers", 
         "I place business equally \nwith many different carriers")
colPull(col, 'pct', relevel = cat)
# ggsave(here('figures', 'agent_carrier_relationship.png'), dpi = 300, bg = "transparent",
       # width = 7.07, height = 6.58)

#22
col <- 22
names(ao[22])
test <- ao %>% 
  select(22) %>%
  rename(q = names(.)) %>% mutate(q = tolower(q)) %>%
  count(q) %>%
  pull(q) 

paste(as.character(test), sep=" ", collapse="|")

ao_vec <- c("a-o|a/0|a/o|a/owners|about-owners|ao|aoi|aoins|aoto owners|auro-owners|auto -owners|auto onwers|auto owneers|auto owner|auto owner's|auto owners|auto owners (new to agcy)|auto owners ins. co.|auto owners insurance|auto ownes|auto- owners|auto-owner|auto-owner's|auto-owners|auto-owners ins co|auto-owners ins group|auto-owners ins.|auto-owners insurance|auto-owners insurance group|autoowner|autoowners")

tmp <- ao %>%
  select(22) %>%
  rename(q = names(.)) %>% mutate(q = tolower(q)) %>%
  mutate(q = str_replace_all(q, ao_vec, "AO")) %>%
  mutate(q = ifelse(str_detect(q, "AO"), "Auto-Owners", q)) %>%
  mutate(q = ifelse(str_detect(q, "bear"), "Bear River Mutual", q)) %>%
  mutate(q = ifelse(str_detect(q, "central"), 'Central Mutual Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("cincinnati|cinncinati")), 'Cincinnati Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("citizen|ctizens")), 'Citizens Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, "emc"), 'EMC', q)) %>%
  mutate(q = ifelse(str_detect(q, "erie"), 'Erie Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("farmers|farmer's")), 'Farmers Mutual of Nebraska', q)) %>%
  mutate(q = ifelse(str_detect(q, c("fcci")), 'FCCI Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("frankenmuth")), 'Frankenmuth Mutual', q)) %>%
  mutate(q = ifelse(str_detect(q, c("grange")), 'Grange', q)) %>%
  mutate(q = ifelse(str_detect(q, c("hastings")), 'Hastings Mutual', q)) %>%
  mutate(q = ifelse(str_detect(q, c("liberty")), 'Liberty Mutual', q)) %>%
  mutate(q = ifelse(str_detect(q, c("met")), 'MetLife', q)) %>%
  mutate(q = ifelse(str_detect(q, c("motorists")), 'Motorists Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("north star")), 'North Star Mutual', q)) %>%
  mutate(q = ifelse(str_detect(q, c("ohio mutual")), 'Ohio Mutual', q)) %>%
  mutate(q = ifelse(str_detect(q, c("pekin")), 'Pekin Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("trav")), 'Travelers', q)) %>%
  mutate(q = ifelse(str_detect(q, c("west bend")), 'West Bend Mutual', q)) %>%
  count(q) %>%
  filter(!is.na(q)) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  mutate(grp = ifelse(n > 600, 1, 2)) %>%
  mutate(q = str_to_title(q))

ggplot(tmp, aes(x = reorder(q, n), y = n, fill = factor(grp), label = n)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  scale_fill_manual(values = c("#154577", "darkgray")) + 
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(nudge_y = 40) + 
  labs(x = "", y = "",
       title = "Top Insurance Carrier (1 of 3)",
       caption = paste0(sum(tmp$n), " Respondents answering"))
# ggsave(here('figures', 'agent_top_carrier1.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)  

#24
col <- 24
names(ao[24])
test <- ao %>% 
  select(24) %>%
  rename(q = names(.)) %>% mutate(q = tolower(q)) %>%
  count(q) %>%
  pull(q) 

tmp <- ao %>%
  select(24) %>%
  rename(q = names(.)) %>% mutate(q = tolower(q)) %>%
  mutate(q = str_replace_all(q, ao_vec, "AO")) %>%
  mutate(q = ifelse(str_detect(q, "AO"), "Auto-Owners", q)) %>%
  mutate(q = ifelse(str_detect(q, "auto/owners"), "Auto-Owners", q)) %>%
  mutate(q = ifelse(str_detect(q, "bear"), "Bear River Mutual", q)) %>%
  mutate(q = ifelse(str_detect(q, "central"), 'Central Mutual Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("cinci|cincinnatti|cincinnnait|cincinnati|cinncinati")), 'Cincinnati Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("cit|citizen|ctizens")), 'Citizens Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, "emc"), 'EMC', q)) %>%
  mutate(q = ifelse(str_detect(q, "erie"), 'Erie Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("farmers|farmer's")), 'Farmers Mutual of Nebraska', q)) %>%
  mutate(q = ifelse(str_detect(q, c("fcci")), 'FCCI Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("frankenmuth")), 'Frankenmuth Mutual', q)) %>%
  mutate(q = ifelse(str_detect(q, c("fremont")), 'Fremont Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("grange")), 'Grange', q)) %>%
  mutate(q = ifelse(str_detect(q, c("hastings")), 'Hastings Mutual', q)) %>%
  mutate(q = ifelse(str_detect(q, c("liberty|libetry")), 'Liberty Mutual', q)) %>%
  mutate(q = ifelse(str_detect(q, c("met")), 'MetLife', q)) %>%
  mutate(q = ifelse(str_detect(q, c("motorists|motorist")), 'Motorists Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("north star")), 'North Star Mutual', q)) %>%
  mutate(q = ifelse(str_detect(q, c("ohio mutual")), 'Ohio Mutual', q)) %>%
  mutate(q = ifelse(str_detect(q, c("pekin")), 'Pekin Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("trav")), 'Travelers', q)) %>%
  mutate(q = ifelse(str_detect(q, c("west bend")), 'West Bend Mutual', q)) %>%
  mutate(q = ifelse(str_detect(q, c("all state|allstate")), 'AllState', q)) %>%
  mutate(q = ifelse(str_detect(q, c("encompass")), 'Encompass', q)) %>%
  mutate(q = ifelse(str_detect(q, c("nationwide")), 'Nationwide Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("progessive")), 'Progressive Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("safeco")), 'Safeco Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("selective")), 'Selective Insurance', q)) %>%
  count(q) %>%
  filter(!is.na(q)) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  mutate(grp = ifelse(n > 130, 1, 2)) %>%
  mutate(q = str_to_title(q))

ggplot(tmp, aes(x = reorder(q, n), y = n, fill = factor(grp), label = n)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  scale_fill_manual(values = c("#154577", "darkgray")) + 
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(nudge_y =5) + 
  labs(x = "", y = "",
       title = "Top Insurance Carrier (2 of 3)",
       caption = paste0(sum(tmp$n), " Respondents answering"))
ggsave(here('figures', 'agent_top_carrier2.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

#26
col <- 26
names(ao[26])
ao %>% 
  select(26) %>%
  rename(q = names(.)) %>% mutate(q = tolower(q)) %>%
  count(q) %>%
  pull(q) 

tmp <- ao %>%
  select(26) %>%
  rename(q = names(.)) %>% mutate(q = tolower(q)) %>%
  mutate(q = str_replace_all(q, ao_vec, "AO")) %>%
  mutate(q = ifelse(str_detect(q, "AO"), "Auto-Owners", q)) %>%
  mutate(q = ifelse(str_detect(q, "auto/owners"), "Auto-Owners", q)) %>%
  mutate(q = ifelse(str_detect(q, "bear"), "Bear River Mutual", q)) %>%
  mutate(q = ifelse(str_detect(q, "central"), 'Central Mutual Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("cinci|cincinnatti|cincinnnait|cincinnati|cinncinati")), 'Cincinnati Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("cit|citizen|ctizens")), 'Citizens Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, "emc"), 'EMC', q)) %>%
  mutate(q = ifelse(str_detect(q, "erie"), 'Erie Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("farmers|farmer's")), 'Farmers Mutual of Nebraska', q)) %>%
  mutate(q = ifelse(str_detect(q, c("fcci")), 'FCCI Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("frankenmuth")), 'Frankenmuth Mutual', q)) %>%
  mutate(q = ifelse(str_detect(q, c("fremont")), 'Fremont Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("grange")), 'Grange', q)) %>%
  mutate(q = ifelse(str_detect(q, c("hastings")), 'Hastings Mutual', q)) %>%
  mutate(q = ifelse(str_detect(q, c("liberty|libetry")), 'Liberty Mutual', q)) %>%
  mutate(q = ifelse(str_detect(q, c("met")), 'MetLife', q)) %>%
  mutate(q = ifelse(str_detect(q, c("motorists|motorist")), 'Motorists Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("north star")), 'North Star Mutual', q)) %>%
  mutate(q = ifelse(str_detect(q, c("ohio mutual")), 'Ohio Mutual', q)) %>%
  mutate(q = ifelse(str_detect(q, c("pekin")), 'Pekin Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("trav")), 'Travelers', q)) %>%
  mutate(q = ifelse(str_detect(q, c("west bend")), 'West Bend Mutual', q)) %>%
  mutate(q = ifelse(str_detect(q, c("all state|allstate")), 'AllState', q)) %>%
  mutate(q = ifelse(str_detect(q, c("encompass")), 'Encompass', q)) %>%
  mutate(q = ifelse(str_detect(q, c("nationwide")), 'Nationwide Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("progessive")), 'Progressive Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("safeco")), 'Safeco Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("selective")), 'Selective Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("grainge")), 'Grange', q)) %>%
  mutate(q = ifelse(str_detect(q, c("grinell|grinnell")), 'Grinnell Mutual', q)) %>%
  mutate(q = ifelse(str_detect(q, c("hanover")), 'Hanover Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("hartford")), 'Hartford Insurance', q)) %>%
  mutate(q = ifelse(str_detect(q, c("stateauto")), 'state auto', q)) %>%
  count(q) %>%
  filter(!is.na(q)) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  mutate(grp = ifelse(n == 54, 1, 2)) %>%
  mutate(q = str_to_title(q))

ggplot(tmp, aes(x = reorder(q, n), y = n, fill = factor(grp), label = n)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  scale_fill_manual(values = c("#154577", "darkgray")) + 
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(nudge_y =5) + 
  labs(x = "", y = "",
       title = "Top Insurance Carrier (3 of 3)",
       caption = paste0(sum(tmp$n), " Respondents answering"))
ggsave(here('figures', 'agent_top_carrier3.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

# HOW TO RELEVEL
# col <- 28
# names(ao[col])
# answer_pull(col)
# cat <- c("I have 1 preferred carrier, \nbut place business with other carriers", 
#          "I have 1 or 2 carriers \nthat I prefer for specific products", 
#          "I have 2-3 preferred carriers, \nbut place business with other carriers", 
#          "I place business equally \nwith many different carriers")
# colPull(col, 'pct', relevel = cat)

#28
percents <- c("0-9%", "10-19%", "20-29%", "30-39%", "40-49%", "50-59%", "60-69%", "70-79%", "80-89%", "90-99%", "100+%")
ao %>%
  select(28) %>%
  rename(q = names(.)) %>%
  mutate(q = as.numeric(q)) %>%
  mutate(x = cut(q, breaks=c(0,.09,.19,.29,.39,.49,.59,.69,.79,.89,.99, Inf), labels = percents)) %>%
  group_by(x) %>%
  summarise(total = n()) %>%
  filter(!is.na(x)) %>%
  mutate(frac = total/sum(total)) %>%
  ggplot(aes(x = x, y = frac, label = scales::percent(round(frac, 4)))) + 
  geom_bar(stat='identity', fill = "#154577") + 
  hrbrthemes::scale_y_percent(limits=c(0, .22)) +
  coord_flip() +
  geom_text(nudge_y = .01) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", y = "",
       title = "Percent of Policyholders Covered by AO for Personal",
       caption = paste0(sum(tmp$n), " Respondents answering"))
ggsave(here('Desktop', 'percent_covered_ao_personal.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

#29
percents <- c("0-9%", "10-19%", "20-29%", "30-39%", "40-49%", "50-59%", "60-69%", "70-79%", "80-89%", "90-99%", "100+%")
ao %>%
  select(29) %>%
  rename(q = names(.)) %>%
  mutate(q = as.numeric(q)) %>%
  mutate(x = cut(q, breaks=c(0,.09,.19,.29,.39,.49,.59,.69,.79,.89,.99, Inf), labels = percents)) %>%
  group_by(x) %>%
  summarise(total = n()) %>%
  filter(!is.na(x)) %>%
  mutate(frac = total/sum(total)) %>%
  ggplot(aes(x = x, y = frac, label = scales::percent(round(frac, 4)))) + 
  geom_bar(stat='identity', fill = "#154577") + 
  hrbrthemes::scale_y_percent(limits=c(0, .22)) +
  coord_flip() +
  geom_text(nudge_y = .01) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", y = "",
       title = "Percent of Policyholders Covered by AO for Commercial",
       caption = paste0(sum(tmp$n), " Respondents answering"))
ggsave(here('Desktop', 'percent_covered_ao_commercial.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

#30
colPull(30, "n")
ggsave(here('Desktop', 'agency_growth_goals.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

#32
segments <- c("$0-4,999", "$5,000-9,999", "$10,000-49,999", "$50,000-99,999", "$100,000-199,999", "$200,000+")
tmp <- ao %>%
  select(32) %>%
  rename(q = names(.)) %>%
  mutate(q = as.numeric(q)) %>%
  mutate(x = cut(q, breaks=c(0, 4999, 9999, 49999, 99999, 199999, Inf), labels = segments)) %>%
  group_by(x) %>%
  summarise(total = n()) %>%
  filter(!is.na(x)) %>%
  mutate(frac = total/sum(total))
ggplot(tmp, aes(x = x, y = frac, label = scales::percent(round(frac, 4)))) + 
  geom_bar(stat='identity', fill = "#154577") + 
  hrbrthemes::scale_y_percent(limits=c(0, .55)) +
  coord_flip() +
  geom_text(nudge_y = .03) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", y = "",
       title = "Marketing Budget",
       caption = paste0(sum(tmp$total), " Respondents answering"))
ggsave(here('Desktop', 'marketing_budget.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

# average 
ao %>%
  select(32) %>%
  rename(q = names(.)) %>%
  mutate(q = as.numeric(q)) %>% summarise(mean(q, na.rm = TRUE))

#33
segments <- c("$0-4,999", "$5,000-9,999", "$10,000-49,999", "$50,000-99,999", "$100,000-199,999", "$200,000+")
tmp <- ao_age %>%
  select(c(11,33)) %>%
  rename(age = names(.[1]), q = names(.[2])) %>%
  mutate(q = as.numeric(q)) %>% 
  mutate(x = cut(q, breaks=c(0, 4999, 9999, 49999, 99999, 199999, Inf), labels = segments)) %>%
  group_by(age, x) %>%
  summarise(total = n()) %>%
  filter(!is.na(x)) %>%
  mutate(frac = total/sum(total))
ggplot(tmp, aes(x = x, y = frac, group = age, fill = age, label = scales::percent(round(frac, 4)))) + 
  geom_bar(stat='identity', position = "dodge") + 
  hrbrthemes::scale_y_percent(limits=c(0, .65)) +
  coord_flip() +
  # geom_text(position = "dodge") +
  theme_minimal() +
  # theme(legend.position = "none") +
  labs(x = "", y = "",
       title = "AO Co-Op Marketing Budget",
       caption = paste0(sum(tmp$total), " Respondents answering"))
ggsave(here('Desktop', 'plot_33_ao_co_op_marketing_budget.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

# average 
ao %>%
  select(33) %>%
  rename(q = names(.)) %>%
  mutate(q = as.numeric(q)) %>% summarise(mean(q, na.rm = TRUE))

#34
segments <- c("$0-4,999", "$5,000-9,999", "$10,000-49,999", "$50,000-99,999", "$100,000-199,999", "$200,000+")
tmp <- ao %>%
  select(34) %>%
  rename(q = names(.)) %>%
  mutate(q = as.numeric(q)) %>%
  mutate(x = cut(q, breaks=c(0, 4999, 9999, 49999, 99999, 199999, Inf), labels = segments)) %>%
  group_by(x) %>%
  summarise(total = n()) %>%
  filter(!is.na(x)) %>%
  mutate(frac = total/sum(total))
ggplot(tmp, aes(x = x, y = frac, label = scales::percent(round(frac, 4)))) + 
  geom_bar(stat='identity', fill = "#154577") + 
  hrbrthemes::scale_y_percent(limits=c(0, .65)) +
  coord_flip() +
  geom_text(nudge_y = .03) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "", y = "",
       title = "Other Carrier Co-Op Marketing Budget",
       caption = paste0(sum(tmp$total), " Respondents answering"))
ggsave(here('Desktop', 'other_carrier_co_op_marketing_budget.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

# average 
ao %>%
  select(34) %>%
  rename(q = names(.)) %>%
  mutate(q = as.numeric(q)) %>% summarise(mean(q, na.rm = TRUE))

#35
ao %>%
  select(35)

########### Taylor's code to segment by questions ###########

nms <- c("Age", "Billboards", "Local TV", "Radio", "Print News", 
         "Direct Mail", "Digital Ads", "SEO", "Organic Social", "Community Involvement",
         "Other")
ao %>%
  select(c(13, 42:51)) %>%
  setNames(nms) %>%
  gather(priority, rank, 2:11) %>%
  filter(!is.na(rank)) %>%
  count(Age, priority, rank) %>%
  filter(rank %in% c(1,2)) %>%
  filter(priority == "Print News") %>%
  group_by(Age) %>%
  summarise(sum(n))
  arrange(Age) %>%
  group_by(Age) %>% 
  mutate(frac = n/sum(n)) %>%
  ggplot(aes(x = priority, y = frac)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~Age) +
  coord_flip()
           
  group_by(Age, priority) %>%
  count(rank) %>%
  # filter(Age == "Urban (close to large metro area)") %>%
  filter(priority == "Community Involvement") %>%
  filter(Age != "Other") %>%
  ggplot(aes(x = factor(rank), y = n)) + 
  geom_bar(stat = "identity", fill = "#154577") +
  facet_wrap(~Age) + 
  theme_minimal() +
  labs(x = "", y = "Count", 
       title = "Importance of Print News marketing tactics",
       caption = "673 Respondents answering")
ggsave(here('Desktop', 'seo_by_region.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

###########
segments <- c("$0-4,999", "$5,000-9,999", "$10,000-49,999", "$50,000-99,999", "$100,000-199,999", "$200,000+")
tmp <- 

ao %>%
  select(33) %>%
  rename(q = names(.)) %>%
  mutate(q = as.numeric(q)) %>% 
  mutate(x = cut(q, breaks=c(0, 4999, 9999, 49999, 99999, 199999, Inf), labels = segments)) %>%
  group_by(x) %>%
  summarise(total = n()) %>%
  filter(!is.na(x)) %>%
  filter(x != "$0-4,999") %>%
  mutate(frac = total/sum(total))
  
ao %>%
  select(34) %>%
  rename(q = names(.)) %>%
  mutate(q = as.numeric(q)) %>% 
  mutate(x = cut(q, breaks=c(0, 4999, 9999, 49999, 99999, 199999, Inf), labels = segments)) %>%
  group_by(x) %>%
  summarise(total = n()) %>%
  filter(!is.na(x)) %>%
  filter(x != "$0-4,999") %>%
  mutate(frac = total/sum(total))

#find on average how much money is given to each region
name <- c("Region", "Budget", "AO_Money", "Other_Money")
ao %>%
  select(c(13, 32:34)) %>%
  setNames(name) %>%
  mutate(Budget = as.numeric(Budget)) %>%
  mutate(AO_Money = as.numeric(AO_Money)) %>%
  mutate(Other_Money = as.numeric(Other_Money)) %>%
  filter(!is.na(Budget)) %>%
  filter(!is.na(AO_Money)) %>%
  filter(!is.na(Other_Money)) %>%
  mutate(total_money = AO_Money + Other_Money) %>%
  filter(total_money >= 5000) %>%
  group_by(Region) %>%
  summarise(total = mean(total_money)) %>%
  ggplot(aes(x = Region, y = total)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(y = "Average Co-Op Dollars", 
       title = "Average Co-Op Dollars by Region")
ggsave(here('Desktop', 'money_given_by_region.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

# Comparative rater by region (64 x 13)
name <- c("region", "comparative_rater")
ao %>%
  select(13, 64) %>%
  setNames(name) %>%
  filter(!is.na(comparative_rater)) %>%
  count(region, comparative_rater) %>%
  group_by(region) %>%
  mutate(frac = n/sum(n)) %>%
  mutate(comparative_rater = ifelse(str_detect(comparative_rater, "with all"),
                                    "Yes",
                                    ifelse(str_detect(comparative_rater, "short list"),
                                           "Yes, some carriers",
                                           ifelse(str_detect(comparative_rater, "Only if"),
                                                  "If sense good option",
                                           comparative_rater)))) %>%
  filter(comparative_rater %in% c("Yes", "No")) %>%
  filter(region != "Other") %>%
  ungroup(region) %>%
  mutate(region = ifelse(str_detect(region, "Small town"), "Small town", region)) %>%
  ggplot(aes(x=region, y=frac, group = comparative_rater, col =comparative_rater, label = paste0(comparative_rater, ": ", scales::percent(frac)))) +
  # geom_bar(stat = 'identity', position = "dodge") +
  geom_point(size = 2.5) + 
  scale_color_manual(values = pal, name = "") + 
  coord_flip() + 
  ggrepel::geom_label_repel() + 
  theme_minimal() + 
  hrbrthemes::scale_y_percent() +
  theme(legend.position = 'none') + 
  labs(x = "", y = "",
       title = "Do you use a comparative rater with \nall carriers for personal lines")
ggsave(here('Desktop', 'comparative_rater_by_region.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

#### TEXT ANALYSIS ########
#q 78, 79, 40-41 (anyone who said 1-3 what are the things they're talking about)
library(tidytext)

name <- c("service_tools")
ao %>%
  select(78) %>%
  setNames(name) %>%
  filter(!is.na(service_tools)) %>%
  unnest_tokens(word, service_tools) %>%
  count(word, sort = TRUE) %>%
  anti_join(stop_words) %>%
  arrange(desc(n)) %>%
  filter(n >= 10) %>%
  ggplot(aes(x=reorder(word, desc(n)), y=n)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  

name <- c("what_ao_could_do_better")
marketing_word <- ao %>%
  select(78) %>%
  setNames(name) %>%
  filter(!is.na(what_ao_could_do_better)) %>%
  # filter(what_ao_could_do_better == "op") %>%
  unnest_tokens(word, what_ao_could_do_better) %>%
  count(word) %>%
  anti_join(stop_words) %>%
  arrange(desc(n)) %>%
  filter(n >= 10) %>%
  ggplot(aes(x=reorder(word, desc(n)), y=n)) +
  geom_point() +
  coord_flip()
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

name <- c("lead_generation_priority")
ao %>%
  select(40) %>%
  setNames(name) %>%
  filter(!is.na(lead_generation_priority))

## - --------------- bigrams  ######  ######  ######  ######  ######  ######
### some bigrams 
bigrams <- ao %>% 
  select(78) %>%
  rename(q = names(.[1])) %>%
  filter(!is.na(q)) %>%
  unnest_tokens(bigram, q, token = "ngrams", n = 2)

bigrams %>% 
  count(bigram, sort = TRUE)

bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

drops <- tibble(word = c("auto", "owners", "ao"))

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% drops$word) %>%
  filter(!word2 %in% drops$word)

# new bigram counts
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

bigram_counts %>%
  toupper() %>%
  write_csv("/Users/david.sager/Desktop/test12345.csv")

install.packages("igraph")
library(igraph)

bigram_graph <- bigram_counts %>%
  filter(n >= 2) %>%
  graph_from_data_frame()

install.packages("ggraph")
library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") + 
  geom_edge_link() + 
  geom_node_point() + 
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)  
theme_ipsum() 
ggsave("~/Desktop/ue_bigram.png", dpi = 400, bg = "transparent")

set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.1, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.02, 'inches')) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), size = 3, vjust = 1, hjust = 1) +
  theme_void()
ggsave("bigrams_78.png", dpi = 300, bg = "transparent", width = 8.32, height = 4.12)


ao %>%
  select(78) %>%
  rename(q = names(.[1])) %>%
  filter(!is.na(q)) %>%
  filter(str_detect(q, "SEO")) %>%
  data.frame() %>%
  write_csv("/Users/david.sager/Desktop/helpful_tools_lead_generation.csv")


#wordcloud

install.packages("ggwordcloud")
library(ggwordcloud)
library(ggplot2)

names(ao)
marketing_bigrams <- marketing_word %>%
  # mutate(str = paste0(word1, " ", word2)) %>%
  filter(n >5) %>%
  arrange(desc(n)) %>%
  filter(word != "na") %>%
  mutate(word = ifelse(str_detect(word, "newsletters"), "newsletter", word)) %>%
  mutate(word = ifelse(str_detect(word, "op"), "coop", word)) %>%
  mutate(word = ifelse(str_detect(word, "coop"), "co-op", word)) %>%
  mutate(word = ifelse(str_detect(word, "email"), "emails", word)) %>%
  mutate(word = ifelse(str_detect(word, "lead"), "leads", word)) %>%
  mutate(word = ifelse(str_detect(word, "info"), "information", word)) %>%
  filter(word != "owners") %>%
  filter(word != "auto") %>%
  group_by(word) %>%
  summarise(n = sum(n)) %>%
  arrange(desc(n))


colors <- c('#d53e4f','#f46d43','#fdae61','#fee08b','#e6f598','#abdda4','#66c2a5','#3288bd')
ggplot(marketing_bigrams, aes(label = word, size = n, color = n)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 24) +
  theme_minimal() +
  scale_color_gradient(low = "darkred", high = "red") +
  labs(title = "What Services, Tools, or Solutions do you Wish Carriers Would Offer in Order to Grow your Business",
       subtitle = "Most Frequently Used Words pulled from Responses") +
  theme(plot.title = element_text(hjust = .5, vjust = -20, size = 17),
        axis.title.x = element_text(hjust = .5, face = "italic", size = 8),
        axis.title.y = element_text(hjust = .5, face = 'italic', size = 8),
        plot.subtitle = element_text(hjust = 0.5, vjust = -20, size = 14),
        plot.caption = element_text(color = "#BEBEBE", face = "italic")) +
ggsave(here('Desktop', 'word_cloud.png'), dpi = 300, bg = "transparent",
       width = 11.07, height = 7.58)


stop_words_new <- tibble(word = c("more", "but", "not", "out", "than", "as", "ao", "auto", "seems", "is", "and", "that", "now", "from", "on", "of", "in", "or", "we", "be", "have", "they", "was", "good", "all", "over", "with", "a", "has", "our", "only", "for", "the", "to", "it", "are", "used", "able", "area", "getting", "area"))
nms <- c("demo_changes")
demo_chngs <- ao %>%
  select(66) %>%
  setNames(nms) %>%
  filter(!is.na(demo_changes)) %>%
  unnest_tokens(word, demo_changes) %>%
  count(word, sort = TRUE) %>% 
  # filter(word == "younger")
  anti_join(stop_words_new) %>%
  mutate(word = ifelse(str_detect(word, "young"), "younger", word)) %>%
  mutate(word = ifelse(str_detect(word, "old"), "older", word)) %>%
  arrange(desc(n)) %>%
  filter(n >= 5) %>%
  group_by(word) %>%
  summarise(n = sum(n)) %>%
  arrange(desc(n))

ggplot(demo_chngs, aes(label = word, size = n, color = n)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 24) +
  theme_minimal() +
  scale_color_gradient(low = "darkred", high = "red") +
  labs(title = "Describe the Demographic Changes in Personal Customers in the Past 3-5 Years",
       subtitle = "Most Frequently Used Words") +
  theme(plot.title = element_text(hjust = .5, vjust = -35, size = 17),
        axis.title.x = element_text(hjust = .5, face = "italic", size = 8),
        axis.title.y = element_text(hjust = .5, face = 'italic', size = 8),
        plot.subtitle = element_text(hjust = 0.5, vjust = -35, size = 14),
        plot.caption = element_text(color = "#BEBEBE", face = "italic"))
  ggsave(here('Desktop', 'word_cloud_demo_changes.png'), dpi = 300, bg = "transparent",
         width = 11.07, height = 7.58)


 ###############Ashleigh questions, ROUND 2 segmentations##########################################################################################################

answer_pull(11)
ao_age <- ao %>% 
  mutate("what_is_your_age" = ifelse(str_detect(.[[11]], "20") | str_detect(.[[11]], "30"), 
                                     "Between 20 and 40", .[[11]]))
ao_age %>%
  select(11)

answer_pull(11)

try_crossTab <- possibly(crossTab, otherwise = NULL)

fct <- c("Between 20 and 40","Between 40 and 50",  "Between 50 and 60", "60+")
single_pull <- c(13, 15:20, 21, 30)

for (i in single_pull) {
  try_crossTab(ao_age, 11, i, relevel = fct)
}

#####################################
# 35-40 (lead generation)
# 35 - cold calling 
# 36 - paid advertising
# 37 - referrals
# 38 - partners
# 39 - community involvement
nms <- c("Cold Calling", "Paid Advertising", "Referrals", "Partners", "Community Involvement", "Other")
ao %>% 
  select(c(35:40)) %>%
  setNames(nms) %>%
  gather(priority, rank) %>%
  filter(!is.na(rank)) %>% 
  count(priority, rank) %>%
  filter(rank <= 3) %>%
  mutate(filt = case_when(rank == 1 ~ "Priority #1",
                          rank == 2 ~ "Priority #2",
                          rank == 3 ~ "Priority #3")) %>% 
  ggplot(aes(x = priority, y = n)) + 
  geom_bar(stat = "identity", fill = "#154577") +
  facet_wrap(~filt) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "Count", 
       title = "What is your priority for lead generation",
       caption = "673 Respondents answering")
ggsave(here('figures', 'agent_lead_gen.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

# 42 - 51 - Marketing Tactics
nms <- c("Age", "Billboards", "Local TV", "Radio", "Print News", 
         "Direct Mail", "Digital Ads", "SEO", "Organic Social", "Community Involvement",
         "Other")
fct <- c("Between 20 and 40", "Between 40 and 50", "Between 50 and 60", "60+")

ao_mark_tactics_age <- ao_age %>% 
  select(c(11, 42:51)) %>%
  setNames(nms) %>%
  gather(priority, rank, 2:11) %>%
  filter(!is.na(rank)) %>% 
  count(Age, priority, rank) %>%
  group_by(Age, rank) %>%
  mutate(frac = n/sum(n)) %>%
  ungroup() %>%
  mutate(Age = factor(Age, levels = fct)) %>%
  filter(rank <= 5)
  
ggplot(ao_mark_tactics_age, aes(x = factor(rank), y = frac, group = Age, fill = Age)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~priority, scales = "free_x") + 
  theme_minimal() +
  hrbrthemes::scale_y_percent() +
  labs(x = "", y = "Count", 
       title = "Importance of various marketing tactics",
       caption = paste0(sum(source_ao_age[source_ao_age$rank ==1, ]$n))) + 
  theme(legend.position = "bottom")
ggsave(here('Desktop', 'age_graphs', 'plot_42_51_agent_marketing_.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

# 53 - 56 - new client profile THIS ONE WORKS
nms <- c("Age", "Direct to Consumer", "Captive Insurance", "Independent Agent", 
         "No Insurance")
fct <- c("Between 20 and 40", "Between 40 and 50", "Between 50 and 60", "60+")

source_ao_age <- ao_age %>% 
  select(c(11, 53:56)) %>%
  setNames(nms) %>%
  gather(priority, rank, 2:5) %>%
  filter(!is.na(rank)) %>% 
  count(Age, priority, rank) %>%
  group_by(Age) %>%
  mutate(frac = n/sum(n)) %>%
  ungroup() %>%
  mutate(Age = factor(Age, levels = fct)) %>%
  mutate(filt = case_when(rank == 1 ~ "Source of New Client #1",
                          rank == 2 ~ "Source of New Client #2",
                          rank == 3 ~ "Source of New Client #3",
                          rank == 4 ~ "Source of New Client #4"))

ggplot(source_ao_age, aes(x = priority, y = frac, group = Age, fill = Age)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~filt) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  hrbrthemes::scale_y_percent() +
  labs(x = "", y = "Count", 
       title = "What is the source of your new clients",
       caption = paste0(sum(source_ao_age[source_ao_age$rank ==1, ]$n), " Respondents answering"))
ggsave(here("Desktop", 'age_graphs', 'plot_53_56_agent_new_clients.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

#57-62 THIS ONE WORKS
names(ao[57:62])
nms <- c("Age", "Negative Claims Experience", "Seek Better Coverage", 
         "More Affordable Coverage", "Negative Exp. Previous Agent",
         'Need Help Understanding Coverage', "Other")
ao_age_tmp <- ao_age %>% 
  select(c(11, 57:62)) %>%
  setNames(nms) %>%
  gather(priority, rank, 2:7) %>%
  filter(!is.na(rank)) %>% 
  count(Age, priority, rank) %>%
  group_by(Age) %>%
  mutate(frac = n/sum(n))

  # filter(Age == "Between 20 and 40") %>%
  ggplot(ao_age_tmp, aes(x = factor(rank), y = frac, group = Age, fill = Age)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~priority) + 
  theme_minimal() +
  hrbrthemes::scale_y_percent() +
  labs(x = "", y = "Count", 
       title = "Why do new clients come to you for insurance",
       caption = paste0(sum(ao_age_tmp[ao_age_tmp$rank ==1, ]$n), " Respondents answering"))
  ggsave(here("Desktop", 'age_graphs', 'plot_57_62_why_new_clients.png'), dpi = 300, bg = "transparent",
         width = 7.07, height = 6.58)

#67
names(ao[67])
nms <- c("Age", "Annual_Review_Policy_for_Personal")
fct <- c("Between 20 and 40", "Between 40 and 50", "Between 50 and 60", "60+")

ao_age_review <- ao_age %>%
  select(c(11, 67)) %>%
  setNames(nms) %>%
  gather(priority, rank, 2) %>%
  filter(!is.na(rank)) %>% 
  count(Age, priority, rank) %>%
  group_by(Age) %>%
  mutate(frac = n/sum(n)) %>%
  ungroup() %>%
  mutate(Age = factor(Age, levels = fct)) %>%
  mutate(rank = ifelse(str_count(rank, "\\S+") > 10, 
                        str_replace_all(rank, word(rank, str_count(rank, "\\S+")/2 +1), 
                                        paste0("\n", word(rank, str_count(rank, "\\S+")/2 + 1))), rank))

ggplot(ao_age_review, aes(x = factor(rank), y = frac, group = Age, fill = Age)) + 
  geom_bar(stat = "identity", position = "dodge") +
  # facet_wrap(~priority) + 
  theme_minimal() +
  hrbrthemes::scale_y_percent() +
  coord_flip() +
  # theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  labs(x = "", y = "Count", 
       title = "What does annual review process look like for Personal Policies",
       caption = paste0(sum(ao_age_review[ao_age_review$rank ==1, ]$n), " Respondents answering"))
ggsave(here("Desktop", 'age_graphs', 'plot_67_review_process_personal.png'), dpi = 300, bg = "transparent",
       width = 15.07, height = 15.58)

#69
nms <- c("Age", "Annual_Review_Policy_for_Commercial")

ao_age_review <- ao_age %>%
  select(c(11, 69)) %>%
  setNames(nms) %>%
  gather(priority, rank, 2) %>%
  filter(!is.na(rank)) %>% 
  count(Age, priority, rank) %>%
  group_by(Age) %>%
  mutate(frac = n/sum(n)) %>%
  ungroup() %>%
  mutate(Age = factor(Age, levels = fct)) %>%
  mutate(rank = ifelse(str_count(rank, "\\S+") > 10, 
                       str_replace_all(rank, word(rank, str_count(rank, "\\S+")/2 +1), 
                                       paste0("\n", word(rank, str_count(rank, "\\S+")/2 + 1))), rank))

ggplot(ao_age_review, aes(x = factor(rank), y = frac, group = Age, fill = Age)) + 
  geom_bar(stat = "identity", position = "dodge") +
  # facet_wrap(~priority) + 
  theme_minimal() +
  hrbrthemes::scale_y_percent() +
  coord_flip() +
  # theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  labs(x = "", y = "Count", 
       title = "What does annual review process look like for Personal Policies",
       caption = paste0(sum(ao_age_review[ao_age_review$rank ==1, ]$n), " Respondents answering"))
ggsave(here("Desktop", 'age_graphs', 'plot_69_review_process_personal.png'), dpi = 300, bg = "transparent",
       width = 15.07, height = 15.58)

#72-75
names(ao[72:75])
nms <- c("Age", "Printed Newsletter", "Electronic Newsletter", 
         "Automated Emails", "None")
fct <- c("Between 20 and 40", "Between 40 and 50", "Between 50 and 60", "60+")

ao_age_education <- ao_age %>% 
  select(c(11, 72:75)) %>%
  setNames(nms) %>%
  gather(priority, rank, 2:5) %>%
  filter(!is.na(rank)) %>% 
  count(Age, priority, rank) %>%
  group_by(Age) %>%
  mutate(frac = n/sum(n)) %>%
  ungroup() %>%
  mutate(Age = factor(Age, levels = fct))

# filter(Age == "Between 20 and 40") %>%
ggplot(ao_age_education, aes(x = factor(rank), y = frac, group = Age, fill = Age)) + 
  geom_bar(stat = "identity", position = "dodge") +
  # facet_wrap(~priority) + 
  theme_minimal() +
  hrbrthemes::scale_y_percent() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  labs(x = "", y = "Count", 
       title = "What educational materials do you distribute to current clients",
       caption = paste0(sum(ao_age_tmp[ao_age_tmp$rank ==1, ]$n), " Respondents answering"))
ggsave(here("Desktop", 'age_graphs', 'plot_72_75_educat_materials.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

# 35-40 (lead generation)
# 35 - cold calling 
# 36 - paid advertising
# 37 - referrals
# 38 - partners
# 39 - community involvement
nms <- c('Age',"Cold Calling", "Paid Advertising", "Referrals", "Partners", "Community Involvement", "Other")
fct <- c("Between 20 and 40", "Between 40 and 50", "Between 50 and 60", "60+")

ao_age_lead_generation <- ao_age %>% 
  select(c(11, 35:40)) %>%
  setNames(nms) %>%
  gather(priority, rank, 2:7) %>%
  filter(!is.na(rank)) %>% 
  count(Age, priority, rank) %>%
  group_by(Age) %>%
  filter(rank <= 3) %>%
  mutate(filt = case_when(rank == 1 ~ "Priority #1",
                          rank == 2 ~ "Priority #2",
                          rank == 3 ~ "Priority #3")) %>%
  mutate(frac = n/sum(n)) %>%
  ungroup() %>%
  mutate(Age = factor(Age, levels = fct))

ggplot(ao_age_lead_generation, aes(x = factor(rank), y = frac, group = Age, fill = Age)) + 
  geom_bar(stat = "identity", position = "dodge") +
  # facet_wrap(~priority) + 
  theme_minimal() +
  hrbrthemes::scale_y_percent() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  labs(x = "", y = "Count", 
       title = "What educational materials do you distribute to current clients",
       caption = paste0(sum(ao_age_tmp[ao_age_tmp$rank ==1, ]$n), " Respondents answering"))
ggsave(here("Desktop", 'age_graphs', 'plot_35_40_lead_generation.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

#32
nms <- c("Age", "Marketing_Budget")
segments <- c("$0-4,999", "$5,000-9,999", "$10,000-24,999", "$25,000-49,999", "$50,000-99,999", "$100,000-199,999", "$200,000+")
fct <- c("Between 20 and 40", "Between 40 and 50", "Between 50 and 60", "60+")

ao_age_marketin_budget_rows <- ao_age %>%
  select(11, 32) %>%
  setNames(nms) %>%
  filter(!is.na(Age)) %>%
  nrow()

ao_age_marketing_budget <- ao_age %>%
  select(11, 32) %>%
  setNames(nms) %>%
  mutate(Marketing_Budget = as.numeric(Marketing_Budget)) %>%
  filter(!is.na(rank)) %>%
  mutate(x = cut(Marketing_Budget, breaks=c(0, 4999, 9999, 24999, 49999, 99999, 199999, Inf), labels = segments)) %>%
  group_by(x) %>%
  # summarise(total = n()) %>%
  filter(!is.na(rank)) %>%
  filter(!is.na(x)) %>% 
  count(Age, x) %>%
  group_by(Age) %>%
  mutate(frac = n/sum(n)) %>%
  ungroup() %>%
  mutate(Age = factor(Age, levels = fct))
  

ggplot(ao_age_marketing_budget, aes(x = x, y = frac, label = scales::percent(round(frac, 4)), group = Age, fill = Age)) + 
  geom_bar(stat='identity', position = "dodge") + 
  hrbrthemes::scale_y_percent(limits=c(0, .55)) +
  coord_flip() +
  # geom_text(nudge_y = .03) +
  theme_minimal() +
  labs(x = "", y = "",
       title = "Marketing Budget",
       caption = paste0(sum(ao_age_marketin_budget_rows), " Respondents answering"))
ggsave(here('Desktop', 'plot_32_marketing_budget.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

#33
nms <- c("Age", "Co_Op_From_AO")
segments <- c("$0-4,999", "$5,000-9,999", "$10,000-24,999", "$25,000-49,999", "$50,000-99,999", "$100,000-199,999", "$200,000+")
fct <- c("Between 20 and 40", "Between 40 and 50", "Between 50 and 60", "60+")

ao_age_coop_ao_rows <- ao_age %>%
  select(11, 33) %>%
  setNames(nms) %>%
  filter(!is.na(Co_Op_From_AO)) %>%
  nrow()

ao_age_coop_ao <- ao_age %>%
  select(11, 33) %>%
  setNames(nms) %>%
  mutate(Co_Op_From_AO = as.numeric(Co_Op_From_AO)) %>%
  filter(!is.na(rank)) %>%
  mutate(x = cut(Co_Op_From_AO, breaks=c(0, 4999, 9999, 24999, 49999, 99999, 199999, Inf), labels = segments)) %>%
  group_by(x) %>%
  # summarise(total = n()) %>%
  filter(!is.na(rank)) %>%
  filter(!is.na(x)) %>% 
  count(Age, x) %>%
  group_by(Age) %>%
  mutate(frac = n/sum(n)) %>%
  ungroup() %>%
  mutate(Age = factor(Age, levels = fct))


ggplot(ao_age_marketing_budget, aes(x = x, y = frac, label = scales::percent(round(frac, 4)), group = Age, fill = Age)) + 
  geom_bar(stat='identity', position = "dodge") + 
  hrbrthemes::scale_y_percent(limits=c(0, .55)) +
  coord_flip() +
  # geom_text(nudge_y = .03) +
  theme_minimal() +
  labs(x = "", y = "",
       title = "Co-Op Dollars From AO",
       caption = paste0(ao_age_coop_ao_rows," ", "Respondents answering"))
ggsave(here('Desktop', 'plot_33_coop_ao.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

#34
nms <- c("Age", "Co_Op_From_Other")
segments <- c("$0-4,999", "$5,000-9,999", "$10,000-24,999", "$25,000-49,999", "$50,000-99,999", "$100,000-199,999", "$200,000+")
fct <- c("Between 20 and 40", "Between 40 and 50", "Between 50 and 60", "60+")

ao_age_coop_other_rows <- ao_age %>%
  select(11, 34) %>%
  setNames(nms) %>%
  filter(!is.na(Co_Op_From_Other)) %>%
  nrow()

ao_age_coop_other <- ao_age %>%
  select(11, 34) %>%
  setNames(nms) %>%
  mutate(Co_Op_From_Other = as.numeric(Co_Op_From_Other)) %>%
  filter(!is.na(rank)) %>%
  mutate(x = cut(Co_Op_From_Other, breaks=c(0, 4999, 9999, 24999, 49999, 99999, 199999, Inf), labels = segments)) %>%
  group_by(x) %>%
  # summarise(total = n()) %>%
  filter(!is.na(rank)) %>%
  filter(!is.na(x)) %>% 
  count(Age, x) %>%
  group_by(Age) %>%
  mutate(frac = n/sum(n)) %>%
  ungroup() %>%
  mutate(Age = factor(Age, levels = fct))


ggplot(ao_age_coop_other, aes(x = x, y = frac, label = scales::percent(round(frac, 4)), group = Age, fill = Age)) + 
  geom_bar(stat='identity', position = "dodge") + 
  hrbrthemes::scale_y_percent(limits=c(0, .55)) +
  coord_flip() +
  # geom_text(nudge_y = .03) +
  theme_minimal() +
  labs(x = "", y = "",
       title = "Co-Op Dollars From Other",
       caption = paste0(ao_age_coop_other_rows, " ", "Respondents answering"))
ggsave(here('Desktop', 'plot_34_coop_other.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

#64
nms <- c("Age", "Use_compare_rater")
fct <- c("Between 20 and 40", "Between 40 and 50", "Between 50 and 60", "60+")

ao_age_compare_rater_rows <- ao_age %>%
  select(11, 64) %>%
  setNames(nms) %>%
  filter(!is.na(Age)) %>%
  nrow()

ao_age_compare_rater <- ao_age %>%
  select(c(11, 64))%>%
  setNames(nms) %>%
  gather(priority, rank, 2) %>%
  filter(!is.na(rank)) %>% 
  count(Age, priority, rank) %>%
  group_by(Age) %>%
  mutate(frac = n/sum(n)) %>%
  ungroup() %>%
  mutate(Age = factor(Age, levels = fct)) %>%
  mutate(rank = ifelse(str_count(rank, "\\S+") > 10, 
                       str_replace_all(rank, word(rank, str_count(rank, "\\S+")/2 +1), 
                                       paste0("\n", word(rank, str_count(rank, "\\S+")/2 + 1))), rank))

# filter(Age == "Between 20 and 40") %>%
ggplot(ao_age_compare_rater, aes(x = factor(rank), y = frac, group = Age, fill = Age)) + 
  geom_bar(stat = "identity", position = "dodge") +
  # facet_wrap(~priority) + 
  theme_minimal() +
  hrbrthemes::scale_y_percent() +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  labs(x = "", y = "Count", 
       title = "Do you use a Comparative Rater",
       caption = paste0(ao_age_compare_rater_rows, " Respondents answering"))
ggsave(here("Desktop", 'age_graphs', 'plot_64_comparative_rater.png'), dpi = 300, bg = "transparent",
       width = 9.07, height = 6.58)

##### end of segmenting by age ###########

#20 carriers used
names(ao[col])
answer_pull(col)
cat <- c("1-2","3-5",  "6-10", "More than 10")
colPull(col, 'pct',relevel = cat)
ggsave(here('figures', 'agent_carriers_used.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

# Education by various things #

ao_region %>%
  select(c(13, 72:76)) %>%
  rename(region = names(.[1]), print_newsletter = names(.[2]),
         e_newsletter = names(.[3]), auto_email = names(.[4]),
         none = names(.[5]), other = names(.[6])) %>%
  mutate_at(vars(print_newsletter:other), list(~ifelse(is.na(.), 0, 1))) %>%
  gather(communication, count, 2:6) %>%
  # group_by(communication) %>% summarise(n = sum(count))
  group_by(region, communication) %>%
  summarise(tot = sum(count)) %>%
  filter(!is.na(region)) %>%
  group_by(region) %>%
  mutate(frac = tot/sum(tot)) %>%
  group_by(region) %>%
  # summarise(total = sum(tot)) %>%
  ggplot(aes(x = region, y = frac, group = communication, col = communication, label = scales::percent(frac))) +
  geom_point(size = 2.5) +
  ggrepel::geom_label_repel() +
  coord_flip() +
  scale_color_manual(values = pal) +
  theme_minimal() +
  hrbrthemes::scale_y_percent() +
  labs(x = "", y = "",
       title = title,
       subtitle = "Segmented by Region")
ggsave(here("Desktop", 'plot_18_region_by_education.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)


#how lead gen by marketing budget
nms <- c("Budget", "Cold_Calling", "Paid_Ads", "Referrals", "Partners", "Community_Involvement", "Other")
segments <- c("$0-4,999", "$5,000-9,999", "$10,000-19,999", "$20,000-29,999", "30,000-39,999", "$40,000-49,999", "$50,000-59,999", "$60,000-69,999", "$70,000-79,999", "$80,000-89,999", "$90,000-99,999", "$100,000-199,999", "$200,000+")

rows <- ao %>%
  select(c(32, 35:40)) %>%
  setNames(nms) %>%
  filter(!is.na(Budget)) %>%
  nrow

ao %>%
  select(c(32, 35:40)) %>%
  setNames(nms) %>%
  gather(priority, rank, 2:7) %>%
  mutate(Budget = as.numeric(Budget)) %>%
  # mutate(x = cut(Budget, breaks=c(0, 4999, 9999, 19999, 29999, 39999, 499999, 599999, 699999, 799999, 899999, 999999, 199999, Inf), labels = segments)) %>%
  mutate(less20 = ifelse(Budget < 20000, 1, 0)) %>%
  filter(!is.na(rank)) %>%
  filter(!is.na(Budget)) %>% 
  # filter(!is.na(x)) %>%
  group_by(less20) %>%
  count(less20, priority, rank) %>%
  filter(rank <= 3) %>%
  mutate(filt = case_when(rank == 1 ~ "Priority #1",
                          rank == 2 ~ "Priority #2",
                          rank == 3 ~ "Priority #3")) %>%
  filter(rank == 1) %>%
  group_by(less20) %>%
  mutate(frac = n/sum(n), sum(n)) %>%
  filter(priority %in% c("Partners", "Referrals")) %>%
  ungroup() %>%
  mutate(col = c("< $20k","< $20k", "> $20k", "> $20k")) %>%
  ggplot(aes(x = priority, y = frac, group = col, fill = factor(col))) + 
  geom_bar(stat = 'identity', position = "dodge") +
  scale_fill_manual(values = pal, name = "Marketing Budget") +
  theme_minimal() +
  labs(x = "Lead Generation Type", y = "Percent", 
     title = "Top Priority for Lead Generation Given Marketing Budget",
     caption = paste0(rows, " Respondents answering"))
ggsave(here("Desktop", 'plot_32_by_35:40_lead_gen_by_mark_budget.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

# Comparative rater by carriers used and carrier relationship
crossTab(20, 64) #sum the yes's'. yes all, yes some, no

nms <- c("How_Many_Carriers", "Comparative_Rater")
fct <- c("No", "Yes Some", "Yes All")

rows <- ao %>%
  select(c(20, 64)) %>%
  setNames(nms) %>%
  filter(!is.na(Comparative_Rater)) %>%
  nrow

colPull(20)

tmp <- ao %>%
  select(c(20, 64)) %>%
  setNames(nms) %>%
  gather(priority, rank, 2) %>%
  filter(!is.na(rank)) %>% 
  count(How_Many_Carriers, priority, rank) %>%
  filter(rank != "Only if I already have a good sense of what the best option might be") %>%
  mutate(rank = ifelse(str_detect(rank, "short list"), 
                                     "Yes, some", rank)) %>%
  mutate(rank = ifelse(str_detect(rank, "our carriers"), 
                       "Yes, all", rank)) %>%
  group_by(How_Many_Carriers) %>%
  mutate(frac = n/sum(n))

  ggplot(tmp, aes(x = How_Many_Carriers, y = frac, group = rank, fill = rank)) + 
  geom_bar(stat = "identity", position = "dodge") +
  # facet_wrap(~priority) + 
  theme_minimal() +
  hrbrthemes::scale_y_percent() +
  coord_flip() +
  scale_fill_manual(values = pal, name = "Comparative Rater?") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  labs(x = "", y = "Count", 
       title = "How Many Carriers Do You Routinely Place Business With",
       caption = paste0(rows, " Respondents answering"))
ggsave(here("Desktop", 'plot_64_by_20_comparative_rater_by_carriers.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

# Comparative rater by carriers used and carrier relationship
crossTab(21, 64) #sum the yes's'. yes all, yes some, no

nms <- c("How_Place_Carriers", "Comparative_Rater")
fct <- c("No", "Yes Some", "Yes All")

rows <- ao %>%
  select(c(21, 64)) %>%
  setNames(nms) %>%
  filter(!is.na(Comparative_Rater)) %>%
  nrow

tmp <- ao %>%
  select(c(21, 64)) %>%
  setNames(nms) %>%
  gather(priority, rank, 2) %>%
  filter(!is.na(rank)) %>% 
  count(How_Place_Carriers, priority, rank) %>%
  filter(rank != "Only if I already have a good sense of what the best option might be") %>%
  mutate(rank = ifelse(str_detect(rank, "short list"), 
                       "Yes, some", rank)) %>%
  mutate(rank = ifelse(str_detect(rank, "our carriers"), 
                       "Yes, all", rank)) %>%
  group_by(How_Place_Carriers) %>%
  mutate(frac = n/sum(n))

ggplot(tmp, aes(x = How_Place_Carriers, y = frac, group = rank, fill = rank)) + 
  geom_bar(stat = "identity", position = "dodge") +
  # facet_wrap(~priority) + 
  theme_minimal() +
  hrbrthemes::scale_y_percent() +
  coord_flip() +
  scale_fill_manual(values = pal, name = "Comparative Rater?") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  labs(x = "", y = "Count", 
       title = "How Do You Regularly Place Business",
       caption = paste0(rows, " Respondents answering"))
ggsave(here("Desktop", 'plot_64_by_21_comparative_rater_by_carriers.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

##3
nums <- seq(1, 79, 1)


my_func <- function(num) {
ao %>%
  select(num) %>%
  filter(!is.na(ao[num])) %>%
  nrow()
}

my_func(66)

#of all 1-5, where they located?
crossTab(15, 13)  
ggsave(here("Desktop", 'plot_15_by_13.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

colPull(13)
crossTab(15, 13)

q <- ao %>% 
  select(question) %>%
  names(.) %>%
  str_to_sentence(.) %>%
  str_replace_all(., "_", " ") %>%
  enframe(name = NULL) %>%
  mutate(value = ifelse(str_count(value, "\\S+") > 10, 
                        str_replace_all(value, word(value, str_count(value, "\\S+")/2 +1), 
                                        paste0("\n", word(value, str_count(value, "\\S+")/2 + 1))), value)) %>%
  pull(value)

seg <- ao %>% 
  select(13) %>%
  names(.) %>%
  str_to_sentence(.) %>%
  str_replace_all(., "_", " ") %>%
  enframe(name = NULL) %>%
  mutate(value = ifelse(str_count(value, "\\S+") > 10, 
                        str_replace_all(value, word(value, str_count(value, "\\S+")/2 +1), 
                                        paste0("\n", word(value, str_count(value, "\\S+")/2 + 1))), value)) %>%
  pull(value)

if (is.null(relevel)) {
  ct_tmp <- ao %>%
    select(c(13, 15)) %>%
    rename(q1 = names(.[1]), q2 = names(.[2])) %>%
    filter(q2 == "1-5 years") %>%
    count(q1, q2) %>%
    filter(!is.na(q1)) %>%
    filter(!is.na(q2)) %>%
    group_by(q1) %>%
    mutate(frac = n/sum(32))
    
} 
}
print(data.frame(ct_tmp))
pal <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#a65628','#f781bf','#999999')

ggplot(ct_tmp, aes(x = q1, y = frac, label = scales::percent(round(frac, 4)))) + 
  geom_point(size = 3, col = "#154577") + 
  hrbrthemes::scale_y_percent() +
  coord_flip() + 
  ggrepel::geom_label_repel() + 
  scale_color_manual(values = pal) + 
  labs(x = "", y="", title = "Region Breakdown of 1-5 Year Agencies",
       caption = paste0(sum(ct_tmp$n), " Respondents answering")) +
  theme_minimal()
ggsave(here("Desktop", 'plot_15_by_13_young_region.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

ao_legacy <- ao %>%
  mutate("how_long_has_your_agency_been_in_business" = ifelse(str_detect(.[[15]], "26-50") | str_detect(.[[15]], "More than 50"), 
                                                              "26+ years", .[[15]]))

ao1 <- ao %>% 
  select(c(15, 21)) %>%
  rename(metric = names(.[1]),
         carrier_relationship = names(.[2])) %>%
  mutate(metric = ifelse(str_detect(metric, "26-50") | str_detect(metric, "More than 50"), 
                                                              "Agency Age: > 25 years", "Agency Age: < 25 years")) %>%
  count(metric, carrier_relationship) %>%
  group_by(metric) %>% 
  mutate(frac = n/sum(n)) %>%
  filter(str_detect(carrier_relationship, "equally")) %>%
  ungroup() %>%
  mutate(col = c(1,0))
  
ao2 <- ao %>% 
  select(c(16, 21)) %>%
  rename(metric = names(.[1]),
         carrier_relationship = names(.[2])) %>%
  mutate(metric = ifelse(str_detect(metric, "1-5"), "Producers: 5 or Less", "Producers: 6 or More")) %>%
  count(metric, carrier_relationship) %>%
  group_by(metric) %>%
  mutate(frac = n/sum(n)) %>%
  filter(str_detect(carrier_relationship, "equally")) %>%
  filter(!is.na(metric)) %>%
  ungroup() %>%
  mutate(col = c(1,0))  

ao3 <- ao %>% 
  select(c(20, 21)) %>%
  rename(metric = names(.[1]),
         carrier_relationship = names(.[2])) %>%
  mutate(metric = ifelse(str_detect(metric, "6-10") | str_detect(metric, "More than 10"), "Preferred Carriers: 6 or More", "Preferred Carriers: 5 or Less")) %>%
  count(metric, carrier_relationship) %>%
  group_by(metric) %>%
  mutate(frac = n/sum(n)) %>%
  filter(str_detect(carrier_relationship, "equally")) %>%
  ungroup() %>%
  mutate(col = c(1,0))  

fct <- c("Agency Age: < 25 years", "Agency Age: > 25 years","Producers: 5 or Less", 
         "Producers: 6 or More", "Preferred Carriers: 5 or Less", "Preferred Carriers: 6 or More")

rbind(ao1, ao2, ao3) %>%
  mutate(metric = factor(metric, levels = fct)) %>%
  mutate(metric2 = c("Agency Age", "Agency Age", "Producer Count", "Producer Count", 
         "Preferred Carriers", "Preferred Carriers")) %>%
  mutate(metric = gsub(".*\\: ", "", metric)) %>%
  ggplot(aes(x = metric, y = frac, group = metric2, fill = factor(col))) + 
  geom_bar(stat = "identity", position = "dodge") +
  # facet_wrap(~metric2, scales = "free_x") +
  coord_flip() + 
  facet_grid(metric2 ~ ., scales = "free_y") +   
  theme_minimal() +
  hrbrthemes::scale_y_percent() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("#154577", "#ffdb58")) +
  labs(x = "Various Factors", y = "Percent Chance of Placing Business Equally", 
       title = "Factors Predicting Placing Business Equally",
       caption = "929 Respondents answering") +
  theme(plot.title = element_text(hjust = .5),
        strip.background =element_rect(fill="lightgray", color = NA),
        # strip.text = element_text(color = "white"),
        axis.title.x = element_text(hjust = .5, face = "italic", size = 8),
        axis.title.y = element_text(hjust = .5, face = 'italic', size = 8),
        plot.caption = element_text(color = "#BEBEBE", face = "italic")) +
  theme(legend.position="none")
ggsave(here("Desktop", 'place_business_equally.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)

aocol <- '#154577'

fct <- c("Grow from within current client base", "Grow new client base - both personal and commercial",
         "Grow new client base - focus on commercial", "Grow new client base - focus on personal",
         "Grow through purchase of other agency/agencies" ,
         "Other - Write In",
         "There are no plans to grow the agency at this time" )


tmp <- ao %>%
  select((30)) %>%
  rename(q = names(.)) %>%
  count(q) %>%
  # mutate(q = factor(q, levels = fct)) %>%
  mutate(q = ifelse(str_count(q, "\\S+") > 6,
                    str_replace_all(q, word(q, str_count(q, "\\S+")/2 + 1),
                                    paste0("\n", word(q, str_count(q, "\\S+")/2 + 1))), q)) %>%
  filter(!is.na(q)) %>%
  # mutate(q = factor(q, levels = relevel)) %>%
  mutate(frac = n/sum(n)) %>%
  arrange(desc(frac))

ggplot(tmp, aes(x = reorder(q, frac), y = frac, label = scales::percent(round(frac, 4)))) + 
  geom_bar(stat = "identity", fill = aocol) + 
  hrbrthemes::scale_y_percent() +
  coord_flip(ylim = c(0,.95)) + 
  geom_text(nudge_y = .065) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = .5),
        axis.title.x = element_text(hjust = .5, face = "italic", size = 8),
        axis.title.y = element_text(hjust = .5, face = 'italic', size = 8),
        plot.caption = element_text(color = "#BEBEBE", face = "italic")) +
  labs(x = "Growth Strategy", y="Percent of Respondents", title = "Goals for Agency Growth", 
       caption = "776 Respondents answering")
ggsave(here("Desktop", 'x.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)


colPull(65)
nme <- c("Demo_Changes")
ao %>%
  select(65) %>%
  setNames(nme) %>%
  filter(!is.na(Demo_Changes)) %>%
  count(Demo_Changes) %>%
  mutate(pct = n/sum(n)) %>%
  ggplot(aes(x="", y= pct, fill =  Demo_Changes)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(round(pct*100), "%")), position = position_stack(vjust = 0.5), size = 7, col = "white") +
  scale_fill_manual(values=c("#154577", "#BEBEBE"), name = "") +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size =15),
        legend.text = element_text(size=12)) +
  labs(x="", y="", title = "Percent of Agents that Have Seen Demographic \nChanges in Personal Line Customers in the Last 3-5 Years")
ggsave(here("Desktop", 'pie_demo_change.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)


## 11 x 65
nms <- c("Demographic_Changes", "Age")
fct <- c("Between 20 and 30", "Between 30 and 40", "Between 40 and 50", "Between 50 and 60", "60+")

tmp <- ao %>%
  select(c(65, 11)) %>%
  setNames(nms) %>%
  gather(priority, rank, 2) %>%
  filter(!is.na(rank)) %>% 
  filter(!is.na(Demographic_Changes)) %>%
  count(Demographic_Changes, priority, rank) %>%
  group_by(Demographic_Changes) %>%
  mutate(frac = n/sum(n)) %>%
  mutate(rank = factor(rank, levels = fct))

pal2 <- c("#deebf7", "#9ecae1", "#6baed6", "#2171b5", "#08306b")


ggplot(tmp, aes(x = Demographic_Changes, y = frac, group = rank, fill = rank)) + 
  geom_bar(stat = "identity", position = "dodge") +
  # facet_wrap(~priority) + 
  theme_minimal() +
  hrbrthemes::scale_y_percent() +
  # coord_flip() +
  scale_fill_manual(values = pal2, name = "Age") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  theme(plot.title = element_text(hjust = .5),
        axis.title.x = element_text(hjust = .5, face = "italic", size = 8),
        axis.title.y = element_text(hjust = .5, face = 'italic', size = 8),
        plot.caption = element_text(color = "#BEBEBE", face = "italic")) +
  labs(x = "", y = "Percent", 
       title = "Percent of Agents that Have Seen Demographic \nChanges in Personal Line Customers in the Last 3-5 Years",
       caption = paste0(rows, " Respondents answering"))
ggsave(here("Desktop", 'pie_demo_change_by_age.png'), dpi = 300, bg = "transparent",
       width = 7.07, height = 6.58)
