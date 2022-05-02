library(tidyverse)
library(magrittr)
library(readxl)

#### proposed inferential model
# % change in SRP signups ~ % turnover of merit staff + % turnover of temporary staff  + average tenure of staff + 
#   % of homes without a computer + % of homes without Internet + median household income + % change in door counts + 
#   % change in virtual circulation

setwd("/Users/alexisathens/Documents/UofU/5 - Spring 2022/Program Evaluation/SLCO Library")


#### clean data -------------------------------------------------------------------


#### DIGITAL DIVIDE DATA --------------------
## get census / digital divide info
census <- read_csv("ACS and Spatial Data/Census Demographics by Library.csv")

# create main data set
all <- census



#### SRP SIGNUP DATA -------------------------

## get SRP signup data (response variable)
# for 2019
srp19 <- read_xlsx("Library Data/2019 Shared Statistics Unprotected.xlsx", skip = 2)

srp19 %<>% 
  select(contains("Branch") | contains("# finishers"))

colnames(srp19) <- c("library", "pre19", "kid19", "teen19", "adult19")

srp19 %<>% 
  filter(!is.na(teen19)) %>% 
  filter(library != "Total")

srp19 %<>% 
  mutate(all19 = pre19 + kid19 + teen19 + adult19)

# for beginners
srp19b <- read_xlsx("Library Data/2019 Shared Statistics Unprotected.xlsx", skip = 2)

srp19b %<>% 
  select(contains("Branch") | contains("# begin"))

colnames(srp19b) <- c("library", "pre19b", "kid19b", "teen19b", "adult19b")

srp19b %<>% 
  filter(!is.na(teen19b)) %>% 
  filter(library != "Total")

srp19b %<>% 
  mutate(all19b = pre19b + kid19b + teen19b + adult19b)


# for 2021
srp21 <- read_xlsx("Library Data/2021 SRP Shared Unprotected.xlsx", skip = 2)

srp21 %<>% 
  select(contains("Branch") | contains("# Finishers"))

colnames(srp21) <- c("library", "pre21", "kid21", "teen21", "adult21")

srp21 %<>% 
  filter(!is.na(teen21)) %>% 
  filter(library != "Total")

srp21 %<>% 
  mutate(all21 = pre21 + kid21 + teen21 + adult21)

# for beginners
srp21b <- read_xlsx("Library Data/2021 SRP Shared Unprotected.xlsx", skip = 2)

srp21b %<>% 
  select(contains("Branch") | contains("# begin"))

colnames(srp21b) <- c("library", "pre21b", "kid21b", "teen21b", "adult21b")

srp21b %<>% 
  filter(!is.na(teen21b)) %>% 
  filter(library != "Total")

srp21b %<>% 
  mutate(all21b = pre21b + kid21b + teen21b + adult21b)


# get finishers
srp <- srp19 %>% left_join(srp21)
srp %<>% filter(library %in% census$lib_code)

srp %<>% 
  mutate(perc_pre = (pre21 - pre19) / pre19 * 100,
         perc_kid = (kid21 - kid19) / kid19 * 100,
         perc_teen = (teen21 - teen19) / teen19 * 100,
         perc_adult = (adult21 - adult19) / adult19 * 100,
         perc_all = (all21 - all19) / all19 * 100)

srp %<>%
  select(library, perc_pre:perc_all)


# get beginners
srpb <- srp19b %>% left_join(srp21b)

srpb %<>% filter(library %in% census$lib_code)

srpb %<>% 
  mutate(perc_pre_b = (pre21b - pre19b) / pre19b * 100,
         perc_kid_b = (kid21b - kid19b) / kid19b * 100,
         perc_teen_b = (teen21b - teen19b) / teen19b * 100,
         perc_adult_b = (adult21b - adult19b) / adult19b * 100,
         perc_all_b = (all21b - all19b) / all19b * 100)

srpb %<>%
  select(library, perc_pre_b:perc_all_b)


# join to main data frame
all %<>% 
  left_join(srp, by = c("lib_code" = "library"))

all %<>% 
  left_join(srpb, by = c("lib_code" = "library"))



#### STAFF SURVEY DATA -----------------------

## get staff survey data
staff <- read_csv("Library Data/Staff Turnover Survey Data.csv")

staff %<>% select(Q9:Q8_6_TEXT)

colnames(staff) <- c("library", "curr_emp", "curr_less2", "num_left", "turnover", "no_change", "exact", "sub_hours")

staff <- staff[3:19,]

staff %<>% select(-c(turnover, no_change, exact))

staff %<>% 
  mutate(curr_emp = as.numeric(curr_emp), curr_less2 = as.numeric(curr_less2), num_left = as.numeric(num_left), 
         sub_hours = as.numeric(sub_hours)) %>% 
  mutate(perc_less2 = curr_less2 / curr_emp * 100) %>% 
  mutate(perc_turnover = num_left / curr_emp * 100)


staff %<>% # get acronyms / final lib list from library
  mutate(lib_code = case_when(str_detect(library, "Binham") ~ "BCR", # typo
                              str_detect(library, "Draper") ~ "DRA",
                              str_detect(library, "Herriman") ~ "HER",
                              str_detect(library, "Hunter") ~ "HUN",
                              str_detect(library, "Magna") ~ "MAG",
                              str_detect(library, "Millcreek") ~ "MCC",
                              str_detect(library, "Riverton") ~ "RIV",
                              str_detect(library, "Sandy") ~ "SAN",
                              str_detect(library, "Smith") ~ "SMI",
                              str_detect(library, "South Jordan") ~ "SJO",
                              str_detect(library, "Taylorsville") ~ "TAY",
                              str_detect(library, "Tyler") ~ "TYL",
                              str_detect(library, "West Jordan") ~ "WJO",
                              str_detect(library, "West Valley") ~ "WVA",
                              str_detect(library, "Whitmore") ~ "WHI",
                              TRUE ~ NA_character_))

# join to main data frame
all %<>% 
  left_join(staff %>% select(-library), by = "lib_code")



#### DOOR COUNT DATA -------------------------

## get visitor counts
# for 2019
visits19 <- read_xlsx("Library Data/2019 Physical Visitors Unprotected.xlsx")

visits19 %<>% 
  rename(library = NameAbbreviation, visits19 = VisitorsWelcomed) %>% 
  group_by(library) %>% 
  summarize(visits19 = sum(visits19)) %>% 
  ungroup()

# for 2021
visits21 <- read_xlsx("Library Data/2021 Physical Visitors Unprotected.xlsx")

visits21 %<>% 
  rename(library = NameAbbreviation, visits21 = VisitorsWelcomed) %>% 
  group_by(library) %>% 
  summarize(visits21 = sum(visits21)) %>% 
  ungroup()

visits <- visits19 %>% 
  left_join(visits21)

visits %<>% 
  filter(!is.na(visits21)) %>% 
  mutate(perc_visits = (visits21 - visits19) / visits19 * 100)


# join to main data frame
all %<>% 
  left_join(visits, by = c("lib_code" = "library"))

# get average diff
all %>% 
  summarize(avg_visits = mean(perc_visits))



#### SCHOOL OUTREACH DATA ----------------------

## get outreach data
# for 2019
out19 <- read_xlsx("Library Data/2019 Outreach Unprotected.xlsx")

out19 %<>% 
  rename(library = NameAbbreviation) %>%
  group_by(library) %>%
  summarize(out19 = n()) %>%
  ungroup()

# for 2021
out21 <- read_xlsx("Library Data/2021 Outreach Unprotected.xlsx")

out21 %<>% 
  rename(library = NameAbbreviation) %>%
  group_by(library) %>%
  summarize(out21 = n()) %>%
  ungroup()

out <- out19 %>% 
  full_join(out21)

out %<>%
  mutate(out19 = ifelse(is.na(out19), 0, out19),
         out21 = ifelse(is.na(out21), 0, out21))

# add observation for missing lib
out %<>% bind_rows(tibble(library = "WHI", out19 = 0, out21 = 0))

out %<>%
  mutate(out_diff = out21 - out19)
# just do raw # change because of 0 denominator when calculating % change


# join to main data frame
all %<>% 
  left_join(out, by = c("lib_code" = "library"))



#### NEW SIGNUPS DATA --------------------------

## get new signups (for control var)
signups <- read_xlsx("Library Data/New Signups Unprotected.xlsx")
# have new signup data for 2019 and 2021 by month... how to calculate?

# want to control for increase in signups and how this affected program attendance in 2021
# perhaps look at # of signups in 2021 leading up to SRP (i.e., Jan - July)

signups %<>% 
  filter(str_detect(PatronCode, "registration")) %>%
  filter(Year == 2021 & Month <= 7) %>%
  rename(library = TransactionBranchName) %>%
  group_by(library) %>%
  summarize(signups = sum(Total)) %>%
  ungroup()

signups %<>% # get acronyms / final lib list from library
  mutate(lib_code = case_when(str_detect(library, "Bingham") ~ "BCR",
                              str_detect(library, "Draper") ~ "DRA",
                              str_detect(library, "Herriman") ~ "HER",
                              str_detect(library, "Hunter") ~ "HUN",
                              str_detect(library, "Magna") ~ "MAG",
                              str_detect(library, "Millcreek") ~ "MCC",
                              str_detect(library, "Riverton") ~ "RIV",
                              str_detect(library, "Sandy") ~ "SAN",
                              str_detect(library, "Smith") ~ "SMI",
                              str_detect(library, "South Jordan") ~ "SJO",
                              str_detect(library, "Taylorsville") ~ "TAY",
                              str_detect(library, "Tyler") ~ "TYL",
                              str_detect(library, "West Jordan") ~ "WJO",
                              str_detect(library, "West Valley") ~ "WVA",
                              str_detect(library, "Whitmore") ~ "WHI",
                              TRUE ~ NA_character_)) %>%
  select(-library)

# join to main data frame
all %<>% 
  left_join(signups, by = c("lib_code"))



#### CIRCULATION DATA ----------------------

circ <- read_xlsx("Library Data/2021 Circulation Unprotected.xlsx")

# look at circulation data over same period as signups:
# in 2021 leading up to SRP (i.e., Jan - July)

circ %<>% 
  rename(lib_code = Libraries) %>% 
  select(lib_code:July) %>% 
  mutate(circ_count = January + February + March + April + May + June + July) %>% 
  select(-c(January:July))

# join to main data frame
all %<>% 
  left_join(circ, by = c("lib_code"))

# calculate normalized signups
all %<>% 
  mutate(circ_count_round = round(circ_count / 1000)) %>% 
  mutate(signups_norm = signups / circ_count_round)

# write_csv(all, "Data by Branch.csv")


#### exploratory analysis -----------------------------------------------------

names(all)

all %>% 
  ggplot(aes(x = log(signups))) +
  geom_dotplot()

### digital divide variables
all %>% 
  ggplot(aes(x = med_income, y = perc_all, label = lib_code)) + # no relationship...
  geom_point() + geom_smooth(method = "lm", se = F) +
  geom_text(hjust = 1, vjust = 1)

all %>% 
  ggplot(aes(x = dig_div, y = perc_all, label = lib_code)) + # no_comp washed out by no_int
  geom_point() + geom_smooth(method = "lm", se = F) +
  geom_text(hjust = 1, vjust = 1)


## staff turnover variables
all %>% 
  ggplot(aes(x = sub_hours, y = perc_all)) +
  geom_point() + geom_smooth(method = "lm", se = F) # only seems to be significant because of outlier...

all %>% 
  ggplot(aes(x = perc_less2, y = perc_all)) +
  geom_point() + geom_smooth(method = "lm", se = F) # not intuitive... the higher percent new employees, the lower the decline

all %>% 
  ggplot(aes(x = perc_turnover, y = perc_all)) +
  geom_point() + geom_smooth(method = "lm", se = F)


## mobility variables
all %>% 
  ggplot(aes(x = perc_visits, y = perc_all, label = lib_code)) +
  geom_point() + geom_smooth(method = "lm", se = F) +
  geom_text(hjust = 1, vjust = 1)


## outreach variables
all %>% 
  ggplot(aes(x = out_diff, y = perc_all, label = lib_code)) +
  geom_point() + geom_smooth(method = "lm", se = F) +
  geom_text(hjust = 1, vjust = 1)
# the more outreach, the more participation decreased?


## signup variable
all %>% 
  ggplot(aes(x = signups, y = perc_all, label = lib_code)) +
  geom_point() + geom_smooth(method = "lm", se = F) +
  geom_text(hjust = 1, vjust = 1)
# the more new signups, the more participation decreased?

# adjust for normalized signup
all %>% 
  # filter(lib_code != "WVA") %>% 
  ggplot(aes(x = signups_norm, y = perc_all, label = lib_code)) +
  geom_point() + geom_smooth(method = "lm", se = F) +
  geom_text(hjust = 1, vjust = 1)
# WVA is a large outlier... but proceed


## response variable
all %>%
  ggplot(aes(x = lib_code, y = perc_visits)) +
  geom_col()

all %>% 
  ggplot(aes(x = perc_all, y = perc_all_b, label = lib_code)) + 
  geom_point() + geom_smooth(method = "lm", se = F) +
  geom_text(hjust = 1, vjust = 1)
# definetely a correlation between beginners and finishers

cor.test(all$perc_all, all$perc_all_b) # strong correlation of 0.77



### create graphic for report
library(gridExtra) # for plotting multiple graphs at once

## for signups
# turnover
p3 <- all %>% 
  ggplot(aes(x = perc_turnover, y = perc_all_b, label = lib_code)) +
  geom_point() + geom_smooth(method = "lm", se = F, color = "dark red") +
  geom_text(hjust = 1, vjust = 1) +
  labs(x = "% Staff Turnover", y = "% Change SRP Signups")

# dig div
p4 <- all %>% 
  ggplot(aes(x = no_comp, y = perc_all_b, label = lib_code)) +
  geom_point() + geom_smooth(method = "lm", se = F, color = "dark red") +
  geom_text(hjust = 1, vjust = 1) +
  labs(x = "% Without Computer", y = "% Change SRP Signups")

# mobility
p1 <- all %>% 
  ggplot(aes(x = perc_visits, y = perc_all_b, label = lib_code)) +
  geom_point() + geom_smooth(method = "lm", se = F, color = "dark red") +
  geom_text(hjust = 1, vjust = 1) +
  labs(x = "% Change Door Counts", y = "% Change SRP Signups")

# outreach
p2 <- all %>% 
  ggplot(aes(x = out_diff, y = perc_all_b, label = lib_code)) +
  geom_point() + geom_smooth(method = "lm", se = F, color = "dark red") +
  geom_text(hjust = 1, vjust = 1) +
  labs(x = "# Change Outreach", y = "% Change SRP Signups")

grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)


## for finishers
# turnover
p3 <- all %>% 
  ggplot(aes(x = perc_turnover, y = perc_all, label = lib_code)) +
  geom_point() + geom_smooth(method = "lm", se = F, color = "dark red") +
  geom_text(hjust = 1, vjust = 1) +
  labs(x = "% Staff Turnover", y = "% Change SRP Finishers")

# dig div
p4 <- all %>% 
  ggplot(aes(x = no_comp, y = perc_all, label = lib_code)) +
  geom_point() + geom_smooth(method = "lm", se = F, color = "dark red") +
  geom_text(hjust = 1, vjust = 1) +
  labs(x = "% Without Computer", y = "% Change SRP Finishers")

# mobility
p1 <- all %>% 
  ggplot(aes(x = perc_visits, y = perc_all, label = lib_code)) +
  geom_point() + geom_smooth(method = "lm", se = F, color = "dark red") +
  geom_text(hjust = 1, vjust = 1) +
  labs(x = "% Change Door Counts", y = "% Change SRP Finishers")

# outreach
p2 <- all %>% 
  ggplot(aes(x = out_diff, y = perc_all, label = lib_code)) +
  geom_point() + geom_smooth(method = "lm", se = F, color = "dark red") +
  geom_text(hjust = 1, vjust = 1) +
  labs(x = "# Change Outreach", y = "% Change SRP Finishers")

grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)




#### run models ----------------------------------------------------------------
library(lsr) # for effect size
# initial model
# % change in SRP signups ~ (% turnover of merit staff + % turnover of temporary staff  + average tenure of staff) + 
#   % of homes without a computer + % of homes without Internet + median household income + % change in door counts + 
#   % change in virtual circulation

get_table <- function(model){
  
  summ <- summary(model)
  
  mod <- tibble(vars = c("intercept", "perc_turnover", "no_comp", "perc_visits", "out_diff", "signups"))
  
  mod %<>% 
    bind_cols(as.data.frame(summ$coefficients)) %>% 
    bind_cols(`Effect Size` = c(NA, etaSquared(model)[, 2]))
  
  colnames(mod) <- c("vars", "estimate", "std error", "t value", "p value", "eff size")
  
  mod %<>% 
    mutate(`prac size` = case_when(
      `eff size` > 0.14 ~ "large",
      `eff size` > 0.06 ~ "moderate",
      `eff size` > 0.01 ~ "small",
      TRUE ~ ""
    ))
  
  return(mod)
}




### models by signups
# all signups
base <- lm(perc_all_b ~ perc_turnover + no_comp + perc_visits + out_diff + signups_norm, data = all)
# base <- lm(perc_all_b ~ perc_turnover + med_income + perc_visits + out_diff + signups_norm, data = all)
summary(base)
get_table(base)

# try % change in age group, change in finishers

# just finishers
fin <- lm(perc_all ~ perc_turnover + no_comp + perc_visits + out_diff + signups_norm, data = all)
# fin <- lm(perc_all ~ perc_turnover + med_income + perc_visits + out_diff + signups_norm, data = all)
summary(fin) # still no significance
get_table(fin)

# beginners by age group
pre <- lm(perc_pre_b ~ perc_turnover + no_comp + perc_visits + out_diff + signups_norm, data = all)
summary(pre)
get_table(pre)

kid <- lm(perc_kid_b ~ perc_turnover + no_comp + perc_visits + out_diff + signups_norm, data = all)
summary(kid)
get_table(kid)

teen <- lm(perc_teen_b ~ perc_turnover + no_comp + perc_visits + out_diff + signups_norm, data = all)
summary(teen) # difference not significant. interesting
get_table(teen)

adult <- lm(perc_adult_b ~ perc_turnover + no_comp + perc_visits + out_diff + signups_norm, data = all)
summary(adult)
get_table(adult)


# finishers by age group 

pre <- lm(perc_pre ~ perc_turnover + no_comp + perc_visits + out_diff + signups_norm, data = all)
summary(pre)
get_table(pre)

kid <- lm(perc_kid ~ perc_turnover + no_comp + perc_visits + out_diff + signups_norm, data = all)
summary(kid)
get_table(kid)

teen <- lm(perc_teen ~ perc_turnover + no_comp + perc_visits + out_diff + signups_norm, data = all)
summary(teen) # difference in outreach significant!! and almost signups
get_table(teen)

adult <- lm(perc_adult ~ perc_turnover + no_comp + perc_visits + out_diff + signups_norm, data = all)
summary(adult)
get_table(adult)


# visualize outreach diff - this looks reliable (not a result of an outlier)
all %>% 
  ggplot(aes(x = out_diff, y = perc_all, label = lib_code)) +
  geom_point() + geom_smooth(method = "lm", se = F) +
  geom_text(hjust = 1, vjust = 1)
