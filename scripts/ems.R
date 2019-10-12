# Load packages
library(tidyverse)
library(lubridate)

# Load data, transform, and apply universal filter
ems <- read_csv(here::here("data/callsWithJurisdictions.csv"),
                col_types = cols(case_id = col_character(),
                                 nature2 = col_character())) %>%
  filter(agency == "EMS") %>%
  mutate(nature = str_replace(nature, "Z", "")) %>%
  mutate(jurisdiction = case_when(
    is.na(jurisdiction) == TRUE ~ "Guilford County/Other",
    is.na(jurisdiction) != TRUE ~ jurisdiction)) %>% 
  filter(cancelled == FALSE) %>%
  filter(rptonly == FALSE) %>%
  filter(is.na(primeunit) == FALSE) %>%
  filter(is.na(priority) == FALSE) %>%
  filter(str_detect(closecode, "[:alpha:]") == FALSE) %>%
  filter(str_detect(primeunit, "MA") == FALSE)

population <- read_csv(here::here("data/population.csv"))

ems <- ems %>%
  mutate(fiscal_year = case_when(
    month(calltime) < 7 ~ year(calltime),
    month(calltime) > 6 ~ year(calltime) + 1
  ))

## Overall Average Response Time
overall_avg_resp <- ems %>%
  mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime) %>%
  summarise(avgresponse = round(mean(secs2ar), digits = 2)) %>%
  ungroup()

## SUBSET 1
tile.set.1 <- ems %>%
  mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime) %>%
  count() %>%
  rename(callcount = n)

## SUBSET 2
tile.set.2 <- ems %>% mutate(calltime = floor_date(calltime, unit = "1 month"))
tile.set.2$priority[tile.set.2$priority != "P" & tile.set.2$priority != "1" & tile.set.2$priority != "2"] <- "Other"
tile.set.2$priority[tile.set.2$priority == "P"] <- "Imminent Life Threat"
tile.set.2$priority[tile.set.2$priority == "1"] <- "Life Threat"
tile.set.2$priority[tile.set.2$priority == "2"] <- "Potential Life Threat"
tile.set.2 <- tile.set.2 %>%
  count(calltime, priority) %>%
  rename(callcount = n)

## SUBSET 3
tile.set.3 <- ems %>%
  mutate(weekday = wday(calltime, label = TRUE)) %>%
  mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  count(calltime, weekday) %>%
  rename(callcount = n)

## SUBSET 4
tile.set.4 <- ems %>% mutate(calltime = floor_date(calltime, unit = "1 month"))
tile.set.4$priority[tile.set.4$priority != "P" & tile.set.4$priority != "1" & tile.set.4$priority != "2"] <- "Other"
tile.set.4$priority[tile.set.4$priority == "P"] <- "Imminent Life Threat"
tile.set.4$priority[tile.set.4$priority == "1"] <- "Life Threat"
tile.set.4$priority[tile.set.4$priority == "2"] <- "Potential Life Threat"
tile.set.4 <- tile.set.4 %>%
  group_by(calltime, priority) %>%
  summarise(avgresponse = round(mean(secs2ar), digits = 2))

## SUBSET 5
tile.set.5 <- ems %>%
  mutate(fiscal_year = case_when(
    month(calltime) < 7 ~ year(calltime),
    month(calltime) > 6 ~ year(calltime) + 1
  )) %>%
  group_by(fiscal_year) %>%
  count()

## Calls Per Person
per_person <- tile.set.5 %>%
  left_join(population, by = c("fiscal_year" = "year")) %>%
  mutate(per_person = round(estimate/n, digits = 2))
  

## SUBSET 6
tile.set.6 <- ems %>%
  mutate(year = year(calltime), month = month(calltime, label = TRUE)) %>%
  group_by(year, month, nature) %>%
  count() %>%
  ungroup() %>%
  group_by(year, month) %>%
  top_n(5)

## SUBSET 7
tile.set.7 <- ems %>%
  mutate(weekday = wday(calltime, label = TRUE)) %>%
  mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime, weekday) %>%
  summarise(avgresponse = round(mean(secs2ar), digits = 2))

## SUBSET 8
tile.set.8 <- ems %>%
  filter(cancelled == FALSE & priority == "P" & secs2ar > 0) %>%
  mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime) %>%
  summarise(`90th Pctile` = quantile(secs2ar, probs = .90))
x2 <- ems %>%
  filter(priority == "P") %>%
  mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime) %>%
  summarise(`Overall Avg` = mean(secs2ar))
tile.set.8 <- left_join(tile.set.8, x2)
tile.set.8$`90th Pctile` <- round(tile.set.8$`90th Pctile`, digits = 2)
tile.set.8$`Overall Avg` <- round(tile.set.8$`Overall Avg`, digits = 2)
tile.set.8 <- tile.set.8 %>% gather("type", "response", 2:3)

## SUBSET 9
tile.set.9 <- ems %>%
  filter(priority == "1") %>%
  mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime) %>%
  summarise(`90th Pctile` = quantile(secs2ar, probs = .90))
x2 <- ems %>%
  filter(priority == "1") %>%
  mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime) %>%
  summarise(`Overall Avg` = mean(secs2ar))
tile.set.9 <- left_join(tile.set.9, x2)
tile.set.9$`90th Pctile` <- round(tile.set.9$`90th Pctile`, digits = 2)
tile.set.9$`Overall Avg` <- round(tile.set.9$`Overall Avg`, digits = 2)
tile.set.9 <- tile.set.9 %>% gather("type", "response", 2:3)

## SUBSET 10
tile.set.10 <- ems %>%
  filter(priority == "2") %>%
  mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime) %>%
  summarise(`90th Pctile` = quantile(secs2ar, probs = .90))
x2 <- ems %>%
  filter(priority == "2") %>%
  mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime) %>%
  summarise(`Overall Avg` = mean(secs2ar))
tile.set.10 <- left_join(tile.set.10, x2)
tile.set.10$`90th Pctile` <- round(tile.set.10$`90th Pctile`, digits = 2)
tile.set.10$`Overall Avg` <- round(tile.set.10$`Overall Avg`, digits = 2)
tile.set.10 <- tile.set.10 %>% gather("type", "response", 2:3)

## SUBSET 11
tile.set.11 <- ems %>%
  filter(priority == "P") %>%
  mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime) %>%
  count() %>%
  rename(totalcount = n)
x2 <- ems %>%
  filter(priority == "P") %>%
  filter(secs2ar < 540) %>%
  mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime) %>%
  count() %>%
  rename(countunder = n)
tile.set.11 <- left_join(tile.set.11, x2) %>% mutate(percentunder = round((countunder / totalcount) * 100, digits = 2))

## SUBSET 12
tile.set.12 <- ems %>%
  filter(priority == "1") %>%
  mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime) %>%
  count() %>%
  rename(totalcount = n)
x2 <- ems %>%
  filter(priority == "1") %>%
  filter(secs2ar < 660) %>%
  mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime) %>%
  count() %>%
  rename(countunder = n)
tile.set.12 <- left_join(tile.set.12, x2) %>% mutate(percentunder = round((countunder / totalcount) * 100, digits = 2))

## SUBSET 13
tile.set.13 <- ems %>%
  filter(priority == "2") %>%
  mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime) %>%
  count() %>%
  rename(totalcount = n)
x2 <- ems %>%
  filter(priority == "2") %>%
  filter(secs2ar < 780) %>%
  mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime) %>%
  count() %>%
  rename(countunder = n)
tile.set.13 <- left_join(tile.set.13, x2) %>% mutate(percentunder = round((countunder / totalcount) * 100, digits = 2))