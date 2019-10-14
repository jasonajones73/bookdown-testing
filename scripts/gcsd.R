# Load packages
library(tidyverse)
library(lubridate)

# Load data, transform, and apply universal filter
gcsd <- read_csv(here::here("data/callsWithJurisdictions.csv"),
                col_types = cols(case_id = col_character(),
                                 nature2 = col_character())) %>%
  filter(agency == "GCSD") %>%
  filter(service == "LAW") %>%
  filter(cancelled == FALSE) %>%
  filter(callsource != "SELF") %>%
  filter(nature != "BE ON THE LOOK OUT")

# Transform priority
gcsd$priority[is.na(gcsd$priority)] <- "None Assigned"
gcsd$priority[gcsd$priority != "P"] <- "All Other"
gcsd$priority[gcsd$priority == "P"] <- "Highest Priority"

population <- read_csv(here::here("data/population.csv"))

## Per Year
per_year <- gcsd %>%
  mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  mutate(fiscal_year = case_when(
    month(calltime) < 7 ~ year(calltime),
    month(calltime) > 6 ~ year(calltime) + 1
  )) %>%
  group_by(fiscal_year) %>%
  summarise(call_count = n())

## Per Thousand
per_person <- per_year %>%
  left_join(population, by = c("fiscal_year" = "year")) %>%
  mutate(per_person = round(call_count/(estimate/1000), digits = 2))

## Overall Average Response Time
overall_avg_resp <- gcsd %>%
  mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime) %>%
  summarise(avgresponse = round(mean(secs2ar), digits = 2)) %>%
  ungroup()

## SUBSET 1
tile.set.1 <- gcsd %>% mutate(calltime=floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime, nature) %>%
  count() %>%
  ungroup() %>%
  group_by(calltime) %>%
  top_n(5) %>%
  ungroup() %>%
  rename(callcount = n)

## SUBSET 2
tile.set.2 <- gcsd %>% mutate(calltime=floor_date(calltime, unit = "1 month"))
tile.set.2 <- tile.set.2 %>% group_by(calltime, priority) %>%
  count() %>%
  ungroup()

## SUBSET 3
tile.set.3 <- gcsd %>% mutate(calltime=floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime) %>%
  count() %>%
  ungroup()

## SUBSET 4
tile.set.4 <- gcsd %>% mutate(calltime=floor_date(calltime, unit = "1 month")) %>%
  filter(district=="SD1" | district=="SD2" | district=="SD3") %>%
  group_by(calltime, district) %>%
  count() %>%
  ungroup()

## SUBSET 5
tile.set.5 <- gcsd %>% filter(secs2ar > 0) %>% 
  filter(secs2ar < 90000000) %>%
  mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime) %>%
  summarise(avgresp = mean(secs2ar)) %>%
  ungroup()

tile.set.5$avgresp <- round(tile.set.5$avgresp, digits = 2)

## SUBSET 6
tile.set.6 <- gcsd %>% filter(secsdi2ar > 0) %>%
  mutate(calltime=floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime) %>%
  summarise(avgreact = mean(secsdi2ar)) %>%
  ungroup()

tile.set.6$avgreact <- round(tile.set.6$avgreact, digits = 2)

## SUBSET 7
tile.set.7 <- gcsd %>% filter(secs2ar < 90000000) %>%
  mutate(calltime=floor_date(calltime, unit = "1 month"))
tile.set.7 <- tile.set.7 %>% group_by(calltime, priority) %>%
  summarise(avgresp = mean(secs2ar)) %>%
  ungroup()
tile.set.7$avgresp <- round(tile.set.7$avgresp, digits = 2)

## SUBSET 8
tile.set.8 <- gcsd %>% mutate(calltime=floor_date(calltime, unit = "1 month"))
tile.set.8 <- tile.set.8 %>% group_by(calltime, priority) %>%
  summarise(avgreact = mean(secsdi2ar)) %>% ungroup()
tile.set.8$avgreact <- round(tile.set.8$avgreact, digits = 2)

## SUBSET 9
tile.set.9 <- gcsd %>% filter(secs2ar < 90000000) %>%
  filter(district == "SD1" | district=="SD2" | district=="SD3") %>%
  mutate(calltime=floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime, district) %>%
  summarise(avgresp=mean(secs2ar)) %>%
  ungroup()
tile.set.9$avgresp <- round(tile.set.9$avgresp, digits = 2)

## SUBSET 10
tile.set.10 = gcsd %>% filter(district == "SD1" | district=="SD2" | district=="SD3") %>%
  mutate(calltime=floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime, district) %>%
  summarise(avgreact = mean(secsdi2ar)) %>%
  ungroup()
tile.set.10$avgreact <- round(tile.set.10$avgreact, digits = 2)

## SUBSET 11
tile.set.11 = gcsd %>% filter(district == "SD1" | district=="SD2" | district=="SD3") %>%
  mutate(calltime=floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime, district, priority) %>%
  count()

## SUBSET 12
x = gcsd %>% mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  filter(priority == "Highest Priority") %>%
  filter(secsdi2ar < 240) %>%
  group_by(calltime) %>%
  count() %>%
  rename(under4 = n)

tile.set.12 = gcsd %>% mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  filter(priority == "Highest Priority") %>%
  group_by(calltime) %>%
  count() %>%
  rename(total = n) %>%
  left_join(x) %>%
  mutate(pct_under = round((under4/total)*100, digits = 2))

## SUBSET 13
x = gcsd %>% mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  filter(priority == "Highest Priority") %>%
  filter(secsdi2ar < 240) %>%
  group_by(calltime, district) %>%
  count() %>%
  rename(under4 = n)

tile.set.13 = gcsd %>% mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  filter(priority == "Highest Priority") %>%
  group_by(calltime, district) %>%
  count() %>%
  rename(total = n) %>%
  left_join(x) %>%
  mutate(pct_under = round((under4/total)*100, digits = 2)) %>%
  filter(district == "SD1" | district == "SD2" | district == "SD3")
tile.set.13$pct_under[is.na(tile.set.13$pct_under)] <- 0

## SUBSET 14
x1 = gcsd %>% mutate(hour = hour(calltime)) %>%
  mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime, hour) %>%
  count() %>%
  ungroup() %>%
  group_by(calltime) %>%
  top_n(1, n) %>%
  ungroup()

x2 = gcsd %>% mutate(day = wday(calltime, label = TRUE, abbr = FALSE)) %>%
  mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime, day) %>%
  count() %>%
  ungroup() %>%
  group_by(calltime) %>%
  top_n(1, n) %>%
  ungroup()

tile.set.14 = left_join(x1, x2, by = "calltime") %>%
  mutate(Year = year(calltime)) %>%
  mutate(Month = month(calltime, label = TRUE, abbr = FALSE)) %>%
  rename(`Peak Day` = day, `Peak Hour` = hour) %>%
  select(-n.x, -n.y, -calltime)
tile.set.14 <- tile.set.14[ ,c(3,4,2,1)]