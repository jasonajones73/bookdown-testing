# Load packages
library(tidyverse)
library(lubridate)

# Load data, transform, and apply universal filter
aco <- read_csv(here::here("data/callsWithJurisdictions.csv"),
                col_types = cols(case_id = col_character(),
                                 nature2 = col_character())) %>%
  filter(agency == "ACO") %>%
  mutate(nature = str_replace(nature, "Z", "")) %>%
  mutate(jurisdiction = case_when(
    is.na(jurisdiction) == TRUE ~ "Guilford County/Other",
    is.na(jurisdiction) != TRUE ~ jurisdiction)) %>%
  filter(cancelled == FALSE)

# Transform specific jurisdictions that should not be generally displayed
aco$jurisdiction[aco$jurisdiction == "High Point" | aco$jurisdiction == "Forest Oaks" | aco$jurisdiction == "Gibsonville" | aco$jurisdiction == "McLeansville" | aco$jurisdiction == "Burlington" | aco$jurisdiction == "Archdale" | aco$jurisdiction == "Kernersville" | aco$jurisdiction == "Trinity" | aco$jurisdiction == "Walkertown" | aco$jurisdiction == "Winston-Salem"] <- "Guilford County/Other"

## SUBSET 1
tile.set.1 <- aco %>%
  mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime, nature) %>%
  count() %>%
  ungroup() %>%
  group_by(calltime) %>%
  top_n(5, n) %>%
  ungroup() %>%
  rename(callcount = n)

## SUBSET 2
tile.set.2 <- aco %>%
  mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime) %>%
  count() %>%
  rename(callcount = n)

## SUBSET 3
tile.set.3 <- aco %>%
  filter(year(calltime) == year(aco$calltime[length(aco$calltime)])) %>%
  filter(month(calltime) == month(aco$calltime[length(aco$calltime)])) %>%
  group_by(jurisdiction) %>%
  count() %>%
  rename(callcount = n)

## SUBSET 4
tile.set.4 <- aco %>%
  mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime) %>%
  summarise(avgresp = round(mean(secsdi2ar)))

## SUBSET 5
tile.set.5 <- aco %>%
  filter(year(calltime) == year(aco$calltime[length(aco$calltime)])) %>%
  filter(month(calltime) == month(aco$calltime[length(aco$calltime)])) %>%
  group_by(jurisdiction) %>%
  summarise(avgresp = round(mean(secsdi2ar)))

## SUBSET 6
tile.set.6 <- aco %>%
  filter(year(calltime) == year(aco$calltime[length(aco$calltime)])) %>%
  filter(month(calltime) == month(aco$calltime[length(aco$calltime)])) %>%
  group_by(nature, jurisdiction) %>%
  count() %>%
  ungroup() %>%
  group_by(jurisdiction) %>%
  top_n(5, n) %>%
  rename(callcount = n)

## SUBSET 7
tile.set.7 <- aco %>%
  mutate(calldow = wday(calltime, label = TRUE, abbr = FALSE)) %>%
  group_by(calldow) %>%
  count() %>%
  rename(callcount = n)

## SUBSET 8
tile.set.8 <- aco %>%
  mutate(calldow = wday(calltime, label = TRUE, abbr = FALSE)) %>%
  filter(year(calltime) == year(aco$calltime[length(aco$calltime)])) %>%
  group_by(calldow) %>%
  count() %>%
  rename(calcount = n)

## SUBSET 9
tile.set.9 <- aco %>%
  mutate(calldow = wday(calltime, label = TRUE, abbr = FALSE)) %>%
  filter(year(calltime) == year(aco$calltime[length(aco$calltime)])) %>%
  filter(month(calltime) == month(aco$calltime[length(aco$calltime)])) %>%
  group_by(calldow) %>%
  count() %>%
  rename(calcount = n)

## SUBSET 10
tile.set.10 <- aco %>%
  mutate(hod = hour(calltime)) %>%
  group_by(hod) %>%
  count() %>%
  rename(calcount = n)

## SUBSET 11
tile.set.11 <- aco %>%
  mutate(hod = hour(calltime)) %>%
  filter(year(calltime) == year(aco$calltime[length(aco$calltime)])) %>%
  group_by(hod) %>%
  count() %>%
  rename(calcount = n)

## SUBSET 12
tile.set.12 <- aco %>%
  mutate(hod = hour(calltime)) %>%
  filter(year(calltime) == year(aco$calltime[length(aco$calltime)])) %>%
  filter(month(calltime) == month(aco$calltime[length(aco$calltime)])) %>%
  group_by(hod) %>%
  count() %>%
  rename(calcount = n)

## SUBSET 13
tile.set.13 <- aco %>%
  filter(year(calltime) == year(aco$calltime[length(aco$calltime)])) %>%
  filter(month(calltime) == month(aco$calltime[length(aco$calltime)])) %>%
  mutate(calldow = wday(calltime, label = TRUE, abbr = FALSE)) %>%
  group_by(calldow) %>%
  summarise(avgresp = round(mean(secsdi2ar)))

## SUBSET 14
tile.set.14 <- aco
tile.set.14$priority[is.na(tile.set.14$priority)] <- "Other"
tile.set.14$priority[tile.set.14$priority != "P" & tile.set.14$priority != "3" & tile.set.14$priority != "7"] <- "Other"
tile.set.14$priority[tile.set.14$priority == "P"] <- "First"
tile.set.14$priority[tile.set.14$priority == "3"] <- "Second"
tile.set.14$priority[tile.set.14$priority == "7"] <- "Third"
tile.set.14 <- tile.set.14 %>%
  mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime, priority) %>%
  summarise(avgresp = round(mean(secsdi2ar)))

## SUBSET 15
tile.set.15 <- aco
tile.set.15$priority[is.na(tile.set.15$priority)] <- "Other"
tile.set.15$priority[tile.set.15$priority != "P" & tile.set.15$priority != "3" & tile.set.15$priority != "7"] <- "Other"
tile.set.15$priority[tile.set.15$priority == "P"] <- "First"
tile.set.15$priority[tile.set.15$priority == "3"] <- "Second"
tile.set.15$priority[tile.set.15$priority == "7"] <- "Third"
tile.set.15 <- tile.set.15 %>%
  mutate(calltime = floor_date(calltime, unit = "1 month")) %>%
  group_by(calltime, priority) %>%
  count() %>%
  rename(callcount = n)