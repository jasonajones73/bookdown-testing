# Load packages
library(tidyverse)
library(lubridate)

fjc_all <- read_csv("J:/fjc_data/fjc_all.csv")

fjc_visit_reason <- read_csv("J:/fjc_data/visit_reason.csv")

fjc_client_type <- read_csv("J:/fjc_data/client_type.csv")

fjc_referalls <- read_csv("J:/fjc_data/service_referalls.csv")

fjc_all <- fjc_all %>%
  mutate(`Office Location` = case_when(
    is.na(`Office Location`) == TRUE ~ "GSO",
    is.na(`Office Location`) != TRUE ~ `Office Location`
  )) %>%
  mutate(Date = as.POSIXct(Date, format = "%m/%d/%Y %I:%M%p")) %>%
  mutate(Date = floor_date(Date, unit = "1 month"))

fjc_client_type <- fjc_client_type %>%
  left_join(select(fjc_all, index, `Office Location`)) %>%
  mutate(date = as.POSIXct(date, format = "%m/%d/%Y %I:%M%p")) %>%
  mutate(date = floor_date(date, unit = "1 month"))
