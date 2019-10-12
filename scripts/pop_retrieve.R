

library(tidycensus)
library(tidyverse)

key <- Sys.getenv("CENSUS_API_KEY")

cen_vars <- load_variables(2017, "acs1")

x <- 2010:2017

population <- map_df(.x = x,
                     .f = ~get_acs(geography = "county", variables = "B01001_001",
                                  year = ., state = 37, county = 81))


population <- population %>%
  mutate(est_year = x)


write_csv(population, path = here::here("data/population.csv"))
