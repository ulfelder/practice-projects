require(tidyverse)
require(lubridate)

options(stringsAsFactors = FALSE)

confirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv" %>%
  read.csv(.) %>%
  pivot_longer(cols = matches("^X[0-9]{1,2}"), names_to = "date", values_to = "n_confirmed") %>%
  separate(date, c("month", "day", "year"), sep = "\\.") %>%
  mutate(month = gsub("X", "", month),
         year = paste0("20", year),
         date = lubridate::date(paste(year, month, day, sep = "-")) ) %>%
  dplyr::select(Country.Region, Province.State, Lat, Long, date, n_confirmed)

deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv" %>%
  read.csv(.) %>%
  pivot_longer(cols = matches("^X[0-9]{1,2}"), names_to = "date", values_to = "n_deaths") %>%
  separate(date, c("month", "day", "year"), sep = "\\.") %>%
  mutate(month = gsub("X", "", month),
         year = paste0("20", year),
         date = lubridate::date(paste(year, month, day, sep = "-")) ) %>%
  dplyr::select(Country.Region, Province.State, Lat, Long, date, n_deaths)

full <- full_join(confirmed, deaths)

national <- full %>%
  group_by(Country.Region, date) %>%
  summarize_at(vars(starts_with("n_")), sum)

national_change <- national %>%
  mutate_at(vars(starts_with("n_")), function(x) x - lag(x)) %>%
  slice(-1)

global <- full %>%
  group_by(date) %>%
  summarize_at(vars(starts_with("n_")), sum)

global_change <- global %>%
  mutate_at(vars(starts_with("n_")), function(x) x - lag(x)) %>%
  slice(-1)    

global_sans_china <- full %>%
  filter(Country.Region != "China") %>%
  group_by(date) %>%
  summarize_at(vars(starts_with("n_")), sum)

global_sans_china_change <- global_sans_china %>%
  mutate_at(vars(starts_with("n_")), function(x) x - lag(x)) %>%
  slice(-1)
