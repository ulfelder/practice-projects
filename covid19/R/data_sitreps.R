require(tidyverse)
require(rvest)
require(lubridate)

# get links to WHO situation reports and give them pubdates as names
sitreps <- read_html("https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports/") %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  grep("-sitrep-", ., value = TRUE) %>%
  sprintf("https://www.who.int%s", .) %>%
  unique(.)

sitrep_dates <- sitreps %>% str_extract("[0-9]{8}") %>% ymd()

names(sitreps) <- sitrep_dates
