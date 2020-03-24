require(tidyverse)
require(lubridate)

options(stringsAsFactors = FALSE)

# get vector of state names to use for filtering, with D.C. in the mix. deliberately doing this outside
# the data-making process so this vector of names is also created in the global environment to use
# in the app.
state_names <- sort(c(state.name, "District of Columbia"))

usa <- map_dfr(seq(from = date("2020-01-22"), to = Sys.Date() - 1, by = "day"), function(x) {

  date_string <- paste(substr(x, 6, 7), substr(x, 9, 10), substr(x, 1, 4), sep = "-")
  url <- url_seq <- sprintf("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/%s.csv", date_string)

  X <- read.csv(url, stringsAsFactors = FALSE)

  # standardize col names, which change over time 
  names(X) <- gsub("_", ".", names(X))
  names(X) <- replace(names(X), grepl("Province.State", names(X)), "Province.State") # gets rid of leading gibberish in early files

  # the format of these files changed on 3/10, so we have to make the munging conditional on the date
  if(x < date("2020-03-10")) {

    # need this to convert postal codes to full names, but we don't want these labels on vector in global env
    names(state_names) = append(state.abb, "DC", after = which(state_names == "District of Columbia") - 1)

    X %>%
      filter(Country.Region == "US") %>%
      separate(Province.State, into = c("locality", "state"), sep = ", ", fill = "left") %>%
      mutate(state = str_trim(state)) %>%
      mutate(state = ifelse(nchar(state) == 2, state_names[state], state)) %>%
      mutate(state = ifelse(state == "D.C.", "District of Columbia", state)) %>%
      group_by(state) %>%
      summarise(date = x,
                n_confirmed = sum(Confirmed, na.rm = TRUE),
                n_deaths = sum(Deaths, na.rm = TRUE)) %>%
      filter(state %in% state_names)

  } else {

    X %>%
      filter(Country.Region == "US") %>%
      group_by(state = Province.State) %>%
      summarise(date = x,
                n_confirmed = sum(Confirmed, na.rm = TRUE),
                n_deaths = sum(Deaths, na.rm = TRUE)) %>%
      filter(state %in% state_names)

  }

})

usa <- usa %>%
  group_by(state) %>%
  arrange(state, date)

usa_change <- usa %>%
  mutate_at(vars(starts_with("n_")), function(x) x - lag(x)) %>%
  slice(-1)
