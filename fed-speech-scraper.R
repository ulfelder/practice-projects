library(tidyverse)
library(rvest)
library(tidytext)

base_page <- read_html("https://www.federalreserve.gov/newsevents/speeches.htm")

# scrape links to pages with links to speeches, organized by year
year_links <- base_page %>%
  # had to open page and right-click to inspect in Chrome to see this, and it returned a long vector of links,
  # most of which were not of interest
  html_nodes("a") %>%
  html_attr("href") %>%
  # next two steps from https://stackoverflow.com/questions/56160445/how-to-pass-multiple-necessary-patterns-to-str-subset
  # saw patterns by doing previous bit, then eyeballing vector of results in console
  map(c("/newsevents/speech/[0-9]{4}-speeches.htm", "/newsevents/speech/[0-9]{4}speech.htm"), str_subset, string = .) %>%
  reduce(union)

# iteratively scrape links to specific speeches from pages with collections of speeches by year
speech_links <- map(year_links, function(x) {

  full_url <- paste0("https://www.federalreserve.gov", x)

  speech_urls <- read_html(full_url) %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    # found this pattern by running previous steps, eyeballing results in console
    str_subset(., "/newsevents/speech/")

  return(paste0("https://www.federalreserve.gov", speech_urls))

})

# convert list of vectors of links by year to one long vector
speech_links <- unlist(speech_links)

# now iterate over that vector of links to scrape speeches and associated metadata and return
# them in a list of one-row data frames
speech_list <- map(speech_links, function(x) {

  Z <- read_html(x)

  # found this and ensuing node types by opening one page in Chrome, right-clicking to inspect it
  date <- Z %>% html_nodes("p.article__time") %>% html_text() %>% as.Date(., format = "%B %d, %Y")

  title <- Z %>% html_nodes("h3.title") %>% html_text()

  speaker <- Z %>% html_nodes("p.speaker") %>% html_text()

  location <- Z %>% html_nodes("p.location") %>% html_text()

  text <- Z %>%
    html_nodes("div.col-xs-12.col-sm-8.col-md-8") %>%
    html_text() %>%
    # discovered via trial and error that previous bit returned vector of two strings, first of which
    # repeated some of elements already scraped (e.g., title)
    .[2] %>%
    # next two steps clean up extraneous bit at head of speech text
    str_replace_all(., "\n", "") %>%
    str_trim(., side = "both")

  return(tibble(date, title, speaker, location, text))

})

# collapse that list of one-row data frames into one big table
speech_table <- bind_rows(speech_list)

# ready for sentiment analysis, NER, etc.
