# This script builds and exports a data frame of Fed officials' speeches, metadata associated with those speeches
# (speaker, date, location), and sentiment scores associated with those speeches for the period from January 2006
# to whenever it is run.

library(tidyverse)
library(lubridate)
library(rvest)
library(tidytext)

options(stringsAsFactors = FALSE)


# scrape links to pages with links to speeches, organized by year
year_links <- read_html("https://www.federalreserve.gov/newsevents/speeches.htm") %>%
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

# convert list of vectors of links by year to one long vector and drop duplicates
speech_links <- unique(unlist(speech_links))

# name the elements of that vector of urls so the user sees something more intelligble when selecting.
# note that a few urls don't follow the pattern of namedate, so this process gives them labels of "NA NA"
speech_ids <- str_extract(speech_links, "[a-z]{3,}[0-9]{8}")
speech_ids <- paste(str_to_title(str_extract(speech_ids, "[a-z]{3,}")), ymd(str_extract(speech_ids, "[0-9]{8}")))
names(speech_links) <- speech_ids

# iterate a scraping process over that vector to generate a list of one-row tibbles of speech texts
# and associated metadata
list_of_speeches <- map(speech_links, function(url) {

  scraped_page <- read_html(url)

  # found this and ensuing node types by opening one page in Chrome, right-clicking to inspect it
  date <- scraped_page %>% html_nodes("p.article__time") %>% html_text() %>% as.Date(., format = "%B %d, %Y")

  title <- scraped_page %>% html_nodes("h3.title") %>% html_text()

  speaker <- scraped_page %>% html_nodes("p.speaker") %>% html_text()

  location <- scraped_page %>% html_nodes("p.location") %>% html_text()

  text <- scraped_page %>%
    html_nodes("div.col-xs-12.col-sm-8.col-md-8") %>%
    html_text() %>%
    # discovered via trial and error that previous bit returned vector of two strings, first of which
    # repeated some of elements already scraped (e.g., title)
    .[2] %>%
    # next two steps clean up extraneous bit at head of speech text
    str_replace_all(., "\n", "") %>%
    str_trim(., side = "both")

    # make a unique id for each speech derived from the url
    id <- gsub("https://www.federalreserve.gov/newsevents/speech/", "", url)
    id <- gsub("\\.htm", "", id)

    return(tibble(id, date, title, speaker, location, text))

})

# compute sentiment stats for all speeches in the list
list_of_speech_stats <- map(list_of_speeches, function(speech_tibble) {

    require(dplyr)
    require(tidytext)

    afinn <- get_sentiments("afinn")
    nrc <- get_sentiments("nrc")

    # create tidy data frame with one word per row and no stop words or numbers
    speech_tidy <- speech_tibble %>%
        select(id, text) %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        mutate(word = str_replace(word, "'s", "")) %>%
        # other cleaning for sentiment analysis
        filter(!grepl("www\\.*\\.[a-z]{2,3}", word),
               !grepl(".\\htm*$", word),
               !(word %in% c("http", "https", "aboutthefed", "pdf")),
               grepl("\\D", word))
     
    # sentiment score (afinn)
    feels_afinn <- inner_join(speech_tidy, afinn) %>%
        summarize(afinn_mean = mean(value))

    # sentiment measures (nrc)
    feels_nrc <- speech_tidy %>%
        count(word, sort = TRUE) %>%
        mutate(share = n/sum(n)) %>%
        inner_join(nrc) %>%
        group_by(sentiment) %>%
        summarize(share = sum(share)) %>%
        mutate(sentiment = paste0("nrc_", sentiment)) %>%
        pivot_wider(names_from = sentiment, values_from = share)

    feels <- cbind(feels_afinn, feels_nrc)

    return(feels)

})

# combine those two lists into one big table with one row per speech and cols for id, metadata, text,
# and sentiment stats
fedspeak_df <- bind_cols(bind_rows(list_of_speeches), bind_rows(list_of_speech_stats))

# add document id that we'll want to help keep corpus in predictable order later
fedspeak_df <- mutate(fedspeak_df, document = as.character(seq_along(id))) %>%
    select(document, everything())

# save the result to the working directory
write.csv(fedspeak_df, "data/fed_speeches_and_sentiment_scores.csv", row.names = FALSE)
