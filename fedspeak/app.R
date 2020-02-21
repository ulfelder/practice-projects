# This app applies some common NLP routines to reactively scraped speeches by
# U.S. Federal Reserve officials from 2006 to the present.

library(tidyverse)
library(shiny)
library(forcats)
library(lubridate)
library(rvest)
library(tidytext)
library(corpus)
library(patchwork)

options(stringsAsFactors = FALSE)

# ---- SETUP ----

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

# convert list of vectors of links by year to one long vector
speech_links <- unlist(speech_links)

# name the elements of that vector of urls so the user sees something more intelligble when selecting
# in the ui to come. with selectInput, names of vector elements are automatically displayed to the user
# instead of the vector elements themselves.
speech_ids <- str_extract(speech_links, "[a-z]{3,}[0-9]{8}")
speech_ids <- paste(str_to_title(str_extract(speech_ids, "[a-z]{3,}")), ymd(str_extract(speech_ids, "[0-9]{8}")))
names(speech_links) <- speech_ids
# note that a few urls don't follow the pattern of namedate, so this process returns a few labels of "NA NA"

# load sentiment dictionaries once at launch so they don't have to get reloaded with each speech selection later
afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")


# --- UI ----

ui <- shinyUI(pageWithSidebar(

    headerPanel("U.S. Federal Reserved speeches, crunched"),

    sidebarPanel(

        p("This tool automatically applies some common natural language processing (NLP) routines
           to speeches by U.S. Federal Reserve officials from 2006 to the present.",
          style = "font-family: 'georgia';"),

        br(),

        p("Pick a speech to analyze.", style = "font-family: 'georgia';"),

        selectInput("speech", "", choices = speech_links)        

    ),

    mainPanel(

        plotOutput("plot_composite", width = "100%")

    )

))


# ---- SERVER ----

server <- function(input, output) {

# helper function to scrape a single speech
speech_scraper <- function(url) {

    Z <- read_html(url)
 
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

    # create a unique id for each speech to help with later munging
    id <- str_extract(url, "[a-z]{3,}[0-9]{8}[a-z]{1}")

    return(tibble(id, date, title, speaker, location, text))

}

    output$plot_composite <- renderPlot({

    # scrape the selected speech
    speech <- speech_scraper(input$speech)

    # create tidy data frame with one word per row and no stop words
    speech_tidy <- speech %>%
        select(id, text) %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words)

    # plot of 20 most frequent terms, stemmed for proper comparison
    plot_word_freq <- speech_tidy %>%
        mutate(word = unlist(corpus::text_tokens(word, stemmer = "en"))) %>%
        count(word, sort = TRUE) %>%
        # get rid of numbers
        filter(grepl("\\D", word)) %>%
        # need this to avoid alphabetical reordering of words when plotting
        mutate(word = reorder(word, n)) %>%
        slice(1:20) %>%
        ggplot(aes(word, n)) +
            geom_col(fill = "dodgerblue", alpha = 1/2) +
            theme_minimal() +
            labs(title = "20 most common terms", y = "count") +
            xlab(NULL) +
            coord_flip()

    # sentiment score (afinn) to use as annotation in next plot
    feels_afinn <- inner_join(speech_tidy, afinn) %>%
        summarize(mean = mean(value)) %>%
        unlist() %>%
        round(., 2)

    # histogram of afinn word values
    plot_afinn_col <- speech_tidy %>%
        inner_join(afinn) %>%
        ggplot(aes(value)) +
            geom_bar(fill = "darkolivegreen", alpha = 3/5) +
            theme_minimal() +
            scale_x_continuous(breaks = seq(-5,5,1), limits = c(-5,5)) +
            xlab(NULL) + 
            theme(panel.grid.minor = element_blank()) +
            labs(title = "Distribution of words by AFINN sentiment scores",
                 subtitle = sprintf("Mean score = %s %s", ifelse(feels_afinn > 0, "+", "-"), feels_afinn),
                 y = "word count")
    
    # create a 2-item list with dfs of word shares of nrc sentiments and emotions
    feels_nrc <- speech_tidy %>%
        count(word, sort = TRUE) %>%
        mutate(share = n/sum(n)) %>%
        inner_join(nrc) %>%
        group_by(sentiment) %>%
        summarize(share = sum(share)) %>%
        list(sentiments = filter(., sentiment %in% c("negative", "positive")),
             emotions = filter(., !(sentiment %in% c("negative", "positive"))))

    # nrc net sentiment by wordshare
    nrc_net <- round(feels_nrc[['sentiments']]$share[feels_nrc[['sentiments']]$sentiment == "positive"] -
        feels_nrc[['sentiments']]$share[feels_nrc[['sentiments']]$sentiment == "negative"], 2)

    # plot of nrc sentiment by emotion
    plot_nrc_col <- feels_nrc[['emotions']] %>%
        # add cols to allow fill color by +/-, to put emotions in desired order for plotting
        mutate(class = ifelse(sentiment %in% c('anger', 'disgust', 'fear', 'sadness'), 'negative', 'positive'),
               sentiment = forcats::fct_relevel(sentiment, "sadness", "fear", "disgust", "anger", "surprise", "anticipation", "trust", "joy")) %>%
        ggplot(aes(x = sentiment, y = share, fill = class)) +
            geom_col(alpha = 3/5) +
            theme_minimal() +
            labs(title = "Distribution of words by NRC emotions",
                 subtitle = sprintf("Net sentiment (pos share - neg share) = %s %s", ifelse(nrc_net > 0, "+", "-"), nrc_net),
                 y = "share of all words") +
            xlab(NULL) +
            theme(legend.position = "none",
                  axis.text.x = element_text(angle = 90, hjust = 1))

         print(plot_word_freq  | ( plot_nrc_col / plot_afinn_col ))

    })

}

# ---- RUN ----

shinyApp(ui, server)


# POSSIBLE EXTENSIONS
# * topic modeling?
# * tab to read full text (color words by nrc emotion? named entity recognition?)
# * time series of sentiment scores across speeches with loess line to show general trend
# * reactive sidebar for selection, tabs for
#   1) plots of term freq and sentiment with pane for viewing full text at bottom and
#   2) trend in sentiment with selection of window
