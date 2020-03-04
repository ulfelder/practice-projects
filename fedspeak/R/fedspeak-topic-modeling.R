library(tidyverse)
library(tidytext)
library(corpus)
library(tm)
library(topicmodels)
library(wesanderson)
library(patchwork)
library(zoo)
library(streamgraph)

options(stringsAsFactors = FALSE)

# load historical data with speech texts
fedspeak_df <- read.csv("data/fed_speeches_and_sentiment_scores.csv") %>%
    mutate(date = lubridate::date(date))


## ---- TOPIC MODELING ----

# attempts to create a DTM failed when applied directly to df$text, but it works if i run it through
# the tidying wringer first, then reassemble
# NOTE: TRY THIS FOR TEXT-RENDERING PROBLEM IN APP, TOO --> TRIED AND FAILED
tidy_texts <- fedspeak_df %>%
    select(document, text) %>%
    unnest_tokens(word, text) %>%
    # remove numbers
    filter(grepl("\\D", word)) %>%
    # remove stop words
    anti_join(stop_words) %>%
    #address repeated problem with '[word].i'
    mutate(word = gsub("\\.[a-z]{1,}$", "", word)) %>%
    # tokenize
    mutate(word = unlist(corpus::text_tokens(word, stemmer = "en"))) %>%
    group_by(document) %>%  # here's the part where having a numeric id produces expected order
    group_map(~paste(unlist(.$word), collapse = " ")) %>%
    unlist()

corpus <- Corpus(VectorSource(tidy_texts))

X <- DocumentTermMatrix(corpus)

fed_lda <- LDA(X, k = 4, control = list(seed = 20912))
 
fed_top_terms <- tidy(fed_lda, matrix = "beta") %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

document_topic_probabilities <- tidy(fed_lda, matrix = "gamma")

# STREAM GRAPH OF TOPICS OVER TIME

fed_doc_probs <- fedspeak_df %>%
    mutate(document = as.character(document)) %>%
    select(document, date, title) %>%
    left_join(document_topic_probabilities, ., by = "document") %>%
    mutate(date = lubridate::date(date)) %>%
    mutate(yrmo = zoo::as.yearmon(date)) %>%
    group_by(yrmo, topic) %>%
    summarize(mean = mean(gamma))

topic_stream <- streamgraph(fed_doc_probs, "topic", "mean", "yrmo", interactive = TRUE) %>%
    sg_axis_x(tick_interval = 1, tick_units = "year", "%Y") %>%
    sg_fill_manual(wes_palette("FantasticFox1")[2:5])

rm(corpus, document_topic_probabilities, fed_doc_probs, fedspeak_df, tidy_texts, X)

save.image("data/fedspeak-topic-models.RData")
