# This app applies some common NLP routines to speeches by U.S. Federal Reserve officials since 2006. It is 
# meant to be useful in two ways: 1) as a tool to explore quantitatively the language used by Fed officials
# in their public speaking, and 2) as a way to learn about how some common NLP rountines work, especially
# sentiment analysis.

library(shiny)
library(tidyverse)
library(lubridate)
library(rvest)
library(tidytext)
library(corpus)
library(tm)
library(patchwork)
library(wesanderson)
library(streamgraph)

options(stringsAsFactors = FALSE)


# ---- LOAD DATA AND OBJECTS ----

# load stored workspace with topic modeling objects, including streamgraph
load("data/fedspeak-topic-models.RData")

# load csv of historical data
fedspeak_df <- read.csv("data/fed_speeches_and_sentiment_scores.csv") %>%
    mutate(date = lubridate::date(date))

# load sentiment dictionaries once at launch so they don't have to reload with each speech
nrc <- get_sentiments("nrc")
afinn <- get_sentiments("afinn")


# ---- SCRAPE LINKS TO SPEECHES ----

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

# convert list of vectors of links by year to one long vector and keep only unique elements
speech_links <- unique(unlist(speech_links))

# name the elements of that vector of urls so the user sees something more intelligble when selecting
# in the ui to come. with selectInput, names of vector elements are automatically displayed to the user
# instead of the vector elements themselves.

# make a unique id for each speech derived from the url
speech_ids <- gsub("https://www.federalreserve.gov/newsevents/speech/", "", speech_links)
speech_ids <- gsub("\\.htm", "", speech_ids)
speech_ids <- paste(str_to_title(str_extract(speech_ids, "[a-z]{3,}")), ymd(str_extract(speech_ids, "[0-9]{8}")))
names(speech_links) <- speech_ids
# note that a few urls don't follow the pattern of namedate, so this process returns a few labels of "NA NA"


# --- UI ----

ui <- navbarPage(inverse = TRUE, windowTitle = "Fedspeak",
                 title = span("Fedspeak    ", style = "font-size: 180%; font-family: verdana; color: white;"),

    # TAB 1: PICK A SPEECH
    tabPanel("Pick a speech",

        fluidPage(

            sidebarLayout(position = "right",

                sidebarPanel(

                    div(p("This app interactively applies some common natural language processing (NLP) routines
                          to more than 800 speeches by U.S. Federal Reserve officials from 2006 to the present,
                          as scraped from the Fed's online archive (",
                          a(href = "https://www.federalreserve.gov/newsevents/speeches.htm", "here", .noWS = "outside"),
                          ")."),
                        p("Pick a speech to analyze. You can read the full text of the speech on this tab, then open
                          other tabs to learn about the sentiment and topics of the speech and how they compare to
                          other speeches in the corpus."),
                        style = "font-family: verdana;"),

                    selectInput("speech", "", choices = speech_links)

                ),

                mainPanel(

                    uiOutput("speech_header"),

                    uiOutput("speech_text")

                )

            )

        )

    ),

    # TAB 2: SENTIMENT SCOREBOARD
    tabPanel("Sentiments",

        fluidPage(

            sidebarLayout(position = "right",

                sidebarPanel(

                    div(p("This panel summarizes the sentiment of the speech you selected, using the ",
                          a(href = "http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010", "AFINN"),
                          "and ",
                          a(href = "https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm", "NRC"), 
                          "lexicons."),
                        p("The plots on the top use standardized scores to show how the selected speech compares to 
                          all speeches from 2006 to early 2020 on six emotions. Zero is average, and higher scores
                          indicate stronger emotion."),
                        p("The plots on the bottom show the distribution of words in the selected speech relative to
                          each lexicon: NRC emotions on the left, AFINN sentiment scores on the right."),
                        style = "font-family: verdana;")

                ),

                mainPanel(

                    plotOutput("sentiment_plot_composite")

                )

            )

        )

    ),

    # TAB 3: TOPIC MODELING
    tabPanel("Topics",

        fluidPage(

            sidebarLayout(position = "right",

                sidebarPanel(

                    div(p("This tab shows the mix of topics present in the speech using four topic models generated from the historical corpus using ",
                          a(href = "https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation", "latent Dirichlet allocation", .noWS = "outside"),
                          " (LDA), an unsupervised topic-modeling techinique."),
                        p("The gauges on top show the probabilities that the document belongs to each of the four topics,
                          while the bar charts on the bottom show the terms most associated with each of those topics."),
                        p("Hat tip to",
                          a(href = "https://pomvlad.blog/2018/05/03/gauges-ggplot2/", "James Bedford"),
                          "for the code used to produce the gauge plots, and to Julia Silge and David Robinson for the",
                          a(href = "https://www.tidytextmining.com/", "tutorial and code"),
                          "on topic modeling the \"tidy\" way."),
                        style = "font-family: verdana;")

                ),

                mainPanel(

                    div(style = "margin-top: -6em; margin-bottom: 0em",
                        fluidRow(column = 8, plotOutput("topic_gauge_plot"))
                    ),

                    div(style = "margin-top:-7em",
                        fluidRow(column = 8, plotOutput("topic_term_plot"))
                    )

                )

            )

        )

    ),

    # TAB 4: HISTORICAL TRENDS
    tabPanel("Historical trends",

        fluidPage(

            sidebarLayout(position = "right",

                sidebarPanel(

                    div(p("This tab visualizes historical trends in the sentiment and topics of Fed officials' speeches from
                          January 2006 through February 2020."),
                        p("The statistics in the sentiment plots (top) were generated by applying AFINN and NRC lexicons to the
                          speech text, as done on the Sentiments tab. The points show the scores for the individual 
                          speeches, and the line is a statistical estimate of the trend in those scores over time (with 95% CI)."),
                        p("Pick a sentiment or emotion statistic to plot."),
                        style = "font-family: verdana;"),

                    selectInput("emotion", "",
                                choices = c("AFINN sentiment score", "fear", "sadness", "anger", "disgust", "surprise", "joy", "anticipation", "trust")),

                    div(p("The stream graph (bottom) shows change over time in the mix of topics covered by Fed officials' speeches,
                          using the same topic models (and colors) described on the Topics tab. It was generated with the",
                          a(href = "https://hrbrmstr.github.io/streamgraph/", "streamgraph"),
                          "package."),
                        style = "font-family: verdana;")

                ),

                mainPanel(

                    plotOutput("plot_sentiment_trends", height = "275px"),

                    streamgraphOutput("topic_streamgraph", height = "275px")

                )

            )

        )

    ),

    # TAB 5: ABOUT
    tabPanel("About",

        fluidPage(

            br(),

            div(p(strong("Creator:"), a(href = "https://github.com/ulfelder", "Jay Ulfelder")), 
                p(strong("R Packages:"), "shiny, tidyverse, tidytext, rvest, corpus, tm, patchwork, streamgraph, wesanderson"),
                p(strong("Source:"), a(href = "https://www.federalreserve.gov/newsevents/speeches.htm", "Speeches of Federal Reserve Officials")),
                style = "font-size: 110%; font-family: verdana;")

        )

    )

)

# ---- SERVER ----

server <- function(input, output, session) {

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
            str_trim(., side = "both")

        # make a unique id for each speech derived from the url
        id <- gsub("https://www.federalreserve.gov/newsevents/speech/", "", url)
        id <- gsub("\\.htm", "", id)

        return(tibble(id, date, title, speaker, location, text))

    }

    # scrape the selected speech
    speech <- reactive({ speech_scraper(input$speech) })

    # TAB 1: SPEECH TEXT ----

    output$speech_header <- renderUI({

        x <- speech()

        tagList(

            h3(em(x$title)),

            h4(x$speaker),

            h4(sprintf("%s %s, %s", month(x$date, label = TRUE, abbr = FALSE), day(x$date), year(x$date))),

            h5(x$location),

            hr()

        )

    })

    output$speech_text <- renderUI({

        # split the text into a vector of strings representing paragraphs, keying off the \n\n
        grafs <- str_split(speech()$text, "\n\n")[[1]]

        # use tagList to get a list of tag$p elements that can be rendered nicely by renderUI
        tagList(

            lapply(grafs, function(x) { p(x, style = "font-family: 'georgia'; font-size:110%;") })

        )

    })  

    # TAB 2: SENTIMENT SCOREBOARD ----

    # plot the sentiment scores and top words
    sentiment_plot <- eventReactive(input$speech, {

        # create tidy data frame with one word per row and no stop words
        speech_tidy <- speech() %>%
            select(id, text) %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words)

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

        # make vector of selected emotions to use in next couple of steps
        emotions <- c("fear", "sadness", "anger", "anticipation", "trust", "joy")

        # make named list of standardized emotion scores to use in gauge plots
        emotion_z_scores <- sapply(emotions, function(x) {

            raw_score <- unlist(feels_nrc[['emotions']][feels_nrc[['emotions']]$sentiment == x, "share"])

            mean <- mean(fedspeak_df[,paste0("nrc_", x)], na.rm = TRUE)
            stddev <- sd(fedspeak_df[,paste0("nrc_", x)], na.rm = TRUE)

            (raw_score - mean) / stddev

        })

        names(emotion_z_scores) <- emotions

        # function to make a gauge plot with a label and z-score
        # h/t https://stackoverflow.com/questions/24900903/how-to-draw-gauge-chart-in-r
        gg_gauge <- function(emotion, pos) {

            require(ggplot2)

            get.poly <- function(a, b, r1 = 0.5, r2 = 1.0) {
                th.start <- pi*(1-a/100)
                th.end <- pi*(1-b/100)
                th <- seq(th.start,th.end,length=100)
                x <- c(r1*cos(th),rev(r2*cos(th)))
                y <- c(r1*sin(th),rev(r2*sin(th)))
                return(data.frame(x,y))
            }

            # set breaks on 0-100 scale to correspond to -2.5,-2,-1,0,1,2,2.5 for z-scores
            breaks <- c(0, 10, 30, 50, 70, 90, 100)

            # make version of position indicator that tops or bottoms out at edge of gauge
            pos_ltd <- (pos + 2.5) * 20  # rescale to get -2.5/2.5 range to 0-100
            if(pos < -2.5) { pos_ltd <- 0 }
            if(pos > 2.5) { pos_ltd <- 100 }

            # uses zissou palette from wesanderson, but with gray in middle; writing it out to minimize dependencies
            p <- ggplot()+ 
                ylim(-1,1.25) +  # need this to prevent annotations from getting cut off
                geom_polygon(data=get.poly(breaks[1], breaks[2]), aes(x,y), fill="#3B9AB2", alpha = 1/3) +
                geom_polygon(data=get.poly(breaks[2], breaks[3]), aes(x,y), fill="#78B7C5", alpha = 1/3) +
                geom_polygon(data=get.poly(breaks[3], breaks[4]), aes(x,y), fill="gray90", alpha = 1/3) +
                geom_polygon(data=get.poly(breaks[4], breaks[5]), aes(x,y), fill="gray90", alpha = 1/3) +
                geom_polygon(data=get.poly(breaks[5], breaks[6]), aes(x,y), fill="#E1AF00", alpha = 1/3) +
                geom_polygon(data=get.poly(breaks[6], breaks[7]), aes(x,y), fill="#F21A00", alpha = 1/3) +
                # this bit makes the needle; tweak the 1st 2 parts of get.poly to widen or narrow it
                geom_polygon(data=get.poly(pos_ltd - 1, pos_ltd + 1, 0.2), aes(x,y)) +
                geom_text(data=as.data.frame(breaks[2:6]), size=4, vjust=0,
                          aes(x = 1.1*cos(pi*(1-breaks[2:6]/100)), y = 1.1*sin(pi*(1-breaks[2:6]/100)), label = c(-2,-1,0,1,2))) +
                # need the ggplot2:: in next 2 lines b/c function is masked by another package; took maybe 3 hrs to get this
                ggplot2::annotate("text", x = 0, y = 0, label = round(pos, 1), vjust = 0, size = 5, fontface = "bold") +
                ggplot2::annotate("text", x = 0, y = -1/2, label = emotion, vjust = 0, size = 7, fontface = "bold") +
                coord_fixed()+
                theme_bw()+
                theme(axis.text=element_blank(),
                      axis.title=element_blank(),
                      axis.ticks=element_blank(),
                      panel.grid=element_blank(),
                      panel.border=element_blank()) 

            return(p)

        }

        emotion_plot_list <- map(seq_along(emotions), ~ gg_gauge(emotion = emotions[.], pos = emotion_z_scores[.]))

        names(emotion_plot_list) <- emotions

        print(

            (emotion_plot_list[['fear']] | emotion_plot_list[['anger']] | emotion_plot_list[['sadness']] |
                 emotion_plot_list[['anticipation']] | emotion_plot_list[['trust']] | emotion_plot_list[['joy']]) /
            ( plot_nrc_col | plot_afinn_col )

        )

    })

    output$sentiment_plot_composite <- renderCachedPlot( sentiment_plot(), cacheKeyExpr = { input$speech } )

    # TAB 3: TOPIC MODELING ----

    # get custom color palette from wesanderson
    my_pal <- wes_palette("FantasticFox1")[2:5]

    topic_gauges <- eventReactive(input$speech, {

        # first step is to make a DocumentTermMatrix for the text to be classified; recycling this code
        # from process i used to fit topic models so the structure matches
        speech_scrubbed <- speech() %>%
            select(text) %>%
            unnest_tokens(word, text) %>%
            filter(grepl("\\D", word)) %>%  # remove numbers
            anti_join(stop_words) %>%  # remove stop words
            mutate(word = gsub("\\.[a-z]{1,}$", "", word)) %>%  #address repeated problem with '[word].i'
            mutate(word = unlist(corpus::text_tokens(word, stemmer = "en"))) %>%  # tokenize
            paste(unlist(.$word), collapse = " ")     
        speech_corpus <- Corpus(VectorSource(speech_scrubbed))
        speech_dtm <- DocumentTermMatrix(speech_corpus)

        # now we apply the fitted topic models to that dtm
        speech_probs <- topicmodels::posterior(fed_lda, speech_dtm)$topics
        
        # now make a data frame to use for gauge plot;
        # h/t https://pomvlad.blog/2018/05/03/gauges-ggplot2/
        gauge_df <- data.frame(variable = colnames(speech_probs),
                               percentage = round(as.vector(speech_probs), 2),
                               label = paste0(round(as.vector(speech_probs), 2) * 100, "%"),
                               title = paste0("Topic ", colnames(speech_probs)),
                               color = my_pal,
                               stringsAsFactors = FALSE)

        # now the plot
        ggplot(gauge_df, aes(ymax = percentage, ymin = 0, xmax = 2, xmin = 1, fill = variable)) +
            geom_rect(aes(ymax=1, ymin=0, xmax=2, xmin=1), fill = "#ece8bd") +
            geom_rect() +
            coord_polar(theta = "y", start = -pi/2) + xlim(c(0,2)) + ylim(c(0,2)) +
            geom_text(aes(x = 0, y = 0, label = label, colour = variable), size = 5.5) +
            geom_text(aes(x = 1.5, y = 1.5, label = title), size = 5) + 
            facet_grid(cols = vars(variable)) +
            scale_color_manual(values = my_pal) +
            scale_fill_manual(values = my_pal) +
            theme_void() +
            theme(strip.background = element_blank(),
                  strip.text.x = element_blank(),
                  plot.margin = margin(0, 0.5, 0, 2, "cm")) +
            guides(fill=FALSE) +
            guides(colour=FALSE)

    })

    output$topic_gauge_plot <- renderCachedPlot( topic_gauges(), cacheKeyExpr = { input$speech } )

    output$topic_term_plot <- renderPlot({

        fed_top_terms %>%
            mutate(term = reorder_within(term, beta, topic)) %>%
            ggplot(aes(term, beta, fill = factor(topic))) +
                geom_col(show.legend = FALSE) +
                facet_wrap(~ topic, nrow = 1, scales = "free") +
                coord_flip() +
                scale_x_reordered() +
                theme(panel.grid.minor = element_blank(),
                      plot.margin = margin(0, 1.5, 0, 1.5, "cm")) +
                scale_fill_manual(values = my_pal) +
                scale_y_continuous(breaks = seq(0,0.04,0.01), labels = c("0", "0.01", "0.02", "0.03", "0.04")) +
                xlab(NULL)

    })

    # TAB 4: HISTORICAL TRENDS ----

    output$plot_sentiment_trends <- renderPlot({

        # helper function for plotting NRC emotions
        nrc_emotion_plot <- function(emotion, my_span = 1/2) {

            ggplot(fedspeak_df, aes_string("date", paste0("nrc_", tolower(emotion)))) +
                geom_point(alpha = 1/2) +
                theme_minimal() +
                geom_smooth(span = my_span) +
                labs(title = sprintf("NRC %s score", emotion), y = "word share") +
                xlab(NULL)

        }

        if(input$emotion == "AFINN sentiment score") {

            ggplot(fedspeak_df, aes(date, afinn_mean)) +
                geom_point(alpha = 1/2) +
                theme_minimal() +
                geom_smooth(span = 1/2) +
                geom_hline(yintercept = 0) +
                labs(title = "AFINN sentiment score", y = "score") +
                xlab(NULL)

        } else {

            nrc_emotion_plot(input$emotion)

        }

    })

    output$topic_streamgraph <- renderStreamgraph({ topic_stream })

}

# ---- RUN ----

shinyApp(ui, server)
