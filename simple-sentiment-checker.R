# brutally simple app for showing how a text gets scored on sentiment, using the pos/neg from NRC

library(shiny)
library(tidyverse)
library(tidytext)
library(rvest)

nrc <- get_sentiments("nrc")

nrc_sentiments <- nrc %>%
    filter(sentiment == "negative" | sentiment == "positive") %>%
    mutate(color = recode(sentiment, positive = "Aqua", negative = "Yellow"))

# ---- UI ----

ui <- mainPanel(    

    fluidRow(

        textInput("usertext", "Type or paste here:", width = "100%"),

        br(),

        br(),

        uiOutput("highlighted_text"),

        br(),

        br(),

        uiOutput("net_score")
    
    )
  
)

server <- function(input, output) {
  
    output$highlighted_text <- renderUI({
    
        bag_of_words <- strsplit(input$usertext, " ")[[1]]
    
        tag_list <- lapply(bag_of_words, function(x) {
      
            col <- "White"
      
            y <- tolower(gsub("[[:punct:] ]+", "", x))

            if (y %in% nrc_sentiments$word) {
        
                col <- nrc_sentiments$color[which(nrc_sentiments$word == y)]
        
            }
      
            tags$span(style = paste("background-color:", col), x)
      
        })

        tagList(tag_list)
    
    }) 

    output$net_score <- renderText({

        bag_of_words <- strsplit(input$usertext, " ")[[1]]

        n <- length(bag_of_words)    

        pos <- sum(bag_of_words %in% nrc_sentiments$word[nrc_sentiments$sentiment == "positive"])

        neg <- sum(bag_of_words %in% nrc_sentiments$word[nrc_sentiments$sentiment == "negative"])

        if(n >= 1) {

            sprintf("Net score: %s %s", ifelse(pos > neg, "+", ifelse(neg > pos, "-", "")), round((pos - neg)/n, 2))

        } else {

            "Not enough information to compute."

        }

    })

}

shinyApp(ui, server)
