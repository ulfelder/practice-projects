# brutally simple app for showing how a text gets scored on sentiment, using the pos/neg from NRC

library(shiny)
library(tidyverse)
library(tidytext)

nrc <- get_sentiments("nrc")

nrc_sentiments <- nrc %>%
    filter(sentiment == "negative" | sentiment == "positive") %>%
    mutate(color = recode(sentiment, positive = "Aqua", negative = "Yellow"))

ui <- mainPanel(    

    fluidRow(

        textInput("usertext", "Type or paste here:", width = "100%"),

        br(),

        br(),

        uiOutput("thetextoutput"),

        br(),

        br(),

        uiOutput("score")
    
    )
  
)

server <- function(input, output) {
  
    output$thetextoutput <- renderUI({
    
        bag_of_words <- strsplit(input$usertext, " ")[[1]]
    
        lapply(bag_of_words, function(x) {
      
            col <- NULL
      
            if (tolower(x) %in% nrc_sentiments$word) {
        
                col <- nrc_sentiments$color[which(nrc_sentiments$word == x)]
        
            }
      
            tags$span(style = paste("background-color:", col), x)
      
        })
    
    }) 

    output$score <- renderText({

        bag_of_words <- strsplit(input$usertext, " ")[[1]]

        n <- length(bag_of_words)    

        pos <- sum(bag_of_words %in% nrc_sentiments$word[nrc_sentiments$sentiment == "positive"])

        neg <- sum(bag_of_words %in% nrc_sentiments$word[nrc_sentiments$sentiment == "negative"])

        sprintf("Net score: %s %s", ifelse(pos > neg, "+", ifelse(neg > pos, "-", "")), round((pos - neg)/n, 2))

    })

}

shinyApp(ui, server)
