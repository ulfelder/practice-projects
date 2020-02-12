# This Shiny app makes it easy to update a probabilistic belief or forecast in light of new
# information using Bayes' theorem. You supply the guesstimates, it provides the posterior.

library(shiny)
library(tidyverse)

# function to crunch the numbers
update_bayes <- function(prior_a, p_b_a, p_b_nota) {

  (prior_a * p_b_a) / ((prior_a * p_b_a) + (p_b_nota * (1 - prior_a)))  

}

# USER INTERFACE ----

ui <- fluidPage(

    # Application title
    titlePanel("Bayes-o-Matic"),

    sidebarLayout(

        sidebarPanel(
                   
            sliderInput("prior", "Prior probability of A:",
                        min = 0, max = 1, value = 0.5, step = 0.01,
                        width = '400px'),

            sliderInput("p_b_a", "Probability of B if A:",
                        min = 0, max = 1, value = 0.5, step = 0.01,
                        width = '400px'),

            sliderInput("p_b_nota", "Probability of B if not A:",
                        min = 0, max = 1, value = 0.5, step = 0.01,
                        width = '400px'),

            hr(),

            helpText("Use Bayes' theorem to update your estimate of the probability of an event
                      or condition of interest, A, in light of new information, B. Start with your
                      prior belief about the probability of A. Now consider how likely it is that
                      we would see B if A were true or going to happen and set 'Probability of B if A'
                      to that. Then consider how likely it is that we would see B if A were not true
                      or were not going to happen and set 'Probability of B if not A' to that.
                      The probability you then see in the chart is your posterior, i.e., your
                      new estimate of the probability of A.")
            
        ),

        mainPanel(plotOutput("posterior_plot"))
    )

)

# SERVER LOGIC ----

server <- function(input, output) {
  
    output$posterior_plot <- renderPlot({

        # run helper function over user-selected inputs
        posterior <- update_bayes(prior = input$prior,
                                  p_b_a = input$p_b_a,
                                  p_b_nota = input$p_b_nota)

        # make a data frame that ggplot2 can easily use
        df_plot <- data.frame(percentage = round(posterior, 2),
                              label = sprintf("%s%%", round(posterior, 2) * 100),
                              title = "Posterior probability of A",
                              stringsAsFactors = FALSE)

        # make a gauge plot; H/T https://pomvlad.blog/2018/05/03/gauges-ggplot2/
        ggplot(df_plot, aes(ymax = percentage, ymin = 0, xmax = 2, xmin = 1)) +
            geom_rect(aes(ymax = 1, ymin = 0, xmax = 2, xmin = 1), fill = "#ece8bd") + 
            geom_rect() +
            coord_polar(theta = "y", start = pi/2) + xlim(c(0, 2)) + ylim(c(0,2)) +
            geom_text(aes(x = 0, y = 0, label = label), size = 12) +
            geom_text(aes(x = 1.5, y = 1.5, label = title), size = 10) +
            theme_void() +
            theme(strip.background = element_blank(),
                  strip.text.x = element_blank()) +
            guides(fill=FALSE) +
            guides(colour=FALSE)
        
    })

}

# RUN APP ----

shinyApp(ui, server)
