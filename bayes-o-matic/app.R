# This Shiny app makes it easy to update a probabilistic belief or forecast in light of new
# information using Bayes' theorem. You supply the guesstimates, it provides the posterior.

library(shiny)
library(tidyverse)

# ---- SETUP ----

# function to crunch the numbers
update_bayes <- function(prior_a, p_b_a, p_b_nota) {
  
  (prior_a * p_b_a) / ((prior_a * p_b_a) + (p_b_nota * (1 - prior_a)))  
  
}

# function to make the gauge plot from a percentage (integer on 0-100 scale)
gg_gauge <- function(pos) {
  
  require(ggplot2)
  
  get.poly <- function(a, b, r1 = 0.5, r2 = 1.0) {
    th.start <- pi*(1-a/100)
    th.end <- pi*(1-b/100)
    th <- seq(th.start,th.end,length=100)
    x <- c(r1*cos(th),rev(r2*cos(th)))
    y <- c(r1*sin(th),rev(r2*sin(th)))
    return(data.frame(x,y))
  }
  
  ggplot() + 
    ylim(0,1) +  # expand if you have labels or annotations significantly above or below the plot area
    geom_polygon(data=get.poly(0, pos), aes(x,y), fill="gray10", alpha = 1/2) +
    geom_polygon(data=get.poly(pos, 100), aes(x,y), fill="gray90", alpha = 1/2) +
    annotate("text", x = 0, y = 0, label = paste0(pos, "%"),
             vjust = 0, size = 12, fontface = "bold", family = "mono") +
    coord_fixed() +
    theme_bw() +
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          axis.ticks=element_blank(),
          panel.grid=element_blank(),
          panel.border=element_blank())
  
}

# USER INTERFACE ----

ui <- fluidPage(theme = "bootstrap.css",
  
  # Application title
  titlePanel(h1("Bayes-o-Matic", style = "font-size: 50px")),
  
  sidebarLayout(
    
    sidebarPanel(
      
      div(
        p("Use Bayes' theorem to update your estimate of the probability of an event
          or condition of interest, A, in light of new information, B."),
        p("Start with your prior belief about the probability of A. Now consider how likely
          it is that we would see B if A were true or going to happen and set \"Probability of
          B if A\" to that. Then consider how likely it is that we would see B if A were ", em("not"),
          " true or were ", em("not"), " going to happen and set \"Probability of B if not A\" to that."),
        p("The probability you then see in the chart is your posterior, i.e., your new
          estimate of the probability of A."),
        style = "font-size: 135%;"
      ),

      hr(),
      
      sliderInput("prior", "Prior probability of A:",
                  min = 0, max = 1, value = 0.5, step = 0.01,
                  width = '400px'),
      
      sliderInput("p_b_a", "Probability of B if A:",
                  min = 0, max = 1, value = 0.5, step = 0.01,
                  width = '400px'),
      
      sliderInput("p_b_nota", "Probability of B if not A:",
                  min = 0, max = 1, value = 0.5, step = 0.01,
                  width = '400px')
      
    ),
    
    mainPanel(
      
      plotOutput("posterior_plot")
      
    )
    
  )
  
)

# SERVER LOGIC ----

server <- function(input, output) {
  
  output$posterior_plot <- renderPlot({
    
    # run helper function over user-selected inputs
    posterior <- update_bayes(prior = input$prior,
                              p_b_a = input$p_b_a,
                              p_b_nota = input$p_b_nota)
    
    # rescale for plotting
    posterior <- round(100 * posterior, 0)
    
    # generate the plot
    gg_gauge(posterior)
    
  })
  
}

# RUN APP ----

shinyApp(ui, server)
