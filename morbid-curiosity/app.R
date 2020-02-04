# This Shiny app scrapes an actuarial life table from the U.S. Social Security Administration and
# lets users explore what those data imply about the probability of dying over various time spans,
# conditional on sex and age.

library(shiny)
library(tidyverse)
library(rvest)

options(stringsAsFactors = FALSE)

varnames <- c("exact_age",
              "male_death_p", "male_lives", "male_life_expectancy",
              "female_death_p", "female_lives", "female_life_expectancy")

# scrape actuarial life table from Social Security Administration website
life_table <- read_html("https://www.ssa.gov/oact/STATS/table4c6.html") %>%
    html_node("table.t") %>%
    html_table(fill = TRUE)

# give cols proper names
colnames(life_table) <- varnames

# data cleaning
life_table <- life_table %>%
    # get rid of crappy col names from scraping and phantom final row
    slice(-1, -n()) %>% 
    # convert everything from string to numeric after getting rid of comma in cols with life counts
    mutate_at(vars(contains("lives")), function(x) { gsub(",", "", x) }) %>%
    mutate_all(as.numeric)

# function to compute probability of death within a given span of years, conditional on
# specified sex, age, and span
deathwatch <- function(sex = "m", age = 50, span = 10) {
    
    # helper function to convert vector of probabilities of event occurrence in sequential
    # time steps to single probability of event's occurrence
    at_least_once <- function(vector) {
        
        p.conditional <- vector()
        
        for (i in seq_along(vector)) {
            
            if (i == 1) {
                
                p.conditional[i] <- vector[i]
                
            } else {
                
                p.conditional[i] <- vector[i] * (1 - sum(p.conditional[1:i - 1]))
                
            }
            
        }
        
        sigma <- sum(p.conditional)
        
        return(sigma)
        
    }
    
    if (sex == "m") {
        
        y <- life_table$male_death_p[life_table$exact_age %in% seq(age, age + (span - 1))]
        
        deathprob <- at_least_once(y)
        
        
    } else {
        
        y <- life_table$female_death_p[life_table$exact_age %in% seq(age, age + (span - 1))]
        
        deathprob <- at_least_once(y)
        
    }
    
    return(deathprob)
    
}

# USER INTERFACE ----

ui <- fluidPage(

    # Application title
    titlePanel("Morbid Curiosity"),

    sidebarLayout(

        sidebarPanel(
            radioButtons("sex", "Sex:",
                         choices = c("Female" = "f", "Male" = "m")),
            numericInput("age", "Current age:",
                         value = 50,
                         min = 1,
                         max = 100,
                         step = 1),
            numericInput("span", "Death within how many years?",
                         value = 5,
                         min = 1,
                         max = 20,
                         step = 1),
            hr(),
            helpText("Data source: U.S. Social Security Administration actuarial life tables (2016).")
            
        ),

        mainPanel(plotOutput("deathometer"))
    )

)

# SERVER LOGIC ----

server <- function(input, output) {
  
    output$deathometer <- renderPlot({

        # run helper function over user-selected inputs
        deathp <- deathwatch(sex = input$sex,
                             age = input$age,
                             span = input$span)

        # validate the inputs and throw error messages if they are nonsensical
        validate(
            need(input$age + input$span < 119, "Your query exceeds the maximum age in the life table. Please choose a lower age or shorter span or both."),
            need(input$age >= 0, "C'mon, man. You can't be negative years old."),
            need(input$span > 0, "Please choose a positive number for the time span to consider.")
        )

        # make a data frame that ggplot2 can easily use
        df_plot <- data.frame(variable = "Death",
                              percentage = round(deathp, 2),
                              label = sprintf("%s%%", round(deathp, 2) * 100),
                              title = "Chance of death",
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
