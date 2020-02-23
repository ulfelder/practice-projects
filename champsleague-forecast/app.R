library(shiny)
library(devtools)
library(tidyverse)
library(rvest)
library(ggridges)
library(knitr)
library(kableExtra)

options(stringsAsFactors = FALSE)

devtools::source_url("https://raw.githubusercontent.com/ulfelder/practice-projects/master/odds-scraper.R")

# ---- DATA PREP ----

# scrape and prep aggregated bookmakers' predictions
bookies <- scrape_odds("https://www.oddschecker.com/football/champions-league/winner")

tbook <- bookies %>%
    filter(source == "mean") %>%
    transmute(team = target, p_book = value)

# scrape and clean 538's forecast table
t538 <- read_html("https://projects.fivethirtyeight.com/soccer-predictions/champions-league/") %>%
    html_node("#forecast-table") %>%
    html_table(fill = TRUE, header = FALSE) %>%
    slice(-1:-3) %>%
    transmute(team = X1, p_538 = X9) %>%
    mutate(p_538 = replace(p_538, grepl("^<1%|—", p_538), "0")) %>%
    mutate(p_538 = as.numeric(gsub("%", "", p_538))/100)  # need to recalibrate these to sum to 1

# force matching team labels, using 538's as master b/c it's more complete
tbook <- mutate(tbook, team = str_replace_all(team, c("Atletico Madrid" = "Atlético Madrid",
                                                      "Borussia Dortmund" = "Dortmund",
                                                      "Man City" = "Man. City",
                                                      "Olympiakos" = "Olympiacos",
                                                      "Red Star Belgrade" = "Red Star",
                                                      "Salzburg" = "RB Salzburg")))

# merge the two tables
t_blend <- left_join(t538, tbook)

# get the unweighted average of the probabilities from the two sources
t_blend <- t_blend %>% rowwise() %>% mutate(p_blend = lift_vd(mean, na.rm = TRUE)(p_538, p_book))

# recalibrate those unweighted averages to sum to 1
t_blend$p_blend = t_blend$p_blend/sum(t_blend$p_blend)


# --- UI ----

ui <- fluidPage(

    title = "2020 Champions League forecast",

    fluidRow(

        column(width = 4, 

            br(),

            plotOutput("plot_ridges")

        ),

        column(width = 4, offset = 1, 

            tableOutput("table_forecast")

        )

    )

)


# ---- SERVER ----

server <- function(input, output) {

    output$plot_ridges <- renderPlot({

        plot_odds(bookies, "Distribution of probabilities implied by bookies' odds")

    })

    output$table_forecast <- function() {

        t_blend %>%
            arrange(-p_blend) %>%
            mutate(p_blend = round(p_blend, 3)) %>%
            select(team, prob = p_blend) %>%
            knitr::kable("html", caption = "Blended forecast") %>%
            kable_styling(full_width = FALSE)

    }

}

# ---- HIT IT ----

shinyApp(ui, server)
