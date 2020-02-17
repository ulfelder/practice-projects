library(tidyverse)
library(shiny)
library(lubridate)

options(stringsAsFactors = FALSE)

## DATA PULLING AND CLEANING

# https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series

confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") %>%
    pivot_longer(cols = matches("^X[1-2]{1}"), names_to = "date", values_to = "n_confirmed") %>%
    separate(date, c("month", "day", "year"), sep = "\\.") %>%
    mutate(month = gsub("X", "", month),
           year = paste0("20", year),
           date = lubridate::date(paste(year, month, day, sep = "-")) ) %>%
    select(Country.Region, Province.State, Lat, Long, date, n_confirmed)

deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv") %>%
    pivot_longer(cols = matches("^X[1-2]{1}"), names_to = "date", values_to = "n_deaths") %>%
    separate(date, c("month", "day", "year"), sep = "\\.") %>%
    mutate(month = gsub("X", "", month),
           year = paste0("20", year),
           date = lubridate::date(paste(year, month, day, sep = "-")) ) %>%
    select(Country.Region, Province.State, Lat, Long, date, n_deaths)

recovered <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv") %>%
    pivot_longer(cols = matches("^X[1-2]{1}"), names_to = "date", values_to = "n_recovered") %>%
    separate(date, c("month", "day", "year"), sep = "\\.") %>%
    mutate(month = gsub("X", "", month),
           year = paste0("20", year),
           date = lubridate::date(paste(year, month, day, sep = "-")) ) %>%
    select(Country.Region, Province.State, Lat, Long, date, n_recovered)

full <- Reduce(function(...) merge(..., all = TRUE), list(confirmed, deaths, recovered))

# relabel this one so it's easier to find when alphabetized
full$Country.Region[full$Country.Region == "Mainland China"] <- "China (mainland)"

national <- full %>%
    group_by(Country.Region, date) %>%
    summarize_at(vars(starts_with("n_")), sum)

global <- full %>%
    group_by(date) %>%
    summarize_at(vars(starts_with("n_")), sum)

## USER INTERFACE

ui <- fluidPage(

    # Application title
    titlePanel("Tracking COVID-19"),

    mainPanel(

        tabsetPanel(

            tabPanel("Worldwide", plotOutput("plot_global", width = "750px", height = "400px")),

            tabPanel("By country", plotOutput("plot_national", width = "1200px", height = "500px"))

        )

    )

)

## SERVER LOGIC

server <- function(input, output) {

    output$plot_global <- renderPlot({

        global %>%
            pivot_longer(-date, names_to = "casetype", values_to = "n") %>%
            filter(casetype != "n_recovered") %>%
            mutate(casetype = gsub("n_", "", casetype)) %>%
            ggplot(aes(x = date, y = n, fill = casetype)) +
                geom_col() +
                theme_minimal() +
                scale_fill_manual(values = c("gray75", "red")) +
                labs(title = sprintf("COVID-19 cases worldwide as of %s", max(global$date)),
                     caption = "Data source: JHU CSEE") +
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      legend.position = "bottom")

    })

    output$plot_national <- renderPlot({

        national %>%
            pivot_longer(n_confirmed:n_recovered, names_to = "casetype", values_to = "n") %>%
            filter(casetype != "n_recovered") %>%
            mutate(casetype = gsub("n_", "", casetype)) %>%
            ggplot(aes(x = date, y = n, fill = casetype)) +
                geom_col() +
                theme_minimal() +
                scale_fill_manual(values = c("gray75", "red")) +
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank()) +
                facet_wrap(vars(Country.Region),
                           scales = "free_y",
                           ncol = 6) +
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      legend.title = element_blank()) +
                labs(title = sprintf("COVID-19 cases by country as of %s", max(national$date)),
                     caption = "Data source: JHU CSEE")

    })

}

## RUN THE APP

shinyApp(ui, server)
