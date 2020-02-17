library(tidyverse)
library(lubridate)
library(shiny)
library(leaflet)

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

            tabPanel("Trend worldwide", plotOutput("plot_global", width = "100%", height = "400px")),

            tabPanel("Trend by country", plotOutput("plot_national", width = "100%", height = "500px")),

            tabPanel("World map of cumulative counts", leafletOutput("map_global", width = "100%", height = "500px"))

        )

    )

)

## SERVER LOGIC

server <- function(input, output) {

    output$plot_global <- renderPlot({

        global %>%
            # recompute confirmed as net of deaths and recovered for stacked plotting
            mutate(n_confirmed = n_confirmed - n_deaths - n_recovered) %>%
            pivot_longer(-date, names_to = "casetype", values_to = "n") %>%
            mutate(casetype = gsub("n_", "", casetype)) %>%
            ggplot(aes(x = date, y = n, fill = casetype)) +
                geom_col(alpha = 2/3) +
                theme_minimal() +
                scale_fill_manual(values = c("gray75", "red", "dodgerblue")) +
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
                      legend.title = element_blank(),
                      legend.position = "bottom") +
                labs(title = sprintf("COVID-19 cases by country as of %s", max(national$date)),
                     caption = "Data source: JHU CSEE")

    })

    output$map_global <- renderLeaflet({

        selected_date <- max(full$date)

        mapdat <- full %>%
            filter(date == selected_date) %>%
            rename(latitude = Lat, longitude = Long) %>%
            # recompute counts to radii of circles with proportionate area
            mutate_at(vars(starts_with("n_")), ~ sqrt(./pi) )

        # get coordinates of province at epicenter of epidemic to use as focal point for map
        focal <- c(mapdat$latitude[mapdat$Province.State == "Hubei"], mapdat$longitude[mapdat$Province.State == "Hubei"])

        leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lat = focal[1], lng = focal[2], zoom = 2) %>%
            addCircleMarkers(data = mapdat, lat = ~latitude, lng = ~longitude, radius = ~n_confirmed,
                             color = "gray75", opacity = 0, fillOpacity = 1/4) %>%
            addCircleMarkers(data = mapdat, lat = ~latitude, lng = ~longitude, radius = ~n_recovered,
                             color = "dodgerblue", opacity = 0, fillOpacity = 1/4) %>%
            addCircleMarkers(data = mapdat, lat = ~latitude, lng = ~longitude, radius = ~n_deaths,
                             color = "red", opacity = 0, fillOpacity = 1/4) %>%
            addLegend(position = "bottomright",
                      colors = c("gray75", "dodgerblue", "red"), opacity = 1/3,
                      labels = c("confirmed", "recovered", "deaths"))

    })

}

## RUN THE APP

shinyApp(ui, server)
