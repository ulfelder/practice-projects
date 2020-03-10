library(shiny)
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(readxl)
library(leaflet)

options(stringsAsFactors = FALSE)

# load script with data-loading helper functions and named vectors
source("r/acledr.r")

# run the acled_grid function now so we have the master frame of country-months
acled_grid()

## USER INTERFACE

ui <- fluidPage(theme = "bootstrap.css", title = "ACLED Explorer",

  sidebarLayout(

    sidebarPanel(

      p("Use this tool to explore data on armed conflict and protest events collected by the",
        a("Armed Conflict Location and Events Data (ACLED)", href = "https://www.acleddata.com/"),
        "program."),

      br(),

      selectInput("country", "Pick a country:",
                  choices = named_acled_isos,
                  selected = 566),  # Nigeria

      selectInput("indicator", "Pick an indicator:",
                  choices = named_acled_indicators,  # allow multiple at once? how?
                  selected = "n_deaths"),  

      sliderInput("years", "Pick a range of years:",
                  min = 1997, max = 2020, value = c(1997,2020), step = 1, sep = "")  # should programmatically limit this to available range by country

    ),

    mainPanel(

        fluidRow(plotOutput("plot_trend", width = "100%", height = "200px")),

        fluidRow(position = "center", leafletOutput("map_country", width = "300px", height = "300px"))

    )

  )

)
               
## SERVER LOGIC

server <- function(input, output) {

  country_data <- reactive({ acled_country(input$country) })

  my_years <- reactive({ seq(input$years[1], input$years[2]) })

  output$plot_trend <- renderPlot({

    country_name <- names(named_acled_isos)[which(named_acled_isos == input$country)]

    selected_indicator <- names(named_acled_indicators)[which(named_acled_indicators == input$indicator)]

    country_data() %>%
      acled_como(.) %>%
      filter(category == input$indicator & year %in% my_years()) %>%
      mutate(date = date(paste(year, month, "1", sep = "-"))) %>%
      ggplot(aes(x = date, y = n)) +
        geom_col() +
        theme_minimal() +
        labs(title = country_name,
             subtitle = paste0(selected_indicator, " by month")) +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank())

  })

  output$map_country <- renderLeaflet({

    if (input$indicator %in% grep("deaths", named_acled_indicators, value = TRUE)) {

      map_data <- country_data() %>%
        mutate(year = as.numeric(year), fatalities = as.numeric(fatalities)) %>%
        filter(year %in% seq(input$years[1], input$years[2])) %>%
        group_by(latitude = as.numeric(latitude), longitude = as.numeric(longitude)) %>%
        summarise(logtot = log1p(sum(fatalities, na.rm = TRUE))) %>%
        ungroup()

    } else {

      map_data <- country_data() %>%
        mutate(year = as.numeric(year)) %>%
        filter(year %in% seq(input$years[1], input$years[2])) %>%
        group_by(latitude = as.numeric(latitude), longitude = as.numeric(longitude)) %>%
        summarise(logtot = log1p(n())) %>%
        ungroup()

    }

    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(data = map_data, lat = ~latitude, lng = ~longitude, radius = ~logtot,
                       color = "red", opacity = 0, fillOpacity = 1/2)    

  })

}

## RUN THE APP

shinyApp(ui, server)
