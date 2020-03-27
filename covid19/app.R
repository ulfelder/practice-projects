library(shiny)
library(tidyverse)
library(lubridate)
library(rvest)
library(patchwork)
library(leaflet)
library(plotly)
library(scales)

options(stringsAsFactors = FALSE)

# run the scripts that load and munge the data from JHU CSEE
walk(paste0("R/", grep("data_", list.files("R/"), value = TRUE)), source)

##################################################
##                      UI                      ##
##################################################

ui <- shinyUI(fluidPage(
  
  titlePanel("Tracking COVID-19"),
  
  mainPanel(
    
    tabsetPanel(
      
      tabPanel("Global",
               
               fluidPage(
                 
                 fluidRow(
                   
                   column(width = 3, 
                          radioButtons("china",
                                       label = "",
                                       choices = c("including China", "excluding China"),
                                       selected = "excluding China")),
                   
                   column(width = 3,
                          radioButtons("trendtype",
                                       label = "",
                                       choices = c("cumulative count", "new cases"),
                                       selected = "cumulative count"))
                   
                 ),
                 
                 fluidRow(
                   
                   plotOutput("plot_global", width = "100%", height = "350px")
                   
                 )
                 
               )
               
      ),
      
      tabPanel("By country",
               
               fluidPage(
                 
                 fluidRow(
                   
                   column(width = 3,
                          selectInput("country",
                                      label = "",
                                      choices = sort(unique(national$Country.Region)),
                                      selected = "US")),
                   
                   column(width = 3,
                          radioButtons("metric",
                                       label = "",
                                       choices = c("cumulative count", "new cases"),
                                       selected = "cumulative count"))
                   
                 ),
                 
                 fluidRow(
                   
                   plotOutput("plot_national", width = "100%", height = "350px")
                   
                 )
                 
               )
               
      ),
      
      tabPanel("By U.S. state",
               
               fluidPage(
                 
                 fluidRow(
                   
                   column(width = 3,
                          selectInput("state",
                                      label = "",
                                      choices = state_names,
                                      selected = "New York")),
                   
                   column(width = 3,
                          radioButtons("metric_state",
                                       label = "",
                                       choices = c("cumulative count", "new cases"),
                                       selected = "cumulative count"))
                   
                 ),
                 
                 fluidRow(
                   
                   plotOutput("plot_state", width = "100%", height = "350px")
                   
                 )
                 
               )
               
      ),
      
      tabPanel("World map",
               
               leafletOutput("map_global", width = "100%", height = "500px")
               
      ),
      
      tabPanel("Growth rates",
               
               fluidPage(
                 
                 br(),
                 
                 fluidRow(                
                   
                   column(width = 3,
                          
                          radioButtons("metric_rate", label = "", choices = c("confirmed cases", "deaths")),
                          
                          div(p("Double-click on a country name in the legend to see only that country's trace. 
                     Single-click on a country name in the legend to remove that country's trace from 
                     the plot (and click again to add it back). Hover on a line to see details for 
                     that country at that point in time."),
                              p("Use the selector below to set the benchmark growth rate (gray line) for comparison.")),
                          
                          br(),
                          
                          numericInput("growth_rate", label = "Growth rate:",
                                       value = 0.3 , min = 0, max = 1, step = 0.1)),
                   
                   column(width = 9,
                          plotlyOutput("plot_rate", height = "450px"))
                   
                 )
                 
               )
               
      ),
      
      tabPanel("WHO Situation Reports",
               
               selectInput("sitrep_date",
                           label = "",
                           choices = sitrep_dates,
                           selected = sitrep_dates[1],
                           width = "200px"),
               
               htmlOutput("frame")
               
      ),
      
      tabPanel("About",
               
               fluidPage(
                 
                 br(),
                 
                 div(p(strong("Creator:"), a(href = "https://github.com/ulfelder", "Jay Ulfelder", target = "_blank")), 
                     p(strong("R Packages:"), "shiny, tidyverse, rvest, lubridate, patchwork,  scales, plotly, leaflet"),
                     p(strong("Data Source:"), a("Johns Hopkins University CSEE", href = "https://github.com/CSSEGISandData/COVID-19", target = "_blank")),
                     br(),
                     p("On the world map, the area of the circles is proportionate to the counts (the radius is half the
                square root of the latest count divided by pi), and the counts of deaths are nested in the counts of 
                confirmed cases."),
                     style = "font-family: courier;")
                 
               )
               
      )
      
    )
    
  )
  
))

##################################################
##                   SERVER                     ##
##################################################

server <- function(input, output, session) {
  
  # helper functions to make plots of daily counts of confirmed cases and deaths
  fig_confirmed <- function(dat) {
    
    require(ggplot2)
    require(scales)
    
    ggplot(dat, aes(x = date, y = n_confirmed)) +
      geom_col(alpha = 2/3, fill = "gray75") +
      theme_minimal() +
      scale_y_continuous(position = "right", labels = comma) +
      labs(title = "Confirmed cases",
           caption = "Data source: JHU CSEE") +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
  }
  
  fig_deaths <- function(dat) {
    
    require(ggplot2)
    require(scales)
    
    ggplot(dat, aes(x = date, y = n_deaths)) +
      geom_col(alpha = 2/3, fill = "red") +
      theme_minimal() +
      scale_y_continuous(position = "right", labels = comma) +
      labs(title = "Deaths",
           caption = "Data source: JHU CSEE") +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
  }
  
  # tab 1: plot of global trends
  output$plot_global <- renderPlot({
    
    if(input$china == "including China" & input$trendtype == "cumulative count") {
      
      df <- global
      
    } else if (input$china == "excluding China" & input$trendtype == "cumulative count") {
      
      df <- global_sans_china
      
    } else if (input$china == "including China" & input$trendtype == "new cases") {
      
      df <- global_change
      
    } else {
      
      df <- global_sans_china_change
      
    }
    
    confirmed <- fig_confirmed(df)
    deaths <- fig_deaths(df)
    
    confirmed / deaths
    
  })
  
  # tab 2: plot of national trends   
  output$plot_national <- renderPlot({
    
    if (input$metric == "cumulative count") {
      
      df <- filter(national, Country.Region == input$country)
      
    } else {
      
      df <- filter(national_change, Country.Region == input$country)
      
    }
    
    confirmed <- fig_confirmed(df)
    deaths <- fig_deaths(df)
    
    confirmed / deaths  
    
  })
  
  # tab 3: plot of u.s. state trends   
  output$plot_state <- renderPlot({
    
    if (input$metric_state == "cumulative count") {
      
      df <- filter(usa, state == input$state)
      
    } else {
      
      df <- filter(usa_change, state == input$state)
      
    }
    
    confirmed <- fig_confirmed(df)
    deaths <- fig_deaths(df)
    
    confirmed / deaths  
    
  })
  
  output$map_global <- renderLeaflet({
    
    selected_date <- max(full$date)
    
    mapdat <- full %>%
      filter(date == selected_date) %>%
      # recompute counts to radii of circles with proportionate area; divide by 2 to make big ones fit better
      mutate_at(vars(starts_with("n_")), ~ (sqrt(./pi))/2 )
    
    # shift focal point to italy, now hardest hit and allows vis of north america and asia
    focal <- c(mapdat$Lat[mapdat$Country.Region == "Italy"], mapdat$Long[mapdat$Country.Region == "Italy"])
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lat = focal[1], lng = focal[2], zoom = 2) %>%
      addCircleMarkers(data = mapdat, lat = ~Lat, lng = ~Long, radius = ~n_confirmed,
                       color = "gray75", opacity = 0, fillOpacity = 1/4) %>%
      addCircleMarkers(data = mapdat, lat = ~Lat, lng = ~Long, radius = ~n_deaths,
                       color = "red", opacity = 0, fillOpacity = 1/4) %>%
      addLegend(position = "bottomright",
                colors = c("gray75", "red"), opacity = 1,
                labels = c("confirmed", "deaths"))
    
  })
  
  output$plot_rate <- renderPlotly({
    
    if(input$metric_rate == "confirmed cases") {
      
      dat <- national %>%
        filter(Country.Region != "Cruise Ship" & Country.Region != "Diamond Princess") %>%
        filter(n_confirmed >= 100) %>%
        mutate(level = log10(n_confirmed)) %>%
        select(Country.Region, date, level) %>%
        group_by(Country.Region) %>%
        mutate(days_since_100th_case = row_number() - 1) %>%
        mutate(hover_label = sprintf("%s: %s after %s days", Country.Region, round(10^level, 0), days_since_100th_case))
      
      p <- plot_ly(type = "scatter", mode = "lines")
      countries <- sort(unique(dat$Country.Region))
      for (i in countries) {
        tmp <- filter(dat, Country.Region == i)
        p <- p %>% add_lines(data = tmp, x = ~days_since_100th_case, y = ~level,
                             hoverinfo = "text", text = ~hover_label,
                             alpha = 1/2, name = i)
      }
      
      p %>% 
        layout(xaxis = list(title = "days since 100+ confirmed cases"),
               yaxis = list(title = "confirmed cases (logged)", range = c(2,6)),
               shapes = list(type = "line", line = list(color = I("gray75"), width = 1.5),
                             x0 = 0, x1 = max(dat$days_since_100th_case),
                             y0 = 2, y1 = log10(100 * (1 + input$growth_rate)^max(dat$days_since_100th_case)),
                             opacity = 1/3, layer = "below"),
               legend = list(x = 100, y = 0.5))
      
    } else {
      
      dat <- national %>%
        filter(Country.Region != "Cruise Ship" & Country.Region != "Diamond Princess") %>%
        filter(n_deaths >= 10) %>%
        mutate(level = log10(n_deaths)) %>%
        select(Country.Region, date, level) %>%
        group_by(Country.Region) %>%
        mutate(days_since_10th_case = row_number() - 1) %>%
        mutate(hover_label = sprintf("%s: %s after %s days", Country.Region, round(10^level, 0), days_since_10th_case))
      
      p <- plot_ly(type = "scatter", mode = "lines")
      countries <- sort(unique(dat$Country.Region))
      for (i in countries) {
        tmp <- filter(dat, Country.Region == i)
        p <- p %>% add_lines(data = tmp, x = ~days_since_10th_case, y = ~level,
                             hoverinfo = "text", text = ~hover_label,
                             alpha = 1/2, name = i)
      }
      
      p %>% 
        layout(xaxis = list(title = "days since 10+ deaths"),
               yaxis = list(title = "total deaths (logged)", range = c(1,5)),
               shapes = list(type = "line", line = list(color = I("gray75"), width = 1.5),
                             x0 = 0, x1 = max(dat$days_since_10th_case),
                             y0 = 1, y1 = log10(10 * (1 + input$growth_rate)^max(dat$days_since_10th_case)),
                             opacity = 1/3, layer = "below"),
               legend = list(x = 100, y = 0.5))
      
    }
    
  })
  
  output$frame <- renderUI({
    
    tags$iframe(style = "height:400px; width:100%; scrolling=yes", src = sitreps[input$sitrep_date])
    
  })
  
}

## RUN THE APP

shinyApp(ui, server)
