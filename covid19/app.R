library(shiny)
library(tidyverse)
library(lubridate)
library(rvest)
library(patchwork)
library(leaflet)

options(stringsAsFactors = FALSE)

## --- DATA PULLING AND PROCESSING ----

# https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series

confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") %>%
    pivot_longer(cols = matches("^X[0-9]{1,2}"), names_to = "date", values_to = "n_confirmed") %>%
    separate(date, c("month", "day", "year"), sep = "\\.") %>%
    mutate(month = gsub("X", "", month),
           year = paste0("20", year),
           date = lubridate::date(paste(year, month, day, sep = "-")) ) %>%
    dplyr::select(Country.Region, Province.State, Lat, Long, date, n_confirmed)

deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv") %>%
    pivot_longer(cols = matches("^X[0-9]{1,2}"), names_to = "date", values_to = "n_deaths") %>%
    separate(date, c("month", "day", "year"), sep = "\\.") %>%
    mutate(month = gsub("X", "", month),
           year = paste0("20", year),
           date = lubridate::date(paste(year, month, day, sep = "-")) ) %>%
    dplyr::select(Country.Region, Province.State, Lat, Long, date, n_deaths)

recovered <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv") %>%
    pivot_longer(cols = matches("^X[0-9]{1,2}"), names_to = "date", values_to = "n_recovered") %>%
    separate(date, c("month", "day", "year"), sep = "\\.") %>%
    mutate(month = gsub("X", "", month),
           year = paste0("20", year),
           date = lubridate::date(paste(year, month, day, sep = "-")) ) %>%
    dplyr::select(Country.Region, Province.State, Lat, Long, date, n_recovered)

full <- Reduce(function(...) merge(..., all = TRUE), list(confirmed, deaths, recovered))

# relabel this one so it's easier to find when alphabetized
full$Country.Region[full$Country.Region == "Mainland China"] <- "China (mainland)"

national <- full %>%
    group_by(Country.Region, date) %>%
    summarize_at(vars(starts_with("n_")), sum)

national_change <- national %>%
    mutate_at(vars(starts_with("n_")), function(x) x - lag(x)) %>%
    slice(-1)

global <- full %>%
    group_by(date) %>%
    summarize_at(vars(starts_with("n_")), sum)

global_change <- global %>%
    mutate_at(vars(starts_with("n_")), function(x) x - lag(x)) %>%
    slice(-1)    

global_sans_china <- full %>%
    filter(Country.Region != "China (mainland)") %>%
    group_by(date) %>%
    summarize_at(vars(starts_with("n_")), sum)

global_sans_china_change <- global_sans_china %>%
    mutate_at(vars(starts_with("n_")), function(x) x - lag(x)) %>%
    slice(-1) 

# get links to WHO situation reports and give them pubdates as names
sitreps <- read_html("https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports/") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    grep("[0-9]{8}-sitrep-[0-9]{1,3}", ., value = TRUE) %>%
    sprintf("https://www.who.int%s", .) %>%
    unique(.)

sitrep_dates <- sitreps %>% str_extract("[0-9]{8}") %>% ymd()

names(sitreps) <- sitrep_dates


# ---- USER INTERFACE ----

ui <- shinyUI(fluidPage(

    # Application title
    titlePanel("Tracking COVID-19"),

    mainPanel(

        tabsetPanel(

            tabPanel("Global trend",

                fluidPage(

                    fluidRow(

                         column(width = 3,

                             radioButtons("china",
                                          label = "",
                                          choices = c("including China", "excluding China"),
                                          selected = "including China")

                         ),

                         column(width = 3,
  
                             radioButtons("trendtype",
                                           label = "",
                                           choices = c("cumulative count", "new cases"),
                                           selected = "cumulative count")

                         )

                    ),

                    fluidRow(

                        plotOutput("plot_global", width = "100%", height = "350px")

                    )

                )

            ),

            tabPanel("Trend by country",

                fluidPage(

                    fluidRow(

                        column(width = 3,
                   
                            selectInput("country",
                                        label = "",
                                        choices = sort(unique(national$Country.Region)),
                                        selected = "China (mainland)"),

                        ),

                        column(width = 3,
  
                            radioButtons("metric",
                                         label = "",
                                         choices = c("cumulative count", "new cases"),
                                         selected = "cumulative count")

                        )

                    ),

                    fluidRow(

                        plotOutput("plot_national", width = "100%", height = "350px")

                    )

                )

            ),

            tabPanel("World map",

                leafletOutput("map_global", width = "100%", height = "500px")

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

                    div(p(strong("Creator:"), a(href = "https://github.com/ulfelder", "Jay Ulfelder")), 
                        p(strong("R Packages:"), "shiny, tidyverse, lubridate, patchwork,  leaflet"),
                        p(strong("Data Source:"), a("Johns Hopkins University CSEE", href = "https://github.com/CSSEGISandData/COVID-19")),
                        br(),
                        p("On the world map, the area of the circles is proportionate to the counts (the radius is the
                          square root of the count divided by pi), and the counts of deaths and recovered cases are nested
                          in the count of confirmed cases."),
                        style = "font-family: courier;")

                )

            )

        )

    )

))

               
## SERVER LOGIC

server <- function(input, output) {

    output$plot_global <- renderPlot({

        if(input$china == "including China" & input$trendtype == "cumulative count") {

            global %>%
                # recompute confirmed as net of deaths and recovered for stacked plotting
                mutate(n_confirmed = n_confirmed - n_deaths - n_recovered) %>%
                pivot_longer(-date, names_to = "casetype", values_to = "n") %>%
                mutate(casetype = gsub("n_", "", casetype)) %>%
                ggplot(aes(x = date, y = n, fill = casetype)) +
                    geom_col(alpha = 2/3) +
                    theme_minimal() +
                    scale_fill_manual(values = c("gray75", "red", "dodgerblue")) +
                    labs(caption = "Data source: JHU CSEE") +
                    theme(axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          legend.position = "bottom")

        } else if (input$china == "excluding China" & input$trendtype == "cumulative count") {

            global_sans_china %>%
                # recompute confirmed as net of deaths and recovered for stacked plotting
                mutate(n_confirmed = n_confirmed - n_deaths - n_recovered) %>%
                pivot_longer(-date, names_to = "casetype", values_to = "n") %>%
                mutate(casetype = gsub("n_", "", casetype)) %>%
                ggplot(aes(x = date, y = n, fill = casetype)) +
                    geom_col(alpha = 2/3) +
                    theme_minimal() +
                    scale_fill_manual(values = c("gray75", "red", "dodgerblue")) +
                    labs(caption = "Data source: JHU CSEE") +
                    theme(axis.title.x = element_blank(),
                          axis.title.y = element_blank(),
                          legend.position = "bottom")

        } else if (input$china == "including China" & input$trendtype == "new cases") {

            new_confirmed <- global_change %>%
                ggplot(aes(x = date, y = n_confirmed)) +
                    geom_col(alpha = 2/3, fill = "gray75") +
                    theme_minimal() +
                    labs(title = "New confirmed cases",
                         caption = "Data source: JHU CSEE") +
                    theme(axis.title.x = element_blank(),
                          axis.title.y = element_blank())

            new_deaths <- global_change %>%
                ggplot(aes(x = date, y = n_deaths)) +
                    geom_col(alpha = 2/3, fill = "red") +
                    theme_minimal() +
                    labs(title = "New deaths",
                         caption = "Data source: JHU CSEE") +
                    theme(axis.title.x = element_blank(),
                          axis.title.y = element_blank())

            new_confirmed / new_deaths

        } else {

            new_confirmed <- global_sans_china_change %>%
                ggplot(aes(x = date, y = n_confirmed)) +
                    geom_col(alpha = 2/3, fill = "gray75") +
                    theme_minimal() +
                    labs(title = "New confirmed cases",
                         caption = "Data source: JHU CSEE") +
                    theme(axis.title.x = element_blank(),
                          axis.title.y = element_blank())

            new_deaths <- global_sans_china_change %>%
                ggplot(aes(x = date, y = n_deaths)) +
                    geom_col(alpha = 2/3, fill = "red") +
                    theme_minimal() +
                    labs(title = "New deaths",
                         caption = "Data source: JHU CSEE") +
                    theme(axis.title.x = element_blank(),
                          axis.title.y = element_blank())

            new_confirmed / new_deaths

        }

    })

    output$plot_national <- renderPlot({

        cumulative <- national %>%
            mutate(n_confirmed = n_confirmed - n_deaths - n_recovered) %>%
            pivot_longer(n_confirmed:n_recovered, names_to = "casetype", values_to = "n") %>%
            filter(Country.Region == input$country) %>%
            mutate(casetype = gsub("n_", "", casetype)) %>%
            ggplot(aes(x = date, y = n, fill = casetype)) +
                geom_col(alpha = 2/3) +
                theme_minimal() +
                scale_fill_manual(values = c("gray75", "red", "dodgerblue")) +
                labs(caption = "Data source: JHU CSEE") +
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      legend.position = "bottom")

        new_confirmed <- national_change %>%
            filter(Country.Region == input$country) %>%
            ggplot(aes(x = date, y = n_confirmed)) +
                geom_col(alpha = 2/3, fill = "gray75") +
                theme_minimal() +
                labs(title = "New confirmed cases",
                     caption = "Data source: JHU CSEE") +
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank())

        new_deaths <- national_change %>%
            filter(Country.Region == input$country) %>%
            ggplot(aes(x = date, y = n_deaths)) +
                geom_col(alpha = 2/3, fill = "red") +
                theme_minimal() +
                labs(title = "New deaths",
                     caption = "Data source: JHU CSEE") +
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank())

        if (input$metric == "cumulative count") { print(cumulative) }

        else { print( new_confirmed / new_deaths ) }

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
                      colors = c("gray75", "dodgerblue", "red"), opacity = 1/10,
                      labels = c("confirmed", "recovered", "deaths"))

    })

    output$frame <- renderUI({

        tags$iframe(style = "height:400px; width:100%; scrolling=yes", src = sitreps[input$sitrep_date])

    })

}

## RUN THE APP

shinyApp(ui, server)
