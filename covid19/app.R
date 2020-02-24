library(tidyverse)
library(lubridate)
library(shiny)
library(leaflet)
library(fitdistrplus)
library(rvest)
library(bbmle)

options(stringsAsFactors = FALSE)
set.seed(20912)

## DATA PULLING AND CLEANING

# https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series

confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") %>%
    pivot_longer(cols = matches("^X[1-2]{1}"), names_to = "date", values_to = "n_confirmed") %>%
    separate(date, c("month", "day", "year"), sep = "\\.") %>%
    mutate(month = gsub("X", "", month),
           year = paste0("20", year),
           date = lubridate::date(paste(year, month, day, sep = "-")) ) %>%
    dplyr::select(Country.Region, Province.State, Lat, Long, date, n_confirmed)

deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv") %>%
    pivot_longer(cols = matches("^X[1-2]{1}"), names_to = "date", values_to = "n_deaths") %>%
    separate(date, c("month", "day", "year"), sep = "\\.") %>%
    mutate(month = gsub("X", "", month),
           year = paste0("20", year),
           date = lubridate::date(paste(year, month, day, sep = "-")) ) %>%
    dplyr::select(Country.Region, Province.State, Lat, Long, date, n_deaths)

recovered <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv") %>%
    pivot_longer(cols = matches("^X[1-2]{1}"), names_to = "date", values_to = "n_recovered") %>%
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

global <- full %>%
    group_by(date) %>%
    summarize_at(vars(starts_with("n_")), sum)

# Estimating case fatality ratio (CFR) of COVID-19
# Christian L. Althaus, 19 February 2020
# https://github.com/calthaus/ncov-cfr

# Estimating distribution from onset of symptoms to death
# Linton et al. (https://doi.org/10.3390/jcm9020538)
linton <- read.csv("https://raw.githubusercontent.com/calthaus/ncov-cfr/master/data/linton_supp_tableS1_S2_8Feb2020.csv")
linton <- dmy(linton$Death) - dmy(linton$Onset)
linton <- as.numeric(na.omit(linton))
fit_linton <- fitdist(linton, "gamma")

linton_df <- data.frame(days = seq(50), prob_density = dgamma(seq(50), coef(fit_linton)[[1]], coef(fit_linton)[[2]]))
linton_mean <- coef(fit_linton)[[1]]/coef(fit_linton)[[2]]
linton_median <- qgamma(0.5, coef(fit_linton)[[1]], coef(fit_linton)[[2]])

# NOT RUN: plot of distribution of estimated times from onset of symptoms to death
# ggplot(linton_df, aes(x = days, y = prob_density)) +
#     geom_density(stat = "identity", fill = "indianred", alpha = 0.5) +
#     theme_minimal() +
#     labs(title = "Probability distribution of esimtated time from onset of symptoms to death",
#          caption = "Analysis by Christian Althaus. See: https://github.com/calthaus/ncov-cfr",
#          x = "time from onset to death (days)", y = "probability density") +
#     geom_segment(x = linton_mean, xend = linton_mean, y = 0, yend = ceiling(max(df$prob_density))) +
#     annotate("text", x = linton_mean + 1, y = max(df$prob_density), hjust = 0, label = sprintf("mean = %s", round(linton_mean, 2)))

# Likelihood and expected mortality helper function
nll <- function(cfr, death_shape, death_rate) {
    cfr <- plogis(cfr)
    expected <- numeric(n_days)
    for(i in days) {
        for(j in 1:n_cases) {
            d <- i - onset[j]
            if(d >= 0) {
                expected[i] <- expected[i] + cfr*diff(pgamma(c(d - 0.5, d + 0.5), shape = death_shape, rate = death_rate))
            }
        }
    }
    ll <- sum(dpois(deaths, expected, log = TRUE))
    return(-ll)
}

# Analyze observed COVID-19 cases outside China
# Source: WHO, ECDC and international media

# scrape links to data sets in Althaus's repo
datlinks <- read_html("https://github.com/calthaus/ncov-cfr/tree/master/data") %>%
    html_nodes(".js-navigation-open") %>%
    html_attr("title") %>%
    grep("^ncov_cases_", ., value = TRUE)

# here's what we'd do if we wanted all the daily updates, not just the latest
# casedat <- map(datlinks, ~ read.csv(sprintf("https://raw.githubusercontent.com/calthaus/ncov-cfr/master/data/%s", .))) %>%
#     map( ~ mutate(., date = date(date)))

# fetch and munge latest data from Althaus's repo
last_link <- datlinks[length(datlinks)]
export <- read.csv(sprintf("https://raw.githubusercontent.com/calthaus/ncov-cfr/master/data/%s", last_link), stringsAsFactors = FALSE)
begin <- date(dplyr::first(export$date))
cases <- export$cases
deaths <- export$deaths
n_cases <- sum(cases)
n_deaths <- sum(deaths)
n_days <- length(cases)
days <- seq(n_days)
interval <- seq(1, n_days + 7, 7)
onset <- rep(days, cases)
# fit the model
free <- c(cfr = 0)
fixed <- c(death_shape = coef(fit_linton)[[1]], death_rate = coef(fit_linton)[[2]])
fit <- mle2(nll, start = as.list(free), fixed = as.list(fixed), method = "Brent", lower = -100, upper = 100)
cfr_est <- round(100 * plogis(coef(fit)[1]), 1)
cfr_ci <- round(100 * plogis(confint(fit)), 1)
# simulations for plotting
cfr_simulations <- sort(plogis(rnorm(10000, mean = coef(fit)[1], sd = sqrt(vcov(fit)))))

# get links to WHO situation reports and give them pubdates as names
sitreps <- read_html("https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports/") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    grep("[0-9]{8}-sitrep-[0-9]{1,3}", ., value = TRUE) %>%
    sprintf("https://www.who.int%s", .) %>%
    unique(.)

sitrep_dates <- sitreps %>%
    str_extract("[0-9]{8}") %>%
    ymd()

names(sitreps) <- sitrep_dates


## USER INTERFACE

ui <- fluidPage(

    # Application title
    titlePanel("Tracking COVID-19"),

    mainPanel(

        tabsetPanel(

            tabPanel("Global trend",

                br(),

                plotOutput("plot_global", width = "100%", height = "400px")

            ),

            tabPanel("Trend by country",

                selectInput("country",
                            label = "",
                            choices = sort(unique(national$Country.Region)),
                            selected = "China (mainland)",
                            width = "200px"),

                plotOutput("plot_national", width = "100%", height = "350px")

            ),

            tabPanel("World map",

                leafletOutput("map_global", width = "100%", height = "500px")

            ),

            tabPanel("Case fatality rate",

                br(),

                plotOutput("plot_cfr", width = "80%", height = "400px")

            ),

            tabPanel("WHO Situation Reports",

                selectInput("sitrep_date",
                            label = "",
                            choices = sitrep_dates,
                            selected = sitrep_dates[1],
                            width = "200px"),
                     
                htmlOutput("frame")

            )

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
                labs(title = sprintf("Cumulative daily count of COVID-19 cases worldwide as of %s", max(global$date)),
                     caption = "Data source: JHU CSEE") +
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      legend.position = "bottom")

    })

    output$plot_national <- renderPlot({

        national %>%
            mutate(n_confirmed = n_confirmed - n_deaths - n_recovered) %>%
            pivot_longer(n_confirmed:n_recovered, names_to = "casetype", values_to = "n") %>%
            filter(Country.Region == input$country) %>%
            mutate(casetype = gsub("n_", "", casetype)) %>%
            ggplot(aes(x = date, y = n, fill = casetype)) +
                geom_col(alpha = 2/3) +
                theme_minimal() +
                scale_fill_manual(values = c("gray75", "red", "dodgerblue")) +
                labs(title = "Cumulative daily count of COVID-19 cases",
                     caption = "Data source: JHU CSEE") +
                theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      legend.position = "bottom")

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

    output$plot_cfr <- renderPlot({

        ggplot(as.data.frame(cfr_simulations), aes(x = cfr_simulations)) +
            geom_histogram(fill = "firebrick1", color = "firebrick1", binwidth = 0.01) + 
            theme_minimal() +
            labs(title = sprintf("Distribution of simulated case fatality rate (CFR) estimates as of %s", max(export$date)),
                 subtitle = sprintf("Estimated CFR outside China: %s%% (95%% CI bounds: %s%%, %s%%)", cfr_est, cfr_ci[1], cfr_ci[2]),
                 x = "simulated rate", y = "share of simulations",
            caption = "Following Althaus; see https://github.com/calthaus/ncov-cfr") +
            scale_y_continuous(breaks = seq(0,3000,1000), labels = c("0", "0.10", "0.20", "0.30")) +
            theme(axis.title.x = element_blank())

    })

    output$frame <- renderUI({

        tags$iframe(style = "height:400px; width:100%; scrolling=yes", src = sitreps[input$sitrep_date])

    })

}

## RUN THE APP

shinyApp(ui, server)
