library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(readxl)
library(forecast)
library(forecastHybrid)
library(xts)
library(plotly)

options(stringsAsFactors = FALSE)

# make vector of links to price histories
price_histories <- c(WTI = "https://www.eia.gov/dnav/pet/hist_xls/RWTCd.xls",
                     Brent = "https://www.eia.gov/dnav/pet/hist_xls/RBRTEd.xls")
  
# data-loading function
fetch_history <- function(url, is_windows = TRUE) {

  require(readxl)

  temp <- paste0(tempfile(), ".xls")
  if (is_windows == TRUE) {
  
    download.file(url, destfile = temp, mode = "wb")
  
  } else {
  
    download.file(url, destfile = temp)
  
  }

  df <- read_excel(path = temp, sheet = 2, skip = 2)
  names(df) <- c('date', 'value')

  return(df)

}

# ---- USER INTERFACE ----
ui <- dashboardPage(skin = "black",
  
  dashboardHeader(title = "Oilcast"),
  
  dashboardSidebar(

    selectInput("series", "Series:", choices = price_histories),
    
    numericInput("horizon", "Days ahead:", value = 1, min = 1, max = 30, step = 1),
    
    textInput("thresholds", "Bin threshold(s):"),
    
    actionButton("do", "Crunch the numbers"),

    br(),

    br(),

    fluidRow(align = "center",
      downloadButton("downloadData", "Download Forecast", style = "font-weight:bold;white-space:normal;")
    )
    
  ),
  
  dashboardBody(
    
    fluidRow(
      
      box(title = "Predicted probabilities", plotOutput("plot_probs", height = 200)),
      
      box(title = "Forecast plot with 50% CI & 60 trading-day history", plotOutput("plot_forecast", height = 200))
      
    ),
    
    fluidRow(
      
      box(title = "Historical data", width = 12, plotlyOutput("plot_history", height = 200))
      
    )
              
  )
  
)

# server
server <- function(input, output) {
 
  v <- reactiveValues(series = NULL, horiz = NULL, my_bins = NULL)
  
  observeEvent(input$do, {
    v$data <- fetch_history(input$series)
  })

  observeEvent(input$do, {
    v$horiz <- input$horizon
  })
  
  observeEvent(input$do, {
    v$my_bins <- c(-Inf, as.numeric(unlist(str_split(input$thresholds, ","))), Inf)
  })
  
  # generate a hybrid model and predicted probabilities from it and return results as a list
  pred_list <- reactive({
    
    x_xts <- with(v$data, xts(value, order.by = date))

    mod_hybrid <- hybridModel(x_xts, models = "aet")

    pred_hybrid <- forecast::forecast(mod_hybrid, h = v$horiz, level = seq(1,99))

    probs <- sapply(seq(length(v$my_bins) - 1), function(i) {
      
      # create a vector spanning the range of 1-99% equal-sized pred slices in ascending order
      pvec <- c(rev(pred_hybrid$lower[v$horiz,]), pred_hybrid$upper[v$horiz,])
      
      # put the elements of that vector in the bins
      cats <- cut(pvec, breaks = v$my_bins, labels = FALSE)
      
      # get the share of equal-sized slices in the bin of interest
      p <- sum(cats == i)/198
      
      # round it to the 1000s (or 0.1 percents)
      p <- round(p, 3)
      
      return(p)
      
    })    
    
    preddat <- data.frame(bin = factor(seq(length(probs)), labels = LETTERS[1:length(probs)]),
                          probability = probs)

    pad <- pred_hybrid$x[(length(pred_hybrid$x) - 59):length(pred_hybrid$x)]   

    pred_df <- data.frame(trade_day = seq(-length(pad) + 1, v$horiz),
                          mean = c(pad, pred_hybrid$mean),
                          lower = c(rep(NA, length(pad)), pred_hybrid$lower[,50]),
                          upper = c(rep(NA, length(pad)), pred_hybrid$upper[,50]))

    download_df_mean <- data.frame(step = seq_along(pred_hybrid$mean), mean = as.numeric(pred_hybrid$mean))
    download_df_lower <- as.data.frame(pred_hybrid$lower)
    names(download_df_lower) <- paste0("lower_", seq(99))
    download_df_upper <- as.data.frame(pred_hybrid$upper)
    names(download_df_upper) <- paste0("upper_", seq(99))
    
    download_df <- bind_cols(list(download_df_mean, download_df_lower, download_df_upper)) %>%
      pivot_longer(-step, names_to = "quantity", values_to = "value")

    pred_list <- list(model = mod_hybrid,
                      forecast = pred_hybrid,
                      forecast_plot_data = pred_df,
                      forecast_download_data = download_df,
                      probabilities = preddat)

    return(pred_list)
    
  })
  
  output$plot_forecast <- renderPlot({
    
    if(is.null(v$horiz)) return()
    
    pred_list()[['forecast_plot_data']] %>%
      ggplot(aes(x = trade_day, y = mean)) +
        geom_line() +
        geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 2, alpha = 0.1) +
        ggplot2::annotate("text", x = 0, y = min(pred_list()[['forecast_plot_data']]$lower, na.rm = TRUE),
                          label = dplyr::last(v$data[,'date'])) +
        theme_minimal() + 
        labs(x = "days since last observed close") +
        theme(axis.title.y = element_blank())
    
  })
  
  output$plot_probs <- renderPlot({
    
    if(is.null(v$horiz)) return()
    
    ggplot(pred_list()[['probabilities']], aes(x = bin, y = probability)) +
      geom_col(fill = "dodgerblue", alpha = 1/2) +
      theme_minimal() + 
      annotate("text", x = pred_list()[['probabilities']]$bin, y = pred_list()[['probabilities']]$probability,
               label = pred_list()[['probabilities']]$probability, vjust = -0.25) +
      theme(axis.title = element_blank(),
            panel.grid.minor = element_blank())

  })
  
  output$plot_history <- renderPlotly({

    if(is.null(v$data)) return()
    
    v$data %>%
      mutate(hover_label = sprintf("%s: $%s", date, value)) %>%
      plot_ly(x = ~date,
              y = ~value,
              type = "scatter",
              mode = "lines",
              hoverinfo = "text",
              text = ~hover_label) %>%
      layout(xaxis = list(rangeslider = list(type = "date")),
             yaxis = list(title = "price per barrel"))
    
  })

  output$downloadData <- downloadHandler(

    filename <- function() {
      paste0(names(price_histories)[price_histories == input$series], "_forecast_", gsub("-", "", dplyr::last(v$data[,'date'])), ".csv")
    },

    content <- function(file) {
      write.csv(pred_list()[['forecast_download_data']], file, row.names = FALSE)
    }

  )
  
}

shinyApp(ui, server)
