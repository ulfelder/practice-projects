# function to scrape and transform tables of fractional odds from oddschecker.com
scrape_odds <- function(url) {

  require(tidyverse)
  require(rvest)

  # vector of labels for the bookmakers; I'd like to do this programmatically, but the
  # bookmakers are represented by images that contain their names rather than text,
  # and I can't find labels in the html
  sources <- c("bet365", "skybet", "ladbrokes", "william_hill", "marathon_bet",
               "betfair", "betvictor", "paddypower", "unibet", "coral",
               "betfred", "betway", "sports_nation", "boyle_sports", "vbet",
               "juventus", "sporting_bet", "888sport", "moplay", "spreadx",
               "redzone", "betfair_exchange", "betdaq", "matchbook", "smarkets")

  # helper function within the function to convert fractional odds to probability scale
  odds_to_probs <- function(x) {

    y <- NA

    if (x == "") { 
        y <- NA
      } else if (grepl("/", x)) {
        z <- as.numeric(strsplit(x, "/")[[1]])
        y <- z[2]/sum(z)
      } else {
        y <- 1/(1 + as.numeric(x))
      } 

    return(y)

  }

  page_raw <- read_html(url)

  odds_table <- page_raw %>%
    html_node("table.eventTable") %>%
    html_table()

  # remove header rows, cueing off position of QuickBet button on page
  headspace <- which(odds_table$X1 == "QuickBet") + 2
  odds_table <- odds_table[headspace:nrow(odds_table),]

  # split off the names of the categories
  targets <- odds_table[,1]
  
  # split off the table of odds, also removing phantom col 23 (hopefully consistent)
  odds <- select(odds_table, -1, -23)

  # run the helper function to convert odds to probability scale
  odds_transformed <- mutate_all(odds, function(x) sapply(x, odds_to_probs))

  # recalibrate resulting probabilities to sum to 1 within markets
  odds_calibrated <- mutate_all(odds_transformed, function(x) x/sum(x, na.rm = TRUE))

  # attach the bookies names
  colnames(odds_calibrated) <- sources

  # get the unweighted mean across markets
  odds_calibrated$mean <- apply(odds_calibrated, 1, mean, na.rm = TRUE)
  
  # attach the names of the categories
  odds_calibrated$target <- targets
  
  # pivot to long format to make plotting easier
  odds_long <- pivot_longer(odds_calibrated, -target, names_to = "source", values_to = "value")

  return(odds_long)

}

# function to generate a ridgeline plot of win probabilities for choices within a single
# market (e.g., teams in a tournament, nominees for a particular Oscar), using the data frame
# returned by scrape_odds() as its main input
plot_odds <- function(my_data, my_title = '') {
  
  require(tidyverse)
  require(ggridges)

  my_data %>%
    filter(source != "mean") %>%
    filter(!is.na(value)) %>%
  ggplot(aes(x = value, y = target)) +
    geom_density_ridges() +
    theme_minimal() +
    labs(title = my_title, x = "win probability") +
    theme(axis.title.y = element_blank())

}
