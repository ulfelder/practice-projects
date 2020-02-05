# function to scrape and transform tables of fractional odds from oddschecker.com
# NOTE: for this to work, you need to target the page with fractional odds, not decimal odds or moneylines
scrape_odds <- function(url) {

  require(tidyverse)
  require(rvest)

  # helper function to convert fractional odds to probability scale
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

  # scrape the whole page
  page_raw <- read_html(url)

  # extract the big table with the odds and a bunch of extraneous crap
  odds_table <- page_raw %>%
    html_node("table.eventTable") %>%
    html_table()

  # remove header rows, cueing off position of QuickBet button on page
  headspace <- which(odds_table$X1 == "QuickBet") + 2
  odds_table <- odds_table[headspace:nrow(odds_table),]

  # get a one-col df of category labels
  targets <- odds_table[,1]
  
  # get a table that *only* contains the cols with odds, dropping col of row labels and phantom
  # 22nd col that's just a break on scraped page
  odds <- select(odds_table ,-1, -22)

  # scrape the names of the bookies and attach them to the table of odds
  labels <- page_raw %>%
    html_nodes(".bk-logo-click") %>%
    html_attrs() %>%
    map('title') %>%
    unlist() %>%
    unique(.)  # for some reason, the sequence of labels appears twice; this cuts it down w/o changing order
  colnames(odds) <- labels

  # run the helper function to convert those fractional odds to probability scale
  odds_transformed <- mutate_all(odds, function(x) sapply(x, odds_to_probs))

  # recalibrate resulting probabilities to sum to 1 within bookies
  odds_calibrated <- mutate_all(odds_transformed, function(x) x/sum(x, na.rm = TRUE))

  # get the unweighted mean across bookies
  odds_calibrated$mean <- apply(odds_calibrated, 1, mean, na.rm = TRUE)
  
  # reattach the names of the categories
  odds_calibrated$target <- targets
  
  # pivot to long format (tidy) to make plotting easier
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
