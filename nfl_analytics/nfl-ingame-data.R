# This script uses the nflscrapR package to build a play-by-play data set of all
# regular-season NFL games for the period 2009-2018. It can be extended to other 
# seasons by changing the values in the seq created in line 13. It can be extended
# to cover postseason games by adding another call to scrape_game_ids that sets
# type = "post". It's not a lot of code, but it takes a long time to run.

library(tidyverse)
library(nflscrapR)

options(stringsAsFactors = FALSE)

# make a vector of years representing the seasons available via nflscrapR when I wrote this
years <- seq(2009, 2018)

# scrape play-by-play data for those seasons
pbp_df <- map(years, ~ season_play_by_play(.)) %>% bind_rows()

# now do the same with the game-level data, which includes stuff like final score
games_df <- map(years, ~ scrape_game_ids(season = ., type = "reg")) %>% bind_rows()

# merge the two, spreading game-level data across all plays from that game
df <- left_join(pbp_df, games_df)

# Get historical game data from FiveThirtyEight repo so we can use their pregame win probability in model. Even with
# the fixes to team labels that follows in a sec, I still lose something like 1,000+ rows in this merge with NAs
# appearing in key fields, and I'm not totally sure why. For current purposes, I decided it wasn't worth my while
# to run that down and fix it.
nfl538 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/nfl-elo-game/master/data/nfl_games.csv") %>%
    filter(season >= 2009) %>%
    # rename some things to facilitate merging to come
    select(home = team1,
           away = team2,
           date,
           pregame_p_538 = elo_prob1)

# change team labels in scraped data to prepare for merging; using 538's as master because it's simpler
df <- mutate(df, home = str_replace_all(home, c("JAC" = "JAX", "^LA$" = "LAR", "SD" = "LAC", "STL" = "LAR", "WAS" = "WSH")),
                 away = str_replace_all(away, c("JAC" = "JAX", "^LA$" = "LAR", "SD" = "LAC", "STL" = "LAR", "WAS" = "WSH")))

# merge the 538 win probabilities into the play-by-play data
df <- left_join(df, nfl538, by = c("home", "away", "date"))

# Go Ravens!
