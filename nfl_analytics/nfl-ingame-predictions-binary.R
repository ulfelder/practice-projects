library(tidyverse)
library(nflscrapR)
library(randomForest)
library(mgcv)
library(pROC)
library(classifierplots)

set.seed(123)
memory.limit(size = 50000)  # need this for random forests

# DATA LOADING AND MUNGING ----

# make a vector of years representing the seasons available via nflscrapR when I wrote this
years <- seq(2009, 2018)

# scrape play-by-play data for those seasons; this takes a *really* long time...but it works, which is awesome
pbp_list <- lapply(years, function(i) season_play_by_play(i))

# collapse the resulting list of seasonal dfs into one big df and get rid of the list
pbp_df <- do.call(rbind, pbp_list)
rm(pbp_list)

# now do the same with the game-level data, which includes stuff like final score
games_list <- lapply(years, function(i) { scrape_game_ids(Season = i) })
games_df <- do.call(rbind, games_list)
rm(games_list)

# merge the two, spreading game-level data across all plays from that game
df <- left_join(pbp_df, games_df)

# Get historical game data from FiveThirtyEight repo so we can use their pregame win probability in model. Even with
# the fixes to team labels that follows in a sec, I still lose something like 1,000+ rows in this merge with NAs
# appearing in key fields, and I'm not totally sure why. For current purposes, I decided it wasn't worth my while
# to run that down and fix it.
nfl538 <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/nfl-elo-game/master/data/nfl_games.csv",
                   stringsAsFactors = FALSE) %>%
  filter(season >= 2009) %>%
  # rename some things to facilitate merging to come
  select(home = team1,
         away = team2,
         date,
         pregame_p_538 = elo_prob1)

# change team labels in scraped data to prepare for merging; using 538's because it's simpler
df <- mutate(df, home = str_replace_all(home, c("JAC" = "JAX", "^LA$" = "LAR", "SD" = "LAC", "STL" = "LAR", "WAS" = "WSH")),
                 away = str_replace_all(away, c("JAC" = "JAX", "^LA$" = "LAR", "SD" = "LAC", "STL" = "LAR", "WAS" = "WSH")))

# merge the 538 win probabilities into the play-by-play data
df <- left_join(df, nfl538, by = c("home", "away", "date"))
rm(nfl538)

# additional data prep for modeling
df <- mutate(df, winner = ifelse(homescore > awayscore, home, away),
                 netfinalscore = homescore - awayscore,
                 poswins = as.factor(ifelse(winner == posteam, "Yes", "No")),
                 poshome = as.factor(ifelse(posteam == home, "Yes", "No")),
                 pos538p = ifelse(posteam == home, pregame_p_538, 1 - pregame_p_538),
                 down = as.factor(down))

# filter out games that go to OT
df <- df %>%
  group_by(GameID) %>%
  mutate(OT = ifelse(any(qtr == 5), 1, 0)) %>%
  filter(OT == 0)

# filter out some other junk rows
df <- filter(df, PlayType != "No Play" & !is.na(down) & !is.na(poswins) & !is.na(pos538p))

# partition the data into training and test sets; k-fold CV would be the next logical step, but
# I was more interested in getting quickly to a credible model, so this is sufficient for now.
train <- filter(df, Season < 2018)
holdout <- filter(df, Season == 2018)

# MODELING ----

# Run a basic feature set through a few good classifiers so we can compare OOS accuracy. FWIW, I also looked at a few
# other specifications (e.g., nonlinear effect of field position, interaction between down and yards to go), but they
# didn't improve accuracy over this version, which achieves an AUC of ~0.85 in the holdout season.
mod_logit <- glm(poswins ~ poshome + TimeSecs + ScoreDiff + I(TimeSecs * ScoreDiff) + pos538p + I(TimeSecs * pos538p) + yrdline100 + down + ydstogo, data = train, family = "binomial")
mod_rf <- randomForest(poswins ~ poshome + pos538p + TimeSecs + ScoreDiff + yrdline100 + down + ydstogo, data = train)
mod_gam <- gam(poswins ~ poshome + s(TimeSecs) + s(ScoreDiff) + ti(TimeSecs, ScoreDiff, k = 5) + pos538p + ti(TimeSecs, pos538p, k = 5) + yrdline100 + ydstogo + down,
               data = train,
               family = "binomial",
               method = "REML")

# apply those models to the test season, and compute the mean of their predictions for good measure
p_logit <- predict(mod_logit, newdata = holdout, type = "response")
p_rf <- predict(mod_rf, newdata = holdout, type = "prob")[,"Yes"]
p_gam <- predict(mod_gam, newdata = holdout, type = "response")
p_mean <- apply(cbind(p_logit, p_rf, p_gam), 1, mean)

# put those vectors in a list so we can easily iterate over them if desired
p_list <- list(p_logit = p_logit, p_rf = p_rf, p_gam = p_gam, p_mean = p_mean)

# get vector of the observed outcomes against which to compare those predictions
target <- ifelse(as.character(holdout$poswins) == "Yes", 1, 0)

# generate ROC curves for the predictions
roc_list <- map(p_list, function(i) { roc(target, i) })

# check out the AUCs from those ROC curves
print(sapply(roc_list, auc))

# merge the predictions into the holdout data to make plotting easier
test <- bind_cols(holdout, p_list)

# function to plot in-game probabilities for completed game
pbpPlot <- function(gameid) {

  test %>%
    filter(GameID == gameid) %>%
    mutate(p_homewin = ifelse(posteam == home, p_logit, 1 - p_logit),
           p_awaywin = 1 - p_homewin,
           elapsedtime = 3600 - TimeSecs) %>%
    ggplot(aes(x = elapsedtime, y = p_homewin)) +
      geom_line() +
      ylim(0,1) +
      theme_minimal() +
      labs(title = sprintf("%s @ %s", unique(test$away[test$GameID==gameid]), unique(test$home[test$GameID==gameid])),
           caption = sprintf( "final score: %s - %s", unique(test$awayscore[test$GameID==gameid]), unique(test$homescore[test$GameID==gameid])),
           y = "prob. of home win") +
      scale_x_continuous(breaks = seq(0,3600,900), labels = c("kickoff", "end 1st Q", "halftime", "end 3rd Q", "game over, man"), name = element_blank())

}

# NOT RUN: bal <- unlist(unique(test[test$home == "BAL" | test$away == "BAL",][,"GameID"]))
# pbpPlot(bal[1])
# pbpPlot(bal[2]) # etc.

# function to compute win probabilities on the fly; could be used in a Shiny app, for example,
# if we saved the model object in an .Rdata file and then loaded just that object on app launch
p_now <- function(team = "home",
                  secsleft = 3600,
                  netscore = 0,
                  pregame_p = 0.5,
                  fieldpos = 75,
                  down = 1,
                  ydstogo = 10) {

  df_now <- data.frame(poshome = if (team == "home") { as.factor(c("No", "Yes"))[2] } else { as.factor(c("No", "Yes"))[1] },
                       TimeSecs = secsleft,
                       ScoreDiff = netscore,
                       pos538p = pregame_p,
                       yrdline100 = fieldpos,
                       down = as.factor(c(1,2,3,4))[down],
                       ydstogo = ydstogo)

  p <- predict(mod_logit, newdata = df_now, type = "response")

  p <- round(p, 3)

  return(p)

}

# Go Ravens!
