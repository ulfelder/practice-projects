library(tidyverse)
library(lubridate)
library(lme4)
library(merTools)
library(randomForest)
library(mgcv)
library(pROC)

set.seed(20912)
memory.limit(size = 100000) # we're gonna need a bigger boat

# DATA PREP ---

# load data set created by nfl-ingame-data.R, here saved to my hard drive as csv
df <- read.csv("~/nfl_analytics/pbp_2009_2019.csv", stringsAsFactors = FALSE)

df <- mutate(df, pos_net_score_final = ifelse(posteam == home, home_score - away_score, away_score - home_score), # target for linear regression
                 pos_result_final = as.factor(ifelse(pos_net_score_final > 0, 1, 0)), # target for classification
                 pos_net_score_realtime = PosTeamScore - DefTeamScore,
                 pos_home = as.factor(ifelse(posteam == home, "Yes", "No")),
                 pos_538p = ifelse(posteam == home, pregame_p_538 - 0.5, 1 - (pregame_p_538 - 0.5)),
                 down = as.factor(down),
                 time_elapsed = 3600 - TimeSecs,
                 posteam_timeouts_pre = as.factor(posteam_timeouts_pre))

# filter out some uninformative rows
df <- filter(df, PlayType != "No Play" & !is.na(down) & !is.na(pos_net_score_final) & !is.na(pos_net_score_realtime) & !is.na(pos_538p))

# put games in a list by season
seasons <- split(df, df$Season)


# MODELING ----

lmer_1 <- bind_rows(lapply(seq_along(seasons), function(i) {

    train <- bind_rows(seasons[-i])
    test <- seasons[[i]]

    # filter out games that go to OT
    train <- group_by(train, GameID) %>% mutate(OT = ifelse(any(qtr == 5), 1, 0)) %>% filter(OT == 0)

    mod <- lmer(pos_net_score_final ~ pos_home + pos_538p +  # home field adv, pregame win probability for possession team per 538
                    pos_net_score_realtime + time_elapsed + I(pos_net_score_realtime * time_elapsed) +  # current score and time
                    yrdline100 + down + ydstogo +  # distance to goal line, down (factor), and yards needed for 1st down
                    posteam_timeouts_pre + # possession team's timeouts remaining as a 4-level factor
                    (1|GameID),  # random effects for games; tried nesting in seasons, but no added power per variance
                data = train)

    # filter out games that go to OT
    test <- group_by(test, GameID) %>% mutate(OT = ifelse(any(qtr == 5), 1, 0)) %>% filter(OT == 0)

    Y <- predictInterval(merMod = mod,
                newdata = test,
                which = "fixed",  # to only consider variation in fixed effects, since we have new levels (games) for which to predict
                n.sims = 1000,
                type = "linear.prediction",
                stat = "mean",
                level = 0.8,
                seed = 20912,
                returnSims = TRUE)

    test_sims <- as.data.frame(attr(Y, "sim.results"))
    colnames(test_sims) <- paste("sim", seq(ncol(test_sims)), sep = "_")

    # for scoring purposes, convert the predicted net score for possession team to net score for home team
    test <- test %>%
        ungroup() %>% # no idea how test got grouped, but have to ungroup to bind properly
        cbind(., test_sims) %>%
        mutate_at(vars(starts_with("sim_")), ~ ifelse(posteam == home, ., -.)) %>%
        mutate_at(vars(starts_with("sim_")), ~ ifelse(. > 0, 1, 0))

    pred <- rowSums(dplyr::select(test, starts_with("sim_")))/1000  # need dplyr part b/c functions conflict

    obs <- as.numeric(test$homescore > test$awayscore)

    output <- data.frame(season = test$Season, game = test$GameID, play = test$play_id, qtr = test$qtr, down = test$down,
                         model = "lmer", pred, obs)

    return(output)

}))

logit_1 <- bind_rows(lapply(seq_along(seasons), function(i) {

    train <- bind_rows(seasons[-i])
    test <- seasons[[i]]

    # filter out games that go to OT
    train <- group_by(train, GameID) %>% mutate(OT = ifelse(any(qtr == 5), 1, 0)) %>% filter(OT == 0)

    mod <- glm(pos_result_final ~ pos_home + pos_538p +
                   pos_net_score_realtime + time_elapsed + I(pos_net_score_realtime * time_elapsed) + 
                   yrdline100 + down + ydstogo + posteam_timeouts_pre,
               data = train,
               family = binomial)

    # filter out games that go to OT from test set
    test <- group_by(test, GameID) %>% mutate(OT = ifelse(any(qtr == 5), 1, 0)) %>% filter(OT == 0)

    pred <- predict(mod, newdata = test, type = "response")

    obs <- test$pos_result_final

    output <- data.frame(season = test$Season, game = test$GameID, play = test$play_id, qtr = test$qtr, down = test$down,
                         model = "logit", pred, obs)
    return(output)

}))

rf_1 <- bind_rows(lapply(seq_along(seasons), function(i) {

    train <- bind_rows(seasons[-i])
    test <- seasons[[i]]

    # filter out games that go to OT
    train <- group_by(train, GameID) %>% mutate(OT = ifelse(any(qtr == 5), 1, 0)) %>% filter(OT == 0)

    mod <- randomForest(pos_result_final ~ pos_home + pos_538p + pos_net_score_realtime + time_elapsed +
                        yrdline100 + down + ydstogo + posteam_timeouts_pre,
                        data = train)

    # filter out games that go to OT from test set
    test <- group_by(test, GameID) %>% mutate(OT = ifelse(any(qtr == 5), 1, 0)) %>% filter(OT == 0)

    pred <- predict(mod, newdata = test, type = "prob")[,"1"]

    obs <- test$pos_result_final

    output <- data.frame(season = test$Season, game = test$GameID, play = test$play_id, qtr = test$qtr, down = test$down,
                         model = "rf", pred, obs)
    return(output)

}))  

# this one takes forever
gam_1 <- bind_rows(lapply(seq_along(seasons), function(i) {

   require(mgcv)

    train <- bind_rows(seasons[-i])
    test <- seasons[[i]]

    # filter out games that go to OT
    train <- group_by(train, GameID) %>% mutate(OT = ifelse(any(qtr == 5), 1, 0)) %>% filter(OT == 0)

    mod <- gam(pos_result_final ~ pos_home + pos_538p +
                   s(pos_net_score_realtime) + s(time_elapsed) + ti(time_elapsed, pos_net_score_realtime, k = 5) +
                   yrdline100 + down + ydstogo + posteam_timeouts_pre,
               data = train,
               family = "binomial",
               method = "REML")

    # filter out games that go to OT from test set
    test <- group_by(test, GameID) %>% mutate(OT = ifelse(any(qtr == 5), 1, 0)) %>% filter(OT == 0)

    pred <- predict(mod, newdata = test, type = "response")

    obs <- test$pos_result_final
  
    output <- data.frame(season = test$Season, game = test$GameID, play = test$play_id, qtr = test$qtr, down = test$down,
                         model = "gam", pred, obs)
    return(output)

}))

# get same-shaped table with win probs in scraped data to use as benchmark
nflscrapr <- df %>%
    group_by(GameID) %>%
    mutate(OT = ifelse(any(qtr == 5), 1, 0)) %>%
    filter(OT == 0) %>%
    ungroup() %>%
    transmute(season = Season, game = GameID, play = play_id, qtr = qtr, down = down,
              model = "nflscrapr", pred = Win_Prob, obs = pos_result_final)

# roll the results up in a list
results_list <- list(lmer = lmer_1, logit = logit_1, rf = rf_1, gam = gam_1, nflscrapr = nflscrapr)

# ASSESSMENT ----

results_roc <- lapply(results_list, function(df) { with(df, pROC::roc(obs, pred)) })

auc_overall <- unlist(map(results_roc, ~ .$auc))

auc_season <- map(results_list, function(data) {

    data %>%
        group_by(season) %>%
        group_split() %>%
        map(~roc(.$obs, .$pred)) %>%
        map(~.$auc) %>%
        unlist()

})

auc_qtr <- map(results_list, function(data) {

    data %>%
        group_by(qtr) %>%
        group_split() %>%
        map(~roc(.$obs, .$pred)) %>%
        map(~.$auc) %>%
        unlist()

})

auc_down <- map(results_list, function(data) {

    data %>%
        group_by(down) %>%
        group_split() %>%
        map(~roc(.$obs, .$pred)) %>%
        map(~.$auc) %>%
        unlist()

})

print(auc_overall)
print(auc_season)
print(auc_qtr)
print(auc_down)

# so RF is the clear winner, both for marginal overall edge and for large advantage in 1st and 2nd qtrs.
# no clear pattern in variation across seasons, no real diff in performance across downs.
# gam is almost identical to simple logit with interactions, and lmer is a small step behind them.
# given RF's big edge in 1st half and use of same features across models, simple ensembling probably won't help.
# may still be worth exploring ebma, though...
