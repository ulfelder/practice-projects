library(tidyverse)
library(lubridate)
library(lme4)
library(merTools)
library(randomForest)
library(pROC)

set.seed(20912)
memory.limit(size = 100000) # we're gonna need a bigger boat

# DATA PREP ---

df <- read.csv("c:/users/ulfel/documents/nfl_analytics/pbp_2009_2018.csv", stringsAsFactors = FALSE)

df <- mutate(df, pos_net_score_final = ifelse(posteam == home, homescore - awayscore, awayscore - homescore), # target for linear regression
                 pos_result_final = as.factor(ifelse(pos_net_score_final > 0, 1, 0)), # target for classification
                 pos_net_score_realtime = PosTeamScore - DefTeamScore,
                 pos_home = as.factor(ifelse(posteam == home, "Yes", "No")),
                 pos_538p = ifelse(posteam == home, pregame_p_538 - 0.5, 1 - (pregame_p_538 - 0.5)),
                 down = as.factor(down),
                 time_elapsed = 3600 - TimeSecs,
                 posteam_timeouts_pre = as.factor(posteam_timeouts_pre))

# filter out some junk rows
df <- filter(df, PlayType != "No Play" & !is.na(down) & !is.na(pos_net_score_final) & !is.na(pos_net_score_realtime) & !is.na(pos_538p))

# put games in a list by season
seasons <- split(df, df$Season)

# MODELING ----

lmer_1 <- bind_rows(lapply(seq_along(seasons), function(i) {

    train <- bind_rows(seasons[-i])
    test <- seasons[[i]]

    # filter out games that go to OT
    train <- group_by(train, GameID) %>% mutate(OT = ifelse(any(qtr == 5), 1, 0)) %>% filter(OT == 0)

    mod <- lmer(pos_net_score_final ~ pos_538p +  # pregame win probability for possession team per 538, net of 0.5; this should capture home field adv, too
                    pos_net_score_realtime + time_elapsed + I(pos_net_score_realtime * time_elapsed) +  # current score and time
                    yrdline100 + down + ydstogo +  # distance to goal line, down (factor), and yards needed for 1st down
                    posteam_timeouts_pre +
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

    output <- data.frame(season = test$Season, game = test$GameID, play = test$play_id, model = "lmer_1", pred, obs)

    return(output)

}))

logit_1 <- bind_rows(lapply(seq_along(seasons), function(i) {

    train <- bind_rows(seasons[-i])
    test <- seasons[[i]]

    # filter out games that go to OT
    train <- group_by(train, GameID) %>% mutate(OT = ifelse(any(qtr == 5), 1, 0)) %>% filter(OT == 0)

    mod <- glm(pos_result_final ~ pos_538p +  # pregame win probability for possession team per 538, net of 0.5; this should capture home field adv, too
                   pos_net_score_realtime + time_elapsed + I(pos_net_score_realtime * time_elapsed) +  # current score and time
                   yrdline100 + down + ydstogo + posteam_timeouts_pre,  # distance to goal line, down (factor), and yards needed for 1st down
               data = train,
               family = binomial)

    # filter out games that go to OT from test set
    test <- group_by(test, GameID) %>% mutate(OT = ifelse(any(qtr == 5), 1, 0)) %>% filter(OT == 0)

    pred <- predict(mod, newdata = test, type = "response")

    obs <- test$pos_result_final

    output <- data.frame(season = test$Season, game = test$GameID, play = test$play_id, model = "logit_1", pred, obs)

    return(output)

}))

rf_1 <- bind_rows(lapply(seq_along(seasons), function(i) {

    train <- bind_rows(seasons[-i])
    test <- seasons[[i]]

    # filter out games that go to OT
    train <- group_by(train, GameID) %>% mutate(OT = ifelse(any(qtr == 5), 1, 0)) %>% filter(OT == 0)

    mod <- randomForest(pos_result_final ~ pos_538p + pos_net_score_realtime + time_elapsed + yrdline100 + down + ydstogo + posteam_timeouts_pre, data = train)

    # filter out games that go to OT from test set
    test <- group_by(test, GameID) %>% mutate(OT = ifelse(any(qtr == 5), 1, 0)) %>% filter(OT == 0)

    pred <- predict(mod, newdata = test, type = "prob")[,"1"]

    obs <- test$pos_result_final

    output <- data.frame(season = test$Season, game = test$GameID, play = test$play_id, model = "rf_1", pred, obs)

    return(output)

}))

results_list <- list(lmer_1, logit_1, rf_1)

# ANALYSIS ----

results_roc <- lapply(results_list, function(df) { with(df, pROC::roc(obs, pred)) })

results_auc <- sapply(results_roc, function(x) x$auc)

# compute auc overall for each model, but also look graphically at distribution of auc across seasons
# and maybe other things (see below) by model. put results in a list, map or lapply over it to split by season,
# compute, then plot...

# add quarter, down, yardline, time remaining to allow possibility of comparing accuracy across those
