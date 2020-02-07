library(tidyverse)
library(animation)
library(lubridate)
library(lme4)
library(merTools)

set.seed(20912)
memory.limit(size = 50000) # we're gonna need a bigger boat

# DATA PREP ---

# load data from local directory
df <- read.csv("~/nfl_analytics/pbp_2009_2018.csv", stringsAsFactors = FALSE)

df <- mutate(df, pos_net_score_final = ifelse(posteam == home, homescore - awayscore, awayscore - homescore), # target for regression
                 pos_net_score_realtime = PosTeamScore - DefTeamScore,
                 pos_home = as.factor(ifelse(posteam == home, "Yes", "No")),
                 pos_538p = ifelse(posteam == home, pregame_p_538 - 0.5, 1 - (pregame_p_538 - 0.5)),
                 down = as.factor(down),
                 time_elapsed = 3600 - TimeSecs)

# filter out games that go to OT
df <- df %>%
  group_by(GameID) %>%
  mutate(OT = ifelse(any(qtr == 5), 1, 0)) %>%
  filter(OT == 0)

# filter out some other junk rows
df <- filter(df, PlayType != "No Play" & !is.na(down) & !is.na(pos_net_score_final) & !is.na(pos_net_score_realtime) & !is.na(pos_538p))

# partition for validation
train <- filter(df, Season < 2017)
test <- filter(df, Season >= 2017)

# MODELING ----

mod <- lmer(pos_net_score_final ~ pos_538p +  # pregame win probability for possession team per 538, net of 0.5; this should capture home field adv, too
                                  pos_net_score_realtime + time_elapsed + I(pos_net_score_realtime * time_elapsed) +  # current score and time
                                  yrdline100 + down + ydstogo +  # distance to goal line, down (factor), and yards needed for 1st down
                                  (1|GameID),  # random effects for games; tried nesting in seasons, but no added power per variance
            data = train)

# see https://ourcodingclub.github.io/2017/03/15/mixed-models.html#six
plot(mod)
qqnorm(resid(mod))
qqline(resid(mod)) # heavy tails!

Y <- predictInterval(merMod = mod,
                newdata = test,
                which = "fixed",  # to only consider variation in fixed effects, since we have new levels (games) for which to predict
                n.sims = 1000,
                type = "linear.prediction",
                stat = "median",
                level = 0.8,
                seed = 20912,
                returnSims = TRUE)

# plot(x = test$pos_net_score_final, y = Y$fit, type = "p")
# RMSE <- sqrt(mean((test$pos_net_score_final - Y$fit)^2))

test_sims <- as.data.frame(attr(Y, "sim.results"))
colnames(test_sims) <- paste("sim", seq(ncol(test_sims)), sep = "_")

# for plotting purposes, convert the predicted net score for possession team to net score for home team
test2 <- test %>%
  ungroup() %>% # no idea how test got grouped, but have to ungroup to bind properly
  cbind(., test_sims) %>%
  mutate_at(vars(starts_with("sim_")), ~ ifelse(posteam == home, ., -.))

# get a list of data frames for individual games
test2_split <- test2 %>% group_by(GameID) %>% group_split()
names(test2_split) <- unique(test2$GameID)  # use game ids as names of list elements to make selection easier

# function to use a game id to get a list of long data frames for individual plays from that game, ready for plotting
game_prep <- function(position, gamelist) {

  gamelist[[position]] %>%
    pivot_longer(cols = starts_with("sim_"), names_to = "sim", values_to = "predscore") %>%  # convert to long for plotting
    mutate(homewin = as.factor(ifelse(predscore > 0, "Yes", "No"))) %>%  # we'll use this to color the plot
    group_by(play_id) %>%
    group_split()

}

# function to plot distribution of predicted net scores for a single play, i.e., one element in list
# returned by previous block of code
play_plot <- function(play) {

  implied_prob <- with(play, sum(predscore > 0)/1000)
  my_title <- with(slice(play, 1), sprintf("%s @ %s | %s, %s", away, home, wday(as.Date(date), label = TRUE, abbr = FALSE), format(as.Date(date), "%B %d, %Y")))
  my_subtitle <- with(slice(play, 1), sprintf("Q%s %s  |  %s ball on %s %s, %s down, %s yds to go  |  %s - %s",
                                              qtr, time, posteam, SideofField, yrdln, down, ydstogo,
                                              ifelse(posteam == away, PosTeamScore, DefTeamScore), ifelse(posteam == home, PosTeamScore, DefTeamScore)))
  my_caption <- with(slice(play, 1), sprintf("Implied probability of %s win: %s", home, implied_prob))
  my_away <- with(slice(play, 1), away)
  my_home <- with(slice(play, 1), home)

  p <- ggplot(play, aes(predscore, fill = homewin)) + 
    geom_histogram(binwidth = 1, color = "black") +
    labs(title = my_title,
         subtitle = my_subtitle,
         caption = my_caption,
         x = "predicted net final score (home - visitor)",
         y = "count of simulations") +
    scale_fill_discrete(name = element_blank(), labels = paste0(c(my_away, my_home), " wins")) +
    xlim(-50,50) +   # so the plot stays locked in one horizontal position across animation
    theme_minimal() +
    theme(legend.position = "bottom")

  return(p)

}

X <- game_prep(1, test2_split)

png("~/nfl_analytics/kc-ne-2018-110.png", res = 300, width = 7, height = 5, unit = "in", bg = "white")
print(play_plot(X[[110]]))
dev.off()
