set.seed(123)

# reproducible example from https://stackoverflow.com/questions/60038784/prediction-for-lmer-model-with-nested-random-effects
library(data.table)
library(lme4)

dt <- data.table(expand.grid(design=c("a", "b"), species=c("x", "y", "z"), report=c("1", "2", "3"), count=seq(1, 10, 1)))
dt$weight <- 0
dt[species=="x"]$weight <- rnorm(60, 70, 10)
dt[species=="y"]$weight <- rnorm(60, 80, 15)
dt[species=="z"]$weight <- rnorm(60, 90, 20)
dt[design=="a"]$weight <- dt[design=="a"]$weight- 0.1*dt[design=="a"]$weight
dt[report=="1"]$weight <- dt[report=="1"]$weight+0.15*dt[report=="1"]$weight
dt[report=="2"]$weight <- dt[report=="2"]$weight-0.15*dt[report=="1"]$weight

m <-lmer(weight~design+(1|report/species), data=dt)

# I'll pick it up here...
library(merTools)

# make a data frame with all possible combinations of the three covariates
newdt <- with(dt, expand.grid(design = unique(design),
                              report = unique(report),
                              species = unique(species)))

# set number of simulations to generate
nsims = 1000

# use the fitted model to run those simulations, setting returnSims = TRUE to keep all results
sims <- merTools::predictInterval(m, n.sims = nsims, newdata = newdt, returnSims = TRUE)

# extract the simulation results, which are a matrix with nrow == nrow(newdt) and ncol == nsims
yhats <- attr(sims, "sim.results")

# do some operations on those simulation results to get quantities of interest
newdt$sim_mean <- rowMeans(yhats)
newdt$sim_95_lower <- apply(yhats, 1, function(i) { sort(i)[0.025 * nsims] })
newdt$sim_95_upper <- apply(yhats, 1, function(i) { sort(i)[0.975 * nsims] })
