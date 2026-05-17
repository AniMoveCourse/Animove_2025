
library(momentuHMM)
library(lubridate)
library(ggplot2)
theme_set(theme_bw())

######################################
#### Data preparation and set-up ####
######################################

# load data
data <- read.csv("data/elephant_seals.csv")

# prepare data for HMM (i.e., calculate observation var)
# note here we can specify our covariates
# IF covariates are missing, momentuHMM will fill them with the closest value
data <- prepData(data, type = "LL", covNames = c("day", "dist"))

# choose distributions to be used in HMMs
dists <- list(step = "gamma", angle = "vm")

########################################
### Model 1: 2 states, no covariates ###
########################################

# set initial parameters (step means/sd, angle mean/concentration)
par0 <- list(step = c(10, 30, 5, 15),
             angle = c(0, 0, 0.5, 5))

# fit 2-state model
hmm <- fitHMM(data = data,
              nbStates = 2,
              dist = dists, 
              Par0 = par0, 
              estAngleMean = list(angle = TRUE))

hmm

# look at pseudo-residuals
plotPR(hmm)

########################################
### Model 2: 2 states, tpm covariates ###
########################################

# set covariate formula
# cosinor is a momentuHMM function to fit a harmonic
tpm_formula <- ~ dist + cosinor(day, period = 365)

# get initial parameters
# momentuHMM has a function to use estimates from a simpler model as par0
par0_cov <- getPar0(hmm, formula = tpm_formula)
par0_cov$Par
hmm$mle$step

# fit model
hmm_cov <- fitHMM(data = data,
                  nbStates = 2,
                  dist = dists, 
                  Par0 = par0_cov$Par, # obs par0
                  beta0 = par0_cov$beta, # state par0
                  formula = tpm_formula,
                  estAngleMean = list(angle = TRUE))

# obs parameters
hmm_cov$mle$step
hmm_cov$mle$angle

# covariate effects: transition probabilities
plot(hmm_cov, plotTracks = FALSE, ask = FALSE)
getTrProbs(hmm_cov)[,,1]

# covariate effects: stationary state probabilities
plotStationary(hmm_cov, plotCI = TRUE)

# activity budgets
timeInStates(hmm_cov)
timeInStates(hmm)

# stationary probabilities
head(stationary(hmm_cov)[[1]])
stationary(hmm)

# look at pseudo-residuals
plotPR(hmm_cov)
plotPR(hmm)


########################################
### Model 3: 2 states, obs covariates ###
######################################### 

# set covariate formula
obs_formula2 <- list(step = list(mean = ~cosinor(day, 365), sd = ~1))
par0_obs <- getPar0(hmm, DM = obs_formula2)

# fit model
hmm_obs <- fitHMM(data = data,
                  nbStates = 2,
                  dist = dists, 
                  Par0 = par0_obs$Par, #obs par
                  beta0 = par0_obs$beta, # state par
                  DM = obs_formula2, 
                  estAngleMean = list(angle = TRUE))

hmm_obs

# obs parameters
# not very interpretable for harmonics
hmm_obs$mle$step
hmm_obs$mle$angle

# look at covariate effects and distributions
plot(hmm_obs, plotTracks = FALSE, ask = FALSE, plotCI = T)

# look at pseudo-residuals
plotPR(hmm_obs)
plotPR(hmm)

# activity budgets
timeInStates(hmm_obs)
timeInStates(hmm)

