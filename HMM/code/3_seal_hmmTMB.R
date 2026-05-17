
library(hmmTMB)
library(ggplot2)
library(lubridate)
library(dplyr)
theme_set(theme_bw())

######################################
#### Data preparation and set-up ####
######################################

# load data
data <- read.csv("data/elephant_seals.csv")
table(data$ID)

# prepare data for HMM (i.e., calculate observation var)
data <- prepData(data, type = "LL", covNames = c("dist", "day"))
head(data)

# hmmTMB requires your date to be formatted as a date
data$time <- ymd_hms(data$time)

# some idea of inter-individual variability
data %>% 
  group_by(ID) %>% 
  summarise(mean_step = mean(step, na.rm = T), 
            sd_step = sd(step, na.rm = T), 
            mean_angle = mean(angle, na.rm = T), 
            sd_angle = sd(angle, na.rm = T))

####################################
########## Model 1: no REs #########
####################################

# set initial parameters (chosen just based on looking at steps/turns)
# I think this inputting is much more intuitive
par0 <- list(step = list(mean = c(10, 30), sd = c(5, 15)), 
             angle = list(mu = c(0, 0), kappa = c(0.5, 5)))

# create observation model object that will be called to fit hmm
# assumes time column (formatted as date), gamma2 = mean/sd
obs1 <- Observation$new(data = data, 
                        dists = list(step = "gamma2", angle = "vm"), 
                        n_states = 2, 
                        par = par0)
obs1

# specifiy tpm/Markov chain model and save as object
# this is the formula for a harmonic (i.e., what cosinor does)
tpm_formula <- ~dist + cos(2*pi*day/365) + sin(2*pi*day/365) 
hid1 <- MarkovChain$new(n_states = 2, 
                        formula = tpm_formula,
                        data = data, 
                        initial_state = "stationary")
hid1

# make hmm object
hmm1 <- HMM$new(obs = obs1, 
                hid = hid1)

# fit hmm
hmm1$fit()

# outputs 
hmm1$print()
hmm1$coeff_fe()
hmm1$print_tpm()

# state-dependent distributions
hmm1$plot_dist("step")
hmm1$plot_dist("angle")

# transition and state probabilities
hmm1$plot("tpm", var = "dist")
hmm1$plot("delta", var = "dist")
hmm1$plot("tpm", var = "day")
hmm1$plot("delta", var = "day")

# plot viterbi sequence
hmm1$plot_ts("x", "y") + coord_map() 

# pseudoresiduals
pr <- hmm1$pseudores()
qqnorm(pr$step, main = "")
abline(0, 1, col="red")
acf(pr$step, na.action = na.pass, main = "")


####################################
######### Model 2: obs REs #########
####################################

# specify observation model (with REs for step mean and angle kappa)
# note this is is a random intercept
obs_formula <- list(step = list(mean = ~s(ID, bs = 're'), sd = ~1), 
                    angle = list(mu = ~1, kappa = ~s(ID, bs = 're')))

# create observation model object that will be called to fit hmm
# assumes time column (formatted as date), gamma2 = mean/sd
obs2 <- Observation$new(data = data, 
                        formulas = obs_formula,
                        dists = list(step = "gamma2", angle = "vm"), 
                        n_states = 2, 
                        par = par0)

# specifiy tpm/Markov chain model and save as object
hid2 <- MarkovChain$new(n_states = 2, 
                        formula = ~1, 
                        data = data, 
                        initial_state = "stationary")

# make hmm object
hmm2 <- HMM$new(obs = obs2, 
                hid = hid2)

# fit hmm
hmm2$fit()

# outputs 
hmm2$coeff_fe()
hmm2$coeff_re()
hmm2$print_tpm()
hmm2$lambda()$obs # smoothing parameter

# state-dependent distributions
hmm2$plot_dist("step")
hmm2$plot_dist("angle")

# how step length and kappa vary by ID
hmm2$plot(what = "obspar", var = "ID", i = c("step.mean", "angle.kappa"))

# plot viterbi sequence
hmm2$plot_ts("x", "y") + coord_map() 


####################################
######### Model 3: splines #########
####################################

# create observation model object that will be called to fit hmm
# assumes time column (formatted as date), gamma2 = mean/sd
obs3 <- Observation$new(data = data, 
                        dists = list(step = "gamma2", angle = "vm"), 
                        n_states = 2, 
                        par = par0)

# specifiy tpm/Markov chain model and save as object
# cyclic smooth of day of year
tpm_formula3 <- ~dist + s(day, k = 10, bs = "cc")
hid3 <- MarkovChain$new(n_states = 2, 
                        formula = tpm_formula3, 
                        data = data, 
                        initial_state = "stationary")

# make hmm object
hmm3 <- HMM$new(obs = obs3, 
                hid = hid3)

# fit hmm
hmm3$fit()

# outputs 
hmm3$coeff_re()
hmm3$lambda()$hid
hmm3$print_tpm()

# state-dependent distributions
hmm3$plot_dist("step")
hmm3$plot_dist("angle")

# plot linear effect of distance
hmm3$plot("tpm", var = "dist")
hmm3$plot("delta", var = "dist")

# plot non-linear effects
hmm3$plot("tpm", var = "day")
hmm3$plot("delta", var = "day")

# plot viterbi sequence
hmm3$plot_ts("x", "y") + coord_map()


