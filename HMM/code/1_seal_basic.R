
library(momentuHMM)
library(lubridate)
library(ggplot2)
theme_set(theme_bw())

######################################
#### Data preparation and set-up ####
######################################

# load data
data <- read.csv("data/elephant_seals.csv")
head(data) # note x, y are longitude/latitude
summary(data)

# plot data
ggplot(data, aes(x, y, group = ID, col = factor(ID))) +
  geom_point(size = 0.3) +
  geom_path() +
  geom_point(aes(x = x, y = y), 
             data = data.frame(x = 71, y = -49, ID = NA), 
             size = 3, color = "black", shape = 15) +
  coord_map() +
  theme(legend.position = "none")

# prepare data for HMM (i.e., calculate observation var)
# need to specify if coordinates are lat/lon or easting/northing ("UTM")
# package assumes that coordinate names are x and y
data <- prepData(data, type = "LL")

# look at momentuHMM data object
summary(data)
head(data)
tail(data)

# look at observation variables
hist(data$step, 50)
hist(data$angle, 50)

# choose distributions to be used in HMMs
# gamma distribution and von Mises distribution
dists <- list(step = "gamma", angle = "vm")


######################################
## Model 1: 2 states, no covariates ##
######################################

# set initial parameters
# step = (mean 1, mean 2, sd 1, sd 2)
# angle = (mean 1, mean 2, concentration 1, concentration 2)
par0_2S <- list(step = c(10, 30, 10, 30),
                angle = c(0, 0, 0.5, 5))

# fit 2-state model
hmm_2S <- fitHMM(data = data,
                 nbStates = 2,
                 dist = dists, 
                 Par0 = par0_2S, 
                 estAngleMean = list(angle = TRUE), 
                 retryFits = TRUE)
hmm_2S

#observation parameters
hmm_2S$mle$step
hmm_2S$mle$angle
plot(hmm_2S, plotTracks = FALSE, ask = FALSE)

# state process parameters
hmm_2S$mle$beta
hmm_2S$mle$gamma #tpm
stationary(hmm_2S)
timeInStates(hmm_2S)

# state decoding
plot(hmm_2S, ask = FALSE, animals = 7)
viterbi(hmm_2S)
head(stateProbs(hmm_2S))
plotStates(hmm_2S, animals = 7, ask = FALSE)

# save decoding
data$vit_2S <- factor(viterbi(hmm_2S))
data$sp1_2S <- stateProbs(hmm_2S)[,1]
data$sp2_2S <- stateProbs(hmm_2S)[,2]

# plot state decoding
cowplot::plot_grid(
  ggplot(data, aes(x, y, group = ID, col = vit_2S)) +
    geom_point(size = 0.3) +
    geom_path() + 
    scale_color_manual(values = c("firebrick", "royalblue")) +
    coord_map(), 
  
  ggplot(data, aes(x, y, group = ID, col = sp2_2S)) +
    geom_point(size = 0.3) +
    geom_path() +
    scale_color_continuous(high = "royalblue", low = "firebrick") +
    coord_map(), 
  ncol = 1)



######################################
## Model 2: 3 states, no covariates ##
######################################

# set initial parameters (now 3 means, 3 sds, etc.)
par0_3S <- list(step = c(5, 20, 40, 5, 20, 40),
                angle = c(0, 0, 0, 0.05, 2, 5))

# fit model
hmm_3S <- fitHMM(data = data,
                 nbStates = 3,
                 dist = dists, 
                 Par0 = par0_3S, 
                 estAngleMean = list(angle = TRUE), 
                 retryFits = TRUE)

hmm_3S

#observation parameters
hmm_3S$mle$step
hmm_3S$mle$angle
plot(hmm_3S, plotTracks = FALSE, ask = FALSE)

# state process parameters
hmm_3S$mle$gamma #tpm
stationary(hmm_3S)
timeInStates(hmm_3S)

# plot viterbi sequence and compare to 2 state model
data$vit_3S <- factor(viterbi(hmm_3S))

cowplot::plot_grid(
  ggplot(data, aes(x, y, group = ID, col = vit_3S)) +
    scale_color_manual(values = c("firebrick", "darkorange", "royalblue")) +
    geom_point(size = 0.3, alpha = 0.5) +
    geom_path() +
    coord_map(), 
  ggplot(data, aes(x, y, group = ID, col = vit_2S)) +
    scale_color_manual(values = c("firebrick", "royalblue")) +
    geom_point(size = 0.3, alpha = 0.5) +
    geom_path() +
    coord_map(), 
  ncol = 1)






